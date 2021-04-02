unit ReleasesVersion;

{$mode objfpc}{$H+}

interface

uses
  Forms,Classes,SysUtils,StrUtils,LCLIntf,
  ComCtrls,StdCtrls,ExtCtrls,Controls,Dialogs,
  html_parse,fpURI,
  UAsyncQueue,u_irc;

Const
 releases_url='https://github.com/red-prig/twitch_vip_bot/releases';

procedure SendReleasesRequest;

implementation

Uses
 Main;

function FetchDot(var Value:RawByteString):RawByteString;
var
 i:SizeInt;
begin
 i:=Pos('.',Value);
 if (i=0) then
 begin
  Result:=Value;
  Value:='';
 end else
 begin
  Result:=Copy(Value,1,(i-1));
  Value:=Copy(Value,i+1,Length(Value)-(i-2));
 end;
end;

function CompareVersion(S1,S2:RawByteString):SizeInt;
var
 v1,v2:RawByteString;
begin
 repeat
  v1:=FetchDot(s1);
  v2:=FetchDot(s2);
  Result:=NaturalCompareText(v1,v2);
 until (Result<>0) or ((s1='') and (s2=''));
end;

function GetFloatSizeRus(i:SizeUInt):RawByteString;
Const
 CKB=1024;
 CMB=1024*1024;
 CGB=1024*1024*1024;
begin
 Case i of
  0  ..CKB-1:Result:=IntToStr(i)+'Б';
  CKB..CMB-1:Result:=FloatToStrF(i/CKB,ffFixed,0,2)+'Кб';
  CMB..CGB-1:Result:=FloatToStrF(i/CMB,ffFixed,0,2)+'Мб';
  else       Result:=FloatToStrF(i/CGB,ffFixed,0,2)+'Гб';
 end;
end;

type
 THtmlLinkParser=packed class(THtmlAttrParser)
  private
   _type_elem:Byte;
   markdown_body:Byte;
  protected
  public
   pattern,download_url,markdown_text:RawByteString;
   Procedure OnAttr;                override;
   Procedure OnElementName;         override;
   Procedure OnData(P:PAnsiChar;Len:SizeInt); override;
 end;

 PQNode_durl=^TQNode_durl;
 TQNode_durl=object(UAsyncQueue.TQNode)
  FUrl:PChar;
  FText:PChar;
  data:record end;
  Procedure OnParent;
 end;

 PQNode_Notify=^TQNode_Notify;
 TQNode_Notify=object(UAsyncQueue.TQNode)
  FOnNotify:TNotifyEvent;
  FSender:TObject;
  Procedure OnParent;
 end;

 THttpStream2Location=class(THttpStream)
  location,fname:RawByteString;
  procedure on_headers(Const name,value:RawByteString;cat:Longint); override;
 end;

 TStatusInfo=class
  url,
  status,
  fname:RawByteString
 end;

 TProgressBarForm=class(TCustomForm)
  public
   Bar:TProgressBar;
   L:TLabel;
   FClientData:THttpClient;
   FHttpStream:THttpStream2File;
   T:TTimer;
   constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
   procedure   OnEndDownload1_node(Sender:TObject);
   procedure   OnEndDownload1_main(Sender:TObject);
   procedure   OnEndDownload2_node(Sender:TObject);
   procedure   OnEndDownload2_main(Sender:TObject);
   Procedure   RunTimer;
   Procedure   StopTimer;
   Procedure   FOnTimer(Sender:TObject);
 end;

 TProgressStream2Mem=class(THttpStream2Mem)
  public
   procedure OnEndStream(Sender:TObject);
 end;

Procedure THtmlLinkParser.OnAttr;
const
 pattern2='download';
Var
 url:RawByteString;
begin
 Case _type_elem of
  1:Case RawByteString(N) of
     'href' :begin
              url:=GetUnescapeHTML(V,StrLen(V));
              if (url<>'') then
              begin
               if Pos(pattern,url)<>0 then
               if Pos(pattern2,url)<>0 then
               begin
                download_url:=url;
                Abort;
               end;
              end;
             end;
    end;
  2:Case RawByteString(N) of
     'class':Case RawByteString(V) of
              'markdown-body':markdown_body:=1;
             end;
    end;
 end;
end;

Procedure THtmlLinkParser.OnElementName;
begin
 Case F of
  '/':
   Case RawByteString(N) of
    'div':if (markdown_body=1) then markdown_body:=2;
   end;
  else
    Case RawByteString(N) of
     'a'  :_type_elem:=1;
     'div':if (markdown_body=0) then _type_elem:=2;
      'p',
      'br':
      if (markdown_body=1) then
      begin
       markdown_text:=TrimLeft(markdown_text+#13#10);
       _type_elem:=0;
      end;
     else  _type_elem:=0;
    end;
 end;
end;

Procedure THtmlLinkParser.OnData(P:PAnsiChar;Len:SizeInt);
begin
 if (markdown_body=1) then
 begin
  markdown_text:=TrimLeft(markdown_text+GetUnescapeHTML(P,Len));
 end;
end;

procedure THttpStream2Location.on_headers(Const name,value:RawByteString;cat:Longint);
begin
 inherited;
 case name of
  'location':location:=value;
 end;
end;

procedure TProgressStream2Mem.OnEndStream(Sender:TObject);
var
 P:PQNode_durl;
 ClientData:THttpClient;
 HttpStream:TProgressStream2Mem;
 Parser:THtmlLinkParser;
 M:TMemoryStream;
 URI:TURI;
 S:RawByteString;
begin
 HttpStream:=TProgressStream2Mem(Sender);
 ClientData:=HttpStream.FClientData;

 URI:=parse_uri(releases_url);
 M:=TMemoryStream(HttpStream.FRecvs);
 Parser:=THtmlLinkParser.Create;
 Parser.pattern:=URI.GetPath;
 Parser.Parse(M.Memory,M.Size);

 if (Parser.download_url<>'') then
 begin
  Parser.markdown_text:=TrimRight(Parser.markdown_text);
  S:=URI.getProtocol+'://'+URI.getAuthority+Parser.download_url;
  P:=AllocMem(SizeOf(TQNode_durl)+Length(S)+Length(Parser.markdown_text)+2);
  P^.Parent:=@P^.OnParent;
  P^.FUrl:=@P^.data;
  P^.FText:=@P^.FUrl[Length(S)+1];
  Move(PAnsiChar(S)^,P^.FUrl^,Length(S)+1);
  Move(PAnsiChar(Parser.markdown_text)^,P^.FText^,Length(Parser.markdown_text)+1);
  SendMainQueue(P);
 end;

 Parser.Free;
 ClientData.terminate;
end;

constructor TProgressBarForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
 inherited;
 BorderIcons:=[biSystemMenu];
 BorderStyle:=bsNone;
 Position:=poMainFormCenter;
 FormStyle:=fsStayOnTop;
 ShowInTaskBar:=stNever;
 if Assigned(AOwner) and AOwner.InheritsFrom(TCustomForm) then
 begin
  Width:=TCustomForm(AOwner).Width;
 end;
 Height:=20;
 Bar:=TProgressBar.Create(Self);
 Bar.Align:=alClient;
 Bar.Style:=pbstMarquee;
 Bar.Parent:=Self;
 L:=TLabel.Create(Self);
 L.Alignment:=taCenter;
 L.Align:=alClient;
 L.Font.Color:=0;
 L.Font.Size:=12;
 L.Caption:='Получение ссылки';
 L.Parent:=Bar;
end;

Procedure TQNode_durl.OnParent;
var
 download_url,rText:RawByteString;
 v,fname:RawByteString;
 ClientData:THttpClient;
 HttpStream:THttpStream2Location;
 Form:TProgressBarForm;
begin
 download_url:=FUrl;
 rText:=FText;

 FreeMem(@Self);

 fname:=ExtractFileExt(download_url);
 fname:=ChangeFileExt('update',fname);

 v:=ExtractFileDir(download_url);
 v:=ExtractFileName(v);

 if CompareVersion(v,current_version)>0 then
 begin
  if QuestionDlg('Загрузить обновление ('+v+')?',
                 'Примечания к версии'+#13#10#13#10+rText,
                 mtInformation,
                 [mrYes,'Да',mrNo,'Нет'],
                 'Загрузить обновление?')=mrYes then
  begin
   Form:=TProgressBarForm.CreateNew(FrmMain);
   Form.Show;

   ClientData:=nil;
   HttpStream:=THttpStream2Location.Create;
   HttpStream.fname:=fname;
   HttpStream.FOnEndStream:=@Form.OnEndDownload1_node;
   HttpStream.AddStdHdr;
   HttpStream.SetUrl(download_url);
   if not replyConnect(ClientData,THttpClient,download_url) then
   begin
    ShowMessage('Ошибка: хост не найден!');
    FreeAndNil(HttpStream);
    Form.Release;
    Exit;
   end;
   ClientData.submit(HttpStream);

   Form.FClientData:=ClientData;
  end;
 end;

end;

Procedure TQNode_Notify.OnParent;
begin
 if Assigned(FOnNotify) then
  FOnNotify(FSender);
 FreeMem(@Self);
end;

procedure TProgressBarForm.OnEndDownload1_node(Sender:TObject);
var
 P:PQNode_Notify;
 StatusInfo:TStatusInfo;
 HttpStream:THttpStream2Location;
 ClientData:THttpClient;
begin

 HttpStream :=THttpStream2Location(Sender);
 StatusInfo       :=TStatusInfo.Create;
 StatusInfo.url   :=HttpStream.location;
 StatusInfo.status:=HttpStream.status;
 StatusInfo.fname :=HttpStream.fname;
 ClientData:=HttpStream.FClientData;
 ClientData.terminate;
 ClientData:=nil;
 HttpStream:=nil;

 P:=AllocMem(SizeOf(TQNode_Notify));
 P^.Parent:=@P^.OnParent;
 P^.FOnNotify:=@OnEndDownload1_main;
 P^.FSender:=StatusInfo;
 SendMainQueue(P);
end;

procedure TProgressBarForm.OnEndDownload2_node(Sender:TObject);
var
 P:PQNode_Notify;
 StatusInfo:TStatusInfo;
 ClientData:THttpClient;
 HttpStream:THttpStream2File;
begin
 HttpStream:=THttpStream2File(Sender);
 StatusInfo       :=TStatusInfo.Create;
 StatusInfo.status:=HttpStream.status;
 StatusInfo.fname :=TFileStream(HttpStream.FRecvs).FileName;
 FreeAndNil(HttpStream.FRecvs);
 ClientData:=HttpStream.FClientData;
 ClientData.terminate;
 ClientData:=nil;
 HttpStream:=nil;

 P:=AllocMem(SizeOf(TQNode_Notify));
 P^.Parent:=@P^.OnParent;
 P^.FOnNotify:=@OnEndDownload2_main;
 P^.FSender:=StatusInfo;
 SendMainQueue(P);
end;

procedure TProgressBarForm.OnEndDownload1_main(Sender:TObject);
var
 StatusInfo:TStatusInfo;
 ClientData:THttpClient;
 HttpStream:THttpStream2File;
begin
 StatusInfo:=TStatusInfo(Sender);
 ClientData:=nil;

 Bar.Style:=pbstNormal;

 if (StatusInfo.status='302') and
    (StatusInfo.url<>'') then
 begin
  try
   HttpStream:=THttpStream2File.Create(StatusInfo.fname,fmCreate);
  except
   on E:Exception do
   begin
    ShowMessage('Ошибка('+E.Message+'), не удалось создать файл!');
    Release;
    FreeAndNil(StatusInfo);
    Exit;
   end;
  end;
  HttpStream.FOnEndStream:=@OnEndDownload2_node;
  HttpStream.AddStdHdr;
  HttpStream.SetUrl(StatusInfo.url);
  if not replyConnect(ClientData,THttpClient,StatusInfo.url) then
  begin
   ShowMessage('Ошибка: хост не найден!');
   Release;
   FreeAndNil(HttpStream);
   FreeAndNil(StatusInfo);
   Exit;
  end;
  ClientData.submit(HttpStream);

  FClientData:=ClientData;
  FHttpStream:=HttpStream;

  RunTimer;
 end else
 begin
  ShowMessage('Ошибка('+StatusInfo.status+'), не удалось получить ссылку на файл!');
  Release;
 end;

 FreeAndNil(StatusInfo);
end;

procedure TProgressBarForm.OnEndDownload2_main(Sender:TObject);
var
 StatusInfo:TStatusInfo;
begin
 StatusInfo:=TStatusInfo(Sender);

 StopTimer;

 if (StatusInfo.status<>'200') then
 begin
  ShowMessage('Ошибка('+StatusInfo.status+'), не удалось загрузить файл!');
  Release;
  FreeAndNil(StatusInfo);
  Exit;
 end;

 OpenDocument(StatusInfo.fname);

 Halt;
end;

Procedure TProgressBarForm.RunTimer;
begin
 T:=TTimer.Create(Self);
 T.Interval:=200;
 T.OnTimer:=@FOnTimer;
 T.Enabled:=True;
end;

Procedure TProgressBarForm.StopTimer;
begin
 if Assigned(T) then
 begin
  T.Enabled:=False;
  FreeAndNil(T);
 end;
end;

Procedure TProgressBarForm.FOnTimer(Sender:TObject);
var
 R:TProgInfo;
begin
 if Assigned(FHttpStream) then
 begin
  R:=FHttpStream.Prog.Read(0);
  Bar.Min:=0;
  Bar.Max:=R.Size-1;
  Bar.Position:=R.Load;
  L.Caption:='Скачивание: '+GetFloatSizeRus(R.Load)
            +'/'+GetFloatSizeRus(R.Size)
            +' Скорость:'+GetFloatSizeRus(R.Speed)+'/сек';
 end;
end;

procedure SendReleasesRequest;
var
 ClientData:THttpClient;
 HttpStream:TProgressStream2Mem;
begin
 ClientData:=nil;
 HttpStream:=TProgressStream2Mem.Create;
 HttpStream.FOnEndStream:=@HttpStream.OnEndStream;
 HttpStream.AddStdHdr;
 HttpStream.SetUrl(releases_url);
 if not replyConnect(ClientData,THttpClient,releases_url) then
 begin
  FreeAndNil(HttpStream);
  Exit;
 end;
 ClientData.submit(HttpStream);
end;


end.

