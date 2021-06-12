unit ytts;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,DateUtils,
  libportaudio,
  evpool,
  html_parse,
  Ufpjson,
  AudioEngine,
  UAsyncQueue,
  u_irc,fpcookie_parse;

type
 Tytts=class;

 Tytts_client=class(ThttpClient)
  parent:Tytts;
  csrf_token:RawByteString;
  Destructor  Destroy; override;
 end;

 TCookieRec=record
  name,Path,value:RawByteString;
  Expires:Int64;
  Flags:set of (crSession,crSecure,crHttpOnly,crStrict,crLax);
 end;

 TCookieDomainRec=object
  Const
   MaxRecs=50;
  Var
   Domain:RawByteString;
   Data:Array of TCookieRec;
  Function  FindCookie(Const _C:TCookieRec):SizeInt;
  procedure DelCookie(i:SizeInt);
  Function  AddCookie(Const _C:TCookieRec):SizeInt;
  Function  CheckExpire:SizeInt;
  Function  BuildCookieStr(Const Path:RawByteString;Secure,SameSite:Boolean):RawByteString;
 end;

 TCookieStore=object
  Data:Array of TCookieDomainRec;
  Function  FindDomain(Const _Domain:RawByteString):SizeInt;
  Function  AddDomain(Const _Domain:RawByteString):SizeInt;
  procedure DelDomain(i:SizeInt);
  Procedure SetCookie(C:TSetCookie;Host:RawByteString);
  Function  CheckExpire:SizeInt;
  Function  BuildCookieStr(Const Host,Path:RawByteString;Secure,SameSite:Boolean):RawByteString;
 end;

 TyVoice=(alyss,jane,oksana,omazh,zahar,ermil,alena,filipp);
 TyEmotion=(good,evil,neutral);

 Tytts=class
  private
   FRef:SizeUInt;
  protected
   msg_timer:Ptimer;
   msg_send_buf:Pevbuffer;
   CookieStore:TCookieStore;
   client:Tytts_client;
   FSpeed:Currency; //0.5..3.0
   FVoice:TyVoice;
   FEmotion:TyEmotion;
   FVolume:Single;
   FIndex:PaDeviceIndex;
   FStartPlay:TNotifyEvent;
   FStopPlay:TNotifyEvent;
   Procedure   SetSpeed(S:Currency);
   function    SendHtml:Boolean;
   function    SendTts:Boolean;
   procedure   SendTimer;
   procedure   OnEndStreamHtml(Sender:TObject);
   procedure   OnEndStreamOgg(Sender:TObject);
  published
   property    DeviceIndex:PaDeviceIndex read FIndex write FIndex;
   property    Volume:Single read FVolume write FVolume;
   property    Speed:Currency read FSpeed write SetSpeed;
   property    Voice:TyVoice read FVoice write FVoice;
   property    Emotion:TyEmotion read FEmotion write FEmotion;
   property    StartPlay:TNotifyEvent read FStartPlay write FStartPlay;
   property    StopPlay:TNotifyEvent read FStopPlay write FStopPlay;
  public
   Constructor Create;
   procedure   FreeInstance; override;
   procedure   push_msg(msg:RawByteString);
 end;

{
 "Элиc (alyss)
 "Джейн (jane)
 "Оксана (oksana)
 "Омаж (omazh)
 "Захар (zahar)
 "Ермил (ermil)
 "Алёна (alena)
 "Филипп (filipp)

 Радостный (good)
 Раздражённый (evil)
 Нейтральный (neutral)
}

type
 PAudioConnection=^TAudioConnection;

var
 FAudioConnection:PAudioConnection;
 Fpool:Pevpool;

implementation

Uses
 WordFilter;

type
 THtmlCsrfParser=packed class(THtmlAttrParser)
  private
   is_meta:Boolean;
   is_name:Boolean;
  protected
  public
   csrf_token:RawByteString;
   Procedure OnAttr;                override;
   Procedure OnElementName;         override;
 end;

Procedure THtmlCsrfParser.OnAttr;
begin
 if is_meta then
  Case RawByteString(N) of
      'name':if RawByteString(V)='csrf-token' then is_name:=True;
   'content':if is_name then
             begin
              csrf_token:=RawByteString(V);
              Abort;
             end;
  end;
end;

Procedure THtmlCsrfParser.OnElementName;
begin
 is_name:=False;
 Case RawByteString(N) of
  'meta':is_meta:=True;
  else   is_meta:=False;
 end;
end;

Destructor Tytts_client.Destroy;
begin
 if parent<>nil then
 begin
  parent.client:=nil;
 end;
 inherited;
end;

Constructor Tytts.Create;
begin
 FIndex:=-1;
 FVolume:=1;
 FSpeed:=1;
 FVoice:=filipp;
 msg_send_buf:=evbuffer_new;
 FRef:=1;
end;

Procedure _on_free_ytts(param1:SizeUInt;param2:Pointer);
var
 Fytts:Tytts;
begin
 Fytts:=Tytts(param2);
 if Fytts=nil then Exit;
 if System.InterlockedDecrement(Pointer(Fytts.FRef))=nil then
 begin
  FreeAndNil(Fytts);
 end;
end;

procedure Tytts.FreeInstance;
begin
 if (FRef=0) then
 begin
  evbuffer_free(msg_send_buf);
  evtimer_del(msg_timer);
  FreeAndNil(client);
  inherited;
 end else
 begin
  evpool_post(Fpool,@_on_free_ytts,0,Pointer(Self));
 end;
end;

type
 THttpStream2MemCookie=class(THttpStream2Mem)
  Fytts:TYtts;
  procedure on_headers(Const name,value:RawByteString;cat:Longint); override;
 end;

Function TCookieStore.FindDomain(Const _Domain:RawByteString):SizeInt;
Var
 i:SizeInt;
begin
 Result:=-1;
 if Length(Data)<>0 then
 For i:=0 to High(Data) do
  if Data[i].Domain=_Domain then Exit(i);
end;

Function TCookieStore.AddDomain(Const _Domain:RawByteString):SizeInt;
begin
 Result:=FindDomain(_Domain);
 if (Result=-1) then
 begin
  Result:=Length(Data);
  SetLength(Data,Result+1);
  Data[Result]:=Default(TCookieDomainRec);
  Data[Result].Domain:=_Domain;
 end;
end;

procedure TCookieStore.DelDomain(i:SizeInt);
begin
 if (i>=0) and (i<Length(Data)) then
 begin
  Move(Data[i+1],Data[i],High(Data)-i);
 end;
 SetLength(Data,High(Data));
end;

Function TCookieDomainRec.FindCookie(Const _C:TCookieRec):SizeInt;
Var
 i:SizeInt;
begin
 Result:=-1;
 if Length(Data)<>0 then
 For i:=0 to High(Data) do
  if (Data[i].name=_C.name) and
     (Data[i].Path=_C.Path) then Exit(i);
end;

procedure TCookieDomainRec.DelCookie(i:SizeInt);
begin
 if (i>=0) and (i<Length(Data)) then
 begin
  Move(Data[i+1],Data[i],High(Data)-i);
 end;
 SetLength(Data,High(Data));
end;

Function TCookieDomainRec.AddCookie(Const _C:TCookieRec):SizeInt;
begin
 if not (crSession in _C.Flags) then
  if DateTimeToUnix(SysUtils.Now,False)>=_C.Expires then
  begin
   Result:=FindCookie(_C);
   if (Result<>-1) then
   begin
    DelCookie(Result);
    Result:=-1;
   end;
   Exit;
  end;
 Result:=FindCookie(_C);
 if (Result=-1) then
 begin
  Result:=Length(Data);
  if (Result>=MaxRecs) then
  begin
   Move(Data[1],Data[0],High(Data));
   Result:=Result-1;
  end else
  begin
   SetLength(Data,Result+1);
  end;
 end;
 Data[Result]:=_C;
end;

Procedure TCookieStore.SetCookie(C:TSetCookie;Host:RawByteString);
Var
 i:SizeInt;
 Domain:RawByteString;
 Rec:TCookieRec;
begin
 if not (cfInit in C.Flags) then Exit;
 if (Host='') then Exit;

 if (Host[1]='.') then
 begin
  Host:=Copy(Host,2,Length(Host)-1);
 end;
 Host:=LowerCase(Host);

 Rec:=Default(TCookieRec);
 Domain:=C.Domain.GetStr;
 if (Domain='') then
  Domain:=Host
 else
 begin
  if (Domain[1]='.') then
  begin
   Domain:=Copy(Domain,2,Length(Domain)-1);
  end;
  Domain:=LowerCase(Domain);
  if (Domain<>Host) Then Exit;
 end;

 if (cfSecure   in C.Flags) then Rec.Flags:=Rec.Flags+[crSecure  ];
 if (cfHttpOnly in C.Flags) then Rec.Flags:=Rec.Flags+[crHttpOnly];
 if (cfStrict   in C.Flags) then Rec.Flags:=Rec.Flags+[crStrict  ];
 if (cfLax      in C.Flags) then Rec.Flags:=Rec.Flags+[crLax     ];

 if (cfExpires in C.Flags) then
 begin
  Rec.Expires:=C.time;
 end else
 if (cfMaxAge  in C.Flags) then
 begin
  Rec.Expires:=DateTimeToUnix(Sysutils.Now,False)+C.time;
 end else
 begin
  Rec.Flags:=Rec.Flags+[crSession];
 end;

 Rec.Path:=C.Path.GetStr;
 if (Rec.Path='') then Rec.Path:='/';

 Rec.name :=C.name.GetStr;
 Rec.value:=C.value.GetStr;

 i:=AddDomain(Domain);
 Data[i].AddCookie(Rec);
end;

Function  TCookieDomainRec.CheckExpire:SizeInt;
Var
 i:SizeInt;
begin
 Result:=0;
 i:=0;
 While (i<Length(Data)) do
 begin
  if not (crSession in Data[i].Flags) then
   if DateTimeToUnix(SysUtils.Now,False)>=Data[i].Expires then
   begin
    DelCookie(i);
    Inc(Result);
   end else
    Inc(i);
 end;
end;

Function TCookieStore.CheckExpire:SizeInt;
Var
 i:SizeInt;
begin
 Result:=0;
 i:=0;
 While (i<Length(Data)) do
 begin
  Result:=Result+Data[i].CheckExpire;
  if Length(Data[i].Data)=0 then
   DelDomain(i)
  else
   Inc(i);
 end;
end;

Function TCookieDomainRec.BuildCookieStr(Const Path:RawByteString;Secure,SameSite:Boolean):RawByteString;
Var
 i:SizeInt;
 IsAdd:Boolean;
begin
 Result:='';
 i:=0;
 While (i<Length(Data)) do
 begin
  if not (crSession in Data[i].Flags) then
   if DateTimeToUnix(SysUtils.Now,False)>=Data[i].Expires then
   begin
    DelCookie(i);
    Continue;
   end;

  IsAdd:=True;
  if (not Secure) and (crSecure in Data[i].Flags) then
  begin
   IsAdd:=False;
  end;

  if IsAdd and (not SameSite) then
  begin
   if (crStrict in Data[i].Flags) then
   begin
    IsAdd:=False;
   end else
   if (not Secure) and (crLax in Data[i].Flags) then
   begin
    IsAdd:=False;
   end;
  end;

  if IsAdd then
  if Copy(Path,1,Length(Data[i].Path))=Data[i].Path then
  begin
   if (Result<>'') then Result:=Result+'; ';
   Result:=Result+Data[i].name+'='+Data[i].value;
  end;
  Inc(i);
 end;
end;

Function TCookieStore.BuildCookieStr(Const Host,Path:RawByteString;Secure,SameSite:Boolean):RawByteString;
Var
 i:SizeInt;
begin
 Result:='';
 i:=FindDomain(Host);
 if (i<>-1) then
 begin
  Result:=Data[i].BuildCookieStr(Path,Secure,SameSite);
 end;
end;

procedure THttpStream2MemCookie.on_headers(Const name,value:RawByteString;cat:Longint);
var
 C:TSetCookie;
begin
 inherited;
 case name of
  'set-cookie':
  if Fytts<>nil then
  begin
   C:=parse_set_cookie(PAnsiChar(value),Length(value));
   Fytts.CookieStore.SetCookie(C,Host);
  end;
 end;
end;

function Tytts.SendHtml:Boolean;
var
 Stream:THttpStream2MemCookie;
begin
 Result:=True;
 if client=nil then
 begin
  Result:=replyConnect(THttpClient(client),Tytts_client,'https://cloud.yandex.ru/',nil);
  if not Result then Exit;
  client.parent:=Self;

  Stream:=THttpStream2MemCookie.Create;
  Stream.Fytts:=Self;
  Stream.FOnEndStream:=@OnEndStreamHtml;

  Stream.AddHeader('User-Agent','Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:82.0) Gecko/20100101 Firefox/82.0');
  Stream.AddHeader('Accept','text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8');
  Stream.AddHeader('Accept-Language','ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3');
  Stream.AddHeader('Upgrade-Insecure-Requests','1');
  Stream.AddHeader('Pragma','no-cache');
  Stream.AddHeader('Cache-Control','no-cache');

  Stream.SetUrl('https://cloud.yandex.ru/services/speechkit');
  client.submit(Stream);
 end else
 begin
  Result:=SendTts;
 end;
end;

procedure Tytts.OnEndStreamHtml(Sender:TObject);
var
 Stream:THttpStream2MemCookie;
 Parser:THtmlCsrfParser;
 M:TMemoryStream;
begin
 Stream:=THttpStream2MemCookie(Sender);
 M:=TMemoryStream(Stream.FRecvs);

 Parser:=THtmlCsrfParser.Create;
 Parser.Parse(M.Memory,M.Size);

 if client<>nil then client.csrf_token:=Parser.csrf_token;
 Parser.Free;

 SendTimer;
end;

type
 THttpStream2MemJson=class(THttpStream2Mem)
  Fytts:TYtts;
  procedure   on_headers(Const name,value:RawByteString;cat:Longint); override;
  Destructor  Destroy; override;
 end;

procedure THttpStream2MemJson.on_headers(Const name,value:RawByteString;cat:Longint);
var
 C:TSetCookie;
begin
 inherited;
 case name of
  'set-cookie':
  if Fytts<>nil then
  begin
   C:=parse_set_cookie(PAnsiChar(value),Length(value));
   Fytts.CookieStore.SetCookie(C,Host);
  end;
 end;
end;

Destructor THttpStream2MemJson.Destroy;
begin
 FreeAndNil(FSends);
 inherited;
end;

Procedure _send_tts_cb(ev:Ptimer;arg:pointer);
var
 Fytts:TYtts;
begin
 Fytts:=TYtts(arg);
 evtimer_del(Fytts.msg_timer);
 Fytts.msg_timer:=nil;
 Fytts.SendTts;
end;

function Tytts.SendTts:Boolean;
const
 ogg_url='https://cloud.yandex.ru/api/speechkit/tts';
var
 v:Piovec;
 Stream:THttpStream2MemJson;
 M:TmemoryStream;
 FAddStr:TAddStr;
 FormatSettings:TFormatSettings;
 tmp:RawByteString;
begin
 Result:=client<>nil;
 if not Result then Exit;

 if (client.csrf_token='') then Exit(False);

 if (msg_timer<>nil) then Exit(False);

 v:=evbuffer_peek(msg_send_buf);
 if (v=nil) then Exit(False);

 Stream:=THttpStream2MemJson.Create;
 Stream.Fytts:=Self;
 Stream.FOnEndStream:=@OnEndStreamOgg;

 Stream.SetUrl(ogg_url);

 Stream.Method:='POST';
 Stream.AddHeader('User-Agent','Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:82.0) Gecko/20100101 Firefox/82.0');
 Stream.AddHeader('Accept','application/json, text/plain, */*');
 Stream.AddHeader('Accept-Language','ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3');
 Stream.AddHeader('Content-Type','application/json;charset=utf-8');
 Stream.AddHeader('x-csrf-token',client.csrf_token);
 Stream.AddHeader('Origin','https://cloud.yandex.ru');
 Stream.AddHeader('Referer','https://cloud.yandex.ru/services/speechkit');
 Stream.AddHeader('Cookie',CookieStore.BuildCookieStr(Stream.Host,Stream.Path,True,True));

 FAddStr:=Default(TAddStr);
 FAddStr.AddStr('{"message":"');
 __StringToJSONString(FAddStr,iovec_getdata(v),iovec_getlen(v));
 FAddStr.AddStr('","language":"ru-RU","speed":');

 FormatSettings:=Sysutils.DefaultFormatSettings;
 FormatSettings.DecimalSeparator:='.';
 tmp:=Sysutils.FloatToStrF(FSpeed,ffGeneral,2,3,FormatSettings);
 FAddStr.AddStr(tmp);

 Case FVoice of
  alyss :FAddStr.AddStr(',"voice":"alyss"');
  jane  :FAddStr.AddStr(',"voice":"jane"');
  oksana:FAddStr.AddStr(',"voice":"oksana"');
  omazh :FAddStr.AddStr(',"voice":"omazh"');
  zahar :FAddStr.AddStr(',"voice":"zahar"');
  ermil :FAddStr.AddStr(',"voice":"ermil"');
  alena :FAddStr.AddStr(',"voice":"alena"');
  filipp:FAddStr.AddStr(',"voice":"filipp"');
 end;

 case FEmotion of
  good   :FAddStr.AddStr(',"emotion":"good","format":"oggopus"}');
  evil   :FAddStr.AddStr(',"emotion":"evil","format":"oggopus"}');
  neutral:FAddStr.AddStr(',"emotion":"neutral","format":"oggopus"}');
 end;

 M:=TmemoryStream.Create;
 M.Write(FAddStr.FStr^,FAddStr.FLen);
 M.Position:=0;
 FAddStr.Free;

 Stream.FSends:=M;

 Stream.AddHeader('Content-Length',IntToStr(M.Size));

 client.submit(Stream);

 evbuffer_pop(msg_send_buf);
 SendTimer;
end;

procedure Tytts.SendTimer;
begin
 if evbuffer_get_length(msg_send_buf)<>0 then
 begin
  if (msg_timer=nil) then
  begin
   msg_timer:=evtimer_new(Fpool,@_send_tts_cb,Pointer(Self));
  end;
  evtimer_add(msg_timer,1000000);
 end;
end;

type
 TAudioHandleTts=class(TAudioHandle)
  FStopPlay:TNotifyEvent;
  function onPlayEnd:SizeInt;
 end;

function TAudioHandleTts.onPlayEnd:SizeInt;
begin
 if Assigned(FStopPlay) then
 begin
  FStopPlay(Self);
 end;
 Result:=0;
 FreeAndNil(Node.FStream);
 Free;
end;

type
 PQNode_Stream=^TQNode_Stream;
 TQNode_Stream=object(UAsyncQueue.TQNode)
  M:TMemoryStream;
  FVolume:Single;
  FIndex:PaDeviceIndex;
  FStartPlay:TNotifyEvent;
  FStopPlay:TNotifyEvent;
  Procedure OnParent;
 end;

Procedure TQNode_Stream.OnParent;
var
 AC:PAudioConnection;
 AH:TAudioHandleTts;
begin
 AC:=FAudioConnection;
 if (M<>nil) and (M.Size<>0) and (AC<>nil) then
 begin
  M.Position:=0;
  AH:=TAudioHandleTts.Create;
  AH.AudioConnection:=AC^;
  AH.OnFinish:=@AH.onPlayEnd;
  AH.Stream:=M;
  AH.Volume:=FVolume;
  AH.DeviceIndex:=FIndex;
  AH.FStopPlay:=FStopPlay;
  if Assigned(FStartPlay) then
  begin
   FStartPlay(AH);
  end;
  AH.Send;
 end else
 begin
  FreeAndNil(M);
 end;
 FreeMem(@Self);
end;

procedure Tytts.OnEndStreamOgg(Sender:TObject);
var
 Stream:THttpStream2MemJson;
 M:TMemoryStream;
 P:PQNode_Stream;
begin
 Stream:=THttpStream2MemJson(Sender);
 M:=TMemoryStream(Stream.FRecvs);
 if (M<>nil) and (M.Size<>0) then
 begin
  Stream.FRecvs:=nil;
  P:=AllocMem(SizeOf(TQNode_Stream));
  P^.Parent:=@P^.OnParent;
  P^.M:=M;
  P^.FVolume:=FVolume;
  P^.FIndex:=FIndex;
  P^.FStartPlay:=FStartPlay;
  P^.FStopPlay:=FStopPlay;
  SendMainQueue(P);
 end;
end;

Procedure _ytts_Connect_post(param1:SizeUInt;param2:Pointer);
var
 Fytts:Tytts;
begin
 Fytts:=Tytts(param2);
 if Fytts=nil then Exit;
 if System.InterlockedDecrement(Pointer(Fytts.FRef))=nil then
 begin
  FreeAndNil(Fytts);
  Exit;
 end;
 if not Fytts.SendHtml then
 begin
  Exit;
 end;
end;

procedure Tytts.push_msg(msg:RawByteString);
Var
 Len:SizeInt;
begin
 msg:=UTF8Encode(UnicodeLowerCase(UTF8Decode(msg)));
 Len:=Utf8Cut(PAnsiChar(msg),Length(msg),5000);
 FiltredChar(PAnsiChar(msg),Len);
 FiltredWords(PAnsiChar(msg),Len);
 evbuffer_add(msg_send_buf,PAnsiChar(msg),Len);
 System.InterlockedIncrement(Pointer(FRef));
 evpool_post(Fpool,@_ytts_Connect_post,0,Pointer(Self));
end;

Procedure Tytts.SetSpeed(S:Currency);
begin
 if S<0.1 then S:=0.5;
 if S>3   then S:=3;
 FSpeed:=S;
end;

end.

