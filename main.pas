unit main;

{$mode objfpc}{$H+}

interface

uses
  INIFiles,
  evpool,
  Classes, SysUtils, StrUtils,DateUtils,
  Forms, Controls, Graphics,
  Dialogs,ComCtrls,StdCtrls,
  toolwin,
  kgrids,kpagecontrol,kcontrols,
  LResources,Menus,LCLType,Clipbrd, ExtCtrls, Buttons,
  ExtStringGrid,

  UAsyncQueue,
  ulog,
  u_irc,

  ujson,

  TaskManager,DbcEngine,
  ZDbcIntfs,
  ZDbcSqLite,ZDbcInterbase6,DbcScript;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    MIAbout: TMenuItem;
    Misl: TMenuItem;
    MIExit: TMenuItem;
    PopupTray: TPopupMenu;
    SystemTray: TTrayIcon;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure SetLognBtn(Login:Boolean);
    procedure add_reward(const S:RawByteString);
    procedure add_to_chat(const S:RawByteString);
    procedure EdtSendKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure SystemTrayClick(Sender: TObject);
    procedure _set_field_story(DT:TDateTime;const user,S:RawByteString;aRow:Integer);
    procedure add_to_story(DT:TDateTime;const user,S:RawByteString);
    procedure FormActivate(Sender: TObject);
    procedure BtnToolPopupClick(Sender:TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetShowChat(V:Boolean);
    procedure SetShowStory(V:Boolean);
    procedure OnPopupClickChat(Sender:TObject);
    procedure OnPopupClickStory(Sender:TObject);
    procedure OnPopupClickAutoEnter(Sender:TObject);
    procedure OnPopupClickUseTray(Sender:TObject);
    procedure OnPopupClickParam(Sender:TObject);
    procedure OnTabClose(Sender:TObject;TabIndex:Integer;var CanClose:Boolean);
    procedure OnBtnEnterClick(Sender:TObject);
    procedure OnBtnCloseClick(Sender:TObject);
    Procedure OnList(Sender:TBaseTask);
    procedure OnPopupClose(Sender:TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
   FInsertScript:TSQLScript;

   login:RawByteString;
   rule_title:RawByteString;
   rule_cmd:RawByteString;
   rule_cmd2:RawByteString;
   rule_perc:Byte;

   frmPanel:TPanel;
   LeftBar,RightBar:TToolbar;
   BtnCfg:TButton;
   BtnView:TButton;

   BtnEnter:TSpeedButton;
   BtnInfo:TSpeedButton;
   BtnClose:TSpeedButton;

   PopupView:TPopupMenu;
   PopupCfg:TPopupMenu;

   Item_Chat,
   Item_Story,
   Item_AutoEnter,
   Item_UseTray:TMenuItem;

   EdtSend:TEdit;

  end;

var
 pool:Tevpool;
 pool_config:Tevpool_config;

 FrmMain: TFrmMain;

procedure push_chat(const S:RawByteString);
procedure push_login;
procedure push_reward(const S:RawByteString);

var
 Config:TINIFile;

implementation

uses
 mtRandom,
 UFrmAbout,
 UFrmParam,
 Uloginf;

{$R *.lfm}

{ TFrmMain }

 //zeosdbo;zeosdbo\component;zeosdbo\core;zeosdbo\plain;zeosdbo\parsesql;zeosdbo\dbc

var
 GridChat:TExtStringGrid;
 GridStory:TExtStringGrid;
 Pages:TKCustomPageControl;

 KCLOSE,KCLOSE_D,DIMAGE:TImageList;

 DbcThread:TDbcConnection;


type
 PQNode_push=^TQNode_push;
 TQNode_push=object(UAsyncQueue.TQNode)
  len:SizeUint;
  data:record end;
  Procedure OnParent1;
  Procedure OnParent2;
 end;

procedure push_chat(const S:RawByteString);
var
 P:PQNode_push;
begin
 P:=AllocMem(SizeOf(TQNode_push)+Length(S));
 P^.Parent:=@P^.OnParent1;
 P^.len:=Length(S);
 Move(PAnsiChar(S)^,P^.data,Length(S));
 SendMainQueue(P);
end;

Procedure TQNode_push.OnParent1;
var
 S:RawByteString;
begin
 SetString(S,@data,len);
 FrmMain.add_to_chat(S);
 FreeMem(@Self);
end;

Procedure TQNode_push.OnParent2;
var
 S:RawByteString;
begin
 SetString(S,@data,len);

 try
  FrmMain.add_reward(S);
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 FreeMem(@Self);
end;

type
 PQNode_login=^TQNode_login;
 TQNode_login=object(UAsyncQueue.TQNode)
  Procedure OnParent;
 end;

Procedure TQNode_login.OnParent;
begin
 FrmMain.SetLognBtn(True);
 FreeMem(@Self);
end;

procedure push_login;
var
 P:PQNode_login;
begin
 P:=AllocMem(SizeOf(TQNode_login));
 P^.Parent:=@P^.OnParent;
 SendMainQueue(P);
end;

procedure TFrmMain.SetLognBtn(Login:Boolean);
begin
 Case Login of
  True :begin
         BtnEnter.Visible:=False;
         BtnInfo.Caption:=Self.login;
         BtnInfo .Visible:=True;
         BtnClose.Visible:=True;
         BtnInfo.Left:=0;

         add_reward(
           '{"type":"reward-redeemed","data":{"timestamp":"2020-07-08T18:38:23.'+
           '141491302Z","redemption":{"id":"62d7f76e-7a16-432d-94ce-541897f02fa3","u'+
           'ser":{"id":"84616392","login":"satan_rulezz","display_name":"Satan_R'+
           'ulezz"},"channel_id":"54742538","redeemed_at":"2020-07-08T18:38:23.01829'+
           '9023Z","reward":{"id":"9c25cd82-30e4-4e23-8dae-e3ae630b9bab","channel_id'+
           '":"54742538","title":"VIP на хз сколько","prompt":"Может передумаю и  '+
           'отниму випку.","cost":450000,"is_user_input_required":true,"is_sub_only":'+
           'false,"image":null,"default_image":{"url_1x":"https://static-cdn.jtvnw.ne'+
           't/custom-reward-images/default-1.png","url_2x":"https://static-cdn.jtvnw.net'+
           '/custom-reward-images/default-2.png","url_4x":"https://static-cdn.jtvnw.net/'+
           'custom-reward-images/default-4.png"},"background_color":"#1F69FF","is_enab'+
           'led":true,"is_paused":false,"is_in_stock":true,"max_per_stream":{"is_ena'+
           'bled":false,"max_per_stream":0},"should_redemptions_skip_request_queue":fal'+
           'se,"template_id":null,"updated_for_indicator_at":"2020-07-06T17:34:56.82009'+
           '8059Z"},"user_input":"Опа -450к","status":"UNFULFILLED","cursor":"Nj'+
           'JkN2Y3NmUtN2ExNi00MzJkLTk0Y2UtNTQxODk3ZjAyZmEzX18yMDIwLTA3LTA4VDE4OjM4OjIzLjAxOD'+
           'I5OTAyM1o="}}}');

         {add_reward(
            '{"type":"reward-redeemed","data":{"timestamp":"2020-07-08T18:49:22.'+
            '62426474Z","redemption":{"id":"c3d94d57-544d-408d-9346-a7e2ccfce57d","us'+
            'er":{"id":"156899307","login":"vrediinao_o","display_name":"Vrediina'+
            'O_O"},"channel_id":"54742538","redeemed_at":"2020-07-08T18:49:22.4228512'+
            '62Z","reward":{"id":"9c25cd82-30e4-4e23-8dae-e3ae630b9bab","channel_id"'+
            ':"54742538","title":"VIP на хз сколько","prompt":"Может передумаю и  от'+
            'ниму випку.","cost":450000,"is_user_input_required":true,"is_sub_only":fa'+
            'lse,"image":null,"default_image":{"url_1x":"https://static-cdn.jtvnw.net/'+
            'custom-reward-images/default-1.png","url_2x":"https://static-cdn.jtvnw.net/c'+
            'ustom-reward-images/default-2.png","url_4x":"https://static-cdn.jtvnw.net/cu'+
            'stom-reward-images/default-4.png"},"background_color":"#1F69FF","is_enable'+
            'd":true,"is_paused":false,"is_in_stock":true,"max_per_stream":{"is_enabl'+
            'ed":false,"max_per_stream":0},"should_redemptions_skip_request_queue":false'+
            ',"template_id":null,"updated_for_indicator_at":"2020-07-06T17:34:56.8200980'+
            '59Z"},"user_input":"Просто дай випку, лол karmik3 ","status":"UNFULFILLE'+
            'D","cursor":"YzNkOTRkNTctNTQ0ZC00MDhkLTkzNDYtYTdlMmNjZmNlNTdkX18yMDIwLTA3LTA'+
            '4VDE4OjQ5OjIyLjQyMjg1MTI2Mlo="}}}');}

           //add_reward('{"type":"custom-reward-updated","data":{"timestamp":"2020-07-10T22:46:44.976712724Z","updated_reward":{"id":"6a39b2f0-af31-4c40-a130-3ea08e9ec79a","channel_id":"54742538","title":"Anti Emote Mode","prompt":"ОФАЕМ 15 минут emote-МОда","cost":30001,"is_user_input_required":true,"is_sub_only":false,"image":null,"default_image":{"url_1x":"https://static-cdn.jtvnw.net/custom-reward-images/default-1.png","url_2x":"https://static-cdn.jtvnw.net/custom-reward-images/default-2.png","url_4x":"https://static-cdn.jtvnw.net/custom-reward-images/default-4.png"},"background_color":"#8205B3","is_enabled":false,"is_paused":false,"is_in_stock":false,"max_per_stream":{"is_enabled":true,"max_per_stream":10},"should_redemptions_skip_request_queue":false,"template_id":null,"updated_for_indicator_at":"2019-12-21T20:50:39.00014802Z"}}}');

        end;
  False:begin
         BtnEnter.Visible:=True;
         BtnInfo .Visible:=False;
         BtnClose.Visible:=False;
        end;
 end;
end;

procedure TFrmMain.FormCloseQuery(Sender:TObject;var CanClose:boolean);
begin
 CanClose:=True;
 if Item_UseTray.Checked then
 begin
  CanClose:=False;
  SystemTray.Show;
  Hide;
 end;
end;

procedure TFrmMain.FormWindowStateChange(Sender: TObject);
begin
 if (WindowState=wsMinimized) and Item_UseTray.Checked then
 begin
  SystemTray.Show;
  Hide;
 end;
end;

procedure TFrmMain.MIAboutClick(Sender: TObject);
begin
 FrmAbout.Show;
end;

procedure push_reward(const S:RawByteString);
var
 P:PQNode_push;
begin
 P:=AllocMem(SizeOf(TQNode_push)+Length(S));
 P^.Parent:=@P^.OnParent2;
 P^.len:=Length(S);
 Move(PAnsiChar(S)^,P^.data,Length(S));
 SendMainQueue(P);
end;

function fetch_dt(msg2:TJson):TDateTime; inline;
begin
 Result:=Rfc3339toDateTime(msg2.Path['data.redemption.redeemed_at'].AsStr,false);
end;

function fetch_user(msg2:TJson):RawByteString; inline;
begin
 Result:=msg2.Path['data.redemption.user.display_name'].AsStr;
end;

function fetch_msg(msg2:TJson):RawByteString; inline;
begin
 Result:=msg2.Path['data.redemption.reward.title'].AsStr+': '+
         msg2.Path['data.redemption.user_input'].AsStr;
end;

procedure TFrmMain.add_reward(const S:RawByteString);
var
 DT:TDateTime;
 ms:TMemoryStream;
 msg2:TJson;
 msg,cmd,user,rs:RawByteString;
 FDbcScript:TDbcStatementScript;
 RCT:TMTRandomContext;
begin

 ms:=TMemoryStream.Create;
 ms.Write(PAnsiChar(s)^,Length(s));
 ms.Position:=0;

 msg2:=Default(TJson);
 try
  msg2:=TJson.New(ms);
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 FreeAndNil(ms);

 DT:=fetch_dt(msg2);
 if IsNullValue(DT) then DT:=Now;

 msg:=fetch_msg(msg2);
 user:=fetch_user(msg2);

 if msg2.Path['type'].AsStr='reward-redeemed' then
 begin

  rs:=s;

  if rule_title=Trim(msg2.Path['data.redemption.reward.title'].AsStr) then
  begin
   RCT:=Default(TMTRandomContext);
   RandomInit(RCT);

      //0..99         //70 0-69
   if Random(RCT,100)<rule_perc then
   begin
    cmd:=Format(rule_cmd,[msg2.Path['data.redemption.user.login'].AsStr]);
    reply_irc_msg(cmd);
    add_to_chat('>'+login+': '+cmd);
   end else
   begin
    cmd:=Format(rule_cmd2,[msg2.Path['data.redemption.user.login'].AsStr]);
    reply_irc_msg(cmd);
    add_to_chat('>'+login+': '+cmd);
   end;

   msg:=msg+' ('+cmd+')';

   msg2.Values['msg_cmd']:=cmd;
   ms:=TMemoryStream.Create;
   msg2.Dump(ms);
   SetLength(rs,ms.Size);
   ms.Position:=0;
   ms.Read(PAnsiChar(rs)^,Length(rs));
   FreeAndNil(ms);

   //Writeln('id               :',msg2.Path['data.redemption.id'].AsStr);
   //Writeln('user.id          :',msg2.Path['data.redemption.user.id'].AsStr);
   //Writeln('user.login       :',msg2.Path['data.redemption.user.login'].AsStr);
   //Writeln('user.display_name:',msg2.Path['data.redemption.user.display_name'].AsStr);
   //Writeln('redeemed_at      :',msg2.Path['data.redemption.redeemed_at'].AsStr);
   //Writeln('reward.id        :',msg2.Path['data.redemption.reward.id'].AsStr);
   //Writeln('reward.title     :',msg2.Path['data.redemption.reward.title'].AsStr);
   //Writeln('reward.prompt    :',msg2.Path['data.redemption.reward.prompt'].AsStr);
   //Writeln('user_input       :',msg2.Path['data.redemption.user_input'].AsStr);
   //Writeln('status           :',msg2.Path['data.redemption.status'].AsStr);
  end;

  add_to_story(DT,user,msg);

  FDbcScript:=TDbcStatementScript.Create;
  FDbcScript.Handle.DbcConnection:=DbcThread;
  FDbcScript.SetSctipt(FInsertScript);
  FDbcScript.ExecuteScript;
  FDbcScript.Params.SetAsDateTime('datetime',DT);
  FDbcScript.Params.SetRawByteString('user',user);
  FDbcScript.Params.SetRawByteString('mes',rs);
  FDbcScript.Start;
  FDbcScript.Release;

 end;

 msg2.Free;

end;

procedure TFrmMain.add_to_chat(const S:RawByteString);
var
 Hr:Word;
 Mn:Word;
 Sc:Word;
 Ms:Word;
 aRow:Integer;
begin
 if (GridChat.RowCount=2) and (GridChat.FieldValue['mes',1]='') then
 begin
  aRow:=1;
 end else
 begin
  aRow:=GridChat.InsertRow(GridChat.RowCount).Index;
 end;
 DecodeTime(Now,Hr,Mn,Sc,Ms);
 GridChat.FieldValue['time',aRow]:=' '+AddChar('0',IntToStr(Hr),2)+':'+AddChar('0',IntToStr(Mn),2);
 GridChat.FieldValue['mes' ,aRow]:=S;

 if GridChat.RowCount>300 then
 begin
  GridChat.DeleteRow(1);
  Dec(aRow);
 end;

 GridChat.ScrollModeVert:=smCell;
 GridChat.ScrollModeVert:=smSmooth;

 GridChat.ScrollBy(0,aRow);

end;

procedure TFrmMain.EdtSendKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
var
 msg:RawByteString;
begin
 if (Key=13) and BtnInfo.Visible then
 begin
  msg:=EdtSend.Text;
  EdtSend.Text:='';
  reply_irc_msg(msg);
  add_to_chat('>'+login+': '+msg);
 end;
end;

procedure TFrmMain.SystemTrayClick(Sender: TObject);
begin
 WindowState:=wsNormal;
 Show;
end;

procedure TFrmMain._set_field_story(DT:TDateTime;const user,S:RawByteString;aRow:Integer);
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='yyyy/mm/dd';
 FS.DateSeparator:='.';
 FS.ShortTimeFormat:='hh:nn:ss';
 FS.LongTimeFormat:='hh:nn:ss';
 FS.TimeSeparator:=':';

 GridStory.FieldValue['datetime',aRow]:=' '+DateTimeToStr(DT,FS,True);
 GridStory.FieldValue['user'    ,aRow]:=user;
 GridStory.FieldValue['mes'     ,aRow]:=S;
end;

procedure TFrmMain.add_to_story(DT:TDateTime;const user,S:RawByteString);
Var
 aRow:Integer;
begin
 if (GridStory.RowCount=2) and (GridStory.FieldValue['mes',1]='') then
 begin
  aRow:=1;
 end else
 begin
  aRow:=GridStory.InsertRow(GridStory.RowCount).Index;
 end;

 _set_field_story(DT,user,S,aRow);

 if GridStory.RowCount>300 then
 begin
  GridStory.DeleteRow(1);
  Dec(aRow);
 end;

 GridStory.ScrollModeVert:=smCell;
 GridStory.ScrollModeVert:=smSmooth;

 GridStory.ScrollBy(0,aRow);
end;

Procedure TFrmMain.OnList(Sender:TBaseTask);
Var
 datetime_f,user_f,mes_f:SizeInt;
 i,c:SizeInt;
 ResultSet:TZResultSet;
 ms:TMemoryStream;
 msg2:TJson;
 msg,cmd:RawByteString;
begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;

 c:=0;
 if ResultSet.Last then
 begin
  c:=ResultSet.GetRow;
  GridStory.RowCount:=c+1;
 end;

 if c>0 then
 begin
  ms:=TMemoryStream.Create;
  datetime_f:=ResultSet.FindColumn('datetime');
  user_f    :=ResultSet.FindColumn('user');
  mes_f     :=ResultSet.FindColumn('mes');
  For i:=1 to c do
  begin
   ResultSet.MoveAbsolute(i);

   msg:=ResultSet.GetRawByteString(mes_f);

   ms.Clear;
   ms.Write(PAnsiChar(msg)^,Length(msg));
   ms.Position:=0;

   msg2:=Default(TJson);
   try
    msg2:=TJson.New(ms);
   except
    on E:Exception do
    begin
     DumpExceptionCallStack(E);
    end;
   end;

   msg:=fetch_msg(msg2);
   cmd:=String(msg2.Values['msg_cmd']);
   if cmd<>'' then
    msg:=msg+' ('+cmd+')';

   _set_field_story(ResultSet.GetDouble(datetime_f),ResultSet.GetRawByteString(user_f),msg,i);

   msg2.Free;
  end;
  FreeAndNil(ms);
 end;

 GridStory.ScrollModeVert:=smCell;
 GridStory.ScrollModeVert:=smSmooth;

 GridStory.ScrollBy(0,c);

end;

procedure TFrmMain.FormActivate(Sender: TObject);
var
 cx:SizeInt;
begin
 GridChat.AutoSizeCol(0,true);
 GridChat.Columns[0].MinExtent:=GridChat.Columns[0].Extent;

 cx:=GridStory.Canvas.TextExtent(' 0000.00.00 00:00:000').cx;
 GridStory.Columns[0].Extent:=cx;
 GridStory.Columns[0].MinExtent:=cx;

 cx:=GridStory.Canvas.TextExtent(' YAEBALETOT').cx;
 GridStory.Columns[1].Extent:=cx;
 GridStory.Columns[1].MinExtent:=cx;

 if Item_AutoEnter.Checked then
 begin
  try
   login:=Trim(Config.ReadString('base','login',''));
   reply_irc_Connect(
     login,
     Trim(Config.ReadString('base','oAuth','')),
     Config.ReadString('base','chat',''),
     Config.ReadString('base','chat_id','')
   );
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmMain.BtnToolPopupClick(Sender:TObject);
Var
 P:TPoint;
begin
 if Sender is TButton then
 if Assigned(TButton(Sender).PopupMenu) then
 begin
  P.Create(0,TButton(Sender).Height);
  P:=TButton(Sender).ClientToScreen(P);
  TButton(Sender).PopupMenu.PopUp(P.x,P.y);
 end;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
 FreeAndNil(Pages);
 DbcThread.Stop;
 FreeAndNil(DbcThread);
 evpool_stop(@pool);
 FreeAndNil(Config);
end;

procedure TFrmMain.SetShowChat(V:Boolean);
Var
 Page:TKTabSheet;
begin
 Item_Chat.Checked:=V;
 Case V of
  True :begin
         Page:=Pages.AddPage(Pages);
         Page.Caption:='[-_-] Чат';
         Page.Tag:=0;
         Page.ImageIndex:=-1;

         Page.DisableAlign;

         EdtSend.Parent:=Page;
         EdtSend.Align:=alBottom;
         EdtSend.Anchors:=[akLeft,akRight,akBottom];

         GridChat.Parent:=Page;
         GridChat.Anchors:=[akTop,akLeft,akRight,akBottom];
         GridChat.AnchorSide[akTop].Side:=asrTop;
         GridChat.AnchorSide[akTop].Control:=Page;
         GridChat.AnchorSide[akBottom].Side:=asrTop;
         GridChat.AnchorSide[akBottom].Control:=EdtSend;
         GridChat.AnchorSide[akLeft].Side:=asrLeft;
         GridChat.AnchorSide[akLeft].Control:=Page;
         GridChat.AnchorSide[akRight].Side:=asrRight;
         GridChat.AnchorSide[akRight].Control:=Page;
         Page.EnableAlign;

         Page.Show;

        end;
  False:begin
         Page:=TKTabSheet(GridChat.Parent);
         Page.DisableAlign;
         GridChat.Parent:=nil;
         GridChat.Anchors:=[];
         GridChat.AnchorSide[akBottom].Control:=nil;
         EdtSend.Parent:=nil;
         Page.EnableAlign;
         Application.ReleaseComponent(Page);
        end;
 end;
end;

procedure TFrmMain.SetShowStory(V:Boolean);
Var
 Page:TKTabSheet;
begin
 Item_Story.Checked:=V;
 Case V of
  True :begin
         Page:=Pages.AddPage(Pages);
         Page.Caption:='История событий';
         Page.Tag:=1;
         Page.ImageIndex:=-1;
         Page.Show;
         GridStory.Parent:=Page;
         GridStory.AnchorAsAlign(alClient,1);
        end;
  False:begin
         Page:=TKTabSheet(GridStory.Parent);
         GridStory.Parent:=nil;
         Application.ReleaseComponent(Page);
        end;
 end;
end;

procedure TFrmMain.OnPopupClickChat(Sender:TObject);
begin
 SetShowChat(not Item_Chat.Checked);
end;

procedure TFrmMain.OnPopupClickStory(Sender:TObject);
begin
 SetShowStory(not Item_Story.Checked);
end;

procedure TFrmMain.OnPopupClickAutoEnter(Sender:TObject);
begin
 Item_AutoEnter.Checked:=not Item_AutoEnter.Checked;
 try
  Case Item_AutoEnter.Checked of
   True :Config.WriteString('view','autologin','1');
   False:Config.WriteString('view','autologin','0');
  end;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;
end;

procedure TFrmMain.OnPopupClickUseTray(Sender:TObject);
begin
 Item_UseTray.Checked:=not Item_UseTray.Checked;
 try
  Case Item_UseTray.Checked of
   True :Config.WriteString('view','systray','1');
   False:Config.WriteString('view','systray','0');
  end;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;
end;

procedure TFrmMain.OnPopupClickParam(Sender:TObject);
begin
 try
  FrmParam.EdtLogin.Text   :=Config.ReadString('base','login','');
  FrmParam.EdtPassword.Text:=Config.ReadString('base','oAuth','');
  FrmParam.EdtChat.Text    :=Config.ReadString('base','chat','');
  FrmParam.EdtChatID.Text  :=Config.ReadString('base','chat_id','');
  FrmParam.EdtTitle.Text   :=Config.ReadString('rule','title','');
  FrmParam.EdtMsg.Text     :=Config.ReadString('rule','msg_cmd','');
  FrmParam.EdtMsg2.Text    :=Config.ReadString('rule','msg_cmd2','');
  FrmParam.EdtPercent.Text :=Config.ReadString('rule','msg_perc','');
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if FrmParam.ShowModal=1 then
 begin
  rule_title:=Trim(FrmParam.EdtTitle.Text);
  rule_cmd  :=Trim(FrmParam.EdtMsg.Text);
  rule_cmd2 :=Trim(FrmParam.EdtMsg2.Text);
  rule_perc :=StrToQWORDDef(FrmParam.EdtPercent.Text,70);
  if rule_perc=0 then   rule_perc:=1;
  if rule_perc>100 then rule_perc:=100;

  try
   Config.WriteString('base','login'   ,FrmParam.EdtLogin.Text);
   Config.WriteString('base','oAuth'   ,FrmParam.EdtPassword.Text);
   Config.WriteString('base','chat'    ,FrmParam.EdtChat.Text);
   Config.WriteString('base','chat_id' ,FrmParam.EdtChatID.Text);
   Config.WriteString('rule','title'   ,rule_title);
   Config.WriteString('rule','msg_cmd' ,rule_cmd);
   Config.WriteString('rule','msg_cmd2',rule_cmd2);
   Config.WriteString('rule','msg_perc',IntToStr(rule_perc));
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmMain.OnTabClose(Sender:TObject;TabIndex:Integer;var CanClose:Boolean);
begin
 CanClose:=False;
 Case Pages.Pages[TabIndex].Tag of
  0:SetShowChat(False);
  1:SetShowStory(False);
 end;
end;

procedure TFrmMain.OnBtnEnterClick(Sender:TObject);
begin

 try
  login:=Config.ReadString('base','login','');
  frmLogin.EdtLogin.Text   :=login;
  frmLogin.EdtPassword.Text:=Config.ReadString('base','oAuth','');
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if frmLogin.ShowModal=1 then
 begin

  try
   login                    :=Trim(frmLogin.EdtLogin.Text);
   frmLogin.EdtPassword.Text:=Trim(frmLogin.EdtPassword.Text);

   Config.WriteString('base','login',login);
   Config.WriteString('base','oAuth',frmLogin.EdtPassword.Text);

   reply_irc_Connect(
     login,
     frmLogin.EdtPassword.Text,
     Config.ReadString('base','chat',''),
     Config.ReadString('base','chat_id','')
   );

  frmLogin.EdtPassword.Text:='';

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;

 end;
end;

procedure TFrmMain.OnBtnCloseClick(Sender:TObject);
begin
 reply_irc_Disconnect;
 SetLognBtn(false);
end;

Const
 CfgName='config.ini';
 DefZURL='zdbc:sqlite:///new.db';

function GetLocalIni:RawByteString;
begin
 Result:=ParamStr(0);
 Result:=ExtractFileDir(Result);
 Result:=IncludeTrailingPathDelimiter(Result);
 Result:=Result+CfgName;
end;

procedure TFrmMain.OnPopupClose(Sender:TObject);
begin
 Item_UseTray.Checked:=False;
 Close;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
Var
 Item:TMenuItem;
 D:RawByteString;
 FMem:TMemoryStream;
 FDbcScript:TDbcStatementScript;
begin

 D:=GetLocalIni;

 IF FileExists(D) then
 begin
  try
   Config:=TINIFile.Create(D);

   rule_title:=Trim(Config.ReadString('rule','title'   ,'VIP на хз сколько'));
   rule_cmd  :=Trim(Config.ReadString('rule','msg_cmd' ,'/vip %s'));
   rule_cmd2 :=Trim(Config.ReadString('rule','msg_cmd2','/timeout %s 86400'));
   rule_perc :=StrToQWORDDef(Config.ReadString('rule','msg_perc',''),70);
   if rule_perc=0 then   rule_perc:=1;
   if rule_perc>100 then rule_perc:=100;

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end else
 begin
  try
   Config:=TINIFile.Create(D);
   Config.WriteString('base','zurl'   ,DefZURL);
   Config.WriteString('base','login'  ,'');
   Config.WriteString('base','oAuth'  ,'');
   Config.WriteString('base','chat'   ,'karmikkoala');
   Config.WriteString('base','chat_id','54742538');

   Config.WriteString('rule','title'   ,'VIP на хз сколько');
   Config.WriteString('rule','msg_cmd' ,'/vip %s');
   Config.WriteString('rule','msg_cmd2','/timeout %s 86400');
   Config.WriteString('rule','msg_perc','70');

   Config.WriteString('view','autologin','0');
   Config.WriteString('view','systray'  ,'1');

   rule_title:=Trim(Config.ReadString('rule','title'   ,'VIP на хз сколько'));
   rule_cmd  :=Trim(Config.ReadString('rule','msg_cmd' ,'/vip %s'));
   rule_cmd2 :=Trim(Config.ReadString('rule','msg_cmd2','/timeout %s 86400'));
   rule_perc :=StrToQWORDDef(Config.ReadString('rule','msg_perc',''),70);
   if rule_perc=0 then   rule_perc:=1;
   if rule_perc>100 then rule_perc:=100;

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;

 {$I DialogControl.lrs}
 {$I KPageControl.lrs}

 frmPanel:=TPanel.Create(FrmMain);
 frmPanel.Align:=alTop;
 frmPanel.Anchors:=[akTop,akLeft,akRight];
 frmPanel.Height:=34;
 frmPanel.Left:=0;
 frmPanel.Parent:=FrmMain;

 LeftBar:=TToolbar.Create(frmPanel);
 LeftBar.Align:=alCustom;
 LeftBar.Anchors:=[akTop,akLeft];
 LeftBar.AutoSize:=True;
 LeftBar.ButtonHeight:=33;
 LeftBar.ButtonWidth:=33;
 LeftBar.EdgeBorders:=[];
 LeftBar.EdgeInner:=esNone;
 LeftBar.EdgeOuter:=esNone;
 LeftBar.Left:=1;
 LeftBar.Parent:=frmPanel;

 RightBar:=TToolbar.Create(frmPanel);
 RightBar.Align:=alCustom;
 RightBar.Anchors:=[akTop,akRight];
 RightBar.AutoSize:=True;
 RightBar.ButtonHeight:=33;
 RightBar.ButtonWidth:=33;
 RightBar.EdgeBorders:=[];
 RightBar.EdgeInner:=esNone;
 RightBar.EdgeOuter:=esNone;
 RightBar.Left:=frmPanel.ClientWidth-RightBar.Width-1;
 RightBar.Parent:=frmPanel;

 BtnCfg:=TButton.Create(LeftBar);
 BtnCfg.Caption:='Настройки';
 BtnCfg.AutoSize:=True;
 BtnCfg.Constraints.MinHeight:=32;
 BtnCfg.Height:=32;
 BtnCfg.Parent:=LeftBar;

 PopupCfg:=TPopupMenu.Create(FrmMain);
 PopupCfg.AutoPopup:=False;

 //param
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Параметры';
 Item.OnClick:=@OnPopupClickParam;
 PopupCfg.Items.Add(Item);

 //------
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='-';
 PopupCfg.Items.Add(Item);

 Item_UseTray:=TMenuItem.Create(PopupCfg);
 Item_UseTray.Caption:='Сворачивать в трей';
 Item_UseTray.OnClick:=@OnPopupClickUseTray;
 PopupCfg.Items.Add(Item_UseTray);

 Item_AutoEnter:=TMenuItem.Create(PopupCfg);
 Item_AutoEnter.Caption:='Автовход при запуске';
 Item_AutoEnter.OnClick:=@OnPopupClickAutoEnter;
 PopupCfg.Items.Add(Item_AutoEnter);

 //------
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='-';
 PopupCfg.Items.Add(Item);

 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='О программе';
 Item.OnClick:=@MIAboutClick;
 PopupCfg.Items.Add(Item);

 //------
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='-';
 PopupCfg.Items.Add(Item);

 //exit
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Выход';
 Item.OnClick:=@OnPopupClose;
 PopupCfg.Items.Add(Item);

 BtnCfg.OnClick:=@BtnToolPopupClick;
 BtnCfg.PopupMenu:=PopupCfg;

 try
  Item_UseTray.Checked  :=Config.ReadString('view','systray'  ,'1')='1';
  Item_AutoEnter.Checked:=Config.ReadString('view','autologin','0')='1';
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 BtnView:=TButton.Create(RightBar);
 BtnView.Caption:='Вид';
 BtnView.AutoSize:=True;
 BtnView.Constraints.MinHeight:=32;
 BtnView.Height:=32;
 BtnView.Parent:=LeftBar;

 PopupView:=TPopupMenu.Create(FrmMain);
 PopupView.AutoPopup:=False;

 Item_Chat:=TMenuItem.Create(PopupView);
 Item_Chat.Caption:='[-_-] Чат';
 Item_Chat.Checked:=False;
 Item_Chat.Tag:=0;
 Item_Chat.OnClick:=@OnPopupClickChat;
 PopupView.Items.Add(Item_Chat);

 Item_Story:=TMenuItem.Create(PopupView);
 Item_Story.Caption:='История событий';
 Item_Story.Checked:=False;
 Item_Story.Tag:=1;
 Item_Story.OnClick:=@OnPopupClickStory;
 PopupView.Items.Add(Item_Story);

 BtnView.OnClick:=@BtnToolPopupClick;
 BtnView.PopupMenu:=PopupView;

 BtnEnter:=TSpeedButton.Create(RightBar);

 BtnEnter.Caption:='Войти';
 BtnEnter.OnClick:=@OnBtnEnterClick;
 BtnEnter.AutoSize:=True;
 BtnEnter.Constraints.MinHeight:=32;
 BtnEnter.Height:=32;
 BtnEnter.Parent:=RightBar;

 BtnInfo:=TSpeedButton.Create(RightBar);
 BtnInfo.Caption:='[user]';
 BtnInfo.AutoSize:=True;
 BtnInfo.Constraints.MinHeight:=32;
 BtnInfo.Height:=32;
 BtnInfo.Visible:=False;
 BtnInfo.Parent:=RightBar;

 BtnClose:=TSpeedButton.Create(RightBar);
 BtnClose.OnClick:=@OnBtnCloseClick;
 BtnClose.Caption:='';
 BtnClose.AutoSize:=True;
 BtnClose.Constraints.MinHeight:=32;
 BtnClose.Height:=32;
 BtnClose.Visible:=False;
 BtnClose.Parent:=RightBar;

 KCLOSE:=TImageList.Create(FrmMain);
 KCLOSE.AddLazarusResource('KCLOSE');
 KCLOSE.AddLazarusResource('KLEFT');
 KCLOSE.AddLazarusResource('KRIGHT');

 KCLOSE_D:=TImageList.Create(FrmMain);
 KCLOSE_D.AddLazarusResource('KCLOSE_D');


 DIMAGE:=TImageList.Create(FrmMain);
 DIMAGE.Width:=28;
 DIMAGE.Height:=28;
 DIMAGE.AddLazarusResource('DGRAY');
 DIMAGE.AddLazarusResource('DGREEN');

 BtnEnter.Images:=DIMAGE;
 BtnEnter.ImageIndex:=0;
 BtnEnter.Layout:=blGlyphRight;
 BtnEnter.Spacing:=-1;
 BtnEnter.Margin:=0;
 BtnEnter.AutoSize:=false;
 BtnEnter.Width:=83;

 BtnInfo.Images:=DIMAGE;
 BtnInfo.ImageIndex:=1;
 BtnInfo.Layout:=blGlyphRight;
 BtnInfo.Spacing:=0;
 BtnInfo.Margin:=0;
 BtnInfo.AutoSize:=false;
 BtnInfo.Width:=83;

 BtnClose.LoadGlyphFromLazarusResource('DCLOSE');
 BtnClose.Layout:=blGlyphRight;
 BtnClose.AutoSize:=false;
 BtnClose.Width:=32;


 Pages:=TKCustomPageControl.Create(FrmMain);
 Pages.Parent:=FrmMain;

 Pages.Parent.DisableAlign;
 Pages.Anchors:=[akTop,akLeft,akRight,akBottom];
 Pages.AnchorSide[akTop].Side:=asrBottom;
 Pages.AnchorSide[akTop].Control:=frmPanel;
 Pages.AnchorSide[akBottom].Side:=asrBottom;
 Pages.AnchorSide[akBottom].Control:=Pages.Parent;
 Pages.AnchorSide[akLeft].Side:=asrLeft;
 Pages.AnchorSide[akLeft].Control:=Pages.Parent;
 Pages.AnchorSide[akRight].Side:=asrRight;
 Pages.AnchorSide[akRight].Control:=Pages.Parent;
 Pages.Parent.EnableAlign;

 Pages.HotTrack:=true;

 Pages.Images:=KCLOSE;
 Pages.DisabledImages:=KCLOSE_D;

 Pages.TabPanelOptions.CloseButtonIndex:=0;
 Pages.TabPanelOptions.LeftButtonIndex :=1;
 Pages.TabPanelOptions.RightButtonIndex:=2;

 Pages.Font.Color:=0;
 Pages.TabPanelOptions.Colors.HotTop        :=0;
 Pages.TabPanelOptions.Colors.HotBottom     :=0;
 Pages.TabPanelOptions.Colors.NormalTop     :=0;
 Pages.TabPanelOptions.Colors.NormalBottom  :=0;
 Pages.TabPanelOptions.Colors.NormalText    :=0;
 Pages.TabPanelOptions.Colors.SelectedTop   :=0;
 Pages.TabPanelOptions.Colors.SelectedBottom:=0;
 Pages.TabPanelOptions.Colors.SelectedText  :=0;

 Pages.OnTabCloseQuery:=@OnTabClose;

 GridChat:=TExtStringGrid.Create(FrmMain);
 GridChat.RowCount:=1;
 GridChat.Options:=GridChat.Options-[goRowSorting];
 GridChat.ScrollBars:=ssVertical;
 GridChat.AddColumn('time',' Время ');
 GridChat.AddColumn('mes',' Сообщение');

 EdtSend:=TEdit.Create(FrmMain);
 EdtSend.OnKeyDown:=@EdtSendKeyDown;

 GridStory:=TExtStringGrid.Create(FrmMain);
 GridStory.RowCount:=1;
 GridStory.Options:=GridChat.Options-[goRowSorting];
 GridStory.ScrollBars:=ssVertical;
 GridStory.AddColumn('datetime',' Дата, Время ');
 GridStory.AddColumn('user',' ЛЕВ ');
 GridStory.AddColumn('mes',' Сообщение');

 SetShowChat(True);
 SetShowStory(True);

 Pages.Pages[0].Show;

 //add_to_chat('Включение!');

 ///////////////////


 try
  DbcThread:=TDbcConnection.Create;
  DbcThread.Open(Config.ReadString('base','zurl',DefZURL));
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 FMem:=TMemoryStream.Create;
 FMem.LoadFromFile('create.sql');
 FMem.Position:=0;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.ExecuteScript(FMem);
 FDbcScript.Start;
 FDbcScript.Release;


 FInsertScript:=Default(TSQLScript);
 FMem.LoadFromFile('insert.sql');
 FMem.Position:=0;
 FInsertScript.Parse(FMem);


 FMem.LoadFromFile('list.sql');
 FMem.Position:=0;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@OnList);
 FDbcScript.ExecuteScript(FMem);
 FDbcScript.Start;
 FDbcScript.Release;

 FreeAndNil(FMem);

end;

end.




