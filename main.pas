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

  mtRandom,
  UAsyncQueue,
  ulog,
  u_irc,

  ujson,

  xml_parse,

  TaskManager,DbcEngine,
  ZDbcIntfs,
  ZDbcSqLite,
  DbcScript;

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
    procedure SetShowSubPanel(V:Boolean);
    procedure OnPopupClickSubPanel(Sender:TObject);
    procedure OnPopupClickChat(Sender:TObject);
    procedure OnPopupClickStory(Sender:TObject);
    procedure OnPopupClickAutoEnter(Sender:TObject);
    procedure OnPopupClickUseTray(Sender:TObject);
    procedure OnPopupClickSubParam(Sender:TObject);
    procedure OnPopupClickVipParam(Sender:TObject);
    procedure OnPopupClickParam(Sender:TObject);
    procedure OnPredClick(Sender:TObject);
    procedure OnTabClose(Sender:TObject;TabIndex:Integer;var CanClose:Boolean);
    procedure OnBtnEnterClick(Sender:TObject);
    procedure OnBtnCloseClick(Sender:TObject);
    Procedure OnList(Sender:TBaseTask);
    Procedure OnGetSubTime(Sender:TBaseTask);
    procedure OnPopupClose(Sender:TObject);
    procedure SetViewFlag(f:byte;b:Boolean);
    function  GetViewFlag(f:byte):Boolean;
    procedure FormCreate(Sender: TObject);
    procedure UpdateTextSubTime(db:Boolean);
    procedure SetDBParam(Const fname,fvalue:RawByteString);
    procedure BtnClickSubModeOn(Sender:TObject);
    procedure BtnClickSubModeOff(Sender:TObject);
    function  CanSetTimerSubMode(m:Boolean):Boolean;
    procedure SetTimerSubMode(m:Boolean);
    procedure SubModeTimerUpdate(Sender:TObject);
    procedure _inc_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
    procedure _dec_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
    procedure OnBtnIncSubModeClick(Sender:TObject);
    procedure OnBtnDecSubModeClick(Sender:TObject);
    procedure LoadXML;
  private

  public
   FCreateScript:TSQLScript;
   FListScript  :TSQLScript;
   FInsertScript:TSQLScript;

   FGetParamScript:TSQLScript;
   FSetParamScript:TSQLScript;

   login:RawByteString;

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
   Item_Subp,
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
procedure push_subs_only(m:Boolean);
procedure push_reward(const S:RawByteString);

function  _get_first_cmd(L:TStringList):RawByteString;

var
 Config:TINIFile;

 RCT:TMTRandomContext;

 chat:RawByteString;
 chat_id:RawByteString;

 view_mask:Byte;

 vip_rnd:record
  Enable:Boolean;
  title:RawByteString;
  cmd:TStringList;
  cmd2:TStringList;
  perc:Byte;
 end;

 sub_mod:record
  Enable:Boolean;
  inc_title:RawByteString;
  dec_title:RawByteString;
  cmd_on :TStringList;
  cmd_off:TStringList;
  cmd_inc:TStringList;
  cmd_dec:TStringList;
  inc_min:DWORD;
  max_inc:DWORD;
  max_dec:DWORD;
 end;

 SubModeTimer:TTimer;
 SubModeTick:Int64;
 SubModeTime:Int64;
 dbSubModeTime:Int64;

function fetch_random_no_more(Var Context:TMTRandomContext):Boolean;

implementation

uses
 ufrmpred,
 UFrmAbout,
 UFrmParam,
 UFrmVipParam,
 UFrmSubParam,
 Uloginf;

{$R *.lfm}

{ TFrmMain }

 //zeosdbo;zeosdbo\component;zeosdbo\core;zeosdbo\plain;zeosdbo\parsesql;zeosdbo\dbc

var
 KCLOSE,KCLOSE_D,DIMAGE:TImageList;

 GridChat:TExtStringGrid;
 GridStory:TExtStringGrid;
 Pages:TKCustomPageControl;

 PanelSub:TPanel;
 LabelSubMode:TLabel;
 TextSubTime:TEdit;

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
  Procedure OnParent2;
  Procedure OnParent3;
 end;

Procedure TQNode_login.OnParent;
begin
 FrmMain.SetLognBtn(True);
 FreeMem(@Self);
end;

Procedure TQNode_login.OnParent2;
begin
 FrmMain.SetTimerSubMode(False); ////////////
 FreeMem(@Self);
end;

Procedure TQNode_login.OnParent3;
begin
 FrmMain.SetTimerSubMode(True);
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

procedure push_subs_only(m:Boolean);
var
 P:PQNode_login;
begin
 P:=AllocMem(SizeOf(TQNode_login));
 Case m of
  False:P^.Parent:=@P^.OnParent2;
  True :P^.Parent:=@P^.OnParent3;
 end;
 SendMainQueue(P);
end;

type
 TPCharStream=class(TCustomMemoryStream)
  public
   constructor Create(P:PChar;len:SizeUint); virtual; overload;
   procedure   SetNew(P:PChar;len:SizeUint);
 end;

constructor TPCharStream.Create(P:PChar;len:SizeUint);
begin
 inherited Create;
 SetPointer(P,len);
end;

procedure TPCharStream.SetNew(P:PChar;len:SizeUint);
begin
 SetPosition(0);
 SetPointer(P,len);
end;

const
 permission='You don''t have permission to perform that action.';

//var
// last_cmd:RawByteString;

procedure push_irc_msg(const msg:RawByteString);
begin
 {if (msg<>'') and (msg[1]='/') then
 begin
  last_cmd:=msg;
 end;}
 reply_irc_msg(msg);
end;

function _get_first_cmd(L:TStringList):RawByteString;
begin
 Result:='';
 if (L<>nil) and (L.Count<>0) then
  Result:=L.Strings[0];
end;

procedure push_irc_list(L:TStringList;const Args:Array of const);
var
 i:SizeInt;
begin
 if (L=nil) or (L.Count=0) then Exit;
 For i:=0 to L.Count-1 do
 begin
  push_irc_msg(Format(L.Strings[i],Args));
 end;
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

         {
         add_reward(
           '{"type":"reward-redeemed","data":{"timestamp":"2020-07-08T18:38:23.'+
           '141491302Z","redemption":{"id":"62d7f76e-7a16-432d-94ce-541897f02fa3","u'+
           'ser":{"id":"84616392","login":"satan_rulezz","display_name":"Satan_R'+
           'ulezz"},"channel_id":"54742538","redeemed_at":"2020-07-08T18:38:23.01829'+
           '9023Z","reward":{"id":"9c25cd82-30e4-4e23-8dae-e3ae630b9bab","channel_id'+
           '":"54742538","title":"Подрубай сабмод","prompt":"Может передумаю и  '+
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
           'I5OTAyM1o="}}}');}

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

function fetch_user_display_name(msg2:TJson):RawByteString; inline;
begin
 Result:=msg2.Path['data.redemption.user.display_name'].AsStr;
end;

function fetch_user_login(msg2:TJson):RawByteString; inline;
begin
 Result:=msg2.Path['data.redemption.user.login'].AsStr;
end;

function fetch_msg(msg2:TJson):RawByteString; inline;
begin
 Result:=msg2.Path['data.redemption.reward.title'].AsStr+': '+
         msg2.Path['data.redemption.user_input'].AsStr;
end;

function fetch_random_no_more(Var Context:TMTRandomContext):Boolean;
Const
 max=3;
var
 Tmp:TMTRandomContext;
 i,p:Byte;
 n:Boolean;
begin
             //0..99         //70 0-69
 Result:=Random(Context,100)<vip_rnd.perc;
 if max<>0 then
  For p:=0 to 3 do
  begin
   Tmp:=Context;
   For i:=0 to max-1 do
   begin
    n:=Random(Tmp,100)<vip_rnd.perc; //pred
    if n<>Result then Exit;
   end;
   n:=Random(Context,100)<vip_rnd.perc; //step up
  end;
end;

procedure TFrmMain.add_reward(const S:RawByteString);
var
 DT:TDateTime;
 ms:TStream;
 msg2:TJson;
 msg,cmd,
 user,
 rs,
 reward_title:RawByteString;
 FDbcScript:TDbcStatementScript;

begin

 ms:=TPCharStream.Create(PAnsiChar(s),Length(s));

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
 user:=fetch_user_display_name(msg2);

 if msg2.Path['type'].AsStr='reward-redeemed' then
 begin

  rs:=s;

  reward_title:=Trim(msg2.Path['data.redemption.reward.title'].AsStr);
  cmd:='';

  if (vip_rnd.Enable) and (reward_title=vip_rnd.title) then //vip/ban
  begin

   if fetch_random_no_more(RCT) then
   begin
    push_irc_list(vip_rnd.cmd,[fetch_user_login(msg2)]);
    //add_to_chat('>'+login+': '+cmd);
    cmd:=Format(_get_first_cmd(vip_rnd.cmd),[fetch_user_login(msg2)]);
   end else
   begin
    push_irc_list(vip_rnd.cmd2,[fetch_user_login(msg2)]);
    //add_to_chat('>'+login+': '+cmd);
    cmd:=Format(_get_first_cmd(vip_rnd.cmd2),[fetch_user_login(msg2)]);
   end;

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
  end else
  if sub_mod.Enable then
  begin
   if reward_title=sub_mod.inc_title then //add sub mode
   begin
    _inc_SubModeTime(cmd,fetch_user_login(msg2));
   end else
   if reward_title=sub_mod.dec_title then //dec sub mode
   begin
    _dec_SubModeTime(cmd,fetch_user_login(msg2));
   end;
  end;

  if cmd<>'' then
  begin
   msg:=msg+' ('+cmd+')';
   msg2.Values['msg_cmd']:=cmd;
   ms:=TMemoryStream.Create;
   msg2.Dump(ms);
   SetLength(rs,ms.Size);
   ms.Position:=0;
   ms.Read(PAnsiChar(rs)^,Length(rs));
   FreeAndNil(ms);
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
 //t:RawByteString;
begin

 {
 if Trim(s)=permission then
 begin
  t:=last_cmd;
  aRow:=Pos(' ',t);
  if aRow<>0 then t:=Copy(t,1,aRow-1);

  Case LowerCase(t) of
   '/vip'           :ShowMessage('У вас нет прав для назначения ВИП!');
   '/timeout'       :ShowMessage('У вас нет прав для таймаута!');
   '/subscribers'   :ShowMessage('У вас нет прав для включения сабмода!');
   '/subscribersoff':ShowMessage('У вас нет прав для выключения сабмода!');
  end;

 end;}

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

 if GridChat.HandleAllocated then
 begin
  GridChat.ScrollModeVert:=smCell;
  GridChat.ScrollModeVert:=smSmooth;
  GridChat.ScrollBy(0,aRow);
 end;

end;

procedure TFrmMain.EdtSendKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
var
 msg:RawByteString;
begin
 if (Key=13) and BtnInfo.Visible then
 begin
  msg:=EdtSend.Text;
  EdtSend.Text:='';
  push_irc_msg(msg);
  //add_to_chat('>'+login+': '+msg);
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

 if GridStory.HandleAllocated then
 begin
  GridStory.ScrollModeVert:=smCell;
  GridStory.ScrollModeVert:=smSmooth;
  GridStory.ScrollBy(0,aRow);
 end;

end;

Procedure TFrmMain.OnList(Sender:TBaseTask);
Var
 datetime_f,user_f,mes_f:SizeInt;
 i,c:SizeInt;
 ResultSet:TZResultSet;
 ms:TPCharStream;
 msg2:TJson;
 msg,cmd:RawByteString;
begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;

 if ResultSet=nil then Exit;

 c:=0;
 if ResultSet.Last then
 begin
  c:=ResultSet.GetRow;
  GridStory.RowCount:=c+1;
 end;

 if c>0 then
 begin
  ms:=TPCharStream.Create(nil,0);
  datetime_f:=ResultSet.FindColumn('datetime');
  user_f    :=ResultSet.FindColumn('user');
  mes_f     :=ResultSet.FindColumn('mes');
  For i:=1 to c do
  begin
   ResultSet.MoveAbsolute(i);

   msg:=ResultSet.GetRawByteString(mes_f);

   ms.SetNew(PAnsiChar(msg),Length(msg));

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

 if GridStory.HandleAllocated then
 begin
  GridStory.ScrollModeVert:=smCell;
  GridStory.ScrollModeVert:=smSmooth;
  GridStory.ScrollBy(0,c);
  GridStory.Columns[0].Extent:=GridStory.Columns[0].MinExtent;
 end;

end;

Procedure TFrmMain.OnGetSubTime(Sender:TBaseTask);
Var
 ResultSet:TZResultSet;

begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;

 if ResultSet=nil then Exit;

 if ResultSet.First then
 begin
  SubModeTime:=ResultSet.GetInt(1);
  dbSubModeTime:=SubModeTime;
  UpdateTextSubTime(False);
 end;

end;

procedure TFrmMain.FormActivate(Sender: TObject);
var
 cx:SizeInt;
begin

 SetShowChat    (GetViewFlag(1));
 SetShowStory   (GetViewFlag(2));
 SetShowSubPanel(GetViewFlag(4));

 if GridChat.HandleAllocated then
 begin
  GridChat.AutoSizeCol(0,true);
  GridChat.Columns[0].MinExtent:=GridChat.Columns[0].Extent;
 end;

 if GridStory.HandleAllocated then
 begin
  cx:=Canvas.TextExtent(' 0000.00.00 00:00:000').cx;
  GridStory.Columns[0].Extent:=cx;
  GridStory.Columns[0].MinExtent:=cx;
  cx:=Canvas.TextExtent(' YAEBALETOT').cx;
  GridStory.Columns[1].Extent:=cx;
  GridStory.Columns[1].MinExtent:=cx;
 end;

 if Item_AutoEnter.Checked then
 begin
  try
   login:=Trim(Config.ReadString('base','login',''));
   reply_irc_Connect(
     login,
     Trim(Config.ReadString('base','oAuth','')),
     chat,chat_id
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
 SetViewFlag(1,V);
 Item_Chat.Checked:=V;
 Case V of
  True :begin
         if GridChat.Parent<>nil then Exit;

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

         if not GridChat.HandleAllocated then
         begin
          GridChat.AutoSizeCol(0,true);
          GridChat.Columns[0].MinExtent:=GridChat.Columns[0].Extent;
         end;

         Page.Show;

        end;
  False:begin
         Page:=TKTabSheet(GridChat.Parent);
         if Page<>nil then
         begin
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
end;

procedure TFrmMain.SetShowStory(V:Boolean);
Var
 cx:Integer;
 Page:TKTabSheet;
begin
 SetViewFlag(2,V);
 Item_Story.Checked:=V;
 Case V of
  True :begin
         if GridStory.Parent<>nil then Exit;

         Page:=Pages.AddPage(Pages);
         Page.Caption:='История наград';
         Page.Tag:=1;
         Page.ImageIndex:=-1;
         Page.Show;
         GridStory.Parent:=Page;
         GridStory.AnchorAsAlign(alClient,1);

         if not GridStory.HandleAllocated then
         begin
          cx:=Canvas.TextExtent(' 0000.00.00 00:00:000').cx;
          GridStory.Columns[0].Extent:=cx;
          GridStory.Columns[0].MinExtent:=cx;
          cx:=Canvas.TextExtent(' YAEBALETOT').cx;
          GridStory.Columns[1].Extent:=cx;
          GridStory.Columns[1].MinExtent:=cx;
         end;

        end;
  False:begin
         Page:=TKTabSheet(GridStory.Parent);
         GridStory.Parent:=nil;
         if Page<>nil then
          Application.ReleaseComponent(Page);
        end;
 end;
end;

procedure TFrmMain.SetShowSubPanel(V:Boolean);
Var
 Page:TKTabSheet;
begin
 SetViewFlag(4,V);
 Item_Subp.Checked:=V;
 Case V of
  True :begin
         if PanelSub.Parent<>nil then Exit;

         Page:=Pages.AddPage(Pages);
         Page.Caption:='Сабмод';
         Page.Tag:=2;
         Page.ImageIndex:=-1;
         Page.Show;
         PanelSub.Parent:=Page;
        end;
  False:begin
         Page:=TKTabSheet(PanelSub.Parent);
         PanelSub.Parent:=nil;
         if Page<>nil then
          Application.ReleaseComponent(Page);
        end;
 end;
end;

procedure TFrmMain.OnPopupClickSubPanel(Sender:TObject);
begin
 SetShowSubPanel(not Item_Subp.Checked);
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

procedure TFrmMain.OnPopupClickSubParam(Sender:TObject);
begin
 try
  FrmSubParam.CBSubEnable.Checked:=sub_mod.Enable;
  FrmSubParam.EdtTitleSubInc.Text:=sub_mod.inc_title;
  FrmSubParam.EdtTitleSubDec.Text:=sub_mod.dec_title;
  FrmSubParam.EdtSubInc.Text     :=IntToStr(sub_mod.inc_min);
  FrmSubParam.EdtSub_max_inc.Text:=IntToStr(sub_mod.max_inc);
  FrmSubParam.EdtSub_max_dec.Text:=IntToStr(sub_mod.max_dec);

 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if FrmSubParam.ShowModal=1 then
 begin
  sub_mod.Enable   :=FrmSubParam.CBSubEnable.Checked;
  sub_mod.inc_title:=FrmSubParam.EdtTitleSubInc.Text;
  sub_mod.dec_title:=FrmSubParam.EdtTitleSubDec.Text;
  sub_mod.inc_min  :=StrToIntDef(FrmSubParam.EdtSubInc.Text,1);
  sub_mod.max_inc  :=StrToIntDef(FrmSubParam.EdtSub_max_inc.Text,0);
  sub_mod.max_dec  :=StrToIntDef(FrmSubParam.EdtSub_max_dec.Text,0);

  try
   case sub_mod.Enable of
    True :Config.WriteString('sub_mod','enable','1');
    False:Config.WriteString('sub_mod','enable','0');
   end;
   Config.WriteString('sub_mod','inc_title',sub_mod.inc_title);
   Config.WriteString('sub_mod','dec_title',sub_mod.dec_title);
   Config.WriteString('sub_mod','inc_min'  ,IntToStr(sub_mod.inc_min));
   Config.WriteString('sub_mod','max_inc'  ,IntToStr(sub_mod.max_inc));
   Config.WriteString('sub_mod','max_dec'  ,IntToStr(sub_mod.max_dec));
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmMain.OnPopupClickVipParam(Sender:TObject);
begin
 try
  FrmVipParam.CBVipEnable.Checked:=vip_rnd.Enable;
  FrmVipParam.EdtTitle.Text      :=vip_rnd.title;
  FrmVipParam.EdtPercent.Text    :=IntToStr(vip_rnd.perc);

 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if FrmVipParam.ShowModal=1 then
 begin
  vip_rnd.Enable:=FrmVipParam.CBVipEnable.Checked;
  vip_rnd.title :=Trim(FrmVipParam.EdtTitle.Text);
  vip_rnd.perc  :=StrToQWORDDef(FrmVipParam.EdtPercent.Text,70);
  if vip_rnd.perc>100 then vip_rnd.perc:=100;

  try
   case vip_rnd.Enable of
    True :Config.WriteString('vip' ,'enable','1');
    False:Config.WriteString('vip' ,'enable','0');
   end;
   Config.WriteString('vip' ,'title'   ,vip_rnd.title);
   Config.WriteString('vip' ,'msg_perc',IntToStr(vip_rnd.perc));
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmMain.OnPopupClickParam(Sender:TObject);
begin
 try
  FrmParam.EdtLogin.Text   :=Config.ReadString('base','login','');
  FrmParam.EdtPassword.Text:=Config.ReadString('base','oAuth','');
  FrmParam.EdtChat.Text    :=chat;
  FrmParam.EdtChatID.Text  :=chat_id;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if FrmParam.ShowModal=1 then
 begin
  chat   :=FrmParam.EdtChat.Text;
  chat_id:=FrmParam.EdtChatID.Text;

  try
   Config.WriteString('base','login'   ,FrmParam.EdtLogin.Text);
   Config.WriteString('base','oAuth'   ,FrmParam.EdtPassword.Text);
   Config.WriteString('base','chat'    ,chat);
   Config.WriteString('base','chat_id' ,chat_id);
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmMain.OnPredClick(Sender:TObject);
begin
 FrmPred.CR:=RCT;
 FrmPred.fetch_pred;
 FrmPred.Show;
end;

procedure TFrmMain.OnTabClose(Sender:TObject;TabIndex:Integer;var CanClose:Boolean);
begin
 CanClose:=False;
 Case Pages.Pages[TabIndex].Tag of
  0:SetShowChat(False);
  1:SetShowStory(False);
  2:SetShowSubPanel(False);
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
     chat,
     chat_id
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
 SetTimerSubMode(false);
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

procedure TFrmMain.SetViewFlag(f:byte;b:Boolean);
begin
 if b<>GetViewFlag(f) then
 begin
  Case b of
   True :view_mask:=view_mask or f;
   False:view_mask:=view_mask and (not f);
  end;
  try
   Config.WriteString('view','mask',IntToStr(view_mask));
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

function  TFrmMain.GetViewFlag(f:byte):Boolean;
begin
 Result:=view_mask and f<>0;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
Var
 Btn:TButton;
 Item:TMenuItem;
 D:RawByteString;
 FDbcScript:TDbcStatementScript;
begin

 RCT:=Default(TMTRandomContext);
 RandomInit(RCT);

 LoadXML;

 D:=GetLocalIni;

 IF FileExists(D) then
 begin
  try
   Config:=TINIFile.Create(D);

   //read

   chat   :=Trim(Config.ReadString('base','chat'   ,chat));
   chat_id:=Trim(Config.ReadString('base','chat_id',chat_id));

   vip_rnd.Enable:=Trim(Config.ReadString('vip','enable','0'))='1';
   vip_rnd.title :=Trim(Config.ReadString('vip','title',vip_rnd.title));
   vip_rnd.perc  :=StrToQWORDDef(Config.ReadString('vip','msg_perc',IntToStr(vip_rnd.perc)),70);
   if vip_rnd.perc>100 then vip_rnd.perc:=100;

   view_mask:=StrToDWORDDef(Config.ReadString('view','mask','1'),1);

   sub_mod.Enable   :=Trim(Config.ReadString('sub_mod','enable','0'))='1';
   sub_mod.inc_title:=Trim(Config.ReadString('sub_mod','inc_title',sub_mod.inc_title));
   sub_mod.dec_title:=Trim(Config.ReadString('sub_mod','dec_title',sub_mod.dec_title));
   sub_mod.inc_min  :=StrToDWORDDef(Config.ReadString('sub_mod','inc_min',IntToStr(sub_mod.inc_min)),30);
   sub_mod.max_inc  :=StrToDWORDDef(Config.ReadString('sub_mod','max_inc',IntToStr(sub_mod.max_inc)),0);
   sub_mod.max_dec  :=StrToDWORDDef(Config.ReadString('sub_mod','max_dec',IntToStr(sub_mod.max_dec)),0);

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end else
 begin
  try
   //write

   vip_rnd.Enable:=True;
   sub_mod.Enable:=False;
   view_mask:=1;

   Config:=TINIFile.Create(D);
   Config.WriteString('base','zurl'   ,DefZURL);
   Config.WriteString('base','login'  ,'');
   Config.WriteString('base','oAuth'  ,'');
   Config.WriteString('base','chat'   ,chat);
   Config.WriteString('base','chat_id',chat_id);

   Config.WriteString('vip' ,'enable'  ,'1');
   Config.WriteString('vip' ,'title'   ,vip_rnd.title);
   Config.WriteString('vip' ,'msg_perc',IntToStr(vip_rnd.perc));

   Config.WriteString('view','autologin','0');
   Config.WriteString('view','systray'  ,'1');
   Config.WriteString('view','mask'     ,IntToStr(view_mask));

   vip_rnd.title:=Trim(Config.ReadString('vip' ,'title',vip_rnd.title));
   vip_rnd.perc :=StrToQWORDDef(Config.ReadString('vip' ,'msg_perc',''),70);
   if vip_rnd.perc>100 then vip_rnd.perc:=100;

   Config.WriteString('sub_mod','enable','0');
   Config.WriteString('sub_mod','inc_title',sub_mod.inc_title);
   Config.WriteString('sub_mod','dec_title',sub_mod.dec_title);
   Config.WriteString('sub_mod','inc_min'  ,IntToStr(sub_mod.inc_min));
   Config.WriteString('sub_mod','max_inc'  ,IntToStr(sub_mod.max_inc));
   Config.WriteString('sub_mod','max_dec'  ,IntToStr(sub_mod.max_dec));

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;

 SystemTray.Icon:=Application.Icon;

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
 Item.Caption:='Общие';
 Item.OnClick:=@OnPopupClickParam;
 PopupCfg.Items.Add(Item);

 //vip param
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='VIP или БАН';
 Item.OnClick:=@OnPopupClickVipParam;
 PopupCfg.Items.Add(Item);

 //sub param
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Саб мод';
 Item.OnClick:=@OnPopupClickSubParam;
 PopupCfg.Items.Add(Item);

 //------
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='-';
 PopupCfg.Items.Add(Item);


 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Оракул';
 Item.OnClick:=@OnPredClick;
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
  Item_UseTray  .Checked:=Trim(Config.ReadString('view','systray'  ,'1'))='1';
  Item_AutoEnter.Checked:=Trim(Config.ReadString('view','autologin','0'))='1';
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

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
 Item_Story.Caption:='История наград';
 Item_Story.Checked:=False;
 Item_Story.Tag:=1;
 Item_Story.OnClick:=@OnPopupClickStory;
 PopupView.Items.Add(Item_Story);

 Item_Subp:=TMenuItem.Create(PopupView);
 Item_Subp.Caption:='Сабмод';
 Item_Subp.Checked:=False;
 Item_Subp.Tag:=2;
 Item_Subp.OnClick:=@OnPopupClickSubPanel;
 PopupView.Items.Add(Item_Subp);

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

 PanelSub:=TPanel.Create(FrmMain);
 PanelSub.Align:=alClient;
 PanelSub.BevelInner:=bvNone;
 PanelSub.BevelOuter:=bvNone;

 LabelSubMode:=TLabel.Create(PanelSub);
 LabelSubMode.AutoSize:=True;
 LabelSubMode.Left:=10;
 LabelSubMode.Top:=10;
 LabelSubMode.Font.Color:=0;
 LabelSubMode.Font.Size:=12;
 LabelSubMode.Caption:='Сабмод выключен';
 LabelSubMode.Parent:=PanelSub;

 TextSubTime:=TEdit.Create(PanelSub);

 TextSubTime.Anchors:=[akTop,akLeft,akRight];
 TextSubTime.AutoSize:=True;
 TextSubTime.Left:=10;

 TextSubTime.Width :=PanelSub.ClientWidth-20;
 TextSubTime.Height:=PanelSub.ClientHeight-20;
 TextSubTime.Modified:=False;
 TextSubTime.Font.Size:=20;
 TextSubTime.Parent:=PanelSub;

 TextSubTime.AnchorSide[akTop].Side:=asrBottom;
 TextSubTime.AnchorSide[akTop].Control:=LabelSubMode;
 TextSubTime.BorderSpacing.Top:=5;

 UpdateTextSubTime(False);

 Btn:=TButton.Create(PanelSub);
 Btn.OnClick:=@BtnClickSubModeOn;
 Btn.AutoSize:=True;
 Btn.Caption:='Включить';
 Btn.Left:=10;
 Btn.AnchorSide[akTop].Side:=asrBottom;
 Btn.AnchorSide[akTop].Control:=TextSubTime;
 Btn.BorderSpacing.Top:=5;
 Btn.Parent:=PanelSub;

 Pointer(Item):=Btn;

 Btn:=TButton.Create(PanelSub);
 Btn.OnClick:=@OnBtnIncSubModeClick;
 Btn.AutoSize:=True;
 Btn.Caption:='[+] время';
 Btn.AnchorSide[akTop].Side:=asrBottom;
 Btn.AnchorSide[akTop].Control:=TextSubTime;
 Btn.AnchorSide[akLeft].Side:=asrRight;
 Btn.AnchorSide[akLeft].Control:=TControl(Item);
 Btn.BorderSpacing.Top:=5;
 Btn.BorderSpacing.Left:=5;
 Btn.Parent:=PanelSub;

 Btn:=TButton.Create(PanelSub);
 Btn.OnClick:=@BtnClickSubModeOff;
 Btn.AutoSize:=True;
 Btn.Anchors:=[akTop,akRight];
 Btn.Caption:='Выключить';
 Btn.Left:=PanelSub.ClientWidth-Btn.Width-10;
 Btn.AnchorSide[akTop].Side:=asrBottom;
 Btn.AnchorSide[akTop].Control:=TextSubTime;
 Btn.BorderSpacing.Top:=5;
 Btn.Parent:=PanelSub;

 Pointer(Item):=Btn;

 Btn:=TButton.Create(PanelSub);
 Btn.OnClick:=@OnBtnDecSubModeClick;
 Btn.AutoSize:=True;
 Btn.Caption:='[-] время';
 Btn.Anchors:=[akTop,akRight];
 Btn.AnchorSide[akTop].Side:=asrBottom;
 Btn.AnchorSide[akTop].Control:=TextSubTime;
 Btn.AnchorSide[akRight].Side:=asrLeft;
 Btn.AnchorSide[akRight].Control:=TControl(Item);
 Btn.BorderSpacing.Top:=5;
 Btn.BorderSpacing.Right:=5;
 Btn.Parent:=PanelSub;

 //SetShowChat    (True);
 //SetShowStory   (True);
 //SetShowSubPanel(True);

 //Pages.Pages[0].Show;

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

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FCreateScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Start;
 FDbcScript.Release;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@OnList);
 FDbcScript.SetSctipt(FListScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Start;
 FDbcScript.Release;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@OnGetSubTime);
 FDbcScript.SetSctipt(FGetParamScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('name','SubTime');
 FDbcScript.Start;
 FDbcScript.Release;

end;

function unixTime2String(T:Int64):RawByteString;
var
 Hr:Int64;
 Mn,Sc:Byte;
begin
 Sc:=(abs(T) mod 60);
 Mn:=(abs(T) div 60) mod 60;
 Hr:=(abs(T) div (60*60)) mod 24;

 if T>=0 then
 begin
  Result:='+'+AddChar('0',IntToStr(Abs(Hr)),2)
         +':'+AddChar('0',IntToStr(Mn),2)
         +':'+AddChar('0',IntToStr(Sc),2);
 end else
 begin
  Result:='-'+AddChar('0',IntToStr(Abs(Hr)),2)
         +':'+AddChar('0',IntToStr(Mn),2)
         +':'+AddChar('0',IntToStr(Sc),2);
 end;
end;

procedure TFrmMain.UpdateTextSubTime(db:Boolean);
var
 S,L:Integer;
begin
 if db then
 begin
  SetDBParam('SubTime',IntToStr(SubModeTime));
  dbSubModeTime:=SubModeTime;
 end;

 TextSubTime.Text:=unixTime2String(SubModeTime);

 TextSubTime.SelStart :=S;
 TextSubTime.SelLength:=L;
end;

procedure TFrmMain.SetDBParam(Const fname,fvalue:RawByteString);
var
 FDbcScript:TDbcStatementScript;
begin
 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FSetParamScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('name',fname);
 FDbcScript.Params.SetRawByteString('value',fvalue);
 FDbcScript.Start;
 FDbcScript.Release;
end;

function TFrmMain.CanSetTimerSubMode(m:Boolean):Boolean;
begin
 if not BtnInfo.Visible then Exit;
 Case m of
  True :
  begin
   if (SubModeTimer=nil) then
   begin
    Result:=True;
   end else
   begin
    Result:=not SubModeTimer.Enabled;
   end;
  end;
  False:
  begin
   if (SubModeTimer=nil) then
   begin
    Result:=False;
   end else
   begin
    Result:=SubModeTimer.Enabled;
   end;
  end;
 end;
end;

procedure TFrmMain.SetTimerSubMode(m:Boolean);
begin
 Case m of
  True :
  begin
   if (SubModeTimer=nil) then
   begin
    SubModeTimer:=TTimer.Create(Self);
    SubModeTimer.Interval:=400;
    SubModeTimer.OnTimer:=@SubModeTimerUpdate;
   end;
   LabelSubMode.Font.Color:=$FF00;
   LabelSubMode.Caption:='Сабмод включён';
   SubModeTick:=GetTickCount64;
   SubModeTimer.Enabled:=m;
  end;
  False:
  begin
   if (SubModeTimer<>nil) then
   begin
    SubModeTimer.Enabled:=m;
   end;
   LabelSubMode.Font.Color:=0;
   LabelSubMode.Caption:='Сабмод выключен';
  end;
 end;
 UpdateTextSubTime(True);
end;

procedure TFrmMain.BtnClickSubModeOn(Sender:TObject);
begin
 //submode send on
 if CanSetTimerSubMode(true) then
 begin
  push_irc_list(sub_mod.cmd_on,[login,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
 end;
end;

procedure TFrmMain.BtnClickSubModeOff(Sender:TObject);
begin
 //submode send off
 if CanSetTimerSubMode(false) then
 begin
  push_irc_list(sub_mod.cmd_off,[login,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
 end;
end;

procedure TFrmMain.SubModeTimerUpdate(Sender:TObject);
var
 s:Int64;
begin
 s:=(GetTickCount64-SubModeTick) div 1000;
 SubModeTick:=SubModeTick+s*1000;
 if SubModeTime=0 then
 begin
  //wtf? submode without timer
 end else
 if (s<>0) then
 begin
  if (SubModeTime>0) then
  begin
   SubModeTime:=SubModeTime-s;
   if SubModeTime<=0 then
   begin
    SubModeTime:=0;
    //submode send off
    if CanSetTimerSubMode(false) then
    begin
     push_irc_list(sub_mod.cmd_off,[login,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
    end;
   end;
   UpdateTextSubTime(abs(dbSubModeTime-SubModeTime)>=10);
  end else
  begin
   //reverse submode time
  end;
 end;
end;

procedure TFrmMain._inc_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
begin
 SubModeTime:=SubModeTime+sub_mod.inc_min*60;
 dbSubModeTime:=SubModeTime;
 if SubModeTime>0 then
 begin
  if (sub_mod.max_inc<>0) and (SubModeTime>sub_mod.max_inc*60*60) then
  begin
   SubModeTime:=sub_mod.max_inc*60*60;
  end;
  if CanSetTimerSubMode(True) then
  begin
   push_irc_list(sub_mod.cmd_on,[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
   cmd:=Format(_get_first_cmd(sub_mod.cmd_on),[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
  end;
 end;
 UpdateTextSubTime(True);
end;

procedure TFrmMain._dec_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
begin
 SubModeTime:=SubModeTime-sub_mod.inc_min*60;
 dbSubModeTime:=SubModeTime;
 if SubModeTime<=0 then
 begin
  if (sub_mod.max_dec=0) then
  begin
   SubModeTime:=0;
  end else
  if (abs(SubModeTime)>sub_mod.max_dec*60*60) then
  begin
   SubModeTime:=-sub_mod.max_dec*60*60;
  end;
  if CanSetTimerSubMode(False) then
  begin
   push_irc_list(sub_mod.cmd_off,[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
   cmd:=Format(_get_first_cmd(sub_mod.cmd_off),[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
  end;
 end;
 UpdateTextSubTime(True);
end;

procedure TFrmMain.OnBtnIncSubModeClick(Sender:TObject);
var
 cmd:RawByteString;
begin
 _inc_SubModeTime(cmd,login);
end;

procedure TFrmMain.OnBtnDecSubModeClick(Sender:TObject);
var
 cmd:RawByteString;
begin
 _dec_SubModeTime(cmd,login);
end;

Const
 XmlName='data.xml';

function GetLocalXml:RawByteString;
begin
 Result:=ParamStr(0);
 Result:=ExtractFileDir(Result);
 Result:=IncludeTrailingPathDelimiter(Result);
 Result:=Result+XmlName;
end;

type
 TXmlNodeReader=class(TNodeReader)
  procedure onDataEvent(Sender:TObject);
 end;

 TRoot_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TLoadSQL_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadStr_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadList_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadPerc_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadDWORD_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TOpenVip_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TOpenSub_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TOpenSQL_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

class procedure TRoot_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'sql':
  begin
   Node.Push(TOpenSQL_Func,Node.CData);
  end;
  'vip_rnd':
  begin
   Node.Push(TOpenVip_Func,Node.CData);
  end;
  'sub_mod':
  begin
   Node.Push(TOpenSub_Func,Node.CData);
  end;
  'chat':
  begin
   Node.Push(TLoadStr_Func,@chat);
  end;
  'chat_id':
  begin
   Node.Push(TLoadStr_Func,@chat_id);
  end;
 end;
end;

class procedure TLoadSQL_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
Var
 FMem:TPCharStream;
begin
 Case Name of
  '':
  begin
   FMem:=TPCharStream.Create(PChar(Value),Length(Value));
   PSQLScript(Node.CData)^:=Default(TSQLScript);
   PSQLScript(Node.CData)^.Parse(FMem);
   FMem.Free;
  end;
 end;
end;

class procedure TLoadStr_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   PRawByteString(Node.CData)^:=Value;
  end;
 end;
end;

Procedure LoadStringList(Var S:TStringList;Const Value:RawByteString);
var
 M:TPCharStream;
 i:SizeInt;
begin
 if S<>nil then
 begin
  S.Clear;
 end else
 begin
  S:=TStringList.Create;
 end;
 M:=TPCharStream.Create(PChar(Value),Length(Value));
 S.LoadFromStream(M,True);
 M.Free;

 if S.Count<>0 then
 For i:=0 to S.Count-1 do
 begin
  S.Strings[i]:=Trim(S.Strings[i]);
 end;
end;

class procedure TLoadList_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
type
 PStringList=^TStringList;
begin
 Case Name of
  '':
  begin
   LoadStringList(PStringList(Node.CData)^,Value);
  end;
 end;
end;

class procedure TLoadPerc_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   vip_rnd.perc :=StrToQWORDDef(Value,70);
   if vip_rnd.perc>100 then vip_rnd.perc:=100;
  end;
 end;
end;

class procedure TLoadDWORD_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   PDWORD(Node.CData)^:=StrToDWORDDef(Value,0);
  end;
 end;
end;

class procedure TOpenVip_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'title':
   begin
    Node.Push(TLoadStr_Func ,@vip_rnd.title);
   end;
  'msg_cmd':
   begin
    Node.Push(TLoadList_Func,@vip_rnd.cmd);
   end;
  'msg_cmd2':
   begin
    Node.Push(TLoadList_Func,@vip_rnd.cmd2);
   end;
  'perc':
   begin
    Node.Push(TLoadPerc_Func,nil);
   end;
 end;
end;

class procedure TOpenSub_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'inc_title':
   begin
    Node.Push(TLoadStr_Func  ,@sub_mod.inc_title);
   end;
  'dec_title':
   begin
    Node.Push(TLoadStr_Func  ,@sub_mod.dec_title);
   end;
  'cmd_on':
   begin
    Node.Push(TLoadList_Func ,@sub_mod.cmd_on);
   end;
  'cmd_off':
   begin
    Node.Push(TLoadList_Func ,@sub_mod.cmd_off);
   end;
  'cmd_inc':
   begin
    Node.Push(TLoadList_Func ,@sub_mod.cmd_inc);
   end;
  'cmd_dec':
   begin
    Node.Push(TLoadList_Func ,@sub_mod.cmd_dec);
   end;
  'inc_min':
   begin
    Node.Push(TLoadDWORD_Func,@sub_mod.inc_min);
   end;
  'max_inc':
   begin
    Node.Push(TLoadDWORD_Func,@sub_mod.max_inc);
   end;
  'max_dec':
   begin
    Node.Push(TLoadDWORD_Func,@sub_mod.max_dec);
   end;
 end;
end;


class procedure TOpenSQL_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'create':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FCreateScript);
   end;
  'list':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FListScript);
   end;
  'insert':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FInsertScript);
   end;
  'get_param':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FGetParamScript);
   end;
  'set_param':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FSetParamScript);
   end;
 end;
end;

procedure TXmlNodeReader.onDataEvent(Sender:TObject);
begin
 With TXmlTextReader(Sender) do
 begin
  DoSet(nodeType,Name,Value);
 end;
end;

procedure TFrmMain.LoadXML;
Var
 F:RawByteString;
 M:TMemoryStream;
 XmlReader:TXmlTextReader;
 NodeReader:TXmlNodeReader;
begin
 F:=GetLocalXml;
 if not FileExists(F) then
 begin
  ShowMessage('Файл '+XmlName+' не найден!');
  Halt;
 end;
 M:=TMemoryStream.Create;
 M.LoadFromFile(F);
 M.Position:=0;

 XmlReader:=TXmlTextReader.Create;
 NodeReader:=TXmlNodeReader.Create;

 NodeReader.Push(TRoot_Func,FrmMain);

 XmlReader.SrcCP:=CP_UTF8;
 XmlReader.Event:=@NodeReader.onDataEvent;
 XmlReader.Parse(M.Memory,M.Size);

 XmlReader.Free;
 NodeReader.Free;
end;

end.




