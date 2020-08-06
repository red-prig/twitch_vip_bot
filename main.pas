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
  LResources,Menus,LCLType,Clipbrd, ExtCtrls, Buttons, MaskEdit,
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

  TRoomState=(
   Rs_emote_only,
   Rs_followers_only,
   Rs_r9k,
   Rs_rituals,
   Rs_subs_only,
   Rs_room_tag
  );
  TRoomStates=Set of TRoomState;

  TPrivMsgState=(
   pm_self,
   pm_admin,
   pm_broadcaster,
   pm_global_mod,
   pm_moderator,
   pm_vip
  );
  TPrivMsgStates=Set of TPrivMsgState;
  TPrivMsgCfg=record
   user_id:QWORD;
   sub_gifter:DWORD;
   subscriber_m:DWORD;
   subscriber_s:DWORD;
   Color:DWORD;
   PS:TPrivMsgStates;
  end;

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
    procedure SetRoomStates(RS:TRoomStates);
    procedure parse_vips(msg:RawByteString);
    procedure add_to_notice(const id,msg:RawByteString);
    procedure add_to_chat(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
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
    procedure SetShowVipsPanel(V:Boolean);
    procedure OnPopupClickVipsPanel(Sender:TObject);
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
    Procedure OnListStory(Sender:TBaseTask);
    Procedure OnListVips(Sender:TBaseTask);
    Procedure OnGetSubTime(Sender:TBaseTask);
    procedure OnPopupClose(Sender:TObject);
    procedure SetViewFlag(f:byte;b:Boolean);
    function  GetViewFlag(f:byte):Boolean;
    procedure VipsEditorCreate(Sender:TObject;ACol,ARow:Integer;var AEditor:TWinControl);
    function  FindVipUser(Const FValue:RawByteString):Integer;
    function  DoVipInsert(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
    function  DoVipUpdate(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
    function  DoVipDelete(aRow:Integer):Boolean;
    procedure DeleteVip(aRow:Integer);
    Function  DoVipAddNew(DT:TDateTime;Const FUser:RawByteString):Boolean;
    procedure OnBtnDeleteVipClick(Sender:TObject);
    procedure OnBtnInsertVipClick(Sender:TObject);
    procedure OnBtnUnVipClick(Sender:TObject);
    procedure OnBtnUpdateVipClick(Sender:TObject);
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
   FCreateScript     :TSQLScript;
   FListStoryScript  :TSQLScript;
   FInsertStoryScript:TSQLScript;

   FListVipsScript  :TSQLScript;
   FAddVipsScript   :TSQLScript;
   FInsertVipsScript:TSQLScript;
   FUpdateVipsScript:TSQLScript;
   FDeleteVipsScript:TSQLScript;

   FGetParamScript:TSQLScript;
   FSetParamScript:TSQLScript;

   login:RawByteString;

   GridChat:TExtStringGrid;
   GridStory:TExtStringGrid;
   Pages:TKCustomPageControl;

   frmPanel:TPanel;
   LeftBar,RightBar:TToolbar;
   BtnCfg :TButton;
   BtnView:TButton;

   BtnEnter:TSpeedButton;
   BtnInfo :TSpeedButton;
   BtnClose:TSpeedButton;

   PopupView:TPopupMenu;
   PopupCfg :TPopupMenu;

   Item_Chat,
   Item_Story,
   Item_Subp,
   Item_Vips,
   Item_AutoEnter,
   Item_UseTray:TMenuItem;

   EdtSend:TEdit;

   PanelSub:TPanel;
   LabelSubMode:TLabel;
   TextSubTime:TEdit;

   PanelVips:TPanel;
   GridVips:TDBStringGrid;

   wait_vip_update:Boolean;

  end;

var
 pool:Tevpool;
 pool_config:Tevpool_config;

 FrmMain: TFrmMain;

procedure push_notice(const id,msg:RawByteString);
procedure push_chat(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
procedure push_login;
procedure push_room_states(RS:TRoomStates);
procedure push_reward(const S:RawByteString);

function  _get_first_cmd(L:TStringList):RawByteString;

var
 Config:TINIFile;

 RCT:TMTRandomContext;

 chat:RawByteString;

 view_mask:Byte;

 vip_rnd:record
  Enable:Boolean;
  title:RawByteString;
  cmd:TStringList;
  cmd2:TStringList;
  already_vip:TStringList;
  unvip_cmd:RawByteString;
  vip_list_cmd:RawByteString;
  perc:Byte;
  days:DWORD;
 end;

 sub_mod:record
  Enable:Boolean;
  _label:record
   name,_on,off:RawByteString;
  end;
  subtime_cmd:RawByteString;
  subtime_kd:DWORD;
  room_tag:RawByteString;
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

 DbcThread:TDbcConnection;

type
 PQNode_notice=^TQNode_notice;
 TQNode_notice=object(UAsyncQueue.TQNode)
  Fmsg:PAnsiChar;
  data:record end;
  Procedure OnParent;
 end;

Procedure TQNode_notice.OnParent;
var
 id,msg:RawByteString;
begin
 SetString(id ,@data,StrLen(@data));
 SetString(msg,Fmsg ,StrLen(Fmsg));
 FreeMem(@Self);
 FrmMain.add_to_notice(id,msg);
end;

procedure push_notice(const id,msg:RawByteString);
var
 P:PQNode_notice;
begin
 P:=AllocMem(SizeOf(TQNode_notice)+Length(id)+Length(msg)+2);
 P^.Parent:=@P^.OnParent;
 P^.Fmsg:=@PAnsiChar(@P^.data)[Length(id)+1];
 Move(PAnsiChar(id)^ ,P^.data ,Length(id)+1);
 Move(PAnsiChar(msg)^,P^.Fmsg^,Length(msg)+1);
 SendMainQueue(P);
end;

type
 PQNode_chat=^TQNode_chat;
 TQNode_chat=object(UAsyncQueue.TQNode)
  PC:TPrivMsgCfg;
  Fdnm:PAnsiChar;
  Fmsg:PAnsiChar;
  data:record end;
  Procedure OnParent;
 end;

procedure push_chat(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
var
 P:PQNode_chat;
begin
 P:=AllocMem(SizeOf(TQNode_chat)+Length(user)+Length(display_name)+Length(msg)+3);
 P^.Parent:=@P^.OnParent;
 P^.PC:=PC;
 P^.Fdnm:=@PAnsiChar(@P^.data)[Length(user)+1];
 P^.Fmsg:=@P^.Fdnm[Length(display_name)+1];

 Move(PAnsiChar(user)^        ,P^.data ,Length(user)+1);
 Move(PAnsiChar(display_name)^,P^.Fdnm^,Length(display_name)+1);
 Move(PAnsiChar(msg)^         ,P^.Fmsg^,Length(msg)+1);

 SendMainQueue(P);
end;

Procedure TQNode_chat.OnParent;
var
 user,display_name,msg:RawByteString;
begin
 SetString(user        ,@data,StrLen(@data));
 SetString(display_name,Fdnm ,StrLen(Fdnm));
 SetString(msg         ,Fmsg ,StrLen(Fmsg));
 FreeMem(@Self);
 FrmMain.add_to_chat(PC,user,display_name,msg);
end;

type
 PQNode_reward=^TQNode_reward;
 TQNode_reward=object(UAsyncQueue.TQNode)
  len:SizeUint;
  data:record end;
  Procedure OnParent;
 end;

Procedure TQNode_reward.OnParent;
var
 S:RawByteString;
begin
 SetString(S,@data,len);
 FreeMem(@Self);

 try
  FrmMain.add_reward(S);
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;
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

type
 PQNode_room=^TQNode_room;
 TQNode_room=object(UAsyncQueue.TQNode)
  RS:TRoomStates;
  Procedure OnParent;
 end;

Procedure TQNode_room.OnParent;
begin
 FrmMain.SetRoomStates(RS);
end;

procedure push_room_states(RS:TRoomStates);
var
 P:PQNode_room;
begin
 P:=AllocMem(SizeOf(TQNode_room));
 P^.Parent:=@P^.OnParent;
 P^.RS:=RS;
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

//const
// permission='You don''t have permission to perform that action.';

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
           '":"54742538","title":"VIP или БАН","prompt":"Может передумаю и  '+
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
           }

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
         BtnEnter.Enabled:=True;
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
 P:PQNode_reward;
begin
 P:=AllocMem(SizeOf(TQNode_reward)+Length(S));
 P^.Parent:=@P^.OnParent;
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
  For p:=0 to 1 do
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
 display_name,
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
 display_name:=fetch_user_display_name(msg2);
 user:=fetch_user_login(msg2);

 if msg2.Path['type'].AsStr='reward-redeemed' then
 begin

  rs:=s;

  reward_title:=Trim(msg2.Path['data.redemption.reward.title'].AsStr);
  cmd:='';

  if (vip_rnd.Enable) and (reward_title=vip_rnd.title) then //vip/ban
  begin

   if fetch_random_no_more(RCT) then
   begin
    if DoVipAddNew(DT,user) then
    begin
     push_irc_list(vip_rnd.cmd,[user]);
     cmd:=Format(_get_first_cmd(vip_rnd.cmd),[user]);
    end else
    begin
     push_irc_list(vip_rnd.already_vip,[user]);
     cmd:=Format(_get_first_cmd(vip_rnd.already_vip),[user]);
    end;
   end else
   begin
    push_irc_list(vip_rnd.cmd2,[user]);
    cmd:=Format(_get_first_cmd(vip_rnd.cmd2),[user]);
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
    _inc_SubModeTime(cmd,user);
   end else
   if reward_title=sub_mod.dec_title then //dec sub mode
   begin
    _dec_SubModeTime(cmd,user);
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

  add_to_story(DT,display_name,msg);

  FDbcScript:=TDbcStatementScript.Create;
  FDbcScript.Handle.DbcConnection:=DbcThread;
  FDbcScript.SetSctipt(FInsertStoryScript);
  FDbcScript.ExecuteScript;
  FDbcScript.Params.SetAsDateTime   ('datetime',DT);
  FDbcScript.Params.SetRawByteString('user'    ,display_name);
  FDbcScript.Params.SetRawByteString('mes'     ,rs);
  FDbcScript.Start;
  FDbcScript.Release;

 end;

 msg2.Free;

end;

procedure TFrmMain.SetRoomStates(RS:TRoomStates);
begin
 SetTimerSubMode(Rs_room_tag in RS);
end;

procedure TFrmMain.parse_vips(msg:RawByteString);
Const
 NAN=0/0;
var
 v:RawByteString;
 i,s,u:SizeInt;
 L:TStringList;
begin
 i:=Pos(':',msg);
 if i<>0 then
 begin
  i:=i+1;
  s:=Length(msg)-i+1;
  msg:=Trim(Copy(msg,i,s));
  if msg[Length(msg)]='.' then
  begin
   SetLength(msg,Length(msg)-1);
  end;
 end;

 L:=TStringList.Create;
 L.Sorted:=True;
 s:=GridVips.RowCount;
 if s>1 then
 begin
  u:=GridVips.FindColumn('user');
  if u<>-1 then
   For i:=1 to s-1 do
    L.AddObject(GridVips.Cells[u,i],GridVips.Rows[i]);
 end;

 repeat
  i:=System.IndexChar(PAnsiChar(msg)^,Length(msg),',');
  if i<>-1 then
  begin
   v:=Trim(Copy(msg,1,i));
   msg:=Copy(msg,i+2,Length(msg)-(i+1));
  end else
  begin
   v:=Trim(msg);
   msg:='';
  end;

  i:=L.IndexOf(v);
  if i=-1 then
  begin
   DoVipAddNew(NAN,v);
  end else
  begin
   L.Delete(i);
  end;

  Writeln(v);

 until (msg='');

 s:=L.Count;
 if s<>0 then
 begin
  For i:=0 to s-1 do
   DeleteVip(TKGridRow(L.Objects[i]).Index);
 end;

 FreeAndNil(L);
end;

procedure TFrmMain.add_to_notice(const id,msg:RawByteString);
Var
 PC:TPrivMsgCfg;
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

 Case msg of
  'Login authentication failed':
   begin
    BtnEnter.Enabled:=True;
    PC:=Default(TPrivMsgCfg);
    PC.Color:=$383838;  //Gray
    add_to_chat(PC,'','','Ошибка аутентификации');
    Exit;
   end;
 end;

 Case id of
  'vips_success':if wait_vip_update then
                 begin
                  parse_vips(msg);
                  Exit;
                 end;
 end;

 PC:=Default(TPrivMsgCfg);
 PC.Color:=$383838;  //Gray
 add_to_chat(PC,'','',msg);
end;

type
 TMsgGridCell=class(TKGridTextCell)
  var
   display_name:RawByteString;
   name_Color:DWORD;
   msg_Color:DWORD;
  procedure ApplyDrawProperties; override;
  procedure Assign(Source: TKGridCell); override;
  procedure DrawCell(ACol,ARow:Integer;const ARect:TRect;State:TKGridDrawState); override;
 end;

procedure TMsgGridCell.ApplyDrawProperties;
begin
 //
end;

procedure TMsgGridCell.Assign(Source:TKGridCell);
begin
 inherited;
 if Source is TMsgGridCell then
 begin
  display_name:=TMsgGridCell(Source).display_name;
  name_Color  :=TMsgGridCell(Source).name_Color;
  msg_Color   :=TMsgGridCell(Source).msg_Color;
 end;
end;

Function SimilarColor(c1,c2:DWORD):Boolean;
var
 d:Single;

 function pow2i(i:integer):Integer; inline;
 begin
  Result:=i*i;
 end;

begin
 d:=Sqrt(pow2i(PByte(@c1)[0]-PByte(@c2)[0])+
         pow2i(PByte(@c1)[1]-PByte(@c2)[1])+
         pow2i(PByte(@c1)[2]-PByte(@c2)[2]));
 Result:=(d<=15);
end;

Function NegColor(c:DWORD):DWORD; inline;
begin
 Result:=not c;
 PByte(@Result)[3]:=0;
end;

Const
 ME_ACTION=#1'ACTION ';

procedure TMsgGridCell.DrawCell(ACol,ARow:Integer;const ARect:TRect;State:TKGridDrawState);
var
 Canvas:TKGridCellPainter;
 BaseRect,Bounds,Interior:TRect;
 Tmp:DWORD;
 TBold:Boolean;
 T:RawByteString;
begin

 Canvas:=Grid.CellPainter;

 Canvas.Col:=ACol;
 Canvas.Row:=ARow;
 Canvas.State:=State;
 Canvas.BlockRect:=ARect;

 if gdSelected in State then
   Canvas.DrawSelectedCellBackground(ARect)
 else
   Canvas.DrawNormalCellBackground(ARect);

 Tmp:=Canvas.Canvas.Font.Color;

 Interior:=Default(TRect);
 if display_name<>'' then
 begin
  if SimilarColor(name_Color,Canvas.BackColor) then
  begin
   name_Color:=NegColor(name_Color);
  end;
  Canvas.Text:=display_name;
  TBold:=Canvas.Canvas.Font.Bold;
  Canvas.Canvas.Font.Bold:=True;
  Canvas.Canvas.Font.Color:=name_Color;
  BaseRect:=ARect;
  Canvas.CellTextRect(BaseRect,Bounds,Interior);
  Canvas.DrawCellText(ARect);
  Canvas.Canvas.Font.Bold:=TBold;
 end;

 BaseRect:=ARect;
 BaseRect.Left:=BaseRect.Left+Interior.Width;

 if display_name<>'' then
 begin
  T:=Text;

  if Copy(T,1,Length(ME_ACTION))=ME_ACTION then
  begin
   T:=Copy(T,Length(ME_ACTION)+1,Length(T)-Length(ME_ACTION));
   Canvas.Canvas.Font.Color:=name_Color;
   Canvas.Text:=' '+T;
  end else
  begin
   Canvas.Canvas.Font.Color:=msg_Color;
   Canvas.Text:=': '+T;
  end;
 end else
 begin
  Canvas.Text:=Text;
  Canvas.Canvas.Font.Color:=name_Color;
 end;

 Canvas.DrawCellText(BaseRect);

 Canvas.Canvas.Font.Color:=Tmp;

 if gdSelected in State then
   Canvas.DrawCellFocus(ARect);

end;

procedure TFrmMain.add_to_chat(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
var
 Hr:Word;
 Mn:Word;
 Sc:Word;
 Ms:Word;
 aRow,aCol:Integer;
 New:TMsgGridCell;
begin

 if (GridChat.RowCount=2) and (GridChat.FieldValue['mes',1]='') then
 begin
  aRow:=1;
 end else
 begin
  aRow:=GridChat.InsertRow(GridChat.RowCount).Index;
 end;
 DecodeTime(Now,Hr,Mn,Sc,Ms);

 GridChat.CellClass:=TKGridTextCell;
 GridChat.FieldValue['time',aRow]:=' '+AddChar('0',IntToStr(Hr),2)+':'+AddChar('0',IntToStr(Mn),2);

 aCol:=GridChat.FindColumn('mes');

 if (aCol<>-1) then
 begin
  GridChat.CellClass:=TMsgGridCell;
  GridChat.Cells[aCol,aRow]:=msg;
  New:=TMsgGridCell(GridChat.Cell[aCol,aRow]);
  New.display_name:=display_name;
  New.name_Color:=PC.Color;
  New.msg_Color :=clBlack;
  GridChat.CellClass:=TKGridTextCell;
 end;

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

function TryGetDateTime(const S:RawByteString;out Value:TDateTime):Boolean;
var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='yyyy/mm/dd';
 FS.DateSeparator:='.';
 FS.ShortTimeFormat:='hh:nn:ss';
 FS.LongTimeFormat:='hh:nn:ss';
 FS.TimeSeparator:=':';
 FS.ListSeparator:=' ';
 Result:=TryStrToDateTime2(S,Value,FS);
end;

function GetDateTimeEnd(const Value:TDateTime):TDateTime;
begin
 Result:=IncDay(Value,vip_rnd.days);
end;

function GetDateTimeStr(Const Value:TDateTime):RawByteString;
var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='yyyy/mm/dd';
 FS.DateSeparator:='.';
 FS.ShortTimeFormat:='hh:nn:ss';
 FS.LongTimeFormat:='hh:nn:ss';
 FS.TimeSeparator:=':';
 FS.ListSeparator:=' ';
 Result:=DateTimeToStr(Value,FS,True);
end;

procedure TFrmMain._set_field_story(DT:TDateTime;const user,S:RawByteString;aRow:Integer);

begin
 GridStory.FieldValue['datetime',aRow]:=' '+GetDateTimeStr(DT);
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

Procedure TFrmMain.OnListStory(Sender:TBaseTask);
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

Procedure TFrmMain.OnListVips(Sender:TBaseTask);
Var
 datetime_f,user_f:SizeInt;
 i,c:SizeInt;
 ResultSet:TZResultSet;
begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;

 if ResultSet=nil then Exit;

 c:=0;
 if ResultSet.Last then
 begin
  c:=ResultSet.GetRow;
  GridVips.RowCount:=c+1;
 end;

 if c>0 then
 begin
  datetime_f:=ResultSet.FindColumn('datetime');
  user_f    :=ResultSet.FindColumn('user');
  For i:=1 to c do
  begin
   ResultSet.MoveAbsolute(i);

   if ResultSet.IsNull(datetime_f) then
   begin
    GridVips.FieldValue['datebeg',i]:='';
    GridVips.FieldValue['dateend',i]:='';
    GridVips.FieldValue['user'   ,i]:=ResultSet.GetRawByteString(user_f);
   end else
   begin
    GridVips.FieldValue['datebeg',i]:=GetDateTimeStr(ResultSet.GetDouble(datetime_f));
    GridVips.FieldValue['dateend',i]:=GetDateTimeStr(GetDateTimeEnd(ResultSet.GetDouble(datetime_f)));
    GridVips.FieldValue['user'   ,i]:=ResultSet.GetRawByteString(user_f);
   end;

  end;
 end;

 if GridVips.HandleAllocated then
 begin
  GridVips.ScrollModeVert:=smCell;
  GridVips.ScrollModeVert:=smSmooth;
  GridVips.Columns[0].Extent:=GridVips.Columns[0].MinExtent;
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

 SetShowChat     (GetViewFlag(1));
 SetShowStory    (GetViewFlag(2));
 SetShowSubPanel (GetViewFlag(4));
 SetShowVipsPanel(GetViewFlag(8));

 if Item_AutoEnter.Checked then
 begin
  try
   login:=Trim(Config.ReadString('base','login',''));
   reply_irc_Connect(
     login,
     Trim(Config.ReadString('base','oAuth','')),
     chat
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

         //if GridChat.HandleAllocated then
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

         //if GridStory.HandleAllocated then
         begin
          cx:=Canvas.TextExtent(' 0000.00.00 00:00:00M').cx;
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
         Page.Caption:=sub_mod._label.name;
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

procedure TFrmMain.SetShowVipsPanel(V:Boolean);
Var
 cx:Integer;
 Page:TKTabSheet;
begin
 SetViewFlag(8,V);
 Item_Vips.Checked:=V;
 Case V of
  True :begin
         if PanelVips.Parent<>nil then Exit;

         Page:=Pages.AddPage(Pages);
         Page.Caption:='Список випов';
         Page.Tag:=3;
         Page.ImageIndex:=-1;
         Page.Show;
         PanelVips.Parent:=Page;

         //if GridVips.HandleAllocated then
         begin
          cx:=Canvas.TextExtent(' 0000.00.00 00:00:00M').cx;
          GridVips.Columns[0].Extent:=cx;
          GridVips.Columns[0].MinExtent:=cx;
          GridVips.Columns[1].Extent:=cx;
          GridVips.Columns[1].MinExtent:=cx;
          cx:=Canvas.TextExtent(' YAEBALETOT').cx;
          GridVips.Columns[2].Extent:=cx;
          GridVips.Columns[2].MinExtent:=cx;
         end;

        end;
  False:begin
         Page:=TKTabSheet(PanelVips.Parent);
         PanelVips.Parent:=nil;
         if Page<>nil then
          Application.ReleaseComponent(Page);
        end;
 end;
end;

procedure TFrmMain.OnPopupClickVipsPanel(Sender:TObject);
begin
 SetShowVipsPanel(not Item_Vips.Checked);
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
  FrmVipParam.EdtVipDays.Text    :=IntToStr(vip_rnd.days);

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
  vip_rnd.perc  :=StrToDWORDDef(FrmVipParam.EdtPercent.Text,70);
  if vip_rnd.perc>100 then vip_rnd.perc:=100;
  vip_rnd.days  :=StrToDWORDDef(FrmVipParam.EdtVipDays.Text,30);

  try
   case vip_rnd.Enable of
    True :Config.WriteString('vip' ,'enable','1');
    False:Config.WriteString('vip' ,'enable','0');
   end;
   Config.WriteString('vip' ,'title'   ,vip_rnd.title);
   Config.WriteString('vip' ,'msg_perc',IntToStr(vip_rnd.perc));
   Config.WriteString('vip' ,'days'    ,IntToStr(vip_rnd.days));
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
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if FrmParam.ShowModal=1 then
 begin
  chat   :=FrmParam.EdtChat.Text;

  try
   Config.WriteString('base','login'   ,FrmParam.EdtLogin.Text);
   Config.WriteString('base','oAuth'   ,FrmParam.EdtPassword.Text);
   Config.WriteString('base','chat'    ,chat);
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
  3:SetShowVipsPanel(False);
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
     chat
   );

  frmLogin.EdtPassword.Text:='';

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;

  BtnEnter.Enabled:=False;
  wait_vip_update:=False;

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

type
 TUserMaskEdit=class(TCustomMaskEdit)
  procedure ValidateEdit; override;
 end;

procedure TUserMaskEdit.ValidateEdit;
begin
 if Text='    .  .     :  :  ' then
 begin
  Clear;
 end else
 begin
  inherited;
 end;
end;

procedure TFrmMain.VipsEditorCreate(Sender:TObject;ACol,ARow:Integer;var AEditor:TWinControl);
Var
 ME:TUserMaskEdit;
begin
 Case GridVips.GetColumnName(ACol) of
  'datebeg':
   begin
    ME:=TUserMaskEdit.Create(GridVips);
    ME.EditMask:='0000/00/00 00:00:00';
    ME.SpaceChar:='_';
    AEditor:=ME;
   end;
  'user'   :
  begin
   AEditor:=TEdit.Create(GridVips);
  end;
 end;
end;

function TFrmMain.FindVipUser(Const FValue:RawByteString):Integer;
Var
 i,u,C:Integer;
begin
 Result:=-1;
 C:=GridVips.RowCount;
 if C>1 then
 begin
  u:=GridVips.FindColumn('user');
  if u<>-1 then
   For i:=1 to C-1 do
    if GridVips.Cells[u,i]=FValue then
     Exit(i);
 end;
end;

function TFrmMain.DoVipInsert(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
Var
 FDbcScript:TDbcStatementScript;
 DT:TDateTime;
 T:RawByteString;

begin
 Result:=True;
 Case FName of
  'datebeg':
   begin
    if FValue='    .  .     :  :  ' then
    begin
     GridVips.FieldValue[FName    ,ARow]:='';
     GridVips.FieldValue['dateend',ARow]:='';
     Result:=False;
    end else
    begin
     if TryGetDateTime(FValue,DT) then
     begin

      FDbcScript:=TDbcStatementScript.Create;
      FDbcScript.Handle.DbcConnection:=DbcThread;
      FDbcScript.SetSctipt(FInsertVipsScript);
      FDbcScript.ExecuteScript;
      FDbcScript.Params.SetRawByteString(':field','datetime');
      FDbcScript.Params.SetAsDateTime('value',DT);
      FDbcScript.Start;
      FDbcScript.Release;

      GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr(GetDateTimeEnd(DT));

      GridVips.ResetRowInsert;
     end else
     begin
      Result:=False;
     end;
    end;

   end;
  'user'   :
  begin
   T:=Trim(FValue);
   if (T='') or (FindVipUser(T)<>-1) then
   begin
    Result:=False;
   end else
   begin
    GridVips.FieldValue[FName,ARow]:=T;
    FDbcScript:=TDbcStatementScript.Create;
    FDbcScript.Handle.DbcConnection:=DbcThread;
    FDbcScript.SetSctipt(FInsertVipsScript);
    FDbcScript.ExecuteScript;
    FDbcScript.Params.SetRawByteString(':field','user');
    FDbcScript.Params.SetRawByteString('value',T);
    FDbcScript.Start;
    FDbcScript.Release;

    GridVips.ResetRowInsert;
   end;
  end;
 end;

end;

function TFrmMain.DoVipUpdate(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
Var
 FDbcScript:TDbcStatementScript;
 DT:TDateTime;
 T:RawByteString;

begin
 Result:=True;
 Case FName of
  'datebeg':
   begin
    if FValue='    .  .     :  :  ' then
    begin

     FDbcScript:=TDbcStatementScript.Create;
     FDbcScript.Handle.DbcConnection:=DbcThread;
     FDbcScript.SetSctipt(FUpdateVipsScript);
     FDbcScript.ExecuteScript;
     FDbcScript.Params.SetRawByteString(':field','datetime');
     FDbcScript.Params.SetAsNull('value');
     FDbcScript.Params.SetRawByteString('user',GridVips.FieldValue['user',ARow]);
     FDbcScript.Start;
     FDbcScript.Release;

     GridVips.FieldValue[FName    ,ARow]:='';
     GridVips.FieldValue['dateend',ARow]:='';

     GridVips.ResetRowInsert;
     Result:=False;
    end else
    begin
     if TryGetDateTime(FValue,DT) then
     begin

      FDbcScript:=TDbcStatementScript.Create;
      FDbcScript.Handle.DbcConnection:=DbcThread;
      FDbcScript.SetSctipt(FUpdateVipsScript);
      FDbcScript.ExecuteScript;
      FDbcScript.Params.SetRawByteString(':field','datetime');
      FDbcScript.Params.SetAsDateTime('value',DT);
      FDbcScript.Params.SetRawByteString('user',GridVips.FieldValue['user',ARow]);
      FDbcScript.Start;
      FDbcScript.Release;

      GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr(GetDateTimeEnd(DT));

      GridVips.ResetRowInsert;
     end else
     begin
      Result:=False;
     end;
    end;

   end;
  'user'   :
  begin
   T:=Trim(FValue);
   if (T='') or (FindVipUser(T)<>-1) then
   begin
    Result:=False;
   end else
   begin
    FDbcScript:=TDbcStatementScript.Create;
    FDbcScript.Handle.DbcConnection:=DbcThread;
    FDbcScript.SetSctipt(FUpdateVipsScript);
    FDbcScript.ExecuteScript;
    FDbcScript.Params.SetRawByteString(':field','user');
    FDbcScript.Params.SetRawByteString('value',T);
    FDbcScript.Params.SetRawByteString('user',GridVips.FieldValue['user',ARow]);
    FDbcScript.Start;
    FDbcScript.Release;

    GridVips.FieldValue[FName,ARow]:=T;
    GridVips.ResetRowInsert;
   end;
  end;
 end;

end;

function TFrmMain.DoVipDelete(aRow:Integer):Boolean;
Var
 FDbcScript:TDbcStatementScript;
begin
 Result:=QuestionDlg('Удаление из таблицы',
                     Format('Удалить из таблицы %s ?',[GridVips.FieldValue['user',ARow]]),
                    mtInformation,
                    [mrYes,'Да',mrNo,'Нет'],
                    'Удаление из таблицы')=mrYes;

 if not Result then Exit;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FDeleteVipsScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user',GridVips.FieldValue['user',ARow]);
 FDbcScript.Start;
 FDbcScript.Release;
end;

procedure TFrmMain.DeleteVip(aRow:Integer);
Var
 FDbcScript:TDbcStatementScript;
begin
 if (aRow>=GridVips.FixedRows) and GridVips.RowValid(aRow) then
 begin
  FDbcScript:=TDbcStatementScript.Create;
  FDbcScript.Handle.DbcConnection:=DbcThread;
  FDbcScript.SetSctipt(FDeleteVipsScript);
  FDbcScript.ExecuteScript;
  FDbcScript.Params.SetRawByteString('user',GridVips.FieldValue['user',ARow]);
  FDbcScript.Start;
  FDbcScript.Release;
  GridVips.DeleteRow(aRow);
 end;
end;

Function TFrmMain.DoVipAddNew(DT:TDateTime;Const FUser:RawByteString):Boolean;
Var
 ARow:Integer;
 FDbcScript:TDbcStatementScript;
begin
 Result:=(FUser<>'') and (FindVipUser(FUser)=-1);
 if not Result then Exit;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FAddVipsScript);
 FDbcScript.ExecuteScript;

 if IsNullValue(DT) then
 begin
  FDbcScript.Params.SetAsNull      ('datetime');
 end else
 begin
  FDbcScript.Params.SetAsDateTime  ('datetime',DT);
 end;

 FDbcScript.Params.SetRawByteString('user'    ,FUser);
 FDbcScript.Start;
 FDbcScript.Release;

 ARow:=GridVips.RowCount;
 GridVips.InsertRow(ARow);

 if IsNullValue(DT) then
 begin
  GridVips.FieldValue['datebeg',ARow]:='';
  GridVips.FieldValue['dateend',ARow]:='';
 end else
 begin
  GridVips.FieldValue['datebeg',ARow]:=GetDateTimeStr(DT);
  GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr(GetDateTimeEnd(DT));
 end;

 GridVips.FieldValue['user'   ,ARow]:=FUser;

 if GridVips.HandleAllocated then
 begin
  GridVips.ScrollModeVert:=smCell;
  GridVips.ScrollModeVert:=smSmooth;
  GridVips.Columns[0].Extent:=GridVips.Columns[0].MinExtent;
 end;
end;

procedure TFrmMain.OnBtnDeleteVipClick(Sender:TObject);
begin
 GridVips.DoDelete;
end;

procedure TFrmMain.OnBtnInsertVipClick(Sender:TObject);
begin
 GridVips.DoInsert;
end;

procedure TFrmMain.OnBtnUnVipClick(Sender:TObject);
var
 user:RawByteString;
 aRow:Integer;
begin
 aRow:=GridVips.Row;
 if (aRow>=GridVips.FixedRows) and GridVips.RowValid(aRow) then
 begin

  user:=GridVips.FieldValue['user',ARow];
  if QuestionDlg('Удалить випку на твиче?',
                 Format('Удалить випку у %s ?',[user]),
                 mtInformation,
                 [mrYes,'Да',mrNo,'Нет'],
                 'Удаление випки на твиче')=mrYes then
  begin
   push_irc_msg(Format(vip_rnd.unvip_cmd,[user]));
   DeleteVip(aRow);
  end;
 end;
end;

procedure TFrmMain.OnBtnUpdateVipClick(Sender:TObject);
begin
 push_irc_msg(vip_rnd.vip_list_cmd);
 wait_vip_update:=True;
end;

//https://docs.microsoft.com/en-us/windows/win32/fileio/volume-management-functions
procedure TFrmMain.FormCreate(Sender: TObject);
Var
 Btn,Tmp:TButton;
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

   vip_rnd.Enable:=Trim(Config.ReadString('vip','enable','0'))='1';
   vip_rnd.title :=Trim(Config.ReadString('vip','title',vip_rnd.title));
   vip_rnd.perc  :=StrToDWORDDef(Config.ReadString('vip','msg_perc',IntToStr(vip_rnd.perc)),70);
   if vip_rnd.perc>100 then vip_rnd.perc:=100;

   vip_rnd.days:=StrToDWORDDef(Config.ReadString('vip','days','30'),30);

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

   Config.WriteString('vip' ,'enable'  ,'1');
   Config.WriteString('vip' ,'title'   ,vip_rnd.title);
   Config.WriteString('vip' ,'msg_perc',IntToStr(vip_rnd.perc));
   Config.WriteString('vip' ,'days'    ,IntToStr(vip_rnd.days));

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
 Item_Subp.Caption:=sub_mod._label.name;
 Item_Subp.Checked:=False;
 Item_Subp.Tag:=2;
 Item_Subp.OnClick:=@OnPopupClickSubPanel;
 PopupView.Items.Add(Item_Subp);

 Item_Vips:=TMenuItem.Create(PopupView);
 Item_Vips.Caption:='Список випов';
 Item_Vips.Checked:=False;
 Item_Vips.Tag:=3;
 Item_Vips.OnClick:=@OnPopupClickVipsPanel;
 PopupView.Items.Add(Item_Vips);

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
 GridChat.AddColumn('mes' ,' Сообщение');

 EdtSend:=TEdit.Create(FrmMain);
 EdtSend.OnKeyDown:=@EdtSendKeyDown;

 GridStory:=TExtStringGrid.Create(FrmMain);
 GridStory.RowCount:=1;
 GridStory.Options:=GridStory.Options-[goRowSorting];
 GridStory.ScrollBars:=ssVertical;
 GridStory.AddColumn('datetime',' Дата, Время ');
 GridStory.AddColumn('user'    ,' ЛЕВ ');
 GridStory.AddColumn('mes'     ,' Сообщение');

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
 LabelSubMode.Caption:=sub_mod._label.off;
 LabelSubMode.Parent:=PanelSub;

 TextSubTime:=TEdit.Create(PanelSub);

 TextSubTime.Anchors:=[akTop,akLeft,akRight];
 TextSubTime.AutoSize:=True;
 TextSubTime.Left:=10;

 TextSubTime.Width :=PanelSub.ClientWidth-20;
 TextSubTime.Height:=PanelSub.ClientHeight-20;
 TextSubTime.ReadOnly:=True;
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

 PanelVips:=TPanel.Create(FrmMain);
 PanelVips.Align:=alClient;
 PanelVips.BevelInner:=bvNone;
 PanelVips.BevelOuter:=bvNone;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnInsertVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Добавить';
 Btn.Left:=5;
 Btn.Top :=5;
 Btn.Parent:=PanelVips;

 Pointer(Item):=Btn;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnDeleteVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Удалить';
 Btn.AnchorSide[akLeft].Side:=asrRight;
 Btn.AnchorSide[akLeft].Control:=TControl(Item);
 Btn.Top :=5;
 Btn.BorderSpacing.Left:=10;
 Btn.Parent:=PanelVips;

 Tmp:=Btn;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnUnVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Unvip';
 Btn.AnchorSide[akLeft].Side:=asrRight;
 Btn.AnchorSide[akLeft].Control:=Tmp;
 Btn.Top :=5;
 Btn.BorderSpacing.Left:=10;
 Btn.Parent:=PanelVips;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnUpdateVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Обновить с твича';
 Btn.Anchors:=[akTop,akRight];
 Btn.AnchorSide[akRight].Side:=asrRight;
 Btn.AnchorSide[akRight].Control:=PanelVips;
 Btn.Top :=5;
 Btn.BorderSpacing.Right:=5;
 Btn.Parent:=PanelVips;

 GridVips:=TDBStringGrid.Create(FrmMain);

 GridVips.Anchors:=[akTop,akLeft,akRight,akBottom];

 GridVips.AnchorSide[akTop]   .Side:=asrBottom;
 GridVips.AnchorSide[akTop]   .Control:=TControl(Item);
 GridVips.AnchorSide[akLeft]  .Side:=asrLeft;
 GridVips.AnchorSide[akLeft]  .Control:=PanelVips;
 GridVips.AnchorSide[akRight] .Side:=asrRight;
 GridVips.AnchorSide[akRight] .Control:=PanelVips;
 GridVips.AnchorSide[akBottom].Side:=asrBottom;
 GridVips.AnchorSide[akBottom].Control:=PanelVips;

 GridVips.BorderSpacing.Top:=5;

 GridVips.RowCount:=1;
 GridVips.Options:=GridVips.Options-[goRowSorting,goAlwaysShowEditor]+[goEditing];
 GridVips.ScrollBars:=ssVertical;
 GridVips.AddColumn('datebeg',' Получено ');
 GridVips.AddColumn('dateend',' Истекает ');
 GridVips.AddColumn('user'   ,' Ник ');
 GridVips.OnEditorCreate:=@VipsEditorCreate;
 GridVips.FOnDbInsert:=@DoVipInsert;
 GridVips.FOnDbUpdate:=@DoVipUpdate;
 GridVips.FOnDbDelete:=@DoVipDelete;
 GridVips.Parent:=PanelVips;

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
 FDbcScript.Notify.Add(T_FIN,@OnListStory);
 FDbcScript.SetSctipt(FListStoryScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Start;
 FDbcScript.Release;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@OnListVips);
 FDbcScript.SetSctipt(FListVipsScript);
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
   LabelSubMode.Caption:=sub_mod._label._on;
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
   LabelSubMode.Caption:=sub_mod._label.off;
  end;
 end;
 UpdateTextSubTime(True);
end;

procedure TFrmMain.BtnClickSubModeOn(Sender:TObject);
begin
 if CanSetTimerSubMode(true) then
 begin
  //submode send on
  push_irc_list(sub_mod.cmd_on,[login,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
 end else
 begin
  //submode send inc
  push_irc_list(sub_mod.cmd_inc,[login,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
 end;
end;

procedure TFrmMain.BtnClickSubModeOff(Sender:TObject);
begin
 if CanSetTimerSubMode(false) then
 begin
  //submode send off
  push_irc_list(sub_mod.cmd_off,[login,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
 end else
 begin
  //submode send dec
  push_irc_list(sub_mod.cmd_dec,[login,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
 end;
end;

procedure TFrmMain.SubModeTimerUpdate(Sender:TObject);
var
 s:Int64;
begin
 s:=(GetTickCount64-SubModeTick) div 1000;
 SubModeTick:=SubModeTick+s*1000;
 if not sub_mod.Enable then Exit;
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
  end else
  begin
   push_irc_list(sub_mod.cmd_inc,[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
   cmd:=Format(_get_first_cmd(sub_mod.cmd_inc),[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
  end;
 end else
 begin
  push_irc_list(sub_mod.cmd_inc,[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
  cmd:=Format(_get_first_cmd(sub_mod.cmd_inc),[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
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
  end else
  begin
   push_irc_list(sub_mod.cmd_dec,[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
   cmd:=Format(_get_first_cmd(sub_mod.cmd_dec),[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
  end;
 end else
 begin
  push_irc_list(sub_mod.cmd_dec,[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
  cmd:=Format(_get_first_cmd(sub_mod.cmd_dec),[user,IntToStr(sub_mod.inc_min),unixTime2String(SubModeTime)]);
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

 TOpenSubLabel_Func=class(TNodeFunc)
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
   PRawByteString(Node.CData)^:=Trim(Value);
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
  'already_vip':
   begin
    Node.Push(TLoadList_Func,@vip_rnd.already_vip);
   end;
  'unvip_cmd':
   begin
    Node.Push(TLoadStr_Func,@vip_rnd.unvip_cmd);
   end;
  'vip_list_cmd':
   begin
    Node.Push(TLoadStr_Func,@vip_rnd.vip_list_cmd);
   end;
  'perc':
   begin
    Node.Push(TLoadPerc_Func,nil);
   end;
  'days':
   begin
    Node.Push(TLoadDWORD_Func,@vip_rnd.days);
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
  'label':
   begin
    Node.Push(TOpenSubLabel_Func,Node.CData);
   end;
  'subtime_cmd':
   begin
    Node.Push(TLoadStr_Func  ,@sub_mod.subtime_cmd);
   end;
  'subtime_kd':
   begin
    Node.Push(TLoadDWORD_Func,@sub_mod.subtime_kd);
   end;
  'room_tag':
   begin
    Node.Push(TLoadStr_Func  ,@sub_mod.room_tag);
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

class procedure TOpenSubLabel_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'name':
   begin
    Node.Push(TLoadStr_Func  ,@sub_mod._label.name);
   end;
  'on':
   begin
    Node.Push(TLoadStr_Func  ,@sub_mod._label._on);
   end;
  'off':
   begin
    Node.Push(TLoadStr_Func  ,@sub_mod._label.off);
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
  'list_story':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FListStoryScript);
   end;
  'insert_story':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FInsertStoryScript);
   end;
  'get_param':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FGetParamScript);
   end;
  'set_param':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FSetParamScript);
   end;
  'list_vips':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FListVipsScript);
   end;
  'add_vips':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FAddVipsScript);
   end;
  'insert_vips':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FInsertVipsScript);
   end;
  'update_vips':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FUpdateVipsScript);
   end;
  'delete_vips':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FDeleteVipsScript);
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




