unit main;

{$mode objfpc}{$H+}

{$DEFINE VOR_RPG}

interface

uses
  LCLIntf,
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

  AudioEngine,ytts,

  TaskManager,DbcEngine,
  ZDbcIntfs,
  ZDbcSqLite,
  DbcScript;

Const
 current_version='1.4.11';

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
   pm_vip,
   pm_highlighted,
   pm_whisper
  );
  TPrivMsgStates=Set of TPrivMsgState;
  TPrivMsgCfg=record
   uid:TGUID;
   user_id:QWORD;
   sub_gifter:DWORD;
   subscriber_m:DWORD;
   subscriber_s:DWORD;
   Color:DWORD;
   PS:TPrivMsgStates;
  end;

  TStateSpeedButton=class(TCustomSpeedButton)
   private
    FCaptionState:SizeUInt;
    Procedure SetCaptionState(S:SizeUInt);
   public
    Items:TStringList;
    property    CaptionState:SizeUInt read FCaptionState write SetCaptionState;
    constructor Create(TheOwner:TComponent); override;
    destructor  Destroy; override;
  end;

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
    procedure ErrorLogn(Code:Byte);
    procedure SetLognBtn(Login:Boolean);
    function  getRandomTmpVip(const msg:RawByteString):SizeInt;
    function  try_theif_vip(const dst_user,msg:RawByteString;var cmd:RawByteString):Boolean;
    procedure add_reward(const S:RawByteString);
    procedure _add_reward_2_log(const s,cmd:RawByteString);
    procedure SetRoomStates(RS:TRoomStates);
    procedure parse_vips(msg:RawByteString);
    procedure add_to_notice(const id,msg:RawByteString);
    procedure add_vol_cmd(const user,cmd,param:RawByteString);
    procedure add_to_chat_cmd(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
    procedure add_to_chat(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
    procedure EdtSendKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure SystemTrayClick(Sender: TObject);
    procedure _set_field_story(DT:TDateTime;const user,rew,msg,cmd:RawByteString;aRow:Integer);
    procedure add_to_story(DT:TDateTime;const user,rew,msg,cmd:RawByteString);
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
    procedure OnPopupClickCheckUpdate(Sender:TObject);
    procedure OnPopupClickYttsParam(Sender:TObject);
    procedure OnPopupClickVolParam(Sender:TObject);
    procedure OnPopupClickSubParam(Sender:TObject);
    procedure OnPopupClickUnSubParam(Sender:TObject);
    procedure OnPopupClickVorRpgParam(Sender:TObject);
    procedure OnPopupClickVipParam(Sender:TObject);
    procedure OnPopupClickParam(Sender:TObject);
    procedure OnExortClick(Sender:TObject);
    procedure OnTabClose(Sender:TObject;TabIndex:Integer;var CanClose:Boolean);
    procedure OnBtnEnterClick(Sender:TObject);
    procedure OnBtnCloseClick(Sender:TObject);
    Procedure OnListStory(Sender:TBaseTask);
    procedure OnPopupClose(Sender:TObject);
    procedure SetViewFlag(f:byte;b:Boolean);
    function  GetViewFlag(f:byte):Boolean;
    procedure FormCreate(Sender: TObject);
    procedure LoadXML;
  private

  public
   FCreateScript     :TSQLScript;
   FListStoryScript  :TSQLScript;
   FInsertStoryScript:TSQLScript;

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

   BtnPlay:TStateSpeedButton;

   PopupView:TPopupMenu;
   PopupCfg :TPopupMenu;

   Item_Chat,
   Item_Story,
   Item_Subp,
   Item_Vips,
   Item_AutoEnter,
   Item_UseTray,
   Item_CheckUpdate:TMenuItem;

   EdtSend:TEdit;

   wait_vip_update:Boolean;

   Procedure LazyInitAudioThread;
   procedure OnBtnPlayClick(Sender:TObject);
  end;

var
 pool:Tevpool;
 pool_config:Tevpool_config;

 FAudioThread:TAudioConnection;

 FrmMain: TFrmMain;

 FExportStoryScript:TSQLScript;
 FGetParamScript:TSQLScript;
 FSetParamScript:TSQLScript;

function  get_spec_room_tag:RawByteString; inline;
procedure push_notice(const id,msg:RawByteString);
procedure push_chat(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
procedure push_login(code:Byte);
procedure push_room_states(RS:TRoomStates);
procedure push_reward(const S:RawByteString);

function  _get_first_cmd(L:TStringList):RawByteString;

function  GetDateTimeStr_US(Const Value:TDateTime):RawByteString;

procedure SetDBParam(Const fname,fvalue:RawByteString);
procedure GetDBParam(Const fname:RawByteString;N:TNotifyTask);

Function  Extract_nick(const s:RawByteString):RawByteString;

function  fetch_msg(msg2:TJson):RawByteString; inline;
function  fetch_reward(msg2:TJson):RawByteString; inline;

var
 Config:TINIFile;

 RCT:TMTRandomContext;

 base:record
  chat:RawByteString;
  login:RawByteString;
  login2:RawByteString;
  useLogin2:Boolean;
  dontWelcome:Boolean;
  useLoginState:Byte;
 end;

 view_mask:Byte;

 DbcThread:TDbcConnection;

function  fetch_random_no_more(Var Context:TMTRandomContext):Boolean;
procedure push_irc_msg(const msg:RawByteString);
procedure push_irc_list(L:TStringList;const Args:Array of const);
function  FetchAny(var Value:RawByteString):RawByteString;

Const
 DefZURL='zdbc:sqlite:///new.db';

type
 TPCharStream=class(TCustomMemoryStream)
  public
   constructor Create(P:PChar;len:SizeUint); virtual; overload;
   procedure   SetNew(P:PChar;len:SizeUint);
 end;

implementation

uses
 xml_parse,data_xml,
 ReleasesVersion,

 //DbcScriptExp,
 //DbcScriptUtils,
 //ZTokenizer,

 UFrmUnSubParam,
 UFrmYtts,
 WinAudioSession,
 ufrmexportstory,
 ufrmvol,
 UFrmAbout,
 UFrmParam,
 UFrmVipParam,
 {$IFDEF VOR_RPG}
 UFrmVorRpg,
 {$ENDIF}
 UFrmSubParam,
 Uloginf;

{$R *.lfm}

{ TFrmMain }

 //zeosdbo;zeosdbo\component;zeosdbo\core;zeosdbo\plain;zeosdbo\parsesql;zeosdbo\dbc

var
 KCLOSE,KCLOSE_D,DIMAGE,AIMAGE:TImageList;

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
 FPC:TPrivMsgCfg;
begin
 SetString(user        ,@data,StrLen(@data));
 SetString(display_name,Fdnm ,StrLen(Fdnm));
 SetString(msg         ,Fmsg ,StrLen(Fmsg));
 FPC:=PC;
 FreeMem(@Self);
 FrmMain.add_to_chat(FPC,user,display_name,msg);
 FrmMain.add_to_chat_cmd(FPC,user,display_name,msg);
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
  Fcode:Byte;
  Procedure OnParent;
 end;

Procedure TQNode_login.OnParent;
begin
 Case FCode of
  0:FrmMain.SetLognBtn(True);
  else
    FrmMain.ErrorLogn(FCode);
 end;
 FreeMem(@Self);
end;

procedure push_login(code:Byte);
var
 P:PQNode_login;
begin
 P:=AllocMem(SizeOf(TQNode_login));
 P^.Parent:=@P^.OnParent;
 P^.Fcode:=code;
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

function get_spec_room_tag:RawByteString; inline;
begin
 Result:=sub_mod.room_tag;
end;

//const
// permission='You don''t have permission to perform that action.';
//This room is in slow mode and you are sending messages too quickly. You will be able to talk again in 0 seconds.

Function _IsCmd(const msg:RawByteString):Boolean;
begin
 Result:=(msg<>'') and (msg[1]='/');
 if Result and (Length(msg)>=3) then
 begin
  Result:=(PDWORD(PChar(Msg))^ and $FFFFFF)<>$656D2F; //msg<>'/me'
 end;
end;

procedure push_irc_msg(const msg:RawByteString);
begin
 if (base.useLoginState=2) then
 begin
  if _IsCmd(msg) then
  begin
   reply_irc_msg(msg);
  end else
  begin
   reply_irc_msg2(msg);
  end;
 end else
 begin
  reply_irc_msg(msg);
 end;
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

procedure TFrmMain.ErrorLogn(Code:Byte);
begin
 SetLognBtn(False);
 Case Code of
  1:ShowMessage('Ошибка подключения к серверу чата!');
  2:ShowMessage('Ошибка подключения к серверу уведомлений!');
  3:ShowMessage('Ошибка подключения к серверу чата, второй учётной записи!');
  4:push_irc_msg('Обрыв соединения к серверу уведомлений, переподключение...'); //reconnect
  5:push_irc_msg('Ошибка соединения с сервером уведомлений, переподключение...'); //reconnect error
  6:push_irc_msg('Соединение с сервером наград восстановлено!'); //reconnect succes
 end;
end;

procedure TFrmMain.SetLognBtn(Login:Boolean);
begin
 Case Login of
  True :begin
         BtnEnter.Visible:=False;
         BtnInfo.Caption:=base.login;
         BtnInfo .Visible:=True;
         BtnClose.Visible:=True;
         BtnClose.Left:=0;

         if base.useLoginState=1 then
            base.useLoginState:=2;

         if not base.dontWelcome then
          push_irc_msg(vip_rnd.login_msg);

         {
         add_reward(
           '{"type":"reward-redeemed","data":{"timestamp":"2020-07-08T18:38:23.'+
           '141491302Z","redemption":{"id":"62d7f76e-7a16-432d-94ce-541897f02fa3","u'+
           'ser":{"id":"84616392","login":"satan_rulezz","display_name":"Satan_R'+
           'ulezz"},"channel_id":"54742538","redeemed_at":"2020-07-08T18:38:23.01829'+
           '9023Z","reward":{"id":"9c25cd82-30e4-4e23-8dae-e3ae630b9bab","channel_id'+
           '":"54742538","title":"ВОР","prompt":"Может передумаю и  '+
           'отниму випку.","cost":450000,"is_user_input_required":true,"is_sub_only":'+
           'false,"image":null,"default_image":{"url_1x":"https://static-cdn.jtvnw.ne'+
           't/custom-reward-images/default-1.png","url_2x":"https://static-cdn.jtvnw.net'+
           '/custom-reward-images/default-2.png","url_4x":"https://static-cdn.jtvnw.net/'+
           'custom-reward-images/default-4.png"},"background_color":"#1F69FF","is_enab'+
           'led":true,"is_paused":false,"is_in_stock":true,"max_per_stream":{"is_ena'+
           'bled":false,"max_per_stream":0},"should_redemptions_skip_request_queue":fal'+
           'se,"template_id":null,"updated_for_indicator_at":"2020-07-06T17:34:56.82009'+
           '8059Z"},"user_input":"2cvo79kr","status":"UNFULFILLED","cursor":"Nj'+
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
         base.useLoginState:=0;
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
 Result:=msg2.Path['data.redemption.user_input'].AsStr;
end;

function fetch_reward(msg2:TJson):RawByteString; inline;
begin
 Result:=msg2.Path['data.redemption.reward.title'].AsStr;
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

function fetch_random_vor(Var Context:TMTRandomContext):Boolean;
begin
 Result:=Random(Context,100)<vip_rnd.perc_vor;
end;

function TFrmMain.getRandomTmpVip(const msg:RawByteString):SizeInt;
var
 i:integer;
 s:SizeInt;
 L,P,T:TStringList;
begin
 Result:=-1;
 P:=nil;
 T:=nil;
 FrmVipParam.getTmpVipList2(P,T);
 if (T.Count=0) then
 begin
  L:=P;
 end else
 begin
  L:=T;
 end;
 s:=L.Count;
 if (s<>0) then
 begin
  i:=-1;
  if not L.Find(LowerCase(Trim(msg)),i) then
  begin
   i:=Random(RCT,s);
  end;
  Result:=TKGridRow(L.Objects[i]).Index;
 end;
 FreeAndNil(P);
 FreeAndNil(T);
end;

Function Extract_nick(const s:RawByteString):RawByteString;
var
 i:SizeInt;
begin
 Result:=s;
 if (s<>'') and (s[1]='@') then
 begin
  Result:=Copy(s,2,Length(s)-1);
 end;

 i:=Pos(' ',Result);
 if (i<>0) then
 begin
  Result:=Copy(Result,1,i-1);
 end;

 if (Result<>'') and (Result[Length(Result)]=':') then
 begin
  Result:=Copy(Result,1,High(Result));
 end;
end;

function TFrmMain.try_theif_vip(const dst_user,msg:RawByteString;var cmd:RawByteString):Boolean;
var
 src_user:RawByteString;
 aRow:SizeInt;

begin
 Result:=False;

 aRow:=getRandomTmpVip(Extract_nick(msg));
 if (aRow=-1) then
 begin
  Result:=True;
  push_irc_list(vip_rnd.is_empty,[dst_user]);
  cmd:=Format(_get_first_cmd(vip_rnd.is_empty),[dst_user]);
  Exit;
 end;

 src_user:=GridVips.FieldValue['user',ARow];

 push_irc_list(vip_rnd.vor_sucs,[dst_user,src_user]);
 cmd:=Format(_get_first_cmd(vip_rnd.vor_sucs),[dst_user,src_user]);

 FrmVipParam.DbUpdateVip_user(src_user,dst_user);
 GridVips.FieldValue['user',ARow]:=dst_user;

 Result:=True;
end;

procedure TFrmMain.add_reward(const S:RawByteString);
var
 DT:TDateTime;
 ms:TStream;
 msg2:TJson;
 msg,cmd,
 user,
 display_name,
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

  reward_title:=Trim(fetch_reward(msg2));
  cmd:='';

  if (vip_rnd.Enable) and (reward_title=vip_rnd.title) then //vip/ban
  begin
   if fetch_random_no_more(RCT) then
   begin
    if (FrmVipParam.FindVipUser(user)<>-1) then
    begin
     push_irc_list(vip_rnd.already_vip,[user]);
     cmd:=Format(_get_first_cmd(vip_rnd.already_vip),[user]);
    end else
    if (vip_rnd.max_vips<>0) and (GridVips.RowCount-1>=vip_rnd.max_vips) then
    begin
     push_irc_list(vip_rnd.is_max_vip,[user]);
     cmd:=Format(_get_first_cmd(vip_rnd.is_max_vip),[user]);
    end else
    if FrmVipParam.DoVipAddNew(DT,user) then
    begin
     push_irc_list(vip_rnd.cmd,[user]);
     cmd:=Format(_get_first_cmd(vip_rnd.cmd),[user]);
    end;
   end else
   begin
    push_irc_list(vip_rnd.cmd2,[user,IntToStr(vip_rnd.ban_time)]);
    cmd:=Format(_get_first_cmd(vip_rnd.cmd2),[user,IntToStr(vip_rnd.ban_time)]);
   end;
  end else

  {$IFDEF VOR_RPG}
  if (vor_rpg.Enable) and (reward_title=vip_rnd.title_vor) then //vor rpg
  begin
   FrmVorRpg.rpg_theif_vip(s,user,msg);
   Exit;
  end else
  {$ENDIF}
  if (vip_rnd.Enable_vor) and (reward_title=vip_rnd.title_vor) then //vor
  begin
   if fetch_random_vor(RCT) then
   begin
    if (FrmVipParam.FindVipUser(user)<>-1) then
    begin
     push_irc_list(vip_rnd.already_vip,[user]);
     cmd:=Format(_get_first_cmd(vip_rnd.already_vip),[user]);
    end else
    if not try_theif_vip(user,msg,cmd) then
    begin
     push_irc_list(vip_rnd.vor_jail,[user]);
     cmd:=Format(_get_first_cmd(vip_rnd.vor_jail),[user]);
    end;
   end else
   begin
    push_irc_list(vip_rnd.vor_jail,[user]);
    cmd:=Format(_get_first_cmd(vip_rnd.vor_jail),[user]);
   end;
  end else

  if sub_mod.Enable then
  begin
   if reward_title=sub_mod.inc_title then //add sub mode
   begin
    FrmSubParam._inc_SubModeTime(cmd,user);
   end else
   if reward_title=sub_mod.dec_title then //dec sub mode
   begin
    FrmSubParam._dec_SubModeTime(cmd,user);
   end;
   if sub_mod.DontLog then
   begin
    msg2.Free;
    Exit;
   end;
  end;

  if unsub_mod.Enable and (reward_title=unsub_mod.inc_title) then
  begin
   //unsub mode
   FrmUnSubParam._inc_msg(cmd,user);
  end;

  add_to_story(DT,display_name,reward_title,msg,cmd);

  FDbcScript:=TDbcStatementScript.Create;
  FDbcScript.Handle.DbcConnection:=DbcThread;
  FDbcScript.SetSctipt(FInsertStoryScript);
  FDbcScript.ExecuteScript;
  FDbcScript.Params.SetAsDateTime   ('datetime',DT);
  FDbcScript.Params.SetRawByteString('user'    ,display_name);
  FDbcScript.Params.SetRawByteString('mes'     ,s);
  FDbcScript.Params.SetRawByteString('cmd'     ,cmd);
  FDbcScript.Start;
  FDbcScript.Release;

 end;

 msg2.Free;

end;

procedure TFrmMain._add_reward_2_log(const s,cmd:RawByteString);
var
 DT:TDateTime;
 ms:TStream;
 msg2:TJson;
 msg,
 display_name,
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
 reward_title:=Trim(fetch_reward(msg2));

 msg2.Free;

 add_to_story(DT,display_name,reward_title,msg,cmd);

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FInsertStoryScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetAsDateTime   ('datetime',DT);
 FDbcScript.Params.SetRawByteString('user'    ,display_name);
 FDbcScript.Params.SetRawByteString('mes'     ,s);
 FDbcScript.Params.SetRawByteString('cmd'     ,cmd);
 FDbcScript.Start;
 FDbcScript.Release;
end;

procedure TFrmMain.SetRoomStates(RS:TRoomStates);
begin
 FrmSubParam.OnRoomState(Rs_room_tag in RS);
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
   FrmVipParam.DoVipAddNew(NAN,v);
  end else
  begin
   L.Delete(i);
  end;

 until (msg='');

 s:=L.Count;
 if s<>0 then
 begin
  For i:=0 to s-1 do
   FrmVipParam.DeleteVip(TKGridRow(L.Objects[i]).Index);
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
    SetLognBtn(False);
    PC:=Default(TPrivMsgCfg);
    PC.Color:=$383838;  //Gray
    add_to_chat(PC,'','','Ошибка аутентификации');
    Exit;
   end;
 end;

 Case id of
  'no_vips'     :wait_vip_update:=False;
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
   Back_Color:DWORD;
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
  Back_Color  :=TMsgGridCell(Source).Back_Color;
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
 Result:=(d<=150);
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
 BackColor:DWORD;
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

 BackColor:=ColorToRGB(Canvas.BackColor);

 if Back_Color<>BackColor then
 begin
  BackColor:=Back_Color;
  Tmp:=Canvas.Canvas.Brush.Color;
  Canvas.Canvas.Brush.Color:=BackColor;
  Canvas.Canvas.Brush.Style:=bsSolid;
  Canvas.Canvas.FillRect(ARect);
  Canvas.Canvas.Brush.Color:=Tmp;
 end;

 Tmp:=Canvas.Canvas.Font.Color;

 Interior:=Default(TRect);
 if display_name<>'' then
 begin
  if SimilarColor(name_Color,BackColor) then
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
   if SimilarColor(msg_Color,BackColor) then
   begin
    msg_Color:=NegColor(msg_Color);
   end;
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

function FetchAny(var Value:RawByteString):RawByteString;
Const
 Delimiter=' ';
Const
 Quotations:Set of AnsiChar=['`','''','"'];
Var
 i:SizeUInt;
 Quote:AnsiChar;
 State:Byte;
begin
 Result:='';
 Quote:=#0;
 State:=0;
 if Length(Value)>0 then
 begin
  For i:=1 to Length(Value) do
  begin
   case State of
    0:begin
       if (Value[i] in Quotations)  then
       begin
        State:=1;
        Quote:=Value[i];
       end else
       if Value[i]=Delimiter then
       begin
        System.Delete(Value,1,i);
        Exit;
       end else
       begin
        Result:=Result+Value[i];
       end;
      end;
    1:begin
       if Value[i]=Quote then
       begin
        State:=2;
       end else
       begin
        Result:=Result+Value[i];
       end;
      end;
    2:begin
       if Value[i]=Quote then
       begin
        State:=1;
        Result:=Result+Quote;
       end else
       if Value[i]=Delimiter then
       begin
        System.Delete(Value,1,i);
        Exit;
       end else
       begin
        State:=0;
        Quote:=#0;
        Result:=Result+Value[i];
       end;
      end;
   end;
  end;
  Value:='';
 end;
end;

procedure TFrmMain.add_vol_cmd(const user,cmd,param:RawByteString);
var
 i:Integer;
 F,v:RawByteString;
 Volume:ISimpleAudioVolume;
begin
 if (LowerCase(cmd)<>vol_cmd.prefix) then Exit;

 F:=LowerCase(param);

 try
  if (F<>'') then
  begin

    v:=FetchAny(F);

    if (F<>'') then
    begin
     //set volume app
     Volume:=FindSessionsStr(v);
     if Volume=nil then
     begin
      F:='nop';
     end else
     begin
      i:=0;
      F:=FetchAny(F);
      case F of
       'm',
       'mute':
       begin
        SetMute(Volume,True);
        F:=GetVolumeInfo(v,Volume);
       end;
       'u',
       'unmute':
       begin
        SetMute(Volume,False);
        F:=GetVolumeInfo(v,Volume);
       end;
       else
       begin
        if TryStrToInt(F,i) then
        begin
         case F[1] of
          '-',
          '+':begin
               i:=GetVolume(Volume)+i;
               if i<0 then   i:=0;
               if i>100 then i:=100;
               SetVolume(Volume,i);
              end;
          else
              begin //absolute
               if i>100 then i:=100;
               SetVolume(Volume,i);
              end;
         end;
         F:=GetVolumeInfo(v,Volume);
        end else
        begin
         F:='nop';
        end;
       end;
      end;
     end;

     push_irc_msg(user+' '+F);
    end else
    begin
     //get info app
     Volume:=FindSessionsStr(v);
     if Volume=nil then
     begin
      F:='nop';
     end else
     begin
      F:=GetVolumeInfo(v,Volume);
     end;
     push_irc_msg(user+' '+F);
    end;

  end else
  begin
   //list app
   F:=GetSessionsStr;

   if F='' then F:='nop';
   push_irc_msg(user+' '+F);

  end;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
   push_irc_msg(user+' Error:'+E.Message );
  end;
 end;
end;

{
var
 calc_TickKd:Int64;

procedure DoCalc(const user,msg:RawByteString);
var
 F,v:RawByteString;
 i:Integer;
 Tokenizer:TZDbcScriptTokenizer;
 FToken:TZToken;
 Exp:TExpressionState;
 calc:TExpressionCalc;
 Buffer,EOS:PAnsiChar;
begin

 F:=Trim(msg);
 v:=FetchAny(F);
 v:=LowerCase(v);

 if (v='!calc') then
 begin

  if (GetTickCount64<calc_TickKd+3000) then Exit;

  Buffer:=PAnsiChar(F);
  EOS:=Buffer+Length(F);

  Tokenizer:=TZDbcScriptTokenizer.Create;

  Exp:=Default(TExpressionState);
  FToken:=Default(TZToken);
  While (FToken.TokenType<>ttEOF) do
  begin
   FToken:=TZDbcScriptTokenizer(Tokenizer).FetchNextToken(Buffer,EOS);
   Exp.Parse(FToken);
  end;
  Tokenizer.Free;

  Exp.Finish;

  calc:=Default(TExpressionCalc);

  calc.Node:=Exp.Node;

  v:=_GetAsRawByteString(calc.calc);

  if v='' then
   push_irc_msg('@'+user+' что то пошло не так((')
  else
   push_irc_msg('@'+user+' '+Trim(F)+'='+v);

  FreeExpression(calc.Node);

  calc_TickKd:=GetTickCount64;
 end;
end;}

procedure TFrmMain.add_to_chat_cmd(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
var
 cmd,param:RawByteString;
 {$IFOPT D+}
  tmpd:DWORD;
 {$ENDIF}
begin

 if (PC.PS=[]) and ((PC.subscriber_s<>0) or (PC.subscriber_m<>0)) then //subs only, no moder, no vip
 begin
  if FrmUnSubParam.subs_msg(Trim(msg),PC.uid) then Exit;
 end;

 param:=Trim(msg);
 cmd:=Trim(FetchAny(param));

 if (cmd<>'') and (cmd[1]='!') then //this is cmd
 begin

  {$IFOPT D+}
  if LowerCase(cmd)='!vip_test' then
  begin
   tmpd:=vip_rnd.ban_time;
   vip_rnd.ban_time:=0;
   add_reward(
     '{"type":"reward-redeemed","data":{"timestamp":"2020-07-08T18:38:23.'+
     '141491302Z","redemption":{"id":"62d7f76e-7a16-432d-94ce-541897f02fa3",'+
     '"user":{"id":"0","login":"'+user+'","display_name":"'+display_name+'"},'+
     '"channel_id":"0","redeemed_at":"2020-07-08T18:38:23.018299023Z",'+
     '"reward":{"id":"9c25cd82-30e4-4e23-8dae-e3ae630b9bab","channel_id":"0",'+
     '"title":"'+vip_rnd.title+'","prompt":"","is_sub_only":false,"user_input":""}}}}');
   vip_rnd.ban_time:=tmpd;
  end;
  {$ENDIF}

  if (PC.PS*[pm_broadcaster,pm_moderator]<>[]) then
  begin
   if LowerCase(cmd)='!reconnect' then
    reply_irc_reconnect;
   if vol_cmd.Enable then
    add_vol_cmd(user,cmd,param);
   FrmVipParam.vip_time_cmd(user,cmd,param);
  end;

  if (sub_mod.Enable) then
   FrmSubParam.add_sub_mod_cmd(cmd);

  {$IFDEF VOR_RPG}
  if vor_rpg.Enable then
   FrmVorRpg.add_to_chat_cmd(PC,user,cmd,param);
  {$ENDIF}
 end;

 FrmYtts.add_to_chat_cmd((pm_highlighted in PC.PS),cmd,param,msg);

 //DoCalc(user,msg);

 //DoCalc('','!calc 1.0/0.0');

 //DoCalc('','!calc 999999999999999999999999/0.0000000000000000000001');
 //!calc 9^9^9^9^9^9^9

 //!calc 1.0/0.0

end;

Procedure TStateSpeedButton.SetCaptionState(S:SizeUInt);
begin
 FCaptionState:=S;
 ImageIndex:=S;
 if S<Items.Count then
 begin
  Caption:=Items[S];
 end else
 begin
  Caption:='';
 end;
 //DoOnResize;
end;

constructor TStateSpeedButton.Create(TheOwner:TComponent);
begin
 inherited Create(TheOwner);
 Items:=TStringList.Create;
end;

destructor  TStateSpeedButton.Destroy;
begin
 FreeAndNil(Items);
 inherited;
end;


Function GetShortTimeStr(Time:TDateTime):RawByteString;
var
 Hr,Mn,Sc,Ms:Word;
begin
 DecodeTime(Time,Hr,Mn,Sc,Ms);
 Result:=AddChar('0',IntToStr(Hr),2)+':'+AddChar('0',IntToStr(Mn),2);
end;

Const
 highlighted_color=$FF4791;
 whisper_color=$494646;

procedure TFrmMain.add_to_chat(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
var
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

 GridChat.CellClass:=TKGridTextCell;
 GridChat.FieldValue['time',aRow]:=' '+GetShortTimeStr(Now);

 aCol:=GridChat.FindColumn('mes');

 if (aCol<>-1) then
 begin
  GridChat.CellClass:=TMsgGridCell;
  GridChat.Cells[aCol,aRow]:=msg;
  New:=TMsgGridCell(GridChat.Cell[aCol,aRow]);
  New.display_name:=display_name;
  New.name_Color  :=PC.Color;
  New.msg_Color   :=clBlack;

  if (pm_whisper in PC.PS) then
  begin
   New.Back_Color:=whisper_color;
  end else
  if (pm_highlighted in PC.PS) then
  begin
   New.Back_Color:=highlighted_color;
  end else
  begin
   New.Back_Color:=ColorToRGB(GridChat.CellPainter.BackColor);
  end;

  GridChat.CellClass:=TKGridTextCell;
 end;

 if GridChat.RowCount>300 then
 begin
  GridChat.DeleteRow(1);
  Dec(aRow);
 end;

 if GridChat.HandleAllocated then
 begin
  //GridChat.ScrollModeVert:=smCell;
  //GridChat.ScrollModeVert:=smSmooth;
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

function GetDateTimeStr_US(Const Value:TDateTime):RawByteString;
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

procedure TFrmMain._set_field_story(DT:TDateTime;const user,rew,msg,cmd:RawByteString;aRow:Integer);

begin
 GridStory.FieldValue['datetime',aRow]:=' '+GetDateTimeStr_US(DT);
 GridStory.FieldValue['user'    ,aRow]:=user;
 GridStory.FieldValue['rew'     ,aRow]:=rew;
 GridStory.FieldValue['mes'     ,aRow]:=msg;
 GridStory.FieldValue['cmd'     ,aRow]:=cmd;
end;

procedure TFrmMain.add_to_story(DT:TDateTime;const user,rew,msg,cmd:RawByteString);
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

 _set_field_story(DT,user,rew,msg,cmd,aRow);

 if GridStory.RowCount>300 then
 begin
  GridStory.DeleteRow(1);
  Dec(aRow);
 end;

 if GridStory.HandleAllocated then
 begin
  //GridStory.ScrollModeVert:=smCell;
  //GridStory.ScrollModeVert:=smSmooth;
  GridStory.ScrollBy(0,aRow);
 end;

end;

Procedure TFrmMain.OnListStory(Sender:TBaseTask);
Var
 datetime_f,user_f,mes_f,cmd_f:SizeInt;
 i,c:SizeInt;
 ResultSet:TZResultSet;
 ms:TPCharStream;
 msg2:TJson;
 msg,cmd,rew:RawByteString;
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
  cmd_f     :=ResultSet.FindColumn('cmd');
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

   rew:=Trim(fetch_reward(msg2));

   cmd:=ResultSet.GetRawByteString(cmd_f);

   _set_field_story(ResultSet.GetDouble(datetime_f),ResultSet.GetRawByteString(user_f),rew,msg,cmd,i);

   msg2.Free;
  end;
  FreeAndNil(ms);
 end;

 if GridStory.HandleAllocated then
 begin
  //GridStory.ScrollModeVert:=smCell;
  //GridStory.ScrollModeVert:=smSmooth;
  GridStory.ScrollBy(0,c);
  GridStory.Columns[0].Extent:=GridStory.Columns[0].MinExtent;
 end;
end;

procedure TFrmMain.FormActivate(Sender: TObject);
begin

 SetShowChat     (GetViewFlag(1));
 SetShowStory    (GetViewFlag(2));
 SetShowSubPanel (GetViewFlag(4));
 SetShowVipsPanel(GetViewFlag(8));

 if Item_AutoEnter.Checked then
 begin
  try
   reply_irc_Connect(
     base.login,
     Trim(Config.ReadString('base','oAuth','')),
     base.chat
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
 FreeAndNil(FAudioThread);
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

          cx:=Canvas.TextExtent(' НаградаM').cx;
          GridStory.Columns[2].Extent:=cx;
          GridStory.Columns[2].MinExtent:=cx;

          cx:=Canvas.TextExtent(' СообщениеM').cx;
          GridStory.Columns[3].Extent:=cx;
          GridStory.Columns[3].MinExtent:=cx;
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

procedure TFrmMain.OnPopupClickCheckUpdate(Sender:TObject);
begin
 Item_CheckUpdate.Checked:=not Item_CheckUpdate.Checked;
 try
  Case Item_CheckUpdate.Checked of
   True :Config.WriteString('view','check_update','1');
   False:Config.WriteString('view','check_update','0');
  end;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;
 if Item_CheckUpdate.Checked then
  SendReleasesRequest;
end;

procedure TFrmMain.OnPopupClickYttsParam(Sender:TObject);
begin
 FrmYtts.Open
end;

procedure TFrmMain.OnPopupClickVolParam(Sender:TObject);
begin
 FrmVolParam.Open;
end;

procedure TFrmMain.OnPopupClickSubParam(Sender:TObject);
begin
 FrmSubParam.Open;
end;

procedure TFrmMain.OnPopupClickUnSubParam(Sender:TObject);
begin
 FrmUnSubParam.Open;
end;

procedure TFrmMain.OnPopupClickVorRpgParam(Sender:TObject);
begin
 {$IFDEF VOR_RPG}
 FrmVorRpg.Open
 {$ENDIF}
end;

procedure TFrmMain.OnPopupClickVipParam(Sender:TObject);
begin
 FrmVipParam.Open;
end;

procedure TFrmMain.OnPopupClickParam(Sender:TObject);
begin
 FrmParam.Open;
end;

procedure TFrmMain.OnExortClick(Sender:TObject);
begin
 FrmExportStory.Open;
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
  frmLogin.EdtLogin.Text   :=base.login;
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
   base.login               :=Trim(frmLogin.EdtLogin.Text);
   frmLogin.EdtPassword.Text:=Trim(frmLogin.EdtPassword.Text);

   Config.WriteString('base','login',base.login);
   Config.WriteString('base','oAuth',frmLogin.EdtPassword.Text);

   reply_irc_Connect(
     base.login,
     frmLogin.EdtPassword.Text,
     base.chat
   );

  if base.useLogin2 then
  begin
   reply_irc_Connect2(
     base.login2,
     Config.ReadString('base','oAuth2',''),
     base.chat
   );
   base.useLoginState:=1;
  end;

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
 FrmSubParam.SetTimerSubMode(false);
end;

Const
 CfgName='config.ini';

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

Procedure TFrmMain.LazyInitAudioThread;
begin
 if (FAudioThread=nil) then
 begin
  FAudioThread:=TAudioConnection.Create;
  Case BtnPlay.CaptionState of
   0:FAudioThread.Start;
   1:FAudioThread.Pause;
  end;
 end;
end;

procedure TFrmMain.OnBtnPlayClick(Sender:TObject);
begin
 Case BtnPlay.CaptionState of
  0:begin
     BtnPlay.CaptionState:=1;
     if (FAudioThread<>nil) then
     begin
      FAudioThread.Pause;
     end;
    end;
  1:begin
     BtnPlay.CaptionState:=0;
     if (FAudioThread<>nil) then
     begin
      FAudioThread.Start;
     end;
    end;
 end;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
Var
 Item:TMenuItem;
 D:RawByteString;
 FDbcScript:TDbcStatementScript;

begin
 Caption:=Caption+' '+current_version;

 RCT:=Default(TMTRandomContext);
 RandomInit(RCT);

 LoadXML;

 D:=GetLocalIni;

 IF FileExists(D) then
 begin
  try
   Config:=TINIFile.Create(D);

   //read

   FrmParam.LoadCfg;

   view_mask:=StrToDWORDDef(Config.ReadString('view','mask','1'),1);

   FrmVipParam.LoadCfg;

   {$IFDEF VOR_RPG}
   FrmVorRpg.LoadCfg;
   {$ENDIF}

   FrmSubParam.LoadCfg;

   FrmUnSubParam.LoadCfg;

   FrmVolParam.LoadCfg;

   FrmYtts.LoadCfg;

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

   view_mask:=1;

   Config:=TINIFile.Create(D);

   FrmParam.InitCfg;

   Config.WriteString('view','check_update','1');
   Config.WriteString('view','autologin','0');
   Config.WriteString('view','systray'  ,'1');
   Config.WriteString('view','mask'     ,IntToStr(view_mask));

   FrmVipParam.InitCfg;

   {$IFDEF VOR_RPG}
   FrmVorRpg.InitCfg;
   {$ENDIF}

   FrmSubParam.InitCfg;

   FrmUnSubParam.InitCfg;

   FrmVolParam.InitCfg;

   FrmYtts.InitCfg;

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
 {$I PlayControl.lrs}

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

 {$IFDEF VOR_RPG}
 //vor rpg param
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Вор Рпг';
 Item.OnClick:=@OnPopupClickVorRpgParam;
 PopupCfg.Items.Add(Item);
 {$ENDIF}

 //sub param
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Саб мод';
 Item.OnClick:=@OnPopupClickSubParam;
 PopupCfg.Items.Add(Item);

 //unsub param
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Анcаб мод';
 Item.OnClick:=@OnPopupClickUnSubParam;
 PopupCfg.Items.Add(Item);

 //------
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='-';
 PopupCfg.Items.Add(Item);

 //sound volume
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Регулятор звука';
 Item.OnClick:=@OnPopupClickVolParam;
 PopupCfg.Items.Add(Item);

 //ytts
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Настройки TTS';
 Item.OnClick:=@OnPopupClickYttsParam;
 PopupCfg.Items.Add(Item);

 //------
 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='-';
 PopupCfg.Items.Add(Item);

 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='Экспорт истории в xls';
 Item.OnClick:=@OnExortClick;
 PopupCfg.Items.Add(Item);

 Item:=TMenuItem.Create(PopupCfg);
 Item.Caption:='-';
 PopupCfg.Items.Add(Item);

 Item_CheckUpdate:=TMenuItem.Create(PopupCfg);
 Item_CheckUpdate.Caption:='Проверка обновлений';
 Item_CheckUpdate.OnClick:=@OnPopupClickCheckUpdate;
 PopupCfg.Items.Add(Item_CheckUpdate);

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
  Item_CheckUpdate.Checked:=Trim(Config.ReadString('view','check_update','1'))='1';
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

 AIMAGE:=TImageList.Create(FrmMain);
 AIMAGE.Width:=28;
 AIMAGE.Height:=28;
 AIMAGE.AddLazarusResource('APAUSE');
 AIMAGE.AddLazarusResource('APLAY');

 BtnView:=TButton.Create(LeftBar);
 BtnView.Caption:='Вид';
 BtnView.AutoSize:=True;
 BtnView.Constraints.MinHeight:=32;
 BtnView.Height:=32;
 BtnView.Parent:=LeftBar;

 BtnPlay:=TStateSpeedButton.Create(LeftBar);
 BtnPlay.Items.Add('Остановить ');
 BtnPlay.Items.Add('Возобновить ');

 BtnPlay.Images:=AIMAGE;
 BtnPlay.Layout:=blGlyphRight;
 BtnPlay.Spacing:=0;
 BtnPlay.Margin:=0;

 BtnPlay.OnClick:=@OnBtnPlayClick;
 BtnPlay.AutoSize:=False;
 BtnPlay.Constraints.MinHeight:=32;
 BtnPlay.Width:=110;
 BtnPlay.Height:=32;
 BtnPlay.Parent:=LeftBar;

 BtnPlay.CaptionState:=0;

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
 GridStory.AddColumn('rew'     ,' Награда');
 GridStory.AddColumn('mes'     ,' Сообщение');
 GridStory.AddColumn('cmd'     ,' Команда');



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

 if Item_CheckUpdate.Checked then
  SendReleasesRequest;

 ytts.Fpool:=@pool;
 ytts.FAudioConnection:=@FAudioThread;
end;

procedure SetDBParam(Const fname,fvalue:RawByteString);
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

procedure GetDBParam(Const fname:RawByteString;N:TNotifyTask);
Var
 FDbcScript:TDbcStatementScript;
begin
 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,N);
 FDbcScript.SetSctipt(FGetParamScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('name',fname);
 FDbcScript.Start;
 FDbcScript.Release;
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
 TOpenSQL_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
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
  'export_story':
   begin
    Node.Push(TLoadSQL_Func,@FExportStoryScript);
   end;
  'insert_story':
   begin
    Node.Push(TLoadSQL_Func,@frmMain.FInsertStoryScript);
   end;
  'get_param':
   begin
    Node.Push(TLoadSQL_Func,@FGetParamScript);
   end;
  'set_param':
   begin
    Node.Push(TLoadSQL_Func,@FSetParamScript);
   end;
  'list_vips':
   begin
    Node.Push(TLoadSQL_Func,@FListVipsScript);
   end;
  'add_vips':
   begin
    Node.Push(TLoadSQL_Func,@FAddVipsScript);
   end;
  'insert_vips':
   begin
    Node.Push(TLoadSQL_Func,@FInsertVipsScript);
   end;
  'update_vips':
   begin
    Node.Push(TLoadSQL_Func,@FUpdateVipsScript);
   end;
  'delete_vips':
   begin
    Node.Push(TLoadSQL_Func,@FDeleteVipsScript);
   end;
  {$IFDEF VOR_RPG}
  'get_rpg_top':
   begin
    Node.Push(TLoadSQL_Func,@FGetRpgTop);
   end;
  'get_rpg_rank':
   begin
    Node.Push(TLoadSQL_Func,@FGetRpgRank);
   end;
  'get_rpg_user1':
   begin
    Node.Push(TLoadSQL_Func,@FGetRpgUser1);
   end;
  'get_rpg_user2':
   begin
    Node.Push(TLoadSQL_Func,@FGetRpgUser2);
   end;
  'get_rnd_user1':
   begin
    Node.Push(TLoadSQL_Func,@FGetRndUser1);
   end;
  'set_rpg_user1':
   begin
    Node.Push(TLoadSQL_Func,@FSetRpgUser1);
   end;
  'set_rpg_user2':
   begin
    Node.Push(TLoadSQL_Func,@FSetRpgUser2);
   end;
  {$ENDIF}
 end;
end;

procedure TFrmMain.LoadXML;
begin
 if not data_xml.LoadXML(GetLocalXml) then
 begin
  ShowMessage('Файл '+XmlName+' не найден!');
  Halt;
 end;
end;

initialization
 if not RegisterXMLNode('sql'    ,TOpenSQL_Func,nil) then Assert(False);
 if not RegisterXMLNode('chat'   ,TLoadStr_Func,@base.chat) then Assert(False);

end.




