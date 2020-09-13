unit UFrmVorRpg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  TaskManager,DbcEngine,DbcScript,Main;

type

  { TFrmVorRpg }

  TFrmVorRpg = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBVorRpgEnable: TCheckBox;
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
  private
  public
    Procedure rpg_theif_vip(const s,dst_user,msg:RawByteString);
    procedure add_to_chat_cmd(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
  end;

var
  FrmVorRpg: TFrmVorRpg;

  FGetRpgUser1:TSQLScript;
  FGetRpgUser2:TSQLScript;

  FSetRpgUser1:TSQLScript;
  FSetRpgUser2:TSQLScript;

implementation

uses
 Ulog,UFrmVipParam,
 math,
 mtRandom,ujson,gset;

{$R *.lfm}

type

 TRawStrCompare=class
  class function c(const a,b:RawByteString):boolean; static;
 end;

 TRawByteStringSet=specialize TSet<RawByteString,TRawStrCompare>;

 TLockScript=class
  protected
   Function  try_lock:Boolean; virtual; abstract;
   procedure unlock;           virtual; abstract;
   procedure Prepare(FDbcScript:TDbcStatementScript); virtual; abstract;
   procedure OnFin(ResultSet:TZResultSet); virtual; abstract;
   procedure OnUnlock(Sender:TBaseTask);
  public
   Procedure OnEvent; virtual; abstract;
 end;

 TDualLockScript=class(TLockScript)
  public
   user1,user2:RawByteString;
   data1,data2:TJson;
   Destructor Destroy; override;
  protected
   Function   try_lock:Boolean; override;
   procedure  unlock;           override;
   procedure  Prepare(FDbcScript:TDbcStatementScript); override;
   procedure  OnFin(ResultSet:TZResultSet); override;
 end;

 TOneLockScript=class(TLockScript)
  public
   user1:RawByteString;
   data1:TJson;
   Destructor Destroy; override;
  protected
   Function   try_lock:Boolean; override;
   procedure  unlock;           override;
   procedure  Prepare(FDbcScript:TDbcStatementScript); override;
   procedure  OnFin(ResultSet:TZResultSet); override;
 end;

 TDbcScriptLock=class(TDbcStatementScript)
  LS:TLockScript;
  Procedure OnFin(Sender:TBaseTask);
  procedure Prepare(FS:TLockScript);
  function  try_start:SizeInt;
 end;

Var
 LockStr:TRawByteStringSet;

class function TRawStrCompare.c(const a,b:RawByteString):boolean;
begin
 Result:=CompareStr(a,b)<0;
end;

procedure TLockScript.OnUnlock(Sender:TBaseTask);
begin
 unlock;
 Free;
end;

//TDualLockScript

Function  TDualLockScript.try_lock:Boolean;
begin
 Result:=((user1='') or (LockStr.NFind(user1)=nil)) and
         ((user2='') or (LockStr.NFind(user2)=nil));
 if Result then
 begin
  if (user1<>'') then
   LockStr.Insert(user1);
  if (user2<>'') then
   LockStr.Insert(user2);
 end;
end;

procedure TDualLockScript.unlock;
begin
 LockStr.Delete(user1);
 LockStr.Delete(user2);
end;

procedure TDualLockScript.Prepare(FDbcScript:TDbcStatementScript);
begin
 FDbcScript.SetSctipt(FGetRpgUser2);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user1',user1);
 FDbcScript.Params.SetRawByteString('user2',user2);
end;

procedure TDualLockScript.OnFin(ResultSet:TZResultSet);
Var
 ms:TStream;
 user_f,data_f:SizeInt;
 u,d:RawByteString;
 i,c:SizeInt;
begin
 if ResultSet<>nil then
 begin
  c:=0;
  if ResultSet.Last then
  begin
   c:=ResultSet.GetRow;
  end;
  if c>0 then
  begin
   user_f:=ResultSet.FindColumn('user');
   data_f:=ResultSet.FindColumn('data');
   For i:=1 to c do
   begin
    ResultSet.MoveAbsolute(i);

    u:=ResultSet.GetRawByteString(user_f);
    d:=ResultSet.GetRawByteString(data_f);

    if user1=u then
    begin
     ms:=TPCharStream.Create(PAnsiChar(d),Length(d));
     data1:=Default(TJson);
     try
      data1:=TJson.New(ms);
     except
      on E:Exception do
      begin
       DumpExceptionCallStack(E);
      end;
     end;
     FreeAndNil(ms);
    end else
    if user2=u then
    begin
     ms:=TPCharStream.Create(PAnsiChar(d),Length(d));
     data2:=Default(TJson);
     try
      data2:=TJson.New(ms);
     except
      on E:Exception do
      begin
       DumpExceptionCallStack(E);
      end;
     end;
     FreeAndNil(ms);
    end;

   end;
  end;
 end;

 OnEvent;
end;

Destructor TDualLockScript.Destroy;
begin
 data1.Free;
 data2.Free;
 inherited;
end;

//TDualLockScript

//TOneLockScript

Function  TOneLockScript.try_lock:Boolean;
begin
 Result:=((user1='') or (LockStr.NFind(user1)=nil));
 if Result then
 begin
  if (user1<>'') then
   LockStr.Insert(user1);
 end;
end;

procedure TOneLockScript.unlock;
begin
 LockStr.Delete(user1);
end;

procedure TOneLockScript.Prepare(FDbcScript:TDbcStatementScript);
begin
 FDbcScript.SetSctipt(FGetRpgUser1);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user1',user1);
end;

procedure TOneLockScript.OnFin(ResultSet:TZResultSet);
Var
 ms:TStream;
 user_f,data_f:SizeInt;
 u,d:RawByteString;
 i,c:SizeInt;
begin
 if ResultSet<>nil then
 begin
  c:=0;
  if ResultSet.Last then
  begin
   c:=ResultSet.GetRow;
  end;
  if c>0 then
  begin
   user_f:=ResultSet.FindColumn('user');
   data_f:=ResultSet.FindColumn('data');
   For i:=1 to c do
   begin
    ResultSet.MoveAbsolute(i);

    u:=ResultSet.GetRawByteString(user_f);
    d:=ResultSet.GetRawByteString(data_f);

    if user1=u then
    begin
     ms:=TPCharStream.Create(PAnsiChar(d),Length(d));
     data1:=Default(TJson);
     try
      data1:=TJson.New(ms);
     except
      on E:Exception do
      begin
       DumpExceptionCallStack(E);
      end;
     end;
     FreeAndNil(ms);
    end;

   end;
  end;
 end;

 OnEvent;
end;

Destructor TOneLockScript.Destroy;
begin
 data1.Free;
 inherited;
end;

//TOneLockScript

Procedure TDbcScriptLock.OnFin(Sender:TBaseTask);
begin
 LS.OnFin(ResultSet);
end;

procedure TDbcScriptLock.Prepare(FS:TLockScript);
begin
 if (FS=nil) then Exit;
 LS:=FS;
 Handle.DbcConnection:=DbcThread;
 Notify.Add(T_FIN,@OnFin);
 LS.Prepare(Self);
end;

function TDbcScriptLock.try_start:SizeInt;
begin
 Result:=0;
 if LS.try_lock then
 begin
  Start;
  Release;
 end else
 begin
  AsyncFunc(@Self.try_start);
 end;
end;

procedure SetDBRpgUser1(Const user1:RawByteString;data1:TJson;N:TNotifyTask);
var
 FDbcScript:TDbcStatementScript;
 ms:TRawByteStringStream;
 d1:RawByteString;
begin

 ms:=TRawByteStringStream.Create;
 data1.Dump(ms);
 d1:=ms.DataString;
 FreeAndNil(ms);

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,N);
 FDbcScript.SetSctipt(FSetRpgUser1);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user1',user1);
 FDbcScript.Params.SetRawByteString('data1',d1);
 FDbcScript.Start;
 FDbcScript.Release;
end;

procedure SetDBRpgUser2(Const user1,user2:RawByteString;data1,data2:TJson;N:TNotifyTask);
var
 FDbcScript:TDbcStatementScript;
 ms:TRawByteStringStream;
 d1,d2:RawByteString;
begin

 ms:=TRawByteStringStream.Create;
 data1.Dump(ms);
 d1:=ms.DataString;
 ms.Clear;
 data2.Dump(ms);
 d2:=ms.DataString;
 FreeAndNil(ms);

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,N);
 FDbcScript.SetSctipt(FSetRpgUser2);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user1',user1);
 FDbcScript.Params.SetRawByteString('data1',d1);
 FDbcScript.Params.SetRawByteString('user2',user2);
 FDbcScript.Params.SetRawByteString('data2',d2);
 FDbcScript.Start;
 FDbcScript.Release;
end;

type
 TVorScript=class(TDualLockScript)
  public
   s:RawByteString;
   Procedure OnEvent; override;
 end;

Procedure TFrmVorRpg.rpg_theif_vip(const s,dst_user,msg:RawByteString);
Var
 FDbcScript:TDbcScriptLock;
 src_user:RawByteString;
 aRow:SizeInt;
 FVorScript:TVorScript;
begin
 aRow:=FrmMain.getRandomTmpVip(Extract_nick(msg));
 if (aRow=-1) then
 begin
  push_irc_list(vip_rnd.is_empty,[dst_user]);
  FrmMain._add_reward_2_log(s,Format(_get_first_cmd(vip_rnd.is_empty),[dst_user]));
  Exit;
 end;
 src_user:=GridVips.FieldValue['user',ARow];

 FVorScript:=TVorScript.Create;
 FVorScript.user1:=dst_user;
 FVorScript.user2:=src_user;
 FVorScript.s:=s;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FVorScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);

end;

Const
 BASE_TIME={4500}0;
 MUL_TIME=300;
 MAX_LVL=100;
 MUL_EXP=2;
 MUL_LUK=4;
 MUL_AGL=5;

type
 TUserPoints=object
  EXP,LVL,PTS,STR,LUK,DEF,CHR,AGL:Int64;
  function  GetExpToLvl:Int64;
  procedure CheckNewLvl;
  Function  GetLUKPercent:Int64;
  Function  GetDEFPercent:Int64;
  Function  GetSTRPercent:Int64;
  Function  GetESCPercent:Int64;
  Function  GetTime:Int64;
 end;

function TUserPoints.GetExpToLvl:Int64;
begin
 Result:=Trunc((LVL+1)*MUL_EXP-Log2((LVL+1)));
end;

procedure TUserPoints.CheckNewLvl;
var
 need:Int64;
begin
 repeat
  if (LVL>=MAX_LVL) then Break;
  need:=GetExpToLvl;
  if (EXP>=need) then
  begin
   Inc(LVL);
   Inc(PTS);
   EXP:=EXP-need;
  end else
  begin
   Break;
  end;
 until false;
end;

Function TUserPoints.GetLUKPercent:Int64;
begin
 if (LUK=0) then Exit(0);
 Result:=Trunc(Log2((LUK+1)*4-2)*MUL_LUK-6);
end;

Function TUserPoints.GetDEFPercent:Int64;
begin
 if (DEF=0) then Exit(0);
 Result:=Trunc(Log2((DEF+1)*4-2)*MUL_LUK-6);
end;

Function TUserPoints.GetSTRPercent:Int64;
begin
 if (STR=0) then Exit(0);
 Result:=Trunc(Log2((STR+1)*4-2)*MUL_LUK-6)
end;

Function TUserPoints.GetESCPercent:Int64;
begin
 if (AGL=0) then Exit(0);
 Result:=Trunc(Log2(AGL*3-2)*MUL_AGL+5);
end;

Function TUserPoints.GetTime:Int64;
begin
 if (CHR=0) then Exit(0);
 Result:=Trunc(Log2(CHR*3-2)*MUL_TIME+MUL_TIME);
end;

Function GetUserPoints(J:TJson):TUserPoints;
begin
 Result.EXP:=J.Path['points.EXP'].AsInt64(0);
 Result.LVL:=J.Path['points.LVL'].AsInt64(0);
 Result.PTS:=J.Path['points.PTS'].AsInt64(0);
 Result.STR:=J.Path['points.STR'].AsInt64(0);
 Result.LUK:=J.Path['points.LUK'].AsInt64(0);
 Result.DEF:=J.Path['points.DEF'].AsInt64(0);
 Result.CHR:=J.Path['points.CHR'].AsInt64(0);
 Result.AGL:=J.Path['points.AGL'].AsInt64(0);
end;

Procedure SetUserPoints(var J:TJson;Const P:TUserPoints);
begin
 J.Path['points.EXP']:=TJson.New(P.EXP);
 J.Path['points.LVL']:=TJson.New(P.LVL);
 J.Path['points.PTS']:=TJson.New(P.PTS);
 J.Path['points.STR']:=TJson.New(P.STR);
 J.Path['points.LUK']:=TJson.New(P.LUK);
 J.Path['points.DEF']:=TJson.New(P.DEF);
 J.Path['points.CHR']:=TJson.New(P.CHR);
 J.Path['points.AGL']:=TJson.New(P.AGL);
end;

procedure ChangeVip(const src_user,dst_user:RawByteString);
var
 aRow:SizeInt;
begin
 aRow:=FrmVipParam.FindVipUser(src_user);
 FrmVipParam.DbUpdateVip_user(src_user,dst_user);
 if (aRow<>-1) then
 begin
  GridVips.FieldValue['user',ARow]:=dst_user;
 end;
 push_irc_list(vor_rpg.vor_sucs,[dst_user,src_user]);
end;

Const
 minus_vip='Невнимательный %s по дороге домой выронил из кармана випку';
 chist_vip='Чистюля %s хотел пойти на дело но вспомнил что забыл помыть руки';
 neudc_vip='Неудачник %s споткнулся и улетел в открытый люк канализации karmikRip';

 pride_vip1='Обаятельный %s в общественной бане выронил из рук мыло karmikPride';
 pride_vip2='Обаятельный %s в общественной бане выронил из рук випку karmikFeels';

 time4_vip='Неудачник %s успешно ушёл от полиции вместе с випкой от %s но повредил себе колено';

 norm_vor1='Вор %s в тишине ночи, забрал випку у %s и не был пойман. karmikThief';
 norm_vor2='Ловкими пальчиками вор %s ловко стащил випку прямо вместе со штанами у %s';

 str_vip1='Скрипнула половица, вор %s был замечен, но успел ударить по голове %s и cбежать с випкой';
 str_vip2='Неуклюжий вор %s разбудил хозяина %s но в неравной борьбе он смог забрать чужую випку';
 str_vip3='Задев скатерть ногой, %s снес половину бабушкиного сервиза, но всё же смог забрать силой випку у %s';

 esc_vip1='Ловкий вор %s хоть и не смог добиться цели, но смог cбежать от полиции.';
 esc_vip2='Неуклюжий вор %s прыгнул в реку Турчанку и смог смыться';
 esc_vip3='Спрятавшись в лесу %s смог избежать погони';

 jail_vip1='Поместье недружелюбно встретило %s засадой полиции в темноте karmikT';
 jail_vip2='Темнота амбара недружелюбно встретила %s ударом дубинки по голове karmikRip';
 jail_vip3='Задев скатерть ногой, %s снес половину бабушкиного сервиза, и был вырублен %s и доставлен в полицию karmikT';
 jail_vip4='Неудачник %s попробовав вскрыть замок был замечен %s и в неравной борьбе был доставлен в участок karmikT';
 jail_vip5='Ловкий %s вскрыл сейф %s, но нашел только бан karmikT';

Procedure TVorScript.OnEvent;
var
 cmd:RawByteString;
 Points1,Points2:TUserPoints;
 rnd:Integer;
 Val:Int64;
begin
 Points1:=GetUserPoints(data1);
 Points2:=GetUserPoints(data2);
 Points1.CheckNewLvl;
 Points2.CheckNewLvl;
 if (FrmVipParam.FindVipUser(user1)<>-1) then
 begin
  rnd:=Random(RCT,100);
  if (rnd<10) then
  begin

   Case Random(RCT,2) of
    0:cmd:=minus_vip;
    1:cmd:=pride_vip2;
   end;

   cmd:=Format(cmd,[user1]);
   push_irc_msg(cmd);

   FrmVipParam.DeleteAndUnVip(user1);

   FrmMain._add_reward_2_log(s,cmd);
   Points1.EXP:=Points1.EXP+1;
  end else
  begin
   rnd:=rnd-10;
   if (rnd<Points1.GetLUKPercent) then
   begin
    cmd:=Format(chist_vip,[user1]);
    push_irc_msg(cmd);
    FrmMain._add_reward_2_log(s,cmd);
    Points1.EXP:=Points1.EXP+3;
   end else
   begin
    Val:=Max(BASE_TIME-Points1.GetTime,0);

    Case Random(RCT,2) of
     0:cmd:=neudc_vip;
     1:cmd:=pride_vip1;
    end;

    cmd:=Format(cmd,[user1]);
    push_irc_msg(cmd);
    if Val<>0 then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
    end;
    FrmMain._add_reward_2_log(s,cmd);
    Points1.EXP:=Points1.EXP+2;
   end;
  end;
  Points1.CheckNewLvl;
  SetUserPoints(data1,Points1);
  SetDBRpgUser1(user1,data1,@OnUnlock);
 end else
 begin
  Val:=(vip_rnd.perc_vor+Points1.GetLUKPercent-Points2.GetDEFPercent);
  Val:=Max(Val,0);
  Val:=Min(Val,100);

  rnd:=Random(RCT,100);

  if (rnd<Val) then
  begin

   Val:=(1+Points1.GetLUKPercent);
   Val:=Max(Val,0);
   Val:=Min(Val,100);

   rnd:=Random(RCT,100);

   if (rnd<Val) then
   begin
    //norm_vor

    Case Random(RCT,2) of
     0:cmd:=norm_vor1;
     1:cmd:=norm_vor2;
    end;

    cmd:=Format(cmd,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Points1.EXP:=Points1.EXP+5;
    Points2.EXP:=Points2.EXP+1;

   end else
   begin
    //change vip and timeout

    cmd:=Format(time4_vip,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Val:=Max(BASE_TIME-Points1.GetTime,0);
    if Val<>0 then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
    end;

    Points1.EXP:=Points1.EXP+4;
    Points2.EXP:=Points2.EXP+1;

   end;

  end else
  begin //str

   Val:=(vip_rnd.perc_vor+Points1.GetSTRPercent-Points2.GetDEFPercent);
   Val:=Max(Val,0);
   Val:=Min(Val,100);

   rnd:=Random(RCT,100);

   if (rnd<Val) then
   begin
    //str_vip

    Case Random(RCT,3) of
     0:cmd:=str_vip1;
     1:cmd:=str_vip2;
     2:cmd:=str_vip3;
    end;

    cmd:=Format(cmd,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Points1.EXP:=Points1.EXP+3;
    Points2.EXP:=Points2.EXP+2;

   end else
   begin
    //escape

    Val:=Points1.GetESCPercent;
    Val:=Max(Val,0);
    Val:=Min(Val,100);

    rnd:=Random(RCT,100);

    if (rnd<Val) then
    begin
     //escape

     Case Random(RCT,3) of
      0:cmd:=esc_vip1;
      1:cmd:=esc_vip2;
      2:cmd:=esc_vip3;
     end;

     cmd:=Format(cmd,[user1,user2]);
     push_irc_msg(cmd);

     Points1.EXP:=Points1.EXP+3;
     Points2.EXP:=Points2.EXP+1;

    end else
    begin
     //jail

     Case Random(RCT,5) of
      0:cmd:=jail_vip1;
      1:cmd:=jail_vip2;
      2:cmd:=jail_vip3;
      3:cmd:=jail_vip4;
      4:cmd:=jail_vip5;
     end;

     cmd:=Format(cmd,[user1,user2]);
     push_irc_msg(cmd);

     Val:=Max(BASE_TIME-Points1.GetTime,0);
     if Val<>0 then
     begin
      push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
     end;

     Points1.EXP:=Points1.EXP+1;
     Points2.EXP:=Points2.EXP+2;

    end;


   end;

  end;

  Points1.CheckNewLvl;
  Points2.CheckNewLvl;
  SetUserPoints(data1,Points1);
  SetUserPoints(data2,Points2);
  SetDBRpgUser2(user1,user2,data1,data2,@OnUnlock);

 end;

 FrmMain._add_reward_2_log(s,cmd);
end;

type
 TDbcGetUserInfo=class(TDbcStatementScript)
  user,cmd:RawByteString;
  data:TJson;
  Procedure Cleanup; override;
  Procedure OnFin(Sender:TBaseTask);
  procedure Print;
 end;

procedure GetDBRpgUserInfo(Const user,cmd:RawByteString);
var
 FDbcScript:TDbcGetUserInfo;
begin
 FDbcScript:=TDbcGetUserInfo.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@FDbcScript.OnFin);
 FDbcScript.SetSctipt(FGetRpgUser1);
 FDbcScript.ExecuteScript;
 FDbcScript.user:=user;
 FDbcScript.cmd:=cmd;
 FDbcScript.Params.SetRawByteString('user1',user);
 FDbcScript.Start;
 FDbcScript.Release;
end;

Procedure TDbcGetUserInfo.Cleanup;
begin
 data.Free;
 inherited;
end;

Procedure TDbcGetUserInfo.OnFin(Sender:TBaseTask);
Var
 ms:TStream;
 user_f,data_f:SizeInt;
 u,d:RawByteString;
 i,c:SizeInt;
begin
 if ResultSet<>nil then
 begin
  c:=0;
  if ResultSet.Last then
  begin
   c:=ResultSet.GetRow;
  end;
  if c>0 then
  begin
   user_f:=ResultSet.FindColumn('user');
   data_f:=ResultSet.FindColumn('data');
   For i:=1 to c do
   begin
    ResultSet.MoveAbsolute(i);

    u:=ResultSet.GetRawByteString(user_f);
    d:=ResultSet.GetRawByteString(data_f);

    if user=u then
    begin
     ms:=TPCharStream.Create(PAnsiChar(d),Length(d));
     data:=Default(TJson);
     try
      data:=TJson.New(ms);
     except
      on E:Exception do
      begin
       DumpExceptionCallStack(E);
      end;
     end;
     FreeAndNil(ms);
    end;

   end;
  end;
 end;

 Print;
end;

Const
 lvl_msg='@%s LVL:%s [%s/%s]';
 pts_msg='@%s LUK:%s;DEF:%s;CHR:%s;AGL:%s;STR:%s;PTS:%s';
 stat_msg='@%s LUK%%:%s;DEF%%:%s;ESC%%:%s;-TIME:%s';
 add_msg='@%s point add to %s';
 not_pts_msg='@%s not enough points';

procedure TDbcGetUserInfo.Print;
var
 Points:TUserPoints;
begin
 Points:=GetUserPoints(data);
 Points.CheckNewLvl;
 case cmd of
  'level',
  'lvl' :push_irc_msg(Format(lvl_msg,[user,
                                      IntToStr(Points.LVL),
                                      IntToStr(Points.EXP),
                                      IntToStr(Points.GetExpToLvl)]));
  'points',
  'pts' :push_irc_msg(Format(pts_msg,[user,
                                      IntToStr(Points.LUK),
                                      IntToStr(Points.DEF),
                                      IntToStr(Points.CHR),
                                      IntToStr(Points.AGL),
                                      IntToStr(Points.STR),
                                      IntToStr(Points.PTS)]));
  'stats',
  'stat':push_irc_msg(Format(stat_msg,[user,
                                      IntToStr(Points.GetLUKPercent),
                                      IntToStr(Points.GetDEFPercent),
                                      IntToStr(Points.GetESCPercent),
                                      IntToStr(Points.GetTime)]));
 end;
end;

type
 TAddPtsScript=class(TOneLockScript)
  public
   cmd:RawByteString;
   Procedure OnEvent; override;
 end;

Procedure TAddPtsScript.OnEvent;
var
 Points1:TUserPoints;
begin
 Points1:=GetUserPoints(data1);
 Points1.CheckNewLvl;

 if (Points1.PTS>0) then
 begin
  Dec(Points1.PTS);
  Case cmd of
   'luk':Inc(Points1.LUK);
   'def':Inc(Points1.DEF);
   'chr':Inc(Points1.CHR);
   'agl':Inc(Points1.AGL);
   'str':Inc(Points1.STR);
  end;
  push_irc_msg(Format(add_msg,[user1,cmd]));
 end else
 begin
  push_irc_msg(Format(not_pts_msg,[user1,cmd]));
 end;

 SetUserPoints(data1,Points1);
 SetDBRpgUser1(user1,data1,@OnUnlock);

end;

Procedure add_pts(Const user,cmd:RawByteString);
Var
 FDbcScript:TDbcScriptLock;
 FAddPtsScript:TAddPtsScript;
begin

 FAddPtsScript:=TAddPtsScript.Create;
 FAddPtsScript.user1:=user;
 FAddPtsScript.cmd:=cmd;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FAddPtsScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);

end;

procedure TFrmVorRpg.add_to_chat_cmd(PC:TPrivMsgCfg;const user,display_name,msg:RawByteString);
var
 F,v:RawByteString;
 i:Integer;
begin
 F:=LowerCase(Trim(msg));

 i:=Pos(' ',F);
 if (i<>0) then
 begin
  v:=Trim(Copy(F,1,i));
  F:=Trim(Copy(F,i+1));
 end else
 begin
  v:=F;
  F:='';
 end;

 if (v='!ban_test') then
 begin

  if (GetTickCount64<vor_rpg.TickKd+vor_rpg.time_kd*1000) then Exit;

  rpg_theif_vip('',user,F);

  vor_rpg.TickKd:=GetTickCount64;

  Exit;
 end;

 if (v='!vor') then
 begin

  if (GetTickCount64<vor_rpg.TickKd+vor_rpg.time_kd*1000) then Exit;

  if (F='') then
  begin
   push_irc_msg('@'+user+' cmd: [lvl,pts,stat,add [param]]');
  end else
  begin
   i:=Pos(' ',F);
   if (i<>0) then
   begin
    v:=Trim(Copy(F,1,i));
    F:=Trim(Copy(F,i+1));
   end else
   begin
    v:=F;
    F:='';
   end;

   case v of
    'level',
    'lvl' ,
    'points',
    'pts' ,
    'stats',
    'stat':GetDBRpgUserInfo(user,v);
    'add':Case F of
           'luk',
           'def',
           'chr',
           'agl',
           'str':add_pts(user,F);
           else
            push_irc_msg('@'+user+' add: [luk,def,chr,agl,str]');
          end;
    else
     push_irc_msg('@'+user+' cmd: [lvl,pts,stat,add [param]]');
   end;

  end;

  vor_rpg.TickKd:=GetTickCount64;
 end;

end;

Procedure TFrmVorRpg.InitCfg;
begin
 vor_rpg.Enable:=False;
 case vor_rpg.Enable of
  True :Config.WriteString('vor_rpg' ,'enable','1');
  False:Config.WriteString('vor_rpg' ,'enable','0');
 end;
end;

Procedure TFrmVorRpg.LoadCfg;
begin
 vor_rpg.Enable:=Trim(Config.ReadString('vor_rpg','enable','0'))='1';
end;

Procedure TFrmVorRpg.Open;
begin
 CBVorRpgEnable.Checked :=vor_rpg.Enable;

 if ShowModal=1 then
 begin
  vor_rpg.Enable:=CBVorRpgEnable.Checked;

  try

   case vor_rpg.Enable of
    True :Config.WriteString('vor_rpg' ,'enable','1');
    False:Config.WriteString('vor_rpg' ,'enable','0');
   end;

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmVorRpg.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmVorRpg.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmVorRpg.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmVorRpg.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

initialization
 LockStr:=TRawByteStringSet.Create;
 vor_rpg.time_kd:=5;

end.

