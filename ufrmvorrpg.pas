unit UFrmVorRpg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
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
  vor_rpg:record
   Enable:Boolean;
   timeout_cmd:RawByteString;
   vor_sucs:TStringList;
   TickKd:Int64;
   time_kd:DWORD;

   jail_vip:TStringList;
   esc_vip:TStringList;
   str_vip:TStringList;
   norm_vor:TStringList;
   minus_vip:TStringList;
   time4_vip:TStringList;
   neudc_vip:TStringList;
   chist_vip:TStringList;

   stat_msg:record
    lvl_msg,
    pts_msg,
    stat_msg,
    add_msg,
    not_msg,
    max_msg,
    help_msg1,
    help_msg2:RawByteString;
   end;

   calc:record
    BASE_TIME:Int64;

    MUL_TIME:Double;
    DEC_TIME:Double;

    MAX_LVL:DWORD;
    MAX_LUK:DWORD;
    MAX_DEF:DWORD;
    MAX_CHR:DWORD;
    MAX_STR:DWORD;
    MAX_AGL:DWORD;

    MUL_EXP:Double;
    MUL_LUK:Double;
    MUL_DEF:Double;
    MUL_STR:Double;
    MUL_AGL:Double;

    DEC_LUK:Double;
    DEC_DEF:Double;
    DEC_STR:Double;
    DEC_AGL:Double;

    PERC_MINUS_VIP:Byte;
   end;

  end;

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

{Const
 BASE_TIME={4500}0;
 MUL_TIME=300;

 MAX_LVL=100;
 MUL_EXP=2;
 MUL_LUK=4;
 MUL_DEF=4;
 MUL_STR=4;
 MUL_AGL=5;

 DEC_LUK=-7;
 DEC_DEF=-6;
 DEC_STR=-6;
 DEC_AGL=5;

 PERCENT_MINUS_VIP=10;}

type
 Tcharacteristic=object
  PTS,STR,LUK,DEF,CHR,AGL:Int64;
  Procedure SumTo(var S:Tcharacteristic);
 end;

 Tdebuf=object
  id:Int64;
  chr:Tcharacteristic;
  text:RawByteString;
 end;

 TdebufCompare=class
  class function c(const a,b:Tdebuf):boolean; static;
 end;

 TdebufSet=specialize TSet<Tdebuf,TdebufCompare>;

 TUserPoints=object(Tcharacteristic)
  EXP,LVL:Int64;
  function  GetExpToLvl:Int64;
  procedure CheckNewLvl;
  procedure Load(J:TJson);
  procedure Save(var J:TJson);
 end;

 TPlayer=object
  Points:TUserPoints;
  Effects:Tcharacteristic;
  Function  GetLUKPercent:Int64;
  Function  GetDEFPercent:Int64;
  Function  GetSTRPercent:Int64;
  Function  GetESCPercent:Int64;
  Function  GetTime:Int64;
  procedure IncEXP(val:Int64);
  procedure Load(J:TJson);
  procedure Save(var J:TJson);
 end;

class function TdebufCompare.c(const a,b:Tdebuf):boolean;
begin
 Result:=(a.id<b.id);
end;

Procedure Tcharacteristic.SumTo(var S:Tcharacteristic);
begin
 S.PTS:=S.PTS+PTS;
 S.STR:=S.STR+STR;
 S.LUK:=S.LUK+LUK;
 S.DEF:=S.DEF+DEF;
 S.CHR:=S.CHR+CHR;
 S.AGL:=S.AGL+AGL;
end;

function TUserPoints.GetExpToLvl:Int64;
begin
 Result:=Trunc((LVL+1)*vor_rpg.calc.MUL_EXP-Log2((LVL+1)));
end;

procedure TUserPoints.CheckNewLvl;
var
 need:Int64;
begin
 repeat
  if (LVL>=vor_rpg.calc.MAX_LVL) then Break;
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

Function TPlayer.GetLUKPercent:Int64;
var
 LUK:Int64;
begin
 LUK:=Points.LUK+Effects.LUK;
 if (LUK<=0) then Exit(0);
 Result:=Trunc(Log2((LUK+1)*4-2)*vor_rpg.calc.MUL_LUK+vor_rpg.calc.DEC_LUK);
end;

Function TPlayer.GetDEFPercent:Int64;
var
 DEF:Int64;
begin
 DEF:=Points.DEF+Effects.DEF;
 if (DEF<=0) then Exit(0);
 Result:=Trunc(Log2((DEF+1)*4-2)*vor_rpg.calc.MUL_DEF+vor_rpg.calc.DEC_DEF);
end;

Function TPlayer.GetSTRPercent:Int64;
var
 STR:Int64;
begin
 STR:=Points.STR+Effects.STR;
 if (STR<=0) then Exit(0);
 Result:=Trunc(Log2((STR+1)*4-2)*vor_rpg.calc.MUL_STR+vor_rpg.calc.DEC_STR)
end;

Function TPlayer.GetESCPercent:Int64;
var
 AGL:Int64;
begin
 AGL:=Points.AGL+Effects.AGL;
 if (AGL<=0) then Exit(0);
 Result:=Trunc(Log2(AGL*3-2)*vor_rpg.calc.MUL_AGL+vor_rpg.calc.DEC_AGL);
end;

Function TPlayer.GetTime:Int64;
var
 CHR:Int64;
begin
 CHR:=Points.CHR+Effects.CHR;
 if (CHR<=0) then Exit(0);
 Result:=Trunc(Log2(CHR*3-2)*vor_rpg.calc.MUL_TIME+vor_rpg.calc.DEC_TIME);
end;

procedure TUserPoints.Load(J:TJson);
begin
 EXP:=J.Path['points.EXP'].AsInt64(0);
 LVL:=J.Path['points.LVL'].AsInt64(0);
 PTS:=J.Path['points.PTS'].AsInt64(0);
 STR:=J.Path['points.STR'].AsInt64(0);
 LUK:=J.Path['points.LUK'].AsInt64(0);
 DEF:=J.Path['points.DEF'].AsInt64(0);
 CHR:=J.Path['points.CHR'].AsInt64(0);
 AGL:=J.Path['points.AGL'].AsInt64(0);
end;

Procedure Save40nul(var J:TJson;const path:RawByteString;val:Int64);
begin
 if (val=0) then
 begin
  J.Delete(path);
 end else
 begin
  J.Path[path]:=TJson.New(val);
 end;
end;

procedure TUserPoints.Save(var J:TJson);
begin
 Save40nul(J,'points.EXP',EXP);
 Save40nul(J,'points.LVL',LVL);
 Save40nul(J,'points.PTS',PTS);
 Save40nul(J,'points.STR',STR);
 Save40nul(J,'points.LUK',LUK);
 Save40nul(J,'points.DEF',DEF);
 Save40nul(J,'points.CHR',CHR);
 Save40nul(J,'points.AGL',AGL);
end;

procedure TPlayer.IncEXP(val:Int64);
begin
 Points.EXP:=Points.EXP+val;
 Points.CheckNewLvl;
end;

var
 debufSet:TdebufSet;

function Get_debuf(id:Int64):Tdebuf;
var
 Node:debufSet.PNode;
begin
 Result:=Default(Tdebuf);
 Result.id:=id;
 if (debufSet=nil) then Exit;
 Node:=debufSet.NFind(Result);
 if (Node=nil) then Exit;
 Result:=Node^.Data;
end;

procedure TPlayer.Load(J:TJson);
Var
 instance:TJson;
 i,C:SizeUint;
 now,time:Int64;
 debuf:Tdebuf;
begin
 Points.Load(J);
 Points.CheckNewLvl;

 Effects:=Default(Tcharacteristic);

 instance:=J.Path['debuf.instance'];

 C:=instance.Count;
 if (C<>0) then
 begin
  now:=DateTimeToUnix(sysutils.Now,False);
  For i:=(C-1) downto 0 do
  begin
   time:=instance.Item[i].Path['time'].AsInt64(0);
   if (now>=time) then
   begin
    instance.Delete(i);
   end else
   begin
    debuf:=Get_debuf(instance.Item[i].Path['id'].AsInt64(0));
    debuf.chr.SumTo(Effects);
   end;
  end;
 end;

end;

procedure TPlayer.Save(var J:TJson);
begin
 Points.Save(J);
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

function get_random_msg(L:TStringList):RawByteString;
begin
 Result:='';
 if L.Count>0 then
  Result:=L.Strings[Random(RCT,L.Count)];
end;

Procedure TVorScript.OnEvent;
var
 cmd:RawByteString;
 Points1,Points2:TPlayer;
 rnd:Integer;
 Val:Int64;
begin
 Points1.Load(data1);
 Points2.Load(data2);
 if (FrmVipParam.FindVipUser(user1)<>-1) then
 begin
  rnd:=Random(RCT,100);
  if (rnd<vor_rpg.calc.PERC_MINUS_VIP) then
  begin

   cmd:=get_random_msg(vor_rpg.minus_vip);

   cmd:=Format(cmd,[user1]);
   push_irc_msg(cmd);

   FrmVipParam.DeleteAndUnVip(user1);

   FrmMain._add_reward_2_log(s,cmd);
   Points1.IncExp(1);
  end else
  begin
   rnd:=Random(RCT,100);
   if (rnd<Points1.GetLUKPercent) then
   begin

    cmd:=get_random_msg(vor_rpg.chist_vip);

    cmd:=Format(cmd,[user1]);
    push_irc_msg(cmd);
    FrmMain._add_reward_2_log(s,cmd);
    Points1.IncExp(1);
   end else
   begin
    Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);

    cmd:=get_random_msg(vor_rpg.neudc_vip);

    cmd:=Format(cmd,[user1]);
    push_irc_msg(cmd);
    if Val<>0 then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
    end;
    FrmMain._add_reward_2_log(s,cmd);
    Points1.IncExp(2);
   end;
  end;
  Points1.Save(data1);
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

    cmd:=get_random_msg(vor_rpg.norm_vor);

    cmd:=Format(cmd,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Points1.IncExp(5);
    Points2.IncExp(1);

   end else
   begin
    //change vip and timeout

    cmd:=get_random_msg(vor_rpg.time4_vip);

    cmd:=Format(cmd,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);
    if Val<>0 then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
    end;

    Points1.IncExp(4);
    Points2.IncExp(1);

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

    cmd:=get_random_msg(vor_rpg.str_vip);

    cmd:=Format(cmd,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Points1.IncExp(3);
    Points2.IncExp(2);

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

     cmd:=get_random_msg(vor_rpg.esc_vip);

     cmd:=Format(cmd,[user1,user2]);
     push_irc_msg(cmd);

     Points1.IncExp(3);
     Points2.IncExp(1);

    end else
    begin
     //jail

     cmd:=get_random_msg(vor_rpg.jail_vip);

     cmd:=Format(cmd,[user1,user2]);
     push_irc_msg(cmd);

     Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);
     if Val<>0 then
     begin
      push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
     end;

     Points1.IncExp(1);
     Points2.IncExp(2);

    end;


   end;

  end;

  Points1.Save(data1);
  Points2.Save(data2);
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

procedure TDbcGetUserInfo.Print;
var
 Points:TPlayer;
begin
 Points.Load(data);
 case cmd of
  'level',
  'lvl' :push_irc_msg(Format(vor_rpg.stat_msg.lvl_msg,[user,
                                      IntToStr(Points.Points.LVL),
                                      IntToStr(Points.Points.EXP),
                                      IntToStr(Points.Points.GetExpToLvl)]));
  'points',
  'pts' :push_irc_msg(Format(vor_rpg.stat_msg.pts_msg,[user,
                                      IntToStr(Points.Points.LUK),
                                      IntToStr(Points.Points.DEF),
                                      IntToStr(Points.Points.CHR),
                                      IntToStr(Points.Points.AGL),
                                      IntToStr(Points.Points.STR),
                                      IntToStr(Points.Points.PTS)]));
  'stats',
  'stat':push_irc_msg(Format(vor_rpg.stat_msg.stat_msg,[user,
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
 do_inc:Boolean;

 function try_inc(var param:Int64;var max:DWORD):Boolean;
 begin
  Result:=(param<max);
  if Result then
  begin
   Inc(param);
  end;
 end;

begin
 Points1.Load(data1);

 if (Points1.PTS>0) then
 begin
  Case cmd of
   'luk':do_inc:=try_inc(Points1.LUK,vor_rpg.calc.MAX_LUK);
   'def':do_inc:=try_inc(Points1.DEF,vor_rpg.calc.MAX_DEF);
   'chr':do_inc:=try_inc(Points1.CHR,vor_rpg.calc.MAX_CHR);
   'agl':do_inc:=try_inc(Points1.AGL,vor_rpg.calc.MAX_AGL);
   'str':do_inc:=try_inc(Points1.STR,vor_rpg.calc.MAX_STR);
  end;

  if do_inc then
  begin
   Dec(Points1.PTS);
   push_irc_msg(Format(vor_rpg.stat_msg.add_msg,[user1,cmd]));
  end else
  begin
   push_irc_msg(Format(vor_rpg.stat_msg.max_msg,[user1,cmd]));
  end;

 end else
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.not_msg,[user1,cmd]));
 end;

 Points1.Save(data1);
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

//А ну да, чисто подкрался незаметно со спины, но споткнулся и упал на свой нож отлетев в бан KEKW

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
   push_irc_msg(Format(vor_rpg.stat_msg.help_msg1,[user]));
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
            push_irc_msg(Format(vor_rpg.stat_msg.help_msg2,[user]));
          end;
    else
     push_irc_msg(Format(vor_rpg.stat_msg.help_msg1,[user]));
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
 debufSet:=TdebufSet.Create;
 vor_rpg.time_kd:=8;

end.

