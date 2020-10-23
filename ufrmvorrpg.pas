unit UFrmVorRpg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, TaskManager, DbcEngine, DbcScript, Main;

type

  { TFrmVorRpg }

  TFrmVorRpg = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBKickEnable: TCheckBox;
    CBVorRpgEnable: TCheckBox;
    EdtBaseTime: TLabeledEdit;
    EdtDebufMaxTime: TLabeledEdit;
    EdtDebufMinTime: TLabeledEdit;
    EdtDebufPerc: TLabeledEdit;
    EdtKickIn: TLabeledEdit;
    EdtKickOut: TLabeledEdit;
    EdtKickPerc: TLabeledEdit;
    EdtPercMinusVip: TLabeledEdit;
    EdtTimeKd: TLabeledEdit;
    PageCtrl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure EdtPercMinusVipKeyPress(Sender:TObject;var Key:char);
    procedure EdtPercMinusVipExit(Sender:TObject);
    procedure EdtDwExit(Sender: TObject);
    procedure EdtDwKeyPress(Sender:TObject;var Key:char);
  private
    prev_perc:Byte;
    prev_dw:DWORD;
  public
    Procedure rpg_theif_vip(const s,dst_user,msg:RawByteString);
    procedure add_to_chat_cmd(PC:TPrivMsgCfg;const user,cmd,param:RawByteString);
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
    help_msg2,
    on_debuf,
    debuf_pr:RawByteString;
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

   debuf:record
    PERC:Byte;
    MIN_TIME:Int64;
    MAX_TIME:Int64;
   end;

   kick:record
    Enable:Boolean;
    in_time :Int64;
    out_time:Int64;
    PERC:Byte;
    not_vor:TStringList;
    go_kick:TStringList;
    go_def:TStringList;
    go_esc:TStringList;
   end;

  end;

  FrmVorRpg: TFrmVorRpg;

  FGetRpgUser1:TSQLScript;
  FGetRpgUser2:TSQLScript;

  FSetRpgUser1:TSQLScript;
  FSetRpgUser2:TSQLScript;

type
 Pcharacteristic=^Tcharacteristic;
 Tcharacteristic=object
  STR,LUK,DEF,CHR,AGL:Int64;
  Procedure SumTo(var S:Tcharacteristic);
 end;

 Pdebuf=^Tdebuf;
 Tdebuf=object
  id:Int64;
  chr:Tcharacteristic;
  text:RawByteString;
 end;

procedure add_debuf(d:Tdebuf);

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

//Const
// DEBUF_PERCENT=20;
//
// DEBUF_MIN_TIME=4500;
// DEBUF_MAX_TIME=86400;

type
 TdebufCompare=class
  class function c(const a,b:Tdebuf):boolean; static;
 end;

 TdebufSet=specialize TSet<Tdebuf,TdebufCompare>;

 TUserPoints=object(Tcharacteristic)
  EXP,LVL,PTS:Int64;
  function  GetExpToLvl:Int64;
  procedure CheckNewLvl;
  procedure CheckMaxPts;
  procedure Load(J:TJson);
  procedure Save(var J:TJson);
 end;

 TPlayer=object
  Points:TUserPoints;
  Effects:Tcharacteristic;
  Function  STR:Int64;
  Function  LUK:Int64;
  Function  DEF:Int64;
  Function  CHR:Int64;
  Function  AGL:Int64;
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

procedure TUserPoints.CheckMaxPts;

 procedure try_max(var param:Int64;var max:DWORD); inline;
 begin
  if (param>max) then
  begin
   PTS:=PTS+(param-max);
   param:=max;
  end;
 end;

begin
 if (PTS<0) then PTS:=0;
 try_max(LUK,vor_rpg.calc.MAX_LUK);
 try_max(DEF,vor_rpg.calc.MAX_DEF);
 try_max(CHR,vor_rpg.calc.MAX_CHR);
 try_max(AGL,vor_rpg.calc.MAX_AGL);
 try_max(STR,vor_rpg.calc.MAX_STR);
end;

Function TPlayer.STR:Int64;
begin
 Result:=Points.STR+Effects.STR;
end;

Function TPlayer.LUK:Int64;
begin
 Result:=Points.LUK+Effects.LUK;
end;

Function TPlayer.DEF:Int64;
begin
 Result:=Points.DEF+Effects.DEF;
end;

Function TPlayer.CHR:Int64;
begin
 Result:=Points.CHR+Effects.CHR;
end;

Function TPlayer.AGL:Int64;
begin
 Result:=Points.AGL+Effects.AGL;
end;

Function TPlayer.GetLUKPercent:Int64;
var
 _LUK:Int64;
begin
 _LUK:=LUK;
 if (_LUK=0) then Exit(0) else
 if (_LUK>0) then
  Result:=Trunc(Log2((_LUK+1)*4-2)*vor_rpg.calc.MUL_LUK+vor_rpg.calc.DEC_LUK)
 else
  Result:=-Trunc(Log2((abs(_LUK)+1)*4-2)*vor_rpg.calc.MUL_LUK+vor_rpg.calc.DEC_LUK);
end;

Function TPlayer.GetDEFPercent:Int64;
var
 _DEF:Int64;
begin
 _DEF:=DEF;
 if (_DEF=0) then Exit(0) else
 if (_DEF>0) then
  Result:=Trunc(Log2((_DEF+1)*4-2)*vor_rpg.calc.MUL_DEF+vor_rpg.calc.DEC_DEF)
 else
  Result:=-Trunc(Log2((abs(_DEF)+1)*4-2)*vor_rpg.calc.MUL_DEF+vor_rpg.calc.DEC_DEF);
end;

Function TPlayer.GetSTRPercent:Int64;
var
 _STR:Int64;
begin
 _STR:=STR;
 if (_STR=0) then Exit(0) else
 if (_STR>0) then
  Result:=Trunc(Log2((_STR+1)*4-2)*vor_rpg.calc.MUL_STR+vor_rpg.calc.DEC_STR)
 else
  Result:=-Trunc(Log2((abs(_STR)+1)*4-2)*vor_rpg.calc.MUL_STR+vor_rpg.calc.DEC_STR);
end;

Function TPlayer.GetESCPercent:Int64;
var
 _AGL:Int64;
begin
 _AGL:=AGL;
 if (_AGL=0) then Exit(0) else
 if (_AGL>0) then
  Result:=Trunc(Log2(_AGL*3-2)*vor_rpg.calc.MUL_AGL+vor_rpg.calc.DEC_AGL)
 else
  Result:=-Trunc(Log2(abs(_AGL)*3-2)*vor_rpg.calc.MUL_AGL+vor_rpg.calc.DEC_AGL);
end;

Function TPlayer.GetTime:Int64;
var
 _CHR:Int64;
begin
 _CHR:=CHR;
 if (_CHR=0) then Exit(0) else
 if (_CHR>0) then
  Result:=Trunc(Log2(_CHR*3-2)*vor_rpg.calc.MUL_TIME+vor_rpg.calc.DEC_TIME)
 else
  Result:=-Trunc(Log2(abs(_CHR)*3-2)*vor_rpg.calc.MUL_TIME+vor_rpg.calc.DEC_TIME);
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
  J.Values[path]:=val;
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

procedure add_debuf(d:Tdebuf);
begin
 if (debufSet=nil) then
 begin
  debufSet:=TdebufSet.Create;
 end;
 d.id:=debufSet.Size;
 debufSet.Insert(d);
end;

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

function Get_random_debuf_id:Int64;
begin
 Result:=-1;
 if (debufSet=nil) then Exit;
 Result:=Random(RCT,int64(debufSet.Size))
end;

procedure Set_debuf(var J:TJson;id,time:Int64);
Var
 instance,item:TJson;
 i,C:SizeUint;
 new:Int64;
begin
 instance:=J.Path['debuf.instance'];
 if not instance.isAssigned then
 begin
  instance:=TJson.New;
  instance.SetArray;
  J.Path['debuf.instance']:=instance;
 end;

 C:=instance.Count;
 new:=DateTimeToUnix(sysutils.Now,False)+time;
 if (C<>0) then
 begin
  For i:=0 to C-1 do
  begin
   item:=instance.Item[i];
   if (item.Path['id'].AsInt64(0)=id) then
   begin
    item.Values['time']:=Max(new,item.Path['time'].AsInt64(0));
    Exit;
   end;
  end;
 end;

 item:=TJson.New;
 item.Values['id']:=id;
 item.Values['time']:=new;

 instance.Add(item);

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
 Points.CheckMaxPts;

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

 if instance.Count=0 then
 begin
  J.Delete('debuf');
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

Function MMP(i:Int64):Int64;
begin
 if (i<0) then
  i:=0
 else
 if (i>100) then
  i:=100;
 Result:=i;
end;

function do_debuf(Const s,user:RawByteString;var data:TJson):Boolean;
var
 id,time:Int64;
 debuf:Tdebuf;
 cmd:RawByteString;
begin
 Result:=False;
 id:=Get_random_debuf_id;
 if (id<>-1) then
 begin
  debuf:=Get_debuf(id);
  time:=abs(vor_rpg.debuf.MIN_TIME)+Random(RCT,abs(vor_rpg.debuf.MAX_TIME-vor_rpg.debuf.MIN_TIME+1));
  Set_debuf(data,id,time);
  cmd:=Format(vor_rpg.stat_msg.on_debuf,[user,debuf.text]);
  push_irc_msg(cmd);
  FrmMain._add_reward_2_log(s,cmd);
  Result:=True;
 end;
end;

function try_debuf(Const s,user:RawByteString;var data:TJson):Boolean;
var
 rnd:Integer;
begin
 Result:=False;
 rnd:=Random(RCT,100);
 if (rnd<vor_rpg.debuf.PERC) then
 begin
  Result:=do_debuf(s,user,data);
 end;
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

   try_debuf(s,user1,data1);

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
    if (Val<>0) then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
    end;
    FrmMain._add_reward_2_log(s,cmd);
    Points1.IncExp(2);

    try_debuf(s,user1,data1);

   end;
  end;
  Points1.Save(data1);
  SetDBRpgUser1(user1,data1,@OnUnlock);
 end else
 begin
  Val:=(vip_rnd.perc_vor+Points1.GetLUKPercent-Points2.GetDEFPercent);
  Val:=MMP(Val);

  rnd:=Random(RCT,100);

  if (rnd<Val) then
  begin

   Val:=(1+Points1.GetLUKPercent);
   Val:=MMP(Val);

   rnd:=Random(RCT,100);

   if (rnd<Val) then
   begin
    //norm_vor

    cmd:=get_random_msg(vor_rpg.norm_vor);

    cmd:=Format(cmd,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Points1.IncExp(4);
    Points2.IncExp(1);

    try_debuf(s,user2,data2);

   end else
   begin
    //change vip and timeout

    cmd:=get_random_msg(vor_rpg.time4_vip);

    cmd:=Format(cmd,[user1,user2]);
    push_irc_msg(cmd);

    ChangeVip(user2,user1);

    Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);
    if (Val<>0) then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
    end;

    Points1.IncExp(3);
    Points2.IncExp(1);

    try_debuf(s,user1,data1);

   end;

  end else
  begin //str

   Val:=(vip_rnd.perc_vor+Points1.GetSTRPercent-Points2.GetDEFPercent);
   Val:=MMP(Val);

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
    Val:=MMP(Val);

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
     if (Val<>0) then
     begin
      push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
     end;

     Points1.IncExp(1);
     Points2.IncExp(2);

     try_debuf(s,user1,data1);

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
  function  Get_debufs:RawByteString;
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

function TDbcGetUserInfo.Get_debufs:RawByteString;
Var
 instance,item:TJson;
 i,C:SizeUint;
 now,time,hr,id:Int64;
 debuf:Tdebuf;
begin
 Result:='';
 instance:=data.Path['debuf.instance'];
 if not instance.isAssigned then Exit;

 C:=instance.Count;
 now:=DateTimeToUnix(sysutils.Now,False);
 if (C<>0) then
 begin
  For i:=0 to C-1 do
  begin
   item:=instance.Item[i];
   time:=item.Path['time'].AsInt64(0);
   time:=time-now;
   if (time>0) then
   begin
    id:=item.Path['id'].AsInt64(0);
    debuf:=Get_debuf(id);
    if (debuf.text<>'') then
    begin
     if (Result<>'') then Result:=Result+', ';
     time:=time div 60;
     hr:=time div 60;
     if (hr<>0) then
     begin
      time:=time-(hr*60);
      Result:=Result+debuf.text+'('+IntToStr(hr)+'ч '+IntToStr(time)+'мин)';
     end else
     begin
      Result:=Result+debuf.text+'('+IntToStr(time)+'мин)';
     end;
    end;
   end;
  end;
 end;

end;

procedure TDbcGetUserInfo.Print;
var
 Points:TPlayer;
begin
 Points.Load(data);
 case cmd of
  'dbf',
  'debuf':push_irc_msg(Format(vor_rpg.stat_msg.debuf_pr,[user,Get_debufs]));

  'level',
  'lvl' :push_irc_msg(Format(vor_rpg.stat_msg.lvl_msg,[user,
                                      IntToStr(Points.Points.LVL),
                                      IntToStr(Points.Points.EXP),
                                      IntToStr(Points.Points.GetExpToLvl)]));
  'points',
  'pts' :push_irc_msg(Format(vor_rpg.stat_msg.pts_msg,[user,
                                      IntToStr(Points.LUK),
                                      IntToStr(Points.DEF),
                                      IntToStr(Points.CHR),
                                      IntToStr(Points.AGL),
                                      IntToStr(Points.STR),
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
 Points1:TPlayer;
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

 if (Points1.Points.PTS>0) then
 begin
  Case cmd of
   'luk':do_inc:=try_inc(Points1.Points.LUK,vor_rpg.calc.MAX_LUK);
   'def':do_inc:=try_inc(Points1.Points.DEF,vor_rpg.calc.MAX_DEF);
   'chr':do_inc:=try_inc(Points1.Points.CHR,vor_rpg.calc.MAX_CHR);
   'agl':do_inc:=try_inc(Points1.Points.AGL,vor_rpg.calc.MAX_AGL);
   'str':do_inc:=try_inc(Points1.Points.STR,vor_rpg.calc.MAX_STR);
  end;

  if do_inc then
  begin
   Dec(Points1.Points.PTS);
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

//kick

type
 TKickScript=class(TDualLockScript)
  public
   is_mod:Boolean;
   Procedure OnEvent; override;
 end;

Procedure TKickScript.OnEvent;
var
 Points1,Points2:TPlayer;
 Val:Int64;
 rnd:Integer;
 Now,time:Int64;
begin

 Now:=DateTimeToUnix(sysutils.Now,False);

 time:=data1.Path['kick._in'].AsInt(0);
 if (time<>0) and ((time+vor_rpg.kick.in_time)>Now) then
 begin
  OnUnlock(nil);
  Exit;
 end;
 data1.Values['kick._in']:=Now;

 if not data2.isAssigned then
 begin
  //not found in base

  push_irc_msg(Format(get_random_msg(vor_rpg.kick.not_vor),[user1]));

  Points1.Load(data1);

  {Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);
  if (Val<>0) then
  begin
   push_irc_msg(Format(vor_rpg.timeout_cmd,[user1,IntToStr(Val)]));
  end;}

  SetDBRpgUser1(user1,data1,@OnUnlock);
  Exit;
 end;

 time:=data2.Path['kick._out'].AsInt(0);
 if (time<>0) and ((time+vor_rpg.kick.out_time)>Now) then
 begin
  OnUnlock(nil);
  Exit;
 end;
 data2.Values['kick._out']:=Now;

 Points1.Load(data1);
 Points2.Load(data2);

 val:=vor_rpg.kick.PERC+Points1.GetESCPercent-Points2.GetESCPercent;
 if is_mod then val:=val+10;
 val:=MMP(val);
 rnd:=Random(RCT,100);
 if (rnd<val) then
 begin
  val:=vor_rpg.kick.PERC+Points1.GetSTRPercent-Points2.GetDEFPercent;
  if is_mod then val:=val+10;
  val:=MMP(val);
  rnd:=Random(RCT,100);
  if (rnd<val) then
  begin
   //kick is
   push_irc_msg(Format(get_random_msg(vor_rpg.kick.go_kick),[user1,user2]));
   do_debuf('',user2,data2);
  end else
  begin
   //do def
   push_irc_msg(Format(get_random_msg(vor_rpg.kick.go_def),[user1,user2]));
   try_debuf('',user1,data1);
  end;
 end else
 begin
  //escape
  push_irc_msg(Format(get_random_msg(vor_rpg.kick.go_esc),[user1,user2]));
  try_debuf('',user1,data1);
 end;

 SetDBRpgUser2(user1,user2,data1,data2,@OnUnlock);

end;

Procedure kick(const dst_user,msg:RawByteString;is_mod:Boolean);
Var
 FDbcScript:TDbcScriptLock;
 src_user:RawByteString;
 FKickScript:TKickScript;
begin
 src_user:=Extract_nick(msg);

 if src_user=dst_user then Exit;

 FKickScript:=TKickScript.Create;
 FKickScript.user1:=dst_user;
 FKickScript.user2:=src_user;
 FKickScript.is_mod:=is_mod;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FKickScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);

end;

procedure TFrmVorRpg.add_to_chat_cmd(PC:TPrivMsgCfg;const user,cmd,param:RawByteString);
var
 F,v:RawByteString;

 Procedure vip_time;
 var
  i:Integer;
  datebeg,dateend:RawByteString;
  D:TDateTime;
 begin
  if (PC.PS*[pm_broadcaster,pm_moderator]<>[]) then Exit;
  if (GetTickCount64<vor_rpg.TickKd+vor_rpg.time_kd*1000) then Exit;

  i:=FrmVipParam.FindVipUser(user);
  datebeg:='';
  dateend:='';
  if (i<>-1) then
  begin
   if TryGetDateTime_US(GridVips.FieldValue['datebeg',i],D) then
   begin
    datebeg:=DateTimeToStr_RU(D);
   end;
   if TryGetDateTime_US(GridVips.FieldValue['dateend',i],D) then
   begin
    dateend:=DateTimeToStr_RU(D);
   end;
   push_irc_msg(Format(vip_rnd.viptime_get_info,[user,user,datebeg,dateend]));
  end;

  vor_rpg.TickKd:=GetTickCount64;
 end;

begin
 F:=LowerCase(Trim(param));

 Case LowerCase(cmd) of
  '!viptime',
  '!vipinfo',
  '!vip':
  begin
   vip_time;
  end;

  '!пнуть':
  if vor_rpg.kick.Enable then
  begin
   if (GetTickCount64<vor_rpg.TickKd+vor_rpg.time_kd*1000) then Exit;

   kick(user,F,PC.PS*[pm_broadcaster,pm_moderator]<>[]);

   vor_rpg.TickKd:=GetTickCount64;
  end;

  {$IFOPT D+}
  '!ban_test':
  begin
   if (GetTickCount64<vor_rpg.TickKd+vor_rpg.time_kd*1000) then Exit;

   F:=FetchAny(F);
   rpg_theif_vip('',user,F);

   vor_rpg.TickKd:=GetTickCount64;
  end;
  {$ENDIF}

  '!vor':
  begin
   if (GetTickCount64<vor_rpg.TickKd+vor_rpg.time_kd*1000) then Exit;

   if (F='') then
   begin
    push_irc_msg(Format(vor_rpg.stat_msg.help_msg1,[user]));
   end else
   begin

    v:=FetchAny(F);

    case v of
     'dbf',
     'debuf',
     'level',
     'lvl' ,
     'points',
     'pts' ,
     'stats',
     'stat':GetDBRpgUserInfo(user,v);
     'add':begin
            v:=FetchAny(F);
            Case v of
             'luk',
             'def',
             'chr',
             'agl',
             'str':add_pts(user,v);
             else
              push_irc_msg(Format(vor_rpg.stat_msg.help_msg2,[user]));
            end;
           end;
     else
      push_irc_msg(Format(vor_rpg.stat_msg.help_msg1,[user]));
    end;

   end;

   vor_rpg.TickKd:=GetTickCount64;
  end;
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
 {$IFOPT D+}
 vor_rpg.timeout_cmd:='';
 {$ENDIF}
 vor_rpg.Enable:=Trim(Config.ReadString('vor_rpg','enable','0'))='1';
 vor_rpg.time_kd            :=StrToDWORDDef(Config.ReadString('vor_rpg','time_kd'       ,IntToStr(vor_rpg.time_kd            )),vor_rpg.time_kd            );
 vor_rpg.calc.BASE_TIME     :=StrToDWORDDef(Config.ReadString('vor_rpg','BASE_TIME'     ,IntToStr(vor_rpg.calc.BASE_TIME     )),vor_rpg.calc.BASE_TIME     );
 vor_rpg.calc.PERC_MINUS_VIP:=StrToDWORDDef(Config.ReadString('vor_rpg','PERC_MINUS_VIP',IntToStr(vor_rpg.calc.PERC_MINUS_VIP)),vor_rpg.calc.PERC_MINUS_VIP);
 vor_rpg.debuf.PERC         :=StrToDWORDDef(Config.ReadString('vor_rpg','DEBUF_PERCENT' ,IntToStr(vor_rpg.debuf.PERC         )),vor_rpg.debuf.PERC         );
 vor_rpg.debuf.MIN_TIME     :=StrToDWORDDef(Config.ReadString('vor_rpg','DEBUF_MIN_TIME',IntToStr(vor_rpg.debuf.MIN_TIME     )),vor_rpg.debuf.MIN_TIME     );
 vor_rpg.debuf.MAX_TIME     :=StrToDWORDDef(Config.ReadString('vor_rpg','DEBUF_MAX_TIME',IntToStr(vor_rpg.debuf.MAX_TIME     )),vor_rpg.debuf.MAX_TIME     );

 vor_rpg.kick.Enable:=Trim(Config.ReadString('vor_rpg','kick_enable','0'))='1';
 vor_rpg.kick.PERC          :=StrToDWORDDef(Config.ReadString('vor_rpg','kick_PERC'     ,IntToStr(vor_rpg.kick.PERC          )),vor_rpg.kick.PERC          );
 vor_rpg.kick.in_time       :=StrToDWORDDef(Config.ReadString('vor_rpg','in_time'       ,IntToStr(vor_rpg.kick.in_time       )),vor_rpg.kick.in_time       );
 vor_rpg.kick.out_time      :=StrToDWORDDef(Config.ReadString('vor_rpg','out_time'      ,IntToStr(vor_rpg.kick.out_time      )),vor_rpg.kick.out_time      );
end;

Procedure TFrmVorRpg.Open;
begin
 CBVorRpgEnable.Checked :=vor_rpg.Enable;
 EdtTimeKd.Text         :=IntToStr(vor_rpg.time_kd);
 EdtBaseTime.Text       :=IntToStr(vor_rpg.calc.BASE_TIME);
 EdtPercMinusVip.Text   :=IntToStr(vor_rpg.calc.PERC_MINUS_VIP);
 EdtDebufPerc.Text      :=IntToStr(vor_rpg.debuf.PERC);
 EdtDebufMinTime.Text   :=IntToStr(vor_rpg.debuf.MIN_TIME);
 EdtDebufMaxTime.Text   :=IntToStr(vor_rpg.debuf.MAX_TIME);

 CBKickEnable.Checked   :=vor_rpg.kick.Enable;
 EdtKickPerc.Text       :=IntToStr(vor_rpg.kick.PERC);

 EdtKickIn.Text         :=IntToStr(vor_rpg.kick.in_time);
 EdtKickOut.Text        :=IntToStr(vor_rpg.kick.out_time);

 if ShowModal=1 then
 begin
  vor_rpg.Enable             :=CBVorRpgEnable.Checked;
  vor_rpg.time_kd            :=StrToDWORDDef(EdtTimeKd.Text      ,vor_rpg.time_kd);
  vor_rpg.calc.BASE_TIME     :=StrToDWORDDef(EdtBaseTime.Text    ,vor_rpg.calc.BASE_TIME);
  vor_rpg.calc.PERC_MINUS_VIP:=StrToDWORDDef(EdtPercMinusVip.Text,vor_rpg.calc.PERC_MINUS_VIP);
  vor_rpg.debuf.PERC         :=StrToDWORDDef(EdtDebufPerc.Text   ,vor_rpg.debuf.PERC);
  vor_rpg.debuf.MIN_TIME     :=StrToDWORDDef(EdtDebufMinTime.Text,vor_rpg.debuf.MIN_TIME);
  vor_rpg.debuf.MAX_TIME     :=StrToDWORDDef(EdtDebufMaxTime.Text,vor_rpg.debuf.MAX_TIME);

  vor_rpg.kick.Enable        :=CBKickEnable.Checked;
  vor_rpg.kick.PERC          :=StrToDWORDDef(EdtKickPerc.Text    ,vor_rpg.kick.PERC);

  vor_rpg.kick.in_time       :=StrToDWORDDef(EdtKickin.Text      ,vor_rpg.kick.in_time);
  vor_rpg.kick.out_time      :=StrToDWORDDef(EdtKickOut.Text     ,vor_rpg.kick.out_time);

  try

   case vor_rpg.Enable of
    True :Config.WriteString('vor_rpg' ,'enable','1');
    False:Config.WriteString('vor_rpg' ,'enable','0');
   end;

   Config.WriteString('vor_rpg','time_kd'       ,IntToStr(vor_rpg.time_kd));
   Config.WriteString('vor_rpg','BASE_TIME'     ,IntToStr(vor_rpg.calc.BASE_TIME));
   Config.WriteString('vor_rpg','PERC_MINUS_VIP',IntToStr(vor_rpg.calc.PERC_MINUS_VIP));
   Config.WriteString('vor_rpg','DEBUF_PERCENT' ,IntToStr(vor_rpg.debuf.PERC));
   Config.WriteString('vor_rpg','DEBUF_MIN_TIME',IntToStr(vor_rpg.debuf.MIN_TIME));
   Config.WriteString('vor_rpg','DEBUF_MAX_TIME',IntToStr(vor_rpg.debuf.MAX_TIME));

   case vor_rpg.kick.Enable of
    True :Config.WriteString('vor_rpg' ,'kick_enable','1');
    False:Config.WriteString('vor_rpg' ,'kick_enable','0');
   end;

   Config.WriteString('vor_rpg','kick_PERC',IntToStr(vor_rpg.kick.PERC));
   Config.WriteString('vor_rpg','in_time'  ,IntToStr(vor_rpg.kick.in_time));
   Config.WriteString('vor_rpg','out_time' ,IntToStr(vor_rpg.kick.out_time));

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

procedure TFrmVorRpg.EdtPercMinusVipKeyPress(Sender:TObject;var Key:char);
begin
 prev_perc:=StrToQWORDDef(TLabeledEdit(Sender).Text,70);
 if prev_perc>100 then prev_perc:=100;
 Case Key of
  #8,#9,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

procedure TFrmVorRpg.EdtPercMinusVipExit(Sender:TObject);
var
 S,L:Integer;
begin
 case StrToDWordDef(TLabeledEdit(Sender).Text,0) of
  1..100:;
  else
  begin
   S:=TLabeledEdit(Sender).SelStart ;
   L:=TLabeledEdit(Sender).SelLength;
   TLabeledEdit(Sender).Text:=IntToStr(prev_perc);
   TLabeledEdit(Sender).SelStart :=S;
   TLabeledEdit(Sender).SelLength:=L;
  end;
 end;
end;

procedure TFrmVorRpg.EdtDwExit(Sender: TObject);
var
 S,L:Integer;
 d:DWORD;
begin
 if not TryStrToDWord(TLabeledEdit(Sender).Text,d) then
 begin
  S:=TLabeledEdit(Sender).SelStart ;
  L:=TLabeledEdit(Sender).SelLength;
  TLabeledEdit(Sender).Text:=IntToStr(prev_dw);
  TLabeledEdit(Sender).SelStart :=S;
  TLabeledEdit(Sender).SelLength:=L;
 end;
end;

procedure TFrmVorRpg.EdtDwKeyPress(Sender:TObject;var Key:char);
begin
 prev_dw:=StrToDWORDDef(TLabeledEdit(Sender).Text,1);
 Case Key of
  #8,#9,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

initialization
 LockStr:=TRawByteStringSet.Create;
 vor_rpg.time_kd:=8;

end.

