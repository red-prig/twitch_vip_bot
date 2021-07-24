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
    CBduelCheckZero: TCheckBox;
    CBKickEnable: TCheckBox;
    CBXchgEnable: TCheckBox;
    CBVorRpgEnable: TCheckBox;
    CBduelEnable: TCheckBox;
    EdtBaseTime: TLabeledEdit;
    EdtDebufMaxTime: TLabeledEdit;
    EdtDebufMinTime: TLabeledEdit;
    EdtDebufPerc: TLabeledEdit;
    EdtKickIn: TLabeledEdit;
    EdtDuelKd: TLabeledEdit;
    EdtDuelMVipPerc: TLabeledEdit;
    EdtXchgMaxCount: TLabeledEdit;
    EdtKickOut: TLabeledEdit;
    EdtKickPerc: TLabeledEdit;
    EdtPercMinusVip: TLabeledEdit;
    EdtTimeKd: TLabeledEdit;
    EdtduelMaxCount: TLabeledEdit;
    EdtXchgMaxTime: TLabeledEdit;
    EdtduelMaxTime: TLabeledEdit;
    PageCtrl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure EdtPercMinusVipKeyPress(Sender:TObject;var Key:char);
    procedure EdtPercMinusVipExit(Sender:TObject);
    procedure EdtDwExit(Sender: TObject);
    procedure EdtDwKeyPress(Sender:TObject;var Key:char);
  private
    prev_perc:Byte;
    prev_dw:DWORD;
    FClearTimer:TTimer;
  public
    Procedure rpg_theif_vip(const s,dst_user,msg:RawByteString);
    procedure check_xchg_vip_time;
    procedure add2xchgVip(const user,nick:RawByteString);
    procedure catch_vip(const user:RawByteString);
    Procedure vip_time(const user:RawByteString);
    procedure check_duel_time;
    procedure add2duel(const user,nick:RawByteString;is_mod:Boolean);
    procedure add_to_chat_cmd(PC:TPrivMsgCfg;const user,cmd,param:RawByteString);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
    procedure SetClearTimer(m:Boolean);
    procedure OnClearProc(Sender:TObject);
    procedure OnClearRnd(Sender:TBaseTask);
  end;

var
  vor_rpg:record
   Enable:Boolean;
   timeout_cmd:RawByteString;
   vor_sucs:TStringList;
   TickKd:Int64;
   time_kd:DWORD;

   xchg:record
    Enable:Boolean;
    kd_time:Int64;
    max_count:DWORD;
    max_time:DWORD;
    exist1_msg,
    exist2_msg,
    max_msg,
    ready_msg,
    cancel_msg,
    sucs_msg,
    time_msg1,
    time_msg2:RawByteString;
   end;

   duel:record
    Enable:Boolean;
    check_zero:Boolean;
    PERC_MINUS_VIP:Byte;
    max_count:DWORD;
    max_time:DWORD;
    kd_time:DWORD;
    any_msg,
    exist1_msg,
    exist2_msg,
    max_msg,
    ready_msg,
    cancel_msg,
    time_msg:RawByteString;
    stand_msg:TStringList;
    vip_msg:TStringList;
    win_msg:TStringList;
   end;

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
    help_msg3,
    top_msg1,
    top_msg2,
    rate_msg,
    rank_msg,
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
    MUL_ESC:Double;

    DEC_LUK:Double;
    DEC_DEF:Double;
    DEC_STR:Double;
    DEC_AGL:Double;
    DEC_ESC:Double;

    PERC_MINUS_VIP:Byte;
   end;

   debuf:record
    PERC:Byte;
    MIN_TIME:Int64;
    MAX_TIME:Int64;
   end;

   kick:record
    Enable:Boolean;
    in_msg:RawByteString;
    out_msg:RawByteString;
    in_time :Int64;
    out_time:Int64;
    PERC:Byte;
    not_vor:TStringList;
    go_kick:TStringList;
    go_def:TStringList;
    go_esc:TStringList;
   end;

   rst:record
    tax:DWORD;
    kd:DWORD;
    rst_msg,
    tax_msg,
    not_msg,
    info_msg:RawByteString;
   end;

  end;

  FrmVorRpg: TFrmVorRpg;

  FGetRpgTop:TSQLScript;
  FGetRpgRank:TSQLScript;

  FGetRpgUser1:TSQLScript;
  FGetRpgUser2:TSQLScript;
  FGetRndUser1:TSQLScript;

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
 ZDbcIntfs,
 UAsyncResultSet,
 Ulog,UFrmVipParam,
 math,
 mtRandom,ujson,gset,
 xml_parse,data_xml;

{$R *.lfm}

type
 PxchgVip=^TxchgVip;
 PxchgNode=^TxchgNode;
 TxchgNode=record
  user:RawByteString;
  link:PxchgVip;
 end;
 TxchgVip=record
  src,dst:TxchgNode;
  time:Int64;
  is_mod:Boolean;
 end;

 TxchgNodeCompare=class
  class function c(a,b:PxchgNode):boolean; static;
 end;

 TxchgNodeSet=specialize TSet<PxchgNode,TxchgNodeCompare>;

 TRawStrCompare=class
  class function c(const a,b:RawByteString):boolean; static;
 end;

 TRawByteStringSet=specialize TSet<RawByteString,TRawStrCompare>;

 TLockScript=class
  protected
   FStatement:TDbcStatementScript;
   Function  try_lock:Boolean; virtual; abstract;
   procedure unlock;           virtual; abstract;
   procedure Prepare(FDbcScript:TDbcStatementScript); virtual;
   procedure OnFin(ResultSet:TZResultSet); virtual; abstract;
   procedure OnUnlock(Sender:TBaseTask);
  public
   Procedure OnEvent; virtual; abstract;
 end;

 TDualLockScript=class(TLockScript)
  public
   user:array[0..1] of RawByteString;
   data:array[0..1] of TJson;
   Destructor Destroy; override;
  protected
   Function   try_lock:Boolean; override;
   procedure  unlock;           override;
   procedure  Prepare(FDbcScript:TDbcStatementScript); override;
   procedure  OnFin(ResultSet:TZResultSet); override;
 end;

 TOneLockScript=class(TLockScript)
  public
   user:RawByteString;
   data:TJson;
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
 xchgSet:TxchgNodeSet;
 duelSeta:array[0..1] of TxchgNodeSet;

class function TRawStrCompare.c(const a,b:RawByteString):boolean;
begin
 Result:=CompareStr(a,b)<0;
end;

class function TxchgNodeCompare.c(a,b:PxchgNode):boolean;
begin
 Result:=CompareStr(a^.user,b^.user)<0;
end;

procedure TLockScript.Prepare(FDbcScript:TDbcStatementScript);
begin
 FStatement:=FDbcScript;
end;

procedure TLockScript.OnUnlock(Sender:TBaseTask);
begin
 unlock;
 Free;
end;

//TDualLockScript

Function  TDualLockScript.try_lock:Boolean;
begin
 Result:=((user[0]='') or (LockStr.NFind(user[0])=nil)) and
         ((user[1]='') or (LockStr.NFind(user[1])=nil));
 if Result then
 begin
  if (user[0]<>'') then
   LockStr.Insert(user[0]);
  if (user[1]<>'') then
   LockStr.Insert(user[1]);
 end;
end;

procedure TDualLockScript.unlock;
begin
 LockStr.Delete(user[0]);
 LockStr.Delete(user[1]);
end;

procedure TDualLockScript.Prepare(FDbcScript:TDbcStatementScript);
begin
 inherited;
 FDbcScript.SetSctipt(FGetRpgUser2);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user1',user[0]);
 FDbcScript.Params.SetRawByteString('user2',user[1]);
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

    if (Trim(u)<>'') then
    begin
     if (user[0]=u) then
     begin
      ms:=TPCharStream.Create(PAnsiChar(d),Length(d));
      data[0]:=Default(TJson);
      try
       data[0]:=TJson.New(ms);
      except
       on E:Exception do
       begin
        DumpExceptionCallStack(E);
       end;
      end;
      FreeAndNil(ms);
     end else
     if (user[1]=u) then
     begin
      ms:=TPCharStream.Create(PAnsiChar(d),Length(d));
      data[1]:=Default(TJson);
      try
       data[1]:=TJson.New(ms);
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
 end;

 OnEvent;
end;

Destructor TDualLockScript.Destroy;
begin
 data[0].Free;
 data[1].Free;
 inherited;
end;

//TDualLockScript

//TOneLockScript

Function  TOneLockScript.try_lock:Boolean;
begin
 Result:=((user='') or (LockStr.NFind(user)=nil));
 if Result then
 begin
  if (user<>'') then
   LockStr.Insert(user);
 end;
end;

procedure TOneLockScript.unlock;
begin
 LockStr.Delete(user);
end;

procedure TOneLockScript.Prepare(FDbcScript:TDbcStatementScript);
begin
 inherited;
 FDbcScript.SetSctipt(FGetRpgUser1);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user1',user);
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

    if (Trim(u)<>'') and (user=u) then
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

 OnEvent;
end;

Destructor TOneLockScript.Destroy;
begin
 data.Free;
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
 LVL1,EXP1:Int64;
begin

 if (Trim(user1)='') then
 begin
  if Assigned(N) then N(nil);
  Exit;
 end;

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
 LVL1:=data1.Path['points.LVL'].AsInt64(0);
 EXP1:=data1.Path['points.EXP'].AsInt64(0);
 FDbcScript.Params.SetInt64('lvl1',LVL1);
 FDbcScript.Params.SetInt64('exp1',EXP1);
 FDbcScript.Start;
 FDbcScript.Release;
end;

procedure SetDBRpgUser2(Const user1,user2:RawByteString;data1,data2:TJson;N:TNotifyTask);
var
 FDbcScript:TDbcStatementScript;
 ms:TRawByteStringStream;
 d1,d2:RawByteString;
 LVL1,EXP1,LVL2,EXP2:Int64;
begin

 if (Trim(user1)='') then
 begin
  if (Trim(user2)='') then
  begin
   if Assigned(N) then N(nil);
  end else
  begin
   SetDBRpgUser1(user2,data2,N);
  end;
  Exit;
 end else
 if (Trim(user2)='') then
 begin
  SetDBRpgUser1(user1,data1,N);
  Exit;
 end;

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
 LVL1:=data1.Path['points.LVL'].AsInt64(0);
 EXP1:=data1.Path['points.EXP'].AsInt64(0);
 FDbcScript.Params.SetInt64('lvl1',LVL1);
 FDbcScript.Params.SetInt64('exp1',EXP1);
 LVL2:=data2.Path['points.LVL'].AsInt64(0);
 EXP2:=data2.Path['points.EXP'].AsInt64(0);
 FDbcScript.Params.SetInt64('lvl2',LVL2);
 FDbcScript.Params.SetInt64('exp2',EXP2);
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
 FVorScript.user[0]:=dst_user;
 FVorScript.user[1]:=src_user;
 FVorScript.s:=s;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FVorScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);

end;

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
  function  TryDecAnyPts:Boolean; inline;
  function  TryDecLvl:Boolean; inline;
  function  TryDecExp:Boolean;
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
  Function  GetAGLPercent:Int64;
  Function  GetTime:Int64;
  Function  kick_in_time:Int64;
  Function  kick_out_time:Int64;
  Function  duel_kd_time:Int64;
  procedure IncEXP(val:Int64);
  Function  NeedReset:Boolean;
  Function  Reset(is_mod:Boolean):Boolean;
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

function TUserPoints.TryDecAnyPts:Boolean; inline;
var
 RNDM:array of Byte;

 procedure tadd(p:Byte;val:Int64);
 begin
  if (val>0) then
  begin
   SetLength(RNDM,Length(RNDM)+1);
   RNDM[Length(RNDM)-1]:=p;
  end;
 end;

begin
 Result:=False;
 if (PTS>0) then
 begin
  Dec(PTS);
  Result:=True;
 end else
 begin
  SetLength(RNDM,0);
  tadd(0,STR);
  tadd(1,LUK);
  tadd(2,DEF);
  tadd(3,CHR);
  tadd(4,AGL);
  if (Length(RNDM)=0) then Exit;
  case RNDM[Random(RCT,Length(RNDM))] of
   0:Dec(STR);
   1:Dec(LUK);
   2:Dec(DEF);
   3:Dec(CHR);
   4:Dec(AGL);
  end;
  Result:=True;
 end;
end;

function TUserPoints.TryDecLvl:Boolean; inline;
begin
 Result:=False;
 if (LVL>0) then
 begin
  Result:=TryDecAnyPts;
  if Result then
  begin
   Dec(LVL);
  end;
 end;
end;

function TUserPoints.TryDecExp:Boolean;
begin
 Result:=False;
 if (EXP>0) then
 begin
  Dec(EXP);
  Result:=True;
 end else
 begin
  Result:=TryDecLvl;
  if Result then
  begin
   EXP:=GetExpToLvl-1;
  end;
 end;
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
  Result:=Trunc(Log2((_AGL+1)*4-2)*vor_rpg.calc.MUL_ESC+vor_rpg.calc.DEC_ESC)
 else
  Result:=-Trunc(Log2((abs(_AGL)+1)*4-2)*vor_rpg.calc.MUL_ESC+vor_rpg.calc.DEC_ESC);
end;

Function TPlayer.GetAGLPercent:Int64;
var
 _AGL:Int64;
begin
 _AGL:=AGL;
 if (_AGL=0) then Exit(0) else
 if (_AGL>0) then
  Result:=Trunc(Log2((_AGL+1)*4-2)*vor_rpg.calc.MUL_AGL+vor_rpg.calc.DEC_AGL)
 else
  Result:=-Trunc(Log2((abs(_AGL)+1)*4-2)*vor_rpg.calc.MUL_AGL+vor_rpg.calc.DEC_AGL);
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

Function TPlayer.kick_in_time:Int64;
var
 t:Int64;
begin
 t:=GetTime;
 if (t>0) and (t>=vor_rpg.kick.in_time) then
 begin
  Result:=vor_rpg.kick.in_time-(vor_rpg.kick.in_time*CHR) div 100;
 end else
 begin
  Result:=vor_rpg.kick.in_time-t;
 end;
end;

Function TPlayer.kick_out_time:Int64;
begin
 Result:=vor_rpg.kick.out_time+GetTime;
end;

Function TPlayer.duel_kd_time:Int64;
var
 t:Int64;
begin
 t:=GetTime;
 if (t>0) and (t>=vor_rpg.duel.kd_time) then
 begin
  Result:=vor_rpg.duel.kd_time-(vor_rpg.duel.kd_time*CHR) div 100;
 end else
 begin
  Result:=vor_rpg.duel.kd_time-t;
 end;
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

Function TPlayer.NeedReset:Boolean;
begin
 Result:=(Points.STR+
          Points.LUK+
          Points.DEF+
          Points.CHR+
          Points.AGL)>0;
end;

Function TPlayer.Reset(is_mod:Boolean):Boolean;
var
 i,tax:SizeInt;
begin
 Result:=True;
 if not is_mod then
 begin
  tax:=(Points.GetExpToLvl*vor_rpg.rst.tax) div 100;
  if (tax=0) then tax:=1;
  For i:=0 to tax-1 do
   if not Points.TryDecExp then Exit(False);
 end;

 Points.PTS:=Points.PTS+
             Points.STR+
             Points.LUK+
             Points.DEF+
             Points.CHR+
             Points.AGL;

 Points.STR:=0;
 Points.LUK:=0;
 Points.DEF:=0;
 Points.CHR:=0;
 Points.AGL:=0;
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
 if (i<=0) then
 begin
  i:=Random(RCT,2);
 end else
 if (i>100) then
  i:=100;
 Result:=i;
end;

function do_debuf(Const {s,}user:RawByteString;var data:TJson;per_time:Byte):Boolean;
var
 id,time,tmax,tmin:Int64;
 debuf:Tdebuf;
 cmd:RawByteString;
begin
 Result:=False;
 id:=Get_random_debuf_id;
 if (id<>-1) then
 begin
  debuf:=Get_debuf(id);
  tmax:=Max(vor_rpg.debuf.MAX_TIME,vor_rpg.debuf.MIN_TIME);
  tmin:=Min(vor_rpg.debuf.MAX_TIME,vor_rpg.debuf.MIN_TIME);
  time:=tmax-tmin+1;
  time:=time-(time*per_time) div 100;
  if (time<=0) then Exit;
  time:=tmin+Random(RCT,time);
  Set_debuf(data,id,time);
  cmd:=Format(vor_rpg.stat_msg.on_debuf,[user,debuf.text]);
  push_irc_msg(cmd);
  //FrmMain._add_reward_2_log(s,cmd);
  Result:=True;
 end;
end;

function try_debuf(Const {s,}user:RawByteString;var data:TJson;per_time:Byte):Boolean;
var
 rnd:Integer;
begin
 Result:=False;
 rnd:=Random(RCT,100);
 if (rnd<vor_rpg.debuf.PERC) then
 begin
  Result:=do_debuf({s,}user,data,per_time);
 end;
end;

Procedure TVorScript.OnEvent;
var
 cmd:RawByteString;
 Points1,Points2:TPlayer;
 rnd:Integer;
 Val:Int64;
begin
 Points1.Load(data[0]);
 Points2.Load(data[1]);
 if (FrmVipParam.FindVipUser(user[0])<>-1) then
 begin
  rnd:=Random(RCT,100);
  if (rnd<vor_rpg.calc.PERC_MINUS_VIP) then
  begin

   cmd:=get_random_msg(vor_rpg.minus_vip);

   cmd:=Format(cmd,[user[0]]);
   push_irc_msg(cmd);

   FrmVipParam.DeleteAndUnVip(user[0]);

   FrmMain._add_reward_2_log(s,cmd);
   Points1.IncExp(1);

   try_debuf({s,}user[0],data[0],Points1.CHR);

  end else
  begin
   rnd:=Random(RCT,100);
   if (rnd<Points1.GetLUKPercent) then
   begin

    cmd:=get_random_msg(vor_rpg.chist_vip);

    cmd:=Format(cmd,[user[0]]);
    push_irc_msg(cmd);
    FrmMain._add_reward_2_log(s,cmd);
    Points1.IncExp(1);
   end else
   begin
    Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);

    cmd:=get_random_msg(vor_rpg.neudc_vip);

    cmd:=Format(cmd,[user[0]]);
    push_irc_msg(cmd);
    if (Val<>0) then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user[0],IntToStr(Val)]));
    end;
    FrmMain._add_reward_2_log(s,cmd);
    Points1.IncExp(2);

    try_debuf({s,}user[0],data[0],Points1.CHR);

   end;
  end;
  Points1.Save(data[0]);
  SetDBRpgUser1(user[0],data[0],@OnUnlock);
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

    cmd:=Format(cmd,[user[0],user[1]]);
    push_irc_msg(cmd);

    ChangeVip(user[1],user[0]);

    Points1.IncExp(4);
    Points2.IncExp(1);

    try_debuf({s,}user[1],data[1],Points2.CHR);

   end else
   begin
    //change vip and timeout

    cmd:=get_random_msg(vor_rpg.time4_vip);

    cmd:=Format(cmd,[user[0],user[1]]);
    push_irc_msg(cmd);

    ChangeVip(user[1],user[0]);

    Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);
    if (Val<>0) then
    begin
     push_irc_msg(Format(vor_rpg.timeout_cmd,[user[0],IntToStr(Val)]));
    end;

    Points1.IncExp(3);
    Points2.IncExp(1);

    try_debuf({s,}user[0],data[0],Points1.CHR);

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

    cmd:=Format(cmd,[user[0],user[1]]);
    push_irc_msg(cmd);

    ChangeVip(user[1],user[0]);

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

     cmd:=Format(cmd,[user[0],user[1]]);
     push_irc_msg(cmd);

     Points1.IncExp(3);
     Points2.IncExp(1);

    end else
    begin
     //jail

     cmd:=get_random_msg(vor_rpg.jail_vip);

     cmd:=Format(cmd,[user[0],user[1]]);
     push_irc_msg(cmd);

     Val:=Max(vor_rpg.calc.BASE_TIME-Points1.GetTime,0);
     if (Val<>0) then
     begin
      push_irc_msg(Format(vor_rpg.timeout_cmd,[user[0],IntToStr(Val)]));
     end;

     Points1.IncExp(1);
     Points2.IncExp(2);

     try_debuf({s,}user[0],data[0],Points1.CHR);

    end;


   end;

  end;

  Points1.Save(data[0]);
  Points2.Save(data[1]);
  SetDBRpgUser2(user[0],user[1],data[0],data[1],@OnUnlock);

 end;

 FrmMain._add_reward_2_log(s,cmd);
end;

type
 TTopRec=record
  user:RawByteString;
  LVL,EXP:Int64;
 end;

 TDbcGetUsersTop=class(TDbcStatementScript)
  top:array of TTopRec;
  user:RawByteString;
  Procedure OnAdd(const _user:RawByteString;_LVL,_EXP:Int64);
  Procedure OnFin(Sender:TBaseTask);
 end;

Procedure TDbcGetUsersTop.OnAdd(const _user:RawByteString;_LVL,_EXP:Int64);
var
 i:SizeUint;
begin
 For i:=Low(top) to High(top) do
 if (top[i].user='') then
 begin
  top[i].user:=_user;
  top[i].LVL:=_LVL;
  top[i].EXP:=_EXP;
  Exit;
 end;
end;

Procedure TDbcGetUsersTop.OnFin(Sender:TBaseTask);
var
 i,c:SizeInt;
 user_f,lvl_f,exp_f:SizeInt;
 u:RawByteString;

 LVL,EXP:Int64;

 list:RawByteString;
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
    lvl_f:=ResultSet.FindColumn('LVL');
    exp_f:=ResultSet.FindColumn('EXP');
   For i:=1 to c do
   begin
    ResultSet.MoveAbsolute(i);
    u:=ResultSet.GetRawByteString(user_f);
    LVL:=ResultSet.GetLong(lvl_f);
    EXP:=ResultSet.GetLong(exp_f);
    OnAdd(u,LVL,EXP);
   end;
  end;
 end;

 list:='';
 For i:=Low(top) to High(top) do
  if (top[i].user<>'') then
  begin
   if (list<>'') then list:=list+' ';
   list:=list+Format(vor_rpg.stat_msg.top_msg1,[IntToStr(i+1),top[i].user,IntToStr(top[i].LVL),IntToStr(top[i].EXP)]);
  end;
 push_irc_msg(Format(vor_rpg.stat_msg.top_msg2,[user,list]));
end;

procedure GetDBRpgUserTop(Const user:RawByteString;count:DWORD);
var
 FDbcScript:TDbcGetUsersTop;
begin
 if (count=0)  then count:=3;
 if (count>10) then count:=10;

 FDbcScript:=TDbcGetUsersTop.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@FDbcScript.OnFin);
 FDbcScript.SetSctipt(FGetRpgTop);
 FDbcScript.ExecuteScript;
 FDbcScript.user:=user;
 SetLength(FDbcScript.top,count);
 FDbcScript.Params.SetInt64('count',count);
 FDbcScript.Start;
 FDbcScript.Release;
end;

type
 TDbcGetUsersRank=class(TDbcStatementScript)
  s:RawByteString;
  Procedure OnFin(Sender:TBaseTask);
 end;

Procedure TDbcGetUsersRank.OnFin(Sender:TBaseTask);
var
 rank_f:SizeInt;
 rank:Int64;
begin
 rank:=0;
 if (ResultSet<>nil) then
 if ResultSet.First then
 begin
  rank_f:=ResultSet.FindColumn('rank');
  rank:=ResultSet.GetLong(rank_f);
 end;

 if (rank=0) then
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.rank_msg,[s,'???']));
 end else
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.rank_msg,[s,IntToStr(rank)]));
 end;
end;

procedure GetDBRpgUserRank(Const s,user:RawByteString);
var
 FDbcScript:TDbcGetUsersRank;
begin
 FDbcScript:=TDbcGetUsersRank.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@FDbcScript.OnFin);
 FDbcScript.SetSctipt(FGetRpgRank);
 FDbcScript.ExecuteScript;
 FDbcScript.s:=s;
 FDbcScript.Params.SetRawByteString('user',user);
 FDbcScript.Start;
 FDbcScript.Release;
end;

procedure _GetDBRpgUserTop(Const user,msg:RawByteString);
var
 count:DWORD;
begin
 if (Trim(msg)='') then
 begin
  GetDBRpgUserTop(user,3);
 end else
 if TryStrToDWord(msg,count) then
 begin
  GetDBRpgUserTop(user,count);
 end else
 begin
  GetDBRpgUserRank(user,LowerCase(Extract_nick(msg)));
 end;
end;

type
 TDbcGetUserInfo=class(TDbcStatementScript)
  src,user,cmd:RawByteString;
  data:TJson;
  Procedure Cleanup; override;
  Procedure OnFin(Sender:TBaseTask);
  function  Get_debufs:RawByteString;
  procedure Print;
 end;

procedure GetDBRpgUserInfo(Const src,user,cmd:RawByteString);
var
 FDbcScript:TDbcGetUserInfo;
begin
 FDbcScript:=TDbcGetUserInfo.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@FDbcScript.OnFin);
 FDbcScript.SetSctipt(FGetRpgUser1);
 FDbcScript.ExecuteScript;
 FDbcScript.src:=src;
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

function GetLongStrTime(sc:Int64):RawByteString;
var
 mn,hr,dy:Int64;
begin
 Result:='';
 if (sc<0) then
 begin
  Result:='-';
  sc:=abs(sc);
 end;
 mn:=sc div 60;
 hr:=mn div 60;
 dy:=hr div 24;
 if (dy<>0) then
 begin
  if (hr<>0) then
  begin
   mn:=mn-(hr*60);
   hr:=hr-(dy*24);
   Result:=Result+IntToStr(dy)+'д '+IntToStr(hr)+'ч '+IntToStr(mn)+'м';
  end else
  begin
   Result:=Result+IntToStr(dy)+'д '+IntToStr(mn)+'м';
  end;
 end else
 if (hr<>0) then
 begin
  mn:=mn-(hr*60);
  Result:=Result+IntToStr(hr)+'ч '+IntToStr(mn)+'м';
 end else
 if (mn<>0) then
 begin
  Result:=Result+IntToStr(mn)+'м';
 end else
 begin
  Result:=Result+IntToStr(sc)+'c';
 end;
end;

function TDbcGetUserInfo.Get_debufs:RawByteString;
Var
 instance,item:TJson;
 i,C:SizeUint;
 now,time,id:Int64;
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
     Result:=Result+debuf.text+'('+GetLongStrTime(time)+')';
    end;
   end;
  end;
 end;

end;

procedure TDbcGetUserInfo.Print;
var
 Points:TPlayer;

 procedure dorate;
 var
  w,l:Int64;
  p:RawByteString;
 begin
  w:=data.Path['rate.win' ].AsInt64(0);
  l:=data.Path['rate.lose'].AsInt64(0);
  p:='';
  if (w+l)>0 then
  begin
   p:=FloatToStrF(w/(w+l)*100,ffFixed,0,2);
  end;
  push_irc_msg(Format(vor_rpg.stat_msg.rate_msg,[src,IntToStr(w),IntToStr(l),p]));
 end;

begin
 Points.Load(data);
 case cmd of
  'winrate',
  'rate',
  'wrt':
  begin
   dorate;
  end;

  'dbf',
  'debuf':
  begin
   push_irc_msg(Format(vor_rpg.stat_msg.debuf_pr,[src,Get_debufs]));
  end;

  'level',
  'lvl' :
  begin
   push_irc_msg(Format(vor_rpg.stat_msg.lvl_msg,[src,
                                      IntToStr(Points.Points.LVL),
                                      IntToStr(Points.Points.EXP),
                                      IntToStr(Points.Points.GetExpToLvl)]));
  end;

  'points',
  'pts' :
  begin
   push_irc_msg(Format(vor_rpg.stat_msg.pts_msg,[src,
                                      IntToStr(Points.LUK),
                                      IntToStr(Points.DEF),
                                      IntToStr(Points.CHR),
                                      IntToStr(Points.AGL),
                                      IntToStr(Points.STR),
                                      IntToStr(Points.Points.PTS)]));
  end;

  'stats',
  'stat':
  begin
   push_irc_msg(Format(vor_rpg.stat_msg.stat_msg,[src,
                                      IntToStr(Points.GetLUKPercent),
                                      IntToStr(Points.GetDEFPercent),
                                      IntToStr(Points.GetSTRPercent),
                                      IntToStr(Points.GetAGLPercent),
                                      IntToStr(Points.GetESCPercent),
                                      IntToStr(Points.GetTime)]));
  end;

 end;
end;

type
 TAddPtsScript=class(TOneLockScript)
  public
   is_mod:Boolean;
   src,cmd:RawByteString;
   Procedure OnEvent; override;
 end;

 TSubPtsScript=class(TOneLockScript)
  public
   src,cmd:RawByteString;
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

 procedure lvl_msg;
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.lvl_msg,[src,
                      IntToStr(Points1.Points.LVL),
                      IntToStr(Points1.Points.EXP),
                      IntToStr(Points1.Points.GetExpToLvl)]));
 end;

 procedure pts_msg;
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.pts_msg,[src,
                      IntToStr(Points1.LUK),
                      IntToStr(Points1.DEF),
                      IntToStr(Points1.CHR),
                      IntToStr(Points1.AGL),
                      IntToStr(Points1.STR),
                      IntToStr(Points1.Points.PTS)]));
 end;

 procedure vacuum;
 var
  now,time:Int64;
  Points:TPlayer;
 begin
  Points.Load(data);
  now:=DateTimeToUnix(sysutils.Now,False);
  time:=data.Path['duel.time'].AsInt(0);
  if (time=0) or ((time+Points.duel_kd_time)<=Now) then
  begin
   Save40nul(data,'duel.time',0);
  end;
  time:=data.Path['kick._in'].AsInt(0);
  if (time=0) or ((time+Points.kick_in_time)<=Now) then
  begin
   Save40nul(data,'kick._in',0);
  end;
  time:=data.Path['kick._out'].AsInt(0);
  if (time=0) or ((time+Points.kick_out_time)<=Now) then
  begin
   Save40nul(data,'kick._out',0);
  end;
  if (data.Path['kick'].Count=0) then
  begin
   data.Delete('kick');
  end;
  time:=data.Path['rst.time'].AsInt(0);
  if (time=0) or ((time+vor_rpg.rst.kd)<=Now) then
  begin
   Save40nul(data,'rst.time',0);
  end;
  if (data.Path['rst'].Count=0) then
  begin
   data.Delete('rst');
  end;
 end;

 procedure DoRst;
 var
  now,time:Int64;
  _is_rst_time:Boolean;

  procedure _rst_time; inline;
  begin
   Save40nul(data,'rst.time',0);
   if (data.Path['rst'].Count=0) then
   begin
    data.Delete('rst');
   end;
  end;

  procedure _not_msg; inline;
  begin
   _rst_time;
   SetDBRpgUser1(user,data,@OnUnlock);
   push_irc_msg(Format(vor_rpg.rst.not_msg,[src]));
  end;

 begin
  _is_rst_time:=True;
  now:=DateTimeToUnix(sysutils.Now,False);
  time:=data.Path['rst.time'].AsInt(0);
  if (time=0) or ((time+vor_rpg.rst.kd)<=Now) then
  begin
   _rst_time;
   _is_rst_time:=False;
  end;

  if _is_rst_time or is_mod then
  begin
   if not Points1.NeedReset then
   begin
    _not_msg;
   end else
   if Points1.Reset(is_mod) then
   begin
    Points1.Save(data);
    SetDBRpgUser1(user,data,@OnUnlock);
    push_irc_msg(Format(vor_rpg.rst.rst_msg,[user]));
   end else
   begin
    _rst_time;
    SetDBRpgUser1(user,data,@OnUnlock);
    push_irc_msg(Format(vor_rpg.rst.tax_msg,[user]));
   end;
  end else
  begin
   if not Points1.NeedReset then
   begin
    _not_msg;
   end else
   begin
    Points1.Save(data);
    data.Values['rst.time']:=Now;
    SetDBRpgUser1(user,data,@OnUnlock);
    push_irc_msg(Format(vor_rpg.rst.info_msg,[src,IntToStr(vor_rpg.rst.tax)]));
   end;
  end;

 end;

begin
 Points1.Load(data);

 Case cmd of
  'rst':begin
         DoRst;
         Exit;
        end;
 end;

 if is_mod then
  Case cmd of
   'vacuum':begin
             vacuum;
             Points1.Save(data);
             SetDBRpgUser1(user,data,@OnUnlock);
             Exit;
            end;
   'dbf.add':begin
              do_debuf({'',}user,data,Points1.CHR);
              Points1.Save(data);
              SetDBRpgUser1(user,data,@OnUnlock);
              Exit;
             end;
   'dbf.clr':begin
              Points1.Save(data);
              data.Delete('debuf');
              SetDBRpgUser1(user,data,@OnUnlock);
              push_irc_msg(Format(vor_rpg.stat_msg.debuf_pr,[src,'']));
              Exit;
             end;

   'exp':begin
          Points1.IncEXP(1);
          lvl_msg;
          Points1.Save(data);
          SetDBRpgUser1(user,data,@OnUnlock);
          Exit;
         end;
   'lvl':begin
          Inc(Points1.Points.LVL);
          lvl_msg;
          Points1.Save(data);
          SetDBRpgUser1(user,data,@OnUnlock);
          Exit;
         end;
   'pts':begin
          Inc(Points1.Points.PTS);
          pts_msg;
          Points1.Save(data);
          SetDBRpgUser1(user,data,@OnUnlock);
          Exit;
         end;
  end;

 if (Points1.Points.PTS>0) or is_mod then
 begin
  do_inc:=False;
  Case cmd of
   'luk':do_inc:=try_inc(Points1.Points.LUK,vor_rpg.calc.MAX_LUK);
   'def':do_inc:=try_inc(Points1.Points.DEF,vor_rpg.calc.MAX_DEF);
   'chr':do_inc:=try_inc(Points1.Points.CHR,vor_rpg.calc.MAX_CHR);
   'agl':do_inc:=try_inc(Points1.Points.AGL,vor_rpg.calc.MAX_AGL);
   'str':do_inc:=try_inc(Points1.Points.STR,vor_rpg.calc.MAX_STR);
  end;

  if do_inc then
  begin
   if not is_mod then
   begin
    Dec(Points1.Points.PTS);
   end;
   push_irc_msg(Format(vor_rpg.stat_msg.add_msg,[src,cmd]));
  end else
  begin
   push_irc_msg(Format(vor_rpg.stat_msg.max_msg,[src,cmd]));
  end;

 end else
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.not_msg,[src,cmd]));
 end;

 Points1.Save(data);
 SetDBRpgUser1(user,data,@OnUnlock);
end;

Procedure TSubPtsScript.OnEvent;
var
 Points1:TPlayer;

 procedure lvl_msg;
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.lvl_msg,[src,
                      IntToStr(Points1.Points.LVL),
                      IntToStr(Points1.Points.EXP),
                      IntToStr(Points1.Points.GetExpToLvl)]));
 end;

 procedure pts_msg;
 begin
  push_irc_msg(Format(vor_rpg.stat_msg.pts_msg,[src,
                      IntToStr(Points1.LUK),
                      IntToStr(Points1.DEF),
                      IntToStr(Points1.CHR),
                      IntToStr(Points1.AGL),
                      IntToStr(Points1.STR),
                      IntToStr(Points1.Points.PTS)]));
 end;

 procedure dec_fix_min(var i:Int64); inline;
 begin
  Dec(i);
  if (i<0) then i:=0;
 end;

begin
 Points1.Load(data);

 Case cmd of
  'exp':begin
         Points1.Points.TryDecExp;
         lvl_msg;
        end;
  'lvl':begin
         dec_fix_min(Points1.Points.LVL);
         lvl_msg;
        end;
  'pts':begin
         dec_fix_min(Points1.Points.PTS);
         pts_msg;
        end;
  'luk':begin
         dec_fix_min(Points1.Points.LUK);
         pts_msg;
        end;
  'def':begin
         dec_fix_min(Points1.Points.DEF);
         pts_msg;
        end;
  'chr':begin
         dec_fix_min(Points1.Points.CHR);
         pts_msg;
        end;
  'agl':begin
         dec_fix_min(Points1.Points.AGL);
         pts_msg;
        end;
  'str':begin
         dec_fix_min(Points1.Points.STR);
         pts_msg;
        end;
 end;

 Points1.Save(data);
 SetDBRpgUser1(user,data,@OnUnlock);
end;

Procedure add_pts(Const src,user,cmd:RawByteString;is_mod:Boolean);
Var
 FDbcScript:TDbcScriptLock;
 FAddPtsScript:TAddPtsScript;
begin

 FAddPtsScript:=TAddPtsScript.Create;
 FAddPtsScript.src:=src;
 FAddPtsScript.user:=user;
 FAddPtsScript.cmd:=cmd;
 FAddPtsScript.is_mod:=is_mod;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FAddPtsScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);
end;

Procedure sub_pts(Const src,user,cmd:RawByteString);
Var
 FDbcScript:TDbcScriptLock;
 FSubPtsScript:TSubPtsScript;
begin

 FSubPtsScript:=TSubPtsScript.Create;
 FSubPtsScript.src:=src;
 FSubPtsScript.user:=user;
 FSubPtsScript.cmd:=cmd;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FSubPtsScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);
end;

//kick

type
 TKickScript=class(TDualLockScript)
  public
   is_mod:Boolean;
   procedure OnGetRnd(Sender:TBaseTask);
   Procedure OnEvent; override;
 end;

procedure GetDBRndUser(N:TNotifyTask);
var
 FDbcScript:TDbcStatementScript;
begin
 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,N);
 FDbcScript.SetSctipt(FGetRndUser1);
 FDbcScript.ExecuteScript;
 FDbcScript.Start;
 FDbcScript.Release;
end;

Procedure kick(const dst_user,msg:RawByteString;is_mod:Boolean); forward;

procedure TKickScript.OnGetRnd(Sender:TBaseTask);
var
 ResultSet:TZResultSet;
 u:RawByteString;
begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;
 if ResultSet<>nil then
 if ResultSet.First then
 begin
  u:=ResultSet.GetRawByteString(ResultSet.FindColumn('user'));
  kick(user[0],u,is_mod);
 end;
 OnUnlock(Sender);
end;

Procedure TKickScript.OnEvent;
var
 Points1,Points2:TPlayer;
 Val:Int64;
 rnd:Integer;
 Now,time:Int64;
begin

 Points1.Load(data[0]);
 Points2.Load(data[1]);

 Now:=DateTimeToUnix(sysutils.Now,False);

 time:=data[0].Path['kick._in'].AsInt(0);
 if (time<>0) and ((time+Points1.kick_in_time)>Now) then
 begin
  time:=(time+Points1.kick_in_time)-Now;
  push_irc_msg(Format(vor_rpg.kick.in_msg,[user[0],GetLongStrTime(time)]));
  OnUnlock(nil);
  Exit;
 end;
 data[0].Values['kick._in']:=Now;

 if not data[1].isAssigned then
 begin
  GetDBRndUser(@OnGetRnd);
  Exit;
 end;

 if (user[0]=user[1]) then
 begin
  data[1].Free;
  data[1]:=data[0];
 end else
 if not data[1].Path['points'].isAssigned then
 begin
  //not found in base

  data[0].Delete('kick._in'); //off timeout

  push_irc_msg(Format(get_random_msg(vor_rpg.kick.not_vor),[user[0]]));

  if is_mod then
  begin
   OnUnlock(nil);
   Exit;
  end;

  SetDBRpgUser1(user[0],data[0],@OnUnlock);
  Exit;
 end;

 time:=data[1].Path['kick._out'].AsInt(0);
 if (time<>0) and ((time+Points2.kick_out_time)>Now) then
 begin
  time:=(time+Points2.kick_out_time)-Now;
  push_irc_msg(Format(vor_rpg.kick.out_msg,[user[1],GetLongStrTime(time)]));
  OnUnlock(nil);
  Exit;
 end;
 data[1].Values['kick._out']:=Now;

 val:=vor_rpg.kick.PERC+Points1.GetESCPercent-Points2.GetESCPercent;
 if is_mod then val:=val+10;
 val:=MMP(val);
 rnd:=Random(RCT,100);
 if (rnd<val) then
 begin
  val:=vor_rpg.kick.PERC+Points1.GetSTRPercent-Points2.GetDEFPercent-1;
  if is_mod then val:=val+10;
  val:=MMP(val);
  rnd:=Random(RCT,100);
  if (rnd<val) then
  begin
   //kick is
   push_irc_msg(Format(get_random_msg(vor_rpg.kick.go_kick),[user[0],user[1]]));
   do_debuf({'',}user[1],data[1],Points2.CHR);
  end else
  begin
   //do def
   push_irc_msg(Format(get_random_msg(vor_rpg.kick.go_def),[user[0],user[1]]));
   try_debuf({'',}user[0],data[0],Points1.CHR);
  end;
 end else
 begin
  //escape
  push_irc_msg(Format(get_random_msg(vor_rpg.kick.go_esc),[user[0],user[1]]));
  try_debuf({'',}user[0],data[0],Points1.CHR);
 end;

 if (user[0]=user[1]) then
 begin
  data[1]:=Default(TJson);
  SetDBRpgUser1(user[0],data[0],@OnUnlock);
 end else
 begin
  SetDBRpgUser2(user[0],user[1],data[0],data[1],@OnUnlock);
 end;

end;

Procedure kick(const dst_user,msg:RawByteString;is_mod:Boolean);
Var
 FDbcScript:TDbcScriptLock;
 src_user:RawByteString;
 FKickScript:TKickScript;
begin
 src_user:=LowerCase(Extract_nick(msg));

 FKickScript:=TKickScript.Create;
 FKickScript.user[0]:=dst_user;
 FKickScript.user[1]:=src_user;
 FKickScript.is_mod:=is_mod;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FKickScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);

end;

procedure TFrmVorRpg.check_xchg_vip_time;
var
 i:TxchgNodeSet.TIterator;
 link:PxchgVip;
begin
 repeat
  i:=xchgSet.Min;
  if not Assigned(i) then Exit;
  repeat
   link:=PxchgNode(i.Data)^.link;
   if Assigned(link) then
   begin
    if (GetTickCount64>link^.time+vor_rpg.xchg.max_time*(1000*60)) then
    begin
     FreeAndNil(i);
     xchgSet.Delete(@link^.src);
     xchgSet.Delete(@link^.dst);
     Finalize(link^);
     FreeMem(link);
     Break;
    end;
   end;
   if not i.Next then
   begin
    FreeAndNil(i);
    Exit;
   end;
  until false;
 until false;
end;

type
 Tadd2xchgVipScript=class(TDualLockScript)
  public
   Procedure OnEvent; override;
 end;

Procedure Tadd2xchgVipScript.OnEvent;
var
 Points1,Points2:TPlayer;
 Now,time:Int64;

 xchgNode:TxchgNode;
 link:PxchgVip;
begin
 xchgNode:=Default(TxchgNode);
 xchgNode.user:=user[0];
 if xchgSet.NFind(@xchgNode)<>nil then
 begin
  OnUnlock(nil);
  Exit;
 end;

 xchgNode:=Default(TxchgNode);
 xchgNode.user:=user[1];
 if xchgSet.NFind(@xchgNode)<>nil then
 begin
  OnUnlock(nil);
  Exit;
 end;

 if (xchgSet.Size>vor_rpg.xchg.max_count*2) then
 begin
  push_irc_msg(Format(vor_rpg.xchg.max_msg,[user[0]]));
  OnUnlock(nil);
  Exit;
 end;

 Points1.Load(data[0]);
 Points2.Load(data[1]);

 Now:=DateTimeToUnix(sysutils.Now,False);

 time:=data[0].Path['xchg.kd_time'].AsInt(0);
 if (time<>0) and ((time+vor_rpg.xchg.kd_time)>Now) then
 begin
  time:=(time+vor_rpg.xchg.kd_time)-Now;
  push_irc_msg(Format(vor_rpg.xchg.time_msg1,[user[0],GetLongStrTime(time)]));
  OnUnlock(nil);
  Exit;
 end;

 time:=data[1].Path['xchg.kd_time'].AsInt(0);
 if (time<>0) and ((time+vor_rpg.xchg.kd_time)>Now) then
 begin
  time:=(time+vor_rpg.xchg.kd_time)-Now;
  push_irc_msg(Format(vor_rpg.xchg.time_msg2,[user[0],user[1],GetLongStrTime(time)]));
  OnUnlock(nil);
  Exit;
 end;

 link:=AllocMem(SizeOf(TxchgVip));
 link^.src.user:=user[0];
 link^.src.link:=link;
 link^.dst.user:=user[1];
 link^.dst.link:=link;
 link^.time:=GetTickCount64;

 xchgSet.Insert(@link^.src);
 xchgSet.Insert(@link^.dst);

 push_irc_msg(Format(vor_rpg.xchg.ready_msg,[user[1],IntToStr(vor_rpg.xchg.max_time)]));

 OnUnlock(nil);
end;

procedure TFrmVorRpg.add2xchgVip(const user,nick:RawByteString);
var
 xchgNode:TxchgNode;

 FDbcScript:TDbcScriptLock;
 FXchgScript:Tadd2xchgVipScript;
begin

 if (nick='') or (user=nick) then Exit;

 if (FrmVipParam.FindVipUser(user)=-1) then Exit;
 if (FrmVipParam.FindVipUser(nick)<>-1) then Exit;

 check_xchg_vip_time;

 xchgNode:=Default(TxchgNode);
 xchgNode.user:=user;
 if xchgSet.NFind(@xchgNode)<>nil then
 begin
  push_irc_msg(Format(vor_rpg.xchg.exist1_msg,[user]));
  Exit;
 end;

 xchgNode:=Default(TxchgNode);
 xchgNode.user:=nick;
 if xchgSet.NFind(@xchgNode)<>nil then
 begin
  push_irc_msg(Format(vor_rpg.xchg.exist2_msg,[user,nick]));
  Exit;
 end;

 if (xchgSet.Size>vor_rpg.xchg.max_count*2) then
 begin
  push_irc_msg(Format(vor_rpg.xchg.max_msg,[user]));
  Exit;
 end;

 FXchgScript:=Tadd2xchgVipScript.Create;
 FXchgScript.user[0]:=user;
 FXchgScript.user[1]:=nick;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FXchgScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);
end;

Procedure IncJInt64(J:TJson;const f:RawByteString;delta:Int64); inline;
begin
 J.Values[f]:=J.Path[f].AsInt64(0)+delta;
end;

type
 TxchgVipScript=class(TDualLockScript)
  public
   Procedure OnEvent; override;
 end;

Procedure TxchgVipScript.OnEvent;
var
 xchgNode:TxchgNode;
 link:PxchgVip;
 Node:TxchgNodeSet.PNode;

 Now:Int64;
begin
 xchgNode:=Default(TxchgNode);
 xchgNode.user:=user[1];
 Node:=xchgSet.NFind(@xchgNode);
 if (Node<>nil) then
 begin
  link:=PxchgNode(Node^.Data)^.link;
  if (link^.src.user=user[0]) and (link^.dst.user=user[1]) then
  if (FrmVipParam.FindVipUser(user[0])<>-1) then
  if (FrmVipParam.FindVipUser(user[1])=-1) then
  begin
   push_irc_msg(Format(vor_rpg.xchg.sucs_msg,[link^.dst.user]));
   ChangeVip(link^.src.user,link^.dst.user);

   xchgSet.Delete(@link^.src);
   xchgSet.Delete(@link^.dst);
   Finalize(link^);
   FreeMem(link);

   Now:=DateTimeToUnix(sysutils.Now,False);
   data[0].Values['xchg.kd_time']:=Now;
   data[1].Values['xchg.kd_time']:=Now;
   IncJInt64(data[0],'xchg._out',1);
   IncJInt64(data[1],'xchg._in' ,1);
   SetDBRpgUser2(user[0],user[1],data[0],data[1],@OnUnlock);
   Exit;
  end;
 end;
 OnUnlock(nil);
end;

procedure TFrmVorRpg.catch_vip(const user:RawByteString);
var
 xchgNode:TxchgNode;
 link:PxchgVip;
 Node:TxchgNodeSet.PNode;

 FDbcScript:TDbcScriptLock;
 FXchgScript:TxchgVipScript;
begin
 check_xchg_vip_time;

 if (FrmVipParam.FindVipUser(user)<>-1) then
 begin
  xchgNode:=Default(TxchgNode);
  xchgNode.user:=user;
  Node:=xchgSet.NFind(@xchgNode);
  if (Node<>nil) then
  begin
   link:=PxchgNode(Node^.Data)^.link;
   if (link^.src.user=user) then
   begin
    push_irc_msg(Format(vor_rpg.xchg.cancel_msg,[user]));

    xchgSet.Delete(@link^.src);
    xchgSet.Delete(@link^.dst);
    Finalize(link^);
    FreeMem(link);

    Exit;
   end;
  end;
 end;

 xchgNode:=Default(TxchgNode);
 xchgNode.user:=user;
 Node:=xchgSet.NFind(@xchgNode);
 if (Node<>nil) then
 begin
  link:=PxchgNode(Node^.Data)^.link;
  if (link^.dst.user=user) then
  begin
   FXchgScript:=TxchgVipScript.Create;
   FXchgScript.user[0]:=link^.src.user;
   FXchgScript.user[1]:=link^.dst.user;

   FDbcScript:=TDbcScriptLock.Create;
   FDbcScript.Prepare(FXchgScript);
   FDbcScript.AsyncFunc(@FDbcScript.try_start);
  end;
 end;

end;

Procedure TFrmVorRpg.vip_time(const user:RawByteString);
var
 i:Integer;
 datebeg,dateend:RawByteString;
 D:TDateTime;
begin
 i:=FrmVipParam.FindVipUser(user);
 datebeg:=GridVips.FieldValue['datebeg',i];
 dateend:=GridVips.FieldValue['dateend',i];
 if (i<>-1) then
 begin
  if TryGetDateTime_US(datebeg,D) then
  begin
   datebeg:=DateTimeToStr_RU(D);
  end;
  if TryGetDateTime_US(dateend,D) then
  begin
   dateend:=DateTimeToStr_RU(D);
  end;
  push_irc_msg(Format(vip_rnd.viptime_get_info,[user,user,datebeg,dateend]));
 end;
end;

procedure TFrmVorRpg.check_duel_time;

 Procedure _check(duelSet:TxchgNodeSet);
 var
  i:TxchgNodeSet.TIterator;
  link:PxchgVip;
 begin
  repeat
   i:=duelSet.Min;
   if not Assigned(i) then Exit;
   repeat
    link:=PxchgNode(i.Data)^.link;
    if Assigned(link) then
    begin
     if (GetTickCount64>link^.time+vor_rpg.duel.max_time*(1000*60)) then
     begin
      FreeAndNil(i);
      duelSet.Delete(@link^.src);
      duelSet.Delete(@link^.dst);
      Finalize(link^);
      FreeMem(link);
      Break;
     end;
    end;
    if not i.Next then
    begin
     FreeAndNil(i);
     Exit;
    end;
   until false;
  until false;
 end;

begin
 _check(duelSeta[0]);
 _check(duelSeta[1]);
end;

type
 TDuelAddScript=class(TOneLockScript)
  public
   is_mod:Boolean;
   nick:RawByteString;
   Procedure OnEvent; override;
 end;
 TDuelScript=class(TDualLockScript)
  public
   is_mod:array[0..1] of Boolean;
   Procedure OnEvent; override;
 end;

Procedure TDuelScript.OnEvent;
var
 Points:array[0..1] of TPlayer;
 HP:array[0..1] of Integer;
 i,osrc,odst,rnd:Integer;
 Val,Now:Int64;
begin

 Now:=DateTimeToUnix(sysutils.Now,False);
 data[0].Values['duel.time']:=Now;
 data[1].Values['duel.time']:=Now;

 Points[0].Load(data[0]);
 Points[1].Load(data[1]);

 HP[0]:=10;
 HP[1]:=10;
 osrc:=1;
 odst:=0;
 For i:=0 to 99 do
 begin
  Val:=50;
  val:=Val+Points[osrc].GetLUKPercent+Points[osrc].GetSTRPercent+Points[osrc].GetAGLPercent;
  val:=Val-Points[odst].GetLUKPercent-Points[odst].GetDEFPercent-Points[odst].GetAGLPercent;
  val:=MMP(val);
  rnd:=Random(RCT,100);
  if (rnd<Val) then
  begin
   Dec(HP[odst]);
   if HP[odst]<=0 then Break;
  end else
  begin
   Val:=10;
   val:=Val+Points[odst].GetLUKPercent+Points[odst].GetAGLPercent;
   val:=Val-Points[osrc].GetDEFPercent-Points[osrc].GetAGLPercent;
   val:=MMP(val);
   rnd:=Random(RCT,100);
   if (rnd<Val) then
   begin
    Dec(HP[osrc]);
    Dec(HP[osrc]);
    if HP[osrc]<=0 then Break;
   end;
  end;
  case osrc of
   0:begin
      osrc:=1;
      odst:=0;
     end;
   1:begin
      osrc:=0;
      odst:=1;
     end;
  end;
 end;

 if (HP[0]=HP[1]) then
 begin
  push_irc_msg(Format(get_random_msg(vor_rpg.duel.stand_msg),[User[0],User[1]]));
  Points[0].Save(data[0]);
  Points[1].Save(data[1]);
  SetDBRpgUser2(user[0],user[1],data[0],data[1],@OnUnlock);
  Exit;
 end else
 if (HP[0]>HP[1]) then
 begin
  osrc:=0;
  odst:=1;
 end else
 begin
  osrc:=1;
  odst:=0;
 end;

 IncJInt64(data[osrc],'rate.win' ,1); //winner
 IncJInt64(data[odst],'rate.lose',1); //loser

 if Points[odst].Points.TryDecExp then //loser
 begin
  Points[osrc].IncEXP(1); //winner
 end;

 i:=0;
 if (HP[odst]<=0) then
 begin
  Val:=vor_rpg.duel.PERC_MINUS_VIP+Points[osrc].GetLUKPercent+Points[osrc].GetSTRPercent;
  val:=MMP(val);
  rnd:=Random(RCT,100);
  if (rnd<val) then
  begin
   if (not is_mod[0]) and (not is_mod[1]) and
      (FrmVipParam.FindVipUser(User[odst])<>-1) and
      (FrmVipParam.FindVipUser(User[osrc])=-1) then
   begin
    i:=1;
    push_irc_msg(Format(get_random_msg(vor_rpg.duel.vip_msg),[User[osrc],User[odst]]));
    ChangeVip(User[odst],User[osrc]);
   end else
   begin
    Points[osrc].IncEXP(1);
   end;
  end;
 end;

 if (i=0) then
  push_irc_msg(Format(get_random_msg(vor_rpg.duel.win_msg),[User[osrc],User[odst]]));

 try_debuf({'',}User[odst],data[odst],Points[odst].CHR);

 Points[0].Save(data[0]);
 Points[1].Save(data[1]);
 SetDBRpgUser2(user[0],user[1],data[0],data[1],@OnUnlock);
 Exit;
end;

Procedure TDuelAddScript.OnEvent;
var
 Points:TPlayer;

 duelSet:TxchgNodeSet;
 P:TxchgNodeSet.PNode;
 Node:TxchgNode;
 link:PxchgVip;

 Procedure _go2duel;
 var
  FDbcScript:TDbcScriptLock;
  FDuelScript:TDuelScript;
 begin
  FDuelScript:=TDuelScript.Create;
  FDuelScript.user[0]:=user;
  FDuelScript.user[1]:=link^.src.user;

  FDuelScript.is_mod[0]:=is_mod;
  FDuelScript.is_mod[1]:=link^.is_mod;

  FDbcScript:=TDbcScriptLock.Create;
  FDbcScript.Prepare(FDuelScript);
  FDbcScript.AsyncFunc(@FDbcScript.try_start);

  duelSet.Delete(@link^.src);
  duelSet.Delete(@link^.dst);
  Finalize(link^);
  FreeMem(link);
 end;

 function _check_time:Boolean;
 var
  Now,time:Int64;
 begin
  Result:=False;
  Now:=DateTimeToUnix(sysutils.Now,False);
  time:=data.Path['duel.time'].AsInt(0);
  if (time<>0) and ((time+Points.duel_kd_time)>Now) then
  begin
   time:=(time+Points.duel_kd_time)-Now;
   push_irc_msg(Format(vor_rpg.duel.time_msg,[user,GetLongStrTime(time)]));
   OnUnlock(nil);
   Result:=True;
  end;
 end;

begin
 Points.Load(data);
 if vor_rpg.duel.check_zero and (Points.Points.LVL<=0) and (Points.Points.EXP<=0) then
 begin //is zero
  duelSet:=duelSeta[0];
 end else
 begin
  duelSet:=duelSeta[1];
 end;

 Node:=Default(TxchgNode);
 Node.user:=user;
 P:=duelSet.NFind(@Node);
 if (P<>nil) then
 begin
  link:=PxchgNode(P^.Data)^.link;
  if (PxchgNode(P^.Data)=@link^.src) then
  begin
   push_irc_msg(Format(vor_rpg.duel.cancel_msg,[user]));
   duelSet.Delete(@link^.src);
   duelSet.Delete(@link^.dst);
   Finalize(link^);
   FreeMem(link);
  end else
  if (nick='') or (link^.src.user=nick) then
  begin
   if _check_time then Exit;
   _go2duel;
  end else
  begin
   push_irc_msg(Format(vor_rpg.duel.exist1_msg,[user,link^.src.user]));
  end;
  OnUnlock(nil);
  Exit;
 end;

 Node:=Default(TxchgNode);
 Node.user:=nick;
 P:=duelSet.NFind(@Node);
 if (P<>nil) then
 begin
  link:=PxchgNode(P^.Data)^.link;
  if (nick<>'') and (link^.dst.user<>'') then
  begin
   push_irc_msg(Format(vor_rpg.duel.exist2_msg,[user]));
  end else
  begin
   if _check_time then Exit;
   _go2duel;
  end;
  OnUnlock(nil);
  Exit;
 end;

 if (duelSet.Size>vor_rpg.duel.max_count*2) then
 begin
  push_irc_msg(Format(vor_rpg.duel.max_msg,[user]));
  OnUnlock(nil);
  Exit;
 end;

 if _check_time then Exit;

 link:=AllocMem(SizeOf(TxchgVip));
 link^.is_mod:=is_mod;

 link^.src.user:=user;
 link^.src.link:=link;
 link^.dst.user:=nick;
 link^.dst.link:=link;
 link^.time:=GetTickCount64;

 duelSet.Insert(@link^.src);
 duelSet.Insert(@link^.dst);

 if (vor_rpg.duel.any_msg='') then
 begin
  vor_rpg.duel.any_msg:='Any';
 end;

 if (nick='') then
 begin
  push_irc_msg(Format(vor_rpg.duel.ready_msg,[vor_rpg.duel.any_msg,IntToStr(vor_rpg.duel.max_time),user]));
 end else
 begin
  push_irc_msg(Format(vor_rpg.duel.ready_msg,[nick,IntToStr(vor_rpg.duel.max_time),user]));
 end;

 OnUnlock(nil);
end;

procedure TFrmVorRpg.add2duel(const user,nick:RawByteString;is_mod:Boolean);
var
 FDbcScript:TDbcScriptLock;
 FDuelScript:TDuelAddScript;
begin
 if (user=nick) then Exit;

 check_duel_time;

 FDuelScript:=TDuelAddScript.Create;
 FDuelScript.user:=user;
 FDuelScript.nick:=nick;
 FDuelScript.is_mod:=is_mod;

 FDbcScript:=TDbcScriptLock.Create;
 FDbcScript.Prepare(FDuelScript);
 FDbcScript.AsyncFunc(@FDbcScript.try_start);
end;

function in_TickKd:Boolean; inline;
begin
 Result:=(GetTickCount64<vor_rpg.TickKd+vor_rpg.time_kd*1000);
end;

procedure up_TickKd; inline;
begin
 vor_rpg.TickKd:=GetTickCount64;
end;

procedure TFrmVorRpg.add_to_chat_cmd(PC:TPrivMsgCfg;const user,cmd,param:RawByteString);
var
 F,v:RawByteString;

begin
 F:=UTF8Encode(UnicodeLowerCase(UTF8Decode(Trim(param))));

 v:=UTF8Encode(UnicodeLowerCase(UTF8Decode(cmd)));
 Case v of
  '!вип',
  '!vip':
  begin
   v:=Trim(FetchAny(F));

   Case v of
    '',
    'time',
    'info':if (PC.PS*[pm_broadcaster,pm_moderator]=[]) then
          begin
           if in_TickKd then Exit;
           vip_time(user);
           up_TickKd;
          end;

    'give',
    'дать',
    'отдать':if vor_rpg.xchg.Enable then
             begin
              if in_TickKd then Exit;
              add2xchgVip(user,Extract_nick(FetchAny(F)));
              up_TickKd;
             end;

    'me',
    'take',
    'моя',
    'мне',
    'сюда',
    'забрать':if vor_rpg.xchg.Enable then
              begin
               if in_TickKd then Exit;
               catch_vip(user);
               up_TickKd;
              end;
   end;

  end;

  '!kick',
  '!пнуть':
  if vor_rpg.kick.Enable then
  begin
   if (PC.PS*[pm_broadcaster,pm_moderator]=[]) and in_TickKd then Exit;
   kick(user,F,PC.PS*[pm_broadcaster,pm_moderator]<>[]);
   up_TickKd;
  end;

  '!duel',
  '!дуэль',
  '!дуель':
  if vor_rpg.duel.Enable then
  begin
   if (PC.PS*[pm_broadcaster,pm_moderator]=[]) and in_TickKd then Exit;
   add2duel(user,Extract_nick(FetchAny(F)),PC.PS*[pm_broadcaster,pm_moderator]<>[]);
   up_TickKd;
  end;

  {$IFOPT D+}
  '!ban_test':
  begin
   if in_TickKd then Exit;
   F:=FetchAny(F);
   rpg_theif_vip('',user,F);
   up_TickKd;
  end;
  {$ENDIF}

  '!vor':
  begin
   if (PC.PS*[pm_broadcaster,pm_moderator]=[]) and in_TickKd then Exit;

   if (F='') then
   begin
    push_irc_msg(Format(vor_rpg.stat_msg.help_msg1,[user]));
   end else
   begin

    v:=FetchAny(F);

    case v of
     'mod':
      if (PC.PS*[pm_broadcaster,pm_moderator]<>[]) then
      begin
       v:=FetchAny(F);
       Case v of
        'base':begin
                v:=FetchAny(F);
                try
                 case v of
                  'pmvip':begin
                          v:=FetchAny(F);
                          vor_rpg.calc.PERC_MINUS_VIP:=StrToDWORDDef(v,vor_rpg.calc.PERC_MINUS_VIP);
                          if vor_rpg.calc.PERC_MINUS_VIP>100 then vor_rpg.calc.PERC_MINUS_VIP:=100;
                          Config.WriteString('vor_rpg','PERC_MINUS_VIP',IntToStr(vor_rpg.calc.PERC_MINUS_VIP));
                          push_irc_msg(Format('@%s pmvip %s',[user,IntToStr(vor_rpg.calc.PERC_MINUS_VIP)]));
                         end;
                    'kd':begin
                          v:=FetchAny(F);
                          vor_rpg.time_kd:=StrToDWORDDef(v,vor_rpg.time_kd);
                          Config.WriteString('vor_rpg','time_kd',IntToStr(vor_rpg.time_kd));
                          push_irc_msg(Format('@%s kd %s',[user,IntToStr(vor_rpg.time_kd)]));
                         end;
               'timeout':begin
                          v:=FetchAny(F);
                          vor_rpg.calc.BASE_TIME:=StrToDWORDDef(v,vor_rpg.calc.BASE_TIME);
                          Config.WriteString('vor_rpg','BASE_TIME',IntToStr(vor_rpg.calc.BASE_TIME));
                          push_irc_msg(Format('@%s timeout %s',[user,IntToStr(vor_rpg.calc.BASE_TIME)]));
                         end;

                  else
                   push_irc_msg(Format('@%s !vor mod base pmvip,kd,timeout',[user]));
                 end;
                except
                 on E:Exception do
                 begin
                  DumpExceptionCallStack(E);
                 end;
                end;
               end;

        'kick':begin
                v:=FetchAny(F);
                try
                 case v of
                     'on':if not vor_rpg.kick.Enable then
                          begin
                           vor_rpg.kick.Enable:=True;
                           Config.WriteString('vor_rpg' ,'kick_enable','1');
                           push_irc_msg(Format('@%s kick on',[user]));
                          end;
                    'off':if vor_rpg.kick.Enable then
                          begin
                           vor_rpg.kick.Enable:=False;
                           Config.WriteString('vor_rpg' ,'kick_enable','0');
                           push_irc_msg(Format('@%s kick off',[user]));
                          end;
                   'perc':begin
                           v:=FetchAny(F);
                           vor_rpg.kick.PERC:=StrToDWORDDef(v,vor_rpg.kick.PERC);
                           if vor_rpg.kick.PERC>100 then vor_rpg.kick.PERC:=100;
                           Config.WriteString('vor_rpg','kick_PERC',IntToStr(vor_rpg.kick.PERC));
                           push_irc_msg(Format('@%s perc %s',[user,IntToStr(vor_rpg.kick.PERC)]));
                          end;
                     'in':begin
                           v:=FetchAny(F);
                           vor_rpg.kick.in_time:=StrToDWORDDef(v,vor_rpg.kick.in_time);
                           Config.WriteString('vor_rpg','in_time'  ,IntToStr(vor_rpg.kick.in_time));
                           push_irc_msg(Format('@%s in %s',[user,IntToStr(vor_rpg.kick.in_time)]));
                          end;
                    'out':begin
                           v:=FetchAny(F);
                           vor_rpg.kick.out_time:=StrToDWORDDef(v,vor_rpg.kick.out_time);
                           Config.WriteString('vor_rpg','out_time'  ,IntToStr(vor_rpg.kick.out_time));
                           push_irc_msg(Format('@%s out %s',[user,IntToStr(vor_rpg.kick.out_time)]));
                          end;

                  else
                   push_irc_msg(Format('@%s !vor mod kick on/off,perc,in,out',[user]));
                 end;
                except
                 on E:Exception do
                 begin
                  DumpExceptionCallStack(E);
                 end;
                end;
               end;

        'xchg':begin
                v:=FetchAny(F);
                try
                 case v of
                     'on':if not vor_rpg.xchg.Enable then
                          begin
                           vor_rpg.xchg.Enable:=True;
                           Config.WriteString('vor_rpg' ,'xchg_enable','1');
                           push_irc_msg(Format('@%s xchg on',[user]));
                          end;
                    'off':if vor_rpg.xchg.Enable then
                          begin
                           vor_rpg.xchg.Enable:=False;
                           Config.WriteString('vor_rpg' ,'xchg_enable','0');
                           push_irc_msg(Format('@%s xchg off',[user]));
                          end;
                  'count':begin
                           v:=FetchAny(F);
                           vor_rpg.xchg.max_count:=StrToDWORDDef(v,vor_rpg.xchg.max_count);
                           Config.WriteString('vor_rpg','xchg_max_count',IntToStr(vor_rpg.xchg.max_count));
                           push_irc_msg(Format('@%s count %s',[user,IntToStr(vor_rpg.xchg.max_count)]));
                          end;
                   'time':begin
                           v:=FetchAny(F);
                           vor_rpg.xchg.max_time:=StrToDWORDDef(v,vor_rpg.xchg.max_time);
                           Config.WriteString('vor_rpg','xchg_max_time',IntToStr(vor_rpg.xchg.max_time));
                           push_irc_msg(Format('@%s time %s',[user,IntToStr(vor_rpg.xchg.max_time)]));
                          end;
                  else
                   push_irc_msg(Format('@%s !vor mod xchg on/off,count,time',[user]));
                 end;
                except
                 on E:Exception do
                 begin
                  DumpExceptionCallStack(E);
                 end;
                end;
               end;

        'duel':begin
                v:=FetchAny(F);
                try
                 case v of
                     'on':if not vor_rpg.duel.Enable then
                          begin
                           vor_rpg.duel.Enable:=True;
                           Config.WriteString('vor_rpg' ,'duel_enable','1');
                           push_irc_msg(Format('@%s duel on',[user]));
                          end;
                    'off':if vor_rpg.duel.Enable then
                          begin
                           vor_rpg.duel.Enable:=False;
                           Config.WriteString('vor_rpg' ,'duel_enable','0');
                           push_irc_msg(Format('@%s duel off',[user]));
                          end;
                   'chzr':begin
                           v:=FetchAny(F);
                           case v of
                             'on':if not vor_rpg.duel.check_zero then
                                  begin
                                   vor_rpg.duel.check_zero:=True;
                                   Config.WriteString('vor_rpg' ,'duel_check_zero','1');
                                   push_irc_msg(Format('@%s duel check zero on',[user]));
                                  end;
                            'off':if vor_rpg.duel.check_zero then
                                  begin
                                   vor_rpg.duel.check_zero:=False;
                                   Config.WriteString('vor_rpg' ,'duel_check_zero','0');
                                   push_irc_msg(Format('@%s duel check zero off',[user]));
                                  end;
                           end;
                          end;
                  'count':begin
                           v:=FetchAny(F);
                           vor_rpg.duel.max_count:=StrToDWORDDef(v,vor_rpg.duel.max_count);
                           Config.WriteString('vor_rpg','duel_max_count',IntToStr(vor_rpg.duel.max_count));
                           push_irc_msg(Format('@%s count %s',[user,IntToStr(vor_rpg.duel.max_count)]));
                          end;
                   'time':begin
                           v:=FetchAny(F);
                           vor_rpg.duel.max_time:=StrToDWORDDef(v,vor_rpg.duel.max_time);
                           Config.WriteString('vor_rpg','duel_max_time',IntToStr(vor_rpg.duel.max_time));
                           push_irc_msg(Format('@%s max time %s',[user,IntToStr(vor_rpg.duel.max_time)]));
                          end;
                     'kd':begin
                           v:=FetchAny(F);
                           vor_rpg.duel.kd_time:=StrToDWORDDef(v,vor_rpg.duel.kd_time);
                           Config.WriteString('vor_rpg','duel_kd_time',IntToStr(vor_rpg.duel.kd_time));
                           push_irc_msg(Format('@%s time kd %s',[user,IntToStr(vor_rpg.duel.kd_time)]));
                          end;
                  'pmvip':begin
                           v:=FetchAny(F);
                           vor_rpg.duel.PERC_MINUS_VIP:=StrToDWORDDef(v,vor_rpg.duel.PERC_MINUS_VIP);
                           if vor_rpg.duel.PERC_MINUS_VIP>100 then vor_rpg.duel.PERC_MINUS_VIP:=100;
                           Config.WriteString('vor_rpg','duel_PERC_MINUS_VIP',IntToStr(vor_rpg.duel.PERC_MINUS_VIP));
                           push_irc_msg(Format('@%s duel pmvip %s',[user,IntToStr(vor_rpg.duel.PERC_MINUS_VIP)]));
                          end;


                  else
                   push_irc_msg(Format('@%s !vor mod duel on/off,chzr on/off,count,time,kd,pmvip',[user]));
                 end;
                except
                 on E:Exception do
                 begin
                  DumpExceptionCallStack(E);
                 end;
                end;
               end;


        'debuf',
        'dbf':begin
               v:=FetchAny(F);
               try
                case v of
                  'add':begin
                         F:=Extract_nick(FetchAny(F));
                         if F<>'' then
                          add_pts(user,F,'dbf.add',true);
                        end;
                  'clr':begin
                         F:=Extract_nick(FetchAny(F));
                          if F<>'' then
                         add_pts(user,F,'dbf.clr',true);
                        end;
                 'info':begin
                         F:=Extract_nick(FetchAny(F));
                         if F<>'' then
                          GetDBRpgUserInfo(user,F,'dbf');
                        end;
                 'perc':begin
                         v:=FetchAny(F);
                         vor_rpg.debuf.PERC:=StrToDWORDDef(v,vor_rpg.debuf.PERC);
                         if vor_rpg.debuf.PERC>100 then vor_rpg.debuf.PERC:=100;
                         Config.WriteString('vor_rpg','DEBUF_PERCENT' ,IntToStr(vor_rpg.debuf.PERC));
                         push_irc_msg(Format('@%s perc %s',[user,IntToStr(vor_rpg.debuf.PERC)]));
                        end;
                  'min':begin
                         v:=FetchAny(F);
                         vor_rpg.debuf.MIN_TIME:=StrToDWORDDef(v,vor_rpg.debuf.MIN_TIME);
                         Config.WriteString('vor_rpg','DEBUF_MIN_TIME',IntToStr(vor_rpg.debuf.MIN_TIME));
                         push_irc_msg(Format('@%s min %s',[user,IntToStr(vor_rpg.debuf.MIN_TIME)]));
                        end;
                  'max':begin
                         v:=FetchAny(F);
                         vor_rpg.debuf.MAX_TIME:=StrToDWORDDef(v,vor_rpg.debuf.MAX_TIME);
                         Config.WriteString('vor_rpg','DEBUF_MAX_TIME',IntToStr(vor_rpg.debuf.MAX_TIME));
                         push_irc_msg(Format('@%s max %s',[user,IntToStr(vor_rpg.debuf.MAX_TIME)]));
                        end;
                 else
                  push_irc_msg(Format('@%s !vor mod dbf [perc,min,max,info/add/clr "user"]',[user]));
                end;
               except
                on E:Exception do
                begin
                 DumpExceptionCallStack(E);
                end;
               end;
              end;
        'level',
        'lvl' ,
        'points',
        'pts' ,
        'stats',
        'stat':
        begin
         F:=Extract_nick(FetchAny(F));
         if (F<>'') then
          GetDBRpgUserInfo(user,F,v);
        end;
        'add':
         begin
          v:=FetchAny(F);
          Case v of
           'pts',
           'lvl',
           'exp',
           'luk',
           'def',
           'chr',
           'agl',
           'str':
           begin
            F:=Extract_nick(FetchAny(F));
            if F<>'' then
             add_pts(user,F,v,true);
           end;
           else
            push_irc_msg(Format('@%s !vor mod add [pts,lvl,exp,luk,def,chr,agl,str] "nick"',[user]));
          end;
         end;
        'reset',
        'rst':begin
               F:=Extract_nick(FetchAny(F));
               if (F<>'') then
                add_pts(user,F,'rst',true);
              end;

        'sub':
         begin
          v:=FetchAny(F);
          Case v of
           'pts',
           'lvl',
           'exp',
           'luk',
           'def',
           'chr',
           'agl',
           'str':
           begin
            F:=Extract_nick(FetchAny(F));
            if F<>'' then
             sub_pts(user,F,v);
           end;
           else
            push_irc_msg(Format('@%s !vor mod sub [pts,lvl,exp,luk,def,chr,agl,str] "nick"',[user]));
          end;
         end;

        else
         push_irc_msg(Format('@%s !vor mod [rst,base,kick,xchg,duel,dbf,lvl,pts,stat,add [prm],sub [prm]] "nick"',[user]));
       end;
      end;
     'winrate',
     'rate',
     'wrt',
     'dbf',
     'debuf',
     'level',
     'lvl' ,
     'points',
     'pts' ,
     'stats',
     'stat':
     begin
      F:=Extract_nick(FetchAny(F));
      if (F<>'') then
      begin
       GetDBRpgUserInfo(user,F,v);
      end else
      begin
       GetDBRpgUserInfo(user,user,v);
      end;
     end;
     'add':begin
            v:=FetchAny(F);
            Case v of
             'luk',
             'def',
             'chr',
             'agl',
             'str':add_pts(user,user,v,false);
             else
              push_irc_msg(Format(vor_rpg.stat_msg.help_msg2,[user]));
            end;
           end;
     'reset',
     'rst':begin
            add_pts(user,user,'rst',false);
           end;
     'hlp',
     'help':begin
             push_irc_msg(Format(vor_rpg.stat_msg.help_msg3,[user]));
            end;
      'топ',
      'top':begin
             v:=FetchAny(F);
             _GetDBRpgUserTop(user,v);
            end;

     else
      push_irc_msg(Format(vor_rpg.stat_msg.help_msg1,[user]));
    end;

   end;

   up_TickKd;
  end;
 end;

end;

Procedure TFrmVorRpg.InitCfg;
begin
 vor_rpg.Enable:=False;
 vor_rpg.xchg.Enable:=False;
 vor_rpg.duel.Enable:=False;
 Config.WriteString('vor_rpg' ,'enable','0');
 Config.WriteString('vor_rpg' ,'xchg_enable','0');
 Config.WriteString('vor_rpg' ,'duel_enable','0');
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

 vor_rpg.xchg.Enable:=Trim(Config.ReadString('vor_rpg','xchg_enable','0'))='1';
 vor_rpg.xchg.max_count     :=StrToDWORDDef(Config.ReadString('vor_rpg','xchg_max_count',IntToStr(vor_rpg.xchg.max_count     )),vor_rpg.xchg.max_count     );
 vor_rpg.xchg.max_time      :=StrToDWORDDef(Config.ReadString('vor_rpg','xchg_max_time' ,IntToStr(vor_rpg.xchg.max_time      )),vor_rpg.xchg.max_time      );

 vor_rpg.duel.Enable:=Trim(Config.ReadString('vor_rpg','duel_enable','0'))='1';
 vor_rpg.duel.check_zero:=Trim(Config.ReadString('vor_rpg','duel_check_zero','0'))='1';
 vor_rpg.duel.max_count     :=StrToDWORDDef(Config.ReadString('vor_rpg','duel_max_count',IntToStr(vor_rpg.duel.max_count     )),vor_rpg.duel.max_count     );
 vor_rpg.duel.max_time      :=StrToDWORDDef(Config.ReadString('vor_rpg','duel_max_time' ,IntToStr(vor_rpg.duel.max_time      )),vor_rpg.duel.max_time      );
 vor_rpg.duel.kd_time       :=StrToDWORDDef(Config.ReadString('vor_rpg','duel_kd_time'  ,IntToStr(vor_rpg.duel.kd_time      )) ,vor_rpg.duel.kd_time       );
 vor_rpg.duel.PERC_MINUS_VIP:=StrToDWORDDef(Config.ReadString('vor_rpg','duel_PERC_MINUS_VIP',IntToStr(vor_rpg.duel.PERC_MINUS_VIP)),vor_rpg.duel.PERC_MINUS_VIP);

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

 CBXchgEnable.Checked   :=vor_rpg.xchg.Enable;
 EdtXchgMaxCount.Text   :=IntToStr(vor_rpg.xchg.max_count);
 EdtXchgMaxTime.Text    :=IntToStr(vor_rpg.xchg.max_time);

 CBduelEnable.Checked   :=vor_rpg.duel.Enable;
 CBduelCheckZero.Checked:=vor_rpg.duel.check_zero;
 EdtduelMaxCount.Text   :=IntToStr(vor_rpg.duel.max_count);
 EdtduelMaxTime.Text    :=IntToStr(vor_rpg.duel.max_time);
 EdtDuelKd.Text         :=IntToStr(vor_rpg.duel.kd_time);
 EdtDuelMVipPerc.Text   :=IntToStr(vor_rpg.duel.PERC_MINUS_VIP);

 if ShowModal=1 then
 begin
  vor_rpg.Enable             :=CBVorRpgEnable.Checked;
  SetClearTimer(vor_rpg.Enable);

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

  vor_rpg.xchg.Enable        :=CBXchgEnable.Checked;
  vor_rpg.xchg.max_count     :=StrToDWORDDef(EdtXchgMaxCount.Text,vor_rpg.xchg.max_count);
  vor_rpg.xchg.max_time      :=StrToDWORDDef(EdtXchgMaxTime.Text ,vor_rpg.xchg.max_time);

  vor_rpg.duel.Enable        :=CBduelEnable.Checked;
  vor_rpg.duel.check_zero    :=CBduelCheckZero.Checked;
  vor_rpg.duel.max_count     :=StrToDWORDDef(EdtduelMaxCount.Text,vor_rpg.duel.max_count);
  vor_rpg.duel.max_time      :=StrToDWORDDef(EdtduelMaxTime.Text ,vor_rpg.duel.max_time);
  vor_rpg.duel.kd_time       :=StrToDWORDDef(EdtDuelKd.Text      ,vor_rpg.duel.kd_time);
  vor_rpg.duel.PERC_MINUS_VIP:=StrToDWORDDef(EdtDuelMVipPerc.Text,vor_rpg.duel.PERC_MINUS_VIP);

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

   case vor_rpg.xchg.Enable of
    True :Config.WriteString('vor_rpg' ,'xchg_enable','1');
    False:Config.WriteString('vor_rpg' ,'xchg_enable','0');
   end;

   Config.WriteString('vor_rpg','xchg_max_count',IntToStr(vor_rpg.xchg.max_count));
   Config.WriteString('vor_rpg','xchg_max_time' ,IntToStr(vor_rpg.xchg.max_time));

   case vor_rpg.duel.Enable of
    True :Config.WriteString('vor_rpg' ,'duel_enable','1');
    False:Config.WriteString('vor_rpg' ,'duel_enable','0');
   end;

   case vor_rpg.duel.check_zero of
    True :Config.WriteString('vor_rpg' ,'duel_check_zero','1');
    False:Config.WriteString('vor_rpg' ,'duel_check_zero','0');
   end;

   Config.WriteString('vor_rpg','duel_max_count',IntToStr(vor_rpg.duel.max_count));
   Config.WriteString('vor_rpg','duel_max_time' ,IntToStr(vor_rpg.duel.max_time));
   Config.WriteString('vor_rpg','duel_kd_time'  ,IntToStr(vor_rpg.duel.kd_time));
   Config.WriteString('vor_rpg','duel_PERC_MINUS_VIP',IntToStr(vor_rpg.duel.PERC_MINUS_VIP));

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmVorRpg.FormCreate(Sender: TObject);
begin
 SetClearTimer(vor_rpg.Enable);
end;

procedure TFrmVorRpg.SetClearTimer(m:Boolean);
begin
 Case m of
  True :
  begin
   if (FClearTimer=nil) then
   begin
    FClearTimer:=TTimer.Create(Self);
    FClearTimer.Interval:=30*60*1000; //30min
    FClearTimer.OnTimer:=@OnClearProc;
   end;
   FClearTimer.Enabled:=m;
  end;
  False:
  begin
   if (FClearTimer<>nil) then
   begin
    FClearTimer.Enabled:=m;
   end;
  end;
 end;
end;

procedure TFrmVorRpg.OnClearProc(Sender:TObject);
begin
 check_xchg_vip_time;
 check_duel_time;
 GetDBRndUser(@OnClearRnd);
end;

procedure TFrmVorRpg.OnClearRnd(Sender:TBaseTask);
var
 ResultSet:TZResultSet;
 user:RawByteString;
begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;
 if ResultSet<>nil then
 if ResultSet.First then
 begin
  user:=ResultSet.GetRawByteString(ResultSet.FindColumn('user'));
  add_pts(user,user,'vacuum',true);
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

type
 TVorRpg_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 Tstat_msg_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpg_calc_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpg_chr_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpg_debuf_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
  class procedure CLS(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpg_kick_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpg_xchg_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpg_duel_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpg_rst_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

 TVorRpgSQL_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

class procedure TVorRpg_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'sql':
   begin
    Node.Push(TVorRpgSQL_Func,nil);
   end;
  'timeout_cmd':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.timeout_cmd);
   end;
  'vor_sucs':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.vor_sucs);
   end;
  'jail_vip':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.jail_vip);
   end;
  'esc_vip':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.esc_vip);
   end;
  'str_vip':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.str_vip);
   end;
  'norm_vor':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.norm_vor);
   end;
  'minus_vip':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.minus_vip);
   end;
  'time4_vip':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.time4_vip);
   end;
  'neudc_vip':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.neudc_vip);
   end;
  'chist_vip':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.chist_vip);
   end;
  'stat_msg':
   begin
    Node.Push(Tstat_msg_Func,nil);
   end;
  'calc':
   begin
    Node.Push(TVorRpg_calc_Func,nil);
   end;
  'DEBUF_PERCENT':
   begin
    Node.Push(TLoadPerc_Func,@vor_rpg.debuf.PERC);
   end;
  'DEBUF_MIN_TIME':
   begin
    Node.Push(TLoadInt64_Func,@vor_rpg.debuf.MIN_TIME);
   end;
  'DEBUF_MAX_TIME':
   begin
    Node.Push(TLoadInt64_Func,@vor_rpg.debuf.MAX_TIME);
   end;
  'debuf':
   begin
    Node.Push(TVorRpg_debuf_Func,AllocMem(SizeOf(Tdebuf)));
   end;
  'kick':
   begin
    Node.Push(TVorRpg_kick_Func,nil);
   end;
  'xchg':
   begin
    Node.Push(TVorRpg_xchg_Func,nil);
   end;
  'duel':
   begin
    Node.Push(TVorRpg_duel_Func,nil);
   end;
  'rst':
   begin
    Node.Push(TVorRpg_rst_Func,nil);
   end;
 end;
end;

class procedure Tstat_msg_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'lvl_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.lvl_msg);
   end;
  'pts_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.pts_msg);
   end;
  'stat_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.stat_msg);
   end;
  'add_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.add_msg);
   end;
  'not_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.not_msg);
   end;
  'max_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.max_msg);
   end;
  'help_msg1':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.help_msg1);
   end;
  'help_msg2':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.help_msg2);
   end;
  'help_msg3':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.help_msg3);
   end;
  'top_msg1':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.top_msg1);
   end;
  'top_msg2':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.top_msg2);
   end;
  'rate_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.rate_msg);
   end;
  'rank_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.rank_msg);
   end;
  'on_debuf':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.on_debuf);
   end;
  'debuf_pr':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.stat_msg.debuf_pr);
   end;
 end;
end;

class procedure TVorRpg_calc_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'BASE_TIME':
  begin
   Node.Push(TLoadInt64_Func,@vor_rpg.calc.BASE_TIME);
  end;
  'MUL_TIME':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.MUL_TIME);
  end;
  'DEC_TIME':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.DEC_TIME);
  end;

  'MAX_LVL':
  begin
   Node.Push(TLoadDWORD_Func,@vor_rpg.calc.MAX_LVL);
  end;
  'MAX_LUK':
  begin
   Node.Push(TLoadDWORD_Func,@vor_rpg.calc.MAX_LUK);
  end;
  'MAX_DEF':
  begin
   Node.Push(TLoadDWORD_Func,@vor_rpg.calc.MAX_DEF);
  end;
  'MAX_CHR':
  begin
   Node.Push(TLoadDWORD_Func,@vor_rpg.calc.MAX_CHR);
  end;
  'MAX_STR':
  begin
   Node.Push(TLoadDWORD_Func,@vor_rpg.calc.MAX_STR);
  end;
  'MAX_AGL':
  begin
   Node.Push(TLoadDWORD_Func,@vor_rpg.calc.MAX_AGL);
  end;

  'MUL_EXP':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.MUL_EXP);
  end;
  'MUL_LUK':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.MUL_LUK);
  end;
  'MUL_DEF':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.MUL_DEF);
  end;
  'MUL_STR':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.MUL_STR);
  end;
  'MUL_AGL':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.MUL_AGL);
  end;
  'MUL_ESC':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.MUL_ESC);
  end;

  'DEC_LUK':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.DEC_LUK);
  end;
  'DEC_DEF':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.DEC_DEF);
  end;
  'DEC_STR':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.DEC_STR);
  end;
  'DEC_AGL':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.DEC_AGL);
  end;
  'DEC_ESC':
  begin
   Node.Push(TLoadDouble_Func,@vor_rpg.calc.DEC_ESC);
  end;

  'PERC_MINUS_VIP':
  begin
   Node.Push(TLoadPerc_Func,@vor_rpg.calc.PERC_MINUS_VIP);
  end;
 end;
end;

class procedure TVorRpg_chr_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'STR':
  begin
   Node.Push(TLoadInt64_Func,@Pcharacteristic(Node.CData)^.STR);
  end;
  'LUK':
  begin
   Node.Push(TLoadInt64_Func,@Pcharacteristic(Node.CData)^.LUK);
  end;
  'DEF':
  begin
   Node.Push(TLoadInt64_Func,@Pcharacteristic(Node.CData)^.DEF);
  end;
  'CHR':
  begin
   Node.Push(TLoadInt64_Func,@Pcharacteristic(Node.CData)^.CHR);
  end;
  'AGL':
  begin
   Node.Push(TLoadInt64_Func,@Pcharacteristic(Node.CData)^.AGL);
  end;
 end;
end;

class procedure TVorRpg_debuf_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'chr':
  begin
   Node.Push(TVorRpg_chr_Func,@Pdebuf(Node.CData)^.chr);
  end;
  'text':
  begin
   Node.Push(TLoadStr_Func,@Pdebuf(Node.CData)^.text);
  end;
 end;
end;

class procedure TVorRpg_debuf_Func.CLS(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'debuf':
  begin
   add_debuf(Pdebuf(Node.CData)^);
   FreeMem(Node.CData);
  end;
 end;
 inherited;
end;

class procedure TVorRpg_kick_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'PERC':
  begin
   Node.Push(TLoadPerc_Func,@vor_rpg.kick.PERC);
  end;
  'in_time':
  begin
   Node.Push(TLoadInt64_Func,@vor_rpg.kick.in_time);
  end;
  'out_time':
  begin
   Node.Push(TLoadInt64_Func,@vor_rpg.kick.out_time);
  end;
  'in_msg':
  begin
   Node.Push(TLoadStr_Func,@vor_rpg.kick.in_msg);
  end;
  'out_msg':
  begin
   Node.Push(TLoadStr_Func,@vor_rpg.kick.out_msg);
  end;
  'not_vor':
  begin
   Node.Push(TLoadList_Func,@vor_rpg.kick.not_vor);
  end;
  'go_kick':
  begin
   Node.Push(TLoadList_Func,@vor_rpg.kick.go_kick);
  end;
  'go_def':
  begin
   Node.Push(TLoadList_Func,@vor_rpg.kick.go_def);
  end;
  'go_esc':
  begin
   Node.Push(TLoadList_Func,@vor_rpg.kick.go_esc);
  end;
 end;
end;

class procedure TVorRpg_xchg_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'kd_time':
   begin
    Node.Push(TLoadInt64_Func,@vor_rpg.xchg.kd_time);
   end;
  'max_count':
   begin
    Node.Push(TLoadDWORD_Func,@vor_rpg.xchg.max_count);
   end;
  'max_time':
   begin
    Node.Push(TLoadDWORD_Func,@vor_rpg.xchg.max_time);
   end;
  'exist1_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.exist1_msg);
   end;
  'exist2_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.exist2_msg);
   end;
  'max_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.max_msg);
   end;
  'ready_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.ready_msg);
   end;
  'cancel_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.cancel_msg);
   end;
  'sucs_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.sucs_msg);
   end;
  'time_msg1':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.time_msg1);
   end;
  'time_msg2':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.xchg.time_msg2);
   end;
 end;
end;

class procedure TVorRpg_duel_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'max_count':
   begin
    Node.Push(TLoadDWORD_Func,@vor_rpg.duel.max_count);
   end;
  'max_time':
   begin
    Node.Push(TLoadDWORD_Func,@vor_rpg.duel.max_time);
   end;
  'kd_time':
   begin
    Node.Push(TLoadDWORD_Func,@vor_rpg.duel.kd_time);
   end;
  'PERC_MINUS_VIP':
   begin
    Node.Push(TLoadPerc_Func,@vor_rpg.duel.PERC_MINUS_VIP);
   end;
  'any_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.duel.any_msg);
   end;
  'exist1_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.duel.exist1_msg);
   end;
  'exist2_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.duel.exist2_msg);
   end;
  'max_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.duel.max_msg);
   end;
  'ready_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.duel.ready_msg);
   end;
  'cancel_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.duel.cancel_msg);
   end;
  'time_msg':
   begin
    Node.Push(TLoadStr_Func,@vor_rpg.duel.time_msg);
   end;
  'stand_msg':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.duel.stand_msg);
   end;
  'vip_msg':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.duel.vip_msg);
   end;
  'win_msg':
   begin
    Node.Push(TLoadList_Func,@vor_rpg.duel.win_msg);
   end;
 end;
end;

class procedure TVorRpg_rst_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'tax':
   begin
    Node.Push(TLoadDWORD_Func,@vor_rpg.rst.tax);
   end;
  'kd':
   begin
    Node.Push(TLoadDWORD_Func,@vor_rpg.rst.kd);
   end;
  'rst_msg':
   begin
    Node.Push(TLoadStr_Func  ,@vor_rpg.rst.rst_msg);
   end;
  'tax_msg':
   begin
    Node.Push(TLoadStr_Func  ,@vor_rpg.rst.tax_msg);
   end;
  'not_msg':
   begin
    Node.Push(TLoadStr_Func  ,@vor_rpg.rst.not_msg);
   end;
  'info_msg':
   begin
    Node.Push(TLoadStr_Func  ,@vor_rpg.rst.info_msg);
   end;
 end;
end;

class procedure TVorRpgSQL_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
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
 end;
end;

initialization
 vor_rpg.stat_msg.on_debuf:='@%s set debuf: (%s)';
 vor_rpg.stat_msg.top_msg1:='%s)%s|LVL:%s|EXP:%s';
 vor_rpg.stat_msg.top_msg2:='@%s %s';
 vor_rpg.stat_msg.rank_msg:='@%s Rank:%s';
 vor_rpg.stat_msg.rate_msg:='@%s wins:%s|loses:%s [%s%%]';
 vor_rpg.stat_msg.debuf_pr:='@%s &lt;%s&gt;';
 vor_rpg.stat_msg.lvl_msg :='@%s LVL:%s [%s/%s]';
 vor_rpg.stat_msg.pts_msg :='@%s LUK:%s |DEF:%s |CHR:%s |AGL:%s |STR:%s |PTS:%s';
 vor_rpg.stat_msg.stat_msg:='@%s LUK%%:%s |DEF%%:%s |STR%%:%s |AGL%%:%s |ESC%%:%s |-TIME:%s';
 vor_rpg.stat_msg.add_msg :='@%s skill point add to %s';
 vor_rpg.stat_msg.max_msg :='@%s max skill points in %s';
 vor_rpg.stat_msg.not_msg :='@%s no free skill points';

 vor_rpg.rst.not_msg      :='@%s not need to reset!';
 vor_rpg.rst.rst_msg      :='@%s skill points is reset!';
 vor_rpg.rst.tax_msg      :='@%s need tax to reset!';
 vor_rpg.rst.info_msg     :='@%s re-enter the text !vor rst';

 vor_rpg.kick.in_msg      :='%s kick in timeout (%s)';
 vor_rpg.kick.out_msg     :='%s kick out timeout (%s)';

 vor_rpg.xchg.max_msg     :='@%s too many requests for exchange';
 vor_rpg.xchg.time_msg1   :='@%s exchange in timeout (%s)';
 vor_rpg.xchg.time_msg2   :='@%s exchange in timeout for %s (%s)';
 vor_rpg.xchg.ready_msg   :='@%s input [!vip me] in %smin';
 vor_rpg.xchg.exist1_msg  :='%s exchange request is exist, for cancel try [!vip моя]';
 vor_rpg.xchg.exist2_msg  :='@%s request for %s is exists';
 vor_rpg.xchg.sucs_msg    :='@%s vip is exchanged TwitchVotes';
 vor_rpg.xchg.cancel_msg  :='@%s request canceled';

 vor_rpg.duel.time_msg    :='@%s duel in timeout (%s)';
 vor_rpg.duel.cancel_msg  :='@%s duel is canceled';
 vor_rpg.duel.exist1_msg  :='@%s you are challenged to a duel with %s';
 vor_rpg.duel.exist2_msg  :='@%s this man is already waiting duel';
 vor_rpg.duel.max_msg     :='@%s too many requests for duel';
 vor_rpg.duel.ready_msg   :='@%s input [!duel] in %smin to begin with %s';

 LockStr:=TRawByteStringSet.Create;
 xchgSet:=TxchgNodeSet.Create;
 duelSeta[0]:=TxchgNodeSet.Create;
 duelSeta[1]:=TxchgNodeSet.Create;
 vor_rpg.time_kd:=8;
 vor_rpg.rst.kd:=1800;
 if not RegisterXMLNode('vor_rpg',TVorRpg_Func,nil) then Assert(False);

end.

