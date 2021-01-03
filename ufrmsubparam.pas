unit UFrmSubParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TaskManager,DbcEngine,
  Buttons;

type

  { TFrmSubParam }

  TFrmSubParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBSubEnable: TCheckBox;
    CBRevTick: TCheckBox;
    CBSubDontLog: TCheckBox;
    EdtSubInc: TLabeledEdit;
    EdtSub_max_inc: TLabeledEdit;
    EdtSub_max_dec: TLabeledEdit;
    EdtTitleSubDec: TLabeledEdit;
    EdtTitleSubInc: TLabeledEdit;
    procedure BtnCancelKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure EdtSubExit(Sender: TObject);
    procedure EdtSubKeyPress(Sender: TObject; var Key: char);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    prev_dw:DWORD;
  public
    submode_Tag:Boolean;
    procedure add_sub_mod_cmd(const cmd:RawByteString);
    procedure UpdateTextSubTime(mode:Byte);
    Procedure OnGetSubTime(Sender:TBaseTask);
    function  _Timer:Boolean;
    //function  CanSetSubMode(m:Boolean):Boolean;
    //function  CanSetRevMode(m:Boolean):Boolean;
    procedure SetSubModeText(m:Boolean);
    procedure SetTimerSubMode(m:Boolean);
    procedure OnRoomState(m:Boolean);
    procedure BtnClickSubModeOn(Sender:TObject);
    procedure BtnClickSubModeOff(Sender:TObject);
    procedure SubModeTimerUpdate(Sender:TObject);
    procedure _inc_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
    procedure _dec_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
    procedure OnBtnIncSubModeClick(Sender:TObject);
    procedure OnBtnDecSubModeClick(Sender:TObject);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
  end;

var
  FrmSubParam: TFrmSubParam;

  PanelSub:TPanel;
  LabelSubMode:TLabel;
  TextSubTime:TEdit;

implementation

Uses
  ULog,Main,StrUtils,DbcScript;

{$R *.lfm}

{ TFrmSubParam }

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

procedure TFrmSubParam.add_sub_mod_cmd(const cmd:RawByteString);
var
 MI,MD:Int64;
begin
 if (LowerCase(cmd)=sub_mod.subtime_get_cmd) and
    (GetTickCount64>=sub_mod.T.TickKd+sub_mod.subtime_kd*1000) then
 begin
  MI:= sub_mod.max_inc*60*60;
  MD:=-sub_mod.max_dec*60*60;
  if MI=0 then
  begin
   push_irc_list(sub_mod.subtime_get_info,[unixTime2String(sub_mod.T.TimeRv),
                                           unixTime2String(MD),'∞']);
  end else
  begin
   push_irc_list(sub_mod.subtime_get_info,[unixTime2String(sub_mod.T.TimeRv),
                                           unixTime2String(MD),
                                           unixTime2String(MI)]);
  end;
  sub_mod.T.TickKd:=GetTickCount64;
 end;
end;

procedure TFrmSubParam.UpdateTextSubTime(mode:Byte);
var
 S,L:Integer;

 procedure Updatedb; inline;
 begin
  SetDBParam('SubTime',IntToStr(sub_mod.T.TimeRv));
  sub_mod.T.TimeDB:=sub_mod.T.TimeRv;
 end;

begin

 Case mode of
  0:Updatedb;
  1:if (abs(sub_mod.T.TimeDB-sub_mod.T.TimeRv)>=10) then
    begin
     Updatedb;
    end;
 end;

 TextSubTime.Text:=unixTime2String(sub_mod.T.TimeRv);

 TextSubTime.SelStart :=S;
 TextSubTime.SelLength:=L;
end;

Procedure TFrmSubParam.OnGetSubTime(Sender:TBaseTask);
Var
 ResultSet:TZResultSet;

begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;

 if ResultSet=nil then Exit;

 if ResultSet.First then
 begin
  sub_mod.T.TimeRv:=ResultSet.GetInt(FirstDbcIndex);
  sub_mod.T.TimeDB:=sub_mod.T.TimeRv;
  UpdateTextSubTime(2);
 end;

end;

function TFrmSubParam._Timer:Boolean;
begin
 if (sub_mod.T.Timer=nil) then
 begin
  Result:=False;
 end else
 begin
  Result:=sub_mod.T.Timer.Enabled;
 end;
end;

procedure TFrmSubParam.SetSubModeText(m:Boolean);
begin
 Case m of
  True :begin
         LabelSubMode.Font.Color:=$FF00;
         LabelSubMode.Caption:=sub_mod._label._on;
        end;
  False:begin
         LabelSubMode.Font.Color:=0;
         LabelSubMode.Caption:=sub_mod._label.off;
        end;
 end;
end;

procedure TFrmSubParam.SetTimerSubMode(m:Boolean);
begin
 Case m of
  True :
  begin
   if (sub_mod.T.Timer=nil) then
   begin
    sub_mod.T.Timer:=TTimer.Create(Self);
    sub_mod.T.Timer.Interval:=400;
    sub_mod.T.Timer.OnTimer:=@SubModeTimerUpdate;
   end;
   SetSubModeText(sub_mod.T.TimeRv>0);
   sub_mod.T.TickRv:=GetTickCount64;
   sub_mod.T.Timer.Enabled:=m;
  end;
  False:
  begin
   if (sub_mod.T.Timer<>nil) then
   begin
    sub_mod.T.Timer.Enabled:=m;
   end;
   SetSubModeText(False);
  end;
 end;
 UpdateTextSubTime(0);
end;

procedure TFrmSubParam.OnRoomState(m:Boolean);
begin
 submode_Tag:=m;
 if not frmmain.BtnInfo.Visible then Exit;
 if m then
 begin
  if sub_mod.Enable and (sub_mod.T.TimeRv>0) then
  begin
   SetTimerSubMode(True);
  end else
  begin
   SetTimerSubMode(False);
   SetSubModeText(True);
  end;
 end else
 begin
  if sub_mod.Enable and sub_mod.Rev_tick and (sub_mod.T.TimeRv<0) then
  begin
   SetTimerSubMode(True);
  end else
  begin
   SetTimerSubMode(False);
   SetSubModeText(False);
  end;
 end;
end;

procedure TFrmSubParam.BtnClickSubModeOn(Sender:TObject);
begin
 if frmmain.BtnInfo.Visible then
 if (sub_mod.T.TimeRv>0) then
 begin
  //submode send on
  push_irc_list(sub_mod.cmd_on ,[base.login,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
 end;
 sub_mod.T.TickKd:=GetTickCount64;
end;

procedure TFrmSubParam.BtnClickSubModeOff(Sender:TObject);
begin
 if frmmain.BtnInfo.Visible then
 if submode_Tag then
 begin
  //submode send off
  push_irc_list(sub_mod.cmd_off,[base.login,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
 end;
 sub_mod.T.TickKd:=GetTickCount64;
end;

procedure TFrmSubParam.SubModeTimerUpdate(Sender:TObject);
var
 s:Int64;
begin
 s:=(GetTickCount64-sub_mod.T.TickRv) div 1000;
 sub_mod.T.TickRv:=sub_mod.T.TickRv+s*1000;
 if not sub_mod.Enable then Exit;
 if (sub_mod.T.TimeRv=0) then
 begin
  //wtf? submode without timer
 end else
 if (s<>0) then
 begin
  if (sub_mod.T.TimeRv>0) then
  begin
   sub_mod.T.TimeRv:=sub_mod.T.TimeRv-s;
   if (sub_mod.T.TimeRv<=0) then
   begin
    sub_mod.T.TimeRv:=0;
    //submode send off
    push_irc_list(sub_mod.cmd_off,[base.login,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
    sub_mod.T.TickKd:=GetTickCount64;
    Exit;
   end;
   UpdateTextSubTime(1);
  end else
  if sub_mod.Rev_tick then
  begin
   //reverse submode time
   sub_mod.T.TimeRv:=sub_mod.T.TimeRv+s;
   if (sub_mod.T.TimeRv>=0) then
   begin
    sub_mod.T.TimeRv:=0;
    //stop
    sub_mod.T.TickKd:=GetTickCount64;
    SetTimerSubMode(False);
    Exit;
   end;
   UpdateTextSubTime(1);
  end;
 end;
end;

procedure TFrmSubParam._inc_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
begin
 sub_mod.T.TimeRv:=sub_mod.T.TimeRv+sub_mod.inc_min*60;
 sub_mod.T.TimeDB:=sub_mod.T.TimeRv;
 if (sub_mod.T.TimeRv>0) then
 begin
  if (sub_mod.max_inc<>0) and (sub_mod.T.TimeRv>sub_mod.max_inc*60*60) then
  begin
   sub_mod.T.TimeRv:=sub_mod.max_inc*60*60;
  end;
  if (not submode_Tag) then
  begin
   if frmmain.BtnInfo.Visible and sub_mod.Enable then
   begin
    push_irc_list(sub_mod.cmd_on,[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
    submode_Tag:=True;
    cmd:=Format(_get_first_cmd(sub_mod.cmd_on),[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
   end;
  end else
  begin
   if frmmain.BtnInfo.Visible and sub_mod.Enable then
   begin
    push_irc_list(sub_mod.cmd_inc,[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
    cmd:=Format(_get_first_cmd(sub_mod.cmd_inc),[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
   end;
  end;
 end else
 begin
  if frmmain.BtnInfo.Visible and sub_mod.Enable then
  begin
   push_irc_list(sub_mod.cmd_inc,[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
   cmd:=Format(_get_first_cmd(sub_mod.cmd_inc),[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
  end;
 end;
 sub_mod.T.TickKd:=GetTickCount64;
 UpdateTextSubTime(0);
end;

procedure TFrmSubParam._dec_SubModeTime(var cmd:RawByteString;Const user:RawByteString);
begin
 sub_mod.T.TimeRv:=sub_mod.T.TimeRv-sub_mod.inc_min*60;
 sub_mod.T.TimeDB:=sub_mod.T.TimeRv;
 if (sub_mod.T.TimeRv<=0) then
 begin
  if (sub_mod.max_dec=0) then
  begin
   sub_mod.T.TimeRv:=0;
  end else
  if (abs(sub_mod.T.TimeRv)>sub_mod.max_dec*60*60) then
  begin
   sub_mod.T.TimeRv:=-sub_mod.max_dec*60*60;
  end;
  if submode_Tag then
  begin
   if frmmain.BtnInfo.Visible and sub_mod.Enable then
   begin
    push_irc_list(sub_mod.cmd_off,[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
    submode_Tag:=False;
    cmd:=Format(_get_first_cmd(sub_mod.cmd_off),[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
   end;
  end else
  begin
   if frmmain.BtnInfo.Visible and sub_mod.Enable then
   begin
    push_irc_list(sub_mod.cmd_dec,[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
    cmd:=Format(_get_first_cmd(sub_mod.cmd_dec),[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
   end;
  end;
  if sub_mod.Enable and sub_mod.Rev_tick and (not submode_Tag) and (sub_mod.T.TimeRv<0) then
  begin
   SetTimerSubMode(True);
  end;
 end else
 begin
  if frmmain.BtnInfo.Visible and sub_mod.Enable then
  begin
   push_irc_list(sub_mod.cmd_dec,[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
   cmd:=Format(_get_first_cmd(sub_mod.cmd_dec),[user,IntToStr(sub_mod.inc_min),unixTime2String(sub_mod.T.TimeRv)]);
  end;
 end;
 sub_mod.T.TickKd:=GetTickCount64;
 UpdateTextSubTime(0);
end;

procedure TFrmSubParam.OnBtnIncSubModeClick(Sender:TObject);
var
 cmd:RawByteString;
begin
 _inc_SubModeTime(cmd,base.login);
end;

procedure TFrmSubParam.OnBtnDecSubModeClick(Sender:TObject);
var
 cmd:RawByteString;
begin
 _dec_SubModeTime(cmd,base.login);
end;

procedure TFrmSubParam.FormCreate(Sender: TObject);
Var
 Btn,Tmp:TButton;
begin
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

 UpdateTextSubTime(2);

 Btn:=TButton.Create(PanelSub);
 Btn.OnClick:=@BtnClickSubModeOn;
 Btn.AutoSize:=True;
 Btn.Caption:='Включить';
 Btn.Left:=10;
 Btn.AnchorSide[akTop].Side:=asrBottom;
 Btn.AnchorSide[akTop].Control:=TextSubTime;
 Btn.BorderSpacing.Top:=5;
 Btn.Parent:=PanelSub;

 Tmp:=Btn;

 Btn:=TButton.Create(PanelSub);
 Btn.OnClick:=@OnBtnIncSubModeClick;
 Btn.AutoSize:=True;
 Btn.Caption:='[+] время';
 Btn.AnchorSide[akTop].Side:=asrBottom;
 Btn.AnchorSide[akTop].Control:=TextSubTime;
 Btn.AnchorSide[akLeft].Side:=asrRight;
 Btn.AnchorSide[akLeft].Control:=Tmp;
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

 Tmp:=Btn;

 Btn:=TButton.Create(PanelSub);
 Btn.OnClick:=@OnBtnDecSubModeClick;
 Btn.AutoSize:=True;
 Btn.Caption:='[-] время';
 Btn.Anchors:=[akTop,akRight];
 Btn.AnchorSide[akTop].Side:=asrBottom;
 Btn.AnchorSide[akTop].Control:=TextSubTime;
 Btn.AnchorSide[akRight].Side:=asrLeft;
 Btn.AnchorSide[akRight].Control:=Tmp;
 Btn.BorderSpacing.Top:=5;
 Btn.BorderSpacing.Right:=5;
 Btn.Parent:=PanelSub;

 GetDBParam('SubTime',@OnGetSubTime);

end;

Procedure TFrmSubParam.InitCfg;
begin
 sub_mod.Enable:=False;
 Config.WriteString('sub_mod','enable','0');
 Config.WriteString('sub_mod','rev_tick','0');
 Config.WriteString('sub_mod','inc_title',sub_mod.inc_title);
 Config.WriteString('sub_mod','dec_title',sub_mod.dec_title);
 Config.WriteString('sub_mod','inc_min'  ,IntToStr(sub_mod.inc_min));
 Config.WriteString('sub_mod','max_inc'  ,IntToStr(sub_mod.max_inc));
 Config.WriteString('sub_mod','max_dec'  ,IntToStr(sub_mod.max_dec));
end;

Procedure TFrmSubParam.LoadCfg;
begin
 sub_mod.Enable   :=Trim(Config.ReadString('sub_mod','enable','0'))='1';
 sub_mod.Rev_tick :=Trim(Config.ReadString('sub_mod','rev_tick','0'))='1';
 sub_mod.DontLog  :=Trim(Config.ReadString('sub_mod','dont_log','0'))='1';
 sub_mod.inc_title:=Trim(Config.ReadString('sub_mod','inc_title',sub_mod.inc_title));
 sub_mod.dec_title:=Trim(Config.ReadString('sub_mod','dec_title',sub_mod.dec_title));
 sub_mod.inc_min  :=StrToDWORDDef(Config.ReadString('sub_mod','inc_min',IntToStr(sub_mod.inc_min)),30);
 sub_mod.max_inc  :=StrToDWORDDef(Config.ReadString('sub_mod','max_inc',IntToStr(sub_mod.max_inc)),0);
 sub_mod.max_dec  :=StrToDWORDDef(Config.ReadString('sub_mod','max_dec',IntToStr(sub_mod.max_dec)),0);
end;

Procedure TFrmSubParam.Open;
begin
 CBSubEnable.Checked :=sub_mod.Enable;
 CBRevTick.Checked   :=sub_mod.Rev_tick;
 CBSubDontLog.Checked:=sub_mod.DontLog;
 EdtTitleSubInc.Text :=sub_mod.inc_title;
 EdtTitleSubDec.Text :=sub_mod.dec_title;
 EdtSubInc.Text      :=IntToStr(sub_mod.inc_min);
 EdtSub_max_inc.Text :=IntToStr(sub_mod.max_inc);
 EdtSub_max_dec.Text :=IntToStr(sub_mod.max_dec);

 if ShowModal=1 then
 begin
  sub_mod.Enable   :=CBSubEnable.Checked;
  sub_mod.Rev_tick :=CBRevTick.Checked;
  sub_mod.DontLog  :=CBSubDontLog.Checked;
  sub_mod.inc_title:=EdtTitleSubInc.Text;
  sub_mod.dec_title:=EdtTitleSubDec.Text;
  sub_mod.inc_min  :=StrToIntDef(EdtSubInc.Text,1);
  sub_mod.max_inc  :=StrToIntDef(EdtSub_max_inc.Text,0);
  sub_mod.max_dec  :=StrToIntDef(EdtSub_max_dec.Text,0);

  try
   case sub_mod.Enable of
    True :Config.WriteString('sub_mod','enable','1');
    False:Config.WriteString('sub_mod','enable','0');
   end;
   case sub_mod.Rev_tick of
    True :Config.WriteString('sub_mod','rev_tick','1');
    False:Config.WriteString('sub_mod','rev_tick','0');
   end;
   case sub_mod.DontLog of
    True :Config.WriteString('sub_mod','dont_log','1');
    False:Config.WriteString('sub_mod','dont_log','0');
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

  sub_mod.T.TickKd:=0;
  OnRoomState(submode_Tag);
 end;
end;

procedure TFrmSubParam.FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

procedure TFrmSubParam.BtnOkClick(Sender: TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmSubParam.BtnCancelClick(Sender: TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmSubParam.EdtSubExit(Sender: TObject);
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

procedure TFrmSubParam.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmSubParam.EdtSubKeyPress(Sender: TObject; var Key: char);
begin
 prev_dw:=StrToDWORDDef(TLabeledEdit(Sender).Text,1);
 Case Key of
  #8,#9,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

end.

