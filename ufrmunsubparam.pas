unit UFrmUnSubParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, TaskManager, DbcEngine;

type

  { TFrmUnSubParam }

  TFrmUnSubParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBEnable: TCheckBox;
    EdtSubInc: TLabeledEdit;
    EdtTitleInc: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure EdtSubExit(Sender: TObject);
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure EdtSubKeyPress(Sender: TObject; var Key: char);
  private
    prev_dw:DWORD;
  public
    Procedure OnGetUnSubMsg(Sender:TBaseTask);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
    function  subs_msg(const msg:RawByteString;uid:TGUID):Boolean;
    procedure _inc_msg(var cmd:RawByteString;Const user:RawByteString);
  end;

var
  FrmUnSubParam: TFrmUnSubParam;

  unsub_mod:record
   Enable:Boolean;
   inc_title:RawByteString;
   inc_msg:DWORD;
   MsgRv:DWORD;
  end;

implementation

Uses
  ULog,u_irc,Main,DbcScript;

{$R *.lfm}

function TFrmUnSubParam.subs_msg(const msg:RawByteString;uid:TGUID):Boolean;
begin
 Result:=unsub_mod.Enable and (unsub_mod.MsgRv<>0);
 if Result then
 begin
  push_irc_msg('/delete '+UIDToString(uid));
  Dec(unsub_mod.MsgRv);
  SetDBParam('UnSubMsg',IntToStr(unsub_mod.MsgRv));
  if (unsub_mod.MsgRv=0) then
  begin
   push_irc_msg('Ансабмод закончен на раз, два, три MrDestructoid');
  end;
 end;
end;

procedure TFrmUnSubParam._inc_msg(var cmd:RawByteString;Const user:RawByteString);
begin
 unsub_mod.MsgRv:=unsub_mod.MsgRv+unsub_mod.inc_msg;
 SetDBParam('UnSubMsg',IntToStr(unsub_mod.MsgRv));
 cmd:=Format('Чел %s добавляет к ансабмоду %s сообщений, всего: %s',[user,IntToStr(unsub_mod.inc_msg),IntToStr(unsub_mod.MsgRv)]);
 push_irc_msg(cmd);
end;

Procedure TFrmUnSubParam.InitCfg;
begin
 unsub_mod.Enable:=False;
 Config.WriteString('unsub_mod','enable','0');
 Config.WriteString('unsub_mod','inc_title',unsub_mod.inc_title);
 Config.WriteString('unsub_mod','inc_msg'  ,IntToStr(unsub_mod.inc_msg));
end;

Procedure TFrmUnSubParam.LoadCfg;
begin
 unsub_mod.Enable   :=Trim(Config.ReadString('unsub_mod','enable','0'))='1';
 unsub_mod.inc_title:=Trim(Config.ReadString('unsub_mod','inc_title',unsub_mod.inc_title));
 unsub_mod.inc_msg  :=StrToDWORDDef(Config.ReadString('unsub_mod','inc_msg',IntToStr(unsub_mod.inc_msg)),20);
end;

Procedure TFrmUnSubParam.Open;
begin
 CBEnable.Checked:=unsub_mod.Enable;
 EdtTitleInc.Text:=unsub_mod.inc_title;
 EdtSubInc.Text  :=IntToStr(unsub_mod.inc_msg);

 if ShowModal=1 then
 begin
  unsub_mod.Enable   :=CBEnable.Checked;
  unsub_mod.inc_title:=EdtTitleInc.Text;
  unsub_mod.inc_msg  :=StrToIntDef(EdtSubInc.Text,0);

  try
   case unsub_mod.Enable of
    True :Config.WriteString('unsub_mod','enable','1');
    False:Config.WriteString('unsub_mod','enable','0');
   end;
   Config.WriteString('unsub_mod','inc_title',unsub_mod.inc_title);
   Config.WriteString('unsub_mod','inc_msg',IntToStr(unsub_mod.inc_msg));
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;

 end;
end;

procedure TFrmUnSubParam.FormCreate(Sender: TObject);
begin
 GetDBParam('UnSubMsg',@OnGetUnSubMsg);
end;

Procedure TFrmUnSubParam.OnGetUnSubMsg(Sender:TBaseTask);
Var
 ResultSet:TZResultSet;
begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;
 if ResultSet=nil then Exit;
 if ResultSet.First then
 begin
  unsub_mod.MsgRv:=ResultSet.GetULong(FirstDbcIndex);
 end;
end;

procedure TFrmUnSubParam.FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

procedure TFrmUnSubParam.BtnOkClick(Sender: TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmUnSubParam.BtnCancelClick(Sender: TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmUnSubParam.EdtSubExit(Sender: TObject);
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

procedure TFrmUnSubParam.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmUnSubParam.EdtSubKeyPress(Sender: TObject; var Key: char);
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
 unsub_mod.inc_title:='Unsub Mode';
 unsub_mod.inc_msg:=20;

end.

