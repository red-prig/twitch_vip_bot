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
   end_msg:RawByteString;
   add_msg:RawByteString;
   inc_msg:DWORD;
   MsgRv:DWORD;

   trg_msg:RawByteString;
   bnd_msg:RawByteString;
   bnd_kd:DWORD;
   bnd_TickKd:Int64;
  end;

implementation

Uses
  ULog,u_irc,Main,DbcScript,
  xml_parse,data_xml;

{$R *.lfm}

function IsPunct(Ch:AnsiChar):Boolean; inline;
begin
 Case Ch of
   #0..' ',
  '!'..'/',
  ':'..'@',
  '['..'`',
  '{'..'~':Result:=True;
  else     Result:=False;
 end;
end;

function TrimPunct(const S:RawByteString):RawByteString;
var
 Ofs, Len: sizeint;
begin
 len := Length(S);
 while (Len>0) and IsPunct(S[Len]) do dec(Len);
 Ofs := 1;
 while (Ofs<=Len) and IsPunct(S[Ofs]) do Inc(Ofs);
 result := Copy(S, Ofs, 1 + Len - Ofs);
end;

Function msg_filtered(msg:RawByteString):Boolean;
var
 q:QWord;
 i:int64;
 d:Double;
begin
 Result:=False;
 msg:=TrimPunct(msg);
 if (msg='') then Exit;
 if TryStrToQWord(msg,q) then Exit;
 if TryStrToQWord('$'+msg,q) then Exit;
 if TryStrToInt64(msg,i) then Exit;
 if TryStrToFloat(msg,d) then Exit;
 Result:=True;
end;

function in_bnd_TickKd:Boolean; inline;
begin
 Result:=(GetTickCount64<unsub_mod.bnd_TickKd+unsub_mod.bnd_kd*1000);
end;

procedure up_bnd_TickKd; inline;
begin
 unsub_mod.bnd_TickKd:=GetTickCount64;
end;

function TFrmUnSubParam.subs_msg(const msg:RawByteString;uid:TGUID):Boolean;
begin
 Result:=unsub_mod.Enable and (unsub_mod.MsgRv<>0);
 if Result then
 begin
  push_irc_msg('/delete '+UIDToString(uid));
  if (unsub_mod.trg_msg<>'') and (msg=unsub_mod.trg_msg) then
  begin
   if in_bnd_TickKd then Exit;
   push_irc_msg(unsub_mod.bnd_msg);
   up_bnd_TickKd;
   Exit;
  end;
  if msg_filtered(msg) then
  begin
   Dec(unsub_mod.MsgRv);
   SetDBParam('UnSubMsg',IntToStr(unsub_mod.MsgRv));
  end;
  if (unsub_mod.MsgRv=0) then
  begin
   if (unsub_mod.end_msg='') then
   begin
    unsub_mod.end_msg:='Unsub mod is end MrDestructoid';
   end;
   push_irc_msg(unsub_mod.end_msg);
  end;
 end;
end;

procedure TFrmUnSubParam._inc_msg(var cmd:RawByteString;Const user:RawByteString);
begin
 unsub_mod.MsgRv:=unsub_mod.MsgRv+unsub_mod.inc_msg;
 SetDBParam('UnSubMsg',IntToStr(unsub_mod.MsgRv));
 if (unsub_mod.add_msg='') then
 begin
  unsub_mod.add_msg:='%s is added to Unsub mod %s msg, all: %s';
 end;
 cmd:=Format(unsub_mod.add_msg,[user,IntToStr(unsub_mod.inc_msg),IntToStr(unsub_mod.MsgRv)]);
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

type
 TUnsubMod_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

class procedure TUnsubMod_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
 Case Name of
  'inc_title':
   begin
    Node.Push(TLoadStr_Func,  @unsub_mod.inc_title);
   end;
  'end_msg':
   begin
    Node.Push(TLoadStr_Func,  @unsub_mod.end_msg);
   end;
  'add_msg':
   begin
    Node.Push(TLoadStr_Func,  @unsub_mod.add_msg);
   end;
  'inc_msg':
   begin
    Node.Push(TLoadDWORD_Func,@unsub_mod.inc_msg);
   end;
  'trg_msg':
   begin
    Node.Push(TLoadStr_Func,  @unsub_mod.trg_msg);
   end;
  'bnd_msg':
   begin
    Node.Push(TLoadStr_Func,  @unsub_mod.bnd_msg);
   end;
  'bnd_kd':
   begin
    Node.Push(TLoadDWORD_Func,@unsub_mod.bnd_kd);
   end;
 end;
end;

initialization
 if not RegisterXMLNode('unsub_mod',TUnsubMod_Func,nil) then Assert(False);

end.

