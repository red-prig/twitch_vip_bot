unit UFrmParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,LCLType,LCLIntf;

type

  { TFrmParam }

  TFrmParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    EdtLogin: TLabeledEdit;
    EdtChat: TLabeledEdit;
    EdtPassword: TLabeledEdit;
    TBView: TToggleBox;
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EdtKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure EdtUTF8KeyPress(Sender:TObject;var UTF8Key:TUTF8Char);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure TBViewChange(Sender:TObject);
    procedure EdtNumKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
  private
    DigLastChar:Char;
    prev_dw:DWORD;
  public
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
  end;

var
  FrmParam: TFrmParam;

implementation

Uses
  ULog,Main;

{$R *.lfm}

Procedure TFrmParam.InitCfg;
begin
 Config.WriteString('base','zurl'   ,DefZURL);
 Config.WriteString('base','login'  ,'');
 Config.WriteString('base','oAuth'  ,'');
 Config.WriteString('base','chat'   ,base.chat);
end;

Procedure TFrmParam.LoadCfg;
begin
 base.chat   :=Trim(Config.ReadString('base','chat'   ,base.chat));
 base.login  :=Trim(Config.ReadString('base','login'  ,base.login));
end;

Procedure TFrmParam.Open;
begin
 try
  EdtLogin.Text   :=base.login;
  EdtPassword.Text:=Config.ReadString('base','oAuth','');
  EdtChat.Text    :=base.chat;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if ShowModal=1 then
 begin
  base.chat   :=EdtChat.Text;
  base.login  :=EdtLogin.Text;

  try
   Config.WriteString('base','login'   ,base.login);
   Config.WriteString('base','oAuth'   ,EdtPassword.Text);
   Config.WriteString('base','chat'    ,base.chat);
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;

 EdtPassword.Text:='';
end;

procedure TFrmParam.EdtKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 DigLastChar:=#0;
 if [ssAlt,ssCtrl]*Shift=[] then
 Case Key of
  ord(' '):Key:=0;
  96..105:DigLastChar:=Char(Key-48);
  ord('A')..ord('Z'):
  begin
    if (GetKeyState(VK_CAPITAL) and 1)<>0 then
    begin
     if ssShift in Shift then
      Shift:=Shift-[ssShift]
     else
      Shift:=Shift+[ssShift];
    end;
    if not (ssShift in Shift) then
     DigLastChar:=lowerCase(Char(Key))
    else
     DigLastChar:=Char(Key);
  end;
 end;
 FormKeyDown(Sender,Key,Shift);
end;

procedure TFrmParam.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmParam.EdtUTF8KeyPress(Sender:TObject;var UTF8Key:TUTF8Char);
begin
 if (DigLastChar<>#0) then UTF8Key:=DigLastChar;
end;

procedure TFrmParam.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

procedure TFrmParam.TBViewChange(Sender:TObject);
begin
 Case TToggleBox(Sender).Checked of
  True :EdtPassword.EchoMode:=emNormal;
  False:EdtPassword.EchoMode:=emPassword;
 end;
 Case TToggleBox(Sender).Checked of
  True :TToggleBox(Sender).Caption:='*';
  False:TToggleBox(Sender).Caption:='a';
 end;
end;

procedure TFrmParam.EdtNumKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if [ssAlt,ssCtrl]*Shift=[] then
 Case Key of
  8,9,37,39:;
  ord('0')..ord('9'):;
  else
   Key:=0;
 end;
 FormKeyDown(Sender,Key,Shift);
end;

procedure TFrmParam.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmParam.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

end.

