unit uloginf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType,LCLIntf, Buttons;

type
  TfrmLogin = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    EdtLogin:TLabeledEdit;
    EdtPassword:TLabeledEdit;
    TBView: TToggleBox;
    procedure BtnCancelClick(Sender:TObject);
    procedure BtnOkClick(Sender:TObject);
    procedure EdtKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure EdtUTF8KeyPress(Sender:TObject; var UTF8Key:TUTF8Char);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure TBViewChange(Sender: TObject);
  private
    DigLastChar:Char;
  public

  end;

var
 frmLogin:TfrmLogin;

implementation

Uses Main;

{$R *.lfm}

procedure TfrmLogin.EdtKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
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

procedure TfrmLogin.EdtUTF8KeyPress(Sender:TObject;var UTF8Key:TUTF8Char);
begin
 if (DigLastChar<>#0) then UTF8Key:=DigLastChar;
end;

procedure TfrmLogin.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

procedure TfrmLogin.TBViewChange(Sender:TObject);
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

procedure TfrmLogin.BtnOkClick(Sender:TObject);
begin
 if Length(EdtLogin.Text)=0 then
 begin
  EdtLogin.SetFocus;
  Application.ProcessMessages;
  Exit;
 end;

 ModalResult:=mrOk;
 Hide;
end;

procedure TfrmLogin.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;


end.

