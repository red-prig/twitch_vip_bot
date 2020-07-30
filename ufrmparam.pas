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
    EdtPercent: TLabeledEdit;
    EdtMsg2: TLabeledEdit;
    EdtTitle: TLabeledEdit;
    EdtChatId: TLabeledEdit;
    EdtLogin: TLabeledEdit;
    EdtChat: TLabeledEdit;
    EdtPassword: TLabeledEdit;
    EdtMsg: TLabeledEdit;
    TBView: TToggleBox;
    procedure EdtKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure EdtPercentExit(Sender: TObject);
    procedure EdtPercentKeyPress(Sender: TObject; var Key: char);
    procedure EdtUTF8KeyPress(Sender:TObject;var UTF8Key:TUTF8Char);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure TBViewChange(Sender:TObject);
    procedure EdtNumKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
  private
    DigLastChar:Char;
    prev_perc:Byte;
  public
  end;

var
  FrmParam: TFrmParam;

implementation

{$R *.lfm}

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

procedure TFrmParam.EdtPercentKeyPress(Sender: TObject; var Key: char);
begin
 prev_perc:=StrToQWORDDef(EdtPercent.Text,70);
 if prev_perc>100 then prev_perc:=100;
 //if [ssAlt,ssCtrl]*Shift=[] then
 Case Key of
  #8,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

procedure TFrmParam.EdtPercentExit(Sender: TObject);
var
 S,L:Integer;
begin
 case StrToDWordDef(EdtPercent.Text,0) of
  1..100:;
  else
  begin
   S:=EdtPercent.SelStart ;
   L:=EdtPercent.SelLength;
   EdtPercent.Text:=IntToStr(prev_perc);
   EdtPercent.SelStart :=S;
   EdtPercent.SelLength:=L;
  end;
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
  8,37,39:;
  ord('0')..ord('9'):;
  else
   Key:=0;
 end;
 FormKeyDown(Sender,Key,Shift);
end;

procedure TFrmParam.BtnOkClick(Sender:TObject);
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

procedure TFrmParam.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

end.

