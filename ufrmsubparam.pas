unit UFrmSubParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TFrmSubParam }

  TFrmSubParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    EdtSubInc: TLabeledEdit;
    EdtSub_max_inc: TLabeledEdit;
    EdtSub_max_dec: TLabeledEdit;
    EdtTitleSubDec: TLabeledEdit;
    EdtTitleSubInc: TLabeledEdit;
    procedure EdtSubExit(Sender: TObject);
    procedure EdtSubKeyPress(Sender: TObject; var Key: char);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    prev_dw:DWORD;
  public

  end;

var
  FrmSubParam: TFrmSubParam;

implementation

{$R *.lfm}

{ TFrmSubParam }

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

procedure TFrmSubParam.EdtSubKeyPress(Sender: TObject; var Key: char);
begin
 prev_dw:=StrToDWORDDef(TLabeledEdit(Sender).Text,1);
 Case Key of
  #8,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

end.

