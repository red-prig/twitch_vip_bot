unit UFrmVipParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TFrmVipParam }

  TFrmVipParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBVipEnable: TCheckBox;
    CBVorEnable: TCheckBox;
    CBVipExpired: TCheckBox;
    EdtMaxVips: TLabeledEdit;
    EdtPercent: TLabeledEdit;
    EdtTitle: TLabeledEdit;
    EdtVipDays: TLabeledEdit;
    EdtVorPercent: TLabeledEdit;
    EdtVorTitle: TLabeledEdit;
    GBVip: TGroupBox;
    GBVor: TGroupBox;
    GBList: TGroupBox;
    procedure BtnCancelClick(Sender:TObject);
    procedure BtnOkClick(Sender:TObject);
    procedure EdtPercentExit(Sender:TObject);
    procedure EdtPercentKeyPress(Sender:TObject;var Key:char);
    procedure EdtVipDaysExit(Sender: TObject);
    procedure EdtVipDaysKeyPress(Sender:TObject;var Key:char);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
  private
    prev_perc:Byte;
    prev_dw:DWORD;
  public

  end;

var
  FrmVipParam: TFrmVipParam;

implementation

{$R *.lfm}

{ TFrmVipParam }

procedure TFrmVipParam.EdtPercentKeyPress(Sender:TObject;var Key:char);
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

procedure TFrmVipParam.EdtVipDaysExit(Sender: TObject);
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

procedure TFrmVipParam.EdtVipDaysKeyPress(Sender:TObject;var Key:char);
begin
 prev_dw:=StrToDWORDDef(TLabeledEdit(Sender).Text,1);
 Case Key of
  #8,#9,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

procedure TFrmVipParam.EdtPercentExit(Sender:TObject);
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

procedure TFrmVipParam.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmVipParam.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmVipParam.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

end.

