unit ufrmpred;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,

  mtRandom,
  kgrids,
  ExtStringGrid;

type

  { TFrmPred }

  TFrmPred = class(TForm)
    BtnNewSedd: TButton;
    BtnSave: TButton;
    Panelg: TPanel;
    procedure BtnNewSeddClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fetch_pred;
  private

  public
   CR:TMTRandomContext;
   GridPred:TExtStringGrid;
  end;

var
  FrmPred: TFrmPred;

implementation

uses
 main;

{$R *.lfm}

{ TFrmPred }

procedure TFrmPred.FormCreate(Sender: TObject);
begin
 GridPred:=TExtStringGrid.Create(Panelg);
 GridPred.Align:=alClient;
 GridPred.Parent:=Panelg;
 GridPred.RowCount:=21;
 GridPred.Options:=GridPred.Options-[goRowSorting];
 GridPred.ScrollBars:=ssVertical;
 GridPred.AddColumn('num',' № ');
 GridPred.AddColumn('res',' Результат');
 GridPred.AutoSizeCol(0,True);
end;

procedure TFrmPred.BtnNewSeddClick(Sender: TObject);
begin
 RandomInit(CR);
 fetch_pred;
end;

procedure TFrmPred.BtnSaveClick(Sender: TObject);
begin
 RCT:=CR;
 Hide;
end;

procedure TFrmPred.fetch_pred;
var
 Tmp:TMTRandomContext;
 i:Byte;
begin
 Tmp:=CR;
 For i:=1 to 20 do
 begin
  GridPred.FieldValue['num' ,i]:=' '+IntToStr(i);
  if fetch_random_no_more(Tmp) then
  begin
   GridPred.FieldValue['res' ,i]:=vip_rnd.cmd;
  end else
  begin
   GridPred.FieldValue['res' ,i]:=vip_rnd.cmd2;
  end;
 end;
end;

end.

