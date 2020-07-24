{ Extension of string grid

  Copyright (C) 2020 Red_prig

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
}

unit ExtStringGrid;

{$mode objfpc}{$H+}

interface

Uses
  Classes,SysUtils,Controls,Graphics,Dialogs,LCLIntf,
  StdCtrls,ComCtrls,Menus,
  ExtCtrls,Clipbrd,
  LCLType,kgrids,kcontrols;

type
 TExtGridColumn=class(TKGridCol)
 protected
  FName,FCaption:RawByteString;
 public
  procedure Assign(Source:TPersistent); override;
  property  Name:RawByteString read FName;
  property  Caption:RawByteString read FCaption write FCaption;
 end;

 TExtStringGrid=class(TKCustomGrid)
  private
   PopupFields:TPopupMenu;
  protected
   procedure   MouseDblClickCell(ACol,ARow:Integer); override;
   procedure   MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure   OnClickPopupFields(Sender:TObject);
   procedure   KeyDown(var Key:Word;Shift:TShiftState); override;
   procedure   PasteCellRectFromClipboardText(ACol,ARow:LongInt);
   function    GetFieldValue(Const FName:String;aRow:Integer):RawByteString; virtual;
   Procedure   SetFieldValue(Const FName:String;aRow:Integer;Const FValue:RawByteString); virtual;
   function    MeasureCell(ACol,ARow:Integer;const ARect:TRect;AState:TKGridDrawState;Priority:TKGridMeasureCellPriority):TPoint; override;
  public
   constructor Create(AOwner:TComponent); override;
   function    AddColumn(Const FName,FCaption:String):TExtGridColumn;
   function    FindColumn(Const FName:String):Integer;
   function    GetColumnName(ACol:Integer):String;
   procedure   DoCopyToClipboard;    virtual;
   procedure   DoPasteFromClipboard; virtual;
   procedure   DoCutToClipboard;     virtual;
   procedure   DoInsert;             virtual;
   procedure   DoDelete;             virtual;
   procedure   CopyCellRectToClipboardText(const R:TKGridRect);
   procedure   PasteCellRectFromClipboard(ACol,ARow:LongInt);
   procedure   AutoSizeCol(ACol:Integer;FixedCells:Boolean); override;
   property    FieldValue[Const FName:String;aRow:Integer]:RawByteString read GetFieldValue write SetFieldValue;
 end;

 TOnDbUpdateRow=function(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean of object;
 TOnDbSelectRow=procedure(FirstRow,LastRow:Integer) of object;
 TOnDbDeleteRow=function(aRow:Integer):Boolean of object;

 TDBStringGrid=class(TExtStringGrid)
  protected
   FRowInsId:Longint;
   FRowInsIsNull:Boolean;

   Procedure   ResetRowInsert; inline;

   function    DrawCell(ACol,ARow:Integer;ARect:TRect;AState:TKGridDrawState):Boolean; override;
   function    EditorCreate(ACol,ARow:Integer):TWinControl; override;
   procedure   EditorDataFromGrid(AEditor:TWinControl;ACol,ARow:Integer); override;
   procedure   EditorDataToGrid(AEditor:TWinControl;ACol,ARow:Integer); override;

   Function    CanDbSelect:Boolean; inline;

   procedure   Scroll(CodeHorz,CodeVert,DeltaHorz,DeltaVert:Integer;CallUpdateEditor:Boolean); override;
   procedure   ChangeBounds(ALeft,ATop,AWidth,AHeight:integer;KeepBase:boolean); override;
   procedure   ChangeDataSize(ColInsert:Boolean;ColAt,ColCnt:Integer;RowInsert:Boolean;RowAt,RowCnt:Integer); override;

   procedure   InternalSetSelection(NewSelection:TKGridRect;Flags:TKGridSelectionFlags); override;
  public
   FOnDbUpdate:TOnDbUpdateRow;
   FOnDbSelect:TOnDbSelectRow;
   FOnDbInsert:TOnDbUpdateRow;
   FOnDbDelete:TOnDbDeleteRow;

   constructor Create(AOwner:TComponent); override;
   procedure   DoInsert;             override;
   procedure   DoDelete;             override;
 end;

 TKVirtualGridRow=class(TKGridRow)
  procedure ValidateInitialPos;         override;
  procedure Assign(Source:TPersistent); override;
  procedure SetVisible(Value:Boolean);  override;
  function  GetVisible:Boolean;         override;
  procedure GridChanged;                override;
 end;

 TKVirtualRows=class(TKGridAxisItems)
 protected
  FRowsV:array[0..1] of TKVirtualGridRow;
  FRowCount:Integer;
  function    GetItem(Index:Integer):TKGridAxisItem;       override;
  procedure   SetItem(Index:Integer;Value:TKGridAxisItem); override;
  procedure   Update(Item:TCollectionItem);                override;
  function    _get_next_row:TKVirtualGridRow; inline;
  function    _get_row:TKVirtualGridRow;
 public
  constructor Create(_Grid:TKCustomGrid;AClass:TCollectionItemClass);
  destructor  Destroy;                                     override;
  function    Count:Integer;                               override;
  function    Add:TKGridAxisItem;                          override;
  function    AddOnly:TKGridAxisItem;                      override;
  procedure   Clear;                                       override;
  procedure   Delete(Index:Integer);                       override;
  procedure   DeleteOnly(Index:Integer);                   override;
  procedure   Exchange(Index1,Index2:Integer);             override;
  function    Insert(At:Integer):TKGridAxisItem;           override;
  function    InsertOnly(At:Integer):TKGridAxisItem;       override;
 end;

implementation

///////

procedure TKVirtualGridRow.ValidateInitialPos;
begin
 ///
end;

procedure TKVirtualGridRow.Assign(Source: TPersistent);
begin
 //
end;

procedure TKVirtualGridRow.SetVisible(Value:Boolean);
begin
 //
end;

function TKVirtualGridRow.GetVisible:Boolean;
begin
 Result:=true;
end;

procedure TKVirtualGridRow.GridChanged;
begin
 //////
end;

///////

function TKVirtualRows.Count:Integer;
begin
 Result:=FRowCount;
end;

constructor TKVirtualRows.Create(_Grid:TKCustomGrid;AClass:TCollectionItemClass);
begin
 FRowsV[0]:=TKVirtualGridRow.Create(self);
 FRowsV[1]:=TKVirtualGridRow.Create(self);
 AClass:=TKVirtualGridRow;
 FRowCount:=_Grid.RowCount;
 inherited;
end;

destructor TKVirtualRows.Destroy;
begin
 FreeAndNil(FRowsV[0]);
 FreeAndNil(FRowsV[1]);
 inherited;
end;

function TKVirtualRows._get_next_row:TKVirtualGridRow; inline;
var
 Tmp:TKVirtualGridRow;
begin
 if PtrUint(FRowsV[0])>PtrUint(FRowsV[1]) then Result:=FRowsV[0] else Result:=FRowsV[1];
 Tmp:=FRowsV[0];
 FRowsV[0]:=FRowsV[1];
 FRowsV[1]:=Tmp;
end;

function TKVirtualRows._get_row:TKVirtualGridRow;
begin
 Result:=_get_next_row;
 Result.Grid          :=nil;
 Result.InitialPos    :=0;
 Result.SortMode      :=smNone;
 Result.Tag           :=nil;
 Result.CanResize     :=true;
 Result.FBackExtent   :=cDefaultRowHeightDef;
 Result.Extent        :=cDefaultRowHeightDef;
 Result.MaxExtent     :=0;
 Result.MinExtent     :=cDefaultRowHeightDef;
 Result.SortArrowIndex:=0;
 Result.Grid          :=Grid;
end;

function   TKVirtualRows.GetItem(Index:Integer):TKGridAxisItem;
Var
 Row:TKVirtualGridRow;
begin
// Write(Index,' ');
 Row:=_get_row;
 Row.InitialPos:=Index;
 Result:=Row;
end;

procedure  TKVirtualRows.SetItem(Index:Integer;Value:TKGridAxisItem);
begin

end;

procedure  TKVirtualRows.Update(Item:TCollectionItem);
begin
 inherited;
end;


function  TKVirtualRows.Add:TKGridAxisItem;
begin
 Inc(FRowCount);
 Result:=Grid.InsertRow(-1);
end;

function  TKVirtualRows.AddOnly:TKGridAxisItem;
begin
 Inc(FRowCount);
 Result:=_get_row;
end;

procedure TKVirtualRows.Clear;
begin
 Grid.DeleteRows(0,Grid.RowCount);
 FRowCount:=Grid.RowCount;
end;

procedure TKVirtualRows.Delete(Index:Integer);
begin
 Grid.DeleteRow(Index);
 FRowCount:=Grid.RowCount;
end;

procedure TKVirtualRows.DeleteOnly(Index:Integer);
begin
 if (Index>0) and (Index<FRowCount) then Dec(FRowCount);
end;

procedure TKVirtualRows.Exchange(Index1,Index2:Integer);
begin
 //
end;

function  TKVirtualRows.Insert(At:Integer):TKGridAxisItem;
begin
 Inc(FRowCount);
 Result:=Grid.InsertRow(At)
end;

function  TKVirtualRows.InsertOnly(At:Integer):TKGridAxisItem;
begin
 Inc(FRowCount);
 Result:=_get_row;
end;

///////////

procedure TExtGridColumn.Assign(Source:TPersistent);
begin
 if (Source is TExtGridColumn) then
 begin
  Grid        :=TExtGridColumn(Source).Grid;
  Extent      :=TExtGridColumn(Source).Extent;
  InitialPos  :=TExtGridColumn(Source).InitialPos;
  SortMode    :=TExtGridColumn(Source).SortMode;
  FName       :=TExtGridColumn(Source).FName;
  FCaption    :=TExtGridColumn(Source).FCaption;
  Tag         :=TExtGridColumn(Source).Tag;
  AssignPublished(TKGridAxisItem(Source));
 end;
end;

//TExtStringGrid

procedure TExtStringGrid.MouseDblClickCell(ACol,ARow:Integer);

 function NextSortMode(ASortMode: TKGridSortMode): TKGridSortMode;
 begin
   case SortStyle of
     ssDownUp: if ASortMode = smDown then Result := smUp else Result := smDown;
     ssDownUpNone:
       case ASortMode of
         smDown: Result := smUp;
         smUp: Result := smNone;
       else
         Result := smDown;
       end;
     ssUpDown: if ASortMode = smUp then Result := smDown else Result := smUp;
   else
     case ASortMode of
       smUp: Result := smDown;
       smDown: Result := smNone;
     else
       Result := smUp;
     end;
   end;
 end;

begin
 if ((FGridState = gsColSortWaiting) or (FGridState = gsRowMoveWaiting)) and
   (goColSorting in Options) and (ACol = FRows[ARow].SortArrowIndex) then
   SortCols(ARow, NextSortMode(Rows[ARow].SortMode))
 else if ((FGridState = gsRowSortWaiting) or (FGridState = gsColMoveWaiting)) and
   (goRowSorting in Options) and (ARow = FCols[ACol].SortArrowIndex) then
   SortRows(ACol, NextSortMode(Cols[ACol].SortMode));

 inherited;
end;

procedure TExtStringGrid.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
Var
 i,ACol,ARow:Integer;
 Item:TMenuItem;
 _Cell:TKGridCell;
begin
 case FGridState of
  gsColSortWaiting,
  gsRowSortWaiting,
  gsColMoveWaiting,
  gsRowMoveWaiting:FGridState:=gsClickWaiting;
 end;
 inherited;
 if Button=mbRight then
 if MouseToCell(X,Y,ACol,ARow) then
 if (ARow<=FixedRows) and ColValid(ACol) then
 begin
  if Assigned(Columns) then
  if Columns.Count>0 then
  begin
   PopupFields.Items.Clear;
   For i:=0 to Columns.Count-1 do
   begin
    _Cell:=InternalGetCell(i,ARow);
    if _Cell.InheritsFrom(TKGridTextCell) then
    begin
     Item:=TMenuItem.Create(PopupFields);
     Item.Tag:=i;
     Item.Caption:=TKGridTextCell(_Cell).Text;
     Item.Checked:=Columns[i].Visible;
     Item.OnClick:=@OnClickPopupFields;
     PopupFields.Items.Add(Item);
    end;
   end;
   PopupFields.PopUp;
  end;
 end;
end;

procedure TExtStringGrid.OnClickPopupFields(Sender:TObject);
begin
 if Sender is TMenuItem then
 if Assigned(Columns) then
  if Columns.Count>0 then
  With TMenuItem(Sender) do
   if Columns.Count>Tag then
   begin
    Columns.Items[Tag].Visible:=not Columns.Items[Tag].Visible;
   end;
end;

constructor TExtStringGrid.Create(AOwner:TComponent);
begin
 inherited;

 FixedCols:=0;
 FixedRows:=0;
 GridLineWidth:=1;

 RowCount:=1;
 ColCount:=1;
 ColClass:=TExtGridColumn;
 RealizeColClass;

 FixedRows:=1;

 Colors.CellLines:=clSilver;
 Colors.FixedThemedCellLines:=cl3DDKShadow;
 Colors.FixedCellLines:=cl3DDKShadow;

 Options:=Options+[goAlignLastCol,goRowSorting,goColMoving,goColSizing]-[goIndicateHiddenCells,goMouseCanHideCells];
 SortStyle:=ssDownUpNone;

 //ScrollBars:=ssAutoBoth;

 PopupFields:=TPopupMenu.Create(Self);
 PopupFields.Parent:=Self;
end;

function TExtStringGrid.AddColumn(Const FName,FCaption:String):TExtGridColumn;
begin
 Result:=nil;
 if (Columns.Count=1) then
 if (Columns[0] is TExtGridColumn) then
 if TExtGridColumn(Columns[0]).FName='' then
 begin
  Result:=TExtGridColumn(Columns[0]);
 end;
 if (Result=nil) then Result:=TExtGridColumn(Columns.Add);
 Result.MinExtent:=20;
 Cells[Result.Index,0]:=FCaption;
 if (Result is TExtGridColumn) then
 begin
  TExtGridColumn(Result).FName:=FName;
  TExtGridColumn(Result).FCaption:=FCaption;
 end;
end;

function TExtStringGrid.FindColumn(Const FName:String):Integer;
var
 A:TKGridAxisItem;
 Index:Integer;
begin
 Result:=-1;
 if (FName<>'') then
 begin
  Index:=FixedCols;
  while (Index<ColCount) do
  begin
   A:=FCols[Index];
   if (A<>nil) and (A is TExtGridColumn) and (TExtGridColumn(A).FName=FName) then
   begin
    Result:=Index;
    Exit;
   end else
    Inc(Index);
  end;
 end;
end;

function TExtStringGrid.GetColumnName(ACol:Integer):String;
var
 A:TKGridAxisItem;
begin
 Result:='';
 if ColValid(ACol) then
 begin
  A:=FCols[ACol];
  if (A<>nil) and (A is TExtGridColumn) then
  begin
   Result:=TExtGridColumn(A).FName;
  end;
 end;
end;

function TExtStringGrid.GetFieldValue(Const FName:String;aRow:Integer):RawByteString;
var
 aCol:Integer;
begin
 Result:='';
 aCol:=FindColumn(FName);
 if (aCol<>-1) then
 begin
  Result:=Cells[aCol,aRow];
 end;
end;

Procedure TExtStringGrid.SetFieldValue(Const FName:String;aRow:Integer;Const FValue:RawByteString);
var
 aCol:Integer;
begin
 aCol:=FindColumn(FName);
 if (aCol<>-1) then
 begin
  Cells[aCol,aRow]:=FValue;
 end;
end;

procedure TExtStringGrid.KeyDown(var Key:Word;Shift:TShiftState);
begin
 inherited;
 if (Shift=[ssModifier]) then
 begin
  case Key of
   VK_C:DoCopyToClipboard;
   VK_V:DoPasteFromClipboard;
   VK_X:DoCutToClipboard;
   VK_A:SelectAll;
  end;
 end else
 if (Shift=[]) then
  case Key of
   VK_INSERT:DoInsert;
   VK_DELETE:DoDelete;
  end;
end;

procedure TExtStringGrid.DoCopyToClipboard;
begin
 CopyCellRectToClipboardText(Selection);
end;

procedure TExtStringGrid.DoPasteFromClipboard;
begin
end;

procedure TExtStringGrid.DoCutToClipboard;
begin
end;

procedure TExtStringGrid.DoInsert;
begin
end;

procedure TExtStringGrid.DoDelete;
begin
end;

procedure TExtStringGrid.CopyCellRectToClipboardText(const R:TKGridRect);
var
 Stream:TMemoryStream;
 aRow,aCol:LongInt;
 Rect:TKGridRect;

 procedure _add_char(Ch:AnsiChar); inline;
 begin
  Stream.Write(Ch,1);
 end;

 procedure _add_Quoted(const S:RawByteString); inline;
 Const
  Quote='"';
 var
  i,j,count:LongInt;
 begin
  _add_char(Quote);
  count:=length(s);
  i:=0;
  j:=0;
  while (i<count) do
  begin
   i:=i+1;
   if S[i]=Quote then
   begin
    Stream.Write(S[1+j],i-j);
    _add_char(Quote);
    j:=i;
   end;
  end;
  if (i<>j) then Stream.Write(S[1+j],i-j);
  _add_char(Quote);
 end;

 procedure AddText(const s:RawByteString); inline;
 begin
  if (pos(#9, s)>0) or
     (pos(#10,s)>0) or
     (pos(#13,s)>0)
  then
   _add_Quoted(s)
  else
   Stream.Write(PAnsiChar(s)^,Length(s));
 end;

 function _GetCells(ACol,ARow:LongInt):RawByteString; inline;
 var
  Data:TKGridCell;
 begin
  Result:='';
  Data:=InternalGetCell(ACol,ARow);
  if (Data<>nil) and (Data.InheritsFrom(TKGridTextCell)) then
    Result:=TKGridTextCell(Data).Text;
 end;

 function Min(f,l:Integer):Integer; inline;
 begin
  if (f>l) then Result:=l else Result:=f;
 end;

 function Max(f,l:Integer):Integer; inline;
 begin
  if (f<l) then Result:=l else Result:=f;
 end;

begin
 if (not Assigned(FCells)) and (not (goVirtualGrid in Options)) then Exit;
 Stream:=TMemoryStream.Create;
 Rect.Row1:=Min(R.Row1,R.Row2);
 Rect.Row2:=Max(R.Row1,R.Row2);
 Rect.Col1:=Min(R.Col1,R.Col2);
 Rect.Col2:=Max(R.Col1,R.Col2);
 for aRow:=Rect.Row1 to Rect.Row2 do
 begin
  if RowValid(aRow) then
  begin
   if (aRow<>Rect.Row1) then _add_char(#13);
   for aCol:=Rect.Col1 to Rect.Col2 do
    if ColValid(aCol) then
    begin
     if (aCol<>Rect.Col1) then _add_char(#9);
     AddText(_GetCells(aCol,aRow));
    end;
  end;
 end;
 _add_char(#0);
 Clipboard.AddFormat(CF_TEXT,Stream);
 FreeAndNil(Stream);
end;

procedure TExtStringGrid.PasteCellRectFromClipboard(ACol,ARow:LongInt);
begin
 if Clipboard.HasFormat(CF_TEXT) then
 begin
  PasteCellRectFromClipboardText(ACol,ARow);
 end;
end;

procedure TExtStringGrid.PasteCellRectFromClipboardText(ACol,ARow:LongInt);
var
 Stream:TMemoryStream;
 Data:RawByteString;
 DataLen:SizeUint;
 Ch:AnsiChar;
 State:Byte;

 _Col,_Row:LongInt;
 Valid,TrueEscape:Boolean;

 Procedure AddChar(Ch:AnsiChar);
 begin
  if not Valid then Exit;
  if DataLen=Length(Data) then
  begin
   if Length(Data)<8 then SetLength(Data,8) else SetLength(Data,DataLen+(DataLen shr 1));
  end;
  Inc(DataLen);
  Data[DataLen]:=Ch;
 end;

 Procedure NextCol;
 begin
  if Valid then
  begin
   InternalSetCells(_Col,_Row,Copy(Data,1,DataLen));
  end;
  Inc(_Col);
  Valid:=ColValid(_Col) and RowValid(_Row);
  DataLen:=0;
  State:=0;
  TrueEscape:=False;
 end;

 Procedure NextRow;
 begin
  if Valid then
  begin
   InternalSetCells(_Col,_Row,Copy(Data,1,DataLen));
  end;
  Inc(_Row);
  _Col:=ACol;
  Valid:=ColValid(_Col) and RowValid(_Row);
  DataLen:=0;
  State:=0;
  TrueEscape:=False;
 end;

 Procedure FixEscape;
 begin
  if Valid and (not TrueEscape) then
  begin
   AddChar('"');
   AddChar(#0);
   Move(Data[1],Data[2],DataLen-1);
   Data[1]:='"';
  end;
 end;

begin
 if not Assigned(FCells) then Exit;

 Valid:=ColValid(ACol) and RowValid(ARow);
 if not Valid then Exit;

 Stream:=TMemoryStream.Create;
 if not Clipboard.GetFormat(CF_TEXT,Stream) then
 begin
  FreeAndNil(Stream);
  Exit;
 end;
 Stream.Position:=0;

 _Col:=ACol;
 _Row:=ARow;

 if (PChar(Stream.Memory)[Stream.Size-1]=#0) then
 begin
  Stream.Size:=Stream.Size-1; //zero char
 end;

 SetLength(Data,0);
 DataLen:=0;
 State:=0;
 While (Stream.Read(Ch,1)=1) do
 begin
  Case State of
   0:Case Ch of //initial
      #9 :NextCol;
      #13:State:=4;
      #10:NextRow;
      '"':State:=2;
      else
      begin
       AddChar(Ch);
       State:=1;
      end;
     end;
   1:Case Ch of //simple string
      #9 :NextCol;
      #13:State:=4;
      #10:NextRow;
      else
       AddChar(Ch);
     end;
   2:Case Ch of //escape string
       #9,
      #13,
      #10:begin
           AddChar(Ch);
           TrueEscape:=True;
          end;
      '"':State:=3;
      else
       AddChar(Ch);
     end;
   3:Case Ch of //escape char
      #9 :
      begin
       FixEscape;
       NextCol;
      end;
      #13:
      begin
       FixEscape;
       State:=4;
      end;
      #10:
      begin
       FixEscape;
       NextRow;
      end;
      '"':begin
           AddChar(Ch);
           State:=2;
           TrueEscape:=True;
          end;
      else
      begin
       AddChar(Ch);
       State:=1;
      end;
     end;
    4:Case Ch of //wait #10
       #9 :begin
            NextRow;
            NextCol;
           end;
       #13:begin
            NextRow;
            NextRow;
           end;
       #10:NextRow;
       '"':begin
            NextCol;
            State:=2;
           end;
       else
       begin
        NextRow;
        AddChar(Ch);
        State:=1;
       end;
      end;
  end;
 end;

 Case State of
  1..4:NextCol;
 end;

 FreeAndNil(Stream);
end;

procedure TExtStringGrid.AutoSizeCol(ACol:Integer;FixedCells:Boolean);
var
  R: TRect;
  Dummy, Extent, FirstRow, LastRow, I, MaxExtent: Integer;
  Span: TKGridCellSpan;
  GridFocused: Boolean;

  function Max(a, b: Integer): Integer;inline;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

  procedure calc(I:Integer);
  begin
   Span := InternalGetCellSpan(ACol, I);
   if (Span.RowSpan > 0) and (Span.ColSpan > 0) then
   begin
     InternalGetHExtent(ACol, Span.ColSpan, R.Right, Dummy);
     InternalGetVExtent(I, Span.RowSpan, R.Bottom, Dummy);
     MaxExtent := Max(MaxExtent, MeasureCell(ACol, I, R, GetDrawState(ACol, I, GridFocused), mpColWidth).X - R.Right + Extent);
   end;
  end;

begin
  if ColValid(ACol) then
  begin
    GridFocused := HasFocus;
    R.Left := 0;
    R.Top := 0;
    MaxExtent := MinColWidth;
    Extent := InternalGetColWidths(ACol);

    if FixedCells and (FixedRows<>0) then
    begin
     for I:=0 to FixedRows-1 do calc(I);
    end;

    FirstRow:=TopRow;
    LastRow:=LastVisibleRow-1;

    if FirstRow<=LastRow then
     for I:=FirstRow to LastRow do calc(I);

    ColWidths[ACol]:=MaxExtent;
  end;
end;

type
 TMyGridCellPainter=class(TKGridCellPainter)
 end;

function TExtStringGrid.MeasureCell(ACol,ARow:Integer;const ARect:TRect;AState:TKGridDrawState;Priority:TKGridMeasureCellPriority):TPoint;
var
 _Cell:TKGridCell;
begin
  CellPainter.Col := ACol;
  CellPainter.Row := ARow;
  CellPainter.State := AState;
  CellPainter.CellPos := ARect.TopLeft;
  CellPainter.Canvas := Canvas;
  CellPainter.CellRect := ARect;
  TMyGridCellPainter(CellPainter).FPrinting := False;
  // prepare cell painter and measure cell data
  TMyGridCellPainter(CellPainter).BeginDraw;
  try
    Result.X := ARect.Right - ARect.Left;
    Result.Y := ARect.Bottom - ARect.Top;
    if Assigned(OnMeasureCell) then
      OnMeasureCell(Self, ACol, ARow, ARect, AState, Priority, Result)
    else
    begin
     _Cell:=InternalGetCell(ACol,ARow);
     if Assigned(_Cell) then _Cell.ApplyDrawProperties;
     Result:=CellPainter.DefaultMeasure(Priority);
    end;
  finally
    TMyGridCellPainter(CellPainter).EndDraw;
  end;
end;

//TDBStringGrid

constructor TDBStringGrid.Create(AOwner:TComponent);
begin
 inherited;
 FRowInsId:=-1;
 FRowInsIsNull:=True;
end;

procedure TDBStringGrid.DoInsert;
begin
 if Enabled and (not EditorMode) and Assigned(FOnDbInsert) then
 if (FRowInsId=-1) then
 begin
  FRowInsIsNull:=True;
  FRowInsId:=RowCount;
  InsertRow(RowCount-1);

  if (goRowSelect in Options) then
  begin
   SelectRow(FRowInsId);
  end else
  begin
   Selection:=GridRect(Col, FRowInsId, Col, FRowInsId);
  end;

 end;
end;

procedure TDBStringGrid.DoDelete;
begin
 if Enabled and (not EditorMode) then
 if (Row>=FixedRows) and (Row=FRowInsId) then
 begin
  if FRowInsIsNull then
  begin
   FRowInsId:=-1;
   DeleteRow(Row);
  end;
 end else
 if Assigned(FOnDbDelete) then
 begin
  if FOnDbDelete(Row) then
   DeleteRow(Row);
 end;
end;

procedure TDBStringGrid.InternalSetSelection(NewSelection:TKGridRect;Flags:TKGridSelectionFlags);
begin
 inherited;
 if Enabled and (not EditorMode) then
 begin
  if (FRowInsId<>-1) and (FRowInsId<>Row) and FRowInsIsNull then
  begin
   FRowInsId:=-1;
   DeleteRow(Row);
  end;
 end;
end;

Procedure TDBStringGrid.ResetRowInsert; inline;
begin
 FRowInsId:=-1;
end;

type
 _TKGridCell=class(TKGridCell)
 end;

function TDBStringGrid.DrawCell(ACol,ARow:Integer;ARect:TRect;AState:TKGridDrawState):Boolean;
var
 _Cell:TKGridCell;
begin
 _Cell:=InternalGetCell(ACol,ARow);
 Result:=Assigned(_Cell);
 if Result then
 begin
  _Cell.ApplyDrawProperties;
  _TKGridCell(_Cell).DrawCell(ACol,ARow,ARect,AState);
 end;
end;

function TDBStringGrid.EditorCreate(ACol,ARow:Integer):TWinControl;
var
 _Cell:TKGridCell;
begin
 Result:=inherited;
 if Assigned(Result) then
 begin
  _Cell:=InternalGetCell(ACol,ARow);
  if Assigned(_Cell) then
  begin
   if FEditedCell=nil then FEditedCell:=TKGridCellClass(_Cell.ClassType).Create(nil);
   FEditedCell.Assign(_Cell);
  end;
 end;
end;

procedure TDBStringGrid.EditorDataFromGrid(AEditor:TWinControl;ACol,ARow:Integer);
var
 _Cell:TKGridCell;
 AssignText:Boolean;
begin
 AssignText:=True;
 _Cell:=InternalGetCell(ACol,ARow);
 if Assigned(_Cell) then
  _TKGridCell(_Cell).EditorDataFromGrid(AEditor,ACol,ARow,AssignText)
 else
  DefaultEditorDataFromGrid(AEditor, ACol, ARow, AssignText);
 if AssignText and Assigned(_Cell) and (_Cell is TKGridTextCell) then
  SetControlText(AEditor,TKGridTextCell(_Cell).Text);
end;

procedure TDBStringGrid.EditorDataToGrid(AEditor:TWinControl;ACol,ARow:Integer);
var
 A:TKGridAxisItem;
 Value:RawByteString;
begin
 if (ARow>=FixedRows) then
 begin
  Value:=GetControlText(AEditor);
  if (FEditedCell=nil) or
     ((FEditedCell is TKGridTextCell) and (TKGridTextCell(FEditedCell).Text<>Value)) then
  begin
   A:=FCols[ACol];
   if (A<>nil) and (A is TExtGridColumn) then
   begin
    if (ARow=FRowInsId) and FRowInsIsNull then
    begin
     if Assigned(FOnDbInsert) then
      if FOnDbInsert(TExtGridColumn(A).Name,Value,ACol,ARow,AEditor) then
      begin
       FRowInsIsNull:=False;
       InternalSetCells(ACol,ARow,Value);
      end;
    end else
    if Assigned(FOnDbUpdate) then
     if FOnDbUpdate(TExtGridColumn(A).Name,Value,ACol,ARow,AEditor) then
     begin
      InternalSetCells(ACol,ARow,Value);
     end;
   end else
    InternalSetCells(ACol,ARow,Value);
  end;
 end;
end;

type
 TRange=object
  Min,Max:Integer;
  procedure new(_Min,_Max:Integer);
  function  Count:Integer; inline;
  function  High:Integer; inline;
 end;

procedure TRange.new(_Min,_Max:Integer);
begin
 Min:=_Min;
 Max:=_Max;
 if (Min>Max) then Min:=Max;
end;

function TRange.Count:Integer; inline;
begin
 Result:=(Max-Min);
end;

function TRange.High:Integer; inline;
begin
 Result:=(Max-1);
end;

function RangeDelta(Old,New:TRange):TRange;
begin
 Result:=Default(TRange);
 if (Old.Min=New.Min) and (Old.Max=New.Max) then Exit;

 if (New.Min>=Old.Min) and (New.Min<Old.Max) then
 begin
  if (New.Max>Old.Min) and (New.Max<=Old.Max) then Exit;
  Result.Min:=Old.Max;
  Result.Max:=New.Max;
 end else
 if (New.Max>Old.Min) and (New.Max<=Old.Max) then
 begin
  Result.Min:=New.Min;
  Result.Max:=Old.Min;
 end else
 begin
  Result:=New;
 end;
end;

Function TDBStringGrid.CanDbSelect:Boolean; inline;
begin
 Result:=Assigned(FOnDbSelect) and Enabled and Visible and HandleAllocated;
end;

procedure TDBStringGrid.Scroll(CodeHorz,CodeVert,DeltaHorz,DeltaVert:Integer;CallUpdateEditor:Boolean);
var
 Old,New:TRange;
begin
 if CanDbSelect then
 begin
  Old.new(TopRow,LastVisibleRow);
  inherited;
  New.new(TopRow,LastVisibleRow);
  New:=RangeDelta(Old,New);
  if New.Count<>0 then
  begin
   FOnDbSelect(New.Min,New.Max-1);
  end;
 end else
 begin
  inherited;
 end;
end;

procedure TDBStringGrid.ChangeBounds(ALeft,ATop,AWidth,AHeight:integer;KeepBase:boolean);
var
 Old,New:TRange;
begin
 if CanDbSelect then
 begin
  Old.new(TopRow,LastVisibleRow);
  inherited;
  New.new(TopRow,LastVisibleRow);
  New:=RangeDelta(Old,New);
  if New.Count<>0 then
  begin
   FOnDbSelect(New.Min,New.Max-1);
  end;
 end else
 begin
  inherited;
 end;
end;

procedure TDBStringGrid.ChangeDataSize(ColInsert:Boolean;ColAt,ColCnt:Integer;RowInsert:Boolean;RowAt,RowCnt:Integer);
var
 Old,New:TRange;
begin
 if CanDbSelect then
 begin
  Old.new(TopRow,LastVisibleRow);
  inherited;
  New.new(TopRow,LastVisibleRow);
  if (New.Max-1)=FRowInsId then New.Max:=(New.Max-1);
  New:=RangeDelta(Old,New);
  if New.Count<>0 then
  begin
   //Writeln('FOnDbSelect:',New.Min,' ',New.Max-1);
   FOnDbSelect(New.Min,New.Max-1);
  end;
 end else
 begin
  inherited;
 end;
end;


end.

