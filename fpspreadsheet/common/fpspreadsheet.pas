{@@ ----------------------------------------------------------------------------
  Unit fpspreadsheet implements spreadsheet documents and their
  properties and methods.

  AUTHORS: Felipe Monteiro de Carvalho, Reinier Olislagers, Werner Pamler

  LICENSE: See the file COPYING.modifiedLGPL.txt, included in the Lazarus
           distribution, for details about the license.
-------------------------------------------------------------------------------}
unit fpSpreadsheet;

{$ifdef fpc}
  {$mode delphi}{$H+}
//  {$mode objpas}{$H+}
{$endif}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$include ..\fps.inc}

interface

uses
 {$ifdef UNIX}{$ifndef DARWIN}{$ifndef FPS_DONT_USE_CLOCALE}
  clocale,
 {$endif}{$endif}{$endif}
  Classes, SysUtils, fpimage, avglvltree, lconvencoding,
  fpsTypes, fpsExprParser, fpsClasses, fpsNumFormat, fpsPageLayout,
  fpsImages, fpsConditionalFormat;

type
  { Forward declarations }
  TsWorksheet = class;
  TsWorkbook = class;


  { TsWorksheet }

  TsNotifyEvent = procedure (Sender: TObject) of object;

  {@@ This event fires whenever a cell value or cell formatting changes. It is
    handled by TsWorkbookSource to update the listening visual controls. }
  TsCellEvent = procedure (Sender: TObject; ARow, ACol: Cardinal) of object;

  {@@ This event fires whenever a column width or column format changes. It is
    handled by TsWorkbookSource to update the listening visual controls. }
  TsColEvent = procedure (Sender: TObject; ACol: Cardinal) of object;

  {@@ This event fires whenever a row height or row format changes. It is
    handled by TsWorkbookSource to update the listening visual controls }
  TsRowEvent = procedure (Sender: TObject; ARow: Cardinal) of object;

  {@@ This event can be used to override the built-in comparing function which
    is called when cells are sorted. }
  TsCellCompareEvent = procedure (Sender: TObject; ACell1, ACell2: PCell;
    var AResult: Integer) of object;

  {@@ This event can be used to override the built-in comparing function which
    is called when cells are sorted. }
  TsCellFullCompareEvent = procedure (Sender: TObject; ACell1, ACell2: PCell;
     ASortKey: TsSortKey; var AResult: Integer) of object;

  {@@ Event fired when writing a file in virtual mode. The event handler has to
    pass data ("AValue") and formatting style to be copied from a template
    cell ("AStyleCell") to the writer }
  TsWorksheetWriteCellDataEvent = procedure(Sender: TsWorksheet; ARow, ACol: Cardinal;
    var AValue: variant; var AStyleCell: PCell) of object;

  {@@ The worksheet contains a list of cells and provides a variety of methods
    to read or write data to the cells, or to change their formatting. }
  TsWorksheet = class(TsBasicWorksheet)
  private
    FWorkbook: TsWorkbook;
    FCells: TsCells;
    FComments: TsComments;
    FMergedCells: TsMergedCells;
    FHyperlinks: TsHyperlinks;
    FFormulas: TsFormulas;
    FImages: TFPList;
    FRows, FCols: TIndexedAVLTree; // This lists contain only rows or cols with styles different from default
    FActiveCellRow: Cardinal;
    FActiveCellCol: Cardinal;
    FTopRow: Cardinal;
    FLeftCol: Cardinal;
    FSelection: TsCellRangeArray;
    FLeftPaneWidth: Integer;
    FTopPaneHeight: Integer;
    FFirstRowIndex: Cardinal;
    FFirstColIndex: Cardinal;
    FLastRowIndex: Cardinal;
    FLastColIndex: Cardinal;
    FDefaultColWidth: Single;
    FDefaultRowHeight: Single;
    FSortParams: TsSortParams;  // Parameters of the current sorting operation
    FBiDiMode: TsBiDiMode;
    FCryptoInfo: TsCryptoInfo;
    FPageLayout: TsPageLayout;
    FVirtualColCount: Cardinal;
    FVirtualRowCount: Cardinal;
    FZoomFactor: Double;
    FTabColor: TsColor;
    FOnChangeCell: TsCellEvent;
    FOnChangeFont: TsCellEvent;
    FOnChangeCol: TsColEvent;
    FOnChangeRow: TsRowEvent;
    FOnZoom: TsNotifyEvent;
    FOnCompareCells: TsCellCompareEvent;
    FOnFullCompareCells: TsCellFullCompareEvent;
    FOnSelectCell: TsCellEvent;
    FOnWriteCellData: TsWorksheetWriteCellDataEvent;

    { Setter/Getter }

    function  GetFormatSettings: TFormatSettings;
    function  GetIndex: Integer;
    procedure SetBiDiMode(AValue: TsBiDiMode);
    procedure SetIndex(AValue: Integer);
    procedure SetTabColor(AValue: TsColor);
    procedure SetVirtualColCount(AValue: Cardinal);
    procedure SetVirtualRowCount(AValue: Cardinal);
    procedure SetZoomFactor(AValue: Double);

  protected
    function CellUsedInFormula(ARow, ACol: Cardinal): Boolean;

    // Remove and delete cells
    procedure DeleteRowOrCol(AIndex: Integer; IsRow: Boolean);
    procedure InsertRowOrCol(AIndex: Integer; IsRow: Boolean);
    function RemoveCell(ARow, ACol: Cardinal): PCell;
    procedure RemoveAndFreeCell(ARow, ACol: Cardinal);

    // Sorting
    function DoCompareCells(AColRow1, AColRow2: Cardinal): Integer;
    procedure DoExchangeColRow(AIsColumn: Boolean; AIndex, WithIndex: Cardinal;
      AFromIndex, AToIndex: Cardinal);
    procedure ExchangeCells(ARow1, ACol1, ARow2, ACol2: Cardinal);

    // inherited setters/getters
    procedure SetName(const AName: String); override;

  public
    { Base methods }
    constructor Create;
    destructor Destroy; override;

    { Utils }
    class function CellInRange(ARow, ACol: Cardinal; ARange: TsCellRange): Boolean;
    class function CellPosToText(ARow, ACol: Cardinal): string;
//    procedure RemoveAllCells;
    procedure UpdateCaches;

    { Reading of values }
    function  ReadAsText(ARow, ACol: Cardinal): string; overload;
    function  ReadAsText(ACell: PCell): string; overload;
    function  ReadAsText(ACell: PCell; AFormatSettings: TFormatSettings): string; overload;
    function  ReadAsUTF8Text(ARow, ACol: Cardinal): string; overload; deprecated 'Use ReadAsText';
    function  ReadAsUTF8Text(ACell: PCell): string; overload; deprecated 'Use ReadAsText';
    function  ReadAsUTF8Text(ACell: PCell; AFormatSettings: TFormatSettings): string; overload; deprecated 'Use ReadAsText';
    function  ReadAsNumber(ARow, ACol: Cardinal): Double; overload;
    function  ReadAsNumber(ACell: PCell): Double; overload;
    function  ReadAsDateTime(ARow, ACol: Cardinal; out AResult: TDateTime): Boolean; overload;
    function  ReadAsDateTime(ACell: PCell; out AResult: TDateTime): Boolean; overload;
    function  ReadFormulaAsString(ACell: PCell; ALocalized: Boolean = false): String;
    function  ReadNumericValue(ACell: PCell; out AValue: Double): Boolean;

    { Reading of cell attributes }
    function GetDisplayedDecimals(ACell: PCell): Byte;
    function GetNumberFormatAttributes(ACell: PCell; out ADecimals: Byte;
      out ACurrencySymbol: String): Boolean;

    function  GetEffectiveCellFormatIndex(ARow, ACol: Cardinal): Integer; overload;
    function  GetEffectiveCellFormatIndex(ACell: PCell): Integer; overload;
    function  GetPointerToEffectiveCellFormat(ARow, ACol: Cardinal): PsCellFormat; //overload;
//    function  GetPointerToEffectiveCellFormat(ACell: PCell): PsCellFormat; overload;

    function  ReadUsedFormatting(ACell: PCell): TsUsedFormattingFields;
    function  ReadBackground(ACell: PCell): TsFillPattern;
    function  ReadBackgroundColor(ACell: PCell): TsColor; overload;
    function  ReadBackgroundColor(AFormatIndex: Integer): TsColor; overload;
    function  ReadCellBorders(ACell: PCell): TsCellBorders;
    function  ReadCellBorderStyle(ACell: PCell; ABorder: TsCellBorder): TsCellBorderStyle;
    function  ReadCellBorderStyles(ACell: PCell): TsCellBorderStyles;
    function  ReadCellFont(ACell: PCell): TsFont;
    function  ReadCellFontIndex(ACell: PCell): Integer;
    function  ReadCellFormat(ACell: PCell): TsCellFormat;
    function  ReadHorAlignment(ACell: PCell): TsHorAlignment;
    procedure ReadNumFormat(ACell: PCell; out ANumFormat: TsNumberFormat;
      out ANumFormatStr: String);
    function  ReadTextRotation(ACell: PCell): TsTextRotation;
    function  ReadVertAlignment(ACell: PCell): TsVertAlignment;
    function  ReadWordwrap(ACell: PCell): boolean;
    function  ReadBiDiMode(ACell: PCell): TsBiDiMode;
    function  ReadCellProtection(ACell: PCell): TsCellProtections;

    function IsEmpty: Boolean;

    { Writing of values }
    function WriteBlank(ARow, ACol: Cardinal; KeepFormula: Boolean = false): PCell; overload;
    procedure WriteBlank(ACell: PCell; KeepFormula: Boolean = false); overload;

    function WriteBoolValue(ARow, ACol: Cardinal; AValue: Boolean): PCell; overload;
    procedure WriteBoolValue(ACell: PCell; AValue: Boolean); overload;

    function WriteCellValueAsString(ARow, ACol: Cardinal; AValue: String): PCell; overload;
    function WriteCellValueAsString(ARow, ACol: Cardinal; AValue: String;
      const AFormatSettings: TFormatSettings): PCell; overload;
    procedure WriteCellValueAsString(ACell: PCell; AValue: String); overload;
    procedure WriteCellValueAsString(ACell: PCell; AValue: String;
      const AFormatSettings: TFormatSettings); overload;

    function WriteCurrency(ARow, ACol: Cardinal; AValue: Double;
      ANumFormat: TsNumberFormat = nfCurrency; ADecimals: Integer = 2;
      ACurrencySymbol: String = '?'; APosCurrFormat: Integer = -1;
      ANegCurrFormat: Integer = -1): PCell; overload;
    procedure WriteCurrency(ACell: PCell; AValue: Double;
      ANumFormat: TsNumberFormat = nfCurrency; ADecimals: Integer = -1;
      ACurrencySymbol: String = '?'; APosCurrFormat: Integer = -1;
      ANegCurrFormat: Integer = -1); overload;
    function WriteCurrency(ARow, ACol: Cardinal; AValue: Double;
      ANumFormat: TsNumberFormat; ANumFormatString: String): PCell; overload;
    procedure WriteCurrency(ACell: PCell; AValue: Double;
      ANumFormat: TsNumberFormat; ANumFormatString: String); overload;

    function WriteDateTime(ARow, ACol: Cardinal; AValue: TDateTime): PCell; overload;
    procedure WriteDateTime(ACell: PCell; AValue: TDateTime); overload;
    function WriteDateTime(ARow, ACol: Cardinal; AValue: TDateTime;
      ANumFormat: TsNumberFormat; ANumFormatStr: String = ''): PCell; overload;
    procedure WriteDateTime(ACell: PCell; AValue: TDateTime;
      ANumFormat: TsNumberFormat; ANumFormatStr: String = ''); overload;
    function WriteDateTime(ARow, ACol: Cardinal; AValue: TDateTime;
      ANumFormatStr: String): PCell; overload;
    procedure WriteDateTime(ACell: PCell; AValue: TDateTime;
      ANumFormatStr: String); overload;

    function WriteErrorValue(ARow, ACol: Cardinal; AValue: TsErrorValue): PCell; overload;
    procedure WriteErrorValue(ACell: PCell; AValue: TsErrorValue); overload;

    function WriteFormula(ARow, ACol: Cardinal; AFormula: String;
      ALocalized: Boolean = false; R1C1Mode: Boolean = false): PCell; overload;
    procedure WriteFormula(ACell: PCell; AFormula: String;
      ALocalized: Boolean = false; R1C1Mode: Boolean = false); overload;

    function WriteNumber(ARow, ACol: Cardinal; ANumber: double): PCell; overload;
    procedure WriteNumber(ACell: PCell; ANumber: Double); overload;
    function WriteNumber(ARow, ACol: Cardinal; ANumber: double;
      ANumFormat: TsNumberFormat; ADecimals: Byte = 2;
      AMinIntDigits: Integer = 1): PCell; overload;
    procedure WriteNumber(ACell: PCell; ANumber: Double;
      ANumFormat: TsNumberFormat; ADecimals: Byte = 2;
      AMinIntDigits: Integer = 1); overload;
    function WriteNumber(ARow, ACol: Cardinal; ANumber: double;
      ANumFormat: TsNumberFormat; ANumFormatString: String): PCell; overload;
    procedure WriteNumber(ACell: PCell; ANumber: Double;
      ANumFormat: TsNumberFormat; ANumFormatString: String); overload;

    function WriteRPNFormula(ARow, ACol: Cardinal;
      AFormula: TsRPNFormula): PCell; overload;
    procedure WriteRPNFormula(ACell: PCell;
      ARPNFormula: TsRPNFormula); overload;

    function WriteText(ARow, ACol: Cardinal; AText: String;
      ARichTextParams: TsRichTextParams = nil): PCell; overload;
    procedure WriteText(ACell: PCell; AText: String;
      ARichTextparams: TsRichTextParams = nil); overload;
    function WriteTextAsHTML(ARow, ACol: Cardinal; AText: String): PCell; overload;
    procedure WriteTextAsHTML(ACell: PCell; AText: String); overload;

    function WriteUTF8Text(ARow, ACol: Cardinal; AText: String;
      ARichTextParams: TsRichTextParams = nil): PCell; overload; deprecated 'Use WriteText';
    procedure WriteUTF8Text(ACell: PCell; AText: String;
      ARichTextparams: TsRichTextParams = nil); overload; deprecated 'Use WriteText';

    procedure DeleteRichTextParams(ACell: PCell);

    { Writing of cell attributes }
    function ChangeBackground(AFormatIndex: Integer; AStyle: TsFillStyle;
      APatternColor: TsColor = scTransparent;
      ABackgroundColor: TsColor = scTransparent) : Integer;
    function WriteBackground(ARow, ACol: Cardinal; AStyle: TsFillStyle;
      APatternColor: TsColor = scTransparent;
      ABackgroundColor: TsColor = scTransparent): PCell; overload;
    procedure WriteBackground(ACell: PCell; AStyle: TsFillStyle;
      APatternColor: TsColor = scTransparent;
      ABackgroundColor: TsColor = scTransparent); overload;
    function WriteBackgroundColor(ARow, ACol: Cardinal; AColor: TsColor): PCell; overload;
    procedure WriteBackgroundColor(ACell: PCell; AColor: TsColor); overload;

    function WriteBorderColor(ARow, ACol: Cardinal; ABorder: TsCellBorder;
      AColor: TsColor): PCell; overload;
    procedure WriteBorderColor(ACell: PCell; ABorder: TsCellBorder;
      AColor: TsColor); overload;
    function WriteBorderLineStyle(ARow, ACol: Cardinal; ABorder: TsCellBorder;
      ALineStyle: TsLineStyle): PCell; overload;
    procedure WriteBorderLineStyle(ACell: PCell; ABorder: TsCellBorder;
      ALineStyle: TsLineStyle); overload;
    function WriteBorders(ARow, ACol: Cardinal;
      ABorders: TsCellBorders): PCell; overload;
    procedure WriteBorders(ACell: PCell; ABorders: TsCellBorders); overload;
    {
    procedure WriteBorders(ALeft, ATop, ARight, ABottom: Integer;
      ABorders: TsCellBorders; ALeftStyle, ATopStyle, ARightStyle, ABottomStyle,
      AInnerHorStyle, AInnerVertStyle: TsCellBorderStyle);
      }
    function WriteBorderStyle(ARow, ACol: Cardinal; ABorder: TsCellBorder;
      AStyle: TsCellBorderStyle): PCell; overload;
    procedure WriteBorderStyle(ACell: PCell; ABorder: TsCellBorder;
      AStyle: TsCellBorderStyle); overload;
    function WriteBorderStyle(ARow, ACol: Cardinal; ABorder: TsCellBorder;
      ALineStyle: TsLineStyle; AColor: TsColor): PCell; overload;
    procedure WriteBorderStyle(ACell: PCell; ABorder: TsCellBorder;
      ALineStyle: TsLineStyle; AColor: TsColor); overload;
    function WriteBorderStyles(ARow, ACol: Cardinal;
      const AStyles: TsCellBorderStyles): PCell; overload;
    procedure WriteBorderStyles(ACell: PCell;
      const AStyles: TsCellBorderStyles); overload;

    procedure WriteCellFormat(ACell: PCell; const ACellFormat: TsCellFormat);
    procedure WriteCellFormatIndex(ACell: PCell; AIndex: Integer);

    function WriteDateTimeFormat(ARow, ACol: Cardinal; ANumFormat: TsNumberFormat;
      const ANumFormatString: String = ''): PCell; overload;
    procedure WriteDateTimeFormat(ACell: PCell; ANumFormat: TsNumberFormat;
      const ANumFormatString: String = ''); overload;

    function WriteDecimals(ARow, ACol: Cardinal; ADecimals: byte): PCell; overload;
    procedure WriteDecimals(ACell: PCell; ADecimals: Byte); overload;

    function  WriteFont(ARow, ACol: Cardinal; const AFontName: String;
      AFontSize: Single; AFontStyle: TsFontStyles; AFontColor: TsColor;
      APosition: TsFontPosition = fpNormal): Integer; overload;
    function  WriteFont(ACell: PCell; const AFontName: String;
      AFontSize: Single; AFontStyle: TsFontStyles; AFontColor: TsColor;
      APosition: TsFontPosition = fpNormal): Integer; overload;
    function WriteFont(ARow, ACol: Cardinal; AFontIndex: Integer): PCell; overload;
    procedure WriteFont(ACell: PCell; AFontIndex: Integer); overload;
    function WriteFontColor(ARow, ACol: Cardinal; AFontColor: TsColor): Integer; overload;
    function WriteFontColor(ACell: PCell; AFontColor: TsColor): Integer; overload;
    function WriteFontName(ARow, ACol: Cardinal; AFontName: String): Integer; overload;
    function WriteFontName(ACell: PCell; AFontName: String): Integer; overload;
    function WriteFontSize(ARow, ACol: Cardinal; ASize: Single): Integer; overload;
    function WriteFontSize(ACell: PCell; ASize: Single): Integer; overload;
    function WriteFontStyle(ARow, ACol: Cardinal; AStyle: TsFontStyles): Integer; overload;
    function WriteFontStyle(ACell: PCell; AStyle: TsFontStyles): Integer; overload;

    function WriteHorAlignment(ARow, ACol: Cardinal; AValue: TsHorAlignment): PCell; overload;
    procedure WriteHorAlignment(ACell: PCell; AValue: TsHorAlignment); overload;

    function WriteNumberFormat(ARow, ACol: Cardinal; ANumFormat: TsNumberFormat;
      const ANumFormatString: String = ''): PCell; overload;
    procedure WriteNumberFormat(ACell: PCell; ANumFormat: TsNumberFormat;
      const ANumFormatString: String = ''); overload;
    function WriteNumberFormat(ARow, ACol: Cardinal; ANumFormat: TsNumberFormat;
      ADecimals: Integer; ACurrencySymbol: String = ''; APosCurrFormat: Integer = -1;
      ANegCurrFormat: Integer = -1): PCell; overload;
    procedure WriteNumberFormat(ACell: PCell; ANumFormat: TsNumberFormat;
      ADecimals: Integer; ACurrencySymbol: String = '';
      APosCurrFormat: Integer = -1; ANegCurrFormat: Integer = -1); overload;
    function WriteFractionFormat(ARow, ACol: Cardinal; AMixedFraction: Boolean;
      ANumeratorDigits, ADenominatorDigits: Integer): PCell; overload;
    procedure WriteFractionFormat(ACell: PCell; AMixedFraction: Boolean;
      ANumeratorDigits, ADenominatorDigits: Integer); overload;

    function WriteTextRotation(ARow, ACol: Cardinal; ARotation: TsTextRotation): PCell; overload;
    procedure WriteTextRotation(ACell: PCell; ARotation: TsTextRotation); overload;

    function WriteUsedFormatting(ARow, ACol: Cardinal;
      AUsedFormatting: TsUsedFormattingFields): PCell; overload;
    procedure WriteUsedFormatting(ACell: PCell;
      AUsedFormatting: TsUsedFormattingFields); overload;

    function WriteVertAlignment(ARow, ACol: Cardinal; AValue: TsVertAlignment): PCell; overload;
    procedure WriteVertAlignment(ACell: PCell; AValue: TsVertAlignment); overload;

    function WriteWordwrap(ARow, ACol: Cardinal; AValue: boolean): PCell; overload;
    procedure WriteWordwrap(ACell: PCell; AValue: boolean); overload;

    function WriteBiDiMode(ARow, ACol: Cardinal; AValue: TsBiDiMode): PCell; overload;
    procedure WriteBiDiMode(ACell: PCell; AValue: TsBiDiMode); overload;

    function WriteCellProtection(ARow, ACol: Cardinal;
      AValue: TsCellProtections): PCell; overload;
    procedure WriteCellProtection(ACell: PCell;
      AValue: TsCellProtections); overload;

    { Conditional formatting }
    // cell-related comparisons
    function WriteConditionalCellFormat(ARange: TsCellRange; ACondition: TsCFCondition;
      ACellFormatIndex: Integer): Integer; overload;
    function WriteConditionalCellFormat(ARange: TsCellRange; ACondition: TsCFCondition;
      AParam: Variant; ACellFormatIndex: Integer): Integer; overload;
    function WriteConditionalCellFormat(ARange: TsCellRange; ACondition: TsCFCondition;
      AParam1, AParam2: Variant; ACellFormatIndex: Integer): Integer; overload;
    // color range
    function WriteColorRange(ARange: TsCellRange;
      AStartColor, AEndColor: TsColor): Integer; overload;
    function WriteColorRange(ARange: TsCellRange;
      AStartColor, ACenterColor, AEndColor: TsColor): Integer; overload;
    function WriteColorRange(ARange: TsCellRange;
      AStartColor: TsColor; AStartKind: TsCFValueKind; AStartValue: Double;
      AEndColor: TsColor; AEndKind: TsCFValueKind; AEndValue: Double): Integer; overload;
    function WriteColorRange(ARange: TsCellRange;
      AStartColor: TsColor; AStartKind: TsCFValueKind; AStartValue: Double;
      ACenterColor: TsColor; ACenterKind: TsCFValueKind; ACenterValue: Double;
      AEndColor: TsColor; AEndKind: TsCFValueKind; AEndValue: Double): Integer; overload;
    // data bars
    function WriteDataBars(ARange: TsCellRange; ABarColor: TsColor): Integer; overload;
    function WriteDataBars(ARange: TsCellRange; ABarColor: TsColor;
      AStartKind: TsCFValueKind; AStartValue: Double;
      AEndKind: TsCFValueKind; AEndValue: Double): Integer; overload;
    // icon sets
    function WriteIconSet(ARange: TsCellRange; AIconSet: TsCFIconSet;
      AHideText: Boolean = false; AReverse: Boolean = false): Integer; overload;
    function WriteIconSet(ARange: TsCellRange; AIconSet: TsCFIconSet;   // 3 icons
      AValueKind1: TsCFValueKind; AValue1: Double;
      AValueKind2: TsCFValueKind; AValue2: Double;
      AHideText: Boolean = false; AReverse: Boolean = false): Integer; overload;
    function WriteIconSet(ARange: TsCellRange; AIconSet: TsCFIconSet;  // 4 icons
      AValueKind1: TsCFValueKind; AValue1: Double;
      AValueKind2: TsCFValueKind; AValue2: Double;
      AValueKind3: TsCFValueKind; AValue3: Double;
      AHideText: Boolean = false; AReverse: Boolean = false): Integer; overload;
    function WriteIconSet(ARange: TsCellRange; AIconSet: TsCFIconSet;  // 5 icons
      AValueKind1: TsCFValueKind; AValue1: Double;
      AValueKind2: TsCFValueKind; AValue2: Double;
      AValueKind3: TsCFValueKind; AValue3: Double;
      AValueKind4: TsCFValueKind; AValue4: Double;
      AHideText: Boolean = false; AReverse: Boolean = false): Integer; overload;

    { Formulas }
    function BuildRPNFormula(ACell: PCell; ADestCell: PCell = nil): TsRPNFormula;
    procedure CalcFormula(AFormula: PsFormula);
    procedure CalcFormulas;
    procedure CalcSheet;
    function ConvertFormulaDialect(ACell: PCell; ADialect: TsFormulaDialect): String;
    function ConvertRPNFormulaToStringFormula(const AFormula: TsRPNFormula): String;
    function GetFormula(ACell: PCell): PsFormula;

    { Data manipulation methods - For Cells }
    procedure CopyCell(AFromCell, AToCell: PCell); overload;
    function CopyCell(AFromRow, AFromCol, AToRow, AToCol: Cardinal;
      AFromWorksheet: TsWorksheet = nil): PCell; overload;
    procedure CopyFormat(AFromCell, AToCell: PCell); overload;
    procedure CopyFormat(AFormatCell: PCell; AToRow, AToCol: Cardinal); overload;
    procedure CopyFormula(AFromCell, AToCell: PCell); overload;
    procedure CopyFormula(AFormulaCell: PCell; AToRow, AToCol: Cardinal); overload;
    procedure CopyValue(AFromCell, AToCell: PCell); overload;
    procedure CopyValue(AValueCell: PCell; AToRow, AToCol: Cardinal); overload;

    procedure CopyCol(AFromCol, AToCol: Cardinal; AFromWorksheet: TsWorksheet = nil);
    procedure CopyRow(AFromRow, AToRow: Cardinal; AFromWorksheet: TsWorksheet = nil);

    procedure Clear;
    procedure DeleteCell(ACell: PCell);
    procedure EraseCell(ACell: PCell; AKeepFormat: Boolean = false);

    function  AddCell(ARow, ACol: Cardinal): PCell;
    function  FindCell(ARow, ACol: Cardinal): PCell; overload;
    function  FindCell(AddressStr: String): PCell; overload;
    function  GetCell(ARow, ACol: Cardinal): PCell; overload;
    function  GetCell(AddressStr: String): PCell; overload;
    function  GetCellCount: Cardinal;

    function  FindNextCellInCol(ARow, ACol: Cardinal): PCell;
    function  FindNextCellInRow(ARow, ACol: Cardinal): PCell;
    function  FindPrevCellInCol(ARow, ACol: Cardinal): PCell;
    function  FindPrevCellInRow(ARow, ACol: Cardinal): PCell;

    function  GetFirstColIndex(AForceCalculation: Boolean = false): Cardinal;
    function  GetLastColIndex(AForceCalculation: Boolean = false): Cardinal;
    function  GetLastColNumber: Cardinal; deprecated 'Use GetLastColIndex';
    function  GetLastOccupiedColIndex: Cardinal;
    function  GetFirstRowIndex(AForceCalculation: Boolean = false): Cardinal;
    function  GetLastOccupiedRowIndex: Cardinal;
    function  GetLastRowIndex(AForceCalculation: Boolean = false): Cardinal;
    function  GetLastRowNumber: Cardinal; deprecated 'Use GetLastRowIndex';

    { Data manipulation methods - For Rows and Cols }
    function  AddCol(ACol: Cardinal): PCol;
    function  AddRow(ARow: Cardinal): PRow;
    function  CalcAutoRowHeight(ARow: Cardinal): Single;
    function  CalcRowHeight(ARow: Cardinal): Single;
    function  FindFirstCol: PCol;
    function  FindFirstRow: PRow;
    function  FindRow(ARow: Cardinal): PRow;
    function  FindCol(ACol: Cardinal): PCol;
    function  GetCellCountInRow(ARow: Cardinal): Cardinal;
    function  GetCellCountInCol(ACol: Cardinal): Cardinal;
    function  GetRow(ARow: Cardinal): PRow;
    function  GetRowFormatIndex(ARow: Cardinal): Integer;
    function  GetRowHeight(ARow: Cardinal; AUnits: TsSizeUnits): Single; overload;
    function  GetRowHeight(ARow: Cardinal): Single; overload; deprecated 'Use version with parameter AUnits.';
    function  GetRowHeightType(ARow: Cardinal): TsRowHeightType;
    function  GetCol(ACol: Cardinal): PCol;
    function  GetColFormatIndex(ACol: Cardinal): Integer;
    function  GetColWidth(ACol: Cardinal; AUnits: TsSizeUnits): Single; overload;
    function  GetColWidth(ACol: Cardinal): Single; overload; deprecated 'Use version with parameter AUnits.';
    function  GetColWidthType(ACol: Cardinal): TsColWidthType;
    function  HasColFormats: Boolean;
    function  HasRowFormats: Boolean;
    function  IsDefaultCol(ACol: PCol): Boolean;
    function  IsDefaultRow(ARow: PRow): Boolean;
    function  ColHidden(ACol: Cardinal): Boolean;
    function  RowHidden(ARow: Cardinal): Boolean;
    procedure HideCol(ACol: Cardinal);
    procedure HideRow(ARow: Cardinal);
    procedure ShowCol(ACol: Cardinal);
    procedure ShowRow(ARow: Cardinal);
    function  IsEmptyRow(ARow: Cardinal): Boolean;
    procedure DeleteCol(ACol: Cardinal);
    procedure DeleteRow(ARow: Cardinal);
    procedure InsertCol(ACol: Cardinal);
    procedure InsertRow(ARow: Cardinal);
    procedure MoveCol(AFromCol, AToCol: Cardinal);
    procedure MoveRow(AFromRow, AToRow: Cardinal);
    function  ReadDefaultColWidth(AUnits: TsSizeUnits): Single;
    function  ReadDefaultRowHeight(AUnits: TsSizeUnits): Single;
    function  ReadColFont(ACol: PCol): TsFont;
    function  ReadRowFont(ARow: PRow): TsFont;
    procedure RemoveAllRows;
    procedure RemoveAllCols;
    procedure RemoveCol(ACol: Cardinal);
    procedure RemoveRow(ARow: Cardinal);
    procedure WriteDefaultColWidth(AValue: Single; AUnits: TsSizeUnits);
    procedure WriteDefaultRowHeight(AValue: Single; AUnits: TsSizeUnits);
    procedure WriteRowInfo(ARow: Cardinal; AData: TRow);
    procedure WriteRowFormatIndex(ARow: Cardinal; AFormatIndex: Integer);
    procedure WriteRowHeight(ARow: Cardinal; AHeight: Single; AUnits: TsSizeUnits;
      ARowHeightType: TsRowHeightType = rhtCustom); overload;
    procedure WriteRowHeight(ARow: Cardinal; AHeight: Single;
      ARowHeightType: TsRowHeightType = rhtCustom); overload; deprecated 'Use version with parameter AUnits';
    procedure WriteColInfo(ACol: Cardinal; AData: TCol);
    procedure WriteColFormatIndex(ACol: Cardinal; AFormatIndex: Integer);
    procedure WriteColWidth(ACol: Cardinal; AWidth: Single; AUnits: TsSizeUnits;
      AColWidthType: TsColWidthType = cwtCustom); overload;
    procedure WriteColWidth(ACol: Cardinal; AWidth: Single;
      AColWidthType: TsColWidthType = cwtCustom); overload; deprecated 'Use version with parameter AUnits';

    procedure AddPageBreakToCol(ACol: Cardinal);
    procedure AddPageBreakToRow(ARow: Cardinal);
    function IsPageBreakCol(ACol: Cardinal): Boolean;
    function IsPageBreakRow(ARow: Cardinal): Boolean;
    procedure RemovePageBreakFromCol(ACol: Cardinal);
    procedure RemovePageBreakFromRow(ARow: Cardinal);

    // Sorting
    function DefaultCompareCells(ACell1, ACell2: PCell; ASortKey: TsSortKey): Integer;
    procedure Sort(const ASortParams: TsSortParams;
      ARowFrom, AColFrom, ARowTo, AColTo: Cardinal); overload;
    procedure Sort(ASortParams: TsSortParams; ARange: String); overload;

    // Selected cell and ranges
    procedure SelectCell(ARow, ACol: Cardinal);
    procedure ClearSelection;
    procedure DeleteSelection;
    procedure EraseSelection(AKeepFormat: Boolean = false);
    function GetSelection: TsCellRangeArray;
    function GetSelectionAsString: String;
    function GetSelectionCount: Integer;
    function GetSelectionRangeIndexOfActiveCell: Integer;
    procedure SetSelection(const ASelection: TsCellRangeArray);

    procedure ScrollTo(ANewTopRow, ANewLeftCol: Cardinal);

    // Comments
    function FindComment(ACell: PCell): PsComment;
    function HasComment(ACell: PCell): Boolean;
    function ReadComment(ARow, ACol: Cardinal): String; overload;
    function ReadComment(ACell: PCell): string; overload;
    procedure RemoveComment(ACell: PCell);
    function WriteComment(ARow, ACol: Cardinal; AText: String): PCell; overload;
    procedure WriteComment(ACell: PCell; AText: String); overload;

    // Hyperlinks
    function FindHyperlink(ACell: PCell): PsHyperlink;
    function ReadHyperlink(ACell: PCell): TsHyperlink;
    procedure RemoveHyperlink(ACell: PCell);
    function ValidHyperlink(AValue: String; out AErrMsg: String): Boolean;
    function WriteHyperlink(ARow, ACol: Cardinal; ATarget: String;
      ATooltip: String = ''): PCell; overload;
    procedure WriteHyperlink(ACell: PCell; ATarget: String;
      ATooltip: String = ''); overload;

    { Merged cells }
    function FindMergeBase(ACell: PCell): PCell;
    function FindMergedRange(ACell: PCell; out ARow1, ACol1, ARow2, ACol2: Cardinal): Boolean;
    procedure MergeCells(ARow1, ACol1, ARow2, ACol2: Cardinal); overload;
    procedure MergeCells(ARange: String); overload;
    function InSameMergedRange(ACell1, ACell2: PCell): Boolean;
    function IsMergeBase(ACell: PCell): Boolean;
    function IsMerged(ACell: PCell): Boolean;
    procedure UnmergeCells(ARow, ACol: Cardinal); overload;
    procedure UnmergeCells(ARange: String); overload;

    { Formulas }
    procedure DeleteFormula(ACell: PCell);
    function ReadFormula(ARow, ACol: Cardinal): String; overload;
    function ReadFormula(ACell: PCell): String; overload;
    procedure UseFormulaInCell(ACell: PCell; AFormula: PsFormula);

    { Embedded images }
    procedure CalcImageCell(AIndex: Integer; x, y, AWidth, AHeight: Double;
      out ARow, ACol: Cardinal; out ARowOffs, AColOffs, AScaleX, AScaleY: Double);
    procedure CalcImageExtent(AIndex: Integer; UsePixels: Boolean;
      out ARow1, ACol1, ARow2, ACol2: Cardinal;
      out ARowOffs1, AColOffs1, ARowOffs2, AColOffs2: Double;
      out x, y, AWidth, AHeight: Double);
    function GetImage(AIndex: Integer): TsImage;
    function GetImageCount: Integer;
    function GetPointerToImage(AIndex: Integer): PsImage;
    procedure RemoveAllImages;
    procedure RemoveImage(AIndex: Integer);
    function WriteImage(ARow, ACol: Cardinal; AFileName: String;
      AOffsetX: Double = 0.0; AOffsetY: Double = 0.0;
      AScaleX: Double = 1.0; AScaleY: Double = 1.0): Integer; overload;
    function WriteImage(ARow, ACol: Cardinal; AStream: TStream;
      AOffsetX: Double = 0.0; AOffsetY: Double = 0.0; AScaleX: Double = 1.0;
      AScaleY: Double = 1.0; ASize: Int64 = -1): Integer; overload;
    function WriteImage(ARow, ACol: Cardinal; AImageIndex: Integer;
      AOffsetX: Double = 0.0; AOffsetY: Double = 0.0; AScaleX: Double = 1.0;
      AScaleY: Double = 1.0): Integer; overload;
    procedure AddHyperlinkToImage(AImageIndex: Integer; ATarget: String;
      AToolTip: String = '');

    { Protection }
    procedure Protect(AEnable: Boolean);

    { Hidden }
    procedure Hide;
    function IsHidden: Boolean; inline;
    procedure Show;

    { Notification of changed cells, rows or columns }
    procedure ChangedCell(ARow, ACol: Cardinal);
    procedure ChangedCol(ACol: Cardinal);
    procedure ChangedFont(ARow, ACol: Cardinal);
    procedure ChangedRow(ARow: Cardinal);

    { Properties }

    {@@ List of cells of the worksheet. Only cells with contents or with formatting
        are listed }
    property  Cells: TsCells read FCells;
    {@@ List of all column records of the worksheet having a non-standard column width }
    property  Cols: TIndexedAVLTree read FCols;
    {@@ Information how the worksheet is encrypted }
    property  CryptoInfo: TsCryptoInfo read FCryptoInfo write FCryptoInfo;
    {@@ List of all comment records }
    property  Comments: TsComments read FComments;
    {@@ List of merged cells (contains TsCellRange records) }
    property  MergedCells: TsMergedCells read FMergedCells;
    {@@ List of hyperlink information records }
    property  Hyperlinks: TsHyperlinks read FHyperlinks;
    {@@ List of all formulas used in the sheet }
    property  Formulas: TsFormulas read FFormulas;
    {@@ FormatSettings for localization of some formatting strings }
    property  FormatSettings: TFormatSettings read GetFormatSettings;
    {@@ Index of the worksheet in the workbook }
    property  Index: Integer read GetIndex write SetIndex;
    {@@ Parameters to be used for printing by the Office applications }
    property PageLayout: TsPageLayout read FPageLayout write FPageLayout;
    {@@ List of all row records of the worksheet having a non-standard row height }
    property  Rows: TIndexedAVLTree read FRows;
    {@@ Color of the tab in the visual control - currently ignored }
    property  TabColor: TsColor read FTabColor write SetTabColor default scNotDefined;
    {@@ Workbook to which the worksheet belongs }
    property  Workbook: TsWorkbook read FWorkbook;
    {@@ In VirtualMode, the value of VirtualColCount signals how many colums
      will be transferred to the worksheet. }
    property VirtualColCount: cardinal read FVirtualColCount write SetVirtualColCount;
    {@@ The value VirtualRowCount indicates how many rows will be transferred
      to the worksheet in VirtualMode. }
    property VirtualRowCount: cardinal read FVirtualRowCount write SetVirtualRowCount;

    // These are properties to interface to TsWorksheetGrid
    property BiDiMode: TsBiDiMode read FBiDiMode write SetBiDiMode;
    {@@ Column index of the selected cell of this worksheet }
    property  ActiveCellCol: Cardinal read FActiveCellCol;
    {@@ Row index of the selected cell of this worksheet }
    property  ActiveCellRow: Cardinal read FActiveCellRow;
    {@@ Index of the left-most visible column in the grid - used by WorksheetGrid}
    property LeftCol: Cardinal read FLeftCol;
    {@@ Index of the top-most visible row in the grid - used by WorksheetGrid }
    property TopRow: Cardinal read FTopRow;
    {@@ Number of frozen columns which do not scroll }
    property  LeftPaneWidth: Integer read FLeftPaneWidth write FLeftPaneWidth;
    {@@ Number of frozen rows which do not scroll }
    property  TopPaneHeight: Integer read FTopPaneHeight write FTopPaneHeight;
    {@@ Zoom factor }
    property  ZoomFactor: Double read FZoomFactor write SetZoomFactor;
    {@@ Event fired when cell contents or formatting changes }
    property  OnChangeCell: TsCellEvent read FOnChangeCell write FOnChangeCell;
    {@@ Event fired when column height or formatting changes }
    property  OnChangeCol: TsColEvent read FOnChangeCol write FOnChangeCol;
    {@@ Event fired when the font size in a cell changes }
    property  OnChangeFont: TsCellEvent read FOnChangeFont write FOnChangeFont;
    {@@ Event fired when a row height or row formatting has changed }
    property  OnChangeRow: TsRowEvent read FOnChangeRow write FOnChangeRow;
    {@@ Event to override cell comparison for sorting }
    property  OnCompareCells: TsCellCompareEvent
      read FOnCompareCells write FOnCompareCells; deprecated 'Use OnFullCompareCells instead';
    {@@ Event to override cell comparison for sorting }
    property  OnFullCompareCells: TsCellFullCompareEvent read FOnFullCompareCells write FOnFullCompareCells;
    {@@ Event fired when a cell is "selected". }
    property  OnSelectCell: TsCellEvent read FOnSelectCell write FOnSelectCell;
    {@@ This event allows to provide external cell data for writing to file,
      standard cells are ignored. Intended for converting large database files
      to a spreadsheet format. Requires Option boVirtualMode to be set. }
    property OnWriteCellData: TsWorksheetWriteCellDataEvent read FOnWriteCellData write FOnWriteCellData;
    {@@ Event triggered when the worksheet is zoomed }
    property OnZoom: TsNotifyEvent read FOnZoom write FOnZoom;
  end;

  {@@ Event fired when reading a file in virtual mode. Read data are provided in
    the "ADataCell" (which is not added to the worksheet in virtual mode). }
  TsWorkbookReadCellDataEvent = procedure(Sender: TObject; ARow, ACol: Cardinal;
    const ADataCell: PCell) of object;

  {@@ Event procedure containing a specific worksheet }
  TsWorksheetEvent = procedure (Sender: TObject; ASheet: TsWorksheet) of object;

  {@@ Event procedure containing a specific workbook }
  TsWorkbookEvent = procedure (Sender: TsWorkbook) of object;

  {@@ Event procedure called when a worksheet is removed. ASheetIndex = -1 --> all sheets }
  TsRemoveWorksheetEvent = procedure (Sender: TObject; ASheetIndex: Integer) of object;

  {@@ FSome action has an effect on existing formulas which must be corrected. }
  TsFormulaCorrection = (fcWorksheetRenamed, fcWorksheetDeleted);


    { TsWorkbook }

  {@@ The workbook contains the worksheets and provides methods for reading from
    and writing to file. }
  TsWorkbook = class(TsBasicWorkbook)
  private
    { Internal data }
    FWorksheets: TFPList;
    FBuiltinFontCount: Integer;
    FReadWriteFlag: TsReadWriteFlag;
    FCalculationLock: Integer;
    FDeleteFormulaLock: Integer;
    FNotificationLock: Integer;
    FRebuildFormulaLock: Integer;
    FActiveWorksheet: TsWorksheet;
    FOnOpenWorkbook: TNotifyEvent;
    FOnCalcWorkbook: TsWorkbookEvent;
    FOnChangeWorksheet: TsWorksheetEvent;
    FOnRenameWorksheet: TsWorksheetEvent;
    FOnAddWorksheet: TsWorksheetEvent;
    FOnRemoveWorksheet: TsRemoveWorksheetEvent;
    FOnRemovingWorksheet: TsWorksheetEvent;
    FOnSelectWorksheet: TsWorksheetEvent;
    FOnReadCellData: TsWorkbookReadCellDataEvent;
    FSearchEngine: TObject;
    FCryptoInfo: TsCryptoInfo;
    FMetaData: TsMetaData;
    {FrevisionsCrypto: TsCryptoInfo;} // Commented out because it needs revision handling

    { Callback procedures }
    procedure RebuildFormulasCallback(Data, Arg: Pointer);
    procedure RemoveWorksheetsCallback(Data, Arg: pointer);

  protected
    FFontList: TFPList;
    FNumFormatList: TFPList;
    FCellFormatList: TsCellFormatList;
    FConditionalFormatList: TsConditionalFormatList;
    FEmbeddedObjList: TFPList;

    { Internal methods }
    class procedure GetFormatFromFileHeader(const AFileName: TFileName;
      out AFormatIDs: TsSpreadFormatIDArray); overload;
    class procedure GetFormatFromFileHeader(AStream: TStream;
      out AFormatIDs: TsSpreadFormatIDArray); overload;

    procedure PrepareBeforeReading;
    procedure PrepareBeforeSaving;

    function FixFormula(AFormula: PsFormula; ACorrection: TsFormulaCorrection;
      AData: Pointer; AParam: PtrInt): Boolean;

    procedure MoveSheet(AFromIndex, AToIndex: Integer);
  public
    { Base methods }
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure ReadFromFile(AFileName: string; AFormatID: TsSpreadFormatID;
      APassword: String = ''; AParams: TsStreamParams = []); overload;
    procedure ReadFromFile(AFileName: string; AFormat: TsSpreadsheetFormat;
      AParams: TsStreamParams = []); overload;
    procedure ReadFromFile(AFileName: string; APassword: String = '';
      AParams: TsStreamParams = []); overload;
    procedure ReadFromFileIgnoringExtension(AFileName: string;
      APassword: String = ''; AParams: TsStreamParams = []);
    procedure ReadFromStream(AStream: TStream; AFormatID: TsSpreadFormatID;
      APassword: String = ''; AParams: TsStreamParams = []); overload;
    procedure ReadFromStream(AStream: TStream; AFormat: TsSpreadsheetFormat;
      AParams: TsStreamParams = []); overload;

    procedure WriteToFile(const AFileName: string; const AFormatID: TsSpreadFormatID;
      const AOverwriteExisting: Boolean = False; AParams: TsStreamParams = []); overload;
    procedure WriteToFile(const AFileName: string; const AFormat: TsSpreadsheetFormat;
      const AOverwriteExisting: Boolean = False; AParams: TsStreamParams = []); overload;
    procedure WriteToFile(const AFileName: String;
      const AOverwriteExisting: Boolean = False; AParams: TsStreamParams = []); overload;
    procedure WriteToStream(AStream: TStream; AFormatID: TsSpreadFormatID;
      AParams: TsStreamParams = []); overload;
    procedure WriteToStream(AStream: TStream; AFormat: TsSpreadsheetFormat;
      AParams: TsStreamParams = []); overload;

    { Worksheet list handling methods }
    function  AddWorksheet(AName: string;
      ReplaceDuplicateName: Boolean = false): TsWorksheet;
    function  CopyWorksheetFrom(AWorksheet: TsWorksheet;
      ReplaceDuplicateName: Boolean): TsWorksheet;
    function  GetFirstWorksheet: TsWorksheet;
    function  GetLastWorksheet: TsWorksheet;
    function  GetNextWorksheet(AWorksheet: TsWorksheet): TsWorksheet;
    function  GetPreviousWorksheet(AWorksheet: TsWorksheet): TsWorksheet;
    function  GetWorksheetByIndex(AIndex: Integer): TsWorksheet;
    function  GetWorksheetByName(AName: String): TsWorksheet;
    function  GetWorksheetCount: Integer;
    function  GetVisibleWorksheetCount: Integer;
    function  GetWorksheetIndex(AWorksheet: TsBasicWorksheet): Integer; overload;
    function  GetWorksheetIndex(const AWorksheetName: String): Integer; overload;
    procedure RemoveAllWorksheets;
    procedure RemoveAllEmptyWorksheets;
    procedure RemoveWorksheet(AWorksheet: TsWorksheet);
    procedure SelectWorksheet(AWorksheet: TsWorksheet);
    function  ValidWorksheetName(var AName: String;
      ReplaceDuplicateName: Boolean = false): Boolean;

    { String-to-cell/range conversion }
    function TryStrToCell(AText: String; out AWorksheet: TsWorksheet;
      out ARow,ACol: Cardinal; AListSeparator: Char = #0): Boolean;
    function TryStrToCellRange(AText: String; out AWorksheet: TsWorksheet;
      out ARange: TsCellRange; AListSeparator: Char = #0): Boolean;
    function TryStrToCellRanges(AText: String; out AWorksheet: TsWorksheet;
      out ARanges: TsCellRangeArray; AListSeparator: Char = #0): Boolean;

    { Cell format handling }
    function AddCellFormat(const AValue: TsCellFormat): Integer;
    function GetCellFormat(AIndex: Integer): TsCellFormat;
    function GetCellFormatAsString(AIndex: Integer): String;
    function GetNumCellFormats: Integer;
    function GetPointerToCellFormat(AIndex: Integer): PsCellFormat;
    procedure RemoveAllCellFormats(AKeepDefaultFormat: Boolean);

    { Conditional formatting }
    function GetConditionalFormat(AIndex: Integer): TsConditionalFormat;
    function GetNumConditionalFormats: Integer;

    { Font handling }
    function AddFont(const AFontName: String; ASize: Single; AStyle: TsFontStyles;
      AColor: TsColor; APosition: TsFontPosition = fpNormal): Integer; overload;
    function AddFont(const AFont: TsFont): Integer; overload;
    function CloneFont(const AFontIndex: Integer): TsFont;
    procedure DeleteFont(const AFontIndex: Integer);
    function FindFont(const AFontName: String; ASize: Single; AStyle: TsFontStyles;
      AColor: TsColor; APosition: TsFontPosition = fpNormal): Integer;
    function GetBuiltinFontCount: Integer;
    function GetDefaultFont: TsFont;
    function GetDefaultFontSize: Single;
    function GetFont(AIndex: Integer): TsFont;
    function GetFontAsString(AIndex: Integer): String;
    function GetFontCount: Integer;
    function GetHyperlinkFont: TsFont;
    procedure InitFonts;
    procedure RemoveAllFonts;
    procedure ReplaceFont(AFontIndex: Integer; AFontName: String;
      ASize: Single; AStyle: TsFontStyles; AColor: TsColor;
      APosition: TsFontPosition = fpNormal);
    procedure SetDefaultFont(const AFontName: String; ASize: Single);

    { Number format handling }
    function AddNumberFormat(AFormatStr: String): Integer;
    function GetNumberFormat(AIndex: Integer): TsNumFormatParams;
    function GetNumberFormatCount: Integer;
    procedure RemoveAllNumberFormats;

    { Formulas }
    procedure CalcFormulas;
    function FixFormulas(ACorrection: TsFormulaCorrection; AData: Pointer;
      AParam: PtrInt): boolean;
    procedure RebuildFormulas;
    procedure LockFormulas;
    procedure UnlockFormulas;

    { Clipboard }
    procedure CopyToClipboardStream(AStream: TStream; AFormat: TsSpreadsheetFormat;
      AParams: TsStreamParams = []);
    procedure PasteFromClipboardStream(AStream: TStream; AFormat: TsSpreadsheetFormat;
      AOperation: TsCopyOperation; AParams: TsStreamParams = [];
      ATransposed: Boolean = false);

    { Embedded objects }
    function AddEmbeddedObj(const AFileName: String): Integer; overload;
    function AddEmbeddedObj(AStream: TStream;
      const AName: String = ''; ASize: Int64 = -1): Integer; overload;
    function FindEmbeddedObj(const AFileName: String): Integer;
    function GetEmbeddedObj(AIndex: Integer): TsEmbeddedObj;
    function GetEmbeddedObjCount: Integer;
    function HasEmbeddedSheetImages: Boolean;
    procedure RemoveAllEmbeddedObj;

    { Utilities }
    function ConvertUnits(AValue: Double; AFromUnits, AToUnits: TsSizeUnits): Double;
    procedure UpdateCaches;
    procedure GetLastRowColIndex(out ALastRow, ALastCol: Cardinal);

    { Notification }
    procedure ChangedWorksheet(AWorksheet: TsWorksheet);
    procedure DisableNotifications;
    procedure EnableNotifications;
    function NotificationsEnabled: Boolean;

    {@@ Identifies the "active" worksheet (only for visual controls)}
    property ActiveWorksheet: TsWorksheet read FActiveWorksheet write SelectWorksheet;
    property CryptoInfo: TsCryptoInfo read FCryptoInfo write FCryptoInfo;
    {property RevisionsCrypto: TsCryptoInfo read FRevisionsCrypto write FRevisionsCrypto;}

    {@@ Meta data}
    property MetaData: TsMetaData read FMetaData write FMetaData;

    {@@ This event fires whenever a new worksheet is added }
    property OnAddWorksheet: TsWorksheetEvent read FOnAddWorksheet write FOnAddWorksheet;
    {@@ This event fires whenever a worksheet is changed }
    property OnChangeWorksheet: TsWorksheetEvent read FOnChangeWorksheet write FOnChangeWorksheet;
    {@@ This event fires whenever a workbook is loaded }
    property OnOpenWorkbook: TNotifyEvent read FOnOpenWorkbook write FOnOpenWorkbook;
    {@@ This event fires whenever a worksheet is renamed }
    property OnRenameWorksheet: TsWorksheetEvent read FOnRenameWorksheet write FOnRenameWorksheet;
    {@@ This event fires AFTER a worksheet has been deleted }
    property OnRemoveWorksheet: TsRemoveWorksheetEvent read FOnRemoveWorksheet write FOnRemoveWorksheet;
    {@@ This event fires BEFORE a worksheet is deleted }
    property OnRemovingWorksheet: TsWorksheetEvent read FOnRemovingWorksheet write FOnRemovingWorksheet;
    {@@ This event fires when a worksheet is made "active"}
    property OnSelectWorksheet: TsWorksheetEvent read FOnSelectWorksheet write FOnSelectWorksheet;
    {@@ This event accepts cell data while reading a spreadsheet file. Data are
      not encorporated in a spreadsheet, they are just passed through to the
      event handler for processing. Requires option boVirtualMode to be set. }
    property OnReadCellData: TsWorkbookReadCellDataEvent read FOnReadCellData write FOnReadCellData;
    {@@ This event is fired when the workbook is recalculated. It allows to
      replace the calculation strategy. }
    property OnCalcWorkbook: TsWorkbookEvent read FOnCalcWorkbook write FOnCalcWorkbook;
  end;

procedure CopyCellFormat(AFromCell, AToCell: PCell);
procedure CopyColFormat(AFromCol, AToCol: PCol; AFromSheet, AToSheet: TsWorksheet);
procedure CopyRowFormat(AFromRow, AToRow: PRow; AFromSheet, AToSheet: TsWorksheet);


implementation

uses
  Math, StrUtils, DateUtils, TypInfo, lazutf8, lazFileUtils, URIParser,
  {%H-}fpsPatches, fpsStrings, fpsUtils, fpsHTMLUtils,
  fpsReaderWriter, fpsCurrency;

(*
const
  { These are reserved system colors by Microsoft
    0x0040 - Default foreground color - window text color in the sheet display.
    0x0041 - Default background color - window background color in the sheet
             display and is the default background color for a cell.
    0x004D - Default chart foreground color - window text color in the
             chart display.
    0x004E - Default chart background color - window background color in the
             chart display.
    0x004F - Chart neutral color which is black, an RGB value of (0,0,0).
    0x0051 - ToolTip text color - automatic font color for comments.
    0x7FFF - Font automatic color - window text color. }

  // Color indexes of reserved system colors
  DEF_FOREGROUND_COLOR = $0040;
  DEF_BACKGROUND_COLOR = $0041;
  DEF_CHART_FOREGROUND_COLOR = $004D;
  DEF_CHART_BACKGROUND_COLOR = $004E;
  DEF_CHART_NEUTRAL_COLOR = $004F;
  DEF_TOOLTIP_TEXT_COLOR = $0051;
  DEF_FONT_AUTOMATIC_COLOR = $7FFF;

  // Color rgb values of reserved system colors
  DEF_FOREGROUND_COLORVALUE = $000000;
  DEF_BACKGROUND_COLORVALUE = $FFFFFF;
  DEF_CHART_FOREGROUND_COLORVALUE = $000000;
  DEF_CHART_BACKGROUND_COLORVALUE = $FFFFFF;
  DEF_CHART_NEUTRAL_COLORVALUE = $FFFFFF;
  DEF_TOOLTIP_TEXT_COLORVALUE = $000000;
  DEF_FONT_AUTOMATIC_COLORVALUE = $000000;
       *)

{@@ ----------------------------------------------------------------------------
  Convenience method which creates the correct reader object for a given
  spreadsheet format.

  @param  AWorkbook  Workbook to be written
  @param  AFormatID  Identifier of the file format which is assumed when reading
                     a document into the workbook. An exception is raised when
                     the document has a different format.

  @param  AParams    Optional parameters to control stream access. If contains
                     the element spClipboard the reader knows that access is to
                     the clipboard, and it can read a special clipboard version
                     of the data.

  @return An instance of a TsBasicSpreadReader descendent which is able to
          read the given file format.
-------------------------------------------------------------------------------}
function CreateSpreadReader(AWorkbook: TsWorkbook; AFormatID: TsSpreadFormatID;
  AParams: TsStreamParams = []): TsBasicSpreadReader;
var
  readerClass: TsSpreadReaderClass;
begin
  Result := nil;
  Unused(AParams);
  readerClass := GetSpreadReaderClass(AFormatID);

  if readerClass <> nil
   then Result := readerClass.Create(AWorkbook);

 if Result = nil then
   raise EFPSpreadsheetReader.Create(rsUnsupportedReadFormat);
end;

{@@ ----------------------------------------------------------------------------
  Convenience method which creates the correct writer object for a given
  spreadsheet format.

  @param  AWorkbook  Workbook to be written
  @param  AFormatID  Identifier of the file format which is used for writing the
                     workbook
  @param  AParams    Optional parameters to control stream access. If contains
                     the element spClipboard then the writer can write a
                     dedicated clipboard version of the stream if required.
  @return An instance of a TsBasicSpreadWriter descendant which is able to
          write the given file format.
-------------------------------------------------------------------------------}

function CreateSpreadWriter(AWorkbook: TsWorkbook; AFormatID: TsSpreadFormatID;
  AParams: TsStreamParams = []): TsBasicSpreadWriter;
var
  writerClass: TsSpreadWriterClass;
begin
  Result := nil;
  Unused(AParams);
  writerClass := GetSpreadWriterClass(AFormatID);

  if writerClass <> nil then
    Result := writerClass.Create(AWorkbook);

  if Result = nil then
    raise EFPSpreadsheetWriter.Create(rsUnsupportedWriteFormat);
end;

{@@ ----------------------------------------------------------------------------
  Copies the format of a cell to another one.

  @param  AFromCell   Cell from which the format is to be copied
  @param  AToCell     Cell to which the format is to be copied
-------------------------------------------------------------------------------}
procedure CopyCellFormat(AFromCell, AToCell: PCell);
var
  sourceSheet, destSheet: TsWorksheet;
  fmt: TsCellFormat;
  numFmtParams: TsNumFormatParams;
  nfs: String;
  font: TsFont;
begin
  Assert(AFromCell <> nil);
  Assert(AToCell <> nil);
  sourceSheet := TsWorksheet(AFromCell^.Worksheet);
  destSheet := TsWorksheet(AToCell^.Worksheet);
  if (sourceSheet=nil) or (destSheet=nil) or (sourceSheet.Workbook = destSheet.Workbook) then
    AToCell^.FormatIndex := AFromCell^.FormatIndex
  else
  begin
    fmt := sourceSheet.ReadCellFormat(AFromCell);
    if (uffFont in fmt.UsedFormattingFields) then
    begin
      font := sourceSheet.ReadCellFont(AFromCell);
      fmt.FontIndex := destSheet.Workbook.FindFont(font.FontName, font.Size, font.Style, font.Color);
      if fmt.FontIndex = -1 then
        fmt.FontIndex := destSheet.Workbook.AddFont(font.FontName, font.Size, font.Style, font.Color);
    end;
    if (uffNumberformat in fmt.UsedFormattingFields) then
    begin
      numFmtParams := sourceSheet.Workbook.GetNumberFormat(fmt.NumberFormatIndex);
      if numFmtParams <> nil then
      begin
        nfs := numFmtParams.NumFormatStr;
        fmt.NumberFormatIndex := destSheet.Workbook.AddNumberFormat(nfs);
      end;
    end;
    destSheet.WriteCellFormat(AToCell, fmt);
  end;
end;

procedure CopyColFormat(AFromCol, AToCol: PCol; AFromSheet, AToSheet: TsWorksheet);
var
  fmt: TsCellFormat;
  numFmtParams: TsNumFormatParams;
  nfs: String;
  font: TsFont;
begin
  if (AFromSheet = nil) or (AToSheet = nil) or (AFromSheet.Workbook = AToSheet.Workbook) then
    // Both columns in the same sheet --> the format index is valid
    AToCol^.FormatIndex := AFromCol^.FormatIndex
  else
  begin
    // Both columns in different worksheets. We must create a new format record
    // in the destination sheet from the format used by the source column
    // and store the new format index in the column record of the dest col.
    fmt := AFromSheet.Workbook.GetCellFormat(AFromCol^.FormatIndex);
    if (uffFont in fmt.UsedFormattingFields) then
    begin
      font := AFromSheet.Workbook.GetFont(fmt.FontIndex);
      fmt.FontIndex := AToSheet.Workbook.FindFont(font.FontName, font.Size, font.Style, font.Color);
      if fmt.FontIndex = -1 then
        fmt.FontIndex := AToSheet.Workbook.AddFont(font.FontName, font.Size, font.Style, font.Color);
    end;
    if (uffNumberformat in fmt.UsedFormattingFields) then
    begin
      numFmtParams := AFromSheet.Workbook.GetNumberFormat(fmt.NumberFormatIndex);
      if numFmtParams <> nil then
      begin
        nfs := numFmtParams.NumFormatStr;
        fmt.NumberFormatIndex := AToSheet.Workbook.AddNumberFormat(nfs);
      end;
    end;
    AToCol^.FormatIndex := AToSheet.Workbook.AddCellFormat(fmt);
  end;
end;

procedure CopyRowFormat(AFromRow, AToRow: PRow; AFromSheet, AToSheet: TsWorksheet);
var
  fmt: TsCellFormat;
  numFmtParams: TsNumFormatParams;
  nfs: String;
  font: TsFont;
begin
  if (AFromSheet = nil) or (AToSheet = nil) or (AFromSheet.Workbook = AToSheet.Workbook) then
    // Both rows are in the same sheet --> the format index is valid
    AToRow^.FormatIndex := AFromRow^.FormatIndex
  else
  begin
    // Both rows are in different worksheets. We must create a new format record
    // in the destination sheet from the format used by the source row
    // and store the new format index in the row record of the dest row.
    fmt := AFromSheet.Workbook.GetCellFormat(AFromRow^.FormatIndex);
    if (uffFont in fmt.UsedFormattingFields) then
    begin
      font := AFromSheet.Workbook.GetFont(fmt.FontIndex);
      fmt.FontIndex := AToSheet.Workbook.FindFont(font.FontName, font.Size, font.Style, font.Color);
      if fmt.FontIndex = -1 then
        fmt.FontIndex := AToSheet.Workbook.AddFont(font.FontName, font.Size, font.Style, font.Color);
    end;
    if (uffNumberformat in fmt.UsedFormattingFields) then
    begin
      numFmtParams := AFromSheet.Workbook.GetNumberFormat(fmt.NumberFormatIndex);
      if numFmtParams <> nil then
      begin
        nfs := numFmtParams.NumFormatStr;
        fmt.NumberFormatIndex := AToSheet.Workbook.AddNumberFormat(nfs);
      end;
    end;
    AToRow^.FormatIndex := AToSheet.Workbook.AddCellFormat(fmt);
  end;
end;


function CompareCells(Item1, Item2: Pointer): Integer;
begin
  result := LongInt(PCell(Item1)^.Row) - PCell(Item2)^.Row;
  if Result = 0 then
    Result := LongInt(PCell(Item1)^.Col) - PCell(Item2)^.Col;
end;

function CompareRows(Item1, Item2: Pointer): Integer;
begin
  Result := LongInt(PRow(Item1)^.Row) - PRow(Item2)^.Row;
end;

function CompareCols(Item1, Item2: Pointer): Integer;
begin
  Result := LongInt(PCol(Item1)^.Col) - PCol(Item2)^.Col;
end;

function CompareMergedCells(Item1, Item2: Pointer): Integer;
begin
  Result := LongInt(PsCellRange(Item1)^.Row1) - PsCellRange(Item2)^.Row1;
  if Result = 0 then
    Result := LongInt(PsCellRange(Item1)^.Col1) - PsCellRange(Item2)^.Col1;
end;


{==============================================================================}
{                           TsWorksheet                                        }
{==============================================================================}

{@@ ----------------------------------------------------------------------------
  Constructor of the TsWorksheet class.
-------------------------------------------------------------------------------}
constructor TsWorksheet.Create;
begin
  inherited Create;

  FCells := TsCells.Create(self);
  FRows := TIndexedAVLTree.Create(@CompareRows);
  FCols := TIndexedAVLTree.Create(@CompareCols);
  FComments := TsComments.Create;
  FMergedCells := TsMergedCells.Create;
  FHyperlinks := TsHyperlinks.Create;
  FFormulas := TsFormulas.Create;
  FImages := TFPList.Create;

  FPageLayout := TsPageLayout.Create(self);

  FDefaultColWidth := ptsToMM(72);   // Excel: about 72 pts
  FDefaultRowHeight := ptsToMM(15);  // Excel: 15pts
  FZoomFactor := 1.0;
  FTabColor := scNotDefined;

  FFirstRowIndex := UNASSIGNED_ROW_COL_INDEX;
  FFirstColIndex := UNASSIGNED_ROW_COL_INDEX;
  FLastRowIndex := UNASSIGNED_ROW_COL_INDEX;
  FLastColIndex := UNASSIGNED_ROW_COL_INDEX;

  FActiveCellRow := UNASSIGNED_ROW_COL_INDEX;
  FActiveCellCol := UNASSIGNED_ROW_COL_INDEX;

  InitCryptoInfo(FCryptoInfo);

  FOptions := [soShowGridLines, soShowHeaders, soAutoDetectCellType];
end;

{@@ ----------------------------------------------------------------------------
  Destructor of the TsWorksheet class.
  Releases all memory, but does not delete from the workbook's worksheetList !!!

  NOTE: Don't call directly. Always use Workbook.RemoveWorksheet to remove a
  worksheet from a workbook.
-------------------------------------------------------------------------------}
destructor TsWorksheet.Destroy;
begin
  RemoveAllImages;
  RemoveAllRows;
  RemoveAllCols;

  FPageLayout.Free;
  FCells.Free;
  FRows.Free;
  FCols.Free;
  FComments.Free;
  FMergedCells.Free;
  FHyperlinks.Free;
  FFormulas.Free;
  FImages.Free;

  inherited Destroy;
end;

{@@ ----------------------------------------------------------------------------
  Helper function which constructs an rpn formula from the cell's string
  formula. This is needed, for example, when writing a formula to xls biff
  file format.
  The formula is stored in ACell.
  If ADestCell is not nil then the relative references are adjusted as seen
  from ADestCell. This means that this function returns the formula that
  would be created if ACell is copied to the location of ADestCell.
  Needed for copying formulas and for splitting shared formulas.
-------------------------------------------------------------------------------}
function TsWorksheet.BuildRPNFormula(ACell: PCell;
  ADestCell: PCell = nil): TsRPNFormula;
var
  formula: PsFormula;
begin
  Result := nil;
  if (ACell = nil) or (not HasFormula(ACell)) then
    exit;

  formula := FFormulas.FindFormula(ACell^.Row, ACell^.Col);
  if formula = nil then
    exit;

  if ADestCell <> nil then begin
    formula^.Parser.PrepareCopyMode(ACell, ADestCell);
    Result := formula^.Parser.RPNFormula;
    formula^.Parser.PrepareCopyMode(nil, nil);
  end else
    Result := formula^.Parser.RPNFormula;
end;

{@@ ----------------------------------------------------------------------------
  Calculates the provided formula

  Should not be called by itself because the result may depend on other formulas
  which may have not yet been calculated. It is better to call CalcFormulas
  instead.

  @param  AFormula  Formula to be calculated. The formula belongs to the
                    cell specified by the formula's Row and Col parameters.
-------------------------------------------------------------------------------}
procedure TsWorksheet.CalcFormula(AFormula: PsFormula);
var
  lCell, lCellRef: PCell;
  parser: TsExpressionParser = nil;
  res: TsExpressionResult;
  p: Integer;
  link, txt: String;
begin
  if (boIgnoreFormulas in Workbook.Options) or (AFormula = nil) then
    exit;

  if (AFormula^.Text = '') and (AFormula^.Parser = nil) then
    raise ECalcEngine.Create('CalcFormula: no formula specified.');

  AFormula^.CalcState := csCalculating;
  if AFormula^.Parser = nil then begin
    parser := TsSpreadsheetParser.Create(self);
    try
      parser.Expression[fdExcelA1] := AFormula^.Text;
      AFormula^.Parser := parser;
    except
      on E:ECalcEngine do begin
        Workbook.AddErrorMsg(E.Message);
        res := ErrorResult(errIllegalRef);
      end;
    end;
  end;

  if AFormula^.Parser <> nil then
    try
      res := AFormula^.Parser.Evaluate;
      if AFormula^.Text = '' then
        AFormula^.Text := AFormula^.Parser.Expression[fdExcelA1];
    except
      on E: ECalcEngine do
      begin
        Workbook.AddErrorMsg(E.Message);
        res := ErrorResult(errIllegalRef);
      end;
    end;

  // Find or create the formula cell
  lCell := GetCell(AFormula^.Row, AFormula^.Col);
  FWorkbook.LockFormulas;
  try
    // Assign formula result
    case res.ResultType of
      rtEmpty     : WriteBlank(lCell, true);
      rtError     : WriteErrorValue(lCell, res.ResError);
      rtInteger   : WriteNumber(lCell, res.ResInteger);
      rtFloat     : WriteNumber(lCell, res.ResFloat);
      rtDateTime  : WriteDateTime(lCell, res.ResDateTime);
      rtString    : WriteText(lCell, res.ResString);
      rtHyperlink : begin
                      link := ArgToString(res);
                      p := pos(HYPERLINK_SEPARATOR, link);
                      if p > 0 then
                      begin
                        txt := Copy(link, p+Length(HYPERLINK_SEPARATOR), Length(link));
                        link := Copy(link, 1, p-1);
                      end else
                        txt := link;
                      WriteHyperlink(lCell, link);
                      WriteText(lCell, txt);
                    end;
      rtBoolean   : WriteBoolValue(lCell, res.ResBoolean);
      rtCell      : begin
                      lCellRef := (res.Worksheet as TsWorksheet).FindCell(res.ResRow, res.ResCol);
                      if lCellRef <> nil then
                        case lCellRef^.ContentType of
                          cctNumber    : WriteNumber(lCell, lCellRef^.NumberValue);
                          cctDateTime  : WriteDateTime(lCell, lCellRef^.DateTimeValue);
                          cctUTF8String: WriteText(lCell, lCellRef^.UTF8StringValue);
                          cctBool      : WriteBoolValue(lCell, lCellRef^.Boolvalue);
                          cctError     : WriteErrorValue(lCell, lCellRef^.ErrorValue);
                          cctEmpty     : WriteBlank(lCell, true);
                        end
                      else
                        WriteBlank(lCell, true);
                    end;
    end;
  finally
    FWorkbook.UnlockFormulas;
  end;

  // Restore the formula. Could have been erased by WriteBlank or WriteText('')
  AFormula^.CalcState := csCalculated;
end;

{@@ ----------------------------------------------------------------------------
  Calculates all formulas of the workbook

  Must be used when the formulas in the workbook contain references to other
  sheets.
  If this is not the case the faster "CalcSheet" can be used.
-------------------------------------------------------------------------------}
procedure TsWorksheet.CalcFormulas;
begin
  Workbook.CalcFormulas;
  // To do: Determine whether the worksheet has in- and out-going links
  // to others sheets. If not call the faster "CalcSheet".
end;

{@@ ----------------------------------------------------------------------------
  Calculates all formulas of the worksheet

  Since formulas may reference not-yet-calculated cells, this occurs in
  two steps:
  1. All formulas are marked as "not calculated".
  2. Formulas are calculated. If formulas in referenced are found as being
     "not calculated" they are calculated and then tagged as "calculated".
  This results in an iterative calculation procedure. In the end, all formulas
  are calculated.

  NOTE: IF THE WORKSHEET CONTAINS CELLS WHICH LINK TO OTHER WORKSHEETS THEN
  THIS CALCULATION MAY NOT BE CORRECT. USE THE METHOD CalcFormulas OF THE
  WORKBOOK INSTEAD !!!
-------------------------------------------------------------------------------}
procedure TsWorksheet.CalcSheet;
var
  formula: PsFormula;
begin
  if (boIgnoreFormulas in Workbook.Options) then
    exit;

  { prevent infinite loop due to triggerung of formula recalculation whenever
    a cell changes during execution of CalcFormulas }
  inc(FWorkbook.FCalculationLock);
  try
    // State 1 - mark all formulas as "not calculated"
    for formula in FFormulas do
      formula^.CalcState := csNotCalculated;

    // State 2 - calculate formulas. If a formula required during calculation
    // is found as not-yet-calculated, then it is calculated immediately.
    for formula in FFormulas do
      CalcFormula(formula);
  finally
    dec(FWorkbook.FCalculationLock);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether a cell given by its row and column indexes belongs to a
  specified rectangular cell range.
-------------------------------------------------------------------------------}
class function TsWorksheet.CellInRange(ARow, ACol: Cardinal;
  ARange: TsCellRange): Boolean;
begin
  Result := (ARow >= ARange.Row1) and (ARow <= ARange.Row2) and
            (ACol >= ARange.Col1) and (ACol <= ARange.Col2);
end;

{@@ ----------------------------------------------------------------------------
  Converts a FPSpreadsheet cell position, which is Row, Col in numbers
  and zero based - e.g. 0,0 - to a textual representation which is [Col][Row],
  where the Col is in letters and the row is in 1-based numbers - e.g. A1
-------------------------------------------------------------------------------}
class function TsWorksheet.CellPosToText(ARow, ACol: Cardinal): string;
begin
  Result := GetCellString(ARow, ACol, [rfRelCol, rfRelRow]);
end;

{@@ ----------------------------------------------------------------------------
  Checks entire worksheet, whether this cell is used in any formula.

  @param   ARow  Row index of the cell considered
  @param   ACol  Column index of the cell considered
  @return  TRUE if the cell is used in a formula, FALSE if not
-------------------------------------------------------------------------------}
function TsWorksheet.CellUsedInFormula(ARow, ACol: Cardinal): Boolean;
var
  cell: PCell;
  fe: TsFormulaElement;
  i: Integer;
  rpnFormula: TsRPNFormula;
begin
  for cell in FCells do
  begin
    if HasFormula(cell) then begin
      if (cell^.Row = ARow) and (cell^.Col = ACol) then
      begin
        Result := true;
        exit;
      end;
      rpnFormula := BuildRPNFormula(cell);
      for i := 0 to Length(rpnFormula)-1 do
      begin
        fe := rpnFormula[i];
        case fe.ElementKind of
          fekCell, fekCellRef:
            if (fe.Row = ARow) and (fe.Col = ACol) then
            begin
              Result := true;
              exit;
            end;
          fekCellRange:
            if (fe.Row <= ARow) and (ARow <= fe.Row2) and
               (fe.Col <= ACol) and (ACol <= fe.Col2) then
            begin
              Result := true;
              exit;
            end;
        end;
      end;
    end;
  end;
  SetLength(rpnFormula, 0);
  Result := false;
end;


{@@ ----------------------------------------------------------------------------
  Is called whenever a cell value or formatting has changed. Fires an event
  "OnChangeCell". This is handled by TsWorksheetGrid to update the grid cell.

  @param  ARow   Row index of the cell which has been changed
  @param  ACol   Column index of the cell which has been changed
-------------------------------------------------------------------------------}
procedure TsWorksheet.ChangedCell(ARow, ACol: Cardinal);
begin
  if FWorkbook.FReadWriteFlag = rwfRead then
    exit;

  if (FWorkbook.FCalculationLock = 0) and (boAutoCalc in FWorkbook.Options) then
  begin
  //  if CellUsedInFormula(ARow, ACol) then
      CalcFormulas;
  end;

  if FWorkbook.NotificationsEnabled and Assigned(FOnChangeCell) then
    FOnChangeCell(Self, ARow, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Is called whenever a column width or column format has changed. Fires an event
  "OnChangedCol" which is handled by TsWorkbookSource

  @param  ACol  Index of the column which as changed
-------------------------------------------------------------------------------}
procedure TsWorksheet.ChangedCol(ACol: Cardinal);
begin
  if FWorkbook.FReadWriteFlag = rwfRead then
    exit;
  if FWorkbook.NotificationsEnabled and Assigned(FOnChangeCol) then
    FOnChangeCol(Self, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Is called whenever a row height or row format has changed. Fires an event
  "OnChangedRow" which is handled by TsWorkbookSource

  @param  ARow  Index of the row which as changed
-------------------------------------------------------------------------------}
procedure TsWorksheet.ChangedRow(ARow: Cardinal);
begin
  if FWorkbook.FReadWriteFlag = rwfRead then
    exit;
  if FWorkbook.NotificationsEnabled and Assigned(FOnChangeRow) then
    FOnChangeRow(Self, ARow);
end;

{@@ ----------------------------------------------------------------------------
  Is called whenever a font height changes. Fires an even "OnChangeFont"
  which is handled by TsWorksheetGrid to update the row heights.

  @param  ARow  Row index of the cell for which the font height has changed
  @param  ACol  Column index of the cell for which the font height has changed.
-------------------------------------------------------------------------------}
procedure TsWorksheet.ChangedFont(ARow, ACol: Cardinal);
begin
  if (FWorkbook.FReadWriteFlag = rwfRead) or not FWorkbook.NotificationsEnabled then
    exit;
  if Assigned(FOnChangeFont) then
    FOnChangeFont(Self, ARow, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Copies a cell to a cell at another location. The new cell has the same values
  and the same formatting. It differs in formula (adapted relative references)
  and col/row indexes.

  Both cells can be in different worksheets.

  @param   FromCell   Pointer to the source cell which will be copied
  @param   ToCell     Pointer to the destination cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyCell(AFromCell, AToCell: PCell);
var
  toRow, toCol: LongInt;
  row1, col1, row2, col2: Cardinal;
  hyperlink: PsHyperlink;
  fnt: TsFont;
  fntIndex: Integer;
  srcSheet, destSheet: TsWorksheet;
  i: Integer;
begin
  if (AFromCell = nil) or (AToCell = nil) then
    exit;

  // Short-cut for source and destination worksheets
  srcSheet := TsWorksheet(AFromcell^.Worksheet);
  destSheet := TsWorksheet(AToCell^.Worksheet);

  // Remember the row and column indexes of the destination cell.
  toRow := AToCell^.Row;
  toCol := AToCell^.Col;

  // Avoid misplaced notifications during the copy operations when things could
  // not yet be in place.
  FWorkbook.DisableNotifications;

  // Copy cell values and flags
  AToCell^ := AFromCell^;

  // Restore row and column indexes overwritten by the previous instruction
  AToCell^.Row := toRow;
  AToCell^.Col := toCol;
  AToCell^.Worksheet := destSheet;  // restore overwritten destination worksheet
     // was: self;

  // Fix relative references in formulas
  // This also fires the OnChange event.
  CopyFormula(AFromCell, AToCell);

  // Copy cell format
  CopyCellFormat(AFromCell, AToCell);

  // Merged?
  if srcSheet.IsMergeBase(AFromCell) then
  begin
    srcSheet.FindMergedRange(AFromCell, row1, col1, row2, col2);
    MergeCells(toRow, toCol, toRow + LongInt(row2) - LongInt(row1), toCol + LongInt(col2) - LongInt(col1));
  end;

  // Copy comment
  if srcSheet.HasComment(AFromCell) then
    WriteComment(AToCell, ReadComment(AFromCell));

  // Copy hyperlink
  hyperlink := srcSheet.FindHyperlink(AFromCell);
  if hyperlink <> nil then
    WriteHyperlink(AToCell, hyperlink^.Target, hyperlink^.Tooltip);

  // Copy rich text parameters
  if (AFromCell^.ContentType = cctUTF8String) and (Length(AFromCell^.RichTextParams) > 0) then
  begin
    SetLength(AToCell^.RichTextParams, Length(AFromCell^.RichTextParams));
    // Make sure that fonts exist at destination
    for i := 0 to High(AFromCell^.RichTextParams) do
    begin
      AToCell^.RichTextParams[i] := AFromCell^.RichTextParams[i];
      fnt := srcSheet.Workbook.GetFont(AFromCell^.RichTextParams[i].FontIndex);
      fntIndex := Workbook.FindFont(fnt.FontName, fnt.Size, fnt.Style, fnt.Color, fnt.Position);
      if fntIndex = -1 then
        fntIndex := Workbook.AddFont(fnt.FontName, fnt.Size, fnt.Style, fnt.Color, fnt.Position);
      AToCell^.RichTextParams[i].FontIndex := fntIndex;
    end;
  end;

  FWorkbook.EnableNotifications;

  // Notify visual controls of changes
  ChangedCell(AToCell^.Row, AToCell^.Col);

  // Notify visual controls of possibly changed row heights.
  ChangedFont(AToCell^.Row, AToCell^.Col);
end;

{@@ ----------------------------------------------------------------------------
  Copies a cell. The source cell can be located in a different worksheet, while
  the destination cell must be in the same worksheet which calls the methode.

  @param AFromRow  Row index of the source cell
  @param AFromCol  Column index of the source cell
  @param AToRow    Row index of the destination cell
  @param AToCol    Column index of the destination cell
  @param AFromWorksheet  Worksheet containing the source cell. Self, if omitted.

  @return Created new destination cell
-------------------------------------------------------------------------------}
function TsWorksheet.CopyCell(AFromRow, AFromCol, AToRow, AToCol: Cardinal;
  AFromWorksheet: TsWorksheet = nil): PCell;
var
  srcCell, destCell: PCell;
begin
  if AFromWorksheet = nil then
    AFromWorksheet := self;

  srcCell := AFromWorksheet.FindCell(AFromRow, AFromCol);
  destCell := GetCell(AToRow, AToCol);

  CopyCell(srcCell, destCell);

  ChangedCell(AToRow, AToCol);
  ChangedFont(AToRow, AToCol);

  Result := destCell;
end;

{@@ ----------------------------------------------------------------------------
  Copies all format parameters from the format cell to another cell.

  @param AFromCell  Pointer to source cell
  @param AToCell    Pointer to destination cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyFormat(AFromCell, AToCell: PCell);
begin
  if (AFromCell = nil) or (AToCell = nil) then
    exit;

  CopyCellFormat(AFromCell, AToCell);

  ChangedCell(AToCell^.Row, AToCell^.Col);
  ChangedFont(AToCell^.Row, AToCell^.Col);
end;

{@@ ----------------------------------------------------------------------------
  Copies all format parameters from a given cell to another cell identified
  by its row/column indexes.

  @param  AFormatCell Pointer to the source cell from which the format is copied.
  @param  AToRow      Row index of the destination cell
  @param  AToCol      Column index of the destination cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyFormat(AFormatCell: PCell; AToRow, AToCol: Cardinal);
begin
  CopyFormat(AFormatCell, GetCell(AToRow, AToCol));
end;

{@@ ----------------------------------------------------------------------------
  Copies the formula of a specified cell to another cell. Adapts relative
  cell references to the new cell.

  @param  AFromCell  Pointer to the source cell from which the formula is to be
                     copied
  @param  AToCell    Pointer to the destination cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyFormula(AFromCell, AToCell: PCell);
var
  srcBook, destBook: TsWorkbook;
  srcSheet, destSheet: TsWorksheet;
  referencedSheet: TsWorksheet;
  sheetName: String;
  srcFormula, destFormula: PsFormula;
  rpn: TsRPNFormula;
  elem: TsFormulaElement;
  i: Integer;
begin
  if (AFromCell = nil) or (AToCell = nil) then
    exit;

  srcSheet := TsWorksheet(AFromCell^.Worksheet);
  destSheet := TsWorksheet(AToCell^.Worksheet);
  srcBook := TsWorkbook(srcSheet.Workbook);
  destBook := TsWorkbook(destSheet.Workbook);

  destSheet.DeleteFormula(AToCell);

  if not HasFormula(AFromCell) then
    exit;

  srcFormula := srcSheet.Formulas.FindFormula(AFromCell^.Row, AFromCell^.Col);
  destFormula := destSheet.Formulas.AddFormula(AToCell^.Row, AToCell^.Col);
  destFormula.Parser := TsSpreadsheetParser.Create(destSheet);

  srcFormula^.Parser.PrepareCopyMode(AFromCell, AToCell);
  try
    rpn := srcFormula^.Parser.RPNFormula;
    // Make sure that referenced sheets exist in destination workbook
    for i:=0 to High(rpn) do begin
      elem := rpn[i];
      if elem.ElementKind in [fekCell3D, fekCellRef3d, fekCellRange3d] then begin
        sheetName := srcBook.GetWorksheetByIndex(elem.Sheet).Name;
        referencedSheet := destBook.GetWorksheetByName(sheetName);
        if referencedSheet = nil then
          referencedSheet := destBook.AddWorksheet(sheetName);
        rpn[i].Sheet := destBook.GetWorksheetIndex(referencedSheet);
        if (elem.Sheet = elem.Sheet2) or (elem.Sheet2 = -1) then
          continue;
        sheetName := srcBook.GetWorksheetByIndex(elem.Sheet2).Name;
        referencedSheet := destBook.GetWorksheetByName(sheetName);
        if referencedSheet = nil then
          referencedSheet := destBook.AddWorksheet(sheetName);
        rpn[i].Sheet2 := destBook.GetWorksheetIndex(referencedSheet);
      end;
    end;
    destFormula^.Parser.RPNFormula := rpn;
    destFormula^.Text := destFormula^.Parser.Expression[fdExcelA1];
    UseFormulaInCell(AToCell, destFormula);
  finally
    srcFormula^.Parser.PrepareCopyMode(nil, nil);
  end;
  ChangedCell(AToCell^.Row, AToCell^.Col);
end;

{@@ ----------------------------------------------------------------------------
  Copies the formula of a specified cell to another cell given by its row and
  column index. Relative cell references are adapted to the new cell.

  @param  AFormatCell Pointer to the source cell containing the formula to be
                      copied
  @param  AToRow      Row index of the destination cell
  @param  AToCol      Column index of the destination cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyFormula(AFormulaCell: PCell; AToRow, AToCol: Cardinal);
begin
  CopyFormula(AFormulaCell, GetCell(AToRow, AToCol));
end;

{@@ ----------------------------------------------------------------------------
  Copies the value of a specified cell to another cell (without copying
  formulas or formats)

  @param  AFromCell  Pointer to the source cell providing the value to be copied
  @param  AToCell    Pointer to the destination cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyValue(AFromCell, AToCell: PCell);
begin
  if (AToCell = nil) then   // AFromCell is allowed to be empty
    exit;

  if AFromCell <> nil then begin
    AToCell^.ContentType := AFromCell^.ContentType;
    AToCell^.NumberValue := AFromCell^.NumberValue;
    AToCell^.DateTimeValue := AFromCell^.DateTimeValue;
    AToCell^.BoolValue := AFromCell^.BoolValue;
    AToCell^.ErrorValue := AFromCell^.ErrorValue;
    AToCell^.UTF8StringValue := AFromCell^.UTF8StringValue;
  end else
    AToCell^.ContentType := cctEmpty;

  // Note: As confirmed with Excel, the formula is not to be copied here.
  // But that of the destination cell must be erased.
  DeleteFormula(AToCell);

  ChangedCell(AToCell^.Row, AToCell^.Col);
end;

{@@ ----------------------------------------------------------------------------
  Copies the value of a specified cell to another cell given by its row and
  column index

  @param  AValueCell  Pointer to the cell containing the value to be copied
  @param  AToRow      Row index of the destination cell
  @param  AToCol      Column index of the destination cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyValue(AValueCell: PCell; AToRow, AToCol: Cardinal);
begin
  CopyValue(AValueCell, GetCell(AToRow, AToCol));
end;

{@@ ----------------------------------------------------------------------------
  Copies a column record to another location. The new column has the same
  colwidth and the same formatting.

  @param   AFromCol    Index of the column to be copied
  @param   AToCol      Index of the destination column
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyCol(AFromCol, AToCol: Cardinal;
  AFromWorksheet: TsWorksheet = nil);
var
  srcCol, destCol: PCol;
begin
  if AFromWorksheet = nil then
    AFromWorksheet := self;
  srcCol := AFromWorksheet.FindCol(AFromCol);
  destCol := FindCol(AToCol);

  // Overwrite destination column with empty column record ?
  if (srcCol = nil) then
  begin
    if destCol <> nil then
      DeleteCol(AToCol);
    exit;
  end;

  // Create new or use existing column record
  destCol := GetCol(AToCol);

  // Copy contents of column record...
  destCol^ := srcCol^;
  // ... and restore column index lost in previous step
  destCol^.Col := AToCol;
  // ... and copy the format record - it may have be missing at destination
  CopyColFormat(srcCol, destCol, AFromWorksheet, self);

  // Notify visual controls of changes
  ChangedCol(destCol^.Col);
end;

{@@ ----------------------------------------------------------------------------
  Copies a row record to another location. The new row has the same
  row heightand the same formatting.

  @param   AFromRow    Index of the row to be copied
  @param   AToTow      Index of the destination row
-------------------------------------------------------------------------------}
procedure TsWorksheet.CopyRow(AFromRow, AToRow: Cardinal;
  AFromWorksheet: TsWorksheet);
var
  srcRow, destRow: PRow;
begin
  if AFromWorksheet = nil then
    AFromWorksheet := self;
  srcRow := AFromWorksheet.FindRow(AFromRow);
  destRow := FindRow(AToRow);

  // Overwrite destination row with empty row record?
  if (srcRow = nil) then
  begin
    if destRow <> nil then
      DeleteRow(AToRow);
    exit;
  end;

  // Create new or use existing row record
  destRow := GetRow(AToRow);

  // Copy contents of row record...
  destRow^ := srcRow^;
  // ... and restore row index lost in previous step
  destRow^.Row := AToRow;
  // ... and copy the format record - it may have be missing at destination
  CopyRowFormat(srcRow, destRow, AFromWorksheet, self);
end;

procedure TsWorksheet.Clear;
begin
  FCells.Clear;
  FComments.Clear;
  FHyperlinks.Clear;
  FMergedCells.Clear;

  RemoveAllImages;
  RemoveAllRows;
  RemoveAllCols;

  ChangedCell(0, 0);
end;

{@@ ----------------------------------------------------------------------------
  Deletes a specified cell. If the cell belongs to a merged block its content
  and formatting is erased. Otherwise the cell is destroyed and its memory is
  released.
-------------------------------------------------------------------------------}
procedure TsWorksheet.DeleteCell(ACell: PCell);
{$warning TODO: Shift cells to the right/below !!! ??? }
var
  r, c: Cardinal;
begin
  if ACell = nil then
    exit;

  // Does cell have a comment? -->  remove it
  if HasComment(ACell) then
    WriteComment(ACell, '');

  // Does cell have a hyperlink? --> remove it
  if HasHyperlink(ACell) then
    WriteHyperlink(ACell, '');

  // Does cell have a formula? --> remove it
  if HasFormula(ACell) then
    WriteFormula(ACell, '');

  // To do: Check if the cell is referencec by a formula. In this case we have
  // a #REF! error.

  // Cell is part of a merged block? --> Erase content, formatting etc.
  if IsMerged(ACell)  then
  begin
    EraseCell(ACell);
    exit;
  end;

  r := ACell^.Row;
  c := ACell^.Col;

  // Destroy the cell, and remove it from the tree
  RemoveAndFreeCell(ACell^.Row, ACell^.Col);

  ChangedCell(r, c);
end;

{@@ ----------------------------------------------------------------------------
  Erases content and formatting of a cell. The cell still occupies memory.

  @param  ACell  Pointer to cell to be erased.
-------------------------------------------------------------------------------}
procedure TsWorksheet.EraseCell(ACell: PCell; AKeepFormat: Boolean = false);
var
  r, c: Cardinal;
begin
  if ACell <> nil then begin
    r := ACell^.Row;
    c := ACell^.Col;

    // Unmerge range if the cell is the base of a merged block
    if IsMergeBase(ACell) then
      UnmergeCells(r, c);

    // Remove the comment if the cell has one
    RemoveComment(ACell);

    // Removes a hyperlink it the cell has one
    RemoveHyperlink(ACell);

    // Removes the formula if the cell has one
    DeleteFormula(ACell);

    if AKeepFormat then
      ACell^.ContentType := cctEmpty
    else
      // Erase all cell content
      InitCell(nil, r, c, ACell^);

    ChangedCell(r, c);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Exchanges two cells

  @param  ARow1   Row index of the first cell
  @param  ACol1   Column index of the first cell
  @param  ARow2   Row index of the second cell
  @param  ACol2   Column index of the second cell

  @note          This method does not take care of merged cells and does not
                 check for this situation. Therefore, the method is not public!
-------------------------------------------------------------------------------}
procedure TsWorksheet.ExchangeCells(ARow1, ACol1, ARow2, ACol2: Cardinal);
begin
  FCells.Exchange(ARow1, ACol1, ARow2, ACol2);
  FComments.Exchange(ARow1, ACol1, ARow2, ACol2);
  FHyperlinks.Exchange(ARow1, ACol1, ARow2, ACol2);
end;

{@@ ----------------------------------------------------------------------------
  Adds a new cell at a specified row and column index to the Cells list.

  NOTE: It is not checked if there exists already another cell at this location.
  This case must be avoided. USE CAREFULLY WITHOUT FindCell
  (e.g., during reading into empty worksheets).
-------------------------------------------------------------------------------}
function TsWorksheet.AddCell(ARow, ACol: Cardinal): PCell;
var
  fmtIndex: Integer;
begin
  Result := Cells.AddCell(ARow, ACol);

  fmtIndex := GetRowFormatIndex(ARow);
  if fmtIndex = 0 then
    fmtIndex := GetColFormatIndex(ACol);
  Result^.FormatIndex := fmtIndex;

  if FFirstColIndex = UNASSIGNED_ROW_COL_INDEX then
    FFirstColIndex := GetFirstColIndex(true) else
    FFirstColIndex := Min(FFirstColIndex, ACol);

  if FFirstRowIndex = UNASSIGNED_ROW_COL_INDEX then
    FFirstRowIndex := GetFirstRowIndex(true) else
    FFirstRowIndex := Min(FFirstRowIndex, ARow);

  if FLastColIndex = UNASSIGNED_ROW_COL_INDEX then
    FLastColIndex := GetLastColIndex(true) else
    FLastColIndex := Max(FLastColIndex, ACol);

  if FLastRowIndex = UNASSIGNED_ROW_COL_INDEX then
    FLastRowIndex := GetLastRowIndex(true) else
    FLastRowIndex := Max(FLastRowIndex, ARow);
end;

{@@ ----------------------------------------------------------------------------
  Tries to locate a Cell in the list of already written Cells

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @return Pointer to the cell if found, or nil if not found
  @see    TCell
-------------------------------------------------------------------------------}
function TsWorksheet.FindCell(ARow, ACol: Cardinal): PCell;
begin
  Result := PCell(FCells.FindByRowCol(ARow, ACol));
end;

{@@ ----------------------------------------------------------------------------
  Tries to locate a cell in the list of already written cells

  @param  AddressStr  Address of the cell in Excel A1 notation
  @return Pointer to the cell if found, or nil if not found
  @see    TCell
-------------------------------------------------------------------------------}
function TsWorksheet.FindCell(AddressStr: String): PCell;
var
  r, c: Cardinal;
begin
  if ParseCellString(AddressStr, r, c) then
    Result := FindCell(r, c)
  else
    Result := nil;
end;

function TsWorksheet.FindNextCellInCol(ARow, ACol: Cardinal): PCell;
var
  last: Cardinal;
begin
  last := GetLastRowIndex;
  if ARow = last then
    Result := nil
  else
    repeat
      inc(ARow);
      Result := FindCell(ARow, ACol);
    until (Result <> nil) or (ARow = last);
end;

function TsWorksheet.FindNextCellInRow(ARow, ACol: Cardinal): PCell;
var
  last: Cardinal;
begin
  last := GetLastColIndex;
  if ACol = last then
    Result := nil
  else
    Repeat
      inc(ACol);
      Result := Findcell(ARow, ACol);
    until (Result <> nil) or (ACol = last);
end;

function TsWorksheet.FindPrevCellInCol(ARow, ACol: Cardinal): PCell;
begin
  if ARow = 0 then
    Result := nil
  else
    repeat
      dec(ARow);
      Result := FindCell(ARow, ACol);
    until (Result <> nil) or (ARow = 0);
end;

function TsWorksheet.FindPrevCellInRow(ARow, ACol: Cardinal): PCell;
begin
  if ACol = 0 then
    Result := nil
  else
    repeat
      dec(ACol);
      Result := FindCell(ARow, ACol);
    until (Result <> nil) or (ACol = 0);
end;

{@@ ----------------------------------------------------------------------------
  Obtains an allocated cell at the desired location.

  If the cell already exists, a pointer to it will be returned.

  If not, then new memory for the cell will be allocated, a pointer to it
  will be returned and it will be added to the list of cells.

  @param  ARow      Row index of the cell
  @param  ACol      Column index of the cell

  @return A pointer to the cell at the desired location.

  @see    TCell
-------------------------------------------------------------------------------}
function TsWorksheet.GetCell(ARow, ACol: Cardinal): PCell;
begin
  Result := Cells.FindCell(ARow, ACol);
  if Result = nil then
    Result := AddCell(ARow, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Obtains an allocated cell at the desired location.

  If the Cell already exists, a pointer to it will be returned.

  If not, then new memory for the cell will be allocated, a pointer to it
  will be returned and it will be added to the list of cells.

  @param  AddressStr  Address of the cell in Excel A1 notation (an exception is
                      raised in case on an invalid cell address).
  @return A pointer to the cell at the desired location.

  @see    TCell
-------------------------------------------------------------------------------}
function TsWorksheet.GetCell(AddressStr: String): PCell;
var
  r, c: Cardinal;
begin
  if ParseCellString(AddressStr, r, c) then
    Result := GetCell(r, c)
  else
    raise EFPSpreadsheet.CreateFmt(rsNoValidCellAddress, [AddressStr]);
end;

{@@ ----------------------------------------------------------------------------
  Returns the number of cells in the worksheet with contents.

  @return The number of cells with contents in the worksheet
-------------------------------------------------------------------------------}
function TsWorksheet.GetCellCount: Cardinal;
begin
  Result := FCells.Count;
end;

{@@ ----------------------------------------------------------------------------
  Determines the number of decimals displayed for the number in the cell

  @param  ACell            Pointer to the cell under investigation
  @return Number of decimals places used in the string display of the cell.
-------------------------------------------------------------------------------}
function TsWorksheet.GetDisplayedDecimals(ACell: PCell): Byte;
var
  i, p: Integer;
  s: String;
begin
  Result := 0;
  if (ACell <> nil) and (ACell^.ContentType = cctNumber) then
  begin
    s := ReadAsText(ACell);
    p := pos(Workbook.FormatSettings.DecimalSeparator, s);
    if p > 0 then
    begin
      i := p+1;
      while (i <= Length(s)) and (s[i] in ['0'..'9']) do inc(i);
      Result := i - (p+1);
    end;
  end;
end;


{@@ ----------------------------------------------------------------------------
  Returns the 0-based index of the first column with a cell with contents.

  If no cells have contents, zero will be returned, which is also a valid value.

  Use GetCellCount to verify if there is at least one cell with contents in the
  worksheet.

  @param  AForceCalculation  The index of the first column is continuously updated
                             whenever a new cell is created. If AForceCalculation
                             is true all cells are scanned to determine the index
                             of the first column.
  @see GetCellCount
-------------------------------------------------------------------------------}
function TsWorksheet.GetFirstColIndex(AForceCalculation: Boolean = false): Cardinal;
var
  cell: PCell;
  i: Integer;
begin
  if AForceCalculation then
  begin
    Result := UNASSIGNED_ROW_COL_INDEX;
    for cell in FCells do
      Result := Math.Min(Result, cell^.Col);
    // In addition, there may be column records defining the column width even
    // without content
    for i:=0 to FCols.Count-1 do
      if FCols[i] <> nil then
        Result := Math.Min(Result, PCol(FCols[i])^.Col);
    // Store the result
    FFirstColIndex := Result;
  end
  else
  begin
    Result := FFirstColIndex;
    if Result = UNASSIGNED_ROW_COL_INDEX then
      Result := GetFirstColIndex(true);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns the 0-based index of the last column containing a cell with a
  column record (due to content or formatting), or containing a Col record.

  If no cells have contents or there are no column records, zero will be
  returned, which is also a valid value.

  Use GetCellCount to verify if there is at least one cell with contents in the
  worksheet.

  @param  AForceCalculation  The index of the last column is continuously updated
                             whenever a new cell is created. If AForceCalculation
                             is true all cells are scanned to determine the index
                             of the last column.
  @see GetCellCount
  @see GetLastOccupiedColIndex
-------------------------------------------------------------------------------}
function TsWorksheet.GetLastColIndex(AForceCalculation: Boolean = false): Cardinal;
var
  i: Integer;
begin
  if AForceCalculation or (FLastColIndex = UNASSIGNED_ROW_COL_INDEX) then
  begin
    // Traverse the tree from lowest to highest.
    // Since tree primary sort order is on row highest col could exist anywhere.
    Result := GetLastOccupiedColIndex;
    // In addition, there may be column records defining the column width even
    // without cells
    for i:=0 to FCols.Count-1 do
      if FCols[i] <> nil then
        Result := Math.Max(Result, PCol(FCols[i])^.Col);
    // Store the result
    FLastColIndex := Result;
  end
  else
    Result := FLastColIndex;
end;

{@@ ----------------------------------------------------------------------------
  Deprecated, use GetLastColIndex instead

  @see GetLastColIndex
-------------------------------------------------------------------------------}
function TsWorksheet.GetLastColNumber: Cardinal;
begin
  Result := GetLastColIndex;
end;

{@@ ----------------------------------------------------------------------------
  Returns the 0-based index of the last column with a cell with contents.
  If no cells have contents, zero will be returned, which is also a valid value.

  Use GetCellCount to verify if there is at least one cell with contents in the
  worksheet.

  @see GetCellCount
  @see GetLastColIndex
-------------------------------------------------------------------------------}
function TsWorksheet.GetLastOccupiedColIndex: Cardinal;
var
  cell: PCell;
begin
  Result := 0;
  // Traverse the tree from lowest to highest.
  // Since tree's primary sort order is on row, highest col could exist anywhere.
  for cell in FCells do
    Result := Math.Max(Result, cell^.Col);
end;

{@@ ----------------------------------------------------------------------------
  Returns the 0-based index of the first row with a cell with data or formatting.
  If no cells have contents, -1 will be returned.

  @param  AForceCalculation  The index of the first row is continuously updated
                             whenever a new cell is created. If AForceCalculation
                             is true all cells are scanned to determine the index
                             of the first row.
  @see GetCellCount
-------------------------------------------------------------------------------}
function TsWorksheet.GetFirstRowIndex(AForceCalculation: Boolean = false): Cardinal;
var
  cell: PCell;
  i: Integer;
begin
  if AForceCalculation then
  begin
    Result := UNASSIGNED_ROW_COL_INDEX;
    cell := FCells.GetFirstCell;
    if cell <> nil then Result := cell^.Row;
    // In addition, there may be row records even for rows without cells.
    for i:=0 to FRows.Count-1 do
      if FRows[i] <> nil then
        Result := Math.Min(Result, PRow(FRows[i])^.Row);
    // Store result
    FFirstRowIndex := Result;
  end
  else
  begin
    Result := FFirstRowIndex;
    if Result = UNASSIGNED_ROW_COL_INDEX then
      Result := GetFirstRowIndex(true);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns the 0-based index of the last row with a cell with contents or with
  a ROW record.

  If no cells have contents, zero will be returned, which is also a valid value.

  Use GetCellCount to verify if there is at least one cell with contents in the
  worksheet.

  @param  AForceCalculation  The index of the last row is continuously updated
                             whenever a new cell is created. If AForceCalculation
                             is true all cells are scanned to determine the index
                             of the last row.
  @see GetCellCount
  @see GetLastOccupiedRowIndex
-------------------------------------------------------------------------------}
function TsWorksheet.GetLastRowIndex(AForceCalculation: Boolean = false): Cardinal;
var
  i: Integer;
begin
  if AForceCalculation or (FLastRowIndex = UNASSIGNED_ROW_COL_INDEX) then
  begin
    // Index of highest row with at least one existing cell
    Result := GetLastOccupiedRowIndex;
    // In addition, there may be row records even for empty rows.
    for i:=0 to FRows.Count-1 do
      if FRows[i] <> nil then
        Result := Math.Max(Result, PRow(FRows[i])^.Row);
    // Store result
    FLastRowIndex := Result;
  end
  else
    Result := FLastRowIndex
end;

{@@ ----------------------------------------------------------------------------
  Returns the 0-based index of the last row with a cell with contents.
  If no cells have contents, zero will be returned, which is also a valid value.

  Use GetCellCount to verify if there is at least one cell with contents in the
  worksheet.

  @see GetCellCount
  @see GetLastRowIndex
-------------------------------------------------------------------------------}
function TsWorksheet.GetLastOccupiedRowIndex: Cardinal;
var
  cell: PCell;
begin
  Result := 0;
  cell := FCells.GetLastCell;
  if Assigned(cell) then
    Result := cell^.Row;
end;

{@@ ----------------------------------------------------------------------------
  Deprecated, use GetLastColIndex instead

  @see GetLastColIndex
-------------------------------------------------------------------------------}
function TsWorksheet.GetLastRowNumber: Cardinal;
begin
  Result := GetLastRowIndex;
end;

{@@ ----------------------------------------------------------------------------
  Reads the contents of a cell and returns an user readable text
  representing the contents of the cell.

  The resulting string is UTF-8 encoded.

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @return The text representation of the cell
-------------------------------------------------------------------------------}
function TsWorksheet.ReadAsText(ARow, ACol: Cardinal): string;
var
  cell: PCell;
begin
  cell := FindCell(ARow, ACol);
  if cell <> nil then Result := ReadAsText(cell) else Result := '';
  { avoid creating a blenk cell if the cell does not exist
  Result := ReadAsText(GetCell(ARow, ACol));              }
end;

function TsWorksheet.ReadAsUTF8Text(ARow, ACol: Cardinal): string;
begin
  Result := ReadAsText(ARow, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Reads the contents of a cell and returns an user readable text
  representing the contents of the cell.

  The resulting string is UTF-8 encoded.

  @param  ACell     Pointer to the cell
  @return The text representation of the cell
-------------------------------------------------------------------------------}
function TsWorksheet.ReadAsText(ACell: PCell): string;
begin
  Result := ReadAsText(ACell, FWorkbook.FormatSettings);
end;

function TsWorksheet.ReadAsUTF8Text(ACell: PCell): string;
begin
  Result := ReadAsText(ACell, FWorkbook.FormatSettings);
end;

{@@ ----------------------------------------------------------------------------
  Reads the contents of a cell and returns an user readable text
  representing the contents of the cell.

  The resulting string is UTF-8 encoded.

  @param  ACell            Pointer to the cell
  @param  AFormatSettings  Format settings to be used for string conversion
                           of numbers and date/times.
  @return The text representation of the cell
-------------------------------------------------------------------------------}
function TsWorksheet.ReadAsText(ACell: PCell;
  AFormatSettings: TFormatSettings): string;
var
  fmt: PsCellFormat;
  hyperlink: PsHyperlink;
  numFmt: TsNumFormatParams;
  nf: TsNumberFormat;
  nfs: String;

begin
  Result := '';
  if ACell = nil then
    Exit;

  fmt := Workbook.GetPointerToCellFormat(ACell^.FormatIndex);
  numFmt := Workbook.GetNumberFormat(fmt^.NumberFormatIndex);

  with ACell^ do
  begin
    case ContentType of
      cctUTF8String:
        Result := UTF8StringValue;

      cctNumber:
        Result := ConvertFloatToStr(NumberValue, numFmt, AFormatSettings);

      cctDateTime:
        if Assigned(numFmt) then
          Result := ConvertFloatToStr(DateTimeValue, numFmt, AFormatSettings)
        else
        if not IsNaN(DateTimeValue) then
        begin
          if frac(DateTimeValue) = 0 then  // date only
            nf := nfShortDate
          else
          if trunc(DateTimeValue) = 0 then  // time only
            nf := nfLongTime
          else
            nf := nfShortDateTime;
          nfs := BuildDateTimeFormatString(nf, AFormatSettings);
          Result := FormatDateTime(nfs, DateTimeValue, AFormatSettings);
        end;

      cctBool:
        Result := StrUtils.IfThen(BoolValue, STR_TRUE, STR_FALSE);

      cctError:
        Result := GetErrorValueStr(TsErrorValue(ErrorValue));
    end;

    if Result = '' then    // blank --> display hyperlink target if available
      if HasHyperlink(ACell) then
      begin
        hyperlink := FindHyperlink(ACell);
        if hyperlink <> nil then Result := hyperlink^.Target;
      end;
  end;
end;

function TsWorksheet.ReadAsUTF8Text(ACell: PCell;
  AFormatSettings: TFormatSettings): string;
begin
  Result := ReadAsText(ACell, AFormatSettings);
end;

{@@ ----------------------------------------------------------------------------
  Returns the value of a cell as a number.

  If the cell contains a date/time value its serial value is returned
  (as FPC TDateTime).

  If the cell contains a text value it is attempted to convert it to a number.

  If the cell is empty or its contents cannot be represented as a number the
  value 0.0 is returned.

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @return Floating-point value representing the cell contents, or 0.0 if cell
          does not exist or its contents cannot be converted to a number.
-------------------------------------------------------------------------------}
function TsWorksheet.ReadAsNumber(ARow, ACol: Cardinal): Double;
begin
  Result := ReadAsNumber(FindCell(ARow, ACol));
end;

{@@ ----------------------------------------------------------------------------
  Returns the value of a cell as a number.

  If the cell contains a date/time value its serial value is returned
  (as FPC TDateTime).

  If the cell contains a text value it is attempted to convert it to a number.

  If the cell is empty or its contents cannot be represented as a number the
  value NaN is returned.

  @param  ACell     Pointer to the cell
  @return Floating-point value representing the cell contents, or NaN if cell
          does not exist or its contents cannot be converted to a number.
-------------------------------------------------------------------------------}
function TsWorksheet.ReadAsNumber(ACell: PCell): Double;
begin
  Result := NaN;
  if ACell = nil then
    exit;

  case ACell^.ContentType of
    cctDateTime:
      Result := ACell^.DateTimeValue; //this is in FPC TDateTime format, not Excel
    cctNumber:
      Result := ACell^.NumberValue;
    cctUTF8String:
      if not TryStrToFloat(ACell^.UTF8StringValue, Result, FWorkbook.FormatSettings)
        then Result := NaN;
    cctBool:
      if ACell^.BoolValue then Result := 1.0 else Result := 0.0;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Reads the contents of a cell and returns the date/time value of the cell.

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @param  AResult   Date/time value of the cell (or 0.0, if no date/time cell)
  @return True if the cell is a datetime value, false otherwise
-------------------------------------------------------------------------------}
function TsWorksheet.ReadAsDateTime(ARow, ACol: Cardinal;
  out AResult: TDateTime): Boolean;
begin
  Result := ReadAsDateTime(FindCell(ARow, ACol), AResult);
end;

{@@ ----------------------------------------------------------------------------
  Reads the contents of a cell and returns the date/time value of the cell.

  @param  ACell     Pointer to the cell
  @param  AResult   Date/time value of the cell (or 0.0, if no date/time cell)
  @return True if the cell is a datetime value, false otherwise
-------------------------------------------------------------------------------}
function TsWorksheet.ReadAsDateTime(ACell: PCell;
  out AResult: TDateTime): Boolean;
begin
  if (ACell = nil) or (ACell^.ContentType <> cctDateTime) then
  begin
    AResult := 0;
    Result := False;
    Exit;
  end;

  AResult := ACell^.DateTimeValue;
  Result := True;
end;

{@@ ----------------------------------------------------------------------------
  If a cell contains a formula (string formula or RPN formula) the formula
  is returned as a string in Excel syntax.

  @param   ACell      Pointer to the cell considered
  @param   ALocalized If true, the formula is returned with decimal and list
                      separators accoding to the workbook's FormatSettings.
                      Otherwise it uses dot and comma, respectively.
  @return  Formula string in Excel syntax (does not contain a leading "=")
-------------------------------------------------------------------------------}
function TsWorksheet.ReadFormulaAsString(ACell: PCell;
  ALocalized: Boolean = false): String;
var
  formula: PsFormula;
begin
  Result := '';
  if ACell = nil then
    exit;
  if HasFormula(ACell) then begin
    formula := FFormulas.FindFormula(ACell^.Row, ACell^.Col);
    if ALocalized then
      Result := formula^.Parser.Expression[fdLocalized]
    else
      Result := formula^.Parser.Expression[fdExcelA1];
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns to numeric equivalent of the cell contents. This is the NumberValue
  of a number cell, the DateTimeValue of a date/time cell, the ordinal BoolValue
  of a boolean cell, or the string converted to a number of a string cell.
  All other cases return NaN.

  @param   ACell   Cell to be considered
  @param   AValue  (output) extracted numeric value
  @return  True if conversion to number is successful, otherwise false
-------------------------------------------------------------------------------}
function TsWorksheet.ReadNumericValue(ACell: PCell; out AValue: Double): Boolean;
begin
  AValue := NaN;
  if ACell <> nil then begin
    Result := True;
    case ACell^.ContentType of
      cctNumber:
        AValue := ACell^.NumberValue;
      cctDateTime:
        AValue := ACell^.DateTimeValue;
      cctBool:
        AValue := ord(ACell^.BoolValue);
      else
        if (ACell^.ContentType <> cctUTF8String) or
           not TryStrToFloat(ACell^.UTF8StringValue, AValue) or
           not TryStrToDateTime(ACell^.UTF8StringValue, AValue)
        then
          Result := False;
      end;
  end else
    Result := False;
end;

function TsWorksheet.ConvertFormulaDialect(ACell: PCell;
  ADialect: TsFormulaDialect): String;
var
  formula: PsFormula;
begin
  Result := '';
  if (ACell = nil) or (not HasFormula(ACell)) then
    exit;

  formula := FFormulas.FindFormula(ACell^.Row, ACell^.Col);
  if ADialect = fdExcelR1C1 then
    Result := formula^.Parser.R1C1Expression[ACell]
  else
    Result := formula^.Parser.Expression[ADialect];
end;

{@@ ----------------------------------------------------------------------------
  Converts an RPN formula (as read from an xls biff file, for example) to a
  string formula.

  @param    AFormula  Array of rpn formula tokens
  @return   Formula string in Excel syntax (without leading "=")
-------------------------------------------------------------------------------}
function TsWorksheet.ConvertRPNFormulaToStringFormula(const AFormula: TsRPNFormula): String;
var
  parser: TsSpreadsheetParser;
begin
  Result := '';

  parser := TsSpreadsheetParser.Create(self);
  try
    parser.RPNFormula := AFormula;
    Result := parser.Expression[fdExcelA1];
  finally
    parser.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns a pointer to the formula record assigned to a cell, or nil if the
  cell has no formula
-------------------------------------------------------------------------------}
function TsWorksheet.GetFormula(ACell: PCell): PsFormula;
begin
  Result := FFormulas.FindFormula(ACell);
end;

{@@ ----------------------------------------------------------------------------
  Returns the index of the effective cell format to be used at the specified
  cell.

  "Effective" cell format means: At first, look for the cell format.
  If it is default, look for the row format. If it is default, look for
  the column format. (see "excelfileformat", p. 89)
-------------------------------------------------------------------------------}
function TsWorksheet.GetEffectiveCellFormatIndex(ARow, ACol: Cardinal): Integer;
var
  cell: PCell;
begin
  cell := FindCell(ARow, ACol);
  if (cell <> nil) then
    Result := GetEffectiveCellFormatIndex(cell)
//    Result := cell^.FormatIndex
  else
  begin
    // Col and row formats are needed explicitely only in case of empty cells.
    // Because if a cells exists the col/row format already has been copied
    // to the cell.
    Result := GetRowFormatIndex(ARow);
    if Result = 0 then
      Result := GetColFormatIndex(ACol);
  end;
end;

function TsWorksheet.GetEffectiveCellFormatIndex(ACell: PCell): Integer;
begin
  Result := 0;
  if ACell <> nil then begin
    Result := ACell^.FormatIndex;
    if Result = 0 then
      Result := GetRowFormatIndex(ACell^.Row);
    if Result = 0 then
      Result := GetColFormatIndex(ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns a pointer to the effective cell format to be used at the cell in
  ARow and ACol.

  "Effective" cell format means: At first, look for the cell format.
  If it is default, look for the row format. If it is default, look for
  the column format. (see "excelfileformat", p. 89)
-------------------------------------------------------------------------------}
function TsWorksheet.GetPointerToEffectiveCellFormat(ARow, ACol: Cardinal): PsCellFormat;
var
  cell: PCell;
  fmtIndex: Integer;
begin
  cell := FindCell(ARow, ACol);
  if (cell <> nil) then
    fmtIndex := cell^.FormatIndex
  else
  begin
    // Col and row formats are needed explicitely only in case of empty cells.
    // Because if a cells exists the col/row format already has been copied
    // to the cell.
    fmtIndex := GetRowFormatIndex(ARow);
    if fmtIndex = 0 then
      fmtIndex := GetColFormatIndex(ACol);
  end;
  Result := FWorkbook.GetPointerToCellFormat(fmtIndex);
end;
                               (*
{@@ ----------------------------------------------------------------------------
  Mainly like GetPointerToEffectiveCellFormat(ARow, ACol), but avoids looking
  for the cell if ACell <> nil
-------------------------------------------------------------------------------}
function TsWorksheet.GetPointerToEffectiveCellFormat(ACell: PCell): PsCellFormat;
var
  fmtIndex: Integer;
begin
  if (ACell <> nil) then
    fmtIndex := ACell^.FormatIndex
  else
    fmtIndex := 0;
  Result := FWorkbook.GetPointerToCellFormat(fmtIndex);
end;                             *)

{@@ ----------------------------------------------------------------------------
  Determines the font used in a specified column record.
  Returns the workbook's default font if the column record does not exist.
-------------------------------------------------------------------------------}
function TsWorksheet.ReadColFont(ACol: PCol): TsFont;
var
  fmt: PsCellFormat;
begin
  Result := nil;
  if ACol <> nil then begin
    fmt := Workbook.GetPointerToCellFormat(ACol^.FormatIndex);
    Result := Workbook.GetFont(fmt^.FontIndex);
  end;
  if Result = nil then
    Result := Workbook.GetDefaultFont;
end;

{@@ ----------------------------------------------------------------------------
  Determines the font used in a specified row record.
  Returns the workbook's default font if the row record does not exist.
-------------------------------------------------------------------------------}
function TsWorksheet.ReadRowFont(ARow: PRow): TsFont;
var
  fmt: PsCellFormat;
begin
  Result := nil;
  if ARow <> nil then
  begin
    fmt := Workbook.GetPointerToCellFormat(ARow^.FormatIndex);
    Result := Workbook.GetFont(fmt^.FontIndex);
  end;
  if Result = nil then
    Result := Workbook.GetDefaultFont;
end;


{@@ ----------------------------------------------------------------------------
  Returns true if the worksheet does not contain any cell, column or row records
-------------------------------------------------------------------------------}
function TsWorksheet.IsEmpty: Boolean;
var
  cell: PCell;
begin
  Result := false;
  for cell in Cells do
    if cell^.ContentType <> cctEmpty then
      exit;

  if (Rows.Count > 0) or (Cols.Count > 0) then
    exit;

  Result := true;
end;


{ Merged cells }

{@@ ----------------------------------------------------------------------------
  Finds the upper left cell of a merged block to which a specified cell belongs.
  This is the "merge base". Returns nil if the cell is not merged.

  @param  ACell  Cell under investigation
  @return A pointer to the cell in the upper left corner of the merged block
          to which ACell belongs.
          If ACell is isolated then the function returns nil.
-------------------------------------------------------------------------------}
function TsWorksheet.FindMergeBase(ACell: PCell): PCell;
var
  rng: PsCellRange;
begin
  Result := nil;
  if IsMerged(ACell) then
  begin
    rng := FMergedCells.FindRangeWithCell(ACell^.Row, ACell^.Col);
    if rng <> nil then
      Result := FindCell(rng^.Row1, rng^.Col1);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Merges adjacent individual cells to a larger single cell

  @param  ARow1   Row index of the upper left corner of the cell range
  @param  ACol1   Column index of the upper left corner of the cell range
  @param  ARow2   Row index of the lower right corner of the cell range
  @param  ACol2   Column index of the lower right corner of the cell range
-------------------------------------------------------------------------------}
procedure TsWorksheet.MergeCells(ARow1, ACol1, ARow2, ACol2: Cardinal);
var
  rng: PsCellRange;
  cell: PCell;
  r, c: Cardinal;
begin
  // A single cell cannot be merged
  if (ARow1 = ARow2) and (ACol1 = ACol2) then
    exit;

  // Is cell ARow1/ACol1 already the base of a merged range? ...
  rng := PsCellRange(FMergedCells.FindByRowCol(ARow1, ACol1));
  // ... no: --> Add a new merged range
  if rng = nil then
    FMergedCells.AddRange(ARow1, ACol1, ARow2, ACol2)
  else
  // ... yes: --> modify the merged range accordingly
  begin
    // unmark previously merged range
    for cell in Cells.GetRangeEnumerator(rng^.Row1, rng^.Col1, rng^.Row2, rng^.Col2) do
      Exclude(cell^.Flags, cfMerged);
    // Define new limits of merged range
    rng^.Row2 := ARow2;
    rng^.Col2 := ACol2;
  end;

  // Mark all cells in the range as "merged"
  for r := ARow1 to ARow2 do
    for c := ACol1 to ACol2 do
    begin
      cell := GetCell(r, c);   // if not existent create new cell
      Include(cell^.Flags, cfMerged);
    end;

  ChangedCell(ARow1, ACol1);
end;

{@@ ----------------------------------------------------------------------------
  Merges adjacent individual cells to a larger single cell

  @param  ARange  Cell range string given in Excel notation (e.g: A1:D5).
                  A non-range string (e.g. A1) is not allowed.
-------------------------------------------------------------------------------}
procedure TsWorksheet.MergeCells(ARange: String);
var
  r1, r2, c1, c2: Cardinal;
begin
  if ParseCellRangeString(ARange, r1, c1, r2, c2) then
    MergeCells(r1, c1, r2, c2);
end;

{@@ ----------------------------------------------------------------------------
  Disconnects merged cells to make them individual cells again.

  Input parameter is a cell which belongs to the range to be unmerged.

  @param  ARow   Row index of a cell considered to belong to the cell block
  @param  ACol   Column index of a cell considered to belong to the cell block
-------------------------------------------------------------------------------}
procedure TsWorksheet.UnmergeCells(ARow, ACol: Cardinal);
var
  rng: PsCellRange;
  cell: PCell;
begin
  rng := FMergedCells.FindRangeWithCell(ARow, ACol);
  if rng <> nil then
  begin
    // Remove the "merged" flag from the cells in the merged range to make them
    // isolated again...
    for cell in Cells.GetRangeEnumerator(rng^.Row1, rng^.Col1, rng^.Row2, rng^.Col2) do
      Exclude(cell^.Flags, cfMerged);
    // ... and delete the range
    FMergedCells.DeleteRange(rng^.Row1, rng^.Col1);
  end;

  ChangedCell(ARow, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Disconnects merged cells to make them individual cells again.

  @param  ARange  Cell (range) string given in Excel notation (e.g: A1, or A1:D5)
                  In case of a range string, only the upper left corner cell is
                  considered. It must belong to the merged range of cells to be
                  unmerged.
-------------------------------------------------------------------------------}
procedure TsWorksheet.UnmergeCells(ARange: String);
var
  sheet: TsWorksheet;
  rng: TsCellRange;
begin
  if Workbook.TryStrToCellRange(ARange, sheet, rng) then
    UnmergeCells(rng.Row1, rng.Col1);
end;

{@@ ----------------------------------------------------------------------------
  Determines the merged cell block to which a particular cell belongs

  @param   ACell  Pointer to the cell being investigated
  @param   ARow1  (output) Top row index of the merged block
  @param   ACol1  (outout) Left column index of the merged block
  @param   ARow2  (output) Bottom row index of the merged block
  @param   ACol2  (output) Right column index of the merged block

  @return  True if the cell belongs to a merged block, False if not, or if the
           cell does not exist at all.
-------------------------------------------------------------------------------}
function TsWorksheet.FindMergedRange(ACell: PCell;
  out ARow1, ACol1, ARow2, ACol2: Cardinal): Boolean;
var
  rng: PsCellRange;
begin
  if IsMerged(ACell) then
  begin
    rng := FMergedCells.FindRangeWithCell(ACell^.Row, ACell^.Col);
    if rng <> nil then
    begin
      ARow1 := rng^.Row1;
      ACol1 := rng^.Col1;
      ARow2 := rng^.Row2;
      ACol2 := rng^.Col2;
      Result := true;
      exit;
    end;
  end;
  Result := false;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the two specified cells belong to the same merged cell block.

  @param   ACell1  Pointer to the first cell
  @param   ACell2  Pointer to the second cell
  @reult   TRUE if both cells belong to the same merged cell block
           FALSE if the cells are not merged or are in different blocks
-------------------------------------------------------------------------------}
function TsWorksheet.InSameMergedRange(ACell1, ACell2: PCell): Boolean;
begin
  Result := IsMerged(ACell1) and IsMerged(ACell2) and
            (FindMergeBase(ACell1) = FindMergeBase(ACell2));
end;

{@@ ----------------------------------------------------------------------------
  Returns true if the specified cell is the base of a merged cell range, i.e.
  the upper left corner of that range.

  @param   ACell  Pointer to the cell being considered
  @return  True if the cell is the upper left corner of a merged range
           False if not
-------------------------------------------------------------------------------}
function TsWorksheet.IsMergeBase(ACell: PCell): Boolean;
begin
  Result := (ACell <> nil) and (ACell = FindMergeBase(ACell));
end;

{@@ ----------------------------------------------------------------------------
  Returns TRUE if the specified cell belongs to a merged block

  @param   ACell  Pointer to the cell of interest
  @return  TRUE if the cell belongs to a merged block, FALSE if not.
-------------------------------------------------------------------------------}
function TsWorksheet.IsMerged(ACell: PCell): Boolean;
begin
  Result := (ACell <> nil) and (cfMerged in ACell^.Flags);
end;

{@@ ----------------------------------------------------------------------------
  Deletes the formula assigned to the specified cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.DeleteFormula(ACell: PCell);
begin
  if HasFormula(ACell) and (FWorkbook.FDeleteFormulaLock = 0) then begin
    FFormulas.DeleteFormula(ACell);
    ACell^.Flags := ACell^.Flags - [cfHasFormula, cf3dFormula];
  end;
end;

{@@ ----------------------------------------------------------------------------
  Reads the formula assigned to a cell in the specified row and column
-------------------------------------------------------------------------------}
function TsWorksheet.ReadFormula(ARow, ACol: Cardinal): String;
var
  cell: PCell;
begin
  cell := FindCell(ARow, ACol);
  Result := ReadFormula(cell)
end;

{@@ ----------------------------------------------------------------------------
  Reads the formula assigned to a specified cell
-------------------------------------------------------------------------------}
function TsWorksheet.ReadFormula(ACell: PCell): String;
var
  formula: PsFormula;
begin
  Result := '';
  if ACell = nil then
    exit;

  formula := Formulas.FindFormula(ACell);
  if formula = nil then
    exit;

  Result := formula^.Text;

  if (Result = '') and (formula^.Parser <> nil) then
    Result := formula^.Parser.Expression[fdExcelA1];
end;

{@@ ----------------------------------------------------------------------------
  Uses a formula in the specified a cell
-------------------------------------------------------------------------------}
procedure TsWorksheet.UseFormulaInCell(ACell: PCell; AFormula: PsFormula);
begin
  Assert(ACell <> nil);

  if AFormula <> nil then
  begin
    AFormula^.Col := ACell^.Col;
    AFormula^.Row := ACell^.Row;

    ACell^.ContentType := cctFormula;

    ACell^.Flags := ACell^.Flags + [cfHasFormula];
    if (AFormula^.Parser <> nil) and AFormula^.Parser.Has3DLinks then
      ACell^.Flags := ACell^.Flags + [cf3dFormula];
  end else
    DeleteFormula(ACell);
end;


{@@ Assigns a hyperlink to an image. The image is specified by its index in the
  internal image list}
procedure TsWorksheet.AddHyperlinkToImage(AImageIndex: Integer; ATarget: String;
  AToolTip: String = '');
var
  img: PsImage;
begin
  img := GetPointerToImage(AImageIndex);
  if Assigned(img) then begin
    img^.HyperlinkTarget := ATarget;
    img^.HyperlinkToolTip := AToolTip;
  end;
end;


{@@ ----------------------------------------------------------------------------
  Removes a cell from its tree container. DOES NOT RELEASE ITS MEMORY!

  @param  ARow   Row index of the cell to be removed
  @param  ACol   Column index of the cell to be removed
  @return  Pointer to the cell removed
-------------------------------------------------------------------------------}
function TsWorksheet.RemoveCell(ARow, ACol: Cardinal): PCell;
begin
  Result := PCell(FCells.FindByRowCol(ARow, ACol));
  if Result <> nil then FCells.Remove(Result);
end;

{@@ ----------------------------------------------------------------------------
  Removes a cell and releases its memory. If a comment is attached to the
  cell then it is removed and releaded as well.

  Just for internal usage since it does not modify the other cells affected.
  And it does not change other records depending on the cell (comments,
  merged ranges etc).

  @param  ARow   Row index of the cell to be removed
  @param  ACol   Column index of the cell to be removed
-------------------------------------------------------------------------------}
procedure TsWorksheet.RemoveAndFreeCell(ARow, ACol: Cardinal);
begin
  FCells.DeleteCell(ARow, ACol);
end;

procedure TsWorksheet.SetBiDiMode(AValue: TsBiDiMode);
begin
  if AValue = FBiDiMode then
    exit;
  FBiDiMode := AValue;
  FWorkbook.ChangedWorksheet(Self);
end;

{@@ ----------------------------------------------------------------------------
  Enables (or disables) protection of the worksheet. Details of protection are
  specified in the set of Sheetprotection options
-------------------------------------------------------------------------------}
procedure TsWorksheet.Protect(AEnable: Boolean);
begin
  if AEnable then
    Include(FOptions, soProtected) else
    Exclude(FOptions, soProtected);
  FWorkbook.ChangedWorksheet(self);
end;

{@@ ----------------------------------------------------------------------------
  Hides the worksheet. Makes sure that the last worksheet cannot be hidden.
  Notifies visual controls
-------------------------------------------------------------------------------}
procedure TsWorksheet.Hide;
var
  idx, n: Integer;
  sheet: TsWorksheet;
begin
  if IsHidden then
    exit;
  if FWorkbook.GetVisibleWorksheetCount = 1 then
    exit;
  Options := Options + [soHidden];
  FWorkbook.ChangedWorksheet(self);
  if (FWorkbook.ActiveWorksheet = self) then begin
    n := FWorkbook.GetWorksheetCount;
    idx := FWorkbook.GetWorksheetIndex(self) + 1;
    if idx < n then begin
      sheet := FWorkbook.GetWorksheetByIndex(idx);
      while Assigned(sheet) and sheet.IsHidden do begin
        inc(idx);
        sheet := FWorkbook.GetWorksheetByIndex(idx);
      end;
      if sheet <> nil then begin
        FWorkbook.SelectWorksheet(sheet);
        exit;
      end;
    end;
    idx := FWorkbook.GetWorkSheetIndex(self) - 1;
    if idx >= 0 then begin
      sheet := FWorkbook.GetWorksheetByIndex(idx);
      while Assigned(sheet) and sheet.IsHidden do begin
        dec(idx);
        sheet := FWorkbook.GetWorksheetByIndex(idx);
      end;
      if sheet <> nil then begin
        FWorkbook.SelectWorksheet(sheet);
        exit;
      end;
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Shows the worksheet if is was previously hidden
  Useful for visual controls
-------------------------------------------------------------------------------}
procedure TsWorksheet.Show;
begin
  if not (soHidden in Options) then
    exit;
  Options := Options - [soHidden];
  FWorkbook.ChangedWorksheet(self);
end;

{@@ ----------------------------------------------------------------------------
  Returns TRUE if the worksheet is hidden
-------------------------------------------------------------------------------}
function TsWorksheet.IsHidden: Boolean;
begin
  Result := soHidden in Options;
end;

{@@ ----------------------------------------------------------------------------
  Setter for the worksheet name property. Checks if the name is valid, and
  exits without any change if not. Creates an event OnChangeWorksheet.
-------------------------------------------------------------------------------}
procedure TsWorksheet.SetName(const AName: String);
begin
  if AName = FName then
    exit;
  if (FWorkbook <> nil) then //and FWorkbook.ValidWorksheetName(AName) then
  begin
    FName := AName;
    if FWorkbook.FReadWriteFlag = rwfNormal then begin
      FWorkbook.RebuildFormulas;
      if (FWorkbook.FNotificationLock = 0) and Assigned(FWorkbook.FOnRenameWorksheet) then
        FWorkbook.FOnRenameWorksheet(FWorkbook, self);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Compare function for sorting of rows and columns called directly by Sort()
  The compare algorithm starts with the first key parameters. If cells are
  found to be "equal" the next parameter is set is used until a difference is
  found, or all parameters are used.

  @param   ARow1         Row index of the first cell to be compared
  @param   ACol1         Column index of the first cell to be compared
  @param   ARow2         Row index of the second cell to be compared
  @parem   ACol2         Column index of the second cell to be compared
  @param   ASortOptions  Sorting options: case-insensitive and/or descending
  @return  -1 if the first cell is "smaller", i.e. is sorted in front of the
              second one
           +1 if the first cell is "larger", i.e. is behind the second one
           0  if both cells are equal
------------------------------------------------------------------------------- }
function TsWorksheet.DoCompareCells(AColRow1, AColRow2: Cardinal): Integer;
var
  cell1, cell2: PCell;  // Pointers to the cells to be compared
  key: Integer;
begin
  Result := 0;
  key := 0;
  while (Result = 0) and (key <= High(FSortParams.Keys)) do
  begin
    if FSortParams.SortByCols then
    begin
      cell1 := FindCell(AColRow1, FSortParams.Keys[key].ColRowIndex);
      cell2 := FindCell(AColRow2, FSortParams.Keys[key].ColRowIndex);
    end else
    begin
      cell1 := FindCell(FSortParams.Keys[key].ColRowIndex, AColRow1);
      cell2 := FindCell(FSortParams.Keys[key].ColRowIndex, AColRow2);
    end;
    if Assigned(FOnFullCompareCells) then
      FOnFullCompareCells(Self, cell1, cell2, FSortParams.Keys[Key], Result)
    else if Assigned(FOnCompareCells) then
      FOnCompareCells(Self, cell1, cell2, Result)
    else
      Result := DefaultCompareCells(cell1, cell2, FSortParams.Keys[key]);
    inc(key);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Compare function for sorting of rows and columns. Called by DoCompareCells.

  @param    ACell1        Pointer to the first cell of the comparison
  @param    ACell2        Pointer to the second cell of the comparison
  @param    ASortKey      Sorting criteria: sorted column/row, descending,
                          case-insensitive, numbers first, etc.
  @return   -1 if the first cell is "smaller"
            +1 if the first cell is "larger",
            0 if both cells are "equal"

            Date/time and boolean cells are sorted like number cells according
            to their number value
            Label cells are sorted as UTF8 strings.

            In case of mixed cell content types the order is determined by
            the parameter Priority of the SortParams.
            Empty cells are always at the end (in both ascending and descending
            order)
-------------------------------------------------------------------------------}
function TsWorksheet.DefaultCompareCells(ACell1, ACell2: PCell;
  ASortKey: TsSortKey): Integer;
// Sort priority in Excel:
// numbers < alpha < blank (ascending)
// alpha < numbers < blank (descending)
var
  number1, number2: Double;
begin
  Result := 0;

  if (ACell1 = nil) or (ACell1^.ContentType = cctEmpty)
  then begin
    if (ACell2 <> nil) and (ACell2^.ContentType <> cctEmpty) then
      Result := +1; // Empty cells go to the end

    Exit; // Avoid SortOrder to bring the empty cell to the top!
  end;

  if (ACell2 = nil) or (ACell2^.ContentType = cctEmpty) then
  begin
    Result := -1;   // Empty cells go to the end
    exit;           // Avoid SortOrder to bring the empty cell to the top!
  end;

  if (ACell1^.ContentType = cctUTF8String) then begin
    if (ACell2^.ContentType = cctUTF8String) then
    begin
      if ssoCaseInsensitive in ASortKey.Options then
        Result := AnsiCompareText(ACell1^.UTF8StringValue, ACell2^.UTF8StringValue)
      else
        Result := AnsiCompareStr(ACell1^.UTF8StringValue, ACell2^.UTF8StringValue);
    end else
    begin
      if ssoAlphaBeforeNum in ASortKey.Options then
        Result := -1
      else
        Result := 1;
    end;

  end else
  begin
    if (ACell2^.ContentType = cctUTF8String) then
    begin
      if ssoAlphaBeforeNum in ASortKey.Options then
        Result := +1
      else
        Result := -1;
    end else
    begin
      ReadNumericValue(ACell1, number1);
      ReadNumericValue(ACell2, number2);
      Result := CompareValue(number1, number2);
    end;
  end;

  if ssoDescending in ASortKey.Options then
    Result := -Result;
end;

{@@ ----------------------------------------------------------------------------
  Exchanges columns or rows, depending on value of "AIsColumn"

  @param  AIsColumn   if true the exchange is done for columns, otherwise for rows
  @param  AIndex      Index of the column (if AIsColumn is true) or the row
                      (if AIsColumn is false) which is to be exchanged with the
                      one having index "WidthIndex"
  @param  WithIndex   Index of the column (if AIsColumn is true) or the row
                      (if AIsColumn is false) with which "AIndex" is to be
                      replaced.
  @param  AFromIndex  First row (if AIsColumn is true) or column (if AIsColumn
                      is false) which is affected by the exchange
  @param  AToIndex    Last row (if AIsColumn is true) or column (if AsColumn is
                      false) which is affected by the exchange
-------------------------------------------------------------------------------}
procedure TsWorksheet.DoExchangeColRow(AIsColumn: Boolean;
  AIndex, WithIndex: Cardinal; AFromIndex, AToIndex: Cardinal);
var
  r, c: Cardinal;
begin
  if AIsColumn then
    for r := AFromIndex to AToIndex do
      ExchangeCells(r, AIndex, r, WithIndex)
  else
    for c := AFromIndex to AToIndex do
      ExchangeCells(AIndex, c, WithIndex, c);
end;

{@@ ----------------------------------------------------------------------------
  Sorts a range of cells defined by the cell rectangle from ARowFrom/AColFrom
  to ARowTo/AColTo according to the parameters specified in ASortParams

  @param  ASortParams   Set of parameters to define sorting along rows or colums,
                        the sorting key column or row indexes, and the sorting
                        directions
  @param  ARange        Cell range to be sorted, in Excel notation, such as 'A1:C8'
-------------------------------------------------------------------------------}
procedure TsWorksheet.Sort(ASortParams: TsSortParams; ARange: String);
var
  r1,c1, r2,c2: Cardinal;
begin
  if ParseCellRangeString(ARange, r1, c1, r2, c2) then
    Sort(ASortParams, r1, c1, r2, c2)
  else
    raise EFPSpreadsheet.CreateFmt(rsNoValidCellRangeAddress, [ARange]);
end;

{@@ ----------------------------------------------------------------------------
  Sorts a range of cells defined by the cell rectangle from ARowFrom/AColFrom
  to ARowTo/AColTo according to the parameters specified in ASortParams

  @param  ASortParams   Set of parameters to define sorting along rows or colums,
                        the sorting key column or row indexes, and the sorting
                        directions
  @param  ARowFrom      Top row of the range to be sorted
  @param  AColFrom      Left column of the range to be sorted
  @param  ARowTo        Last row of the range to be sorted
  @param  AColTo        Right column of the range to be sorted
-------------------------------------------------------------------------------}
procedure TsWorksheet.Sort(const ASortParams: TsSortParams;
  ARowFrom, AColFrom, ARowTo, AColTo: Cardinal);
// code "borrowed" from grids.pas and adapted to multi-key sorting

  procedure QuickSort(L,R: Integer);
  var
    I,J: Integer;
    P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) div 2;
      repeat
        if ASortParams.SortByCols then
        begin
          while DoCompareCells(P, I) > 0 do inc(I);
          while DoCompareCells(P, J) < 0 do dec(J);
        end else
        begin
          while DoCompareCells(P, I) > 0 do inc(I);
          while DoCompareCells(P, J) < 0 do dec(J);
        end;

        if I <= J then
        begin
          if I <> J then
          begin
            if ASortParams.SortByCols then
            begin
              if DoCompareCells(I, J) <> 0 then
                DoExchangeColRow(not ASortParams.SortByCols, J,I, AColFrom, AColTo);
            end else
            begin
              if DoCompareCells(I, J) <> 0 then
                DoExchangeColRow(not ASortParams.SortByCols, J,I, ARowFrom, ARowTo);
            end;
          end;

          if P = I then
            P := J
          else
          if P = J then
            P := I;

          inc(I);
          dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);

      L := I;
    until I >= R;
  end;

  function ContainsMergedCells: boolean;
  var
    cell: PCell;
  begin
    result := false;
    for cell in Cells.GetRangeEnumerator(ARowFrom, AColFrom, ARowTo, AColTo) do
      if IsMerged(cell) then
        exit(true);
  end;

begin
  if ContainsMergedCells then
    raise EFPSpreadsheet.Create(rsCannotSortMerged);

  FSortParams := ASortParams;
  if ASortParams.SortByCols then
    QuickSort(ARowFrom, ARowTo)
  else
    QuickSort(AColFrom, AColTo);
  ChangedCell(ARowFrom, AColFrom);
end;

{@@ ----------------------------------------------------------------------------
  Marks a specified cell as "selected". Only needed by the visual controls.
-------------------------------------------------------------------------------}
procedure TsWorksheet.SelectCell(ARow, ACol: Cardinal);
var
  cell: PCell;
begin
  // Avoid selecting a non-base cell of a merged block.
  cell := FindCell(ARow, ACol);
  if Assigned(cell) then
  begin
    if IsMerged(cell) then
      cell := FindMergeBase(cell);
    ACol := cell^.Col;
  end;

  FActiveCellRow := ARow;
  FActiveCellCol := ACol;
  if FWorkbook.NotificationsEnabled and Assigned(FOnSelectCell) then
    FOnSelectCell(Self, ARow, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Clears the list of seleccted cell ranges
  Only needed by the visual controls.
-------------------------------------------------------------------------------}
procedure TsWorksheet.ClearSelection;
begin
  SetLength(FSelection, 0);
end;

{@@ ----------------------------------------------------------------------------
  Deletes all selected cells (delete = make them empty)
-------------------------------------------------------------------------------}
procedure TsWorksheet.DeleteSelection;
var
  i: Integer;
  r, c: Cardinal;
  cell: PCell;
begin
  for i:=0 to High(FSelection) do
    for r := FSelection[i].Row1 to FSelection[i].Row2 do
      for c := FSelection[i].Col1 to FSelection[i].Col2 do
      begin
        cell := FindCell(r, c);
        DeleteCell(cell);
      end;
  ClearSelection;
end;

{@@ ----------------------------------------------------------------------------
  Erases all selected cells (erase = keep cell, but delete content)
  If AKeepFormat is true the cell format is left unchanged.
-------------------------------------------------------------------------------}
procedure TsWorksheet.EraseSelection(AKeepFormat: Boolean = false);
var
  i: Integer;
  r, c: Cardinal;
  cell: PCell;
begin
  for i:=0 to High(FSelection) do
    for r := FSelection[i].Row1 to FSelection[i].Row2 do
      for c := FSelection[i].Col1 to FSelection[i].Col2 do
      begin
        cell := FindCell(r, c);
        EraseCell(cell, AKeepFormat);
      end;
  ClearSelection;
end;


{@@ ----------------------------------------------------------------------------
  Returns the list of selected cell ranges
-------------------------------------------------------------------------------}
function TsWorksheet.GetSelection: TsCellRangeArray;
var
  i: Integer;
begin
  SetLength(Result{%H-}, Length(FSelection));
  for i:=0 to High(FSelection) do
    Result[i] := FSelection[i];
end;

{@@ ----------------------------------------------------------------------------
  Returns all selection ranges as an Excel string
-------------------------------------------------------------------------------}
function TsWorksheet.GetSelectionAsString: String;
const
  RELATIVE = [rfRelRow, rfRelCol, rfRelRow2, rfRelCol2];
var
  i: Integer;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    for i:=0 to Length(FSelection)-1 do
      with FSelection[i] do
        L.Add(GetCellRangeString(Row1, Col1, Row2, Col2, RELATIVE, true));
    L.Delimiter := DefaultFormatSettings.ListSeparator;
    L.StrictDelimiter := true;
    Result := L.DelimitedText;
  finally
    L.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns the number of selected cell ranges
-------------------------------------------------------------------------------}
function TsWorksheet.GetSelectionCount: Integer;
begin
  Result := Length(FSelection);
end;

{@@ ----------------------------------------------------------------------------
  Returns the index of the selected block which contains the active cell
-------------------------------------------------------------------------------}
function TsWorksheet.GetSelectionRangeIndexOfActiveCell: Integer;
var
  sel: TsCellRange;
begin
  for Result := 0 to High(FSelection) do
  begin
    sel := FSelection[Result];
    if (FActiveCellRow >= sel.Row1) and (FActiveCellRow <= sel.Row2) and
       (FActiveCellCol >= sel.Col1) and (FActiveCellCol <= sel.Col2) then exit;
  end;
  Result := -1;
end;

{@@ ----------------------------------------------------------------------------
  Marks an array of cell ranges as "selected". Only needed for visual controls
-------------------------------------------------------------------------------}
procedure TsWorksheet.SetSelection(const ASelection: TsCellRangeArray);
var
  i: Integer;
begin
  SetLength(FSelection, Length(ASelection));
  for i:=0 to High(FSelection) do
    FSelection[i] := ASelection[i];
end;

{@@ ----------------------------------------------------------------------------
  Uses the passed parameters a TopRow and LeftCol. These are used by the
  TsWorksheetGrid to scroll the visible grid such that the corresponding cell
  is at the top/left.
-------------------------------------------------------------------------------}
procedure TsWorksheet.ScrollTo(ANewTopRow, ANewLeftCol: Cardinal);
begin
  FTopRow := ANewTopRow;
  FLeftCol := ANewLeftCol;
end;

{@@ ----------------------------------------------------------------------------
  Helper method to update internal caching variables
-------------------------------------------------------------------------------}
procedure TsWorksheet.UpdateCaches;
begin
  FFirstColIndex := GetFirstColIndex(true);
  FFirstRowIndex := GetFirstRowIndex(true);
  FLastColIndex := GetLastColIndex(true);
  FLastRowIndex := GetLastRowIndex(true);
end;

{@@ ----------------------------------------------------------------------------
  Setter method for the count of columns to be written in VirtualMode
-------------------------------------------------------------------------------}
procedure TsWorksheet.SetVirtualColCount(AValue: Cardinal);
begin
  if FWorkbook.FReadWriteFlag = rwfWrite then exit;
  FVirtualColCount := AValue;
end;

{@@ ----------------------------------------------------------------------------
  Setter method for the count of rows to be written in VirtualMode
-------------------------------------------------------------------------------}
procedure TsWorksheet.SetVirtualRowCount(AValue: Cardinal);
begin
  if FWorkbook.FReadWriteFlag = rwfWrite then exit;
  FVirtualRowCount := AValue;
end;

{@@ ----------------------------------------------------------------------------
  Setter method for the zoom factor
-------------------------------------------------------------------------------}
procedure TsWorksheet.SetZoomFactor(AValue: Double);
begin
  if AValue = FZoomFactor then exit;
  FZoomFactor := AValue;
  if Assigned(FOnZoom) then FOnZoom(Self);
end;


{@@ ----------------------------------------------------------------------------
  Writes UTF-8 encoded text to a cell.

  On formats that don't support unicode, the text will be converted
  to ISO Latin 1.

  @param  ARow             The row of the cell
  @param  ACol             The column of the cell
  @param  AText            The text to be written encoded in utf-8
  @param  ARichTextParams  Array of formatting instructions for characters or
                           groups of characters (see TsRichTextParam).

  @return Pointer to cell created or used

  @see    TsRichTextParams
  @see    TsRichTextParam
-------------------------------------------------------------------------------}
function TsWorksheet.WriteText(ARow, ACol: Cardinal; AText: String;
  ARichTextParams: TsRichTextParams = nil): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteText(Result, AText, ARichTextParams);
end;

function TsWorksheet.WriteUTF8Text(ARow, ACol: Cardinal; AText: String;
  ARichTextParams: TsRichTextParams = nil): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteText(Result, AText, ARichTextParams);
end;

{@@ ----------------------------------------------------------------------------
  Writes UTF-8 encoded text to a cell.

  @param  ACell            Pointer to the cell
  @param  AText            The text to be written encoded in utf-8
  @param  ARichTextParams  Array of formatting instructions for characters or
                           groups of characters (see TsRichTextParam).

  @note   The cell content will be set to cctEmpty if the string is empty.

  @see    TsRichTextParams
  @see    TsRichTextParam
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteText(ACell: PCell; AText: String;
  ARichTextParams: TsRichTextParams = nil);
var
  i: Integer;
  hyperlink: TsHyperlink;
begin
  if ACell = nil then
    exit;

  if (AText = '') and HasHyperlink(ACell) then
  begin
    hyperlink := ReadHyperlink(ACell);
    AText := hyperlink.Target;
    if pos('file:', hyperlink.Target)=1 then
    begin
      URIToFileName(AText, AText);
      ForcePathDelims(AText);
    end;
  end;

  // Delete any pre-existing formula
  DeleteFormula(ACell);

  ACell^.UTF8StringValue := AText;
  if (AText = '') then
  begin
    { Initially, the cell was destroyed here if AText = '' and the cell is not
      formatted, has no comment, no hyperlink, no formula, and is not merged.
      This is not good... The calling procedure cannot be notified that
      ACell is destroyed here.
      See issue #0030049 }
    ACell^.ContentType := cctEmpty;
  end else
  begin
    ACell^.ContentType := cctUTF8String;
    SetLength(ACell^.RichTextParams, Length(ARichTextParams));
    if Length(ARichTextParams) > 0 then
      for i:=0 to High(ARichTextParams) do
        ACell^.RichTextParams[i] := ARichTextParams[i];
  end;

  ChangedCell(ACell^.Row, ACell^.Col);
end;

procedure TsWorksheet.WriteUTF8Text(ACell: PCell; AText: String;
  ARichTextParams: TsRichTextParams = nil);
begin
  WriteText(ACell, AText, ARichTextParams);
end;

{@@ ----------------------------------------------------------------------------
  Writes text containing HTML codes to a cell. Here are the allowed HTML codes:
    <b>, <strong>  ... bold text
    <i>, <em> ........ italic text
    <u>, <ins> ....... underlined text
    <s>, <del> ....... strike-out text
    <sub> ............ subscript
    <sup> ............ superscript
    <font tags> ...... full font selection. "tags" can be:
                       face="..." ... font name
                       size="..." ... font size, in pt, em, px, % (add units!)
                       color="..." .. font color (e.g. red, or #FF0000).

  @param  ARow         The row of the cell
  @param  ACol         The column of the cell
  @param  AText        The text containing the html codes

  @return Pointer to cell created or used

  @see    TsRichTextParams
  @see    TsRichTextParam
-------------------------------------------------------------------------------}
function TsWorksheet.WriteTextAsHTML(ARow, ACol: Cardinal; AText: String): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteTextAsHTML(Result, AText);
end;

{@@ ----------------------------------------------------------------------------
  Removes any previously assigned richtext parameters from a specific cell.
  This action fully restores the font of the cell.
-------------------------------------------------------------------------------}
procedure TsWorksheet.DeleteRichTextParams(ACell: PCell);
begin
  if (ACell <> nil) and (Length(ACell^.RichTextParams) > 0) then
  begin
    SetLength(ACell^.RichTextParams, 0);
    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes text containing HTML codes to a cell. Here are the allowed HTML codes:
    <b>, <strong>  ... bold text
    <i>, <em> ........ italic text
    <u>, <ins> ....... underlined text
    <s>, <del> ....... strike-out text
    <sub> ............ subscript
    <sup> ............ superscript
    <font tags> ...... full font selection. "tags" can be:
                       face="..." ... font name
                       size="..." ... font size, in pt, em, px, % (add units!)
                       color="..." .. font color (e.g. red, or #FF0000).

  @param  ACell        Pointer to the cell
  @param  AText        The text containing the html codes

  @see    TsRichTextParams
  @see    TsRichTextParam
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteTextAsHTML(ACell: PCell; AText: String);
var
  plainText: String;
  rtParams: TsRichTextParams;
begin
  if ACell = nil then
    exit;

  HTMLToRichText(FWorkbook, ReadCellFont(ACell), AText, plainText, rtParams);
  WriteText(ACell, plainText, rtParams);
end;

{@@ ----------------------------------------------------------------------------
  Writes a floating-point number to a cell, does not change the number format

  @param  ARow         Cell row index
  @param  ACol         Cell column index
  @param  ANumber      Number to be written

  @return Pointer to cell created or used
-------------------------------------------------------------------------------}
function TsWorksheet.WriteNumber(ARow, ACol: Cardinal; ANumber: Double): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteNumber(Result, ANumber);
end;

{@@ ----------------------------------------------------------------------------
  Writes a floating-point number to a cell, does not change the number format

  @param  ACell        Pointer to the cell
  @param  ANumber      Number to be written
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteNumber(ACell: PCell; ANumber: Double);
begin
  if ACell <> nil then begin
    // Delete any pre-existing formula, but only if FormulaLock is ON.
    DeleteFormula(ACell);
    // Write number to cell
    ACell^.ContentType := cctNumber;
    ACell^.NumberValue := ANumber;
    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a floating-point number to a cell

  @param  ARow          Cell row index
  @param  ACol          Cell column index
  @param  ANumber       Number to be written
  @param  ANumFormat    Identifier for a built-in number format,
                          e.g. nfFixed (optional)
  @param  ADecimals     Number of decimal places used for formatting (optional)
  @param  AMinIntDigits Minimum count of digits before the decimal separator
  @return Pointer to cell created or used
  @see    TsNumberFormat
-------------------------------------------------------------------------------}
function TsWorksheet.WriteNumber(ARow, ACol: Cardinal; ANumber: Double;
  ANumFormat: TsNumberFormat; ADecimals: Byte = 2;
  AMinIntDigits: Integer = 1): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteNumber(Result, ANumber, ANumFormat, ADecimals, AMinIntDigits);
end;

{@@ ----------------------------------------------------------------------------
  Writes a floating-point number to a cell

  @param  ACell          Pointer to the cell
  @param  ANumber        Number to be written
  @param  ANumFormat     Identifier for a built-in number format, e.g. nfFixed
  @param  ADecimals      Optional number of decimal places used for formatting
                         If ANumFormat is nfFraction the ADecimals defines the
                         digits of Numerator and denominator.
  @param  AMinIntDigits  Minimum count of digits before the decimal separator
  @see TsNumberFormat
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteNumber(ACell: PCell; ANumber: Double;
  ANumFormat: TsNumberFormat; ADecimals: Byte = 2;
  AMinIntDigits: Integer = 1);
var
  fmt: TsCellFormat;
  nfs: String;
begin
  if IsDateTimeFormat(ANumFormat) or IsCurrencyFormat(ANumFormat) then
    raise EFPSpreadsheet.Create(rsInvalidNumberFormat);

  if ACell <> nil then begin
    // Delete any pre-existing formula
    DeleteFormula(ACell);

    // Write value to cell
    ACell^.ContentType := cctNumber;
    ACell^.NumberValue := ANumber;

    fmt := Workbook.GetCellFormat(ACell^.FormatIndex);
    fmt.NumberFormat := ANumFormat;
    if ANumFormat <> nfGeneral then begin
      Include(fmt.UsedFormattingFields, uffNumberFormat);
      if ANumFormat = nfFraction then
      begin
        if ADecimals = 0 then ADecimals := 1;
        nfs := '# ' + DupeString('?', ADecimals) + '/' + DupeString('?', ADecimals);
      end else
        nfs := BuildNumberFormatString(fmt.NumberFormat, Workbook.FormatSettings, ADecimals, AMinIntDigits);
      fmt.NumberFormatIndex := Workbook.AddNumberFormat(nfs);
    end else begin
      Exclude(fmt.UsedFormattingFields, uffNumberFormat);
      fmt.NumberFormatIndex := -1;
    end;
    ACell^.FormatIndex := Workbook.AddCellFormat(fmt);

    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a floating point number to the cell and uses a custom number format
  specified by the format string.
  Note that fpspreadsheet may not be able to detect the formatting when reading
  the file.

  @param  ARow              Cell row index
  @param  ACol              Cell column index
  @param  ANumber           Number to be written
  @param  ANumFormat        Format identifier (nfCustom)
  @param  ANumFormatString  String of formatting codes (such as 'dd/mmm'
  @return Pointer to cell created or used
-------------------------------------------------------------------------------}
function TsWorksheet.WriteNumber(ARow, ACol: Cardinal; ANumber: Double;
  ANumFormat: TsNumberFormat; ANumFormatString: String): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteNumber(Result, ANumber, ANumFormat, ANumFormatString);
end;

{@@ ----------------------------------------------------------------------------
  Writes a floating point number to the cell and uses a custom number format
  specified by the format string.
  Note that fpspreadsheet may not be able to detect the formatting when reading
  the file.

  @param  ACell             Pointer to the cell considered
  @param  ANumber           Number to be written
  @param  ANumFormat        Format identifier (nfCustom)
  @param  ANumFormatString  String of formatting codes (such as 'dd/mmm' )
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteNumber(ACell: PCell; ANumber: Double;
  ANumFormat: TsNumberFormat; ANumFormatString: String);
var
  parser: TsNumFormatParser;
  fmt: TsCellFormat;
begin
  if ACell <> nil then begin
    // Delete any pre-existing formula
    DeleteFormula(ACell);

    // Write value to cell
    ACell^.ContentType := cctNumber;
    ACell^.NumberValue := ANumber;

    parser := TsNumFormatParser.Create(ANumFormatString, FWorkbook.FormatSettings);
    try
      // Format string ok?
      if parser.Status <> psOK then
        raise EFPSpreadsheet.Create(rsNoValidNumberFormatString);
      // Make sure that we do not write a date/time value here
      if parser.IsDateTimeFormat
        then raise EFPSpreadsheet.Create(rsInvalidNumberFormat);
      // If format string matches a built-in format use its format identifier,
      // All this is considered when calling Builtin_NumFormat of the parser.
    finally
      parser.Free;
    end;

    fmt := Workbook.GetCellFormat(ACell^.FormatIndex);
    if ANumFormat <> nfGeneral then begin
      fmt.NumberFormatIndex := Workbook.AddNumberFormat(ANumFormatString);
      Include(fmt.UsedFormattingFields, uffNumberFormat);
    end else begin
      Exclude(fmt.UsedFormattingFields, uffNumberFormat);
      fmt.NumberFormatIndex := -1;
    end;
    ACell^.FormatIndex := Workbook.AddCellFormat(fmt);

    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes an empty cell

  @param  ARow          The row of the cell
  @param  ACol          The column of the cell
  @param  KeepFormula   Does not erase the formula. Off by default because it
                        would be very confusing if the formula had a
                        non-blank result.
  @return Pointer to the cell
  Note:   Empty cells are useful when, for example, a border line extends
          along a range of cells including empty cells.
-------------------------------------------------------------------------------}
function TsWorksheet.WriteBlank(ARow, ACol: Cardinal;
  KeepFormula: Boolean = false): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteBlank(Result, KeepFormula);
end;

{@@ ----------------------------------------------------------------------------
  Writes an empty cell

  @param  ACel          Pointer to the cell
  @param  KeepFormula   Does not erase the formula. Off by default because it
                        would be very confusing if the formula had a
                        non-blank result.
  Note:   Empty cells are useful when, for example, a border line extends
          along a range of cells including empty cells.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteBlank(ACell: PCell; KeepFormula: Boolean = false);
begin
  if ACell <> nil then begin
    if not KeepFormula then
      DeleteFormula(ACell);
    // NOTE: Erase the formula because if it would return a non-blank result
    // this would be very confusing!
    if HasHyperlink(ACell) then
      WriteText(ACell, '')  // '' will be replaced by the hyperlink target.
    else
    begin
      ACell^.ContentType := cctEmpty;
      ChangedCell(ACell^.Row, ACell^.Col);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a boolean cell

  @param  ARow       The row of the cell
  @param  ACol       The column of the cell
  @param  AValue     The boolean value
  @return Pointer to the cell
-------------------------------------------------------------------------------}
function TsWorksheet.WriteBoolValue(ARow, ACol: Cardinal; AValue: Boolean): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteBoolValue(Result, AValue);
end;

{@@ ----------------------------------------------------------------------------
  Writes a boolean cell

  @param  ACell      Pointer to the cell
  @param  AValue     The boolean value
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteBoolValue(ACell: PCell; AValue: Boolean);
begin
  if ACell <> nil then begin
    // Delete any pre-existing formula
    DeleteFormula(ACell);
    // Write value to cell
    ACell^.ContentType := cctBool;
    ACell^.BoolValue := AValue;
    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes data defined as a string into a cell. Depending on the structure of the
  string, the worksheet tries to guess whether it is a number, a date/time or
  a text and calls the corresponding writing method.

  @param  ARow    Row index of the cell
  @param  ACol    Column index of the cell
  @param  AValue  Value to be written into the cell given as a string. Depending
                  on the structure of the string, however, the value is written
                  as a number, a date/time or a text.
  @return Pointer to the cell
-------------------------------------------------------------------------------}
function TsWorksheet.WriteCellValueAsString(ARow, ACol: Cardinal;
  AValue: String): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteCellValueAsString(Result, AValue);
end;

{@@ ----------------------------------------------------------------------------
  Writes data defined as a string into a cell. Depending on the structure of the
  string, the worksheet tries to guess whether it is a number, a date/time or
  a text and calls the corresponding writing method.

  @param  ARow    Row index of the cell
  @param  ACol    Column index of the cell
  @param  AValue  Value to be written into the cell given as a string. Depending
                  on the structure of the string, however, the value is written
                  as a number, a date/time or a text.
  @param  AFormatSettings  FormatSettings record used for conversion of strings
                  with date/time, numbers etc.
  @return Pointer to the cell
-------------------------------------------------------------------------------}
function TsWorksheet.WriteCellValueAsString(ARow, ACol: Cardinal;
  AValue: String; const AFormatSettings: TFormatSettings): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteCellValueAsString(Result, AValue, AFormatSettings);
end;

{@@ ----------------------------------------------------------------------------
  Writes data defined as a string into a cell. Depending on the structure of the
  string, the worksheet tries to guess whether it is a number, a date/time or
  a text and calls the corresponding writing method.
  Conversion of strings to values is done by means of the FormatSettings
  defined in the workbook.

  @param  ACell   Pointer to the cell
  @param  AValue  Value to be written into the cell given as a string. Depending
                  on the structure of the string, however, the value is written
                  as a number, a date/time or a text.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteCellValueAsString(ACell: PCell; AValue: String);
begin
  WriteCellValueAsString(ACell, AValue, FWorkbook.FormatSettings);
end;

{@@ ----------------------------------------------------------------------------
  Writes data defined as a string into a cell. Depending on the structure of the
  string, the worksheet tries to guess whether it is a number, a date/time or
  a text and calls the corresponding writing method.
  Uses the provided FormatSettings for date/time etc.

  @param  ACell   Pointer to the cell
  @param  AValue  Value to be written into the cell given as a string. Depending
                  on the structure of the string, however, the value is written
                  as a number, a date/time or a text.
  @param  AFormatSettings  FormatSettings record used for conversion of strings
                  with date/time, numbers etc.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteCellValueAsString(ACell: PCell; AValue: String;
  const AFormatSettings: TFormatSettings);
const          // isAMPM   isLongTime
  TIME_FMT: array[boolean, boolean] of TsNumberFormat = (
    (nfShortTime, nfLongTime),
    (nfShortTimeAM, nfLongTimeAM)
  );
var
  isPercent: Boolean;
  number: Double;
  currSym: String;
  fmt: TsCellFormat;
  numFmtParams: TsNumFormatParams;
  maxDig: Integer;
  isMixed: Boolean;
  isAMPM: Boolean;
  isLongTime: Boolean;
  rtParams: TsRichTextParams;
  plain: String;
  fmtIndex: Integer;
  ucValue: String;
begin
  if ACell = nil then
    exit;

  DeleteFormula(ACell);

  // Empty cell
  if AValue = '' then
  begin
    WriteText(ACell, '');
    exit;
  end;

  {
  // Force text format by putting an apostrophe at the text beginning
  if AValue[1] = '''' then
  begin
    Delete(AValue, 1, 1);
    WriteNumberFormat(ACell, nfText);
  end;
  }

  // Typing an apostrophe in front of the text bypasses format detection and
  // takes the text literally.
  if AValue[1] = '''' then begin
    WriteText(ACell, Copy(AValue, 2, MaxInt));
    exit;
  end;

  // Cell format
  fmtIndex := GetEffectiveCellFormatIndex(ACell);
  fmt := Workbook.GetCellFormat(fmtIndex);
  numFmtParams := Workbook.GetNumberFormat(fmt.NumberFormatIndex);
  ACell^.FormatIndex := fmtIndex;

  // Handle some cases first in which content autodetection is not wanted.
  if not (soAutoDetectCellType in FOptions) then begin
    // Write text content if the cell has number format nfText
    if IsTextFormat(numFmtParams) then begin
      WriteText(ACell, AValue);
      exit;
    end;
  end;

  isPercent := Pos('%', AValue) = Length(AValue);
  if isPercent then Delete(AValue, Length(AValue), 1);

  // Try to detect the cell content type automatically
  if TryStrToCurrency(AValue, number, currSym, AFormatSettings) then
  begin
    if (soAutoDetectCellType in FOptions) then begin
      WriteCurrency(ACell, number, nfCurrencyRed, -1, currSym);
      if IsTextFormat(numFmtParams) then begin
        WriteNumberFormat(ACell, nfText);
        WriteText(ACell, AValue);
      end;
    end else
      WriteNumber(ACell, number);
    exit;
  end;

  // Check for a fraction string
  if TryFractionStrToFloat(AValue, number, ismixed, maxdig) then
  begin
    WriteNumber(ACell, number);
    if (soAutoDetectCellType in FOptions) then begin
      WriteFractionFormat(ACell, ismixed, maxdig, maxdig);
      if IsTextFormat(numFmtParams) then
      begin
        WriteNumberFormat(ACell, nfText);
        WriteText(ACell, AValue);
      end;
    end;
    exit;
  end;

  // Check for a "number" value (floating point, or integer)
  if TryStrToFloat(AValue, number, AFormatSettings) then
  begin
    if (soAutoDetectCellType in FOptions) then begin
      if isPercent then
        WriteNumber(ACell, number/100, nfPercentage)
      else
      begin
        if IsDateTimeFormat(numFmtParams) then
          WriteNumber(ACell, number, nfGeneral)
        else
          WriteNumber(ACell, number);
      end;
      if IsTextFormat(numFmtParams) then
      begin
        WriteNumberFormat(ACell, nfText);
        WriteText(ACell, AValue);
      end;
    end else
      // Use pre-formatted style
      WriteNumber(ACell, number);
    exit;
  end;


  // Check for a date/time value:
  // Must be after float detection because StrToDateTime will accept a string
  // "1" as a valid date/time.
  if TryStrToDateTime(AValue, number, AFormatSettings) then
  begin
    if (soAutoDetectCellType in FOptions) then begin
      if number < 1.0 then          // this is a time alone
      begin
        if not IsTimeFormat(numFmtParams) then
        begin
          ucValue := Uppercase(AValue);
          isAMPM := (pos('AM', ucValue) > 0) or (pos('PM', ucValue) > 0);
          isLongTime := IsLongTimeFormat(AValue, AFormatSettings.TimeSeparator);
          WriteDateTime(ACell, number, TIME_FMT[isAMPM, isLongTime]);
        end else
          WriteDateTime(ACell, number);
      end else
      if frac(number) = 0.0 then  // this is a date alone
      begin
        if pos(' ', AValue) > 0 then
          WriteDateTime(ACell, number, nfShortDateTime)
        else
          WriteDateTime(ACell, number, nfShortDate);
      end else
      if not IsDateTimeFormat(fmt.NumberFormat) then
        WriteDateTime(ACell, number, nfShortDateTime)
      else
        WriteDateTime(ACell, number);
      if IsTextFormat(numFmtParams) then
      begin
        WriteNumberFormat(ACell, nfText);
        WriteText(ACell, AValue);
      end;
    end else
      // Use pre-formatted style
      WriteDateTime(ACell, number);
    exit;
  end;

  HTMLToRichText(FWorkbook, ReadCellFont(ACell), AValue, plain, rtParams);
  WriteText(ACell, plain, rtParams);
end;

{@@ ----------------------------------------------------------------------------
  Writes a currency value to a given cell. Its number format can be provided
  optionally by specifying various parameters.

  @param ARow            Cell row index
  @param ACol            Cell column index
  @param AValue          Number value to be written
  @param ANumFormat      Format identifier, must be nfCurrency, or nfCurrencyRed.
  @param ADecimals       Number of decimal places
  @param APosCurrFormat  Code specifying the order of value, currency symbol
                         and spaces (see pcfXXXX constants)
  @param ANegCurrFormat  Code specifying the order of value, currency symbol,
                         spaces, and how negative values are shown
                         (see ncfXXXX constants)
  @param ACurrencySymbol String to be shown as currency, such as '$', or 'EUR'.
                         In case of '?' the currency symbol defined in the
                         workbook's FormatSettings is used.
  @return Pointer to the cell
-------------------------------------------------------------------------------}
function TsWorksheet.WriteCurrency(ARow, ACol: Cardinal; AValue: Double;
  ANumFormat: TsNumberFormat = nfCurrency; ADecimals: Integer = 2;
  ACurrencySymbol: String = '?'; APosCurrFormat: Integer = -1;
  ANegCurrFormat: Integer = -1): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteCurrency(Result, AValue, ANumFormat, ADecimals, ACurrencySymbol,
    APosCurrFormat, ANegCurrFormat);
end;

{@@ ----------------------------------------------------------------------------
  Writes a currency value to a given cell. Its number format can be provided
  optionally by specifying various parameters.

  @param ACell           Pointer to the cell considered
  @param AValue          Number value to be written
  @param ANumFormat      Format identifier, must be nfCurrency or nfCurrencyRed.
  @param ADecimals       Number of decimal places
  @param APosCurrFormat  Code specifying the order of value, currency symbol
                         and spaces (see pcfXXXX constants)
  @param ANegCurrFormat  Code specifying the order of value, currency symbol,
                         spaces, and how negative values are shown
                         (see ncfXXXX constants)
  @param ACurrencySymbol String to be shown as currency, such as '$', or 'EUR'.
                         In case of '?' the currency symbol defined in the
                         workbook's FormatSettings is used.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteCurrency(ACell: PCell; AValue: Double;
  ANumFormat: TsNumberFormat = nfCurrency; ADecimals: Integer = -1;
  ACurrencySymbol: String = '?'; APosCurrFormat: Integer = -1;
  ANegCurrFormat: Integer = -1);
var
  nfs: String;
begin
  if ADecimals = -1 then
    ADecimals := Workbook.FormatSettings.CurrencyDecimals;
  if APosCurrFormat = -1 then
    APosCurrFormat := Workbook.FormatSettings.CurrencyFormat;
  if ANegCurrFormat = -1 then
    ANegCurrFormat := Workbook.FormatSettings.NegCurrFormat;
  if ACurrencySymbol = '?' then
    ACurrencySymbol := Workbook.FormatSettings.CurrencyString;
  RegisterCurrency(ACurrencySymbol);

  nfs := BuildCurrencyFormatString(
    ANumFormat,
    Workbook.FormatSettings,
    ADecimals,
    APosCurrFormat, ANegCurrFormat,
    ACurrencySymbol);

  WriteCurrency(ACell, AValue, ANumFormat, nfs);
end;

{@@ ----------------------------------------------------------------------------
  Writes a currency value to a given cell. Its number format is specified by
  means of a format string.

  @param ARow               Cell row index
  @param ACol               Cell column index
  @param AValue             Number value to be written
  @param ANumFormat         Format identifier, must be nfCurrency or nfCurrencyRed.
  @param ANumFormatString   String of formatting codes, including currency symbol.
                            Can contain sections for different formatting of positive
                            and negative number.
                            Example: '"EUR" #,##0.00;("EUR" #,##0.00)'
  @return Pointer to the cell
-------------------------------------------------------------------------------}
function TsWorksheet.WriteCurrency(ARow, ACol: Cardinal; AValue: Double;
  ANumFormat: TsNumberFormat; ANumFormatString: String): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteCurrency(Result, AValue, ANumFormat, ANumFormatString);
end;

{@@ ----------------------------------------------------------------------------
  Writes a currency value to a given cell. Its number format is specified by
  means of a format string.

  @param ACell              Pointer to the cell considered
  @param AValue             Number value to be written
  @param ANumFormat         Format identifier, must be nfCurrency or nfCurrencyRed.
  @param ANumFormatString   String of formatting codes, including currency symbol.
                            Can contain sections for different formatting of positive
                            and negative number.
                            Example: '"EUR" #,##0.00;("EUR" #,##0.00)'
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteCurrency(ACell: PCell; AValue: Double;
  ANumFormat: TsNumberFormat; ANumFormatString: String);
var
  fmt: TsCellFormat;
begin
  if not IsCurrencyFormat(ANumFormat) then
    raise EFPSpreadsheet.Create('[TsWorksheet.WriteCurrency] ANumFormat can only be nfCurrency or nfCurrencyRed');

  if (ACell <> nil) then begin
    // Delete any pre-existing formula
    DeleteFormula(ACell);

    // Write value to cell
    ACell^.ContentType := cctNumber;
    ACell^.NumberValue := AValue;

    fmt := FWorkbook.GetCellFormat(ACell^.FormatIndex);
    fmt.NumberFormatIndex := Workbook.AddNumberFormat(ANumFormatString);
    Include(fmt.UsedFormattingFields, uffNumberFormat);
    ACell^.FormatIndex := FWorkbook.AddCellFormat(fmt);

    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a date/time value to a cell, does not change number format

  @param  ARow          The row of the cell
  @param  ACol          The column of the cell
  @param  AValue        The date/time/datetime to be written
  @return Pointer to the cell
-------------------------------------------------------------------------------}
function TsWorksheet.WriteDateTime(ARow, ACol: Cardinal; AValue: TDateTime): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteDateTime(Result, AValue);
end;

{@@ ----------------------------------------------------------------------------
  Writes a date/time value to a cell. Does not change number format

  @param  ACell         Pointer to the cell considered
  @param  AValue        The date/time/datetime to be written
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteDateTime(ACell: PCell; AValue: TDateTime);
begin
  if ACell <> nil then begin
    // Delete pre-existing formula
    DeleteFormula(ACell);
    // Write date to cell
    ACell^.ContentType := cctDateTime;
    ACell^.DateTimeValue := AValue;
    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a date/time value to a cell

  @param  ARow          The row of the cell
  @param  ACol          The column of the cell
  @param  AValue        The date/time/datetime to be written
  @param  ANumFormat    The format specifier, e.g. nfShortDate (optional)
                        If not specified format is not changed.
  @param  ANumFormatStr Format string, used only for nfCustom or nfTimeInterval.
  @return Pointer to the cell

  Note: at least Excel xls does not recognize a separate datetime cell type:
  a datetime is stored as a (floating point) number, and the cell is formatted
  as a date (either built-in or a custom format).
-------------------------------------------------------------------------------}
function TsWorksheet.WriteDateTime(ARow, ACol: Cardinal; AValue: TDateTime;
  ANumFormat: TsNumberFormat; ANumFormatStr: String = ''): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteDateTime(Result, AValue, ANumFormat, ANumFormatStr);
end;

{@@ ----------------------------------------------------------------------------
  Writes a date/time value to a cell

  @param  ACell         Pointer to the cell considered
  @param  AValue        The date/time/datetime to be written
  @param  ANumFormat    The format specifier, e.g. nfShortDate (optional)
                        If not specified format is not changed.
  @param  ANumFormatStr Format string, used only for nfCustom or nfTimeInterval.

  Note: at least Excel xls does not recognize a separate datetime cell type:
  a datetime is stored as a (floating point) number, and the cell is formatted
  as a date (either built-in or a custom format).
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteDateTime(ACell: PCell; AValue: TDateTime;
  ANumFormat: TsNumberFormat; ANumFormatStr: String = '');
var
  parser: TsNumFormatParser;
  fmt: TsCellFormat;
begin
  if ACell <> nil then begin
    // Delete any pre-existing formula
    DeleteFormula(ACell);

    // Write date to cell
    ACell^.ContentType := cctDateTime;
    ACell^.DateTimeValue := AValue;

    // Date/time is actually a number field in Excel.
    // To make sure it gets saved correctly, set a date format (instead of General).
    // The user can choose another date format if he wants to

    if ANumFormat = nfGeneral then begin
      if trunc(AValue) = 0 then         // time only
        ANumFormat := nfLongTime
      else if frac(AValue) = 0.0 then   // date only
        ANumFormat := nfShortDate;
    end;

    if ANumFormatStr = '' then
      ANumFormatStr := BuildDateTimeFormatString(ANumFormat, Workbook.FormatSettings, ANumFormatStr)
    else
    if ANumFormat = nfTimeInterval then
      ANumFormatStr := AddIntervalBrackets(ANumFormatStr);

    // Check whether the formatstring is for date/times.
    if ANumFormatStr <> '' then begin
      parser := TsNumFormatParser.Create(ANumFormatStr, Workbook.FormatSettings);
      try
        // Format string ok?
        if parser.Status <> psOK then
          raise EFPSpreadsheet.CreateFmt(rsNoValidNumberFormatString, [ANumFormatStr]);
        // Make sure that we do not use a number format for date/times values.
        if not parser.IsDateTimeFormat then
          raise EFPSpreadsheet.CreateFmt(rsInvalidDateTimeFormat, [ANumFormatStr]);
        // Avoid possible duplication of standard formats
        if ANumFormat = nfCustom then
          ANumFormat := parser.NumFormat;
      finally
        parser.Free;
      end;
    end;

    fmt := FWorkbook.GetCellFormat(ACell^.FormatIndex);
    Include(fmt.UsedFormattingFields, uffNumberFormat);
    fmt.NumberFormat := ANumFormat;
    fmt.NumberFormatStr := ANumFormatStr;
    fmt.NumberFormatIndex := Workbook.AddNumberFormat(fmt.NumberFormatStr);
    ACell^.FormatIndex := FWorkbook.AddCellFormat(fmt);

    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a date/time value to a cell

  @param  ARow          The row index of the cell
  @param  ACol          The column index of the cell
  @param  AValue        The date/time/datetime to be written
  @param  ANumFormatStr Format string (the format identifier nfCustom is used to
                        classify the format).
  @return Pointer to the cell

  Note: at least Excel xls does not recognize a separate datetime cell type:
  a datetime is stored as a (floating point) number, and the cell is formatted
  as a date (either built-in or a custom format).
-------------------------------------------------------------------------------}
function TsWorksheet.WriteDateTime(ARow, ACol: Cardinal; AValue: TDateTime;
  ANumFormatStr: String): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteDateTime(Result, AValue, ANumFormatStr);
end;

{@@ ----------------------------------------------------------------------------
  Writes a date/time value to a cell

  @param  ACell         Pointer to the cell considered
  @param  AValue        The date/time/datetime to be written
  @param  ANumFormatStr Format string (the format identifier nfCustom is used to
                        classify the format).

  Note: at least Excel xls does not recognize a separate datetime cell type:
  a datetime is stored as a (floating point) number, and the cell is formatted
  as a date (either built-in or a custom format).
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteDateTime(ACell: PCell; AValue: TDateTime;
  ANumFormatStr: String);
begin
  WriteDateTime(ACell, AValue, nfCustom, ANumFormatStr);
end;

{@@ ----------------------------------------------------------------------------
  Writes an error value to a cell.

  @param  ARow       The row of the cell
  @param  ACol       The column of the cell
  @param  AValue     The error code value
  @return Pointer to the cell

  @see TsErrorValue
-------------------------------------------------------------------------------}
function TsWorksheet.WriteErrorValue(ARow, ACol: Cardinal; AValue: TsErrorValue): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteErrorValue(Result, AValue);
end;

{@@ ----------------------------------------------------------------------------
  Writes an error value to a cell.

  @param  ACol       Pointer to the cell to be written
  @param  AValue     The error code value

  @see TsErrorValue
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteErrorValue(ACell: PCell; AValue: TsErrorValue);
begin
  if ACell <> nil then begin
    // Delete any pre-existing formula
    DeleteFormula(ACell);
    // Write value to cell
    ACell^.ContentType := cctError;
    ACell^.ErrorValue := AValue;
    ChangedCell(ACell^.Row, ACell^.Col);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a formula to a given cell

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @param  AFormula  The formula string to be written. A leading "=" will be removed.
  @param  ALocalized If true, the formula is expected to have decimal and list
                     separators of the workbook's FormatSettings. Otherwise
                     uses dot and comma, respectively.
  @param  R1C1Mode   If true, the formula is expected to contain cell references
                     in Excel's "R1C1" notation; otherwise "A1" references are
                     expected.
  @return Pointer to the cell
-------------------------------------------------------------------------------}
function TsWorksheet.WriteFormula(ARow, ACol: Cardinal; AFormula: String;
  ALocalized: Boolean = false; R1C1Mode: Boolean = false): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteFormula(Result, AFormula, ALocalized, R1C1Mode);
end;

{@@ ----------------------------------------------------------------------------
  Writes a formula to a given cell

  @param  ACell      Pointer to the cell
  @param  AFormula   Formula string to be written. A leading '=' will be removed.
                     If AFormula is '' then an formula already assigned to this
                     cell is deleted.
  @param  ALocalized If true, the formula is expected to have decimal and list
                     separators of the workbook's FormatSettings. Otherwise
                     uses dot and comma, respectively.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteFormula(ACell: PCell; AFormula: String;
  ALocalized: Boolean = false; R1C1Mode: Boolean = false);
var
  parser: TsExpressionParser = nil;
  formula: PsFormula;
begin
  if ACell = nil then
    exit;

  if AFormula = '' then begin
    DeleteFormula(ACell);
    ChangedCell(ACell^.Row, ACell^.Col);
    exit;
  end;

  if not (boIgnoreFormulas in Workbook.Options) then
  begin
    // Remove '='; is not stored internally
    if (AFormula[1] = '=') then
      AFormula := Copy(AFormula, 2, Length(AFormula));

    parser := TsSpreadsheetParser.Create(self);
    try
      if ALocalized then
        parser.Expression[fdLocalized] := AFormula
      else
      if R1C1Mode then
        parser.R1C1Expression[ACell] := AFormula
      else
        parser.Expression[fdExcelA1] := AFormula;

      AFormula := parser.Expression[fdExcelA1];

      formula := FFormulas.AddFormula(ACell^.Row, ACell^.Col, AFormula);
    except
      on E:Exception do begin
        if FWorkbook.FReadWriteFlag = rwfNormal then
          raise
        else begin
          FWorkbook.AddErrorMsg('Formula error in cell "%s!%s": %s', [
            FName, GetCellString(ACell^.Row, ACell^.Col), E.Message]
          );
          parser.Free;
          //FFormulas.DeleteFormula(ACell^.Row, ACell^.Col);
          exit;
        end;
      end;
    end;

    if parser.Has3DLinks then
      ACell.Flags := ACell.Flags + [cf3dFormula]
    else
      ACell.Flags := ACell.Flags - [cf3dFormula];

    formula^.Text := AFormula;
    formula^.Parser := parser;

    // parser will be destroyed by formula
  end;

  // Set formula flags in cell
  ACell^.ContentType := cctFormula;
  ACell^.Flags := ACell^.Flags + [cfHasFormula];

  // Notify controls of changed cell
  ChangedCell(ACell^.Row, ACell^.Col);
end;


{@@ ----------------------------------------------------------------------------
  Writes an RPN formula to a cell. An RPN formula is an array of tokens
  describing the calculation to be performed.

  @param  ARow          Row indows of the cell considered
  @param  ACol          Column index of the cell
  @param  AFormula      Array of TsFormulaElements. The array can be created by
                        using "CreateRPNFormla".
  @return Pointer to the cell

  @see    TsNumberFormat
  @see    TsFormulaElements
  @see    CreateRPNFormula
-------------------------------------------------------------------------------}
function TsWorksheet.WriteRPNFormula(ARow, ACol: Cardinal;
  AFormula: TsRPNFormula): PCell;
begin
  Result := GetCell(ARow, ACol);
  WriteRPNFormula(Result, AFormula);
end;

{@@ ----------------------------------------------------------------------------
  Writes an RPN formula to a cell. An RPN formula is an array of tokens
  describing the calculation to be performed. In addition,the RPN formula is
  converted to a string formula.

  @param  ACell         Pointer to the cell
  @param  AFormula      Array of TsFormulaElements. The array can be created by
                        using "CreateRPNFormla".

  @see    TsNumberFormat
  @see    TsFormulaElements
  @see    CreateRPNFormula
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteRPNFormula(ACell: PCell; ARPNFormula: TsRPNFormula);
var
  formula: PsFormula;
begin
  if ACell = nil then
    exit;

  formula := FFormulas.FindFormula(ACell);
  if formula = nil then begin
    formula := FFormulas.AddFormula(ACell^.Row, ACell^.Col);
    formula^.Parser := TsSpreadsheetParser.Create(self);
  end;
  formula^.Parser.RPNFormula := ARPNFormula;
  formula^.Text := formula^.Parser.Expression[fdExcelA1];
  UseFormulaInCell(ACell, formula);
  ACell^.ContentType := cctFormula;

  ChangedCell(ACell^.Row, ACell^.Col);
end;

function TsWorksheet.GetFormatSettings: TFormatSettings;
begin
  Result := FWorkbook.FormatSettings;
end;

function TsWorksheet.GetIndex: Integer;
begin
  Result := TsWorkbook(FWorkbook).GetWorksheetIndex(self);
end;

{@@ ----------------------------------------------------------------------------
  Moves the worksheet to the specified index in the workbook.

  @param  AValue   New index of the sheet in the workbook. If less than 0 the
                   worksheet will become the first, if greater than the
                   worksheet count it will become the last worksheet of the
                   workbook.
-------------------------------------------------------------------------------}
procedure TsWorksheet.SetIndex(AValue: Integer);
var
  oldIndex: Integer;
begin
  if AValue < 0 then
    AValue := 0
  else if AValue >= TsWorkbook(FWorkbook).GetWorksheetCount then
    AValue := TsWorkbook(FWorkbook).GetWorksheetCount - 1;
  oldIndex := GetIndex;
  if oldIndex <> AValue then
    TsWorkbook(FWorkbook).MoveSheet(oldIndex, Avalue);
end;

procedure TsWorksheet.SetTabColor(AValue: TsColor);
begin
  if AValue = FTabColor then exit;
  FTabColor := AValue;
  TsWorkbook(FWorkbook).ChangedWorksheet(self);
end;

{@@ ----------------------------------------------------------------------------
  Calculates the optimum height of a given row. Depends on the font size
  of the individual cells in the row. Is converted to workbook units.

  @param    ARow   Index of the row to be considered
  @return Row height in workbook units
-------------------------------------------------------------------------------}
function TsWorksheet.CalcAutoRowHeight(ARow: Cardinal): Single;
var
  cell: PCell;
begin
  Result := 0;
  for cell in Cells.GetRowEnumerator(ARow) do
    Result := Max(Result, ReadCellFont(cell).Size);
    // FixMe: This is not correct if text is rotated or wrapped
  Result := FWorkbook.ConvertUnits(Result, suPoints, FWorkbook.Units);
end;

function TsWorksheet.CalcRowHeight(ARow: Cardinal): Single;
// In workbook units
var
  r: PRow;
begin
  r := FindRow(ARow);
  if (r <> nil) and (r^.RowHeightType = rhtCustom) then
    Result := GetRowHeight(ARow, FWorkbook.Units)
  else
  begin
    Result := CalcAutoRowHeight(ARow);
    if Result = 0 then
      Result := GetRowHeight(ARow, FWorkbook.Units);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns the first column record, i.e. that of the left-most column
-------------------------------------------------------------------------------}
function TsWorksheet.FindFirstCol: PCol;
var
  AVLNode: TAVGLVLTreeNode;
begin
  Result := nil;
  AVLNode := FCols.FindLowest;
  if AVLNode <> nil then Result := PCol(AVLNode.Data);
end;

{@@ ----------------------------------------------------------------------------
  Returns the first row record, i.e. that of the top-most row
-------------------------------------------------------------------------------}
function TsWorksheet.FindFirstRow: PRow;
var
  AVLNode: TAVGLVLTreeNode;
begin
  Result := nil;
  AVLNode := FRows.FindLowest;
  if AVLNode <> nil then Result := PRow(AVLNode.Data);
end;

{@@ ----------------------------------------------------------------------------
 Checks if a row record exists for the given row index and returns a pointer
 to the row record, or nil if not found

 @param    ARow   Index of the row looked for
 @return   Pointer to the row record with this row index, or nil if not found
-------------------------------------------------------------------------------}
function TsWorksheet.FindRow(ARow: Cardinal): PRow;
var
  LElement: TRow;
  AVLNode: TAVGLVLTreeNode;
begin
  Result := nil;
  LElement.Row := ARow;
  AVLNode := FRows.Find(@LElement);
  if Assigned(AVLNode) then
    result := PRow(AVLNode.Data);
end;

{@@ ----------------------------------------------------------------------------
 Checks if a column record exists for the given column index and returns a
 pointer to the TCol record, or nil if not found

 @param  ACol   Index of the column looked for
 @return        Pointer to the column record with this column index, or nil
                if not found
-------------------------------------------------------------------------------}
function TsWorksheet.FindCol(ACol: Cardinal): PCol;
var
  LElement: TCol;
  AVLNode: TAVGLVLTreeNode;
begin
  Result := nil;
  LElement.Col := ACol;
  AVLNode := FCols.Find(@LElement);
  if Assigned(AVLNode) then
    result := PCol(AVLNode.Data);
end;

{@@ ----------------------------------------------------------------------------
 Checks if a row record exists for the given row index and creates it if not
 found.

 @param  ARow   Index of the row looked for
 @return        Pointer to the row record with this row index. It can safely be
                assumed that this row record exists.
-------------------------------------------------------------------------------}
function TsWorksheet.GetRow(ARow: Cardinal): PRow;
begin
  Result := FindRow(ARow);
  if (Result = nil) then
    Result := AddRow(ARow);
end;

{@@ ----------------------------------------------------------------------------
 Creates a new row record for the specific row index. It is not checked whether
 a row record already exists for this index. Dupliate records must be avoided!

 @param  ARow   Index of the row to be added
 @return        Pointer to the row record with this row index.
-------------------------------------------------------------------------------}
function TsWorksheet.AddRow(ARow: Cardinal): PRow;
begin
  Result := GetMem(SizeOf(TRow));
  FillChar(Result^, SizeOf(TRow), #0);
  Result^.Row := ARow;
  FRows.Add(Result);
  if FFirstRowIndex = UNASSIGNED_ROW_COL_INDEX
    then FFirstRowIndex := GetFirstRowIndex(true)
    else FFirstRowIndex := Min(FFirstRowIndex, ARow);
  if FLastRowIndex = 0
    then FLastRowIndex := GetLastRowIndex(true)
    else FLastRowIndex := Max(FLastRowIndex, ARow);
end;

{@@ ----------------------------------------------------------------------------
 Checks if a column record exists for the given column index and creates it
 if not found.

 @param  ACol   Index of the column looked for
 @return        Pointer to the TCol record with this column index. It can
                safely be assumed that this column record exists.
-------------------------------------------------------------------------------}
function TsWorksheet.GetCol(ACol: Cardinal): PCol;
begin
  Result := FindCol(ACol);
  if (Result = nil) then
    Result := AddCol(ACol);
end;

{@@ ----------------------------------------------------------------------------
 Creates a new column record for the specific column index.
 It is not checked whether a column record already exists for this index.
 Dupliate records must be avoided!

 @param  ACol   Index of the column to be added
 @return        Pointer to the column record with this column index.
-------------------------------------------------------------------------------}
function TsWorksheet.AddCol(ACol: Cardinal): PCol;
begin
  Result := GetMem(SizeOf(TCol));
  FillChar(Result^, SizeOf(TCol), #0);
  Result^.Col := ACol;
  FCols.Add(Result);
  if FFirstColIndex = UNASSIGNED_ROW_COL_INDEX
    then FFirstColIndex := GetFirstColIndex(true)
    else FFirstColIndex := Min(FFirstColIndex, ACol);
  if FLastColIndex = UNASSIGNED_ROW_COL_INDEX
    then FLastColIndex := GetLastColIndex(true)
    else FLastColIndex := Max(FLastColIndex, ACol);
end;

{@@ ----------------------------------------------------------------------------
  Counts how many cells exist in the given column. Blank cells do contribute
  to the sum, as well as formatted cells.

  @param  ACol  Index of the column considered
  @return Count of cells with value or format in this column
-------------------------------------------------------------------------------}
function TsWorksheet.GetCellCountInCol(ACol: Cardinal): Cardinal;
var
  cell: PCell;
  r: Cardinal;
  row: PRow;
begin
  Result := 0;
  for r := GetFirstRowIndex to GetLastRowIndex do begin
    cell := FindCell(r, ACol);
    if cell <> nil then
      inc(Result)
    else begin
      row := FindRow(r);
      if row <> nil then inc(Result);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Counts how many cells exist in the given row. Blank cells do contribute
  to the sum, as well as formatted cell.s

  @param  ARow  Index of the row considered
  @return Count of cells with value or format in this row
-------------------------------------------------------------------------------}
function TsWorksheet.GetCellCountInRow(ARow: Cardinal): Cardinal;
var
  cell: PCell;
  c: Cardinal;
  col: PCol;
begin
  Result := 0;
  for c := 0 to GetLastColIndex do begin
    cell := FindCell(ARow, c);
    if cell <> nil then
      inc(Result)
    else begin
      col := FindCol(c);
      if col <> nil then inc(Result);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns the index to the cell format to be used for a given column.
  If there is no column record then the default format (index 0) is used.

  @param   ACol   Index of the column considered
  @return  Index of the format into the workbook's FCellFormatList. This format
           will be used for formatting a cell if itself does not have a
           non-zero format index, and if there is no row format either.
-------------------------------------------------------------------------------}
function TsWorksheet.GetColFormatIndex(ACol: Cardinal): Integer;
var
  lCol: PCol;
begin
  Result := 0;   // Default format has index 0
  if ACol <> UNASSIGNED_ROW_COL_INDEX then
  begin
    lCol := FindCol(ACol);
    if lCol <> nil then
      Result := lCol^.FormatIndex
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns the width of the given column. If there is no column record then
  the default column width is returned.

  @param  ACol   Index of the column considered
  @param  AUnits Units for the column width.
  @return Width of the column
-------------------------------------------------------------------------------}
function TsWorksheet.GetColWidth(ACol: Cardinal; AUnits: TsSizeUnits): Single;
var
  col: PCol;
begin
  Result := FDefaultColWidth;
  if ACol <> UNASSIGNED_ROW_COL_INDEX then begin
    col := FindCol(ACol);
    if (col <> nil) and (col^.ColWidthType <> cwtDefault) then
      Result := col^.Width;
  end;
  Result := FWorkbook.ConvertUnits(Result, FWorkbook.Units, AUnits);

  {
  if ACol = UNASSIGNED_ROW_COL_INDEX then
    Result := 0
  else
  begin
    col := FindCol(ACol);
    if (col = nil) or (col^.ColWidthType = cwtDefault) then
      Result := FDefaultColWidth
    else
      Result := col^.Width;
    Result := FWorkbook.ConvertUnits(Result, FWorkbook.Units, AUnits);
  end;
  }
end;

function TsWorksheet.GetColWidth(ACol: Cardinal): Single;
begin
  Result := GetColWidth(ACol, suChars);
end;

{@@ ----------------------------------------------------------------------------
  Returns the type of column width of a specific column.
  If there is no column record then cwtDefault is returned.

  @param  ACol    Index of the column considered
  @param  AUnits  Units for the column width.
  @return Width of the column. This is the "raw" value, without application of
          the zoom factor.
-------------------------------------------------------------------------------}
function TsWorksheet.GetColWidthType(ACol: Cardinal): TsColWidthType;
var
  lCol: PCol;
begin
  lCol := FindCol(ACol);
  if lCol = nil then
    Result := cwtDefault
  else
    Result := lCol^.ColWidthType;
end;


{@@ ----------------------------------------------------------------------------
  Returns the index to the cell format to be used for a given row.
  If there is no row record then the default format (index 0) is returned.

  @param   ARow  Index of the row considered
  @return  Index of the format into the workbook's FCellFormatList. This format
           will be used for formatting a cell if itself does not have a
           non-zero format index.
-------------------------------------------------------------------------------}
function TsWorksheet.GetRowFormatIndex(ARow: Cardinal): Integer;
var
  row: PRow;
begin
  Result := 0;   // Default format has index 0
  if ARow <> UNASSIGNED_ROW_COL_INDEX then
  begin
    row := FindRow(ARow);
    if row <> nil then
      Result := row^.FormatIndex
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns the height of the given row. If there is no row record then the
  default row height is returned

  @param  ARow    Index of the row considered
  @param  AUnits  Units for the row height.
  @return Height of the row. This is the "raw" value, without application of
          the zoom factor.
-------------------------------------------------------------------------------}
function TsWorksheet.GetRowHeight(ARow: Cardinal; AUnits: TsSizeUnits): Single;
var
  lRow: PRow;
begin
  Result := FDefaultRowHeight;
  if ARow <> UNASSIGNED_ROW_COL_INDEX then
  begin
    lRow := FindRow(ARow);
    if (lRow <> nil) and (lRow^.RowHeightType <> rhtDefault) then
      Result := lRow^.Height;
  end;
  Result := FWorkbook.ConvertUnits(Result, FWorkbook.Units, AUnits);

  {
  if ARow = UNASSIGNED_ROW_COL_INDEX then
    Result := 0
  else
  begin
    lRow := FindRow(ARow);
    if lRow <> nil then begin
      Result := lRow^.Height;
      if lRow.RowHeightType = rhtDefault then
        Result := FDefaultRowHeight;
    end else
      Result := FDefaultRowHeight;
    Result := FWorkbook.ConvertUnits(Result, FWorkbook.Units, AUnits);
  end;
  }
end;

function TsWorksheet.GetRowHeight(ARow: Cardinal): Single;
begin
  Result := GetRowHeight(ARow, suLines);
end;

{@@ ----------------------------------------------------------------------------
  Returns the type of rowheight of a specific row.
  If there is no row record then rhtDefault is returned.

  @param  ARow    Index of the row considered
  @param  AUnits  Units for the row height.
  @return Height of the row. This is the "raw" value, without application of
          the zoom factor.
-------------------------------------------------------------------------------}
function TsWorksheet.GetRowHeightType(ARow: Cardinal): TsRowHeightType;
var
  lRow: PRow;
begin
  lRow := FindRow(ARow);
  if lRow = nil then
    Result := rhtDefault
  else
    Result := lRow^.RowHeightType;
end;

function TsWorksheet.HasColFormats: Boolean;
var
  c: Integer;
begin
  for c := 0 to FCols.Count-1 do
    if PCol(FCols[c]).FormatIndex > 0 then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

function TsWorksheet.HasRowFormats: Boolean;
var
  r: Integer;
begin
  for r := 0 to FRows.Count-1 do
    if PRow(FRows[r]).FormatIndex > 0 then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

{@@ ----------------------------------------------------------------------------
  Determines whether the properties stored in a TCol record are default values
  only. Such a record usually can be removed.
-------------------------------------------------------------------------------}
function TsWorksheet.IsDefaultCol(ACol: PCol): Boolean;
begin
  Result :=
    (ACol = nil) or (
    (ACol^.ColWidthType = cwtDefault) and (ACol^.FormatIndex = 0) and (ACol^.Options = [])
    );
end;

{@@ ----------------------------------------------------------------------------
  Determines whether the properties stored in a TRow record are default values
  only. Such a record normally can be removed.
-------------------------------------------------------------------------------}
function TsWorksheet.IsDefaultRow(ARow: PRow): Boolean;
begin
  Result :=
    (ARow = nil) or (
    (ARow^.RowHeightType = rhtDefault) and (ARow^.FormatIndex = 0) and (ARow^.Options = [])
    );
end;

{@@ ----------------------------------------------------------------------------
  Returns whether the specified column is hidden
-------------------------------------------------------------------------------}
function TsWorksheet.ColHidden(ACol: Cardinal): Boolean;
var
  c: PCol;
begin
  c := FindCol(ACol);
  Result := Assigned(c) and (croHidden in c^.Options);
end;

{@@ ----------------------------------------------------------------------------
  Returns whether the specified row is hidden
-------------------------------------------------------------------------------}
function TsWorksheet.RowHidden(ARow: Cardinal): Boolean;
var
  r: PRow;
begin
  r := FindRow(ARow);
  Result := Assigned(r) and (croHidden in r^.Options);
end;

{@@ ----------------------------------------------------------------------------
  Hides the specified column
-------------------------------------------------------------------------------}
procedure TsWorksheet.HideCol(ACol: Cardinal);
var
  c: PCol;
begin
  c := GetCol(ACol);
  if not (croHidden in c^.Options) then begin
    Include(c^.Options, croHidden);
    ChangedCell(0, ACol);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Hides the specified row
-------------------------------------------------------------------------------}
procedure TsWorksheet.HideRow(ARow: Cardinal);
var
  r: PRow;
begin
  r := GetRow(ARow);
  if not (croHidden in r^.Options) then begin
    Include(r^.Options, croHidden);
    ChangedCell(ARow, 0);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Shows the specified column which was hidden previously
-------------------------------------------------------------------------------}
procedure TsWorksheet.ShowCol(ACol: Cardinal);
var
  c: PCol;
begin
  c := FindCol(ACol);
  if Assigned(c) and (croHidden in c^.Options) then begin
    Exclude(c^.Options, croHidden);
    ChangedCell(0, ACol);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Shows the specified row which was hidden previously
-------------------------------------------------------------------------------}
procedure TsWorksheet.ShowRow(ARow: Cardinal);
var
  r: PRow;
begin
  r := FindRow(ARow);
  if Assigned(r) and (croHidden in r^.Options) then begin
    Exclude(r^.Options, croHidden);
    ChangedCell(ARow, 0);
  end;
end;


{@@ ----------------------------------------------------------------------------
  Determines whether the specified row contains any occupied cell.
-------------------------------------------------------------------------------}
function TsWorksheet.IsEmptyRow(ARow: Cardinal): Boolean;
begin
  Result := Cells.GetFirstCellOfRow(ARow) = nil;
end;

{@@ ----------------------------------------------------------------------------
  Deletes the column at the index specified. Cells with greader column indexes
  are moved one column to the left. Merged cell blocks and cell references in
  formulas are considered as well.

  @param   ACol   Index of the column to be deleted
-------------------------------------------------------------------------------}
procedure TsWorksheet.DeleteCol(ACol: Cardinal);
begin
  DeleteRowOrCol(ACol, false);
end;

{@@ ----------------------------------------------------------------------------
  Deletes the row at the index specified. Cells with greater row indexes are
  moved one row up. Merged cell blocks and cell references in formulas
  are considered as well.

  @param   ARow   Index of the row to be deleted
-------------------------------------------------------------------------------}
procedure TsWorksheet.DeleteRow(ARow: Cardinal);
begin
  DeleteRowOrCol(ARow, true);
end;

{@@ ----------------------------------------------------------------------------
  Deletes the row or column at the index specified. AIsRow determines whether
  the index is a row or column index.

  Cells with greader row/column indexes are moved one row up/left.
  Merged cell blocks and cell references in formulas are considered as well.

  @param   AIndex   Index of the row to be deleted
  @param   IsRow    If TRUE then AIndex is a row index, otherwise a column index
-------------------------------------------------------------------------------}
procedure TsWorksheet.DeleteRowOrCol(AIndex: Integer; IsRow: Boolean);
var
  cell: PCell;
  row: PRow;
  col: PCol;
  i: Integer;
  formula: PsFormula;
  sheet: TsWorksheet;
begin
  // Fix merged cells
  FMergedCells.DeleteRowOrCol(AIndex, IsRow);

  // Fix comments
  FComments.DeleteRowOrCol(AIndex, IsRow);

  // Fix hyperlinks
  FHyperlinks.DeleteRowOrCol(AIndex, IsRow);

  // Fix formulas:
  // 1) Fix Row/Col index of in-sheet formulas
  FFormulas.DeleteRowOrCol(AIndex, IsRow);
  // 2) Fix formula references to this sheet
  for i := 0 to FWorkbook.GetWorksheetcount-1 do begin
    sheet := FWorkbook.GetWorksheetByIndex(i);
    sheet.Formulas.FixReferences(AIndex, IsRow, true, self);
  end;

  // Delete cells
  FCells.DeleteRowOrCol(AIndex, IsRow);

  // Fix formula flags
  for cell in FCells do
    if HasFormula(cell) and (FFormulas.FindFormula(cell) = nil) then
      cell^.Flags := cell^.flags - [cfHasFormula, cf3dFormula];

  // Fix formula left-overs (formulas having no cell)
  for formula in FFormulas do
    if FindCell(formula^.Row, formula^.Col) = nil then
      FFormulas.DeleteFormula(formula^.Row, formula^.Col);

  if IsRow then
  begin
    for i:= FRows.Count-1 downto 0 do begin
      row := PRow(FRows.Items[i]);
      if Integer(row^.Row) > AIndex then
        dec(row^.Row)
      else
        break;
    end;
    // Update first and last row index
    UpdateCaches;
    ChangedCell(AIndex, 0);
  end else
  begin
    // Update column index of col records
    for i:=FCols.Count-1 downto 0 do begin
      col := PCol(FCols.Items[i]);
      if Integer(col^.Col) > AIndex then
        dec(col^.Col)
      else
        break;
    end;
    // Update first and last column index
    UpDateCaches;
    ChangedCell(0, AIndex);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Inserts a column BEFORE the column index specified.
  Cells with greater column indexes are moved one row to the right.
  Merged cell blocks and cell references in formulas are considered as well.

  @param   ACol   Index of the column before which a new column is inserted.
-------------------------------------------------------------------------------}
procedure TsWorksheet.InsertCol(ACol: Cardinal);
begin
  InsertRowOrCol(ACol, false);
end;

{@@ ----------------------------------------------------------------------------
  Inserts a row BEFORE the row specified. Cells with greater row indexes are
  moved one row down. Merged cell blocks and cell references in formulas are
  considered as well.

  @param   ARow   Index of the row before which a new row is inserted.
-------------------------------------------------------------------------------}
procedure TsWorksheet.InsertRow(ARow: Cardinal);
begin
  InsertRowOrCol(ARow, true);
end;

{@@ ----------------------------------------------------------------------------
  Inserts a row or column BEFORE the row/column specified by AIndex. Depending
  on IsRow this is either the row or column index.

  Cells with greater row/column indexes are moved one row down/right.
  Merged cell blocks and cell references in formulas are considered as well.

  @param   AIndex   Index of the row or column before which a new row or
                    column is inserted.
  @param   IsRow    Determines whether AIndex refers to a row index (TRUE) or
                    column index (FALSE).
-------------------------------------------------------------------------------}
procedure TsWorksheet.InsertRowOrCol(AIndex: Integer; IsRow: Boolean);
var
  cell: PCell;
  row: PRow;
  col: PCol;
  i: Integer;
  rng: PsCellRange;
  sheet: TsWorksheet;
begin
  // Update row indexes of cell comments
  FComments.InsertRowOrCol(AIndex, IsRow);

  // Update row indexes of cell hyperlinks
  FHyperlinks.InsertRowOrCol(AIndex, IsRow);

  // Fix formulas:
  // 1) Update Row/Col index of in-sheet formulas
  FFormulas.InsertRowOrCol(AIndex, IsRow);
  // 2) Fix formula references to this sheet
  for i := 0 to FWorkbook.GetWorksheetcount-1 do begin
    sheet := FWorkbook.GetWorksheetByIndex(i);
    sheet.Formulas.FixReferences(AIndex, IsRow, false, self);
  end;

  // Update cell indexes of cell records
  FCells.InsertRowOrCol(AIndex, IsRow);

  if IsRow then begin
    // Update row index of row records
    for i:=0 to FRows.Count-1 do begin
      row := PRow(FRows.Items[i]);
      if Integer(row^.Row) >= AIndex then inc(row^.Row);
    end;
  end else
  begin
    // Update column index of column records
    for i:=0 to FCols.Count-1 do begin
      col := PCol(FCols.Items[i]);
      if Integer(col^.Col) >= AIndex then inc(col^.Col);
    end;
  end;

  // Update first and last row/column index
  UpdateCaches;

  if IsRow then
  begin
    // Fix merged cells
    for rng in FMergedCells do
    begin
      // The new row is ABOVE the merged block --> Shift entire range down by 1 row
      if (AIndex < Integer(rng^.Row1)) then
      begin
        // The formerly first row is no longer merged --> un-tag its cells
        for cell in Cells.GetRowEnumerator(rng^.Row1, rng^.Col1, rng^.Col2) do
          Exclude(cell^.Flags, cfMerged);

        // Shift merged block down
        // (Don't call "MergeCells" here - this would add a new merged block
        // because of the new merge base! --> infinite loop!)
        inc(rng^.Row1);
        inc(rng^.Row2);
        // The last row needs to be tagged
        for cell in Cells.GetRowEnumerator(rng^.Row2, rng^.Col1, rng^.Col2) do
          Include(cell^.Flags, cfMerged);
      end else
      // The new row goes through this cell block --> Shift only the bottom row
      // of the range down by 1
      if (AIndex >= Integer(rng^.Row1)) and (AIndex <= Integer(rng^.Row2)) then
        MergeCells(rng^.Row1, rng^.Col1, rng^.Row2+1, rng^.Col2);
    end;

    ChangedCell(AIndex, 0);
  end else
  begin
    // Fix merged cells
    for rng in FMergedCells do
    begin
      // The new column is at the LEFT of the merged block
      // --> Shift entire range to the right by 1 column
      if (AIndex < Integer(rng^.Col1)) then
      begin
        // The former first column is no longer merged --> un-tag its cells
        for cell in Cells.GetColEnumerator(rng^.Col1, rng^.Row1, rng^.Row2) do
          Exclude(cell^.Flags, cfMerged);

        // Shift merged block to the right
        // Don't call "MergeCells" here - this would add a new merged block
        // because of the new merge base! --> infinite loop!
        inc(rng^.Col1);
        inc(rng^.Col2);
        // The right column needs to be tagged
        for cell in Cells.GetColEnumerator(rng^.Col2, rng^.Row1, rng^.Row2) do
          Include(cell^.Flags, cfMerged);
      end else
      // The new column goes through this cell block --> Shift only the right
      // column of the range to the right by 1
      if (AIndex >= Integer(rng^.Col1)) and (AIndex <= Integer(rng^.Col2)) then
        MergeCells(rng^.Row1, rng^.Col1, rng^.Row2, rng^.Col2+1);
    end;

    ChangedCell(0, AIndex);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Moves a column from a specified column index to another column index.
  The operation includes everything associated with the column (cell values,
  cell properties, formats, formulas, column formats, column widths).
  Formulas are automatically adjusted for the new position.
-------------------------------------------------------------------------------}
procedure TsWorksheet.MoveCol(AFromCol, AToCol: Cardinal);
var
  r: Integer;
begin
  if AFromCol = AToCol then
    // Nothing to do
    exit;

  Workbook.DisableNotifications;
  try
    for r := 0 to GetLastRowIndex do begin
      FCells.MoveAlongRow(r, AFromCol, AToCol);
      FComments.MoveAlongRow(r, AFromCol, AToCol);
      FHyperlinks.MoveAlongRow(r, AFromCol, AToCol);
      FFormulas.MoveAlongRow(r, AFromCol, AToCol);
    end;
  finally
    Workbook.EnableNotifications;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Moves a row from a specified row index to another row index.
  The operation includes everything associated with the row (cell values,
  cell properties, formats, formulas, column formats, column widths).
  Formulas are automatically adjusted for the new position.
-------------------------------------------------------------------------------}
procedure TsWorksheet.MoveRow(AFromRow, AToRow: Cardinal);
var
  c: Integer;
begin
  if AFromRow = AToRow then
    // Nothing to do
    exit;

  Workbook.DisableNotifications;
  try
    for c := 0 to GetLastColIndex do begin
      FCells.MoveAlongCol(AFromRow, c, AToRow);
      FComments.MoveAlongCol(AFromRow, c, AToRow);
      FHyperlinks.MoveAlongCol(AFromRow, c, AToRow);
      FFormulas.MoveAlongCol(AFromRow, c, AToRow);
    end;
  finally
    Workbook.EnableNotifications;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Reads the value of the default column width and converts it to the specified
  units
-------------------------------------------------------------------------------}
function TsWorksheet.ReadDefaultColWidth(AUnits: TsSizeUnits): Single;
begin
  Result := FWorkbook.ConvertUnits(FDefaultColWidth, FWorkbook.Units, AUnits);
end;

{@@ ----------------------------------------------------------------------------
  Reads the value of the default row height and converts it to the specified
  units
-------------------------------------------------------------------------------}
function TsWorksheet.ReadDefaultRowHeight(AUnits: TsSizeUnits): Single;
begin
  Result := FWorkbook.ConvertUnits(FDefaultRowHeight, FWorkbook.Units, AUnits);
end;

{@@ ----------------------------------------------------------------------------
  Removes all row records from the worksheet and frees the occupied memory.
  Note: Cells are retained.
-------------------------------------------------------------------------------}
procedure TsWorksheet.RemoveAllRows;
var
  Node: Pointer;
  i: Integer;
begin
  for i := FRows.Count-1 downto 0 do begin
    Node := FRows.Items[i];
    FreeMem(Node, SizeOf(TRow));
  end;
  FRows.Clear;
end;

{@@ ----------------------------------------------------------------------------
  Removes all column records from the worksheet and frees the occupied memory.
  Note: Cells are retained.
-------------------------------------------------------------------------------}
procedure TsWorksheet.RemoveAllCols;
var
  Node: Pointer;
  i: Integer;
begin
  for i := FCols.Count-1 downto 0 do begin
    Node := FCols.Items[i];
    FreeMem(Node, SizeOf(TCol));
  end;
  FCols.Clear;
end;

{@@ ----------------------------------------------------------------------------
  Removes a specified column record from the worksheet and frees the occupied
  memory. This resets its column width and format to default.

  Note: Cells in that column are retained.
-------------------------------------------------------------------------------}
procedure TsWorksheet.RemoveCol(ACol: Cardinal);
var
  AVLNode: TAVGLVLTreeNode;
  lCol: TCol;
begin
  lCol.Col := ACol;
  AVLNode := FCols.Find(@lCol);
  if Assigned(AVLNode) then
  begin
    FreeMem(PCol(AVLNode.Data), SizeOf(TCol));
    FCols.Delete(AVLNode);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Removes a specified row record from the worksheet and frees the occupied memory.
  This resets the its row height to default.
  Note: Cells in that row are retained.
-------------------------------------------------------------------------------}
procedure TsWorksheet.RemoveRow(ARow: Cardinal);
var
  AVLNode: TAVGLVLTreeNode;
  lRow: TRow;
begin
  lRow.Row := ARow;
  AVLNode := FRows.Find(@lRow);
  if Assigned(AVLNode) then
  begin
    FreeMem(PRow(AVLNode.Data), SizeOf(TRow));
    FRows.Delete(AVLNode);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes a row record for the row at a given index to the spreadsheet.
  The row record contains info on the row height and the row format index.

  Creates a new row record if it does not yet exist.

  @param  ARow   Index of the row record which will be created or modified
  @param  AData  Data to be written. Row height expected to be already in the
                 units defined for the workbook.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteRowInfo(ARow: Cardinal; AData: TRow);
var
  lRow: PRow;
begin
  lRow := GetRow(ARow);
  lRow^.Height := AData.Height;
  lRow^.RowHeightType := AData.RowHeightType;
  lRow^.FormatIndex := AData.FormatIndex;
  lRow^.Options := AData.Options;
  ChangedRow(ARow);
end;

{@@ ----------------------------------------------------------------------------
  Sets the cell format index for a specific row.
  Creates a new row record if it does not yet exist.

  @param  ARow          Index of the row to be considered
  @param  AFormatIndex  Index into the workbook's FCellFormatList. This format
                        will be used if a cell has default format index (0).
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteRowFormatIndex(ARow: Cardinal; AFormatIndex:Integer);
var
  lRow: PRow;
begin
  if ARow = UNASSIGNED_ROW_COL_INDEX then
    exit;
  lRow := GetRow(ARow);
  lRow^.FormatIndex := AFormatIndex;
  ChangedRow(ARow);
end;

{@@ ----------------------------------------------------------------------------
  Sets the row height for a given row. Creates a new row record if it
  does not yet exist.

  @param  ARow     Index of the row to be considered
  @param  AHeight  Row height to be assigned to the row.
  @param  AUnits   Units measuring the row height.
  @param  ARowHeightType  Specifies whether the row height is a default,
                   automatic or custom row height.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteRowHeight(ARow: Cardinal; AHeight: Single;
  AUnits: TsSizeUnits; ARowHeightType: TsRowHeightType = rhtCustom);
var
  lRow: PRow;
begin
  if ARow = UNASSIGNED_ROW_COL_INDEX then
    exit;
  lRow := GetRow(ARow);
  if not (croHidden in lRow^.Options) then
  begin
    lRow^.Height := FWorkbook.ConvertUnits(AHeight, AUnits, FWorkbook.FUnits);
    lRow^.RowHeightType := ARowHeightType;
    ChangedRow(ARow);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Sets the row height for a given row. The height is given in terms of
  line count of the worksheet's default font.

  Note that this method is deprecated and will be removed.
  Use the variant in which the units of the new height can be specified.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteRowHeight(ARow: Cardinal; AHeight: Single;
  ARowHeightType: TsRowHeightType = rhtCustom);
begin
  WriteRowHeight(ARow, AHeight, suLines, ARowHeightType);
end;

{@@ ----------------------------------------------------------------------------
  Writes a column record for the column at a specific index to the spreadsheet.
  The column record contains info on the column width and the format index.

  Creates a new column record if it does not yet exist.

  @param  ACol   Index of the column record which will be created or modified
  @param  AData  Data to be written. The column width must already be in
                 the units defined for the workbook.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteColInfo(ACol: Cardinal; AData: TCol);
var
  lCol: PCol;
begin
  lCol := GetCol(ACol);
  lCol^.Width := AData.Width;
  lCol^.ColWidthType := AData.ColWidthType;
  lCol^.FormatIndex := AData.FormatIndex;
  lCol^.Options := AData.Options;
  ChangedCol(ACol);
end;

{@@ ----------------------------------------------------------------------------
  Sets the cell format index for a specific column.
  Creates a new column record if it does not yet exist.

  @param  ACol          Index of the column to be considered
  @param  AFormatIndex  Index into the workbook's FCellFormatList. This format
                        will be used if a cell has default format index (0) and
                        if there is no specific default row format.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteColFormatIndex(ACol: Cardinal; AFormatIndex:Integer);
var
  lCol: PCol;
begin
  if ACol = UNASSIGNED_ROW_COL_INDEX then
    exit;
  lCol := GetCol(ACol);
  lCol^.FormatIndex := AFormatIndex;
  ChangedCol(ACol);
end;

{@@ ----------------------------------------------------------------------------
  Sets the column width for a given column. Creates a new column record if it
  does not yet exist.

  @param  ACol     Index of the column to be considered
  @param  AWidth   Width to be assigned to the column.
  @param  AColWidthType Type of the column width (default -> AWidth is ignored)
                   or custom)
  @param  AUnits   Units used for parameter AWidth.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteColWidth(ACol: Cardinal; AWidth: Single;
  AUnits: TsSizeUnits; AColWidthType: TsColWidthType = cwtCustom);
var
  lCol: PCol;
begin
  if ACol = UNASSIGNED_ROW_COL_INDEX then
    exit;
  lCol := GetCol(ACol);
  if not (croHidden in lCol^.Options) then
  begin
    lCol^.Width := FWorkbook.ConvertUnits(AWidth, AUnits, FWorkbook.FUnits);
    lCol^.ColWidthType := AColWidthType;
    ChangedCol(ACol);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Sets the column width for a given column. The width is given in terms of
  count of the "0" character using the worksheet's default font.

  Note that this method is deprecated and will be removed.
  Use the variant in which the units of the new width can be specified.
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteColWidth(ACol: Cardinal; AWidth: Single;
  AColWidthType: TsColWidthType = cwtCustom);
begin
  WriteColWidth(ACol, AWidth, suChars, AColWidthType);
end;

{@@ ----------------------------------------------------------------------------
  Sets the default column widtht of the worksheet. The value will be stored
  in workbook units.

  @param  AValue   New value of the default column width
  @param  AUnits   Units used by AValue
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteDefaultColWidth(AValue: Single; AUnits: TsSizeUnits);
begin
  FDefaultColWidth := FWorkbook.ConvertUnits(AValue, AUnits, FWorkbook.Units);
end;

{@@ ----------------------------------------------------------------------------
  Sets the default row height of the worksheet. The value will be stored
  in workbook units.

  @param  AValue   New value of the default row height
  @param  AUnits   Units used by AValue
-------------------------------------------------------------------------------}
procedure TsWorksheet.WriteDefaultRowHeight(AValue: Single; AUnits: TsSizeUnits);
begin
  FDefaultRowHeight := FWorkbook.ConvertUnits(AValue, AUnits, FWorkbook.Units);
end;

{@@ ----------------------------------------------------------------------------
  Sets the PageBreak flag for the column record with the specified column index.
  This means that, when printed, a page break will occur before this column.
  Note that FPS currently does not support printing by itself.
-------------------------------------------------------------------------------}
procedure TsWorksheet.AddPageBreakToCol(ACol: Cardinal);
var
  lCol: PCol;
begin
  lCol := GetCol(ACol);
  Include(lCol^.Options, croPageBreak);
  ChangedCol(ACol);
end;

{@@ ----------------------------------------------------------------------------
  Sets the PageBreak flag for the row record with the specified row index.
  This means that, when printed, a page break will occur before this row.
  Note that FPS currently does not support printing by itself.
-------------------------------------------------------------------------------}
procedure TsWorksheet.AddPageBreakToRow(ARow: Cardinal);
var
  lRow: PRow;
begin
  lRow := GetRow(ARow);
  Include(lRow^.Options, croPageBreak);
  ChangedRow(ARow);
end;

{@@ ----------------------------------------------------------------------------
  Returns true if the column with the specified index is the first one after a
  manual page break.
-------------------------------------------------------------------------------}
function TsWorksheet.IsPageBreakCol(ACol: Cardinal): Boolean;
var
  lCol: PCol;
begin
  lCol := FindCol(ACol);
  Result := Assigned(lCol) and (croPageBreak in lCol^.Options);
end;

{@@ ----------------------------------------------------------------------------
  Returns true if the row with the specified index is the first one after a
  manual page break.
-------------------------------------------------------------------------------}
function TsWorksheet.IsPageBreakRow(ARow: Cardinal): Boolean;
var
  lRow: PRow;
begin
  lRow := FindRow(ARow);
  Result := Assigned(lRow) and (croPageBreak in lRow^.Options);
end;

{@@ ----------------------------------------------------------------------------
  Removes the PageBreak flag for the column record with the specified column
  index.
  This means that, during printing, page break handling of this column will be
  automatic.
  Note that FPS currently does not support printing by itself.
-------------------------------------------------------------------------------}
procedure TsWorksheet.RemovePageBreakFromCol(ACol: Cardinal);
var
  lCol: PCol;
begin
  lCol := FindCol(ACol);
  if Assigned(lCol) then begin
    Exclude(lCol^.Options, croPageBreak);
    // Free and delete node when the col record only has default values now.
    if (lCol^.Options = []) and (lCol^.FormatIndex = 0) and (lCol^.ColWidthType = cwtDefault) then
      RemoveCol(ACol);
    ChangedCol(ACol);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Removes the PageBreak flag for the row record with the specified row index.
  This means that, during printing, page break handling of this row will be
  automatic.
  Note that FPS currently does not support printing by itself.
-------------------------------------------------------------------------------}
procedure TsWorksheet.RemovePageBreakFromRow(ARow: Cardinal);
var
  lRow: PRow;
begin
  lRow := FindRow(ARow);
  if Assigned(lRow) then begin
    Exclude(lRow^.Options, croPageBreak);
    // Free and delete node if the row record only has default values now.
    if (lRow^.Options = []) and (lRow^.FormatIndex = 0) and (lRow^.RowHeightType = rhtDefault) then
      RemoveRow(ARow);
    ChangedRow(ARow);
  end;
end;


{==============================================================================}
{                              TsWorkbook                                      }
{==============================================================================}

{@@ ----------------------------------------------------------------------------
  Helper method called before reading the workbook. Clears the error log.
-------------------------------------------------------------------------------}
procedure TsWorkbook.PrepareBeforeReading;
begin
  Clear;

  // Abort if virtual mode is active without an event handler
  if (boVirtualMode in FOptions) and not Assigned(OnReadCellData) then
    raise EFPSpreadsheet.Create('[TsWorkbook.PrepareBeforeReading] Event handler "OnReadCellData" required for virtual mode.');
end;

{@@ ----------------------------------------------------------------------------
  Helper method called before saving the workbook. Clears the error log, and
  calculates the formulas in all worksheets if workbook option soCalcBeforeSaving
  is set.
-------------------------------------------------------------------------------}
procedure TsWorkbook.PrepareBeforeSaving;
var
  sheet: TsWorksheet;
  virtModeOK: Boolean;
begin
  // Clear error log
  ClearErrorList;

  // Updates fist/last column/row index
  UpdateCaches;

  // Calculated formulas (if requested)
  if (boCalcBeforeSaving in FOptions) then
    for sheet in FWorksheets do
      sheet.CalcFormulas;

  // Abort if virtual mode is active without an event handler
  if (boVirtualMode in FOptions) then
  begin
    virtModeOK := false;
    for sheet in FWorksheets do
      if Assigned(sheet.OnWriteCellData) then
        virtModeOK := true;
    if not virtModeOK then
      raise EFPSpreadsheet.Create('[TsWorkbook.PrepareBeforeWriting] At least one '+
        'sheet must have an event handler "OnWriteCellData" for virtual mode.');
  end;
end;

{@@ ----------------------------------------------------------------------------
  Conversion of length values between units
-------------------------------------------------------------------------------}
function TsWorkbook.ConvertUnits(AValue: Double;
  AFromUnits, AToUnits: TsSizeUnits): Double;
begin
  if AFromUnits = AToUnits then
  begin
    Result := AValue;
    exit;
  end;
  // Convert to mm
  case AFromUnits of
    suMillimeters:
      Result := AValue;
    suCentimeters:
      Result := AValue * 10.0;
    suInches:
      Result := inToMM(AValue);
    suPoints:
      Result := ptsToMM(AValue);
    suChars:
      Result := ptsToMM(GetDefaultFont.Size * ZERO_WIDTH_FACTOR * AValue);
    suLines:
      Result := ptsToMM(GetDefaultFont.Size * (AValue + ROW_HEIGHT_CORRECTION));
    else
      raise EFPSpreadsheet.Create('Unit not supported.');
  end;
  // Convert from mm
  case AToUnits of
    suMillimeters: ; // nothing to do
    suCentimeters:
      Result := Result * 0.1;
    suInches:
      Result := mmToIn(Result);
    suPoints:
      Result := mmToPts(Result);
    suChars:
      Result := mmToPts(Result) / (GetDefaultFont.Size * ZERO_WIDTH_FACTOR);
    suLines:
      Result := mmToPts(Result) / GetDefaultFont.Size - ROW_HEIGHT_CORRECTION;
    else
      raise EFPSpreadsheet.Create('Unit not supported.');
  end;
end;

{@@ ----------------------------------------------------------------------------
  Helper method for rebuilding all string formulas of the workbook from the
  pared formulas.
-------------------------------------------------------------------------------}
procedure TsWorkbook.RebuildFormulasCallback(Data, Arg: Pointer);
var
  formula: PsFormula;
begin
  Unused(Arg);
  for formula in TsWorksheet(Data).Formulas do
    formula^.Text := formula^.Parser.Expression[fdExcelA1];
end;


{@@ ----------------------------------------------------------------------------
  Helper method for clearing the spreadsheet list.
-------------------------------------------------------------------------------}
procedure TsWorkbook.RemoveWorksheetsCallback(Data, Arg: pointer);
begin
  Unused(Arg);
  TsWorksheet(Data).Free;
end;

{@@ ----------------------------------------------------------------------------
  Notification of visual controls that some global data of a worksheet
  have changed.
-------------------------------------------------------------------------------}
procedure TsWorkbook.ChangedWorksheet(AWorksheet: TsWorksheet);
begin
  if FReadWriteFlag = rwfRead then
    exit;

  if NotificationsEnabled and Assigned(FOnChangeWorksheet)
    then OnChangeWorksheet(self, AWorksheet);
end;

{@@ ----------------------------------------------------------------------------
  Helper method to disable notification of visual controls
-------------------------------------------------------------------------------}
procedure TsWorkbook.DisableNotifications;
begin
  inc(FNotificationLock);
end;

{@@ ----------------------------------------------------------------------------
  Helper method to enable notification of visual controls
-------------------------------------------------------------------------------}
procedure TsWorkbook.EnableNotifications;
begin
  dec(FNotificationLock);
end;

{@@ ----------------------------------------------------------------------------
  Helper method to determine whether visual controls are notified of changes
-------------------------------------------------------------------------------}
function TsWorkbook.NotificationsEnabled: Boolean;
begin
  Result := (FNotificationLock = 0);
end;

{@@ ----------------------------------------------------------------------------
  Helper method to update internal caching variables
-------------------------------------------------------------------------------}
procedure TsWorkbook.UpdateCaches;
var
  sheet: TsWorksheet;
begin
  for sheet in FWorksheets do
    sheet.UpdateCaches;
end;

{@@ ----------------------------------------------------------------------------
  Constructor of the workbook class. Among others, it initializes the built-in
  fonts, defines the default font, and sets up the FormatSettings for
  localization of some number formats.
-------------------------------------------------------------------------------}
constructor TsWorkbook.Create;
var
  fmt: TsCellFormat;
begin
  inherited Create;
  FWorksheets := TFPList.Create;

  FFontList := TFPList.Create;
  SetDefaultFont(DEFAULT_FONTNAME, DEFAULT_FONTSIZE);
  InitFonts;

  FNumFormatList := TsNumFormatList.Create(FormatSettings, true);
  FCellFormatList := TsCellFormatList.Create(false);
  FConditionalFormatList := TsConditionalFormatList.Create;
  FEmbeddedObjList := TFPList.Create;

  // Add default cell format
  InitFormatRecord(fmt);
  AddCellFormat(fmt);

  // Protection
  InitCryptoInfo(FCryptoInfo);

  // Metadata
  FMetaData := TsMetaData.Create;
end;

{@@ ----------------------------------------------------------------------------
  Destructor of the workbook class
-------------------------------------------------------------------------------}
destructor TsWorkbook.Destroy;
begin
  DisableNotifications;
  RemoveAllWorksheets;
  EnableNotifications;
  FWorksheets.Free;

  FMetaData.Free;
  FConditionalFormatList.Free;
  FCellFormatList.Free;
  FNumFormatList.Free;

  RemoveAllFonts;
  FFontList.Free;

  RemoveAllEmbeddedObj;
  FEmbeddedObjList.Free;

  FreeAndNil(FSearchEngine);

  inherited Destroy;
end;

{@@ ----------------------------------------------------------------------------
  Clears content and formats from the workbook
-------------------------------------------------------------------------------}
procedure TsWorkbook.Clear;
begin
  // Initialize fonts
  InitFonts;

  // Remove already existing worksheets.
  RemoveAllWorksheets;

  // Remove all cell formats, but keep the default format
  RemoveAllCellFormats(true);

  // Remove all number formats
  RemoveAllNumberFormats;

  // Remove embedded images
  RemoveAllEmbeddedObj;

  // Reset cryptoinfo
  InitCryptoInfo(FCryptoInfo);

  // Clear error log
  ClearErrorList;

  // Clear metadata
  FMetaData.Clear;
end;


{@@ ----------------------------------------------------------------------------
  Helper method for determining the spreadsheet type. Read the first few bytes
  of a file and determines the spreadsheet type from the characteristic
  signature.
-------------------------------------------------------------------------------}
class procedure TsWorkbook.GetFormatFromFileHeader(const AFileName: TFileName;
  out AFormatIDs: TsSpreadFormatIDArray);
var
  stream: TStream;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    GetFormatFromFileHeader(stream, AFormatIDs)
  finally
    stream.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Helper method for determining the spreadsheet format. Reads the first
  few bytes of a stream and determines the spreadsheet type from the
  characteristic signature.
-------------------------------------------------------------------------------}
class procedure TsWorkbook.GetFormatFromFileHeader(AStream: TStream;
  out AFormatIDs: TsSpreadFormatIDArray); overload;
var
  reader: TsSpreadReaderClass;
  fmtIDs: TsSpreadformatIDArray;
  i, j: Integer;
begin
  AFormatIDs := nil;
  if AStream = nil then
    exit;

  fmtIDs := GetSpreadFormats(faRead, [ord(sfExcel8)]);
  SetLength(AFormatIDs, Length(fmtIDs));
  j := 0;
  for i:=0 to High(fmtIDs) do begin
    reader := GetSpreadReaderClass(fmtIDs[i]);
    if Assigned(reader) and reader.CheckFileFormat(AStream) then begin
      AFormatIDs[j] := fmtIDs[i];
      inc(j);
    end;
  end;
  SetLength(AFormatIDs, j);
end;

{@@ ----------------------------------------------------------------------------
  Determines the maximum index of used columns and rows in all sheets of this
  workbook. Respects VirtualMode.
  Is needed to disable saving when limitations of the format is exceeded.
-------------------------------------------------------------------------------}
procedure TsWorkbook.GetLastRowColIndex(out ALastRow, ALastCol: Cardinal);
var
  sheet: TsWorksheet;
begin
  ALastRow := 0;
  ALastCol := 0;
  if (boVirtualMode in Options) then
  begin
    for sheet in FWorksheets do
      if Assigned(sheet.OnWriteCellData) then
      begin
        if sheet.VirtualRowCount > 0 then
          ALastRow := Max(ALastRow, sheet.VirtualRowCount - 1);
        if sheet.VirtualColCount > 0 then
          ALastCol := Max(ALastCol, sheet.VirtualColCount - 1);
      end;
  end else
  begin
    for sheet in FWorksheets do
    begin
      ALastRow := Max(ALastRow, sheet.GetLastRowIndex);
      ALastCol := Max(ALastCol, sheet.GetLastColIndex);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Reads the document from a file. It is assumed to have the given file format.

  This method is intended for built-in file formats only. For user-provided
  formats, call the overloaded method with the FormadID parameter.

  @param  AFileName  Name of the file to be read
  @param  AFormat    File format assumed

-------------------------------------------------------------------------------}
procedure TsWorkbook.ReadFromFile(AFileName: string;
  AFormat: TsSpreadsheetFormat; AParams: TsStreamParams = []);
begin
  if AFormat = sfUser then
    raise EFPSpreadsheetReader.Create('[TsWorkbook.ReadFromFile] Don''t call this method for user-provided file formats.');
  ReadFromFile(AFilename, ord(AFormat), '', AParams);
end;

{@@ ----------------------------------------------------------------------------
  Reads the document from a file. It is assumed to have the given file format.
  Works also for user-provided file formats.

  @param  AFileName  Name of the file to be read
  @param  AFormatID  Identifier of the file format assumed
-------------------------------------------------------------------------------}
procedure TsWorkbook.ReadFromFile(AFileName: string; AFormatID: TsSpreadFormatID;
  APassword: String = ''; AParams: TsStreamParams = []);
var
  AReader: TsBasicSpreadReader;
  ok: Boolean;
begin
  if not FileExists(AFileName) then
    raise EFPSpreadsheetReader.CreateFmt(rsFileNotFound, [AFileName]);

  if AFormatID = sfIDUnknown then begin
    ReadFromFile(AFileName, APassword, AParams);
    exit;
  end;

  AReader := CreateSpreadReader(self, AFormatID);
  try
    FFileName := AFileName;
    PrepareBeforeReading;
    ok := false;
    FReadWriteFlag := rwfRead;
    inc(FNotificationLock);          // This locks various notifications from being sent
    try
      AReader.ReadFromFile(AFileName, APassword, AParams);
      ok := true;
      UpdateCaches;
      if (boAutoCalc in Options) then
        CalcFormulas;
//        Recalc;
      FFormatID := AFormatID;
    finally
      FReadWriteFlag := rwfNormal;
      dec(FNotificationLock);
      if ok and Assigned(FOnOpenWorkbook) then   // ok is true if file has been read successfully
        FOnOpenWorkbook(self);   // send common notification
    end;
  finally
    AReader.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Reads the document from a file. This method will try to guess the format from
  the extension. In the case of the ambiguous xls extension, it will simply
  assume that it is BIFF8. Note that it could be BIFF2 or 5 as well.
-------------------------------------------------------------------------------}
procedure TsWorkbook.ReadFromFile(AFileName: string; APassword: String = '';
  AParams: TsStreamParams = []);
var
  success: Boolean;
  fmtID: TsSpreadFormatID;
  fileFormats: TsSpreadFormatIDArray;
  i: Integer;
  ext: String;
begin
  if not FileExists(AFileName) then
    raise EFPSpreadsheetReader.CreateFmt(rsFileNotFound, [AFileName]);

  // Try to get file format from file header
  GetFormatFromFileHeader(AFileName, fileformats);
  if Length(fileformats) = 0 then
    // If not successful use formats defined by extension
    fileFormats := GetSpreadFormatsFromFileName(faRead, AFileName);

  if Length(fileformats) = 0 then
    fileformats := GetSpreadFormats(faRead, [ord(sfExcel8)]);

  // Move file format corresponding to file extension to the top to load it first.
  ext := Lowercase(ExtractFileExt(AFileName));
  for i := 0 to High(fileformats) do
    if ext = GetSpreadFormatExt(fileformats[i]) then begin
      fmtID := fileformats[0];
      fileFormats[0] := fileformats[i];
      fileFormats[i] := fmtID;
    end;

  // No file format found for this file --> error
  if Length(fileformats) = 0 then
    raise EFPSpreadsheetReader.CreateFmt(rsReaderNotFound, [AFileName]);

  // Here is the trial-and-error loop checking for the various formats.
  success := false;
  for i:=0 to High(fileformats) do begin
    try
      ReadFromFile(AFileName, fileformats[i], APassword, AParams);
      success := true;
      break;  // Exit the loop if we reach this point successfully.
    except
    end;
  end;

  // The file could not be opened successfully --> Error.
  if not success then
    raise EFPSpreadsheetReader.CreateFmt(rsInvalidSpreadsheetFile, [AFileName]);
end;

{@@ ----------------------------------------------------------------------------
  Reads the document from a file, but ignores the extension.
-------------------------------------------------------------------------------}
procedure TsWorkbook.ReadFromFileIgnoringExtension(AFileName: string;
  APassword: String = ''; AParams: TsStreamParams = []);
var
  formatID: TsSpreadFormatID;
  fileformats: TsSpreadFormatIDArray;
  success: Boolean;
begin
  fileformats := GetSpreadFormats(faRead, [ord(sfOOXML), ord(sfOpenDocument), ord(sfExcel8)]);
  for formatID in fileformats do begin
    try
      ReadFromFile(AFileName, formatID, APassword, AParams);
      success := true;
      break;
    except
      success := false;
    end;
  end;
  if not success then
    raise EFPSpreadsheetReader.CreateFmt(rsInvalidSpreadsheetFile, [AFileName]);
end;

{@@ ----------------------------------------------------------------------------
  Reads the document from a seekable stream.

  @param  AStream  Stream being read
  @param  AFormat  File format assumed.
  @param  AParams  Optional parameters to control stream access.
-------------------------------------------------------------------------------}
procedure TsWorkbook.ReadFromStream(AStream: TStream;
  AFormat: TsSpreadsheetFormat; AParams: TsStreamParams = []);
begin
  if AFormat = sfUser then
    raise EFPSpreadsheetReader.Create('[TsWorkbook.ReadFromFile] Don''t call this method for user-provided file formats.');
  ReadFromStream(AStream, ord(AFormat), '', AParams);
end;

{@@ ----------------------------------------------------------------------------
  Reads the document from a seekable stream.

  @param  AStream    Stream being read
  @param  AFormatID  Identifier of the file format assumed.
  @param  AParams    Optional parameters to control stream access.
-------------------------------------------------------------------------------}
procedure TsWorkbook.ReadFromStream(AStream: TStream; AFormatID: TsSpreadFormatID;
  APassword: String = ''; AParams: TsStreamParams = []);
var
  AReader: TsBasicSpreadReader;
  ok: Boolean;
begin
  AReader := CreateSpreadReader(self, AFormatID);
  try
    PrepareBeforeReading;
    FReadWriteFlag := rwfRead;
    ok := false;
    inc(FNotificationLock);
    try
      AStream.Position := 0;
      AReader.ReadFromStream(AStream, APassword, AParams);
      ok := true;
      UpdateCaches;
      if (boAutoCalc in Options) then
        CalcFormulas;
//        Recalc;
      FFormatID := AFormatID;
    finally
      FReadWriteFlag := rwfNormal;
      dec(FNotificationLock);
      if ok and Assigned(FOnOpenWorkbook) then   // ok is true if stream has been read successfully
        FOnOpenWorkbook(self);   // send common notification
    end;
  finally
    AReader.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes the document to a file. If the file doesn't exist, it will be created.
  Can be used only for built-in file formats.

  @param  AFileName  Name of the file to be written
  @param  AFormat    The file will be written in this file format.
  @param  AOverwriteExisting  If the file is already existing it will be
                     overwritten in case of AOverwriteExisting = true.
                     If false an exception will be raised.
  @param  AParams    Optional parameters to control stream access.
-------------------------------------------------------------------------------}
procedure TsWorkbook.WriteToFile(const AFileName: string;
  const AFormat: TsSpreadsheetFormat; const AOverwriteExisting: Boolean = False;
  AParams: TsStreamParams = []);
begin
  if AFormat = sfUser then
    raise EFPSpreadsheetWriter.Create('[TsWorkbook.WriteToFile] Don''t call this method for user-provided file formats.');
  WriteToFile(AFilename, ord(AFormat), AOverwriteExisting, AParams);
end;

{@@ ----------------------------------------------------------------------------
  Writes the document to a file. If the file doesn't exist, it will be created.
  Can be used for both built-in and user-provided file formats.

  @param  AFileName  Name of the file to be written
  @param  AFormatID  The file will be written in the file format identified by
                     this number.
  @param  AOverwriteExisting  If the file is already existing it will be
                     overwritten in case of AOverwriteExisting = true.
                     If the parameter is FALSE then an exception will be raised.
  @param  AParams    Optional parameters to control stream access.
-------------------------------------------------------------------------------}
procedure TsWorkbook.WriteToFile(const AFileName: string;
  const AFormatID: TsSpreadFormatID; const AOverwriteExisting: Boolean = False;
  AParams: TsStreamParams = []);
var
  AWriter: TsBasicSpreadWriter;
begin
  AWriter := CreateSpreadWriter(self, AFormatID);
  try
    FFileName := AFileName;
    FFormatID := AFormatID;
    PrepareBeforeSaving;
    AWriter.CheckLimitations;
    FReadWriteFlag := rwfWrite;
    AWriter.WriteToFile(AFileName, AOverwriteExisting, AParams);
  finally
    FReadWriteFlag := rwfNormal;
    AWriter.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Writes the document to file based on the extension.
  If this was an earlier sfExcel type file, it will be upgraded to sfExcel8.

  @param  AFileName  Name of the destination file
  @param  AOverwriteExisting  If the file already exists it will be overwritten
                     of AOverwriteExisting is true. In case of false, an
                     exception will be raised.
  @param  AParams    Optional parameters to control stream access
-------------------------------------------------------------------------------}
procedure TsWorkbook.WriteToFile(const AFileName: String;
  const AOverwriteExisting: Boolean; AParams: TsStreamParams = []);
var
  fileformats: TsSpreadFormatIDArray;
  ext: String;
begin
  ext := ExtractFileExt(AFileName);

  if Lowercase(ext) = STR_EXCEL_EXTENSION then
    fileformats := GetSpreadFormatsFromFileName(faWrite, AFileName, ord(sfExcel8)) // give preference to BIFF8
  else
    fileformats := GetSpreadFormatsFromFileName(faWrite, AFileName);

  if Length(fileformats) > 0 then
    WriteToFile(AFileName, fileformats[0], AOverwriteExisting, AParams)
  else
    raise EFPSpreadsheetWriter.Create(Format(rsInvalidExtension, [ext]));
end;

{@@ ----------------------------------------------------------------------------
  Writes the document to a stream

  Can be used only for built-in file formats.

  @param  AStream         Instance of the stream being written to
  @param  AFormat         File format to be written.
  @param  AClipboardMode  Stream will be used by calling method for clipboard access
  @param  AParams         Optional parameters to control stream access
                          The HTML writer, for example, can be forced to write
                          a valid html document in Windows.
-------------------------------------------------------------------------------}
procedure TsWorkbook.WriteToStream(AStream: TStream; AFormat: TsSpreadsheetFormat;
  AParams: TsStreamParams = []);
begin
  if AFormat = sfUser then
    raise EFPSpreadsheet.Create('[TsWorkbook.WriteToFile] Don''t call this method for user-provided file formats.');
  WriteToStream(AStream, ord(AFormat), AParams);
end;

{@@ ----------------------------------------------------------------------------
  Writes the document to a stream

  Can be used for both built-in and userprovided file formats.

  @param  AStream         Instance of the stream being written to
  @param  AFormatID       Identifier of the file format to be written.
  @param  AClipboardMode  Stream will be used by calling method for clipboard access
  @param  AParams         Optional parameters to control stream access
                          The HTML writer, for example, can be forced to write
                          a valid html document in Windows.
-------------------------------------------------------------------------------}
procedure TsWorkbook.WriteToStream(AStream: TStream;
  AFormatID: TsSpreadFormatID; AParams: TsStreamParams = []);
var
  AWriter: TsBasicSpreadWriter;
begin
  AWriter := CreateSpreadWriter(self, AFormatID, AParams);
  try
    FFormatID := AFormatID;
    PrepareBeforeSaving;
    AWriter.CheckLimitations;
    FReadWriteFlag := rwfWrite;
    AWriter.WriteToStream(AStream, AParams);
  finally
    FReadWriteFlag := rwfNormal;
    AWriter.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Adds a new worksheet to the workbook.
  It is put to the end of the worksheet list.

  @param  AName                The name of the new worksheet
  @param  ReplaceDupliateName  If true and the sheet name already exists then
                               a number is added to the sheet name to make it
                               unique.
  @return The instance of the newly created worksheet
  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.AddWorksheet(AName: string;
  ReplaceDuplicateName: Boolean = false): TsWorksheet;
begin
  // Check worksheet name
  if not ReplaceDuplicateName and (GetWorksheetByName(AName) <> nil) then
    raise EFPSpreadsheet.CreateFmt(rsDuplicateWorksheetName, [AName]);
  if not ValidWorksheetName(AName, ReplaceDuplicateName) then
    raise EFPSpreadsheet.CreateFmt(rsInvalidWorksheetName, [AName]);

  // Create worksheet...
  Result := TsWorksheet.Create;

  // Add it to the internal worksheet list
  FWorksheets.Add(Pointer(Result));

  // Remember the workbook to which it belongs (This must occur before
  // setting the workbook name because the workbook is needed there).
  Result.FWorkbook := Self;
  Result.FActiveCellRow := 0;
  Result.FActiveCellCol := 0;

  // Set the name of the new worksheet.
  // For this we turn off notification of listeners. This is not necessary here
  // because it will be repeated at end when OnAddWorksheet is executed below.
  inc(FNotificationLock);
  inc(FRebuildFormulaLock);
  try
    Result.Name := AName;
  finally
    dec(FNotificationLock);
    dec(FRebuildFormulaLock);
  end;

  // Send notification for new worksheet to listeners. They get the worksheet
  // name here as well.
  if (FNotificationLock = 0) and Assigned(FOnAddWorksheet) then
    FOnAddWorksheet(self, Result);

  // Make sure that there is an "active" worksheet
  if FActiveWorksheet = nil then
    SelectWorksheet(result);
end;

{@@ ----------------------------------------------------------------------------
  Copies a worksheet (even from an external workbook) and adds it to the
  current workbook

  @param  AWorksheet            Worksheet to be copied. Can be in a different
                                workbook.
  @param  ReplaceDuplicateName  The copied worksheet gets the name of the original.
                                If ReplaceDuplicateName is true and this sheet
                                name already exists then a number is added to
                                the sheet name to make it unique.
  @return The instance of the newly created worksheet
  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.CopyWorksheetFrom(AWorksheet: TsWorksheet;
  ReplaceDuplicateName: boolean): TsWorksheet;
var
  r, c: Cardinal;
  cell: PCell;
  col: PCol;
  row: PRow;
  i: Integer;
  w, h: Single;
  fnt: TsFont;
begin
  Result := nil;
  if (AWorksheet = nil) then
    exit;

  Result := AddWorksheet(AWorksheet.Name, ReplaceDuplicateName);
  inc(FNotificationLock);
  try
    // Make sure to use the same default font, colwidths depend on it!
    if Result.WorkBook <> AWorksheet.Workbook then
    begin
      fnt := AWorksheet.Workbook.GetDefaultFont;
      Result.Workbook.SetDefaultFont(fnt.FontName, fnt.Size);
    end;

    // Copy DefaultColWidth
    w := AWorksheet.ReadDefaultColWidth(Units);
    Result.WriteDefaultColWidth(w, Units);

    // Copy DefaultRowHeight
    h := AWorksheet.ReadDefaultRowHeight(Units);
    Result.WriteDefaultRowHeight(h, Units);

    // Copy cells (incl formulas, comments, hyperlinks etc).
    for cell in AWorksheet.Cells do
    begin
      r := cell^.Row;
      c := cell^.Col;
      Result.CopyCell(r, c, r, c, AWorksheet);
    end;

    // Copy col records
    for i := 0 to AWorksheet.Cols.Count-1 do
    begin
      col := AWorksheet.Cols[i];
      c := col^.Col;
      Result.CopyCol(c, c, AWorksheet);
    end;

    // Copy row records
    for i := 0 to AWorksheet.Rows.Count-1 do
    begin
      row := AWorksheet.Rows[i];
      r := row^.Row;
      Result.CopyRow(r, r, AWorksheet);
    end;
  finally
    dec(FNotificationLock);
  end;

  Result.ChangedCell(r, c);
end;

{@@ ----------------------------------------------------------------------------
  Quick helper routine which returns the first worksheet

  @return A TsWorksheet instance if at least one is present.
          nil otherwise.

  @see    TsWorkbook.GetWorksheetByIndex
  @see    TsWorkbook.GetWorksheetByName
  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.GetFirstWorksheet: TsWorksheet;
begin
  Result := TsWorksheet(FWorksheets.First);
end;

{@@ ----------------------------------------------------------------------------
  Quick helper routine which returns the last worksheet

  @return A TsWorksheet instance if at least one is present.
          nil otherwise.

  @see    TsWorkbook.GetWorksheetByIndex
  @see    TsWorkbook.GetWorksheetByName
  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.GetLastWorksheet: TsWorksheet;
begin
  Result := TsWorksheet(FWorksheets.Last);
end;


{@@ ----------------------------------------------------------------------------
  Returns the worksheet following the specified one.

  @return A TsWorksheet instance if the specified worksheet is not the last one
          nil otherwise.

  @see    TsWorkbook.GetFirstWorksheet
  @see    TsWorkbook.GetPreviousWorksheet
  @see    TsWorkbook.GetLastWorksheet
  @see    TsWorkbook.GetFirstWorksheet
  @see    TsWorkbook.GetWorksheetByIndex
  @see    TsWorkbook.GetWorksheetByName
  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.GetNextWorksheet(AWorksheet: TsWorksheet): TsWorksheet;
var
  idx: Integer;
begin
  idx := FWorksheets.Indexof(AWorksheet);
  if idx < FWorksheets.Count-1 then
    Result := TsWorksheet(FWorksheets.Items[idx + 1])
  else
    Result := nil;
end;

{@@ ----------------------------------------------------------------------------
  Returns the worksheet preceding the specified one.

  @return   A TsWorksheet instance if the specified worksheet is not
            the first one, nil otherwise.

  @see      TsWorkbook.GetFirstWorksheet
  @see      TsWorkbook.GetNextWorksheet
  @see      TsWorkbook.GetLastWorksheet
  @see      TsWorkbook.GetFirstWorksheet
  @see      TsWorkbook.GetWorksheetByIndex
  @see      TsWorkbook.GetWorksheetByName
  @see      TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.GetPreviousWorksheet(AWorksheet: TsWorksheet): TsWorksheet;
var
  idx: Integer;
begin
  idx := FWorksheets.IndexOf(AWorksheet);
  if idx > 0 then
    Result := TsWorksheet(FWorksheets.Items[idx - 1])
  else
    Result := nil;
end;

{@@ ----------------------------------------------------------------------------
  Gets the worksheet with a given index

  The index is zero-based, so the first worksheet
  added has index 0, the second 1, etc.

  @param  AIndex    The index of the worksheet (0-based)

  @return A TsWorksheet instance if one is present at that index.
          nil otherwise.

  @see    TsWorkbook.GetFirstWorksheet
  @see    TsWorkbook.GetWorksheetByName
  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.GetWorksheetByIndex(AIndex: Integer): TsWorksheet;
begin
  if (integer(AIndex) < FWorksheets.Count) and (integer(AIndex) >= 0) then
    Result := TsWorksheet(FWorksheets.Items[AIndex])
  else
    Result := nil;
end;

{@@ ----------------------------------------------------------------------------
  Gets the worksheet with a given worksheet name

  @param  AName    The name of the worksheet
  @return A TsWorksheet instance if one is found with that name,
          nil otherwise. Case is ignored.

  @see    TsWorkbook.GetFirstWorksheet
  @see    TsWorkbook.GetWorksheetByIndex
  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.GetWorksheetByName(AName: String): TsWorksheet;
var
  i:integer;
  s: String;
begin
  Result := nil;
  for i:=0 to FWorksheets.Count-1 do
  begin
    s := TsWorksheet(FWorksheets.Items[i]).Name;
    if UTF8CompareText(s, AName) = 0 then
    begin
      Result := TsWorksheet(FWorksheets.Items[i]);
      exit;
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  The number of worksheets on the workbook

  @see    TsWorksheet
-------------------------------------------------------------------------------}
function TsWorkbook.GetWorksheetCount: Integer;
begin
  Result := FWorksheets.Count;
end;

{@@ ----------------------------------------------------------------------------
  Counts the number of visible (= not hidden) worksheets
-------------------------------------------------------------------------------}
function TsWorkbook.GetVisibleWorksheetCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:=0 to GetWorksheetCount-1 do
    if not (soHidden in GetWorksheetByIndex(i).Options) then
      inc(Result);
end;

{@@ ----------------------------------------------------------------------------
  Returns the index of a worksheet in the worksheet list
-------------------------------------------------------------------------------}
function TsWorkbook.GetWorksheetIndex(AWorksheet: TsBasicWorksheet): Integer;
begin
  Result := FWorksheets.IndexOf(AWorksheet);
end;

{@@ ----------------------------------------------------------------------------
  Returns the index of the worksheet having the specified name, or -1 if the
  worksheet does not exist.
-------------------------------------------------------------------------------}
function TsWorkbook.GetWorksheetIndex(const AWorksheetName: String): Integer;
var
  s: String;
begin
  for Result := 0 to FWorksheets.Count-1 do
  begin
    s := TsWorksheet(FWorksheets[Result]).Name;
    if SameText(s, AWorksheetName) then
      exit;
  end;
  Result := -1;
end;

{@@ ----------------------------------------------------------------------------
  Clears the list of Worksheets and releases their memory.

  NOTE: This procedure conflicts with the WorkbookLink mechanism which requires
  at least 1 worksheet per workbook!
-------------------------------------------------------------------------------}
procedure TsWorkbook.RemoveAllWorksheets;
begin
  FActiveWorksheet := nil;
  FWorksheets.ForEachCall(RemoveWorksheetsCallback, nil);
  FWorksheets.Clear;
  if (FNotificationLock = 0) and Assigned(FOnRemoveWorksheet) then
    FOnRemoveWorksheet(self, -1);
end;

{@@ ----------------------------------------------------------------------------
  Removes all empty worksheets
-------------------------------------------------------------------------------}
procedure TsWorkbook.RemoveAllEmptyWorksheets;
var
  sheet: TsWorksheet;
  i: Integer;
begin
  for i:= FWorksheets.Count-1 downto 0 do
  begin
    sheet := TsWorksheet(FWorksheets.Items[i]);
    if sheet.IsEmpty then
      RemoveWorksheet(sheet);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Removes the specified worksheet: Removes the sheet from the internal sheet
  list, generates an event OnRemoveWorksheet, and releases all memory.
  The event handler specifies the index of the deleted worksheet; the worksheet
  itself does no longer exist.
-------------------------------------------------------------------------------}
procedure TsWorkbook.RemoveWorksheet(AWorksheet: TsWorksheet);
var
  i: Integer;
  rebuildFormulas: Boolean;
begin
  if GetWorksheetCount > 1 then     // There must be at least 1 worksheet left!
  begin
    i := GetWorksheetIndex(AWorksheet);
    if (i <> -1) and (AWorksheet <> nil) then
    begin
      if Assigned(FOnRemovingWorksheet) then
        FOnRemovingWorksheet(self, AWorksheet);
      rebuildFormulas := FixFormulas(fcWorksheetDeleted, AWorksheet, 0);
      FWorksheets.Delete(i);
      AWorksheet.Free;
      if rebuildFormulas then Self.RebuildFormulas;
      if boAutoCalc in Options then
        CalcFormulas;
      if Assigned(FOnRemoveWorksheet) then
        FOnRemoveWorksheet(self, i);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Makes the specified worksheet "active". Only needed for visual controls.
  The active worksheet is displayed in a TsWorksheetGrid and in the selected
  tab of a TsWorkbookTabControl.
-------------------------------------------------------------------------------}
procedure TsWorkbook.SelectWorksheet(AWorksheet: TsWorksheet);
begin
  if (AWorksheet <> nil) and (FWorksheets.IndexOf(AWorksheet) = -1) then
    raise EFPSpreadsheet.Create('[TsWorkbook.SelectSheet] Worksheet does not belong to the workbook');
  FActiveWorksheet := AWorksheet;
  if FReadWriteFlag = rwfRead then
    exit;
  if Assigned(FOnSelectWorksheet) then
    FOnSelectWorksheet(self, AWorksheet);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the passed string is a valid worksheet name according to Excel
  (ODS seems to be a bit less restrictive, but if we follow Excel's convention
  we always have valid sheet names independent of the format.

  @param   AName                Name to be checked.
  @param   ReplaceDuplicateName If there exists already a sheet name equal to
                                AName then a number is added to AName such that
                                the name is unique.
  @return  TRUE if it is a valid worksheet name, FALSE otherwise
-------------------------------------------------------------------------------}
function TsWorkbook.ValidWorksheetName(var AName: String;
  ReplaceDuplicateName: Boolean = false): Boolean;
// see: http://stackoverflow.com/questions/451452/valid-characters-for-excel-sheet-names
const
  INVALID_CHARS: set of char = ['[', ']', ':', '*', '?', '/', '\'];
var
  unique: Boolean;
  ch: char;
  i: Integer;
begin
  Result := false;

  // Name must not be empty
  if (AName = '') then
    exit;

  { wp: the length restriction has been moved to the writer...

  // Length must be less than 31 characters
  if UTF8Length(AName) > 31 then
    exit;
  }
  // Name must not contain any of the INVALID_CHARS
  for ch in AName  do
    if ch in INVALID_CHARS then
      exit;

  // Name must be unique
  unique := (GetWorksheetByName(AName) = nil);
  if not unique then
  begin
    if ReplaceDuplicateName then
    begin
      i := 0;
      repeat
        inc(i);
        unique := (GetWorksheetByName(AName + IntToStr(i)) = nil);
      until unique;
      AName := AName + IntToStr(i);
    end else
      exit;
  end;

  Result := true;
end;


{ String-to-cell/range conversion }

{@@ ----------------------------------------------------------------------------
  Analyses a string which can contain an array of cell ranges along with a
  worksheet name. Extracts the worksheet (if missing the "active" worksheet of
  the workbook is returned) and the cell's row and column indexes.

  @param  AText        General cell range string in Excel notation,
                       i.e. worksheet name + ! + cell in A1 notation.
                       Example: Sheet1!A1:A10; A1:A10 or A1 are valid as well.
  @param  AWorksheet   Pointer to the worksheet referred to by AText. If AText
                       does not contain the worksheet name, the active worksheet
                       of the workbook is returned
  @param  ARow, ACol   Zero-based row and column index of the cell identified
                       by ATest. If AText contains one ore more cell ranges
                       then the upper left corner of the first range is returned.
  @param  AListSeparator  Character to separate the cell blocks in the text
                       If #0 then the ListSeparator of the workbook's FormatSettings
                       is used.
  @returns TRUE if AText is a valid list of cell ranges, FALSE if not. If the
           result is FALSE then AWorksheet, ARow and ACol may have unpredictable
           values.
-------------------------------------------------------------------------------}
function TsWorkbook.TryStrToCell(AText: String; out AWorksheet: TsWorksheet;
  out ARow,ACol: Cardinal; AListSeparator: Char = #0): Boolean;
var
  ranges: TsCellRangeArray;
begin
  Result := TryStrToCellRanges(AText, AWorksheet, ranges, AListSeparator);
  if Result then
  begin
    ARow := ranges[0].Row1;
    ACol := ranges[0].Col1;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Analyses a string which can contain an array of cell ranges along with a
  worksheet name. Extracts the worksheet (if missing the "active" worksheet of
  the workbook is returned) and the cell range (or the first cell range, if there
  are several ranges).

  @param  AText        General cell range string in Excel notation,
                       i.e. worksheet name + ! + cell in A1 notation.
                       Example: Sheet1!A1:A10; A1:A10 or A1 are valid as well.
  @param  AWorksheet   Pointer to the worksheet referred to by AText. If AText
                       does not contain the worksheet name, the active worksheet
                       of the workbook is returned
  @param  ARange       TsCellRange records identifying the cell block. If AText
                       contains several cell ranges the first one is returned.
  @param  AListSeparator  Character to separate the cell blocks in the text
                       If #0 then the ListSeparator of the workbook's FormatSettings
                       is used.
  @returns TRUE if AText is a valid cell range, FALSE if not. If the
           result is FALSE then AWorksheet and ARange may have unpredictable
           values.
-------------------------------------------------------------------------------}
function TsWorkbook.TryStrToCellRange(AText: String; out AWorksheet: TsWorksheet;
  out ARange: TsCellRange; AListSeparator: Char = #0): Boolean;
var
  ranges: TsCellRangeArray;
begin
  Result := TryStrToCellRanges(AText, AWorksheet, ranges, AListSeparator);
  if Result then ARange := ranges[0];
end;

{@@ ----------------------------------------------------------------------------
  Analyses a string which can contain an array of cell ranges along with a
  worksheet name. Extracts the worksheet (if missing the "active" worksheet of
  the workbook is returned) and the range array.

  @param  AText        General cell range string in Excel notation,
                       i.e. worksheet name + ! + cell in A1 notation.
                       Example: Sheet1!A1:A10; A1:A10 or A1 are valid as well.
  @param  AWorksheet   Pointer to the worksheet referred to by AText. If AText
                       does not contain the worksheet name, the active worksheet
                       of the workbook is returned
  @param  ARanges      Array of TsCellRange records identifying the cell blocks
  @param  AListSeparator  Character to separate the cell blocks in the text
                       If #0 then the ListSeparator of the workbook's FormatSettings
                       is used.
  @returns TRUE if AText is a valid list of cell ranges, FALSE if not. If the
           result is FALSE then AWorksheet and ARanges may have unpredictable
           values.
-------------------------------------------------------------------------------}
function TsWorkbook.TryStrToCellRanges(AText: String; out AWorksheet: TsWorksheet;
  out ARanges: TsCellRangeArray; AListSeparator: Char = #0): Boolean;
var
  i: Integer;
  L: TStrings;
  sheetname: String;
begin
  Result := false;
  AWorksheet := nil;
  ARanges := nil;

  if AText = '' then
    exit;

  i := pos(SHEETSEPARATOR, AText);
  if i = 0 then
    AWorksheet := FActiveWorksheet
  else begin
    sheetname := Copy(AText, 1, i-1);
    if (sheetname <> '') and (sheetname[1] = '''') then
      Delete(sheetname, 1, 1);
    if (sheetname <> '') and (sheetname[Length(sheetname)] = '''') then
      Delete(sheetname, Length(sheetname), 1);
    AWorksheet := GetWorksheetByName(sheetname);
    if AWorksheet = nil then
      exit;
    AText := Copy(AText, i+1, Length(AText));
  end;

  L := TStringList.Create;
  try
    if AListSeparator = #0 then
      L.Delimiter := FormatSettings.ListSeparator
    else
      L.Delimiter := AListSeparator;
    L.StrictDelimiter := true;
    L.DelimitedText := AText;
    if L.Count = 0 then
    begin
      AWorksheet := nil;
      exit;
    end;
    SetLength(ARanges, L.Count);
    for i:=0 to L.Count-1 do begin
      if pos(':', L[i]) = 0 then begin
        Result := ParseCellString(L[i], ARanges[i].Row1, ARanges[i].Col1);
        if Result then begin
          ARanges[i].Row2 := ARanges[i].Row1;
          ARanges[i].Col2 := ARanges[i].Col1;
        end;
      end else
        Result := ParseCellRangeString(L[i], ARanges[i]);
      if not Result then begin
        SetLength(ARanges, 0);
        AWorksheet := nil;
        exit;
      end;
    end;
  finally
    L.Free;
  end;
end;


{@@ ----------------------------------------------------------------------------
  Calculates all formulas of the workbook.

  Since formulas may reference not-yet-calculated cells, this occurs in
  two steps:
  1. All formulas are marked as "not calculated".
  2. Formulas are calculated. If referenced formulas are found as being
     "not calculated" they are calculated and then tagged as "calculated".
  This results in an iterative calculation procedure. In the end, all formulas
  are calculated. This strategy is often very ineffective because it
  unnecessarily recalculates formulas. You can provide a different algorithm in
  the OnCalcWorkbook event.
-------------------------------------------------------------------------------}
procedure TsWorkbook.CalcFormulas;
var
  formula: PsFormula;
  sheet: TsWorksheet;
  p: Pointer;
begin
  if (boIgnoreFormulas in Options) then
    exit;

  inc(FCalculationLock);
  try
    if Assigned(FOnCalcWorkbook) then
    begin
      FOnCalcWorkbook(self);
      exit;
    end;

    // Step1 - mark all formulas as "not calculated"
    for p in FWorksheets do begin
      sheet := TsWorksheet(p);
      for formula in sheet.Formulas do
        formula^.CalcState := csNotCalculated;
    end;

    // Step 2 - calculate formulas. If the formula calculted requires another
    // the result of another formula not yet calculated this formula is
    // calculated immediately.
    for p in FWorksheets do begin
      sheet := TsWorksheet(p);
      for formula in sheet.Formulas do
        sheet.CalcFormula(formula);
    end;

  finally
    dec(FCalculationLock);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Something was changed anywhere in the workbook which has an effect on existing
  formulas. This procedure runs through all formulas and performs the
  correction.

  @param  ACorrection   Describes what has to be corrected.
                        Example: fcWorksheetRenamed means that a worksheet has
                        been renamed and the new name must be used in
                        corresponding formulas
  @param AData          A pointer with further information on the correction to
                        be made. Depends on ACorrection.
                        Example:
                        In the fcWorksheetRenamed example above this points to
                        the worksheet that was renamed.
  @param AParam         Provides additional information. Depends on ACorrection
  @return               The function returns true if the string formulas of the
                        workbook have to be recreated.
-------------------------------------------------------------------------------}
function TsWorkbook.FixFormulas(ACorrection: TsFormulaCorrection;
  AData: Pointer; AParam: PtrInt): Boolean;
var
  i: Integer;
  sheet: TsWorksheet;
  formula: PsFormula;
begin
  if (boIgnoreFormulas in Options) then
    exit;

  Result := false;
  inc(FCalculationLock);
  try
    for i := 0 to GetWorksheetCount-1 do begin
      sheet := GetWorksheetByIndex(i);
      for formula in sheet.Formulas do
        Result := FixFormula(formula, ACorrection, AData, AParam);
    end;
  finally
    dec(FCalculationLock);
    {
    if (boAutoCalc in Options) and formulaChanged then
      CalcFormulas;
    }
  end;
end;

procedure TsWorkbook.RebuildFormulas;
begin
  if FRebuildFormulaLock = 0 then
    FWorksheets.ForEachCall(RebuildFormulasCallback, nil);
end;

procedure TsWorkbook.LockFormulas;
begin
  inc(FDeleteFormulaLock);
end;

procedure TsWorkbook.UnlockFormulas;
begin
  dec(FDeleteFormulaLock);
end;


{ AData points to the deleted worksheet }
procedure FixWorksheetDeletedCallback(ANode: TsExprNode; AData1, AData2: Pointer;
  var MustRebuildFormulas: Boolean);
var
  deletedindex: Integer;
  deletedSheet: TsWorksheet;
  cellNode: TsCellExprNode;
  rngNode: TsCellRangeExprNode;
  index, index1, index2: Integer;
begin
  Unused(AData2);

  if ANode is TsCellExprNode then
  begin
    cellNode := TsCellExprNode(ANode);
    deletedSheet := TsWorksheet(AData1);
    deletedindex := TsWorkbook(cellNode.GetWorkbook).GetWorksheetIndex(deletedSheet);
    index := cellNode.GetSheetIndex;
    if deletedindex < index then begin
      cellNode.SetSheetIndex(index-1);
      MustRebuildFormulas := true;
    end else
    if deletedIndex = index then begin
      cellNode.Error := errIllegalRef;
      MustRebuildFormulas := true;
    end;
  end else
  if ANode is TsCellRangeExprNode then
  begin
    rngNode := TsCellRangeExprNode(ANode);
    deletedSheet := TsWorksheet(AData1);
    deletedIndex := TsWorkbook(rngNode.GetWorkbook).GetWorksheetIndex(deletedSheet);
    index1 := rngNode.GetSheetIndex(1);
    index2 := rngNode.GetSheetIndex(2);
    if deletedIndex < index1 then begin
      rngNode.SetSheetIndex(1, index1-1);
      rngNode.SetSheetIndex(2, index2-1);
      MustRebuildFormulas := true;
    end else
    if (deletedIndex > index1) and (deletedIndex < index2) then begin
      rngNode.SetSheetIndex(2, index2-1);
      MustRebuildFormulas := true;
    end else
    if (deletedIndex = index1) and (index1 <> index2) then begin
      rngNode.SetSheetIndex(2, index2-1);
      MustRebuildFormulas := true;
    end else
    if (deletedIndex = index2) and (index1 <> index2) then begin
      rngNode.SetSheetIndex(2, index2-1);
      MustRebuildFormulas := true;
    end else
    if (deletedIndex = index1) and (deletedIndex = index2) then begin
      rngNode.Error := errIllegalRef;
      MustRebuildFormulas := true;
    end;
  end;
end;

function TsWorkbook.FixFormula(AFormula: PsFormula;
  ACorrection: TsFormulaCorrection; AData: Pointer; AParam: PtrInt): Boolean;
begin
  Unused(AParam);   // Maybe later...

  Result := false;
  case ACorrection of
    fcWorksheetRenamed:
      Result := true; // Nothing to do, no sheet names in formula nodes
    fcWorksheetDeleted:
      Result := AFormula^.Parser.IterateNodes(FixWorksheetDeletedCallback, AData, nil);
  end;
end;

procedure TsWorkbook.MoveSheet(AFromIndex, AToIndex: Integer);
begin
  FWorksheets.Move(AFromIndex, AToIndex);
  if Assigned(FOnChangeWorksheet) then
    FOnChangeWorksheet(Self, GetWorksheetByIndex(AToIndex));
end;


{$include fpspreadsheet_fmt.inc}         // cell formatting
{$include fpspreadsheet_fonts.inc}       // fonts
{$include fpspreadsheet_numfmt.inc}      // number formats
{$include fpspreadsheet_cf.inc}          // conditional formatting
{$include fpspreadsheet_comments.inc}    // comments
{$include fpspreadsheet_hyperlinks.inc}  // hyperlinks
{$include fpspreadsheet_embobj.inc}      // embedded objects
{$include fpspreadsheet_clipbrd.inc}     // clipboard access


end.   {** End Unit: fpspreadsheet }

