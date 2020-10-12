{
fpsopendocument.pas

Writes an OpenDocument 1.0 Spreadsheet document

An OpenDocument document is a compressed ZIP file with the following files inside:

content.xml     - Actual contents
meta.xml        - Authoring data
settings.xml    - User persistent viewing information, such as zoom, cursor position, etc.
styles.xml      - Styles, which are the only way to do formatting
mimetype        - application/vnd.oasis.opendocument.spreadsheet
META-INF\manifest.xml  - Describes the other files in the archive

Specifications obtained from:

http://docs.oasis-open.org/office/v1.1/OS/OpenDocument-v1.1.pdf

AUTHORS: Felipe Monteiro de Carvalho / Jose Luis Jurado Rincon / Werner Pamler

NOTICE: Active define FPSpreadDebug in the project options to get a log during
  reading/writing.
}


unit fpsOpenDocument;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$I ..\fps.inc}

interface

uses
  Classes, SysUtils,
  laz2_xmlread, laz2_DOM,
  avglvltree, math, dateutils, contnrs,
 {$IF FPC_FULLVERSION >= 20701}
  zipper,
 {$ELSE}
  fpszipper,
 {$ENDIF}
  fpstypes, fpsReaderWriter, fpsutils, fpsHeaderFooterParser,
  fpsNumFormat, fpsxmlcommon, fpsPagelayout;
  
type
  TDateModeODS=(
    dmODS1899 {default for ODF; almost same as Excel 1900},
    dmODS1900 {StarCalc legacy only},
    dmODS1904 {e.g. Quattro Pro, Mac Excel compatibility}
  );

  { TsSpreadOpenDocHeaderFooterParser }

  TsSpreadOpenDocHeaderFooterParser = class(TsHeaderFooterParser)
  private
    FNode: TDOMNode;
    XMLMode: Boolean;
  protected
    procedure AddNodeElement(ANode: TDOMNode);
    function FindStyle(AStyleName: String): Integer;
    function GetCurrFontIndex: Integer; override;
    procedure Parse; override;
  public
    constructor Create(ANode: TDOMNode; AFontList: TList;
      ADefaultFont: TsHeaderFooterFont); overload;
    function BuildHeaderFooterAsXMLString: String;
  end;

  { TsSpreadOpenDocNumFormatParser }

  TsSpreadOpenDocNumFormatParser = class(TsNumFormatParser)
  protected
    function BuildXMLAsStringFromSection(ASection: Integer;
      AFormatName: String): String;
  public
    function BuildXMLAsString(AFormatName: String): String;
  end;

  { TsSpreadOpenDocReader }

  TsSpreadOpenDocReader = class(TsSpreadXMLReader)
  private
    FTableStyleList: TFPList;
    FColumnStyleList: TFPList;
    FColumnList: TFPList;
    FRowStyleList: TFPList;
    FRowList: TFPList;
    FPageLayoutList: TFPList;
    FMasterPageList: TFPList;
    FHeaderFooterFontList: TObjectList;
    FActiveSheet: String;
    FDateMode: TDateModeODS;
    FFontFaces: TStringList;
    FRichTextFontList: TFPList;
    FRepeatedCols: TsRowColRange;
    FRepeatedRows: TsRowColRange;
    procedure ApplyColData;
    procedure ApplyStyleToCell(ACell: PCell; AStyleIndex: Integer);
    function ApplyStyleToCell(ACell: PCell; AStyleName: String): Boolean;
    function ApplyTableStyle(ASheet: TsBasicWorksheet;
      AStyleName: String): Boolean;
    function ExtractBoolFromNode(ANode: TDOMNode): Boolean;
    function ExtractDateTimeFromNode(ANode: TDOMNode;
      ANumFormat: TsNumberFormat; const AFormatStr: String): TDateTime;
    function ExtractErrorFromNode(ANode: TDOMNode; out AErrorValue: TsErrorValue): Boolean;
    function ExtractFormatIndexFromStyle(ACellStyleName: String; ACol: Integer): Integer;
    function FindColumnByCol(AColIndex: Integer): Integer;
    function FindColStyleByName(AStyleName: String): integer;
    function FindNumFormatByName(ANumFmtName: String): Integer;
    function FindRowStyleByName(AStyleName: String): Integer;
    function FindTableStyleByName(AStyleName: String): Integer;
    procedure ReadCell(ANode: TDOMNode; ARow, ACol: Integer;
      AFormatIndex: Integer; out AColsRepeated: Integer);
    procedure ReadCellImages(ANode: TDOMNode; ARow, ACol: Cardinal);
    procedure ReadCFCellFormat(ANode: TDOMNode; ASheet: TsBasicWorksheet; ARange: TsCellRange);
    procedure ReadCFColorScale(ANode: TDOMNode; ASheet: TsBasicWorksheet; ARange: TsCellRange);
    procedure ReadCFDataBars(ANode: TDOMNode; ASheet: TsBasicWorksheet; ARange: TsCellRange);
    procedure ReadCFDateFormat(ANode: TDOMNode; ASheet: TsBasicWorksheet; ARange: TsCellRange);
    procedure ReadCFIconSet(ANode: TDOMNode; ASheet: TsBasicWorksheet; ARange: TsCellRange);
    procedure ReadColumns(ATableNode: TDOMNode);
    procedure ReadColumnStyle(AStyleNode: TDOMNode);
    procedure ReadConditionalFormats(ANode: TDOMNode; AWorksheet: TsBasicWorksheet);
    procedure ReadDateMode(SpreadSheetNode: TDOMNode);
    procedure ReadDocumentProtection(ANode: TDOMNode);
    procedure ReadFont(ANode: TDOMNode; var AFontName: String;
      var AFontSize: Single; var AFontStyle: TsFontStyles; var AFontColor: TsColor;
      var AFontPosition: TsFontPosition);
//    function ReadFont(ANode: TDOMnode; APreferredIndex: Integer = -1): Integer;
    procedure ReadFontFaces(ANode: TDOMNode);
    procedure ReadHeaderFooterFont(ANode: TDOMNode; var AFontName: String;
      var AFontSize: Double; var AFontStyle: TsHeaderFooterFontStyles;
      var AFontColor: TsColor);
    function ReadHeaderFooterText(ANode: TDOMNode): String;
    procedure ReadMetaData(ANode: TDOMNode);
    procedure ReadPictures(AStream: TStream);
    procedure ReadPrintRanges(ATableNode: TDOMNode; ASheet: TsBasicWorksheet);
    procedure ReadRowsAndCells(ATableNode: TDOMNode);
    procedure ReadRowStyle(AStyleNode: TDOMNode);
    procedure ReadShape(ANode: TDOMNode; ARow: Cardinal = UNASSIGNED_ROW_COL_INDEX;
      ACol: Cardinal = UNASSIGNED_ROW_COL_INDEX);
    procedure ReadShapes(ATableNode: TDOMNode);
    procedure ReadSheetProtection(ANode: TDOMNode; ASheet: TsBasicWorksheet);
    procedure ReadSheets(ANode: TDOMNode);
    procedure ReadStyle_ParagraphProperties(ANode: TDOMNode; var AFormat: TsCellFormat);
    procedure ReadStyle_TableCellProperties(ANode: TDOMNode; var AFormat: TsCellFormat);
    procedure ReadStyle_TextProperties(ANode: TDOMNode; AStyleName: String;
      AFont: TsFont; var AFormat: TsCellFormat);
    procedure ReadTableStyle(AStyleNode: TDOMNode);

  protected
    FPointSeparatorSettings: TFormatSettings;
    procedure AddBuiltinNumFormats; override;
    procedure ReadAutomaticStyles(AStylesNode: TDOMNode);
    procedure ReadMasterStyles(AStylesNode: TDOMNode);
    procedure ReadNumFormats(AStylesNode: TDOMNode);
    procedure ReadPageLayout(AStylesNode: TDOMNode; ATableStyleName: String;
      APageLayout: TsPageLayout);
    procedure ReadSettings(AOfficeSettingsNode: TDOMNode);
    procedure ReadStyles(AStylesNode: TDOMNode);
    { Record writing methods }
    procedure ReadBlank(ARow, ACol: Cardinal;
      AStyleIndex: Integer; ACellNode: TDOMNode); reintroduce;
    procedure ReadBoolean(ARow, ACol: Cardinal;
      AStyleIndex: Integer; ACellNode: TDOMNode);
    procedure ReadComment(ARow, ACol: Cardinal; ACellNode: TDOMNode);
    procedure ReadDateTime(ARow, ACol: Cardinal;
      AStyleIndex: Integer; ACellNode: TDOMNode);
    procedure ReadError(ARow, ACol: Cardinal; AStyleIndex: Integer;
      ACellNode: TDOMNode);
    procedure ReadFormula(ARow, ACol: Cardinal; AstyleIndex: Integer;
      ACellNode: TDOMNode); reintroduce;
    procedure ReadLabel(ARow, ACol: Cardinal; AStyleIndex: Integer;
      ACellNode: TDOMNode); reintroduce;
    procedure ReadNumber(ARow, ACol: Cardinal; AStyleIndex: Integer;
      ACellNode: TDOMNode); reintroduce;

  public
    constructor Create(AWorkbook: TsBasicWorkbook); override;
    destructor Destroy; override;

    { File format detection }
    class function CheckFileFormat(AStream: TStream): Boolean; override;

    { General reading methods }
    procedure ReadFromStream(AStream: TStream;
      APassword: String = ''; AParams: TsStreamParams = []); override;
  end;

  { TsSpreadOpenDocWriter }

  TsSpreadOpenDocWriter = class(TsCustomSpreadWriter)
  private
    FColumnStyleList: TFPList;
    FRowStyleList: TFPList;
    FRichTextFontList: TStringList;
    FHeaderFooterFontList: TObjectList;
    FHasColFormats: Boolean;
    FHasRowFormats: Boolean;

    // Routines to write parts of files
    procedure WriteAutomaticStyles(AStream: TStream);
    procedure WriteCellRow(AStream: TStream; ASheet: TsBasicWorksheet;
      ARowIndex, ALastColIndex: Integer);
    procedure WriteCellStyles(AStream: TStream);
    procedure WriteColStyles(AStream: TStream);
    procedure WriteColumns(AStream: TStream; ASheet: TsBasicWorksheet);
    procedure WriteConditionalFormats(AStream: TStream; ASheet: TsBasicWorksheet);
    procedure WriteConditionalStyles(AStream: TStream);
    procedure WriteEmptyRow(AStream: TStream; ASheet: TsBasicWorksheet;
      ARowIndex, AFirstColIndex, ALastColIndex, ALastRowIndex: Integer;
      out ARowsRepeated: Integer);
    procedure WriteFontNames(AStream: TStream);
    procedure WriteMasterStyles(AStream: TStream);
    procedure WriteNamedExpressions(AStream: TStream; ASheet: TsBasicWorksheet);
    procedure WriteNumFormats(AStream: TStream);
    procedure WriteOfficeStyles(AStream: TStream);
    procedure WriteRowStyles(AStream: TStream);
    procedure WriteRowsAndCells(AStream: TStream; ASheet: TsBasicWorksheet);
    procedure WriteShapes(AStream: TStream; ASheet: TsBasicWorksheet);
    procedure WriteStyleNode(AStream: TStream; const AStyleName: String;
      const AFormat: TsCellFormat; AConditionalFormatIndex: Integer);
    procedure WriteTableSettings(AStream: TStream);
    procedure WriteTableStyles(AStream: TStream);
    procedure WriteTextStyles(AStream: TStream);
    procedure WriteVirtualCells(AStream: TStream; ASheet: TsBasicWorksheet);

    function WriteBackgroundColorStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WriteBiDiModeStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WriteBorderStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WriteCellProtectionStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WriteCommentXMLAsString(AComment: String): String;
    function WriteConditionalStyleXMLAsString(ACFIndex: Integer): String;
    function WriteDefaultFontXMLAsString: String;
    function WriteDefaultGraphicStyleXMLAsString: String; overload;
    function WriteDocumentProtectionXMLAsString: String;
    function WriteFontStyleXMLAsString(const AFormat: TsCellFormat): String; overload;
    function WriteFontStyleXMLAsString(AFont: TsFont): String; overload;
    function WriteHeaderFooterFontXMLAsString(AFont: TsHeaderFooterFont): String;
    function WriteHorAlignmentStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WriteNumFormatStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WritePageLayoutXMLAsString(AStyleName: String; const APageLayout: TsPageLayout): String;
    function WritePrintRangesXMLAsString(ASheet: TsBasicWorksheet): String;
    function WriteSheetProtectionXMLAsString(ASheet: TsBasicWorksheet): String;
    function WriteSheetProtectionDetailsXMLAsString(ASheet: TsBasicWorksheet): String;
    function WriteTextRotationStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WriteVertAlignmentStyleXMLAsString(const AFormat: TsCellFormat): String;
    function WriteWordwrapStyleXMLAsString(const AFormat: TsCellFormat): String;

  protected
    FPointSeparatorSettings: TFormatSettings;
    // Streams with the contents of files
    FSMeta, FSSettings, FSStyles, FSContent: TStream;
    FSMimeType, FSMetaInfManifest: TStream;

    { Helpers }
    procedure AddBuiltinNumFormats; override;
    procedure CreateStreams;
    procedure DestroyStreams;
    function FindRowStyle(ASheet: TsBasicWorksheet; ARowIndex: Integer): Integer;
    procedure GetHeaderFooterImageName(APageLayout: TsPageLayout;
      out AHeader, AFooter: String);
    procedure GetHeaderFooterImagePosStr(APagelayout: TsPageLayout;
      out AHeader, AFooter: String);
    function GetStyleName(ACell: PCell): String;
    {
    procedure GetRowStyleAndHeight(ASheet: TsBasicWorksheet; ARowIndex: Integer;
      out AStyleName: String; out AHeight: Single);
    }
    procedure InternalWriteToStream(AStream: TStream);
    procedure ListAllColumnStyles;
    procedure ListAllHeaderFooterFonts;
    procedure ListAllNumFormats; override;
    procedure ListAllRowStyles;
    procedure ResetStreams;

    { Routines to write those files }
    procedure WriteContent;
    procedure WriteMetaInfManifest;
    procedure WriteMeta;
    procedure WriteMimetype;
    procedure WriteSettings;
    procedure WriteStyles;
    procedure WriteWorksheet(AStream: TStream; ASheetIndex: Integer);
    procedure ZipPictures(AZip: TZipper);

    { Record writing methods }
    procedure WriteBlank(AStream: TStream; const ARow, ACol: Cardinal;
      ACell: PCell); override;
    procedure WriteBool(AStream: TStream; const ARow, ACol: Cardinal;
      const AValue: Boolean; ACell: PCell); override;
    procedure WriteDateTime(AStream: TStream; const ARow, ACol: Cardinal;
      const AValue: TDateTime; ACell: PCell); override;
    procedure WriteError(AStream: TStream; const ARow, ACol: Cardinal;
      const AValue: TsErrorValue; ACell: PCell); override;
    procedure WriteFormula(AStream: TStream; const ARow, ACol: Cardinal;
      ACell: PCell); override;
    procedure WriteLabel(AStream: TStream; const ARow, ACol: Cardinal;
      const AValue: string; ACell: PCell); override;
    procedure WriteNumber(AStream: TStream; const ARow, ACol: Cardinal;
      const AValue: double; ACell: PCell); override;

  public
    constructor Create(AWorkbook: TsBasicWorkbook); override;
    destructor Destroy; override;

    { General writing methods }
    procedure WriteStringToFile(AString, AFileName: string);
    procedure WriteToStream(AStream: TStream; AParams: TsStreamParams = []); override;
  end;

procedure InitOpenDocLimitations(out ALimitations: TsSpreadsheetFormatLimitations);

var
  sfidOpenDocument: TsSpreadFormatID;

implementation

uses
 {$IFDEF FPSpreadDebug}
  LazLogger,
 {$ENDIF}
  StrUtils, Variants, LazFileUtils, URIParser, LazUTF8,
 {$IFDEF FPS_VARISBOOL}
  fpsPatches,
 {$ENDIF}
  fpsStrings, fpsStreams, fpsCrypto, fpsClasses, fpspreadsheet,
  fpsExprParser, fpsImages, fpsConditionalFormat;

const
  { OpenDocument general XML constants }
  XML_HEADER             = '<?xml version="1.0" encoding="utf-8" ?>';

  { OpenDocument Directory structure constants }
  OPENDOC_PATH_CONTENT   = 'content.xml';
  OPENDOC_PATH_META      = 'meta.xml';
  OPENDOC_PATH_SETTINGS  = 'settings.xml';
  OPENDOC_PATH_STYLES    = 'styles.xml';
  OPENDOC_PATH_MIMETYPE  = 'mimetype';
  {%H-}OPENDOC_PATH_METAINF   = 'META-INF' + '/';
  {%H-}OPENDOC_PATH_METAINF_MANIFEST = 'META-INF' + '/' + 'manifest.xml';

  { OpenDocument schemas constants }
  SCHEMAS_XMLNS_OFFICE   = 'urn:oasis:names:tc:opendocument:xmlns:office:1.0';
  SCHEMAS_XMLNS_DCTERMS  = 'http://purl.org/dc/terms/';
  SCHEMAS_XMLNS_META     = 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0';
  SCHEMAS_XMLNS          = 'http://schemas.openxmlformats.org/officeDocument/2006/extended-properties';
  SCHEMAS_XMLNS_CONFIG   = 'urn:oasis:names:tc:opendocument:xmlns:config:1.0';
  SCHEMAS_XMLNS_OOO      = 'http://openoffice.org/2004/office';
  SCHEMAS_XMLNS_DRAW     = 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0';
  SCHEMAS_XMLNS_MANIFEST = 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0';
  SCHEMAS_XMLNS_FO       = 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0';
  SCHEMAS_XMLNS_STYLE    = 'urn:oasis:names:tc:opendocument:xmlns:style:1.0';
  SCHEMAS_XMLNS_SVG      = 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0';
  SCHEMAS_XMLNS_TABLE    = 'urn:oasis:names:tc:opendocument:xmlns:table:1.0';
  SCHEMAS_XMLNS_TEXT     = 'urn:oasis:names:tc:opendocument:xmlns:text:1.0';
  SCHEMAS_XMLNS_V        = 'urn:schemas-microsoft-com:vml';
  SCHEMAS_XMLNS_XLINK    = 'http://www.w3.org/1999/xlink';
  {%H-}SCHEMAS_XMLNS_NUMBER   = 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0';
  {%H-}SCHEMAS_XMLNS_CHART    = 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0';
  {%H-}SCHEMAS_XMLNS_DR3D     = 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0';
  {%H-}SCHEMAS_XMLNS_MATH     = 'http://www.w3.org/1998/Math/MathML';
  {%H-}SCHEMAS_XMLNS_FORM     = 'urn:oasis:names:tc:opendocument:xmlns:form:1.0';
  {%H-}SCHEMAS_XMLNS_SCRIPT   = 'urn:oasis:names:tc:opendocument:xmlns:script:1.0';
  {%H-}SCHEMAS_XMLNS_OOOW     = 'http://openoffice.org/2004/writer';
  {%H-}SCHEMAS_XMLNS_OOOC     = 'http://openoffice.org/2004/calc';
  {%H-}SCHEMAS_XMLNS_DOM      = 'http://www.w3.org/2001/xml-events';
  {%H-}SCHEMAS_XMLNS_XFORMS   = 'http://www.w3.org/2002/xforms';
  {%H-}SCHEMAS_XMLNS_XSD      = 'http://www.w3.org/2001/XMLSchema';
  {%H-}SCHEMAS_XMLNS_XSI      = 'http://www.w3.org/2001/XMLSchema-instance';

  { DATEMODE similar to but not the same as XLS format; used in time only values. }
  DATEMODE_1899_BASE=0; //apparently 1899-12-30 for ODF in FPC DateTime;
  // due to Excel's leap year bug, the date floats in the spreadsheets are the same starting
  // 1900-03-01
  DATEMODE_1900_BASE=2; //StarCalc compatibility, 1900-01-01 in FPC DateTime
  DATEMODE_1904_BASE=1462; //1/1/1904 in FPC TDateTime

const
  // lsThin, lsMedium, lsDashed, lsDotted, lsThick, lsDouble, lsHair
  // lsMediumDash, lsDashDot, lsMediumDashDot, lsDashDotDot, lsMediumDashDotDot, lsSlantDashDot
  BORDER_LINESTYLES: array[TsLineStyle] of string = (
    'solid', 'solid', 'dashed', 'fine-dashed', 'solid', 'double-thin', 'dotted',
    'dashed', 'dash-dot', 'dash-dot', 'dash-dot-dot', 'dash-dot-dot', 'dash-dot'
    );
  BORDER_LINEWIDTHS: array[TsLinestyle] of string =
    ('0.74pt', '1.76pt', '0.74pt', '0.74pt', '2.49pt', '0.74pt', '0.74pt',
     '1.76pt', '0.74pt', '1.76pt', '0.74pt', '1.76pt', '1.76pt');

  FALSE_TRUE: array[boolean] of String = ('false', 'true');

  PAGE_BREAK: array[boolean] of string = ('auto', 'page');

  COLWIDTH_EPS  = 1e-3;
  ROWHEIGHT_EPS = 1e-3;

  CF_STYLE_OP: array[TsCFCondition] of string = (
    'cell-content()=%s', 'cell-content()!=%s',              // cfcEqual, cfcNotEqual
    'cell-content()&gt;%s', 'cell-content()&lt;%s',         //cfcGreaterThan, cfcLessThan
    'cell-content()&gt;=%s', 'cell-content&lt;=%s',         // cfcGreaterEqual, cfdLessEqual
    'cell-is-between(%s,%s)', 'cell-is-not-between(%s,%s)', // cfcBetween, cfcNotBetween,
    '', '', '', '',   // cfcAboveAverage, cfcBelowAverage, cfcAboveEqualAverage, cfcBelowEqualAverage
    '', '', '', '',   // cfcTop, cfcBottom, cfcTopPercent, cfcBottomPercent,
    '', '',           // cfcDuplicate, cfcUnique,
    '', '', '', '',   // cfcBeginsWith, cfcEndsWith, cfcContainsText, cfcNotContainsText,
    '', '',           // cfcContainsErrors, cfcNotContainsErrors
    '', '', '', '',   // cfcYesterday .. cfcLast7Days
    '', '', '',       // cfcLastWeek .. cfcNextWeek
    '', '', '',       // cfcLastMonth .. cfcNextMonth
    '', '', '',       // cfcLastYear .. cfcNextYear
    'is-true-formula(%s)'  // cfcExpression
  );

  CF_CALCEXT_OP: array[TsCFCondition] of string = (
    '=%s', '!=%s',                                // cfcEqual, cfcNotEqual
    '&gt;%s', '&lt;%s',                           //cfcGreaterThan, cfcLessThan
    '&gt;=%s', '&lt;=%s',                         // cfcGreaterEqual, cfdLessEqual
    'between(%s,%s)', 'not-between(%s,%s)',       // cfcBetween, cfcNotBetween,
    'above-average', 'below-average',             // cfcAboveAverage, cfcBelowAverage,
    'above-equal-average', 'below-equal-average', // cfcAboveEqualAverage, cfcBelowEqualAverage
    'top-elements(%s)', 'bottom-elements(%s)',    // cfcTop, cfcBottom,
    'top-percent(%s)', 'bottom-percent(%s)',      // cfcTopPercent, cfcBottomPercent,
    'duplicate', 'unique',                        // cfcDuplicate, cfcUnique,
    'begins-with(%s)', 'ends-with(%s)',           // cfcBeginsWith, cfcEndsWith,
    'contains-text(%s)', 'not-contains-text(%s)', // cfcContainsText, cfcNotContainsText,
    'is-error', 'is-no-error',                    // cfcContainsErrors, cfcNotContainsErrors
    'yesterday', 'today', 'tomorrow', 'last-7-days',   // cfcYesterday .. cfcLast7Days
    'last-week', 'this-week', 'next-week',        // cfcLastWeek .. cfcNextWeek
    'last-month', 'this-month', 'next-month',     // cfcLastMonth .. cfcNextMonth
    'last-year', 'this-year', 'next-year',        // cfcLastYear .. cfcNextYear
    'formula-is(%s)'                              // cfcExprssion
  );

  CF_VALUE_KIND: array[TsCFValueKind] of string = (
    '',            // vkNone
    'minimum',     // vkMin
    'maximum',     // vkMax
    'percent',     // vkPercent
    'percentile',  // vkPercentile
    'number'       // vkValue
  );

  CF_ICON_SET: array[TsCFIconSet] of string = (
    '3Arrows', '3ArrowsGray', '3Flags',   // is3Arrows, is3ArrowsGray, is3Flags
    '3TrafficLights1', '3TrafficLights2', // is3TrafficLights1, is3TrafficLights2
    '3Signs', '3Symbols', '3Symbols2',    // is3Signs, is3Symbols, is3Symbols2
    '3Smilies', '3Stars', '3Triangles',   // is3Smilies, is3Stars, is3Triangles
    '3ColorSmilies',                      // is3ColorSmilies,
    '4Arrows', '4ArrowsGray',             // is4Arrows, is4ArrowsGray
    '4RedToBlack', '4Rating',             // is4RedToBlack, is4Rating,
    '4RedToBlack',                        // is4TrafficLights,                   // not in ODS
    '5Arrows', '5ArrowsGray',             // is5Arrows, is5ArrowsGray
    '5Rating', '5Quarters', '5Boxes'      // is5Rating, is5Quarters, is5Boxes
  );

function CFOperandToStr(v: variant; AWorksheet: TsWorksheet): String;
var
  r,c: Cardinal;
begin
  Result := VarToStr(v);
  if Result = '' then
    exit;

  if VarIsStr(v) then begin
    // Special case: v is a formula, i.e. begins with '='
    if (Length(Result) > 1) and (Result[1] = '=') then
      Result := ConvertFormulaDialect(Result, fdExcelA1, fdOpenDocument, AWorksheet)
    else
    // Special case: cell reference (Note: relative refs are made absolute!)
    if ParseCellString(Result, r, c) then
      Result := Format('[.%s]', [GetCellString(r, c, [])])  // Need absolute reference!
    else
      Result := UTF8TextToXMLText(SafeQuoteStr(Result))
  end;
end;

function StrToValueKind(s: String): TsCFValueKind;
var
  vk: TsCFValuekind;
begin
  for vk in TsCFValueKind do
    if CF_VALUE_KIND[vk] = s then
    begin
      Result := vk;
      exit;
    end;
  Result := vkNone;
end;


type
  { Table style items stored in TableStyleList of the reader }
  TTableStyleData = class
  public
    Name: String;
    BiDiMode: TsBiDiMode;
    Hidden: boolean;
    TabColor: TsColor;
  end;

  { Column style items stored in ColStyleList of the reader }
  TColumnStyleData = class
  public
    Name: String;
    ColWidth: Double;                // in workbook units
    PageBreak: Boolean;              // Indicator that col follows a page break
  end;

  { Column data items stored in the ColumnList }
  TColumnData = class
  public
    Col: Integer;
    ColStyleIndex: integer;          // index into FColumnStyleList of reader
    DefaultCellStyleIndex: Integer;  // Index of default cell style in FCellStyleList of reader
    Hidden: Boolean;                 // Indicates that column is hidden
    PageBreak: Boolean;              // Indicates that page break occurs at left of column
  end;

  { Row style items stored in RowStyleList of the reader }
  TRowStyleData = class
  public
    Name: String;
    RowHeight: Double;    // in workbook units
    RowHeightType: TsRowHeightType;
    PageBreak: Boolean;
  end;

  { PageLayout items stored in PageLayoutList }
  TPageLayoutData = class
  public
    Name: String;
    PageLayout: TsPageLayout;
    constructor Create;
    destructor Destroy; override;
  end;

  constructor TPageLayoutData.Create;
  begin
    inherited;
    PageLayout := TsPageLayout.Create(nil);
  end;

  destructor TPageLayoutData.destroy;
  begin
    PageLayout.Free;
    inherited;
  end;

type
  { MasterPage items stored in MasterPageList }
  TMasterPageData = class
  public
    Name: String;
    PageLayoutName: String;
  end;

  (* --- presently not used, but this may change... ---

  { Row data items stored in the RowList of the reader }
  TRowData = class
    Row: Integer;
    RowStyleIndex: Integer;   // index into FRowStyleList of reader
    DefaultCellStyleIndex: Integer;  // Index of default row style in FCellStyleList of reader
  end;
  *)


{******************************************************************************}
{                         Clipboard utility                                    }
{******************************************************************************}
                              (*
{@@ ----------------------------------------------------------------------------
  Writes the "Star Object Descriptor". This is written to the clipboard by
  Open/LibreOffice. No idea about the meaning of this...
-------------------------------------------------------------------------------}
procedure WriteStarObjectDescriptorToStream(AStream: TStream);
const
  BYTES: packed array[0..$38] of byte = (
    $39,$00,$00,$00,$CB,$B4,$BB,$47,$4C,$CE,$80,$4E,$A5,$91,$42,$D9,
    $AE,$74,$95,$0F,$01,$00,$00,$00,$D2,$08,$00,$00,$C4,$01,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$63,$61,$6C,$63,$38,$00,
    $00,$67,$45,$23,$01,$EF,$CD,$AB,$89);
begin
  AStream.Write(BYTES, SizeOf(BYTES));
end;                            *)


procedure InitOpenDocLimitations(out ALimitations: TsSpreadsheetFormatLimitations);
begin
  // http://en.wikipedia.org/wiki/List_of_spreadsheet_software#Specifications
  ALimitations.MaxColCount := 1024;
  ALimitations.MaxRowCount := 1048576;
  //https://forum.openoffice.org/en/forum/viewtopic.php?f=9&t=11247&p=52985
  ALimitations.MaxCharsInTextCell := 65535;
end;


{******************************************************************************}
{                          TXMLHeaderFooterFont                                }
{******************************************************************************}
type
  TXMLHeaderFooterFont = class(TsHeaderFooterFont)
    StyleName: String;
  end;


{******************************************************************************}
{                    TsSpreadOpenDocHeaderFooterParser                         }
{******************************************************************************}

constructor TsSpreadOpenDocHeaderFooterParser.Create(ANode: TDOMNode;
  AFontList: TList; ADefaultFont: TsHeaderFooterFont);
begin
  inherited Create;
  XMLMode := true;
  FNode := ANode; // this is a child of the "<style-header>" etc nodes.
  FFontList := AFontList;
  FDefaultFont := ADefaultFont;
  FFontClass := TXMLHeaderFooterFont;
  Parse;
end;

procedure TsSpreadOpenDocHeaderFooterParser.AddNodeElement(ANode: TDOMNode);
var
  nodeName: String;
begin
  nodeName := ANode.NodeName;
  case nodeName of
    'text:sheet-name':
      AddElement(hftSheetName);
    'text:file-name' :
      case GetAttrValue(ANode, 'text:display') of
        'full': begin
                  AddElement(hftPath);
                  AddElement(hftFileName);
                end;
        'path': AddElement(hftPath);
        else    AddElement(hftFileName);
      end;
    'text:date':
      AddElement(hftDate);
    'text:time':
      AddElement(hftTime);
    'text:page-number':
      AddElement(hftPage);
    'text:page-count':
      AddElement(hftPageCount);
    '#text':
      begin
        FCurrText := ANode.NodeValue; //GetNodeValue(ANode);
        AddCurrTextElement;
      end;
  end;
end;

function TsSpreadOpenDocHeaderFooterParser.BuildHeaderFooterAsXMLString: String;
var
  regionStr: array[TsHeaderFooterSectionIndex] of String;
  sec: TsHeaderFooterSectionIndex;
  element: TsHeaderFooterElement;
  styleName: String;
  s: String;
begin
  for sec := hfsLeft to hfsRight do
  begin
    if Length(FSections[sec]) = 0 then
      Continue;
    regionStr[sec] := '';
    for element in FSections[sec] do
    begin
      stylename := TXMLHeaderFooterFont(FFontList[element.FontIndex]).StyleName;
      case element.Token of
        hftText:
          s := Format('<text:span text:style-name="%s">%s</text:span>', [
                 stylename, UTF8TextToXMLText(element.TextValue)]);
        hftNewLine:
          s := '</text:p><text:p>';
        hftSheetName:
          s := '<text:sheet-name>???</text:sheet-name>';
        hftPath:
          s := '<text:file-name text:display="path">???</text:file-name>';
        hftFileName:
          s := '<text:file-name text:display="name-and-extension">???</text:file-name>';
        hftDate:
          s := Format('<text:date style:data-style-name="N2" text:date-value="%s">%s</text:date>', [
                 FormatDateTime('yyyy"-"mm"-"dd', date()), DateToStr(date())]);
        hftTime:
          s := Format('<text:time>%s</text:time>', [
                 FormatDateTime('hh:nn:ss', time()) ]);
        hftPage:
          s := '<text:page-number>1</text:page-number>';
        hftPageCount:
          s := '<text:page-count>1</text:page-count>';
        else
          s := '';
      end;
      if s <> '' then
        regionStr[sec] := regionStr[sec] + s;
    end; // for element
    if regionStr[sec] <> '' then
      regionStr[sec] := '<text:p>' + regionStr[sec] + '</text:p>';
  end;  // for sec

  Result := '';
  for sec := hfsLeft to hfsRight do
  begin
    case sec of
      hfsLeft   : s := 'style:region-left';
      hfsCenter : s := 'style:region-center';
      hfsRight  : s := 'style:region-right';
    end;
    if regionStr[sec] = '' then
      Result := Result + '<' + s + ' />' else
      Result := Result + '<' + s + '>' + regionStr[sec] + '</' + s + '>';
  end;
end;

function TsSpreadOpenDocHeaderFooterParser.FindStyle(AStyleName: String): Integer;
var
  fnt: TXMLHeaderFooterFont;
begin
  for Result := 0 to FFontList.Count-1 do
  begin
    fnt := TXMLHeaderFooterFont(FFontList[Result]);
    if SameText(fnt.StyleName, AStyleName) then
      exit;
  end;
  Result := -1;
end;

function TsSpreadOpenDocHeaderFooterParser.GetCurrFontIndex: Integer;
begin
  if XMLMode then
    Result := FCurrFontIndex
  else
    Result := inherited GetCurrFontIndex;
end;

procedure TsSpreadOpenDocHeaderFooterParser.Parse;
var
  node, pnode, childpnode, childspannode: TDOMNode;
  nodeName: String;
  s: String;
  firstP: Boolean;
begin
  FFontClass := TXMLHeaderFooterFont;

  if not XMLMode then
  begin
    inherited Parse;
    exit;
  end;

  node := FNode;
  while Assigned(node) do
  begin
    nodeName := node.NodeName;
    case nodeName of
      'style:region-left'   : FCurrSection := hfsLeft;
      'style:region-center' : FCurrSection := hfsCenter;
      'style:region-right'  : FCurrSection := hfsRight;
    end;
    firstP := true;
    pnode := node.FirstChild;
    while Assigned(pnode) do
    begin
      nodeName := pnode.NodeName;
      if nodeName = 'text:p' then
      begin
        if not firstP then AddElement(hftNewLine);
        childpnode := pnode.FirstChild;
        while Assigned(childpnode) do
        begin
          nodeName := childpnode.NodeName;
          if nodeName = 'text:span' then begin
            s := GetAttrValue(childpnode, 'text:style-name');
            if s <> '' then
              FCurrFontIndex := FindStyle(s) else
              FCurrFontIndex := -1;
            childspannode := childpnode.FirstChild;
            while Assigned(childspannode) do
            begin
              nodeName := childspannode.NodeName;
              AddNodeElement(childspannode);
              childspannode := childspannode.NextSibling;
            end;
          end else
            AddNodeElement(childpnode);
          childpnode := childpnode.NextSibling;
        end;
        firstP := false;
      end;
      pnode := pnode.NextSibling;
    end;
    node := node.NextSibling;
  end;
end;


{******************************************************************************}
{                     TsSpreadOpenDocNumFormatParser                           }
{******************************************************************************}

function TsSpreadOpenDocNumFormatParser.BuildXMLAsString(AFormatName: String): String;
var
  i: Integer;
begin
  Result := '';
  { When there is only one section the next statement is the only one executed.
    When there are several sections the file contains at first the
    positive section (index 0), then the negative section (index 1), and
    finally the zero section (index 2) which contains the style-map. }
  for i:=0 to Length(FSections)-1 do
    Result := Result + BuildXMLAsStringFromSection(i, AFormatName);
end;

function TsSpreadOpenDocNumFormatParser.BuildXMLAsStringFromSection(
  ASection: Integer; AFormatName: String): String;
var
  n: Integer;
  el, nEl: Integer;
  ns: Integer;
  clr: TsColor;
  mask: String;
  s: String;
  timeIntervalStr: String;
  styleMapStr: String;
  int,num,denom: Integer;
begin
  Result := '';
  if ASection > 2 then    // ods supports only at most 3 sections
    exit;

  ns := Length(FSections);
  if (ns = 0) then
    exit;
  if ns > 3 then          // ods supports only at most 3 sections
    ns := 3;

  styleMapStr := '';
  timeIntervalStr := '';

  if (ns > 1) then
  begin
    // The file corresponding to the last section contains the styleMap.
    if (ASection = ns - 1) then
      case ns of
        2: styleMapStr :=
             '<style:map ' +
               'style:apply-style-name="' + AFormatName + 'P0" ' +
               'style:condition="value()&gt;=0" />';                   // >= 0
        3: styleMapStr :=
             '<style:map '+
               'style:apply-style-name="' + AFormatName + 'P0" ' +     // > 0
               'style:condition="value()&gt;0" />' +
             '<style:map '+
               'style:apply-style-name="' + AFormatName + 'P1" ' +     // < 0
               'style:condition="value()&lt;0" />';
        {
        else
          raise EFPSpreadsheet.Create('At most 3 format sections allowed.');
          }
      end
    else
      AFormatName := AFormatName + 'P' + IntToStr(ASection);
  end;

  with FSections[ASection] do
  begin
    nEl := Length(Elements);
    el := 0;
    while (el < nEl) do begin
      case Elements[el].Token of
        nftColor:
          begin
            clr := TsColor(Elements[el].IntValue);
            Result := Result + '<style:text-properties fo:color="' + ColorToHTMLColorStr(clr) + '" />';
          end;

        nftSign, nftSignBracket, nftText, nftSpace:
          if Elements[el].TextValue = ' ' then
            Result := Result + '<number:text><![CDATA[ ]]></number:text>'
          else
            Result := Result + '<number:text>' + Elements[el].TextValue + '</number:text>';

        nftPercent:
          Result := Result + '<number:text>%</number:text>';

        nftFactor:
          ;

        nftCurrSymbol:
          Result := Result + '<number:currency-symbol>' + Elements[el].TextValue + '</number:currency-symbol>';

        nftGeneral:
           Result := Result + '<number:number number:min-integer-digits="1" />';

        nftIntTh:
          begin
            Result := Result + '<number:number number:min-integer-digits="1" number:grouping="true"';
            if (el+2 < nel) and (Elements[el+1].Token = nftDecSep) and
               (Elements[el+2].Token in [nftZeroDecs, nftOptDecs, nftSpaceDecs]) then
            begin
              n := IfThen(Elements[el+2].Token = nftZeroDecs, Elements[el+2].IntValue, 1);
              Result := Result + ' number:decimal-places="' + IntToStr(n) + '"';
              inc(el, 2);
            end else
            if (el = nel-1) or (Elements[el+1].Token <> nftDecSep) then
              Result := Result + ' number:decimal-places="0"';
            if (nfkHasFactor in Kind) and (Factor <> 0) then
              Result := Result + Format(' number:display-factor="%.0f"', [1.0/Factor]);
            Result := Result + ' />';
          end;

        nftFracNumZeroDigit, nftFracNumOptDigit, nftFracNumSpaceDigit:
          begin
            num := Elements[el].IntValue;
            inc(el);
            while (el < nel) and (Elements[el].Token in [nftSpace, nftText, nftEscaped]) do
              inc(el);
            if (el < nel) and (Elements[el].Token <> nftFracSymbol) then
              Continue;
            while (el < nel) and (Elements[el].Token in [nftSpace, nftText, nftEscaped]) do
              inc(el);
            if (el < nel) and
               (Elements[el].Token in [nftFracDenomOptDigit, nftFracDenomSpaceDigit, nftFracDenomZeroDigit, nftFracDenom])
            then
              denom := Elements[el].IntValue
            else
              Continue;
            if Elements[el].Token = nftFracDenom then    // fixed denominator
              Result := Result +
                '<number:fraction' +
                ' number:min-numerator-digits="' + IntToStr(num) +
                '" number:min-denominator-digits="' + IntToStr(num) +
                '" number:denominator-value="' + IntToStr(denom) +
                '" />'
            else
              Result := Result +
                '<number:fraction' +
                ' number:min-numerator-digits="' + IntToStr(num) +
                '" number:min-denominator-digits="' + IntToStr(num) +
                '" />'
          end;

        nftIntZeroDigit, nftIntOptDigit, nftIntSpaceDigit:
          begin
            // Mixed fraction
            if nfkFraction in Kind then
            begin
              if Elements[el].Token = nftIntOptDigit
                then int := 0
                else int := Elements[el].IntValue;
              inc(el);
              while (el < nel) and not
                (Elements[el].Token in [nftFracNumZeroDigit, nftFracNumOptDigit, nftFracNumSpaceDigit])
              do
                inc(el);
              if el = nel then
                Continue;
              num := Elements[el].IntValue;
              while (el < nel) and not
                (Elements[el].Token in [nftFracDenomZeroDigit, nftFracDenomOptDigit, nftFracDenomSpaceDigit, nftFracDenom])
              do
                inc(el);
              if el = nel then
                Continue;
              denom := Elements[el].IntValue;
              if (Elements[el].Token = nftFracDenom) then
                Result := Result +
                  '<number:fraction' +
                  ' number:min-integer-digits="' + IntToStr(int) +
                  '" number:min-numerator-digits="' + IntToStr(num) +
                  '" number:min-denominator-digits="' + IntToStr(num) +
                  '" number:denominator-value="' + IntToStr(denom) +
                  '" />'
              else
                Result := Result +
                  '<number:fraction' +
                  ' number:min-integer-digits="' + IntToStr(int) +
                  '" number:min-numerator-digits="' + IntToStr(num) +
                  '" number:min-denominator-digits="' + IntToStr(denom) +
                  '" />';
            end
            else
            // Scientific, no decimals
            if (el+3 < nel) and (Elements[el+1].Token = nftExpChar) then
            begin
              Result := Result + '<number:scientific-number number:decimal-places="0"';
//              n := IfThen(Elements[el].Token = nftIntZeroDigit, Elements[el].IntValue, 1);
              n := FSections[ASection].MinIntDigits;
              Result := Result + ' number:min-integer-digits="' + IntToStr(n) + '"';
              n := Elements[el+3].IntValue;
              Result := Result + ' number:min-exponent-digits="' + IntToStr(n) + '"';
              Result := Result + ' />';
              inc(el, 3);
            end
            else
            // Scientific, with decimals
            if (el+5 < nel) and (Elements[el+1].Token = nftDecSep) and (Elements[el+3].Token = nftExpChar)
            then begin
              Result := Result + '<number:scientific-number';
//              n := IfThen(Elements[el].Token = nftIntZeroDigit, Elements[el].IntValue, 1);
              n := FSections[ASection].MinIntDigits;
              Result := Result + ' number:min-integer-digits="' + IntToStr(n) + '"';
              n := IfThen(Elements[el+2].Token = nftZeroDecs, Elements[el+2].IntValue, 1);
              Result := Result + ' number:decimal-places="' + IntToStr(n) + '"';
              Result := Result + ' number:min-exponent-digits="' + IntToStr(Elements[el+5].IntValue) + '"';
              Result := Result + ' />';
              inc(el, 5);
            end
            else
            // Standard decimal number
            if (el+2 < nel) and (Elements[el+1].Token = nftDecSep) then
            begin
              Result := Result + '<number:number';
//              n := IfThen(Elements[el].Token = nftIntZeroDigit, Elements[el].IntValue, 1);
              n := FSections[ASection].MinIntDigits;
              Result := Result + ' number:min-integer-digits="' + IntToStr(n) + '"';
              n := IfThen(Elements[el+2].Token = nftZeroDecs, Elements[el+2].IntValue, 1);
              Result := Result + ' number:decimal-places="' + IntToStr(n) + '"';
              if (nfkHasFactor in Kind) and (Factor <> 0) then
                Result := Result + Format(' number:display-factor="%.0f"', [1.0/Factor]);
              Result := Result + ' />';
              inc(el, 2);
            end
            else
            (*
            // Standard integer, format '#'
            if (el = 0) and (nel = 1) and (Elements[el].Token = nftIntOptDigit) then
              Result := Result + '<number:number number:min-integer-digits="0" number:decimal-places="0" />'
            else
            *)
            // Standard integer
            if (el = nel-1) or (Elements[el+1].Token <> nftDecSep) then
            begin
              Result := Result + '<number:number number:decimal-places="0"';
//              n := IfThen(Elements[el].Token = nftIntZeroDigit, Elements[el].IntValue, 1);
              n := FSections[ASection].MinIntDigits;
              Result := Result + ' number:min-integer-digits="' + IntToStr(n) + '"';
              if (nfkHasFactor in Kind) and (Factor <> 0) then
                Result := Result + Format(' number:display-factor="%.0f"', [1.0/Factor]);
              Result := Result + ' />';
            end;
          end;

        nftYear:
          if Elements[el].IntValue > 2 then
            Result := Result + '<number:year number:style="long" />'
          else
            Result := Result + '<number:year />';

        nftMonth:
          case Elements[el].IntValue of
            1: Result := Result + '<number:month />';
            2: Result := Result + '<number:month number:style="long" />';
            3: Result := Result + '<number:month number:textual="true" />';
            4: Result := Result + '<number:month number:style="long" number:textual="true" />';
          end;

        nftDay:
          case Elements[el].IntValue of
            1: Result := Result + '<number:day />';
            2: Result := Result + '<number:day number:style="long" />';
            3: Result := Result + '<number:day-of-week />';
            4: Result := Result + '<number:day-of-week number:style="long" />';
          end;

        nftHour:
          begin
            case abs(Elements[el].IntValue) of
              1: Result := Result + '<number:hours />';
              2: Result := Result + '<number:hours number:style="long" />';
            end;
            if Elements[el].IntValue < 0 then
              timeIntervalStr := ' number:truncate-on-overflow="false"';
          end;

        nftMinute:
          begin
            case abs(Elements[el].IntValue) of
              1: Result := Result + '<number:minutes />';
              2: Result := Result + '<number:minutes number:style="long" />';
            end;
            if Elements[el].IntValue < 0 then
              timeIntervalStr := ' number:truncate-on-overflow="false"';
          end;

        nftSecond:
          begin
            s := '';
            if (el < nel - 2) and (Elements[el+1].Token = nftDecSep) and
              (Elements[el+2].Token = nftMilliseconds)
            then
              s := Format('number:decimal-places="%d"', [Elements[el+2].IntValue]);
            case abs(Elements[el].IntValue) of
              1: Result := Result + '<number:seconds />';
              2: Result := Result + '<number:seconds number:style="long" ' + s + '/>';
            end;
            if Elements[el].IntValue < 0 then
              timeIntervalStr := ' number:truncate-on-overflow="false"';
          end;

        nftMilliseconds:
          ; // ???

        nftAMPM:
          Result := Result + '<number:am-pm />';

        nftDateTimeSep:
          case Elements[el].TextValue of
            '/': Result := Result + '<number:text>' + FFormatSettings.DateSeparator + '</number:text>';
            ':': Result := Result + '<number:text>' + FFormatSettings.TimeSeparator + '</number:text>';
            ' ': Result := Result + '<number:text><![CDATA[ ]]></number:text>';
            else Result := Result + '<number:text>' + Elements[el].TextValue + '</number:text>';
          end;

        nftTextFormat:
          Result := Result + '<number:text-content />';
      end;

      inc(el);
    end;

    if (nfkPercent in Kind) then
      mask := '<number:percentage-style style:name="%s"%s>%s%s</number:percentage-style>'
    else
    if (nfkCurrency in Kind) then
      mask := '<number:currency-style style:name="%s"%s>%s%s</number:currency-style>'
    else
    if (nfkDate in Kind) then
      mask := '<number:date-style style:name="%s"%s>%s%s</number:date-style>'
    else
    if (Kind * [nfkDate, nfkTime] = [nfkTime]) then
      mask := '<number:time-style style:name="%s"%s>%s%s</number:time-style>'
    else
    if (Kind = [nfkText]) then
      mask := '<number:text-style style:name="%s"%s>%s%s</number:text-style>'
    else
      mask := '<number:number-style style:name="%s"%s>%s%s</number:number-style>';

    Result := Format(mask, [AFormatName, TimeIntervalStr, Result, StyleMapStr]);
  end;
end;


{******************************************************************************}
{                         TsSpreadOpenDocReader                                }
{******************************************************************************}

constructor TsSpreadOpenDocReader.Create(AWorkbook: TsBasicWorkbook);
begin
  inherited Create(AWorkbook);

  InitOpenDocLimitations(FLimitations);

  FPointSeparatorSettings := DefaultFormatSettings;
  FPointSeparatorSettings.DecimalSeparator := '.';
  FPointSeparatorSettings.ListSeparator := ';';  // for formulas

  FCellFormatList := TsCellFormatList.Create(true);
    // true = allow duplicates because style names used in cell records will not be found any more.

  FTableStyleList := TFPList.Create;
  FColumnStyleList := TFPList.Create;
  FColumnList := TFPList.Create;
  FRowStyleList := TFPList.Create;
  FRowList := TFPList.Create;
  FPageLayoutList := TFPList.Create;
  FMasterPageList := TFPList.Create;
  FHeaderFooterFontList := TObjectList.Create;  // frees objects
  FFontFaces := TStringList.Create;
  FRichTextFontList := TFPList.Create;

  FRepeatedRows.FirstIndex := UNASSIGNED_ROW_COL_INDEX;
  FRepeatedRows.LastIndex := UNASSIGNED_ROW_COL_INDEX;
  FRepeatedCols.FirstIndex := UNASSIGNED_ROW_COL_INDEX;
  FRepeatedCols.LastIndex := UNASSIGNED_ROW_COL_INDEX;

  // Initial base date in case it won't be read from file
  FDateMode := dmODS1899;
end;

destructor TsSpreadOpenDocReader.Destroy;
var
  j: integer;
begin
  FFontFaces.Free;

  for j := FRichTextFontList.Count-1 downto 0 do TObject(FRichTextFontList[j]).Free;
  FreeAndNil(FRichTextFontList);

  for j := FColumnList.Count-1 downto 0 do TObject(FColumnList[j]).Free;
  FColumnList.Free;

  for j := FTableStyleList.Count-1 downto 0 do TObject(FTableStyleList[j]).Free;
  FTableStyleList.Free;

  for j := FColumnStyleList.Count-1 downto 0 do TObject(FColumnStyleList[j]).Free;
  FColumnStyleList.Free;

  for j := FRowList.Count-1 downto 0 do TObject(FRowList[j]).Free;
  FRowList.Free;

  for j := FRowStyleList.Count-1 downto 0 do TObject(FRowStyleList[j]).Free;
  FRowStyleList.Free;

  for j := FPageLayoutList.Count-1 downto 0 do TObject(FPageLayoutList[j]).Free;
  FPageLayoutList.Free;

  for j := FMasterPageList.Count-1 downto 0 do TObject(FMasterPageList[j]).Free;
  FMasterPageList.Free;

  FHeaderFooterFontList.Free;
  inherited Destroy;
end;

procedure TsSpreadOpenDocReader.AddBuiltinNumFormats;
begin
  FNumFormatList.Clear;
  FNumFormatList.Add('N0:');
end;

{ Creates for each non-default column width as well as non-default column
  property stored internally in FColumnList a TCol record in the
  current worksheet. }
procedure TsSpreadOpenDocReader.ApplyColData;
var
  colIndex: Integer;
  colData: TColumnData;
  colStyleIndex: Integer;
  colStyle: TColumnStyleData;
  i: Integer;
  defColWidth: Single;
  colWidth: Single;
  colWidthType: TsColWidthType;
  lastOccCol: Integer;
  sheet: TsWorksheet;
begin
  sheet := FWorksheet as TsWorksheet;
  defColWidth := sheet.ReadDefaultColWidth(FWorkbook.Units);
  lastOccCol := sheet.GetLastOccupiedColIndex;

  for i:=0 to FColumnList.Count-1 do begin
    colData := TColumnData(FColumnList[i]);
    if (colData.Col > lastOccCol) then begin
      if colData.Hidden or colData.PageBreak then
        lastOccCol := colData.Col
    end;
  end;

  for i:=0 to FColumnList.Count-1 do
  begin
    colData := TColumnData(FColumnList[i]);
    colIndex := colData.Col;

    // Skip column records beyond the last data column - there's a bug in OO/LO
    // which adds column records up to the max column limit.
    if colIndex > lastOccCol then
      Continue;

    colStyleIndex := colData.ColStyleIndex;
    colStyle := TColumnStyleData(FColumnStyleList[colStyleIndex]);
    //defCellStyleIndex := colData.DefaultCellStyleIndex;
    {
    // Get column format
    fmt := FCellFormatList.Items[defCellStyleIndex];
    if fmt <> nil then
      fmtIndex := FWorkbook.AddCellFormat(fmt^)
    else
      fmtIndex := 0;
    }

    // Prepare column record for the worksheet
    colWidth := colStyle.ColWidth;        // is already in workbook units
    if SameValue(colWidth, defColWidth, COLWIDTH_EPS) then
      colWidthType := cwtDefault
    else
      colWidthType := cwtCustom;

    // Write non-default column width to the worksheet
    if (colWidthType = cwtCustom) then
      sheet.WriteColWidth(colIndex, colWidth, FWorkbook.Units);

    // Column visibility
    if colData.Hidden then
      sheet.HideCol(colIndex);

    // Column page break flag
    if colData.PageBreak then
      sheet.AddPageBreakToCol(colIndex);

    // Note: we don't store the column format index here; this is done in the
    // row/cell reading method (ReadRowsAndCells).
  end;
end;

class function TsSpreadOpenDocReader.CheckFileFormat(AStream: TStream): Boolean;
begin
  Result := HasZipHeader(AStream);
end;

function TsSpreadOpenDocReader.ExtractFormatIndexFromStyle(ACellStyleName: String;
  ACol: Integer): Integer;
var
  idx: Integer;
begin
  Result := -1;
  if ACellStyleName <> '' then
    Result := FCellFormatList.FindIndexOfName(ACellStyleName);
  if Result = -1 then begin
    idx := FindColumnByCol(ACol);
    if idx > -1 then
      Result := TColumnData(FColumnList[idx]).DefaultCellStyleIndex;
  end;
  if Result = -1 then
    Result := 0;
end;


procedure TsSpreadOpenDocReader.ApplyStyleToCell(ACell: PCell; AStyleIndex: Integer);
var
  fmt: TsCellFormat;
  sheet: TsWorksheet;
begin
  sheet := FWorksheet as TsWorksheet;

  if sheet.HasHyperlink(ACell) then
    sheet.WriteFont(ACell, HYPERLINK_FONTINDEX);

  fmt := FCellFormatList.Items[AStyleIndex]^;
  if (AStyleIndex = 0) and sheet.HasHyperlink(ACell) then begin
    // Make sure to use hyperlink font for hyperlink cells in case of default cell style
    fmt.FontIndex := HYPERLINK_FONTINDEX;
    Include(fmt.UsedFormattingFields, uffFont);
  end;
  ACell^.FormatIndex := (FWorkbook as TsWorkbook).AddCellFormat(fmt);
end;

{ Applies the style data referred to by the style name to the specified cell
  The function result is false if a style with the given name could not be found }
function TsSpreadOpenDocReader.ApplyStyleToCell(ACell: PCell; AStyleName: String): Boolean;
var
  fmt: TsCellFormat;
  styleIndex: Integer;
  //i: Integer;
begin
  Result := false;

  if FWorksheet.HasHyperlink(ACell) then
    (FWorksheet as TsWorksheet).WriteFont(ACell, HYPERLINK_FONTINDEX);

  styleIndex := ExtractFormatIndexFromStyle(AStyleName, ACell^.Col);
  (*
  // Is there a style attached to the cell?
  styleIndex := -1;
  if AStyleName <> '' then
    styleIndex := FCellFormatList.FindIndexOfName(AStyleName);
  if (styleIndex = -1) then
  begin
    // No - look for the style attached to the column of the cell and
    // find the cell style by the DefaultCellStyleIndex stored in the column list.
    i := FindColumnByCol(ACell^.Col);
    if i = -1 then
      exit;
    styleIndex := TColumnData(FColumnList[i]).DefaultCellStyleIndex;
  end;
  *)

  fmt := FCellFormatList.Items[styleIndex]^;
  if (styleIndex = 0) and FWorksheet.HasHyperlink(ACell) then
  begin
    // Make sure to use hyperlink font for hyperlink cells in case of default cell style
    fmt.FontIndex := HYPERLINK_FONTINDEX;
    Include(fmt.UsedFormattingFields, uffFont);
  end;
  ACell^.FormatIndex := (FWorkbook as TsWorkbook).AddCellFormat(fmt);

  Result := true;
end;

function TsSpreadOpenDocReader.ApplyTableStyle(ASheet: TsBasicWorksheet;
  AStyleName: String): Boolean;
var
  styleIndex: Integer;
  tableStyle: TTableStyleData;
  sheet: TsWorksheet absolute ASheet;
begin
  Result := false;
  if (AStyleName = '') or (ASheet = nil) then
    exit;
  styleIndex := FindTableStyleByName(AStyleName);
  if styleIndex = -1 then
    exit;
  tableStyle := TTableStyleData(FTableStyleList[styleIndex]);
  if (tableStyle.BiDiMode = bdRTL) or (tableStyle.BiDiMode = bdLTR) then
    sheet.BiDiMode := tableStyle.BiDiMode;
  if tableStyle.Hidden then
    sheet.Options := sheet.Options + [soHidden];
  sheet.TabColor := tableStyle.TabColor;
  Result := true;
end;


{ Extracts a boolean value from a "boolean" cell node.
  Is called from ReadBoolean }
function TsSpreadOpenDocReader.ExtractBoolFromNode(ANode: TDOMNode): Boolean;
var
  value: String;
begin
  value := GetAttrValue(ANode, 'office:boolean-value');
  if (lowercase(value) = 'true') then
    Result := true
  else
    Result := false;
end;

{ Extracts a date/time value from a "date-value" or "time-value" cell node.
  Requires the number format and format strings to optimize agreement with
  fpc date/time values.
  Is called from "ReadDateTime". }
function TsSpreadOpenDocReader.ExtractDateTimeFromNode(ANode: TDOMNode;
  ANumFormat: TsNumberFormat; const AFormatStr: String): TDateTime;
var
  Value: String;
  Fmt : TFormatSettings;
  FoundPos : integer;
  Hours, Minutes, Days: integer;
  Seconds: Double;
  HoursPos, MinutesPos, SecondsPos: integer;
begin
  Unused(AFormatStr);

  // Format expects ISO 8601 type date string or time string
  fmt := DefaultFormatSettings;
  fmt.ShortDateFormat := 'yyyy-mm-dd';
  fmt.DateSeparator := '-';
  fmt.LongTimeFormat := 'hh:nn:ss';
  fmt.TimeSeparator := ':';

  Value := GetAttrValue(ANode, 'office:date-value');

  if Value <> '' then
  begin
    // Date or date/time string
    Value := StringReplace(Value,'T',' ',[rfIgnoreCase,rfReplaceAll]);
    // Strip milliseconds?
    FoundPos := Pos('.',Value);
    if (FoundPos > 1) then
       Value := Copy(Value, 1, FoundPos-1);
    Result := StrToDateTime(Value, Fmt);

    // If the date/time is within 1 day of the base date the value is most
    // probably a time-only value (< 1).
    // We need to subtract the datemode offset, otherwise the date/time value
    // would not be < 1 for fpc.
    case FDateMode of
      dmODS1899: if Result - DATEMODE_1899_BASE < 1 then Result := Result - DATEMODE_1899_BASE;
      dmODS1900: if Result - DATEMODE_1900_BASE < 1 then Result := Result - DATEMODE_1900_BASE;
      dmODS1904: if Result - DATEMODE_1904_BASE < 1 then Result := Result - DATEMODE_1904_BASE;
    end;

  end else begin
    // Try time only, e.g. PT23H59M59S
    //                     12345678901
    Value := GetAttrValue(ANode, 'office:time-value');
    if (Value <> '') and (Pos('PT', Value) = 1) then
    begin
      // Get hours
      HoursPos := Pos('H', Value);
      if (HoursPos > 0) then
        Hours := StrToInt(Copy(Value, 3, HoursPos-3))
      else
        Hours := 0;

      // Get minutes
      MinutesPos := Pos('M', Value);
      if (MinutesPos > 0) and (MinutesPos > HoursPos) then
        Minutes := StrToInt(Copy(Value, HoursPos+1, MinutesPos-HoursPos-1))
      else
        Minutes := 0;

      // Get seconds
      SecondsPos := Pos('S', Value);
      if (SecondsPos > 0) and (SecondsPos > MinutesPos) then
        Seconds := StrToFloat(Copy(Value, MinutesPos+1, SecondsPos-MinutesPos-1), FPointSeparatorSettings)
      else
        Seconds := 0;

      Days := Hours div 24;
      Hours := Hours mod 24;
      Result := Days + (Hours + (Minutes + Seconds/60)/60)/24;

      { Values < 1 day are certainly time-only formats --> no datemode correction
        nfTimeInterval formats are differences --> no date mode correction
        In all other case, we have a date part that needs to be corrected for
        the file's datemode. }
      if (ANumFormat <> nfTimeInterval) and (abs(Days) > 0) then
      begin
        case FDateMode of
          dmODS1899: Result := Result + DATEMODE_1899_BASE;
          dmODS1900: Result := Result + DATEMODE_1900_BASE;
          dmODS1904: Result := Result + DATEMODE_1904_BASE;
        end;
      end;
    end;
  end;
end;

function TsSpreadOpenDocReader.ExtractErrorFromNode(ANode: TDOMNode;
  out AErrorValue: TsErrorValue): Boolean;
var
  s: String;
begin
  s := GetAttrValue(ANode, 'table:formula');
  if s = '' then
  begin
    AErrorValue := errOK;
    Result := true;
    exit;
  end;
  if pos('of:', s) = 1 then Delete(s, 1, 3);
  Delete(s, 1, 1);  // Delete '='
  if s = '' then
  begin
    AErrorValue := errOK;
    Result := true;
    exit;
  end;

  Result := TryStrToErrorValue(s, AErrorValue);
  if not Result then
  begin
    s := ANode.NodeName;
    ANode:= ANode.FirstChild;
    while Assigned(ANode) do
    begin
      s := ANode.NodeName;
      if s = 'text:p' then
      begin
        s := GetNodeValue(ANode);
        Result := TryStrToErrorValue(s, AErrorValue);
        exit;
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

function TsSpreadOpenDocReader.FindColumnByCol(AColIndex: Integer): Integer;
begin
  for Result := 0 to FColumnList.Count-1 do
    if TColumnData(FColumnList[Result]).Col = AColIndex then
      exit;
  Result := -1;
end;

function TsSpreadOpenDocReader.FindColStyleByName(AStyleName: String): Integer;
begin
  for Result := 0 to FColumnStyleList.Count-1 do
    if TColumnStyleData(FColumnStyleList[Result]).Name = AStyleName then
      exit;
  Result := -1;
end;

function TsSpreadOpenDocReader.FindNumFormatByName(ANumFmtName: String): Integer;
begin
  for Result := 0 to FNumFormatList.Count-1 do
    if pos(ANumFmtName+':', FNumFormatList[Result]) = 1 then
      exit;
  Result := -1;
end;

function TsSpreadOpenDocReader.FindRowStyleByName(AStyleName: String): Integer;
begin
  for Result := 0 to FRowStyleList.Count-1 do
    if TRowStyleData(FRowStyleList[Result]).Name = AStyleName then
      exit;
  Result := -1;
end;

function TsSpreadOpenDocReader.FindTableStyleByName(AStyleName: String): Integer;
begin
  for Result := 0 to FTableStyleList.Count-1 do
    if TTableStyleData(FTableStyleList[Result]).Name = AStyleName then
      exit;
  Result := -1;
end;

procedure TsSpreadOpenDocReader.ReadAutomaticStyles(AStylesNode: TDOMNode);
var
  nodeName: String;
  layoutNode, fontNode: TDOMNode;
  node, child, childchild: TDOMNode;
  s: String;
  href: String;
  imgpos: String;
  data: TPageLayoutData;
  isHeader: Boolean;
  h, dist: Double;
  fnt: TXMLHeaderFooterFont;
  defFnt: TsFont;
  fntName: String;
  fntSize: Double;
  fntStyle: TsHeaderFooterFontStyles;
  fntColor: TsColor;
  n: Integer;
  hfs: TsHeaderFooterSectionIndex;
begin
  if not Assigned(AStylesNode) then
    exit;
  defFnt := (Workbook as TsWorkbook).GetDefaultFont;
  layoutNode := AStylesNode.FirstChild;
  while layoutNode <> nil do
  begin
    nodeName := layoutNode.NodeName;
    if nodeName = 'style:style' then
    begin
      // Read fonts used by page layout's header/footer
      fntName := defFnt.FontName;
      fntSize := defFnt.Size;
      fntColor := defFnt.Color;
      fntStyle := [];
      s := GetAttrValue(layoutNode, 'style:family');
      if s = 'text' then
      begin
        s := GetAttrValue(layoutNode, 'style:name');
        fontNode := layoutNode.FirstChild;
        ReadHeaderFooterFont(fontNode, fntName, fntSize, fntStyle, fntColor);
        fnt := TXMLHeaderFooterFont.Create(fntName, fntSize, fntStyle, fntColor);
        fnt.StyleName := s;
        FHeaderFooterFontList.Add(fnt);
      end;
    end
    else
    if nodeName = 'style:page-layout' then
    begin
      // Read page layout parameters
      data := TPageLayoutData.Create;
      data.Name := GetAttrValue(layoutNode, 'style:name');

      node := layoutNode.FirstChild;
      while node <> nil do
      begin
        nodeName := node.NodeName;
        if nodeName = 'style:page-layout-properties' then
        begin
          s := GetAttrValue(node, 'style:print-orientation');
          if s = 'landscape' then
            data.PageLayout.Orientation := spoLandscape
          else if s = 'portrait' then
            data.PageLayout.Orientation := spoPortrait;

          s := GetAttrValue(node, 'fo:page-width');
          if s <> '' then
            data.PageLayout.PageWidth := PtsToMM(HTMLLengthStrToPts(s));

          s := GetAttrValue(node, 'fo:page-height');
          if s <> '' then
            data.PageLayout.PageHeight := PtsToMM(HTMLLengthStrToPts(s));

          s := GetAttrValue(node, 'fo:margin-top');
          if s <> '' then
            data.PageLayout.TopMargin := PtsToMM(HTMLLengthStrToPts(s));
            // But: if there is a header this value is the headermargin!

          s := GetAttrValue(node, 'fo:margin-bottom');
          if s <> '' then
            data.PageLayout.BottomMargin := PtsToMM(HTMLLengthStrToPts(s));
            // But: if there is a footer this value is the footermargin!

          s := GetAttrValue(node, 'fo:margin-left');
          if s <> '' then
            data.PageLayout.LeftMargin := PtsToMM(HTMLLengthStrToPts(s));

          s := GetAttrValue(node, 'fo:margin-right');
          if s <> '' then
            data.PageLayout.RightMargin := PtsToMM(HTMLLengthStrToPts(s));

          s := GetAttrValue(node, 'style:scale-to');
          if (s <> '') then
          begin
            if s[Length(s)] = '%' then Delete(s, Length(s), 1);
            data.PageLayout.ScalingFactor := round(StrToFloat(s, FPointSeparatorSettings));
            with data.PageLayout do Options := Options - [poFitPages];
          end;

          s := GetAttrValue(node, 'style:scale-to-X');
          if s <> '' then
          begin
            data.PageLayout.FitWidthToPages := StrToInt(s);
            with data.PageLayout do Options := Options + [poFitPages];
          end;

          s := GetAttrValue(node, 'style:scale-to-Y');
          if s <> '' then
          begin
            data.PageLayout.FitHeightToPages := StrToInt(s);
            with data.PageLayout do Options := Options + [poFitPages];
          end;

          s := GetAttrValue(node, 'style:table-centering');
          case s of
            'both':
              with data.PageLayout do Options := Options + [poHorCentered, poVertCentered];
            'horizontal':
              with data.PageLayout do Options := Options + [poHorCentered] - [poVertCentered];
            'vertical':
              with data.PageLayout do Options := Options - [poHorCentered] + [poVertCentered];
          end;

          s := GetAttrValue(node, 'style:print');
          if pos('grid', s) > 0 then
            with data.PageLayout do Options := Options + [poPrintGridLines];
          if pos('headers', s) > 0 then
            with data.PageLayout do Options := Options + [poPrintHeaders];
          if pos('annotations', s) > 0 then
            with data.PageLayout do Options := Options + [poPrintCellComments];

          s := GetAttrValue(node, 'style:print-page-order');
          if s = 'ltr' then    // "left-to-right", the other option is "ttb = top-to-bottom"
            with data.PageLayout do Options := Options + [poPrintPagesByRows];

          s := GetAttrValue(node, 'style:first-page-number');
          if s = 'continue' then
            with Data.PageLayout do Options := Options - [poUseStartPageNumber]
          else
          if TryStrToInt(s, n) then
            data.PageLayout.StartPageNumber := n;
            // Sets poUseStartPageNumber automatically

          FPageLayoutList.Add(data);
        end else
        if (nodeName = 'style:header-style') or (nodeName = 'style:footer-style')
        then
        begin
          isHeader := nodeName = 'style:header-style';
          child := node.FirstChild;
          while child <> nil do
          begin
            nodeName := child.NodeName;
            if nodeName = 'style:header-footer-properties' then
            begin
              h := 0;
              dist := 0;
              s := GetAttrValue(child, 'svg:height');
              if s <> '' then
                h := PtsToMM(HTMLLengthStrToPts(s))
              else begin
                s := GetAttrValue(child, 'fo:min-height');
                if s <> '' then
                  h := PtsToMM(HTMLLengthStrToPts(s)) else h := 0;
              end;
              if isHeader then
                s := GetAttrValue(child, 'fo:margin-bottom') else
                s := GetAttrValue(child, 'fo:margin-top');
              if s <> '' then
                dist := PtsToMM(HTMLLengthStrToPts(s));
              if isHeader then
              begin
                data.PageLayout.HeaderMargin := h + dist;
                // Note: TopMargin and HeaderMargin are not yet the same as in Excel
                // Will be fixed in ReadMasterStyles where it will be known
                // whether the header is displayed.
              end else
              begin
                data.Pagelayout.FooterMargin := h + dist;
              end;
            end;
            childchild := child.FirstChild;
            while Assigned(childchild) do
            begin
              nodeName := childchild.NodeName;
              if nodeName = 'style:background-image' then
              begin
                href := GetAttrValue(childchild, 'xlink:href');
                imgpos := GetAttrValue(childchild, 'style:position');
                if (href <> '') and (imgpos <> '') then
                begin
                  n := (FWorkbook as TsWorkbook).FindEmbeddedObj(ExtractFileName(href));
                  if n > -1 then
                  begin
                    if pos('left', imgpos) > 0 then hfs := hfsLeft else
                      if pos('right', imgpos) > 0 then hfs := hfsRight else
                      hfs := hfsCenter;
                    if isHeader then
                      data.PageLayout.AddHeaderImage(HEADER_FOOTER_INDEX_ALL, hfs, n) else
                      data.PageLayout.AddFooterImage(HEADER_FOOTER_INDEX_ALL, hfs, n);
                  end;
                end;
              end;
              childchild := childchild.NextSibling;
            end;
            child := child.NextSibling;
          end;
        end;
        node := node.NextSibling;
      end;
    end;
    layoutNode := layoutNode.NextSibling;
  end;
end;

function TsSpreadOpenDocReader.ReadHeaderFooterText(ANode: TDOMNode): String;
var
  parser: TsSpreadOpenDocHeaderFooterParser;
  defFnt: TsHeaderFooterFont;
begin
  defFnt := TsHeaderFooterFont.Create((Workbook as TsWorkbook).GetDefaultFont);
  parser := TsSpreadOpenDocHeaderFooterParser.Create(ANode.FirstChild,
    FHeaderFooterFontList, defFnt);
  try
    Result := parser.BuildHeaderFooter;
  finally
    parser.Free;
    defFnt.Free;
  end;
end;

{ Reads the master styles nodes which contain the header/footer texts }
procedure TsSpreadOpenDocReader.ReadMasterStyles(AStylesNode: TDOMNode);
var
  masternode, stylenode: TDOMNode;
  nodeName: String;
  s: String;
  data: TMasterPageData;
  pagelayout: TsPageLayout;
  j: Integer;
  h: Double;
  hfs: TsHeaderFooterSectionIndex;
  hfnew: Array[TsHeaderFooterSectionIndex] of string;

begin
  if AStylesNode = nil then
    exit;

  masterNode := AStylesNode.FirstChild;
  while (masterNode <> nil) do
  begin
    nodeName := masterNode.NodeName;
    if nodeName = 'style:master-page' then begin
      s := GetAttrvalue(masterNode, 'style:page-layout-name');

      { Find the page layout data belonging to the current node }
      pageLayout := nil;
      for j:=0 to FPageLayoutList.Count-1 do
        if TPageLayoutData(FPageLayoutList[j]).Name = s then
        begin
          pageLayout := TPageLayoutData(FPageLayoutList[j]).PageLayout;
          break;
        end;
      if pagelayout = nil then
        exit;

      data := TMasterPageData.Create;
      data.Name := GetAttrValue(masternode, 'style:name');
      data.PageLayoutName := s;
      FMasterPageList.Add(data);

      styleNode := masterNode.FirstChild;
      while styleNode <> nil do begin
        nodeName := styleNode.NodeName;
        if nodeName = 'style:header' then
        begin
          s := ReadHeaderFooterText(styleNode);
          if s <> '' then
          begin
            // If the header contains an image add the code &G.
            with pagelayout do begin
              SplitHeaderFooterText(s,
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
              for hfs in TsHeaderFooterSectionIndex do
                if HeaderImages[hfs].Index > -1 then hfnew[hfs] := '&G' + hfnew[hfs];
              Headers[HEADER_FOOTER_INDEX_ODD] := JoinHeaderFooterText(
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
            end;
          end;
          s := GetAttrValue(styleNode, 'style:display');
          if s <> 'false' then
          begin
            h := pageLayout.HeaderMargin;
            pagelayout.HeaderMargin := pageLayout.TopMargin;
            pagelayout.TopMargin := pageLayout.TopMargin + h;
          end;
        end else
        if nodeName = 'style:header-left' then
        begin
          s := ReadHeaderFooterText(styleNode);
          if s <> '' then
          begin
            // If the header contains an image add the code &G.
            with pagelayout do begin
              SplitHeaderFooterText(s,
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
              for hfs in TsHeaderFooterSectionIndex do
                if HeaderImages[hfs].Index > -1 then hfnew[hfs] := '&G' + hfnew[hfs];
              Headers[HEADER_FOOTER_INDEX_ODD] := JoinHeaderFooterText(
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
              Options := Options + [poDifferentOddEven];
            end;
          end;
          s := GetAttrValue(styleNode, 'style:display');
          if s = 'false' then
            pageLayout.Options := pagelayout.Options - [poDifferentOddEven]
          else begin
            h := pageLayout.HeaderMargin;
            pageLayout.HeaderMargin := pageLayout.TopMargin;
            pagelayout.TopMargin := pageLayout.TopMargin + h;
          end;
        end else
        if nodeName = 'style:footer' then
        begin
          s := ReadHeaderFooterText(styleNode);
          if s <> '' then
            with pagelayout do begin
              SplitHeaderFooterText(s,
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
              for hfs in TsHeaderFooterSectionIndex do
                if FooterImages[hfs].Index > -1 then hfnew[hfs] := '&G' + hfnew[hfs];
              Footers[HEADER_FOOTER_INDEX_ODD] := JoinHeaderFooterText(
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
            end;
          s := GetAttrValue(styleNode, 'style:display');
          if s <> 'false' then
          begin
            h := pageLayout.FooterMargin;
            pageLayout.FooterMargin := pageLayout.BottomMargin;
            pageLayout.BottomMargin := pageLayout.BottomMargin + h;
          end;
        end else
        if nodeName = 'style:footer-left' then
        begin
          s := ReadHeaderFooterText(styleNode);
          if s <> '' then
          begin
            with pagelayout do begin
              SplitHeaderFooterText(s,
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
              for hfs in TsHeaderFooterSectionIndex do
                if FooterImages[hfs].Index > -1 then hfnew[hfs] := '&G' + hfnew[hfs];
              Footers[HEADER_FOOTER_INDEX_EVEN] := JoinHeaderFooterText(
                hfnew[hfsLeft], hfnew[hfsCenter], hfnew[hfsRight]);
              Options := Options + [poDifferentOddEven];
            end;
          end;
          s := GetAttrValue(styleNode, 'style:display');
          if s = 'false' then
            pagelayout.Options := pagelayout.Options - [poDifferentOddEven]
          else begin
            h := pagelayout.FooterMargin;
            pagelayout.FooterMargin := pagelayout.BottomMargin;
            pagelayout.BottomMargin := pagelayout.BottomMargin + h;
          end;
        end;
        styleNode := styleNode.NextSibling;
      end;
    end;
    masterNode := masterNode.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadMetaData(ANode: TDOMNode);
var
  book: TsWorkbook;
  nodeName: String;
  s: String;
  name: String;
begin
  book := TsWorkbook(FWorkbook);

  ANode := ANode.FirstChild;
  while ANode <> nil do
  begin
    nodeName := ANode.NodeName;
    s := GetNodeValue(ANode);
    case nodeName of
      'meta:initial-creator':
        book.MetaData.CreatedBy := s;
      'meta:creation-date':
        if s <> '' then
          book.MetaData.DateCreated := ISO8601StrToDateTime(s);
      'meta:keyword':
        if s <> '' then
          book.MetaData.KeyWords.Add(s);
      '<dc:creator>':
        if s <> '' then
          book.MetaData.LastModifiedBy := s;
      'dc:date':
        if s <> '' then
          book.MetaData.DateLastModified := ISO8601StrToDateTime(s);
      'dc:description':
        book.MetaData.Comments.Text := s;
      'dc:title':
        book.MetaData.Title := s;
      'dc:subject':
        book.Metadata.Subject := s;
      'meta:user-defined':
        begin
          name := GetAttrValue(ANode, 'meta:name');
          if name <> '' then
            book.MetaData.AddCustom(name, s);
        end;
    end;
    ANode := ANode.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadBlank(ARow, ACol: Cardinal;
  AStyleIndex: Integer; ACellNode: TDOMNode);
var
  cell: PCell;
begin
  Unused(ACellNode);

  // No need to store a record for an empty, unformatted cell
  if AStyleIndex = 0 then
    exit;

  if FIsVirtualMode then
  begin
    InitCell(FWorksheet, ARow, ACol, FVirtualCell);
    cell := @FVirtualCell;
  end else
    cell := (FWorksheet as TsWorksheet).AddCell(ARow, ACol);
  (FWorkSheet as TsWorksheet).WriteBlank(cell);
  ApplyStyleToCell(cell, AStyleIndex);

  if FIsVirtualMode then
    (Workbook as TsWorkbook).OnReadCellData(Workbook, ARow, ACol, cell);
end;

procedure TsSpreadOpenDocReader.ReadBoolean(ARow, ACol: Cardinal;
  AStyleIndex: Integer; ACellNode: TDOMNode);
var
//  styleName: String;
  cell: PCell;
  boolValue: Boolean;
begin
  if FIsVirtualMode then
  begin
    InitCell(FWorksheet, ARow, ACol, FVirtualCell);
    cell := @FVirtualCell;
  end else
    cell := (FWorksheet as TsWorksheet).AddCell(ARow, ACol);

  boolValue := ExtractBoolFromNode(ACellNode);
  (FWorkSheet as TsWorksheet).WriteBoolValue(cell, boolValue);

  ApplyStyleToCell(cell, AStyleIndex);
  {
  styleName := GetAttrValue(ACellNode, 'table:style-name');
  ApplyStyleToCell(cell, stylename);
   }
  if FIsVirtualMode then
    (Workbook as TsWorkbook).OnReadCellData(Workbook, ARow, ACol, cell);
end;

{ Collection columns used in the given table. The columns contain links to
  styles that must be used when cells in that columns are without styles. }
procedure TsSpreadOpenDocReader.ReadColumns(ATableNode: TDOMNode);
var
  col: Integer;
  colNode, childnode: TDOMNode;
  nodeName: String;
  i: Integer;

  procedure ProcessCol(AColNode: TDOMNode);
  var
    s: String;
    colStyleIndex: Integer;
    colData: TColumnData;
    defCellStyleIndex: Integer = -1;
    colsRepeated: Integer;
    j: Integer;
    isHidden: Boolean;
    isPageBreak: Boolean;
  begin
         {
    for j := 0 to FCellFormatList.Count-1 do
    begin
      WriteLn(j, ' ', FCellFormatList[j]^.Name);
    end;
          }

    s := GetAttrValue(AColNode, 'table:style-name');
    colStyleIndex := FindColStyleByName(s);
    if colStyleIndex <> -1 then
    begin
      defCellStyleIndex := -1;

      s := GetAttrValue(AColNode, 'table:visibility');
      isHidden := (s = 'collapse');

      isPageBreak := TColumnStyleData(FColumnStyleList[colStyleIndex]).PageBreak;

      s := GetAttrValue(AColNode, 'table:default-cell-style-name');
      if (s <> '') or isHidden or isPageBreak then
      begin
        defCellStyleIndex := FCellFormatList.FindIndexOfName(s); //FindCellStyleByName(s);
        colData := TColumnData.Create;
        colData.Col := col;
        colData.ColStyleIndex := colStyleIndex;
        colData.DefaultCellStyleIndex := defCellStyleIndex;
        colData.Hidden := isHidden;
        colData.PageBreak := isPageBreak;
        FColumnList.Add(colData);
      end;

      s := GetAttrValue(AColNode, 'table:number-columns-repeated');
      if s = '' then
        colsRepeated := 0
      else
        colsRepeated := StrToInt(s);
      inc(col);
      if (defCellStyleIndex > -1) or isHidden then begin
        for j := 1 to colsRepeated-1 do           // was: j := 0 to ...
        begin
          coldata := TColumnData.Create;
          colData.Col := col + j;
          colData.ColStyleIndex := colStyleIndex;
          colData.DefaultCellStyleIndex := defCellStyleIndex;
          colData.Hidden := isHidden;
          colData.PageBreak := isPageBreak;
          FColumnList.Add(colData);
          inc(col);
        end;
      end;
    end;
  end;

begin
  // clear previous column list (from other sheets)
  for i := FColumnList.Count-1 downto 0 do TObject(FColumnList[i]).Free;
  FColumnList.Clear;

  col := 0;
  colNode := ATableNode.FirstChild;
  while Assigned(colNode) do
  begin
    nodename := colNode.NodeName;
    if nodeName = 'table:table-header-columns' then
    begin
      if FRepeatedCols.FirstIndex = cardinal(UNASSIGNED_ROW_COL_INDEX) then
        FRepeatedCols.FirstIndex := col;
      childnode := colNode.FirstChild;
      while Assigned(childnode) do
      begin
        ProcessCol(childnode);
        childnode := childnode.NextSibling;
      end;
      FRepeatedCols.LastIndex := col-1;
    end
    else
    if nodeName = 'table:table-column' then
      ProcessCol(colnode);
    colNode := colNode.NextSibling;
  end;
end;

{ Reads the column styles and stores them in the FColumnStyleList for later use }
procedure TsSpreadOpenDocReader.ReadColumnStyle(AStyleNode: TDOMNode);
var
  colStyle: TColumnStyleData;
  styleName: String;
  styleChildNode: TDOMNode;
  colWidth: double;
  colPageBreak: Boolean;
  s: String;
  nodeName: String;
begin
  styleName := GetAttrValue(AStyleNode, 'style:name');
  styleChildNode := AStyleNode.FirstChild;
  colWidth := -1;
  colPageBreak := false;

  while Assigned(styleChildNode) do
  begin
    nodeName := styleChildNode.NodeName;
    if nodeName = 'style:table-column-properties' then
    begin
      s := GetAttrValue(styleChildNode, 'style:column-width');
      if s <> '' then
        // convert to workbook units
        colWidth := (FWorkbook as TsWorkbook).ConvertUnits(HTMLLengthStrToPts(s), suPoints, FWorkbook.Units);
      s := GetAttrValue(styleChildNode, 'fo:break-before');
      if s = 'page' then
        colPageBreak := true;
    end;
    styleChildNode := styleChildNode.NextSibling;
  end;

  colStyle := TColumnStyleData.Create;
  colStyle.Name := styleName;
  colStyle.ColWidth := colWidth;
  colStyle.PageBreak := colPageBreak;
  FColumnStyleList.Add(colStyle);
end;

procedure TsSpreadOpenDocReader.ReadComment(ARow, ACol: Cardinal;
  ACellNode: TDOMNode);
var
  cellChildNode, pNode, pChildNode: TDOMNode;
  comment, line: String;
  nodeName: String;
  s: String;
  found: Boolean;
begin
  if ACellNode = nil then
    exit;

  comment := '';
  found := false;

  cellChildNode := ACellNode.FirstChild;
  while cellChildNode <> nil do begin
    nodeName := cellChildNode.NodeName;
    if nodeName = 'office:annotation' then begin
      pNode := cellChildNode.FirstChild;
      while pNode <> nil do begin
        nodeName := pNode.NodeName;
        if nodeName = 'text:p' then
        begin
          line := '';
          pChildNode := pNode.FirstChild;
          while pChildNode <> nil do
          begin
            nodeName := pChildNode.NodeName;
            if nodeName = '#text' then
            begin
              s := pChildNode.NodeValue;
              line := IfThen(line = '', s, line + s);
              found := true;
            end else
            if nodeName = 'text:span' then
            begin
              s := GetNodeValue(pChildNode);
              line := IfThen(line = '', s, line + s);
              found := true;
            end;
            pChildNode := pChildNode.NextSibling;
          end;
          comment := IfThen(comment = '', line, comment + LineEnding + line);
        end;
        pNode := pNode.NextSibling;
      end;
    end;
    cellChildNode := cellChildNode.NextSibling;
  end;
  if found then
    (FWorksheet as TsWorksheet).WriteComment(ARow, ACol, comment);
end;

procedure TsSpreadOpenDocReader.ReadConditionalFormats(ANode: TDOMNode;
  AWorksheet: TsBasicWorksheet);
var
  childNode: TDOMNode;
  nodeName: String;
  s: String;
  range: TsCellRange = (Row1: Cardinal(-1); Col1: Cardinal(-1); Row2: Cardinal(-1); Col2: Cardinal(-1));
  sheet1, sheet2: string;
  flags: TsRelFlags;
begin
  ANode := ANode.FindNode('calcext:conditional-formats');
  if ANode = nil then
    exit;

  ANode := ANode.FirstChild;
  while ANode <> nil do
  begin
    nodeName := ANode.NodeName;
    if nodeName = 'calcext:conditional-format' then
    begin
      // Cell range
      s := GetAttrValue(ANode, 'calcext:target-range-address');
      if (s = '') or not TryStrToCellRange_ODS(s, sheet1, sheet2, range.Row1, range.Col1, range.Row2, range.Col2, flags) then
      begin
        ANode := ANode.NextSibling;
        Continue;
      end;

      childNode := ANode.FirstChild;
      while childNode <> nil do
      begin
        nodeName := childNode.NodeName;
        case nodeName of
          'calcext:condition': ReadCFCellFormat(childNode, AWorksheet, range);
          'calcext:date-is': ReadCFDateFormat(childNode, AWorksheet, range);
          'calcext:color-scale': ReadCFColorScale(childNode, AWorksheet, range);
          'calcext:data-bar': ReadCFDataBars(childNode, AWorksheet, range);
          'calcext:icon-set': ReadCFIconSet(childNode, AWorksheet, range);
        end;
        childNode := childNode.NextSibling;
      end;
    end;
    ANode := ANode.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadDateTime(ARow, ACol: Cardinal;
  AStyleIndex: Integer; ACellNode: TDOMNode);
var
  dt: TDateTime;
//  styleName: String;
  cell: PCell;
  fmt: PsCellFormat;
begin
  if FIsVirtualMode then
  begin
    InitCell(FWorksheet, ARow, ACol, FVirtualCell);
    cell := @FVirtualCell;
  end else
    cell := (FWorksheet as TsWorksheet).AddCell(ARow, ACol);

  ApplyStyleToCell(cell, AStyleIndex);
  //styleName := GetAttrValue(ACellNode, 'table:style-name');
  //ApplyStyleToCell(cell, stylename);
  fmt := (FWorkbook as TsWorkbook).GetPointerToCellFormat(cell^.FormatIndex);;

  dt := ExtractDateTimeFromNode(ACellNode, fmt^.NumberFormat, fmt^.NumberFormatStr);
  (FWorkSheet as TsWorksheet).WriteDateTime(cell,
    dt, fmt^.NumberFormat, fmt^.NumberFormatStr
  );

  if FIsVirtualMode then
    TsWorkbook(Workbook).OnReadCellData(Workbook, ARow, ACol, cell);
end;

procedure TsSpreadOpenDocReader.ReadError(ARow, ACol: Cardinal;
  AStyleIndex: Integer; ACellNode: TDOMNode);
var
  //styleName: String;
  cell: PCell;
  errValue: TsErrorValue;
begin
  if FIsVirtualMode then
  begin
    InitCell(FWorksheet, ARow, ACol, FVirtualCell);
    cell := @FVirtualCell;
  end else
    cell := TsWorksheet(FWorksheet).AddCell(ARow, ACol);

  if ExtractErrorFromNode(ACellNode, errValue) then
    TsWorksheet(FWorkSheet).WriteErrorValue(cell, errValue)
  else
    TsWorksheet(FWorksheet).WriteText(cell, 'ERROR');

  ApplyStyleToCell(cell, AStyleIndex);
  {
  styleName := GetAttrValue(ACellNode, 'table:style-name');
  ApplyStyleToCell(cell, stylename);
  }

  if FIsVirtualMode then
    TsWorkbook(Workbook).OnReadCellData(Workbook, ARow, ACol, cell);
end;

procedure TsSpreadOpenDocReader.ReadDateMode(SpreadSheetNode: TDOMNode);
var
  CalcSettingsNode, NullDateNode: TDOMNode;
  NullDateSetting: string;
begin
  // Default datemode for ODF:
  NullDateSetting := '1899-12-30';
  CalcSettingsNode := SpreadsheetNode.FindNode('table:calculation-settings');
  if Assigned(CalcSettingsNode) then
  begin
    NullDateNode := CalcSettingsNode.FindNode('table:null-date');
    if Assigned(NullDateNode) then
      NullDateSetting := GetAttrValue(NullDateNode,'table:date-value');
  end;
  if NullDateSetting = '1899-12-30' then
    FDateMode := dmODS1899
  else if NullDateSetting = '1900-01-01' then
    FDateMode := dmODS1900
  else if NullDateSetting = '1904-01-01' then
    FDateMode := dmODS1904
  else
    raise EFPSpreadsheetReader.CreateFmt('Spreadsheet file corrupt: cannot handle null-date format %s', [NullDateSetting]);
end;

procedure TsSpreadOpenDocReader.ReadDocumentProtection(ANode: TDOMNode);
var
  cinfo: TsCryptoInfo;
begin
  if ANode = nil then
    exit;

  if GetAttrValue(ANode, 'table:structure-protected') = 'true' then
    Workbook.Protection := Workbook.Protection + [bpLockStructure]
  else
    exit;

  InitCryptoInfo(cinfo);
  cinfo.PasswordHash := GetAttrValue(ANode, 'table:protection-key');
  cinfo.Algorithm := StrToAlgorithm(GetAttrValue(ANode, 'table:protection-key-digest-algorithm'));
  (Workbook as TsWorkbook).CryptoInfo := cinfo;
end;

{ Reads font data from an xml node and returns the font elements. }
procedure TsSpreadOpenDocReader.ReadFont(ANode: TDOMNode; var AFontName: String;
  var AFontSize: Single; var AFontStyle: TsFontStyles; var AFontColor: TsColor;
  var AFontPosition: TsFontPosition);
const
  EPS = 1E-6;
var
  stylename, s, s1, s2: String;
  i, p: Integer;
begin
  if ANode = nil then
    exit;

  stylename := GetAttrValue(ANode, 'style:font-name');
  if stylename <> '' then
    // Look for the true font name in the FFontFaces list. The items in
    // FFontfaces are "style name"|"font name" pairs.
    for i:=0 to FFontFaces.Count-1 do
    begin
      p := pos('|', FFontFaces[i]);
      if p > 0 then begin
        s := copy(FFontfaces[i], 1, p-1);  // The first part is the style name
        if s = styleName then
        begin
          AFontName := copy(FFontfaces[i], p+1, MaxInt);
          // the second part is the font name
          break;
        end;
      end;
    end;
  // In all other case, leave the AFontName of the input untouched.

  s := GetAttrValue(ANode, 'fo:font-size');
  if s <> '' then
    AFontSize := HTMLLengthStrToPts(s);

  if GetAttrValue(ANode, 'fo:font-style') = 'italic' then
    Include(AFontStyle, fssItalic);

  if GetAttrValue(ANode, 'fo:font-weight') = 'bold' then
    Include(AFontStyle, fssBold);

  s := GetAttrValue(ANode, 'style:text-underline-style');
  if not ((s = '') or (s = 'none')) then
    Include(AFontStyle, fssUnderline);

  s := GetAttrValue(ANode, 'style:text-line-through-style');
  if s = '' then s := GetAttrValue(ANode, 'style:text-line-through-type');
  if not ((s = '') or (s = 'none')) then
    Include(AFontStyle, fssStrikeout);

  { The "style:text-position" attribute specifies whether text is positioned
    above or below the baseline and defines the relative font height that is
    used for this text. The attribute can have one or two values.
    1st value: percentage of vertical text displacement, or "super" or "sub"
    2nd value: percentage of font height used for text (optional) }
  s := GetAttrValue(ANode, 'style:text-position');
  if s <> '' then
  begin
    p := pos(' ', s);
    if p > 0 then
    begin
      s1 := Copy(s, 1, p-1);
      s2 := Copy(s, p+1, MaxInt);
    end else
    begin
      s1 := s;
      s2 := '100';
    end;
    if s1[Length(s1)] = '%' then SetLength(s1, Length(s1) - 1);
    if s2[Length(s2)] = '%' then SetLength(s2, Length(s2) - 1);
    if s1 = 'super' then
      AFontPosition := fpSuperscript
    else if s1 = 'sub' then
      AFontPosition := fpSubscript
    else if SameValue(StrToFloat(s1, FPointSeparatorSettings), 0.0, EPS) then
      AFontPosition := fpNormal
    else if s1[1] = '-' then
      AFontPosition := fpSubScript;
  end;

  s := GetAttrValue(ANode, 'fo:color');
  if s <> '' then
    AFontColor := HTMLColorStrToColor(s);
end;


                            (*
{ Reads font data from an xml node, adds the font to the workbooks FontList
  (if not yet contained), and returns the index in the font list.
  If the font is a special font (such as DefaultFont, or HyperlinkFont) then
  APreferredIndex defines the index under which the font should be stored in the
  list. }
function TsSpreadOpenDocReader.ReadFont(ANode: TDOMnode;
  APreferredIndex: Integer = -1): Integer;
var
  fntName: String;
  fntSize: Single;
  fntStyles: TsFontStyles;
  fntColor: TsColor;
  fntPosition: TsFontPosition;
  s: String;
  i: Integer;
  p: Integer;
begin
  if ANode = nil then
  begin
    Result := 0;
    exit;
  end;

  fntName := GetAttrValue(ANode, 'style:font-name');
  if fntName = '' then
    fntName := FWorkbook.GetDefaultFont.FontName
  else
    // Look for the true font name in the FFontFaces list. The items in
    // FFontfaces are "style name"|"font name" pairs.
    for i:=0 to FFontFaces.Count-1 do
    begin
      p := pos('|', FFontFaces[i]);
      if p > 0 then begin
        s := copy(FFontfaces[i], 1, p-1);
        if s = fntName then
        begin
          fntName := copy(FFontfaces[i], p+1, MaxInt);
          break;
        end;
      end;
    end;

  s := GetAttrValue(ANode, 'fo:font-size');
  if s <> '' then
    fntSize := HTMLLengthStrToPts(s)
  else
    fntSize := FWorkbook.GetDefaultFontSize;

  fntStyles := [];
  if GetAttrValue(ANode, 'fo:font-style') = 'italic' then
    Include(fntStyles, fssItalic);
  if GetAttrValue(ANode, 'fo:font-weight') = 'bold' then
    Include(fntStyles, fssBold);
  s := GetAttrValue(ANode, 'style:text-underline-style');
  if not ((s = '') or (s = 'none')) then
    Include(fntStyles, fssUnderline);
  s := GetAttrValue(ANode, 'style:text-line-through-style');
  if s = '' then s := GetAttrValue(ANode, 'style:text-line-through-type');
  if not ((s = '') or (s = 'none')) then
    Include(fntStyles, fssStrikeout);

  fntPosition := fpNormal;
  s := GetAttrValue(ANode, 'style:text-position');
  if Length(s) >= 3 then
  begin
    if (s[3] = 'b') or (s[1] = '-') then
      fntPosition := fpSubscript
    else
      fntPosition := fpSuperscript;
  end;

  s := GetAttrValue(ANode, 'fo:color');
  if s <> '' then
    fntColor := HTMLColorStrToColor(s)
  else
    fntColor := FWorkbook.GetDefaultFont.Color;

  if APreferredIndex = 0 then
  begin
    FWorkbook.SetDefaultFont(fntName, fntSize);
    Result := 0;
  end else
  if (APreferredIndex > -1) then
  begin
    FWorkbook.ReplaceFont(APreferredIndex, fntName, fntSize, fntStyles, fntColor, fntPosition);
    Result := APreferredIndex;
  end else
  begin
    Result := FWorkbook.FindFont(fntName, fntSize, fntStyles, fntColor, fntPosition);
    if Result = -1 then
      Result := FWorkbook.AddFont(fntName, fntSize, fntStyles, fntColor, fntPosition);
  end;
end;
                              *)
{ Collects the fontnames associated with a style-name in the list FFontfaces.
  stylenames and fontnames are packed into a single string using | as a
  separator. }
procedure TsSpreadOpenDocReader.ReadFontFaces(ANode: TDOMNode);
var
  faceNode: TDOMNode;
  nodename: String;
  stylename: String;
  fontfamily: String;
begin
  faceNode := ANode.FirstChild;
  while Assigned(faceNode) do
  begin
    nodename := faceNode.NodeName;
    if nodename = 'style:font-face' then
    begin
      stylename := GetAttrValue(faceNode, 'style:name');
      fontfamily := GetAttrValue(faceNode, 'svg:font-family');
      if FFontFaces.IndexOf(stylename + '|' + fontfamily) = -1 then
        FFontFaces.Add(stylename + '|' + fontfamily);
    end;
    faceNode := faceNode.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadFormula(ARow, ACol: Cardinal;
  AStyleIndex: Integer; ACellNode: TDOMNode);
var
  cell: PCell;
  formula: PsFormula;
  formulaStr: String;
//  stylename: String;
  floatValue: Double;
  boolValue: Boolean;
  errorValue: TsErrorValue;
  valueType, calcExtValueType: String;
  valueStr: String;
  node: TDOMNode;
  p: Integer;
  fmt: PsCellFormat;
  ns: String;
begin
  {$IFDEF FPSpreadDebug}
  DebugLn(Format('[ReadFormula] ARow=%d, ACol=%d, AStyleIndex=%d', [ARow, ACol, AStyleIndex]));
  {$ENDIF}

  // Create cell and apply format
  if FIsVirtualMode then
  begin
    InitCell(FWorksheet, ARow, ACol, FVirtualCell);
    cell := @FVirtualCell;
  end else
    cell := TsWorksheet(FWorksheet).GetCell(ARow, ACol);   // Don't use AddCell here

  ApplyStyleToCell(cell, AStyleIndex);
  fmt := TsWorkbook(Workbook).GetPointerToCellFormat(cell^.FormatIndex);

  formulaStr := '';
  if (boReadFormulas in FWorkbook.Options) then
  begin
    // Read formula, trim it, ...
    formulaStr := GetAttrValue(ACellNode, 'table:formula');
    if formulaStr <> '' then
    begin
      // Formulas written by Spread begin with 'of:=', by Excel with 'msof:='.
      // Remove that. And both use different list separators.
      p := pos('=', formulaStr);
      ns := Copy(formulaStr, 1, p-2);
      case ns of
        'of'   : FPointSeparatorSettings.ListSeparator := ';';
        'msoxl': FPointSeparatorSettings.ListSeparator := ',';
      end;
      Delete(formulaStr, 1, p);
    end;

    // ... and store in cell's FormulaValue field. Convert from ODS to ExcelA1 dialect.
    formula := TsWorksheet(FWorksheet).Formulas.AddFormula(ARow, ACol);
    formula^.Parser := TsSpreadsheetParser.Create(FWorksheet);
    formula^.Parser.Expression[fdOpenDocument] := formulaStr;  // Parse in ODS dialect
    formula^.Text := formula^.Parser.Expression[fdExcelA1];    // Convert to Excel A1 dialect
    cell^.Flags := cell^.Flags + [cfHasFormula];

    {$IFDEF FPSpreadDebug}
    DebugLn('  Formula found: ' + formula);
    {$ENDIF}
  end;

  // Read formula results

  // Prevent formulas from being erased when formula results are written to cells
  TsWorkbook(FWorkbook).LockFormulas;
  try
    valueType := GetAttrValue(ACellNode, 'office:value-type');
    valueStr := GetAttrValue(ACellNode, 'office:value');
    calcExtValueType := GetAttrValue(ACellNode, 'calcext:value-type');
    // ODS wants a 0 in the NumberValue field in case of an error. If there is
    // no error, this value will be corrected below.
    cell^.NumberValue := 0.0;
    // (a) number value
    if (valueType = 'float') then
    begin
      if UpperCase(valueStr) = '1.#INF' then
        TsWorksheet(FWorksheet).WriteNumber(cell, 1.0/0.0)
      else
      begin
        floatValue := StrToFloat(valueStr, FPointSeparatorSettings);
        TsWorksheet(FWorksheet).WriteNumber(cell, floatValue);
      end;
      if IsDateTimeFormat(fmt^.NumberFormat) then
      begin
        cell^.ContentType := cctDateTime;
        // No datemode correction for intervals and for time-only values
        if (fmt^.NumberFormat = nfTimeInterval) or (cell^.NumberValue < 1) then
          cell^.DateTimeValue := cell^.NumberValue
        else
          case FDateMode of
            dmODS1899: cell^.DateTimeValue := cell^.NumberValue + DATEMODE_1899_BASE;
            dmODS1900: cell^.DateTimeValue := cell^.NumberValue + DATEMODE_1900_BASE;
            dmODS1904: cell^.DateTimeValue := cell^.NumberValue + DATEMODE_1904_BASE;
          end;
      end;
    end else
    // (b) Date/time value
    if (valueType = 'date') or (valueType = 'time') then
    begin
      floatValue := ExtractDateTimeFromNode(ACellNode, fmt^.NumberFormat, fmt^.NumberFormatStr);
      TsWorksheet(FWorkSheet).WriteDateTime(cell, floatValue);
    end else
    // (c) text
    if (valueType = 'string') and (calcextValueType <> 'error') then
    begin
      node := ACellNode.FindNode('text:p');
      if (node <> nil) and (node.FirstChild <> nil) then
      begin
        valueStr := node.FirstChild.Nodevalue;
        TsWorksheet(FWorksheet).WriteText(cell, valueStr);
      end;
    end else
    // (d) boolean
    if (valuetype = 'boolean') then
    begin
      boolValue := ExtractBoolFromNode(ACellNode);
      TsWorksheet(FWorksheet).WriteBoolValue(cell, boolValue);
    end else
    if (calcextValuetype = 'error') then
    begin
      if ExtractErrorFromNode(ACellNode, errorValue) then
        TsWorksheet(FWorksheet).WriteErrorValue(cell, errorValue) else
        TsWorksheet(FWorksheet).WriteText(cell, 'ERROR');
    end else
    // (e) Text
    if (valueStr <> '') then
      TsWorksheet(FWorksheet).WriteText(cell, valueStr);

    if FIsVirtualMode then
      TsWorkbook(Workbook).OnReadCellData(Workbook, ARow, ACol, cell);

  finally
    TsWorkbook(FWorkbook).UnlockFormulas;
  end;
end;

procedure TsSpreadOpenDocReader.ReadFromStream(AStream: TStream;
  APassword: String = ''; AParams: TsStreamParams = []);
var
  Doc : TXMLDocument;
  BodyNode, SpreadSheetNode, TableNode: TDOMNode;
  StylesNode: TDOMNode;
  OfficeSettingsNode: TDOMNode;
  nodename: String;
  XMLStream: TStream;
  sheet: TsWorksheet;
  sheetName: String;
  tablestyleName: String;

  function CreateXMLStream: TStream;
  begin
    if boFileStream in FWorkbook.Options then
      Result := TFileStream.Create(GetTempFileName, fmCreate)
    else
    if boBufStream in FWorkbook.Options then
      Result := TBufStream.Create(GetTempFileName, fmCreate)
    else
      Result := TMemoryStream.Create;
  end;

begin
  Unused(APassword, AParams);

  Doc := nil;
  try
    // Extract the embedded pictures
    ReadPictures(AStream);

    // process the styles.xml file
    XMLStream := CreateXMLStream;
    try
      if UnzipToStream(AStream, 'styles.xml', XMLStream) then
        ReadXMLStream(Doc, XMLStream);
    finally
      XMLStream.Free;
    end;

    if Assigned(Doc) then begin
      ReadFontFaces(Doc.DocumentElement.FindNode('office:font-face-decls'));

      StylesNode := Doc.DocumentElement.FindNode('office:styles');
      ReadNumFormats(StylesNode);
      ReadStyles(StylesNode);
      ReadAutomaticStyles(Doc.DocumentElement.FindNode('office:automatic-styles'));
      ReadMasterStyles(Doc.DocumentElement.FindNode('office:master-styles'));
      FreeAndNil(Doc);
    end;

    // process the content.xml file
    XMLStream := CreateXMLStream;
    try
      if UnzipToStream(AStream, 'content.xml', XMLStream) then
        ReadXMLStream(Doc, XMLStream)
      else
        raise EFPSpreadsheetReader.CreateFmt(rsDefectiveInternalFileStructure, ['ods']);
    finally
      XMLStream.Free;
    end;

    if Assigned(Doc) then begin
      ReadFontFaces(Doc.DocumentElement.FindNode('office:font-face-decls'));
      StylesNode := Doc.DocumentElement.FindNode('office:automatic-styles');
      ReadNumFormats(StylesNode);
      ReadStyles(StylesNode);

      BodyNode := Doc.DocumentElement.FindNode('office:body');
      if not Assigned(BodyNode) then
        raise EFPSpreadsheetReader.Create('[TsSpreadOpenDocReader.ReadFromStream] Node "office:body" not found.');

      SpreadSheetNode := BodyNode.FindNode('office:spreadsheet');
      if not Assigned(SpreadSheetNode) then
        raise EFPSpreadsheetReader.Create('[TsSpreadOpenDocReader.ReadFromStream] Node "office:spreadsheet" not found.');

      ReadSheets(SpreadsheetNode);
      ReadDocumentProtection(SpreadsheetNode);
      ReadDateMode(SpreadSheetNode);

      //process each table (sheet)
      TableNode := SpreadSheetNode.FindNode('table:table');
      while Assigned(TableNode) do
      begin
        nodename := TableNode.Nodename;
        // These nodes occur due to leading spaces which are not skipped
        // automatically any more due to PreserveWhiteSpace option applied
        // to ReadXMLFile
        if nodeName <> 'table:table' then
        begin
          TableNode := TableNode.NextSibling;
          continue;
        end;

        // Tables with external references contain a copy of the external table
        // having the filename as sheet name - which is not valid for fps.
        // Since external references are not supported ATM we skip this table.
        if TableNode.FindNode('table:table-source') <> nil then begin
          TableNode := TableNode.NextSibling;
          Continue;
        end;

        sheetName := GetAttrValue(TableNode, 'table:name');
        FWorksheet := TsWorkbook(FWorkbook).GetWorksheetByName(sheetName);
//      FWorkSheet := TsWorkbook(FWorkbook).AddWorksheet(sheetName, true);
        tablestyleName := GetAttrValue(TableNode, 'table:style-name');
        // Read protection
        ReadSheetProtection(TableNode, FWorksheet);
        // Collect embedded images
        ReadShapes(TableNode);
        // Collect column styles used
        ReadColumns(TableNode);
        // Process each row inside the sheet and process each cell of the row
        ReadRowsAndCells(TableNode);
        // Read conditional formats
        ReadConditionalFormats(TableNode, FWorksheet);
        // Read page layout
        ReadPageLayout(StylesNode, GetAttrValue(TableNode, 'table:style-name'),
          (FWorksheet as TsWorksheet).PageLayout);
        // Repeated cols/rows already have been determined.
        (FWorksheet as TsWorksheet).PageLayout.SetRepeatedRows(
          FRepeatedRows.FirstIndex, FRepeatedRows.LastIndex);
        (FWorksheet as TsWorksheet).PageLayout.SetRepeatedCols(
          FRepeatedCols.FirstIndex, FRepeatedCols.LastIndex);
        // Read print ranges
        ReadPrintRanges(TableNode, FWorksheet);
        // Apply table style
        ApplyTableStyle(FWorksheet, tablestylename);
        // Handle columns
        ApplyColData;
        // Page layout
        FixCols(FWorksheet);
        FixRows(FWorksheet);
        // Continue with next table
        TableNode := TableNode.NextSibling;
      end; //while Assigned(TableNode)

      FreeAndNil(Doc);
    end;

    // process the meta.xml file
    XMLStream := CreateXMLStream;
    try
      if UnzipToStream(AStream, 'meta.xml', XMLStream) then
      begin
        ReadXMLStream(Doc, XMLStream);
        ReadMetaData(Doc.DocumentElement.FindNode('office:meta'));
      end;
    finally
      XMLStream.Free;
    end;

    // process the settings.xml file (Note: it does not always exist!)
    XMLStream := CreateXMLStream;
    try
      if UnzipToStream(AStream, 'settings.xml', XMLStream) then
      begin
        ReadXMLStream(Doc, XMLStream);
        OfficeSettingsNode := Doc.DocumentElement.FindNode('office:settings');
        ReadSettings(OfficeSettingsNode);
      end;
    finally
      XMLStream.Free;
    end;

    // Active sheet
    if FActiveSheet <> '' then
      sheet := (FWorkbook as TsWorkbook).GetWorksheetByName(FActiveSheet) else
      sheet := (FWorkbook as TsWorkbook).GetWorksheetByIndex(0);
    (FWorkbook as TsWorkbook).SelectWorksheet(sheet);

  finally
    FreeAndNil(Doc);
  end;
end;

procedure TsSpreadOpenDocReader.ReadHeaderFooterFont(ANode: TDOMNode;
  var AFontName: String; var AFontSize: Double;
  var AFontStyle: TsHeaderFooterFontStyles; var AFontColor: TsColor);
var
  s: String;
begin
  if ANode = nil then
    exit;

  AFontName := GetAttrValue(ANode, 'style:font-name');

  s := GetAttrValue(ANode, 'fo:font-size');
  if s <> '' then
    AFontSize := HTMLLengthStrToPts(s);

  AFontStyle := [];

  if GetAttrValue(ANode, 'fo:font-style') = 'italic' then
    Include(AFontStyle, hfsItalic);

  if GetAttrValue(ANode, 'fo:font-weight') = 'bold' then
    Include(AFontStyle, hfsBold);

  s := GetAttrValue(ANode, 'style:text-underline-style');
  if not ((s = '') or (s = 'none')) then
  begin
    if GetAttrValue(ANode, 'style:text-underline-type') = 'double' then
      Include(AFontStyle, hfsDblUnderline)
    else
      Include(AFontStyle, hfsUnderline);
  end;

  s := GetAttrValue(ANode, 'style:text-line-through-style');
  if not ((s = '') or (s = 'none')) then
    Include(AFontStyle, hfsStrikeout);

  if GetAttrValue(ANode, 'style:text-outline') = 'true' then
    Include(AFontStyle, hfsOutline);

  s := GetAttrValue(ANode, 'fo:text-shadow');
  if not ((s = '') or (s = 'none')) then
    Include(AFontStyle, hfsShadow);

  s := GetAttrValue(ANode, 'style:text-position');
  if pos('sub', s) = 1 then
    Include(AFontStyle, hfsSubscript)
  else if pos('super', s) = 1 then
    Include(AFontStyle, hfsSuperscript);

  s := GetAttrValue(ANode, 'fo:color');
  if s <> '' then
    AFontColor := HTMLColorStrToColor(s);
end;

procedure TsSpreadOpenDocReader.ReadLabel(ARow, ACol: Cardinal;
  AStyleIndex: Integer; ACellNode: TDOMNode);
var
  cellText, spanText: String;
  styleName: String;
  childnode: TDOMNode;
  subnode: TDOMNode;
  nodeName: String;
  cell: PCell;
  hyperlink: string;
  rtParams: TsRichTextParams = nil;
  idx: Integer;
  rtFntIndex, fntIndex: Integer;
  rtFnt, fnt: TsFont;
  fmt: PsCellFormat;

  procedure AddToCellText(AText: String);
  begin
    if cellText = '' then
      cellText := AText
    else
      cellText := cellText + AText;
  end;

begin
  // Initalize cell
  if FIsVirtualMode then
  begin
    InitCell(FWorksheet, ARow, ACol, FVirtualCell);
    cell := @FVirtualCell;
  end else
    cell := (FWorksheet as TsWorksheet).AddCell(ARow, ACol);

  // Apply style to cell
  // We do this already here because we need the cell font for rich-text
  ApplyStyleToCell(cell, AStyleIndex);
  {
  styleName := GetAttrValue(ACellNode, 'table:style-name');
  ApplyStyleToCell(cell, stylename);
  }
  fmt := (FWorkbook as TsWorkbook).GetPointerToCellFormat(cell^.FormatIndex);
  fntIndex := fmt^.FontIndex;
  fnt := (FWorkbook as TsWorkbook).GetFont(fntIndex);

  // Prepare reading of node data
  cellText := '';
  hyperlink := '';
  SetLength(rtParams, 0);
  childnode := ACellNode.FirstChild;
  while Assigned(childnode) do
  begin
    nodeName := childNode.NodeName;
    if nodeName = 'text:p' then begin
      // Each 'text:p' node is a paragraph --> we insert a line break after the first paragraph
      if cellText <> '' then
        cellText := cellText + LineEnding;
      subnode := childnode.FirstChild;
      while Assigned(subnode) do
      begin
        nodename := subnode.NodeName;
        case nodename of
          '#text' :
            begin
              if Length(rtParams) > 0 then
              begin
                SetLength(rtParams, Length(rtParams) + 1);
                rtParams[High(rtParams)].FirstIndex := UTF8Length(cellText) + 1;
                rtParams[High(rtParams)].FontIndex := fntIndex;
                rtParams[High(rtParams)].HyperlinkIndex := -1;  // TO DO !!!!
              end;
              AddToCellText(subnode.TextContent);
            end;
          'text:a':     // "hyperlink anchor"
            begin
              hyperlink := GetAttrValue(subnode, 'xlink:href');
              AddToCellText(subnode.TextContent);
            end;
          'text:span':
            begin
              spanText := subnode.TextContent;
              stylename := GetAttrValue(subnode, 'text:style-name');
              if stylename <> '' then begin
                idx := FCellFormatList.FindIndexOfName(stylename);
                if idx > -1 then
                begin
                  rtFntIndex := FCellFormatList[idx]^.FontIndex;
                  rtFnt := TsFont(FRichTextFontList[rtFntIndex]);
                  // Replace missing font elements by those from the cell font
                  if rtFnt.FontName = '' then rtFnt.FontName := fnt.FontName;
                  if rtFnt.Size = -1 then rtFnt.Size := fnt.Size;
                  if rtFnt.Style = [] then rtFnt.Style := fnt.Style;
                  if rtFnt.Color = scNone then rtFnt.Color := fnt.Color;
                  if rtFnt.Position = fpNormal then rtFnt.Position := fnt.Position;
                  // Find this font in the workbook's font list
                  rtfntIndex := (FWorkbook as TsWorkbook).FindFont(
                    rtFnt.FontName, rtFnt.Size, rtFnt.Style, rtFnt.Color, rtFnt.Position
                  );
                  // If not found add to font list
                  if rtfntIndex = -1 then
                    rtfntIndex := (FWorkbook as TsWorkbook).AddFont(
                      rtFnt.FontName, rtFnt.Size, rtFnt.Style, rtFnt.Color, rtFnt.Position
                    );
                  // Use this font index in the rich-text parameter
                  SetLength(rtParams, Length(rtParams)+1);
                  rtParams[High(rtParams)].FirstIndex := UTF8Length(cellText) + 1;  // 1-based character index
                  rtParams[High(rtParams)].FontIndex := rtFntIndex;
                  rtParams[High(rtParams)].HyperlinkIndex := -1;  // TO DO !!!!
                end;
              end;
              AddToCellText(spanText);
            end;
          'text:line-break':
            AddToCellText(FPS_LINE_ENDING);
        end;
        subnode := subnode.NextSibling;
      end;
    end;
    childnode := childnode.NextSibling;
  end;

  (FWorkSheet as TsWorksheet).WriteText(cell, cellText, rtParams);
  if hyperlink <> '' then
  begin
    // ODS sees relative paths relative to the internal own file structure
    // --> we must remove 1 level-up to be at the same level where fps expects
    // the file.
    if pos('../', hyperlink) = 1 then
      Delete(hyperlink, 1, Length('../'));
    (FWorksheet as TsWorksheet).WriteHyperlink(cell, hyperlink);
    (FWorksheet as TsWorksheet).WriteFont(cell, HYPERLINK_FONTINDEX);
  end;

  if FIsVirtualMode then
    (Workbook as TsWorkbook).OnReadCellData(Workbook, ARow, ACol, cell);
end;

procedure TsSpreadOpenDocReader.ReadNumber(ARow, ACol: Cardinal;
  AStyleIndex: Integer; ACellNode: TDOMNode);
var
  Value, Str: String;
  lNumber: Double;
//  styleName: String;
  cell: PCell;
  fmt: PsCellFormat;
  numFmt: TsNumFormatParams;
  txtNode: TDOMNode;
begin
  if FIsVirtualMode then
  begin
    InitCell(FWorksheet, ARow, ACol, FVirtualCell);
    cell := @FVirtualCell;
  end else
    cell := (FWorksheet as TsWorksheet).AddCell(ARow, ACol);

  Value := GetAttrValue(ACellNode,'office:value');
  if UpperCase(Value)='1.#INF' then
    (FWorkSheet as TsWorksheet).WriteNumber(cell, 1.0/0.0)
  else
  begin
    // Don't merge, or else we can't debug
    Str := GetAttrValue(ACellNode, 'office:value');
    lNumber := StrToFloat(Str, FPointSeparatorSettings);
    (FWorkSheet as TsWorksheet).WriteNumber(cell, lNumber);
  end;

  ApplyStyleToCell(cell, AStyleIndex);
  {
  styleName := GetAttrValue(ACellNode, 'table:style-name');
  ApplyStyleToCell(cell, stylename);
  }
  fmt := (Workbook as TsWorkbook).GetPointerToCellFormat(cell^.FormatIndex);
  numFmt := (Workbook as TsWorkbook).GetNumberFormat(fmt^.NumberFormatIndex);

  // Sometimes date/time cells are stored as "float".
  // We convert them to date/time and also correct the date origin offset if
  // needed.
  if IsDateTimeFormat(numFmt) then
  begin
    cell^.ContentType := cctDateTime;
    // No datemode correction for intervals and for time-only values
    if (numFmt.NumFormat = nfTimeInterval) or (cell^.NumberValue < 1) then
      cell^.DateTimeValue := cell^.NumberValue
    else
      case FDateMode of
        dmODS1899: cell^.DateTimeValue := cell^.NumberValue + DATEMODE_1899_BASE;
        dmODS1900: cell^.DateTimeValue := cell^.NumberValue + DATEMODE_1900_BASE;
        dmODS1904: cell^.DateTimeValue := cell^.NumberValue + DATEMODE_1904_BASE;
      end;
  end else
  if IsTextFormat(numFmt) then begin
    // Cell has TEXT format @ --> store number as text
    txtNode := ACellNode.FirstChild;
    if txtNode.NodeName = 'text:p' then
      (FWorksheet as TsWorksheet).WriteText(cell, GetNodeValue(txtNode));
  end;

  if FIsVirtualMode then
    (Workbook as TsWorkbook).OnReadCellData(Workbook, ARow, ACol, cell);
end;

procedure TsSpreadOpenDocReader.ReadNumFormats(AStylesNode: TDOMNode);

  procedure ReadStyleMap(ANode: TDOMNode; var ANumFormat: TsNumberFormat;
    var AFormatStr: String);
  var
    condition: String;
    stylename: String;
    styleindex: Integer;
    fmt: String;
    posfmt, negfmt, zerofmt, currfmt: String;
    nf: TsNumberFormat;
    parser: TsNumFormatParser;
    counter: Integer;
    op: TsCompareOperation;
    x: Extended;
  begin
    posfmt := '';
    negfmt := '';
    zerofmt := '';
    currfmt := AFormatStr;
    counter := 0;

    AFormatStr := '';
    ANumFormat := nfCustom;

    while ANode <> nil do
    begin
      condition := ANode.NodeName;

      if (ANode.NodeName = '#text') or not ANode.HasAttributes then
      begin
        ANode := ANode.NextSibling;
        Continue;
      end;

      condition := GetAttrValue(ANode, 'style:condition');
      stylename := GetAttrValue(ANode, 'style:apply-style-name');
      if (condition = '') or (stylename = '') then
      begin
        ANode := ANode.NextSibling;
        continue;
      end;

      Delete(condition, 1, Length('value()'));
      styleindex := -1;
      styleIndex := FindNumFormatByName(stylename);
      if (styleindex = -1) or (condition = '') then
      begin
        ANode := ANode.NextSibling;
        continue;
      end;

      fmt := NumFormatList[styleIndex];
      fmt := Copy(fmt, pos(':', fmt)+1, Length(fmt));
      parser := TsNumFormatParser.Create(fmt, Workbook.FormatSettings);
      try
        nf := parser.NumFormat;
        if (nf = nfCurrency) and (parser.ParsedSections[0].Color = scRed) then
          nf := nfCurrencyRed;
        if nf in [nfCurrency, nfCurrencyRed] then
          ANumFormat := nf;
      finally
        parser.Free;
      end;

      if TryStrToFloat(AnalyzeCompareStr(condition, op), x, FPointSeparatorSettings) then
      begin
        if x = 0 then
          // This condition is used in currency formats
          case op of
            coEqual: zerofmt := fmt;
            coLess, coLessEqual: negfmt := fmt;
            coGreater, coGreaterEqual: posfmt := fmt;
          end
        else if (x > 1E308) and (op in [coLess, coLessEqual]) then
          // used "in hh:mm;@"
          posfmt := fmt;
      end;
      ANode := ANode.NextSibling;
      inc(counter);
    end;

    case counter of
      1: begin
           if negfmt = '' then negfmt := currfmt;
           AFormatStr := posfmt + ';' + negfmt;
         end;
      2: begin
           if zerofmt = '' then zerofmt := currfmt;
           AFormatStr := posfmt + ';' + negfmt + ';' + zerofmt;
         end;
      3: AFormatStr := posfmt + ';' + negfmt + ';' + zerofmt;
    end;

    if not (ANumFormat in [nfCurrency, nfCurrencyRed]) then
      ANumFormat := nfCustom;
  end;

  procedure ReadNumberStyle(ANumFormatNode: TDOMNode; ANumFormatName: String);
  var
    node, childNode: TDOMNode;
    nodeName: String;
    nf: TsNumberFormat;
    nfs: String;
    s: String;
    f: Double;
    fracInt, fracNum, fracDenom: Integer;
    grouping: Boolean;
    nex: Integer;
    nint: Integer;
    ndecs: Integer;
    cs: String;
    color: TsColor;
    hasColor: Boolean;
  begin
    nfs := '';
    cs := '';
    hasColor := false;
    node := ANumFormatNode.FirstChild;
    while Assigned(node) do
    begin
      nodeName := node.NodeName;
      if nodeName = '#text' then
      begin
        node := node.NextSibling;
        Continue;
      end else
      if nodeName = 'number:number' then
      begin
        s := GetAttrValue(node, 'number:min-integer-digits');
        if s <> '' then nint := StrToInt(s) else nint := 0;
        s := GetAttrValue(node, 'number:decimal-places');
        if s = '' then
          s := GetAttrValue(node, 'decimal-places');
        if s = '' then
        begin
          if nfs='' then nf := nfGeneral else nf := nfCustom;
          nfs := nfs + 'General';
        end else
        begin
          ndecs := StrToInt(s);
          grouping := GetAttrValue(node, 'number:grouping') = 'true';
          s := GetAttrValue(node, 'number:display-factor');
          if s <> '' then f := StrToFloat(s, FPointSeparatorSettings) else f := 1.0;
          nf := IfThen(grouping, nfFixedTh, nfFixed);
          nfs := nfs + BuildNumberFormatString(nf, Workbook.FormatSettings, ndecs, nint);
          if f <> 1.0 then begin
            nf := nfCustom;
            while (f > 1.0) do
            begin
              nfs := nfs + ',';
              f := f / 1000;
            end;
          end;
        end;
      end else
      if nodeName = 'number:fraction' then
      begin
        nf := nfFraction;
        s := GetAttrValue(node, 'number:min-integer-digits');
        if s <> '' then fracInt := StrToInt(s) else fracInt := -1;
        s := GetAttrValue(node, 'number:min-numerator-digits');
        if s <> '' then fracNum := StrToInt(s) else fracNum := 0;
        s := GetAttrValue(node, 'number:min-denominator-digits');
        if s <> '' then fracDenom := StrToInt(s) else fracDenom := 0;
        s := GetAttrValue(node, 'number:denominator-value');
        if s <> '' then fracDenom := -StrToInt(s);
        nfs := nfs + BuildFractionFormatString(fracInt > -1, fracNum, fracDenom);
      end else
      if nodeName = 'number:scientific-number' then
      begin
        nf := nfExp;
        s := GetAttrValue(node, 'number:min-integer-digits');
        if s <> '' then nint := StrToInt(s) else nint := 0;
        s := GetAttrValue(node, 'number:decimal-places');
        if s <> '' then ndecs := StrToInt(s) else ndecs := 0;
        s := GetAttrValue(node, 'number:min-exponent-digits');
        if s <> '' then nex := StrToInt(s) else nex := 1;
        nfs := nfs + BuildNumberFormatString(nfFixed, Workbook.FormatSettings, ndecs, nint);
        nfs := nfs + 'E+' + DupeString('0', nex);
      end else
      if nodeName = 'number:currency-symbol' then
      begin
        childnode := node.FirstChild;
        while childnode <> nil do
        begin
          cs := cs + childNode.NodeValue;
          nfs := nfs + '"' + childNode.NodeValue + '"';
          childNode := childNode.NextSibling;
        end;
      end else
      if nodeName = 'number:text' then
      begin
        childNode := node.FirstChild;
        while childNode <> nil do
        begin
          nfs := nfs + childNode.NodeValue;
          childNode := childNode.NextSibling;
        end;
      end else
      if nodeName = 'style:text-properties' then
      begin
        s := GetAttrValue(node, 'fo:color');
        if s <> '' then
        begin
          hasColor := true;
          color := HTMLColorStrToColor(s);
          case color of
            scBlack   : nfs := '[black]' + nfs;
            scWhite   : nfs := '[white]' + nfs;
            scRed     : nfs := '[red]' + nfs;
            scGreen   : nfs := '[green]' + nfs;
            scBlue    : nfs := '[blue]' + nfs;
            scYellow  : nfs := '[yellow]' + nfs;
            scMagenta : nfs := '[magenta]' + nfs;
            scCyan    : nfs := '[cyan]' + nfs;
          end;
        end;
      end;
      node := node.NextSibling;
    end;

    node := ANumFormatNode.FindNode('style:map');
    if node <> nil then
      ReadStyleMap(node, nf, nfs);

    if ANumFormatNode.NodeName = 'number:percentage-style' then
      nf := nfPercentage
    else
    if (ANumFormatNode.NodeName = 'number:currency-style') then
      nf := IfThen(hasColor, nfCurrencyRed, nfCurrency);

    NumFormatList.Add(Format('%s:%s', [ANumFormatName, nfs]));
  end;

  procedure ReadDateTimeStyle(ANumFormatNode: TDOMNode; ANumFormatName: String);
  var
    node, childNode: TDOMNode;
    nf: TsNumberFormat;
    nfs: String;
    nodeName: String;
    s, stxt, sovr: String;
    isInterval: Boolean;
  begin
    nfs := '';
    isInterval := false;
    sovr := GetAttrValue(ANumFormatNode, 'number:truncate-on-overflow');
    if (sovr = 'false') then
      isInterval := true;
    node := ANumFormatNode.FirstChild;
    while Assigned(node) do
    begin
      nodeName := node.NodeName;
      if nodeName = '#text' then
      begin
        node := node.NextSibling;
        Continue;
      end else
      if nodeName = 'number:year' then
      begin
        s := GetAttrValue(node, 'number:style');
        nfs := nfs + IfThen(s = 'long', 'yyyy', 'yy');
      end else
      if nodeName = 'number:month' then
      begin
        s := GetAttrValue(node, 'number:style');
        stxt := GetAttrValue(node, 'number:textual');
        if (stxt = 'true') then  // Month as text
          nfs := nfs + IfThen(s = 'long', 'mmmm', 'mmm')
        else                     // Month as number
          nfs := nfs + IfThen(s = 'long', 'mm', 'm');
      end else
      if nodeName = 'number:day' then
      begin
        s := GetAttrValue(node, 'number:style');
        nfs := nfs + IfThen(s = 'long', 'dd', 'd');
      end else
      if nodeName = 'number:day-of-week' then
      begin
        s := GetAttrValue(node, 'number:style');
        nfs := nfs + IfThen(s = 'long', 'dddd', 'ddd');
      end else
      if nodeName = 'number:hours' then
      begin
        s := GetAttrValue(node, 'number:style');
        if (sovr = 'false') then
          nfs := nfs + IfThen(s = 'long', '[hh]', '[h]')
        else
          nfs := nfs + IfThen(s = 'long', 'hh', 'h');
        sovr := '';
      end else
      if nodeName = 'number:minutes' then
      begin
        s := GetAttrValue(node, 'number:style');
        if (sovr = 'false') then
          nfs := nfs + IfThen(s = 'long', '[nn]', '[n]')
        else
          nfs := nfs + IfThen(s = 'long', 'nn', 'n');
        sovr := '';
      end else
      if nodeName = 'number:seconds' then
      begin
        s := GetAttrValue(node, 'number:style');
        if (sovr = 'false') then
          nfs := nfs + IfThen(s = 'long', '[ss]', '[s]')
        else
          nfs := nfs + IfThen(s = 'long', 'ss', 's');
        sovr := '';
        s := GetAttrValue(node, 'number:decimal-places');
        if (s <> '') and (s <> '0') then
          nfs := nfs + '.' + DupeString('0', StrToInt(s));
      end else
      if nodeName = 'number:am-pm' then
        nfs := nfs + 'AM/PM'
      else
      if nodeName = 'number:text' then
      begin
        childnode := node.FirstChild;
        if childnode <> nil then
        begin
          s := childNode.NodeValue;
          if pos(';', s) > 0 then
            nfs := nfs + '"' + s + '"'
            // avoid "misunderstanding" the semicolon as a section separator!
          else
            nfs := nfs + childnode.NodeValue;
        end;
      end;
      node := node.NextSibling;
    end;

    nf := IfThen(isInterval, nfTimeInterval, nfCustom);
    node := ANumFormatNode.FindNode('style:map');
    if node <> nil then
      ReadStyleMap(node, nf, nfs);

    NumFormatList.Add(ANumFormatName + ':' + nfs);
//    NumFormatList.AddFormat(ANumFormatName, nf, nfs);
  end;

  procedure ReadTextStyle(ANumFormatNode: TDOMNode; ANumFormatName: String);
  var
    node, childNode: TDOMNode;
    nf: TsNumberFormat = nfGeneral;
    nfs: String;
    nodeName: String;
  begin
    nfs := '';
    node := ANumFormatNode.FirstChild;
    while Assigned(node) do
    begin
      nodeName := node.NodeName;
      if nodeName = '#text' then
      begin
        node := node.NextSibling;
        Continue;
      end else
      if nodeName = 'number:text-content' then
      begin
        nfs := nfs + '@';
      end else
      if nodeName = 'number:text' then
      begin
        childnode := node.FirstChild;
        if childnode <> nil then
          nfs := nfs + childnode.NodeValue;
      end;
      node := node.NextSibling;
    end;

    node := ANumFormatNode.FindNode('style:map');
    if node <> nil then
      ReadStyleMap(node, nf, nfs);
    nf := nfCustom;

    NumFormatList.Add(Format('%s:%s', [ANumFormatName, nfs]));

    //NumFormatList.AddFormat(ANumFormatName, nf, nfs);
  end;

var
  NumFormatNode: TDOMNode;
  numfmt_nodename, numfmtname: String;

begin
  if not Assigned(AStylesNode) then
    exit;

  NumFormatNode := AStylesNode.FirstChild;
  while Assigned(NumFormatNode) do
  begin
    numfmt_nodename := NumFormatNode.NodeName;

    if NumFormatNode.HasAttributes then
      numfmtName := GetAttrValue(NumFormatNode, 'style:name') else
      numfmtName := '';

    // Numbers (nfFixed, nfFixedTh, nfExp, nfPercentage)
    if (numfmt_nodename = 'number:number-style') or
       (numfmt_nodename = 'number:percentage-style') or
       (numfmt_nodename = 'number:currency-style')
    then
      ReadNumberStyle(NumFormatNode, numfmtName);

    // Date/time values
    if (numfmt_nodename = 'number:date-style') or (numfmt_nodename = 'number:time-style') then
      ReadDateTimeStyle(NumFormatNode, numfmtName);

    // Text values
    if (numfmt_nodename = 'number:text-style') then
      ReadTextStyle(NumFormatNode, numfmtName);

    // Next node
    NumFormatNode := NumFormatNode.NextSibling;
  end;
end;

{ Finds the PageLayout record for a given TableStyle name in the "styles" nodes.
  First, seeks the TableStyle among the children of the "styles" node in the
  contents.xml - this node contains the name of the used master page.
  Then seeks the FMasterPageList for the entry with the determined master page
  name. This entry contains the name of the associated PageLayoutData stored in
  the PageLayoutList which, finally, contains the requested PageLayout record. }
procedure TsSpreadOpenDocReader.ReadPageLayout(AStylesNode: TDOMNode;
  ATableStyleName: String; APageLayout: TsPageLayout);
var
  nodeName, s: String;
  node: TDOMNode;
  masterPageName: String;
  masterPageData: TMasterPageData;
  pageLayoutData: TPageLayoutData;
  i, j: Integer;
begin
  if AStylesNode = nil then
    exit;

  { Looking through the "styles" node...}
  node := AStylesNode.FirstChild;
  while node <> nil do
  begin
    nodeName := node.NodeName;
    { ... for the node which is named like the requested TableStyle }
    if nodeName = 'style:style' then
    begin
      s := GetAttrValue(node, 'style:name');
      if s = ATableStyleName then
      begin
        { Found: extract the name of the master page }
        masterPageName := GetAttrValue(node, 'style:master-page-name');
        if masterPageName = '' then
          exit;

        { Looking through the MasterPage list...}
        for i:=0 to FMasterPageList.Count-1 do
        begin
          masterPageData := TMasterPageData(FMasterPageList[i]);
          { ... for the entry with the found master page name }
          if masterPageData.Name = masterPageName then
          begin
            { Found: looking through the PageLayout list ...}
            for j:=0 to FPageLayoutList.Count-1 do
            begin
              pageLayoutData := TPageLayoutData(FPageLayoutList[j]);
              { ... for the entry with the name specified by the master page }
              if pageLayoutData.Name = masterPageData.PageLayoutName then
              begin
                { Found: Return a pointer to the PageLayout record stored in the list }
                APageLayout.Assign(pageLayoutData.PageLayout);
                exit;
              end;
            end;
          end;
        end;
      end;
    end;
    { Not found: try next node in the styles list }
    node := node.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadPictures(AStream: TStream);
var
  memstream: TMemoryStream;
  unzip: TStreamUnzipper;
  fn: String;
  i: Integer;
begin
  unzip := TStreamUnzipper.Create(AStream);
  try
    unzip.Examine;
    for i := 0 to unzip.Entries.Count-1 do begin
      fn := unzip.Entries.Entries[i].ArchiveFileName;
      if ExtractFileDir(fn) = 'Pictures' then begin
        memStream := TMemoryStream.Create;
        unzip.UnzipFile(fn, memStream);
        memstream.Position := 0;
        (FWorkbook as TsWorkbook).AddEmbeddedObj(memstream, ExtractFileName(fn));
        memStream.Free;
      end;
    end;
  finally
    unzip.Free;
  end;
end;

procedure TsSpreadOpenDocReader.ReadPrintRanges(ATableNode: TDOMNode;
  ASheet: TsBasicWorksheet);
var
  L: TStringList;
  s, sheetname: String;
  i, p: Integer;
  r1,c1,r2,c2: Cardinal;
  inName: Boolean;
begin
  s := GetAttrValue(ATableNode, 'table:print-ranges');
  if s = '' then
    exit;
  L := TStringList.Create;
  try
    // Scan the string for spaces. But note: Spaces may be contained also in
    // the sheet names!
    s := s + ' ';
    i := 1;
    p := 1;
    inName := false;
    while (i <= Length(s)) do
    begin
      case s[i] of
        '''': inName := not inName;
        ' ' : if not inName then begin
                L.Add(Copy(s, p, i-p));
                while (i <= Length(s)) and (s[i] = ' ') do
                  inc(i);
                p := i;
                if p <= Length(s) then
                  Continue
                else
                  break;
              end;
      end;
      inc(i);
    end;

    // L lists all the ranges. Split each range into its components.
    for i:=0 to L.Count-1 do begin
      s := L[i];
      p := pos(':', L[i]);
      s := Copy(L[i], 1, p-1);
      ParseSheetCellString(s, sheetname, r1, c1, '.');
      if (sheetname <> '') then
      begin
        if (sheetname[1] = '''') then
          Delete(sheetname, 1,1);
        if (sheetname[Length(sheetname)] = '''') then
          Delete(sheetname, Length(sheetname), 1);
        if (sheetname <> ASheet.Name) then
        begin
          FWorkbook.AddErrorMsg(rsDifferentSheetPrintRange, [L[i]]);
          Continue;
        end;
      end;
      s := Copy(L[i], p+1, Length(L[i]));
      ParseSheetCellString(s, sheetname, r2, c2, '.');
      if (sheetname <> '') then begin
        if (sheetname[1] = '''') then
          Delete(sheetname, 1, 1);
        if (sheetname[Length(sheetname)] = '''') then
          Delete(sheetname, Length(sheetname), 1);
        if (sheetname <> ASheet.name) then
        begin
          FWorkbook.AddErrorMsg(rsDifferentSheetPrintRange, [L[i]]);
          Continue;
        end;
      end;
      // Add found range to worksheet
      (ASheet as TsWorksheet).PageLayout.AddPrintRange(r1, c1, r2, c2);
    end;
  finally
    L.Free;
  end;
end;

procedure TsSpreadOpenDocReader.ReadCell(ANode: TDOMNode; ARow, ACol: Integer;
  AFormatIndex: Integer; out AColsRepeated: Integer);
var
  paramValueType, paramFormula: String;
  s: String;
  colsSpanned, rowsSpanned: Integer;
begin
 {$IFDEF FPSpreadDebug}
  DebugLn(Format('[ReadCell] ARow=%d, ACol=%d, AFormatIndex=%d',
    [ARow, ACol, AFormatIndex])
  );
 {$ENDIF}

  // Workaround for Excel files converted to ods by Calc: These files are
  // expanded to fill the entire max worksheet. They also have single empty
  // cell in the outermost cells --> don't write anything here to prevent this.
  if (ARow > FLimitations.MaxRowCount - 10)  or (ACol > FLimitations.MaxColCount - 10) then
    exit;

  // select this cell value's type
  paramValueType := GetAttrValue(ANode, 'office:value-type');
  paramFormula := GetAttrValue(ANode, 'table:formula');

  if paramFormula <> '' then
    ReadFormula(ARow, ACol, AFormatIndex, ANode)
  else
  begin
    if paramValueType = 'string' then
      ReadLabel(ARow, ACol, AFormatIndex, ANode)
    else
    if (paramValueType = 'float') or
       (paramValueType = 'percentage') or
       (paramValueType = 'currency')
    then
      ReadNumber(ARow, ACol, AFormatIndex, ANode)
    else if (paramValueType = 'date') or (paramValueType = 'time') then
      ReadDateTime(ARow, ACol, AFormatIndex, ANode)
    else if (paramValueType = 'boolean') then
      ReadBoolean(ARow, ACol, AFormatIndex, ANode)
    else
    if (paramValueType = '') and (AFormatIndex > 0) and
      (ARow < FLimitations.MaxRowCount-10) and
      (ACol < FLimitations.MaxColCount-10)
    then
      ReadBlank(ARow, ACol, AFormatIndex, ANode);
      { NOTE 1: Empty cells having no cell format, but a column format only,
        are skipped here. --> Currently the reader does not detect the format
        of empty cells correctly.
        It would work if the "(cellStyleName <> '')" would be omitted, but    // <--- wp: still up-to-date?
        then the reader would create a record for all 1E9 cells prepared by
        the Excel2007 export --> crash!
        The column format is available in the FColumnList, but since the usage
        of colsSpanned in the row it is possible to miss the correct column format.
        Pretty nasty situation!

        NOTE 2: Sometimes, ods files have an additional empty cell at the end
        of the spreadsheet range. Adding a cell to the worksheet here would
        extend the sheet range unrealistically, and, if using the WorksheetGrid,
        would add unnecessary rows/columns to the grid. --> Check against
        FLimitations.MaxRowCount/MaxColCount; use some spare values because I
        don't understand this mechanism of ods at all }
  end;


  // Read cell comment
  ReadComment(ARow, ACol, ANode);

  // Read cell image(s)
  ReadCellImages(ANode, ARow, ACol);

  s := GetAttrValue(ANode, 'table:number-columns-spanned');
  if s <> '' then
    colsSpanned := StrToInt(s) - 1
  else
    colsSpanned := 0;

  s := GetAttrValue(ANode, 'table:number-rows-spanned');
  if s <> '' then
    rowsSpanned := StrToInt(s) - 1
  else
    rowsSpanned := 0;

  if (colsSpanned <> 0) or (rowsSpanned <> 0) then
    (FWorksheet as TsWorksheet).MergeCells(ARow, ACol, ARow + rowsSpanned, ACol + colsSpanned);

  s := GetAttrValue(ANode, 'table:number-columns-repeated');
  if s <> '' then
    AColsRepeated := StrToInt(s)
  else
    AColsRepeated := 1;
end;

procedure TsSpreadOpenDocReader.ReadCellImages(ANode: TDOMNode;
  ARow, ACol: Cardinal);
var
  childNode: TDOMNode;
  {%H-}nodeName: String;
begin
  childNode := ANode.FirstChild;
  while Assigned(childNode) do
  begin
    nodeName := childNode.NodeName;
    ReadShape(childnode, ARow, ACol);
    childNode := childNode.NextSibling;
  end;
end;

function ExtractArguments(s: String; IsExpression: Boolean;
  var arg1, arg2: String): Boolean;
var
  p: Integer;
  sa: TStringArray;
begin
  if s = '' then
    raise Exception.Create('Empty string not allowed');

  if IsExpression then
  begin
    p := pos('(', s);
    if p = 0 then
      exit(false);
    Delete(s, 1, p);
    Delete(s, Length(s), 1);
    arg1 := UnquoteStr(s);
    exit(true);
  end;

  case s[1] of
    '=': arg1 := UnquoteStr(Copy(s, 2, MaxInt));
    '<', '>', '!':
         if (s[2] = '=') then
           arg1 := UnquoteStr(Copy(s, 3, MaxInt))
         else
           arg1 := UnquoteStr(Copy(s, 2, MaxInt));
  else
    p := pos('(', s);
    if p = 0 then
      exit(false);
    Delete(s, 1, p);

    p := pos(')', s);
    if p = 0 then
      exit(false);
    Delete(s, p, MaxInt);

    sa := s.Split(',');
    arg1 := UnquoteStr(sa[0]);
    if Length(sa) > 1 then
      arg2 := UnquoteStr(sa[1]);
  end;
  Result := true;
end;

procedure TsSpreadOpenDocReader.ReadCFCellFormat(ANode: TDOMNode;
  ASheet: TsBasicWorksheet; ARange: TsCellRange);
const
  CONDITONS_WITHOUT_ARGUMENTS: set of TsCFCondition = [
    cfcAboveAverage, cfcBelowAverage, cfcAboveEqualAverage, cfcBelowEqualAverage,
    cfcUnique, cfcDuplicate,
    cfcContainsErrors, cfcNotContainsErrors
  ];
var
  s: String;
  fmtIndex: Integer;
  fmt: TsCellFormat;
  ok: Boolean;
  condition: TsCFCondition;
  param1: String = '';
  param2: String = '';
  op1, op2: Variant;
  sheet: TsWorksheet;
begin
  sheet := TsWorksheet(ASheet);

  // Style
  s := GetAttrValue(ANode, 'calcext:apply-style-name');
  if s <> '' then
  begin
    fmtIndex := ExtractFormatIndexFromStyle(s, -1);
    fmt := FCellFormatList.Items[fmtIndex]^;
    fmtIndex := (FWorkbook as TsWorkbook).AddCellFormat(fmt);
  end;

  // Type of CF, operation
  s := GetAttrValue(ANode, 'calcext:value');
  if s = '' then
    exit;

  ok := true;
  case s[1] of
    '=': condition := cfcEqual;
    '!': if (s[2] = '=') then
           condition := cfcNotEqual
         else
           ok := false;
    '<': if (s[2] = '=') then
           condition := cfcLessEqual
          else
            condition := cfcLessThan;
    '>': if s[2] = '=' then
           condition := cfcGreaterEqual
         else
           condition := cfcGreaterThan;
    'a': case s of
           'above-average': condition := cfcAboveAverage;
           'above-equal-average': condition := cfcAboveEqualAverage
         end;
    'b': case s of
           'below-average': condition := cfcBelowAverage;
           'below-equal-average': condition := cfcBelowEqualAverage;
         else
           if pos('begins-with(', s) = 1 then
             condition := cfcBeginsWith
           else
           if pos('between(', s) = 1 then
             condition := cfcBetween
           else
           if pos('bottom-elements(', s) = 1 then
             condition := cfcBottom
           else
           if pos('bottom-percent(', s) = 1 then
             condition := cfcBottomPercent
           else
             ok := false;
         end;
    'c': if pos('contains-text(', s) = 1 then
           condition := cfcContainsText
         else
           ok := false;
    'd': if s = 'duplicate' then
           condition := cfcDuplicate
         else
           ok := false;
    'e': if pos('ends-with(', s) = 1 then
           condition := cfcEndsWith
         else
           ok := false;
    'f': if pos('formula-is(', s) = 1 then
           condition := cfcExpression
         else
           ok := false;
    'i': if (s = 'is-error') then
           condition := cfcContainsErrors
         else if s = 'is-no-error' then
           condition := cfcNotContainsErrors
         else
           ok := false;
    'n': if pos('not-contains-text(', s) = 1 then
           condition := cfcNotContainsText
         else if pos('not-between(', s) = 1 then
           condition := cfcNotBetween
         else
           ok := false;
    't': if pos('top-elements(', s) = 1 then
           condition := cfcTop
         else if pos('top-percent(', s) = 1 then
           condition := cfcTopPercent
         else
           ok := false;
    'u' : if s = 'unique' then
            condition := cfcUnique
          else
            ok := false;
  end;

  if ok then
  begin
    if (condition in CONDITONS_WITHOUT_ARGUMENTS) then
      ok := true
    else if (condition = cfcExpression) then
    begin
      ok := ExtractArguments(s, true, param1, param2);
      param1 := ConvertFormulaDialect(param1, fdOpenDocument, fdExcelA1, sheet);
    end else
      ok := ExtractArguments(s, false, param1, param2);
    end;

  if not ok then
    exit;

  if param1 = '' then VarClear(op1{%H-}) else op1 := param1;
  if param2 = '' then VarClear(op2{%H-}) else op2 := param2;
  sheet.WriteConditionalCellFormat(ARange, condition, op1, op2, fmtIndex);
end;

procedure TsSpreadOpenDocReader.ReadCFColorScale(ANode: TDOMNode;
  ASheet: TsBasicWorksheet; ARange: TsCellRange);
{ <calcext:color-scale>
    <calcext:color-scale-entry calcext:value="0" calcext:type="minimum" calcext:color="#ff0000" />
    <calcext:color-scale-entry calcext:value="50" calcext:type="percentile" calcext:color="#ffff00" />
    <calcext:color-scale-entry calcext:value="0" calcext:type="maximum" calcext:color="#00a933" />
  </calcext:color-scale>
  }
var
  sheet: TsWorksheet;
  nodeName: String;
  s: String;
  values: Array of Double = nil;
  kinds: Array of TsCFValueKind = nil;
  colors: Array of TsColor = nil;
  n: Integer;
begin
  sheet := TsWorksheet(ASheet);
  ANode := ANode.FirstChild;
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    if nodeName = 'calcext:color-scale-entry' then
    begin
      s := GetAttrValue(ANode, 'calcext:value');
      SetLength(values, Length(values)+1);
      if not TryStrToFloat(s, values[High(values)], FPointSeparatorSettings) then
        values[High(values)] := 0;

      s := GetAttrValue(ANode, 'calcext:type');
      SetLength(kinds, Length(kinds)+1);
      kinds[High(kinds)] := StrToValueKind(s);

      s := GetAttrvalue(ANode, 'calcext:color');
      if s <> '' then
      begin
        SetLength(colors, Length(colors)+1);
        colors[High(colors)] := HTMLColorStrToColor(s);
      end;
    end;
    ANode := ANode.NextSibling;
  end;

  n := MinValue([Integer(Length(values)), Integer(Length(kinds)), Integer(Length(colors))]);
  // Cast needed due to stupid 64-bit FPC not knowing which overload to select ...
  case n of
    0,
    1: exit;
    2: sheet.WriteColorRange(
         ARange,
         colors[0], kinds[0], values[0],
         colors[1], kinds[1], values[1]
       );
    else
       sheet.WriteColorRange(
         ARange,
         colors[0], kinds[0], values[0],
         colors[1], kinds[1], values[1],
         colors[2], kinds[2], values[2]
       );
  end;
end;

procedure TsSpreadOpenDocReader.ReadCFDataBars(ANode: TDOMNode;
  ASheet: TsBasicWorksheet; ARange: TsCellRange);
{ <calcext:data-bar calcext:min-length="10" calcext:max-length="90" calcext:negative-color="#ff0000" calcext:positive-color="#ff0000" calcext:axis-color="#000000">
    <calcext:formatting-entry calcext:value="0" calcext:type="auto-minimum" />
    <calcext:formatting-entry calcext:value="0" calcext:type="auto-maximum" />
  </calcext:data-bar>  }
var
  sheet: TsWorksheet;
  nodeName: String;
  s: String;
  values: array of double = nil;
  kinds: array of TsCFValueKind = nil;
  posColor, negColor: TsColor;
  n: Integer;
begin
  if ANode = nil then
    exit;

  sheet := TsWorksheet(ASheet);

  s := GetAttrValue(ANode, 'calcext:positive-color');
  if s <> '' then
    posColor := HTMLColorStrToColor(s)
  else
    posColor := scNotDefined;

  s := GetAttrValue(ANode, 'calcext:negative-color');
  if s <> '' then
    negColor := HTMLColorStrToColor(s)
  else
    negColor := scNotDefined;

  ANode := ANode.FirstChild;
  while ANode <> nil do
  begin
    nodeName := ANode.NodeName;
    if nodeName = 'calcext:formatting-entry' then
    begin
      s := GetAttrValue(ANode, 'calcext:value');
      SetLength(values, Length(values)+1);
      if not TryStrToFloat(s, values[High(values)], FPointSeparatorSettings) then
        values[High(values)] := 0;

      s := GetAttrValue(ANode, 'calcext:type');
      SetLength(kinds, Length(kinds)+1);
      kinds[High(kinds)] := StrToValueKind(s);
    end;
    ANode := ANode.NextSibling;
  end;

  // We only support a single color, ATM.
  if (posColor = scNotDefined) and (negColor <> scNotDefined) then
    posColor := negColor;

  n := MinValue([Integer(Length(values)), Integer(Length(kinds))]);
  // Cast needed due to stupid 64-bit FPC not knowing which overload to select...
  if n < 2 then
    exit;

  sheet.WriteDataBars(
    ARange,
    posColor,
    kinds[0], values[0],
    kinds[1], values[1]
  );
end;

procedure TsSpreadOpenDocReader.ReadCFDateFormat(ANode: TDOMNode;
  ASheet: TsBasicWorksheet; ARange: TsCellRange);
var
  sheet: TsWorksheet;
  s: String;
  c, condition: TsCFCondition;
  found: Boolean = false;
  fmt: TsCellFormat;
  fmtIndex: Integer;
begin
  if ANode = nil then
    exit;

  sheet := TsWorksheet(ASheet);

  // Style
  s := GetAttrValue(ANode, 'calcext:style');
  if s <> '' then
  begin
    fmtIndex := ExtractFormatIndexFromStyle(s, -1);
    fmt := FCellFormatList.Items[fmtIndex]^;
    fmtIndex := (FWorkbook as TsWorkbook).AddCellFormat(fmt);
  end;

  // value to compare with
  s := GetAttrValue(ANode, 'calcext:date');
  if s = '' then
    exit;

  // condition for comparison
  for c in [cfcYesterday..cfcNextYear] do
    if CF_CALCEXT_OP[c] = s then
    begin
      condition := c;
      found := true;
      break;
    end;

  if not found then
    exit;

  // Write conditional format to worksheet
  sheet.WriteConditionalCellFormat(ARange, condition, fmtIndex);
end;

procedure TsSpreadOpenDocReader.ReadCFIconSet(ANode: TDOMNode;
  ASheet: TsBasicWorksheet; ARange: TsCellRange);
{ <calcext:icon-set calcext:icon-set-type="3Stars">
    <calcext:formatting-entry calcext:value="0" calcext:type="percent" />
    <calcext:formatting-entry calcext:value="33" calcext:type="percent" />
    <calcext:formatting-entry calcext:value="66" calcext:type="percent" />
  </calcext:icon-set> }
var
  sheet: TsWorksheet;
  nodeName: String;
  s: String;
  values: array of double = nil;
  kinds: array of TsCFValueKind = nil;
  iconSet, tmp: TsCFIconSet;
  sIconSet: String;
  found: Boolean;
  n: Integer;
begin
  if ANode = nil then
    exit;

  sIconSet := GetAttrValue(ANode, 'calcext:icon-set-type');
  if sIconSet = '' then
    exit;

  for tmp in TsCFIconSet do
    if sIconSet = CF_ICON_SET[tmp] then begin
      iconSet := tmp;
      found := true;
      break;
    end;

  if (not found) then
    exit;

  // Number of icons
  n := GetCFIconCount(iconSet);
  if (n < 3) or (n > 5) then  // only 3, 4 or 5 icons allowed
    exit;

  ANode := ANode.FirstChild;
  while ANode <> nil do
  begin
    nodeName := ANode.NodeName;
    if nodeName = 'calcext:formatting-entry' then
    begin
      s := GetAttrValue(ANode, 'calcext:value');
      SetLength(values, Length(values)+1);
      if not TryStrToFloat(s, values[High(values)], FPointSeparatorSettings) then
        values[High(values)] := 0;

      s := GetAttrValue(ANode, 'calcext:type');
      SetLength(kinds, Length(kinds)+1);
      kinds[High(kinds)] := StrToValueKind(s);
    end;
    ANode := ANode.NextSibling;
  end;

  sheet := TsWorksheet(ASheet);
  // Ignore the first value because it is always 0
  case n of
    3: sheet.WriteIconSet(ARange, iconSet, kinds[1], values[1], kinds[2], values[2]);
    4: sheet.WriteIconSet(ARange, iconSet, kinds[1], values[1], kinds[2], values[2], kinds[3], values[3]);
    5: sheet.WriteIconSet(ARange, iconSet, kinds[1], values[1], kinds[2], values[2], kinds[3], values[3], kinds[4], values[4]);
  end;
end;


{ Reads the cells in the given table. Loops through all rows, and then finds all
  cells of each row. }
procedure TsSpreadOpenDocReader.ReadRowsAndCells(ATableNode: TDOMNode);
var
  row: Integer;
  rNode, childnode: TDOMNode;
  nodeName: String;
  rowsRepeated: Integer;
  colFmt: array of Integer;
  isFirstRow: Boolean;

  procedure ProcessRow(ARowNode: TDOMNode; GetRowFormat: Boolean);
  var
    rowStyleName: String;
    rowStyleIndex: Integer;
    rowStyle: TRowStyleData;
    rowHeight: Double;
    rowHeightType: TsRowHeightType;
    col: Integer;
    cellNode: TDOMNode;
    nodeName: String;
    lRow: PRow;
    cellRecord: TCell;
    cell: PCell;
    cellStyleName: String;
    s: String;
    colsRepeated: Integer;
    i: Integer;
    hasRowFormat: Boolean;
    styleIndex: Integer;
    firstStyleIndex: Integer;
    rowHidden: Boolean;
    rowPageBreak: Boolean;
  begin
    // Read rowstyle
    rowStyleName := GetAttrValue(ARowNode, 'table:style-name');
    rowStyleIndex := FindRowStyleByName(rowStyleName);
    if rowStyleIndex > -1 then        // just for safety
    begin
      rowStyle := TRowStyleData(FRowStyleList[rowStyleIndex]);
      rowHeight := rowStyle.RowHeight;    // in Workbook units (see ReadRowStyles)
      rowHeightType := rowStyle.RowHeightType;
      rowPageBreak := rowStyle.PageBreak;
    end else begin
      rowHeight := (FWorksheet as TsWorksheet).ReadDefaultRowHeight(FWorkbook.Units);
      rowHeightTYpe := rhtDefault;
      rowPageBreak := false;
    end;

    // If the row contains the PageBreak flag we add store it in a row record.
    if rowPageBreak then
      (FWorksheet as TsWorksheet).AddPageBreakToRow(row);

    col := 0;
    firstStyleIndex := -1;
    hasRowFormat := true;

    //process each cell of the row
    cellNode := ARowNode.FirstChild;
    //  cellNode := rowNode.FindNode('table:table-cell');
    while Assigned(cellNode) do
    begin
      nodeName := cellNode.NodeName;
      if nodeName = 'table:table-cell' then
      begin
        cellStyleName := GetAttrValue(CellNode, 'table:style-name');
        styleIndex := ExtractFormatIndexFromStyle(cellStyleName, col);
        ReadCell(cellNode, row, col, styleIndex, colsRepeated);

        // Check whether the current cell format is still the same as for the
        // first cell. If it is then we might have a row format here.
        if (firstStyleIndex = -1) and hasRowFormat then
          firstStyleIndex := styleIndex
        else if (styleIndex <> firstStyleIndex) and (cellStyleName <> 'Default') then
          hasRowFormat := false;

        // If all cell styles in the row are the same then hasRowFormat is true
        // and we can store the format of the first cell in the row record.
        if GetRowFormat and hasRowFormat and
           (col + colsRepeated >= LongInt(FLimitations.MaxColCount) - 10) and
           (row < FLimitations.MaxRowCount - 10) then
        begin
          lRow := (FWorksheet as TsWorksheet).GetRow(row);
          // Find first cell in row, all cells have the same format here.
          cell := (FWorksheet as TsWorksheet).FindNextCellInRow(row, 0);
          if cell <> nil then
            // Cell found --> copy its format index to cell record
            lRow^.FormatIndex := cell^.FormatIndex
          else
          begin
            // No cell in row --> appy format to dummy cell to get its format index
            InitCell(FWorksheet, row, 0, cellRecord);
            ApplyStyleToCell(@cellRecord, styleIndex);
            lRow^.FormatIndex := cellRecord.FormatIndex;
          end;
        end;

        if (colsRepeated > 1) and (col + colsRepeated < LongInt(FLimitations.MaxColCount) - 10) then
        begin
          // The 2nd condition belongs to a workaround for a bug of LO/OO whichs
          // extends imported xlsx files with blank cols up to their
          // specification limit.
          // React some columns earlier because the added column range is
          // sometimes split into two parts.
          cell := (FWorksheet as TsWorksheet).FindCell(row, col);
          if cell <> nil then
            for i:=1 to colsRepeated-1 do begin
              cell := TsWorksheet(FWorksheet).CopyCell(row, col, row, col+i);
              styleIndex := ExtractFormatIndexFromStyle(cellStyleName, col+i);
              ApplyStyleToCell(cell, styleIndex);
            end;
        end;
      end
      else
      if nodeName = 'table:covered-table-cell' then
      begin
        s := GetAttrValue(cellNode, 'table:number-columns-repeated');
        if s = '' then colsRepeated := 1 else colsRepeated := StrToInt(s);
      end else
        colsRepeated := 0;

      col := col + colsRepeated;
      cellNode := cellNode.NextSibling;
    end; //while Assigned(cellNode)

    // Row visibility
    rowHidden := GetAttrValue(ARowNode, 'table:visibility') = 'collapse';
    if rowHidden then
      TsWorksheet(FWorksheet).HideRow(row);

    // Repeated rows
    s := GetAttrValue(ARowNode, 'table:number-rows-repeated');
    if s = '' then
      rowsRepeated := 1
    else
      rowsRepeated := StrToInt(s);

    // Transfer non-default row heights and row hidden status to sheet's rows
    // This first "if" is a workaround for a bug of LO/OO whichs extends imported
    // xlsx files with blank rows up to their specification limit.
    // Process some rows earlier because the added row range is sometimes split
    // into two parts.
    if row + rowsRepeated < LongInt(FLimitations.MaxRowCount) - 10 then
      for i:=1 to rowsRepeated do begin
        TsWorksheet(FWorksheet).WriteRowHeight(row + i - 1, rowHeight, FWorkbook.Units, rowHeightType);
        if rowHidden then
          TsWorksheet(FWorksheet).HideRow(row + i - 1);
      end;

    // Prepare checking of column format
    if GetRowFormat then begin
      // Store the format indexes of all cells in the first row
      if isFirstRow then begin
        SetLength(colFmt, col);
        for col:=0 to High(colFmt) do begin
          cell := TsWorksheet(FWorksheet).FindCell(row, col);
          if cell <> nil then
            colFmt[col] := cell^.FormatIndex
          else begin
            InitCell(FWorksheet, row, col, cellRecord);
            ApplyStyleToCell(@cellRecord, styleIndex);
            colFmt[col] := cellRecord.FormatIndex;
          end;
        end;
      end else
        // In the other rows compare the cell format indexes with those stored
        // from the first row. If an index does not match then this col cannot
        // have a column format.
        for col:=0 to High(colFmt) do begin
          if colFmt[col] > -1 then begin
            cell := TsWorksheet(FWorksheet).FindCell(row, col);
            if ((cell <> nil) and (cell^.FormatIndex <> colFmt[col])) then
              colFmt[col] := -1;
          end;
        end;
    end;

    row := row + rowsRepeated;
    isFirstRow := false;
  end;

var
  PrintRowMode: Boolean;
  c: Cardinal;

begin
  rowsRepeated := 0;
  row := 0;
  isFirstRow := true;
  PrintRowMode := false;

  rnode := ATableNode.FirstChild;
  while Assigned(rNode) do
  begin
    nodename := rNode.NodeName;

    // Repeated print rows
    if nodeName = 'table:table-header-rows' then
    begin
      PrintRowMode := true;
      if FRepeatedRows.FirstIndex = Cardinal(UNASSIGNED_ROW_COL_INDEX) then
        FRepeatedRows.FirstIndex := row;
      childnode := rNode.FirstChild;
      while Assigned(childnode) do
      begin
        nodename := childnode.NodeName;
        if nodename = 'table:table-row' then
        begin
          ProcessRow(childnode, false);
        end;
        childnode := childnode.NextSibling;
      end;
      FRepeatedRows.LastIndex := row-1;
    end
    else
    // "normal" rows
    if nodeName = 'table:table-row' then
      ProcessRow(rNode, true);

    rNode := rNode.NextSibling;
  end;

  // Construct column records with column format
  if not PrintRowMode and (row > FLimitations.MaxRowCount-10) then begin
    for c := 0 to High(colFmt) do
      if colFmt[c] > 0 then
        (FWorksheet as TsWorksheet).WriteColFormatIndex(c, colFmt[c]);
  end;
end;

procedure TsSpreadOpenDocReader.ReadRowStyle(AStyleNode: TDOMNode);
var
  styleName, nodename: String;
  styleChildNode: TDOMNode;
  rowHeight: Double;
  rowPageBreak: Boolean;
  s: String;
  rowStyle: TRowStyleData;
  rowHeightType: TsRowHeightType;
begin
  styleName := GetAttrValue(AStyleNode, 'style:name');
  styleChildNode := AStyleNode.FirstChild;
  rowHeight := 0;
  rowHeightType := rhtCustom;
  rowPageBreak := false;

  while Assigned(styleChildNode) do
  begin
    nodename := styleChildNode.NodeName;
    if nodeName = 'style:table-row-properties' then
    begin
      s := GetAttrValue(styleChildNode, 'style:row-height');
      if s <> '' then
        rowHeight := TsWorkbook(FWorkbook).ConvertUnits(HTMLLengthStrToPts(s), suPoints, FWorkbook.Units);
        // convert to workbook units
      s := GetAttrValue(styleChildNode, 'style:use-optimal-row-height');
      if s = 'true' then
        rowHeightType := rhtAuto;
      // Page break
      s := GetAttrValue(styleChildNode, 'fo:break-before');
      if s = 'page' then
       rowPageBreak := true;
    end;
    styleChildNode := styleChildNode.NextSibling;
  end;

  rowStyle := TRowStyleData.Create;
  rowStyle.Name := styleName;
  rowStyle.RowHeight := rowHeight;
  rowStyle.RowHeightType := rowHeightType;
  rowStyle.PageBreak := rowPageBreak;
  FRowStyleList.Add(rowStyle);
end;

procedure TsSpreadOpenDocReader.ReadSettings(AOfficeSettingsNode: TDOMNode);
var
  cfgItemSetNode, cfgItemNode, cfgItemMapEntryNode, cfgEntryItemNode, cfgTableItemNode, node: TDOMNode;
  nodeName, cfgName, cfgValue, tblName: String;
  sheet: TsWorksheet;
  vsm, hsm, hsp, vsp: Integer;
  zoom: Double;
  showGrid, showHeaders: Boolean;
  actCol, actRow: Cardinal;
  i: Integer;
begin
  showGrid := true;
  showHeaders := true;
  zoom := 100.0;
  actRow := 0;
  actCol := 0;
  cfgItemSetNode := AOfficeSettingsNode.FirstChild;
  while Assigned(cfgItemSetNode) do
  begin
    if (cfgItemSetNode.NodeName <> '#text') and
       (GetAttrValue(cfgItemSetNode, 'config:name') = 'ooo:view-settings') then
    begin
      cfgItemNode := cfgItemSetNode.FirstChild;
      while Assigned(cfgItemNode) do begin
        if (cfgItemNode.NodeName <> '#text') and
           (cfgItemNode.NodeName = 'config:config-item-map-indexed') and
           (GetAttrValue(cfgItemNode, 'config:name') = 'Views') then
        begin
          cfgItemMapEntryNode := cfgItemNode.FirstChild;
          while Assigned(cfgItemMapEntryNode) do
          begin
            cfgEntryItemNode := cfgItemMapEntryNode.FirstChild;
            while Assigned(cfgEntryItemNode) do
            begin
              nodeName := cfgEntryItemNode.NodeName;
              if (nodeName = 'config:config-item') then
              begin
                cfgName := lowercase(GetAttrValue(cfgEntryItemNode, 'config:name'));
                if cfgName = 'activetable' then
                begin
                  cfgValue := GetNodeValue(cfgEntryItemNode);
                  FActiveSheet := cfgValue;
                end else
                if cfgName = 'showgrid' then
                begin
                  cfgValue := GetNodeValue(cfgEntryItemNode);
                  if cfgValue = 'false' then showGrid := false;
                end else
                if cfgName = 'hascolumnrowheaders' then
                begin
                  cfgValue := GetNodeValue(cfgEntryItemNode);
                  if cfgValue = 'false' then showHeaders := false;
                end;
              end else
              if (nodeName = 'config:config-item-map-named') and
                 (GetAttrValue(cfgEntryItemNode, 'config:name') = 'Tables') then
              begin
                cfgTableItemNode := cfgEntryItemNode.FirstChild;
                while Assigned(cfgTableItemNode) do
                begin
                  nodeName := cfgTableItemNode.NodeName;
                  if nodeName <> '#text' then
                  begin
                    tblName := GetAttrValue(cfgTableItemNode, 'config:name');
                    if tblName <> '' then
                    begin
                      hsm := 0; vsm := 0;
                      sheet := TsWorkbook(Workbook).GetWorksheetByName(tblName);
                      if sheet <> nil then
                      begin
                        node := cfgTableItemNode.FirstChild;
                        while Assigned(node) do
                        begin
                          nodeName := node.NodeName;
                          if nodeName <> '#text' then
                          begin
                            cfgName := GetAttrValue(node, 'config:name');
                            cfgValue := GetNodeValue(node);
                            case cfgName of
                              'CursorPositionX': actCol := StrToInt(cfgValue);
                              'CursorPositionY': actRow := StrToInt(cfgValue);
                              'VerticalSplitMode': vsm := StrToInt(cfgValue);
                              'HorizontalSplitMode': hsm := StrToInt(cfgValue);
                              'VerticalSplitPosition': vsp := StrToInt(cfgValue);
                              'HorizontalSplitPosition': hsp := StrToInt(cfgValue);
                              'ZoomValue': zoom := StrToFloat(cfgValue, FPointSeparatorSettings);
                            end;
                          end;
                          node := node.NextSibling;
                        end;
                        if (hsm = 2) or (vsm = 2) then
                        begin
                          sheet.Options := sheet.Options + [soHasFrozenPanes];
                          sheet.LeftPaneWidth := hsp;
                          sheet.TopPaneHeight := vsp;
                        end else
                          sheet.Options := sheet.Options - [soHasFrozenPanes];
                        // Active cell
                        sheet.SelectCell(actRow, actCol);
                        // Zoom factor
                        sheet.ZoomFactor := zoom / 100.0;
                      end;
                    end;
                  end;
                  cfgTableItemNode := cfgTableItemNode.NextSibling;
                end;
              end;
              cfgEntryItemNode := cfgEntryItemNode.NextSibling;
            end;
            cfgItemMapEntryNode := cfgItemMapEntryNode.NextSibling;
          end;
        end;
        cfgItemNode := cfgItemNode.NextSibling;
      end;
    end;
    cfgItemSetNode := cfgItemSetNode.NextSibling;
  end;

  { Now let's apply the showGrid and showHeader values to all sheets - they
    are document-wide settings (although there is a ShowGrid in the Tables node) }
  for i:=0 to (Workbook as TsWorkbook).GetWorksheetCount-1 do
  begin
    sheet := TsWorkbook(Workbook).GetWorksheetByIndex(i);
    if not showGrid then sheet.Options := sheet.Options - [soShowGridLines];
    if not showHeaders then sheet.Options := sheet.Options - [soShowHeaders];
  end;
end;

{      '<draw:frame draw:z-index="%d" draw:name="Image %d" '+
        'draw:style-name="gr1" draw:text-style-name="P1" '+
        'svg:width="%.2fmm" svg:height="%.2fmm" '+
        'svg:x="%.2fmm" svg:y="%.2fmm">' +
        '<draw:image xlink:href="Pictures/%d.%s" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad">' +
          '<text:p />' +
        '</draw:image>' +
      '</draw:frame>', [
}
{ ARow, ACol are specified when called from a cell node,
  unspecified when called from the Shapes node. }
procedure TsSpreadOpenDocReader.ReadShape(ANode: TDOMNode;
  ARow: Cardinal = UNASSIGNED_ROW_COL_INDEX;
  ACol: Cardinal = UNASSIGNED_ROW_COL_INDEX);

  procedure ReadDrawFrame(ANode: TDOMNode; AHLink: String);
  var
    r, c: Cardinal;
    x, y, w, h: Double;
    dx: Double = 0.0;
    dy: Double = 0.0;
    sx: Double = 1.0;
    sy: Double = 1.0;
    childNode: TDOMNode;
    idx: Integer;
    href: String;
    img: PsImage;
  begin
    x := PtsToMM(HTMLLengthStrToPts(GetAttrValue(ANode, 'svg:x')));
    y := PtsToMM(HTMLLengthStrToPts(GetAttrValue(ANode, 'svg:y')));
    w := PtsToMM(HTMLLengthStrToPts(GetAttrValue(ANode, 'svg:width')));
    h := PtsToMM(HTMLLengthStrToPts(GetAttrValue(ANode, 'svg:height')));
    childNode := ANode.FirstChild;
    while Assigned(childNode) do
    begin
      href := GetAttrValue(childNode, 'xlink:href');
      if href <> '' then
      begin
        idx := TsWorkbook(FWorkbook).FindEmbeddedObj(ExtractFileName(href));
        with FWorksheet as TsWorksheet do begin
          // When called from a cell node, x and y are relative to the cell.
          // When called from the Shapes node, x and y refer to the worksheet.
          CalcImageCell(idx, x, y, w, h, r, c, dy, dx, sx, sy);  // order of dx and dy is correct!
          if ARow <> UNASSIGNED_ROW_COL_INDEX then begin
            r := ARow;
            dy := y;
          end;
          if ACol <> UNASSIGNED_ROW_COL_INDEX then begin
            c := ACol;
            dx := x;
          end;
          idx := WriteImage(r, c, idx, dx, dy, sx, sy);
          if AHLink <> '' then begin
            img := GetPointerToImage(idx);
            img^.HyperlinkTarget := AHLink;
          end;
        end;
      end;
      childNode := childnode.NextSibling;
    end;
  end;

var
  nodeName: String;
  hlink: String;
  linktype: String;
  childnode: TDOMNode;
begin
  if ANode = nil then
    exit;
  nodeName := ANode.NodeName;
  if nodeName = 'draw:frame' then
    ReadDrawFrame(ANode, '')
  else
  if nodeName = 'draw:a' then begin
    hlink := GetAttrValue(ANode, 'xlink:href');
    linktype := GetAttrValue(ANode, 'xlink:type');
    if Lowercase(linktype) = 'simple' then
    begin
      childNode := ANode.FirstChild;
      while assigned(childNode) do begin
        nodeName := childNode.NodeName;
        if nodeName = 'draw:frame' then
          ReadDrawFrame(childNode, hlink);
        childNode := childNode.NextSibling;
      end;
    end;
  end;
end;

procedure TsSpreadOpenDocReader.ReadShapes(ATableNode: TDOMNode);
var
  shapesNode, shapeNode: TDOMNode;
  nodeName: String;
begin
  shapesNode := ATableNode.FirstChild;
  while Assigned(shapesNode) do
  begin
    nodeName := shapesNode.NodeName;
    if nodeName = 'table:shapes' then
    begin
      shapeNode := shapesNode.FirstChild;
      while Assigned(shapeNode) do
      begin
        nodeName := shapeNode.NodeName;
        ReadShape(shapeNode);
        shapeNode := shapeNode.NextSibling;
      end;
    end;
    shapesNode := shapesNode.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadSheetProtection(ANode: TDOMNode;
  ASheet: TsBasicWorksheet);
var
  s: String;
  sp: TsWorksheetProtections;
  cinfo: TsCryptoInfo;
  childNode: TDOMNode;
  nodeName: String;
begin
  if ANode = nil then
    exit;
  s := GetAttrValue(ANode, 'table:protected');
  if s = 'true' then begin
    sp := DEFAULT_SHEET_PROTECTION;
    Include(sp, spCells);

    // These items are ALLOWED (unlike Excel where they are FORBIDDEN).
    // <loext:table-protection loext:select-unprotected-cells="true" />
    // <loext:table-protection loext:select-protected-cells="true" />
    // <loext:table-protection />
    childNode := ANode.FirstChild;
    while childNode <> nil do
    begin
      nodeName := childnode.NodeName;
      if nodeName = 'loext:table-protection' then begin
        s := GetAttrValue(childnode, 'loext:select-unprotected-cells');
        if s='true' then Exclude(sp, spSelectUnlockedCells)
          else Include(sp, spSelectUnlockedCells);

        s := GetAttrValue(childnode, 'loext:select-protected-cells');
        if s='true' then Exclude(sp, spSelectLockedCells)
          else Include(sp, spSelectLockedCells);
      end;
      childNode := childNode.NextSibling;
    end;
    with ASheet as TsWorksheet do begin
      Protection := sp;
      Protect(true);
    end;

    InitCryptoInfo(cinfo);
    cinfo.PasswordHash := GetAttrValue(ANode, 'table:protection-key');
    cinfo.Algorithm := StrToAlgorithm(GetAttrValue(ANode, 'table:protection-key-digest-algorithm'));
    (ASheet as TsWorksheet).CryptoInfo := cinfo;
  end else
    (ASheet as TsWorksheet).Protect(false);
end;

procedure TsSpreadOpenDocReader.ReadSheets(ANode: TDOMNode);
var
  nodename: String;
  sheetName: String;
begin
  ANode := ANode.FirstChild;
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    if nodeName = 'table:table' then begin
      sheetName := GetAttrValue(ANode, 'table:name');
      if sheetName <> '' then
        // Create worksheet immediately because it may be needed for 3d formulas
        (FWorkbook as TsWorkbook).AddWorksheet(sheetname, true);
    end;
    ANode := ANode.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadStyle_ParagraphProperties(ANode: TDOMNode;
  var AFormat: TsCellFormat);
var
  s: String;
begin
  // Horizontal text alignment
  s := GetAttrValue(ANode, 'fo:text-align');
  if s = 'start' then
    AFormat.HorAlignment := haLeft
  else if s = 'end' then
    AFormat.HorAlignment := haRight
  else if s = 'center' then
    AFormat.HorAlignment := haCenter;
  if AFormat.HorAlignment <> haDefault then
    Include(AFormat.UsedFormattingFields, uffHorAlign);

  // BiDi mode
  s := GetAttrValue(ANode, 'style:writing-mode');
  if s = 'lr-tb' then
    AFormat.BiDiMode := bdRTL
  else if s = 'rl-tb' then
    AFormat.BiDiMode := bdRTL;
  if AFormat.BiDiMode <> bdDefault then
    Include(AFormat.UsedFormattingFields, uffBiDi);
end;

procedure TsSpreadOpenDocReader.ReadStyle_TableCellProperties(ANode: TDOMNode;
  var AFormat: TsCellFormat);

  procedure SetBorderStyle(ABorder: TsCellBorder; AStyleValue: String);
  const
    EPS = 0.1;  // takes care of rounding errors for line widths
  var
    L: TStringList;
    i: Integer;
    s: String;
    wid: Double;
    linestyle: String;
    rgb: TsColor;
    p: Integer;
  begin
    L := TStringList.Create;
    try
      L.Delimiter := ' ';
      L.StrictDelimiter := true;
      L.DelimitedText := AStyleValue;
      wid := 0;
      rgb := scNotDefined;
      linestyle := '';
      for i:=0 to L.Count-1 do
      begin
        s := L[i];
        if (s = 'solid') or (s = 'dashed') or (s = 'fine-dashed') or
           (s = 'dotted') or (s = 'double') or (s = 'dash-dot') or
           (s = 'dash-dot-dot') or (s = 'double-thin')
        then begin
          linestyle := s;
          continue;
        end;
        p := pos('pt', s);
        if p = Length(s)-1 then
        begin
          wid := StrToFloat(copy(s, 1, p-1), FPointSeparatorSettings);
          continue;
        end;
        p := pos('mm', s);
        if p = Length(s)-1 then
        begin
          wid := mmToPts(StrToFloat(copy(s, 1, p-1), FPointSeparatorSettings));
          Continue;
        end;
        p := pos('cm', s);
        if p = Length(s)-1 then
        begin
          wid := cmToPts(StrToFloat(copy(s, 1, p-1), FPointSeparatorSettings));
          Continue;
        end;
        rgb := HTMLColorStrToColor(s);
      end;
      AFormat.BorderStyles[ABorder].LineStyle := lsThin;
      if (linestyle = 'solid') then
      begin
        if (wid >= 2.4 - EPS) then
          AFormat.BorderStyles[ABorder].LineStyle := lsThick
        else if (wid >= 1.7 - EPS) then
          AFormat.BorderStyles[ABorder].LineStyle := lsMedium
      end else
      if (linestyle = 'dotted') then
        AFormat.BorderStyles[ABorder].LineStyle := lsHair
      else
      if (linestyle = 'dashed') then
      begin
        if (wid >= 1.7 - EPS) then
          AFormat.BorderStyles[ABorder].LineStyle := lsMediumDash
        else
          AFormat.BorderStyles[ABorder].LineStyle := lsDashed
      end else
      if (linestyle = 'dash-dot') then
      begin
        if (wid >= 1.7 - EPS) then
          AFormat.BorderStyles[ABorder].LineStyle := lsMediumDashDot
        else
          AFormat.BorderStyles[ABorder].LineStyle := lsDashDot
      end else
      if (linestyle = 'dash-dot-dot') then
      begin
        if (wid >= 1.7 - EPS) then
          AFormat.BorderStyles[ABorder].LineStyle := lsMediumDashDotDot
        else
          AFormat.BorderStyles[ABorder].LineStyle := lsDashDotDot
      end else
      if (linestyle = 'fine-dashed') then
        AFormat.BorderStyles[ABorder].LineStyle := lsDotted
      else
      if (linestyle = 'double') or (linestyle = 'double-thin') then
        AFormat.BorderStyles[ABorder].LineStyle := lsDouble;
      AFormat.BorderStyles[ABorder].Color := IfThen(rgb = scNotDefined, scBlack, rgb);
    finally
      L.Free;
    end;
  end;

var
  s: String;
  clr: TsColor;
begin
  // Background color
  s := GetAttrValue(ANode, 'fo:background-color');
  if (s <> '') and (s <> 'transparent') then begin
    clr := HTMLColorStrToColor(s);
    // ODS does not support background fill patterns!
    AFormat.Background.FgColor := IfThen(clr = scNotDefined, scTransparent, clr);
    AFormat.Background.BgColor := AFormat.Background.FgColor;
    if (AFormat.Background.BgColor <> scTransparent) then
    begin
      AFormat.Background.Style := fsSolidFill;
      Include(AFormat.UsedFormattingFields, uffBackground);
    end;
  end;

  // Borders
  s := GetAttrValue(ANode, 'fo:border');
  if (s <> '') and (s <> 'none') then
  begin
    AFormat.Border := AFormat.Border + [cbNorth, cbSouth, cbEast, cbWest];
    SetBorderStyle(cbNorth, s);
    SetBorderStyle(cbSouth, s);
    SetBorderStyle(cbEast, s);
    SetBorderStyle(cbWest, s);
    Include(AFormat.UsedFormattingFields, uffBorder);
  end;
  s := GetAttrValue(ANode, 'fo:border-top');
  if (s <> '') and (s <> 'none') then
  begin
    Include(AFormat.Border, cbNorth);
    SetBorderStyle(cbNorth, s);
    Include(AFormat.UsedFormattingFields, uffBorder);
  end;
  s := GetAttrValue(ANode, 'fo:border-right');
  if (s <> '') and (s <> 'none') then
  begin
    Include(AFormat.Border, cbEast);
    SetBorderStyle(cbEast, s);
    Include(AFormat.UsedFormattingFields, uffBorder);
  end;
  s := GetAttrValue(ANode, 'fo:border-bottom');
  if (s <> '') and (s <> 'none') then
  begin
    Include(AFormat.Border, cbSouth);
    SetBorderStyle(cbSouth, s);
    Include(AFormat.UsedFormattingFields, uffBorder);
  end;
  s := GetAttrValue(ANode, 'fo:border-left');
  if (s <> '') and (s <> 'none') then
  begin
    Include(AFormat.Border, cbWest);
    SetBorderStyle(cbWest, s);
    Include(AFormat.UsedFormattingFields, uffBorder);
  end;
  s := GetAttrValue(ANode, 'style:diagonal-bl-tr');
  if (s <> '') and (s <> 'none') then
  begin
    Include(AFormat.Border, cbDiagUp);
    SetBorderStyle(cbDiagUp, s);
    Include(AFormat.UsedFormattingFields, uffBorder);
  end;
  s := GetAttrValue(ANode, 'style:diagonal-tl-br');
  if (s <> '') and (s <>'none') then
  begin
    Include(AFormat.Border, cbDiagDown);
    SetBorderStyle(cbDiagDown, s);
    Include(AFormat.UsedFormattingFields, uffBorder);
  end;

  // Text wrap
  s := GetAttrValue(ANode, 'fo:wrap-option');
  if (s = 'wrap') then
    Include(AFormat.UsedFormattingFields, uffWordwrap);

  // Test rotation
  s := GetAttrValue(ANode, 'style:rotation-angle');
  if s = '90' then
    AFormat.TextRotation := rt90DegreeCounterClockwiseRotation
  else if s = '270' then
    AFormat.TextRotation := rt90DegreeClockwiseRotation;
  s := GetAttrValue(ANode, 'style:direction');
  if s = 'ttb' then
    AFormat.TextRotation := rtStacked;
  if AFormat.TextRotation <> trHorizontal then
    Include(AFormat.UsedFormattingFields, uffTextRotation);

  // Vertical text alignment
  s := GetAttrValue(ANode, 'style:vertical-align');
  if s = 'top' then
    AFormat.VertAlignment := vaTop
  else if s = 'middle' then
    AFormat.VertAlignment := vaCenter
  else if s = 'bottom' then
    AFormat.VertAlignment := vaBottom;
  if AFormat.VertAlignment <> vaDefault then
    Include(AFormat.UsedFormattingFields, uffVertAlign);

  // Protection
  s := GetAttrValue(ANode, 'style:cell-protect');
  if s = 'none' then
    AFormat.Protection := []
  else if (s = 'protected formula-hidden') or (s = 'formula-hidden protected') then
    AFormat.Protection := [cpLockCell, cpHideFormulas]
  else if s = 'protected' then
    AFormat.Protection := [cpLockCell]
  else if s = 'formula-hidden' then
    AFormat.Protection := [cpHideFormulas]
  else if s = 'hidden-and-protected' then
    AFormat.Protection := [cpLockCell, cpHideFormulas];
  // NOTE: This not exact... According to
  // https://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html,
  // section 20.246, this hides and locks cell content, not just
  // formulas...
  if AFormat.Protection <> DEFAULT_CELL_PROTECTION then
    Include(AFormat.UsedFormattingFields, uffProtection);
end;

procedure TsSpreadOpenDocReader.ReadStyle_TextProperties(ANode: TDOMNode;
  AStyleName: String; AFont: TsFont; var AFormat: TsCellFormat);
var
  fntName: String;
  fntSize: Single;
  fntStyle: TsFontStyles;
  fntColor: TsColor;
  fntPos: TsFontPosition;
begin
  fntName := AFont.FontName;
  fntSize := AFont.Size;
  fntStyle := AFont.Style;
  fntColor := AFont.Color;
  fntPos := AFont.Position;

  ReadFont(ANode, fntName, fntSize, fntStyle, fntColor, fntPos);

  if SameText(AStylename, 'Default') then
  begin
    TsWorkbook(FWorkbook).ReplaceFont(DEFAULT_FONTINDEX, fntName, fntSize, fntStyle, fntColor, fntPos);
    AFormat.FontIndex := DEFAULT_FONTINDEX;
  end else
  if SameText(AStylename, 'Excel_20_Built-in_20_Hyperlink') then
  begin
    TsWorkbook(FWorkbook).ReplaceFont(HYPERLINK_FONTINDEX, fntName, fntSize, fntStyle, fntColor, fntPos);
    AFormat.FontIndex := HYPERLINK_FONTINDEX;
    //fntIndex := ReadFont(styleChildNode, HYPERLINK_FONTINDEX)
  end else
  begin
    AFormat.FontIndex := TsWorkbook(FWorkbook).FindFont(fntName, fntSize, fntStyle, fntColor, fntPos);
    if AFormat.FontIndex = -1 then
      AFormat.FontIndex := TsWorkbook(FWorkbook).AddFont(fntName, fntSize, fntStyle, fntColor, fntPos);
  end;

  if AFormat.FontIndex > 0 then
    Include(AFormat.UsedFormattingFields, uffFont);
end;

procedure TsSpreadOpenDocReader.ReadStyles(AStylesNode: TDOMNode);
var
  styleNode: TDOMNode;
  styleChildNode: TDOMNode;
  nodeName: String;
  family: String;
  styleName: String;
  parentstyle: String;
  fmt: TsCellFormat;
  numFmtIndexDefault: Integer;
  numFmtName: String;
  numFmtStr: String;
  numFmtIndex: Integer;
  numFmtParams: TsNumFormatParams;
  clr: TsColor;
  fnt: TsFont;
  fntName: String;
  fntSize: Single;
  fntStyle: TsFontStyles;
  fntColor: TsColor;
  fntPos: TsFontPosition;
  fntIndex: Integer;
  s: String;
  idx: Integer;
begin
  if not Assigned(AStylesNode) then
    exit;

  nodeName := AStylesNode.NodeName;
  numFmtIndexDefault := FindNumFormatByName('N0');

  styleNode := AStylesNode.FirstChild;
  while Assigned(styleNode) do begin
    nodeName := styleNode.NodeName;
    if nodeName = 'style:default-style' then
    begin
      family := GetAttrValue(stylenode, 'style:family');
      if family = 'table-cell' then begin
        InitFormatRecord(fmt);
        fmt.Name := 'DefaultStyle';
        fnt := (FWorkbook as TsWorkbook).GetFont(fmt.FontIndex);
        fntName := fnt.FontName;
        fntSize := fnt.Size;
        fntStyle := fnt.Style;
        fntColor := fnt.Color;
        fntPos := fnt.Position;
        styleChildNode := stylenode.FirstChild;
        while Assigned(styleChildNode) do begin
          nodename := styleChildNode.NodeName;
          if nodename = 'style:text-properties' then
            ReadFont(
              styleNode.FindNode('style:text-properties'),
              fntName, fntSize, fntStyle, fntColor, fntPos
            )
//            fmt.FontIndex := ReadFont(styleNode.FindNode('style:text-properties'), DEFAULT_FONTINDEX)
          else
          if nodename = 'style:paragraph-properties' then;
            // not used;
          styleChildNode := styleChildNode.nextSibling;
        end;
        fmt.FontIndex := TsWorkbook(FWorkbook).FindFont(fntName, fntSize, fntStyle, fntColor, fntPos);
        if fmt.FontIndex = -1 then
          fmt.FontIndex := TsWorkbook(FWorkbook).AddFont(fntname, fntsize, fntstyle, fntColor, fntPos);
        if fmt.FontIndex > 0 then
          Include(fmt.UsedFormattingFields, uffFont);
        FCellFormatList.Add(fmt);
      end;
    end else
    if nodeName = 'style:style' then
    begin
      family := GetAttrValue(styleNode, 'style:family');
      parentstyle := GetAttrValue(stylenode, 'style:parent-style-name');

      // Table styles
      if family = 'table' then
        ReadTableStyle(styleNode);

      // Column styles
      if family = 'table-column' then
        ReadColumnStyle(styleNode);

      // Row styles
      if family = 'table-row' then
        ReadRowStyle(styleNode);

      // Cell styles
      if family = 'table-cell' then
      begin
        styleName := GetAttrValue(styleNode, 'style:name');

        InitFormatRecord(fmt);

        if parentstyle <> '' then
        begin
          idx := FCellFormatList.FindIndexOfName(parentstyle);
          if idx > -1 then
            fmt := FCellFormatList[idx]^;
        end else
        if styleName <> '' then
        begin
          idx := FCellFormatList.FindIndexOfName(stylename);
          if idx > -1 then
            fmt := FCellFormatList[idx]^;
        end;
        fmt.Name := styleName;

        fnt := (Workbook as TsWorkbook).GetFont(fmt.FontIndex);
        fntName := fnt.FontName;
        fntSize := fnt.Size;
        fntStyle := fnt.Style;
        fntColor := fnt.Color;
        fntPos := fnt.Position;

        numFmtIndex := -1;
        numFmtName := GetAttrValue(styleNode, 'style:data-style-name');
        if numFmtName <> '' then numFmtIndex := FindNumFormatByName(numFmtName);
        if numFmtIndex = -1 then numFmtIndex := numFmtIndexDefault;
        numFmtStr := NumFormatList[numFmtIndex];
        numFmtStr := Copy(numFmtStr, pos(':', numFmtStr)+1, Length(numFmtStr));
        fmt.NumberFormatIndex := (Workbook as TsWorkbook).AddNumberFormat(numFmtStr);
        numFmtParams := (Workbook as TsWorkbook).GetNumberFormat(fmt.NumberFormatIndex);
        if numFmtParams <> nil then begin
          fmt.NumberFormat := numFmtParams.NumFormat;
          fmt.NumberFormatStr := numFmtStr;
          Include(fmt.UsedFormattingFields, uffNumberFormat);
        end;

        styleChildNode := styleNode.FirstChild;
        while Assigned(styleChildNode) do
        begin
          nodeName := styleChildNode.NodeName;
          case nodeName of
            'style:text-properties':
              ReadStyle_TextProperties(styleChildNode, styleName, fnt, fmt);
            'style:table-cell-properties':
              ReadStyle_TableCellProperties(styleChildNode, fmt);
            'style:paragraph-properties':
              ReadStyle_ParagraphProperties(styleChildNode, fmt);
          end;
          styleChildNode := styleChildNode.NextSibling;
        end;
        FCellFormatList.Add(fmt);
      end
      else
      if family = 'text' then
      begin
        // "Rich-text formatting run" style
        // Nodes are named "T1", "T2", etc.
        styleName := GetAttrValue(styleNode, 'style:name');
        styleChildNode := styleNode.FirstChild;
        while Assigned(styleChildNode) do
        begin
          nodeName := styleChildNode.NodeName;
          if nodeName = 'style:text-properties' then
          begin
            // Setup default values which identify font elements to be replaced
            // by the cell font value
            fntName := '';
            fntSize := -1;
            fntStyle := [];
            fntColor := scNone;
            fntPos := fpNormal;
            ReadFont(styleChildNode, fntName, fntSize, fntStyle, fntColor, fntPos);
            // Does this font already exist in the FRichTextFontList?
            fntIndex := FindFontInList(FRichTextFontList, fntName, fntSize, fntStyle, fntColor, fntPos);
            // No - add the font to the list.
            if fntIndex = -1 then
            begin
              fnt := TsFont.Create(fntName, fntSize, fntStyle, fntColor, fntPos);
              fntIndex := FRichTextFontList.Add(fnt);
            end;

            // Store this is in a dummy format in the cell format list
            InitFormatRecord(fmt);
            fmt.Name := styleName;
            fmt.FontIndex := fntIndex;
            Include(fmt.UsedFormattingFields, uffFont);
            FCellFormatList.Add(fmt);
          end;
          styleChildNode := stylechildNode.NextSibling;
        end;
      end;
    end;
    styleNode := styleNode.NextSibling;
  end;
end;

procedure TsSpreadOpenDocReader.ReadTableStyle(AStyleNode: TDOMNode);
var
  stylename, nodename: String;
  styleChildNode: TDOMNode;
  bidi: String;
  tablestyle: TTableStyleData;
  display: String = '';
  tabColor: String = '';
begin
 // nodeName := GetAttrValue(AStyleNode, 'style:name');
  stylename := GetAttrValue(AStyleNode, 'style:name');
  styleChildNode := AStyleNode.FirstChild;

  while Assigned(styleChildNode) do
  begin
    nodename := styleChildNode.NodeName;
    if nodeName = 'style:table-properties' then
    begin
//      stylename := GetAttrValue(styleChildNode, 'style:name');
      bidi := GetAttrValue(styleChildNode, 'style:writing-mode');
      display := GetAttrValue(styleChildNode, 'table:display');
      tabcolor := GetAttrValue(styleChildNode, 'tableooo:tab-color');
    end;
    styleChildNode := styleChildNode.NextSibling;
  end;

  tablestyle := TTableStyleData.Create;
  tablestyle.Name := styleName;
  if bidi = 'rl-tb' then
    tablestyle.BiDiMode := bdRTL
  else
    tablestyle.BiDiMode := bdLTR;
  tablestyle.Hidden := display = 'false';
  if tabcolor = '' then
    tablestyle.TabColor := scNotDefined
  else
    tablestyle.TabColor := HTMLColorStrToColor(tabcolor);
  FTableStyleList.Add(tablestyle);
end;


{ TsSpreadOpenDocWriter }

procedure TsSpreadOpenDocWriter.AddBuiltinNumFormats;
begin
  FNumFormatList.Clear;
  FNumFormatList.Add('N0:');
end;

{ Creates the streams for the individual data files. Will be zipped into a
  single xlsx file. }
procedure TsSpreadOpenDocWriter.CreateStreams;
begin
  FSMeta := CreateTempStream(FWorkbook, 'fpsM');
  FSSettings := CreateTempStream(FWorkbook, 'fpsS');
  FSStyles := CreateTempStream(FWorkbook, 'fpsSTY');
  FSContent := CreateTempStream(FWorkbook, 'fpsC');
  FSMimeType := CreateTempStream(FWorkbook, 'fpsMT');
  FSMetaInfManifest := CreateTempStream(FWorkbook, 'fpsMIM');
  {
  if boFileStream in FWorkbook.Options then
  begin
    FSMeta := TFileStream.Create(GetTempFileName('', 'fpsM'), fmCreate);
    FSSettings := TFileStream.Create(GetTempFileName('', 'fpsS'), fmCreate);
    FSStyles := TFileStream.Create(GetTempFileName('', 'fpsSTY'), fmCreate);
    FSContent := TFileStream.Create(GetTempFileName('', 'fpsC'), fmCreate);
    FSMimeType := TFileStream.Create(GetTempFileName('', 'fpsMT'), fmCreate);
    FSMetaInfManifest := TFileStream.Create(GetTempFileName('', 'fpsMIM'), fmCreate);
  end else
  if (boBufStream in Workbook.Options) then
  begin
    FSMeta := TBufStream.Create(GetTempFileName('', 'fpsM'));
    FSSettings := TBufStream.Create(GetTempFileName('', 'fpsS'));
    FSStyles := TBufStream.Create(GetTempFileName('', 'fpsSTY'));
    FSContent := TBufStream.Create(GetTempFileName('', 'fpsC'));
    FSMimeType := TBufStream.Create(GetTempFileName('', 'fpsMT'));
    FSMetaInfManifest := TBufStream.Create(GetTempFileName('', 'fpsMIM'));
  end else
  begin
    FSMeta := TMemoryStream.Create;
    FSSettings := TMemoryStream.Create;
    FSStyles := TMemoryStream.Create;
    FSContent := TMemoryStream.Create;
    FSMimeType := TMemoryStream.Create;
    FSMetaInfManifest := TMemoryStream.Create;
  end;
  }
  // FSSheets will be created when needed.
end;

{ Destroys the temporary streams that were created by the writer }
procedure TsSpreadOpenDocWriter.DestroyStreams;
begin
  DestroyTempStream(FSMeta);
  DestroyTempStream(FSSettings);
  DestroyTempStream(FSStyles);
  DestroyTempStream(FSContent);
  DestroyTempStream(FSMimeType);
  DestroyTempStream(FSMetaInfManifest);
end;

procedure TsSpreadOpenDocWriter.GetHeaderFooterImageName(
  APageLayout: TsPageLayout; out AHeader, AFooter: String);
var
  sct: TsHeaderFooterSectionIndex;
  img: TsHeaderFooterImage;
  ext: String;
begin
  AHeader := '';
  AFooter := '';
  if APageLayout.HasHeaderFooterImages then
  begin
    // ods supports only a single image per header/footer. We use the first one.
    for sct in TsHeaderFooterSectionIndex do
      if APageLayout.HeaderImages[sct].Index > -1 then
      begin
        img := APageLayout.HeaderImages[sct];
        ext := GetImageTypeExt(TsWorkbook(FWorkbook).GetEmbeddedObj(img.Index).ImageType);
        AHeader := Format('%d.%s', [img.Index+1, ext]);
        break;
      end;
    for sct in TsHeaderFooterSectionIndex do
      if APageLayout.FooterImages[sct].Index > -1 then
      begin
        img := APageLayout.FooterImages[sct];
        ext := GetImageTypeExt(TsWorkbook(FWorkbook).GetEmbeddedObj(img.Index).Imagetype);
        AFooter := Format('%d.%s', [img.Index+1, ext]);
        break;
      end;
  end;
end;

procedure TsSpreadOpenDocWriter.GetHeaderFooterImagePosStr(
  APagelayout: TsPageLayout; out AHeader, AFooter: String);

  function GetPosStr(tags: String): String;
  begin
    if tags[1] in ['L', 'x'] then
      Result := 'left' else
    if tags[2] in ['C', 'x'] then
      Result := 'center' else
    if tags[3] in ['R', 'x'] then
      Result := 'right'
    else
      Result := '';
  end;

var
  hdrTags, ftrTags: String;
begin
  APageLayout.GetImageSections(hdrTags, ftrTags);
  AHeader := GetPosStr(hdrTags);
  AFooter := GetPosStr(ftrTags);
end;

function TsSpreadOpenDocWriter.GetStyleName(ACell: PCell): String;
var
  fmt: TsCellFormat;
  ncf: Integer;
  cf: Integer;
begin
  ncf := Length(ACell^.ConditionalFormatIndex);
  if ncf > 0 then
  begin
    cf := ACell^.ConditionalFormatIndex[ncf-1];
    // Support only last conditional format of cell (having highest priority)
    Result := 'ce' + IntToStr((cf+1)*1000 + ACell^.FormatIndex);
  end else
  begin
    fmt := (FWorkbook as TsWorkbook).GetCellFormat(ACell^.FormatIndex);
    if fmt.UsedFormattingFields <> [] then
      Result :=  'ce' + IntToStr(ACell^.FormatIndex)
    else
      Result := '';
  end;
end;

procedure TsSpreadOpenDocWriter.InternalWriteToStream(AStream: TStream);
var
  FZip: TZipper;
begin
  { Analyze the workbook and collect all information needed }
  ListAllNumFormats;
  ListAllColumnStyles;
  ListAllRowStyles;
  ListAllHeaderFooterFonts;

  { Create the streams that will hold the file contents }
  CreateStreams;

  { Fill the strings with the contents of the files }
  WriteMimetype();
  WriteMetaInfManifest();
  WriteMeta();
  WriteSettings();
  WriteStyles();
  WriteContent;

  { Now compress the files }
  FZip := TZipper.Create;
  try
    FZip.FileName := GetTempFilename;   // needed if the zipped file is too big for in-memory processing
    FZip.Entries.AddFileEntry(FSMeta, OPENDOC_PATH_META);
    FZip.Entries.AddFileEntry(FSSettings, OPENDOC_PATH_SETTINGS);
    FZip.Entries.AddFileEntry(FSStyles, OPENDOC_PATH_STYLES);
    FZip.Entries.AddFileEntry(FSContent, OPENDOC_PATH_CONTENT);
    FZip.Entries.AddFileEntry(FSMimetype, OPENDOC_PATH_MIMETYPE);
    FZip.Entries.AddFileEntry(FSMetaInfManifest, OPENDOC_PATH_METAINF_MANIFEST);
    ZipPictures(FZip);

    ResetStreams;

    FZip.SaveToStream(AStream);

  finally
    DestroyStreams;
    FZip.Free;
  end;
end;

procedure TsSpreadOpenDocWriter.ListAllColumnStyles;
var
  i, j, c: Integer;
  book: TsWorkbook;
  sheet: TsWorksheet;
  found: Boolean;
  colstyle: TColumnStyleData;
  item: TColumnStyleData;
  colPageBreak: Boolean;
  w: Double;
  wDef: Double;   // Default column width
  col: PCol;
begin
  book := TsWorkbook(FWorkbook);
  sheet := book.GetFirstWorksheet;
  if sheet = nil then
    exit;

  wDef := sheet.ReadDefaultColWidth(book.Units);

  { At first, add the default column width }
  colStyle := TColumnStyleData.Create;
  colStyle.Name := 'co1';              // 1 = "one"
  colStyle.ColWidth := wDef;
  FColumnStyleList.Add(colStyle);

  { Then iterate through all sheets and all columns and store the records with
    unique properties in the FColumnStyleList. }
  for i:=0 to book.GetWorksheetCount-1 do
  begin
    sheet := book.GetWorksheetByIndex(i);
    for c := 0 to sheet.Cols.Count-1 do
    begin
      col := PCol(sheet.Cols[c]);
      if not sheet.IsDefaultCol(col) then
      begin
        colPageBreak := (croPageBreak in col^.Options);  // has page break?
        if col^.ColWidthType = cwtDefault then
          w := wDef
        else
          w := col^.Width;   // is in workbook units
        // Look for this width in the current ColumnStyleList
        found := false;
        for j := 0 to FColumnStyleList.Count - 1 do begin
          item := TColumnStyleData(FColumnStyleList[j]);
          if SameValue(item.ColWidth, w, COLWIDTH_EPS) and
            (item.PageBreak = colPageBreak) then
          begin
            found := true;
            break;
          end;
        end;
        // Not found? Then add the column as a new column style
        if not found then
        begin
          colStyle := TColumnStyleData.Create;
          colStyle.Name := Format('co%d', [FColumnStyleList.Count + 1]);
          colStyle.ColWidth := w;
          colStyle.PageBreak := colPageBreak;
          FColumnStyleList.Add(colStyle);
        end;
      end;
    end;
  end;
end;

{ Collects the fonts used by headers and footers in the FHeaderFooterFontList }
procedure TsSpreadOpenDocWriter.ListAllHeaderFooterFonts;

  { Add the fonts used in the specified header/footer line to the
    HeaderFooterFontList. This is done while the HeaderFooterParser is created. }
  procedure AddFontsOfHeaderFooter(AText: String; ADefaultFont: TsHeaderFooterFont);
  begin
    TsSpreadOpenDocHeaderFooterParser.Create(AText, FHeaderFooterFontList, ADefaultFont).Free;
  end;

var
  defFnt: TsHeaderFooterFont;
  i: Integer;
  sheet: TsWorksheet;
  book: TsWorkbook;
begin
  book := FWorkbook as TsWorkbook;
  defFnt := TsHeaderFooterFont.Create(book.GetDefaultFont);
  try
    for i:=0 to book.GetWorksheetCount-1 do
    begin
      sheet := book.GetWorksheetByIndex(i);
      AddFontsOfHeaderFooter(sheet.pageLayout.Headers[1], defFnt);
      AddFontsOfHeaderFooter(sheet.PageLayout.Headers[2], defFnt);
      AddFontsOfHeaderFooter(sheet.PageLayout.Footers[1], defFnt);
      AddFontsOfHeaderFooter(sheet.PageLayout.Footers[2], defFnt);
    end;
  finally
    defFnt.Free;
  end;
end;

{ Contains all number formats used in the workbook. Overrides the inherited
  method to assign a unique name according to the OpenDocument syntax ("N<number>"
  to the format items. }
procedure TsSpreadOpenDocWriter.ListAllNumFormats;
const
  FMT_BASE = 1000;  // Format number to start with. Not clear if this is correct...
var
  i: Integer;
  nfparams: TsNumFormatParams;
begin
  // The default format has already been added.
  for i:=0 to (Workbook as TsWorkbook).GetNumberFormatCount - 1 do
  begin
    nfParams := TsWorkbook(Workbook).GetNumberFormat(i);
    if nfParams <> nil then
      FNumFormatList.Add(Format('N%d:%s', [FMT_BASE+i, nfParams.NumFormatStr]));
  end;
end;

procedure TsSpreadOpenDocWriter.ListAllRowStyles;
var
  i, j, r: Integer;
  sheet: TsWorksheet;
  row: PRow;
  found: Boolean;
  rowstyle: TRowStyleData;
  item: TRowStyleData;
  rowPageBreak: Boolean;
  h: Double;
  book: TsWorkbook;
begin
  book := FWorkbook as TsWorkbook;

  { At first, add the default row height }
  { Initially, row height units will be the same as in the workbook }
  rowStyle := TRowStyleData.Create;
  rowStyle.Name := 'ro1';        // 1 = "one"
  rowStyle.RowHeight := book.ConvertUnits(15, suPoints, FWorkbook.Units);
  rowStyle.RowHeightType := rhtAuto;
  FRowStyleList.Add(rowStyle);

  for i:=0 to book.GetWorksheetCount-1 do
  begin
    sheet := book.GetWorksheetByIndex(i);
    for r:=0 to sheet.GetLastRowIndex do
    begin
      row := sheet.FindRow(r);
      if not sheet.IsDefaultRow(row) then
      begin
        rowPageBreak := (croPageBreak in row^.Options);
        h := sheet.GetRowHeight(r, FWorkbook.Units);
        // Look for this height in the current RowStyleList
        found := false;
        for j:=0 to FRowStyleList.Count-1 do begin
          item := TRowStyleData(FRowStyleList[j]);
          if SameValue(item.RowHeight, h, ROWHEIGHT_EPS) and
            (item.RowHeightType = row^.RowHeightType) and
            (item.PageBreak = rowPageBreak) then
          begin
            found := true;
            break;
          end;
        end;
        // Not found? --> Add the row as a new row style
        if not found then
        begin
          rowStyle := TRowStyleData.Create;
          rowStyle.Name := Format('ro%d', [FRowStyleList.Count + 1]);
          rowStyle.RowHeight := h;
          rowStyle.RowHeightType := row^.RowHeightType;
          rowStyle.PageBreak := rowPageBreak;
          FRowStyleList.Add(rowStyle);
        end;
      end;
    end;
  end;
end;

{ Is called before zipping the individual file parts. Rewinds the streams. }
procedure TsSpreadOpenDocWriter.ResetStreams;
begin
  FSMeta.Position := 0;
  FSSettings.Position := 0;
  FSStyles.Position := 0;
  FSContent.Position := 0;
  FSMimeType.Position := 0;
  FSMetaInfManifest.Position := 0;
end;

{ Writes the node "office:automatic-styles". Although this node occurs in both
  "contents.xml" and "styles.xml" files, this method is called only for writing
  to "styles.xml". }
procedure TsSpreadOpenDocWriter.WriteAutomaticStyles(AStream: TStream);
var
  i: Integer;
  sheet: TsWorksheet;
  fnt: TXMLHeaderFooterFont;
  book: TsWorkbook;

begin
  AppendToStream(AStream,
    '<office:automatic-styles>');

  AppendToStream(AStream,
      '<style:page-layout style:name="Mpm1">' +

        '<style:page-layout-properties '+
             'fo:margin-top="1.25cm" '+
             'fo:margin-bottom="1.25cm" '+
             'fo:margin-left="1.905cm" '+
             'fo:margin-right="1.905cm" />' +

        '<style:header-style>' +
          '<style:header-footer-properties '+
             'fo:min-height="0.751cm" '+
             'fo:margin-left="0cm" '+
             'fo:margin-right="0cm" '+
             'fo:margin-bottom="0.25cm" '+
             'fo:margin-top="0cm" />' +
        '</style:header-style>' +

        '<style:footer-style>' +
          '<style:header-footer-properties '+
             'fo:min-height="0.751cm" '+
             'fo:margin-left="0cm" '+
             'fo:margin-right="0cm" '+
             'fo:margin-top="0.25cm" '+
             'fo:margin-bottom="0cm" />' +
        '</style:footer-style>' +

      '</style:page-layout>');

  book := TsWorkbook(FWorkbook);

  for i:=0 to book.GetWorksheetCount-1 do begin
    sheet := book.GetWorksheetByIndex(i);
    AppendToStream(AStream,
      WritePageLayoutXMLAsString('Mpm' + IntToStr(3+i), sheet.PageLayout));
  end;

  for i:=0 to FHeaderFooterFontList.Count-1 do
  begin
    fnt := TXMLHeaderFooterFont(FHeaderFooterFontList[i]);
    fnt.StyleName := 'MT' + IntToStr(i+1);
    AppendToStream(AStream, Format(
      '<style:style style:name="%s" style:family="text">' +
        '<style:text-properties %s />' +
      '</style:style>', [
      fnt.StyleName, WriteHeaderFooterFontXMLAsString(fnt)
    ]));
  end;

  AppendToStream(AStream,
    '</office:automatic-styles>');
end;

procedure TsSpreadOpenDocWriter.WriteMetaInfManifest;
var
  i: Integer;
  ext: String;
  mime: String;
  imgtype: Integer;
  embObj: TsEmbeddedObj;
begin
  AppendToStream(FSMetaInfManifest,
    '<manifest:manifest xmlns:manifest="' + SCHEMAS_XMLNS_MANIFEST + '">');
  AppendToStream(FSMetaInfManifest,
      '<manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.spreadsheet" manifest:full-path="/" />');
  AppendToStream(FSMetaInfManifest,
      '<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml" />');
  AppendToStream(FSMetaInfManifest,
      '<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="styles.xml" />');
  AppendToStream(FSMetaInfManifest,
      '<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="meta.xml" />');
  AppendToStream(FSMetaInfManifest,
      '<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="settings.xml" />');
  for i:=0 to (FWorkbook as TsWorkbook).GetEmbeddedObjCount-1 do
  begin
    embObj := TsWorkbook(FWorkbook).GetEmbeddedObj(i);
    imgtype := embObj.ImageType;
    if imgtype = itUnknown then
      continue;
    mime := GetImageMimeType(imgtype);
    ext := GetImageTypeExt(imgType);
    AppendToStream(FSMetaInfManifest, Format(
      '<manifest:file-entry manifest:media-type="%s" manifest:full-path="Pictures/%d.%s" />',
      [mime, i+1, ext]
    ));
  end;
  AppendToStream(FSMetaInfManifest,
    '</manifest:manifest>');
end;

procedure TsSpreadOpenDocWriter.WriteMeta;
var
  book: TsWorkbook;
  i: Integer;
  s: String;
begin
  book := TsWorkbook(FWorkbook);

  AppendToStream(FSMeta,
    XML_HEADER);
  AppendToStream(FSMeta,
    '<office:document-meta ' +
      'xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" ' +
      'xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" ' +
      'xmlns:grddl="http://www.w3.org/2003/g/data-view#" ' +
      'xmlns:ooo="http://openoffice.org/2004/office" ' +
      'xmlns:xlink="http://www.w3.org/1999/xlink" '+
      'xmlns:dc="http://purl.org/dc/elements/1.1/" '+
      'office:version="1.2">');
  {
  AppendToStream(FSMeta,
    '<office:document-meta xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
      '" xmlns:dcterms="' + SCHEMAS_XMLNS_DCTERMS +
      '" xmlns:meta="' + SCHEMAS_XMLNS_META +
      '" xmlns="' + SCHEMAS_XMLNS +
      '" xmlns:ex="' + SCHEMAS_XMLNS + '">');
      }
  AppendToStream(FSMeta,
      '<office:meta>',
        '<meta:generator>FPSpreadsheet Library</meta:generator>' +
        '<meta:document-statistic />');

  if book.Metadata.Title <> '' then
  begin
    s := book.Metadata.Title;
    AppendToStream(FSMeta, Format(
        '<dc:title>%s</dc:title>', [s]));
  end;

  if book.Metadata.Subject <> '' then
  begin
    s := book.Metadata.Subject;
    AppendToStream(FSMeta, Format(
        '<dc:subject>%s</dc:subject>', [s]));
  end;

  if book.Metadata.CreatedBy <> '' then
    AppendToStream(FSMeta, Format(
        '<meta:initial-creator>%s</meta:initial-creator>', [book.MetaData.CreatedBy]));

  if book.MetaData.LastModifiedBy <> '' then
    AppendToStream(FSMeta, Format(
        '<dc:creator>%s</dc:creator>', [book.Metadata.LastModifiedBy]));

  if book.MetaData.DateCreated > 0 then
  begin
    // ODS stored the creation date in UTC.
    s := FormatDateTime(ISO8601FormatExtendedUTC, book.MetaData.DateCreated);
    AppendToStream(FSMeta, Format(
        '<meta:creation-date>%s</meta:creation-date>', [s]));
  end;

  if book.MetaData.DateLastModified > 0 then
  begin
    // Date of last modification is NOT UTC.
    s := FormatDateTime(ISO8601FormatExtended, book.MetaData.DateLastModified);
    AppendToStream(FSMeta, Format(
        '<dc:date>%s</dc:date>', [s]));
  end;

  if book.MetaData.Keywords.Count > 0 then
    for i := 0 to book.MetaData.Keywords.Count-1 do
      AppendToStream(FSMeta, Format(
        '<meta:keyword>%s</meta:keyword>', [book.MetaData.Keywords[i]]));

  if book.MetaData.Comments.Count > 0 then
  begin
    s := book.MetaData.Comments[0];
    for i := 1 to book.MetaData.Comments.Count-1 do
      s := s + #10 + book.MetaData.Comments[i];
    AppendToStream(FSMeta, Format(
        '<dc:description>%s</dc:description>', [s]));
  end;

  if book.MetaData.Custom.Count > 0 then
  begin
    for i := 0 to book.Metadata.Custom.Count-1 do
      AppendToStream(FSMeta, Format(
        '<meta:user-defined meta:name="%s">%s</meta:user-defined>', [
          book.Metadata.Custom.Names[i],
          book.Metadata.Custom.ValueFromIndex[i]
        ]));
  end;

  AppendToStream(FSMeta,
      '</office:meta>');
  AppendToStream(FSMeta,
    '</office:document-meta>');
end;

procedure TsSpreadOpenDocWriter.WriteMimetype;
begin
  AppendToStream(FSMimeType,
    'application/vnd.oasis.opendocument.spreadsheet'
  );
end;

procedure TsSpreadOpenDocWriter.ZipPictures(AZip: TZipper);
var
  i: Integer;
  embObj: TsEmbeddedObj;
  embName: String;
  ext: String;
begin
  for i:=0 to (FWorkbook as TsWorkbook).GetEmbeddedObjCount-1 do
  begin
    embObj := (FWorkbook as TsWorkbook).GetEmbeddedObj(i);
    // The original ods files have a very long, ranomd, unique (?) filename.
    // Tests show that a simple, unique, increasing number works as well.
    ext := GetImageTypeExt(embObj.ImageType);
    embName := Format('%d.%s', [i+1, ext]);
    embObj.Stream.Position := 0;
    AZip.Entries.AddFileEntry(embObj.Stream, 'Pictures/' + embname);
  end;
end;

procedure TsSpreadOpenDocWriter.WriteSettings;
var
  i: Integer;
  showGrid, showHeaders: Boolean;
  sheet: TsWorksheet;
  actSheet: String;
  zoomvalue: String;
  book: TsWorkbook;
begin
  book := FWorkbook as TsWorkbook;

  // Open/LibreOffice allow to change showGrid and showHeaders only globally.
  // As a compromise, we check whether there is at least one page with these
  // settings off. Then we assume it to be valid also for the other sheets.
  showGrid := true;
  showHeaders := true;
  actSheet := 'Table1';
  zoomValue := '100';
  for i:=0 to book.GetWorksheetCount-1 do
  begin
    sheet := book.GetWorksheetByIndex(i);
    if sheet = book.ActiveWorksheet then
      actSheet := UTF8TextToXMLText(sheet.Name);
    if not (soShowGridLines in sheet.Options) then showGrid := false;
    if not (soShowHeaders in sheet.Options) then showHeaders := false;
  end;

  AppendToStream(FSSettings,
    XML_HEADER);
  AppendToStream(FSSettings,
    '<office:document-settings xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
     '" xmlns:config="' + SCHEMAS_XMLNS_CONFIG +
     '" xmlns:ooo="' + SCHEMAS_XMLNS_OOO + '">');
  AppendToStream(FSSettings,
      '<office:settings>' +
        '<config:config-item-set config:name="ooo:view-settings">' +
         '<config:config-item-map-indexed config:name="Views">' +
            '<config:config-item-map-entry>' +
              '<config:config-item config:name="ActiveTable" config:type="string">'+actSheet+'</config:config-item>' +
              '<config:config-item config:name="ZoomValue" config:type="int">'+zoomValue+'</config:config-item>' +
              '<config:config-item config:name="PageViewZoomValue" config:type="int">100</config:config-item>' +
              '<config:config-item config:name="ShowPageBreakPreview" config:type="boolean">false</config:config-item>' +
              '<config:config-item config:name="ShowGrid" config:type="boolean">'+FALSE_TRUE[showGrid]+'</config:config-item>' +
              '<config:config-item config:name="HasColumnRowHeaders" config:type="boolean">'+FALSE_TRUE[showHeaders]+'</config:config-item>' +
              '<config:config-item-map-named config:name="Tables">');

                WriteTableSettings(FSSettings);

  AppendToStream(FSSettings,
            '</config:config-item-map-named>' +
          '</config:config-item-map-entry>' +
        '</config:config-item-map-indexed>' +
      '</config:config-item-set>' +
    '</office:settings>' +
   '</office:document-settings>');
end;

{ Writes the style node in "content.xml" as well as the conditional format
  part of a style to "styles.xml". }
procedure TsSpreadopenDocWriter.WriteStyleNode(AStream: TStream;
  const AStyleName: String; const AFormat: TsCellFormat;
  AConditionalFormatIndex: Integer);
var
  s: String;
  nfs: String;
  nfParams: TsNumFormatParams;
  nfIdx: Integer;
  j, p: Integer;
  addProtection: Boolean;
begin
  addProtection := (AConditionalFormatIndex = -1);

  nfs := WriteNumFormatStyleXMLAsString(AFormat);
  AppendToStream(AStream, Format(
    '<style:style style:name="%s" style:family="table-cell" ' +
                 'style:parent-style-name="Default"%s>',
    [AStyleName, nfs]
  ));

  // style:text-properties  --->  font
  s := WriteFontStyleXMLAsString(AFormat);
  if s <> '' then
    AppendToStream(AStream,
      '<style:text-properties '+ s + '/>');

  // - border, background, wordwrap, text rotation, vertical alignment
  s := WriteBorderStyleXMLAsString(AFormat) +
       WriteBackgroundColorStyleXMLAsString(AFormat) +
       WriteWordwrapStyleXMLAsString(AFormat) +
       WriteTextRotationStyleXMLAsString(AFormat) +
       WriteVertAlignmentStyleXMLAsString(AFormat);
  if addProtection then
    s := s +  WriteCellProtectionStyleXMLAsString(AFormat);
  if s <> '' then
    AppendToStream(AStream,
      '<style:table-cell-properties ' + s + '/>');

  // style:paragraph-properties  ---> hor alignment, bidi
  s := WriteHorAlignmentStyleXMLAsString(AFormat) +
       WriteBiDiModeStyleXMLAsString(AFormat);
  if s <> '' then
    AppendToStream(AStream,
      '<style:paragraph-properties ' + s + '/>');

  if (AConditionalFormatIndex > -1) then
  begin
    s := WriteConditionalStyleXMLAsString(AConditionalFormatIndex);
    if s <> '' then
      AppendToStream(AStream, s);
  end;

  AppendToStream(AStream,
    '</style:style>');
end;

{ Writes the file "styles.xml" }
procedure TsSpreadOpenDocWriter.WriteStyles;
begin
  AppendToStream(FSStyles,
     XML_HEADER);

  AppendToStream(FSStyles,
    '<office:document-styles xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
      '" xmlns:fo="' + SCHEMAS_XMLNS_FO +
      '" xmlns:style="' + SCHEMAS_XMLNS_STYLE +
      '" xmlns:svg="' + SCHEMAS_XMLNS_SVG +
      '" xmlns:table="' + SCHEMAS_XMLNS_TABLE +
      '" xmlns:text="' + SCHEMAS_XMLNS_TEXT +
      '" xmlns:xlink="' + SCHEMAS_XMLNS_XLINK +
      '" xmlns:draw="' + SCHEMAS_XMLNS_DRAW +
      '" xmlns:v="' + SCHEMAS_XMLNS_V + '">');

  AppendToStream(FSStyles,
      '<office:font-face-decls>');
  WriteFontNames(FSStyles);
  AppendToStream(FSStyles,
      '</office:font-face-decls>');

  WriteOfficeStyles(FSStyles);
  WriteAutomaticStyles(FSStyles);
  WriteMasterStyles(FSStyles);

  AppendToStream(FSStyles,
    '</office:document-styles>');
end;

procedure TsSpreadOpenDocWriter.WriteContent;
var
  i: Integer;
begin
  AppendToStream(FSContent,
    XML_HEADER);
  AppendToStream(FSContent,
    '<office:document-content ' +
      'xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" '+
      'xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" '+
      'xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" '+
      'xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" '+
      'xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" '+
      'xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" '+
      'xmlns:xlink="http://www.w3.org/1999/xlink" '+
      'xmlns:dc="http://purl.org/dc/elements/1.1/" '+
      'xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" '+
      'xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" '+
      'xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" '+
      'xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" '+
      'xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" '+
      'xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" '+
      'xmlns:math="http://www.w3.org/1998/Math/MathML" '+
      'xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" '+
      'xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" '+
      'xmlns:ooo="http://openoffice.org/2004/office" '+
      'xmlns:ooow="http://openoffice.org/2004/writer" '+
      'xmlns:oooc="http://openoffice.org/2004/calc" '+
      'xmlns:dom="http://www.w3.org/2001/xml-events" '+
      'xmlns:xforms="http://www.w3.org/2002/xforms" '+
      'xmlns:xsd="http://www.w3.org/2001/XMLSchema" '+
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '+
      'xmlns:rpt="http://openoffice.org/2005/report" '+
      'xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2" '+
      'xmlns:xhtml="http://www.w3.org/1999/xhtml" '+
      'xmlns:grddl="http://www.w3.org/2003/g/data-view#" '+
      'xmlns:tableooo="http://openoffice.org/2009/table" '+
      'xmlns:drawooo="http://openoffice.org/2010/draw" '+
      'xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0" '+
      'xmlns:loext="urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0" '+
      'xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0" '+
      'xmlns:formx="urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0" '+
      'xmlns:css3t="http://www.w3.org/TR/css3-text/" '+
      'xmlns:rdfa="http://docs.oasis-open.org/opendocument/meta/rdfa#" '+
      'office:version="1.2">' +
    '<office:scripts />'
  );

{
    '<office:document-content xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
        '" xmlns:fo="'     + SCHEMAS_XMLNS_FO +
        '" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/' +
        '" xmlns:style="'  + SCHEMAS_XMLNS_STYLE +
        '" xmlns:text="'   + SCHEMAS_XMLNS_TEXT +
        '" xmlns:table="'  + SCHEMAS_XMLNS_TABLE +
        '" xmlns:svg="'    + SCHEMAS_XMLNS_SVG +
        '" xmlns:number="' + SCHEMAS_XMLNS_NUMBER +
        '" xmlns:meta="'   + SCHEMAS_XMLNS_META +
        '" xmlns:chart="'  + SCHEMAS_XMLNS_CHART +
        '" xmlns:dr3d="'   + SCHEMAS_XMLNS_DR3D +
        '" xmlns:math="'   + SCHEMAS_XMLNS_MATH +
        '" xmlns:form="'   + SCHEMAS_XMLNS_FORM +
        '" xmlns:script="' + SCHEMAS_XMLNS_SCRIPT +
        '" xmlns:ooo="'    + SCHEMAS_XMLNS_OOO +
        '" xmlns:ooow="'   + SCHEMAS_XMLNS_OOOW +
        '" xmlns:oooc="'   + SCHEMAS_XMLNS_OOOC +
        '" xmlns:dom="'    + SCHEMAS_XMLNS_DOM +
        '" xmlns:xforms="' + SCHEMAS_XMLNS_XFORMS +
        '" xmlns:xsd="'    + SCHEMAS_XMLNS_XSD +
        '" xmlns:xsi="'    + SCHEMAS_XMLNS_XSI + '">' +
      '<office:scripts />');
}
  // Fonts
  AppendToStream(FSContent,
      '<office:font-face-decls>');
         WriteFontNames(FSContent);
  AppendToStream(FSContent,
      '</office:font-face-decls>');

  // Automatic styles
  AppendToStream(FSContent,
      '<office:automatic-styles>');

  WriteNumFormats(FSContent);        // "N1" ...
  WriteColStyles(FSContent);         // "co1" ...
  WriteRowStyles(FSContent);         // "ro1" ...
  WriteTableStyles(FSContent);       // "ta1" ...
  WriteCellStyles(FSContent);        // "ce1" ...
  WriteTextStyles(FSContent);        // "T1" ...

  AppendToStream(FSContent,
      '</office:automatic-styles>');

  // Body
  AppendToStream(FSContent,
      '<office:body>' +
        '<office:spreadsheet' + WriteDocumentProtectionXMLAsString + '>');

  // Write all worksheets
  for i := 0 to (Workbook as TsWorkbook).GetWorksheetCount - 1 do
    WriteWorksheet(FSContent, i);

  AppendToStream(FSContent,
        '</office:spreadsheet>' +
      '</office:body>' +
    '</office:document-content>'
  );
end;

procedure TsSpreadOpenDocWriter.WriteWorksheet(AStream: TStream;
  ASheetIndex: Integer);
begin
  FWorksheet := (FWorkbook as TsWorkbook).GetWorksheetByIndex(ASheetIndex);

  // Buffer the information whether the worksheet contains column or row formats
  // Needed for writing rows and cells
  FHasColFormats := (FWorksheet as TsWorksheet).HasColFormats;
  FHasRowFormats := (FWorksheet as TsWorksheet).HasRowFormats;

  // Header
  AppendToStream(AStream, Format(
    '<table:table table:name="%s" table:style-name="ta%d"%s%s>%s', [
    UTF8TextToXMLText(FWorkSheet.Name),
    ASheetIndex+1,
    WriteSheetProtectionXMLAsString(FWorksheet),
    WritePrintRangesXMLAsString(FWorksheet),
    WriteSheetProtectionDetailsXMLAsString(FWorksheet)
  ]));

  // shapes
  WriteShapes(AStream, FWorksheet);

  // columns
  WriteColumns(AStream, FWorkSheet);

  // rows and cells
  // The cells need to be written in order, row by row, cell by cell
  if (boVirtualMode in Workbook.Options) then
    WriteVirtualCells(AStream, FWorksheet)
  else
    WriteRowsAndCells(AStream, FWorksheet);

  // Conditional formats
  WriteConditionalFormats(AStream, FWorksheet);

  // named expressions, i.e. print range, repeated cols/rows
  WriteNamedExpressions(AStream, FWorksheet);

  // Footer
  AppendToStream(AStream,
    '</table:table>');
end;


{ Writes the cell styles ("ce0", "ce1", ...). Directly maps to the CellFormats
  list of the workbook. "ce0" is the default format }
procedure TsSpreadOpenDocWriter.WriteCellStyles(AStream: TStream);
var
  book: TsWorkbook;
  cf: TsConditionalFormat;
  cf_sheet: TsWorksheet;
  cf_range: TsCellRange;
  ncf: Integer;
  i: Integer;
  cell: PCell;
  r, c: Cardinal;
  L: TStrings;
  s: String;
  styleName: String;
  fmtIndex: Integer = 0;
  cfIndex: Integer = 0;
begin
  book := TsWorkbook(FWorkbook);

  // Write fixed formats only
  for i := 0 to book.GetNumCellFormats - 1 do
  begin
    styleName := 'ce' + IntToStr(i);
    WriteStyleNode(AStream, styleName, book.GetCellFormat(i), -1);
  end;

  // Conditional formats contain the fixed formats plus the condition params
  // To avoid duplicate style entries in the file we first collect all style
  // names in a list
  ncf := book.GetNumConditionalFormats;
  if ncf = 0 then
    exit;

  L := TStringList.Create;
  try
    for i := 0 to ncf - 1 do
    begin
      cf := book.GetConditionalFormat(i);
      cf_sheet := TsWorksheet(cf.Worksheet);
      cf_range := cf.Cellrange;
      for r := cf_range.Row1 to cf_range.Row2 do
        for c := cf_range.Col1 to cf_range.Col2 do
        begin
          cell := cf_sheet.FindCell(r, c);
          if Assigned(cell) and
            (cell^.ConditionalFormatIndex[High(cell^.ConditionalFormatIndex)] = i)
          then begin
            s := 'ce' + IntToStr((i+1) * 1000 + cell^.FormatIndex);
            // To distinguish conditional from fixed format numbers we increment
            // cfIndex by 1 to have thousands in the number even when cfIndex = 0.
            if L.IndexOf(s) = -1 then
              L.Add(s);
          end;
        end;
    end;

    // Now write the combined styles to the stream. The style names were stored
    // in the unique way in the string list; the format index can be extracted
    // from the style name.
    for i := 0 to L.Count-1 do begin
      styleName := L.Strings[i];
      s := Copy(L[i], 3, MaxInt);  // remove 'ce'
      DivMod(StrToInt(s), 1000, cfIndex, fmtIndex); // extract cfIndex and fmt Index from style name
      WriteStyleNode(AStream, styleName, book.GetCellFormat(fmtIndex), cfIndex-1);  // cfIndex was incremented by 1.
    end;
  finally
    L.Free;
  end;
end;

{ Writes the "office:automatic" > "style:style" node for table-column style family
  in content.xml for all column records. }
procedure TsSpreadOpenDocWriter.WriteColStyles(AStream: TStream);
var
  i: Integer;
  colstyle: TColumnStyleData;
begin
  if FColumnStyleList.Count = 0 then
  begin
    AppendToStream(AStream,
      '<style:style style:name="co1" style:family="table-column">',
        '<style:table-column-properties fo:break-before="auto" style:column-width="2.267cm"/>',
      '</style:style>');
    exit;
  end;

  for i := 0 to FColumnStyleList.Count-1 do
  begin
    colStyle := TColumnStyleData(FColumnStyleList[i]);

    // Start and Name
    AppendToStream(AStream, Format(
      '<style:style style:name="%s" style:family="table-column">', [colStyle.Name]));

    // Column width
    AppendToStream(AStream, Format(
        '<style:table-column-properties style:column-width="%.3fmm" fo:break-before="%s"/>', [
          colStyle.ColWidth,
          PAGE_BREAK[colStyle.PageBreak]
        ], FPointSeparatorSettings)
    );

    // End
    AppendToStream(AStream,
      '</style:style>');
  end;
end;

procedure TsSpreadOpenDocWriter.WriteColumns(AStream: TStream;
  ASheet: TsBasicWorksheet);
var
  sheet: TsWorksheet absolute ASheet;
  lastCol: Integer;
  c, k: Integer;
  w, w1: Double;
  styleName: String;
  colsRepeated: Integer;
  colsRepeatedStr: String;
  firstRepeatedPrintCol, lastRepeatedPrintCol: Longint;
  headerCols: Boolean;
  isHidden1, isHidden: Boolean;
  isPageBreak1, isPageBreak: Boolean;
  colHiddenStr: String;
  colStyleData: TColumnStyleData;
begin
  lastCol := sheet.GetLastColIndex;
  firstRepeatedPrintCol := longInt(sheet.PageLayout.RepeatedCols.FirstIndex);
  lastRepeatedPrintCol := longint(sheet.PageLayout.RepeatedCols.LastIndex);
  if (firstRepeatedPrintCol <> Longint(UNASSIGNED_ROW_COL_INDEX)) and
     (lastRepeatedPrintCol = LongInt(UNASSIGNED_ROW_COL_INDEX))
  then
    lastRepeatedPrintCol := firstRepeatedPrintCol;

  headerCols := false;
  c := 0;
  while (c <= lastCol) do
  begin
    w1 := sheet.GetColWidth(c, FWorkbook.Units);
    isHidden1 := sheet.ColHidden(c) or (w1 = 0);
    isPageBreak1 := sheet.IsPageBreakCol(c);

    if (c = firstRepeatedPrintCol) then
    begin
      headerCols := true;
      AppendToStream(AStream, '<table:table-header-columns>');
    end;

    // Find width in ColumnStyleList to retrieve corresponding style name
    styleName := '';
    for k := 0 to FColumnStyleList.Count-1 do begin
      colStyleData := TColumnStyleData(FColumnStyleList[k]);
      if SameValue(colStyleData.ColWidth, w1, COLWIDTH_EPS) and
         (colStyleData.PageBreak = isPageBreak1) then
      begin
        styleName := colStyleData.Name;
        break;
      end;
    end;
      {
      if SameValue(TColumnStyleData(FColumnStyleList[k]).ColWidth, w1, COLWIDTH_EPS) then begin
        styleName := TColumnStyleData(FColumnStyleList[k]).Name;
        break;
      end;
      }
    if stylename = '' then
      stylename := 'co1';
    {
    if stylename = '' then
      raise EFPSpreadsheet.Create(rsColumnStyleNotFound);
      }

    // Determine value for "number-columns-repeated"
    k := c+1;
    colsRepeated := 1;
    if headerCols then
      while (k <= lastCol) and (k <= lastRepeatedPrintCol) do
      begin
        w := sheet.GetColWidth(k, FWorkbook.Units);
        isHidden := sheet.ColHidden(k) or (w = 0);
        isPageBreak := sheet.IsPageBreakCol(k);
        if (w = w1) and (isHidden = isHidden1) and (isPageBreak = isPageBreak1) then
          inc(colsRepeated)
        else
          break;
        inc(k);
      end
    else
      while (k <= lastCol) and (k < firstRepeatedPrintCol) do
      begin
        w := sheet.GetColWidth(k, FWorkbook.Units);
        isHidden := sheet.ColHidden(k) or (w = 0);
        isPageBreak := sheet.IsPageBreakCol(k);
        if (w = w1) and (isHidden = isHidden1) and (isPageBreak = isPageBreak1) then
          inc(colsRepeated)
        else
          break;
        inc(k);
      end;
    if FHasRowFormats and (k = lastcol) then
      colsRepeated := FLimitations.MaxColCount - c;

    colsRepeatedStr := IfThen(colsRepeated > 1,
      Format(' table:number-columns-repeated="%d"', [colsRepeated]), '');
    colHiddenStr := IfThen(isHidden1, ' table:visibility="collapse"', '');

    AppendToStream(AStream, Format(
      '<table:table-column table:style-name="%s"%s table:default-cell-style-name="Default"%s />',
        [styleName, colsRepeatedStr, colHiddenStr]));

    if headerCols and (k >= lastRepeatedPrintCol) then
    begin
      AppendToStream(AStream, '</table:table-header-columns>');
      headerCols := false;
    end;

    c := c + colsRepeated;
  end;
end;

function TsSpreadOpenDocWriter.WriteCellProtectionStyleXMLAsString(
  const AFormat: TsCellFormat): String;
// style:cell-protect="protected formula-hidden"
begin
  if AFormat.Protection * [cpLockCell, cpHideFormulas] = [] then
    Result := 'none'
  else if (AFormat.Protection * [cpLockCell, cpHideFormulas] = [cpLockCell]) then
    Result := 'protected'
  else if (AFormat.Protection *[cpLockCell, cpHideFormulas] = [cpHideFormulas]) then
    Result := 'formula-hidden'
  else
    Result := 'protected formula-hidden';   // or:  'hidden-and-protected'
  Result := ' style:cell-protect="' + Result + '"';
end;

function TsSpreadOpenDocWriter.WriteCommentXMLAsString(AComment: String): String;
var
  L: TStringList;
  s: String;
  err: Boolean;
  i: Integer;
begin
  Result := '';
  if AComment = '' then exit;

  result := '<office:annotation office:display="false">';
  err := false;
  L := TStringList.Create;
  try
    L.Text := AComment;
    for i:=0 to L.Count-1 do begin
      s := L[i];
      if not ValidXMLText(s) then begin
        if not err then
          Workbook.AddErrorMsg(rsInvalidCharacterInCellComment, [AComment]);
        err := true;
      end;
      Result := Result + '<text:p>' + s + '</text:p>';
    end;
  finally
    L.Free;
  end;

  Result := Result + '</office:annotation>';
end;

{@@ ----------------------------------------------------------------------------
  Writes the "calcext:conditional-formats" node after the table block
  in "contents.xml". This is the third part needed for conditional formatting.
  The other part are implemented in
  #1 WriteConditionalStyles/WriteConditionalStyle
  #2 WriteCellStyle
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteConditionalFormats(AStream: TStream;
  ASheet: TsBasicWorksheet);
{<calcext:conditional-formats>
    <calcext:conditional-format calcext:target-range-address="Tabelle1.B4:Tabelle1.J4">
      <calcext:condition calcext:apply-style-name="cf" calcext:value="=5" calcext:base-cell-address="Tabelle1.B4" />
    </calcext:conditional-format>
  </calcext:conditional-formats> }
const
  VALUE_OR_DATE: array[boolean] of string = ('value', 'date');
var
  book: TsWorkbook;
  ncf: Integer;
  cf: TsConditionalFormat;
  cf_range: TsCellRange;
  cf_styleName: String;
  cf_cellRule: TsCFCellRule;
  cf_DataBarRule: TsCFDataBarRule;
  cf_ColorRangeRule: TsCFColorRangeRule;
  cf_IconSetRule: TsCFIconSetRule;
  i, j, k: Integer;
  sheet: TsWorksheet;
  rangeStr: String;
  firstCellStr: string;
  value1Str, value2Str: String;
  opStr: String;
  isDateFmt: Boolean;
begin
  book := TsWorkbook(FWorkbook);
  sheet := TsWorksheet(ASheet);
  ncf := book.GetNumConditionalFormats;

  AppendToStream(AStream,
    '<calcext:conditional-formats>');

  for i := 0 to ncf-1 do begin
    cf := book.GetConditionalFormat(i);
    if cf.Worksheet <> ASheet then
      continue;
    cf_range := cf.CellRange;
    firstCellStr := sheet.Name + '.' + GetCellString(cf_range.Row1, cf_range.Col1);
    rangeStr := firstCellStr + ':' + sheet.Name + '.' + GetCellString(cf_range.Row2, cf_range.Col2);

    AppendToStream(AStream, Format(
      '<calcext:conditional-format calcext:target-range-address="%s">', [
      rangeStr
    ]));

    for j := 0 to cf.RulesCount-1 do
    begin
      if cf.Rules[j] is TsCFCellRule then
      begin
        cf_cellRule := TsCFCellRule(cf.Rules[j]);
        cf_styleName := Format('conditional_%d', [cf_CellRule.FormatIndex]);
        value1Str := CFOperandToStr(cf_cellRule.Operand1, sheet);
        value2Str := CFOperandToStr(cf_cellRule.Operand2, sheet);
        opStr := Format(CF_CALCEXT_OP[cf_cellRule.Condition], [value1Str, value2str]);
        isDateFmt := cf_cellRule.Condition in [cfcYesterday..cfcNextYear];
        if opStr <> '' then
        begin
          if isDateFmt then
            AppendToStream(AStream, Format(
              '<calcext:date-is calcext:style="%s" calcext:date="%s" />',
              [cf_stylename, opStr]
            ))
          else
            AppendToStream(AStream, Format(
              '<calcext:condition calcext:apply-style-name="%s" calcext:value="%s" calcext:base-cell-address="%s" />',
              [cf_stylename, opStr, firstCellStr]
            ));
        end;
      end
      else
      if cf.Rules[j] is TsCFDatabarRule then
      begin
        cf_DatabarRule := TsCFDatabarRule(cf.Rules[j]);
        AppendToStream(AStream, Format(
          '<calcext:data-bar calcext:max-length="100" ' +
               'calcext:negative-color="%s" calcext:positive-color="%s" ' +
               'calcext:axis-color="#000000">' +
            '<calcext:formatting-entry calcext:value="%g" calcext:type="%s" />' +
            '<calcext:formatting-entry calcext:value="%g" calcext:type="%s" />' +
          '</calcext:data-bar>', [
          ColorToHTMLColorStr(cf_DatabarRule.Color), ColorToHTMLColorStr(cf_DatabarRule.Color),
          cf_DatabarRule.StartValue, CF_VALUE_KIND[cf_DatabarRule.StartValueKind],
          cf_DatabarRule.EndValue, CF_VALUE_KIND[cf_DatabarRule.EndValueKind]
        ]));
        // This is the default node after import from xlsx
      end
      else
      if cf.Rules[j] is TsCFColorRangeRule then
      begin
        cf_ColorRangeRule := TsCFColorRangeRule(cf.Rules[j]);
        if cf_ColorRangeRule.ThreeColors then
          AppendToStream(AStream, Format(
            '<calcext:color-scale>' +
              '<calcext:color-scale-entry calcext:value="%g" calcext:type="%s" calcext:color="%s" />' +
              '<calcext:color-scale-entry calcext:value="%g" calcext:type="%s" calcext:color="%s" />' +
              '<calcext:color-scale-entry calcext:value="%g" calcext:type="%s" calcext:color="%s" />' +
            '</calcext:color-scale>', [
            cf_ColorRangeRule.StartValue,
              CF_VALUE_KIND[cf_ColorRangeRule.StartValueKind],
              ColorToHTMLColorStr(cf_ColorRangeRule.StartColor),
            cf_ColorRangeRule.CenterValue,
              CF_VALUE_KIND[cf_ColorRangeRule.CenterValueKind],
              ColorToHTMLColorStr(cf_ColorRangeRule.CenterColor),
            cf_ColorRangeRule.EndValue,
              CF_VALUE_KIND[cf_ColorRangeRule.EndValueKind],
              ColorToHTMLColorStr(cf_ColorRangeRule.EndColor)
          ]))
        else
          AppendToStream(AStream, Format(
            '<calcext:color-scale>' +
              '<calcext:color-scale-entry calcext:value="%g" calcext:type="%s" calcext:color="%s" />' +
              '<calcext:color-scale-entry calcext:value="%g" calcext:type="%s" calcext:color="%s" />' +
            '</calcext:color-scale>', [
            cf_ColorRangeRule.StartValue,
              CF_VALUE_KIND[cf_ColorRangeRule.StartValueKind],
              ColorToHTMLColorStr(cf_ColorRangeRule.StartColor),
            cf_ColorRangeRule.EndValue,
              CF_VALUE_KIND[cf_ColorRangeRule.EndValueKind],
              ColorToHTMLColorStr(cf_ColorRangeRule.EndColor)
          ]));
      end else
      if cf.Rules[j] is TsCFIconSetRule then
      begin
        cf_IconSetRule := TsCFIconSetRule(cf.Rules[j]);
        AppendToStream(AStream, Format(
          '<calcext:icon-set calcext:icon-set-type="%s">', [
            CF_ICON_SET[cf_IconSetRule.IconSet]
          ]));
        AppendToStream(AStream,
            '<calcext:formatting-entry calcext:value="0" calcext:type="percent" />');
        for k := 0 to cf_IconSetRule.IconCount-2 do
          AppendToStream(AStream, Format(
            '<calcext:formatting-entry calcext:value="%g" calcext:type="%s" />', [
            cf_IconSetRule.Values[k],
            CF_VALUE_KIND[cf_IconSetRule.ValueKinds[k]]
          ]));
        AppendToStream(AStream,
          '</calcext:icon-set>');
      end;
    end;

    AppendToStream(AStream,
      '</calcext:conditional-format>');
  end;
  AppendToStream(AStream,
    '</calcext:conditional-formats>' );
end;

{@@ ----------------------------------------------------------------------------
  Writes the styles used by conditional formatting to "styles.xml".
  In total there are four parts which must be implemented
  for condtional formatting:
  #1 Definition of the styles (here, and in WriteStyleNode) (can be omitted if
     one of the already existing styles is used)
  #2 Definition of the cell styles in contents.xml
     (in WriteCellStyles), style:map nodes)
  #3 Definition of the cell ranges in WriteConditionalFormats in content.xml
     (calcext:conditional-formattings node)
  #4 Find the correct style when cells are written in content.xml
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteConditionalStyles(AStream: TStream);
var
  book: TsWorkbook;
  i, j: Integer;
  nCF: Integer;
  CF: TsConditionalFormat;
  fmt: TsCellFormat;
  fmtIndex: Integer;
  cf_rule: TsCFRule;
  stylename: String;
  L: TStrings;
begin
  book := TsWorkbook(FWorkbook);
  nCF := book.GetNumConditionalFormats;

  L := TStringList.Create;
  try
    for i := 0 to nCF-1 do
    begin
      CF := book.GetConditionalFormat(i);
      for j := 0 to CF.RulesCount-1 do
      begin
        cf_Rule := CF.Rules[j];
        if cf_Rule is TsCFCellRule then
        begin
          fmtIndex := TsCFCellRule(cf_Rule).FormatIndex;
          fmt := book.GetCellFormat(TsCFCellRule(cf_Rule).FormatIndex);
          stylename := Format('conditional_%d', [fmtIndex]);
          if L.IndexOf(styleName) = -1 then begin
            WriteStyleNode(AStream, stylename, fmt, i);
            L.Add(styleName);
          end;
        end;
      end;
    end;
  finally
    L.Free;
  end;
end;


{@@ ----------------------------------------------------------------------------
  Writes the declaration of the font faces used in the workbook.
  Is used in styles.xml and content.xml.

  Procedure must be enclosed by
    <office:font-face-decls> ... </office:font-face-decls>
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteFontNames(AStream: TStream);
var
  L: TStringList;
  fnt: TsFont;
  hfFnt: TXMLHeaderFooterFont;
  i: Integer;
begin
  // Collect all unique font names in a string list
  L := TStringList.Create;
  try
    // First collect the font names from the workbook's FontList
    for i:=0 to TsWorkbook(Workbook).GetFontCount-1 do
    begin
      fnt := TsWorkbook(Workbook).GetFont(i);
      if (fnt <> nil) and (L.IndexOf(fnt.FontName) = -1) then
        L.Add(fnt.FontName);
    end;
    // Then collect the header/footer font names from the HeaderFooterFontList
    for i:=0 to FHeaderFooterFontList.Count-1 do
    begin
      hfFnt := TXMLHeaderFooterFont(FHeaderFooterFontList[i]);
      if (hfFnt <> nil) and (L.Indexof(hfFnt.FontName) = -1) then
        L.Add(hfFnt.FontName);
    end;
    // Done. Now write all font names as xml nodes to the stream
    for i:=0 to L.Count-1 do
      AppendToStream(AStream, Format(
        '<style:font-face style:name="%s" svg:font-family="%s" />', [L[i], L[i]]));
  finally
    L.Free;
  end;
end;

procedure TsSpreadOpenDocWriter.WriteMasterStyles(AStream: TStream);
var
  defFnt: TsHeaderFooterFont;
  i: Integer;
  sheet: TsWorksheet;

  function HeaderFooterAsString(AIndex: Integer; AIsHeader: Boolean;
    const APageLayout: TsPageLayout): String;
  var
    parser: TsSpreadOpenDocHeaderFooterParser;
    str: String;
  begin
    if AIsHeader then
      str := APageLayout.Headers[AIndex] else
      str := APageLayout.Footers[AIndex];
    if str = '' then
      exit;
    parser := TsSpreadOpenDocHeaderFooterParser.Create(str, FHeaderFooterFontList,
      defFnt);
    try
      Result := parser.BuildHeaderFooterAsXMLString;
    finally
      parser.Free;
    end;
  end;

  function MasterPageAsString(AStyleName, ADisplayName, APageLayoutName: String;
    const APageLayout: TsPageLayout): String;
  const
    IS_HEADER = true;
    IS_FOOTER = false;
  begin
    Result := Format(
      '<style:master-page style:name="%s" ' +
                        'style:display-name="%s" ' +
                        'style:page-layout-name="%s">', [
      AStyleName, ADisplayName, APageLayoutName
    ]);

    if (APageLayout.Headers[1] <> '') then
      Result := Result +
        '<style:header>' +
          HeaderFooterAsString(1, IS_HEADER, APageLayout) +
        '</style:header>'
    else
      Result := Result +
        '<style:header style:display="false" />';

    if (APageLayout.Footers[1] <> '') then
      Result := Result +
        '<style:footer>' +
          HeaderFooterAsString(1, IS_FOOTER, APageLayout) +
        '</style:footer>'
    else
      Result := Result +
        '<style:footer style:display="false" />';

    if poDifferentOddEven in APageLayout.Options then
    begin
      if (APageLayout.Headers[2] <> '') then
        Result := Result +
          '<style:header-left>' +
            HeaderFooterAsString(2, IS_HEADER, APageLayout) +
          '</style:header-left>'
      else
        Result := Result +
          '<style:header-left style:display="false" />';

      if (APageLayout.Footers[2] <> '') then
        Result := Result +
          '<style:footer-left>' +
            HeaderFooterAsString(2, IS_FOOTER, APageLayout) +
          '</style:footer-left>'
      else
        Result := Result +
          '<style:footer-left display="false" />';
    end;
    Result := Result + '</style:master-page>';
  end;

var
  sheetname: String;
begin
  defFnt := TsHeaderFooterFont.Create((Workbook as TsWorkbook).GetDefaultFont);

  AppendToStream(AStream,
    '<office:master-styles>');

  AppendToStream(AStream,
      '<style:master-page style:name="Default" style:page-layout-name="Mpm1">' +
        '<style:header />' +
        '<style:header-left style:display="false" />' +
        '<style:footer />' +
        '<style:footer-left style:display="false" />' +
      '</style:master-page>');

  for i:=0 to (FWorkbook as TsWorkbook).GetWorksheetCount-1 do begin
    sheet := (FWorkbook as TsWorkbook).GetWorksheetByIndex(i);
    sheetname := UTF8TextToXMLText(sheet.name);
    AppendToStream(AStream,
      MasterPageAsString('PageStyle_5f_' + sheetName, 'PageStyle_' + sheetname,
        'Mpm' + IntToStr(3+i), sheet.PageLayout));
  end;

  AppendToStream(AStream,
    '</office:master-styles>');

  defFnt.Free;
end;

{<table:named-expressions>
   <table:named-expression table:name="_xlnm.Print_Area" table:base-cell-address="$Sheet1.$A$1" table:expression="[$Sheet1.$A$2:.$F$6];[$Sheet1.$A$11:.$K$21]" />
   <table:named-expression table:name="_xlnm.Print_Titles" table:base-cell-address="$Sheet1.$A$1" table:expression="[$Sheet1.$A$1:.$D$1048576];[$Sheet1.$A$1:.$AMJ$2]" />
 </table:named-expressions>}

procedure TsSpreadOpenDocWriter.WriteNamedExpressions(AStream: TStream;
  ASheet: TsBasicWorksheet);
var
  stotal, srng, sheetname: String;
  j: Integer;
  prng: TsCellRange;
  sheet: TsWorksheet absolute ASheet;
begin
  sheetname := UTF8TextToXMLText(ASheet.Name);
  stotal := '';

  // Cell block of print range
  srng := '';
  for j := 0 to sheet.PageLayout.NumPrintRanges - 1 do
  begin
    prng := sheet.PageLayout.PrintRange[j];
    srng := srng + ';' + Format('[$%s.%s]', [
      sheetname, GetCellRangeString(prng.Row1, prng.Col1, prng.Row2, prng.Col2, [])
    ]);
  end;
  if srng <> '' then
  begin
    Delete(srng, 1, 1);
    stotal := stotal + Format(
      '<table:named-expression table:name="_xlnm.Print_Area" table:base-cell-address="$%s.$A$1" table:expression="%s" />',
      [sheetname, srng]
    );
  end;

  // Next commented part appears only in files converted from Excel

  {
  // repeated columns ...
  srng := '';
  if sheet.PageLayout.RepeatedCols.FirstIndex <> UNASSIGNED_ROW_COL_INDEX then
  begin
    if sheet.PageLayout.RepeatedCols.LastIndex = UNASSIGNED_ROW_COL_INDEX then
      srng := srng + ';' + Format('[$%s.$%s]',
        [sheet.Name, GetColString(sheet.pageLayout.RepeatedCols.FirstIndex)]
      )
    else
      srng := srng + ';' + Format('[$%s.$%s1:.$%s1048576]', [      // [$Sheet1.$A$1:.$D$1048576]
        sheet.Name,
        GetColString(sheet.Pagelayout.RepeatedCols.FirstIndex),
        GetColString(sheet.PageLayout.RepeatedCols.LastIndex)
      ]);
  end;
  // ... and repeated rows
  if sheet.PageLayout.RepeatedRows.FirstIndex <> UNASSIGNED_ROW_COL_INDEX then
  begin
    if sheet.PageLayout.RepeatedRows.LastIndex = UNASSIGNED_ROW_COL_INDEX then
      srng := srng + ';' + Format('[$%s.$%d]',
        [sheet.Name, sheet.pageLayout.RepeatedRows.FirstIndex]
      )
    else
      srng := srng + ';' + Format('[$%s.$A$%d:.$AMJ$%d]', [              // [$Sheet1.$A$1:.$AMJ$2]"
        sheet.Name,
        sheet.Pagelayout.RepeatedRows.FirstIndex+1,
        sheet.PageLayout.RepeatedRows.LastIndex+1
      ]);
  end;
  if srng <> '' then begin
    Delete(srng, 1,1);
    stotal := stotal + Format(
      '<table:named-expression table:name="_xlnm.Print_Titles" table:bases-cell-address="$%s.$A$1" table:expression="%s" />',
      [sheet.Name, srng]
    );
  end;
   }
  // Write to stream if any defined names exist
  if stotal <> '' then
    AppendtoStream(AStream,
      '<table:named-expressions>' + stotal + '</table:named-expressions>');
end;

procedure TsSpreadOpenDocWriter.WriteNumFormats(AStream: TStream);
var
  i, p: Integer;
  numFmtXML: String;
  numFmtStr: String;
  numFmtName: String;
  parser: TsSpreadOpenDocNumFormatParser;
begin
  for i:=0 to NumFormatList.Count-1 do
  begin
    numFmtStr := NumFormatList[i];
    p := pos(':', numFmtStr);
    numFmtName := Copy(numFmtStr, 1, p-1);
    numFmtStr := Copy(numFmtStr, p+1, Length(numFmtStr));
    parser := TsSpreadOpenDocNumFormatParser.Create(numFmtStr, Workbook.FormatSettings);
    try
      numFmtXML := parser.BuildXMLAsString(numFmtName);
      if numFmtXML <> '' then
        AppendToStream(AStream, numFmtXML);
    finally
      parser.Free;
    end;
  end;
end;

{ Writes the node <office:style> which is in "styles.xml" }
procedure TsSpreadOpenDocWriter.WriteOfficeStyles(AStream: TStream);
begin
  AppendToStream(AStream,
    '<office:styles>');

  {   --- causes trouble with empty columns
  AppendToStream(AStream,
      '<style:default-style style:family="table-cell">' +
        '<style:paragraph-properties style:tab-stop-distance="1.25cm" />' +
        WriteDefaultFontXMLAsString +
//        '<style:text-properties style:font-name="Liberation Sans" />' +
      '</style:default-style>');
  }

  AppendToStream(AStream,
      '<style:style style:name="Default" style:famile="table-cell" />');

  AppendToStream(AStream,
      '<style:style style:name="Default" style:family="table-cell">',
        WriteDefaultFontXMLAsString,
      '</style:style>');

  WriteConditionalStyles(AStream);

  if (FWorkbook as TsWorkbook).HasEmbeddedSheetImages then
    AppendToStream(AStream,
      '<style:default-style style:family="graphic">',
         WriteDefaultGraphicStyleXMLAsString,
      '</style:default-style>');

  AppendToStream(AStream,
    '</office:styles>');

end;

procedure TsSpreadOpenDocWriter.WriteRowsAndCells(AStream: TStream;
  ASheet: TsBasicWorksheet);
var
  r: Integer;
  rowsRepeated: Integer;
  firstCol, firstRow, lastCol, lastRow: Cardinal;
  firstRepeatedPrintRow, lastRepeatedPrintRow: Integer;
  headerRows: Boolean;
  sheet: TsWorksheet;
begin
  sheet := (ASheet as TsWorksheet);

  // some abbreviations...
  GetSheetDimensions(ASheet, firstRow, lastRow, firstCol, lastCol);

  headerRows := false;
  firstRepeatedPrintRow := Integer(sheet.PageLayout.RepeatedRows.FirstIndex);
  lastRepeatedPrintRow := Integer(sheet.PageLayout.RepeatedRows.LastIndex);
  if (firstRepeatedPrintRow <> Integer(UNASSIGNED_ROW_COL_INDEX)) and
     (lastRepeatedPrintRow = Integer(UNASSIGNED_ROW_COL_INDEX))
  then
    lastRepeatedPrintRow := firstRepeatedPrintRow;

  r := 0;
  while r <= Integer(lastRow) do
  begin
    if (r = firstRepeatedPrintRow) then begin
      AppendToStream(AStream, '<table:table-header-rows>');
      headerRows := true;
    end;

    // Write rows
    if sheet.IsEmptyRow(r) then
      WriteEmptyRow(AStream, ASheet, r, firstCol, lastCol, lastRow, rowsRepeated)
    else begin
      WriteCellRow(AStream, ASheet, r, lastCol);
      rowsRepeated := 1;
    end;
    r := r + rowsRepeated;

    // Header rows need a special tag
    if headerRows and (r > lastRepeatedPrintRow) then
    begin
      AppendToStream(AStream, '</table:table-header-rows>');
      headerRows := false;
    end;
  end;

  // Finally, if the sheet contains column formats an empty row has to be
  // added which is repeated up to the max worksheet size.
  if FHasColFormats then
    WriteEmptyRow(AStream, ASheet, r, firstCol, lastCol, -1, rowsRepeated);
end;

                       (*
  // Now loop through all rows
  r := firstRow;
  while (r <= lastRow) do
  begin
    rowsRepeated := 1;

    // Header rows need a special tag
    if (r = firstRepeatedPrintRow) then
    begin
      AppendToStream(AStream, '<table:table-header-rows>');
      headerRows := true;
    end;

    // Look for the row style of the current row (r): row style contains only
    // row height, no row format!
    row := ASheet.FindRow(r);
    styleName := '';
    if row <> nil then
    begin
      h := row^.Height;    // row height in workbook units
      for k := 0 to FRowStyleList.Count-1 do begin
        rowStyleData := TRowStyleData(FRowStyleList[k]);
        // Compare row heights, but be aware of rounding errors
        if SameValue(rowStyleData.RowHeight, h, ROWHEIGHT_EPS) and
           (rowstyleData.RowHeightType = row^.RowHeightType) and
           (rowstyleData.RowHeightType <> rhtDefault)
        then begin
          styleName := rowStyleData.Name;
          break;
        end;
      end;
    end;
    if styleName = '' then begin
      styleName := 'ro1';   // "ro1" is default row record - see ListAllRowStyles
      h := ASheet.ReadDefaultRowHeight(FWorkbook.Units);
    end;

    // Take care of empty rows above the first row with cells
    if (r = firstRow) and emptyRowsAbove then
    begin
      rowsRepeated := r;
      rowsRepeatedStr := IfThen(rowsRepeated = 1, '',
        Format('table:number-rows-repeated="%d"', [rowsRepeated]));
      if FHasRowFormats then
        colsRepeated := FLimitations.MaxColCount else
        colsRepeated := lastCol + 1;
      colsRepeatedStr := IfThen(colsRepeated = 1, '',
        Format('table:number-columns-repeated="%d"', [colsRepeated]));
      AppendToStream(AStream, Format(
        '<table:table-row table:style-name="%s" %s>' +
          '<table:table-cell %s/>' +
        '</table:table-row>',
        [styleName, rowsRepeatedStr, colsRepeatedStr]));
      rowsRepeated := 1;
    end
    else
    // Look for empty rows with the same style, they need the "number-rows-repeated" element.
    if (ASheet.Cells.GetFirstCellOfRow(r) = nil) then
    begin
      rr := r + 1;
      while (rr <= lastRow) do
      begin
        if ASheet.Cells.GetFirstCellOfRow(rr) <> nil then
          break;
        h1 := ASheet.GetRowHeight(rr, FWorkbook.Units);
        if not SameValue(h, h1, ROWHEIGHT_EPS) then
          break;
        inc(rr);
      end;
      rowsRepeated := rr - r;
      rowsRepeatedStr := IfThen(rowsRepeated = 1, '',
        Format('table:number-rows-repeated="%d"', [rowsRepeated]));
      if FHasRowFormats then
        colsRepeated := FLimitations.MaxColCount else
        colsRepeated := lastCol - firstCol + 1;
      colsRepeatedStr := IfThen(colsRepeated = 1, '',
        Format('table:number-columns-repeated="%d"', [colsRepeated]));

      AppendToStream(AStream, Format(
        '<table:table-row table:style-name="%s" %s>' +
          '<table:table-cell %s/>' +
        '</table:table-row>',
        [styleName, rowsRepeatedStr, colsRepeatedStr]));

      r := rr;

      // Header rows need a special tag
      if headerRows and (r >= lastRepeatedPrintRow) then
      begin
        AppendToStream(AStream, '</table:table-header-rows>');
        headerRows := false;
      end;

      continue;
    end;

    // Now we know that there are cells.
    // Write the row XML
    AppendToStream(AStream, Format(
        '<table:table-row table:style-name="%s">', [styleName]));

    // Loop along the row and find the cells.
    c := 0;
    while c <= lastCol do
    begin
      // Get the cell from the sheet
      cell := ASheet.FindCell(r, c);

      // Belongs to merged block?
      if (cell <> nil) and not FWorksheet.IsMergeBase(cell) and FWorksheet.IsMerged(cell) then
      // this means: all cells of a merged block except for the merge base
      begin
        AppendToStream(AStream,
          '<table:covered-table-cell />');
        inc(c);
        continue;
      end;

      colsRepeated := 1;
      if cell <> nil then
        WriteCellToStream(AStream, cell)
      else
      begin
        row := ASheet.FindRow(r);
        col := ASheet.FindCol(c);
        // Empty cell with column format
        if (col <> nil) and (col^.FormatIndex > 0) and
           ((row = nil) or (row^.FormatIndex = 0))
        then
          AppendToStream(AStream, Format(
            '<table:table-cell table:style-name="ce%d" />',
            [col^.FormatIndex]))
        else
        begin
          // Empty cell? Need to count how often to add "table:number-columns-repeated"
          cc := c + 1;
          while (cc <= lastCol) do
          begin
            col := nil;
            cell := ASheet.FindCell(r, cc);
            if cell <> nil then
              break;
            if (row = nil) or (row^.FormatIndex = 0) then
            begin
              col := ASheet.FindCol(cc);
              if (col <> nil) and (col^.FormatIndex > 0) then
                break;
            end;
            inc(cc)
          end;
          if FHasRowFormats and (cc > lastcol) then
            colsRepeated := FLimitations.MaxColCount - c
          else
            colsRepeated := cc - c;
          colsRepeatedStr := IfThen(colsRepeated = 1, '',
            Format(' table:number-columns-repeated="%d"', [colsRepeated]));
          row := ASheet.FindRow(r);
          if (row <> nil) and (row^.FormatIndex > 0) then
            stylename := Format(' table:style-name="ce%d"', [row^.FormatIndex]) else
            stylename := '';
          AppendToStream(AStream, Format(
            '<table:table-cell%s%s />', [colsRepeatedStr, stylename]));
          if (col <> nil) then //and ((row = nil) or (row^.FormatIndex = 0)) then
          begin
            AppendToStream(AStream, Format(
              '<table:table-cell table:style-name="ce%d" />', [col^.FormatIndex]));
          end;
          if (col <> nil) and (cc = lastcol) then
            break;
        end;
      end;
      inc(c, colsRepeated);
    end;

    AppendToStream(AStream,
        '</table:table-row>');

    // Header rows need a special tag
    if headerRows and (r = lastRepeatedPrintRow) then
    begin
      AppendToStream(AStream, '</table:table-header-rows>');
      headerRows := false;
    end;

    // Next row
    inc(r, rowsRepeated);
  end;

  // Finally, if the sheet contains column formats an empty row has to be
  // added which is repeated up to the max worksheet size.
  if FHasColFormats then begin
    k := 0;
    c := 0;
    cellStr := '';
    while k < ASheet.Cols.Count do begin
      col := PCol(ASheet.Cols[k]);
      if col^.FormatIndex > 0 then
      begin
        colsRepeated := col^.Col - c;
        if colsRepeated > 0 then begin
          cellStr := cellStr + Format(
            '<table:table-cell table:number-columns-repeated="%d" />',
            [colsRepeated]);
        end;
        cellStr := cellStr + Format(
          '<table:table-cell table:style-name="ce%d" />',
            [col^.FormatIndex]);
        c := col^.Col + 1;
      end;
      inc(k);
    end;

    colsRepeated := IfThen(FHasRowFormats, FLimitations.MaxColCount, lastcol) - c;
    if colsRepeated > 0 then
      cellStr := cellStr + Format(
        '<table:table-cell table:number-columns-repeated="%d" />',
        [colsRepeated]);

    rowsRepeated := FLimitations.MaxRowCount - r;
    AppendToStream(AStream, Format(
      '<table:table-row table:style-name="ro1" table:number-rows-repeated="%d">' +
        '%s' +
      '</table:table-row>', [
      rowsRepeated,
      cellStr
    ]));
  end;
end;
*)

procedure TsSpreadOpenDocWriter.WriteCellRow(AStream: TStream;
  ASheet: TsBasicWorksheet; ARowIndex, ALastColIndex: Integer);
var
  row: PRow;
  col: PCol;
  cell: PCell;
  stylename: string;
  firstcol: Integer;
  lastcol: Integer;
  c, cc: integer;
  colsRepeated: Integer;
  fmtIndex: integer;
  sheet: TsWorksheet absolute ASheet;
  rowHiddenStr: String;
  styleIdx: Integer;
  rowStyleData: TRowStyleData;
begin
  // Get row
  row := sheet.FindRow(ARowIndex);

  // Get row style
  styleIdx := FindRowStyle(ASheet, ARowIndex);
  if styleIdx = -1 then
  begin
    stylename := 'ro1';   // Default row style - see ListAllRowStyles
    rowHiddenStr := '';
  end else
  begin
    rowStyleData := TRowStyleData(FRowStyleList[styleIdx]);
    styleName := rowStyleData.Name;
    if (croHidden in row^.Options) or (
       (round(rowStyleData.RowHeight) = 0) and (rowStyleData.RowHeightType = rhtCustom))
    then
      rowHiddenStr := ' table:visibility="collapse"'
  end;


{
  // Get style and height of row
  GetRowStyleAndHeight(ASheet, ARowIndex, stylename, h);

  // Row hidden?
  if (round(h) = 0) or (Assigned(row) and (croHidden in row^.Options)) then
    rowHiddenStr := ' table:visibility="collapse"'
  else
    rowHiddenStr := '';
                   }
  // Write opening row tag. We don't support repeatedRows here.
  AppendToStream(AStream, Format(
    '<table:table-row table:style-name="%s"%s>', [stylename, rowHiddenStr]));

  // Find first cell or column in this row
  cell := sheet.Cells.GetFirstCellOfRow(ARowIndex);  // first cell
  col := sheet.FindFirstCol;   // left-most column
  if col <> nil then
    firstcol := Min(col^.Col, cell^.Col) else
    firstcol := cell^.Col;

  // Find last cell or column in this row
  cell := sheet.Cells.GetlastCellOfRow(ARowIndex);
  if sheet.Cols.Count = 0 then
    lastCol := cell^.Col
  else begin
    col := sheet.Cols[sheet.Cols.Count-1];
    if col <> nil then
      lastcol := Max(col^.Col, cell^.Col) else
      lastCol := cell^.Col;
  end;

  // Cells left to the first col are "empty" with default format
  if firstcol > 0 then
    AppendToStream(AStream, Format(
      '<table:table-cell table:number-columns-repeated="%d" />', [firstcol]));

  // Iterate between first and last column
  c := firstcol;
  while (c <= lastcol) do
  begin
    cell := sheet.FindCell(ARowIndex, c);
    if cell <> nil then
    begin
      // Belongs to merged block?
      if not sheet.IsMergeBase(cell) and sheet.IsMerged(cell) then
      // this means: all cells of a merged block except for the merge base
      begin
        AppendToStream(AStream,
          '<table:covered-table-cell />');
        inc(c);
        continue;
      end;
      // Ordinary cell
      WriteCellToStream(AStream, cell);
      inc(c);
      Continue;
    end;

    // Column format
    col := sheet.FindCol(c);
    if (col <> nil) and (col^.FormatIndex > 0) then
    begin
      // row format has priority...
      if (row <> nil) and (row^.FormatIndex > 0) then
        fmtIndex := row^.FormatIndex else
        fmtIndex := col^.FormatIndex;
      AppendToStream(AStream, Format(
        '<table:table-cell table:style-name="ce%d" />', [fmtIndex]));
      inc(c);
      Continue;
    end;

    // Empty cell
    cc := c + 1;
    while (cc <= lastcol) do begin
      cell := sheet.FindCell(ARowIndex, cc);
      if cell <> nil then
        break;
      col := sheet.FindCol(cc);
      if (col <> nil) and (col^.FormatIndex > 0) then
        break;
      inc(cc);
    end;
    colsRepeated := cc - c;
    // Empty cell with row format?
    if (row <> nil) and (row^.FormatIndex > 0) then
      AppendToStream(AStream, Format(
        '<table:table-cell table:style-name="ce%d" table:number-columns-repeated="%d" />',
        [row^.FormatIndex, colsRepeated]))
    else
      AppendToStream(AStream, Format(
        '<table:table-cell table:number-columns-repeated="%d" />',
        [colsRepeated]));
    inc(c, colsRepeated);
  end;

  // Fill empty cells at right, in case of RowFormats up to limit of format.
  if FHasRowFormats then
    colsRepeated := FLimitations.MaxColCount - c
  else if c <= ALastColIndex then
    colsRepeated := ALastColIndex - c
  else
    colsRepeated := 0;
  if colsRepeated > 0 then
  begin
    if (row <> nil) and (row^.FormatIndex > 0) then
      AppendToStream(AStream, Format(
        '<table:table-cell table:style-name="ce%d" table:number-columns-repeated="%d" />',
        [row^.FormatIndex, colsRepeated]))
    else
      AppendToStream(AStream, Format(
        '<table:table-cell table:number-columns-repeated="%d" />',
        [colsRepeated]));
  end;

  // Write closing row tag.
  AppendToStream(AStream,
    '</table:table-row>');
end;

{ Writes a complete row node for the specified row of the worksheet. Correctly
  handles row and column formats.
  If ALastRowIndex = -1 then the filler rows below the used sheet are written }
procedure TsSpreadOpenDocWriter.WriteEmptyRow(AStream: TStream;
  ASheet: TsBasicWorksheet;
  ARowIndex, AFirstColIndex, ALastColIndex, ALastRowIndex: Integer;
  out ARowsRepeated: Integer);
var
  row: PRow;
  col: PCol;
  c, cc, r: Integer;
  colsRepeated: Integer;
  stylename: String;
  h, h1: Single;
  fmtIndex: Integer;
  sheet: TsWorksheet absolute ASheet;
  rowsRepeatedStr: String;
  rowHiddenStr: String;
  isHidden1, isHidden: Boolean;
  isPageBreak1, isPageBreak: Boolean;
  styleIdx: Integer;
  rowStyleData: TRowStyleData;
begin
  // Get row style
  styleIdx := FindRowStyle(ASheet, ARowIndex);
  if styleIdx = -1 then
  begin
    stylename := 'ro1';   // Default row style - see ListAllRowStyles
    h := -1;
  end else
  begin
    rowStyleData := TRowStyleData(FRowStyleList[styleIdx]);
    styleName := rowStyleData.Name;
    if rowStyleData.RowHeightType = rhtCustom then
      h := rowStyleData.RowHeight
    else
      h := -1;
    {
    if (croHidden in row^.Options) or (
       (round(rowStyleData.RowHeight) = 0) and (rowStyleData.RowHeightType = rhtCustom))
    then
      rowHiddenStr := ' table:visibility="collapse"'
      }
  end;
     {

  // Get style and height of row
  GetRowStyleAndHeight(ASheet, ARowIndex, stylename, h);
      }

  // Determine how often this row is repeated
  row := sheet.FindRow(ARowIndex);
  if Assigned(row) then begin
    isPageBreak1 := (croPageBreak in row^.Options);
    isHidden1 := (round(h) = 0) or ((row <> nil) and (croHidden in row^.Options));
  end else begin
    isPageBreak1 := false;
    isHidden1 := false;
  end;
  rowHiddenStr := IfThen(isHidden1, ' table:visibility="collapse"', '');

  // Rows with format are not repeated - too complicated...
  if (row <> nil) and (row^.FormatIndex > 0) then
    ARowsRepeated := 1
  else
  // Count how many rows are empty and have the row record values
  if ALastRowIndex > -1 then begin
    r := ARowIndex + 1;
    while r <= ALastRowIndex do
    begin
      if not sheet.IsEmptyRow(r) then
        break;
      row := sheet.FindRow(r);
      isPageBreak := (row <> nil) and (croPageBreak in row^.Options);
      isHidden := (row <> nil) and
        ((croHidden in row^.Options) or ((row^.RowHeightType=rhtCustom) and (row^.Height = 0)));
      if ((row <> nil) and (row^.FormatIndex > 0)) or
         (isHidden <> isHidden1) or
         (isPageBreak <> isPageBreak1)
      then
        break;
      h1 := sheet.GetRowHeight(r, FWorkbook.Units);
      if not SameValue(h, h1, ROWHEIGHT_EPS) then
        break;
      inc(r);
    end;
    ARowsRepeated := r - ARowIndex;
  end else
    ARowsRepeated := FLimitations.MaxRowCount - ARowIndex;
  rowsRepeatedStr := IfThen(ARowsRepeated > 1,
    Format(' table:number-rows-repeated="%d"', [ARowsRepeated]), '');

  // Write opening row tag
  AppendToStream(AStream, Format(
    '<table:table-row table:style-name="%s"%s%s>',
      [stylename, rowsRepeatedStr, rowHiddenStr]));

  // Empty cells left of the first column
  colsRepeated := AFirstColIndex;
  if colsRepeated > 0 then
    AppendToStream(AStream, Format(
      '<table:table-cell table:number-columns-repeated="%d" />', [colsRepeated]));

  // Cells between first and last columns
  r := ARowIndex;
  c := AFirstColIndex;

  row := sheet.FindRow(r);
  while (c <= ALastColIndex) do
  begin
    // Empty cell in a column with a column format
    col := sheet.FindCol(c);
    if (col <> nil) and (col^.FormatIndex > 0) then
    begin
      if (row <> nil) and (row^.FormatIndex > 0) then
        fmtIndex := row^.FormatIndex
      else
        fmtIndex := col^.FormatIndex;
      AppendToStream(AStream, Format(
        '<table:table-cell table:style-name="ce%d" />', [fmtIndex]));
      inc(c);
      Continue;
    end;

    // Empty cell? Need to count how often to add "table:number-columns-repeated"
    cc := c + 1;
    while (cc <= ALastColIndex) do
    begin
      col := sheet.FindCol(cc);
      if (col <> nil) and (col^.FormatIndex > 0) then
        break;
      inc(cc);
    end;

    if (c = ALastColIndex) and FHasRowFormats then
      colsRepeated := FLimitations.MaxColCount - c else
      colsRepeated := cc - c;
    if (row <> nil) and (row^.FormatIndex > 0) then
      AppendToStream(AStream, Format(
        '<table:table-cell table:style-name="ce%d" table:number-columns-repeated="%d" />',
        [row^.FormatIndex, colsRepeated]))
    else
      AppendToStream(AStream, Format(
        '<table:table-cell table:number-columns-repeated="%d" />',
        [colsRepeated]));
    c := cc
  end;

  // in case of row formats: extend up to the max column limit of the format
  if FHasRowFormats then begin
    colsRepeated := FLimitations.MaxColCount - ALastColIndex;
    if (row <> nil) and (row^.FormatIndex > 0) then
      AppendToStream(AStream, Format(
        '<table:table-cell table:style-name="ce%d" table:number-columns-repeated="%d" />',
        [row^.FormatIndex, colsRepeated]))
    else
      AppendToStream(AStream, Format(
        '<table:table-cell table:number-columns-repeated="%d" />',
        [colsRepeated]));
  end;

  // Write out closing tag for this row
  AppendToStream(AStream,
    '</table:table-row>');
end;

function TsSpreadOpenDocWriter.FindRowStyle(ASheet: TsBasicWorksheet;
  ARowIndex: Integer): Integer;
var
  row: PRow;
  k: Integer;
  rowStyleData: TRowStyleData;
begin
  Result := -1;

  row := (ASheet as TsWorksheet).FindRow(ARowIndex);
  if row = nil then
    exit;

  for k := 0 to FRowStyleList.Count - 1 do
  begin
    rowStyleData := TRowStyleData(FRowStyleList[k]);
    // Compare elements of row records. Be aware of rounding error when comparing
    // the row height
    if (rowStyleData.PageBreak = (croPageBreak in row^.Options)) and
       ( (rowStyleData.RowHeightType = rhtDefault) or
         (rowStyleData.RowHeightType = row^.RowHeightType) and
         SameValue(rowStyleData.RowHeight, row^.Height, ROWHEIGHT_EPS) ) then
    begin
      Result := k;
      exit;
    end;
  end;
end;

{
procedure TsSpreadOpenDocWriter.GetRowStyleAndHeight(ASheet: TsBasicWorksheet;
  ARowIndex: Integer; out AStyleName: String; out AHeight: Single);
var
  row: PRow;
  rowStyleData: TRowStyleData;
  k: Integer;
begin
  AStyleName := '';
  row := (ASheet as TsWorksheet).FindRow(ARowIndex);
  if row <> nil then
  begin
    AHeight := row^.Height;    // row height in workbook units
    for k := 0 to FRowStyleList.Count-1 do begin
      rowStyleData := TRowStyleData(FRowStyleList[k]);
      // Compare elements of row records. Be aware of rounding errors
      if (rowStyleData.PageBreak = (croPageBreak in row^.Options)) and
         ( (rowStyleData.RowHeightType = rhtDefault) or
           (rowStyleData.RowHeightType = row^.RowHeightType) and
           SameValue(rowStyleData.RowHeight, AHeight, ROWHEIGHT_EPS)
         ) then
                                                                           {
      if SameValue(rowStyleData.RowHeight, AHeight, ROWHEIGHT_EPS) and
         (rowstyleData.RowHeightType = row^.RowHeightType) and
         (rowstyleData.RowHeightType <> rhtDefault) and
         (rowstyleData.PageBreak = isPageBreak) then
      }
      begin
        AStyleName := rowStyleData.Name;
        break;
      end;
    end;
  end;
  if AStyleName = '' then begin
    AStyleName := 'ro1';   // "ro1" is default row record - see ListAllRowStyles
    AHeight := (ASheet as TsWorksheet).ReadDefaultRowHeight(FWorkbook.Units);
  end;
end;
}

{ Write the style nodes for rows ("ro1", "ro2", ...); they contain only
  row height information. "ro1" is the default row height }
procedure TsSpreadOpenDocWriter.WriteRowStyles(AStream: TStream);
var
  i: Integer;
  rowstyle: TRowStyleData;
  book: TsWorkbook;
  sheet: TsWorksheet;
begin
  book := TsWorkbook(FWorkbook);
  sheet := TsWorksheet(FWorksheet);

  if FRowStyleList.Count = 0 then
  begin
    AppendToStream(AStream, Format(
      '<style:style style:name="ro1" style:family="table-row">' +
        '<style:table-row-properties style:row-height="%.3fmm" ' +
          'fo:break-before="auto" style:use-optimal-row-height="true"/>' +
      '</style:style>',
      [sheet.ReadDefaultRowHeight(suMillimeters)]
    ));
    exit;
  end;

  for i := 0 to FRowStyleList.Count-1 do
  begin
    rowStyle := TRowStyleData(FRowStyleList[i]);

    // Start and Name
    AppendToStream(AStream, Format(
      '<style:style style:name="%s" style:family="table-row">', [rowStyle.Name]));

    // Row height
    AppendToStream(AStream, Format(
      '<style:table-row-properties style:row-height="%.3fmm" ',
        [book.ConvertUnits(rowStyle.RowHeight, book.Units, suMillimeters)],
        FPointSeparatorSettings));
    AppendToStream(AStream, Format(
        'style:use-optimal-row-height="%s" ', [FALSE_TRUE[rowstyle.RowHeightType <> rhtCustom]]));
    AppendToStream(AStream, Format(
        'fo:break-before="%s"/>', [PAGE_BREAK[rowStyle.PageBreak]]));

    // End
    AppendToStream(AStream,
      '</style:style>');
  end;
end;


constructor TsSpreadOpenDocWriter.Create(AWorkbook: TsBasicWorkbook);
begin
  inherited Create(AWorkbook);

  FColumnStyleList := TFPList.Create;
  FRowStyleList := TFPList.Create;
  FRichTextFontList := TStringList.Create;
  FHeaderFooterFontList := TObjectList.Create;

  FPointSeparatorSettings := SysUtils.DefaultFormatSettings;
  FPointSeparatorSettings.DecimalSeparator:='.';
  FPointSeparatorSettings.ListSeparator := ';';   // for formulas

  InitOpenDocLimitations(FLimitations);
end;

destructor TsSpreadOpenDocWriter.Destroy;
var
  j: Integer;
begin
  for j:=FColumnStyleList.Count-1 downto 0 do TObject(FColumnStyleList[j]).Free;
  FColumnStyleList.Free;

  for j:=FRowStyleList.Count-1 downto 0 do TObject(FRowStyleList[j]).Free;
  FRowStyleList.Free;

  FRichTextFontList.Free;    // Do not destroy fonts, they are owned by Workbook
  FHeaderFooterFontList.Free;

  inherited Destroy;
end;

{@@ ----------------------------------------------------------------------------
  Writes a string to a file. Helper convenience method.
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteStringToFile(AString, AFileName: string);
var
  TheStream : TFileStream;
  S : String;
begin
  TheStream := TFileStream.Create(AFileName, fmCreate);
  S := AString;
  TheStream.WriteBuffer(Pointer(S)^, Length(S));
  TheStream.Free;
end;
                         (*
{@@ ----------------------------------------------------------------------------
  Writes an OOXML document to a file.
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteToFile(const AFileName: string;
  const AOverwriteExisting: Boolean);
var
  lStream: TStream;
  lMode: word;
begin
  if AOverwriteExisting
    then lMode := fmCreate or fmOpenWrite
    else lMode := fmCreate;

  if (boBufStream in Workbook.Options) then
    lStream := TBufStream.Create(AFileName, lMode)
  else
    lStream := TFileStream.Create(AFileName, lMode);

  try
    WriteToStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;                       *)

procedure TsSpreadOpenDocWriter.WriteToStream(AStream: TStream;
  AParams: TsStreamParams = []);
begin
  Unused(AParams);
  InternalWriteToStream(AStream);
end;

{ Writes an empty cell to the stream }
procedure TsSpreadOpenDocWriter.WriteBlank(AStream: TStream;
  const ARow, ACol: Cardinal; ACell: PCell);
var
  colsSpannedStr: String;
  rowsSpannedStr: String;
  spannedStr: String;
  comment: String;
  r1,c1,r2,c2: Cardinal;
  fmt: TsCellFormat;
  sheet: TsWorksheet;
  lStyle: String;
begin
  Unused(ARow, ACol);
  sheet := FWorksheet as TsWorksheet;

  // Style
  lStyle := GetStyleName(ACell);
  if lStyle <> '' then
    lStyle := Format(' table:style-name="%s"', [lStyle]);

  // Hyperlink
  if sheet.HasHyperlink(ACell) then
    FWorkbook.AddErrorMsg(rsODSHyperlinksOfTextCellsOnly, [GetCellString(ARow, ACol)]);

  // Comment
  comment := WriteCommentXMLAsString(sheet.ReadComment(ACell));

  // Merged?
  if sheet.IsMergeBase(ACell) then
  begin
    sheet.FindMergedRange(ACell, r1, c1, r2, c2);
    rowsSpannedStr := Format(' table:number-rows-spanned="%d"', [r2 - r1 + 1]);
    colsSpannedStr := Format(' table:number-columns-spanned="%d"', [c2 - c1 + 1]);
    spannedStr := colsSpannedStr + rowsSpannedStr;
  end else
    spannedStr := '';

  fmt := (FWorkbook as TsWorkbook).GetCellFormat(ACell^.FormatIndex);
  if (fmt.UsedFormattingFields <> []) then
    AppendToStream(AStream, Format(
      '<table:table-cell%s%s>', [lStyle, spannedStr]),
        comment,
      '</table:table-cell>')
  else
  if comment <> '' then
    AppendToStream(AStream,
      '<table:table-cell' + spannedStr + '>' + comment + '</table:table-cell>')
  else
    AppendToStream(AStream,
      '<table:table-cell' + spannedStr + '/>');
end;

{@@ ----------------------------------------------------------------------------
  Writes a boolean cell to the stream
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteBool(AStream: TStream;
  const ARow, ACol: Cardinal; const AValue: Boolean; ACell: PCell);
var
  lStyle, valType: String;
  r1,c1,r2,c2: Cardinal;
  rowsSpannedStr, colsSpannedStr, spannedStr: String;
  comment: String;
  strValue: String;
  displayStr: String;
begin
  Unused(ARow, ACol);

  valType := 'boolean';

  // Style
  lStyle := GetStyleName(ACell);
  if lStyle <> '' then
    lStyle := Format(' table:style-name="%s"', [lStyle]);

  // Comment
  comment := WriteCommentXMLAsString((FWorksheet as TsWorksheet).ReadComment(ACell));

  // Merged?
  if (FWorksheet as TsWorksheet).IsMergeBase(ACell) then
  begin
    (FWorksheet as TsWorksheet).FindMergedRange(ACell, r1, c1, r2, c2);
    rowsSpannedStr := Format('table:number-rows-spanned="%d"', [r2 - r1 + 1]);
    colsSpannedStr := Format('table:number-columns-spanned="%d"', [c2 - c1 + 1]);
    spannedStr := colsSpannedStr + ' ' + rowsSpannedStr;
  end else
    spannedStr := '';

  // Displayed value
  if AValue then
  begin
    StrValue := 'true';
    DisplayStr := STR_TRUE;
  end else
  begin
    strValue := 'false';
    DisplayStr := STR_FALSE;
  end;

  // Hyperlink
  if FWorksheet.HasHyperlink(ACell) then
    FWorkbook.AddErrorMsg(rsODSHyperlinksOfTextCellsOnly, [GetCellString(ARow, ACol)]);

  AppendToStream(AStream, Format(
    '<table:table-cell office:value-type="%s" office:boolean-value="%s" %s %s >' +
      comment +
      '<text:p>%s</text:p>' +
    '</table:table-cell>', [
    valType, StrValue, lStyle, spannedStr,
    DisplayStr
  ]));
end;

{@@ ----------------------------------------------------------------------------
  Creates an XML string for inclusion of the background color into the
  written file from the backgroundcolor setting in the given format record.
  Is called from WriteStyles (via WriteStylesXMLAsString).

  NOTE: ODS does not support fill patterns. Fill patterns are converted to
  solid fills by mixing pattern and background colors in the ratio defined
  by the fill pattern. Result agrees with that what LO/OO show for an imported
  xls file.
-------------------------------------------------------------------------------}
function TsSpreadOpenDocWriter.WriteBackgroundColorStyleXMLAsString(
  const AFormat: TsCellFormat): String;
const  // fraction of pattern color in fill pattern
  FRACTION: array[TsFillStyle] of Double = (
    0.0, 1.0, 0.75, 0.50, 0.25, 0.125, 0.0625,  // fsNoFill..fsGray6
    0.5, 0.5, 0.5, 0.5,                         // fsStripeHor..fsStripeDiagDown
    0.25, 0.25, 0.25, 0.25,                     // fsThinStripeHor..fsThinStripeDiagDown
    0.5, 6.0/16, 0.75, 7.0/16);                 // fsHatchDiag..fsThinHatchHor
var
  fc,bc: TsColor;
  mix: TRgba;
  fraction_fc, fraction_bc: Double;
begin
  Result := '';

  if not (uffBackground in AFormat.UsedFormattingFields) then
    exit;

  // Foreground and background colors
  fc := AFormat.Background.FgColor;
  if Aformat.Background.BgColor = scTransparent then
    bc := scWhite
  else
    bc := AFormat.Background.BgColor;

  // Mixing fraction
  fraction_fc := FRACTION[AFormat.Background.Style];
  fraction_bc := 1.0 - fraction_fc;

  // Mixed color
  mix.r := Min(round(fraction_fc*TRgba(fc).r + fraction_bc*TRgba(bc).r), 255);
  mix.g := Min(round(fraction_fc*TRgba(fc).g + fraction_bc*TRgba(bc).g), 255);
  mix.b := Min(round(fraction_fc*TRgba(fc).b + fraction_bc*TRgba(bc).b), 255);

  Result := Format('fo:background-color="%s" ', [ColorToHTMLColorStr(TsColor(mix))]);
end;

function TsSpreadOpenDocWriter.WriteBiDiModeStyleXMLAsString(
  const AFormat: TsCellFormat): String;
begin
  Result := '';
  if not (uffBiDi in AFormat.UsedFormattingFields) then
    exit;
  case AFormat.BiDiMode of
    bdLTR : Result := 'style:writing-mode="lr-tb" ';
    bdRTL : Result := 'style:writing-mode="rl-tb" ';
  end;
end;

{@@ ----------------------------------------------------------------------------
  Creates an XML string for inclusion of borders and border styles into the
  written file from the border settings in the given format record.
  Is called from WriteStyles (via WriteStylesXMLAsString).
-------------------------------------------------------------------------------}
function TsSpreadOpenDocWriter.WriteBorderStyleXMLAsString(
  const AFormat: TsCellFormat): String;
begin
  Result := '';

  if not (uffBorder in AFormat.UsedFormattingFields) then
    exit;

  if cbSouth in AFormat.Border then
  begin
    Result := Result + Format('fo:border-bottom="%s %s %s" ', [
      BORDER_LINEWIDTHS[AFormat.BorderStyles[cbSouth].LineStyle],
      BORDER_LINESTYLES[AFormat.BorderStyles[cbSouth].LineStyle],
      ColorToHTMLColorStr(AFormat.BorderStyles[cbSouth].Color)
    ]);
    if AFormat.BorderStyles[cbSouth].LineStyle = lsDouble then
      Result := Result + 'style:border-linewidth-bottom="0.002cm 0.035cm 0.002cm" ';
  end
  else
    Result := Result + 'fo:border-bottom="none" ';

  if cbWest in AFormat.Border then
  begin
    Result := Result + Format('fo:border-left="%s %s %s" ', [
      BORDER_LINEWIDTHS[AFormat.BorderStyles[cbWest].LineStyle],
      BORDER_LINESTYLES[AFormat.BorderStyles[cbWest].LineStyle],
      ColorToHTMLColorStr(AFormat.BorderStyles[cbWest].Color)
    ]);
    if AFormat.BorderStyles[cbWest].LineStyle = lsDouble then
      Result := Result + 'style:border-linewidth-left="0.002cm 0.035cm 0.002cm" ';
  end
  else
    Result := Result + 'fo:border-left="none" ';

  if cbEast in AFormat.Border then
  begin
    Result := Result + Format('fo:border-right="%s %s %s" ', [
      BORDER_LINEWIDTHS[AFormat.BorderStyles[cbEast].LineStyle],
      BORDER_LINESTYLES[AFormat.BorderStyles[cbEast].LineStyle],
      ColorToHTMLColorStr(AFormat.BorderStyles[cbEast].Color)
    ]);
    if AFormat.BorderStyles[cbSouth].LineStyle = lsDouble then
      Result := Result + 'style:border-linewidth-right="0.002cm 0.035cm 0.002cm" ';
  end
  else
    Result := Result + 'fo:border-right="none" ';

  if cbNorth in AFormat.Border then
  begin
    Result := Result + Format('fo:border-top="%s %s %s" ', [
      BORDER_LINEWIDTHS[AFormat.BorderStyles[cbNorth].LineStyle],
      BORDER_LINESTYLES[AFormat.BorderStyles[cbNorth].LineStyle],
      ColorToHTMLColorStr(AFormat.BorderStyles[cbNorth].Color)
    ]);
    if AFormat.BorderStyles[cbSouth].LineStyle = lsDouble then
      Result := Result + 'style:border-linewidth-top="0.002cm 0.035cm 0.002cm" ';
  end else
    Result := Result + 'fo:border-top="none" ';

  if cbDiagUp in AFormat.Border then
  begin
    Result := Result + Format('style:diagonal-bl-tr="%s %s %s" ', [
      BORDER_LINEWIDTHS[AFormat.BorderStyles[cbDiagUp].LineStyle],
      BORDER_LINESTYLES[AFormat.BorderStyles[cbDiagUp].LineStyle],
      ColorToHTMLColorStr(AFormat.BorderStyles[cbDiagUp].Color)
    ]);
  end;

  if cbDiagDown in AFormat.Border then
  begin
    Result := Result + Format('style:diagonal-tl-br="%s %s %s" ', [
      BORDER_LINEWIDTHS[AFormat.BorderStyles[cbDiagDown].LineStyle],
      BORDER_LINESTYLES[AFormat.BorderStyles[cbDiagDown].LineStyle],
      ColorToHTMLColorStr(AFormat.BorderStyles[cbDiagDown].Color)
    ]);
  end;
end;

function TsSpreadOpenDocWriter.WriteConditionalStyleXMLAsString(ACFIndex: Integer): string;
var
  book: TsWorkbook;
  k: Integer;
  cf: TsConditionalFormat;
  cf_CellRule: TsCFCellRule;
  cf_StyleName: String;
  cf_Condition: String;
  cf_Sheet: TsWorksheet;
  firstCellOfRange: String;
  operand1Str, operand2Str: String;
begin
  Result := '';

  book := TsWorkbook(FWorkbook);
  cf := book.GetConditionalFormat(ACFIndex);
  cf_sheet := cf.Worksheet as TsWorksheet;
  firstCellOfRange := cf_sheet.Name + '.' + GetCellString(cf.CellRange.Row1, cf.CellRange.Col1);

  // Some rules have a style:map node
  for k := 0 to cf.RulesCount-1 do begin
    if cf.Rules[k] is TsCFCellRule then
    begin
      cf_cellRule := TsCFCellRule(cf.Rules[k]);
      cf_styleName := Format('conditional_%d', [cf_cellRule.FormatIndex]);
      if cf_cellRule.Condition = cfcExpression then
      begin
        operand1Str := VarToStr(cf_cellRule.Operand1);
        if (operand1Str <> '') and (operand1Str[1] <> '=') then
          operand1Str := '=' + operand1Str;
        operand1Str := CFOperandToStr(operand1Str, cf_sheet);
        operand2Str := '';
      end else
      begin
        operand1Str := CFOperandToStr(cf_cellrule.Operand1, cf_sheet);
        operand2Str := CFOperandToStr(cf_cellrule.Operand2, cf_sheet);
      end;
      cf_condition := Format(CF_STYLE_OP[cf_cellRule.Condition], [operand1Str, operand2Str]);

      if cf_Condition <> '' then begin
        Result := Result +
          Format('<style:map style:condition="%s" style:apply-style-name="%s" style:base-cell-address="%s" />', [
            cf_Condition,
            cf_StyleName,
            firstCellOfRange
          ]);
      end;
    end;
  end;
end;

function TsSpreadOpenDocWriter.WriteDefaultFontXMLAsString: String;
var
  fnt: TsFont;
begin
  fnt := (Workbook as TsWorkbook).GetDefaultFont;
  Result := Format(
    '<style:text-properties style:font-name="%s" fo:font-size="%.1fpt" />',
    [fnt.FontName, fnt.Size], FPointSeparatorSettings
  );
end;

function TsSpreadOpenDocWriter.WriteDefaultGraphicStyleXMLAsString: String;
begin
  Result :=
    '<style:graphic-properties svg:stroke-color="#3465a4" '+
      'draw:fill-color="#729fcf" fo:wrap-option="no-wrap" '+
      'draw:shadow-offset-x="3mm" draw:shadow-offset-y="3mm" />' +
    '<style:paragraph-properties style:text-autospace="ideograph-alpha" '+
      'style:punctuation-wrap="simple" style:line-break="strict" '+
      'style:writing-mode="page" style:font-independent-line-spacing="false">'+
      '<style:tab-stops />'+
    '</style:paragraph-properties>'+
    '<style:text-properties style:use-window-font-color="true" '+
      'fo:font-family="''Liberation Serif''" style:font-family-generic="roman" '+
      'style:font-pitch="variable" fo:font-size="12pt" ' +
      //'fo:language="de" fo:country="DE" '+
      'style:letter-kerning="true" '+
      'style:font-name-asian="Segoe UI" style:font-size-asian="12pt" '+
      'style:language-asian="zh" style:country-asian="CN" '+
      'style:font-name-complex="Tahoma" style:font-size-complex="12pt" '+
      'style:language-complex="hi" style:country-complex="IN" />';
end;

function TsSpreadOpenDocWriter.WriteDocumentProtectionXMLAsString: String;
var
  cinfo: TsCryptoInfo;
  pwd, algo: String;
begin
  if bpLockStructure in Workbook.Protection then
  begin
    cinfo := (Workbook as TsWorkbook).CryptoInfo;
    if cinfo.PasswordHash <> '' then
      pwd := Format(' table:protection-key="%s"', [cinfo.PasswordHash])
    else
      pwd := '';
    if cinfo.Algorithm <> caUnknown then
      algo := Format(' table:protection-key-digest-algorithm="%s"',
        [AlgorithmToStr(cinfo.Algorithm, auOpenDocument)])
    else
      algo := '';
    Result := ' table:structure-protected="true"' + pwd + algo;
  end
  else
    Result := '';
end;

procedure TsSpreadOpenDocWriter.WriteError(AStream: TStream;
  const ARow, ACol: Cardinal; const AValue: TsErrorValue; ACell: PCell);
var
  lStyle: String;
  comment: String;
  rowsSpannedStr, colsSpannedStr: String;
  spannedStr: String;
  valueStr: String;
  r1,c1,r2,c2: Cardinal;
  sheet: TsWorksheet;
begin
  Unused(ARow, ACol, AValue);

  sheet := FWorksheet as TsWorksheet;

  // Style
  lStyle := GetStyleName(ACell);
  if lStyle <> '' then
    lStyle := Format(' table:style-name="%s"', [lStyle]);

  // Comment
  comment := WriteCommentXMLAsString(sheet.ReadComment(ACell));

  // Merged?
  if sheet.IsMergeBase(ACell) then
  begin
    sheet.FindMergedRange(ACell, r1, c1, r2, c2);
    rowsSpannedStr := Format(' table:number-rows-spanned="%d"', [r2 - r1 + 1]);
    colsSpannedStr := Format(' table:number-columns-spanned="%d"', [c2 - c1 + 1]);
    spannedStr := colsSpannedStr + rowsSpannedStr;
  end else
    spannedStr := '';

  // Displayed value
  valueStr := GetErrorValueStr(ACell^.ErrorValue);

  // Hyperlink
  if sheet.HasHyperlink(ACell) then
    (FWorkbook as TsWorkbook).AddErrorMsg(rsODSHyperlinksOfTextCellsOnly, [GetCellString(ARow, ACol)]);

  // Write to stream
  AppendToStream(AStream, Format(
    '<table:table-cell table:formula="%s" office:value-type="string"%s%s>'+
      'office:string-value="" calcext:value-type="error">' +
      comment +
      '<text:p>%s</text:p>' +
    '</table:table-cell>', [
    valueStr, lStyle, spannedStr,
    valueStr
  ]));
       (*
  <table:table-cell table:formula="of:=#N/A" office:value-type="string"
    office:string-value="" calcext:value-type="error">
    <text:p>#NV</text:p>
    </table:table-cell>

  AppendToStream(AStream, Format(
    '<table:table-cell office:value-type="%s" office:boolean-value="%s" %s %s >' +
      comment +
      '<text:p>%s</text:p>' +
    '</table:table-cell>', [
    valType, StrValue, lStyle, spannedStr,
    DisplayStr
  ]));
         *)
end;

function TsSpreadOpenDocWriter.WriteFontStyleXMLAsString(AFont: TsFont): String;
var
  defFnt: TsFont;
begin
  Result := '';

  defFnt := (Workbook as TsWorkbook).GetDefaultFont;
  if AFont = nil then AFont := defFnt;

  if AFont.FontName <> '' then
    Result := Result + Format('style:font-name="%s" ', [AFont.FontName]);

  if AFont.Size > 0 then
    Result := Result + Format('fo:font-size="%.1fpt" style:font-size-asian="%.1fpt" style:font-size-complex="%.1fpt" ',
      [AFont.Size, AFont.Size, AFont.Size], FPointSeparatorSettings);

  if fssBold in AFont.Style then
    Result := Result + 'fo:font-weight="bold" style:font-weight-asian="bold" style:font-weight-complex="bold" ';

  if fssItalic in AFont.Style then
    Result := Result + 'fo:font-style="italic" style:font-style-asian="italic" style:font-style-complex="italic" ';

  if fssUnderline in AFont.Style then
    Result := Result + 'style:text-underline-style="solid" style:text-underline-width="auto" style:text-underline-color="font-color" ';

  if fssStrikeout in AFont.Style then
    Result := Result + 'style:text-line-through-style="solid" ';

  if AFont.Position = fpSubscript then
    Result := Result + 'style:text-position="sub 58%" ';

  if AFont.Position = fpSuperscript then
    Result := Result + 'style:text-position="super 58%" ';

  if (AFont.Color <> defFnt.Color) and (AFont.Color <> scNotDefined) then
    Result := Result + Format('fo:color="%s" ', [ColorToHTMLColorStr(AFont.Color)]);
end;

function TsSpreadOpenDocWriter.WriteFontStyleXMLAsString(
  const AFormat: TsCellFormat): String;
begin
  Result := '';
  if (uffFont in AFormat.UsedFormattingFields) then
    Result := WriteFontStyleXMLAsString((Workbook as TsWorkbook).GetFont(AFormat.FontIndex));
end;

function TsSpreadOpenDocWriter.WriteHeaderFooterFontXMLAsString(
  AFont: TsHeaderFooterFont): String;
begin
  Result := Format('style:font-name="%s" fo:font-size="%dpt" ', [
    AFont.FontName, round(AFont.Size)
  ]);

  if hfsBold in AFont.Style then
    Result := Result + 'fo:font-weight="bold" ';

  if hfsItalic in AFont.Style then
    Result := Result + 'fo:font-style="italic" ';

  if hfsUnderline in AFont.Style then
    Result := Result + 'style:text-underline-style="solid" '+
                       'style:text-underline-width="auto" '+
                       'style:text-underline-color="font-color" ';

  if hfsDblUnderline in AFont.Style then
    Result := Result + 'style:text-underline-style="solid" '+
                       'style:text-underline-type="double" ' +
                       'style:text-underline-width="auto" '+
                       'style:text-underline-color="font-color" ';

  if hfsStrikeout in AFont.Style then
    Result := Result + 'style:text-line-through-style="solid" ';

  if hfsOutline in AFont.Style then
    Result := Result + 'style:text-outline="true" ';

  if hfsShadow in AFont.Style then
    Result := Result + 'fo:text-shadow="1pt 1pt" ' +
                       'style:text-outline="none" ';

  if hfsSubscript in AFont.Style then
    Result := Result + 'style:text-position="sub 58%" ';

  if hfsSuperscript in AFont.Style then
    Result := Result + 'style:text-position="super 58%" ';

  if AFont.Color <> 0 then
    Result := Result + Format('fo:color="%s" ', [ColorToHTMLColorStr(AFont.Color)]);
end;


{@@ ----------------------------------------------------------------------------
  Creates an XML string for inclusion of the horizontal alignment into the
  written file from the horizontal alignment setting in the format cell.
  Is called from WriteStyles (via WriteStylesXMLAsString).
-------------------------------------------------------------------------------}
function TsSpreadOpenDocWriter.WriteHorAlignmentStyleXMLAsString(
  const AFormat: TsCellFormat): String;
begin
  Result := '';
  if not (uffHorAlign in AFormat.UsedFormattingFields) then
    exit;
  case AFormat.HorAlignment of
    haLeft   : Result := 'fo:text-align="start" ';
    haCenter : Result := 'fo:text-align="center" ';
    haRight  : Result := 'fo:text-align="end" ';
  end;
end;

function TsSpreadOpenDocWriter.WriteNumFormatStyleXMLAsString(
  const AFormat: TsCellFormat): String;
var
  nfParams: TsNumFormatParams;
  nfs: String;
  j: Integer;
  s: String;
  p: Integer;
begin
  Result := '';
  if not (uffNumberFormat in AFormat.UsedFormattingFields) then
    exit;

  nfParams := TsWorkbook(FWorkbook).GetNumberFormat(AFormat.NumberFormatIndex);
  if nfParams <> nil then
  begin
    nfs := nfParams.NumFormatStr;
    for j:=0 to NumFormatList.Count-1 do
    begin
      s := NumFormatList[j];
      p := pos(':', s);
      if SameText(Copy(s, p+1, Length(s)), nfs) then
      begin
        Result := Format(' style:data-style-name="%s"', [copy(s, 1, p-1)]);
        Exit;
      end;
    end;
  end;
end;

function TsSpreadOpenDocWriter.WritePageLayoutXMLAsString(AStyleName: String;
  const APageLayout: TsPageLayout): String;

  function CalcPageLayoutPropStr: String;
  var
    topmargin, bottommargin: Double;
    options: String;
  begin
    topMargin := IfThen(APageLayout.HasHeader,
      APageLayout.HeaderMargin, APageLayout.TopMargin);
    bottomMargin := IfThen(APageLayout.HasFooter,
      APageLayout.FooterMargin, APageLayout.BottomMargin);

    Result := Format(
        'fo:page-width="%.2fmm" fo:page-height="%.2fmm" '+
        'fo:margin-top="%.2fmm" fo:margin-bottom="%.2fmm" '+
        'fo:margin-left="%.2fmm" fo:margin-right="%.2fmm" ', [
        APageLayout.PageWidth, APageLayout.PageHeight,
        topmargin, bottommargin,
        APageLayout.LeftMargin, APageLayout.RightMargin
      ], FPointSeparatorSettings);

    if APageLayout.Orientation = spoLandscape then
      Result := Result + 'style:print-orientation="landscape" ';

    if poPrintPagesByRows in APageLayout.Options then
      Result := Result + 'style:print-page-order="ltr" ';

    if poUseStartPageNumber in APageLayout.Options then
      Result := Result + 'style:first-page-number="' + IntToStr(APageLayout.StartPageNumber) +'" '
    else
      Result := Result + 'style:first-page-number="continue" ';

    if APageLayout.Options * [poHorCentered, poVertCentered] = [poHorCentered, poVertCentered] then
      Result := Result + 'style:table-centering="both" '
    else if poHorCentered in APageLayout.Options then
      Result := Result + 'style:table-centering="horizontal" '
    else if poVertCentered in APageLayout.Options then
      Result := Result + 'style:table-centering="vertical" ';

    if poFitPages in APageLayout.Options then
    begin
      if APageLayout.FitWidthToPages > 0 then
        Result := Result + 'style:scale-to-X="' + IntToStr(APageLayout.FitWidthToPages) + '" ';
      if APageLayout.FitHeightToPages > 0 then
        Result := Result + 'style:scale-to-Y="' + IntToStr(APageLayout.FitHeightToPages) + '" ';
    end else
      Result := Result + 'style:scale-to="' + IntToStr(APageLayout.ScalingFactor) + '%" ';

    options := 'charts drawings objects zero-values';
    if poPrintGridLines in APageLayout.Options then
      options := options + ' grid';
    if poPrintHeaders in APageLayout.Options then
      options := options + ' headers';
    if poPrintCellComments in APageLayout.Options then
      options := options + ' annotations';

    Result := Result + 'style:print="' + options + '" ';
  end;

  function CalcStyleStr(AName, AHeaderFooterImageStr: String;
    APageMargin, AHeaderFooterMargin: Double): String;
  var
    marginKind: String;
  begin
    if AName = 'header' then marginKind := 'bottom' else marginKind := 'top';
    Result := Format(
      '<style:%s-style>' +                // e.g. <style:header-style>
        '<style:header-footer-properties ' +
          'fo:margin-left="0mm" fo:margin-right="0mm" '+
          'svg:height="%.2fmm" fo:margin-%s="%.2fmm" ' + // fo:margin-bottom or -top
          'fo:background-color="transparent">' +
          '%s' +
        '</style:header-footer-properties>' +
      '</style:%s-style>', [
      AName,
      APageMargin - AHeaderFooterMargin, marginKind, 0.0,
      AHeaderFooterImageStr,
      AName
    ], FPointSeparatorSettings);
  end;

  procedure CalcHeaderFooterImageStr(out AHeaderImageStr, AFooterImageStr: String);
  var
    hdrImg, ftrImg, hdrImgPos, ftrImgPos: String;
  begin
    GetHeaderFooterImageName(APageLayout, hdrImg, ftrImg);
    GetHeaderFooterImagePosStr(APageLayout, hdrImgPos, ftrImgPos);

    AHeaderImageStr := IfThen((hdrImg = '') or (hdrImgPos = ''), '', Format(
      '<style:background-image xlink:href="Pictures/%s" '+
        'xlink:type="simple" xlink:actuate="onLoad" '+
        'style:position="center %s" style:repeat="no-repeat" />',
      [hdrImg, hdrImgPos] ));

    AFooterImageStr := IfThen((ftrImg = '') or (ftrImgPos = ''), '', Format(
      '<style:background-image xlink:href="Pictures/%s" '+
        'xlink:type="simple" xlink:actuate="onLoad" '+
        'style:position="center %s" style:repeat="no-repeat" />',
      [ftrImg, ftrImgPos]));
  end;

var
  hdrImgStr: String = '';
  ftrImgStr: String = '';
begin
  CalcHeaderFooterImageStr(hdrImgStr, ftrImgStr);
  Result :=
    '<style:page-layout style:name="' + AStyleName + '">' +
      '<style:page-layout-properties ' + CalcPageLayoutPropStr + '/>'+
      CalcStyleStr('header', hdrImgStr, APageLayout.TopMargin, APageLayout.HeaderMargin) +
      CalcStyleStr('footer', ftrImgStr, APageLayout.BottomMargin, APageLayout.FooterMargin) +
    '</style:page-layout>';
end;

function TsSpreadOpenDocWriter.WritePrintRangesXMLAsString(
  ASheet: TsBasicWorksheet): String;
var
  i: Integer;
  rng: TsCellRange;
  sheetName: String;
  sheet: TsWorksheet absolute ASheet;
begin
  Result := '';
  if sheet.PageLayout.NumPrintRanges > 0 then
  begin
    for i := 0 to sheet.PageLayout.NumPrintRanges - 1 do
    begin
      rng := sheet.PageLayout.PrintRange[i];
      if pos(' ', ASheet.Name) > 0 then
        sheetName := '&apos;' + UTF8TextToXMLText(sheet.Name) + '&apos;' else
        sheetname := UTF8TextToXMLText(sheet.Name);
      Result := Result + ' ' + Format('%s.%s:%s.%s', [
        sheetName, GetCellString(rng.Row1,rng.Col1),
        sheetName, GetCellString(rng.Row2,rng.Col2)
      ]);
    end;
    if Result <> '' then
    begin
      Delete(Result, 1, 1);
      Result := ' table:print-ranges="' + Result + '"';
    end;
  end;
end;

function TsSpreadOpenDocWriter.WriteSheetProtectionXMLAsString(
  ASheet: TsBasicWorksheet): String;
{table:protected="true" table:protection-key="h/jtkVcSX/xNqeBqe4ARrYClP+E=" table:protection-key-digest-algorithm="http://www.w3.org/2000/09/xmldsig#sha1"}
var
  pwd: String;
  algo: String;
  sheet: TsWorksheet absolute ASheet;
begin
  Result := '';
  if ASheet.IsProtected then
  begin
    if sheet.CryptoInfo.PasswordHash <> '' then
      pwd := ' table:protection-key="' + sheet.CryptoInfo.PasswordHash + '"' else
      pwd := '';
    algo := AlgorithmToStr(sheet.CryptoInfo.Algorithm, auOpenDocument);
    if algo <> '' then
      algo := ' table:protection-key-digest-algorithm="%s"';
    Result := ' table:protected="true"' + pwd + algo;
  end;
end;

function TsSpreadOpenDocWriter.WriteSheetProtectionDetailsXMLAsString(
  ASheet: TsBasicWorksheet): String;
// <loext:table-protection loext:select-unprotected-cells="true" />
begin
  Result := '';
  if ASheet.IsProtected then
  begin
    if not (spSelectUnlockedCells in ASheet.Protection) then
      Result := Result + ' loext:select-unprotected-cells="true"';
    if not (spSelectLockedCells in ASheet.Protection) then
      Result := Result + ' loext:select-protected-cells="true"';
    Result := '<loext:table-protection' + Result + '/>';
  end;
end;

procedure TsSpreadOpenDocWriter.WriteShapes(AStream: TStream;
  ASheet: TsBasicWorksheet);
{
<table:shapes>
  <draw:frame draw:z-index="0" draw:name="Bild 1" draw:style-name="gr1" draw:text-style-name="P1"
       svg:width="4.45mm" svg:height="4.24mm" svg:x="0mm" svg:y="0mm">
    <draw:image xlink:href="Pictures/100002010000001000000010DC3B2E96AAE6D486.png" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad">
      <text:p />
    </draw:image>
  </draw:frame>
</table:shapes>
}
var
  i: Integer;
  img: TsImage;
  imgType: TsImageType;
  r1,c1,r2,c2: Cardinal;
  roffs1,coffs1, roffs2, coffs2: Double;
  x, y, w, h: Double;
  xml: String;
  target, bookmark: String;
  u: TURI;
begin
  if (ASheet as TsWorksheet).GetImageCount = 0 then
    exit;

  AppendToStream(AStream,
    '<table:shapes>');

  for i:=0 to (ASheet as TsWorksheet).GetImageCount-1 do
  begin
    img := (ASheet as TsWorksheet).GetImage(i);
    imgType := (FWorkbook as TsWorkbook).GetEmbeddedObj(img.Index).ImageType;
    if imgType = itUnknown then
      Continue;

    (ASheet as TsWorksheet).CalcImageExtent(i, false,  // not clear if UsePixels=false is correct. Not harmful at least
      r1, c1, r2, c2,
      roffs1, coffs1, roffs2, coffs2,  // mm
      x, y, w, h);                     // mm

    xml := Format(
      '<draw:frame draw:z-index="%d" draw:name="Image %d" '+
        'draw:style-name="gr1" draw:text-style-name="P1" '+
        'svg:width="%.2fmm" svg:height="%.2fmm" '+
        'svg:x="%.2fmm" svg:y="%.2fmm">' +
        '<draw:image xlink:href="Pictures/%d.%s" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad">' +
          '<text:p />' +
        '</draw:image>' +
      '</draw:frame>', [
      i+1, i+1,
      w, h,
      x, y,
      img.Index+1, GetImageTypeExt(imgType)
    ], FPointSeparatorSettings);

    if img.HyperlinkTarget <> '' then begin
      SplitHyperlink(img.HyperlinkTarget, target, bookmark);
      if (target <> '') and (pos('file:', target) = 0) then
      begin
        u := ParseURI(target);
        if u.Protocol = '' then
          target := '../' + target;
      end;

      // ods absolutely wants "/" path delimiters in the file uri!
      FixHyperlinkPathdelims(target);

      if (bookmark <> '') then
        target := target + '#' + bookmark;

      xml := Format('<draw:a xlink:type="simple" xlink:href="%s">%s</draw:a>', [target, xml]);
    end;

    AppendToStream(AStream, xml);
  end;

  AppendToStream(AStream,
    '</table:shapes>');
end;

procedure TsSpreadOpenDocWriter.WriteTableSettings(AStream: TStream);
var
  i: Integer;
  sheet: TsWorkSheet;
  sheetname: String;
  hsm: Integer;         // HorizontalSplitMode
  vsm: Integer;         // VerticalSplitMode
  asr: Integer;         // ActiveSplitRange
  actX, actY: Integer;  // Active cell col/row index
  zoom: String;
begin
  zoom := '100';
  for i:=0 to (Workbook as TsWorkbook).GetWorksheetCount-1 do
  begin
    sheet := (Workbook as TsWorkbook).GetWorksheetByIndex(i);
    sheetname := UTF8TextToXMLText(sheet.Name);

    AppendToStream(AStream,
      '<config:config-item-map-entry config:name="' + sheetname + '">');

    hsm := 0; vsm := 0; asr := 2;
    if (soHasFrozenPanes in sheet.Options) then
    begin
      if (sheet.LeftPaneWidth > 0) and (sheet.TopPaneHeight > 0) then
      begin
        hsm := 2; vsm := 2; asr := 3;
      end else
      if (sheet.LeftPaneWidth > 0) then
      begin
        hsm := 2; vsm := 0; asr := 3;
      end else if (sheet.TopPaneHeight > 0) then
      begin
        hsm := 0; vsm := 2; asr := 2;
      end;
    end;
    {showGrid := (soShowGridLines in sheet.Options);}

    if (sheet.ActiveCellRow <> cardinal(-1)) and (sheet.ActiveCellCol <> cardinal(-1)) then
    begin
      actX := sheet.ActiveCellCol;
      actY := sheet.ActiveCellRow;
    end else
    begin
      actX := sheet.LeftPaneWidth;
      actY := sheet.TopPaneHeight;
    end;

    if boWriteZoomFactor in FWorkbook.Options then
      zoom := IntToStr(round(sheet.ZoomFactor*100.0));

    AppendToStream(AStream,
        '<config:config-item config:name="CursorPositionX" config:type="int">'+IntToStr(actX)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="CursorPositionY" config:type="int">'+IntToStr(actY)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="HorizontalSplitMode" config:type="short">'+IntToStr(hsm)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="VerticalSplitMode" config:type="short">'+IntToStr(vsm)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="HorizontalSplitPosition" config:type="int">'+IntToStr(sheet.LeftPaneWidth)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="VerticalSplitPosition" config:type="int">'+IntToStr(sheet.TopPaneHeight)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="ActiveSplitRange" config:type="short">'+IntToStr(asr)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="PositionLeft" config:type="int">0</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="PositionRight" config:type="int">'+IntToStr(sheet.LeftPaneWidth)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="PositionTop" config:type="int">0</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="PositionBottom" config:type="int">'+IntToStr(sheet.TopPaneHeight)+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="ZoomType" config:type="short">0</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="ZoomValue" config:type="int">'+zoom+'</config:config-item>');
    AppendToStream(AStream,
        '<config:config-item config:name="ShowGrid" config:type="boolean">true</config:config-item>');
       // this "ShowGrid" overrides the global setting. But Open/LibreOffice do not allow to change ShowGrid per sheet.
    AppendToStream(AStream,
      '</config:config-item-map-entry>');
  end;
end;

procedure TsSpreadOpenDocWriter.WriteTableStyles(AStream: TStream);
var
  i: Integer;
  sheet: TsWorksheet;
  sheetname, bidi, tabColor: String;
begin
  for i:=0 to (FWorkbook as TsWorkbook).GetWorksheetCount-1 do
  begin
    sheet := (FWorkbook as TsWorkbook).GetWorksheetByIndex(i);
    sheetname := UTF8TextToXMLText(sheet.Name);
    case sheet.BiDiMode of
      bdDefault: bidi := '';
      bdLTR    : bidi := 'style:writing-mode="lr-tb" ';
      bdRTL    : bidi := 'style:writing-mode="rl-tb" ';
    end;
    if sheet.TabColor = scNotDefined then
      tabColor := ''
    else
      tabColor := 'tableooo:tab-color="' + ColorToHTMLColorStr(sheet.TabColor) + '" ';
    AppendToStream(AStream, Format(
      '<style:style style:name="ta%d" style:family="table" style:master-page-name="PageStyle_5f_%s">' +
        '<style:table-properties table:display="%s" %s %s/>' +
      '</style:style>', [
      i+1, UTF8TextToXMLText(sheetname),
      FALSE_TRUE[not (soHidden in sheet.Options)], bidi, tabColor
    ]));

    if sheet.GetImageCount > 0 then
    begin
      // Embedded images written by fps refer to a graphic style "gr1"...
      AppendToStream(AStream,
        '<style:style style:name="gr1" style:family="graphic">'+
          '<style:graphic-properties draw:stroke="none" draw:fill="none" '+
            'draw:textarea-horizontal-align="center" '+
            'draw:textarea-vertical-align="middle" '+
            'draw:color-mode="standard" '+
            'draw:luminance="0%" draw:contrast="0%" draw:image-opacity="100%" '+
            'draw:gamma="100%" draw:red="0%" draw:green="0%" draw:blue="0%" '+
            'fo:clip="rect(0mm, 0mm, 0mm, 0mm)" '+
            'style:mirror="none"/>'+
          '</style:style>');
      // ... and a paragraph style named "P1"
      AppendToStream(AStream,
        '<style:style style:name="P1" style:family="paragraph">' +
          '<loext:graphic-properties draw:fill="none" />' +
          '<style:paragraph-properties fo:text-align="center" />' +
        '</style:style>');
    end;
  end;
end;

procedure TsSpreadOpenDocWriter.WriteTextStyles(AStream: TStream);
var
  cell: PCell;
  rtp: TsRichTextParam;
  styleCounter: Integer;
  fnt: TsFont;
  fntStr: String;
  styleName: String;
  sheet: TsWorksheet;
  i: Integer;
begin
  styleCounter := 0;
  for i := 0 to (FWorkbook as TsWorkbook).GetWorksheetCount-1 do
  begin
    sheet := (FWorkbook as TsWorkbook).GetWorksheetByIndex(i);
    for cell in sheet.Cells do
    begin
      if Length(cell^.RichTextParams) = 0 then
        Continue;
      for rtp in cell^.RichTextParams do
      begin
        inc(styleCounter);
        stylename := Format('T%d', [stylecounter]);
        fnt := (FWorkbook as TsWorkbook).GetFont(rtp.FontIndex);
        FRichTextFontList.AddObject(stylename, fnt);
        fntStr := WriteFontStyleXMLAsString(fnt);
        AppendToStream(AStream,
          '<style:style style:name="' + stylename + '" style:family="text">' +
            '<style:text-properties ' + fntStr + '/>' +
          '</style:style>');
      end;
    end;
  end;
end;


{@@ ----------------------------------------------------------------------------
  Creates an XML string for inclusion of the text rotation style option into the
  written file from the textrotation setting in the format cell.
  Is called from WriteStyles (via WriteStylesXMLAsString).
-------------------------------------------------------------------------------}
function TsSpreadOpenDocWriter.WriteTextRotationStyleXMLAsString(
  const AFormat: TsCellFormat): String;
begin
  Result := '';
  if not (uffTextRotation in AFormat.UsedFormattingFields) then
    exit;

  case AFormat.TextRotation of
    rt90DegreeClockwiseRotation        : Result := 'style:rotation-angle="270" ';
    rt90DegreeCounterClockwiseRotation : Result := 'style:rotation-angle="90" ';
    rtStacked                          : Result := 'style:direction="ttb" ';
  end;
end;

{@@ ----------------------------------------------------------------------------
  Creates an XML string for inclusion of the vertical alignment into the
  written file from the vertical alignment setting in the given format record.
  Is called from WriteStyles (via WriteStylesXMLAsString).
-------------------------------------------------------------------------------}
function TsSpreadOpenDocWriter.WriteVertAlignmentStyleXMLAsString(
  const AFormat: TsCellFormat): String;
begin
  Result := '';
  if not (uffVertAlign in AFormat.UsedFormattingFields) then
    exit;
  case AFormat.VertAlignment of
    vaTop    : Result := 'style:vertical-align="top" ';
    vaCenter : Result := 'style:vertical-align="middle" ';
    vaBottom : Result := 'style:vertical-align="bottom" ';
  end;
end;

procedure TsSpreadOpenDocWriter.WriteVirtualCells(AStream: TStream;
  ASheet: TsBasicWorksheet);
var
  r, c, cc: Cardinal;
  lCell: TCell;
  row: PRow;
  value: variant;
  styleCell: PCell;
  styleName: String;
  h: Single;      // row height workbook units
  k: Integer;
  rowStyleData: TRowStyleData;
  rowsRepeated: Cardinal;
  colsRepeated: Cardinal;
  colsRepeatedStr: String;
  lastCol, lastRow: Cardinal;
  sheet: TsWorksheet absolute ASheet;
begin
  if sheet.VirtualColCount = 0 then
    exit;
  if sheet.VirtualRowCount = 0 then
    exit;
  if not Assigned(sheet.OnWriteCellData) then
    exit;

  // some abbreviations...
  lastCol := LongInt(sheet.VirtualColCount) - 1;
  lastRow := LongInt(sheet.VirtualRowCount) - 1;

  rowsRepeated := 1;
  r := 0;
  while (r <= lastRow) do
  begin
    // Look for the row style of the current row (r)
    row := sheet.FindRow(r);
    if row = nil then
      styleName := 'ro1'
    else
    begin
      styleName := '';

      h := row^.Height;   // row height in workbook units
      for k := 0 to FRowStyleList.Count-1 do
      begin
        rowStyleData := TRowStyleData(FRowStyleList[k]);
        // Compare row heights, but be aware of rounding errors
        if SameValue(rowStyleData.RowHeight, h, ROWHEIGHT_EPS) then
        begin
          styleName := rowStyleData.Name;
          break;
        end;
      end;
      if styleName = '' then
        raise EFPSpreadsheetWriter.Create(rsRowStyleNotFound);
    end;

    // No empty rows allowed here for the moment!

    // Write the row XML
    AppendToStream(AStream, Format(
        '<table:table-row table:style-name="%s">', [styleName]));

    // Loop along the row and write the cells.
    c := 0;
    while c <= lastCol do
    begin
      // Empty cell? Need to count how many "table:number-columns-repeated" to be added
      colsRepeated := 1;

      lCell.Row := r;  // to silence a compiler hint...
      InitCell(ASheet, r, c, lCell);
      value := varNull;
      styleCell := nil;

      sheet.OnWriteCellData(sheet, r, c, value, styleCell);

      if VarIsNull(value) then
      begin
        // Local loop to count empty cells
        cc := c + 1;
        while (cc <= lastCol) do
        begin
          InitCell(ASheet, r, cc, lCell);
          value := varNull;
          styleCell := nil;
          sheet.OnWriteCellData(sheet, r, cc, value, styleCell);
          if not VarIsNull(value) then
            break;
          inc(cc);
        end;
        colsRepeated := cc - c;
        colsRepeatedStr := IfThen(colsRepeated = 1, '',
          Format('table:number-columns-repeated="%d"', [colsRepeated]));
        AppendToStream(AStream, Format(
          '<table:table-cell %s />', [colsRepeatedStr]));
      end else begin
        if VarIsNumeric(value) then
        begin
          lCell.ContentType := cctNumber;
          lCell.NumberValue := value;
        end else
        if VarType(value) = varDate then
        begin
          lCell.ContentType := cctDateTime;
          lCell.DateTimeValue := StrToDateTime(VarToStr(value), Workbook.FormatSettings);  // was: StrToDate
        end else
        if VarIsStr(value) then
        begin
          lCell.ContentType := cctUTF8String;
          lCell.UTF8StringValue := VarToStrDef(value, '');
        end else
        if VarIsBool(value) then
        begin
          lCell.ContentType := cctBool;
          lCell.BoolValue := value <> 0;
        end else
          lCell.ContentType := cctEmpty;
        WriteCellToStream(AStream, @lCell);
      end;
      inc(c, colsRepeated);
    end;

    AppendToStream(AStream,
        '</table:table-row>');

    // Next row
    inc(r, rowsRepeated);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Creates an XML string for inclusion of the wordwrap option into the
  written file from the wordwrap setting in the format cell.
  Is called from WriteStyles (via WriteStylesXMLAsString).
-------------------------------------------------------------------------------}
function TsSpreadOpenDocWriter.WriteWordwrapStyleXMLAsString(
  const AFormat: TsCellFormat): String;
begin
  if (uffWordWrap in AFormat.UsedFormattingFields) then
    Result := 'fo:wrap-option="wrap" '
  else
    Result := '';
end;

{@@ ----------------------------------------------------------------------------
  Writes a string formula
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteFormula(AStream: TStream; const ARow,
  ACol: Cardinal; ACell: PCell);
var
  lStyle: String = '';
  formula: PsFormula;
  formulaStr: String;
  valuetype: String;
  value: string;
  valueStr: String;
  colsSpannedStr: String;
  rowsSpannedStr: String;
  spannedStr: String;
  comment: String;
  r1,c1,r2,c2: Cardinal;
  ignoreFormulas: Boolean;
  sheet: TsWorksheet;
  oldDialect: TsFormulaDialect;
begin
  Unused(ARow, ACol);
  ignoreFormulas := (boIgnoreFormulas in FWorkbook.Options);

  sheet := FWorksheet as TsWorksheet;

  // Style
  lStyle := GetStyleName(ACell);
  if lStyle <> '' then
    lStyle := Format(' table:style-name="%s"', [lStyle]);

  // Comment
  comment := WriteCommentXMLAsString(sheet.ReadComment(ACell));

  // Merged?
  if sheet.IsMergeBase(ACell) then
  begin
    sheet.FindMergedRange(ACell, r1, c1, r2, c2);
    rowsSpannedStr := Format(' table:number-rows-spanned="%d"', [r2 - r1 + 1]);
    colsSpannedStr := Format(' table:number-columns-spanned="%d"', [c2 - c1 + 1]);
    spannedStr := colsSpannedStr + rowsSpannedStr;
  end else
    spannedStr := '';

  // Hyperlink
  if sheet.HasHyperlink(ACell) then
    FWorkbook.AddErrorMsg(rsODSHyperlinksOfTextCellsOnly, [GetCellString(ARow, ACol)]);

  // Formula string
  formula := sheet.Formulas.FindFormula(ACell);

  if ignoreFormulas then begin
    formulaStr := formula^.Text;
    if (formulaStr <> '') then begin
      if not ((pos('of:=', formulaStr) = 1) or (pos('=', formulaStr) = 1)) then
        formulaStr := 'of:=' + formulaStr;
    end;
  end else
  begin
    valueStr := '';
    if formula^.Parser = nil then begin
      formula^.Parser := TsSpreadsheetParser.Create(FWorksheet);
      formula^.Parser.Expression[fdExcelA1] := formula^.Text;  // the formula text is in ExcelA1 dialect
    end;
    // Convert string formula to the format needed by ods
    oldDialect := formula^.Parser.Dialect;
    try
      formulaStr := formula^.Parser.Expression[fdOpenDocument];  // Formula converted to ODS dialect
      if (formulaStr <> '') and (formulastr[1] <> '=') then
        formulaStr := '=' + formulaStr;
    finally
      formula^.Parser.Dialect := oldDialect;
    end;

    case ACell^.ContentType of
      cctNumber:
        begin
          valuetype := 'float';
          value := ' office:value="' + Format('%g', [ACell^.NumberValue], FPointSeparatorSettings) + '"';
        end;
      cctDateTime:
        if trunc(ACell^.DateTimeValue) = 0 then
        begin
          valuetype := 'time';
          value := ' office:time-value="' + FormatDateTime(ISO8601FormatTimeOnly, ACell^.DateTimeValue) + '"';
        end
        else
        begin
          valuetype := 'date';
          if frac(ACell^.DateTimeValue) = 0.0 then
            value := ' office:date-value="' + FormatDateTime(ISO8601FormatDateOnly, ACell^.DateTimeValue) + '"'
          else
            value := ' office:date-value="' + FormatDateTime(ISO8601FormatExtended, ACell^.DateTimeValue) + '"';
        end;
      cctUTF8String:
        begin
          valuetype := 'string';
          value := ' office:string-value="' + ACell^.UTF8StringValue +'"';
          valueStr := '<text:p>' + ACell^.UTF8StringValue + '</text:p>';
        end;
      cctBool:
        begin
          valuetype := 'boolean';
          value := ' office:boolean-value="' + BoolToStr(ACell^.BoolValue, 'true', 'false') + '"';
        end;
      cctError:
        if HasFormula(ACell) then
        begin
          // Open/LibreOffice always writes a float value 0 to the cell
          valuetype := 'float';                // error as result of a formula
          value := ' office:value="0"';
        end else
        begin
          valuetype := 'string" calcext:value-type="error';   // an error "constant"
          value := ' office:value=""';
        end;
    end;
  end;

  { Fix special xml characters }
  formulaStr := UTF8TextToXMLText(formulaStr);

  { We are writing a very rudimentary formula here without result and result
    data type. Seems to work... }
//  if not ignoreFormulas or (sheet.GetCalcState(ACell) = csCalculated) then
  if not ignoreFormulas or (formula^.CalcState = csCalculated) then              // LOOKS STRANGE - IS THIS CORRECT?
    AppendToStream(AStream, Format(
      '<table:table-cell table:formula="%s" office:value-type="%s"%s%s%s>' +
        comment +
        valueStr +
      '</table:table-cell>', [
      formulaStr, valuetype, value, lStyle, spannedStr
    ]))
  else
  begin
    AppendToStream(AStream, Format(
      '<table:table-cell table:formula="%s"%s%s', [
        formulaStr, lStyle, spannedStr]));
    if comment <> '' then
      AppendToStream(AStream, '>' + comment + '</table:table-cell>')
    else
      AppendToStream(AStream, '/>');
  end;
end;


{@@ ----------------------------------------------------------------------------
  Writes a cell with text content

  The UTF8 Text needs to be converted, because some chars are invalid in XML
  See bug with patch 19422
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteLabel(AStream: TStream; const ARow,
  ACol: Cardinal; const AValue: string; ACell: PCell);
var
  lStyle: string = '';
  colsSpannedStr: String;
  rowsSpannedStr: String;
  spannedStr: String;
  r1,c1,r2,c2: Cardinal;
  totaltxt, target, bookmark, comment: String;
  fnt: TsFont;
  fntName: String;
  hyperlink: PsHyperlink;
  u: TUri;
  i, idx, endidx, fntidx, len: Integer;
  rtParam: TsRichTextParam;
  wideStr, txt: WideString;
  ch: WideChar;
  sheet: TsWorksheet;

  function IsNewLine(var idx: Integer): Boolean;
  begin
    if (wideStr[idx] = #13) or (wideStr[idx] = #10) then
    begin
      Result := true;
      if (idx < len) and (
         ((wideStr[idx] = #13) and (wideStr[idx+1] = #10)) or
         ((wideStr[idx] = #10) and (wideStr[idx+1] = #13)) ) then inc(idx);
    end else
      Result := false;
  end;

  procedure AppendTxt(NewLine: Boolean; FntStyle: String);
  var
    s: String;
  begin
    s := UTF8Encode(txt);
    ValidXMLText(s);
    {
    if FntStyle <> '' then
      FntStyle := ' text:style-name="' + FntStyle + '"';
      }
    if NewLine and (s = '') then
      totaltxt := totaltxt + '</text:p><text:p>'
    else
    begin
      if FntStyle = '' then
        totaltxt := totaltxt + s
      else
        totaltxt := totaltxt +
          '<text:span text:style-name="' + FntStyle + '">' + s + '</text:span>';
      if NewLine then
        totaltxt := totaltxt + '</text:p><text:p>';
    end;
    txt := '';
  end;

begin
  Unused(ARow, ACol);

  sheet := FWorksheet as TsWorksheet;

  // Style
  lStyle := GetStyleName(ACell);
  if lStyle <> '' then
    lStyle := Format(' table:style-name="%s"', [lStyle]);

  // Comment
  comment := WriteCommentXMLAsString(sheet.ReadComment(ACell));

  // Merged?
  if sheet.IsMergeBase(ACell) then
  begin
    sheet.FindMergedRange(ACell, r1, c1, r2, c2);
    rowsSpannedStr := Format(' table:number-rows-spanned="%d"', [r2 - r1 + 1]);
    colsSpannedStr := Format(' table:number-columns-spanned="%d"', [c2 - c1 + 1]);
    spannedStr := colsSpannedStr + rowsSpannedStr;
  end else
    spannedStr := '';

  // Check for invalid characters, get the error message
  totaltxt := AValue;
  if not ValidXMLText(totaltxt) then
    Workbook.AddErrorMsg(
      rsInvalidCharacterInCell, [
      GetCellString(ARow, ACol)
    ]);

  // Hyperlink?
  if sheet.HasHyperlink(ACell) then
  begin
    hyperlink := sheet.FindHyperlink(ACell);
    SplitHyperlink(hyperlink^.Target, target, bookmark);

    if (target <> '') and (pos('file:', target) = 0) then
    begin
      u := ParseURI(target);
      if u.Protocol = '' then
        target := '../' + target;
    end;

    // ods absolutely wants "/" path delimiters in the file uri!
    FixHyperlinkPathdelims(target);

    if (bookmark <> '') then
      target := target + '#' + bookmark;

    totaltxt := Format(
      '<text:p>'+
        '<text:a xlink:href="%s" xlink:type="simple">%s</text:a>'+
      '</text:p>', [target, totaltxt]);
  end
  else
  begin
    // No hyperlink, normal text only
    if Length(ACell^.RichTextParams) = 0 then
    begin
      // Standard text formatting
      (*
      { ods writes "<text:line-break/>" nodes for line-breaks. BUT:
        LibreOffice Calc fails to detect these during reading.
        OpenOffice Calc and Excel are ok.
        Therefore, we skip this part until LO gets fixed. }

      wideStr := UTF8Decode(AValue);
      len := Length(wideStr);
      idx := 1;
      totaltxt := '<text:p>';
      while idx <= len do
      begin
        ch := widestr[idx];
        totaltxt := totaltxt + IfThen(IsNewLine(idx), '<text:line-break />', ch);
        inc(idx);
      end;
      totaltxt := totaltxt + '</text:p>';
      *)
      totaltxt := '<text:p>' + totaltxt + '</text:p>' ;  // has &#13; and &#10; for line breaks
    end else
    begin
      // "Rich-text" formatting
      wideStr := UTF8Decode(AValue);  // Convert to unicode
      // Before the first formatted section having the cell's format
      len := Length(wideStr);
      totaltxt := '<text:p>';
      rtParam := ACell^.RichTextParams[0];
      idx := 1;
      txt := '';
      if rtParam.FirstIndex > 1 then
      begin
        while (idx <= len) and (idx < rtParam.FirstIndex) do
        begin
          ch := wideStr[idx];
          if IsNewLine(idx) then
            AppendTxt(true, '')
          else
            txt := txt + ch;
          inc(idx);
        end;
        if txt <> '' then
          AppendTxt(false, '');
      end;
      txt := '';
      for i := 0 to High(ACell^.RichTextParams) do
      begin
        // Formatted parts of the string according the RichTextParams
        rtParam := ACell^.RichTextParams[i];
        fnt := (FWorkbook as TsWorkbook).GetFont(rtParam.FontIndex);
        fntidx := FRichTextFontList.IndexOfObject(fnt);
        fntName := FRichTextFontList[fntIdx];
        if i < High(ACell^.RichTextParams) then
          endidx := ACell^.RichTextParams[i+1].FirstIndex-1 else
          endidx := len;
        while (idx <= len) and (idx <= endidx) do
        begin
          ch := wideStr[idx];
          if IsNewLine(idx) then
            AppendTxt(true, fntName)
          else
            txt := txt + ch;
          inc(idx);
        end;
        if txt <> '' then
          AppendTxt(false, fntName);
      end;
      totaltxt := totaltxt + '</text:p>';
    end;
  end;

  // Write it ...
  AppendToStream(AStream, Format(
    '<table:table-cell office:value-type="string"%s%s>' +
      comment +
      totaltxt +
    '</table:table-cell>', [
    lStyle, spannedStr
  ]));
end;

procedure TsSpreadOpenDocWriter.WriteNumber(AStream: TStream; const ARow,
  ACol: Cardinal; const AValue: double; ACell: PCell);
var
  StrValue: string;
  DisplayStr: string;
  lStyle: string = '';
  valType: String;
  colsSpannedStr: String;
  rowsSpannedStr: String;
  spannedStr: String;
  comment: String;
  r1,c1,r2,c2: Cardinal;
  fmt: TsCellFormat;
  numFmt: TsNumFormatParams;
  nfSection: TsNumFormatSection;
begin
  Unused(ARow, ACol);

  valType := 'float';

  lStyle := GetStyleName(ACell);
  if lStyle <> '' then
    lStyle := Format(' table:style-name="%s"', [lStyle]);

  fmt := (FWorkbook as TsWorkbook).GetCellFormat(ACell^.FormatIndex);
  if fmt.UsedFormattingFields <> [] then
  begin
    numFmt := (FWorkbook as TsWorkbook).GetNumberFormat(fmt.NumberFormatIndex);
    if (numFmt <> nil) then begin
      if (Length(numFmt.Sections) > 1) and (AValue < 0) then
        nfSection := numFmt.Sections[1]
      else
      if (Length(numFmt.Sections) > 2) and (AValue = 0) then
        nfSection := NumFmt.Sections[2]
      else
        nfSection := numFmt.Sections[0];
      if (nfkPercent in nfSection.Kind) then
        valType := 'percentage'
      else
      if (nfkCurrency in nfSection.Kind) then
        valtype := 'currency'
    end;
  end;

  // Comment
  comment := WriteCommentXMLAsString((FWorksheet as TsWorksheet).ReadComment(ACell));

  // Merged?
  if (FWorksheet as TsWorksheet).IsMergeBase(ACell) then
  begin
    (FWorksheet as TsWorksheet).FindMergedRange(ACell, r1, c1, r2, c2);
    rowsSpannedStr := Format(' table:number-rows-spanned="%d"', [r2 - r1 + 1]);
    colsSpannedStr := Format(' table:number-columns-spanned="%d"', [c2 - c1 + 1]);
    spannedStr := colsSpannedStr + rowsSpannedStr;
  end else
    spannedStr := '';

  // Displayed value
  if IsInfinite(AValue) then
  begin
    StrValue := '1.#INF';
    DisplayStr := '1.#INF';
  end else begin
    StrValue := FloatToStr(AValue, FPointSeparatorSettings); // Uses '.' as decimal separator
    DisplayStr := (FWorksheet as TsWorksheet).ReadAsText(ACell); //FloatToStr(AValue); // Uses locale decimal separator
  end;

  // Hyperlink
  if FWorksheet.HasHyperlink(ACell) then
    FWorkbook.AddErrorMsg(rsODSHyperlinksOfTextCellsOnly, [GetCellString(ARow, ACol)]);

  AppendToStream(AStream, Format(
    '<table:table-cell office:value-type="%s" office:value="%s"%s%s >' +
      comment +
      '<text:p>%s</text:p>' +
    '</table:table-cell>', [
    valType, StrValue, lStyle, spannedStr,
    DisplayStr
  ]));
end;

{@@ ----------------------------------------------------------------------------
  Writes a date/time value
-------------------------------------------------------------------------------}
procedure TsSpreadOpenDocWriter.WriteDateTime(AStream: TStream;
  const ARow, ACol: Cardinal; const AValue: TDateTime; ACell: PCell);
const
  DATE_FMT: array[boolean] of string = (ISO8601FormatExtended, ISO8601FormatTimeOnly);
  DT: array[boolean] of string = ('date', 'time');
  // Index "boolean" is to be understood as "isTimeOnly"
var
  lStyle: string;
  strValue: String;
  displayStr: String;
  isTimeOnly: Boolean;
  colsSpannedStr: String;
  rowsSpannedStr: String;
  spannedStr: String;
  comment: String;
  r1,c1,r2,c2: Cardinal;
  fmt: TsCellFormat;
  numFmtParams: TsNumFormatParams;
  h,m,s,ms: Word;
  sheet: TsWorksheet;
begin
  Unused(ARow, ACol);

  sheet := FWorksheet as TsWorksheet;

  // Merged?
  if sheet.IsMergeBase(ACell) then
  begin
    sheet.FindMergedRange(ACell, r1, c1, r2, c2);
    colsSpannedStr := Format(' table:number-columns-spanned="%d"', [c2 - c1 + 1]);
    rowsSpannedStr := Format(' table:number-rows-spanned="%d"', [r2 - r1 + 1]);
    spannedStr := colsSpannedStr + rowsSpannedStr;
  end else
    spannedStr := '';

  // Style
  lStyle := GetStyleName(ACell);
  if lStyle <> '' then
    lStyle := Format(' table:style-name="%s"', [lStyle]);

  fmt := (FWorkbook as TsWorkbook).GetCellFormat(ACell^.FormatIndex);
  numFmtParams := (FWorkbook as TsWorkbook).GetNumberFormat(fmt.NumberFormatIndex);

  // Comment
  comment := WriteCommentXMLAsString(sheet.ReadComment(ACell));

  // Hyperlink
  if FWorksheet.HasHyperlink(ACell) then
    FWorkbook.AddErrorMsg(rsODSHyperlinksOfTextCellsOnly, [GetCellString(ARow, ACol)]);

  // nfTimeInterval is a special case - let's handle it first:

  if IsTimeIntervalformat(numFmtParams) then
  begin
    DecodeTime(AValue, h,m,s,ms);
    strValue := Format('PT%.2dH%.2dM%.2d.%.3dS', [trunc(AValue)*24+h, m, s, ms], FPointSeparatorSettings);
//    strValue := FormatDateTime(ISO8601FormatHoursOverflow, AValue, [fdoInterval]);
    displayStr := sheet.ReadAsText(ACell);
//    displayStr := FormatDateTime(fmt.NumberFormatStr, AValue, [fdoInterval]);
    AppendToStream(AStream, Format(
      '<table:table-cell office:value-type="time" office:time-value="%s"%s%s>' +
        comment +
        '<text:p>%s</text:p>' +
      '</table:table-cell>', [
      strValue, lStyle, spannedStr,
      displayStr
    ]));
  end else
  begin
    // We have to distinguish between time-only values and values that contain date parts.
    if (numFmtParams <> nil) then
      isTimeOnly := Assigned(numFmtParams) and (numFmtParams.Sections[0].Kind * [nfkDate, nfkTime] = [nfkTime])
    else
      isTimeOnly := false;
    // ODS wants the date/time in the ISO format.
    strValue := FormatDateTime(DATE_FMT[isTimeOnly], AValue);
    // Add milliseconds; they must be appended as decimals to the seconds.
    if Assigned(numFmtParams) and (nfkTime in numFmtParams.Sections[0].Kind) and
       (numFmtParams.Sections[0].Decimals > 0) then
    begin
      strValue[Length(strValue)] := '.';  // replace trailing 'S' by '.'
      // add value of milliseconds, rounded to required decimal places
      DecodeTime(AValue, h,m,s,ms);
      case numFmtParams.Sections[0].Decimals of
        1: strValue := strValue + FormatFloat('0', round(ms/100));
        2: strValue := strValue + FormatFloat('00', round(ms/10));
        3: strValue := strValue + FormatFloat('000', ms);
      end;
      strValue := strValue + 'S';
    end;
    displayStr := sheet.ReadAsText(ACell);
    AppendToStream(AStream, Format(
      '<table:table-cell office:value-type="%s" office:%s-value="%s" %s %s>' +
        comment +
        '<text:p>%s</text:p> ' +
      '</table:table-cell>', [
      DT[isTimeOnly], DT[isTimeOnly], strValue, lStyle, spannedStr,
      displayStr
    ]));
  end;
end;


initialization

  // Registers this reader / writer in fpSpreadsheet
  sfidOpenDocument := RegisterSpreadFormat(sfOpenDocument,
    TsSpreadOpenDocReader, TsSpreadOpenDocWriter,
    STR_FILEFORMAT_OPENDOCUMENT, 'ODS', [STR_OPENDOCUMENT_CALC_EXTENSION]
  );

end.

