{ Dbc script independent ResultSet

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

unit UAsyncResultSet;

interface

{$mode objfpc}{$H+}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}FmtBCD,
  ZDbcResultSetMetadata,ZClasses, ZDbcIntfs, ZDbcResultSet, ZDbcCache,
  ZCompatibility,ZVariant;

type
  TZIndexPairList=ZDbcCache.TZIndexPairList;

  TZCustomAsyncResultSet=class;

  TZAsyncResultSetMetadata = object
   private
    FResultSet:TZCustomAsyncResultSet;
   public
    function FindColumn(const ColumnName: string): Integer;

    function GetColumnCount: Integer;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean;
    function IsCaseSensitive(ColumnIndex: Integer): Boolean;
    function IsSearchable(ColumnIndex: Integer): Boolean;
    function IsCurrency(ColumnIndex: Integer): Boolean;
    function IsNullable(ColumnIndex: Integer): TZColumnNullableType;

    function IsSigned(ColumnIndex: Integer): Boolean;
    function GetColumnLabel(ColumnIndex: Integer): string;
    function GetOrgColumnLabel(ColumnIndex: Integer): string;
    function GetColumnName(ColumnIndex: Integer): string;
    function GetColumnCodePage(ColumnIndex: Integer): Word;
    function GetSchemaName(ColumnIndex: Integer): string;
    function GetPrecision(ColumnIndex: Integer): Integer;
    function GetScale(ColumnIndex: Integer): Integer;
    function GetTableName(ColumnIndex: Integer): string;
    function GetCatalogName(ColumnIndex: Integer): string;
    function GetColumnType(ColumnIndex: Integer): TZSQLType;
    function GetColumnTypeName(ColumnIndex: Integer): string;
    function IsReadOnly(ColumnIndex: Integer): Boolean;
    function IsWritable(ColumnIndex: Integer): Boolean;
    function IsDefinitelyWritable(ColumnIndex: Integer): Boolean;
    function GetDefaultValue(ColumnIndex: Integer): string;
    function HasDefaultValue(ColumnIndex: Integer): Boolean;
  end;

  TZCustomAsyncResultSet = class
  private
    Const
     FCachedLobs=true;
    var
     FConSettings: PZConSettings;
     FLastRowNo: Integer;
     FMaxRows: Integer;
     FClosed: Boolean;
     FColumnsInfo: TObjectList;

     FSelectedRow: PZRowBuffer;
     FRowAccessor: TZRowAccessor;
     FNextRowIndex: Integer;
  protected
    FRowNo: Integer;
    LastWasNull: Boolean;

    procedure CheckBlobColumn(ColumnIndex: Integer);
    procedure Open; virtual;

    function GetColumnIndex(const ColumnName: string): Integer;
    property RowNo: Integer read FRowNo write FRowNo;
    property LastRowNo: Integer read FLastRowNo write FLastRowNo;
    property MaxRows: Integer read FMaxRows write FMaxRows;
    property Closed: Boolean read FClosed write FClosed;

    class function GetRowAccessorClass: TZRowAccessorClass; virtual;
    procedure CheckAvailable;
    function GetNextRowIndex: Integer; virtual;

    property SelectedRow: PZRowBuffer read FSelectedRow write FSelectedRow;
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
    property NextRowIndex: Integer read FNextRowIndex write FNextRowIndex;
  public

    constructor Create(FResultSet:IZResultSet);
    destructor Destroy; override;

    function GetConSettings: PZConSettings;

    function  Next: Boolean; virtual;
    procedure BeforeClose; virtual;
    procedure Close; virtual;
    function  WasNull: Boolean;
    function  IsClosed: Boolean;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function GetBytes(ColumnIndex: Integer): TBytes;
    function GetAsciiStream(ColumnIndex: Integer): TStream; virtual;
    function GetAnsiStream(ColumnIndex: Integer): TStream; virtual;
    function GetUnicodeStream(ColumnIndex: Integer): TStream; virtual;
    function GetUTF8Stream(ColumnIndex: Integer): TStream; virtual;
    function GetBinaryStream(ColumnIndex: Integer): TStream; virtual;

    function GetMetaData: TZAsyncResultSetMetadata; virtual;
    function FindColumn(const ColumnName: string): Integer; virtual;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function IsBeforeFirst: Boolean; virtual;
    function IsAfterLast: Boolean; virtual;
    function IsFirst: Boolean; virtual;
    function IsLast: Boolean; virtual;
    procedure BeforeFirst; virtual;
    procedure AfterLast; virtual;
    function First: Boolean; virtual;
    function Last: Boolean; virtual;
    function GetRow: NativeInt; virtual;
    function MoveRelative(Rows: Integer): Boolean; virtual;
    function Previous: Boolean; virtual;

    property ColumnsInfo: TObjectList read FColumnsInfo write FColumnsInfo;

    //-------------------

    procedure AfterClose;  virtual;
    procedure ResetCursor; virtual;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean; virtual;
    function GetValue(ColumnIndex: Integer): TZVariant; virtual;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
    function GetString(ColumnIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetUnicodeString(ColumnIndex: Integer): ZWidestring;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte;
    procedure GetDate(ColumnIndex: Integer; Var Result: TZDate); overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); overload;
    procedure GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp); overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    function GetDefaultExpression(ColumnIndex: Integer): string;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function MoveAbsolute(Row: Integer): Boolean; virtual;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean;  virtual;
    function RowInserted: Boolean; virtual;
    function RowDeleted: Boolean;  virtual;

    procedure UpdateNull(ColumnIndex: Integer);
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure UpdateByte(ColumnIndex: Integer; Value: Byte);
    procedure UpdateShort(ColumnIndex: Integer; Value: ShortInt);
    procedure UpdateWord(ColumnIndex: Integer; Value: Word);
    procedure UpdateSmall(ColumnIndex: Integer; Value: SmallInt);
    procedure UpdateUInt(ColumnIndex: Integer; Value: Cardinal);
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer);
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64);
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64);
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single);
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double);
    procedure UpdateCurrency(ColumnIndex: Integer; const Value: Currency);
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: TBCD);
    procedure UpdateGUID(ColumnIndex: Integer; const Value: TGUID);
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; var Len: NativeUint); overload;
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar; var Len: NativeUint); overload;
    procedure UpdateString(ColumnIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiString(ColumnIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8String(ColumnIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure UpdateRawByteString(ColumnIndex: Integer; const Value: RawByteString);
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString);
    procedure UpdateBytes(ColumnIndex: Integer; Value: PByte; var Len: NativeUInt); overload;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TZDate); overload;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TZTime); overload;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TZTimeStamp); overload;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateLob(ColumnIndex: Integer; const Value: IZBlob);
    procedure UpdateDefaultExpression(ColumnIndex: Integer; const Value: string);

    procedure InsertRow;  virtual;
    procedure DeleteRow;  virtual;

    procedure MoveToCurrentRow; virtual;

    //---------------------------------------------------------------------
    // Cached Updates
    //---------------------------------------------------------------------

    function CreateLob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode): IZBlob; virtual;
  end;

  TZAsyncResultSet = class(TZCustomAsyncResultSet)
   private
    FRowsList:TZSortedList;
   protected
    function  LocateRow(RowsList: TZSortedList; RowIndex: Integer): Integer;
    function  AppendRow(Row: PZRowBuffer): PZRowBuffer;
    procedure Open; override;

    property  RowsList: TZSortedList read FRowsList write FRowsList;
   public
    procedure AfterClose;  override;
    procedure ResetCursor; override;
    function  MoveAbsolute(Row: Integer): Boolean; override;
    procedure InsertRow;  override;
    procedure DeleteRow;  override;
    function  Fetch(FResultSet:IZResultSet;FIndexPairList:TZIndexPairList):Boolean;
  end;

function  _NewIndexPair(Count:Integer):TZIndexPairList;
function  _FindColumn(FColumnsInfo:TObjectList;const ColumnName:string):Integer;
procedure _LoadColumnInfo(FColumnsInfo:TObjectList;FResultSet:IZResultSet);
function  _FetchRow(FRowAccessor:TZRowAccessor;FIndexPairList:TZIndexPairList;FResultSet:IZResultSet):PZRowBuffer;

implementation

uses ZMessages, ZDbcUtils, ZEncoding, ZDbcProperties,
  ZSysUtils;

function _FindColumn(FColumnsInfo:TObjectList;const ColumnName:string):Integer;
var
  I: Integer;
  ColumnNameUpper: string;
begin
  { Search for case sensitive columns. }
  for I := 0 to FColumnsInfo.Count-1 do
    if TZColumnInfo(FColumnsInfo[I]).ColumnName = ColumnName then
    begin
      Result := I+FirstDbcIndex;
      Exit;
    end;

  { Search for case insensitive columns. }
  ColumnNameUpper := AnsiUpperCase(ColumnName);
  for I := 0 to FColumnsInfo.Count-1 do
    if AnsiUpperCase(TZColumnInfo(FColumnsInfo[I]).ColumnName) = ColumnNameUpper then
    begin
      Result := I+FirstDbcIndex;
      Exit;
    end;

  Result := InvalidDbcIndex;
end;

function    TZAsyncResultSetMetadata.FindColumn(const ColumnName: string): Integer;
begin
 Result:=_FindColumn(FResultSet.FColumnsInfo,ColumnName);
end;

function    TZAsyncResultSetMetadata.GetColumnCount: Integer;
begin
  Result := FResultSet.ColumnsInfo.Count;
end;

function    TZAsyncResultSetMetadata.IsAutoIncrement(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).AutoIncrement;
end;

function    TZAsyncResultSetMetadata.IsCaseSensitive(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).CaseSensitive;
end;

function    TZAsyncResultSetMetadata.IsSearchable(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).Searchable;
end;

function    TZAsyncResultSetMetadata.IsCurrency(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).Currency;
end;

function    TZAsyncResultSetMetadata.IsNullable(ColumnIndex: Integer): TZColumnNullableType;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).Nullable;
end;

function    TZAsyncResultSetMetadata.IsSigned(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).Signed;
end;

function    TZAsyncResultSetMetadata.GetColumnLabel(ColumnIndex: Integer): string;
begin
 Result:=TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).ColumnLabel;
end;

function    TZAsyncResultSetMetadata.GetOrgColumnLabel(ColumnIndex: Integer): string;
begin
  Result := GetColumnLabel(ColumnIndex);
end;

function    TZAsyncResultSetMetadata.GetColumnName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).ColumnName;
end;

function    TZAsyncResultSetMetadata.GetColumnCodePage(ColumnIndex: Integer): Word;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).ColumnCodePage;
end;

function    TZAsyncResultSetMetadata.GetSchemaName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).SchemaName;
end;

function    TZAsyncResultSetMetadata.GetPrecision(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).Precision;
end;

function    TZAsyncResultSetMetadata.GetScale(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).Scale;
end;

function    TZAsyncResultSetMetadata.GetTableName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).TableName;
end;

function    TZAsyncResultSetMetadata.GetCatalogName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).CatalogName;
end;

function    TZAsyncResultSetMetadata.GetColumnType(ColumnIndex: Integer): TZSQLType;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).ColumnType;
end;

function    TZAsyncResultSetMetadata.GetColumnTypeName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).GetColumnTypeName;
end;

function    TZAsyncResultSetMetadata.IsReadOnly(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).ReadOnly;
end;

function    TZAsyncResultSetMetadata.IsWritable(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).Writable;
end;

function    TZAsyncResultSetMetadata.IsDefinitelyWritable(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).DefinitelyWritable;
end;

function    TZAsyncResultSetMetadata.GetDefaultValue(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).DefaultValue;
end;

function    TZAsyncResultSetMetadata.HasDefaultValue(ColumnIndex: Integer): Boolean;
begin
  Result := not(TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex+InvalidDbcIndex]).DefaultValue = '');
end;

procedure TZCustomAsyncResultSet.CheckBlobColumn(ColumnIndex: Integer);
var
  InitialType: TZSQLType;
begin
  if (FColumnsInfo = nil) or (ColumnIndex < FirstDbcIndex) or
     (ColumnIndex > FColumnsInfo.Count+InvalidDbcIndex) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  InitialType := TZColumnInfo(FColumnsInfo[ColumnIndex+InvalidDbcIndex]).ColumnType;
  if not (InitialType in [stAsciiStream, stBinaryStream, stUnicodeStream]) then
    raise EZSQLException.Create(Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(InitialType)]));
end;


function TZCustomAsyncResultSet.GetColumnIndex(const ColumnName: string): Integer;
begin
  Result := FindColumn(ColumnName);

  if Result = InvalidDbcIndex then
    raise EZSQLException.Create(Format(SColumnWasNotFound, [ColumnName]));
end;

function TZCustomAsyncResultSet.FindColumn(const ColumnName: string): Integer;
begin
 Result:=_FindColumn(FColumnsInfo,ColumnName);
end;

function TZCustomAsyncResultSet.GetConSettings: PZConSettings;
begin
 Result:=FConSettings;
end;

function TZCustomAsyncResultSet.Next: Boolean;
begin
  Result := MoveAbsolute(FRowNo + 1);
end;

function TZCustomAsyncResultSet.IsBeforeFirst: Boolean;
begin
  Result := (FRowNo = 0);
end;

function TZCustomAsyncResultSet.IsClosed: Boolean;
begin
  Result := fClosed;
end;

function TZCustomAsyncResultSet.IsAfterLast: Boolean;
begin
  Result := (FRowNo > FLastRowNo);
end;

function TZCustomAsyncResultSet.IsFirst: Boolean;
begin
  Result := (FRowNo = 1);
end;

function TZCustomAsyncResultSet.IsLast: Boolean;
begin
  Result := (FRowNo = FLastRowNo);
end;

procedure TZCustomAsyncResultSet.BeforeClose;
begin
  ResetCursor;
end;

procedure TZCustomAsyncResultSet.BeforeFirst;
begin
  MoveAbsolute(0);
end;

procedure TZCustomAsyncResultSet.AfterLast;
begin
  Last;
  Next;
end;

function TZCustomAsyncResultSet.First: Boolean;
begin
  Result := MoveAbsolute(1);
end;

function TZCustomAsyncResultSet.Last: Boolean;
begin
  Result := MoveAbsolute(FLastRowNo);
end;

function TZCustomAsyncResultSet.GetRow: NativeInt;
begin
  Result := FRowNo;
end;

function TZCustomAsyncResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := MoveAbsolute(FRowNo + Rows);
end;

function TZCustomAsyncResultSet.Previous: Boolean;
begin
  Result := MoveAbsolute(FRowNo - 1);
end;

procedure TZCustomAsyncResultSet.Close;
begin
  if not Closed then
  begin
    BeforeClose;
    FClosed := True;
    AfterClose;
  end;
end;

function TZCustomAsyncResultSet.WasNull: Boolean;
begin
  Result := LastWasNull;
end;


function TZCustomAsyncResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var P: PByte;
  L: NativeUInt;
begin
  P := GetBytes(ColumnIndex, L);
  if (P <> nil) and (L > 0) then begin
    SetLength(Result, L);
    Move(P^, Pointer(Result)^, L);
  end else
    Result := nil;
end;

function TZCustomAsyncResultSet.GetAsciiStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
    Clob: IZCLob;
    CP: Word;
begin
  Result := nil;
  if IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) then
      Blob.QueryInterface(IZCLob, Clob);
      if Clob = nil then
        Result := (Blob).GetStream
      else begin
        CP := TZColumnInfo(ColumnsInfo[ColumnIndex+InvalidDbcIndex]).ColumnCodePage;
        if CP = zCP_UTF16 then
          CP := GetW2A2WConversionCodePage(FConSettings);
        Result := Clob.GetStream(CP)
      end;
    LastWasNull := (Result = nil);
  end;
end;

function TZCustomAsyncResultSet.GetAnsiStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
  CLob: IZCLob;
begin
  Result := nil;
  if IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) and Supports(Blob, IZClob, CLob)
    then Result := Clob.GetStream(zOSCodePage)
    else Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

function TZCustomAsyncResultSet.GetUnicodeStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
    CLob: IZCLob;
begin
  Result := nil;
  if IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) and Supports(Blob, IZClob, CLob)
    then Result := Clob.GetStream(zCP_UTF16)
    else Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

function TZCustomAsyncResultSet.GetUTF8Stream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
    CLob: IZCLob;
begin
  Result := nil;
  if IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) and Supports(Blob, IZClob, CLob)
    then Result := Clob.GetStream(zCP_UTF8)
    else Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

function TZCustomAsyncResultSet.GetBinaryStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
begin
  Result := nil;
  if IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) then
      Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

function TZCustomAsyncResultSet.GetMetaData: TZAsyncResultSetMetadata;
begin
 Result.FResultSet:=Self;
end;

{ TZCustomAsyncResultSet }

function TZCustomAsyncResultSet.CreateLob(ColumnIndex: Integer;LobStreamMode: TZLobStreamMode): IZBlob;
var SQLType: TZSQLType;
  DataAddress: Pointer;
  FIsNull: Boolean;
  CP: Word;
label Fail;
begin

    DataAddress := RowAccessor.GetColumnData(ColumnIndex, FIsNull);
    SQLType := RowAccessor.GetColumnType(ColumnIndex);
    CP := RowAccessor.GetColumnCodePage(ColumnIndex);
    case SQLType of
      stBytes: if RowAccessor.GetColumnLength( ColumnIndex) <= 0
          then Result := TZRowAccessorBytesLob.CreateWithDataAddess(DataAddress, zCP_Binary, FConSettings, nil)
          else goto Fail;
      stString, stUnicodeString: if RowAccessor.GetColumnLength( ColumnIndex) <= 0 then
          if CP = zCP_UTF16
          then Result := TZRowAccessorUnicodeStringLob.CreateWithDataAddess(DataAddress, CP, FConSettings, nil)
          else Result := TZRowAccessorRawByteStringLob.CreateWithDataAddess(DataAddress, CP, FConSettings, nil)
        else goto Fail;
      stAsciiStream, stUnicodeStream: begin
          Result := TZLocalMemCLob.Create(CP, FConSettings, nil);
          PIZLob(DataAddress)^ := Result;
          if FIsNull then
            PByte(PAnsiChar(DataAddress)-1)^ := 1;
        end;
      stBinaryStream: begin
          Result := TZLocalMemBLob.Create(nil);
          PIZLob(DataAddress)^ := Result;
          if FIsNull then
            PByte(PAnsiChar(DataAddress)-1)^ := 1;
        end
      else
Fail:   raise CreateCanNotAccessBlobRecordException(ColumnIndex, SQLType);
    end;

  Result.Open(LobStreamMode);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZCustomAsyncResultSet.Destroy;
begin
 if not FClosed then Close;
 FreeAndNil(FColumnsInfo);
 Finalize(FConSettings^.ClientCodePage^);
 Finalize(FConSettings^);
 FreeMem(FConSettings);
 FConSettings:=nil;
end;

{**
  Checks for availability of the cached buffer.
}
procedure TZCustomAsyncResultSet.CheckAvailable;
begin
  if (FRowAccessor = nil) or (FRowAccessor.RowBuffer = nil) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
end;

{**
  Generates the next row index value.
  @return the new generated row index.
}
function TZCustomAsyncResultSet.GetNextRowIndex: Integer;
begin
  Result := FNextRowIndex;
  Inc(FNextRowIndex);
end;


class function TZCustomAsyncResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZRowAccessor;
end;

{**
  Opens this recordset.
}
procedure TZCustomAsyncResultSet.Open;
begin
  if not Closed then
    raise EZSQLException.Create(SResultsetIsAlreadyOpened);

  FRowAccessor := GetRowAccessorClass.Create(ColumnsInfo, FConSettings, nil, FCachedLobs);

  FSelectedRow := nil;

  FNextRowIndex := 0;

  FClosed:=False;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZCustomAsyncResultSet.AfterClose;
begin
  FColumnsInfo.Clear;
  if Assigned(FRowAccessor) then
  begin
    FSelectedRow := nil;
    FreeAndNil(FRowAccessor);
  end;
end;

procedure TZCustomAsyncResultSet.ResetCursor;
begin
 if not FClosed then
 begin
   FRowNo := 0;
   FLastRowNo := 0;
   LastWasNull := True;
 end;
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}

function TZCustomAsyncResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  Result := FRowAccessor.IsNull(ColumnIndex);
end;

function TZCustomAsyncResultSet.GetValue(ColumnIndex: Integer): TZVariant;
begin
 Result := FRowAccessor.GetValue(ColumnIndex);
end;

function TZCustomAsyncResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
  Result := FRowAccessor.GetPAnsiChar(ColumnIndex, LastWasNull, Len);
end;

function TZCustomAsyncResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
begin
  Result := FRowAccessor.GetPWideChar(ColumnIndex, LastWasNull, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZCustomAsyncResultSet.GetString(ColumnIndex: Integer): String;
begin
  Result := FRowAccessor.GetString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZCustomAsyncResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
begin
  Result := FRowAccessor.GetAnsiString(ColumnIndex, LastWasNull);
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZCustomAsyncResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
begin
  Result := FRowAccessor.GetUTF8String(ColumnIndex, LastWasNull);
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZCustomAsyncResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
begin
  Result := FRowAccessor.GetRawByteString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Widestring</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZCustomAsyncResultSet.GetUnicodeString(ColumnIndex: Integer): ZWidestring;
begin
  Result := FRowAccessor.GetUnicodeString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZCustomAsyncResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  Result := FRowAccessor.GetBoolean(ColumnIndex, LastWasNull);
end;

{**
  Gets the address of value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len return the length of the addressed buffer
  @return the adressed column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZCustomAsyncResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
  Result := FRowAccessor.GetBytes(ColumnIndex, LastWasNull, len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZCustomAsyncResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  Result := FRowAccessor.GetUInt(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZCustomAsyncResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  Result := FRowAccessor.GetInt(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZCustomAsyncResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
  Result := FRowAccessor.GetULong(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZCustomAsyncResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  Result := FRowAccessor.GetLong(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZCustomAsyncResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := FRowAccessor.GetFloat(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UUID</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>Zero-UUID</code>
}
procedure TZCustomAsyncResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
begin
  FRowAccessor.GetGUID(ColumnIndex, Result, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZCustomAsyncResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  Result := FRowAccessor.GetDouble(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZCustomAsyncResultSet.GetCurrency(ColumnIndex: Integer): Currency;
begin
  Result := FRowAccessor.GetCurrency(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZCustomAsyncResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
begin
  FRowAccessor.GetBigDecimal(ColumnIndex, Result, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZCustomAsyncResultSet.GetDate(ColumnIndex: Integer; Var Result: TZDate);
begin
  FRowAccessor.GetDate(ColumnIndex, LastWasNull, Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZCustomAsyncResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
begin
  FRowAccessor.GetTime(ColumnIndex, LastWasNull, Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
procedure TZCustomAsyncResultSet.GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp);
begin
  FRowAccessor.GetTimestamp(ColumnIndex, LastWasNull, Result);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZCustomAsyncResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
begin
  Result := FRowAccessor.GetBlob(ColumnIndex, LastWasNull);
  if (Result = nil) and (LobStreamMode <> lsmRead) then
    Result := CreateLob(ColumnIndex, LobStreamMode);
end;

{**
  Gets the DefaultExpression value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the DefaultExpression value
}
function TZCustomAsyncResultSet.GetDefaultExpression(ColumnIndex: Integer): string;
begin
  Result := FRowAccessor.GetColumnDefaultExpression(ColumnIndex);
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a nullable column a null value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code>
  or <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZCustomAsyncResultSet.UpdateNull(ColumnIndex: Integer);
begin
  FRowAccessor.SetNull(ColumnIndex);
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateBoolean(ColumnIndex: Integer;
  Value: Boolean);
begin
  FRowAccessor.SetBoolean(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateByte(ColumnIndex: Integer;
  Value: Byte);
begin
  FRowAccessor.SetByte(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>byte</code> array value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the address of new column value
  @param Len the length of the addressed value
}
procedure TZCustomAsyncResultSet.UpdateBytes(ColumnIndex: Integer;
  Value: PByte; var Len: NativeUInt);
begin
  FRowAccessor.SetBytes(ColumnIndex, Value, Len);
end;

{**
  Updates the designated column with a <code>smallint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateShort(ColumnIndex: Integer;
  Value: ShortInt);
begin
  FRowAccessor.SetShort(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>word</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateWord(ColumnIndex: Integer;
  Value: Word);
begin
  FRowAccessor.SetWord(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>smallint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateSmall(ColumnIndex: Integer;
  Value: SmallInt);
begin
  FRowAccessor.SetSmall(ColumnIndex, Value);
end;

{**
  Updates the designated column with an <code>uint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateUInt(ColumnIndex: Integer;
  Value: Cardinal);
begin
  FRowAccessor.SetUInt(ColumnIndex, Value);
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateInt(ColumnIndex: Integer;
  Value: Integer);
begin
  FRowAccessor.SetInt(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>ulong</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateULong(ColumnIndex: Integer;
  const Value: UInt64);
begin
  FRowAccessor.SetULong(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateLong(ColumnIndex: Integer;
  const Value: Int64);
begin
  FRowAccessor.SetLong(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>float</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateFloat(ColumnIndex: Integer;
  Value: Single);
begin
  FRowAccessor.SetFloat(ColumnIndex, Value);
end;

procedure TZCustomAsyncResultSet.UpdateGUID(ColumnIndex: Integer;
  const Value: TGUID);
begin
  FRowAccessor.SetGUID(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateDouble(ColumnIndex: Integer;
  const Value: Double);
begin
  FRowAccessor.SetDouble(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>currency</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateCurrency(ColumnIndex: Integer;
  const Value: Currency);
begin
  FRowAccessor.SetCurrency(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateBigDecimal(ColumnIndex: Integer;
  const Value: TBCD);
begin
  FRowAccessor.SetBigDecimal(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>TZAnsiRec</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdatePAnsiChar(ColumnIndex: Integer;
  Value: PAnsiChar; var Len: NativeUInt);
begin
  FRowAccessor.SetPAnsiChar(ColumnIndex, Value, Len);
end;

{**
  Updates the designated column with a <code>PWideChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len a pointer to the Length of the value in codepoints
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdatePWideChar(ColumnIndex: Integer;
  Value: PWideChar; var Len: NativeUInt);
begin
  FRowAccessor.SetPWideChar(ColumnIndex, Value, Len);
end;


{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateString(ColumnIndex: Integer;
  const Value: String);
begin
  FRowAccessor.SetString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>AnsiString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_ANSISTRING}
procedure TZCustomAsyncResultSet.UpdateAnsiString(ColumnIndex: Integer;
  const Value: AnsiString);
begin
  FRowAccessor.SetAnsiString(ColumnIndex, Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>UTF8String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_UTF8STRING}
procedure TZCustomAsyncResultSet.UpdateUTF8String(ColumnIndex: Integer;
  const Value: UTF8String);
begin
  FRowAccessor.SetUTF8String(ColumnIndex, Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>RawByteString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateRawByteString(ColumnIndex: Integer;
  const Value: RawByteString);
begin
  FRowAccessor.SetRawByteString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>Widestring</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateUnicodeString(ColumnIndex: Integer;
  const Value: ZWideString);
begin
  FRowAccessor.SetUnicodeString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateDate(ColumnIndex: Integer;
  const Value: TZDate);
begin
  FRowAccessor.SetDate(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateTime(ColumnIndex: Integer;
  const Value: TZTime);
begin
  FRowAccessor.SetTime(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateTimestamp(ColumnIndex: Integer;
  const Value: TZTimeStamp);
begin
  FRowAccessor.SetTimestamp(ColumnIndex, Value);
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateAsciiStream(ColumnIndex: Integer;
  const Value: TStream);
var Blob: IZBlob;
    CLob: IZClob;
    CP: Word;
    _IsNull: Boolean;
begin
  if (Value = nil) then
    RowAccessor.SetNull(ColumnIndex)
  else begin
    Blob := FRowAccessor.GetBlob(ColumnIndex, _IsNull);
    if Blob = nil
    then Blob := CreateLob(ColumnIndex, lsmWrite)
    else Blob.Open(lsmWrite);
    if Blob.QueryInterface(IZCLob, Clob) = S_OK then begin
      CP := FRowAccessor.GetColumnCodePage(ColumnIndex);
      if CP = zCP_UTF16 then
        CP := GetW2A2WConversionCodePage(FConSettings);
      Clob.SetStream(Value, CP);
    end else Blob.SetStream(Value);
  end;
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZCustomAsyncResultSet.UpdateBinaryStream(
  ColumnIndex: Integer; const Value: TStream);
var Blob: IZBlob;
  FIsNull: Boolean;
begin
  if (Value = nil) or (Value.Size = 0) then
    RowAccessor.SetNull(ColumnIndex)
  else begin
    Blob := FRowAccessor.GetBlob(ColumnIndex, FIsNull);
    if Blob = nil
    then Blob := CreateLob(ColumnIndex, lsmWrite)
    else Blob.Open(lsmWrite);
    Blob.SetStream(Value);
  end;
end;

procedure TZCustomAsyncResultSet.UpdateLob(ColumnIndex: Integer;
  const Value: IZBlob);
begin
  FRowAccessor.SetBlob(ColumnIndex, Value);
end;

{**
  Updates the designated column with a UTF16 character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZCustomAsyncResultSet.UpdateUnicodeStream(
  ColumnIndex: Integer; const Value: TStream);
var Blob: IZBlob;
    CLob: IZClob;
    FIsNull: Boolean;
begin
  if (Value = nil) then
    RowAccessor.SetNull(ColumnIndex)
  else begin
    Blob := FRowAccessor.GetBlob(ColumnIndex, FIsNull);
    if Blob = nil
    then Blob := CreateLob(ColumnIndex, lsmWrite)
    else Blob.Open(lsmWrite);
    if Blob.QueryInterface(IZCLob, Clob) = S_OK
    then Clob.SetStream(Value, zCP_UTF16)
    else Blob.SetStream(Value);
  end;
end;

{**
  Updates the DefaultExpression of the designated column with a <code>String</code> value.
  This changes the behaviour of the RowAccessor used by the Resultset
  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new DefaultExpression value for the column
}
procedure TZCustomAsyncResultSet.UpdateDefaultExpression(ColumnIndex: Integer;
  const Value: string);
begin
  FRowAccessor.SetColumnDefaultExpression(ColumnIndex, Value);
end;

//---------------------------------------------------------------------
// Processing methods
//---------------------------------------------------------------------

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZCustomAsyncResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
 Result:=False;
end;

{**
  Indicates whether the current row has been updated.  The value returned
  depends on whether or not the result set can detect updates.

  @return <code>true</code> if the row has been visibly updated
    by the owner or another, and updates are detected
}
function TZCustomAsyncResultSet.RowUpdated: Boolean;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  if Assigned(FSelectedRow) then
  begin
    Result := FSelectedRow^.UpdateType = utModified;
  end
  else
    Result := False;
end;

{**
  Indicates whether the current row has had an insertion.
  The value returned depends on whether or not this
  <code>ResultSet</code> object can detect visible inserts.

  @return <code>true</code> if a row has had an insertion
    and insertions are detected; <code>false</code> otherwise
}
function TZCustomAsyncResultSet.RowInserted: Boolean;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  if Assigned(FSelectedRow) then
  begin
    Result := FSelectedRow^.UpdateType = utInserted;
  end else
    Result := False;
end;

{**
  Indicates whether a row has been deleted.  A deleted row may leave
  a visible "hole" in a result set.  This method can be used to
  detect holes in a result set.  The value returned depends on whether
  or not this <code>ResultSet</code> object can detect deletions.

  @return <code>true</code> if a row was deleted and deletions are detected;
    <code>false</code> otherwise
}
function TZCustomAsyncResultSet.RowDeleted: Boolean;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  if Assigned(FSelectedRow) then
  begin
    Result := FSelectedRow^.UpdateType = utDeleted;
  end
  else
    Result := False;
end;

{**
  Inserts the contents of the insert row into this
  <code>ResultSet</code> object and into the database.
  The cursor must be on the insert row when this method is called.
}
procedure TZCustomAsyncResultSet.InsertRow;
begin
end;

{**
  Deletes the current row from this <code>ResultSet</code> object
  and from the underlying database.  This method cannot be called when
  the cursor is on the insert row.
}

procedure TZCustomAsyncResultSet.DeleteRow;
begin
end;

{**
  Moves the cursor to the remembered cursor position, usually the
  current row.  This method has no effect if the cursor is not on
  the insert row.
}
procedure TZCustomAsyncResultSet.MoveToCurrentRow;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
    FRowAccessor.RowBuffer := FSelectedRow
  else
    FRowAccessor.RowBuffer := nil;
end;

constructor TZCustomAsyncResultSet.Create(FResultSet:IZResultSet);
var
 New,Old:PZConSettings;
begin
 if Assigned(FResultSet) then
 begin
  New:=AllocMem(SizeOf(TZConSettings)+SizeOf(TZCodePage));
  Old:=(FResultSet as TZAbstractResultSet).GetConSettings;
  New^:=Old^;
  //New^.DataBaseSettings:=nil;
  New^.ClientCodePage:=Pointer(@New[1]);
  New^.ClientCodePage^:=Old^.ClientCodePage^;
 end;

 FConSettings := New;
 LastWasNull := True;
 FRowNo := 0;
 FLastRowNo := 0;
 FClosed := True;
 FMaxRows := 0;

 FColumnsInfo := TObjectList.Create(True); //Free the MemoryLeaks of TZColumnInfo

 _LoadColumnInfo(FColumnsInfo,FResultSet);
 Open;
end;

function _NewIndexPair(Count:Integer):TZIndexPairList;
var
 I:Integer;
begin
 Result:=TZIndexPairList.Create;
 Result.Clear;
 Result.Capacity:=Count;
 for I := FirstDbcIndex to Count+InvalidDbcIndex do
   Result.Add(I,I);
end;

procedure _LoadColumnInfo(FColumnsInfo:TObjectList;FResultSet:IZResultSet);
var
 I:Integer;
 ColumnInfo:TZColumnInfo;
 TmpMetaData:IZResultSetMetaData;
begin
 FColumnsInfo.Clear;
 TmpMetaData := FResultSet.GetMetadata;
 for I:=FirstDbcIndex to TmpMetadata.GetColumnCount+InvalidDbcIndex do
 begin
   ColumnInfo:=TZColumnInfo.Create;
   with ColumnInfo do
   begin
     AutoIncrement     :=TmpMetaData.IsAutoIncrement     (I);
     CaseSensitive     :=TmpMetaData.IsCaseSensitive     (I);
     Searchable        :=TmpMetaData.IsSearchable        (I);
     Currency          :=TmpMetaData.IsCurrency          (I);
     Nullable          :=TmpMetaData.IsNullable          (I);
     Signed            :=TmpMetaData.IsSigned            (I);
     ColumnLabel       :=TmpMetaData.GetColumnLabel      (I);
     ColumnName        :=TmpMetaData.GetColumnName       (I);
     SchemaName        :=TmpMetaData.GetSchemaName       (I);
     Precision         :=TmpMetaData.GetPrecision        (I);
     Scale             :=TmpMetaData.GetScale            (I);
     TableName         :=TmpMetaData.GetTableName        (I);
     CatalogName       :=TmpMetaData.GetCatalogName      (I);
     ColumnType        :=TmpMetaData.GetColumnType       (I);
     ReadOnly          :=TmpMetaData.IsReadOnly          (I);
     Writable          :=TmpMetaData.IsWritable          (I);
     DefinitelyWritable:=TmpMetaData.IsDefinitelyWritable(I);
     DefaultValue      :=TmpMetaData.GetDefaultValue     (I);
     ColumnCodePage    :=TmpMetaData.GetColumnCodePage   (I);
   end;
   FColumnsInfo.Add(ColumnInfo);
 end;
end;

function _FetchRow(FRowAccessor:TZRowAccessor;FIndexPairList:TZIndexPairList;FResultSet:IZResultSet):PZRowBuffer;
var
 TempRow:PZRowBuffer;
begin
 Result:=nil;
 TempRow:=FRowAccessor.RowBuffer;
 try
   FRowAccessor.Alloc;
   FRowAccessor.RowBuffer^.UpdateType:=utUnmodified;
   FRowAccessor.FillFromFromResultSet(FResultSet,FIndexPairList);
   Result:=FRowAccessor.RowBuffer;
 finally
   if (Result=nil) then //OutOfMem? FetchError?
     FRowAccessor.Dispose;
   FRowAccessor.RowBuffer:=TempRow;
 end;
end;

/////

{**
  Finds a row with specified index among list of rows.
  @param RowsList a list of rows.
  @param Index a row index.
  @return a found row buffer of <code>null</code> otherwise.
}
function TZAsyncResultSet.LocateRow(RowsList:TZSortedList; RowIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to RowsList.Count - 1 do
  begin
    if PZRowBuffer(RowsList[I])^.Index = RowIndex then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Appends a row to the list of rows if such row is not exist.
  @param Row a row buffer.
  @return an appended row buffer.
}
function TZAsyncResultSet.AppendRow(Row: PZRowBuffer): PZRowBuffer;
begin
 Result:=Row;
 FRowsList.Add(Row);
 LastRowNo := FRowsList.Count;
end;

procedure TZAsyncResultSet.Open;
begin
 inherited;
 FRowsList := TZSortedList.Create;
end;

procedure TZAsyncResultSet.AfterClose;
var
 I:Integer;
begin
 if Assigned(FRowAccessor) then
 begin
  for I:=0 to FRowsList.Count-1 do
    FRowAccessor.DisposeBuffer(PZRowBuffer(FRowsList[I]));
  FreeAndNil(FRowsList);
 end;
 inherited;
end;

procedure TZAsyncResultSet.ResetCursor;
var
 I:Integer;
begin
 if Assigned(FRowAccessor) then
 begin
  for I:=0 to FRowsList.Count-1 do
    FRowAccessor.DisposeBuffer(PZRowBuffer(FRowsList[I]));
  FRowsList.Clear;
 end;
 inherited;
end;

function TZAsyncResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  if not Closed and (Row >= 0) and (Row <= LastRowNo + 1) then
  begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then
    begin
      Result := True;
      FSelectedRow := PZRowBuffer(FRowsList[Row - 1]);
      RowAccessor.RowBuffer := FSelectedRow;
    end
    else
    begin
      Result := False;
      FSelectedRow := nil;
      RowAccessor.RowBuffer := FSelectedRow;
    end;
  end
  else
    Result := False;
end;

procedure TZAsyncResultSet.InsertRow;
begin
  FRowAccessor.Alloc;
  FRowAccessor.RowBuffer^.UpdateType := utInserted;
  FRowAccessor.RowBuffer^.Index := GetNextRowIndex;

  AppendRow(FRowAccessor.RowBuffer);

  FRowAccessor.ClearBuffer(FRowAccessor.RowBuffer, True);
  MoveAbsolute(LastRowNo);
end;

procedure TZAsyncResultSet.DeleteRow;
begin
 if (RowNo < 1) or (RowNo > LastRowNo) or (FSelectedRow = nil) then Exit;
 begin
   FRowsList.Delete(RowNo - 1);
   FRowAccessor.DisposeBuffer(FSelectedRow);
   FLastRowNo:=FLastRowNo-1;
   FSelectedRow:=nil;
   if (RowNo<=FLastRowNo) then FSelectedRow:=FRowsList.Items[RowNo-1];
   MoveToCurrentRow;
 end;
end;

function TZAsyncResultSet.Fetch(FResultSet:IZResultSet;FIndexPairList:TZIndexPairList):Boolean;
var
 TempRow:PZRowBuffer;

begin
 if Assigned(FResultSet) then
   Result:=FResultSet.Next
 else
   Result:=False;
 if not Result or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then
   Exit;

 TempRow:=_FetchRow(FRowAccessor,FIndexPairList,FResultSet);
 if (TempRow=nil) then Exit;

 TempRow^.Index:=GetNextRowIndex;

 AppendRow(TempRow);
end;

end.
