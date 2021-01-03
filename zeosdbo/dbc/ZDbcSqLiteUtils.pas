{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcSqLiteUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
uses
  Classes, SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcLogging, ZCompatibility;

{**
  Convert string SQLite field type to SQLType
  @param string field type value
  @param Precision the column precision or size
  @param Decimals the column position after decimal point
  @param SQLiteIntAffinity the column position after decimal point
  @result the SQLType field type value
}
function ConvertSQLiteTypeToSQLType(var TypeName: RawByteString;
  UndefinedVarcharAsStringLength: Integer; out Precision: Integer;
  out Decimals: Integer; SQLiteIntAffinity: Boolean = False): TZSQLType;

{**
  Decodes a SQLite Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param SQLiteVersion an integer containing the Full Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertSQLiteVersionToSQLVersion(SQLiteVersion: PAnsiChar ): Integer;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZMessages, ZFastCode, ZDbcUtils;

{**
  Convert string SQLite field type to SQLType
  @param string field type value
  @param Precision the column precision or size
  @param Decimals the column position after decimal point
  @result the SQLType field type value
}
function ConvertSQLiteTypeToSQLType(var TypeName: RawByteString;
  UndefinedVarcharAsStringLength: Integer; out Precision: Integer;
  out Decimals: Integer; SQLiteIntAffinity: Boolean = False): TZSQLType;
var
  pBL, pBR, pC: Integer;
  P: PAnsiChar;
begin
  TypeName := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}UpperCase(TypeName);
  Result := stString;
  Precision := 0;
  Decimals := 0;
  pBL := ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('('), TypeName);
  if pBL > 0 then begin
    P := {%H-}Pointer(NativeUInt(TypeName)+Word(pBL));
    Precision := ValRawInt(P, pC);
    while (P+pC-1)^ = ' ' do inc(pC);
    if (P+pC-1)^ = ',' then begin
      Decimals := ValRawInt(P+pC, pBR);
      while (P+pC+pBR-1)^ = ' ' do inc(pBR);
      if (P+pC+pBR-1)^ = ')' then begin
        while (P-2)^ = ' ' do Dec(p); //trim rigth
        TypeName := Copy(TypeName, 1, P-Pointer(TypeName)-1)
      end else begin //invalid
        Precision := 0;
        Decimals := 0;
      end;
    end else if (P+pC-1)^ = ')' then begin
      while (P-2)^ = ' ' do Dec(p); //trim rigth
      TypeName := Copy(TypeName, 1, P-Pointer(TypeName)-1)
    end else
      Precision := 0;
  end;
  if TypeName = '' then
    Result := stString
  else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('BOOL')) then
    Result := stBoolean
  else if ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('INT'), TypeName) > 0 then
    (* EH: This is a hack to use integer affinity for Currency type ranges *)
    if (Decimals > 0) and (Decimals <= 4) and (Precision >= Decimals) and (Precision <= zDbcUtils.sAlignCurrencyScale2Precision[Decimals]) then
      Result := stCurrency
    else if SQLiteIntAffinity then
      Result := stLong
    else begin
      if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('TINY')) or StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('INT8')) then
        Result := stShort
      else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('SMALL')) or StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('INT16'))  then
        Result := stSmall
      else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('BIG')) or
               (TypeName = 'INTEGER') then //http://www.sqlite.org/autoinc.html
        Result := stLong
      else //includes 'INT' / 'MEDIUMINT' /INT32
        Result := stInteger;
      if PosEx({$IFDEF UNICODE}RawByteString{$ENDIF}('UNSIGEND'), TypeName) > 0 then
        Result := TZSQLType(Ord(stLongWord)-1);
    end
  else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('REAL')) then
    Result := stDouble
  else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('FLOAT')) then
    Result := stDouble
  else if (TypeName = {$IFDEF UNICODE}RawByteString{$ENDIF}('NUMERIC')) or
    (TypeName = {$IFDEF UNICODE}RawByteString{$ENDIF}('DECIMAL'))
      or (TypeName = {$IFDEF UNICODE}RawByteString{$ENDIF}('NUMBER')) then
    Result := stDouble
  else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('DOUB')) then
    Result := stDouble
  else if EndsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('MONEY')) then
    Result := stCurrency
  else if ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('CHAR'), TypeName) > 0 then
    if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('LONG')) then
      Result := stAsciiStream
    else
      Result := stString
  else if EndsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('BINARY')) then
    Result := stBytes
  else if TypeName = {$IFDEF UNICODE}RawByteString{$ENDIF}('DATE') then
    Result := stDate
  else if TypeName = {$IFDEF UNICODE}RawByteString{$ENDIF}('TIME') then
    Result := stTime
  else if TypeName = {$IFDEF UNICODE}RawByteString{$ENDIF}('TIMESTAMP') then
    Result := stTimestamp
  else if TypeName = {$IFDEF UNICODE}RawByteString{$ENDIF}('DATETIME') then
    Result := stTimestamp
  else if ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('BLOB'), TypeName) > 0 then
    Result := stBinaryStream
  else if ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('CLOB'), TypeName) > 0 then
    Result := stAsciiStream
  else if ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('TEXT'), TypeName) > 0 then
    Result := stAsciiStream
  else if ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('GUID'), TypeName) > 0 then
    Result := stGUID;
  if (Result = stInteger) and (Precision <> 0) then
    if Precision <= 2 then
      Result := stByte
    else if Precision <= 4 then
      Result := stSmall
    else if Precision <= 9 then
      Result := stInteger
    else
      Result := stLong;

  if (Result = stString) and (Precision = 0) then
    if (UndefinedVarcharAsStringLength = 0) then
      Result := stAsciiStream
    else
      Precision := UndefinedVarcharAsStringLength;
end;

{**
  Decodes a SQLite Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param SQLiteVersion an integer containing the Full Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertSQLiteVersionToSQLVersion(SQLiteVersion: PAnsiChar ): Integer;
var
  MajorVersion, MinorVersion, SubVersion, Code: Integer;
begin
  Code := 0;
  MajorVersion := ValRawInt(SQLiteVersion, Code);
  Inc(SQLiteVersion, Code);
  MinorVersion := ValRawInt(SQLiteVersion, Code);
  Inc(SQLiteVersion, Code);
  SubVersion := ValRawInt(SQLiteVersion, Code);
  Result := EncodeSQLVersioning(MajorVersion,MinorVersion,SubVersion);
end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
end.

