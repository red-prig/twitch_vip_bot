{ Dbc script utils

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

unit DbcScriptUtils;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 dateutils,
 math,
 Classes,
 FmtBCD,
 ZSysUtils,
 ZDbcIntfs,
 ZCompatibility,
 ZEncoding,
 ZVariant,
 ZFastCode,
 ZTokenizer,ZGenericSqlToken;

function  _GetAsRawByteString(const Value:TZVariant):RawByteString;
function  _GetAsUnicodeString(const Value:TZVariant):UnicodeString;
function  _GetAsBoolean(const Value:TZVariant):Boolean; inline;
function  _GetAsBoolean(const Value:TZVariant;var _Result:Boolean):Boolean;
function  _GetAsBytes(const Value:TZVariant):TBytes;
function  _GetAsDouble(const Value:TZVariant):Double; inline;
function  _GetAsDouble(const Value:TZVariant;var _Result:Double):Boolean;
function  _GetAsInteger(const Value:TZVariant):Int64; inline;
function  _GetAsInteger(const Value:TZVariant;var _Result:Int64):Boolean;
function  _GetAsUInteger(const Value:TZVariant):UInt64; inline;
function  _GetAsUInteger(const Value:TZVariant;var _Result:UInt64):Boolean;
function  _GetAsCurrency(const Value:TZVariant):Currency; inline;
function  _GetAsCurrency(const Value:TZVariant;var _Result:Currency):Boolean;
function  _GetAsDateTime(const Value:TZVariant):TDateTime;
function  _GetAsBigDecimal(const Value:TZVariant;Var _Result:TBCD):Boolean;
function  _GetAsGUID(const Value:TZVariant;Var _Result:TGUID):Boolean;
function  _GetAsDate(const Value:TZVariant;var _Result:TZDate):Boolean;
function  _GetAsTime(const Value:TZVariant;var _Result: TZTime):Boolean;
function  _GetAsTimeStamp(const Value: TZVariant;var _Result: TZTimeStamp):Boolean;
function  _GetAsPointer(const Value: TZVariant): Pointer;

function _ValueIsStr(const Value:TZVariant):Boolean;
function _ValueIsOrd(const Value:TZVariant):Boolean;
function _Convert(const Value:TZVariant;NewType:TZVariantType):TZVariant;
function _OpAdd(const Value1,Value2:TZVariant):TZVariant;
function _OpSub(const Value1,Value2:TZVariant):TZVariant;
function _OpMul(const Value1,Value2:TZVariant):TZVariant;
function _OpPow(const Value1,Value2:TZVariant):TZVariant;
function _OpDiv(const Value1,Value2:TZVariant):TZVariant;
function _OpMod(const Value1,Value2:TZVariant):TZVariant;
function _IsNull(const Value:TZVariant):Boolean; inline;
function _Compare(const Value1,Value2:TZVariant):TZVariant;
function _OpEqual(const Value1,Value2:TZVariant):TZVariant;
function _OpNotEqual(const Value1,Value2:TZVariant):TZVariant;
function _OpLess(const Value1,Value2:TZVariant):TZVariant;
function _OpLessEqual(const Value1,Value2:TZVariant):TZVariant;
function _OpMore(const Value1,Value2:TZVariant):TZVariant;
function _OpMoreEqual(const Value1,Value2:TZVariant):TZVariant;
function _OpNot(const Value:TZVariant):TZVariant;
function _OpAnd(const Value1,Value2:TZVariant):TZVariant;
function _OpOr(const Value1,Value2:TZVariant):TZVariant;

function  DoTokenOp(id1,id2:Byte;Const L,R:TZVariant):TZVariant;
procedure DoAssignOp(id:Byte;var L:TZVariant;Const R:TZVariant);

Function _get_op_prior(id:SizeInt):Byte;
Function _get_op_arity(id:SizeInt):Byte; inline;
function _get_op_name(id1,id2:Byte):RawByteString;

Function DecodeVariable(P:PChar;L:SizeInt):RawByteString;
Function GetVariableType(P:PChar;L:SizeInt):Byte;
Function DecodeToken(P:PChar;L:SizeInt;QuoteChar:Char):RawByteString;
Function GetParamStr(P:PChar;L:SizeInt):RawByteString;
Function GetWordStr(P:PChar;L:SizeInt):RawByteString; inline;

function _get_const_keyword(data:Pointer;len:SizeUInt):SizeInt;
function _get_op_id(data:Pointer;len:SizeUInt):SizeInt;
function _fetch_op1(data:Pointer;len:SizeUInt):SizeInt;
function _fetch_op2(data:Pointer;len:SizeUInt):SizeInt;
function _is_global_word(data:Pointer;len:SizeUInt):Boolean;
function _is_from_word(data:Pointer;len:SizeUInt):Boolean;
function _is_into_word(data:Pointer;len:SizeUInt):Boolean;
function _is_select_word(data:Pointer;len:SizeUInt):Boolean;

function _GetDataTypeId(data:Pointer;len:SizeUInt):SizeInt;
function GetDataType(data:Pointer;len:SizeUInt):TZVariantType;

function _get_fetch_type(data:Pointer;len:SizeUInt):SizeInt;
function _get_begin_op(data:Pointer;len:SizeUInt):SizeInt;
function _get_cmd_keyword(data:Pointer;len:SizeUInt):SizeInt;

const
 op_br_open=36;
 op_br_clos=37;

type
 TZDbcScriptTokenizer = class (TZTokenizer)
 protected
   //LastTokenType:TZTokenType;
   FPos:TPoint;
   procedure CreateTokenStates; override;
 public
   property  LinePos:TPoint read FPos;
   function  FetchNextToken(var Buffer:PChar;EOS:PChar):TZToken;
 end;

implementation

function _GetFormat:TZFormatSettings; inline;
begin
 Result.DateFormat        := DefaultFormatSettings.ShortDateFormat;
 Result.DateFormatLen     := Length(Result.DateFormat);
 Result.TimeFormat        := DefaultFormatSettings.LongTimeFormat;
 Result.TimeFormatLen     := Length(Result.TimeFormat);
 Result.DateTimeFormat    := Result.DateFormat+' '+Result.TimeFormat;
 Result.DateTimeFormatLen := Length(Result.DateTimeFormat);
end;

function _GetAsRawByteString(const Value:TZVariant):RawByteString;

 function _TimeToRaw(const Value:TZVariant):RawByteString; inline;
 var
  Buffer:array[0..cMaxTimeLenQuoted] of AnsiChar;
  L:Byte;
 begin
  L:=TimeToRaw(Value.VTime.Hour, Value.VTime.Minute,
               Value.VTime.Second, Value.VTime.Fractions, @Buffer,
               DefaultFormatSettings.LongTimeFormat, False, Value.VDate.IsNegative);
  System.SetString(Result,@Buffer,L);
 end;

 function _DateToRaw(const Value:TZVariant):RawByteString; inline;
 var
  Buffer:array[0..cMaxTimeLenQuoted] of AnsiChar;
  L:Byte;
 begin
  L:=DateToRaw(Value.VDate.Year, Value.VDate.Month, Value.VDate.Day,
               @Buffer, DefaultFormatSettings.ShortDateFormat, False, Value.VDate.IsNegative);
  System.SetString(Result,@Buffer,L);
 end;

 function _DateTimeToRaw(const Value:TZVariant):RawByteString; inline;
 var
  Buffer:array[0..cMaxTimeLenQuoted] of AnsiChar;
  L:Byte;
 begin
  L:=DateTimeToRaw(Value.VTimeStamp.Year, Value.VTimeStamp.Month,
                   Value.VTimeStamp.Day, Value.VTimeStamp.Hour, Value.VTimeStamp.Minute,
                   Value.VTimeStamp.Second, Value.VTimeStamp.Fractions, @Buffer,
                   DefaultFormatSettings.ShortDateFormat+' '+DefaultFormatSettings.LongTimeFormat,
                   False, Value.VTimeStamp.IsNegative);
  System.SetString(Result,@Buffer,L);
 end;

 function _DateTimeToRawSQLTimeStamp(const Value:TDateTime):RawByteString;
 var
  l, Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..cMaxTimeStampLenQuoted] of AnsiChar;
 begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  L:=DateTimeToRaw(Year, Month, Day, Hour, Minute, Second, MSec*NanoSecsPerMSec,
    @Buffer,
    DefaultFormatSettings.ShortDateFormat+' '+DefaultFormatSettings.LongTimeFormat,
    False, False);
  System.SetString(Result,@Buffer,L);
 end;

begin
 Result:='';
 case Value.VType of
  {$IFNDEF UNICODE}vtString,{$ENDIF}
  {$IFNDEF NO_ANSISTRING}
  vtAnsiString,
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  vtUTF8String,
  {$ENDIF}
  vtRawByteString:Result:=Value.VRawByteString;
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString:Result:=RawByteString(Value.VUnicodeString);

  vtBoolean      :Result:=BoolStrsUpRaw[Value.VBoolean];
  vtInteger      :Result:=IntToRaw(Value.VInteger);
  vtUInteger     :Result:=IntToRaw(Value.VUInteger);
  vtDouble       :Result:=ZSysUtils.FloatToSqlRaw(Value.VDouble);
  vtCurrency     :Result:=ZFastCode.CurrToRaw(Value.VCurrency);
  vtBigDecimal   :Result:=ZSysUtils.BcdToSQLRaw(Value.VBigDecimal);
  vtGUID         :Result:=GUIDToRaw(Value.VGUID,[guidWithBrackets]);
  vtTime         :Result:=_TimeToRaw(Value);
  vtDate         :Result:=_DateToRaw(Value);
  vtTimeStamp    :Result:=_DateTimeToRaw(Value);
  vtDateTime     :Result:=_DateTimeToRawSQLTimeStamp(Value.VDateTime);
 end;
end;


function _GetAsUnicodeString(const Value:TZVariant):UnicodeString;

 function _TimeToUni(const Value:TZVariant):UnicodeString; inline;
 var
  Buffer:array[0..cMaxTimeLenQuoted] of WideChar;
  L:Byte;
 begin
  L:=TimeToUni(Value.VTime.Hour, Value.VTime.Minute,
               Value.VTime.Second, Value.VTime.Fractions, @Buffer,
               DefaultFormatSettings.LongTimeFormat, False, Value.VDate.IsNegative);
  System.SetString(Result,@Buffer,L);
 end;

 function _DateToUni(const Value:TZVariant):UnicodeString; inline;
 var
  Buffer:array[0..cMaxTimeLenQuoted] of WideChar;
  L:Byte;
 begin
  L:=DateToUni(Value.VDate.Year, Value.VDate.Month, Value.VDate.Day,
               @Buffer, DefaultFormatSettings.ShortDateFormat, False, Value.VDate.IsNegative);
  System.SetString(Result,@Buffer,L);
 end;

 function _DateTimeToUni(const Value:TZVariant):UnicodeString; inline;
 var
  Buffer:array[0..cMaxTimeLenQuoted] of WideChar;
  L:Byte;
 begin
  L:=DateTimeToUni(Value.VTimeStamp.Year, Value.VTimeStamp.Month,
                   Value.VTimeStamp.Day, Value.VTimeStamp.Hour, Value.VTimeStamp.Minute,
                   Value.VTimeStamp.Second, Value.VTimeStamp.Fractions, @Buffer,
                   DefaultFormatSettings.ShortDateFormat+' '+DefaultFormatSettings.LongTimeFormat,
                   False, Value.VTimeStamp.IsNegative);
  System.SetString(Result,@Buffer,L);
 end;

 function _DateTimeToUniSQLTimeStamp(const Value:TDateTime):UnicodeString;
 var
  l, Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..cMaxTimeStampLenQuoted] of AnsiChar;
 begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  L:=DateTimeToUni(Year, Month, Day, Hour, Minute, Second, MSec*NanoSecsPerMSec,
    @Buffer,
    DefaultFormatSettings.ShortDateFormat+' '+DefaultFormatSettings.LongTimeFormat,
    False, False);
  System.SetString(Result,@Buffer,L);
 end;

begin
 Result:='';
 case Value.VType of
  {$IFNDEF UNICODE}vtString,{$ENDIF}
  {$IFNDEF NO_ANSISTRING}
  vtAnsiString,
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  vtUTF8String,
  {$ENDIF}
  vtRawByteString:Result:=UnicodeString(Value.VRawByteString);
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString:Result:=Value.VUnicodeString;

  vtBoolean      :Result:=BoolStrsUpW[Value.VBoolean];
  vtInteger      :Result:=IntToUnicode(Value.VInteger);
  vtUInteger     :Result:=IntToUnicode(Value.VUInteger);
  vtDouble       :Result:=ZSysUtils.FloatToSqlUnicode(Value.VDouble);
  vtCurrency     :Result:=ZFastCode.CurrToUnicode(Value.VCurrency);
  vtBigDecimal   :Result:=ZSysUtils.BcdToSQLUni(Value.VBigDecimal);
  vtGUID         :Result:=GUIDToUnicode(Value.VGUID,[guidWithBrackets]);
  vtTime         :Result:=_TimeToUni(Value);
  vtDate         :Result:=_DateToUni(Value);
  vtTimeStamp    :Result:=_DateTimeToUni(Value);
  vtDateTime     :Result:=_DateTimeToUniSQLTimeStamp(Value.VDateTime);
 end;
end;

function _GetAsBoolean(const Value:TZVariant):Boolean; inline;
begin
 Result:=false;
 _GetAsBoolean(Value,Result);
end;

function _GetAsBoolean(const Value:TZVariant;var _Result:Boolean):Boolean;
begin
 Result:=true;
 case Value.VType of
  vtBoolean:  _Result := Value.VBoolean;
  vtInteger:  _Result := Value.VInteger <> 0;
  vtUInteger: _Result := Value.VUInteger <> 0;
  vtDouble:   _Result := Value.VDouble <> 0;
  vtCurrency: _Result := Value.VCurrency <> 0;
  vtBigDecimal: _Result := not (Value.VBigDecimal.Precision = 10) and (Value.VBigDecimal.SignSpecialPlaces = 2);
  {$IFNDEF UNICODE}vtString,{$ENDIF}
  {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
  vtRawByteString: _Result := StrToBoolEx(Value.VRawByteString);
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString: _Result := StrToBoolEx(Value.VUnicodeString);
  vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
    then _Result := StrToBoolEx(PWideChar(Value.VCharRec.P))
    else _Result := StrToBoolEx(PAnsiChar(Value.VCharRec.P));
  vtDateTime: _Result := Value.VDateTime <> 0;
  vtDate: _Result := PInt64(@Value.VDate.Year)^ <> 0;
  vtTime: _Result := (PCardinal(@Value.VTime.Hour)^ <> 0) or (PInt64(@Value.VTime.Second)^ <> 0);
  vtTimeStamp: _Result := (PInt64(@Value.VTimeStamp.Year)^ <> 0) or (PInt64(@Value.VTimeStamp.Minute)^ <> 0)
    or (PInt64(PAnsichar(@Value.VTimeStamp.TimeZoneHour)-2)^ <> 0);
  else
   Result:=False;
 end;
end;

function _GetAsBytes(const Value:TZVariant):TBytes;
begin
 Result := nil;
 case Value.VType of
  vtBoolean: Result := BufferToBytes(PAnsiChar(@Value.VBoolean), SizeOf(Boolean));
  vtDate,vtInteger, vtUInteger, vtDouble, vtCurrency: Result := BufferToBytes(@Value.VInteger, 8);
  vtBigDecimal: Result := BufferToBytes(@Value.VBigDecimal.Precision, SizeOf(TBCD));
  vtTime: Result := BufferToBytes(@Value.VTime.Hour, SizeOf(TZTime));
  vtTimeStamp: Result := BufferToBytes(@Value.VTimeStamp.Year, SizeOf(TZTimeStamp));
  vtBytes{$IFNDEF UNICODE}, vtString{$ENDIF},
  {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String, {$ENDIF}
  vtRawByteString: Result := {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}StrToBytes{$ENDIF}(Value.VRawByteString);
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString: begin
                    SetLength(Result, Length(Value.VUnicodeString) shl 1);
                    if Pointer(Result) = nil then Exit;
                    Move(Pointer(Value.VUnicodeString)^, Pointer(Result)^, Length(Result));
                  end;
  vtCharRec:  begin
                if (Value.VCharRec.CP = zCP_UTF16)
                then SetLength(Result, Value.VCharRec.Len shl 1)
                else SetLength(Result, Value.VCharRec.Len);
                if Pointer(Result) = nil then Exit;
                Move(Value.VCharRec.P^, Pointer(Result)^, Length(Result));
              end;
 end;
end;

function TrySQLStrToFloat(Value:PChar;out _Result:Double):Boolean;
var
 E:Integer;
begin
 Result:=false;
 if Assigned(Value) then
 begin
  _Result:=ValRawDbl(PByteArray(Value),'.',E);
  Result:=(E=0);
  if not Result then
  begin
   _Result:=ValRawDbl(PByteArray(Value),',',E);
   Result:=(E=0);
  end;
 end;
end;

function TrySQLStrToFloat(Value:PWideChar;out _Result:Double):Boolean;
var
 E:Integer;
begin
 Result:=false;
 if Assigned(Value) then
 begin
  _Result:=ValUnicodeDbl(PWordArray(Value),WideChar('.'),E);
  Result:=(E=0);
  if not Result then
  begin
   _Result:=ValUnicodeDbl(PWordArray(Value),WideChar(','),E);
   Result:=(E=0);
  end;
 end;
end;

function TrySQLStrToCurr(Value:PChar;Len:Integer;out _Result:Currency):Boolean;
var
 E:Integer;
begin
 Result:=false;
 if Assigned(Value) then
 begin
  E:=Len;
  _Result:=ValRawCurr(PByteArray(Value),'.',E);
  Result:=(E=Len);
  if not Result then
  begin
   E:=Len;
   _Result:=ValRawCurr(PByteArray(Value),',',E);
   Result:=(E=Len);
  end;
 end;
end;

function TrySQLStrToCurr(Value:PWideChar;Len:Integer;out _Result:Currency):Boolean;
var
 E:Integer;
begin
 Result:=false;
 if Assigned(Value) then
 begin
  E:=Len;
  _Result:=ValUnicodeCurr(PWordArray(Value),WideChar('.'),E);
  Result:=(E=Len);
  if not Result then
  begin
   E:=Len;
   _Result:=ValUnicodeCurr(PWordArray(Value),WideChar(','),E);
   Result:=(E=Len);
  end;
 end;
end;

function TryRawToInt64(Buf:PAnsiChar;Len:SizeUint;var R:Int64):Boolean; inline;
var
 P,PEnd:PAnsiChar;
begin
 PEnd:=Buf+Len;
 P:=PEnd;
 R:=ValRawInt64(Buf,P);
 Result:=(P=PEnd);
end;

function TryRawToUInt64(Buf:PAnsiChar;Len:SizeUint;var R:UInt64):Boolean; inline;
var
 P,PEnd:PAnsiChar;
begin
 PEnd:=Buf+Len;
 P:=PEnd;
 R:=ValRawUInt64(Buf,P);
 Result:=(P=PEnd);
end;

function TryUniToInt64(Buf:PWideChar;Len:SizeUint;var R:Int64):Boolean; inline;
var
 P,PEnd:PWideChar;
begin
 PEnd:=@Buf[Len];
 P:=PEnd;
 R:=ValUnicodeInt64(Buf,P);
 Result:=(P=PEnd);
end;

function TryUniToUInt64(Buf:PWideChar;Len:SizeUint;var R:UInt64):Boolean; inline;
var
 P,PEnd:PWideChar;
begin
 PEnd:=@Buf[Len];
 P:=PEnd;
 R:=ValUnicodeUInt64(Buf,P);
 Result:=(P=PEnd);
end;

function _GetAsDouble(const Value:TZVariant):Double; inline;
begin
 Result:=0;
 _GetAsDouble(Value,Result)
end;

function _GetAsDouble(const Value:TZVariant;var _Result:Double):Boolean;
begin
 Result:=true;
 case Value.VType of
   vtBoolean:   _Result := Ord(Value.VBoolean);
   vtInteger:   _Result := Value.VInteger;
   vtUInteger:  _Result := Value.VUInteger;
   vtDouble:    _Result := Value.VDouble;
   vtCurrency:  _Result := Value.VCurrency;
   vtBigDecimal:_Result := BCDToDouble(Value.VBigDecimal);
   vtDate:      Result:=TryDateToDateTime(Value.VDate, PDatetime(@_Result)^);
   vtTime:      Result:=TryTimeToDateTime(Value.VTime, PDatetime(@_Result)^);
   vtTimeStamp: Result:=TryTimeStampToDateTime(Value.VTimeStamp, PDatetime(@_Result)^);
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString:  Result:=TrySQLStrToFloat(PAnsiChar(Pointer(Value.VRawByteString)),_Result);
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString:  Result:=TrySQLStrToFloat(PWideChar(Pointer(Value.VUnicodeString)),_Result);
   vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
     then Result:=TrySQLStrToFloat(PWideChar(Value.VCharRec.P),_Result)
     else Result:=TrySQLStrToFloat(PAnsiChar(Value.VCharRec.P),_Result);
   vtDateTime: _Result := Value.VDateTime;
  else
   Result:=False;
 end;
end;

function _GetAsInteger(const Value:TZVariant):Int64; inline;
begin
 Result:=0;
 _GetAsInteger(Value,Result);
end;

function _GetAsInteger(const Value:TZVariant;var _Result:Int64):Boolean;
Var
 D:Double;
begin
 Result:=true;
 case Value.VType of
  vtBoolean:    _Result := Ord(Value.VBoolean);
  vtInteger:    _Result := Value.VInteger;
  vtUInteger:   _Result := Value.VUInteger;
  vtCurrency:   _Result := PInt64(@Value.VCurrency)^ div 10000;
  vtBigDecimal: BCD2Int64(Value.VBigDecimal, _Result);
  {$IFNDEF UNICODE}vtString,{$ENDIF}
  {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
  vtRawByteString: Result := TryRawToInt64(PAnsiChar(Value.VRawByteString),Length(Value.VRawByteString),_Result);
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString: Result := TryUniToInt64(PWideChar(Value.VUnicodeString),Length(Value.VUnicodeString),_Result);
  vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
    then  Result := TryUniToInt64(PWideChar(Value.VCharRec.P),Value.VCharRec.Len,_Result)
    else  Result := TryRawToInt64(PAnsiChar(Value.VCharRec.P),Value.VCharRec.Len,_Result);

  vtPointer:    _Result := Int64(NativeUInt(Value.VPointer));
  else
   begin
    D:=0;
    Result:=_GetAsDouble(Value,D);
    if Result then _Result:=Trunc(D);
   end;
 end;
end;

function _GetAsUInteger(const Value:TZVariant):UInt64; inline;
begin
 Result:=0;
 _GetAsUInteger(Value,Result);
end;

function _GetAsUInteger(const Value:TZVariant;var _Result:UInt64):Boolean;
Var
 D:Double;
begin
 Result:=true;
 case Value.VType of
  vtBoolean:  _Result := Ord(Value.VBoolean);
  vtInteger:  _Result := Value.VInteger;
  vtUInteger: _Result := Value.VUInteger;
  vtCurrency: _Result := PInt64(@Value.VCurrency)^ div 10000;
  vtBigDecimal:BCD2UInt64(Value.VBigDecimal, _Result);
  {$IFNDEF UNICODE}vtString,{$ENDIF}
  {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
  vtRawByteString: Result := TryRawToUInt64(PAnsiChar(Value.VRawByteString),Length(Value.VRawByteString),_Result);
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString: Result := TryUniToUInt64(PWideChar(Value.VUnicodeString),Length(Value.VUnicodeString),_Result);
  vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
    then Result := TryUniToUInt64(PWideChar(Value.VCharRec.P),Value.VCharRec.Len,_Result)
    else Result := TryRawToUInt64(PAnsiChar(Value.VCharRec.P),Value.VCharRec.Len,_Result);
  vtPointer:  _Result := NativeUInt(Value.VPointer);
  else
   begin
    D:=0;
    Result:=_GetAsDouble(Value,D);
    if Result then _Result:=Trunc(D);
   end;
 end;
end;

function _GetAsCurrency(const Value:TZVariant):Currency; inline;
begin
 Result:=0;
  _GetAsCurrency(Value,Result);
end;

function _GetAsCurrency(const Value:TZVariant;var _Result:Currency):Boolean;
Var
 D:Double;
begin
 Result:=true;
 case Value.VType of
  vtBoolean:  _Result := Ord(Value.VBoolean);
  vtInteger:  _Result := Value.VInteger;
  vtUInteger: _Result := Value.VUInteger;
  vtCurrency: _Result := Value.VCurrency;
  vtBigDecimal: BCDToCurr(Value.VBigDecimal, _Result);
  {$IFNDEF UNICODE}vtString,{$ENDIF}
  {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
  vtRawByteString: Result := TrySQLStrToCurr(PAnsiChar(Value.VRawByteString),Length(Value.VRawByteString),_Result);
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString: Result := TrySQLStrToCurr(PWideChar(Value.VUnicodeString),Length(Value.VUnicodeString),_Result);
  vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
    then Result := TrySQLStrToCurr(PWideChar(Value.VCharRec.P),Value.VCharRec.Len,_Result)
    else Result := TrySQLStrToCurr(PAnsiChar(Value.VCharRec.P),Value.VCharRec.Len,_Result);
  else
   begin
    D:=0;
    Result:=_GetAsDouble(Value,D);
    if Result then _Result:=D;
   end;
 end;
end;

function _GetAsDateTime(const Value:TZVariant):TDateTime;
begin
 Result:=0;
 case Value.VType of
  {$IFNDEF UNICODE}vtString, {$ENDIF}
  {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
  vtRawByteString:
    Result := AnsiSQLDateToDateTime(Value.VRawByteString);
  {$IFDEF UNICODE}vtString, {$ENDIF}
  vtUnicodeString:
    Result := AnsiSQLDateToDateTime(Value.VUnicodeString);
  vtCharRec:
    if (Value.VCharRec.CP = zCP_UTF16)
    then Result := AnsiSQLDateToDateTime(PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
    else Result := AnsiSQLDateToDateTime(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len);
  else Result := _GetAsDouble(Value);
 end;
end;

function _GetAsDateTime(const Value:TZVariant;Var _Result:TDateTime):Boolean;
begin
 Result:=true;
 case Value.VType of
  {$IFNDEF UNICODE}vtString, {$ENDIF}
  {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
  vtRawByteString:
    Result:=TryPCharToDateTime(PAnsiChar(Value.VRawByteString),Length(Value.VRawByteString),_GetFormat,_Result);
  {$IFDEF UNICODE}vtString, {$ENDIF}
  vtUnicodeString:
    Result:=TryPCharToDateTime(PWideChar(Value.VUnicodeString),Length(Value.VRawByteString),_GetFormat,_Result);
  vtCharRec:
    if (Value.VCharRec.CP=zCP_UTF16) then
     Result:=TryPCharToDateTime(PWideChar(Value.VCharRec.P),Value.VCharRec.Len,_GetFormat,_Result)
    else
     Result:=TryPCharToDateTime(PAnsiChar(Value.VCharRec.P),Value.VCharRec.Len,_GetFormat,_Result);
  else
   Result:=_GetAsDouble(Value,_Result);
 end;
end;

function _GetAsBigDecimal(const Value:TZVariant;Var _Result:TBCD):Boolean;
begin
 Result:=true;
 _Result:=NullBCD;
 case Value.VType of
   vtBoolean:  ScaledOrdinal2BCD(Word(Ord(Value.VBoolean)), 0, _Result, False);
   vtInteger:  ScaledOrdinal2BCD(Value.VInteger, 0, _Result);
   vtUInteger: ScaledOrdinal2BCD(Value.VUInteger, 0, _Result, False);
   vtDouble:   Double2BCD(Value.VDouble, _Result);
   vtCurrency: Result:=CurrToBCD(Value.VCurrency, _Result);
   vtBigDecimal: _Result := Value.VBigDecimal;
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString: Result:=TryRawToBcd(PAnsiChar(Value.VRawByteString),Length(Value.VRawByteString),_Result,'.');
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString: Result:=TryUniToBcd(PWideChar(Value.VUnicodeString),Length(Value.VUnicodeString),_Result,'.');
   vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
     then Result:=TryUniToBcd(PWideChar(Value.VCharRec.P),Value.VCharRec.Len,_Result,'.')
     else Result:=TryRawToBcd(PAnsiChar(Value.VCharRec.P),Value.VCharRec.Len,_Result,'.');
   vtDateTime: Double2BCD(Value.VDateTime, _Result);
  else
    Result:=false;
 end;
end;

function _GetAsGUID(const Value:TZVariant;Var _Result:TGUID):Boolean;
var
 P:Pointer;
 L:LengthInt;
begin
 Result:=true;
 _Result:=Default(TGUID);
 case Value.VType of
   vtGUID:     _Result := Value.VGUID;
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString:  begin
                       L := Length(Value.VRawByteString);
                       if (L = 36) or (L = 38) then
                       begin
                         P := PChar(Value.VRawByteString);
                         ZSysUtils.ValidGUIDToBinary(PAnsiChar(P), @_Result.D1);
                       end;
                     end;
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString:  begin
                       L := Length(Value.VUnicodeString);
                       if (L = 36) or (L = 38) then
                       begin
                         P := PWideChar(Value.VUnicodeString);
                         ZSysUtils.ValidGUIDToBinary(PWideChar(P), @_Result.D1);
                       end;
                     end;
   vtCharRec:        if (Value.VCharRec.Len = 36) or (Value.VCharRec.Len = 38) then
                       if (Value.VCharRec.CP = zCP_UTF16)
                       then ZSysUtils.ValidGUIDToBinary(PWideChar(Value.VCharRec.P), @_Result.D1)
                       else ZSysUtils.ValidGUIDToBinary(PWideChar(Value.VCharRec.P), @_Result.D1);
   vtBytes:  if Length(Value.VRawByteString) = SizeOf(TGUID)
             then _Result := PGUID(Value.VRawByteString)^;
   else
     Result:=false;
 end;
end;

function _GetAsDate(const Value:TZVariant;var _Result:TZDate):Boolean;
var
 P:Pointer;
 D:Double;
begin
 Result:=true;
 PInt64(@_Result.Year)^ := 0;
 case Value.VType of
   vtDate: _Result := Value.VDate;
   vtTimeStamp: DateFromTimeStamp(Value.VTimeStamp, _Result);
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString: begin
                     P := PChar(Value.VRawByteString);
                     Result:=TryPCharToDate(PAnsiChar(P), Length(Value.VRawByteString), _GetFormat, _Result);
   {$IFDEF UNICODE}vtString,{$ENDIF}
   end;
   vtUnicodeString: begin
                     P := PWideChar(Value.VRawByteString);
                     Result:=TryPCharToDate(PWideChar(P), Length(Value.VRawByteString), _GetFormat, _Result);
                    end;
   vtCharRec: if (Value.VCharRec.CP = zCP_UTF16) then
              begin
               Result:=TryPCharToDate(PWideChar(Value.VCharRec.P), Value.VCharRec.Len, _GetFormat, _Result);
              end else
              begin
               Result:=TryPCharToDate(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, _GetFormat, _Result);
              end
   else
    begin
     Result:=_GetAsDouble(Value,D);
     if Result then
      ZSysUtils.DecodeDateTimeToDate(D,_Result);
    end;
 end;
end;

function _GetAsTime(const Value:TZVariant;var _Result: TZTime):Boolean;
var
 P:Pointer;
 D:Double;
begin
 Result:=true;
 PCardinal(@_Result.Hour)^ := 0;
 PInt64(@_Result.Second)^  := 0;
 case Value.VType of
   vtTime: _Result := Value.VTime;
   vtTimeStamp: ZSysUtils.TimeFromTimeStamp(Value.VTimeStamp, _Result);
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString: begin
                     P := PChar(Value.VRawByteString);
                     Result:=TryPCharToTime(PAnsiChar(P), Length(Value.VRawByteString), _GetFormat, _Result);
                    end;
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString: begin
                     P := PWideChar(Value.VRawByteString);
                     Result:=TryPCharToTime(PWideChar(P), Length(Value.VRawByteString), _GetFormat, _Result);
                    end;
   vtCharRec: if (Value.VCharRec.CP = zCP_UTF16) then
              begin
               Result:=TryPCharToTime(PWideChar(Value.VCharRec.P), Value.VCharRec.Len, _GetFormat, _Result);
              end else
              begin
               Result:=TryPCharToTime(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, _GetFormat, _Result);
              end
   else
    begin
     Result:=_GetAsDouble(Value,D);
     if Result then
      ZSysUtils.DecodeDateTimeToTime(D,_Result);
    end;
 end;
end;

function _GetAsTimeStamp(const Value: TZVariant;var _Result: TZTimeStamp):Boolean;
var
 P:Pointer;
 D:Double;
begin
 Result:=true;
 _Result:=Default(TZTimeStamp);
 case Value.VType of
   vtDate: TimeStampFromDate(Value.VDate, _Result);
   vtTime: TimeStampFromTime(Value.VTime, _Result);
   vtTimeStamp: _Result := Value.VTimeStamp;
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString: begin
                     P := PChar(Value.VRawByteString);
                     Result:=TryPCharToTimeStamp(PAnsiChar(P), Length(Value.VRawByteString), _GetFormat, _Result);
                    end;
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString: begin
                     P := PWideChar(Value.VRawByteString);
                     Result:=TryPCharToTimeStamp(PWideChar(P), Length(Value.VRawByteString), _GetFormat, _Result);
                    end;
   vtCharRec: if (Value.VCharRec.CP = zCP_UTF16) then
              begin
               Result:=TryPCharToTimeStamp(PWideChar(Value.VCharRec.P), Value.VCharRec.Len, _GetFormat, _Result);
              end else
               Result:=TryPCharToTimeStamp(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, _GetFormat, _Result);
   else
   begin
    Result:=_GetAsDouble(Value,D);
    if Result then
     ZSysUtils.DecodeDateTimeToTimeStamp(D,_Result);
   end;
 end;
end;

function _GetAsPointer(const Value: TZVariant): Pointer;
begin
 Result := nil;
 case Value.VType of
   vtInteger:  Result := Pointer(Value.VInteger);
   vtUInteger: Result := Pointer(Value.VUInteger);
   {$IFNDEF UNICODE}vtString, {$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtBytes, vtRawByteString: Result := Pointer(Value.VRawByteString);
   vtCharRec: Result := Value.VCharRec.P;
   vtUnicodeString: Result := Pointer(Value.VUnicodeString);
   vtinterface: Result := Pointer(Value.VInterface);
 end;
end;

function _ValueIsStr(const Value:TZVariant):Boolean;
begin
 Result:=false;
 case Value.VType of
  vtString,
  {$IFNDEF NO_ANSISTRING}
  vtAnsiString,
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  vtUTF8String,
  {$ENDIF}
  vtRawByteString,
  vtUnicodeString:Result:=true;
 end;
end;

function _ValueIsOrd(const Value:TZVariant):Boolean;
begin
 Result:=false;
 case Value.VType of
  vtInteger,vtUInteger:Result:=true;
 end;
end;

procedure ProcessBytes(const Value: TZVariant; out Result: TZVariant);
begin
 Result.VType := vtBytes;
 case Value.VType of
  vtNull: Result.VRawByteString := EmptyRaw;
  vtInteger, vtUInteger, vtDouble: ZSetString(PAnsiChar(@Value.VInteger), 8, Result.VRawByteString);
  vtBytes{$IFNDEF UNICODE}, vtString{$ENDIF},
  {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
  {$IFNDEF NO_UTF8STRING}vtUTF8String, {$ENDIF}
  vtRawByteString:Result.VRawByteString := Value.VRawByteString;
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString: ZSetString(PAnsiChar(Pointer(Value.VUnicodeString)), Length(Value.VUnicodeString) shl 1, Result.VRawByteString);
  vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
      then ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len shl 1, Result.VRawByteString)
      else ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, Result.VRawByteString);
  else
    Result:=Default(TZVariant);
 end;
end;

procedure ProcessCharRec(const Value: TZVariant;out Result: TZVariant);
label {$IFDEF UNICODE}AsVCharRecFromVString, {$ENDIF}AsVCharRecFromRaw;
begin
  Result.VType := vtCharRec;
  Result.VCharRec.Len := 0;
  Result.VCharRec.CP := High(Word);
  Result.VCharRec.P := nil;
  case Value.VType of
    vtBoolean, vtInteger, vtUInteger, vtDouble, vtCurrency, vtBigDecimal,
    vtBytes, vtDate, vtTime, vtTimeStamp, vtDateTime:
      begin
       {$IFDEF UNICODE}
        Result.VUnicodeString:=_GetAsUnicodeString(Value);
        Goto AsVCharRecFromVString;
        {$ELSE}
        Result.VRawByteString:=_GetAsRawByteString(Value);
        Result.VCharRec.CP := ZOSCodePage;
        goto AsVCharRecFromRaw;
        {$ENDIF}
      end;
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: begin
        Result.VCharRec.CP := ZOSCodePage;
        goto AsVCharRecFromRaw;
      end;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: begin
        Result.VCharRec.CP := zCP_UTF8;
        goto AsVCharRecFromRaw;
      end;
    {$ENDIF}
    {$IFNDEF UNICODE}
    vtString: begin
        Result.VCharRec.CP := ZOSCodePage;
        goto AsVCharRecFromRaw;
      end;
    {$ENDIF}
    vtRawByteString:
      begin
        Result.VCharRec.CP := High(Word);
AsVCharRecFromRaw:
        Result.VRawByteString := Value.VRawByteString;
        if Pointer(Result.VRawByteString) = nil then begin
          Result.VCharRec.Len := 0;
          Result.VCharRec.P := PEmptyAnsiString;
        end else begin
          Result.VCharRec.Len := PLengthInt(NativeUInt(Result.VRawByteString) - StringLenOffSet)^; //fast Length() helper
          Result.VCharRec.P := Pointer(Result.VRawByteString); //avoid RTL call of PChar conversion
        end;
      end;
    {$IFDEF UNICODE}vtString, {$ENDIF}
    vtUnicodeString:
      begin
{$IFDEF UNICODE}AsVCharRecFromVString:{$ENDIF}
        Result.VUnicodeString := Value.VUnicodeString;
        Result.VCharRec.CP := zCP_UTF16;
        Result.VCharRec.Len := Length(Result.VUnicodeString); //don't use PLengthInt helper: VUnicodeString may be Wide/Unicode-String
        if Result.VCharRec.Len = 0
        then Result.VCharRec.P := PEmptyUnicodeString
        else Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL call of PWideChar conversion
      end;
    vtCharRec:
      Result.VCharRec := Value.VCharRec;
  end;
end;

function _Convert(const Value:TZVariant;NewType:TZVariantType):TZVariant;
begin
 if Value.VType=NewType then
 begin
  Result:=Value;
 end else
 begin
  InitializeVariant(Result, NewType);
  case NewType of
   vtBoolean:    if not _GetAsBoolean(Value, Result.VBoolean) then Result:=Default(TZVariant);
   vtInteger:    if not _GetAsInteger(Value, Result.VInteger) then Result:=Default(TZVariant);
   vtUInteger:   if not _GetAsUInteger(Value, Result.VUInteger) then Result:=Default(TZVariant);
   vtDouble:     if not _GetAsDouble(Value,Result.VDouble) then Result:=Default(TZVariant);
   vtCurrency:   if not _GetAsCurrency(Value,Result.VCurrency) then Result:=Default(TZVariant);
   vtBigDecimal: if not _GetAsBigDecimal(Value, Result.VBigDecimal) then Result:=Default(TZVariant);
   vtGUID:       if not _GetAsGUID(Value, Result.VGUID) then Result:=Default(TZVariant);
   vtDate:       if not _GetAsDate(Value, Result.VDate) then Result:=Default(TZVariant);
   vtTime:       if not _GetAsTime(Value, Result.VTime) then Result:=Default(TZVariant);
   vtTimeStamp:  if not _GetAsTimeStamp(Value, Result.VTimeStamp) then Result:=Default(TZVariant);
   vtBytes:      ProcessBytes(Value, Result);
   vtDateTime:   Result.VDateTime := _GetAsDateTime(Value);
   vtPointer:    Result.VPointer  := _GetAsPointer(Value);
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString: Result.VRawByteString:=_GetAsRawByteString(Value);
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString: Result.VUnicodeString:=_GetAsUnicodeString(Value);

   vtCharRec:       ProcessCharRec(Value, Result);
  end;
 end;
end;

function _OpAdd_Int(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 if (High(Value1)-abs(Value1))<abs(Value2.VInteger) then
 begin
  Result.VType:=vtBigDecimal;
  ScaledOrdinal2Bcd(Value1,0,Result.VBigDecimal);
  BCDAdd(Result.VBigDecimal, Value2.VInteger, Result.VBigDecimal);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1+Value2.VInteger;
 end;
end;

function _OpAdd_UInt(Value1:UInt64;const Value2:TZVariant):TZVariant; inline;
Var
 T:TBCD;
begin
 if (High(Value1)-Value1)<Value2.VUInteger then
 begin
  Result.VType:=vtBigDecimal;
  T:=Default(TBCD);
  ScaledOrdinal2Bcd(Value1,0,T,false);
  ScaledOrdinal2Bcd(Value2.VUInteger,0,Result.VBigDecimal,false);
  BCDAdd(T, Result.VBigDecimal, Result.VBigDecimal);
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VUInteger:=Value1+Value2.VUInteger;
 end;
end;

function _OpAdd_Int_UInt(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if (High(Value1.VInteger)-abs(Value1.VInteger))<Value2.VUInteger then
 begin
  Result.VType:=vtBigDecimal;
  ScaledOrdinal2Bcd(Value2.VUInteger,0,Result.VBigDecimal,false);
  BCDAdd(Result.VBigDecimal, Value1.VInteger, Result.VBigDecimal);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1.VInteger+Value2.VUInteger;
 end;
end;

function _OpAdd_Curr(Value1:Currency;const Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtCurrency;
 Result.VCurrency:=Value1+Value2.VCurrency;
end;

function _OpAdd_Bool(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1.VBoolean)+Ord(Value2.VBoolean);
end;

function _OpAdd_Int_BCD(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDAdd(Value1, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpAdd_UInt_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 ScaledOrdinal2BCD(Value1.VUInteger,0,Result.VBigDecimal,False);
 BCDAdd(Result.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpAdd_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDAdd(Value1.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpAdd_Double(Value1:Double;const Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=Value2.VType;
 Result.VDouble:=Value1+Value2.VDouble;
end;

function _OpAdd_Double_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDAdd(Value1.VDouble, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpAdd_Curr_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDAdd(Value1.VCurrency, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _Concat_Raw_type(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=Value1.VType;
 Result.VRawByteString:=Value1.VRawByteString+_GetAsRawByteString(Value2);
end;

function _Concat_type_Raw(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=Value2.VType;
 Result.VRawByteString:=_GetAsRawByteString(Value1)+Value2.VRawByteString;
end;

function _Concat_Uni_type(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=Value1.VType;
 Result.VUnicodeString:=Value1.VUnicodeString+_GetAsUnicodeString(Value2);
end;

function _Concat_type_Uni(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=Value2.VType;
 Result.VUnicodeString:=_GetAsUnicodeString(Value1)+Value2.VUnicodeString;
end;

function _OpAdd(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
   vtBoolean:  case Value2.VType of
                vtBoolean:    Result:=_OpAdd_Bool   (Value1,Value2);
                vtInteger:    Result:=_OpAdd_Int    (ord(Value1.VBoolean),Value2);
                vtUInteger:   Result:=_OpAdd_UInt   (ord(Value1.VBoolean),Value2);
                vtCurrency:   Result:=_OpAdd_Curr   (ord(Value1.VBoolean),Value2);
                vtBigDecimal: Result:=_OpAdd_Int_BCD(ord(Value1.VBoolean),Value2);
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString: Result:=_Concat_type_Raw(Value1,Value2);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString: Result:=_Concat_type_Uni(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpAdd_Double(ord(Value1.VBoolean),Value2);
               end;
   vtInteger:  case Value2.VType of
                vtBoolean:    Result:=_OpAdd_Int     (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpAdd_Int     (Value1.VInteger,Value2);
                vtUInteger:   Result:=_OpAdd_Int_UInt(Value1,Value2);
                vtCurrency:   Result:=_OpAdd_Curr    (Value1.VInteger,Value2);
                vtBigDecimal: Result:=_OpAdd_Int_BCD (Value1.VInteger,Value2);
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString: Result:=_Concat_type_Raw(Value1,Value2);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString: Result:=_Concat_type_Uni(Value1,Value2);
                vtDouble,
                vtDateTime:Result:=_OpAdd_Double      (Value1.VInteger,Value2);
               end;
   vtUInteger: case Value2.VType of
                vtBoolean:    Result:=_OpAdd_UInt    (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpAdd_Int_UInt(Value2,Value1);
                vtUInteger:   Result:=_OpAdd_UInt    (Value1.VUInteger,Value2);
                vtCurrency:   Result:=_OpAdd_Curr    (Value2.VCurrency,Value1);
                vtBigDecimal: Result:=_OpAdd_UInt_BCD(Value1,Value2);
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString: Result:=_Concat_type_Raw(Value1,Value2);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString: Result:=_Concat_type_Uni(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpAdd_Double(Value1.VUInteger,Value2);
               end;
   vtDateTime,
   vtDouble:   case Value2.VType of
                vtBoolean:    Result:=_OpAdd_Double    (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpAdd_Double    (Value2.VInteger,Value1);
                vtUInteger:   Result:=_OpAdd_Double    (Value2.VUInteger,Value1);
                vtCurrency:   Result:=_OpAdd_Double    (Value2.VCurrency,Value1);
                vtBigDecimal: Result:=_OpAdd_Double_BCD(Value1,Value2);
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString: Result:=_Concat_type_Raw(Value1,Value2);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString: Result:=_Concat_type_Uni(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpAdd_Double(Value1.VDouble,Value2);
               end;
   vtCurrency: case Value2.VType of
                vtBoolean:    Result:=_OpAdd_Curr    (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpAdd_Curr    (Value2.VInteger,Value1);
                vtUInteger:   Result:=_OpAdd_Curr    (Value2.VUInteger,Value1);
                vtCurrency:   Result:=_OpAdd_Curr    (Value1.VCurrency,Value2);
                vtBigDecimal: Result:=_OpAdd_Curr_BCD(Value1,Value2);
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString: Result:=_Concat_type_Raw(Value1,Value2);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString: Result:=_Concat_type_Uni(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpAdd_Double(Value1.VCurrency,Value2);
               end;
   vtBigDecimal:case Value2.VType of
                vtBoolean:    Result:=_OpAdd_Int_BCD (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpAdd_Int_BCD (Value2.VInteger,Value1);
                vtUInteger:   Result:=_OpAdd_UInt_BCD(Value2,Value1);
                vtCurrency:   Result:=_OpAdd_Curr_BCD(Value2,Value1);
                vtBigDecimal: Result:=_OpAdd_BCD     (Value1,Value2);
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString: Result:=_Concat_type_Raw(Value1,Value2);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString: Result:=_Concat_type_Uni(Value1,Value2);
                vtDouble,
                vtDateTime:Result:=_OpAdd_Double_BCD(Value2,Value1);
               end;
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString:Result:=_Concat_Raw_type(Value1,Value2);
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString:Result:=_Concat_Uni_type(Value1,Value2);
 end;
end;

////////////

function  _OpSub_Bool(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1.VBoolean)-Ord(Value2.VBoolean);
end;

function  _OpSub_Int(Value1,Value2:Int64):TZVariant; inline;
begin
 if (Value1<Value2) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1-Value2;
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VInteger:=Value1-Value2;
 end;
end;

function  _OpSub_Int_UInt(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 if (Value1<Value2.VUInteger) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1-Value2.VUInteger;
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VInteger:=Value1-Value2.VUInteger;
 end;
end;

function  _OpSub_Int_BCD(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDSubtract(Value1, Value2.VBigDecimal, Result.VBigDecimal);
end;

function  _OpSub_UInt_Int(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if (Value1.VUInteger<Value2.VInteger) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1.VUInteger-Value2.VInteger;
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VUInteger:=Value1.VUInteger-Value2.VInteger;
 end;
end;

function  _OpSub_UInt(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if (Value1.VUInteger<Value2.VUInteger) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1.VUInteger-Value2.VUInteger;
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VUInteger:=Value1.VUInteger-Value2.VUInteger;
 end;
end;

function  _OpSub_UInt_BCD (const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 ScaledOrdinal2BCD(Value1.VUInteger,0,Result.VBigDecimal,False);
 BCDSubtract(Result.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
end;

function  _OpSub_Double_BCD (const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDSubtract(Value1.VDouble, Value2.VBigDecimal, Result.VBigDecimal);
end;

function  _OpSub_Double(Value1,Value2:Double;VType:TZVariantType):TZVariant; inline;
begin
 Result.VType:=VType;
 Result.VDouble:=Value1-Value2;
end;

function  _OpSub_Curr(Value1,Value2:Currency):TZVariant; inline;
begin
 Result.VType:=vtCurrency;
 Result.VCurrency:=Value1-Value2;
end;

function  _OpSub_Curr_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDSubtract(Value1.VCurrency, Value2.VBigDecimal, Result.VBigDecimal);
end;

function  _OpSub_BCD_Int(const Value1:TZVariant;Value2:Int64):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDSubtract(Value1.VBigDecimal, Value2, Result.VBigDecimal);
end;

function  _OpSub_BCD_UInt(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 ScaledOrdinal2BCD(Value2.VUInteger,0,Result.VBigDecimal,False);
 BCDSubtract(Value1.VBigDecimal, Result.VBigDecimal, Result.VBigDecimal);
end;

function  _OpSub_BCD_Curr(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDSubtract(Value1.VBigDecimal, Value2.VCurrency, Result.VBigDecimal);
end;

function  _OpSub_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDSubtract(Value1.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
end;

function  _OpSub_BCD_Double(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDSubtract(Value1.VBigDecimal, Value2.VDouble, Result.VBigDecimal);
end;

function _OpSub(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
   vtBoolean:  case Value2.VType of
                vtBoolean:    Result:=_OpSub_Bool     (Value1,Value2);
                vtInteger:    Result:=_OpSub_Int      (ord(Value1.VBoolean),ord(Value2.VBoolean));
                vtUInteger:   Result:=_OpSub_Int_UInt (ord(Value1.VBoolean),Value2);
                vtCurrency:   Result:=_OpSub_Curr     (ord(Value1.VBoolean),ord(Value2.VBoolean));
                vtBigDecimal: Result:=_OpSub_Int_BCD  (ord(Value1.VBoolean),Value2);
                vtDouble,
                vtDateTime:   Result:=_OpSub_Double   (ord(Value1.VBoolean),Value2.VDouble,Value2.VType);
               end;
   vtInteger:  case Value2.VType of
                vtBoolean:    Result:=_OpSub_Int      (Value1.VInteger,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpSub_Int      (Value1.VInteger,Value2.VInteger);
                vtUInteger:   Result:=_OpSub_Int_UInt (Value1.VInteger,Value2);
                vtCurrency:   Result:=_OpSub_Curr     (Value1.VInteger,Value2.VInteger);
                vtBigDecimal: Result:=_OpSub_Int_BCD  (Value1.VInteger,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpSub_Double   (Value1.VInteger,Value2.VDouble,Value2.VType);
               end;
   vtUInteger: case Value2.VType of
                vtBoolean:    Result:=_OpSub_UInt     (Value1,Value2);
                vtInteger:    Result:=_OpSub_UInt_Int (Value1,Value2);
                vtUInteger:   Result:=_OpSub_UInt     (Value1,Value2);
                vtCurrency:   Result:=_OpSub_Curr     (Value1.VUInteger,Value2.VUInteger);
                vtBigDecimal: Result:=_OpSub_UInt_BCD (Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpSub_Double   (Value1.VUInteger,Value2.VDouble,Value2.VType);
               end;
   vtDateTime,
   vtDouble:   case Value2.VType of
                vtBoolean:    Result:=_OpSub_Double    (Value1.VDouble,ord(Value2.VBoolean),Value1.VType);
                vtInteger:    Result:=_OpSub_Double    (Value1.VDouble,Value2.VInteger,Value1.VType);
                vtUInteger:   Result:=_OpSub_Double    (Value1.VDouble,Value2.VUInteger,Value1.VType);
                vtCurrency:   Result:=_OpSub_Double    (Value1.VDouble,Value2.VCurrency,Value1.VType);
                vtBigDecimal: Result:=_OpSub_Double_BCD(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpSub_Double    (Value1.VDouble,Value2.VDouble,Value1.VType);
               end;
   vtCurrency: case Value2.VType of
                vtBoolean:    Result:=_OpSub_Curr     (Value1.VCurrency,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpSub_Curr     (Value1.VCurrency,Value2.VInteger);
                vtUInteger:   Result:=_OpSub_Curr     (Value1.VCurrency,Value2.VUInteger);
                vtCurrency:   Result:=_OpSub_Curr     (Value1.VCurrency,Value2.VCurrency);
                vtBigDecimal: Result:=_OpSub_Curr_BCD (Value1,Value2);
                vtDateTime,
                vtDouble:     Result:=_OpSub_Double   (Value1.VCurrency,Value2.VDouble,Value1.VType);
               end;
   vtBigDecimal:case Value2.VType of
                vtBoolean:    Result:=_OpSub_BCD_Int (Value1,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpSub_BCD_Int (Value1,Value2.VInteger);
                vtUInteger:   Result:=_OpSub_BCD_UInt(Value1,Value2);
                vtCurrency:   Result:=_OpSub_BCD_Curr(Value1,Value2);
                vtBigDecimal: Result:=_OpSub_BCD     (Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpSub_BCD_Double(Value1,Value2);
               end;
 end;
end;

//////////////

function _OpMul_Int(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Value1*Value2.VInteger;
 if (Value1<>0) and ((Result.VInteger div Value1)<>Value2.VInteger) then
 begin
  Result.VType:=vtBigDecimal;
  ScaledOrdinal2Bcd(Value1,0,Result.VBigDecimal);
  BCDMultiply(Result.VBigDecimal, Value2.VInteger, Result.VBigDecimal);
 end;
end;

function _OpMul_UInt(Value1:UInt64;const Value2:TZVariant):TZVariant; inline;
Var
 T:TBCD;
begin
 Result.VType:=vtUInteger;
 Result.VUInteger:=Value1*Value2.VUInteger;
 if (Value1<>0) and ((Result.VInteger div Value1)<>Value2.VUInteger) then
 begin
  Result.VType:=vtBigDecimal;
  T:=Default(TBCD);
  ScaledOrdinal2Bcd(Value1,0,T,false);
  ScaledOrdinal2Bcd(Value2.VUInteger,0,Result.VBigDecimal,false);
  BCDMultiply(T, Result.VBigDecimal, Result.VBigDecimal);
 end;
end;

function _OpMul_Int_UInt(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Value1.VInteger*Value2.VUInteger;
 if (Value1.VInteger<>0) and ((Result.VInteger div Value1.VInteger)<>Value2.VUInteger) then
 begin
  Result.VType:=vtBigDecimal;
  ScaledOrdinal2Bcd(Value2.VUInteger,0,Result.VBigDecimal,false);
  BCDMultiply(Result.VBigDecimal, Value1.VInteger, Result.VBigDecimal);
 end;
end;

function _OpMul_Curr(Value1:Currency;const Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtCurrency;
 Result.VCurrency:=Value1*Value2.VCurrency;
end;

function _OpMul_Bool(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1.VBoolean)*Ord(Value2.VBoolean);
end;

function _OpMul_Int_BCD(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDMultiply(Value1, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpMul_UInt_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 ScaledOrdinal2BCD(Value1.VUInteger,0,Result.VBigDecimal,False);
 BCDMultiply(Result.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpMul_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDMultiply(Value1.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpMul_Double(Value1,Value2:Double;VType:TZVariantType):TZVariant; inline;
begin
 Result.VType:=VType;
 Result.VDouble:=Value1*Value2;
end;

function _OpMul_Double_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDMultiply(Value1.VDouble, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpMul_Curr_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 Result.VType:=vtBigDecimal;
 BCDMultiply(Value1.VCurrency, Value2.VBigDecimal, Result.VBigDecimal);
end;

function _OpMul(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
   vtBoolean:  case Value2.VType of
                vtBoolean:    Result:=_OpMul_Bool    (Value1,Value2);
                vtInteger:    Result:=_OpMul_Int     (ord(Value1.VBoolean),Value2);
                vtUInteger:   Result:=_OpMul_UInt    (ord(Value1.VBoolean),Value2);
                vtCurrency:   Result:=_OpMul_Curr    (ord(Value1.VBoolean),Value2);
                vtBigDecimal: Result:=_OpMul_Int_BCD (ord(Value1.VBoolean),Value2);
                vtDouble,
                vtDateTime:   Result:=_OpMul_Double  (ord(Value1.VBoolean),Value2.VDouble,Value2.VType);
               end;
   vtInteger:  case Value2.VType of
                vtBoolean:    Result:=_OpMul_Int     (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpMul_Int     (Value1.VInteger,Value2);
                vtUInteger:   Result:=_OpMul_Int_UInt(Value1,Value2);
                vtCurrency:   Result:=_OpMul_Curr    (Value1.VInteger,Value2);
                vtBigDecimal: Result:=_OpMul_Int_BCD (Value1.VInteger,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpMul_Double  (Value1.VInteger,Value2.VDouble,Value2.VType);
               end;
   vtUInteger: case Value2.VType of
                vtBoolean:    Result:=_OpMul_UInt    (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpMul_Int_UInt(Value2,Value1);
                vtUInteger:   Result:=_OpMul_UInt    (Value1.VUInteger,Value2);
                vtCurrency:   Result:=_OpMul_Curr    (Value2.VCurrency,Value1);
                vtBigDecimal: Result:=_OpMul_UInt_BCD(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpMul_Double (Value1.VUInteger,Value2.VDouble,Value2.VType);
               end;
   vtDateTime,
   vtDouble:   case Value2.VType of
                vtBoolean:    Result:=_OpMul_Double    (Value1.VDouble,ord(Value2.VBoolean),Value1.VType);
                vtInteger:    Result:=_OpMul_Double    (Value1.VDouble,Value2.VInteger,Value1.VType);
                vtUInteger:   Result:=_OpMul_Double    (Value1.VDouble,Value2.VUInteger,Value1.VType);
                vtCurrency:   Result:=_OpMul_Double    (Value1.VDouble,Value2.VCurrency,Value1.VType);
                vtBigDecimal: Result:=_OpMul_Double_BCD(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpMul_Double    (Value1.VDouble,Value2.VDouble,Value1.VType);
               end;
   vtCurrency: case Value2.VType of
                vtBoolean:    Result:=_OpMul_Curr    (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpMul_Curr    (Value2.VInteger,Value1);
                vtUInteger:   Result:=_OpMul_Curr    (Value2.VUInteger,Value1);
                vtCurrency:   Result:=_OpMul_Curr    (Value1.VCurrency,Value2);
                vtBigDecimal: Result:=_OpMul_Curr_BCD(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpMul_Double  (Value1.VCurrency,Value2.VDouble,Value2.VType);
               end;
   vtBigDecimal:case Value2.VType of
                vtBoolean:    Result:=_OpMul_Int_BCD (ord(Value2.VBoolean),Value1);
                vtInteger:    Result:=_OpMul_Int_BCD (Value2.VInteger,Value1);
                vtUInteger:   Result:=_OpMul_UInt_BCD(Value2,Value1);
                vtCurrency:   Result:=_OpMul_Curr_BCD(Value2,Value1);
                vtBigDecimal: Result:=_OpMul_BCD     (Value1,Value2);
                vtDouble,
                vtDateTime:Result:=_OpMul_Double_BCD(Value2,Value1);
               end;
 end;
end;

////////

function _OpPow(const Value1,Value2:TZVariant):TZVariant;
Var
 D1,D2:Double;
begin
 Result:=Default(TZVariant);
 if _GetAsDouble(Value1,D1) and _GetAsDouble(Value2,D2) then
 begin
  Result:=EncodeDouble(Power(D1,D2));
 end;
end;

/////////

function  _OpDiv_Bool(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if Ord(Value2.VBoolean)=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Ord(Value1.VBoolean) div Ord(Value2.VBoolean);
 end;
end;

function  _OpDiv_Int(Value1,Value2:Int64):TZVariant; inline;
begin
 if (Value2=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1 div Value2;
 end;
end;

function  _OpDiv_Int_UInt(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 if (Value2.VUInteger=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1 div Value2.VUInteger;
 end;
end;

function  _OpDiv_Int_BCD(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 if BCDCompare(Value2.VBigDecimal,NullBCD)=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  BCDDivide(Value1, Value2.VBigDecimal, Result.VBigDecimal);
 end;
end;

function  _OpDiv_UInt_Int(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if (Value2.VInteger=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1.VUInteger div Value2.VInteger;
 end;
end;

function  _OpDiv_UInt(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if (Value2.VUInteger=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VInteger:=Value1.VUInteger div Value2.VUInteger;
 end;
end;

function  _OpDiv_UInt_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if BCDCompare(Value2.VBigDecimal,NullBCD)=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  ScaledOrdinal2BCD(Value1.VUInteger,0,Result.VBigDecimal,False);
  BCDDivide(Result.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
 end;
end;

function  _OpDiv_Double_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if BCDCompare(Value2.VBigDecimal,NullBCD)=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  BCDDivide(Value1.VDouble, Value2.VBigDecimal, Result.VBigDecimal);
 end;
end;

function  _OpDiv_Double(Value1,Value2:Double;VType:TZVariantType):TZVariant; inline;
begin
 if Value2=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=VType;
  Result.VDouble:=Value1/Value2;
 end;
end;

type
 TCWord=record
  Case Byte of
   0:(C:Currency);
   1:(Q:QWORD);
 end;

function  _OpDiv_Curr(Value1,Value2:Currency):TZVariant; inline;
begin
 if Value2=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VUInteger:=TCWord(Value1).Q div TCWord(Value2).Q;
 end;
end;

function  _OpDiv_Curr_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if BCDCompare(Value2.VBigDecimal,NullBCD)=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  BCDDivide(Value1.VCurrency, Value2.VBigDecimal, Result.VBigDecimal);
 end;
end;

function  _OpDiv_BCD_Int(const Value1:TZVariant;Value2:Int64):TZVariant; inline;
begin
 if Value2=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  BCDDivide(Value1.VBigDecimal, Value2, Result.VBigDecimal);
 end;
end;

function  _OpDiv_BCD_UInt(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if Value2.VUInteger=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  ScaledOrdinal2BCD(Value2.VUInteger,0,Result.VBigDecimal,False);
  BCDDivide(Value1.VBigDecimal, Result.VBigDecimal, Result.VBigDecimal);
 end;
end;

function  _OpDiv_BCD_Curr(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if Value2.VCurrency=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  BCDDivide(Value1.VBigDecimal, Value2.VCurrency, Result.VBigDecimal);
 end;
end;

function  _OpDiv_BCD(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if BCDCompare(Value2.VBigDecimal,NullBCD)=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  BCDDivide(Value1.VBigDecimal, Value2.VBigDecimal, Result.VBigDecimal);
 end;
end;

function  _OpDiv_BCD_Double(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if Value2.VDouble=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtBigDecimal;
  BCDDivide(Value1.VBigDecimal, Value2.VDouble, Result.VBigDecimal);
 end;
end;

function _OpDiv(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
   vtBoolean:  case Value2.VType of
                vtBoolean:    Result:=_OpDiv_Bool    (Value1,Value2);
                vtInteger:    Result:=_OpDiv_Int     (ord(Value1.VBoolean),ord(Value2.VBoolean));
                vtUInteger:   Result:=_OpDiv_Int_UInt(ord(Value1.VBoolean),Value2);
                vtCurrency:   Result:=_OpDiv_Curr    (ord(Value1.VBoolean),ord(Value2.VBoolean));
                vtBigDecimal: Result:=_OpDiv_Int_BCD (ord(Value1.VBoolean),Value2);
                vtDouble,
                vtDateTime:   Result:=_OpDiv_Double  (ord(Value1.VBoolean),Value2.VDouble,Value2.VType);
               end;
   vtInteger:  case Value2.VType of
                vtBoolean:    Result:=_OpDiv_Int     (Value1.VInteger,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpDiv_Int     (Value1.VInteger,Value2.VInteger);
                vtUInteger:   Result:=_OpDiv_Int_UInt(Value1.VInteger,Value2);
                vtCurrency:   Result:=_OpDiv_Curr    (Value1.VInteger,Value2.VInteger);
                vtBigDecimal: Result:=_OpDiv_Int_BCD (Value1.VInteger,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpDiv_Double  (Value1.VInteger,Value2.VDouble,Value2.VType);
               end;
   vtUInteger: case Value2.VType of
                vtBoolean:    Result:=_OpDiv_UInt    (Value1,Value2);
                vtInteger:    Result:=_OpDiv_UInt_Int(Value1,Value2);
                vtUInteger:   Result:=_OpDiv_UInt    (Value1,Value2);
                vtCurrency:   Result:=_OpDiv_Curr    (Value1.VUInteger,Value2.VUInteger);
                vtBigDecimal: Result:=_OpDiv_UInt_BCD(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpDiv_Double  (Value1.VUInteger,Value2.VDouble,Value2.VType);
               end;
   vtDateTime,
   vtDouble:   case Value2.VType of
                vtBoolean:    Result:=_OpDiv_Double    (Value1.VDouble,ord(Value2.VBoolean),Value1.VType);
                vtInteger:    Result:=_OpDiv_Double    (Value1.VDouble,Value2.VInteger,Value1.VType);
                vtUInteger:   Result:=_OpDiv_Double    (Value1.VDouble,Value2.VUInteger,Value1.VType);
                vtCurrency:   Result:=_OpDiv_Double    (Value1.VDouble,Value2.VCurrency,Value1.VType);
                vtBigDecimal: Result:=_OpDiv_Double_BCD(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpDiv_Double    (Value1.VDouble,Value2.VDouble,Value1.VType);
               end;
   vtCurrency: case Value2.VType of
                vtBoolean:    Result:=_OpDiv_Curr    (Value1.VCurrency,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpDiv_Curr    (Value1.VCurrency,Value2.VInteger);
                vtUInteger:   Result:=_OpDiv_Curr    (Value1.VCurrency,Value2.VUInteger);
                vtCurrency:   Result:=_OpDiv_Curr    (Value1.VCurrency,Value2.VCurrency);
                vtBigDecimal: Result:=_OpDiv_Curr_BCD(Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpDiv_Double  (Value1.VCurrency,Value2.VDouble,Value1.VType);
               end;
   vtBigDecimal:case Value2.VType of
                vtBoolean:    Result:=_OpDiv_BCD_Int (Value1,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpDiv_BCD_Int (Value1,Value2.VInteger);
                vtUInteger:   Result:=_OpDiv_BCD_UInt(Value1,Value2);
                vtCurrency:   Result:=_OpDiv_BCD_Curr(Value1,Value2);
                vtBigDecimal: Result:=_OpDiv_BCD     (Value1,Value2);
                vtDouble,
                vtDateTime:   Result:=_OpDiv_BCD_Double(Value1,Value2);
               end;
 end;
end;

/////////

function  _OpMod_Bool(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if Ord(Value2.VBoolean)=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Ord(Value1.VBoolean) mod Ord(Value2.VBoolean);
 end;
end;

function  _OpMod_Int(Value1,Value2:Int64):TZVariant; inline;
begin
 if (Value2=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1 mod Value2;
 end;
end;

function  _OpMod_Int_UInt(Value1:Int64;const Value2:TZVariant):TZVariant; inline;
begin
 if (Value2.VUInteger=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1 mod Value2.VUInteger;
 end;
end;

function  _OpMod_UInt_Int(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if (Value2.VInteger=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=Value1.VUInteger mod Value2.VInteger;
 end;
end;

function  _OpMod_UInt(const Value1,Value2:TZVariant):TZVariant; inline;
begin
 if (Value2.VUInteger=0) then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtUInteger;
  Result.VInteger:=Value1.VUInteger mod Value2.VUInteger;
 end;
end;

function  _OpMod_Curr(Value1,Value2:Currency):TZVariant; inline;
begin
 if Value2=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=vtCurrency;
  Result.VCurrency:=TCWord(TCWord(Value1).Q mod TCWord(Value2).Q).C;
 end;
end;

function  _OpMod_Double(Value1,Value2:Double;VType:TZVariantType):TZVariant; inline;
begin
 if Value2=0 then
 begin
  Result:=Default(TZVariant);
 end else
 begin
  Result.VType:=VType;
  Result.VDouble:=Value1-Trunc(Value1/Value2)*Value2;
 end;
end;

function _OpMod(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
   vtBoolean:  case Value2.VType of
                vtBoolean:    Result:=_OpMod_Bool    (Value1,Value2);
                vtInteger:    Result:=_OpMod_Int     (ord(Value1.VBoolean),ord(Value2.VBoolean));
                vtUInteger:   Result:=_OpMod_Int_UInt(ord(Value1.VBoolean),Value2);
                vtCurrency:   Result:=_OpMod_Curr    (ord(Value1.VBoolean),ord(Value2.VBoolean));
                vtDouble,
                vtDateTime:   Result:=_OpMod_Double  (ord(Value1.VBoolean),Value2.VDouble,Value2.VType);
               end;
   vtInteger:  case Value2.VType of
                vtBoolean:    Result:=_OpMod_Int     (Value1.VInteger,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpMod_Int     (Value1.VInteger,Value2.VInteger);
                vtUInteger:   Result:=_OpMod_Int_UInt(Value1.VInteger,Value2);
                vtCurrency:   Result:=_OpMod_Curr    (Value1.VInteger,Value2.VInteger);
                vtDouble,
                vtDateTime:   Result:=_OpMod_Double  (Value1.VInteger,Value2.VDouble,Value2.VType);
               end;
   vtUInteger: case Value2.VType of
                vtBoolean:    Result:=_OpMod_UInt    (Value1,Value2);
                vtInteger:    Result:=_OpMod_UInt_Int(Value1,Value2);
                vtUInteger:   Result:=_OpMod_UInt    (Value1,Value2);
                vtCurrency:   Result:=_OpMod_Curr    (Value1.VUInteger,Value2.VUInteger);
                vtDouble,
                vtDateTime:   Result:=_OpMod_Double  (Value1.VUInteger,Value2.VDouble,Value2.VType);
               end;
   vtCurrency: case Value2.VType of
                vtBoolean:    Result:=_OpMod_Curr    (Value1.VCurrency,ord(Value2.VBoolean));
                vtInteger:    Result:=_OpMod_Curr    (Value1.VCurrency,Value2.VInteger);
                vtUInteger:   Result:=_OpMod_Curr    (Value1.VCurrency,Value2.VUInteger);
                vtCurrency:   Result:=_OpMod_Curr    (Value1.VCurrency,Value2.VCurrency);
                vtDouble,
                vtDateTime:   Result:=_OpMod_Double  (Value1.VCurrency,Value2.VDouble,Value2.VType);
               end;
   vtDouble,
   vtDateTime:case Value2.VType of
                vtBoolean:    Result:=_OpMod_Double(Value1.VDouble,ord(Value2.VBoolean),Value1.VType);
                vtInteger:    Result:=_OpMod_Double(Value1.VDouble,Value2.VInteger,Value1.VType);
                vtUInteger:   Result:=_OpMod_Double(Value1.VDouble,Value2.VUInteger,Value1.VType);
                vtCurrency:   Result:=_OpMod_Double(Value1.VDouble,Value2.VCurrency,Value1.VType);
                vtDouble,
                vtDateTime:   Result:=_OpMod_Double(Value1.VDouble,Value2.VDouble,Value2.VType);
               end;
 end;
end;

/////////

function _IsNull(const Value:TZVariant):Boolean; inline;
begin
 Result:=Value.VType=vtNull;
end;

function _CompareDiff(const Diff: Double): Integer; inline;
begin
 Result:= Ord(Diff>FLOAT_COMPARE_PRECISION)-Ord(Diff<-FLOAT_COMPARE_PRECISION)
end;

function _Compare_Double(Value1,Value2:Double):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=_CompareDiff(Value1-Value2);
end;

function _Compare_Curr(const Value1,Value2:Currency):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1>Value2)-Ord(Value1<Value2);
end;

function _Compare_Int(Value1,Value2:Int64):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1>Value2)-Ord(Value1<Value2);
end;

function _Compare_Int_UInt(Value1:Int64;Value2:UInt64):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1>Value2)-Ord(Value1<Value2);
end;

function _Compare_UInt_Int(Value1:UInt64;Value2:Int64):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1>Value2)-Ord(Value1<Value2);
end;

function _Compare_UInt(Value1,Value2:UInt64):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=Ord(Value1>Value2)-Ord(Value1<Value2);
end;

function _Compare_BCD(const Value1,Value2:TBCD):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=BCDCompare(Value1,Value2);
end;

function _Compare_Int_BCD(Value1:Int64;const Value2:TBCD):TZVariant; inline;
Var
 T:TBCD;
begin
 ScaledOrdinal2Bcd(Value1,0,T);
 Result:=_Compare_BCD(T,Value2);
end;


function _Compare_BCD_Int(const Value1:TBCD;Value2:Int64):TZVariant; inline;
Var
 T:TBCD;
begin
 ScaledOrdinal2Bcd(Value2,0,T);
 Result:=_Compare_BCD(Value1,T);
end;

function _Compare_UInt_BCD(Value1:UInt64;const Value2:TBCD):TZVariant; inline;
Var
 T:TBCD;
begin
 ScaledOrdinal2BCD(Value1,0,T,False);
 Result:=_Compare_BCD(T,Value2)
end;

function _Compare_BCD_UInt(const Value1:TBCD;Value2:UInt64):TZVariant; inline;
Var
 T:TBCD;
begin
 ScaledOrdinal2BCD(Value2,0,T,False);
 Result:=_Compare_BCD(Value1,T)
end;

function _Compare_Double_BCD(Value1:Double;const Value2:TBCD):TZVariant; inline;
begin
 Result:=_Compare_BCD(DoubleToBCD(Value1),Value2)
end;

function _Compare_BCD_Double(const Value1:TBCD;Value2:Double):TZVariant; inline;
begin
 Result:=_Compare_BCD(Value1,DoubleToBCD(Value2))
end;

function _Compare_Curr_BCD(Value1:Currency;const Value2:TBCD):TZVariant; inline;
begin
 Result:=_Compare_BCD(CurrToBCD(Value1),Value2)
end;

function _Compare_BCD_Curr(const Value1:TBCD;const Value2:Currency):TZVariant; inline;
begin
 Result:=_Compare_BCD(Value1,CurrToBCD(Value2))
end;

function _Compare_Raw(const Value1,Value2:RawByteString):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=AnsiCompareStr(Value1,Value2);
end;

function _Compare_uni(const Value1,Value2:UnicodeString):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=WideCompareStr(Value1,Value2);
end;

function _Compare_DateTime(Value1,Value2:TDateTime):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=ZCompareDateTime(Value1,Value2);
end;

function _Compare_GUID(const Value1,Value2:TGUID):TZVariant; inline;
begin
 Result.VType:=vtInteger;
 Result.VInteger:=ZSysUtils.ZMemLComp(@Value1.D1,@Value2.D1,SizeOf(TGUID));
end;

function _Compare_GUID_type(const Value1:TGUID;Const Value2:TZVariant):TZVariant; inline;
Var
 T:TGUID;
begin
 if _GetAsGUID(Value2,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZSysUtils.ZMemLComp(@Value1.D1,@T.D1,SizeOf(TGUID));
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

function _Compare_type_GUID(Const Value1:TZVariant;const Value2:TGUID):TZVariant; inline;
Var
 T:TGUID;
begin
 if _GetAsGUID(Value1,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZSysUtils.ZMemLComp(@T.D1,@Value2.D1,SizeOf(TGUID));
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

function _Compare_DateTime_type(const Value1:TDateTime;Const Value2:TZVariant):TZVariant; inline;
Var
 T:TDateTime;
begin
 if _GetAsDateTime(Value2,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareDateTime(Value1,T);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

function _Compare_type_DateTime(Const Value1:TZVariant;const Value2:TDateTime):TZVariant; inline;
Var
 T:TDateTime;
begin
 if _GetAsDateTime(Value1,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareDateTime(T,Value2);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

//

function _Compare_Date_type(const Value1:TZDate;Const Value2:TZVariant):TZVariant; inline;
Var
 T:TZDate;
begin
 if _GetAsDate(Value2,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareDate(Value1,T);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

function _Compare_type_Date(Const Value1:TZVariant;const Value2:TZDate):TZVariant; inline;
Var
 T:TZDate;
begin
 if _GetAsDate(Value1,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareDate(T,Value2);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

//

function _Compare_Time_type(const Value1:TZTime;Const Value2:TZVariant):TZVariant; inline;
Var
 T:TZTime;
begin
 if _GetAsTime(Value2,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareTime(Value1,T);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

function _Compare_type_Time(Const Value1:TZVariant;const Value2:TZTime):TZVariant; inline;
Var
 T:TZTime;
begin
 if _GetAsTime(Value1,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareTime(T,Value2);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

//

function _Compare_TimeStamp_type(const Value1:TZTimeStamp;Const Value2:TZVariant):TZVariant; inline;
Var
 T:TZTimeStamp;
begin
 if _GetAsTimeStamp(Value2,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareTimeStamp(Value1,T);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

function _Compare_type_TimeStamp(Const Value1:TZVariant;const Value2:TZTimeStamp):TZVariant; inline;
Var
 T:TZTimeStamp;
begin
 if _GetAsTimeStamp(Value1,T) then
 begin
  Result.VType:=vtInteger;
  Result.VInteger:=ZCompareTimeStamp(T,Value2);
 end else
 begin
  Result:=Default(TZVariant);
 end;
end;

function _Compare(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
   vtBoolean:  case Value2.VType of
                vtPointer:    Result:=_Compare_UInt    (ord(Value1.VBoolean),PtrUint(Value2.VPointer));
                vtBoolean:    Result:=_Compare_Int     (ord(Value1.VBoolean),Ord(Value2.VBoolean));
                vtInteger:    Result:=_Compare_Int     (ord(Value1.VBoolean),Value2.VInteger);
                vtUInteger:   Result:=_Compare_UInt    (ord(Value1.VBoolean),Value2.VUInteger);
                vtCurrency:   Result:=_Compare_Curr    (ord(Value1.VBoolean),Value2.VCurrency);
                vtBigDecimal: Result:=_Compare_Int_BCD (ord(Value1.VBoolean),Value2.VBigDecimal);
                vtDouble:     Result:=_Compare_Double  (ord(Value1.VBoolean),Value2.VDouble);
                vtDateTime:   Result:=_Compare_DateTime(ord(Value1.VBoolean),Value2.VDateTime);
                vtDate:       Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:       Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:  Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   vtInteger:  case Value2.VType of
                vtPointer:    Result:=_Compare_Int_UInt(Value1.VInteger,PtrUint(Value2.VPointer));
                vtBoolean:    Result:=_Compare_Int     (Value1.VInteger,ord(Value2.VBoolean));
                vtInteger:    Result:=_Compare_Int     (Value1.VInteger,Value2.VInteger);
                vtUInteger:   Result:=_Compare_Int_UInt(Value1.VInteger,Value2.VUInteger);
                vtCurrency:   Result:=_Compare_Curr    (Value1.VInteger,Value2.VCurrency);
                vtBigDecimal: Result:=_Compare_Int_BCD (Value1.VInteger,Value2.VBigDecimal);
                vtDouble:     Result:=_Compare_Double  (Value1.VInteger,Value2.VDouble);
                vtDateTime:   Result:=_Compare_DateTime(Value1.VInteger,Value2.VDateTime);
                vtDate:       Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:       Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:  Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   vtUInteger: case Value2.VType of
                vtPointer:    Result:=_Compare_UInt    (Value1.VUInteger,PtrUint(Value2.VPointer));
                vtBoolean:    Result:=_Compare_UInt    (Value1.VUInteger,ord(Value2.VBoolean));
                vtInteger:    Result:=_Compare_UInt_Int(Value1.VUInteger,Value2.VInteger);
                vtUInteger:   Result:=_Compare_UInt    (Value1.VUInteger,Value2.VUInteger);
                vtCurrency:   Result:=_Compare_Curr    (Value1.VUInteger,Value2.VCurrency);
                vtBigDecimal: Result:=_Compare_UInt_BCD(Value1.VUInteger,Value2.VBigDecimal);
                vtDouble:     Result:=_Compare_Double  (Value1.VUInteger,Value2.VDouble);
                vtDateTime:   Result:=_Compare_DateTime(Value1.VUInteger,Value2.VDateTime);
                vtDate:       Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:       Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:  Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   vtDateTime: case Value2.VType of
                vtBoolean:    Result:=_Compare_DateTime  (Value1.VDateTime,ord(Value2.VBoolean));
                vtInteger:    Result:=_Compare_DateTime  (Value1.VDateTime,Value2.VInteger);
                vtUInteger:   Result:=_Compare_DateTime  (Value1.VDateTime,Value2.VUInteger);
                vtBigDecimal: Result:=_Compare_Double_BCD(Value1.VDateTime,Value2.VBigDecimal);
                vtCurrency:   Result:=_Compare_DateTime  (Value1.VDateTime,Value2.VCurrency);
                vtDouble,
                vtDateTime:   Result:=_Compare_DateTime  (Value1.VDateTime,Value2.VDateTime);
                else
                              Result:=_Compare_DateTime_type(Value1.VDateTime,Value2);
               end;
   vtDouble:   case Value2.VType of
                vtBoolean:    Result:=_Compare_Double    (Value1.VDouble,ord(Value2.VBoolean));
                vtInteger:    Result:=_Compare_Double    (Value1.VDouble,Value2.VInteger);
                vtUInteger:   Result:=_Compare_Double    (Value1.VDouble,Value2.VUInteger);
                vtBigDecimal: Result:=_Compare_Double_BCD(Value1.VDouble,Value2.VBigDecimal);
                vtCurrency:   Result:=_Compare_Double    (Value1.VDouble,Value2.VCurrency);
                vtDouble:     Result:=_Compare_Double    (Value1.VDouble,Value2.VDouble);
                vtDateTime:   Result:=_Compare_DateTime  (Value1.VDateTime,Value2.VDateTime);
                vtDate:       Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:       Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:  Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   vtCurrency: case Value2.VType of
                vtBoolean:    Result:=_Compare_Curr    (Value1.VCurrency,ord(Value2.VBoolean));
                vtInteger:    Result:=_Compare_Curr    (Value1.VCurrency,Value2.VInteger);
                vtUInteger:   Result:=_Compare_Curr    (Value1.VCurrency,Value2.VUInteger);
                vtCurrency:   Result:=_Compare_Curr    (Value1.VCurrency,Value2.VCurrency);
                vtBigDecimal: Result:=_Compare_Curr_BCD(Value1.VCurrency,Value2.VBigDecimal);
                vtDouble:     Result:=_Compare_Double  (Value1.VCurrency,Value2.VDouble);
                vtDateTime:   Result:=_Compare_DateTime(Value1.VCurrency,Value2.VDateTime);
                vtDate:       Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:       Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:  Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   vtBigDecimal:case Value2.VType of
                vtBoolean:    Result:=_Compare_BCD_Int (Value1.VBigDecimal,ord(Value2.VBoolean));
                vtInteger:    Result:=_Compare_BCD_Int (Value1.VBigDecimal,Value2.VInteger);
                vtUInteger:   Result:=_Compare_BCD_UInt(Value1.VBigDecimal,Value2.VUInteger);
                vtCurrency:   Result:=_Compare_BCD_Curr(Value1.VBigDecimal,Value2.VCurrency);
                vtBigDecimal: Result:=_Compare_BCD     (Value1.VBigDecimal,Value2.VBigDecimal);
                vtDouble,
                vtDateTime:   Result:=_Compare_BCD_Double(Value1.VBigDecimal,Value2.VDouble);
                vtDate:       Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:       Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:  Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   {$IFNDEF UNICODE}vtString,{$ENDIF}
   {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
   {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
   vtRawByteString:
               case Value2.VType of
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString:Result:=_Compare_Raw(Value1.VRawByteString,Value2.VRawByteString);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString:Result:=_Compare_uni(UnicodeString(Value1.VRawByteString),Value2.VUnicodeString);
                vtCharRec:if (Value2.VCharRec.CP=zCP_UTF16) then
                           Result:=_Compare_uni(UnicodeString(Value1.VRawByteString),PWideChar(Value2.VCharRec.P))
                          else
                           Result:=_Compare_Raw(Value1.VRawByteString,PAnsiChar(Value2.VCharRec.P));
                vtGUID:    Result:=_Compare_type_GUID(Value1,Value2.VGUID);
                vtDateTime:Result:=_Compare_type_DateTime(Value1,Value2.VDateTime);
                vtDate:    Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:    Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString:
               case Value2.VType of
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString:Result:=_Compare_uni(Value1.VUnicodeString,UnicodeString(Value2.VRawByteString));
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString:Result:=_Compare_uni(Value1.VUnicodeString,Value2.VUnicodeString);
                vtCharRec:if (Value2.VCharRec.CP=zCP_UTF16) then
                           Result:=_Compare_uni(Value1.VUnicodeString,PWideChar(Value2.VCharRec.P))
                          else
                           Result:=_Compare_uni(Value1.VUnicodeString,UnicodeString(PAnsiChar(Value2.VCharRec.P)));
                vtGUID:    Result:=_Compare_type_GUID(Value1,Value2.VGUID);
                vtDateTime:Result:=_Compare_type_DateTime(Value1,Value2.VDateTime);
                vtDate:    Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:    Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;
   vtCharRec:  case Value2.VType of
                {$IFNDEF UNICODE}vtString,{$ENDIF}
                {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                vtRawByteString:if (Value1.VCharRec.CP=zCP_UTF16) then
                                 Result:=_Compare_uni(PWideChar(Value1.VCharRec.P),UnicodeString(Value2.VRawByteString))
                                else
                                 Result:=_Compare_Raw(PAnsiChar(Value1.VCharRec.P),Value2.VRawByteString);
                {$IFDEF UNICODE}vtString,{$ENDIF}
                vtUnicodeString:if (Value1.VCharRec.CP=zCP_UTF16) then
                                 Result:=_Compare_uni(PWideChar(Value1.VCharRec.P),Value2.VUnicodeString)
                                else
                                 Result:=_Compare_uni(PAnsiChar(Value1.VCharRec.P),Value2.VUnicodeString);

                vtCharRec:if (Value1.VCharRec.CP=zCP_UTF16) then
                          begin
                           if (Value2.VCharRec.CP=zCP_UTF16) then
                            Result:=_Compare_uni(PWideChar(Value1.VCharRec.P),PWideChar(Value2.VCharRec.P))
                           else
                            Result:=_Compare_uni(PWideChar(Value1.VCharRec.P),UnicodeString(PAnsiChar(Value2.VCharRec.P)));
                          end else
                          begin
                           if (Value2.VCharRec.CP=zCP_UTF16) then
                            Result:=_Compare_uni(UnicodeString(PAnsiChar(Value1.VCharRec.P)),PWideChar(Value2.VCharRec.P))
                           else
                            Result:=_Compare_raw(PAnsiChar(Value1.VCharRec.P),PAnsiChar(Value2.VCharRec.P));
                          end;
                vtGUID:    Result:=_Compare_type_GUID(Value1,Value2.VGUID);
                vtDateTime:Result:=_Compare_type_DateTime(Value1,Value2.VDateTime);
                vtDate:    Result:=_Compare_type_Date(Value1,Value2.VDate);
                vtTime:    Result:=_Compare_type_Time(Value1,Value2.VTime);
                vtTimeStamp:Result:=_Compare_type_TimeStamp(Value1,Value2.VTimeStamp);
               end;

   vtGUID:Result:=_Compare_GUID_type(Value1.VGUID,Value2);

   vtPointer:case Value2.VType of
              vtPointer:    Result:=_Compare_UInt    (PtrUint(Value1.VPointer),PtrUint(Value2.VPointer));
              vtBoolean:    Result:=_Compare_UInt    (PtrUint(Value1.VPointer),Ord(Value2.VBoolean));
              vtInteger:    Result:=_Compare_Int_Uint(PtrUint(Value1.VPointer),Value2.VInteger);
              vtUInteger:   Result:=_Compare_UInt    (PtrUint(Value1.VPointer),Value2.VUInteger);
             end;

   vtDate:Result:=_Compare_Date_type(Value1.VDate,Value2);
   vtTime:Result:=_Compare_Time_type(Value1.VTime,Value2);
   vtTimeStamp:Result:=_Compare_TimeStamp_type(Value1.VTimeStamp,Value2);

 end;
end;

//'='
function _OpEqual(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=_Compare(Value1,Value2);
 if (Result.VType=vtInteger) then
 begin
  Result.VType:=vtBoolean;
  Result.VBoolean:=(Result.VInteger=0);
 end;
end;

//'<>'
function _OpNotEqual(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=_Compare(Value1,Value2);
 if (Result.VType=vtInteger) then
 begin
  Result.VType:=vtBoolean;
  Result.VBoolean:=(Result.VInteger<>0);
 end;
end;

//'<'
function _OpLess(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=_Compare(Value1,Value2);
 if (Result.VType=vtInteger) then
 begin
  Result.VType:=vtBoolean;
  Result.VBoolean:=(Result.VInteger<0);
 end;
end;

//'<='
function _OpLessEqual(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=_Compare(Value1,Value2);
 if (Result.VType=vtInteger) then
 begin
  Result.VType:=vtBoolean;
  Result.VBoolean:=(Result.VInteger<=0);
 end;
end;

//'>'
function _OpMore(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=_Compare(Value1,Value2);
 if (Result.VType=vtInteger) then
 begin
  Result.VType:=vtBoolean;
  Result.VBoolean:=(Result.VInteger>0);
 end;
end;

//'>='
function _OpMoreEqual(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=_Compare(Value1,Value2);
 if (Result.VType=vtInteger) then
 begin
  Result.VType:=vtBoolean;
  Result.VBoolean:=(Result.VInteger>=0);
 end;
end;

//

function _OpNot(const Value:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value.VType of
  vtBoolean: Result:=EncodeBoolean (not Value.VBoolean);
  vtInteger: Result:=EncodeInteger (not Value.VInteger);
  vtUInteger:Result:=EncodeUInteger(not Value.VUInteger);
 end;
end;

function _OpAnd(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
  vtBoolean: case Value2.VType of
              vtBoolean: Result:=EncodeBoolean (Value1.VBoolean and Value2.VBoolean);
              vtInteger: Result:=EncodeInteger (ord(Value1.VBoolean) and Value2.VInteger);
              vtUInteger:Result:=EncodeInteger (ord(Value1.VBoolean) and Value2.VUInteger);
              vtNull:    if (not Value1.VBoolean) then
                          Result:=EncodeBoolean(false);
             end;
  vtInteger: case Value2.VType of
              vtBoolean: Result:=EncodeInteger (Value1.VInteger and ord(Value2.VBoolean));
              vtInteger: Result:=EncodeInteger (Value1.VInteger and Value2.VInteger);
              vtUInteger:Result:=EncodeUInteger(Value1.VInteger and Value2.VUInteger);
             end;
  vtUInteger:case Value2.VType of
              vtBoolean: Result:=EncodeUInteger (Value1.VUInteger and ord(Value2.VBoolean));
              vtInteger: Result:=EncodeUInteger (Value1.VUInteger and Value2.VInteger);
              vtUInteger:Result:=EncodeUInteger (Value1.VUInteger and Value2.VUInteger);
             end;
  vtNull:    case Value2.VType of
              vtBoolean:if (not Value2.VBoolean) then
                         Result:=EncodeBoolean(false);
             end;
 end;
end;

function _OpOr(const Value1,Value2:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);
 case Value1.VType of
  vtBoolean: case Value2.VType of
              vtBoolean: Result:=EncodeBoolean (Value1.VBoolean or Value2.VBoolean);
              vtInteger: Result:=EncodeInteger (ord(Value1.VBoolean) or Value2.VInteger);
              vtUInteger:Result:=EncodeInteger (ord(Value1.VBoolean) or Value2.VUInteger);
              vtNull:    if Value1.VBoolean then
                          Result:=EncodeBoolean(true);
             end;
  vtInteger: case Value2.VType of
              vtBoolean: Result:=EncodeInteger (Value1.VInteger or ord(Value2.VBoolean));
              vtInteger: Result:=EncodeInteger (Value1.VInteger or Value2.VInteger);
              vtUInteger:Result:=EncodeUInteger(Value1.VInteger or Value2.VUInteger);
             end;
  vtUInteger:case Value2.VType of
              vtBoolean: Result:=EncodeUInteger (Value1.VUInteger or ord(Value2.VBoolean));
              vtInteger: Result:=EncodeUInteger (Value1.VUInteger or Value2.VInteger);
              vtUInteger:Result:=EncodeUInteger (Value1.VUInteger or Value2.VUInteger);
             end;
  vtNull:    case Value2.VType of
              vtBoolean:if Value2.VBoolean then
                          Result:=EncodeBoolean(true);
             end;
 end;
end;

/////////

function DoAllOp(id:Byte;Const L,R:TZVariant):TZVariant;
Var
 Z:IZResultSet;
 V:TZVariant;
begin
 Result.VType   :=vtBoolean;
 Result.VBoolean:=false;
 if (R.VType<>vtInterface) then Exit;
 Z:=IZResultSet(R.VInterface);
 Case id of
  8..16:begin
         try
          if Z.Next then
           repeat
            V:=DoTokenOp(id,0,L,Z.GetValue(1));
            if (V.VType<>vtBoolean) or (not V.VBoolean) then Exit;
           until (not Z.Next);
          Result.VBoolean:=True;
         except
         end;
        end;
 end;
end;

function DoAnyOp(id:Byte;Const L,R:TZVariant):TZVariant;
Var
 Z:IZResultSet;
 V:TZVariant;
begin
 Result.VType   :=vtBoolean;
 Result.VBoolean:=false;
 if (R.VType<>vtInterface) then Exit;
 Z:=IZResultSet(R.VInterface);
 Case id of
  8..16:begin
         try
          if Z.Next then
           repeat
            V:=DoTokenOp(id,0,L,Z.GetValue(1));
            if (V.VType=vtBoolean) and (V.VBoolean) then
            begin
             Result.VBoolean:=True;
             Exit;
            end;
           until (not Z.Next);
         except
         end;
        end;
 end;
end;

function DoInOp(id:Byte;Const L,R:TZVariant):TZVariant;
Var
 Z:IZResultSet;
 V:TZVariant;
begin
 if (L.VType=vtNull) then
 begin
  Result.VType:=vtNull;
  Exit;
 end;
 Result.VType   :=vtBoolean;
 Result.VBoolean:=(id<>0);
 if (R.VType<>vtInterface) then Exit;
 Z:=IZResultSet(R.VInterface);
 try
  if Z.Next then
   repeat
    V:=_OpEqual(L,Z.GetValue(1));
    Case V.VType of
     vtNull:   begin
                Result.VType:=vtNull;
                Exit;
               end;
     vtBoolean:if V.VBoolean then
               begin
                Result.VBoolean:=(id=0);
                Exit;
               end;
    end;
   until (not Z.Next);
 except
 end;
end;

function DoIsNULLOp(id:Byte;Const R:TZVariant):TZVariant;
begin
 Result.VType:=vtBoolean;
 Result.VBoolean:=(R.VType=vtNull) xor (id<>0);
end;

{
 VType=vtNull id<>0
 false        false = false
 false        true  = true
 true         false = true
 true         true  = false
}

/////////

function DoTokenOp(id1,id2:Byte;Const L,R:TZVariant):TZVariant;
begin
 Result:=Default(TZVariant);

 Case id1 of
   13,9:Result:=_OpMoreEqual(L,R);//'!<','>='
  14,10:Result:=_OpLessEqual(L,R);//'!>','<='
  11,12:Result:=_OpNotEqual(L,R); //'<>','!='
      8:Result:=_OpEqual(L,R);    //'='
     15:Result:=_OpMore(L,R);     //'>'
     16:Result:=_OpLess(L,R);     //'<'
     17:Result:=_OpAdd(L,R);      //'+'
     18:Result:=_OpSub(L,R);      //'-'
     20:Result:=_OpPow(L,R);      //'^'
     22:Result:=_OpMul(L,R);      //'*'
     23:Result:=_OpDiv(L,R);      //'/'
     24:Result:=_OpMod(L,R);      //'%'
  25,26:Result:=_OpNot(R);        //'~','NOT'
  19,27:Result:=_OpAnd(L,R);      //'&','AND'
  21,28:Result:=_OpOr(L,R);       //'|','OR'

  29:Result:=DoInOp (id2,L,R); //'IN'
  30:Result:=DoAllOp(id2,L,R); //'ALL'
  31,                          //'ANY'
  32:Result:=DoAnyOp(id2,L,R); //'SOME'

  35:Result:=DoIsNULLOp(id2,R);
 end;

 //Writeln(_GetAsRawByteString(L),' ',_get_op_name(id),' ',_GetAsRawByteString(R),'=',_GetAsRawByteString(Result),':',Result.VType);

end;

procedure DoAssignOp(id:Byte;var L:TZVariant;Const R:TZVariant);
begin
 Case id of
  0:L:=_OpAdd(L,R); //'+='
  1:L:=_OpSub(L,R); //'-='
  6:L:=_OpPow(L,R); //'^='
  2:L:=_OpMul(L,R); //'*='
  3:L:=_OpDiv(L,R); //'/='
  4:L:=_OpMod(L,R); //'%='
  5:L:=_OpAnd(L,R); //'&='
  7:L:=_OpOr(L,R);  //'|='
  8:L:=R;           //'='
 end;
end;

Function DecodeVariable(P:PChar;L:SizeInt):RawByteString;
begin
 Result:='';
 if (L>=1) and ((P^=':') or (P^='@')) then
 begin
  Inc(P);
  Dec(L);
  if (L>=1) and ({(P^=':') or (P^='@') or} (P^='+')) then
  begin
   Inc(P);
   Dec(L);
  end;
 end;
 SetString(Result,P,L);
end;

Function GetVariableType(P:PChar;L:SizeInt):Byte;
begin
 Result:=0; //not Variable
 if (L>=2) and ((P^=':') or (P^='@')) then
  Case P[1] of
   ':','@':Result:=2; //system
   '+'    :Result:=3; //concatate
   else    Result:=1; //simple
  end;
end;

Function DecodeToken(P:PChar;L:SizeInt;QuoteChar:Char):RawByteString;
Var
 i,m:SizeInt;
begin
 Result:='';
 if (L>=1) and (P^=QuoteChar) then
 begin
  Inc(P);
  Dec(L);
 end;
 if (L>=1) and (P[L-1]=QuoteChar) then
 begin
  Dec(L);
 end;

 SetLength(Result,L);
 m:=1;
 i:=0;
 while (i<L) do
 begin
  if (P[i]=QuoteChar) and (i+1<L) and (P[i+1]=QuoteChar) then
  begin
   i:=i+1;
   Move(P^,Result[m],i);
   m:=m+i;
   L:=L-i-1;
   P:=@P[i+1];
   i:=0;
  end else
   i:=i+1;
 end;
 Move(P^,Result[m],i);
 SetLength(Result,m+i-1);
end;

Function GetParamStr(P:PChar;L:SizeInt):RawByteString;
begin
 Result:='';
 if (P<>nil) and (L>0) then
 begin
  Case P^ of
   #39,'`','"':Result:=DecodeToken(P,L,P^);
   'N':if (L>1) then
       begin
        Case P[1] of
         #39,'`','"':Result:=DecodeToken(@P[1],L-1,P[1]);
         else
          System.SetString(Result,P,L);
        end;
       end else
       begin
        System.SetString(Result,P,L);
       end;
   else
        System.SetString(Result,P,L);
  end;
 end;
end;

Function GetWordStr(P:PChar;L:SizeInt):RawByteString; inline;
begin
 System.SetString(Result,P,L);
end;

type
 TZScriptSQLNumberState = class (TZNumberState)
 public
   function NextToken(var SPos: PChar; const NTerm: PChar;
     Tokenizer: TZTokenizer): TZToken; override;
 end;

function TZScriptSQLNumberState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;

  procedure ReadExp; inline;
  begin
    Inc(SPos, Ord((SPos < NTerm) and ((Ord(SPos^) = Ord('-')) or (Ord(SPos^) = Ord('+')))));
    ReadDecDigits(SPos, NTerm)
  end;

var
 CurrDec    :Boolean;
 HexDecimal :Boolean;
 FloatPoint :Boolean;
 BCDPoint   :Boolean;
 GotDecDigit:Boolean;
begin
 CurrDec    :=false;
 HexDecimal :=False;
 GotDecDigit:=False;
 BCDPoint   :=true;

 Result.P := SPos;
 Result.TokenType := ttUnknown;

 if (SPos^='$') then
 begin
  Inc(SPos);
  CurrDec:=true;
 end;

 FloatPoint := SPos^ = '.';

 { Reads the first part of the number before decimal point }
 if not FloatPoint then
 begin
   GotDecDigit := ReadDecDigits(SPos, NTerm);
   if GotDecDigit then
     FloatPoint := SPos^= '.';
   if FloatPoint then
     Inc(SPos); //roll forward to dot
 end;

 { Reads the second part of the number after decimal point }
 if FloatPoint then
 begin
   Inc(Spos, Ord(not GotDecDigit));
   GotDecDigit := ReadDecDigits(SPos, NTerm);
 end;

 { Reads a power part of the number }
 if (not CurrDec) and GotDecDigit and ((Ord((SPos)^) or $20) = ord('e')) then
 begin
   Inc(SPos); //skip exponent
   FloatPoint := True;
   ReadExp;
   BCDPoint:=false;
 end;

 { Reads the hexadecimal number }
 if GotDecDigit and not FloatPoint then
 begin
   if (SPos-1 = Result.P) and ((SPos-1)^ = '0') and
     ((Byte(Ord((SPos)^)) or $20) = ord('x')) then
   begin
     Inc(SPos, 1);  //skip x
     HexDecimal := ReadHexDigits(Spos, NTerm);
   end;
 end;

 Dec(SPos); //push back wrong result
 { Prepare the result }
 if (SPos^ = '.') and (SPos = Result.P) then
 begin
   if Tokenizer.SymbolState <> nil then
     Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer);
 end else
 begin
   Result.L := SPos-Result.P+1;
   if HexDecimal then
     Result.TokenType := ttHexDecimal
   else
   if FloatPoint then
   begin
    if CurrDec or BCDPoint then
     Result.TokenType := ttNumber
    else
     Result.TokenType := ttFloat
   end else
   if CurrDec then
     Result.TokenType := ttNumber
   else
     Result.TokenType := ttInteger;
 end;
end;

type
 TZScriptSQLWordState = class (TZWordState)
 public
   constructor Create;
 public
   function NextToken(var SPos:PChar;const NTerm:PChar;Tokenizer:TZTokenizer):TZToken; override;
 end;

constructor TZScriptSQLWordState.Create;
begin
 SetWordChars(#0, #191, False);
 SetWordChars(#192, high(char), True);
 SetWordChars('a', 'z', True);
 SetWordChars('A', 'Z', True);
 SetWordChars('0', '9', True);
 SetWordChars('$', '$', True);
 SetWordChars('_', '_', True);
end;

function TZScriptSQLWordState.NextToken(var SPos:PChar;const NTerm:PChar;Tokenizer:TZTokenizer):TZToken;
begin
 Case SPos^ of
  ':','@':if (SPos+1<>NTerm) then
          begin
           Case (SPos[1]) of
            '"',#39,'`':Result:=Tokenizer.SymbolState.NextToken(SPos,NTerm,Tokenizer);
            '+':begin
                 SPos:=SPos+2;
                 Result:=inherited NextToken(SPos,NTerm,Tokenizer);
                 if Result.L<>0 then
                 begin
                  Result.TokenType:=ttSpecial;
                  Result.L:=Result.L+2;
                  Result.P:=Result.P-1;
                 end;
                end;
            else
                begin
                 Inc(SPos);
                 Result:=inherited NextToken(SPos,NTerm,Tokenizer);
                 if Result.L<>0 then
                 begin
                  Result.TokenType:=ttSpecial;
                  inc(Result.L);
                  dec(Result.P);
                 end;
                end;
           end;
          end else
          begin
           Result:=Tokenizer.SymbolState.NextToken(SPos,NTerm,Tokenizer);
          end;
  'N':if (SPos+1<>NTerm) then
      begin
       Case (SPos[1]) of
        '"',#39,'`':begin
                     Inc(SPos);
                     Result:=Tokenizer.QuoteState.NextToken(SPos,NTerm,Tokenizer);
                     if Result.L<>0 then
                     begin
                      inc(Result.L);
                      dec(Result.P);
                     end;
                    end;
        else
                    begin
                     Result:=inherited NextToken(SPos,NTerm,Tokenizer);
                    end;
       end;
      end else
      begin
       Result:=inherited NextToken(SPos,NTerm,Tokenizer);
      end;
  else
   Result:=inherited NextToken(SPos,NTerm,Tokenizer);
 end;
end;

procedure TZDbcScriptTokenizer.CreateTokenStates;
begin
 NumberState    :=TZScriptSQLNumberState.Create;
 QuoteState     :=TZGenericSQLQuoteState.Create;
 WhitespaceState:=TZWhitespaceState.Create;
 CommentState   :=TZCppCommentState.Create;
 SymbolState    :=TZGenericSQLSymbolState.Create;
 WordState      :=TZScriptSQLWordState.Create;

 SetCharacterState(#0, #32, WhitespaceState);
 SetCharacterState(#33, #191, SymbolState);
 SetCharacterState(#192, High(Char), WordState);

 SetCharacterState('a', 'z', WordState);
 SetCharacterState('A', 'Z', WordState);
 SetCharacterState('_', '_', WordState);
 SetCharacterState('$', '$', NumberState);

 SetCharacterState('0', '9', NumberState);
 SetCharacterState('.', '.', NumberState);

 SetCharacterState('"', '"', QuoteState);
 SetCharacterState(#39, #39, QuoteState);
 SetCharacterState('`', '`', QuoteState);

 SetCharacterState('/', '/', CommentState);
 SetCharacterState('-', '-', CommentState);

 SetCharacterState(':', ':', WordState);
 SetCharacterState('@', '@', WordState);

 SymbolState.Add('+=');
 SymbolState.Add('-=');
 SymbolState.Add('*=');
 SymbolState.Add('/=');
 SymbolState.Add('%=');
 SymbolState.Add('&=');
 SymbolState.Add('^=');
 SymbolState.Add('|=');
 SymbolState.Add('>=');
 SymbolState.Add('<=');
 SymbolState.Add('<>');
 SymbolState.Add('!=');
 SymbolState.Add('!<');
 SymbolState.Add('!>');

 //LastTokenType:=ttUnknown;
 FPos:=Point(1,1);
end;

procedure ScanNextLine(var Token:TZToken;Var P:TPoint);
Var
 i:Sizeint;
 R:TZToken;
begin
 R:=Token;
 R.L:=1;
 P:=Default(TPoint);
 i:=0;
 While (i<Token.L) do
 begin
  Case Token.P[i] of
   #13:begin
        R.P:=@Token.P[i];
        R.L:=1;
        Inc(P.Y);
        if (i+1<Token.L) and (Token.P[i+1]=#10) then
        begin
         Inc(i);
         R.L:=2;
        end;
       end;
   #10:begin
        R.P:=@Token.P[i];
        R.L:=1;
        Inc(P.Y);
        if (i+1<Token.L) and (Token.P[i+1]=#13) then
        begin
         Inc(i);
         R.L:=2;
        end;
       end;
  end;
  Inc(i);
 end;
 P.X:=(Token.P+Token.L)-(R.P+R.L)+1;
 Token:=R;
end;

function TZDbcScriptTokenizer.FetchNextToken(var Buffer:PChar;EOS:PChar):TZToken;
var
 P:TPoint;
 State:TZTokenizerState;
begin
 while (Buffer < EOS) do
 begin
  State:=GetCharacterState(Buffer^);
  if (State<>nil) then
  begin
   Result:=State.NextToken(Buffer,EOS,Self);
   Case Result.TokenType of
    ttComment:
    begin
     ScanNextLine(Result,P);
     if (P.Y<>0) then
     begin
      FPos.x:=P.X;
      FPos.y:=FPos.y+P.Y;
     end;
     Inc(Buffer);
    end;
    ttWhitespace:
    begin
     ScanNextLine(Result,P);
     if (P.Y<>0) then
     begin
      FPos.x:=P.X;
      FPos.y:=FPos.y+P.Y;
     end;
     Inc(Buffer);
     Exit;
    end;
    else
     begin
      FPos.x:=FPos.x+Result.L;
      Inc(Buffer);
      Exit;
     end;
   end;
  end else
  begin
   FPos.x:=FPos.x+Result.L;
   Result.P:=Buffer;
   Result.L:=0;
   Result.TokenType:=ttUnknown;
   Inc(Buffer);
   Exit;
  end;
 end;

 Result.P:=EOS;
 Result.L:=0;
 Result.TokenType:=ttEOF;
end;

Function _get_op_prior(id:SizeInt):Byte;
begin
 Result:=0;
 Case id of
  8..16:Result:=4;
  17..21:Result:=3;
  22..24:Result:=2;
  25:Result:=1;
  26:Result:=5;
  27:Result:=6;
  28..35:Result:=7;
 end;
end;

Function _get_op_arity(id:SizeInt):Byte; inline;
begin
 Result:=2;
 Case id of
  25,26,35:Result:=1;
  30..32:Result:=3;
 end;
end;

function _get_op_name(id1,id2:Byte):RawByteString;
begin
 Result:='';
 case id1 of
   0:Result:='+=';
   1:Result:='-=';
   2:Result:='*=';
   3:Result:='/=';
   4:Result:='%=';
   5:Result:='&=';
   6:Result:='^=';
   7:Result:='|=';
   8:Result:='=';
   9:Result:='>=';
  10:Result:='<=';
  11:Result:='<>';
  12:Result:='!=';
  13:Result:='!<';
  14:Result:='!>';
  15:Result:='>';
  16:Result:='<';
  17:Result:='+';
  18:Result:='-';
  19:Result:='&';
  20:Result:='^';
  21:Result:='|';
  22:Result:='*';
  23:Result:='/';
  24:Result:='%';
  25:Result:='~';
  26:Result:='NOT';
  27:Result:='AND';
  28:Result:='OR';
  29:if (id2<>0) then
     begin
      Result:='NOT IN';
     end else
     begin
      Result:='IN';
     end;
  30:if (id2<>0) then
     begin
      Result:=_get_op_name(id2,0)+' ALL';
     end else
     begin
      Result:='ALL';
     end;
  31:if (id2<>0) then
     begin
      Result:=_get_op_name(id2,0)+' ANY';
     end else
     begin
      Result:='ANY';
     end;
  32:Result:='SOME';
  33:Result:='BETWEEN';
  34:Result:='LIKE';
  35:if (id2<>0) then
     begin
      Result:='NOT IS NULL';
     end else
     begin
      Result:='IS NULL';
     end;
  36:Result:='(';
  37:Result:=')';
 end;

end;

function GetDataType(data:Pointer;len:SizeUInt):TZVariantType;
begin
 Result:=vtNull;
 case _GetDataTypeId(data,len) of
  0  {'numeric'},
  1  {'decimal'}:Result:=vtBigDecimal;
  2  {'bigint'},
  3  {'smallint'},
  4  {'tinyint'},
  5  {'int'}:Result:=vtInteger;
  6  {'bit'}:Result:=vtBoolean;
  7  {'smallmoney'},
  8  {'money'}:Result:=vtCurrency;
  9  {'float'},
  10 {'real'}:Result:=vtDouble;
  11 {'date'}:Result:=vtDate;
  12 {'datetimeoffset'},
  13 {'datetime2'}:Result:=vtTimeStamp;
  14 {'smalldatetime'},
  15 {'datetime'}:Result:=vtDateTime;
  16 {'time'}:Result:=vtTime;
  17 {'char'},
  18 {'varchar'},
  19 {'text'}:Result:=vtRawByteString;
  20 {'nchar'},
  21 {'nvarchar'},
  22 {'ntext'}:Result:=vtUnicodeString;
  23 {'uniqueidentifier'}:Result:=vtGUID;
 end;
end;

{$I _parse_val.inc}

end.

