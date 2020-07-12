{ Sometime utils

  Copyright (C) 2018-2020 Red_prig

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

unit rutils;

{$mode objfpc}
{$H+}

interface

Uses
  SysUtils,DateUtils,Classes,URIParser{,Variants},typinfo;

type
  TTextStream=class(TStream)
   Function  EOF:Boolean;
   Function  Readln:RawByteString;
   Procedure Write(const S:RawByteString); overload; inline;
   Procedure Writeln(const S:RawByteString); inline;
  end;
  TPostStream=class(TTextStream)
   Procedure Post(const NAME,CONTENTS:RawByteString);
  end;
  TPostRec=packed record
   Name,Value:RawByteString;
  end;
  TFields=object
   Data:array of TPostRec;
   Procedure Clear; inline;
   function DoSet(const N,V:RawByteString):SizeInt;
   function DoAdd(const N,V:RawByteString):SizeInt;
   function DoSet(const F:TFields):SizeInt;
   function DoAdd(const F:TFields):SizeInt;
   function DoGet(const N:RawByteString):RawByteString;
   function Find(const N:RawByteString;Var Index:SizeInt):Boolean;
   function Find(const N:RawByteString):Boolean; inline;
   function GetURLText:RawByteString;
   function IsSpace:Boolean; inline;
  end;

  TChars=Set of AnsiChar;

  TFinder=object
   P:Pointer;
   L,R,I:SizeInt;
   Procedure Open(_P:Pointer;_Len:SizeInt);     inline;
   Procedure Open(S:TMemoryStream);             inline;
   Function  IsValid:Boolean;                   inline;
   Function  Pos(Const Substr:RawByteString):TFinder;  inline;
   Function  Pos(c:AnsiChar):TFinder;           inline;
   Function  Pos(c:TChars):TFinder;         inline;
   Function  Next(Const Substr:RawByteString):TFinder; inline;
   Function  Next(c:AnsiChar):TFinder;          inline;
   Procedure Inc(Const A:SizeInt=1);            inline;
   Function  Str:RawByteString;                        inline;
  end;

  AString=Array of RawByteString;

  TExtractCallback=Procedure(const Name,Value:String) of object;
  TStringCallback=Procedure(const Value:String) of object;
  TRawStringCallback=Procedure(const Value:RawByteString) of object;

  //TConstArray=array of TVarRec;

  size_t=SizeUInt;

function EncodeURLElement(Const S:RawByteString):RawByteString;
function DecodeURLElement(Const S:RawByteString):RawByteString;
Function CmpText(buf1,buf2:Pchar;Len:SizeInt):SizeInt;
Function MemPos(Memory:Pointer;c:AnsiChar;i,Size:SizeInt):SizeInt;
Function MemPos(Memory:Pointer;c:TChars;i,Size:SizeInt):SizeInt;
Function MemPos(Memory:Pointer;Const Substr:RawByteString;i,Size:SizeInt):SizeInt;
Function MemNext(Memory:Pointer;Const Substr:RawByteString;i,Size:SizeInt):SizeInt;
Function MemNext(Memory:Pointer;c:AnsiChar;i,Size:SizeInt):SizeInt;
function MemCopy(Memory:Pointer;Index,Count:SizeInt):RawByteString;

Procedure DecodePlainText(S:TStream);
Procedure DecodePlainText(S:TMemoryStream);
Procedure DecodeJsonp(Src,Dst:TStream);

Function  DecodeTimeStamp(Q:QWORD):TDateTime;
Function  EncodeTimeStamp(D:TDateTime):QWORD;
Function  DecodeTimeStampUTC(Q:QWORD):TDateTime;
Function  EncodeTimeStampUTC(D:TDateTime):QWORD;
Function  DecodeTimeStampUTC3(Q:QWORD):TDateTime;
Function  EncodeTimeStampUTC3(D:TDateTime):QWORD;

function GetURLHost(Const S:RawByteString):RawByteString; inline;
function GetURLPath(Const S:RawByteString):RawByteString; inline;
function GetURLName(Const S:RawByteString):RawByteString; inline;
function GetURLParams(Const S:RawByteString):RawByteString; inline;
function GetURLRequest(Const S:RawByteString):RawByteString; inline;

function isErrorCode(i:SizeInt):Boolean;    inline;
function isTrueCode(i:SizeInt):Boolean;     inline;
function isRedirectCode(i:SizeInt):Boolean; inline;

Procedure Split2line(Const S:RawByteString;CB:TStringCallback);
Procedure Split2line(Const S:RawByteString;CB:TRawStringCallback);
Procedure Split2line(Const S:RawByteString;L:TStrings);
Procedure CorrectFileName(Var FName:RawByteString;Replace:Char=#0);
function  Fetch(var Value:RawByteString;const Delimiter:RawByteString):RawByteString;
function  Fetch(var Value:RawByteString;const Delimiter:Char):RawByteString;
function  FetchEx(var Value:RawByteString;const Delimiter,Quotation:Char):RawByteString;
Procedure _Trim(P:PChar;Var i,Len:size_t); inline;
function  _TrimLeft(P:PChar):PChar; inline;
Function  GetStr(data:Pchar;size:size_t):RawByteString; inline;
function  GetPChar(Const S:RawByteString):PChar;
function  CopyPChar(P:PChar):PChar; inline;
function  CopyPChar(P:PChar;Len:SizeUInt):PChar; inline;
function  ConcatPChar(P1,P2:PChar):PChar;
function  GetFloatSizeRus(i:SizeUInt):RawByteString;
Function  GetEnumName(TypeInfo:Pointer;Value:Integer):RawByteString;

type
 TPropInfoCallback=Procedure(Sender:TObject;PI:PPropInfo) of object;

Procedure GetPropertyList(Sender:TObject;CB:TPropInfoCallback);

Const
 PCharNil :PAnsiChar='';
 PCharCRLF:PAnsiChar=#13#10;

type
 TVarParam=object
  private
   Var
    data:array of Byte;
   Procedure AddParam(VType:Byte;Src:Pointer;size:PtrUint);
   Procedure AddVariant(const V:Variant);
   Procedure AddVarRec(const Item:TVarRec);
   Procedure AddUnknown(P:Pointer);
   Procedure AddPChar(P:PChar); inline;
   Procedure AddPWideChar(P:PWideChar); inline;
  public

   type
    TIterator=object
     private
      PVP:^TVarParam;
      Pos:SizeInt;
     public
      function  Next:Boolean;
      function  GetType:Byte;
      function  tryGetBool(Var B:Boolean):Boolean;
      function  tryGetUInt(Var i:PtrUInt):Boolean;
      function  tryGetInt(Var i:PtrInt):Boolean;
      function  tryGetInt64(Var i:Int64):Boolean;
      function  tryGetStr(Var S:RawByteString):Boolean;
      function  tryGetPointer(Var P:Pointer):Boolean;
      function  tryGetClass(Var P:TObject):Boolean;
      function  tryGetInterface(Var P:Pointer):Boolean;
    end;

   function  First:TIterator; inline;

   function  IsSpace:Boolean; inline;
   Procedure Free;// inline;
   Procedure Add(const Elements:array of const);
   Procedure Create(const Elements:array of const);

 end;

operator := (const Elements:array of const):TVarParam;

implementation

Procedure TVarParam.AddParam(VType:Byte;Src:Pointer;size:PtrUint);
Var
 i:PtrUint;
begin
 if size=0 then Exit;
 i:=Length(data);
 SetLength(data,i+size+1);
 data[i]:=VType;
 Inc(i);
 Move(Src^,data[i],size);
end;

Procedure TVarParam.AddVariant(const V:Variant);
Var
 VInteger:Longint;
 VExtended:Extended;
 VBoolean:Boolean;
 vqword:qword;

begin
 //Writeln(TVarData(V).vType and varTypeMask);
 Case (TVarData(V).vType) of
  varsmallint:
  begin
   VInteger:=TVarData(V).vsmallint;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varinteger:
  begin
   AddParam(vtInteger,@TVarData(V).vinteger,SizeOf(Integer));
  end;
  varsingle:
  begin
   VExtended:=TVarData(V).vsingle;
   AddParam(vtExtended,@VExtended,SizeOf(Extended));
  end;
  vardouble:
  begin
   VExtended:=TVarData(V).vdouble;
   AddParam(vardouble,@VExtended,SizeOf(Extended));
  end;
  varcurrency:
  begin
   AddParam(vtCurrency,@TVarData(V).vCurrency,SizeOf(Currency));
  end;
  varolestr:
  begin
   AddPWideChar(PWideChar(TVarData(V).volestr));
  end;
  varboolean:
  begin
   VBoolean:=TVarData(V).vboolean;
   AddParam(vtBoolean,@vBoolean,SizeOf(Boolean));
  end;
  varunknown:
  begin
   AddUnknown(TVarData(V).vunknown);
  end;
  varshortint:
  begin
   VInteger:=TVarData(V).vshortint;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varbyte:
  begin
   VInteger:=TVarData(V).vbyte;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varword:
  begin
   VInteger:=TVarData(V).vword;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varlongword:
  begin
   vqword:=TVarData(V).vlongword;
   AddParam(vtQWord,@vqword,SizeOf(QWord));
  end;
  varint64:
  begin
   AddParam(vtInt64,@TVarData(V).vint64,SizeOf(int64));
  end;
  varqword:
  begin
   AddParam(vtQWord,@TVarData(V).vqword,SizeOf(Qword));
  end;
  varstring:
  begin
   AddPChar(PChar(String(TVarData(V).vstring)));
  end;
  varustring:
  begin
   AddPWideChar(PWideChar(UnicodeString(TVarData(V).vstring)));
  end;

  //varByRef

  varsmallint or varByRef:
  begin
   VInteger:=PSmallInt(TVarData(V).vpointer)^;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varinteger or varByRef:
  begin
   AddParam(vtInteger,TVarData(V).vpointer,SizeOf(Integer));
  end;
  varsingle or varByRef:
  begin
   VExtended:=PSingle(TVarData(V).vpointer)^;
   AddParam(vtExtended,@VExtended,SizeOf(Extended));
  end;
  vardouble or varByRef:
  begin
   VExtended:=PDouble(TVarData(V).vpointer)^;
   AddParam(vardouble,@VExtended,SizeOf(Extended));
  end;
  varcurrency or varByRef:
  begin
   AddParam(vtCurrency,TVarData(V).vpointer,SizeOf(Currency));
  end;
  varolestr or varByRef:
  begin
   AddPWideChar(PWideChar(TVarData(V).vpointer^));
  end;
  varboolean or varByRef:
  begin
   VBoolean:=Pwordbool(TVarData(V).vpointer)^;
   AddParam(vtBoolean,@vBoolean,SizeOf(Boolean));
  end;
  varunknown or varByRef:
  begin
   AddUnknown(Pointer(TVarData(V).vpointer^));
  end;
  varshortint or varByRef:
  begin
   VInteger:=Pshortint(TVarData(V).vpointer)^;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varbyte or varByRef:
  begin
   VInteger:=TVarData(V).vbyte;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varword or varByRef:
  begin
   VInteger:=Pbyte(TVarData(V).vpointer)^;
   AddParam(vtInteger,@VInteger,SizeOf(Integer));
  end;
  varlongword or varByRef:
  begin
   vqword:=Plongword(TVarData(V).vpointer)^;
   AddParam(vtQWord,@vqword,SizeOf(QWord));
  end;
  varint64 or varByRef:
  begin
   AddParam(vtInt64,TVarData(V).vpointer,SizeOf(int64));
  end;
  varqword or varByRef:
  begin
   AddParam(vtQWord,TVarData(V).vpointer,SizeOf(Qword));
  end;
  varstring or varByRef:
  begin
   AddPChar(PChar(PString(TVarData(V).vpointer)^));
  end;
  varustring or varByRef:
  begin
   AddPWideChar(PWideChar(PUnicodeString(TVarData(V).vstring)^));
  end;

  else
  begin
   AddParam(vtVariant,@V,SizeOf(Variant));
  end;
 end;
end;

Procedure TVarParam.AddUnknown(P:Pointer);
begin
 if Assigned(P) then
 begin
  IUnknown(P)._AddRef;
 end;
 AddParam(vtInterface,@P,SizeOf(Pointer));
end;

Procedure TVarParam.AddPChar(P:PChar); inline;
begin
 AddParam(vtPChar,P,StrLen(P)+SizeOf(Char));
end;

Procedure TVarParam.AddPWideChar(P:PWideChar); inline;
begin
 AddParam(vtPWideChar,P,Sysutils.StrLen(P)*SizeOf(WideChar)+SizeOf(WideChar));
end;

Procedure TVarParam.AddVarRec(const Item:TVarRec);
begin
 case Item.VType of
  vtInteger      :AddParam(Item.VType,@Item.vInteger,SizeOf(Integer));
  vtBoolean      :AddParam(Item.VType,@Item.vBoolean,SizeOf(Boolean));
  vtChar         :AddParam(Item.VType,@Item.vChar,SizeOf(Char));
  vtExtended     :AddParam(Item.VType,Item.vExtended,SizeOf(Extended));
  vtString       :AddParam(Item.VType,Item.VString,Length(Item.VString^)+SizeOf(Char));
  vtObject,
  vtClass,
  vtPointer      :AddParam(Item.VType,@Item.vPointer,SizeOf(Pointer));
  vtInterface    :AddUnknown(Item.vPointer);
  vtCurrency     :AddParam(Item.VType,@Item.vCurrency,SizeOf(Currency));
  vtQWord,
  vtInt64        :AddParam(Item.VType,Item.vInt64,SizeOf(Int64));
  vtVariant      :AddVariant(Item.vVariant^);
  //vtVariant      :AddParam(Item.VType,Item.vVariant,SizeOf(Variant));
  vtPChar        :AddPChar(Item.VPChar);
  vtWideChar     :AddParam(Item.VType,@Item.vWideChar,SizeOf(WideChar));
  vtPWideChar    :AddPWideChar(Item.VPWideChar);
  vtAnsiString   :AddPChar(PChar(AnsiString(Item.VAnsiString)));
  vtWideString   :AddPWideChar(PWideChar(WideString(Item.VWideString)));
  vtUnicodeString:AddPWideChar(PWideChar(UnicodeString(Item.VUnicodeString)));
 end;
end;

function TVarParam.First:TIterator; inline;
begin
 Result.PVP:=@Self;
 Result.Pos:=0;
end;

Procedure TVarParam.Free;
Var
 I:TIterator;
 P:Pointer;
begin
 if not IsSpace then
 begin
  I:=First;
  repeat
   if I.tryGetInterface(P) then
   begin
    if Assigned(P) then
    begin
     IUnknown(P)._Release;
     IUnknown(P)._Release;
    end;
   end;
  until not I.Next;
 end;
 SetLength(data,0);
end;

function TVarParam.IsSpace:Boolean; inline;
begin
 Result:=Length(data)=0;
end;

Procedure TVarParam.Add(const Elements:array of const);
var
 I:SizeUInt;
begin
 if Length(Elements)>0 then
 for I:=Low(Elements) to High(Elements) do
  AddVarRec(Elements[I]);
end;

Procedure TVarParam.Create(const Elements:array of const);
begin
 Free;
 Add(Elements);
end;

operator := (const Elements:array of const):TVarParam;
begin
 Result:=Default(TVarParam);
 Result.Add(Elements);
end;

function TVarParam.TIterator.Next:Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtInteger      :Pos:=Pos+1+SizeOf(Integer);
   vtBoolean      :Pos:=Pos+1+SizeOf(Boolean);
   vtChar         :Pos:=Pos+1+SizeOf(Char);
   vtExtended     :Pos:=Pos+1+SizeOf(Extended);
   vtString       :Pos:=Pos+1+Length(PShortString(@PVP^.Data[Pos+1])^)+SizeOf(Char);
   vtObject,
   vtClass,
   vtInterface,
   vtPointer      :Pos:=Pos+1+SizeOf(Pointer);
   vtPChar        :Pos:=Pos+1+StrLen(PChar(@PVP^.Data[Pos+1]))+SizeOf(Char);
   vtWideChar     :Pos:=Pos+1+SizeOf(WideChar);
   vtPWideChar    :Pos:=Pos+1+Sysutils.StrLen(PWideChar(@PVP^.Data[Pos+1]))*SizeOf(WideChar)+SizeOf(WideChar);
   vtCurrency     :Pos:=Pos+1+SizeOf(Currency);
   vtVariant      :Pos:=Pos+1+SizeOf(Variant);
   vtQWord,
   vtInt64        :Pos:=Pos+1+SizeOf(Int64);
  end;
end;

Function AsString(V:Variant):String;
begin
 try
  If TVarData(V).vType<>varNull then
   Result:=V
  else
   Result:='';
 except
  Result:='';
 end;
end;

function TVarParam.TIterator.GetType:Byte;
begin
 Result:=High(Byte);
 if not Assigned(PVP) then Exit;
 if Pos>=Length(PVP^.data) then Exit;
 Result:=PVP^.Data[Pos];
end;

function TVarParam.TIterator.tryGetBool(Var B:Boolean):Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtBoolean      :B:=PBoolean(@PVP^.Data[Pos+1])^;
   vtInteger      :B:=PInteger(@PVP^.Data[Pos+1])^<>0;
   vtInt64        :B:=PInt64(@PVP^.Data[Pos+1])^<>0;
   vtQWord        :B:=PQWord(@PVP^.Data[Pos+1])^<>0;
   else
    Result:=False;
  end;
end;

function TVarParam.TIterator.tryGetUInt(Var i:PtrUInt):Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtInteger      :i:=PInteger(@PVP^.Data[Pos+1])^;
   vtInt64        :i:=PInt64(@PVP^.Data[Pos+1])^;
   vtQWord        :i:=PQWord(@PVP^.Data[Pos+1])^;
   else
    Result:=False;
  end;
end;

function TVarParam.TIterator.tryGetInt(Var i:PtrInt):Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtInteger      :i:=PInteger(@PVP^.Data[Pos+1])^;
   vtInt64        :i:=PInt64(@PVP^.Data[Pos+1])^;
   vtQWord        :i:=PQWord(@PVP^.Data[Pos+1])^;
   else
    Result:=False;
  end;
end;

function TVarParam.TIterator.tryGetInt64(Var i:Int64):Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtInteger      :i:=PInteger(@PVP^.Data[Pos+1])^;
   vtInt64        :i:=PInt64(@PVP^.Data[Pos+1])^;
   vtQWord        :i:=PQWord(@PVP^.Data[Pos+1])^;
   else
    Result:=False;
  end;
end;

function TVarParam.TIterator.tryGetStr(Var S:RawByteString):Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtString       :S:=PShortString(@PVP^.Data[Pos+1])^;
   vtPChar        :S:=PChar(@PVP^.Data[Pos+1]);
   vtPWideChar    :S:=PWideChar(@PVP^.Data[Pos+1]);
   vtVariant      :S:=AsString(PVariant(@PVP^.Data[Pos+1])^);
   else
    Result:=False;
  end;
end;

function TVarParam.TIterator.tryGetPointer(Var P:Pointer):Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtPointer      :P:=PPointer(@PVP^.Data[Pos+1])^;
   vtPChar        :P:=PChar(@PVP^.Data[Pos+1]);
   else
    Result:=False;
  end;
end;

function TVarParam.TIterator.tryGetClass(Var P:TObject):Boolean;
type
 PTObject=^TObject;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtObject      :P:=PTObject(@PVP^.Data[Pos+1])^;
   else
    Result:=False;
  end;
end;

function TVarParam.TIterator.tryGetInterface(Var P:Pointer):Boolean;
begin
 if not Assigned(PVP) then Exit(False);
 Result:=Pos<Length(PVP^.data);
 if Result then
  case PVP^.Data[Pos] of
   vtInterface   :
   begin
    P:=PPointer(@PVP^.Data[Pos+1])^;
    if Assigned(P) then
    begin
     IUnknown(P)._AddRef;
    end;
   end;
   else
    Result:=False;
  end;
end;

type
 {$PUSH}
 {$PACKRECORDS C}
 PEnumData=^TEnumData;
 TEnumData=
 {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
 packed
 {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
 record
  OrdType:Byte;
  MinValue,MaxValue:Longint;
  BaseType:Pointer;
  NameList:ShortString;
 end;
{$POP}

Function GetEnumName(TypeInfo:Pointer;Value:Integer):RawByteString;
Var
 PS:PShortString;
 PT:PEnumData;

begin
 Result:='';
 if Assigned(TypeInfo) then
 begin
  PT:=@PByte(TypeInfo)[PByte(TypeInfo)[1]+2];
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
   PT:=align(Pointer,sizeof(Pointer));
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  PS:=@PT^.NameList;
  if Value<=PT^.MaxValue then
  begin
   dec(Value,PT^.MinValue);
   While Value>0 Do
   begin
    PS:=PShortString(pointer(PS)+PByte(PS)^+1);
    Dec(Value);
   end;
   Result:=PS^;
  end;
 end;
end;

Procedure CorrectFileName(Var FName:RawByteString;Replace:Char=#0);
Var
 i:SizeInt;
begin
 if Length(FName)=0 then Exit;
 i:=1;
 While i<=Length(FName) do
 begin
  Case FName[i] of
   #0..#31,'\','/',':','*','?','"','<','>','|':
   if Replace=#0 then
    System.Delete(FName,i,1)
   else
    FName[i]:=Replace;
  end;
  Inc(i);
 end;
end;

function Fetch(var Value:RawByteString;const Delimiter:RawByteString):RawByteString;
Var
 i:SizeUInt;
begin
 Result:='';
 i:=MemPos(@Value[1],Delimiter,1,Length(Value));
 if i<>0 then
 begin
  Result:=Copy(Value,1,i-1);
  //Value:=Copy(Value,i+Length(Delimiter),Length(Value)-i-Length(Delimiter)+1);
  System.Delete(Value,1,i+Length(Delimiter)-1);
 end;
end;

function Fetch(var Value:RawByteString;const Delimiter:Char):RawByteString;
Var
 i:SizeUInt;
begin
 Result:='';
 if Length(Value)>0 then
 begin
  For i:=1 to Length(Value) do
  begin
   if Value[i]=Delimiter then
   begin
    System.Delete(Value,1,i);
    Exit;
   end else
   begin
    Result:=Result+Value[i];
   end;
  end;
  Value:='';
 end;
end;

function FetchEx(var Value:RawByteString;const Delimiter,Quotation:Char):RawByteString;
Var
 QE:Boolean;
 i:SizeUInt;
begin
 Result:='';
 QE:=False;
 if Length(Value)>0 then
 begin
  For i:=1 to Length(Value) do
  begin
   if Value[i]=Quotation then
   begin
    Result:=Result+Value[i];
    QE:=not QE;
   end else
   if Value[i]=Delimiter then
   begin
    if QE then
     Result:=Result+Value[i]
    else
    begin
     System.Delete(Value,1,i);
     Exit;
    end;
   end else
   begin
    //if QE then
     Result:=Result+Value[i];
   end;
  end;
  Value:='';
 end;
end;

Procedure _Trim(P:PChar;Var i,Len:size_t); inline;
begin
 while (Len>0) and (P[i+Len-1]<=' ') do Dec(Len);
 while (Len>0) and (P[i]<=' ') do
 begin
  Inc(i);
  Dec(Len);
 end;
end;

function _TrimLeft(P:PChar):PChar; inline;
begin
 Result:=P;
 while (Result^<>#0) and (Result^<=' ') do Inc(SizeUInt(Result));
end;

Function GetStr(data:Pchar;size:size_t):RawByteString; inline;
begin
 SetLength(Result,size);
 Move(data^,Result[1],size);
end;

function GetPChar(Const S:RawByteString):PChar;
begin
 if S='' then
  Result:=nil
 else
 begin
  Result:=GetMem(Length(S)+1);
  Move(S[1],Result^,Length(S));
  Result[Length(S)]:=#0;
 end;
end;

function CopyPChar(P:PChar):PChar; inline;
Var
 Len:SizeUInt;
begin
 Len:=StrLen(P);
 Inc(Len);
 Result:=GetMem(Len);
 Move(P^,Result^,Len);
end;

function CopyPChar(P:PChar;Len:SizeUInt):PChar; inline;
begin
 Result:=GetMem(Len+1);
 Move(P^,Result^,Len);
 Result[Len]:=#0;
end;

function ConcatPChar(P1,P2:PChar):PChar;
Var
 Len1,Len2:SizeUInt;
begin
 Len1:=StrLen(P1);
 Len2:=StrLen(P2)+1;
 Result:=GetMem(Len1+Len2);
 Move(P1^,Result^,Len1);
 Move(P2^,Result[Len1],Len2);
end;

function GetFloatSizeRus(i:SizeUInt):RawByteString;
Const
 CKB=1024;
 CMB=1024*1024;
 CGB=1024*1024*1024;
begin
 Case i of
  0  ..CKB-1:Result:=IntToStr(i)+'Б';
  CKB..CMB-1:Result:=FloatToStrF(i/CKB,ffFixed,0,2)+'Кб';
  CMB..CGB-1:Result:=FloatToStrF(i/CMB,ffFixed,0,2)+'Мб';
  else       Result:=FloatToStrF(i/CGB,ffFixed,0,2)+'Гб';
 end;
end;

function isErrorCode(i:SizeInt):Boolean; inline;
begin
 Case abs(i) of
  200..205,207..399:Result:=False;
  else Result:=True;
 end;
end;

function isTrueCode(i:SizeInt):Boolean; inline;
begin
 Result:=not isErrorCode(i);
end;

function isRedirectCode(i:SizeInt):Boolean; inline;
begin
 Case i of
  300..399:Result:=True;
  else Result:=False;
 end;
end;

function GetURLHost(Const S:RawByteString):RawByteString; inline;
begin
 Result:=ParseURI(S).Host;
end;

function GetURLPath(Const S:RawByteString):RawByteString; inline;
begin
 Result:=ParseURI(S).Path;
end;

function GetURLName(Const S:RawByteString):RawByteString; inline;
Var
 URI:TURI;
begin
 URI:=ParseURI(S);
 Result:=URI.Path+URI.Document;
end;

function GetURLParams(Const S:RawByteString):RawByteString; inline;
Var
 URI:TURI;
begin
 URI:=ParseURI(S);
 Result:=URI.Params;
end;

function GetURLRequest(Const S:RawByteString):RawByteString; inline;
Var
 URI:TURI;
begin
 URI:=ParseURI(S);
 Result:=URI.Path+URI.Document+URI.Params;
end;

function EncodeURLElement(Const S:RawByteString):RawByteString;
Const
 Allowed=['a'..'z','A'..'Z','0'..'9','-','.','_','~'];
var
 i,l:Integer;
 h:String[2];
 P:PChar;
 c:AnsiChar;
begin
 Result:='';
 l:=Length(S);
 If (l=0) then Exit;
 SetLength(Result,l*3);
 P:=Pchar(Result);
 for I:=1 to L do
 begin
  C:=S[i];
  if not (c in Allowed) then
  begin
   P^:='%';
   Inc(P);
   h:=IntToHex(Ord(c),2);
   p^:=h[1];
   Inc(P);
   p^:=h[2];
   Inc(P);
  end
  else
  begin
   P^:=c;
   Inc(p);
  end;
 end;
 SetLength(Result,P-PChar(Result));
end;

function DecodeURLElement(Const S:RawByteString):RawByteString;
var
 i,l,o:Integer;
 p:pchar;
 h:RawByteString;
begin
 l:=Length(S);
 if l=0 then exit('');
 SetLength(Result,l);
 P:=@Result[1];
 i:=1;
 While (I<=L) do
 begin
  Case S[i] of
   '%':if (I<L-1) then
       begin
        H:='$'+Copy(S,I+1,2);
        o:=StrToIntDef(H,-1);
        If (O>=0) and (O<=255) then
        begin
         P^:=char(O);
         Inc(P);
         Inc(I,2);
        end;
       end;
   '+':begin
        P^:=' ';
        Inc(P);
       end;
   else
    begin
     P^:=S[i];
     Inc(P);
    end;
  end;
  Inc(i);
 end;
 SetLength(Result,P-PChar(Result));
end;

Function MemPos(Memory:Pointer;c:AnsiChar;i,Size:SizeInt):SizeInt;
var
 pc:PAnsiChar;
begin
 //system.IndexByte();
 if i>0 then
 begin
  c:=UpCase(c);
  pc:=@PAnsiChar(Memory)[i-1];
  for i:=i to Size do
  begin
   if pc^=c then exit(i);
   inc(pc);
  end;
 end;
 Result:=0;
end;

Function MemPos(Memory:Pointer;c:TChars;i,Size:SizeInt):SizeInt;
var
 pc:PAnsiChar;
begin
 //system.IndexByte();
 if i>0 then
 begin
  pc:=@PAnsiChar(Memory)[i-1];
  for i:=i to Size do
  begin
   if pc^ in c then exit(i);
   inc(pc);
  end;
 end;
 Result:=0;
end;

Function MemPos(Memory:Pointer;c,c2:AnsiChar;i,Size:SizeInt):SizeInt;
var
 pc:PAnsiChar;
begin
 //system.IndexByte();
 if i>0 then
 begin
  c:=UpCase(c);
  c2:=UpCase(c2);
  pc:=@PAnsiChar(Memory)[i-1];
  for i:=i to Size do
  begin
   if (pc^=c) or (pc^=c2) then exit(i);
   inc(pc);
  end;
 end;
 Result:=0;
end;

Function CmpText(buf1,buf2:Pchar;Len:SizeInt):SizeInt;
Var
 i:SizeInt;
 a,b:Byte;
begin
 Result:=0;
 For i:=0 to Len-1 do
 begin
  a:=Byte(UpCase(buf1[i]));
  b:=Byte(UpCase(buf2[i]));
  Result:=a-b;
  if (a=0) or (b=0) or (Result<>0) then Break;
 end;
end;

Function _CmpText(buf1,buf2:Pchar;Len:SizeInt):SizeInt;
Var
 i:SizeInt;
begin
 Result:=0;
 For i:=0 to Len-1 do
 begin
  Result:=Byte(UpCase(buf1[i]));
  Result:=Result-Byte(UpCase(buf2[i]));
  if Result<>0 then Break;
 end;
end;

Function MemPos(Memory:Pointer;Const Substr:RawByteString;i,Size:SizeInt):SizeInt;
Const
 ASIZEM=255;
var
 q,Len:SizeInt;
 qs_bc:array[0..ASIZEM] of SizeInt;
begin
 Result:=0;
 if (Length(SubStr)=0) or (i<1) or (i>Size) or (Size-i+1<Length(SubStr)) then Exit;

 Len:=Length(SubStr)+1;
 For q:=0 to ASIZEM do qs_bc[q]:=Len;
 For q:=1 to Length(SubStr) do qs_bc[Byte(UpCase(SubStr[q]))]:=Len-q;

 Dec(i);
 Len:=Size-Length(SubStr);
 while (i<=Len) do
 begin
  if _CmpText(@PByte(Memory)[i],@SubStr[1],Length(SubStr))=0 then Exit(i+1);
  i:=i+qs_bc[Byte(UpCase(PChar(Memory)[i+Length(SubStr)]))];
 end;

end;

Function MemNext(Memory:Pointer;Const Substr:RawByteString;i,Size:SizeInt):SizeInt; inline;
begin
 Result:=0;
 i:=MemPos(Memory,Substr,i,Size);
 if i<>0 then Result:=i+Length(Substr);
end;

Function MemNext(Memory:Pointer;c:AnsiChar;i,Size:SizeInt):SizeInt; inline;
begin
 Result:=0;
 i:=MemPos(Memory,c,i,Size);
 if i<>0 then Result:=i+1;
end;

function MemCopy(Memory:Pointer;Index,Count:SizeInt):RawByteString; inline;
begin
 SetLength(Result,Count);
 System.Move(PByte(Memory)[Index-1],Result[1],Count);
end;

Procedure TFinder.Open(_P:Pointer;_Len:SizeInt); inline;
begin
 P:=_P;
 L:=_Len;
 R:=1;
 I:=1;
end;

Procedure TFinder.Open(S:TMemoryStream); inline;
begin
 P:=S.Memory;
 L:=S.Size;
 R:=1;
 I:=1;
end;

Function TFinder.IsValid:Boolean; inline;
begin
 Result:=I<>0;
end;

Function TFinder.Pos(Const Substr:RawByteString):TFinder; inline;
begin
 Result.P:=P;
 Result.L:=L;
 Result.R:=I;
 Result.I:=MemPos(P,Substr,I,L);
end;

Function TFinder.Pos(c:AnsiChar):TFinder; inline;
begin
 Result.P:=P;
 Result.L:=L;
 Result.R:=I;
 Result.I:=MemPos(P,c,I,L);
end;

Function TFinder.Pos(c:TChars):TFinder; inline;
begin
 Result.P:=P;
 Result.L:=L;
 Result.R:=I;
 Result.I:=MemPos(P,c,I,L);
end;

Function TFinder.Next(Const Substr:RawByteString):TFinder; inline;
begin
 Result.P:=P;
 Result.L:=L;
 Result.R:=I;
 Result.I:=MemNext(P,Substr,I,L);
end;

Function TFinder.Next(c:AnsiChar):TFinder; inline;
begin
 Result.P:=P;
 Result.L:=L;
 Result.R:=I;
 Result.I:=MemNext(P,c,I,L);
end;

Procedure TFinder.Inc(Const A:SizeInt=1); inline;
begin
 I:=I+A;
end;

Function TFinder.Str:RawByteString; inline;
begin
 if (I<>0) and (R<>0) then
  Result:=MemCopy(P,R,I-R)
 else
  Result:='';
end;

Function TTextStream.EOF:Boolean; inline;
begin
 Result:=(Size<=Position);
end;

Function TTextStream.Readln:RawByteString;
Var
 C:Char;
begin
 Result:='';
 While Not EOF do
 begin
  Read(C,1);
  Case C of
   #0..#9,
   #11..#12,
   #14..#31:;
   #13:
   begin
    Read(C,1);
    if C<>#10 then Seek(-1,soCurrent);
    Break;
   end;
   #10:
   begin
    Read(C,1);
    if C<>#13 then Seek(-1,soCurrent);
    Break;
   end;
   else Result:=Result+C;
  end;
 end;
end;

Procedure TTextStream.Write(const S:RawByteString); overload; inline;
begin
 Write(S[1],Length(S));
end;

Procedure TTextStream.Writeln(const S:RawByteString); inline;
Const
 NL:array[0..1] of Char=#13#10;
begin
 Write(S[1],Length(S));
 Write(NL,2);
end;

Procedure TPostStream.Post(const NAME,CONTENTS:RawByteString);
begin
 Writeln(NAME+'='+CONTENTS);
end;

Procedure DecodePlainText(S:TStream);
Var
 M:TMemoryStream;
begin
 if (S is TMemoryStream) then
 begin
  DecodePlainText(TMemoryStream(S));
  S.Position:=0;
 end else
 begin
  S.Position:=0;
  M:=TMemoryStream.Create;
  M.CopyFrom(S,S.Size);
  DecodePlainText(M);
  M.Position:=0;
  S.Position:=0;
  S.Size:=0;
  S.CopyFrom(M,M.Size);
  S.Position:=0;
 end;
end;

Procedure DecodePlainText(S:TMemoryStream);
Var
 P:Pchar;
 i,Size:SizeUInt;

 Procedure SetOneChar; inline;
 begin
  Move(P[i],P[i-1],Size-i);
  Dec(Size);
 end;

 Procedure SetOneChar(Ch:Char); inline;
 begin
  SetOneChar;
  P[i-1]:=Ch;
 end;

 Procedure SetUnicode; inline;
 Var
  k,u,d:SizeUInt;
 begin
  u:=0;
  For k:=1 to 4 do
  begin
   if (i+k>=Size) then Break;
   Case P[i+k] of
    '0'..'9': u:=u*16+ord(P[i+k])-ord('0');
    'A'..'F': u:=u*16+ord(P[i+k])-ord('A')+10;
    'a'..'f': u:=u*16+ord(P[i+k])-ord('a')+10;
   end;
  end;
  u:=UnicodeToUtf8(@d,SizeOf(d),@u,1);
  Move(P[i],P[i-5+u],Size-(i+k));
  Move(d,P[i-1],u);
  Size:=Size-5+u;
 end;

begin
 if not Assigned(S) then Exit;
 Size:=S.Size;
 P:=S.Memory;
 if (P=nil) or (Size=0) then Exit;
 if P[0]='"' then
 begin
  Move(P[1],P[0],Size-1);
  Dec(Size);
 end;
 if P[Size-1]='"' then
  Dec(Size);
 i:=0;
 While (i<Size) do
  if P[i]='\' then
  begin
   Inc(i);
   if (i>=Size) then Break;
   Case P[i] of
     '"','''','\','/':SetOneChar;
     't':SetOneChar(#9);
     'b':SetOneChar(#8);
     'n':SetOneChar(#10);
     'r':SetOneChar(#13);
     'f':SetOneChar(#12);
     'u':SetUnicode;
   end;
  end else Inc(i);
 S.Size:=Size;
end;

Procedure DecodeJsonp(Src,Dst:TStream);
Var
 Size:SizeUInt;
 B:Byte;
 W:Boolean;
begin
 Src.Position:=0;
 Size:=Src.Size;
 W:=False;
 While (Src.Position<Size) do
 begin
  B:=Src.ReadByte;
  case B of
   Byte('('):W:=True;
   Byte(')'):Break;
   else
    if W then Dst.WriteByte(B);
  end;
 end;
end;

Const
 CEpoh=62135683200000;

Function DecodeTimeStamp(Q:QWORD):TDateTime;
begin
 Result:=TimeStampToDateTime(MSecsToTimeStamp(Q+CEpoh));
end;

Function EncodeTimeStamp(D:TDateTime):QWORD;
begin
 Result:=Round(TimeStampToMSecs(DateTimeToTimeStamp(D))-CEpoh);
end;

Function DecodeTimeStampUTC(Q:QWORD):TDateTime;
begin
 Result:=DecodeTimeStamp(Q);
 Result:=UniversalTimeToLocal(Result);
end;

Function EncodeTimeStampUTC(D:TDateTime):QWORD;
begin
 D:=LocalTimeToUniversal(D);
 Result:=EncodeTimeStamp(D);
end;

Function DecodeTimeStampUTC3(Q:QWORD):TDateTime;
begin
 Result:=DecodeTimeStamp(Q);
 Result:=UniversalTimeToLocal(Result,3*60);
end;

Function EncodeTimeStampUTC3(D:TDateTime):QWORD;
begin
 D:=LocalTimeToUniversal(D,3*60);
 Result:=EncodeTimeStamp(D);
end;

Procedure TFields.Clear; inline;
begin
 Self:=Default(TFields);
end;

function TFields.IsSpace:Boolean; inline;
begin
 Result:=Length(Data)=0;
end;

function TFields.DoSet(const N,V:RawByteString):SizeInt;
begin
 if N='' then Exit;
 Result:=0;
 if Find(N,Result) then
 begin
  Data[Result].Value:=V;
 end else
 begin
  Result:=Length(Data);
  SetLength(Data,Result+1);
  Data[Result].Name:=N;
  Data[Result].Value:=V;
 end;
end;

function TFields.DoAdd(const N,V:RawByteString):SizeInt;
begin
 if N='' then Exit;
 begin
  Result:=Length(Data);
  SetLength(Data,Result+1);
  Data[Result].Name:=N;
  Data[Result].Value:=V;
 end;
end;

function TFields.DoSet(const F:TFields):SizeInt;
 var
  I:SizeInt;
begin
 if Length(F.Data)>0 then
 For i:=0 to High(F.Data) do
 begin
  Result:=DoSet(F.Data[i].Name,F.Data[i].Value);
 end;
end;

function TFields.DoAdd(const F:TFields):SizeInt;
var
 I:SizeInt;
begin
 if Length(F.Data)>0 then
 For i:=0 to High(F.Data) do
 begin
  Result:=DoAdd(F.Data[i].Name,F.Data[i].Value);
 end;
end;

function TFields.DoGet(const N:RawByteString):RawByteString;
var
 I:SizeInt;
begin
 Result:='';
 if Find(N,i) then
 begin
  Result:=Data[i].Value;
 end;
end;

function TFields.Find(const N:RawByteString;Var Index:SizeInt):Boolean;
var
 I:SizeInt;
begin
 Result:=False;
 if Length(Data)>Index then
 begin
  if Index<0 then Index:=0;
  For i:=Index to High(Data) do
  begin
   if CompareText(Data[i].Name,N)=0 then
   begin
    Index:=i;
    Exit(True);
   end;
  end;
 end;
end;

function TFields.Find(const N:RawByteString):Boolean; inline;
var
 I:SizeInt;
begin
 I:=0;
 Result:=Find(N,i);
end;

function TFields.GetURLText:RawByteString;
var
 I:SizeInt;
begin
 Result:='';
 if Length(Data)>0 then
 For i:=0 to High(Data) do
 begin
  if i<>0 then Result:=Result+'&';
  Result:=Result+EncodeURLElement(Data[i].Name)+'='+EncodeURLElement(Data[i].Value);
 end;
end;

Procedure Split2line(Const S:RawByteString;CB:TStringCallback);
begin
 Split2line(S,TRawStringCallback(CB));
end;

Procedure Split2line(Const S:RawByteString;CB:TRawStringCallback);
Var
 i:SizeInt;
 R:RawByteString;

begin
 if not Assigned(CB) then Exit;
 R:='';
 i:=1;
 While i<=Length(S) do
 begin
  Case S[i] of
   #13:
   begin
    if S[i+1]=#10 then Inc(i);
    CB(R);
    R:='';
   end;
   #10:
   begin
    if S[i+1]=#13 then Inc(i);
    CB(R);
    R:='';
   end;
   else R:=R+S[i];
  end;
  Inc(i);
 end;
 If R<>'' then CB(R);
end;

Procedure Split2line(Const S:RawByteString;L:TStrings);
begin
 if not Assigned(L) then Exit;
 L.Beginupdate;
 try
  Split2line(S,@L.Append);
 finally
  L.EndUpdate;
 end;
end;

Procedure GetPropertyList(Sender:TObject;CB:TPropInfoCallback);
Var
  PT : PTypeData;
  PI : PTypeInfo;
  I : Longint;
  PP : PPropList;

begin
 if (not Assigned(Sender)) or (not Assigned(CB)) then Exit;

 PI:=Sender.ClassInfo;
 PT:=GetTypeData(PI);
 if PT^.PropCount>0 then
 begin
  GetMem(PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
   if Assigned(PP^[i]) then
   begin
    CB(Sender,PP^[i]);
   end;
  FreeMem(PP);
 end;

end;

end.





