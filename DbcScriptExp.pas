{ Dbc script expression parser

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

unit DbcScriptExp;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 Classes,
 gFastStack,
 FmtBCD,
 ZSysUtils,
 ZDbcIntfs,
 ZCompatibility,
 ZEncoding,
 ZVariant,
 ZFastCode,
 ZTokenizer,ZGenericSqlToken,
 DbcScriptUtils;

//scalar_subquery variable operator Integer Double Currency Bcd Quoted

type
 TZNodeType=(ntNull      ,ntBoolean ,ntInteger ,
             ntUInteger  ,ntDouble  ,ntCurrency,
             ntBigDecimal,ntGUID    ,ntBytes   ,
             ntQuoted    ,ntDateTime,ntFunction,
             ntOperator  ,ntVariable,ntSubquery);

 TExpData=record
  nType:TZNodeType;
  Token:RawByteString;
  case TZNodeType of
   ntBoolean   :(VBoolean   :Boolean);
   ntInteger   :(VInteger   :Int64);
   ntUInteger  :(VUInteger  :UInt64);
   ntDouble    :(VDouble    :Double);
   ntCurrency  :(VCurrency  :Currency);
   ntDateTime  :(VDateTime  :TDateTime);
   ntOperator  :(VOP1,VOP2  :Byte);
   ntSubquery  :(VSID       :Int64);
 end;

 PExpNode=^TExpNode;
 TExpNode=record
  Parent,Left,Right:PExpNode;
  Data:TExpData;
 end;

 TCb_GetValue=function(Const FName:RawByteString):TZVariant of object;
 TCb_GetQuery=function(VSID:Int64):IZResultSet of object;
 TCb_SubQueryParse =procedure(Const FToken:TZToken) of object;
 TCb_SubQueryFinish=function:Int64 of object;

 TExpressionState=object
  cb_parse:TCb_SubQueryParse;
  cb_fin:TCb_SubQueryFinish;
  Node:PExpNode;
  subquery_count:SizeUint;
  procedure Clean;
  function  Finish:Boolean;
  function  GetStr:RawByteString;
  procedure Print;
  Function  Parse(Const FToken:TZToken):Boolean;
 end;

 TExpressionCalc=object
  cb_GetValue:TCb_GetValue;
  cb_GetQuery:TCb_GetQuery;
  Node:PExpNode;
  Function calc:TZVariant;
 end;

Procedure FreeExpression(Node:PExpNode);

implementation

Function NewExpNode(Const FToken:TZToken):PExpNode;

  procedure _EncodeNumber(var Node:TExpNode); inline;
  Var
   Len:Integer;
   BCD:TBCD;
  begin
   if (FToken.L<>0) and (FToken.P^='$') then
   begin
    Len:=FToken.L-1;
    Node.Data.VCurrency:=ValRawCurr(PByteArray(@FToken.P[1]),'.',Len);
    if (Len=FToken.L-1) then Node.Data.nType:=ntCurrency;
   end else
   begin
    BCD:=Default(TBCD);
    if TryRawToBcd(FToken.P,FToken.L,BCD,'.') then
    begin
     SetLength(Node.Data.Token,SizeOf(TBCD));
     Move(BCD,PChar(Node.Data.Token)^,SizeOf(TBCD));
     Node.Data.nType:=ntBigDecimal;
    end;
   end;
  end;

  procedure _EncodeDouble(var Node:TExpNode); inline;
  Var
   E:Integer;
  begin
   Node.Data.VDouble:=ValRawDbl(PByteArray(FToken.P),'.',E);
   if (E<>0) then Node.Data.nType:=ntDouble;
  end;

  Function TryHexToBin(HexValue:PChar;L:Integer;var _Result:RawByteString):Boolean; inline;
  Var
   TCH:array[0..1] of AnsiChar;
  begin
   if (L and 1)=0 then
   begin
    L:=L shr 1;
    SetLength(_Result,L);
    Result:=HexToBin(HexValue,PChar(_Result),L)=L;
   end else
   begin
    L:=(L shr 1);
    SetLength(_Result,L+1);
    TCH[0]:='0';
    TCH[1]:=HexValue[0];
    HexToBin(@TCH,PChar(_Result),1);
    Result:=HexToBin(@HexValue[1],@PChar(_Result)[1],L)=L;
   end;
   if not Result then _Result:='';
  end;

  procedure _EncodeHex(var Node:TExpNode); inline;
  begin
   if FToken.L>2 then
    if TryHexToBin(@FToken.P[2],FToken.L-2,Node.Data.Token) then
    begin
     if Length(Node.Data.Token)>SizeOf(Node.Data.VUInteger) then
     begin
      if Length(Node.Data.Token)=SizeOf(TGUID) then
      begin
       Node.Data.nType:=ntGUID;
      end else
      begin
       Node.Data.nType:=ntBytes;
      end;
     end else
     begin
      Node.Data.VUInteger:=0;
      {$IFDEF ENDIAN_BIG}
       With Node.Data do
        Move(Token[1],VUInteger,Length(Token));
      {$ELSE}
       With Node.Data do
       begin
        Move(Token[1],PByte(@VUInteger)[SizeOf(VUInteger)-Length(Token)],Length(Token));
        VUInteger:=SwapEndian(VUInteger);
       end;
      {$ENDIF}
      Node.Data.nType:=ntUInteger;
      Node.Data.Token:='';
     end;
    end;
  end;

  procedure _EncodeUInteger(var Node:TExpNode); inline;
  var
   P,PEnd:PAnsiChar;
   BCD:TBCD;
  begin
   PEnd:=FToken.P+FToken.L;
   P:=PEnd;
   Node.Data.VUInteger:=ValRawUInt64(FToken.P,P);
   if P=PEnd then
   begin
    Node.Data.nType:=ntUInteger;
   end else
   begin
    BCD:=Default(TBCD);
    if TryRawToBcd(FToken.P,FToken.L,BCD,'.') then
    begin
     SetLength(Node.Data.Token,SizeOf(TBCD));
     Move(BCD,PChar(Node.Data.Token)^,SizeOf(TBCD));
     Node.Data.nType:=ntBigDecimal;
    end;
   end;
  end;

  procedure _EncodeQuoted(var Node:TExpNode); inline;
  begin
   Node.Data.nType:=ntQuoted;
   Node.Data.Token:=GetParamStr(FToken.P,FToken.L);
  end;

  procedure _EncodeBoolean(v:Boolean;var Node:TExpNode); inline;
  begin
   Node.Data.VBoolean:=v;
   Node.Data.nType:=ntBoolean;
  end;

  procedure _EncodeVariable(var Node:TExpNode); inline;
  begin
   Node.Data.nType:=ntVariable;
   Node.Data.Token:=DecodeVariable(FToken.P,FToken.L);
  end;

  function _EncodeOp(var Node:TExpNode):Boolean; inline;
  Var
   i:SizeInt;
  begin
   i:=_get_op_id(FToken.P,FToken.L);
   if (i<>-1) then
   begin
    Result:=true;
    Node.Data.nType:=ntOperator;
    Node.Data.VOP1 :=i;
   end;
  end;

begin
 Result:=AllocMem(SizeOf(TExpNode));

 Case FToken.TokenType of
  ttNumber    :_EncodeNumber(Result^);

  ttFloat     :_EncodeDouble  (Result^);
  ttInteger   :_EncodeUInteger(Result^);
  ttHexDecimal:_EncodeHex     (Result^);

  ttQuoted,
  ttQuotedIdentifier:_EncodeQuoted(Result^);

  ttWord,ttKeyword:
  begin
   Case _get_const_keyword(FToken.P,FToken.L) of
     0:Result^.Data.nType:=ntNull;
     1:_EncodeBoolean(true ,Result^);
     2:_EncodeBoolean(false,Result^);
     else
      _EncodeOp(Result^);
    // if not _EncodeOp =  function
   end;
  end;

  ttSpecial:_EncodeVariable(Result^);

  ttSymbol:_EncodeOp(Result^);

  //ttDateTime:Result:=EncodeRawByteString(FToken^.Token);
 end;

end;

Function NewExpZeroNode:PExpNode;
begin
 Result:=AllocMem(SizeOf(TExpNode));
 Result^.Data.nType:=ntUInteger;
 Result^.Data.VUInteger:=0;
end;

Function NewExpNullNode:PExpNode;
begin
 Result:=AllocMem(SizeOf(TExpNode));
 Result^.Data.nType:=ntNull;
end;

Function GetFirstValue(R:IZResultSet):TZVariant; inline;
begin
 Result:=Default(TZVariant);
 if Assigned(R) and (R.Next) then
 begin
  Result:=R.GetValue(1);
 end;
end;

procedure ResolveFirstValue(Var V:TZVariant); inline;
begin
 if (V.VType=vtInterface) then
 begin
  V:=GetFirstValue(IZResultSet(V.VInterface));
 end;
end;

function GetVariant(Node:PExpNode;cb_GetValue:TCb_GetValue;cb_GetQuery:TCb_GetQuery):TZVariant;

 function _EncodeBCD(const Value:RawByteString):TZVariant; inline;
 begin
  Result.VType:=vtBigDecimal;
  Move(PChar(Value)^,Result.VBigDecimal,Length(Value));
 end;

 function _EncodeGUID(const Value:RawByteString):TZVariant; inline;
 begin
  Result.VType:=vtGUID;
  Move(PChar(Value)^,Result.VGUID,Length(Value));
 end;

 function _EncodeBytes(const Value:RawByteString):TZVariant; inline;
 begin
  Result.VType:=vtBytes;
  Result.VRawByteString:=Value;
 end;

begin
 Result:=Default(TZVariant);
 if Assigned(Node) then
 begin

  Case Node^.Data.nType of
   ntBoolean   :Result:=EncodeBoolean (Node^.Data.VBoolean);
   ntInteger   :Result:=EncodeInteger (Node^.Data.VInteger);
   ntUInteger  :Result:=EncodeUInteger(Node^.Data.VUInteger);
   ntDouble    :Result:=EncodeDouble  (Node^.Data.VDouble);
   ntCurrency  :Result:=EncodeCurrency(Node^.Data.VCurrency);
   ntBigDecimal:Result:=_EncodeBCD    (Node^.Data.Token);
   ntGUID      :Result:=_EncodeGUID   (Node^.Data.Token);
   ntBytes     :Result:=_EncodeBytes  (Node^.Data.Token);
   ntQuoted    :Result:=EncodeRawByteString(Node^.Data.Token);
   ntDateTime  :Result:=EncodeDateTime(Node^.Data.VDateTime);
   ntVariable  :if Assigned(cb_GetValue) then
                 Result:=cb_GetValue(Node^.Data.Token)
               else
                 Result:=EncodeRawByteString(':'+Node^.Data.Token);

   ntFunction  :Result:=EncodeRawByteString(Node^.Data.Token);
   ntOperator  :Result:=EncodeRawByteString(_get_op_name(Node^.Data.VOP1,Node^.Data.VOP2));

   ntSubquery  :if Assigned(cb_GetQuery) then
                 Result:=EncodeInterface(cb_GetQuery(Node^.Data.VSID))
                else
                 Result:=EncodeRawByteString('('+IntToStr(Node^.Data.VSID)+')');

  end;

  //Writeln('GET:',Node^.nType,':',Node^.Token,':',_GetAsRawByteString(Result));
 end;
end;

function pushLeft(Node:PExpNode;New:PExpNode):PExpNode;
Var
 P:PExpNode;
begin
 Result:=New;
 if Assigned(Node) then
 begin
  P:=Node^.Parent;
  Result^.Left:=Node;
  Result^.Parent:=P;
  Node^.Parent:=Result;
  if Assigned(P) then
  begin
   if (P^.Right=Node) then
    P^.Right:=Result
   else
    P^.Left:=Result;
  end;
 end;
end;

function pushRight(Node:PExpNode;New:PExpNode):PExpNode;
Var
 P:PExpNode;
begin
 Result:=New;
 if Assigned(Node) then
 begin
  P:=Node^.Parent;
  Result^.Right:=Node;
  Result^.Parent:=P;
  Node^.Parent:=Result;
  if Assigned(P) then
  begin
   if (P^.Right=Node) then
    P^.Right:=Result
   else
    P^.Left:=Result;
  end;
 end;
end;

function PopRight(Node:PExpNode):PExpNode;
Var
 P,R:PExpNode;
begin
 Result:=nil;
 R:=Node^.Right;
 P:=Node^.Parent;
 if Assigned(R) then
 begin
  R^.Parent:=P;
  Result:=R;
 end;
 if Assigned(P) then
 begin
  if (P^.Left=Node) then
  begin
   P^.Left:=R;
  end else
  begin
   P^.Right:=R;
  end;
  Result:=P;
 end;
 Finalize(Node^);
 FreeMem(Node);
end;

function PopLeft(Node:PExpNode):PExpNode;
Var
 P,L:PExpNode;
begin
 Result:=nil;
 L:=Node^.Left;
 P:=Node^.Parent;
 if Assigned(L) then
 begin
  L^.Parent:=P;
  Result:=L;
 end;
 if Assigned(P) then
 begin
  if (P^.Left=Node) then
  begin
   P^.Left:=L;
  end else
  begin
   P^.Right:=L;
  end;
  Result:=P;
 end;
 Finalize(Node^);
 FreeMem(Node);
end;

function SetLeft(var Node:PExpNode;New:PExpNode):PExpNode;
begin
 if Assigned(Node) then
 begin
  Node^.Left:=New;
  New^.Parent:=Node;
 end else
 begin
  Node:=New;
 end;
 Result:=New;
end;

function SetRight(var Node:PExpNode;New:PExpNode):PExpNode;
begin
 if Assigned(Node) then
 begin
  Node^.Right:=New;
  New^.Parent:=Node;
 end else
 begin
  Node:=New;
 end;
 Result:=New;
end;

function LeftIsExist(Node:PExpNode):Boolean; inline;
begin
 Result:=Assigned(Node) and Assigned(Node^.Left);
end;

function RightIsExist(Node:PExpNode):Boolean; inline;
begin
 Result:=Assigned(Node) and Assigned(Node^.Right);
end;

function ParentUp(Node:PExpNode):PExpNode;
begin
 Result:=Node;
 if Assigned(Result) then
  While Assigned(Result^.Parent) do Result:=Result^.Parent;
end;

Function GetNodeOp(Node:PExpNode):SizeInt; inline;
begin
 Result:=-1;
 if Assigned(Node) then
 if (Node^.Data.nType=ntOperator) then
 begin
  Result:=Node^.Data.VOP1;
 end;
end;

Procedure FreeNode(Node:PExpNode); inline;
begin
 if Assigned(Node) then
 begin
  Finalize(Node^);
  FreeMem(Node);
 end;
end;

Function _down(Node:PExpNode):PExpNode; inline;
begin
 Result:=Node;
 if Assigned(Result) then
  repeat
   if Assigned(Result^.Left) then
    Result:=Result^.Left
   else if Assigned(Result^.Right) then
    Result:=Result^.Right
   else
    Break;
  until false;
end;

Procedure FreeExpression(Node:PExpNode);
var
 I,N:PExpNode;
begin
 I:=_down(Node);
 if Assigned(I) then
 begin
  repeat
   if (I^.Parent=nil) then Break;
   N:=I;
   I:=I^.Parent;
   FreeNode(N);
   if (I^.Left=N) then
   begin
    if (I^.Right=nil) then Continue;
    I:=I^.Right;
    I:=_down(I);
   end;
  until false;
 end;
 FreeNode(I);
end;

procedure TExpressionState.Clean;
begin
 FreeExpression(Node);
 Node:=nil;
 subquery_count:=0;
end;

{
function _GetStr(Node:PExpNode):RawByteString;
var
 p,n:SizeInt;
begin
 if Assigned(Node) then
 begin
  if Node^.nType=ntOperator then
  begin

   p:=GetNodeOp(Node^.Parent);
   n:=GetNodeOp(Node);

   if Assigned(Node^.Parent) and (_get_op_prior(n)>_get_op_prior(p)) then
   begin

    Case n of
     26..34:Result:='('+_GetStr(Node^.Left)+' '+_get_op_name(n)+' '+_GetStr(Node^.Right)+')';
     else
            Result:='('+_GetStr(Node^.Left)+_get_op_name(n)+_GetStr(Node^.Right)+')';
    end;

   end else
   begin

    Case n of
     26..34:Result:=_GetStr(Node^.Left)+' '+_get_op_name(n)+' '+_GetStr(Node^.Right);
     else
            Result:=_GetStr(Node^.Left)+_get_op_name(n)+_GetStr(Node^.Right);
    end;

   end;

  end else
  begin
   Result:=_GetAsRawByteString(GetVariant(Node,nil,nil));
  end;
 end else
  Result:='';
end;}

function TExpressionState.GetStr:RawByteString;
var
 I,N:PExpNode;

 procedure concat(Var S:RawByteString;I:PExpNode); inline;
 var
  n:SizeInt;
 begin
  n:=GetNodeOp(I);
  Case n of
       -1:Case I^.Data.nType of
           ntQuoted:S:=S+''''+I^.Data.Token+'''';
           else
            S:=S+_GetAsRawByteString(GetVariant(I,nil,nil));
          end;
   26..35:S:=S+' '+_get_op_name(I^.Data.VOP1,I^.Data.VOP2)+' ';
   else
          S:=S+_get_op_name(I^.Data.VOP1,I^.Data.VOP2);
  end;
 end;

 Function _down(Var S:RawByteString;Node:PExpNode):PExpNode; inline;
 var
  p,n:SizeInt;
 begin
  Result:=Node;
  if Assigned(Result) then
   repeat

    p:=GetNodeOp(Result);

    if Assigned(Result^.Left) then
     Result:=Result^.Left
    else if Assigned(Result^.Right) then
    begin
     concat(S,Result);
     Result:=Result^.Right;
    end else
     Break;

    n:=GetNodeOp(Result);
    if (_get_op_prior(n)>_get_op_prior(p)) then
    begin
     S:=S+'(';
    end;

   until false;
 end;

 procedure _up_br(Var S:RawByteString;Node:PExpNode); inline;
 var
  p,n:SizeInt;
 begin
  p:=GetNodeOp(Node^.Parent);
  n:=GetNodeOp(Node);
  if Assigned(Node^.Parent) and (_get_op_prior(n)>_get_op_prior(p)) then
  begin
   S:=S+')';
  end;
 end;

begin
 I:=ParentUp(Node);
 //Result:=_GetStr(I);

 Result:='';
 I:=_down(Result,Node);
 if Assigned(I) then
 begin
  concat(Result,I);
  repeat
   N:=I;
   I:=I^.Parent;
   if (I=nil) then Break;
   if (I^.Left=N) then
   begin
    concat(Result,I);
    if (I^.Right=nil) then Continue;
    I:=I^.Right;
    I:=_down(Result,I);
    concat(Result,I);
   end else
   begin
    _up_br(Result,I);
   end;
  until false;
 end;

end;

procedure _print(Node,orig:PExpNode;L:Byte);
begin
  if Assigned(Node) then
  begin

   _print(Node^.Right,orig,L+2);

   if Assigned(Node^.Parent) then
    if (Node^.Parent^.Left=Node) then
    begin
     Writeln(Space(L-1),'\');
    end;

   if Node=orig then
    Writeln(Space(L),_GetAsRawByteString(GetVariant(Node,nil,nil))+' <---')
   else
    Writeln(Space(L),_GetAsRawByteString(GetVariant(Node,nil,nil)));

   if Assigned(Node^.Parent) then
    if (Node^.Parent^.Left<>Node) then
    begin
     Writeln(Space(L-1),'/');
    end;

   _print(Node^.Left,orig,L+2);
  end;
end;

procedure TExpressionState.Print;
var
 tmp:PExpNode;
begin
 tmp:=ParentUp(Node);
 _print(tmp,Node,0);
end;

function node_is_const(Node:PExpNode):Boolean; inline;
begin
 if Assigned(Node) then
 begin
  Result:=False;
  if (Node^.Right=nil) and (Node^.Left=nil) then
   Case Node^.Data.nType of
    ntNull..ntDateTime:Result:=True;
   end;
 end else
 begin
  Result:=True;
 end;
end;

Function NewSubQueryNode(id:Int64):PExpNode; inline;
begin
 Result:=AllocMem(SizeOf(TExpNode));
 Result^.Data.nType:=ntSubquery;
 Result^.Data.VSID :=id;
end;

Function NewExpNode(Const V:TZVariant):PExpNode;
begin
 Result:=AllocMem(SizeOf(TExpNode));

 Case V.VType of
        vtNull:Result^.Data.nType:=ntNull;
     vtBoolean:begin
                Result^.Data.nType   :=ntBoolean;
                Result^.Data.VBoolean:=V.VBoolean;
               end;
     vtInteger:begin
                Result^.Data.nType   :=ntInteger;
                Result^.Data.VInteger:=V.VInteger;
               end;
    vtUInteger:begin
                Result^.Data.nType    :=ntUInteger;
                Result^.Data.VUInteger:=V.VUInteger;
               end;
      vtDouble:begin
                Result^.Data.nType    :=ntDouble;
                Result^.Data.VDouble  :=V.VDouble;
               end;
    vtCurrency:begin
                Result^.Data.nType    :=ntCurrency;
                Result^.Data.VCurrency:=V.VCurrency;
               end;
  vtBigDecimal:begin
                Result^.Data.nType    :=ntBigDecimal;
                SetLength(Result^.Data.Token,SizeOf(TBCD));
                Move(V.VBigDecimal,PChar(Result^.Data.Token)^,SizeOf(TBCD));
               end;
        vtGUID:begin
                Result^.Data.nType    :=ntGUID;
                SetLength(Result^.Data.Token,SizeOf(TGUID));
                Move(V.VGUID,PChar(Result^.Data.Token)^,SizeOf(TGUID));
               end;
       vtBytes:begin
                Result^.Data.nType    :=ntBytes;
                Result^.Data.Token    :=V.VRawByteString;
               end;

        vtDate,
        vtTime,
   vtTimeStamp,
    vtDateTime:begin
                Result^.Data.nType    :=ntDateTime;
                Result^.Data.VDateTime:=_GetAsDateTime(V);
               end;

  {$IFNDEF UNICODE}vtString,{$ENDIF}
  {$IFNDEF NO_ANSISTRING}
  vtAnsiString,
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  vtUTF8String,
  {$ENDIF}
  vtRawByteString:begin
                   Result^.Data.nType    :=ntQuoted;
                   Result^.Data.Token    :=V.VRawByteString;
                  end;
  {$IFDEF UNICODE}vtString,{$ENDIF}
  vtUnicodeString:begin
                   Result^.Data.nType    :=ntQuoted;
                   Result^.Data.Token    :=RawByteString(V.VUnicodeString);
                  end;

  else
   begin
    FreeMem(Result);
    Result:=nil;
   end;
 end;
end;

Procedure Simp_LRR(var Node:PExpNode);
var
 n_op,l_op:SizeInt;
 VL,VR:TZVariant;
 new,left:PExpNode;

begin
 n_op:=GetNodeOp(Node);
 Case n_op of
  -1,29..32:Exit;
 end;
 if not node_is_const(Node^.Right) then Exit;

 left:=Node^.Left;

 repeat
  if not Assigned(left) then Exit;
  l_op:=GetNodeOp(left);
  if (l_op=-1) then Exit;
  if (_get_op_prior(l_op)=_get_op_prior(n_op))
     and node_is_const(left^.Right) then Break;
  left:=Left^.Left;
 until false;

 VL:=GetVariant(left^.Right,nil,nil);
 VR:=GetVariant(Node^.Right,nil,nil);
 VL:=DoTokenOp(n_op,0,VL,VR);

 new:=NewExpNode(VL);

 if Assigned(new) then
 begin
  FreeNode(left^.Right);
  PopLeft(left);

  Node^.Data.VOP1:=l_op;

  FreeNode(Node^.Right);
  SetRight(Node,new);
 end;
end;

Procedure Simp_RLR(var Node:PExpNode);
var
 n_op,r_op:SizeInt;
 VL,VR:TZVariant;
 new,Right:PExpNode;

begin
 n_op:=GetNodeOp(Node);
 Case n_op of
  -1,29..32:Exit;
 end;
 if not node_is_const(Node^.Left) then Exit;

 Right:=Node^.Right;

 repeat
  if not Assigned(Right) then Exit;
  r_op:=GetNodeOp(Right);
  if (r_op=-1) then Exit;
  if (_get_op_prior(r_op)=_get_op_prior(n_op))
     and node_is_const(Right^.Right) then Break;
  Right:=Right^.Right;
 until false;

 VL:=GetVariant(Node^.Left  ,nil,nil);
 VR:=GetVariant(Right^.Right,nil,nil);
 VL:=DoTokenOp(r_op,0,VL,VR);

 new:=NewExpNode(VL);

 if Assigned(new) then
 begin
  FreeNode(Right^.Right);
  PopLeft(Right);

  FreeNode(Node^.Left);
  SetLeft(Node,new);
 end;
end;

Procedure Simp_LR(var Node:PExpNode);
var
 n:SizeInt;
 VL,VR:TZVariant;
 new,old,parent:PExpNode;

begin
 n:=GetNodeOp(Node);
 Case n of
  -1,29..32:Exit;
 end;
 if node_is_const(Node^.Left) and node_is_const(Node^.Right) then
 begin
  VL:=GetVariant(Node^.Left ,nil,nil);
  VR:=GetVariant(Node^.Right,nil,nil);
  VL:=DoTokenOp(n,0,VL,VR);

  new:=NewExpNode(VL);

  if Assigned(new) then
  begin
   old:=Node;
   parent:=Node^.Parent;

   Node:=new;
   Node^.Parent:=parent;

   if Assigned(parent) then
   begin
    if (parent^.Left=old) then
    begin
     parent^.Left:=new;
    end else
    begin
     parent^.Right:=new;
    end;
   end;

   FreeNode(old^.Left);
   FreeNode(old^.Right);
   FreeNode(old);
  end;
 end;
end;

Procedure Simplification(var Node:PExpNode);
var
 I,N:PExpNode;
begin
 I:=_down(Node);
 if Assigned(I) then
 begin
  repeat
   N:=I;
   I:=I^.Parent;
   if (I=nil) then Break;
   if (I^.Left=N) then
   begin
    if (I^.Right=nil) then Continue;
    I:=I^.Right;
    I:=_down(I);
   end else
   begin
    N:=I;
    Simp_LR(I);
    Simp_LRR(I);
    Simp_RLR(I);
    if (N<>I) and (Node=N) then
    begin
     Node:=I;
    end;
   end;
  until false;
 end;
end;

function DoExistsOp(R:IZResultSet):TZVariant;
begin
 Result.VType   :=vtBoolean;
 try
  Result.VBoolean:=R.Next;
 except
 end;
end;

function TExpressionState.Finish:Boolean;
begin
 Result:=true;
 if (subquery_count<>0)  then
 begin
  if Assigned(cb_fin) then
  begin
   if Assigned(Node) then
   begin
    Result:=false;
    if (not RightIsExist(Node)) and (GetNodeOp(Node)=op_br_open) then
    begin
     SetRight(Node,NewSubQueryNode(cb_fin()));
     Node:=PopRight(Node);
    end;
   end else
   begin
    Node:=NewSubQueryNode(cb_fin());
   end;
  end else
   Result:=false;
 end;
 if Assigned(Node) then
 begin
  While Assigned(Node^.Parent) do
  begin
   if (GetNodeOp(Node)=op_br_open) then
   begin
    Node:=PopRight(Node);
    Result:=false;
   end else
    Node:=Node^.Parent;
  end;
  if (GetNodeOp(Node)=op_br_open) then
  begin
   Node:=PopRight(Node);
   Result:=false;
  end;

  //Print;

  Simplification(Node);
 end;
end;

Function TExpressionState.Parse(Const FToken:TZToken):Boolean;
var
 p,n:SizeInt;
 //tmp:RawByteString;

 Function CanSelect:Byte; inline;
 begin
  Result:=0;
  if _is_select_word(FToken.P,FToken.L) then
  begin
   if not Assigned(cb_parse) then Exit(1);
   cb_parse(FToken);
   subquery_count:=1;
   Result:=2;
  end;
 end;

 Function OnFirstOp:Boolean; inline;
 begin
  Result:=true;
  Case p of
   25,
   26:begin // ~ NOT
       if RightIsExist(Node) then Exit(false);
       Node:=SetRight(Node,NewExpNode(FToken));
      end;
   17:; //+ignore
   18:begin //(0-
       if RightIsExist(Node) then Exit(false);
       Node:=SetRight(Node,NewExpZeroNode);
       Node:=pushLeft(Node,NewExpNode(FToken));
      end;
   else
       Result:=False;
  end;
 end;

 procedure PushingLeft; inline;
 begin
  while Assigned(Node^.Parent) do
  begin
   p:=n;
   n:=GetNodeOp(Node^.Parent);
   if (_get_op_arity(n)<>1) or (_get_op_prior(n)>_get_op_prior(p)) then Break;
   Node:=Node^.Parent;
  end;

  Node:=pushLeft(Node,NewExpNode(FToken));
 end;

begin
 //SetString(tmp,FToken.P,FToken.L);
 //Writeln(GetStr,'|',tmp,'|',FToken.TokenType);
 //Writeln('----------------------------');
 //Print;
 //Writeln('----------------------------');

 Result:=True;

 if (subquery_count<>0) then
 begin
  if (FToken.TokenType=ttSymbol) and (FToken.L=1) then
  begin
   Case FToken.P^ of
    '(':Inc(subquery_count);
    ')':begin
         Dec(subquery_count);
         if (subquery_count=0) then
         begin
          if not Assigned(cb_fin) then Exit(false);

          if RightIsExist(Node) then Exit(false);
          SetRight(Node,NewSubQueryNode(cb_fin()));

          if (GetNodeOp(Node)<>op_br_open) then Exit(false);
          Node:=PopRight(Node);

          Exit;
         end;
        end;
   end;
  end;
  if not Assigned(cb_parse) then Exit(false);
  cb_parse(FToken);
 end else
 begin
  if FToken.TokenType=ttWhitespace then Exit;

  p:=_get_op_id(FToken.P,FToken.L);
  Case p of
   0..7:Exit(false);
  end;

  n:=GetNodeOp(Node);

  Case p of
   -1:begin //const or var or funct
       Case n of
        -1:begin
            Case CanSelect of
             0:begin
                Case FToken.TokenType of
                 ttWord,ttKeyword:Exit(false); //funct not support
                end;
                if Assigned(Node) then Exit(false);
                if RightIsExist(Node) then Exit(false);
                SetRight(Node,NewExpNode(FToken));
               end;
             1:Exit(false);
             2:Exit;
            end;
           end;
         op_br_open:
           begin //(
            Case CanSelect of
             0:begin
                Case FToken.TokenType of
                 ttWord,ttKeyword:Exit(false); //funct not support
                end;
                if RightIsExist(Node) then Exit(false);
                Node:=SetRight(Node,NewExpNode(FToken));
               end;
             1:Exit(false);
             2:Exit;
            end;
           end;
        else
          begin
           if (n=35) and (_get_const_keyword(FToken.P,FToken.L)=0) then //IS NULL
           begin
            //Writeln('skip null');
           end else
           begin
            Case FToken.TokenType of
             ttWord,ttKeyword:Exit(false); //funct not support
            end;
            if RightIsExist(Node) then Exit(false);
            SetRight(Node,NewExpNode(FToken));
           end;
          end;
       end;
      end;
   op_br_open:
      begin //(
       if RightIsExist(Node) then
        Node:=pushRight(Node,NewExpNode(FToken))
       else
        Node:=SetRight(Node,NewExpNode(FToken));
      end;
   op_br_clos:
     begin //)
      repeat
       if not Assigned(Node^.Parent) then Exit(false);
       Node:=Node^.Parent;
      until (GetNodeOp(Node)=op_br_open);
      Node:=PopRight(Node);
     end;
   else
     begin //op
      if not Assigned(Node) then
      begin
       if not OnFirstOp then Exit(false);
      end else
      begin
       Case n of
        35:begin//IS NOT
            case p of
             26:begin //NOT
                 Node^.Data.VOP2:=1;
                end;
             else
                Exit(false);
            end;
           end;
        -1:begin
            case p of
             26:begin //NOT  //ONLY FOR 'NOT IN', 'NOT IS NULL'
                 if RightIsExist(Node) then Exit(false);
                 Node:=PushRight(Node,NewExpNode(FToken));
                end;
             else
                PushingLeft;
            end;
           end;
        op_br_open:if not OnFirstOp then Exit(false);
        else
          begin //op
           Case _get_op_arity(p) of
            1:begin //~, NOT
               if RightIsExist(Node) then Exit(false);
               Node:=SetRight(Node,NewExpNode(FToken));
              end;
            3:begin //ALL, ANY, SOME
               if not Assigned(Node^.Left) then Exit(false);
               Case n of
                8..16:;
                else Exit(false);
               end;
               Node^.Data.nType:=ntOperator;
               Node^.Data.VOP1 :=p;
               Node^.Data.VOP2 :=n;
              end;
            else
              begin
               if (p=29) and (n=26) then //'NOT IN'
               begin
                if not Assigned(Node^.Right) then Exit(false);
                Node^.Data.nType:=ntOperator;
                Node^.Data.VOP1 :=p;
                Node^.Data.VOP2 :=1;
                Node^.Left:=Node^.Right;
                Node^.Right:=nil;
                //print;
                //writeln;
               end else
               if (_get_op_prior(n)<=_get_op_prior(p)) then
               begin
                if not Assigned(Node^.Right) then Exit(false);
                PushingLeft;
               end else
               begin
                if not Assigned(Node^.Right) then Exit(false);
                Node:=Node^.Right;
                Node:=pushLeft(Node,NewExpNode(FToken));
               end;
              end;
           end;
          end;
       end;
      end;
     end;
  end;

 end;

end;

type
 TZVariantStack=specialize TFastStack<TZVariant>;

Function TExpressionCalc.calc:TZVariant;
label
 do_op;
var
 Stack:TZVariantStack;
 //Stack:array of TZVariant;
 I:PExpNode;

 VL,VR:TZVariant;
 op:SizeInt;

 procedure PushV(V:TZVariant); inline;
 {Var
  i:SizeInt;}
 begin
  //Writeln('PushV:',_GetAsRawByteString(V));
  Stack.Push(V);
  {i:=Length(Stack);
  SetLength(Stack,i+1);
  Stack[i]:=V;}
 end;

 function PopV:TZVariant;
 {Var
  i:SizeInt;}
 begin
  Result:=Default(TZVariant);
  if Stack.Top<>nil then
  begin
   Result:=Stack.Top^;
   Stack.Pop;
  end;
  {Result:=Default(TZVariant);
  i:=Length(Stack);
  if (i=0) then Exit;
  Dec(i);
  Result:=Stack[i];
  SetLength(Stack,i);}
 end;

begin
 Result:=Default(TZVariant);

 I:=_down(Node);
 if Assigned(I) then
 begin
  Stack:=Default(TZVariantStack);

  VL:=GetVariant(I,cb_GetValue,cb_GetQuery);
  PushV(VL);
  //Writeln('PushV:',_GetAsRawByteString(VL));

  repeat

   if (I^.Parent=nil) then Break;

   if (I^.Parent^.Left=I) then
   begin
    I:=I^.Parent;

    if GetNodeOp(I)=-1 then Exit;

    if (I^.Right=nil) then goto do_op;
    I:=I^.Right;

    I:=_down(I);
    VL:=GetVariant(I,cb_GetValue,cb_GetQuery);
    PushV(VL);

   end else
   begin
    I:=I^.Parent;

    do_op:

    op:=GetNodeOp(I);

    if op=-1 then Exit;

    //Writeln(' ',_get_op_name(op,0));

    Case op of
     25,26,35:begin //~, NOT, IS NULL
               VR:=PopV;
               ResolveFirstValue(VR);
               PushV(DoTokenOp(op,I^.Data.VOP2,Default(TZVariant),VR));
              end;
       29..32:begin //IN, ALL, ANY, SOME
               VR:=PopV;
               VL:=PopV;
               ResolveFirstValue(VL);
               PushV(DoTokenOp(op,I^.Data.VOP2,VL,VR));
            end;
     else
            begin
             VR:=PopV;
             VL:=PopV;
             ResolveFirstValue(VR);
             ResolveFirstValue(VL);
             PushV(DoTokenOp(op,0,VL,VR));
            end;
    end;

   end;

  until false;

 end;

 Result:=PopV;

 Stack.Free;

 //Writeln('Len.Stack=',Length(Stack));

end;

end.

