unit WordFilter;

{$mode objfpc}{$H+}

interface

function  Utf8Cut(P:PAnsiChar;Len,MaxChars:SizeInt):SizeInt;
procedure FiltredChar(P:PAnsiChar;Len:SizeInt);
procedure FiltredWord(Const SubStr:RawByteString;P:PAnsiChar;Len:SizeInt);
procedure FiltredWords(P:PAnsiChar;Len:SizeInt);

implementation


function Utf8Cut(P:PAnsiChar;Len,MaxChars:SizeInt):SizeInt;
Var
 d,h:SizeInt;
begin
 Result:=0;
 h:=0;
 While (Result<Len) and (h<MaxChars) do
 begin
  d:=Utf8CodePointLen(PAnsiChar(P),4,True);
  if (d<=0) then Exit;
  Inc(h);
  Inc(Result,d);
  Inc(P,d);
 end;
end;

function PosPChar(const SubStr:RawByteString;P:PAnsiChar;Len:SizeInt):SizeInt;
var
 i,MaxLen,SubLen,Offset:SizeInt;
 SubFirst:AnsiChar;
 pc:PAnsiChar;
begin
 Result:=-1;
 SubLen:=Length(SubStr);
 if (SubLen > 0) and (Len>0) and (P<>nil) then
 begin
  Offset:=0;
  MaxLen:=Len-SubLen;
  SubFirst:=SubStr[1];
  i:=indexbyte(P[Offset],Len-Offset,Byte(SubFirst));
  while (i>=0) and (i+Offset<=MaxLen) do
  begin
   pc:=@P[Offset+i];
   if (CompareByte(PAnsiChar(Substr)^,pc^,SubLen)=0) then
   begin
    Result:=Offset+i;
    Exit;
   end;
   Offset:=Offset+i+1;
   i:=indexbyte(P[Offset],Len-Offset,Byte(SubFirst));
  end;
 end;
end;

procedure FiltredChar(P:PAnsiChar;Len:SizeInt);
begin
 While (Len>0) do
 begin
  Case P^ of
   '#'..'*','<','>','@','{'..'}',
   '^','~','_','!','?':P^:=' ';
  end;
  Inc(P);
  Dec(Len);
 end;
end;

procedure DumpSpace(P:PAnsiChar;Len:SizeInt); inline;
begin
 While (Len>0) do
 begin
  P^:=' ';
  Inc(P);
  Dec(Len);
 end;
end;

procedure FiltredWord(Const SubStr:RawByteString;P:PAnsiChar;Len:SizeInt);
var
 i:Integer;
begin
 While (Len>0) do
 begin
  i:=PosPChar(SubStr,P,Len);
  if (i<0) then Exit;
  P:=@P[i];
  Len:=Len-i;
  DumpSpace(P,Length(SubStr));
  P:=@P[Length(SubStr)];
  Len:=Len-Length(SubStr);
 end;
end;

procedure FiltredWords(P:PAnsiChar;Len:SizeInt);
begin
 FiltredWord('nig' ,P,Len);
 FiltredWord('nug' ,P,Len);
 FiltredWord('ниг' ,P,Len);
 FiltredWord('nиг' ,P,Len);
 FiltredWord('нiг' ,P,Len);
 FiltredWord('ниg' ,P,Len);
 FiltredWord('niг' ,P,Len);
 FiltredWord('нig' ,P,Len);
 FiltredWord('сук' ,P,Len);
 FiltredWord('хуй' ,P,Len);
 FiltredWord('бляд',P,Len);
 FiltredWord('пизд',P,Len);
 FiltredWord('pidr',P,Len);
 FiltredWord('пидр',P,Len);
 FiltredWord('pидр',P,Len);
 FiltredWord('пiдр',P,Len);
 FiltredWord('пиdр',P,Len);
 FiltredWord('пидr',P,Len);
 FiltredWord('piдр',P,Len);
 FiltredWord('пidр',P,Len);
 FiltredWord('пиdr',P,Len);
 FiltredWord('pidр',P,Len);
 FiltredWord('пidr',P,Len);
 FiltredWord('pido',P,Len);
 FiltredWord('пидо',P,Len);
 FiltredWord('pидо',P,Len);
 FiltredWord('пiдо',P,Len);
 FiltredWord('пиdо',P,Len);
 FiltredWord('пидo',P,Len);
 FiltredWord('piдо',P,Len);
 FiltredWord('пidо',P,Len);
 FiltredWord('пиdo',P,Len);
 FiltredWord('pidо',P,Len);
 FiltredWord('пido',P,Len);
 FiltredWord('хидж',P,Len);
 FiltredWord('симп',P,Len);
 FiltredWord('simp',P,Len);
end;


end.

