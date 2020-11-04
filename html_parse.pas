{ Simple and not full html parser

  Copyright (C) 2019-2020 Red_prig

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

unit html_parse;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils;

type
 THtmlTagParser=packed class
  private
   FState:Byte;
   EState:Byte;
  public
   function  IsComment:Boolean; inline;
   function  IsScript:Boolean; inline;
   function  IsSingleQuoted:Boolean; inline;
   function  IsDoubleQuoted:Boolean; inline;
   function  IsQuoted:Boolean;       inline;
   Procedure SkipElement;            inline;
   Procedure Abort;                  inline;
   Procedure Parse(P:PAnsiChar;Len:SizeInt);
   Procedure OnEndElement;                    virtual;
   Procedure OnElement(C:AnsiChar);           virtual;
   Procedure OnData(P:PAnsiChar;Len:SizeInt); virtual;
 end;
 THtmlAttrParser=packed class(THtmlTagParser)
  private
   PState:Byte;
  protected
   N,V:Pchar;
   F:Char;  
  public
   Destructor Destroy;               override;
   Procedure  OnEndElement;          override;
   Procedure  OnElement(C:AnsiChar); override;
   Procedure  OnAttr;                virtual;
   Procedure  OnElementName;         virtual;
 end;

function GetUnescapeHTML(data:Pchar;size:SizeInt):RawByteString;

implementation

Procedure SetPcharZero(var Dst:Pchar); inline;
begin
 if Dst=nil then Exit;
 Dst[0]:=#0;
end;

function isZero(Dst:Pchar):Boolean; inline;
begin
 if Dst=nil then Exit(True);
 Result:=Dst[0]=#0;
end;

Procedure SetPchar(var Dst:Pchar;Src:Pchar);
Var
 Len:SizeUInt;
begin
 if (Src=nil) then
 begin
  SetPcharZero(Dst);
  Exit;
 end;
 Len:=StrLen(Src)+1;
 if (Dst=nil) then
 begin
  Dst:=GetMem(Len);
 end else
 if (MemSize(Dst)<Len) then
 begin
  Dst:=ReAllocMem(Dst,Len);
 end;
 Move(Src^,Dst^,Len);
end;

Procedure SetChar(var Dst:Pchar;C:Char);
begin
 if (C=#0) then
 begin
  SetPcharZero(Dst);
  Exit;
 end;
 if (Dst=nil) then
 begin
  Dst:=GetMem(SizeOf(Pointer));
 end else
 if (MemSize(Dst)<2) then
 begin
  Dst:=ReAllocMem(Dst,SizeOf(Pointer));
 end;
 Dst[0]:=C;
 Dst[1]:=#0;
end;

Procedure AddChar(var Dst:Pchar;C:Char);
Var
 Len:SizeUInt;
begin
 if (Dst=nil) then
 begin
  Dst:=GetMem(SizeOf(Pointer));
  Dst[0]:=C;
  Dst[1]:=#0;
 end else
 begin
  Len:=StrLen(Dst)+2;
  if (MemSize(Dst)<Len) then
  begin
   Case Len of
    0..SizeOf(Pointer):
         Dst:=ReAllocMem(Dst,SizeOf(Pointer));
    else
         Dst:=ReAllocMem(Dst,Len+(Len div 2));
   end;
  end;
  Dec(Len);
  Dst[Len]:=#0;
  Dec(Len);
  Dst[Len]:=C;
 end;
end;


Function GetPunctuation(Const name:RawByteString):WideChar;
begin
 Result:=#0;
 Case name of

  'Mu':Result:=#$039C;
  'Nu':Result:=#$039D;
  'Pi':Result:=#$03A0;
  'Xi':Result:=#$039E;
  'ge':Result:=#$2265;
  'gt':Result:=#$003E;
  'le':Result:=#$2264;
  'lt':Result:=#$003C;
  'mu':Result:=#$03BC;
  'ne':Result:=#$2260;
  'ni':Result:=#$220B;
  'nu':Result:=#$03BD;
  'or':Result:=#$2228;
  'pi':Result:=#$03C0;
  'xi':Result:=#$03BE;

  'Chi':Result:=#$03A7;
  'ETH':Result:=#$00D0;
  'Eta':Result:=#$0397;
  'Phi':Result:=#$03A6;
  'Psi':Result:=#$03A8;
  'Rho':Result:=#$03A1;
  'Tau':Result:=#$03A4;
  'amp':Result:=#$0026;
  'and':Result:=#$2227;
  'ang':Result:=#$2220;
  'cap':Result:=#$2229;
  'chi':Result:=#$03C7;
  'cup':Result:=#$222A;
  'deg':Result:=#$00B0;
  'eta':Result:=#$03B7;
  'eth':Result:=#$00F0;
  'int':Result:=#$222B;
  'loz':Result:=#$25CA;
  'lrm':Result:=#$200E;
  'not':Result:=#$00AC;
  'phi':Result:=#$03C6;
  'piv':Result:=#$03D6;
  'psi':Result:=#$03C8;
  'reg':Result:=#$00AE;
  'rho':Result:=#$03C1;
  'rlm':Result:=#$200F;
  'shy':Result:=#$00AD;
  'sim':Result:=#$223C;
  'sub':Result:=#$2282;
  'sum':Result:=#$2211;
  'sup':Result:=#$2283;
  'tau':Result:=#$03C4;
  'uml':Result:=#$00A8;
  'yen':Result:=#$00A5;
  'zwj':Result:=#$200D;

  'Auml':Result:=#$00C4;
  'Beta':Result:=#$0392;
  'Euml':Result:=#$00CB;
  'Iota':Result:=#$0399;
  'Iuml':Result:=#$00CF;
  'Ouml':Result:=#$00D6;
  'Uuml':Result:=#$00DC;
  'Yuml':Result:=#$014E;
  'Zeta':Result:=#$0396;
  'auml':Result:=#$00E4;
  'beta':Result:=#$03B2;
  'bull':Result:=#$2022;
  'cent':Result:=#$00A2;
  'circ':Result:=#$02C6;
  'cong':Result:=#$2245;
  'copy':Result:=#$00A9;
  'dArr':Result:=#$21D3;
  'darr':Result:=#$2193;
  'emsp':Result:=#$2003;
  'ensp':Result:=#$2002;
  'euml':Result:=#$00EB;
  'euro':Result:=#$20AC;
  'fnof':Result:=#$0192;
  'hArr':Result:=#$21D4;
  'harr':Result:=#$2194;
  'iota':Result:=#$03B9;
  'isin':Result:=#$2208;
  'iuml':Result:=#$00EF;
  'lArr':Result:=#$21D0;
  'lang':Result:=#$2329;
  'larr':Result:=#$2190;
  'macr':Result:=#$00AF;
  'nbsp':Result:=#$00A0;
  'nsub':Result:=#$2284;
  'ordf':Result:=#$00AA;
  'ordm':Result:=#$00BA;
  'ouml':Result:=#$00F6;
  'para':Result:=#$00B6;
  'part':Result:=#$2202;
  'perp':Result:=#$22A5;
  'prod':Result:=#$220F;
  'prop':Result:=#$221D;
  'quot':Result:=#$0022;
  'rArr':Result:=#$21D2;
  'rang':Result:=#$232A;
  'rarr':Result:=#$2192;
  'real':Result:=#$211C;
  'sdot':Result:=#$22C5;
  'sect':Result:=#$00A7;
  'sube':Result:=#$2286;
  'sup1':Result:=#$00B9;
  'sup2':Result:=#$00B2;
  'sup3':Result:=#$00B3;
  'supe':Result:=#$2287;
  'uArr':Result:=#$21D1;
  'uarr':Result:=#$2191;
  'uuml':Result:=#$00FC;
  'yuml':Result:=#$00FF;
  'zeta':Result:=#$03B6;
  'zwnj':Result:=#$200C;

  'AElig':Result:=#$00C6;
  'Acirc':Result:=#$00C2;
  'Alpha':Result:=#$0391;
  'Aring':Result:=#$00C5;
  'Delta':Result:=#$0394;
  'Ecirc':Result:=#$00CA;
  'Gamma':Result:=#$0393;
  'Icirc':Result:=#$00CE;
  'Kappa':Result:=#$039A;
  'OElig':Result:=#$0152;
  'Ocirc':Result:=#$00D4;
  'Omega':Result:=#$03A9;
  'Prime':Result:=#$2033;
  'Sigma':Result:=#$03A3;
  'THORN':Result:=#$00DE;
  'Theta':Result:=#$0398;
  'Ucirc':Result:=#$00DB;
  'acirc':Result:=#$00E2;
  'acute':Result:=#$00B4;
  'aelig':Result:=#$00E6;
  'alpha':Result:=#$03B1;
  'aring':Result:=#$00E5;
  'asymp':Result:=#$2248;
  'bdquo':Result:=#$201E;
  'cedil':Result:=#$00B8;
  'clubs':Result:=#$2663;
  'crarr':Result:=#$21B5;
  'delta':Result:=#$03B4;
  'diams':Result:=#$2666;
  'ecirc':Result:=#$00EA;
  'empty':Result:=#$2205;
  'equiv':Result:=#$2261;
  'exist':Result:=#$2203;
  'frasl':Result:=#$2044;
  'gamma':Result:=#$03B3;
  'icirc':Result:=#$00EE;
  'iexcl':Result:=#$00A1;
  'image':Result:=#$2111;
  'infin':Result:=#$221E;
  'kappa':Result:=#$03BA;
  'laquo':Result:=#$00AB;
  'lceil':Result:=#$2308;
  'ldquo':Result:=#$201C;
  'lsquo':Result:=#$2018;
  'mdash':Result:=#$2014;
  'micro':Result:=#$00B5;
  'minus':Result:=#$2212;
  'nabla':Result:=#$2207;
  'ndash':Result:=#$2013;
  'notin':Result:=#$2209;
  'ocirc':Result:=#$00F4;
  'oelig':Result:=#$0153;
  'oline':Result:=#$203E;
  'omega':Result:=#$03C9;
  'oplus':Result:=#$2295;
  'pound':Result:=#$00A3;
  'prime':Result:=#$2032;
  'radic':Result:=#$221A;
  'raquo':Result:=#$00BB;
  'rceil':Result:=#$2309;
  'rdquo':Result:=#$201D;
  'rsquo':Result:=#$2019;
  'sbquo':Result:=#$201A;
  'sigma':Result:=#$03C3;
  'szlig':Result:=#$00DF;
  'theta':Result:=#$03B8;
  'thorn':Result:=#$00FE;
  'tilde':Result:=#$02DC;
  'times':Result:=#$00D7;
  'trade':Result:=#$2122;
  'ucirc':Result:=#$00FB;
  'upsih':Result:=#$03D2;

  'Aacute':Result:=#$00C1;
  'Agrave':Result:=#$00C0;
  'Atilde':Result:=#$00C3;
  'Ccedil':Result:=#$00C7;
  'Dagger':Result:=#$2021;
  'Eacute':Result:=#$00C9;
  'Egrave':Result:=#$00C8;
  'Iacute':Result:=#$00CD;
  'Igrave':Result:=#$00CC;
  'Lambda':Result:=#$039B;
  'Ntilde':Result:=#$00D1;
  'Oacute':Result:=#$00D3;
  'Ograve':Result:=#$00D2;
  'Oslash':Result:=#$00D8;
  'Otilde':Result:=#$00D5;
  'Scaron':Result:=#$0160;
  'Uacute':Result:=#$00DA;
  'Ugrave':Result:=#$00D9;
  'Yacute':Result:=#$00DD;
  'aacute':Result:=#$00E1;
  'agrave':Result:=#$00E0;
  'atilde':Result:=#$00E3;
  'brvbar':Result:=#$00A6;
  'ccedil':Result:=#$00E7;
  'curren':Result:=#$00A4;
  'dagger':Result:=#$2020;
  'divide':Result:=#$00F7;
  'eacute':Result:=#$00E9;
  'egrave':Result:=#$00E8;
  'forall':Result:=#$2200;
  'frac12':Result:=#$00BD;
  'frac14':Result:=#$00BC;
  'frac34':Result:=#$00BE;
  'hearts':Result:=#$2665;
  'hellip':Result:=#$2026;
  'iacute':Result:=#$00ED;
  'igrave':Result:=#$00EC;
  'iquest':Result:=#$00BF;
  'lambda':Result:=#$03BB;
  'lfloor':Result:=#$230A;
  'lowast':Result:=#$2217;
  'lsaquo':Result:=#$2039;
  'middot':Result:=#$00B7;
  'ntilde':Result:=#$00F1;
  'oacute':Result:=#$00F3;
  'ograve':Result:=#$00F2;
  'oslash':Result:=#$00F8;
  'otilde':Result:=#$00F5;
  'otimes':Result:=#$2297;
  'permil':Result:=#$2030;
  'plusmn':Result:=#$00B1;
  'rfloor':Result:=#$230B;
  'rsaquo':Result:=#$203A;
  'scaron':Result:=#$0161;
  'sigmaf':Result:=#$03C2;
  'spades':Result:=#$2660;
  'there4':Result:=#$2234;
  'thinsp':Result:=#$2009;
  'uacute':Result:=#$00FA;
  'ugrave':Result:=#$00F9;
  'weierp':Result:=#$2118;
  'yacute':Result:=#$00FD;


  'Epsilon':Result:=#$0395;
  'Omicron':Result:=#$039F;
  'Upsilon':Result:=#$03A5;
  'alefsym':Result:=#$2135;
  'epsilon':Result:=#$03B5;
  'omicron':Result:=#$03BF;
  'upsilon':Result:=#$03C5;

  'thetasym':Result:=#$03D1;

 end;
end;

Function GetStr(data:Pchar;size:size_t):RawByteString; inline;
begin
 SetLength(Result,size);
 Move(data^,Result[1],size);
end;

function GetUnescapeHTML(data:Pchar;size:SizeInt):RawByteString;
Var
 i,v:SizeInt;
 W:WideChar;
begin
 Result:='';
 if (size=0) then Exit;
 i:=0;
 v:=0;
 repeat

  v:=System.IndexByte(data[i],size-i,Byte('&'));
  if (v<0) then
  begin
   if (i<size) then Result:=Result+GetStr(@data[i],size-i);
   Exit;
  end else
  begin
   if (v>0) then Result:=Result+GetStr(@data[i],v);
   i:=i+v;

   if (i>=size) then
   begin
    Result:=Result+'&';
    Exit;
   end;

   v:=System.IndexByte(data[i+1],size-i-1,Byte(';'));
   if (v<0) then
   begin
    if (i<size) then Result:=Result+GetStr(@data[i],size-i);
    Exit;
   end else
   begin

    if (v>0) then
    begin
     W:=GetPunctuation(GetStr(@data[i+1],v));
     if (W<>#0) then
     begin
      Result:=Result+UTF8Encode(W);
     end else
     begin
      Result:=Result+GetStr(@data[i],v+1);
     end;
    end;

    i:=i+v+2;

   end;


  end;

 until (i>=size);
end;

{Function _CmpText(buf1,buf2:Pchar;Len:SizeInt):SizeInt;
Var
 i:SizeInt;
begin
 Result:=0;
 For i:=0 to Len-1 do
 begin
  Result:=Byte(LowerCase(buf1[i]));
  Result:=Result-Byte(LowerCase(buf2[i]));
  if Result<>0 then Break;
 end;
end;}

Function CmpComment(buf:Pchar):Boolean; inline;
Const
 DWCMP=$3E2D2D;
begin
// Result:=False;
 //if buf[0]<>'-' then Exit;
 //if buf[1]<>'-' then Exit;
 //if buf[2]<>'>' then Exit;

 Result:=(PDword(buf)^ and $FFFFFF)=DWCMP;

 //Result:=True;
end;

Function CmpScript(buf:Pchar):Boolean;
Const
 WCMP=$2F3C;
begin
 Result:=False;
 //if buf[0]<>'<' then Exit;
 //if buf[1]<>'/' then Exit;

 if PWord(buf)^<>WCMP then Exit;

 if LowerCase(buf[2])<>'s' then Exit;
 if LowerCase(buf[3])<>'c' then Exit;
 if LowerCase(buf[4])<>'r' then Exit;
 if LowerCase(buf[5])<>'i' then Exit;
 if LowerCase(buf[6])<>'p' then Exit;
 if LowerCase(buf[7])<>'t' then Exit;
 Result:=True;
end;

Function CmpStyle(buf:Pchar):Boolean;
Const
 WCMP=$2F3C;
begin
 Result:=False;
 //if buf[0]<>'<' then Exit;
 //if buf[1]<>'/' then Exit;

 if PWord(buf)^<>WCMP then Exit;

 if LowerCase(buf[2])<>'s' then Exit;
 if LowerCase(buf[3])<>'t' then Exit;
 if LowerCase(buf[4])<>'y' then Exit;
 if LowerCase(buf[5])<>'l' then Exit;
 if LowerCase(buf[6])<>'e' then Exit;
 Result:=True;
end;

Function MemNext_Comment(Memory:Pointer;i,Size:SizeInt):SizeInt;
begin
 Result:=-1;
 if (i<0) or (i>=Size) or (Size-i<3) then Exit;

 Size:=Size-3;
 while (i<=Size) do
 begin
  if CmpComment(@PByte(Memory)[i]) then Exit(i+3);
  case PByte(Memory)[i+3] of
   45:i:=i+2;
   62:i:=i+1;
   else
      i:=i+4;
  end;
 end;
end;

Function MemPos_Script(Memory:Pointer;i,Size:SizeInt):SizeInt;
begin
 Result:=-1;
 if (i<0) or (i>=Size) or (Size-i<8) then Exit;

 Size:=Size-8;
 while (i<=Size) do
 begin
  if CmpScript(@PByte(Memory)[i]) then Exit(i);
  case PByte(Memory)[i+8] of
   47    :i:=i+7;
   60    :i:=i+8;
   99 ,67:i:=i+5;
   105,73:i:=i+3;
   112,80:i:=i+2;
   114,82:i:=i+4;
   115,83:i:=i+6;
   116,84:i:=i+1;
   else
          i:=i+9;
  end;
 end;
end;

Function MemPos_Style(Memory:Pointer;i,Size:SizeInt):SizeInt;
begin
 Result:=-1;
 if (i<0) or (i>=Size) or (Size-i<7) then Exit;

 Size:=Size-7;
 while (i<=Size) do
 begin
  if CmpStyle(@PByte(Memory)[i]) then Exit(i);
  case PByte(Memory)[i+7] of
   47    :i:=i+6;
   60    :i:=i+7;
   101,69:i:=i+1;
   108,76:i:=i+2;
   115,83:i:=i+5;
   116,84:i:=i+4;
   121,89:i:=i+3;
   else
          i:=i+8;
  end;
 end;
end;

{
s 115 83
c 99  67
r 114 82
i 105 73
p 112 80
t 116 84
y 121 89
l 108 76
e 101 69
}

{Procedure WriteUp(C:Char);
begin
 Writeln(C,' ',Ord(LowerCase(C)),' ',Ord(UpCase(C)));
end;}

{
Function MemPos(Memory:Pointer;Const Substr:RawByteString;i,Size:SizeInt):SizeInt;
Const
 ASIZEM=255;
var
 q,Len:SizeInt;
 qs_bc:array[0..ASIZEM] of SizeInt;
begin
 Result:=-1;
 if (Length(SubStr)=0) or (i<0) or (i>=Size) or (Size-i<Length(SubStr)) then Exit;

 Len:=Length(SubStr)+1;
 For q:=0 to ASIZEM do qs_bc[q]:=Len;
 For q:=1 to Length(SubStr) do qs_bc[Byte(LowerCase(SubStr[q]))]:=Len-q;

 Writeln(Substr,' Len=',Length(SubStr));
 For q:=0 to ASIZEM do
 begin
  Writeln('#',q,'=',qs_bc[q]);
 end;

 {WriteUp('s');
 WriteUp('c');
 WriteUp('r');
 WriteUp('i');
 WriteUp('p');
 WriteUp('t');
 WriteUp('y');
 WriteUp('l');
 WriteUp('e');}

 Len:=Size-Length(SubStr);
 while (i<=Len) do
 begin
  if _CmpText(@PByte(Memory)[i],@SubStr[1],Length(SubStr))=0 then Exit(i);
  i:=i+qs_bc[Byte(LowerCase(PChar(Memory)[i+Length(SubStr)]))];
 end;

end;
}

{
Function MemNext(Memory:Pointer;Const Substr:RawByteString;i,Size:SizeInt):SizeInt; inline;
begin
 Result:=-1;
 i:=MemPos(Memory,Substr,i,Size);
 if (i<>-1) then Result:=i+Length(Substr);
end;
}

Destructor THtmlAttrParser.Destroy;
begin
 FreeMem(N);
 FreeMem(V);
 inherited;
end;

Procedure THtmlAttrParser.OnEndElement;
begin

 Case PState of
  0,
  5:OnElementName;
  1,
  2,
  3:OnAttr;
 end;

 SetPcharZero(N);
 SetPcharZero(V);

 PState:=0;
end;  

Procedure THtmlAttrParser.OnAttr;
begin
end;

Procedure THtmlAttrParser.OnElementName;
begin
end;

//0 5      4 1   2 3      4
//?element   Name="value"

Procedure THtmlAttrParser.OnElement(C:AnsiChar);
begin
 if IsQuoted then
 begin
  Case PState of
   0:begin
      F:=#0;
      PState:=5;
      AddChar(N,C);
     end;
   5,
   1:AddChar(N,C);
   2:begin
      PState:=3;
      SetPcharZero(V);
     end;
   3:if IsSingleQuoted then
     begin
      if (C<>'''') then AddChar(V,C);
     end else
     begin
      if (C<>'"')  then AddChar(V,C);
     end;
  end;
 end else
  Case PState of
   0:Case C of
       #0..' ',#$FF:
       begin
        F:=#0;
        PState:=4;
        OnElementName;
        SetPcharZero(N);
       end;
       '!','?','/':
       begin
        F:=C;
        PState:=5;
       end;
       else
       begin
        F:=#0;
        PState:=5;
        AddChar(N,C);
       end;
     end;
   5:Case C of
       #0..' ',#$FF:
       begin
        PState:=4;
        OnElementName;
        SetPcharZero(N);
       end;
       else
       begin
        AddChar(N,C);
       end;
     end;
   1:Case C of
       #0..' ',#$FF:
       begin
        PState:=4;
        OnAttr;
        SetPcharZero(N);
        SetPcharZero(V);
       end;
       '=':
       begin
        PState:=2;
       end;
       else
       begin
        AddChar(N,C);
       end;
      end;
   2:Case C of
      #0..' ',#$FF:
      begin
       PState:=4;
       OnAttr;
       SetPcharZero(N);
       SetPcharZero(V);
      end;
      else
      begin
       PState:=3;
       SetChar(V,C);
      end;
     end;
   3:Case C of
      #0..' ',#$FF:
      begin
       PState:=4;
       OnAttr;
       SetPcharZero(N);
       SetPcharZero(V);
      end;
      else
      begin
       AddChar(V,C);
      end;
     end;
   4:Case C of
      #0..' ',#$FF:;
      else
      begin
       PState:=1;
       SetChar(N,C);
      end;
     end;
  end;

end;

Procedure THtmlTagParser.OnEndElement;
begin
end;

Procedure THtmlTagParser.OnElement(C:AnsiChar);
begin
end;

Procedure THtmlTagParser.OnData(P:PAnsiChar;Len:SizeInt);
begin

end;

function THtmlTagParser.IsComment:Boolean; inline;
begin
 Result:=(FState=02);
end;   

function THtmlTagParser.IsScript:Boolean; inline;
begin
 Result:=(FState=03);
end;

function THtmlTagParser.IsSingleQuoted:Boolean; inline;
begin
 Result:=(FState=12)
end;

function THtmlTagParser.IsDoubleQuoted:Boolean; inline;
begin
 Result:=(FState=13)
end;

function THtmlTagParser.IsQuoted:Boolean; inline;
begin
 Result:=IsSingleQuoted or IsDoubleQuoted;
end;

Procedure THtmlTagParser.SkipElement; inline;
begin
 EState:=40;
end;

Procedure THtmlTagParser.Abort; inline;
begin
 FState:=05;
end;

Procedure THtmlTagParser.Parse(P:PAnsiChar;Len:SizeInt);
Var
 i,v:SizeInt;
begin
 if (P=nil) or (Len<=0) then Exit;
 i:=0;
 v:=0;
 While (i<Len) do
 begin
  Case FState of
   01:begin //open tag
       FState:=11;
       Case P[i] of
        '>':
        begin
         FState:=00;
         OnEndElement;
         v:=i+1;
        end;
        '''':begin
              FState:=12;
              OnElement(P[i]);
             end;
        '"':begin
              FState:=13;
              OnElement(P[i]);
            end;
        '!':EState:=14;
        's',
        'S':begin
             EState:=20;
             OnElement(P[i]);
            end;
        else
         OnElement(P[i]);
       end;
      end;

   12:begin
       if EState<>40 then OnElement(P[i]);
       Case P[i] of //tag qote1 close
        '''':FState:=11;
       end;
      end;
   13:begin
       if EState<>40 then OnElement(P[i]);
       Case P[i] of //tag qote1 close
        '"':FState:=11;
       end;
      end;

   11:begin //opened tag

       Case EState of
        14:Case P[i] of //open tag comment1
            '-': EState:=15;
            else
            begin
             EState:=00;
             OnElement('!');
            end;
           end;
        15:Case P[i] of //open tag comment2
            '-': FState:=02;
            else
            begin
             EState:=00;
             OnElement('!');
             OnElement('-');
            end;
           end;

        20:Case P[i] of //open tag s
            'c',
            'C': EState:=21;
            't',
            'T': EState:=30;
            else EState:=00;
           end;
        21:Case P[i] of //open tag c
            'r',
            'R': EState:=22;
            else EState:=00;
           end;
        22:Case P[i] of //open tag r
            'i',
            'I': EState:=23;
            else EState:=00;
           end;
        23:Case P[i] of //open tag i
            'p',
            'P': EState:=24;
            else EState:=00;
           end;
        24:Case P[i] of //open tag p
            't',
            'T': EState:=25; //opened script tag
            else EState:=00;
           end;

        30:Case P[i] of //open tag t
            'y',
            'Y': EState:=31;
            else EState:=00;
           end;
        31:Case P[i] of //open tag y
            'l',
            'L': EState:=32;
            else EState:=00;
           end;
        32:Case P[i] of //open tag l
            'e',
            'E': EState:=33; //opened style tag
            else EState:=00;
           end;


       end;

       Case EState of
        14,15:;//nothing
        else
        begin
         Case P[i] of //opened tag //
          '>':        //close tag
          begin
           Case EState of
            25:FState:=03; //opened script tag
            33:FState:=04; //opened style tag
            else
            begin
             FState:=00;
             EState:=00;
            end;
           end;
           OnEndElement;
           v:=i+1;
          end;
          '!','?','/':;
          '''':begin
                FState:=12;
                if EState<>40 then OnElement(P[i]);
               end;
          '"':begin
                FState:=13;
                if EState<>40 then OnElement(P[i]);
              end;
          else
           if EState<>40 then OnElement(P[i]);
         end;
        end;
       end;


      end;

    00:begin //data tag
        v:=i;
        i:=System.IndexByte(P[i],Len-i,Byte('<'));
        if (i=-1) then
        begin
         i:=Len;
         Break;
        end else
        begin
         i:=i+v;
         if (i>v) then OnData(@P[v],i-v);
         FState:=01;
         EState:=00;
        end;
       end;

    02:begin //comment tag
        v:=i;
        i:=MemNext_Comment(P,i,Len);
        if (i=-1) then
        begin
         i:=Len;
         Break;
        end else
        begin
         if (i-3>v) then OnData(@P[v],(i-3)-v);
         i:=i-1;
         FState:=00;
         EState:=00;
        end;
       end;  

    03:begin //script tag
       v:=i;
       i:=MemPos_Script(P,i,Len);
       if (i=-1) then
       begin
        i:=Len;
        Break;
       end else
       begin
        if (i>v) then OnData(@P[v],i-v);
        FState:=01;
        EState:=00;
       end;
      end;

    04:begin //style tag
       v:=i;
       i:=MemPos_Style(P,i,Len);
       if (i=-1) then
       begin
        i:=Len;
        Break;
       end else
       begin
        if (i>v) then OnData(@P[v],i-v);
        FState:=01;
        EState:=00;
       end;
      end;

    05:Exit;
  end;

  Inc(i);
 end;

 if (i>v) then
  Case FState of
   00,02,03,04:OnData(@P[v],i-v);
  end;

end;


end.

