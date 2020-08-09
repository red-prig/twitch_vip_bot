{ Simple and not full xml parser

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

unit xml_parse;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils;

type
 TXmlTagParser=packed class
  private
   FState:Byte;
   EState:Byte;
  public
   Pos:SizeInt;
   SrcCP:TSystemCodePage;
   Procedure Parse(P:PAnsiChar;Len:SizeInt);
   Procedure Abort;                  inline;
  protected
   function  IsComment:Boolean; inline;
   function  IsSingleQuoted:Boolean; inline;
   function  IsDoubleQuoted:Boolean; inline;
   function  IsQuoted:Boolean;       inline;
   Procedure SkipElement;            inline;
   Procedure OnEndElement;                    virtual;
   Procedure OnElement(C:AnsiChar);           virtual;
   Procedure OnData(P:PAnsiChar;Len:SizeInt); virtual;
 end;
 TXmlAttrParser=packed class(TXmlTagParser)
  private
   PState:Byte;
  protected
   N,V:Pchar;
   F,L:Char;
  public
   Destructor Destroy;               override;
  protected
   Procedure  OnEndElement;          override;
   Procedure  OnElement(C:AnsiChar); override;
   Procedure  OnAttr;                virtual;
   Procedure  OnElementName;         virtual;
 end;

 TXmlSimpleParser=packed class(TXmlTagParser)
  protected
   N:Pchar;
   F,L:Char;
  public
   Destructor Destroy;               override;
  protected
   Procedure  OnEndElement;          override;
   Procedure  OnElement(C:AnsiChar); override;
   Procedure  OnElementName;         virtual;
 end;

 TXMLNodeType = (
   ntNone,
   ntElement,
   ntAttribute,
   ntText,
   //ntCDATA,
   ntComment,
   ntEndElement,
   ntXmlDeclaration
   );

 TReaderEvent=procedure(Sender:TObject) of object;

 TXmlTextReader=packed class(TXmlSimpleParser)
  protected
   FNodeType:TXMLNodeType;
   LN,LV:Pchar;
   Procedure  OnEvent;
  public
   Event:TReaderEvent;

   property   nodeType:TXMLNodeType read FNodeType;

   Function   Name:RawByteString;
   Function   Value:RawByteString;

   Procedure  OnData(P:PAnsiChar;Len:SizeInt); override;
   Procedure  OnElementName;         override;

   Destructor Destroy;               override;
 end;
  
 TXmlAttrTextReader=packed class(TXmlAttrParser)
  protected
   FNodeType:TXMLNodeType;
   LV:Pchar;
   Procedure  OnEvent;
  public
   Event:TReaderEvent;

   property   nodeType:TXMLNodeType read FNodeType;

   Function   Name:RawByteString;
   Function   Value:RawByteString;

   Procedure  OnData(P:PAnsiChar;Len:SizeInt); override;
   Procedure  OnEndElement;          override;
   Procedure  OnAttr;                override;
   Procedure  OnElementName;         override;

   Destructor Destroy;               override;
 end;

 PReadNode=^TReadNode;
 TClassNode=class of TNodeFunc;

 TNodeReader=class
  private
   Root:PReadNode;
   UNode:SizeUInt;
  public
   LastName:RawByteString;

   function   CData:Pointer; inline;
   Procedure  Reset;
   Procedure  Push(C:TClassNode;D:Pointer);
   Procedure  Pop;
   Destructor Destroy; override;

   Procedure  DoSet(nType:TXMLNodeType;Const Name,Value:RawByteString);
 end;

 TNodeFunc=class
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); virtual;
  class procedure CLS(Node:TNodeReader;Const Name:RawByteString); virtual;
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); virtual;
  class procedure ATR(Node:TNodeReader;Const Name,Value:RawByteString); virtual;
 end;

 TReadNode=object
  private
   Next,Prev:PReadNode;
  public
   {$IFOPT D+}
   CName:RawByteString;
   {$ENDIF}
   CNode:TClassNode;
   CData:Pointer;
 end;


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

Procedure SetPchar(var Dst:Pchar;Src:Pchar;Len:SizeUInt);
begin
 if (Src=nil) then
 begin
  SetPcharZero(Dst);
  Exit;
 end;
 Inc(Len);
 if (Dst=nil) then
 begin
  Dst:=GetMem(Len);
 end else
 if (MemSize(Dst)<Len) then
 begin
  Dst:=ReAllocMem(Dst,Len);
 end;
 Dec(Len);
 Move(Src^,Dst^,Len);
 Dst[Len]:=#0;
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

Procedure AddPChar(var Dst:Pchar;Src:Pchar;SLen:SizeUInt);
Var
 i,Len:SizeUInt;
begin
 if (Dst=nil) then
 begin
  Dst:=GetMem(SLen+1);
  Move(Src^,Dst^,SLen);
  Dst[SLen]:=#0;
 end else
 begin
  i:=StrLen(Dst);
  Len:=i+SLen+1;
  if (MemSize(Dst)<Len) then
  begin
   Case Len of
    0..SizeOf(Pointer):
         Dst:=ReAllocMem(Dst,SizeOf(Pointer));
    else
         Dst:=ReAllocMem(Dst,Len+(Len div 2));
   end;
  end;
  Move(Src^,Dst[i],SLen);
  Dec(Len);
  Dst[Len]:=#0;
 end;
end;

Function CmpComment(buf:Pchar):Boolean; inline;
Const
 DWCMP=$3E2D2D;
begin
 Result:=(PDword(buf)^ and $FFFFFF)=DWCMP;
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

Function GetStr(data:Pchar;size:size_t):RawByteString; inline;
begin
 SetLength(Result,size);
 Move(data^,Result[1],size);
end;

Procedure GetUnescape(var Dst:Pchar;data:Pchar;size:SizeInt);
Var
 s,i,v:SizeInt;
begin
 SetPcharZero(Dst);
 if (size=0) then Exit;
 i:=0;
 s:=0;
 v:=0;
 repeat
  Case s of
   0:begin
      v:=i;
      i:=System.IndexByte(data[i],size-i,Byte('&'));
      if (i=-1) then
      begin
       i:=size;
       if (v<size) then AddPChar(Dst,@data[v],size-v);
       Exit;
      end else
      begin
       if (i>0) then AddPChar(Dst,@data[v],i);
       i:=i+v+1;
       s:=2;
       v:=i;
       //Writeln(data[i]);
      end;
     end;
   1:begin
      v:=i;
      i:=System.IndexByte(data[i],size-i,Byte(';'));
      if (i=-1) then
      begin
       Exit;
      end else
      begin
       i:=i+v+1;
       s:=0;
      end;
     end;
   2:begin
      case data[i] of
       'l':s:=3;
       'g':s:=4;
       'a':s:=5;
       'q':s:=6;
       else s:=1;
      end;
      Inc(i);
     end;
   3:begin //l
      s:=1;
      case data[i] of
       't':begin
            AddChar(Dst,'<');
           end;
      end;
      Inc(i);
     end;
   4:begin //g
      s:=1;
      case data[i] of
       't':begin
            AddChar(Dst,'>');
           end;
      end;
      Inc(i);
     end;
   5:begin //a
      case data[i] of
       'm':s:=7;
       'p':s:=8;
       else s:=1;
      end;
      Inc(i);
     end;
   6:begin //q
      case data[i] of
       'u':s:=10;
       else s:=1;
      end;
      Inc(i);
     end;
   7:begin //m
      s:=1;
      case data[i] of
       'p':begin
            AddChar(Dst,'&');
           end;
      end;
      Inc(i);
     end;
   8:begin //p
      case data[i] of
       'o':s:=9;
       else s:=1;
      end;
      Inc(i);
     end;
   9:begin //o
      s:=1;
      case data[i] of
       's':begin
            AddChar(Dst,'''');
           end;
      end;
      Inc(i);
     end;
  10:begin //u
      case data[i] of
       'o':s:=11;
       else s:=1;
      end;
      Inc(i);
     end;
  11:begin //o
      s:=1;
      case data[i] of
       't':begin
            AddChar(Dst,'"');
           end;
      end;
      Inc(i);
     end;

  end;
 until (i>=size);
end;

Destructor TXmlTextReader.Destroy;
begin
 FreeMem(LN);
 FreeMem(LV);
 inherited;
end;

Function TXmlTextReader.Name:RawByteString;
begin
 Result:=LN;
 SetCodePage(Result,SrcCP,false);
end;

Function TXmlTextReader.Value:RawByteString;
begin
 Result:=LV;
 SetCodePage(Result,SrcCP,false);
end;

Procedure _Trim(Var P:PChar;Var Len:SizeInt); inline;
begin
 while (Len>0) and (P[Len-1]<=' ') do Dec(Len);
 while (Len>0) and (P^<=' ') do
 begin
  P:=@P[1];
  Dec(Len);
 end;
end;

Procedure TXmlTextReader.OnData(P:PAnsiChar;Len:SizeInt);
begin
 _Trim(P,Len);
 if Len=0 then Exit;

 if not IsComment then
 begin
  FNodeType:=ntText;
  GetUnescape(LV,P,Len);
  OnEvent;
 end;

end;

Procedure TXmlTextReader.OnElementName;
begin
 inherited;
 SetPchar(LN,N);
 Case F of
  '/':FNodeType:=ntEndElement;
  '?':FNodeType:=ntXmlDeclaration;
  else
   if (L='/') then
   begin
    FNodeType:=ntElement;
    OnEvent;
    FNodeType:=ntEndElement;
   end else
   begin
    FNodeType:=ntElement;
   end;
 end;
 OnEvent;
end;

Procedure TXmlTextReader.OnEvent;
begin
 if Assigned(Event) then Event(Self);
end;

//--------------------

Function TXmlAttrTextReader.Name:RawByteString;
begin
 Result:='';
 Case FNodeType of
  ntAttribute,
  ntElement,
  ntEndElement,
  ntXmlDeclaration:Result:=N;
 end;
 SetCodePage(Result,SrcCP,false);
end;

Function TXmlAttrTextReader.Value:RawByteString;
begin
 Result:='';
 Case FNodeType of
  ntComment,
  ntText     :Result:=LV;
  ntAttribute:Result:=V;
 end;
 SetCodePage(Result,SrcCP,false);
end;

Procedure TXmlAttrTextReader.OnData(P:PAnsiChar;Len:SizeInt);
begin
 _Trim(P,Len);
 if Len=0 then Exit;

 if IsComment then
 begin
  FNodeType:=ntComment;
 end else
 begin
  FNodeType:=ntText;
 end;

 GetUnescape(LV,P,Len);

 OnEvent;
end;

Procedure TXmlAttrTextReader.OnEndElement;
begin
 inherited;
 if (F<>'/') and (L='/') then
 begin
  FNodeType:=ntEndElement;
  OnEvent;
 end;
end;

Procedure TXmlAttrTextReader.OnAttr;
begin
 FNodeType:=ntAttribute;
 OnEvent;
end;

Procedure TXmlAttrTextReader.OnElementName;
begin
 Case F of
  '/':FNodeType:=ntEndElement;
  '?':FNodeType:=ntXmlDeclaration;
  else
      FNodeType:=ntElement;
 end;
 OnEvent;
end;

Procedure TXmlAttrTextReader.OnEvent;
begin
 if Assigned(Event) then Event(Self);
end;

Destructor TXmlAttrTextReader.Destroy;
begin
 FreeMem(LV);
 inherited;
end;

//----------------

Destructor TXmlAttrParser.Destroy;
begin
 FreeMem(N);
 FreeMem(V);
 inherited;
end;

function GetLastChar(Dst:Pchar):Char;
Var
 Len:SizeUInt;
begin
 Result:=#0;
 if Dst=nil then Exit;
 Len:=StrLen(Dst);
 if (Len>0) then
 Case Dst[Len-1] of
  '!','?','/':
  begin
   Result:=Dst[Len-1];
   Dst[Len-1]:=#0;
  end;
 end;
end;

function GetFirstChar(Dst:Pchar):Char;
begin
 Result:=#0;
 if Dst=nil then Exit;
 Case Dst[0] of
  '!','?','/':
  begin
   Result:=Dst[0];
   Move(Dst[1],Dst[0],StrLen(Dst));
  end;
 end;
end;

Procedure TXmlAttrParser.OnEndElement;
begin

 Case PState of
  5,
  1:L:=GetLastChar(N);
  3:L:=GetLastChar(V);
 end;

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

Procedure TXmlAttrParser.OnAttr;
begin
end;

Procedure TXmlAttrParser.OnElementName;
begin
end;

//0 5      4 1   2 3      4
//?element   Name="value"

Procedure TXmlAttrParser.OnElement(C:AnsiChar);
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

//

Destructor TXmlSimpleParser.Destroy;
begin
 FreeMem(N);
 inherited;
end;

Procedure TXmlSimpleParser.OnElement(C:AnsiChar);
begin
 AddChar(N,C);
end;

Procedure TXmlSimpleParser.OnEndElement;
begin
 F:=GetFirstChar(N);
 L:=GetLastChar(N);

 OnElementName;

 SetPcharZero(N);
end;

Procedure  TXmlSimpleParser.OnElementName;
begin

end;

//

Procedure TXmlTagParser.OnEndElement;
begin
end;

Procedure TXmlTagParser.OnElement(C:AnsiChar);
begin
end;

Procedure TXmlTagParser.OnData(P:PAnsiChar;Len:SizeInt);
begin

end;

function TXmlTagParser.IsComment:Boolean; inline;
begin
 Result:=(FState=02);
end;

function TXmlTagParser.IsSingleQuoted:Boolean; inline;
begin
 Result:=(FState=12);
end;

function TXmlTagParser.IsDoubleQuoted:Boolean; inline;
begin
 Result:=(FState=13);
end;

function TXmlTagParser.IsQuoted:Boolean; inline;
begin
 Result:=IsSingleQuoted or IsDoubleQuoted;
end;

Procedure TXmlTagParser.SkipElement; inline;
begin
 EState:=40;
end;

Procedure TXmlTagParser.Abort; inline;
begin
 FState:=05;
end;

Procedure TXmlTagParser.Parse(P:PAnsiChar;Len:SizeInt);
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
         Pos:=i;
         OnEndElement;
         v:=i+1;
        end;
        '''':begin
              FState:=12;
              Pos:=i;
              OnElement(P[i]);
             end;
        '"':begin
              FState:=13;
              Pos:=i;
              OnElement(P[i]);
            end;
        '!':EState:=14;
        else
         Pos:=i;
         OnElement(P[i]);
       end;
      end;

   12:begin
       Pos:=i;
       if EState<>40 then OnElement(P[i]);
       Case P[i] of //tag qote1 close
        '''':FState:=11;
       end;
      end;
   13:begin
       Pos:=i;
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
             Pos:=i;
             OnElement('!');
            end;
           end;
        15:Case P[i] of //open tag comment2
            '-': FState:=02;
            else
            begin
             EState:=00;
             Pos:=i;
             OnElement('!');
             OnElement('-');
            end;
           end;

       end;

       Case EState of
        14,15:;//nothing
        else
        begin
         Case P[i] of //opened tag //
          '>':        //close tag
          begin
           FState:=00;
           EState:=00;
           Pos:=i;
           OnEndElement;
           v:=i+1;
          end;
          '''':begin
                Pos:=i;
                FState:=12;
                if EState<>40 then OnElement(P[i]);
               end;
          '"':begin
                Pos:=i;
                FState:=13;
                if EState<>40 then OnElement(P[i]);
              end;
          else
          begin
           Pos:=i;
           if EState<>40 then OnElement(P[i]);
          end;
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
         Pos:=i;
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
         Pos:=i;
         if (i-3>v) then OnData(@P[v],(i-3)-v);
         i:=i-1;
         FState:=00;
         EState:=00;
        end;
       end;

    05:Exit;
  end;

  Inc(i);

 end;

 if (i>v) then
  Case FState of
   00,02,03,04:
   begin
    Pos:=i;
    OnData(@P[v],i-v);
   end;
  end;

end;

//--------------------------------


function TNodeReader.CData:Pointer; inline;
begin
 if (Root<>nil) then
  Result:=Root^.CData
 else
  Result:=nil;
end;

Procedure TNodeReader.Reset;
begin
 While (Root<>nil) and (Root^.Prev<>nil) do Pop;
 UNode:=0;
end;

Procedure TNodeReader.Push(C:TClassNode;D:Pointer);
Var
 New:PReadNode;
begin
 if Root=nil then
 begin
  New:=AllocMem(SizeOf(TReadNode));
  Root:=New;
 end else
 if Root^.Next<>nil then
 begin
  New:=Root^.Next;
  Root:=New;
 end else
 begin
  New:=AllocMem(SizeOf(TReadNode));

  Root^.Next:=New;
  New^.Prev:=Root;
  Root:=New;
 end;

 New^.CNode:=C;
 New^.CData:=D;
end;

Procedure TNodeReader.Pop;
begin
 if Root<>nil then
 if Root^.Prev<>nil then
 begin
  {$IFOPT D+}
  Root^.CName:='';
  {$ENDIF}
  Root:=Root^.Prev;
 end;
end;

Destructor TNodeReader.Destroy;
Var
 Old:PReadNode;
begin
 Reset;
 While (Root<>nil) do
 begin
  {$IFOPT D+}
  Root^.CName:='';
  {$ENDIF}
  Old:=Root;
  Root:=Root^.Next;
  FreeMem(Old);
 end;
end;

class procedure TNodeFunc.OPN(Node:TNodeReader;Const Name:RawByteString);
begin
end;

class procedure TNodeFunc.CLS(Node:TNodeReader;Const Name:RawByteString);
begin
 {$IFOPT D+}
 if Node.Root^.CName<>Name then
 begin
  raise Exception.Create('Unexpected close tag:'+Name+' for '+Node.Root^.CName);
 end;
 {$ENDIF}
 Node.Pop;
end;

class procedure TNodeFunc.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
end;

class procedure TNodeFunc.ATR(Node:TNodeReader;Const Name,Value:RawByteString);
begin
end;

Procedure TNodeReader.DoSet(nType:TXMLNodeType;Const Name,Value:RawByteString);
Var
 Old:PReadNode;
begin
 Case nType of
  ntElement   :
  begin
   LastName:=Name;
   if (UNode=0) then
   begin
    Old:=Root;
    Root^.CNode.OPN(Self,Name);
    if Root<>Old then
    begin
     {$IFOPT D+}
     Root^.CName:=Name;
     {$ENDIF}
    end else
    begin
     Inc(UNode);
    end;
   end else Inc(UNode);
  end;
  ntText      :
   Case UNode of
    0:begin
       Root^.CNode.TXT(Self,'',Value);
      end;
    1:begin
       Root^.CNode.TXT(Self,LastName,Value);
      end;
   end;
  ntAttribute:
  if (UNode=0) then
  begin
   Root^.CNode.ATR(Self,Name,Value);
  end;
  ntEndElement:
  begin
   if (UNode=0) then
   begin
    Root^.CNode.CLS(Self,Name);
    //Pop(Root);
   end else
   begin
    Dec(UNode);
   end;
  end;
 end;
end;

end.

