unit data_xml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gset, xml_parse, DbcScript;

type
 TLoadSQL_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadStr_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadList_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadPerc_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadDWORD_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadInt64_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

 TLoadDouble_Func=class(TNodeFunc)
  class procedure TXT(Node:TNodeReader;Const Name,Value:RawByteString); override;
 end;

function RegisterXMLNode(Const N:RawByteString;C:TClassNode;D:Pointer):Boolean;
function LoadXML(const F:RawByteString):Boolean;

implementation

type
 TRootNode=record
  N:RawByteString;
  C:TClassNode;
  D:Pointer;
 end;

 TRootNodeCompare=class
  class function c(var a,b:TRootNode):boolean; static;
 end;

 TRootNodeSet=specialize TSet<TRootNode,TRootNodeCompare>;

class function TRootNodeCompare.c(var a,b:TRootNode):boolean;
begin
 Result:=CompareStr(a.N,b.N)<0;
end;

Var
 RootNodeSet:TRootNodeSet=nil;

function RegisterXMLNode(Const N:RawByteString;C:TClassNode;D:Pointer):Boolean;
Var
 Node:TRootNode;
begin
 Result:=True;
 if (RootNodeSet=nil) then
 begin
  RootNodeSet:=TRootNodeSet.Create;
 end;
 Node.N:=N;
 Node.C:=C;
 Node.D:=D;
 if (RootNodeSet.NFind(Node)<>nil) then Exit(False);
 RootNodeSet.Insert(Node);
end;

type
 TXmlNodeReader=class(TNodeReader)
  procedure onDataEvent(Sender:TObject);
 end;

 TRoot_Func=class(TNodeFunc)
  class procedure OPN(Node:TNodeReader;Const Name:RawByteString); override;
 end;

procedure TXmlNodeReader.onDataEvent(Sender:TObject);
begin
 With TXmlTextReader(Sender) do
 begin
  DoSet(nodeType,Name,Value);
 end;
end;

class procedure TRoot_Func.OPN(Node:TNodeReader;Const Name:RawByteString);
Var
 rNode:TRootNode;
 pNode:TRootNodeSet.PNode;
begin
 if (RootNodeSet=nil) then Exit;
 rNode:=Default(TRootNode);
 rNode.N:=Name;
 pNode:=RootNodeSet.NFind(rNode);
 if (pNode=nil) then Exit;
 rNode:=pNode^.Data;
 Node.Push(rNode.C,rNode.D);
end;

function LoadXML(const F:RawByteString):Boolean;
Var
 M:TMemoryStream;
 XmlReader:TXmlTextReader;
 NodeReader:TXmlNodeReader;
begin
 Result:=False;
 if not FileExists(F) then Exit;
 M:=TMemoryStream.Create;
 M.LoadFromFile(F);
 M.Position:=0;

 XmlReader:=TXmlTextReader.Create;
 NodeReader:=TXmlNodeReader.Create;

 NodeReader.Push(TRoot_Func,nil);

 XmlReader.SrcCP:=CP_UTF8;
 XmlReader.Event:=@NodeReader.onDataEvent;
 XmlReader.Parse(M.Memory,M.Size);

 XmlReader.Free;
 NodeReader.Free;
 Result:=True;
end;

type
 TPCharStream=class(TCustomMemoryStream)
  public
   constructor Create(P:PChar;len:SizeUint); virtual; overload;
   procedure   SetNew(P:PChar;len:SizeUint);
 end;

constructor TPCharStream.Create(P:PChar;len:SizeUint);
begin
 inherited Create;
 SetPointer(P,len);
end;

procedure TPCharStream.SetNew(P:PChar;len:SizeUint);
begin
 SetPosition(0);
 SetPointer(P,len);
end;

class procedure TLoadSQL_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
Var
 FMem:TPCharStream;
begin
 Case Name of
  '':
  begin
   FMem:=TPCharStream.Create(PChar(Value),Length(Value));
   PSQLScript(Node.CData)^:=Default(TSQLScript);
   PSQLScript(Node.CData)^.Parse(FMem);
   FMem.Free;
  end;
 end;
end;

class procedure TLoadStr_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   PRawByteString(Node.CData)^:=Trim(Value);
  end;
 end;
end;

Procedure LoadStringList(Var S:TStringList;Const Value:RawByteString);
var
 M:TPCharStream;
 i:SizeInt;
begin
 if S<>nil then
 begin
  S.Clear;
 end else
 begin
  S:=TStringList.Create;
 end;
 M:=TPCharStream.Create(PChar(Value),Length(Value));
 S.LoadFromStream(M,True);
 M.Free;

 if S.Count<>0 then
 For i:=0 to S.Count-1 do
 begin
  S.Strings[i]:=Trim(S.Strings[i]);
 end;
end;

class procedure TLoadList_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
type
 PStringList=^TStringList;
begin
 Case Name of
  '':
  begin
   LoadStringList(PStringList(Node.CData)^,Value);
  end;
 end;
end;

class procedure TLoadPerc_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   PByte(Node.CData)^:=StrToQWORDDef(Value,0);
   if PByte(Node.CData)^>100 then PByte(Node.CData)^:=100;
  end;
 end;
end;

class procedure TLoadDWORD_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   PDWORD(Node.CData)^:=StrToDWORDDef(Value,0);
  end;
 end;
end;

class procedure TLoadInt64_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   PInt64(Node.CData)^:=StrToInt64Def(Value,0);
  end;
 end;
end;

class procedure TLoadDouble_Func.TXT(Node:TNodeReader;Const Name,Value:RawByteString);
begin
 Case Name of
  '':
  begin
   PDouble(Node.CData)^:=StrToFloatDef(Value,0);
  end;
 end;
end;

end.

