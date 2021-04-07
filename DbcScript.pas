{ Dbc script similar transact-sql

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

unit DbcScript;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 dateutils,
 Classes,
 gset,
 gFastStack,
 FmtBCD,
 ZSysUtils,
 ZClasses,
 ZDbcIntfs,
 ZCompatibility,
 ZEncoding,
 ZVariant,
 ZFastCode,
 ZTokenizer,
 ZGenericSqlToken,
 UAsyncResultSet,
 DbcScriptUtils,
 DbcScriptExp,
 DbcEngine;

type
 TSQLCommandType=(
   qcIf,
   qcGoto,
   qcBeginTran,
   qcCommitTran,
   qcCommitWork,
   qcRollbackTran,
   qcRollbackWork,
   qcSetVar,
   qcPrint,
   qcStatment,
   qcDeclareVar,
   qcDeclareCursor,
   qcDeleteCursor,
   qcOpenCursor,
   qcCloseCursor,
   qcFetchNext,
   qcFetchPrior,
   qcFetchFirst,
   qcFetchLast,
   qcFetchAbs,
   qcFetchRel
  );

 //PSQLCommand=^TSQLCommand;
 TSQLCommand=record
  VType:TSQLCommandType;
  Value:RawByteString;
  SID:SizeInt;
 end;

 TParams=array of RawByteString;

 TSQLStatment=object
  FParamsID,FValuesID:SizeInt;
  FResultTp:TZResultSetType;
 end;

 TSQLExp=object
  OpID:SizeInt;
  Node:PExpNode;
 end;

 TSQLBatchCmd=object
  FCount:SizeUint;
  FCommands:array of TSQLCommand;
 end;

 TSQLVariables=class(TStringList)
  protected
   Procedure  SetValue(Const FName:RawByteString;Const V:TZVariant);
   function   GetValue(Const FName:RawByteString):TZVariant;
   function   GetMutable(Const FName:RawByteString):PZVariant;
  public
   destructor Destroy; override;
   procedure  Clear; override;
   function   GetRawByteString(Const FName:RawByteString):RawByteString;
   function   GetAsBoolean (Const FName:RawByteString):Boolean;
   function   GetAsBytes   (Const FName:RawByteString):TBytes;
   function   GetAsInteger (Const FName:RawByteString):Int64;
   function   GetAsUInteger(Const FName:RawByteString):UInt64;
   function   GetAsDouble  (Const FName:RawByteString):Double;
   function   GetAsCurrency(Const FName:RawByteString):Currency;
   function   GetAsDateTime(Const FName:RawByteString):TDateTime;
   Procedure  SetRawByteString(Const FName,FValue:RawByteString);
   Procedure  SetAsDateTime(Const FName:RawByteString;FValue:TDateTime);
   Procedure  SetAsNull(Const FName:RawByteString);
   Procedure  SetInt64(Const FName:RawByteString;FValue:Int64);
   property   ZValues[const FName:RawByteString]:TZVariant read GetValue write SetValue;
 end;

 PZCursor=^TZCursor;
 TZCursor=record
  FName:RawByteString;
  FTag:SizeInt;
  FOpen:Boolean;
  VInterface:IZInterface;
 end;

 TZCursorCompare=class
  class function c(var a,b:TZCursor):boolean; static;
 end;

 _TZCursorSet=specialize TSet<TZCursor,TZCursorCompare>;

 TZCursorSet=class(_TZCursorSet)
  procedure DeclareStatement(Const Name:RawByteString;Tag:SizeInt;stmt:IZPreparedStatement);
  Function  GetCursor(Const Name:RawByteString):PZCursor;
  Procedure DelCursor(Const Name:RawByteString);
 end;

 PSQLContext=^TSQLContext;
 TSQLContext=object
  protected
   FCursors:TZCursorSet;
  public
   FGlobal:IZResultSet;
   FVariables:TSQLVariables;
   FConnection:IZConnection;
   FGlobalCache:TZPreparedCacheSet;

   Function   GetCursor(Const Name:RawByteString):PZCursor;
   Procedure  DelCursor(Const Name:RawByteString);
   Procedure  ClearCursors;
   Procedure  DeclareStatement(Const Name,FSQL:RawByteString;Tag:SizeInt;ResultTp:TZResultSetType);

   procedure  SetParams2stmt(Const FParams:TParams;Fstmt:IZPreparedStatement);
   procedure  SetValues2ctx(Const FValues:TParams;R:IZResultSet;withi:Longint=0);
   function   GetStatment(Const FValues:TParams):RawByteString;

   Function   PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;

   Procedure  Clear;
 end;

 TOnPrintCb=Procedure(Const S:RawByteString);

 PSQLScript=^TSQLScript;

 TSQLScriptIterator=object
  private
   Script:PSQLScript;
   Context:PSQLContext;
   b,c,i:Longint;
   FCmd:TSQLCommand;
   FTRANCOUNT:SizeUint;
   FSaveAutoCommit:Boolean;
   Procedure OpenCursor;
   Procedure CloseCursor;
   Procedure FetchCursor;
   Procedure FetchCursorNum;
   Procedure DeclareCursor;
   Procedure ExecuteCmd;
   procedure BeginTran;
   procedure CommitTran;
   procedure RollbackTran;
   procedure CommitWork;
   procedure RollbackWork;
   procedure SetVar;
   function  GetQuery(VSID:Int64):IZResultSet;
   procedure PrintExp;
   procedure GotoId;
   procedure OnIf;
  public
   FOnPrintCb:TOnPrintCb;
   function  Next:Boolean;
   procedure Close(isCommit:Boolean);
 end;

 TGotoBackward=array of SizeInt;

 TSQLScript=object
  private
   FStatmentList:array of TSQLStatment;
   FBatchList:array of TSQLBatchCmd;
   FParamsList:array of TParams;
   FExps:array of TSQLExp;
   function  TryGetParams(i:SizeInt;var R:TParams):Boolean;
   function  TryGetStatment(i:SizeInt;var R:TSQLStatment):Boolean;
   function  AddParams(Const _Params:TParams):SizeInt;
   function  AddStatment(Const FParams,FValues:TParams;ResultTp:TZResultSetType):SizeInt;
   procedure AddBath(LastCount:SizeUint);
   procedure TrimBath;
   procedure AddCmd(Const _Value:RawByteString;_Type:TSQLCommandType;_SID:SizeInt);
   function  GetLastCmdId:Sizeint;
   function  TrySetCmdOp(i,o:SizeInt):Boolean;
   function  TryGoto(b,i:SizeInt):Boolean;
   function  AddExp(OpID:SizeInt;Node:PExpNode):SizeInt;
   function  TryGetExp(i:SizeInt;var R:TSQLExp):Boolean;
   function  TrySetExpOp(i,o:SizeInt):Boolean;
   function  TryGetExpOp(i:SizeInt;var R:SizeInt):Boolean;
   procedure SetGotoIds(id:SizeInt;Back:TGotoBackward);
  public
   Procedure Parse(Stream:TStream);
   function  Excecute(var Context:TSQLContext):TSQLScriptIterator;
 end;

 TDbcStatementScript=class(TDbcQueryTask)
  protected
   FRZ:TZResultSet;
   FParams:TSQLVariables;
   FScript:TSQLScript;
   Procedure   OnQuery;
   function    GetResultSet:TZResultSet;
   Procedure   SetResultSet(R:TZResultSet);
   function    GetParams:TSQLVariables;
   Procedure   SetParams(R:TSQLVariables);
  public
   Constructor Create;  override;
   Procedure   Cleanup; override;
   Procedure   SetSctipt(Const FNew:TSQLScript);
   Procedure   ExecuteScript;
   Procedure   ExecuteScript(Stream:TStream);
   property    ResultSet:TZResultSet read GetResultSet write SetResultSet;
   property    Params:TSQLVariables read GetParams write SetParams;
 end;

Const
 vtCursor=vtInterface;
 FirstDbcIndex=ZDbcIntfs.FirstDbcIndex;
 InvalidDbcIndex=ZDbcIntfs.InvalidDbcIndex;

implementation

class function TZCursorCompare.c(var a,b:TZCursor):boolean;
begin
 Result:=CompareStr(a.FName,b.FName)<0;
end;

procedure TZCursorSet.DeclareStatement(Const Name:RawByteString;Tag:SizeInt;stmt:IZPreparedStatement);
Var
 Z:TZCursor;
 FNode:TZCursorSet.PNode;
begin
 Z.FName:=Name;
 FNode:=NFind(Z);
 if Assigned(FNode) then
 begin
  FNode^.Data.FTag  :=Tag;
  FNode^.Data.FOpen :=false;
  FNode^.Data.VInterface:=stmt;
 end else
 begin
  Z.FTag  :=Tag;
  Z.FOpen :=false;
  Z.VInterface:=stmt;
  Insert(Z);
 end;
end;

Function  TZCursorSet.GetCursor(Const Name:RawByteString):PZCursor;
Var
 Z:TZCursor;
 FNode:TZCursorSet.PNode;
begin
 Result:=nil;
 Z.FName:=Name;
 FNode:=NFind(Z);
 if Assigned(FNode) then
 begin
  Result:=@FNode^.Data;
 end;
end;

Procedure TZCursorSet.DelCursor(Const Name:RawByteString);
Var
 Z:TZCursor;
begin
 Z.FName:=Name;
 Delete(Z);
end;

Procedure DoPrint(Const S:RawByteString);
begin
 //Writeln(S);
end;

Procedure TDbcStatementScript.OnQuery;
Var
 FContext:TSQLContext;
 FGlobal:IZResultSet;
 I:TSQLScriptIterator;
begin
 FContext:=Default(TSQLContext);
 FContext.FVariables:=FParams;
 FContext.FConnection:=ZConnection;

 if Assigned(FHandle) then
  if Assigned(FHandle.DbcConnection) then
   FContext.FGlobalCache:=FHandle.DbcConnection.GetPreparedCache;

 I:=FScript.Excecute(FContext);
 I.FOnPrintCb:=@DoPrint;
 While (I.Next) do
 begin
  if FHandle.isCancel then
  begin
   FContext.FGlobal:=nil;
   I.Close(false);
   Break;
  end;
 end;
 I.Close(true);

 FGlobal:=FContext.FGlobal;
 FContext.ClearCursors;

 FreeAndNil(FRZ);
 if Assigned(FGlobal) then
 begin
  FRZ:=TZAsyncResultSet.Create(FGlobal);
  While TZAsyncResultSet(FRZ).Fetch(FGlobal) and (not FHandle.isCancel) do;
 end;
end;

function TDbcStatementScript.GetResultSet:TZResultSet;
begin
 Result:=nil;
 if FHandle.State then Exit;
 Result:=FRZ;
end;

Procedure TDbcStatementScript.SetResultSet(R:TZResultSet);
begin
 if FHandle.State then Exit;
 FRZ:=R;
end;

function  TDbcStatementScript.GetParams:TSQLVariables;
begin
 Result:=nil;
 if FHandle.State then Exit;
 Result:=FParams;
end;

Procedure TDbcStatementScript.SetParams(R:TSQLVariables);
begin
 if FHandle.State then Exit;
 FParams:=R;
end;

Constructor TDbcStatementScript.Create;
begin
 inherited;
 FParams:=TSQLVariables.Create;
end;

Procedure TDbcStatementScript.Cleanup;
begin
 FreeAndNil(FParams);
 FreeAndNil(FRZ);
 inherited;
end;

Procedure TDbcStatementScript.SetSctipt(Const FNew:TSQLScript);
begin
 if FHandle.State then Exit;
 FScript:=FNew;
end;

Procedure TDbcStatementScript.ExecuteScript(Stream:TStream);
begin
 if FHandle.State then Exit;
 FScript.Parse(Stream);
 FHandle.OnDbcProc:=@OnQuery;
end;

Procedure TDbcStatementScript.ExecuteScript;
begin
 FHandle.OnDbcProc:=@OnQuery;
end;

///

Procedure TSQLVariables.SetValue(Const FName:RawByteString;Const V:TZVariant);
Var
 i:Longint;
 P:PZVariant;
begin
 if (Self=nil) then Exit;
 i:=IndexOf(FName);
 if (i=-1) then
 begin
  P:=AllocMem(SizeOf(TZVariant));
  P^:=V;
  AddObject(FName,TObject(P));
 end else
 begin
  P:=Pointer(Objects[i]);
  P^:=V;
 end;
end;

function TSQLVariables.GetValue(Const FName:RawByteString):TZVariant;
Var
 i:Longint;
 P:PZVariant;
begin
 Result:=Default(TZVariant);
 if (Self=nil) then Exit;
 i:=IndexOf(FName);
 if (i=-1) then Exit;
 P:=Pointer(Objects[i]);
 Result:=P^;
end;

function TSQLVariables.GetMutable(Const FName:RawByteString):PZVariant;
Var
 i:Longint;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 i:=IndexOf(FName);
 if (i=-1) then Exit;
 Result:=Pointer(Objects[i]);
end;

////

function TSQLVariables.GetRawByteString(Const FName:RawByteString):RawByteString;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsRawByteString(V);
end;

function TSQLVariables.GetAsBoolean(Const FName:RawByteString):Boolean;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsBoolean(V);
end;

function TSQLVariables.GetAsBytes(Const FName:RawByteString):TBytes;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsBytes(V);
end;

function TSQLVariables.GetAsInteger(Const FName:RawByteString):Int64;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsInteger(V);
end;

function TSQLVariables.GetAsUInteger(Const FName:RawByteString):UInt64;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsUInteger(V);
end;

function TSQLVariables.GetAsDouble(Const FName:RawByteString):Double;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsDouble(V);
end;

function TSQLVariables.GetAsCurrency(Const FName:RawByteString):Currency;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsCurrency(V);
end;

function TSQLVariables.GetAsDateTime(Const FName:RawByteString):TDateTime;
var
 V:TZVariant;
begin
 V:=GetValue(FName);
 Result:=_GetAsDateTime(V);
end;

//

Procedure TSQLVariables.SetRawByteString(Const FName,FValue:RawByteString);
begin
 SetValue(FName,EncodeRawByteString(FValue));
end;

Procedure TSQLVariables.SetAsDateTime(Const FName:RawByteString;FValue:TDateTime);
begin
 SetValue(FName,EncodeDateTime(FValue));
end;

Procedure TSQLVariables.SetAsNull(Const FName:RawByteString);
begin
 SetValue(FName,EncodeNull);
end;

Procedure TSQLVariables.SetInt64(Const FName:RawByteString;FValue:Int64);
begin
 SetValue(FName,EncodeInteger(FValue));
end;

procedure TSQLVariables.Clear;
Var
 i:Longint;
 P:PZVariant;
begin
 if (Self<>nil) and (Count<>0) then
 For i:=0 to Count-1 do
 begin
  P:=Pointer(Objects[i]);
  if Assigned(P) then
  begin
   Finalize(P^);
   FreeMem(P);
  end;
 end;
 inherited;
end;

destructor TSQLVariables.Destroy;
begin
 Clear;
 inherited;
end;

Procedure TSQLContext.ClearCursors;
begin
 FGlobal:=nil;
 FreeAndNil(FCursors);
end;

Procedure TSQLContext.DeclareStatement(Const Name,FSQL:RawByteString;Tag:SizeInt;ResultTp:TZResultSetType);
var
 stmt:IZPreparedStatement;
begin
 stmt:=FConnection.PrepareStatement(FSQL);
 stmt.SetResultSetConcurrency(rcReadOnly);
 stmt.SetResultSetType(ResultTp);

 if (ResultTp=rtForwardOnly) then
  stmt.SetFetchDirection(fdForward)
 else
  stmt.SetFetchDirection(fdUnknown);

 if not Assigned(FCursors) then
 begin
  FCursors:=TZCursorSet.Create;
 end;
 FCursors.DeclareStatement(Name,Tag,stmt);
end;

Function  TSQLContext.GetCursor(Const Name:RawByteString):PZCursor;
begin
 Result:=nil;
 if Assigned(FCursors) then Result:=FCursors.GetCursor(Name);
end;

Procedure TSQLContext.DelCursor(Const Name:RawByteString);
begin
 if Assigned(FCursors) then FCursors.DelCursor(Name);
end;

Procedure TSQLContext.Clear;
begin
 if Assigned(FVariables) then
 begin
  FVariables.Clear;
  FreeAndNil(FVariables);
 end;
 ClearCursors;
end;

type
 TQueryState=object
  Data:RawByteString;
  DataLen:SizeUint;
  FParams,FValues:TParams;
  Fbrackets:SizeUint;
  Procedure AddChar(Ch:AnsiChar);
  Procedure AddToken(Const _Token:TZToken);
  Procedure AddParam(Const _Name:RawByteString);
  Procedure AddValue(isVar:Boolean;Const _Name:RawByteString);
  Procedure Add4String(TrimR:Boolean); inline;
  Procedure Add2Param; inline;
  function  IsSpace:Boolean; inline;
  procedure Clean;
  procedure Finish;
  procedure Parse(Const FToken:TZToken);
 end;

 PSQLParseContext=^TSQLParseContext;

 TDeclareState=object
  Context:PSQLParseContext;
  FName:RawByteString;
  FType:TZVariantType;
  FResultTp:TZResultSetType;
  procedure Clean;
  procedure AddDeclare; inline;
  procedure AddCursor; inline;
  Function  Finish:Boolean;
  Function  Parse(Const FToken:TZToken):Boolean;
 end;

 TFetchState=object
  Context:PSQLParseContext;
  FName:RawByteString;
  FLastCmd:TSQLCommandType;
  procedure Clean;
  Function  Finish:Boolean;
  Function  Parse(Const FToken:TZToken):Boolean;
 end;

 {TBlockState=record
  _type,
  VSID:SizeInt;
 end;}

 TBlockType=(btBegin,btIf1,btIf2,btElse,btWhile);

 PBlockState=^TBlockState;
 TBlockState=object
  _type:TBlockType;
  FBeginId:SizeInt;
  GotoBackward:TGotoBackward;
  Procedure ClearGotoId; inline;
  Procedure AddGotoId(id:SizeInt);
 end;

 TBlockStack=specialize TFastStack<TBlockState>;

 {IF () ->1         >-v
 BEGIN ->0            |
 END   <-0 = 1 :=2    |
                      | >-v GOTO END
 ELSE  :=3            |   |
                    --<   |
 BEGIN ->0                |
 END   <-0 = 3          --<

 WHILE ->4      >-v
 BEGIN ->0        |
 END   <-0 = 4    |  GOTO WHILE
                --<
 }


 TSQLParseCb=procedure of object;
 TSQLParseContext=object
  Script:PSQLScript;

  FTokenizer:TZTokenizer;

  FExpressionState:TExpressionState;

  FQueryState:TQueryState;

  FDeclareState:TDeclareState;

  FFetchState:TFetchState;

  FBlockStack:TBlockStack;
  //FBlockStack:array of TBlockState;

  FOpId:SizeInt;
  FParamName:RawByteString;
  FToken:TZToken;

  State:SizeUint;

  cb:TSQLParseCb;

  function  PushBlock(_type:TBlockType;_VSID:SizeInt):PBlockState;
  function  PopBlock:Boolean; inline;
  //function  TopBlock:TBlockState;
  //procedure SetTopBlockType(o:SizeInt);

  procedure AddCmd(Const FName:RawByteString;_Type:TSQLCommandType;_SID:SizeInt); inline;
  procedure AddBath(LastCount:SizeUint); inline;
  function  AddStatment(ResultTp:TZResultSetType):SizeInt;
  //Function  GetUpperCaseToken:RawByteString; inline;
  Function  GetAsUInt64(def:UInt64):UInt64; inline;
  Function  GetType:TZTokenType; inline;
  Function  isEOC(Const _Token:TZToken):Boolean;
  Procedure Reset;
  Procedure SetCb(_cb:TSQLParseCb;new:SizeUint=0); inline;
  Procedure ResetErr;
  Procedure cbError;
  Procedure cbNormal;
  Procedure cbSet;
  function  SubQueryFinish:Int64;
  Procedure cbPrint;
  Procedure cbDeclare;
  Procedure cbCursor;
  Procedure cbFetch;
  Procedure cbBegin;
  //Procedure OnEOCCmd;
  Procedure OnAfterAddCmd;
  Procedure OnBeforeAddBath;
  Procedure cbEnd;
  Procedure cbCond;
  Procedure cbBreak;
  Procedure cbContinue;
  Procedure cbElse;
  Procedure cbCommit;
  Procedure cbRollback;

  Procedure BeginParse;
  Procedure EndParse;
 end;

Procedure TBlockState.ClearGotoId; inline;
begin
 SetLength(GotoBackward,0);
end;

Procedure TBlockState.AddGotoId(id:SizeInt);
Var
 i:SizeInt;
begin
 i:=Length(GotoBackward);
 SetLength(GotoBackward,i+1);
 GotoBackward[i]:=id;
end;

Procedure TQueryState.AddChar(Ch:AnsiChar);
begin
 if DataLen=Length(Data) then
 begin
  if Length(Data)<8 then
   SetLength(Data,8)
  else
   SetLength(Data,DataLen+(DataLen shr 1));
 end;
 Inc(DataLen);
 Data[DataLen]:=Ch;
end;

Procedure TQueryState.AddToken(Const _Token:TZToken);
Var
 New,A:SizeUint;
begin
 if IsSpace and (_Token.TokenType=ttWhitespace) then Exit;
 A:=Length(Data);
 New:=DataLen+_Token.L;
 if (New>=A) then
 begin
  if A<8 then A:=8;
  While (New>A) do A:=A+(A shr 1);
  SetLength(Data,A);
 end;
 Move(_Token.P^,Data[DataLen+1],_Token.L);
 DataLen:=New;
end;

Procedure TQueryState.AddParam(Const _Name:RawByteString);
Var
 i:SizeUint;
begin
 i:=Length(FParams);
 SetLength(FParams,i+1);
 FParams[i]:=_Name;
end;

Procedure TQueryState.AddValue(isVar:Boolean;Const _Name:RawByteString);
Var
 i:SizeUint;
begin
 i:=Length(FValues);
 SetLength(FValues,i+1);
 if isVar then
 begin
  FValues[i]:=':'+_Name;
 end else
 if (_Name<>'') and (_Name[1]=':') then
 begin
  FValues[i]:=' '+_Name;
 end else
 begin
  FValues[i]:=_Name;
 end;
end;

Procedure TQueryState.Add4String(TrimR:Boolean); inline;
Var
 T:RawByteString;
begin
 if DataLen=0 then Exit;

 if Length(FValues)=0 then
 begin
  T:=TrimLeft(Copy(Data,1,DataLen));
 end else
 begin
  T:=Copy(Data,1,DataLen);
 end;

 if TrimR then
 begin
  T:=TrimRight(T);
 end;

 AddValue(false,T);
 DataLen:=0;
end;

Procedure TQueryState.Add2Param; inline;
begin
 AddParam(Trim(Copy(Data,1,DataLen)));
 DataLen:=0;
end;

function TQueryState.IsSpace:Boolean; inline;
begin
 Result:=(DataLen=0) and (Length(FValues)=0);
end;

procedure TQueryState.Clean;
begin
 DataLen:=0;
 SetLength(FParams,0);
 SetLength(FValues,0);
 Fbrackets:=0;
end;

procedure TQueryState.Finish;
begin
 Add4String(True);
end;

procedure TQueryState.Parse(Const FToken:TZToken);
begin
 Case FToken.TokenType of
  ttSymbol:
  begin
   AddToken(FToken);
   Case FToken.P^ of
    '(':Inc(Fbrackets);
    ')':Dec(Fbrackets);
   end;
  end;
  ttWhitespace:if (DataLen<>0) then
                AddToken(FToken);
  ttSpecial:if GetVariableType(FToken.P,FToken.L)=3 then
            begin
             Add4String(False);
             AddValue(true,DecodeVariable(FToken.P,FToken.L));
            end else
            begin
             AddChar('?');
             AddParam(DecodeVariable(FToken.P,FToken.L));
            end;
  else
             AddToken(FToken);
 end;
end;

function TSQLParseContext.PushBlock(_type:TBlockType;_VSID:SizeInt):PBlockState;
Var
 B:TBlockState;
begin
 B:=Default(TBlockState);
 B._type:=_type;
 B.FBeginId:=_VSID;
 FBlockStack.Push(B);
 Result:=FBlockStack.Top;
end;

{
procedure TSQLParseContext.PushBlock(_type,_VSID:SizeInt);
Var
 i:SizeInt;
begin
 i:=Length(FBlockStack);
 SetLength(FBlockStack,i+1);
 FBlockStack[i]._type:=_type;
 FBlockStack[i].VSID:=_VSID;
end;
}

function TSQLParseContext.PopBlock:Boolean; inline;
begin
 Result:=FBlockStack.Pop;
end;

{
procedure TSQLParseContext.PopBlock;
Var
 i:SizeInt;
begin
 i:=Length(FBlockStack);
 if (i=0) then Exit;
 Dec(i);
 SetLength(FBlockStack,i);
end;

function TSQLParseContext.TopBlock:TBlockState;
Var
 i:SizeInt;
begin
 Result._type:=-1;
 Result.VSID:=-1;
 i:=Length(FBlockStack);
 if (i=0) then Exit;
 Dec(i);
 Result:=FBlockStack[i];
end;
}

{
procedure TSQLParseContext.SetTopBlockType(o:SizeInt);
Var
 i:SizeInt;
begin
 i:=Length(FBlockStack);
 if (i=0) then Exit;
 Dec(i);
 FBlockStack[i]._type:=o;
end;
}

procedure TSQLParseContext.AddCmd(Const FName:RawByteString;_Type:TSQLCommandType;_SID:SizeInt); inline;
begin
 Script^.AddCmd(FName,_Type,_SID);
 OnAfterAddCmd;
end;

procedure TSQLParseContext.AddBath(LastCount:SizeUint); inline;
begin
 OnBeforeAddBath;
 Script^.AddBath(LastCount);
end;

function TSQLParseContext.AddStatment(ResultTp:TZResultSetType):SizeInt;
begin
 Result:=Script^.AddStatment(FQueryState.FParams,FQueryState.FValues,ResultTp);
 FQueryState.Clean;
end;

{
Function TSQLParseContext.GetUpperCaseToken:RawByteString; inline;
begin
 System.SetString(Result,FToken.P,FToken.L);
 Result:=UpperCase(Result);
end;
}

Function TSQLParseContext.GetAsUInt64(def:UInt64):UInt64; inline;
begin
 Result:=RawToUInt64Def(FToken.P,FToken.P+FToken.L,def)
end;

Function TSQLParseContext.GetType:TZTokenType; inline;
begin
 Result:=FToken.TokenType;
end;

Function TSQLParseContext.isEOC(Const _Token:TZToken):Boolean;
begin
 Result:=(_Token.TokenType=ttEOF) or
         ((_Token.TokenType=ttSymbol) and (_Token.L=1) and (_Token.P^=';'));
end;

Procedure TSQLParseContext.Reset;
begin
 FOpId:=-1;
 FParamName:='';
 cb:=@cbNormal;
 State:=0;
 FExpressionState.Clean;
 FQueryState.Clean;
 FDeclareState.Clean;
 FFetchState.Clean;
end;

Procedure TSQLParseContext.SetCb(_cb:TSQLParseCb;new:SizeUint=0); inline;
begin
 cb:=_cb;
 State:=new;
end;

Procedure TSQLParseContext.ResetErr;
begin
 cb:=@cbError;
 FExpressionState.Clean;
 FQueryState.Clean;
 FDeclareState.Clean;
 FFetchState.Clean;
 FParamName:='';
end;

Procedure TSQLParseContext.cbError;
begin
 if isEOC(FToken) then Reset;
end;

function _is_cond_break(const FToken:TZToken):Boolean; inline;
begin
 Result:=false;
 Case _get_cmd_keyword(FToken.P,FToken.L) of
  6..9,18:Result:=true;
 end;
end;

Procedure TSQLParseContext.cbNormal;

 Procedure _Finish; inline;
 begin
  FQueryState.Finish;
  if (not FQueryState.IsSpace) then
  begin
   AddCmd('',qcStatment,AddStatment(rtForwardOnly));
  end;
 end;

var
 id:SizeInt;
Label
 _initial;
begin
 Case State of
  0:begin //normal
     _initial:
     if isEOC(FToken) then
     begin //end statment
      //OnEOCCmd;
      _Finish;
     end else
     Case GetType of
      ttWord,
      ttKeyWord:begin
                 if (FQueryState.IsSpace) then
                 begin
                  {if ((id<>-1) and (id<>0) and (id<>23) and (FQueryState.DataLen<>0))
                   or (FQueryState.DataLen=0) then
                  begin
                   if (FQueryState.DataLen<>0) then
                   begin
                    //end statment
                    FQueryState.Finish;
                    AddCmd('',qcStatment,AddStatment(rtForwardOnly));
                   end;}

                  id:=_get_cmd_keyword(FToken.P,FToken.L);
                  Case id of
                    0:SetCb(@cbSet);      //'SET'
                    1:SetCb(@cbDeclare);  //'DECLARE'
                    2:SetCb(@cbCursor,0); //'DEALLOCATE'
                    3:SetCb(@cbCursor,1); //'OPEN'
                    4:SetCb(@cbCursor,2); //'CLOSE'
                    5:SetCb(@cbFetch);    //'FETCH'
                    6:State:=1;           //'GO'
                    7:State:=2;           //'USE'

                    8:SetCb(@cbBegin);    //'BEGIN'
                    9:cbEnd;              //'END'

                   10:SetCb(@cbCommit);   //'COMMIT'
                   11:SetCb(@cbRollback); //'ROLLBACK'

                   12:cbBreak;            //'BREAK'
                   13:cbContinue;         //'CONTINUE'
                   14:SetCb(@cbCond,1);   //'WHILE'
                   15:;                   //'GOTO'
                   //16:;                 //'WAITFOR'
                   17:SetCb(@cbCond,0);   //'IF'
                   18:cbElse;             //'ELSE'
                   19:;                   //'RETURN'

                   20:SetCb(@cbPrint);    //'PRINT'

                   //21:'THROW'
                   //22:'RAISERROR'
                   //23:'SELECT'
                   //24:'DELETE'
                   //25:'INSERT'
                   //26:'UPDATE'
                   //27:'EXEC'
                   //28:'EXECUTE'
                   //29:'SAVE'
                   //30:'ALTER'
                   //31:'CREATE'
                   //32:'DROP'

                   else
                    FQueryState.Parse(FToken);
                  end;
                 end else
                 if FQueryState.Fbrackets=0 then
                 begin
                  id:=_get_cmd_keyword(FToken.P,FToken.L);
                  Case id of
                   6..9,18:begin
                            _Finish;
                            Case id of
                              6:State:=1;           //'GO'
                              7:State:=2;           //'USE'
                              8:SetCb(@cbBegin);    //'BEGIN'
                              9:cbEnd;              //'END'
                             18:cbElse;             //'ELSE'
                            end;
                           end;
                   else
                    FQueryState.Parse(FToken);
                  end;
                 end else
                 begin
                  FQueryState.Parse(FToken);
                 end;
                end;
      else
      begin
       FQueryState.Parse(FToken);
      end;
     end;
    end;
  1:begin //go
     if isEOC(FToken) then
      State:=0
     else
     Case GetType of
      ttWhitespace:;
      ttInteger:begin
                 AddBath(GetAsUInt64(1));
                 State:=0;
                end;
      else
       begin
        AddBath(1);
        State:=0;
        goto _initial;
       end;
     end;
    end;
  2:begin //use
     if isEOC(FToken) then
      State:=0
     else
     Case GetType of
      ttWhitespace:;
      else
       State:=5;
     end;
    end;
  3:begin //use name
     if isEOC(FToken) then
      State:=0
     else
     Case GetType of
      ttWhitespace:;
      else
       begin
        State:=0;
        goto _initial;
       end;
     end;
    end;
 end;
end;

Procedure TSQLParseContext.cbSet;
var
 i:SizeInt;

 Procedure _Finish; inline;
 begin
  //OnEOCCmd;
  FExpressionState.Finish;
  //Writeln(FExpressionState.GetStr);
  //FExpressionState.Print;

  if State=2 then
  begin
   AddCmd(FParamName,qcSetVar,Script^.AddExp(FOpId,FExpressionState.Node));
   FExpressionState.Node:=nil;
  end;

  FExpressionState.Clean;

  FQueryState.Clean;
  FParamName:='';
  //end exp
  Reset;
 end;

begin
 if isEOC(FToken) then
 begin
  _Finish;
 end else
 if _is_cond_break(FToken) then
 begin
  _Finish;
  cbNormal;
 end else
  Case State of
   0:begin //var name
      Case GetType of
       ttWhitespace:;
       ttSpecial:
       begin
        FQueryState.Clean;
        FParamName:=DecodeVariable(FToken.P,FToken.L);
        State:=1;
       end;
       ttWord,
       ttKeyword: //need a set system prop
       begin
        FQueryState.Clean;
        FParamName:=GetWordStr(FToken.P,FToken.L);
        State:=1;
       end;
       else
        ResetErr;  //err exp
      end;
     end;
   1:begin //=
      case GetType of
       ttWhitespace:;
       ttSymbol:begin
                 i:=_get_op_id(FToken.P,FToken.L);
                 case i of
                  0..8:begin
                        FOpId:=i;
                        State:=2;
                       end;
                  else
                   ResetErr;  //err exp
                 end;
                end;
       else
        ResetErr; //err exp
      end;
     end;
   2:begin //exp
      if not FExpressionState.Parse(FToken) then ResetErr; //err exp
     end;
  end;
end;

function TSQLParseContext.SubQueryFinish:Int64;
begin
 FQueryState.Finish;
 Result:=AddStatment(rtForwardOnly);
 FQueryState.Clean;
end;

Procedure TSQLParseContext.cbPrint;

 Procedure _Finish; inline;
 begin
  //OnEOCCmd;
  FExpressionState.Finish;
  AddCmd(FParamName,qcPrint,Script^.AddExp(0,FExpressionState.Node));
  FExpressionState.Node:=nil;
  FExpressionState.Clean;
  FQueryState.Clean;
  Reset;
 end;

begin
 if isEOC(FToken) then
 begin
  _Finish;
 end else
 if _is_cond_break(FToken) then
 begin
  _Finish;
  cbNormal;
 end else
  if not FExpressionState.Parse(FToken) then
  begin
   Case FToken.TokenType of
    ttWord,
    ttKeyword:begin
               _Finish;
              end;
    else
              begin
               ResetErr; //err exp
              end;
   end;
  end;

end;

procedure TDeclareState.Clean;
begin
 FName:='';
 FType:=vtNull;
 FResultTp:=rtForwardOnly;
end;

procedure TDeclareState.AddDeclare; inline;
begin
 if FName<>'' then
 begin
  Context^.AddCmd(FName,qcDeclareVar,ord(FType));
  FName:='';
  FType:=vtNull;
 end;
 Context^.State:=0;
end;

procedure TDeclareState.AddCursor; inline;
begin
 if FName<>'' then
 begin
  Context^.AddCmd(FName,qcDeclareCursor,Context^.AddStatment(FResultTp));
 end;
end;

Function TDeclareState.Finish:Boolean;
begin
 Result:=True;
 Context^.FQueryState.Finish;
 Case Context^.State of
  1,3:AddDeclare;
  5:AddCursor;
  6:;
  else
   Result:=False;
 end;
end;

Function TDeclareState.Parse(Const FToken:TZToken):Boolean;
begin
 Result:=True;
 Case Context^.State of
  0:Case FToken.TokenType of //declare
     ttWhitespace:;
     ttSpecial:begin
                FName:=DecodeVariable(FToken.P,FToken.L); //declare variable
                Context^.State:=1;
               end;
     ttWord,
     ttKeyWord:begin
                FName:=GetWordStr(FToken.P,FToken.L); //declare cursor only
                Context^.State:=2;
               end;
     else
      Result:=False;
    end;

  1:Case FToken.TokenType of //declare @name
      ttWhitespace:;
      ttSymbol:Case FToken.P^ of
                ',':AddDeclare; //declare without type
                else Result:=False;
               end;
      ttWord,
      ttKeyWord:begin
                 Case _fetch_op1(FToken.P,FToken.L) of
                  2:begin //'CURSOR'
                     FType:=vtCursor;
                     AddDeclare;
                    end;
                  3:;//'AS'
                  else //data type
                   begin
                    FType:=GetDataType(FToken.P,FToken.L);
                    Context^.State:=3;
                   end;
                 end;
                end;
      else
       Result:=False;
     end;

  2:Case FToken.TokenType of //declare name
      ttWhitespace:;
      ttWord,
      ttKeyWord:Case _fetch_op1(FToken.P,FToken.L) of
                 0:FResultTp:=rtScrollInsensitive;//'INSENSITIVE'
                 1:FResultTp:=rtScrollSensitive;  //'SCROLL'
                 2:Context^.State:=4; //'CURSOR'
                 else
                  Result:=False;
                end;
      else
       Result:=False;
     end;

   3:Case FToken.TokenType of //declare @name type
      ttWhitespace:;
      ttSymbol:Case FToken.P^ of
                ',':AddDeclare;
                else Result:=False;
               end;
     end;

   4:Case FToken.TokenType of //declare name cursor
      ttWhitespace:;
      ttWord,
      ttKeyWord:Case _fetch_op2(FToken.P,FToken.L) of
                  0:;//'LOCAL' :;
                  1:;//'GLOBAL':;
                  2:FResultTp:=rtForwardOnly;  //'FORWARD_ONLY':
                  3:FResultTp:=rtScrollSensitive; //'SCROLL':
                  4:FResultTp:=rtScrollInsensitive; //'STATIC':
                  5:;//'KEYSET':;
                  6:;//'DYNAMIC':;
                  7:FResultTp:=rtForwardOnly; //'FAST_FORWARD':
                  8:;//'READ_ONLY':;
                  9:;//'SCROLL_LOCKS':;
                 10:;//'OPTIMISTIC':;
                 11:;//'TYPE_WARNING':;
                 12:begin //'FOR':
                     Context^.FQueryState.Clean;
                     Context^.State:=5;
                    end;
                 else
                  Result:=False;
                end;
      else
       Result:=False;
     end;

   5:Case _fetch_op2(FToken.P,FToken.L) of
      12:begin; //'FOR':
          AddCursor;
          Context^.State:=6; //after for ignored
         end;
      else
       Context^.FQueryState.Parse(FToken);
     end;

 end;
end;

Procedure TSQLParseContext.cbDeclare;

 Procedure _Finish; inline;
 begin
  //OnEOCCmd;
  FDeclareState.Finish;
  Reset;
 end;

begin
 if isEOC(FToken) then
 begin
  _Finish;
 end else
 if _is_cond_break(FToken) then
 begin
  _Finish;
  cbNormal;
 end else
 begin
  if not FDeclareState.Parse(FToken) then ResetErr;
 end;
end;

Procedure TSQLParseContext.cbCursor;

 Procedure _Finish(_Type:TSQLCommandType); inline;
 begin
  if (FParamName<>'') then
  begin
   //OnEOCCmd;
   AddCmd(FParamName,_Type,-1);
  end;
  Reset; //reset
 end;

begin
 Case State of
  0:begin //delete cursor
     if isEOC(FToken) then
     begin
      _Finish(qcDeleteCursor);
     end else
     if _is_cond_break(FToken) then
     begin
      _Finish(qcDeleteCursor);
      cbNormal;
     end else
     Case GetType of
      ttWhitespace:;
      ttSpecial,
      ttWord,
      ttKeyWord:if not _is_global_word(FToken.P,FToken.L) then
                 FParamName:=GetWordStr(FToken.P,FToken.L);
     end;
    end;
  1:begin //open cursor
     if isEOC(FToken) then
     begin
      _Finish(qcOpenCursor);
     end else
     if _is_cond_break(FToken) then
     begin
      _Finish(qcOpenCursor);
      cbNormal;
     end else
     Case GetType of
      ttWhitespace:;
      ttSpecial,
      ttWord,
      ttKeyWord:if not _is_global_word(FToken.P,FToken.L) then
                 FParamName:=GetWordStr(FToken.P,FToken.L);
     end;
    end;
  2:begin //close cursor
     if isEOC(FToken) then
     begin
      _Finish(qcCloseCursor);
     end else
     if _is_cond_break(FToken) then
     begin
      _Finish(qcCloseCursor);
      cbNormal;
     end else
     Case GetType of
      ttWhitespace:;
      ttSpecial,
      ttWord,
      ttKeyWord:if not _is_global_word(FToken.P,FToken.L) then
                 FParamName:=GetWordStr(FToken.P,FToken.L);
     end;
    end;
 end;
end;

procedure TFetchState.Clean;
begin
 FName:='';
end;

Function TFetchState.Finish:Boolean;
begin
 Result:=True;
 Case Context^.State of
  3,4:begin
       if FName='' then Exit(False);
       if (Context^.State=4) and (Context^.FParamName<>'') then
        Context^.FQueryState.AddParam(Context^.FParamName);
       Context^.FQueryState.Finish;
       Context^.AddCmd(FName,FLastCmd,Context^.Script^.AddParams(Context^.FQueryState.FParams));
       Context^.FQueryState.Clean;
      end;
  else
   Result:=False;
 end;
end;

Function TFetchState.Parse(Const FToken:TZToken):Boolean;
begin
 Result:=True;
 Case Context^.State of
  0:Case FToken.TokenType of //fetch
     ttWhitespace:;
     ttWord,
     ttKeyWord:Case _get_fetch_type(FToken.P,FToken.L) of
                0:begin//'NEXT'
                   Context^.FQueryState.Clean;
                   FLastCmd:=qcFetchNext;
                   Context^.State:=2;
                  end;
                1:begin//'PRIOR'
                   Context^.FQueryState.Clean;
                   FLastCmd:=qcFetchPrior;
                   Context^.State:=2;
                  end;
                2:begin//'FIRST'
                   Context^.FQueryState.Clean;
                   FLastCmd:=qcFetchFirst;
                   Context^.State:=2;
                  end;
                3:begin//'LAST'
                   Context^.FQueryState.Clean;
                   FLastCmd:=qcFetchLast;
                   Context^.State:=2;
                  end;
                4:begin//'ABSOLUTE'
                   Context^.FQueryState.Clean;
                   FLastCmd:=qcFetchAbs;
                   Context^.State:=1;
                  end;
                5:begin//'RELATIVE'
                   Context^.FQueryState.Clean;
                   FLastCmd:=qcFetchRel;
                   Context^.State:=1;
                  end;
                else
                 Result:=False;
               end;
     else
      Result:=False;
    end;
  1:Case FToken.TokenType of //fetch +num
     ttWhitespace:;
     ttSymbol:if (FToken.L<>1) then
              begin
               Result:=False;
              end else
              Case FToken.P^ of
               '+','-':Context^.FQueryState.AddToken(FToken);
              end;
     ttWord,
     ttKeyword,
     ttInteger:
     begin
      Context^.FQueryState.AddToken(FToken);
      Context^.FQueryState.Add2Param;
      Context^.State:=2;
     end;
     else
      Result:=False;
    end;
  2:Case FToken.TokenType of //fetch wait from
     ttWhitespace:;
     ttWord,
     ttKeyWord:if _is_from_word(FToken.P,FToken.L) then
               begin
                Context^.State:=3;
               end else
               begin
                Result:=False;
               end;
     else
      Result:=False;
    end;
  3:Case FToken.TokenType of //fetch from
     ttWhitespace:;
     ttWord,
     ttKeyWord:begin
                if _is_into_word(FToken.P,FToken.L) then
                begin
                 Context^.State:=4;
                 Context^.FParamName:='';
                end else
                 if (FName<>'') then Result:=False else FName:=GetWordStr(FToken.P,FToken.L);
               end;
     ttSpecial:if (FName<>'') then Result:=False else FName:=GetWordStr(FToken.P,FToken.L);
     else
      Result:=False;
    end;
  4:Case FToken.TokenType of //fetch from into
     ttWhitespace:;
     ttSymbol:Case FToken.P^ of
               ',':if (Context^.FParamName<>'') then
                   begin
                    Context^.FQueryState.AddParam(Context^.FParamName);
                    Context^.FParamName:='';
                   end;
               else
                Result:=False;
              end;
     ttWord,
     ttKeyWord:Context^.FParamName:=GetWordStr(FToken.P,FToken.L);
     ttSpecial:Context^.FParamName:=DecodeVariable(FToken.P,FToken.L);
     else
      Result:=False;
    end;
 end;
end;

Procedure TSQLParseContext.cbFetch;

 Procedure _Finish; inline;
 begin
  //OnEOCCmd;
  FFetchState.Finish;
  Reset;
 end;

begin
 if isEOC(FToken) then
 begin
  _Finish;
 end else
 if _is_cond_break(FToken) then
 begin
  _Finish;
  cbNormal;
 end else
 begin
  if not FFetchState.Parse(FToken) then ResetErr;
 end;
end;

Procedure TSQLParseContext.cbBegin;
begin
 Case State of
  0:begin
     if isEOC(FToken) then
     begin
      //begin block
      PushBlock(btBegin,Script^.GetLastCmdId);
      Reset;
     end else
     Case FToken.TokenType of
      ttWhitespace:;
      ttWord,
      ttKeyWord:Case _get_begin_op(FToken.P,FToken.L) of
                 0,1:begin //TRAN TRANSACTION
                      AddCmd('',qcBeginTran,-1);
                      State:=1;
                     end;
                 2:;//TRY
                 3:;//CATCH
                 else //begin block
                     begin
                      PushBlock(btBegin,Script^.GetLastCmdId);
                      Reset;
                      cbNormal;
                     end;
                end;
     end;
    end;
  1:if isEOC(FToken) then
    begin
     Reset;
    end;
 end;
end;

Procedure TSQLParseContext.OnAfterAddCmd;
var
 key:PBlockState;
 //id:SizeInt;
begin
 repeat
  key:=FBlockStack.Top;
  if key=nil then Break;
  Case key^._type of
   btIf1:begin //CMD AFTER IF
          //set if id
          Script^.SetGotoIds(Script^.GetLastCmdId+1,key^.GotoBackward);
          key^._type:=btIf2;
          {if Script^.TrySetExpOp(TopBlock.VSID,Script^.GetLastCmdId+1) then
          begin
           SetTopBlockType(2);
          end else
          begin
           ResetErr;
          end;}
          Break;
         end;
   btIf2 :PopBlock; //CMD AFTER IF BEGIN END
   btElse:begin     //CMD AFTER IF BEGIN END ELSE
           //set goto id
           Script^.SetGotoIds(Script^.GetLastCmdId+1,key^.GotoBackward);
           //if not Script^.TrySetCmdOp(TopBlock.VSID,Script^.GetLastCmdId+1) then
           // ResetErr;
           PopBlock;
          end;
   btWhile:begin
            //add goto
            Script^.AddCmd('',qcGoto,key^.FBeginId);
            //set while id
            Script^.SetGotoIds(Script^.GetLastCmdId+1,key^.GotoBackward);
            PopBlock;
            {if Script^.TryGetExpOp(TopBlock.VSID,id) then
            begin
             //add goto
             Script^.AddCmd('',qcGoto,id);
             //set while id
             Script^.TrySetExpOp(TopBlock.VSID,Script^.GetLastCmdId+1);
             PopBlock;
            end else
             ResetErr;}
            Break;
           end;
   else
     Break;
  end;
 until false;
end;

Procedure TSQLParseContext.OnBeforeAddBath;
var
 key:PBlockState;
begin
 repeat
  key:=FBlockStack.Top;
  if key=nil then Break;
  Case key^._type of
   //btBegin:;//NOTHING
   btIf1  , //Not ended if (error)
   btIf2  , //normal end if
   btElse , //Not ended if else (error)
   btWhile: //Not ended while   (error)
           Script^.SetGotoIds(Script^.GetLastCmdId+1,key^.GotoBackward);
  end;
  {Case TopBlock._type of
   -1:Break;
   1,3,4:Script^.TrySetExpOp(TopBlock.VSID,Script^.GetLastCmdId+1);
  end;}
  PopBlock;
 until false;
end;

Procedure TSQLParseContext.cbEnd;
var
 key:PBlockState;
begin
 key:=FBlockStack.Top;
 if key=nil then
 begin
  //error
  Exit;
 end;
 Case key^._type of
  btBegin:begin
           PopBlock;
           Reset;
           key:=FBlockStack.Top;
           if key=nil then
           begin
            //error
            Exit;
           end;
           Case key^._type of
            btIf1  :begin; //IF BEGIN END
                     //set if id
                     Script^.SetGotoIds(Script^.GetLastCmdId+1,key^.GotoBackward);
                     key^._type:=btIf2;
                     PopBlock;
                     {if Script^.TrySetExpOp(TopBlock.VSID,Script^.GetLastCmdId+1) then
                     begin
                      SetTopBlockType(2);
                     end else
                     begin
                      ResetErr;
                     end;}
                    end;
            btIf2  :PopBlock; //IF BEGIN END BEGIN END
            btElse :begin; //IF ELSE BEGIN END
                     //set goto id
                     Script^.SetGotoIds(Script^.GetLastCmdId+1,key^.GotoBackward);
                     {if not Script^.TrySetCmdOp(TopBlock.VSID,Script^.GetLastCmdId+1) then
                      ResetErr;}
                     PopBlock;
                    end;
            btWhile:begin
                     //add goto
                     Script^.AddCmd('',qcGoto,key^.FBeginId);
                     //set while id
                     Script^.SetGotoIds(Script^.GetLastCmdId+1,key^.GotoBackward);
                     PopBlock;
                     {if Script^.TryGetExpOp(TopBlock.VSID,id) then
                     begin
                      //add goto
                      Script^.AddCmd('',qcGoto,id);
                      //set while id
                      Script^.TrySetExpOp(TopBlock.VSID,Script^.GetLastCmdId+1);
                      PopBlock;
                     end else
                      ResetErr;}
                    end;
           end;
          end;
  else
    begin
     //error
    end;
 end;
end;

Procedure TSQLParseContext.cbCond;

 Procedure _FinishErr; inline;
 begin
  FExpressionState.Finish;
  FExpressionState.Clean;
  FQueryState.Clean;
  ResetErr;
 end;

 Procedure _Finish; inline;
 var
  i:SizeInt;
 begin
  FExpressionState.Finish;

  //Writeln(FExpressionState.GetStr);

  i:=Script^.AddExp(-1,FExpressionState.Node);
  AddCmd('',qcIf,i);
  i:=Script^.GetLastCmdId;

  Case State of
   0:begin //IF
      PushBlock(btIf1,i)^.AddGotoId(i);
     end;
   1:begin //WHILE
      PushBlock(btWhile,i)^.AddGotoId(i);
     end;
  end;

  FExpressionState.Node:=nil;
  FExpressionState.Clean;
  FQueryState.Clean;
  Reset;
  cbNormal;
 end;

begin
 if isEOC(FToken) then
 begin
  _FinishErr;
 end else
 if _is_cond_break(FToken) then
 begin
  _Finish;
 end else
 begin
  if not FExpressionState.Parse(FToken) then
  begin
   Case FToken.TokenType of
    ttWord,
    ttKeyword:begin
               _Finish;
              end;
    else
              begin
               _FinishErr;
              end;
   end;
  end;
 end;
end;

Procedure TSQLParseContext.cbBreak;
var
 I:TBlockStack.Iterator;
begin
 I:=FBlockStack.cend;
 repeat
  if I.Key=nil then Break;
  if I.Key^._type=btWhile then
  begin
   //add id
   I.Key^.AddGotoId(Script^.GetLastCmdId+1);
   //add goto
   AddCmd('',qcGoto,-1);
   Exit;
  end;
  I.Prev;
 until false;
 //error
end;

Procedure TSQLParseContext.cbContinue;
var
 I:TBlockStack.Iterator;
begin
 I:=FBlockStack.cend;
 repeat
  if I.Key=nil then Break;
  if I.Key^._type=btWhile then
  begin
   //add goto
   AddCmd('',qcGoto,I.Key^.FBeginId);
   Exit;
  end;
  I.Prev;
 until false;
 //error
end;

Procedure TSQLParseContext.cbElse;
var
 i:SizeInt;
 key:PBlockState;
begin
 key:=FBlockStack.Top;
 if key=nil then
 begin
  //error
  Exit;
 end;
 Case key^._type of
  btIf1,
  btIf2:begin
         //add goto
         Script^.AddCmd('',qcGoto,-1);
         //set if id
         i:=Script^.GetLastCmdId;
         Script^.SetGotoIds(i+1,key^.GotoBackward);
         //add goto id
         key^._type:=btElse;
         key^.ClearGotoId;
         key^.AddGotoId(i);
         {if Script^.TrySetExpOp(TopBlock.VSID,Script^.GetLastCmdId+1) then
         begin
          //set goto block id
          PopBlock;
          PushBlock(btElse,Script^.GetLastCmdId);
         end else
         begin
          ResetErr;
         end;}
        end;
  else
    begin
     //error
    end;
 end
end;

Procedure TSQLParseContext.cbCommit;
begin
 if isEOC(FToken) then
 begin
  //OnEOCCmd;
  Reset;
 end else
 if _is_cond_break(FToken) then
 begin
  Reset;
  cbNormal;
 end else
 Case State of
  0:Case FToken.TokenType of
     ttWhitespace:;
     ttWord,
     ttKeyWord:Case _get_begin_op(FToken.P,FToken.L) of
                 0,1:begin //TRAN TRANSACTION
                      AddCmd('',qcCommitTran,-1);
                      State:=1;
                     end;
                else begin
                      AddCmd('',qcCommitWork,-1);
                      State:=1;
                     end;
               end;
    end;
  1:;
 end;
end;

Procedure TSQLParseContext.cbRollback;
begin
 if isEOC(FToken) then
 begin
  //OnEOCCmd;
  Reset;
 end else
 if _is_cond_break(FToken) then
 begin
  Reset;
  cbNormal;
 end else
 Case State of
  0:Case FToken.TokenType of
     ttWhitespace:;
     ttWord,
     ttKeyWord:Case _get_begin_op(FToken.P,FToken.L) of
                 0,1:begin //TRAN TRANSACTION
                      AddCmd('',qcRollbackTran,-1);
                      State:=1;
                     end;
                else begin
                      AddCmd('',qcRollbackWork,-1);
                      State:=1;
                     end;
               end;
    end;
  1:;
 end;
end;

Procedure TSQLParseContext.BeginParse;
begin
 FDeclareState.Context    :=@Self;
 FFetchState.Context      :=@Self;
 FExpressionState.cb_parse:=@FQueryState.Parse;
 FExpressionState.cb_fin  :=@SubQueryFinish;
 Reset;
end;

Procedure TSQLParseContext.EndParse;
begin
 OnBeforeAddBath;
 Script^.TrimBath;
 FBlockStack.Free;
end;

function CmpParams(p1,p2:TParams):Boolean;
Var
 i:SizeInt;
begin
 Result:=False;
 if Length(p1)<>Length(p2) then Exit;
 if Length(p1)=0 then Exit(True);
 For i:=0 to High(p1) do if (p1[i]<>p2[i]) then Exit;
 Result:=True;
end;

function TSQLScript.TryGetParams(i:SizeInt;var R:TParams):Boolean;
begin
 Result:=False;
 R:=Default(TParams);
 if (i>=0) and (i<Length(FParamsList)) then
 begin
  Result:=True;
  R:=FParamsList[i];
 end;
end;

function TSQLScript.TryGetStatment(i:SizeInt;var R:TSQLStatment):Boolean;
begin
 Result:=False;
 R.FValuesID:=-1;
 R.FParamsID:=-1;
 if (i>=0) and (i<Length(FStatmentList)) then
 begin
  Result:=True;
  R:=FStatmentList[i];
 end;
end;

function TSQLScript.AddParams(Const _Params:TParams):SizeInt;
Var
 i,L:SizeInt;
begin
 if (Length(_Params)=0) then Exit(-1);
 L:=Length(FParamsList);
 if L<>0 then
 For i:=0 to L-1 do if CmpParams(FParamsList[i],_Params) then Exit(i);
 SetLength(FParamsList,L+1);
 FParamsList[L]:=_Params;
 Result:=L;
end;

function TSQLScript.AddStatment(Const FParams,FValues:TParams;ResultTp:TZResultSetType):SizeInt;

begin
 Result:=Length(FStatmentList);
 SetLength(FStatmentList,Result+1);
 FStatmentList[Result].FParamsID:=AddParams(FParams);
 FStatmentList[Result].FValuesID:=AddParams(FValues);
 FStatmentList[Result].FResultTp:=ResultTp;
end;

procedure TSQLScript.AddBath(LastCount:SizeUint);
Var
 i:SizeUint;
begin
 i:=Length(FBatchList);
 if (i<>0) then
 begin
  if (Length(FBatchList[i-1].FCommands)<>0) then
  begin
   if (LastCount=0) then LastCount:=1;
   FBatchList[i-1].FCount:=LastCount;
   SetLength(FBatchList,i+1);
   FBatchList[i]:=Default(TSQLBatchCmd);
   FBatchList[i].FCount:=1;
  end;
 end else
 begin
  SetLength(FBatchList,i+1);
  FBatchList[i]:=Default(TSQLBatchCmd);
  FBatchList[i].FCount:=1;
 end;
end;

procedure TSQLScript.TrimBath;
Var
 i:SizeUint;
begin
 i:=Length(FBatchList);
 if (i<>0) then
 begin
  Dec(i);
  if (Length(FBatchList[i].FCommands)=0) then
  begin
   SetLength(FBatchList,i);
  end;
 end;
end;

procedure TSQLScript.AddCmd(Const _Value:RawByteString;_Type:TSQLCommandType;_SID:SizeInt);
Var
 i,b:SizeUint;
begin
 b:=Length(FBatchList);
 if (b=0) then Exit;
 Dec(b);
 i:=Length(FBatchList[b].FCommands);
 SetLength(FBatchList[b].FCommands,i+1);
 With FBatchList[b].FCommands[i] do
 begin
  VType:=_Type;
  SID  :=_SID;
  Value:=_Value;
 end;
end;

function TSQLScript.GetLastCmdId:Sizeint;
Var
 b:SizeUint;
begin
 Result:=-1;
 b:=Length(FBatchList);
 if (b=0) then Exit;
 Dec(b);
 Result:=Length(FBatchList[b].FCommands)-1;
end;

function TSQLScript.TrySetCmdOp(i,o:SizeInt):Boolean;
Var
 b:SizeUint;
begin
 Result:=false;
 b:=Length(FBatchList);
 if (b=0) then Exit;
 Dec(b);
 if (i>=0) and (i<Length(FBatchList[b].FCommands)) then
 begin
  FBatchList[b].FCommands[i].SID:=o;
  Result:=true;
 end;
end;

function TSQLScript.TryGoto(b,i:SizeInt):Boolean;
begin
 Result:=(b>=0) and (b<Length(FBatchList))
     and (i>=0) and (i<=Length(FBatchList[b].FCommands));
end;

function TSQLScript.AddExp(OpID:SizeInt;Node:PExpNode):SizeInt;
begin
 Result:=Length(FExps);
 SetLength(FExps,Result+1);
 FExps[Result].OpID:=OpID;
 FExps[Result].Node:=Node;
end;

function TSQLScript.TryGetExp(i:SizeInt;var R:TSQLExp):Boolean;
begin
 Result:=False;
 R:=Default(TSQLExp);
 if (i>=0) and (i<Length(FExps)) then
 begin
  Result:=True;
  R:=FExps[i];
 end;
end;

function TSQLScript.TrySetExpOp(i,o:SizeInt):Boolean;
begin
 Result:=False;
 if (i>=0) and (i<Length(FExps)) then
 begin
  Result:=True;
  FExps[i].OpID:=o;
 end;
end;

function TSQLScript.TryGetExpOp(i:SizeInt;var R:SizeInt):Boolean;
begin
 Result:=False;
 if (i>=0) and (i<Length(FExps)) then
 begin
  Result:=True;
  R:=FExps[i].OpID;
 end;
end;

procedure TSQLScript.SetGotoIds(id:SizeInt;Back:TGotoBackward);
var
 i,b,c:SizeInt;
begin
 b:=Length(FBatchList);
 if (b=0) then Exit;
 Dec(b);
 if Length(Back)<>0 then
  For i:=0 to High(Back) do
  begin
   c:=Back[i];
   if (c>=0) and (c<Length(FBatchList[b].FCommands)) then
   begin
    Case FBatchList[b].FCommands[c].VType of
     qcIf  :TrySetExpOp(FBatchList[b].FCommands[c].SID,id);
     qcGoto:FBatchList[b].FCommands[c].SID:=id;
    end;
   end;
  end;
end;

Procedure TSQLScript.Parse(Stream:TStream);
Var
 Context:TSQLParseContext;

 MStream:TMemoryStream;
 Buffer,EOS:PChar;

 //tmp:RawByteString;

begin
 Context:=Default(TSQLParseContext);
 Context.Script:=@Self;
 Context.FTokenizer:=TZDbcScriptTokenizer.Create;
 Context.BeginParse;

 MStream:=nil;
 if Stream.InheritsFrom(TMemoryStream) then
 begin
  Buffer:=TMemoryStream(Stream).Memory;
  EOS:=Buffer+Stream.Size;
 end else
 begin
  MStream:=TMemoryStream.Create;
  MStream.CopyFrom(Stream,Stream.Size);
  Buffer:=TMemoryStream(MStream).Memory;
  EOS:=Buffer+MStream.Size;
 end;

 Context.FToken:=Default(TZToken);
 AddBath(1);
 While (Context.FToken.TokenType<>ttEOF) do
 begin
  Context.FToken:=TZDbcScriptTokenizer(Context.FTokenizer).FetchNextToken(Buffer,EOS);

  //SetString(tmp,Context.FToken.P,Context.FToken.L);
  //if Context.FToken.TokenType<>ttWhitespace then
  // Writeln(Context.FToken.TokenType,':[',tmp,']');

  Context.cb();
 end;
 Context.EndParse;

 FreeAndNil(MStream);
 FreeAndNil(Context.FTokenizer);
end;

procedure TSQLContext.SetParams2stmt(Const FParams:TParams;Fstmt:IZPreparedStatement);
var
 i:Longint;
 V:TZVariant;
begin
 if Length(FParams)=0 then Exit;
 For i:=0 to High(FParams) do
 begin
  V:=FVariables.GetValue(FParams[i]);
  Fstmt.SetValue(i+FirstDbcIndex,V);
 end;
end;

procedure TSQLContext.SetValues2ctx(Const FValues:TParams;R:IZResultSet;withi:Longint=0);
var
 i,L:Longint;
 V:TZVariant;
 M:IZResultSetMetadata;
begin
 L:=Length(FValues);
 M:=R.GetMetadata;
 if Assigned(M) then
 begin
  i:=M.GetColumnCount;
  M:=nil;
 end else
 begin
  i:=0;
 end;
 if (i<L) then L:=i;
 if (withi<0) then withi:=0;
 if (L<>0) and (withi<L) then
 begin
  L:=L-1;
  For i:=withi to L do
  begin
   V:=R.GetValue(i+1);
   FVariables.SetValue(FValues[i],V);
  end;
 end;
end;

Function GetValueType(Const S:RawByteString):Boolean; inline;
begin
 Result:=(S<>'') and (S[1]=':');
end;

function TSQLContext.GetStatment(Const FValues:TParams):RawByteString;
var
 i:Longint;
begin
 Result:='';
 if Length(FValues)=0 then Exit;
 For i:=0 to High(FValues) do
 begin
  Case GetValueType(FValues[i]) of
   false:Result:=Result+FValues[i];
   true :Result:=Result+FVariables.GetRawByteString(FValues[i]);
  end;
 end;
end;

Function TSQLContext.PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;
begin
 if Assigned(FGlobalCache) then
 begin
  Result:=FGlobalCache.PrepareStatement(FConnection,FSQL);
 end else
 begin
  Result:=FConnection.PrepareStatement(FSQL);
 end;
end;

function TSQLScript.Excecute(var Context:TSQLContext):TSQLScriptIterator;
begin
 Result:=Default(TSQLScriptIterator);
 if Length(FBatchList)=0 then Exit;

 Result.Script :=@Self;
 Result.Context:=@Context;
 Result.FSaveAutoCommit:=Context.FConnection.GetAutoCommit;

 //Context.FConnection.SetAutoCommit(false);
 //Context.FConnection.CreateStatement.Execute('ROLLBACK TRANSACTION');

 //Writeln('StartTransaction=',Context.FConnection.StartTransaction);

end;

procedure TSQLScriptIterator.BeginTran;
begin
 Context^.FConnection.StartTransaction;
 Inc(FTRANCOUNT);
end;

procedure TSQLScriptIterator.CommitTran;
begin
 if (FTRANCOUNT<>0) then
 begin
  Context^.FConnection.Commit;
  Dec(FTRANCOUNT);
 end;
end;

procedure TSQLScriptIterator.RollbackTran;
begin
 if (FTRANCOUNT<>0) then
 begin
  Context^.FConnection.Rollback;
  Dec(FTRANCOUNT);
 end;
end;

procedure TSQLScriptIterator.CommitWork;
begin
 While (FTRANCOUNT<>0) do
 begin
  Context^.FConnection.Commit;
  Dec(FTRANCOUNT);
 end;
end;

procedure TSQLScriptIterator.RollbackWork;
begin
 While (FTRANCOUNT<>0) do
 begin
  Context^.FConnection.Rollback;
  Dec(FTRANCOUNT);
 end;
end;

procedure TSQLScriptIterator.SetVar;
var
 R:TSQLExp;
 P:PZVariant;
 RCalc:TExpressionCalc;
 Z:TZVariant;
begin
 P:=Context^.FVariables.GetMutable(FCmd.Value);
 R:=Default(TSQLExp);
 if Assigned(P) and Script^.TryGetExp(FCmd.SID,R) then
 begin
  RCalc.cb_GetValue:=@Context^.FVariables.GetValue;
  RCalc.cb_GetQuery:=@GetQuery;
  RCalc.Node:=R.Node;
  Z:=RCalc.calc;

  //Writeln('SET:=',_GetAsRawByteString(Z));

  DoAssignOp(R.OpID,P^,Z);
 end;
end;

function TSQLScriptIterator.GetQuery(VSID:Int64):IZResultSet;
Var
 stmt:IZPreparedStatement;
 S:TSQLStatment;
 FValues:TParams;
begin
 Result:=nil;
 stmt:=nil;
 if Script^.TryGetStatment(VSID,S) then
 if Script^.TryGetParams(S.FValuesID,FValues) then
 begin
  stmt:=Context^.PrepareStatement(Context^.GetStatment(FValues));
  if Script^.TryGetParams(S.FParamsID,FValues) then
  begin
   Context^.SetParams2stmt(FValues,stmt);
  end;
  Result:=stmt.ExecuteQueryPrepared;
 end;
end;

procedure TSQLScriptIterator.PrintExp;
var
 R:TSQLExp;
 RCalc:TExpressionCalc;
 Z:TZVariant;
begin
 R:=Default(TSQLExp);
 if Assigned(FOnPrintCb) then
 if Script^.TryGetExp(FCmd.SID,R) then
 begin
  RCalc.cb_GetValue:=@Context^.FVariables.GetValue;
  RCalc.cb_GetQuery:=@GetQuery;
  RCalc.Node:=R.Node;
  Z:=RCalc.calc;
  FOnPrintCb(_GetAsRawByteString(Z));
  //Writeln(_GetAsRawByteString(Z));
 end;
end;

procedure TSQLScriptIterator.GotoId;
begin
 if Script^.TryGoto(b,FCmd.SID) then
 begin
  i:=FCmd.SID-1;
 end;
end;

procedure TSQLScriptIterator.OnIf;
var
 R:TSQLExp;
 RCalc:TExpressionCalc;
 Z:TZVariant;
begin
 R:=Default(TSQLExp);
 if Script^.TryGetExp(FCmd.SID,R) then
 begin
  RCalc.cb_GetValue:=@Context^.FVariables.GetValue;
  RCalc.cb_GetQuery:=@GetQuery;
  RCalc.Node:=R.Node;
  Z:=RCalc.calc;
  if not _GetAsBoolean(Z) then
  begin
   if Script^.TryGoto(b,R.OpID) then
   begin
    i:=R.OpID-1;
   end;
  end;
 end;
end;

function TSQLScriptIterator.Next:Boolean;
begin
 Result:=Assigned(Script) and Assigned(Context);
 if (not Result) then Exit;

 FCmd:=Script^.FBatchList[b].FCommands[i];

 Case FCmd.VType of
  qcIf           :OnIf;
  qcGoto         :GotoId;
  qcBeginTran    :BeginTran;
  qcCommitTran   :CommitTran;
  qcCommitWork   :CommitWork;
  qcRollbackTran :RollbackTran;
  qcRollbackWork :RollbackWork;
  qcSetVar       :SetVar;
  qcPrint        :PrintExp;
  qcStatment     :ExecuteCmd;
  qcDeclareVar   :Context^.FVariables.SetValue(FCmd.Value,EncodeNull);
  qcDeclareCursor:DeclareCursor;
  qcDeleteCursor :Context^.DelCursor(FCmd.Value);
  qcOpenCursor   :OpenCursor;
  qcCloseCursor  :CloseCursor;
  qcFetchNext    ,
  qcFetchPrior   ,
  qcFetchFirst   ,
  qcFetchLast    :FetchCursor;
  qcFetchAbs,
  qcFetchRel     :FetchCursorNum;
 end;

 Inc(i);
 if (i>=Length(Script^.FBatchList[b].FCommands)) then
 begin
  i:=0;
  Inc(c);
  if (c>=Script^.FBatchList[b].FCount) then
  begin
   c:=0;
   Inc(b);
   While (b<Length(Script^.FBatchList)) do
   begin
    if (Length(Script^.FBatchList[b].FCommands)<>0) then
    begin
     Exit(true);
    end;
    Inc(b);
   end;
   Exit(false);
  end;
 end;

end;

Procedure TSQLScriptIterator.OpenCursor;
var
 Z:PZCursor;

 stmt:IZPreparedStatement;
 FParams:TParams;
 FParamsID:SizeInt;

 R:IZResultSet;
begin
 Z:=Context^.GetCursor(FCmd.Value);
 if not Assigned(Z) then Exit;
 if not Assigned(Z^.VInterface) then Exit;

 if Z^.FOpen then Exit;

 stmt:=IZPreparedStatement(Z^.VInterface);
 FParamsID:=Z^.FTag;

 if Script^.TryGetParams(FParamsID,FParams) then
 begin
  stmt.ClearParameters;
  Context^.SetParams2stmt(FParams,stmt);
 end;
 R:=stmt.ExecuteQueryPrepared;
 Z^.VInterface:=IZInterface(R);

 Z^.FOpen:=true;
end;

Procedure TSQLScriptIterator.CloseCursor;
var
 Z:PZCursor;
 R:IZResultSet;
 stmt:IZPreparedStatement;
begin
 Z:=Context^.GetCursor(FCmd.Value);
 if not Assigned(Z) then Exit;
 if not Assigned(Z^.VInterface) then Exit;

 if not Z^.FOpen then Exit;

 R:=IZResultSet(Z^.VInterface);

 stmt:=IZPreparedStatement(R.GetStatement);
 R.Close;

 Z^.VInterface:=IZInterface(stmt);

 Z^.FOpen:=false;
end;

Procedure TSQLScriptIterator.FetchCursor;
var
 Z:PZCursor;
 R:IZResultSet;
 F:Boolean;
 FValues:TParams;
begin
 Z:=Context^.GetCursor(FCmd.Value);
 if not Assigned(Z) then Exit;
 if not Assigned(Z^.VInterface) then Exit;

 if not Z^.FOpen then Exit;

 R:=IZResultSet(Z^.VInterface);

 F:=false;
 Case FCmd.VType of
  qcFetchNext    :F:=R.Next;
  qcFetchPrior   :F:=R.Previous;
  qcFetchFirst   :F:=R.First;
  qcFetchLast    :F:=R.Last;
 end;
 if F and Script^.TryGetParams(FCmd.SID,FValues) then
 begin
  Context^.SetValues2ctx(FValues,R);
 end;
end;

Procedure TSQLScriptIterator.FetchCursorNum;
var
 Z:PZCursor;
 R:IZResultSet;
 F:Boolean;
 FValues:TParams;
 num:Integer;

 function TryRawToInt(Const S:RawByteString;var num:Integer):Boolean; inline;
 var
  P,PEnd:PAnsiChar;
 begin
  P:=PAnsiChar(S)+Length(S);
  PEnd:=P;
  num:=ValRawInt(PAnsiChar(S),PEnd);
  Result:=(PEnd=P);
 end;

begin
 Z:=Context^.GetCursor(FCmd.Value);
 if not Assigned(Z) then Exit;
 if not Assigned(Z^.VInterface) then Exit;

 if not Z^.FOpen then Exit;

 R:=IZResultSet(Z^.VInterface);

 F:=false;
 if Script^.TryGetParams(FCmd.SID,FValues) then
 if Length(FValues)<>0 then
 begin
  num:=0;
  if not TryRawToInt(FValues[0],num) then
  begin
   num:=Context^.FVariables.GetAsInteger(FValues[0]);
  end;
  if (num<>0) then
  begin
   Case FCmd.VType of
    qcFetchAbs:F:=R.MoveAbsolute(num);
    qcFetchRel:F:=R.MoveRelative(num);
   end;
   if F then
   begin
    Context^.SetValues2ctx(FValues,R,1);
   end;
  end;
 end;
end;

Procedure TSQLScriptIterator.DeclareCursor;
Var
 S:TSQLStatment;
 FValues:TParams;
begin
 if Script^.TryGetStatment(FCmd.SID,S) then
 if Script^.TryGetParams(S.FValuesID,FValues) then
 begin
  Context^.DeclareStatement(FCmd.Value,Context^.GetStatment(FValues),S.FParamsID,S.FResultTp);
 end;
end;

Procedure TSQLScriptIterator.ExecuteCmd;
Var
 R:IZResultSet;
 stmt:IZPreparedStatement;
 S:TSQLStatment;
 FValues:TParams;
begin
 stmt:=nil;
 if Script^.TryGetStatment(FCmd.SID,S) then
 if Script^.TryGetParams(S.FValuesID,FValues) then
 begin
  //Writeln(Context^.GetStatment(FValues));
  stmt:=Context^.PrepareStatement(Context^.GetStatment(FValues));
  if Script^.TryGetParams(S.FParamsID,FValues) then
  begin
   Context^.SetParams2stmt(FValues,stmt);
  end;
  R:=stmt.ExecuteQueryPrepared;
  if Assigned(R) then
  begin
   Context^.FGlobal:=R;
  end;
 end;
end;

procedure TSQLScriptIterator.Close(isCommit:Boolean);
begin
 if Assigned(Script) and Assigned(Context) then
 begin
  //Writeln('StartTransaction=',Context^.FConnection.StartTransaction);
  if (not Context^.FConnection.GetAutoCommit) then
  begin
   Case isCommit of
    true :CommitWork;
    false:RollbackWork;
   end;
  end;
  Context^.FConnection.SetAutoCommit(FSaveAutoCommit);
 end;
 Self:=Default(TSQLScriptIterator);
end;

end.

