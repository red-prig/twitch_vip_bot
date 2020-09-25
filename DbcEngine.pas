{ async dbc

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

unit DbcEngine;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 Classes,
 gset,
 RWLock,

 ZSysUtils,
 ZDbcIntfs,
 ZAbstractConnection,
 ZDbcConnection,
 ZDbcLogging,
 ZCompatibility,

 UAsyncResultSet,

 TaskManager,
 UAsyncQueue,
 ULog,
 rutils;

type
 dbc_event=class(app_logic)
 end;

 TZResultSet        =UAsyncResultSet.TZAsyncResultSet;
 TZResultSetMetadata=UAsyncResultSet.TZAsyncResultSetMetadata;
 TZSQLType          =ZDbcIntfs.TZSQLType;
 TZResultSetType    =ZDbcIntfs.TZResultSetType;
 TZURL              =ZDbcIntfs.TZURL;

 TOnDbcProc=UAsyncQueue.TOnParent;

 TOnParent=UAsyncQueue.TOnParent;
 PQNode=^TQNode;
 TQNode=object(UAsyncQueue.TQFrom2Node)
  public
   FOnDbcProc:TOnDbcProc;
   FErrorCode:Integer;
   FMessage:RawByteString;
 end;

 TDbcConnection=class;

 PZPreparedCache=^TZPreparedCache;
 TZPreparedCache=record
  FKey:RawByteString;
  FTag:SizeInt;
  stmt:IZPreparedStatement;
 end;

 TZPreparedCacheCompare=class
  class function c(var a,b:TZPreparedCache):boolean; static;
 end;

 _TZPreparedCacheSet=specialize TSet<TZPreparedCache,TZPreparedCacheCompare>;

 TZPreparedCacheSet=class(_TZPreparedCacheSet)
  Function  PrepareStatement(FConnection:IZConnection;Const FSQL:RawByteString):IZPreparedStatement;
  Function  GetStatement(Const FSQL:RawByteString):IZPreparedStatement;
  Procedure DelStatement(Const FSQL:RawByteString);
 end;

 TDbcThread=class(TCustomAsyncThread)
  private
   FDbcCon:TDbcConnection;
   FConnection:IZConnection;
   FPreparedCache:TZPreparedCacheSet;
   FPingSupport:Boolean;
  protected
   FThreadID:TThreadID;
   Procedure   DBOpen;
   Function    DBPing:Boolean;
   Procedure   DBClose;
   Procedure   InherBegin;  override;
   Procedure   InherUpdate; override;
   Procedure   InherEnd;    override;
   Function    GetPreparedCache:TZPreparedCacheSet;
   Function    PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;
  public
   TransactIsolation:TZTransactIsolationLevel;
   Constructor Create(F:TDbcConnection);
 end;

 TDbcConnection=class(TBaseTask)
  private
   FThread:TDbcThread;
   FNode:TQNode;
   FZURL:RawByteString;
  protected
   Procedure   SetZURL(Const _ZURL:RawByteString);
   Procedure   FinishOpen;
   Procedure   FinishClose;
   Procedure   FinishError;
   function    SendCmd(Node:PQNode):Boolean;
   Procedure   SetTransactIsolation(V:TZTransactIsolationLevel);
   function    GetTransactIsolation:TZTransactIsolationLevel;
  published
   property    ZURL:RawByteString read FZURL write SetZURL;
   property    TransactIsolation:TZTransactIsolationLevel read GetTransactIsolation write SetTransactIsolation;
   property    ErrorCode:Integer read FNode.FErrorCode;
   property    Message:RawByteString read FNode.FMessage;
  public

   Function    ZConnection:IZConnection;
   Function    GetPreparedCache:TZPreparedCacheSet;

   Constructor Create; override;
   Procedure   Cleanup;override;

   Procedure   Open(Const _ZURL:RawByteString);
   Procedure   Close; inline;

   Function    Start:SizeInt; override;
   Function    Pause:SizeInt; override;
   Function    Stop:SizeInt;  override;

   function    Send(Node:PQNode):Boolean;
 end;

 TQueryHandle=class
  protected
   FDbcCon:TDbcConnection;
   Node:TQNode;
   FState:Boolean;
  public
   Var
    OnFinish:TFuncEvent;
  protected
   Procedure   SetConnection(C:TDbcConnection);
   Procedure   BreakSend;
   Procedure   Finish;
   Procedure   SetProc(P:TOnDbcProc);
   Function    GetMessage:RawByteString;
   Function    ZConnection:IZConnection;
   Function    CreateStatement:IZStatement;
   Function    PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;
  published
   property    DbcConnection:TDbcConnection read FDbcCon write SetConnection;
   property    OnDbcProc:TOnDbcProc read Node.Parent write SetProc;
   property    State:Boolean read FState;
   property    ErrorCode:Integer read Node.FErrorCode;
   property    Message:RawByteString read GetMessage;
  public

   Procedure   Clear;

   Constructor Create;
   Destructor  Destroy; override;
   Procedure   Send;
   Procedure   Cancel;
   Function    isCancel:Boolean; inline;
 end;

 TDbcQueryTask=class(TBaseTask)
  protected
   FHandle:TQueryHandle;
   Function    ZConnection:IZConnection;
   Function    CreateStatement:IZStatement;
   Function    PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;
   function    OnFinish:SizeInt;
  public
   property    Handle:TQueryHandle read FHandle;
   Constructor Create;        override;
   Procedure   Cleanup;       override;
   Function    Start:SizeInt; override;
   Function    Pause:SizeInt; override;
   Function    Stop:SizeInt;  override;
 end;

 TDbcStatementTask=class(TDbcQueryTask)
  protected
   FSQL:RawByteString;
   FRZ:TZResultSet;
   FCount:Integer;
   Procedure   OnQuery;
   Procedure   OnUpdate;
   Procedure   OnExe;
   function    GetResultSet:TZResultSet;
   Procedure   SetResultSet(R:TZResultSet);
  public
   Procedure   Cleanup; override;
   Procedure   ExecuteQuery(const SQL:RawByteString);
   Procedure   ExecuteUpdate(const SQL:RawByteString);
   Procedure   Execute(const SQL:RawByteString);
   function    UpdateCount:Integer;
   function    MoreResults:Boolean;
   property    ResultSet:TZResultSet read GetResultSet write SetResultSet;
 end;

 TDbcBatchTask=class(TDbcQueryTask)
  protected
   FBatch:AString;
   FCount:Integer;
   Procedure   OnBatch;
  public
   Procedure   AddSQL(const SQL:RawByteString);
   Procedure   Clear;
   function    GetCount:Integer;
 end;

Function TranslateFBError(S:RawByteString):RawByteString;
function insert_into_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
function insert_or_ignore_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
function replace_into_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
function select_sql(Const TableName,Opt1,Opt2:RawByteString;Const Defs:TFields):RawByteString;
function update_sql(Const TableName,Opt1,Opt2:RawByteString;Const Defs:TFields):RawByteString;
function delete_sql(Const TableName,Opt1,Opt2:RawByteString):RawByteString;
function where_sql(Const Defs:TFields):RawByteString;

function create_table_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
function create_index_sql(Const TableName,Options,IndexName,Colums:RawByteString):RawByteString;
function create_unique_index_sql(Const TableName,Options,IndexName,Colums:RawByteString):RawByteString;

function SpaceToNull(Const S:RawByteString):RawByteString; inline;
function SpaceToNullQuoted(Const S:RawByteString):RawByteString; inline;
function QuotedStr(Const S:RawByteString):RawByteString; inline;

implementation

Var
 GlobalGetConnLock:TRWLock;

class function TZPreparedCacheCompare.c(var a,b:TZPreparedCache):boolean;
begin
 Result:=CompareStr(a.FKey,b.FKey)<0;
end;

type
 TLogListen = class(TInterfacedObject,IZLoggingListener)
  procedure LogEvent(Event:TZLoggingEvent);
 end;

Var
 LogListen:TLogListen;

procedure TLogListen.LogEvent(Event:TZLoggingEvent);

 function getCat:String; inline;
 begin
  case Event.Category of
    lcConnect:      Result := 'Connect';
    lcDisconnect:   Result := 'Disconnect';
    lcTransaction:  Result := 'Transaction';
    lcExecute:      Result := 'Execute';
    lcPrepStmt:     Result := 'Prepare';
    lcBindPrepStmt: Result := 'Bind prepared';
    lcExecPrepStmt: Result := 'Execute prepared';
    lcUnprepStmt:   Result := 'Unprepare prepared';
  else
    Result := 'Other';
  end;
 end;

begin
 if Assigned(Event) then
 begin
  if Event.Error='' then
  begin
   //if Info then
    Log(dbc_event,Event.ErrorCodeOrAffectedRows,[getCat,', ',Event.Protocol,', ',Event.Message]);
  end else
  begin
   //if Error then
    Log(dbc_event,Event.ErrorCodeOrAffectedRows,[getCat,', ',Event.Protocol,', ',Event.Message,', ',Event.Error]);
  end;;
 end;
end;

Constructor TDbcThread.Create(F:TDbcConnection);
begin
 FPingSupport:=false;
 TransactIsolation:=tiReadCommitted;
 FDbcCon:=F;
 inherited Create;
end;

Constructor TDbcConnection.Create;
begin
 inherited;
 FThread:=TDbcThread.Create(Self);
 if FThread.State then
 begin
  inherited Pause;
 end;
end;

Procedure TDbcConnection.Cleanup;
begin
 FreeAndNil(FThread);
 inherited;
end;

Procedure TDbcConnection.SetZURL(Const _ZURL:RawByteString);
begin
 Case State of
  T_NEW,
  T_RUN,
  T_FIN:FZURL:=_ZURL;
 end;
end;

Procedure TDbcConnection.SetTransactIsolation(V:TZTransactIsolationLevel);
begin
 if Assigned(FThread) then FThread.TransactIsolation:=V;
end;

function  TDbcConnection.GetTransactIsolation:TZTransactIsolationLevel;
begin
 Result:=tiNone;
 if Assigned(FThread) then Result:=FThread.TransactIsolation;
end;

Procedure TDbcConnection.FinishOpen;
begin
 if FNode.FErrorCode=0 then
 begin
  inherited Start;
  Notify.Call(T_SUC);
 end else
 begin
  inherited Stop;
  Notify.Call(T_ERR);
 end;
end;

Procedure TDbcConnection.FinishClose;
begin
 inherited Stop;
end;

Procedure TDbcConnection.FinishError;
begin
 Notify.Call(T_ERR);
 inherited Stop;
end;

Procedure TDbcConnection.Open(Const _ZURL:RawByteString);
begin
 ZURL:=_ZURL;
 Start;
end;

Procedure TDbcConnection.Close; inline;
begin
 Stop;
end;

Function TDbcConnection.Start:SizeInt;
begin
 Result:=R_ERR;
 if State<>T_PAU then
  if Trim(ZURL)<>'' then
  begin
   Acquire;
   Result:=inherited Pause;
   Release;
  end;
 if Result=R_SUC then
 begin
  FNode:=Default(TQNode);
  FNode.Parent:=@FinishOpen;
  FNode.FErrorCode:=0;
  FNode.FOnDbcProc:=@FThread.DBOpen;
  SendCmd(@FNode);
 end;
end;

Function TDbcConnection.Pause:SizeInt;
begin
 Result:=R_ERR;
end;

Function TDbcConnection.Stop:SizeInt;
begin
 Result:=R_ERR;
 Acquire;
 if State<>T_PAU then Result:=inherited Pause;
 if Result=R_SUC then
 begin
  FNode:=Default(TQNode);
  FNode.Parent:=@FinishClose;
  FNode.FErrorCode:=0;
  FNode.FOnDbcProc:=@FThread.DBClose;
  SendCmd(@FNode);
 end;
 if Release then Free;
end;

function TDbcConnection.Send(Node:PQNode):Boolean;
begin
 Result:=FThread.Send(Node);
end;

function TDbcConnection.SendCmd(Node:PQNode):Boolean;
begin
 Result:=FThread.SendCmd(Node);
end;

function TDbcConnection.ZConnection:IZConnection;
begin
 Result:=nil;
 if Assigned(FThread) then
 if (FThread.FThreadID=system.ThreadID) then
 begin
  Result:=FThread.FConnection;
 end;
end;

Function TDbcConnection.GetPreparedCache:TZPreparedCacheSet;
begin
 Result:=nil;
 if Assigned(FThread) then
 if (FThread.FThreadID=system.ThreadID) then
 begin
  Result:=FThread.GetPreparedCache;
 end;
end;

Procedure TDbcThread.DBOpen;
Var
 F1,F2:Pointer;
begin
 if (FThreadID=system.ThreadID) then
 if Assigned(FDbcCon) then
 begin
  DBClose;
  rwlock_wrlock(GlobalGetConnLock);
  FConnection:=DriverManager.GetConnection(FDbcCon.FZURL);
  rwlock_unlock(GlobalGetConnLock);
  FConnection.Open;
  FConnection.SetAutoCommit(True);
  FConnection.SetTransactionIsolation(TransactIsolation);

  F1:=TMethod(@((FConnection as TZAbstractDbcConnection).PingServer)).Code;
  F2:=Pointer(@TZAbstractDbcConnection.PingServer);
  FPingSupport:=F1<>F2;
 end;
end;

Function TDbcThread.DBPing:Boolean;
begin
 Result:=true;
 if (FThreadID<>system.ThreadID) or (not FPingSupport) then Exit;
 try
  if Assigned(FConnection) then
  begin
   if FConnection.PingServer<>0 then
   begin
    Log(app_logic,1,['DbcThread.PingError']);
    Result:=False;
   end;
  end;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
   Result:=False;
  end;
 end;
end;

Procedure TDbcThread.DBClose;
begin
 if (FThreadID<>system.ThreadID) then Exit;

 try
  //not neeed iteration finalize
  FreeAndNil(FPreparedCache);
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 try
  FConnection:=nil;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
   FConnection:=nil;
  end;
 end;
end;

Procedure TDbcThread.InherBegin;
begin
 FThreadID:=System.ThreadID;
end;

Procedure TDbcThread.InherUpdate;
Var
 Node:PQNode;
 rc:PtrUInt;
 DbcProc:TOnDbcProc;

begin
 Node:=nil;
 rc:=0;

 rc:=FCmdQueue.Count;
 if rc>100 then rc:=100;
 While FCmdQueue.Recv(Node) do
 begin
  try
   Node^.FErrorCode:=0;
   DbcProc:=Node^.FOnDbcProc;
   if Assigned(DbcProc) then  DbcProc();
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
    if E.InheritsFrom(EZSQLThrowable) then
    begin
     Node^.FErrorCode:=EZSQLThrowable(E).ErrorCode;
    end;
    if Node^.FErrorCode=0 then Node^.FErrorCode:=-1;
    Node^.FMessage:=E.Message;
   end;
  end;
  SendFrom(Node);
  if rc=0 then Break;
  Dec(rc);
  if rc=0 then Break;
 end;

 rc:=FQueue.Count;
 if rc>100 then rc:=100;
 While FQueue.Recv(Node) do
 begin

  try
   Node^.FErrorCode:=0;
   if (not Assigned(FConnection)) then
   begin
    Node^.FErrorCode:=4;
    Node^.FMessage:='Нет соединения с сервером!';
   end else
   if not DBPing then
   begin
    Node^.FErrorCode:=5;
    Node^.FMessage:='Потеряно соединение с сервером!';
    DBClose;
    //asy
    SyncFunc(@FDbcCon.FinishError);
   end else
   begin
    DbcProc:=Node^.FOnDbcProc;
    if Assigned(DbcProc) then
    begin
     DbcProc();
    end;
   end;
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
    if E.InheritsFrom(EZSQLThrowable) then
    begin
     Node^.FErrorCode:=EZSQLThrowable(E).ErrorCode;
    end;
    if Node^.FErrorCode=0 then
    begin
     Node^.FErrorCode:=-1;
    end;
    Node^.FMessage:=E.Message;
   end;
  end;

  SendFrom(Node);

  if rc=0 then Break;
  Dec(rc);
  if rc=0 then Break;
 end;

 if FQueue.IsEmpty then
 begin
  RTLeventWaitFor(FQueue.hEvent,{2*60*}10*1000);
  if (not FState) and FPingSupport and FQueue.IsEmpty then
  begin
   if not DBPing then
   begin
    if Assigned(FDbcCon) then
    begin
     Node:=@FDbcCon.FNode;
     Node^.FErrorCode:=5;
     Node^.FMessage:='Потеряно соединение с сервером!';
    end;

    DBClose;
    //asy
    SyncFunc(@FDbcCon.FinishError);
   end;
  end;
 end;

end;

Procedure TDbcThread.InherEnd;
Var
 Node:PQNode;
begin
 Node:=nil;
 While FQueue.Recv(Node) do
 begin
  SendFrom(Node);
 end;
 DBClose;
end;

Function TZPreparedCacheSet.PrepareStatement(FConnection:IZConnection;Const FSQL:RawByteString):IZPreparedStatement;
Var
 Z:TZPreparedCache;
 FNode:TZPreparedCacheSet.PNode;
begin
 Result:=nil;
 Z.FKey:=Trim(FSQL);
 Z.stmt:=nil;
 FNode:=NFind(Z);
 if Assigned(FNode) then
 begin
  Result:=FNode^.Data.stmt;
  Result.ClearParameters;
 end else
 begin
  Z.stmt:=FConnection.PrepareStatement(FSQL);
  Insert(Z);
  Result:=Z.stmt;
  Result.SetResultSetConcurrency(rcReadOnly);
  Result.SetFetchDirection(fdForward);
 end;
end;

Function TZPreparedCacheSet.GetStatement(Const FSQL:RawByteString):IZPreparedStatement;
Var
 Z:TZPreparedCache;
 FNode:TZPreparedCacheSet.PNode;
begin
 Result:=nil;
 Z.FKey:=Trim(FSQL);
 Z.stmt:=nil;
 FNode:=NFind(Z);
 if Assigned(FNode) then
 begin
  Result:=FNode^.Data.stmt;
 end;
end;

Procedure TZPreparedCacheSet.DelStatement(Const FSQL:RawByteString);
Var
 Z:TZPreparedCache;
begin
 Z.FKey:=Trim(FSQL);
 Z.stmt:=nil;
 Delete(Z);
end;

Function TDbcThread.GetPreparedCache:TZPreparedCacheSet;
begin
 Result:=nil;
 if (FThreadID=system.ThreadID) then
 begin
  Result:=FPreparedCache;
 end;
end;

Function TDbcThread.PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;
begin
 Result:=nil;
 if (FThreadID=system.ThreadID) then
 if Assigned(FConnection) then
 begin
  if not Assigned(FPreparedCache) then
  begin
   FPreparedCache:=TZPreparedCacheSet.Create;
  end;
  Result:=FPreparedCache.PrepareStatement(FConnection,FSQL);
 end;
end;

Procedure TQueryHandle.Clear;
begin
 if FState then Exit;
 OnFinish:=nil;
 Node.FOnDbcProc:=nil;
 Node.FErrorCode:=0;
 Node.FMessage:='';
end;

Procedure TQueryHandle.SetConnection(C:TDbcConnection);
begin
 if not State then
 if FDbcCon<>C then
 begin
  if Assigned(FDbcCon) then
  begin
   FDbcCon.Free;
  end;
  FDbcCon:=C;
  if Assigned(FDbcCon) then
  begin
   FDbcCon.Acquire;
  end;
 end;
end;

Constructor TQueryHandle.Create;
begin
 Node:=Default(TQNode);
 Node.Parent:=@Finish;
 FState:=False;
end;

Destructor  TQueryHandle.Destroy;
begin
 Clear;
 SetConnection(nil);
end;

Procedure TQueryHandle.BreakSend;
begin
 FState:=True;
 Node.FErrorCode:=0;
 Node.Parent:=@Finish;
 SendCurrQueue(@Node);
end;

Procedure TQueryHandle.Send;
begin
 if not Assigned(Self) then
 begin
  Log(app_logic,1,['QueryHandle.Send,Assigned(Self):',Self]);
  Exit;
 end;
 if FState then
 begin
  Log(app_logic,1,['QueryHandle.Send,FState:',FState]);
  Exit;
 end;

 Node.Parent:=@Finish;
 Node.FErrorCode:=0;
 Node.FMessage:='';

 if not Assigned(FDbcCon) then
 begin
  Log(app_logic,1,['QueryHandle.Send,Assigned(FDbcCon):',FDbcCon]);
  Finish;
  Exit;
 end;

 if FDbcCon.Send(@Node) then
 begin
  FState:=True;
 end else
 begin
  Log(app_logic,1,['QueryHandle.SendRequest=false']);
  BreakSend;
  Exit;
 end;

end;

Procedure TQueryHandle.Finish;
begin
 FState:=False;
 if not Assigned(OnFinish) then
 begin
  Log(app_logic,1,['QueryHandle.Send,Assigned(OnFinish):',TMethod(OnFinish).Code,TMethod(OnFinish).Data]);
  Exit;
 end;
 Log(app_logic,0,['QueryHandle.Send,OnFinish:',TMethod(OnFinish).Code,TMethod(OnFinish).Data]);

 OnFinish();
end;

Procedure TQueryHandle.Cancel;
begin
 if FState then
 begin
  Node.FOnDbcProc:=nil;
 end;
end;

Function TQueryHandle.isCancel:Boolean; inline;
begin
 Result:=Node.FOnDbcProc=nil;
end;

Procedure TQueryHandle.SetProc(P:TOnDbcProc);
begin
 if not FState then
 begin
  Node.FOnDbcProc:=P;
 end;
end;

Function TQueryHandle.GetMessage:RawByteString;
begin
 Result:='';
 if FState then Exit;
 Result:=Node.FMessage;
end;

Constructor TDbcQueryTask.Create;
begin
 inherited;
 FHandle:=TQueryHandle.Create;
end;

Procedure TDbcQueryTask.Cleanup;
begin
 FHandle.Free;
 inherited;
end;

function TDbcQueryTask.OnFinish:SizeInt;
begin
 Result:=R_SUC;
 if not FHandle.isCancel then
 begin
  if FHandle.ErrorCode=0 then
  begin
   Notify.Call(T_SUC);
  end else
  begin
   Notify.Call(T_ERR);
  end;
 end;
 Stop;
end;

function TDbcQueryTask.Start:SizeInt;
begin
 Result:=R_ERR;
 if not FHandle.State then Result:=inherited;
 if Result=R_SUC then
 begin
  FHandle.OnFinish:=@OnFinish;
  FHandle.Send;
 end;
end;

function TDbcQueryTask.Pause:SizeInt;
begin
 if FHandle.State then
 begin
  FHandle.OnFinish:=@Self.Pause;
  FHandle.Cancel;
  Result:=R_ASY;
 end else
 begin
  Result:=inherited;
 end;
end;

function TDbcQueryTask.Stop:SizeInt;
begin
 if FHandle.State then
 begin
  FHandle.OnFinish:=@Self.Stop;
  FHandle.Cancel;
  Result:=R_ASY;
 end else
 begin
  Result:=inherited;
 end;
end;

///////

Function TQueryHandle.ZConnection:IZConnection;
begin
 Result:=nil;
 if Assigned(DbcConnection) then
 begin
  Result:=DbcConnection.ZConnection;
 end;
end;

Function TQueryHandle.CreateStatement:IZStatement;
Var
 Connection:IZConnection;
begin
 Result:=nil;
 Connection:=ZConnection;
 if Assigned(Connection) then
 begin
  Result:=Connection.CreateStatement;
  Result.SetResultSetConcurrency(rcReadOnly);
  Result.SetFetchDirection(fdForward);
  Result.SetResultSetType(rtForwardOnly);
 end;
end;

Function TQueryHandle.PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;
begin
 Result:=nil;
 if Assigned(FDbcCon) then
 if Assigned(FDbcCon.FThread) then
 begin
  Result:=FDbcCon.FThread.PrepareStatement(FSQL);
  Result.SetResultSetType(rtForwardOnly);
 end;
end;

////

Function TDbcQueryTask.ZConnection:IZConnection;
begin
 Result:=nil;
 if Assigned(FHandle) then
 begin
  Result:=FHandle.ZConnection;
 end;
end;

Function TDbcQueryTask.CreateStatement:IZStatement;
begin
 Result:=nil;
 if Assigned(FHandle) then
 begin
  Result:=FHandle.CreateStatement;
 end;
end;

Function TDbcQueryTask.PrepareStatement(Const FSQL:RawByteString):IZPreparedStatement;
begin
 Result:=nil;
 if Assigned(FHandle) then
  Result:=FHandle.PrepareStatement(FSQL);
end;

//-----

Procedure TDbcStatementTask.OnQuery;
Var
 stmt:IZStatement;
 FR:IZResultSet;
 FIndexPairList:TZIndexPairList;
begin
 FreeAndNil(FRZ);
 stmt:=CreateStatement;
 FR:=stmt.ExecuteQuery(FSQL);
 FRZ:=TZAsyncResultSet.Create(FR);
 FIndexPairList:=_NewIndexPair(FRZ.ColumnsInfo.Count);
 While TZAsyncResultSet(FRZ).Fetch(FR,FIndexPairList) and (not FHandle.isCancel) do;
 FreeAndNil(FIndexPairList);
end;

Procedure TDbcStatementTask.OnUpdate;
Var
 stmt:IZStatement;
begin
 stmt:=CreateStatement;
 FCount:=stmt.ExecuteUpdate(FSQL);
end;

Procedure TDbcStatementTask.OnExe;
Var
 stmt:IZStatement;
 R:Boolean;
begin
 stmt:=CreateStatement;
 R:=stmt.Execute(FSQL);
 FCount:=Integer(R);
end;

Procedure TDbcStatementTask.Cleanup;
begin
 FreeAndNil(FRZ);
 inherited;
end;

Procedure TDbcStatementTask.ExecuteQuery(const SQL:RawByteString);
begin
 if FHandle.State then Exit;
 FSQL:=SQL;
 FHandle.OnDbcProc:=@OnQuery;
 FreeAndNil(FRZ);
end;

Procedure TDbcStatementTask.ExecuteUpdate(const SQL:RawByteString);
begin
 if FHandle.State then Exit;
 FSQL:=SQL;
 FHandle.OnDbcProc:=@OnUpdate;
end;

Procedure TDbcStatementTask.Execute(const SQL:RawByteString);
begin
 if FHandle.State then Exit;
 FSQL:=SQL;
 FHandle.OnDbcProc:=@OnExe;
end;

function TDbcStatementTask.GetResultSet:TZResultSet;
begin
 Result:=nil;
 if FHandle.State then Exit;
 Result:=FRZ;
end;

Procedure TDbcStatementTask.SetResultSet(R:TZResultSet);
begin
 if FHandle.State then Exit;
 FRZ:=R;
end;

function TDbcStatementTask.UpdateCount:Integer;
begin
 Result:=0;
 if FHandle.State then Exit;
 Result:=FCount;
end;

function TDbcStatementTask.MoreResults:Boolean;
begin
 Result:=False;
 if FHandle.State then Exit;
 Result:=Boolean(FCount);
end;

Procedure TDbcBatchTask.OnBatch;
Const
 C_CREATE='CREATE';
Var
 Connection:IZConnection;
 i,L:SizeInt;
 stmt:IZStatement;
begin
 stmt:=nil;
 L:=Length(FBatch);
 if L>0 then
 begin

  Connection:=ZConnection;

  FCount:=0;

  For i:=0 to L-1 do
  begin

   if FHandle.isCancel then Break;

   if not Assigned(stmt) then
   begin
    stmt:=CreateStatement;
   end;

   //Log(app_logic,1,FBatch[i]);
   if UpperCase(Copy(TrimLeft(FBatch[i]),1,Length(C_CREATE)))=C_CREATE then
   begin
    //Log(app_logic,1,C_CREATE);

    if not Connection.GetAutoCommit then
    begin
     Connection.Commit;
     Connection.SetAutoCommit(true);
    end;

    try
     stmt.Execute(FBatch[i]);
    except
     try
      stmt:=nil;
     except
     end;
     stmt:=nil;
    end;

   end else
   begin

    if Connection.GetAutoCommit then
    begin
     Connection.SetAutoCommit(false);
    end;

    try
     FCount:=FCount+stmt.ExecuteUpdate(FBatch[i]);
    except
     try
      stmt:=nil;
     except
     end;
     stmt:=nil;
    end;
   end;

  end;

  if FHandle.isCancel then
  begin
   if not Connection.GetAutoCommit then
   begin
    Connection.Rollback;
    Connection.SetAutoCommit(true);
   end;
  end else
  begin
   if not Connection.GetAutoCommit then
   begin
    Connection.Commit;
    Connection.SetAutoCommit(true);
   end;
  end;

 end;
end;

Procedure TDbcBatchTask.AddSQL(const SQL:RawByteString);
Var
 i:SizeInt;
begin
 if FHandle.State then Exit;
 i:=Length(FBatch);
 SetLength(FBatch,i+1);
 FBatch[i]:=SQL;
 FHandle.OnDbcProc:=@OnBatch;
end;

Procedure TDbcBatchTask.Clear;
begin
 SetLength(FBatch,0);
 FCount:=0;
end;

function TDbcBatchTask.GetCount:Integer;
begin
 Result:=0;
 if FHandle.State then Exit;
 Result:=FCount;
end;

//--------------

Const
 CantLoadClent='Can not load default Firebird clients';
 UnableRequest='Unable to complete network request to host "';
 FailedConnect='Failed to establish a connection';
 UserPasNotDef='Your user name and password are not defined';
 MisingContext='Missing security context for entry';
 NoPremissionf='no permission for';
 ErrorReading ='Error reading data from the connection';
 ErrorWriting ='Error writing data to the connection';
 TransactionER='transaction';
 DynamicSQLERR='Dynamic SQL Error';
 InputERRProc ='Input parameter mismatch for procedure';
 InputErrFunc ='Input parameter mismatch for function';
 ConversionErr='conversion error from string';
 ConnectionRej='connection rejected by remote interface';

Function TranslateFBError(S:RawByteString):RawByteString;
Var
 i,c:Integer;

 Procedure DoTr(R:RawByteString);
 Var
  i:Integer;
  R2:String;
 begin
  i:=Pos(UnableRequest,R);
  if i<>0 then
  begin
   i:=i+Length(UnableRequest);
   R:=Copy(R,i,Length(R)-i+1);
   i:=Pos('"',R);
   if i<>0 then R:=Copy(R,1,i-1);
   R:=' -Не удалось выполнить сетевой запрос по адресу "'+R+'".';
  end;

  i:=Pos(FailedConnect,R);
  if i<>0 then
  begin
   R:=' -Не удалось установить соединение.'
  end;

  i:=Pos(ConnectionRej,R);
  if i<>0 then
  begin
   R:=' -Не удалось установить соединение.'
  end;

  i:=Pos(CantLoadClent,R);
  if i<>0 then
  begin
   R:=' -Не удалось загрузить клиент Firebird';
   {$IFDEF LINUX}
    R:=R+' ("libfbclient.so" или "libgds.so" или "libfbembed.so").';
   {$ENDIF}
   {$IFDEF WINDOWS}
    R:=R+' ("fbclient.dll" или "gds32.dll" или "fbembed.dll").';
   {$ENDIF}
   R:=R+#13' -Проверьте правильность установки программы.';
  end;

  i:=Pos(UserPasNotDef,R);
  if i<>0 then
  begin
   R:=' -Неправильное имя пользователя или пароль.';
     //+#13' -Попросите администратора базы данных настроить учетную запись Firebird.'
  end;

  i:=Pos(ErrorReading,R);
  if i<>0 then
  begin
   R:=' -Ошибка чтения из соединения.'
  end;

  i:=Pos(ErrorWriting,R);
  if i<>0 then
  begin
   R:=' -Ошибка записи в соединение.'
  end;

  i:=Pos(MisingContext,R);
  if i<>0 then
  begin
   R:=' -Не введен пароль для входа.';
  end;

  i:=Pos(DynamicSQLERR,R);
  if i<>0 then
  begin
   R:=' -Динамическая ошибка SQL.';
  end;

  i:=Pos(InputERRProc,R);
  if i<>0 then
  begin
   i:=i+Length(InputERRProc);
   R:=Trim(Copy(R,i,Length(R)-i+1));
   R:=' -Несоответствие входных параметров для процедуры '+R;
  end;

  i:=Pos(InputErrFunc,R);
  if i<>0 then
  begin
   i:=i+Length(InputErrFunc);
   R:=Trim(Copy(R,i,Length(R)-i+1));
   R:=' -Несоответствие входных параметров для функции '+R;
  end;

  i:=Pos(ConversionErr,R);
  if i<>0 then
  begin
   i:=i+Length(ConversionErr);
   R:=Trim(Copy(R,i,Length(R)-i+1));
   R:=' -Ошибка преобразования из строки '+R;
  end;

  i:=Pos(TransactionER,R);
  if i<>0 then
  begin
   R:=' -Ошибка транзакции.';
  end;

  i:=Pos(NoPremissionf,R);
  if i<>0 then
  begin
   i:=i+Length(NoPremissionf);
   R:=Trim(Copy(R,i,Length(R)-i+1));

   i:=Pos(' ',R);
   if i<>0 then
   begin
    R2:=Copy(R,1,i-1);
    R:=Trim(Copy(R,i,Length(R)-i+1));
   end;

   i:=Pos(' ',R);
   if i<>0 then R:=Trim(Copy(R,i,Length(R)-i+1));

   i:=Pos(' ',R);
   if i<>0 then R:=Trim(Copy(R,i,Length(R)-i+1));

   R:=' -Нет доступа к '+R2+' для '+R;
  end;

  Result:=Result+#13+R;
 end;

begin
 Result:='';
 c:=0;
 repeat
  i:=Pos(#13,S);
  if i<>0 then
  begin
   if c>1 then
   begin
    DoTr(Copy(S,1,i-1));
   end;
   Inc(i);
   if S[i]=#10 then Inc(i);
   S:=Copy(S,i,Length(S)-i+1);
  end else
  begin
   DoTr(S);
   Break;
  end;
  Inc(c);
 until false;

 Result:='Ошибка: '+Result;
end;

Procedure _AddOpt1(Var R:RawByteString;Const O:RawByteString);
begin
 if Trim(O)<>'' then
  R:=R+O+' ';
end;

Procedure _AddOpt2(Var R:RawByteString;Const O:RawByteString);
begin
 if Trim(O)<>'' then
  R:=R+' '+O;
end;

Procedure _AddNames(Var R:RawByteString;Const Defs:TFields;Quote:AnsiChar);
Var
 i:SizeInt;
begin
 if Length(Defs.Data)>0 then
 For i:=0 to High(Defs.Data) do
 begin
  if Quote=#0 then
  begin
   R:=R+Defs.Data[i].Name;
  end else
  begin
   R:=R+SQLQuotedStr(Defs.Data[i].Name,'"');
  end;
  if i<>High(Defs.Data) then R:=R+',';
 end;
end;

Procedure _AddValues(Var R:RawByteString;Const Defs:TFields);
Var
 i:SizeInt;
begin
 if Length(Defs.Data)>0 then
 For i:=0 to High(Defs.Data) do
 begin
  R:=R+Defs.Data[i].Value;
  if i<>High(Defs.Data) then R:=R+',';
 end;
end;

Procedure _AddNames_Values(Var R:RawByteString;Const Defs:TFields;Quote,D:AnsiChar);
Var
 i:SizeInt;
begin
 if Length(Defs.Data)>0 then
 For i:=0 to High(Defs.Data) do
 begin
  if Quote=#0 then
  begin
   R:=R+Defs.Data[i].Name;
  end else
  begin
   R:=R+SQLQuotedStr(Defs.Data[i].Name,'"');
  end;
  R:=R+D;
  R:=R+Defs.Data[i].Value;
  if i<>High(Defs.Data) then R:=R+',';
 end;
end;

function insert_into_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
begin
 Result:='';
 if (Length(Defs.Data)>0) and (TableName<>'') then
 begin
  Result:='INSERT INTO ';
  _AddOpt1(Result,Options);
  Result:=Result+SQLQuotedStr(TableName,'"')+' (';
  _AddNames(Result,Defs,'"');
  Result:=Result+') VALUES (';
  _AddValues(Result,Defs);
  Result:=Result+');';
 end;
end;

function insert_or_ignore_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
begin
 Result:='';
 if (Length(Defs.Data)>0) and (TableName<>'') then
 begin
  Result:='INSERT OR IGNORE INTO ';
  _AddOpt1(Result,Options);
  Result:=Result+SQLQuotedStr(TableName,'"')+' (';
  _AddNames(Result,Defs,'"');
  Result:=Result+') VALUES (';
  _AddValues(Result,Defs);
  Result:=Result+');';
 end;
end;

function replace_into_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
begin
 Result:='';
 if (Length(Defs.Data)>0) and (TableName<>'') then
 begin
  Result:='REPLACE INTO ';
  _AddOpt1(Result,Options);
  Result:=Result+SQLQuotedStr(TableName,'"')+' (';
  _AddNames(Result,Defs,'"');
  Result:=Result+') VALUES (';
  _AddValues(Result,Defs);
  Result:=Result+');';
 end;
end;

function select_sql(Const TableName,Opt1,Opt2:RawByteString;Const Defs:TFields):RawByteString;
begin
 Result:='';
 if (Length(Defs.Data)>0) and (TableName<>'') then
 begin
  Result:='SELECT ';
  _AddOpt1(Result,Opt1);
  _AddNames(Result,Defs,#0);
  Result:=Result+' FROM '+SQLQuotedStr(TableName,'"');
  _AddOpt2(Result,Opt2);
  Result:=Result+';';
 end;
end;

function update_sql(Const TableName,Opt1,Opt2:RawByteString;Const Defs:TFields):RawByteString;
begin
 Result:='';
 if (Length(Defs.Data)>0) and (TableName<>'') then
 begin
  Result:='UPDATE ';
  _AddOpt1(Result,Opt1);
  Result:=Result+SQLQuotedStr(TableName,'"')+' SET ';
  _AddNames_Values(Result,Defs,'"','=');
  _AddOpt2(Result,Opt2);
  Result:=Result+';';
 end;
end;

function delete_sql(Const TableName,Opt1,Opt2:RawByteString):RawByteString;
begin
 Result:='';
 if (TableName<>'') then
 begin
  Result:='DELETE FROM ';
  _AddOpt1(Result,Opt1);
  Result:=Result+SQLQuotedStr(TableName,'"');
  _AddOpt2(Result,Opt2);
  Result:=Result+';';
 end;
end;

function where_sql(Const Defs:TFields):RawByteString;
Var
 i:SizeInt;
begin
 Result:='';
 if (Length(Defs.Data)>0) then
 begin
  Result:='WHERE ';

   For i:=0 to High(Defs.Data) do
   begin
    if i<>0 then
    begin
     if Trim(Defs.Data[i].Name)='' then
     begin
      Result:=Result+' AND';
     end else
     begin
      Result:=Result+' '+Trim(Defs.Data[i].Name);
     end;
     Result:=Result+' ('+Defs.Data[i].Value+')';
    end else
    begin
     Result:=Result+'('+Defs.Data[i].Value+')';
    end;
   end;

 end;
end;

function create_table_sql(Const TableName,Options:RawByteString;Const Defs:TFields):RawByteString;
begin
 Result:='';
 if (Length(Defs.Data)>0) and (TableName<>'') then
 begin
  Result:='CREATE TABLE ';
  _AddOpt1(Result,Options);
  Result:=Result+SQLQuotedStr(TableName,'"')+' (';
   _AddNames_Values(Result,Defs,'"',' ');
  Result:=Result+');';
 end;
end;

function create_index_sql(Const TableName,Options,IndexName,Colums:RawByteString):RawByteString;
begin
 Result:='';
 if (TableName='') or (IndexName='') or (Colums='')  then Exit;
 Result:='CREATE INDEX ';
 _AddOpt1(Result,Options);
 Result:=Result+SQLQuotedStr(IndexName,'"')
               +' ON '+SQLQuotedStr(TableName,'"')
               +' ( '+Colums+' );';
end;

function create_unique_index_sql(Const TableName,Options,IndexName,Colums:RawByteString):RawByteString;
begin
 Result:='';
 if (TableName='') or (IndexName='') or (Colums='')  then Exit;
 Result:='CREATE UNIQUE INDEX ';
 _AddOpt1(Result,Options);
 Result:=Result+SQLQuotedStr(IndexName,'"')
               +' ON '+SQLQuotedStr(TableName,'"')
               +' ( '+Colums+' );';
end;

function SpaceToNull(Const S:RawByteString):RawByteString; inline;
begin
 if S='' then Result:='NULL' else Result:=S;
end;

function SpaceToNullQuoted(Const S:RawByteString):RawByteString; inline;
begin
 if S='' then Result:='NULL' else Result:=SQLQuotedStr(S,'''');
end;

function QuotedStr(Const S:RawByteString):RawByteString; inline;
begin
 Result:=SQLQuotedStr(S,'''');
end;

initialization
 rwlock_init(GlobalGetConnLock);
 LogListen:=TLogListen.Create;
 DriverManager.AddLoggingListener(LogListen);

finalization
 DriverManager.RemoveLoggingListener(LogListen);

end.

