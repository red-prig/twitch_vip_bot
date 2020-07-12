{ Async exchange Queue

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

unit UAsyncQueue;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,LFQueue;

type
 TOnParent=Procedure of object;
 PQNode=^TQNode;
 TQNode=object
  private
   next_:PQNode;
  public
   Parent:TOnParent;
 end;

 TCustomQueue=class
  protected
   FCount:PtrUInt;
  public
   Function  Send(Node:Pointer):Boolean;     virtual; abstract;
   Function  Recv(Var Node:Pointer):Boolean; virtual; abstract;
   Function  IsEmpty:Boolean;                virtual; abstract;
   Function  Count:PtrUInt;                  inline;
   procedure OnUpdate;
   procedure _OnUpdate(AData:PtrInt;AFlags:dword);
   procedure _OnUpdate(Sender:TObject;var Done:Boolean);
 end;

 TNodeQueue=class(TCustomQueue)
  protected
   _Q:TIntrusiveMPSCQueue;
  public
   Constructor Create;
   Function    Send(Node:Pointer):Boolean;     override;
   Function    Recv(Var Node:Pointer):Boolean; override;
   Function    IsEmpty:Boolean;                override;
 end;

 TNodeQueueEvent=class(TNodeQueue)
  protected
   FhEvent:pRTLEvent;
  public
   Constructor Create;
   Destructor  Destroy;                    override;
   Function    Send(Node:Pointer):Boolean; override;
   property    hEvent:pRTLEvent read FhEvent;
 end;

 TNodeQueueSharedEvent=class(TNodeQueue)
  protected
   FhEvent:pRTLEvent;
  public
   Function    Send(Node:Pointer):Boolean; override;
   property    hEvent:pRTLEvent read FhEvent write FhEvent;
 end;

 TSimpleQueue=class(TCustomQueue)
  private
   type
    PNode=^TNode;
    TNode=record
     pNext:PNode;
     //some data
    end;
    Var
     pHead,pTail:PNode;
   public
    Constructor Create;
    Function    Send(Node:Pointer):Boolean;     override;
    Function    Recv(Var Node:Pointer):Boolean; override;
    Function    IsEmpty:Boolean;                override;
 end;

 PQFrom2Node=^TQFrom2Node;
 TQFrom2Node=object(TQNode)
  protected
   FQueue:TCustomQueue;
  public
 end;

 TCustomAsyncThread=class
  Const
   RtlWaitTime=10*1000;
  private
   hThread:TThreadID;
  protected
   FState:Boolean;
   FQueue:TNodeQueueEvent;
   FCmdQueue:TNodeQueueSharedEvent;
   FCreator:TCustomQueue;

   Procedure   InherBegin;  virtual;
   Procedure   InherUpdate; virtual;
   Procedure   InherEnd;    virtual;

  public
   property    State:Boolean read FState;

   Constructor Create;
   Destructor  Destroy; override;
   function    Send(Node:PQFrom2Node):Boolean;    virtual;
   function    SendCmd(Node:PQFrom2Node):Boolean; virtual;
   procedure   SyncFunc(N:TOnParent);             virtual;
 end;

function SendMainQueue(Node:Pointer):Boolean;
function SendCurrQueue(Node:Pointer):Boolean;
function SendFrom(Node:PQFrom2Node):Boolean;

Var
 MainQueue:TCustomQueue;

threadvar
 CurrQueue:TCustomQueue;

implementation

Uses ULog;

function SendMainQueue(Node:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(MainQueue) then
 begin
  Result:=MainQueue.Send(Node);
 end;

 if not Result then raise exception.create('SendMainQueue');
end;

function SendCurrQueue(Node:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(CurrQueue) then
 begin
  Result:=CurrQueue.Send(Node);
 end;

 if not Result then raise exception.create('SendCurrQueue');
end;

function SendFrom(Node:PQFrom2Node):Boolean;
begin
 Result:=False;

 if Assigned(Node) then
 if Assigned(Node^.FQueue) then
 begin
  Result:=Node^.FQueue.Send(Node);
 end;

 if not Result then Log(app_logic,1,['Node(',Node,')^.SendFrom:',Node^.FQueue]);
end;

function CustomAsyncThread_Execute(RT:TCustomAsyncThread):ptrint; forward;

Constructor TCustomAsyncThread.Create;
begin
 FState:=False;
 FQueue:=TNodeQueueEvent.Create;
 FCmdQueue:=TNodeQueueSharedEvent.Create;
 FCmdQueue.hEvent:=FQueue.hEvent;
 FCreator:=CurrQueue;

 hThread:=BeginThread(tthreadfunc(@CustomAsyncThread_Execute),Self);
 if hThread=0 then
 begin
  FState:=True;
  Log(app_logic,1,['BeginThread(',ClassName,'):',hThread]);
 end;
end;

Destructor TCustomAsyncThread.Destroy;
begin
 if (not (FState)) or (hThread<>0) then
 begin
  FState:=True;
  RTLeventSetEvent(FQueue.hEvent);
  WaitForThreadTerminate(hThread,-1);
  CloseThread(hThread);
 end;
 FreeAndNil(FQueue);
 FreeAndNil(FCmdQueue);
 inherited;
end;

function TCustomAsyncThread.Send(Node:PQFrom2Node):Boolean;
begin
 Result:=False;
 if Assigned(Self) then
 begin
  if FState then Exit;
  if Assigned(Node) then
  begin
   Node^.FQueue:=UAsyncQueue.CurrQueue;
   Result:=FQueue.Send(Node);
  end else
  begin
   Log(app_logic,1,['Assigned(Node):',Node]);
  end;
 end else
 begin
  Log(app_logic,1,['Assigned(CustomAsyncThread):',Self]);
 end;
end;

function TCustomAsyncThread.SendCmd(Node:PQFrom2Node):Boolean;
begin
 Result:=False;
 if Assigned(Self) then
 begin
  if FState then Exit;
  if Assigned(Node) then
  begin
   Node^.FQueue:=UAsyncQueue.CurrQueue;
   Result:=FCmdQueue.Send(Node);
  end else
  begin
   Log(app_logic,1,['Assigned(Node):',Node]);
  end;
 end else
 begin
  Log(app_logic,1,['Assigned(CustomAsyncThread):',Self]);
 end;
end;

type
 PSyncFuncNode=^TSyncFuncNode;
 TSyncFuncNode=object(UAsyncQueue.TQNode)
  N:TOnParent;
  Procedure Call;
 end;

Procedure TSyncFuncNode.Call;
begin
 if Assigned(N) then N();
 FreeMem(@Self);
end;

procedure TCustomAsyncThread.SyncFunc(N:TOnParent);
Var
 Node:PSyncFuncNode;
begin
 if Assigned(N) and Assigned(FCreator) then
 begin
  Node:=AllocMem(SizeOf(TSyncFuncNode));
  Node^:=Default(TSyncFuncNode);
  Node^.Parent:=@Node^.Call;
  Node^.N:=N;
  FCreator.Send(Node);
 end;
end;

Procedure TCustomAsyncThread.InherBegin;
begin
 CurrQueue:=FCmdQueue;
 if not Assigned(Self) then
 begin
  Log(app_logic,1,['Assigned(Thread):',Self]);
  FState:=True;
 end;
end;

Procedure TCustomAsyncThread.InherUpdate;
begin
 if FQueue.IsEmpty then
 begin
  RTLeventWaitFor(FQueue.hEvent,RtlWaitTime);
 end;
end;

Procedure TCustomAsyncThread.InherEnd;
begin
end;

function CustomAsyncThread_Execute(RT:TCustomAsyncThread):ptrint;
begin
 Result:=0;
 RT.InherBegin;

 if not RT.FState then
 repeat
  try

   RT.InherUpdate;

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 until RT.FState;

 RT.InherEnd;
end;

///////////////

Constructor TNodeQueue.Create;
begin
 inherited;
 _Q.Create;
end;

Function TNodeQueue.Send(Node:Pointer):Boolean;
begin
 Result:=_Q.Push(Node);
 if Result then
 begin
  System.InterLockedIncrement(Pointer(FCount));
 end;
end;

Function TNodeQueue.Recv(Var Node:Pointer):Boolean;
begin
 Result:=_Q.Pop(Node);
 if Result then
 begin
  System.InterLockedDecrement(Pointer(FCount));
 end;
end;

Function TNodeQueue.IsEmpty:Boolean;
begin
 Result:=_Q.IsEmpty;
end;

Function TCustomQueue.Count:PtrUInt;
begin
 Result:=FCount;
end;

procedure TCustomQueue.OnUpdate;
Var
 rc:PtrUInt;
 Node:PQNode;
begin
 if not Assigned(Self) then Exit;
 Node:=nil;
 rc:=Count;
 try
  While Recv(Node) do
  begin
   if Assigned(Node^.Parent) then
   begin
    Node^.Parent();
   end else
   begin
    Log(app_logic,1,['Assigned(Node^.Parent):',TMethod(Node^.Parent).Code,TMethod(Node^.Parent).Data]);
   end;
   if rc=0 then Break;
   Dec(rc);
   if rc=0 then Break;
  end;
 except
  on E:Exception do
   DumpExceptionCallStack(E);
 end;
end;

procedure TCustomQueue._OnUpdate(AData:PtrInt;AFlags:dword);
begin
 OnUpdate;
end;

procedure TCustomQueue._OnUpdate(Sender:TObject;var Done:Boolean);
begin
 Done:=False;
 OnUpdate;
end;

Constructor TNodeQueueEvent.Create;
begin
 inherited;
 FhEvent:=RTLEventCreate;
end;

Destructor TNodeQueueEvent.Destroy;
begin
 RTLEventDestroy(FhEvent);
 inherited;
end;

Function TNodeQueueEvent.Send(Node:Pointer):Boolean;
begin
 Result:=inherited Send(Node);
 if Result then
 begin
  RTLEventSetEvent(FhEvent);
 end;
end;

////TNodeQueueSharedEvent

Function TNodeQueueSharedEvent.Send(Node:Pointer):Boolean;
begin
 Result:=inherited Send(Node);
 if Result and Assigned(FhEvent) then
 begin
  RTLEventSetEvent(FhEvent);
 end;
end;

//TSimpleQueue

Constructor TSimpleQueue.Create;
begin
 pHead:=nil;
 pTail:=nil;
end;

Function  TSimpleQueue.Send(Node:Pointer):Boolean;
begin
 if not Assigned(Node) then Exit(False);
 if pTail=nil then
 begin
  pHead:=Node;
 end else
 begin
  pTail^.pNext:=Node;
 end;
 pTail:=Node;
 pTail^.pNext:=nil;
 Result:=True;
 Inc(FCount);
end;

Function  TSimpleQueue.Recv(Var Node:Pointer):Boolean;
begin
 Node:=pHead;
 Result:=Node<>nil;
 if Result then
 begin
  pHead:=PNode(Node)^.pNext;
  PNode(Node)^.pNext:=nil;
  if pHead=nil then
   pTail:=nil;
  Dec(FCount);
 end;
end;

Function  TSimpleQueue.IsEmpty:Boolean;
begin
 Result:=pTail=nil;
end;

initialization
 MainQueue:=nil;
 CurrQueue:=nil;

finalization
 FreeAndNil(MainQueue);

end.

