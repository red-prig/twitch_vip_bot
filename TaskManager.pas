{ Async Task manager

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

unit TaskManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,Classes,gset,UAsyncQueue,ULog{,mtHeap};

Const
 T_NEW=0;
 T_RUN=1;
 T_PAU=2;
 T_FIN=3;
 T_DEL=4;

 T_SUC=5;
 T_ERR=6;

 T_INF=7;

 T_ADD=8;
 T_REM=9;

 T_LDR=10;
 T_LDE=11;

 R_SUC=0;
 R_ERR=-1;
 R_ASY=1;
 //R_RST=2;

type
 TBaseTask=class;
 TProtoTask=class of TBaseTask;

 TNotifyTask=Procedure(Sender:TBaseTask) of object;
 TFuncEvent =function:SizeInt of object;

 PObserver=^TObserver;
 TObserver=object
  private
   Var
    FPos:SizeInt;
    Data:array of TNotifyTask;
  public
   Tag:SizeInt;
  function  IndexOf(N:TNotifyTask):SizeInt;
  function  Add(N:TNotifyTask):Boolean;
  function  Remove(N:TNotifyTask):Boolean;
  Procedure Remove; inline;
  function  New(State:SizeUInt):PObserver; static;
  Procedure Free;
  function  Call(FSender:TBaseTask):SizeInt;
  Procedure Abort; inline;
  function  Count:SizeInt; inline;
 end;

 TObserverList=object
  private
   Var
    Data:array of PObserver;
  public
   function  IndexOf(Tag:SizeInt):SizeInt;
   Procedure Add(Tag:SizeInt;N:TNotifyTask);
   Function  Call(Tag:SizeInt;Sender:TBaseTask):SizeInt;
   function  Count(Tag:SizeInt):SizeInt;
   Procedure Free;
 end;

 TNotify=object
  private
   Root:TBaseTask;
  public
   Procedure Add(Tag:SizeInt;N:TNotifyTask);
   Procedure Remove(Tag:SizeInt;N:TNotifyTask);
   Procedure Remove(N:TNotifyTask);
   Procedure Remove(Tag:SizeInt);
   Procedure Clear;
   Procedure Abort;
   Function  Call(Tag:SizeInt):SizeInt;
   Function  Call(Tag:SizeInt;Sender:TBaseTask):SizeInt;
   function  Count(Tag:SizeInt):SizeInt;
 end;

 {TJob=object
  private
   type
    Tdeferred=record
     onFul,onRej:TFuncEvent;
    end;
   Var
    Fstate,FPos:SizeInt;
    FData:array of Tdeferred;
  public
    Tag:SizeInt;
   function    State:SizeInt;inline;
   Procedure   Clear;        inline;
   Procedure   Reset;        inline;
   Procedure   Throw;        inline;
   function    Call:SizeInt; inline;
   function    Add(onFul,onRej:TFuncEvent):SizeInt;
 end;

 TJobList=object
  private
   FTagi:SizeInt;
   Data:array of TJob;
   function  IndexOf(Tag:SizeInt):SizeInt;
  public
   Procedure Clear; inline;
   procedure Add(Tag:SizeInt;onFul,onRej:TFuncEvent);
   function  Call(Tag:SizeInt):SizeInt; inline;
   procedure Throw(Tag:SizeInt); inline;
 end;}

 TBaseTask=class
  private
   FState:SizeInt;
   FRef:SizeUInt;
   FInher:SizeUInt;
   FOBS:TObserverList;
  protected
   Procedure OnState; inline;
   procedure AsyncFunc(N:TFuncEvent);
   procedure AsyncNotify(S:SizeInt);
   procedure AsyncNotify(S:SizeInt;Sender:TBaseTask);
   Procedure InherInc; inline;
   Procedure InherDec; inline;
   Function  BeforeStop:SizeInt;  virtual;
   Function  BeforeStart:SizeInt; virtual;
   Function  BeforePause:SizeInt; virtual;
   Function  GetState:SizeInt;
  published
   property  State:SizeInt  read GetState;
   property  Ref:SizeUInt   read FRef;
   property  Inher:SizeUInt read FInher;
  public

   FOnState:TNotifyTask; static;
   //Tag:SizeUInt;

   Procedure   Acquire;         inline;
   function    Release:Boolean; inline;
   procedure   FreeInstance;    override;

   function    Notify:TNotify; inline;

   Constructor Create;        virtual;
   Procedure   Cleanup;       virtual;

   Function    Pause:SizeInt; virtual;
   Function    Start:SizeInt; virtual;
   Function    Stop:SizeInt;  virtual;

   Procedure   AsyncStart;
   Procedure   AsyncPause;
   Procedure   AsyncStop;

   Procedure   DoEnd(Tag:SizeInt); inline;
 end;

 TTaskCompare=class
  class function c(a,b:TBaseTask):boolean; static;
 end;

 TTaskSet=specialize TSet<TBaseTask,TTaskCompare>;

 TTaskList=class(TBaseTask)
  private
   LWork:TTaskSet;
   LI:TTaskSet.TIterator;
   function    _Next:Boolean;
   Procedure   _Prev(N:TBaseTask); inline;
  protected
   function    GetCount:SizeUInt; inline;
  published
   property    Count:SizeUInt read GetCount;
  public
   Constructor Create;        override;
   Procedure   Abort;
   //Procedure   ForEachDo(S:SizeInt);
   Procedure   ForEachDo(A:array of SizeInt);
   Procedure   ForEachNotify(S:SizeInt);
   Procedure   ForEachCall(N:TNotifyTask);
   Procedure   Cleanup;       override;
   Procedure   Add(N:TBaseTask);    virtual;
   Procedure   Remove(N:TBaseTask); virtual;
   function    Item(i:SizeInt):TBaseTask;
 end;

 TQueryList=class(TTaskList)
  //private
   //Procedure OnRemove(Sender:TBaseTask);
  public
   Procedure Add(N:TBaseTask);    override;
   Procedure Remove(N:TBaseTask); override;
   Function  BeforeStop:SizeInt;  override;
 end;

 TQuerySuccused=class(TTaskList)
  private
   //Procedure _RemNotify(Sender:TBaseTask);
   //Procedure OnRem(Sender:TBaseTask);
   Procedure OnSuc(Sender:TBaseTask);
   Procedure OnErr(Sender:TBaseTask);
   Procedure _Remove(N:TBaseTask);
  public
   Procedure Add(N:TBaseTask);    override;
   Procedure Remove(N:TBaseTask); override;
   Function  BeforeStop:SizeInt;  override;
 end;

 TNotifyGroup=object
  private
   type
    TN=record
     T:SizeInt;
     N:TNotifyTask;
    end;
   Var
    Data:array of TN;
    FTask:TBaseTask;
   function  IndexOf(Tag:SizeInt;N:TNotifyTask):SizeInt;
   Procedure DoAdd(Const N:TN);
   Procedure DoRem(Const N:TN);
  public
   function  Add(Tag:SizeInt;N:TNotifyTask):Boolean;
   function  Remove(Tag:SizeInt;N:TNotifyTask):Boolean;
   function  Remove(N:TNotifyTask):Boolean;
   function  Remove(Tag:SizeInt):Boolean;
   Procedure Clear;
   Procedure Bind(Sender:TBaseTask);
   Procedure UnBind(Sender:TBaseTask);
 end;

 TTaskLink=object
  private
   Procedure OnDel(Sender:TBaseTask);
  public
   Var
    Notify:TNotifyGroup;
    FTask:TBaseTask;

  Procedure   Reset;
  function    Create(Proto:TProtoTask):Boolean;
  Procedure   Free;
  Procedure   Start;
  Procedure   Pause;
  Procedure   Stop;

  Function    State:SizeInt; inline;

  //Procedure   AsyncStart;
  //Procedure   AsyncPause;
  //Procedure   AsyncStop;
 end;

 TQueryListLink=object(TTaskLink)
  public
   Procedure   Create; reintroduce;
   Procedure   Abort;
   Procedure   ForEachDo(S:SizeInt);
   Procedure   ForEachNotify(S:SizeInt);
   Procedure   ForEachCall(N:TNotifyTask);
   Procedure   Add(N:TBaseTask);
   Procedure   Remove(N:TBaseTask);
   function    Item(i:SizeInt):TBaseTask;
   function    Count:SizeUInt;
 end;

procedure SyncFunc(N:TFuncEvent);
procedure SyncFunc(N:TOnParent);

implementation

class function TTaskCompare.c(a,b:TBaseTask):boolean; inline;
begin
 Result:=System.CompareByte(a,b,SizeOf(TBaseTask))>0;
end;

type
 PAsyncFuncNode=^TAsyncFuncNode;
 TAsyncFuncNode=object(UAsyncQueue.TQNode)
  N:TFuncEvent;
  Procedure Call;
 end;

 PSyncFuncNodeEvent=^TSyncFuncNodeEvent;
 TSyncFuncNodeEvent=object(UAsyncQueue.TQNode)
  N:TFuncEvent;
  Procedure Call;
 end;

 PSyncFuncNode=^TSyncFuncNode;
 TSyncFuncNode=object(UAsyncQueue.TQNode)
  N:TOnParent;
  Procedure Call;
 end;

 PAsyncNotifyNode=^TAsyncNotifyNode;
 TAsyncNotifyNode=object(UAsyncQueue.TQNode)
  R:TBaseTask;
  S:SizeInt;
  Procedure Call;
 end;

 PAsyncNotifyNode2=^TAsyncNotifyNode2;
 TAsyncNotifyNode2=object(UAsyncQueue.TQNode)
  R,C:TBaseTask;
  S:SizeInt;
  Procedure Call;
 end;

Procedure TBaseTask.Acquire; inline;
begin
 if FRef<>High(FRef) then FRef:=FRef+1;
 //Writeln('FRef+:',FRef,' ',Self.ClassName,' ($',hexstr(Self),')');
end;

function TBaseTask.Release:Boolean; inline;
begin
 if FRef<>0 then FRef:=FRef-1;
 Result:=FRef=0;
 //Writeln('FRef-:',FRef,' ',Self.ClassName,' ($',hexstr(Self),')');
end;

Procedure TBaseTask.InherInc; inline;
begin
 if FInher<>High(FInher) then FInher:=FInher+1;
 //Writeln('InherInc',FInher);
end;

Procedure TBaseTask.InherDec; inline;
begin
 if FInher<>0 then FInher:=FInher-1;
 //Writeln('InherDec',FInher);
end;

procedure TBaseTask.FreeInstance;
begin
 if Release then
 if FState<>T_RUN then
 begin
  Cleanup;
  inherited;
 end;
end;

Procedure TBaseTask.OnState;
begin
 if Assigned(FOnState) then FOnState(Self);
end;

function TBaseTask.Notify:TNotify; inline;
begin
 Result:=Default(TNotify);
 if Assigned(Self) then
 begin
  Result.Root:=Self;
 end;
end;

function TNotify.Call(Tag:SizeInt):SizeInt;
begin
 Log(app_logic,2,[Root,':TNotify.Call:',Tag]);
 Result:=0;
 if Assigned(Root) then
 begin
  if Root.FInher=0 then
  begin
   Root.Acquire;
   Root.InherInc;
   Result:=Root.FOBS.Call(Tag,Root);
   Root.InherDec;
   Root.Release;
  end else
  begin
   Result:=-1;
   Root.AsyncNotify(Tag);
   //Writeln('Inher Notify:',ClassName,' ',State);
  end;
 end;
end;

function TNotify.Call(Tag:SizeInt;Sender:TBaseTask):SizeInt;
begin
 Log(app_logic,2,[Root,':TNotify.Call:',Tag,':',Sender]);
 if Assigned(Root) then
 if Assigned(Sender) then
 begin
  if Root.FInher=0 then
  begin
   Root.Acquire;
   Root.InherInc;
   Sender.Acquire;
   Result:=Root.FOBS.Call(Tag,Sender);
   Sender.Release;
   Root.InherDec;
   Root.Release;
  end else
  begin
   Result:=-1;
   Root.AsyncNotify(Tag,Sender);
   //Writeln('Inher Notify:',ClassName,' ',State);
  end;
 end;
end;

function TNotify.Count(Tag:SizeInt):SizeInt;
begin
 Result:=0;
 if Assigned(Root) then
 begin
  Result:=Root.FOBS.Count(Tag);
 end;
end;

function TNotifyGroup.IndexOf(Tag:SizeInt;N:TNotifyTask):SizeInt;
Var
 i:SizeInt;
begin
 Result:=-1;
 if Length(Data)>0 then
  For i:=0 to High(Data) do
   if CompareByte(Data[i].T,Tag,SizeOf(SizeInt))=0 then
   if CompareByte(Data[i].N,N,SizeOf(TNotifyTask))=0 then
    Exit(i);
end;

Procedure TNotifyGroup.DoAdd(Const N:TN);
begin
 if Assigned(FTask) then
 begin
  FTask.Notify.Add(N.T,N.N);
 end;
end;

Procedure TNotifyGroup.DoRem(Const N:TN);
begin
 if Assigned(FTask) then
 begin
  FTask.Notify.Remove(N.T,N.N);
 end;
end;

function TNotifyGroup.Add(Tag:SizeInt;N:TNotifyTask):Boolean;
Var
 i:SizeInt;
begin
 Result:=Length(Data)=0;
 if Assigned(N) then
 begin
  i:=IndexOf(Tag,N);
  if i=-1 then
  begin
   i:=Length(Data);
   SetLength(Data,i+1);
   Data[i].T:=Tag;
   Data[i].N:=N;
   DoAdd(Data[i]);
  end;
 end;
end;

function TNotifyGroup.Remove(Tag:SizeInt;N:TNotifyTask):Boolean;
Var
 i:SizeInt;
begin
 if Assigned(N) then
 begin
  i:=IndexOf(Tag,N);
  if i<>-1 then
  begin
   DoRem(Data[i]);
   if i<High(Data) then
   begin
    System.Move(Data[i+1],Data[i],(High(Data)-i)*SizeOf(TN));
   end;
   SetLength(Data,High(Data));
  end;
 end;
 Result:=Length(Data)=0;
end;

function TNotifyGroup.Remove(N:TNotifyTask):Boolean;
Var
 i:SizeInt;
begin
 if Assigned(N) then
 begin
  i:=0;
  While (i<Length(Data)) do
  begin
   if CompareByte(Data[i].N,N,SizeOf(TNotifyTask))=0 then
   begin
    DoRem(Data[i]);
    if i<High(Data) then
    begin
     System.Move(Data[i+1],Data[i],(High(Data)-i)*SizeOf(TN));
    end;
    SetLength(Data,High(Data));
   end;
   i:=i+1;
  end;
 end;
 Result:=Length(Data)=0;
end;

function TNotifyGroup.Remove(Tag:SizeInt):Boolean;
Var
 i:SizeInt;
begin
 i:=0;
 While (i<Length(Data)) do
 begin
  if CompareByte(Data[i].T,Tag,SizeOf(SizeInt))=0 then
  begin
   DoRem(Data[i]);
   if i<High(Data) then
   begin
    System.Move(Data[i+1],Data[i],(High(Data)-i)*SizeOf(TN));
   end;
   SetLength(Data,High(Data));
  end;
  i:=i+1;
 end;
 Result:=Length(Data)=0;
end;

Procedure TNotifyGroup.Clear;
Var
 i:SizeInt;
begin
 if Length(Data)>0 then
 begin
  For i:=0 to High(Data) do
  begin
   DoRem(Data[i]);
  end;
  SetLength(Data,0);
 end;
end;

Procedure TNotifyGroup.Bind(Sender:TBaseTask);
Var
 i:SizeInt;
begin
 FTask:=Sender;
 if Assigned(Sender) then
 begin
  if Length(Data)>0 then
   For i:=0 to High(Data) do
   begin
    Sender.Notify.Add(Data[i].T,Data[i].N);
   end;
 end;
end;

Procedure TNotifyGroup.UnBind(Sender:TBaseTask);
Var
 i:SizeInt;
begin
 if Assigned(Sender) then
 begin
  if Length(Data)>0 then
   For i:=0 to High(Data) do
   if Data[i].T<>T_DEL then
   begin
    Sender.Notify.Remove(Data[i].T,Data[i].N);
   end;
 end;
 FTask:=nil;
end;


Procedure TTaskLink.OnDel(Sender:TBaseTask);
begin
 //Writeln('OnDel:',Sender.ClassName);
 if Sender=FTask then FTask:=nil;
end;

Procedure TTaskLink.Reset;
begin
 if Assigned(FTask) then
 begin
  Case FTask.State of
   T_RUN,T_PAU:
   begin
    if FTask.Stop<>T_SUC then
    begin
     Notify.UnBind(FTask);
     //FTask.Notify.Remove(T_DEL,@OnDel);
     FreeAndNil(FTask);
     //Writeln('Reset:');
    end;
   end;
  end;
 end;
end;

function TTaskLink.Create(Proto:TProtoTask):Boolean;
begin
 Result:=False;

 Reset;

 if not Assigned(FTask) then
 begin
  FTask:=Proto.Create;
  FTask.Notify.Add(T_DEL,@OnDel);
  Notify.Bind(FTask);
  //FTask.Notify.Call(T_NEW);
  //Writeln('New:');
  Result:=True;
 end;

end;

Procedure TTaskLink.Free;
begin
 if Assigned(FTask) then
 begin
  //FTask.Notify.Remove(T_DEL,@OnDel);
  FTask.Stop;
  Notify.UnBind(FTask);
  FreeAndNil(FTask);
 end;
end;

Procedure TTaskLink.Start;
begin
 if Assigned(FTask) then
 begin
  //Notify.Bind(FTask);
  FTask.Start;
 end;
end;

Procedure TTaskLink.Pause;
begin
 if Assigned(FTask) then
 begin
  FTask.Pause;
 end;
end;

Procedure TTaskLink.Stop;
begin
 if Assigned(FTask) then
 begin
  FTask.Stop;
 end;
end;

Function TTaskLink.State:SizeInt; inline;
begin
 Result:=FTask.State;
end;

{Procedure TTaskLink.AsyncStart;
begin
 if Assigned(FTask) then
 begin
  //Notify.Bind(FTask);
  FTask.AsyncStart;
 end;
end;

Procedure TTaskLink.AsyncPause;
begin
 if Assigned(FTask) then
 begin
  FTask.AsyncPause;
 end;
end;

Procedure TTaskLink.AsyncStop;
begin
 if Assigned(FTask) then
 begin
  FTask.AsyncStop;
 end;
end;}

//

Procedure TQueryListLink.Create;
//Var
// T:TBaseTask;
begin
 //T:=FTask;
 inherited Create(TQueryList);
 //if T<>FTask then FTask.Release;
end;

Procedure TQueryListLink.Abort;
begin
 if Assigned(FTask) then
 begin
  TQueryList(FTask).Abort;
 end;
end;

Procedure TQueryListLink.ForEachDo(S:SizeInt);
begin
 if Assigned(FTask) then
 begin
  TQueryList(FTask).ForEachDo(S);
 end;
end;

Procedure TQueryListLink.ForEachNotify(S:SizeInt);
begin
 if Assigned(FTask) then
 begin
  TQueryList(FTask).ForEachNotify(S);
 end;
end;

Procedure TQueryListLink.ForEachCall(N:TNotifyTask);
begin
 if Assigned(FTask) then
 begin
  TQueryList(FTask).ForEachCall(N);
 end;
end;

Procedure TQueryListLink.Add(N:TBaseTask);
begin
 //Writeln('FTask:',HexStr(FTask));
 if not Assigned(FTask) then Create;
 TQueryList(FTask).Add(N);
end;

Procedure TQueryListLink.Remove(N:TBaseTask);
begin
 if Assigned(FTask) then
 begin
  TQueryList(FTask).Remove(N);
 end;
end;

function  TQueryListLink.Item(i:SizeInt):TBaseTask;
begin
 Result:=nil;
 if Assigned(FTask) then
 begin
  Result:=TQueryList(FTask).Item(i);
 end;
end;

function  TQueryListLink.Count:SizeUInt;
begin
 Result:=0;
 if Assigned(FTask) then
 begin
  Result:=TQueryList(FTask).Count;
 end;
end;

//000

Procedure TAsyncFuncNode.Call;
Var
 T:TBaseTask;
begin
 T:=TBaseTask(TMethod(N).Data);
 if Assigned(T) then
 begin

  if T.FInher<>0 then
  begin
   SendCurrQueue(@Self);
   Exit;
  end;

  if Assigned(N) then N();
  T.Free;
 end;
 FreeMem(@Self);
end;

Procedure TSyncFuncNodeEvent.Call;
begin
 if Assigned(N) then N();
 FreeMem(@Self);
end;

Procedure TSyncFuncNode.Call;
begin
 if Assigned(N) then N();
 FreeMem(@Self);
end;

Procedure TAsyncNotifyNode.Call;
begin
 if Assigned(R) then
 begin

  if R.FInher<>0 then
  begin
   SendCurrQueue(@Self);
   Exit;
  end;

  R.Notify.Call(S);
  R.Free;
 end;
 FreeMem(@Self);
end;

Procedure TAsyncNotifyNode2.Call;
begin
 if Assigned(R) then
 if Assigned(C) then
 begin

  if R.FInher<>0 then
  begin
   SendCurrQueue(@Self);
   Exit;
  end;

  R.Notify.Call(S,C);
  C.Free;
  R.Free;
 end;
 FreeMem(@Self);
end;

procedure TBaseTask.AsyncFunc(N:TFuncEvent);
Var
 Node:PAsyncFuncNode;
begin
 if Assigned(N) then
 begin
  Acquire;
  Node:=AllocMem(SizeOf(TAsyncFuncNode));
  Node^:=Default(TAsyncFuncNode);
  Node^.Parent:=@Node^.Call;
  Node^.N:=N;
  SendCurrQueue(Node);
 end;
end;

procedure SyncFunc(N:TFuncEvent);
Var
 Node:PSyncFuncNodeEvent;
begin
 if Assigned(N) then
 begin
  Node:=AllocMem(SizeOf(TSyncFuncNodeEvent));
  Node^:=Default(TSyncFuncNodeEvent);
  Node^.Parent:=@Node^.Call;
  Node^.N:=N;
  SendCurrQueue(Node);
 end;
end;

procedure SyncFunc(N:TOnParent);
Var
 Node:PSyncFuncNode;
begin
 if Assigned(N) then
 begin
  Node:=AllocMem(SizeOf(TSyncFuncNode));
  Node^:=Default(TSyncFuncNode);
  Node^.Parent:=@Node^.Call;
  Node^.N:=N;
  SendCurrQueue(Node);
 end;
end;

procedure TBaseTask.AsyncNotify(S:SizeInt);
Var
 Node:PAsyncNotifyNode;
begin
 Acquire;
 Node:=AllocMem(SizeOf(TAsyncNotifyNode));
 Node^:=Default(TAsyncNotifyNode);
 Node^.Parent:=@Node^.Call;
 Node^.R:=Self;
 Node^.S:=S;
 SendCurrQueue(Node);
end;

procedure TBaseTask.AsyncNotify(S:SizeInt;Sender:TBaseTask);
Var
 Node:PAsyncNotifyNode2;
begin
 Acquire;
 Sender.Acquire;
 Node:=AllocMem(SizeOf(TAsyncNotifyNode2));
 Node^:=Default(TAsyncNotifyNode2);
 Node^.Parent:=@Node^.Call;
 Node^.R:=Self;
 Node^.C:=Sender;
 Node^.S:=S;
 SendCurrQueue(Node);
end;

Constructor TBaseTask.Create;
begin
 Acquire;
 FState:=T_NEW;
 FOBS.Free;
 OnState;
end;

Procedure TBaseTask.Cleanup;
begin
 FState:=T_DEL;
 OnState;
 FOBS.Call(T_DEL,Self);
 FOBS.Free;
end;

Function TBaseTask.GetState:SizeInt; inline;
begin
 if Assigned(Self) then
 begin
  Result:=FState;
 end else
  Result:=T_DEL;
end;

Function TBaseTask.Pause:SizeInt;
Var
 s:SizeInt;
begin
 Result:=R_ERR;

 if State<>T_DEL then
 if FInher<>0 then
 begin
  AsyncFunc(@Self.Pause);
  Result:=R_ASY;
  Exit;
 end;

 Case State of
  T_NEW,T_RUN,T_FIN:
  begin
   //----------

   s:=FState;

   InherInc;
   Result:=BeforePause;
   InherDec;
   if Result=R_SUC then
   begin
    FState:=T_PAU;
    OnState;
    Notify.Call(T_PAU);
    if s=T_RUN then
     Release;
   end;

  end;
 end;
end;

//If TMethod(@C.MyMethod).Code=Pointer(@system.AbstractError) then

Function TBaseTask.Start:SizeInt;
begin
 Result:=R_ERR;

 if State<>T_DEL then
 if FInher<>0 then
 begin
  AsyncFunc(@Self.Start);
  Result:=R_ASY;
  Exit;
 end;

 Case State of
  T_NEW,T_PAU,T_FIN:
  begin
   //----------

   InherInc;
   Result:=BeforeStart;
   InherDec;
   if Result=R_SUC then
   begin
    Acquire;
    FState:=T_RUN;
    OnState;
    Notify.Call(T_RUN);
   end;

  end;
 end;
end;

Function TBaseTask.Stop:SizeInt;
Var
 s:SizeInt;
begin
 Result:=R_ERR;

 if State<>T_DEL then
 if FInher<>0 then
 begin
  //Writeln('InherCall',FInher);
  AsyncFunc(@Self.Stop);
  Result:=R_ASY;
  Exit;
 end;

 Case State of
  T_NEW,T_PAU,T_RUN:
  begin
   //----------

   s:=FState;

   InherInc;
   Result:=BeforeStop;
   InherDec;
   if Result=R_SUC then
   begin
    FState:=T_FIN;
    OnState;
    Notify.Call(T_FIN);
    Case s of
     T_RUN:if Release then Free;
     else
     begin
      if FRef=0 then Free;
     end;
    end;
   end;

  end;
 end;
end;

Function TBaseTask.BeforeStart:SizeInt;
begin
 Result:=R_SUC;
end;

Function TBaseTask.BeforePause:SizeInt;
begin
 Result:=R_SUC;
end;

Function TBaseTask.BeforeStop:SizeInt;
begin
 Result:=R_SUC;
end;

Procedure TBaseTask.AsyncStart;
begin
 AsyncFunc(@Start);
end;

Procedure TBaseTask.AsyncPause;
begin
 AsyncFunc(@Pause);
end;

Procedure TBaseTask.AsyncStop;
begin
 AsyncFunc(@Stop);
end;

Procedure TBaseTask.DoEnd(Tag:SizeInt); inline;
begin
 Acquire;
 Notify.Call(Tag);
 Stop;
 Free;
end;

function TObserver.IndexOf(N:TNotifyTask):SizeInt;
Var
 i:SizeInt;
begin
 Result:=-1;
 if Length(Data)>0 then
  For i:=0 to High(Data) do
   if CompareByte(Data[i],N,SizeOf(TNotifyTask))=0 then
    Exit(i);
end;

function TObserver.Add(N:TNotifyTask):Boolean;
Var
 i:SizeInt;
begin
 Result:=Length(Data)=0;
 if Assigned(N) then
 begin
  i:=IndexOf(N);
  if i=-1 then
  begin
   i:=Length(Data);
   SetLength(Data,i+1);
   Data[i]:=N;
  end;
 end;
end;

function TObserver.Remove(N:TNotifyTask):Boolean;
Var
 i:SizeInt;
begin
 if Assigned(N) then
 begin
  i:=IndexOf(N);
  if i<>-1 then
  begin
   if i<High(Data) then
   begin
    System.Move(Data[i+1],Data[i],(High(Data)-i)*SizeOf(TNotifyTask));
   end;
   SetLength(Data,High(Data));
   if FPos>=i then FPos:=FPos-1;
  end;
 end;
 Result:=Length(Data)=0;
end;

Procedure TObserver.Remove; inline;
begin
 SetLength(Data,0);
 FPos:=-1;
end;

function TObserver.New(State:SizeUInt):PObserver;
begin
 Result:=nil;
 System.New(Result);
 Result^.FPos:=-1;
 Result^.Tag:=State;
end;

Procedure TObserver.Free;
begin
 SetLength(Data,0);
 Dispose(PObserver(@Self));
end;

function TObserver.Call(FSender:TBaseTask):SizeInt;
Var
 f:TNotifyTask;
begin
 Result:=0;
 //Writeln('Obs:',FSender.ClassName,' ($',hexstr(FSender),')');
 FPos:=0;
 While (FPos<Length(Data)) do
 begin
  if Assigned(Data[FPos]) then
  begin
   //Writeln('$>',hexstr(@Self));
   f:=Data[FPos];
   try
    f(FSender);
   except
    on E:Exception do
     DumpExceptionCallStack(E);
   end;
   //Writeln('$<',hexstr(@Self));
   Inc(Result);
  end;
  if FPos<0 then Break;
  FPos:=FPos+1;
 end;
 FPos:=-1;
end;

Procedure TObserver.Abort; inline;
begin
 FPos:=-1;
end;

function TObserver.Count:SizeInt; inline;
begin
 Result:=Length(Data);
end;

{function TObserverList.IndexOf(Tag:SizeInt):SizeInt;
Var
 i:SizeInt;
begin
 Result:=-1;
 if Length(Data)>0 then
  For i:=0 to High(Data) do
   if Data[i]^.Tag=Tag then
    Exit(i);
end;}

function TObserverList.IndexOf(Tag:SizeInt):SizeInt;
Var
 l,r,m:SizeUInt;
begin
 Result:=-1;
 l:=0;
 r:=Length(Data);
 while (l<r) do
 begin
  m:=l+(r-l) div 2;
  if (Data[m]^.Tag=Tag) then
  begin
   Result:=m;
   Exit;
  end;
  if (Data[m]^.Tag>Tag) then
  begin
   r:=m;
  end else
  begin
   l:=m+1;
  end;
 end;
 Result:=-l-1;
end;

Procedure TObserverList.Add(Tag:SizeInt;N:TNotifyTask);
Var
 i,L:SizeInt;
begin
 if Assigned(N) then
 begin
  i:=IndexOf(Tag);
  if (i<0) then
  begin
   i:=-i-1;
   L:=Length(Data);
   SetLength(Data,L+1);
   if (L-i)<>0 then
   begin
    System.Move(Data[i],Data[i+1],(L-i)*SizeOf(PObserver));
   end;
   Data[i]:=TObserver.New(Tag);
  end;
  Data[i]^.Add(N);
 end;
end;

Procedure TNotify.Add(Tag:SizeInt;N:TNotifyTask);
begin
 if Assigned(Root) then
 With Root do
 begin
  FOBS.Add(Tag,N);
 end;
end;

Procedure TNotify.Remove(Tag:SizeInt;N:TNotifyTask);
Var
 i:SizeInt;
begin
 if Assigned(Root) then
 if Assigned(N) then
 With Root do
 begin
  i:=FOBS.IndexOf(Tag);
  if (i>=0) then
  begin
   FOBS.Data[i]^.Remove(N);
  end;
 end;
end;

Procedure TNotify.Remove(N:TNotifyTask);
Var
 i:SizeInt;
begin
 if Assigned(Root) then
 With Root do
 begin
  if Length(FOBS.Data)>0 then
  For i:=0 to High(FOBS.Data) do
   FOBS.Data[i]^.Remove(N);
 end;
end;

Procedure TNotify.Remove(Tag:SizeInt);
Var
 i:SizeInt;
begin
 if Assigned(Root) then
 With Root do
 begin
  i:=FOBS.IndexOf(Tag);
  if (i>=0) then
  begin
   FOBS.Data[i]^.Remove;
  end;
 end;
end;

Procedure TNotify.Clear;
Var
 i:SizeInt;
begin
 if Assigned(Root) then
 With Root do
 begin
  if Length(FOBS.Data)>0 then
  For i:=0 to High(FOBS.Data) do
   FOBS.Data[i]^.Remove;
 end;
end;

Procedure TNotify.Abort;
Var
 i:SizeInt;
begin
 if Assigned(Root) then
 With Root do
 begin
  if Length(FOBS.Data)>0 then
  For i:=0 to High(FOBS.Data) do
   FOBS.Data[i]^.Abort;
 end;
end;

function TObserverList.Call(Tag:SizeInt;Sender:TBaseTask):SizeInt;
Var
 i:SizeInt;
begin
 Result:=0;
 i:=IndexOf(Tag);
 if (i>=0) then
 begin
  Result:=Data[i]^.Call(Sender);
 end;
end;

function TObserverList.Count(Tag:SizeInt):SizeInt;
Var
 i:SizeInt;
begin
 Result:=0;
 i:=IndexOf(Tag);
 if (i>=0) then
 begin
  Result:=Data[i]^.Count;
 end;
end;

Procedure TObserverList.Free;
Var
 i:SizeInt;
begin
 if Length(Data)>0 then
  For i:=0 to High(Data) do
   Data[i]^.Free;
 Self:=Default(TObserverList);
end;

{function TJob.State:SizeInt; inline;
begin
 Result:=FState;
end;

Procedure TJob.Clear; inline;
begin
 Self:=Default(TJob);
end;

function TJob.Call:SizeInt; inline;

 Procedure CallProc(f:TFuncEvent); inline;
 Var
  R:SizeInt;
 begin
  if Assigned(f) then
  begin
   R:=R_ERR;
   try
    R:=f();
   except
    on E:Exception do
     DumpExceptionCallStack(E);
   end;
   Case R of
    R_SUC,
    R_ASY,
    R_RST:Fstate:=R;
    else  Fstate:=R_ERR;
   end;
  end;
 end;

begin
 While (FPos<Length(FData)) do
 begin
  if Fstate=R_SUC then
  begin
   CallProc(FData[FPos].onFul);
  end;
  if Fstate=R_ERR then
  begin
   CallProc(FData[FPos].onFul);
  end;
  Case Fstate of
   R_ASY:Exit(R_ASY);
   R_RST:Break;
  end;
  FPos:=FPos+1;
 end;
 Result:=Fstate;
 FPos:=0;
 Fstate:=R_SUC;
end;

Procedure TJob.Reset; inline;
begin
 FPos:=0;
 Fstate:=R_SUC;
end;

Procedure TJob.Throw; inline;
begin
 Fstate:=R_ERR;
end;

function TJob.Add(onFul,onRej:TFuncEvent):SizeInt;
begin
 if Assigned(onFul) or Assigned(onRej) then
 begin
  Result:=Length(FData);
  SetLength(FData,Result+1);
  FData[Result].onFul:=onFul;
  FData[Result].onRej:=onRej;
 end;
end;

Procedure TJobList.Clear; inline;
begin
 Self:=Default(TJobList);
 FTagi:=-1;
end;

function TJobList.IndexOf(Tag:SizeInt):SizeInt;
Var
 i:SizeInt;
begin
 Result:=-1;
 if Length(Data)>0 then
  For i:=0 to High(Data) do
   if Data[i].Tag=Tag then
    Exit(i);
end;

procedure TJobList.Add(Tag:SizeInt;onFul,onRej:TFuncEvent);
Var
 i:SizeInt;
begin
 if Assigned(onFul) or Assigned(onRej) then
 begin
  i:=IndexOf(Tag);
  if i=-1 then
  begin
   i:=Length(Data);
   SetLength(Data,i+1);
   Data[i].Clear;
   Data[i].Tag:=Tag;
  end;
  Data[i].Add(onFul,onRej);
 end;
end;

procedure TJobList.Throw(Tag:SizeInt); inline;
Var
 i:SizeInt;
begin
 i:=IndexOf(Tag);
 if i=-1 then
 begin
  Data[i].Throw;
 end;
end;

function TJobList.Call(Tag:SizeInt):SizeInt; inline;
Var
 i:SizeInt;
begin
 Result:=R_SUC;
 i:=IndexOf(Tag);
 if i=-1 then Exit;
 if FTagi=-1 then
 begin
  FTagi:=i;
 end else
 if FTagi<>i then
 begin
  Data[FTagi].Reset;
  FTagi:=i;
 end;
 Result:=Data[i].Call;
 Case Result of
  R_ASY:Exit;
 end;
 FTagi:=-1;
end;}

Constructor TTaskList.Create;
begin
 inherited;
 LWork:=TTaskSet.Create;
end;

function TTaskList._Next:Boolean;
begin
 if Assigned(LWork) then
 begin
  if Assigned(LI) then
  begin
   if not Assigned(LI.FNode) then
   begin
    LI.FNode:=LWork.NMin;
    Result:=Assigned(LI.FNode);
   end else
   begin
    Result:=LI.Next;
   end;
  end else
  begin
   LI:=LWork.Min;
   Result:=Assigned(LI);
  end;
 end;
end;

Procedure TTaskList._Prev(N:TBaseTask); inline;
begin
 if Assigned(LI) then
 begin
  if LI.Data=N then
  begin
   if not LI.Prev then LI.FNode:=nil;
  end;
 end;
end;

Procedure TTaskList.Abort;
begin
 if Assigned(LWork) then
 if Assigned(LI) then
 begin
  LI.FNode:=LWork.NMax;
 end;
end;

{Procedure TTaskList.ForEachDo(S:SizeInt);
var
 T:TBaseTask;
begin
 if Assigned(LWork) then
 begin

  if Assigned(LI) then Exit;

  if _Next then
  begin
   repeat
    T:=LI.Data;
    if Assigned(T) then
    begin
     Case S of
      T_RUN:T.Start;
      T_PAU:T.Pause;
      T_FIN:T.Stop;
      T_DEL:Remove(T);
      T_REM:begin
             T.Stop;
             Remove(T);
            end;
     end;
    end;

   until not _Next;
   FreeAndNil(LI);
  end;
 end;
end;}

Procedure TTaskList.ForEachDo(A:array of SizeInt);
var
 i:SizeInt;
 T:TBaseTask;
begin
 if Assigned(LWork) and (Length(A)>0) then
 begin

  if Assigned(LI) then Exit;

  if _Next then
  begin
   repeat
    T:=LI.Data;
    if Assigned(T) then
    begin
     For i:=0 to High(A) do
     begin
      Case A[i] of
       T_NEW:T.Acquire;
       T_DEL:T.Release;

       T_RUN:T.Start;
       T_PAU:T.Pause;
       T_FIN:T.Stop;

       T_REM:Remove(T);
       T_ADD:Add(T);
       else
       begin
        T.Notify.Call(A[i]);
       end;
      end;
     end;
    end;

   until not _Next;
   FreeAndNil(LI);
  end;
 end;
end;

Procedure TTaskList.ForEachNotify(S:SizeInt);
var
 T:TBaseTask;
begin
 if Assigned(LWork) then
 begin
  if Assigned(LI) then Exit;
  if _Next then
  begin
   repeat
    T:=LI.Data;
    if Assigned(T) then
    begin
     T.Notify.Call(S);
    end;
   until not _Next;
   FreeAndNil(LI);
  end;
 end;
end;

Procedure TTaskList.ForEachCall(N:TNotifyTask);
begin
 if Assigned(N) then
 if Assigned(LWork) then
 begin
  if Assigned(LI) then Exit;
  if _Next then
  begin
   repeat
    N(LI.Data);
   until not _Next;
   FreeAndNil(LI);
  end;
 end;
end;

Procedure TTaskList.Cleanup;
begin
 ForEachDo([T_REM]);
 inherited;
 FreeAndNil(LWork);
end;

Procedure TTaskList.Add(N:TBaseTask);
begin
 if State=T_DEL then Exit;
 if Assigned(LWork) then
 if Assigned(N) then
 begin
  if not Assigned(LWork.NFind(N)) then
  begin
   N.Acquire;
   LWork.Insert(N);
   N.Notify.Call(T_ADD);

   //Writeln('Insert(',HexStr(N),'),',self.Count);

   //NotifyCall(T_ADD);
  end;
 end;
end;

Procedure TTaskList.Remove(N:TBaseTask);
begin
 //Writeln('Remove(',HexStr(N),')');
 if Assigned(LWork) then
 if Assigned(N) then
 begin
  if Assigned(LWork.NFind(N)) then
  begin

   _Prev(N);

   LWork.Delete(N);
   //Writeln('Delete(',HexStr(N),'),',self.Count);

   N.Notify.Call(T_REM);
   N.Free;
   //NotifyCall(T_REM);
  end;
 end;
end;

function TTaskList.Item(i:SizeInt):TBaseTask;
var
 IT:TTaskSet.TIterator;
begin
 Result:=nil;
 if Assigned(LWork) then
 if (i>=0) and (i<LWork.Size) then
 begin
  IT:=LWork.Min;
  if Assigned(IT) then
  begin
   repeat
    if i=0 then
    begin
     Result:=IT.Data;
     Break;
    end;
    Dec(i);
   until not IT.Next;
   FreeAndNil(IT);
  end;
 end;
end;

function TTaskList.GetCount:SizeUInt; inline;
begin
 Result:=0;
 if Assigned(LWork) then
 begin
  Result:=LWork.Size;
 end;
end;

{Procedure TQueryList.OnRemove(Sender:TBaseTask);
begin
 //Writeln('OnRemove:',HexStr(Self),' ',Self.ClassName,' ',Count);
 Sender.Notify.Remove(@onRemove);
 Remove(Sender);
 if Count=0 then Stop;
end;}

Procedure TQueryList.Add(N:TBaseTask);
begin
 N.Notify.Add(T_DEL,@Remove);
 N.Notify.Add(T_FIN,@Remove);
 N.Notify.Add(T_REM,@Remove);
 inherited;
 Start;
end;

Procedure TQueryList.Remove(N:TBaseTask);
begin
 N.Notify.Remove(@Remove);
 inherited;
 if Count=0 then Stop;
end;

Function TQueryList.BeforeStop:SizeInt;
begin
 Result:=inherited;
 if Result=R_SUC then
 if Count>0 then
 begin
  //Writeln('ForEachDo(T_FIN):',Count);
  //ForEachDo([T_FIN]);
  ForEachDo([T_FIN,T_REM]);
  Result:=R_ASY;
 end;
end;

//-------------------------

{Procedure TQuerySuccused._RemNotify(Sender:TBaseTask);
begin
 Sender.Notify.Remove(@onRem);
 Sender.Notify.Remove(@onSuc);
 Sender.Notify.Remove(@onErr);
end;}

{Procedure TQuerySuccused.OnRem(Sender:TBaseTask);
begin
 _RemNotify(Sender);
 Remove(Sender);
 if Count=0 then
 begin

  //Writeln('TQuerySuccused.OnRem');

  //Acquire;
  Stop;
  //Notify.Call(T_ERR);
  //Free;
 end;
end;}

Procedure TQuerySuccused.OnSuc(Sender:TBaseTask);
begin
 //_RemNotify(Sender);
 _Remove(Sender);

 Case State of
  T_FIN,T_DEL:Exit;
 end;

 //Writeln('TQuerySuccused.OnSuc');

 Notify.Call(T_LDR,Sender);
 Notify.Call(T_INF);

 if Count=0 then
 begin
  DoEnd(T_SUC);
 end;
end;

Procedure TQuerySuccused.OnErr(Sender:TBaseTask);
begin
 //_RemNotify(Sender);
 _Remove(Sender);

 Case State of
  T_FIN,T_DEL:Exit;
 end;

 //Writeln('TQuerySuccused.OnErr');

 if Notify.Count(T_LDE)>0 then
 begin
  Notify.Call(T_LDE,Sender);
  Notify.Call(T_INF);
 end else
 begin
  DoEnd(T_ERR);
 end;
end;

Procedure TQuerySuccused.Add(N:TBaseTask);
begin
 N.Notify.Add(T_DEL,@Remove);
 N.Notify.Add(T_REM,@Remove);
 N.Notify.Add(T_SUC,@onSuc);
 N.Notify.Add(T_ERR,@onErr);
 inherited;
 Start;
end;

Procedure TQuerySuccused._Remove(N:TBaseTask);
begin
 N.Notify.Remove(@Remove);
 N.Notify.Remove(@onSuc);
 N.Notify.Remove(@onErr);
 inherited Remove(N);
end;

Procedure TQuerySuccused.Remove(N:TBaseTask);
begin
 _Remove(N);
 if Count=0 then Stop;
end;

Function TQuerySuccused.BeforeStop:SizeInt;
begin
 Result:=inherited;
 if Result=R_SUC then
 if Count>0 then
 begin
  //Writeln('TQuerySuccused.ForEachDo(T_REM)');
  ForEachDo([T_FIN,T_REM]);
  Result:=R_ASY;
 end;
end;


end.

