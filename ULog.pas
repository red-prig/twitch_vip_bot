{ Async log 

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

unit ULog;

{$mode objfpc}{$H+}

interface

uses
 SysUtils;


type
 app_logic=class
  LogDetail:Boolean; static;
  class Function Filter(code_:SizeInt):Boolean; virtual;
  class Function Explain(code_:SizeInt):RawByteString; virtual;
 end;

 LogType=class of app_logic;

 //LogType=(curl_easy,curl_share,curl_multi,curl_debug,app_logic,dbc_event);
 TOnLog=Procedure(time_:TDateTime;type_:LogType;code_:SizeInt;Const S:String) of object;

Var
 OnLog:TOnLog;
 //LogDetail:Boolean=False;

function  ToString(Args:array of const):String;
Procedure Log(type_:LogType;code_:SizeInt;Args:array of const);
Procedure Log(type_:LogType;code_:SizeInt;Const S:String);
Procedure Log(Args:array of const);
Procedure Log(Const S:String); inline;
Procedure DumpExceptionCallStack(E: Exception);

procedure CatchLog;

implementation

Uses UAsyncQueue;

type
 PLogNode=^TLogNode;
 TLogNode=object(UAsyncQueue.TQNode)
  time_:TDateTime;
  type_:LogType;
  code_:SizeInt;
  text_:record end;
  //below text in PChar;
  Procedure Finnale;
 end;

Procedure LogPush(type_:LogType;code_:SizeInt;Const S:String);
Var
 Node:PLogNode;
 Len:sizeint;
begin
 Len:=Length(S)+1;
 Node:=GetMem(SizeOf(TLogNode)+Len);  //mt
 Node^.Parent:=@Node^.Finnale;
 Node^.time_:=Now;
 Node^.type_:=type_;
 Node^.code_:=code_;
 Move(S[1],Node^.text_,Len);
 SendMainQueue(Node);
end;

Procedure TLogNode.Finnale;
begin
 if Assigned(OnLog) then
 begin
  OnLog(time_,type_,code_,PChar(@text_));
 end;
 FreeMem(@Self); //mt
end;

function ToString(Args:array of const):String;
Var
 I:longint;
begin
 Result:='';
 if Length(Args)>0 then
 For i:=0 to High(Args) do
 With Args[i] do
 begin
  case vtype of
   vtInteger      :Result:=Result+IntToStr(VInteger);
   vtBoolean      :Result:=Result+BoolToStr(VBoolean,True);
   vtChar         :Result:=Result+VChar;
   vtString       :Result:=Result+VString^;
   vtPointer      :Result:=Result+'$'+hexStr(VPointer);
   vtPChar        :Result:=Result+String(VPChar);
   vtObject       :Result:=Result+'$'+hexStr(Pointer(VObject));
   vtClass        :Result:=Result+'$'+hexStr(Pointer(VClass));
   vtWideChar     :Result:=Result+String(VWideChar);
   vtPWideChar    :Result:=Result+String(VPWideChar);
   vtAnsiString   :Result:=Result+String(VAnsiString);
   vtVariant      :Result:=Result+String(VVariant);
   vtWideString   :Result:=Result+String(WideString(VWideString));
   vtInt64        :Result:=Result+IntToStr(VInt64^);
   vtQWord        :Result:=Result+IntToStr(VQWord^);
   vtUnicodeString:Result:=Result+String(UnicodeString(VUnicodeString));
  end;
 end;
end;

class Function app_logic.Filter(code_:SizeInt):Boolean;
begin
 Result:=(LogDetail) or (code_<>0);
end;

class Function app_logic.Explain(code_:SizeInt):RawByteString;
begin
 System.Str(code_,Result);
end;

{Function Filter(type_:LogType;code_:SizeInt):Boolean;
begin
 Result:=Assigned(OnLog);
 if Result and (not LogDetail) then
 Case type_ of
  curl_easy :Result:=(code_<>SizeInt(CURLE_OK));
  curl_share:Result:=(code_<>SizeInt(CURLSHE_OK));
  curl_multi:Result:=((code_<>SizeInt(CURLM_OK)) and (code_<>SizeInt(CURLM_CALL_MULTI_PERFORM)));
  else
             Result:=(code_<>0);
 end;
end;}

Function Filter(type_:LogType;code_:SizeInt):Boolean;
begin
 Result:=Assigned(OnLog);
 if Result then
 begin
  Result:=type_.Filter(code_);
 end;
end;

Procedure Log(type_:LogType;code_:SizeInt;Args:array of const);
begin
 if Filter(type_,code_) then
  LogPush(type_,code_,ToString(Args));
end;

Procedure Log(type_:LogType;code_:SizeInt;Const S:String);
begin
 if Filter(type_,code_) then
  LogPush(type_,code_,S);
end;

Procedure Log(Args:array of const);
begin
 //if Filter(app_logic,0) then
  LogPush(app_logic,0,ToString(Args));
end;

Procedure Log(Const S:String); inline;
begin
 //if Filter(app_logic,0) then
  LogPush(app_logic,0,S);
end;

Procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := '';
  if E <> nil then
  begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Log(app_logic,2,Report);
end;

procedure CatchLog;
var
 rc:PtrUInt;
 Node:PQNode;
begin
 Node:=nil;
 rc:=MainQueue.Count;
 With MainQueue do
  While Recv(Node) do
  begin
   if TMethod(Node^.Parent).Code=Pointer(@TLogNode.Finnale) then
   begin
    Node^.Parent();
   end else
   begin
    Send(Node);
   end;
   if rc=0 then Break;
   Dec(rc);
   if rc=0 then Break;
  end;
end;

initialization

end.

