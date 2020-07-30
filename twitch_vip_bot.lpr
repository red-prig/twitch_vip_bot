{ twitch vip bot

  Copyright (C) 2020 Red_prig

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}


program twitch_vip_bot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  FastMM4,

  evpool,bufferevent_openssl,

  Interfaces, // this includes the LCL widgetset

  UniqueInstanceBase,
  uniqueinstanceraw,

  LCLIntf,

  SysUtils,ExtCtrls,Classes,UAsyncQueue,ULog,
  Forms, printer4lazarus, main,Uloginf, UFrmParam, UFrmAbout, ufrmpred;

{$R *.res}

type
 TUserApp=class(TApplication)
   procedure TimerIdleUpdate(Sender:TObject);
   procedure CustomExceptionHandler(Sender:TObject;E:Exception);
   procedure OnIdleUpdate(Sender:TObject;var Done:Boolean);
   Procedure FOnLog(time_:TDateTime;type_:LogType;code_:SizeInt;Const S:String);
   procedure OnIPCEvent(Sender:TObject);
 end;

procedure TUserApp.TimerIdleUpdate(Sender:TObject);
begin
 ProcessMessages;
 if not Terminated then Idle(False);
end;

procedure TUserApp.CustomExceptionHandler(Sender:TObject;E:Exception);
begin
 ULog.CatchLog;
 Application.ShowException(E);
end;

procedure TUserApp.OnIdleUpdate(Sender:TObject;var Done:Boolean);
begin
 Done:=True;
 MainQueue.OnUpdate;
end;

Var
 TFS:TFileStream;

Procedure TFSWriteln(Const S:String); inline;
Const
 NL:array[0..1] of Char=#13#10;
begin
 if Assigned(TFS) then
 begin
  TFS.Write(S[1],Length(S));
  TFS.Write(NL,2);
 end;
end;

Procedure TUserApp.FOnLog(time_:TDateTime;type_:LogType;code_:SizeInt;Const S:String);
Var
 FS:TFormatSettings;
 T:String;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='yyyy/mm/dd';
 FS.DateSeparator:='.';
 FS.ShortTimeFormat:='hh:nn:ss.zzz';
 FS.LongTimeFormat:='hh:nn:ss.zzz';
 FS.TimeSeparator:=':';
 T:=DateTimeToStr(time_,FS,True)+'|';
 T:=T+type_.ClassName+'|';
 T:=T+type_.Explain(code_)+'|'+S;
 TFSWriteln(T);
 {if Assigned(PageOnLog) then
 begin
  PageOnLog(time_,type_,code_,S);
 end;}
end;

procedure TUserApp.OnIPCEvent(Sender:TObject);
begin
 Application.MainForm.WindowState:=wsNormal;
 Application.MainForm.Show;
end;

Var
 TimerIdleUpdate:TTimer;

Const
 BOM:PAnsiChar=#$EF#$BB#$BF;

function GetLocalLog:RawByteString;
begin
 Result:=ParamStr(0);
 Result:=ExtractFileDir(Result);
 Result:=IncludeTrailingPathDelimiter(Result);
 {$IFDEF UNIX}
 Result:=Result+'log_'+GetEnvironmentVariable('USER')+'.txt';
 {$ELSE}
 Result:=Result+'log_'+UTF8Encode(GetEnvironmentVariable(UnicodeString('USERNAME')))+'.txt';
 {$ENDIF}
end;

begin

  if InstanceRunning('twitch_vip_bot',True,True) then
  begin
   Halt;
  end;
  UniqueInstanceBase.FIPCServer.OnMessageQueued:=@TUserApp(Application).OnIPCEvent;

  RequireDerivedFormResource:=True;

  UAsyncQueue.MainQueue:=TNodeQueue.Create;
  UAsyncQueue.CurrQueue:=UAsyncQueue.MainQueue;

  Application.OnException:=@TUserApp(Application).CustomExceptionHandler;
  Application.AddOnIdleHandler(@TUserApp(Application).OnIdleUpdate,False);

  try
   TStream(TFS):=TFileStream.Create(GetLocalLog,fmCreate or fmShareDenyWrite);
   TFS.Write(BOM^,Length(BOM));
  except
   TFS:=nil;
  end;

  ULog.OnLog:=@TUserApp(Application).FOnLog;
  ULog.app_logic.LogDetail:=true;

  pool:=Default(Tevpool);
  pool_config:=Default(Tevpool_config);
  evpool_start(@pool,1,@pool_config);

  TimerIdleUpdate:=TTimer.Create(Application);
  TimerIdleUpdate.Interval:=400;
  TimerIdleUpdate.OnTimer:=@TUserApp(Application).TimerIdleUpdate;
  TimerIdleUpdate.Enabled:=True;

  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TFrmParam, FrmParam);
  Application.CreateForm(TFrmAbout, FrmAbout);
  Application.CreateForm(TFrmPred, FrmPred);
  Application.Run;
end.

