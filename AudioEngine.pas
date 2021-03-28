{ async Audio player

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

unit AudioEngine;

{$mode objfpc}{$H+}

interface

uses
 {$IFDEF WINDOWS}
 Windows,
 ActiveX,
 SpeechLib_TLB,
 {$ENDIF}
 ctypes,
 SysUtils,
 Classes,

 libportaudio,
 libsndfile,
 libmpg123,

 TaskManager,
 UAsyncQueue,
 ULog;

type
 audio_event=class(app_logic)
 end;

 TOnParent=UAsyncQueue.TOnParent;
 PQNode=^TQNode;
 TQNode=object(UAsyncQueue.TQFrom2Node)
  public
   Ftype:SizeUInt;
 end;

 PQNodeStream=^TQNodeStream;
 TQNodeStream=object(TQNode)
  public
   FStream:TStream;
   FVolume:Single;
   FIndex:PaDeviceIndex;
 end;

 PQNodeSAPI=^TQNodeSAPI;
 TQNodeSAPI=object(TQNode)
  public
   FRate:Integer;
   FVolume:Integer;
   FText:RawByteString;
   FVoiceName:RawByteString;
   FAudioOutputName:RawByteString;
 end;

 TAudioThread=class(TCustomAsyncThread)
  private
   hVoice:ISpeechVoice;
   FCanPlay:Boolean;
  protected
   Procedure   InherBegin;  override;
   Procedure   InherEnd;    override;
   Procedure   InherUpdate; override;
   function    OpenStream(FIndex:PaDeviceIndex;Channels:Integer;sampleRate:Double):PaStream;
   Procedure   OnPlay_sndfile(FStream:TStream;FVolume:Single;FIndex:PaDeviceIndex);
   Procedure   OnPlay_mpgfile(FStream:TStream;FVolume:Single;FIndex:PaDeviceIndex);
   Procedure   OnPlay_sapi(FRate,FVolume:Integer;Const FText,FVoiceName,FAudioOutputName:RawByteString);
  public
   Constructor Create;
  published
   property    CanPlay:Boolean read FCanPlay write FCanPlay;
 end;

 TAudioConnection=class(TBaseTask)
  private
   FThread:TAudioThread;
  protected
  published
  public
   Constructor Create; override;
   Procedure   Cleanup;override;
   Function    Start:SizeInt; override;
   Function    Pause:SizeInt; override;
   Function    Stop:SizeInt;  override;
   function    Send(Node:PQNode):Boolean;
 end;

 TAudioHandle=class
  protected
   FAudioCon:TAudioConnection;
   Node:TQNodeStream;
   FState:Boolean;
  public
   Var
    OnFinish:TFuncEvent;
  protected
   Procedure   SetConnection(C:TAudioConnection);
   Procedure   SetStream(S:TStream);
   Procedure   BreakSend;
   Procedure   Finish;
   Procedure   SetIsMpg(b:Boolean);
   function    GetIsMpg:Boolean;
  published
   property    AudioConnection:TAudioConnection read FAudioCon write SetConnection;
   property    State:Boolean  read FState;
   property    Stream:TStream read Node.FStream write SetStream;
   property    Volume:Single  read Node.FVolume write Node.FVolume;
   property    DeviceIndex:PaDeviceIndex read Node.FIndex write Node.FIndex;
   property    IsMpg:Boolean read GetIsMpg write SetIsMpg;
  public
   Procedure   Clear;
   Constructor Create;
   Destructor  Destroy; override;
   Procedure   Send;
 end;

 TSAPIHandle=class
  protected
   FAudioCon:TAudioConnection;
   Node:TQNodeSAPI;
   FState:Boolean;
  public
   Var
    OnFinish:TFuncEvent;
  protected
   Procedure   SetConnection(C:TAudioConnection);
   Procedure   BreakSend;
   Procedure   Finish;
  published
   property    AudioConnection:TAudioConnection read FAudioCon write SetConnection;
   property    State:Boolean                 read FState;
   property    Rate:Integer                  read Node.FRate write Node.FRate;
   property    Volume:Integer                read Node.FVolume write Node.FVolume;
   property    Text:RawByteString            read Node.FText write Node.FText;
   property    VoiceName:RawByteString       read Node.FVoiceName write Node.FVoiceName;
   property    AudioOutputName:RawByteString read Node.FAudioOutputName write Node.FAudioOutputName;
  public
   Procedure   Clear;
   Constructor Create;
   Destructor  Destroy; override;
   Procedure   Send;
 end;

type
 TEnumDevice=procedure(Index:PaDeviceIndex;Const name:RawByteString) of object;

Procedure EnumOutputDevice(cb:TEnumDevice);

function sf_open_stream(S:TStream;mode:Integer;var sfinfo:TSF_INFO):TSNDFILE_HANDLE;

type
 ISpeechObjectToken=SpeechLib_TLB.ISpeechObjectToken;
 TEnumSAPIVoiceCb=procedure(const name:RawByteString;Voice:ISpeechObjectToken) of object;

Procedure EnumSAPIVoices(cb:TEnumSAPIVoiceCb);
Procedure EnumSAPIAudioOutputs(cb:TEnumSAPIVoiceCb);

implementation

uses
 atomic;

function tm_get_filelen(user_data:Pointer): off_t; cdecl;
begin
 Result:=TStream(user_data).Size;
end;

function tm_seek(tm_seekoffset:off_t;whence:Integer;user_data:Pointer): off_t; cdecl;
begin
 Result:=-1;
 case whence of
  libsndfile.SEEK_CUR:Result:=TStream(user_data).Seek(tm_seekoffset,soCurrent);
  libsndfile.SEEK_SET:Result:=TStream(user_data).Seek(tm_seekoffset,soBeginning);
  libsndfile.SEEK_END:Result:=TStream(user_data).Seek(tm_seekoffset,soEnd);
 end;
end;

function tm_read(const buf: Pointer; count: off_t; user_data:Pointer): off_t; cdecl;
begin
 Result:=TStream(user_data).Read(buf^,count);
end;

function tm_write(const buf: Pointer; count: off_t; user_data:Pointer): off_t; cdecl;
begin
 Result:=TStream(user_data).Write(buf^,count);
end;

function tm_tell(user_data:Pointer): off_t; cdecl;
begin
 Result:=TStream(user_data).Position;
end;

Const
 SF_VIRTUAL:TSF_VIRTUAL=(
  get_filelen:@tm_get_filelen;
  seek       :@tm_seek;
  read       :@tm_read;
  write      :@tm_write;
  tell       :@tm_tell;
 );

function sf_open_stream(S:TStream;mode:Integer;var sfinfo:TSF_INFO):TSNDFILE_HANDLE;
begin
 Result:=sf_open_virtual(@SF_VIRTUAL,mode,@sfinfo,Pointer(S));
end;

function mpg_read_stream(user_data:Pointer;AData:Pointer;ACount:size_t):size_t; cdecl;
begin
 Result:=TStream(user_data).Read(AData^,ACount);
end;

function mpg_seek_stream(user_data:Pointer;offset:off_t;whence:Integer):off_t; cdecl;
begin
 Result:=-1;
 case whence of
  libmpg123.SEEK_CUR:Result:=TStream(user_data).Seek(offset,soCurrent);
  libmpg123.SEEK_SET:Result:=TStream(user_data).Seek(offset,soBeginning);
  libmpg123.SEEK_END:Result:=TStream(user_data).Seek(offset,soEnd);
 end;
end;

function mpg_open_stream(FStream:TStream):Tmpg123_handle;
var
 err:integer;
 mp3_rates:pclong;
 mp3_rate_count,i:size_t;
begin
 Result:=nil;
 if (FStream=nil) then Exit;
 err:=0;
 Result:=mpg123_new(nil,err);
 if (Result=nil) then Exit;
 if (err<>0) then
 begin
  mpg123_delete(Result);
  Exit(nil);
 end;
 mpg123_format_none(Result);
 mp3_rates:=nil;
 mp3_rate_count:=0;
 mpg123_rates(mp3_rates,mp3_rate_count);
 if mp3_rate_count<>0 then
  For i:=0 to mp3_rate_count-1 do
  begin
   Writeln(mp3_rates[i]);
   mpg123_format(Result,mp3_rates[i],1,MPG123_ENC_FLOAT_32);
   mpg123_format(Result,mp3_rates[i],2,MPG123_ENC_FLOAT_32);
  end;
 err:=mpg123_replace_reader_handle(Result,@mpg_read_stream,@mpg_seek_stream,nil);
 if (err<>0) then
 begin
  mpg123_delete(Result);
  Exit(nil);
 end;
 err:=mpg123_open_handle(Result,Pointer(FStream));
 if (err<>0) then
 begin
  mpg123_close(Result);
  mpg123_delete(Result);
  Exit(nil);
 end;
end;

procedure fscale_data(data:PSingle;len:SizeInt;scale:Single);
begin
 While (len>0) do
 begin
  data^:=data^*scale;
  Inc(data);
  Dec(len);
 end;
end;

var
 FAudioInitLock:Pointer=nil;

procedure spin_lock_AudioInit; inline;
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (XCHG(FAudioInitLock,Pointer(1))=Pointer(1)) do bkoff.Wait;
end;

procedure spin_unlock_AudioInit; inline;
begin
 store_release(FAudioInitLock,Pointer(2));
end;

Procedure LazyAudioInit;
var
 k,err:Integer;
 format_info:TSF_FORMAT_INFO;
begin
 spin_lock_AudioInit;

 Log(audio_event,0,Pa_GetVersionText);
 Log(audio_event,0,sf_version_string);

 err:=Pa_Initialize();
 Log(audio_event,err,'Pa_Initialize');

 err:=0;
 sf_command(nil,SFC_GET_FORMAT_MAJOR_COUNT,@err,sizeof(err));

 For k:=0 to err-1 do
 begin
  format_info:=Default(TSF_FORMAT_INFO);
  format_info.format:=k;
  sf_command(nil,SFC_GET_FORMAT_MAJOR,@format_info,sizeof(format_info));
  Log(audio_event,0,[pchar(format_info.name),':',pchar(format_info.extension)]);
 end;

 mpg123_init;

 spin_unlock_AudioInit;
end;

Procedure EnumOutputDevice(cb:TEnumDevice);
var
 i,len:PaDeviceIndex;
 di:PPaDeviceInfo;
 S:RawByteString;
begin
 if (cb=nil) then Exit;
 LazyAudioInit;
 len:=Pa_GetDeviceCount;
 if len>0 then
 For i:=0 to len-1 do
 begin
  di:=Pa_GetDeviceInfo(i);
  if (di<>nil) then
  if (di^.maxOutputChannels>0) then
  begin
   S:=pchar(di^._name);
   {$IFDEF WINDOWS}
    SetCodePage(S,GetACP,False);
    SetCodePage(S,System.DefaultRTLFileSystemCodePage,True);
   {$ENDIF}
   cb(i,S);
  end;
 end;
end;

Constructor TAudioThread.Create;
begin
 LazyAudioInit;
 inherited;
 FCanPlay:=False;
end;

Procedure TAudioThread.InherBegin;
begin
 hVoice:=nil;
end;

Procedure TAudioThread.InherEnd;
var
 Node:PQNode;
begin
 Node:=nil;
 While FQueue.Recv(Node) do
 begin
  SendFrom(Node);
 end;
 hVoice:=nil;
end;

function TAudioThread.OpenStream(FIndex:PaDeviceIndex;Channels:Integer;sampleRate:Double):PaStream;
Label
 to_default;
var
 err:Integer;
 D:PPaDeviceInfo;
 Param:PaStreamParameters;
begin
 Result:=nil;

 if (FIndex=-1) then
 begin
  to_default:
  err:=Pa_OpenDefaultStream(@Result,
                            0,
                            Channels,
                            paFloat32,
                            sampleRate,
                            paFramesPerBufferUnspecified,
                            nil,
                            nil);
  if (err<>0) then
  begin
   Pa_CloseStream(Result);
   Log(audio_event,err,'Pa_OpenDefaultStream');
   Exit(nil);
  end;
 end else
 begin
  D:=Pa_GetDeviceInfo(FIndex);
  if (D=nil) then Goto to_default;
  Param:=Default(PaStreamParameters);
  Param.device       :=FIndex;
  Param.channelCount :=Channels;
  Param.sampleFormat :=paFloat32;
  Param.suggestedLatency:=D^.defaultLowOutputLatency;
  err:=Pa_OpenStream(@Result,
                     nil,
                     @Param,
                     sampleRate,
                     paFramesPerBufferUnspecified,
                     paClipOff,
                     nil,
                     nil);
  if (err<>0) then
  begin
   Pa_CloseStream(Result);
   Log(audio_event,err,'Pa_OpenStream');
   Exit(nil);
  end;
 end;

 err:=Pa_StartStream(Result);

 if (err<>0) then
 begin
  Pa_CloseStream(Result);
  Log(audio_event,err,'Pa_StartStream');
  Exit(nil);
 end;
end;

Procedure TAudioThread.OnPlay_mpgfile(FStream:TStream;FVolume:Single;FIndex:PaDeviceIndex);
Label
 new_format;
var
 err:integer;
 mp3h:Tmpg123_handle;
 rate:clong;
 channels,encoding:cint;
 sndStream:PaStream;
 len,read_len:size_t;
 data:PSingle;
begin
 if (FStream=nil) then Exit;
 try
  mp3h:=mpg_open_stream(FStream);
  if (mp3h=nil) then
  begin
   Log(audio_event,1,[FStream,'mpg stream not open']);
   Exit;
  end;
  Log(audio_event,0,[FStream,'mpg stream open']);

  new_format:

  rate:=0;
  channels:=0;
  encoding:=0;
  err:=mpg123_getformat(mp3h,rate,channels,encoding);
  if (err<>0) then
  begin
   Log(audio_event,err,[FStream,'mpg123_getformat']);
   mpg123_close(mp3h);
   mpg123_delete(mp3h);
   Exit;
  end;

  sndStream:=OpenStream(FIndex,channels,rate);
  if (sndStream=nil) then
  begin
   mpg123_close(mp3h);
   mpg123_delete(mp3h);
   Exit;
  end;

  len:=1024;
  len:=(len div channels)*channels;
  data:=GetMem(len*SizeOf(Single));

  err:=0;
  While (not FState) and FCanPlay do
  begin

   err:=mpg123_read(mp3h,data,len,read_len);
   if (err=MPG123_NEW_FORMAT) then
   begin
    FreeMem(data);
    Pa_StopStream(sndStream);
    Pa_CloseStream(sndStream);
    Goto new_format;
   end;

   if (err<>0) then Break;
   if (read_len=0) then Break;
   read_len:=read_len div SizeOf(Single);

   fscale_data(data,read_len,FVolume);

   read_len:=read_len div channels;

   err:=Pa_WriteStream(sndStream,data,read_len);
   if (err<>0) then Break;
  end;

  FreeMem(data);

  if (err<>0) then
  begin
   Log(audio_event,err,'Pa_WriteStream');
  end;

  Pa_Sleep(100);
  Pa_StopStream(sndStream);

 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 Pa_CloseStream(sndStream);

 mpg123_close(mp3h);
 mpg123_delete(mp3h);
end;

Procedure TAudioThread.OnPlay_sndfile(FStream:TStream;FVolume:Single;FIndex:PaDeviceIndex);
var
 err:Integer;
 sfInfo:TSF_INFO;
 sndFile:TSNDFILE_HANDLE;
 sndStream:PaStream;

 len,read_len:SizeInt;
 data:PSingle;

begin
 if (FStream=nil) then Exit;
 try
  sfInfo:=Default(TSF_INFO);
  sndFile:=sf_open_stream(FStream,SFM_READ,sfInfo);
  if sndFile=nil then
  begin
   Log(audio_event,1,[FStream,' stream not opened']);
   Exit;
  end;
  err:=sf_error(sndFile);
  if (err<>0) or FState then
  begin
   sf_close(sndFile);
   Log(audio_event,err,[FStream,sf_error_number(err)]);
   Exit;
  end;
  Log(audio_event,0,[FStream,' stream open']);

  sf_command(sndFile,SFC_SET_NORM_FLOAT,nil,SF_TRUE);

  sndStream:=OpenStream(FIndex,sfInfo.channels,sfInfo.samplerate);
  if (sndStream=nil) then
  begin
   sf_close(sndFile);
   Exit;
  end;

  len:=1024;
  len:=len div sfInfo.channels;
  data:=GetMem(len*sfInfo.channels*SizeOf(Single));

  err:=0;
  While (not FState) and FCanPlay do
  begin
   read_len:=sf_readf_float(sndFile,data,len);
   if (read_len<=0) then Break;

   fscale_data(data,read_len*sfInfo.channels,FVolume);

   err:=Pa_WriteStream(sndStream,data,read_len);
   if (err<>0) then Break;
  end;

  FreeMem(data);

  if (err<>0) then
  begin
   Log(audio_event,err,'Pa_WriteStream');
  end;

  Pa_Sleep(100);
  Pa_StopStream(sndStream);

 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 Pa_CloseStream(sndStream);

 sf_close(sndFile);
end;

Procedure TAudioThread.OnPlay_sapi(FRate,FVolume:Integer;Const FText,FVoiceName,FAudioOutputName:RawByteString);

 procedure SetVoiceName(const name:RawByteString);
 var
  i,c:Integer;
  FVoices:ISpeechObjectTokens;
  FVoice:ISpeechObjectToken;
  W:WideString;
 begin
  if (Trim(name)='') then Exit;

  W:=UTF8Decode(Trim(name));
  //FVoice:=hVoice.Get_Voice;
  //if (FVoice<>nil) then
  // if (Trim(FVoice.GetDescription(0))=W) then Exit;
  FVoices:=hVoice.GetVoices('name='+W,'');
  if (FVoices=nil) then Exit;
  c:=FVoices.Count;
  if c<>0 then
  for i:=0 to c-1 do
  begin
   FVoice:=FVoices.Item(i);
   if (W=Trim(FVoice.GetDescription(0))) then
   begin
    hVoice._Set_Voice(FVoice);
    Break;
   end;
  end;
 end;

 procedure SetAudioOutputName(const name:RawByteString);
 var
  i,c:Integer;
  FVoices:ISpeechObjectTokens;
  FVoice:ISpeechObjectToken;
  W:WideString;
 begin
  if (Trim(name)='') then Exit;

  W:=UTF8Decode(Trim(name));
  //FVoice:=hVoice.Voice;
  //if (FVoice<>nil) then
  // if (Trim(FVoice.GetDescription(0))=W) then Exit;
  FVoices:=hVoice.GetAudioOutputs('name='+W,'');
  if (FVoices=nil) then Exit;
  c:=FVoices.Count;
  if c<>0 then
  for i:=0 to c-1 do
  begin
   FVoice:=FVoices.Item(i);
   if (W=Trim(FVoice.GetDescription(0))) then
   begin
    hVoice._Set_AudioOutput(FVoice);
    Break;
   end;
  end;
 end;

begin
 try
  if (hVoice=nil) then
  begin
   CoInitialize(nil);
   hVoice:=CoSpVoice.Create;
  end;
  if (hVoice=nil) then Exit;
  hVoice.Rate  :=FRate;
  hVoice.Volume:=FVolume;
  SetVoiceName(FVoiceName);
  SetAudioOutputName(FAudioOutputName);

  if FState or (not FCanPlay) then Exit;

  hVoice.Resume;
  hVoice.Speak(UTF8Decode(FText),SVSFlagsAsync or SVSFPurgeBeforeSpeak);

  While (not FState) and FCanPlay do
  begin
   hVoice.WaitUntilDone(200);
   if (hVoice.Status.RunningState=SRSEDone) then Break;
  end;
  hVoice.Skip('Sentence',1);

 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;
end;

Procedure EnumSAPIVoices(cb:TEnumSAPIVoiceCb);
var
 i,c:Integer;
 hVoice:ISpeechVoice;
 FVoices:ISpeechObjectTokens;
 FVoice:ISpeechObjectToken;
begin
 if (cb=nil) then Exit;
 CoInitialize(nil);
 hVoice:=CoSpVoice.Create;
 if (hVoice=nil) then Exit;
 FVoices:=hVoice.GetVoices('','');
 if (FVoices=nil) then Exit;
 c:=FVoices.Count;
 if c<>0 then
 for i:=0 to c-1 do
 begin
  FVoice:=FVoices.Item(i);
  cb(UTF8Encode(FVoice.GetDescription(0)),FVoice);
 end;
end;

Procedure EnumSAPIAudioOutputs(cb:TEnumSAPIVoiceCb);
var
 i,c:Integer;
 hVoice:ISpeechVoice;
 FVoices:ISpeechObjectTokens;
 FVoice:ISpeechObjectToken;
begin
 if (cb=nil) then Exit;
 CoInitialize(nil);
 hVoice:=CoSpVoice.Create;
 if (hVoice=nil) then Exit;
 FVoices:=hVoice.GetAudioOutputs('','');
 if (FVoices=nil) then Exit;
 c:=FVoices.Count;
 if c<>0 then
 for i:=0 to c-1 do
 begin
  FVoice:=FVoices.Item(i);
  cb(UTF8Encode(FVoice.GetDescription(0)),FVoice);
 end;
end;

Procedure TAudioThread.InherUpdate;
Var
 Node:PQNode;
 rc:PtrUInt;

begin
 Node:=nil;

 rc:=FQueue.Count;
 if rc>100 then rc:=100;
 While FQueue.Recv(Node) do
 begin
  try
   if FCanPlay then
    Case Node^.Ftype of
     0:With PQNodeStream(Node)^ do
        OnPlay_sndfile(FStream,FVolume,FIndex);
     1:With PQNodeStream(Node)^ do
        OnPlay_mpgfile(FStream,FVolume,FIndex);
     2:With PQNodeSAPI(Node)^ do
        OnPlay_sapi(FRate,FVolume,FText,FVoiceName,FAudioOutputName);
    end;
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
  SendFrom(Node);
  if rc=0 then Break;
  Dec(rc);
  if rc=0 then Break;
  if FState then Exit;
 end;

 if FQueue.IsEmpty and (not FState) then
 begin
  RTLeventWaitFor(FQueue.hEvent,RtlWaitTime);
 end;

end;

Constructor TAudioConnection.Create;
begin
 inherited;
 FThread:=TAudioThread.Create;
 if FThread.State then
 begin
  inherited Pause;
 end;
end;

Procedure TAudioConnection.Cleanup;
begin
 FreeAndNil(FThread);
 inherited;
end;

Function TAudioConnection.Start:SizeInt;
begin
 Result:=inherited;
 if Result=R_SUC then
 begin
  FThread.CanPlay:=True;
 end;
end;

Function TAudioConnection.Pause:SizeInt;
begin
 Result:=inherited;
 if Result=R_SUC then
 begin
  FThread.CanPlay:=False;
 end;
end;

Function TAudioConnection.Stop:SizeInt;
begin
 Result:=inherited;
 if Result=R_SUC then
 begin
  FThread.CanPlay:=False;
 end;
end;

function TAudioConnection.Send(Node:PQNode):Boolean;
begin
 Result:=FThread.Send(Node);
end;

Procedure TAudioHandle.Clear;
begin
 if FState then Exit;
 OnFinish:=nil;
 Node.FStream:=nil;
 Node.FVolume:=1;
 Node.FIndex:=-1;
end;

Procedure TAudioHandle.SetConnection(C:TAudioConnection);
begin
 if not State then
 if FAudioCon<>C then
 begin
  if Assigned(FAudioCon) then
  begin
   FAudioCon.Free;
  end;
  FAudioCon:=C;
  if Assigned(FAudioCon) then
  begin
   FAudioCon.Acquire;
  end;
 end;
end;

Procedure TAudioHandle.SetStream(S:TStream);
begin
 if not State then
 begin
  Node.FStream:=S;
 end;
end;

Constructor TAudioHandle.Create;
begin
 Node:=Default(TQNodeStream);
 Node.Parent:=@Finish;
 Node.FVolume:=1;
 Node.FIndex:=-1;
 Node.Ftype:=0;
 FState:=False;
end;

Destructor  TAudioHandle.Destroy;
begin
 Clear;
 SetConnection(nil);
end;

Procedure TAudioHandle.BreakSend;
begin
 FState:=True;
 Node.Parent:=@Finish;
 SendCurrQueue(@Node);
end;

Procedure TAudioHandle.Send;
begin
 if not Assigned(Self) then
 begin
  Log(app_logic,1,['AudioHandle.Send,Assigned(Self):',Self]);
  Exit;
 end;
 if FState then
 begin
  Log(app_logic,1,['AudioHandle.Send,FState:',FState]);
  Exit;
 end;

 Node.Parent:=@Finish;

 if not Assigned(FAudioCon) then
 begin
  Log(app_logic,1,['AudioHandle.Send,Assigned(FAudioCon):',FAudioCon]);
  Finish;
  Exit;
 end;

 if FAudioCon.Send(@Node) then
 begin
  FState:=True;
 end else
 begin
  Log(app_logic,1,['AudioHandle.SendRequest=false']);
  BreakSend;
  Exit;
 end;

end;

Procedure TAudioHandle.Finish;
begin
 FState:=False;
 if not Assigned(OnFinish) then
 begin
  Log(app_logic,1,['AudioHandle.Send,Assigned(OnFinish):',TMethod(OnFinish).Code,TMethod(OnFinish).Data]);
  Exit;
 end;
 Log(app_logic,0,['AudioHandle.Send,OnFinish:',TMethod(OnFinish).Code,TMethod(OnFinish).Data]);

 OnFinish();
end;

Procedure TAudioHandle.SetIsMpg(b:Boolean);
begin
 Case b of
  True :Node.Ftype:=0;
  False:Node.Ftype:=1;
 end;
end;

function  TAudioHandle.GetIsMpg:Boolean;
begin
 Result:=(Node.Ftype=1);
end;

/////////

Procedure TSAPIHandle.Clear;
begin
 if FState then Exit;
 OnFinish:=nil;
 Node.FRate:=0;
 Node.FVolume:=100;
 Node.FText:='';
 Node.FVoiceName:='';
 Node.FAudioOutputName:='';
end;

Procedure TSAPIHandle.SetConnection(C:TAudioConnection);
begin
 if not State then
 if FAudioCon<>C then
 begin
  if Assigned(FAudioCon) then
  begin
   FAudioCon.Free;
  end;
  FAudioCon:=C;
  if Assigned(FAudioCon) then
  begin
   FAudioCon.Acquire;
  end;
 end;
end;

Constructor TSAPIHandle.Create;
begin
 Node:=Default(TQNodeSAPI);
 Node.Parent:=@Finish;
 Node.FRate:=0;
 Node.FVolume:=100;
 Node.FText:='';
 Node.FVoiceName:='';
 Node.FAudioOutputName:='';
 Node.Ftype:=2;
 FState:=False;
end;

Destructor  TSAPIHandle.Destroy;
begin
 Clear;
 SetConnection(nil);
end;

Procedure TSAPIHandle.BreakSend;
begin
 FState:=True;
 Node.Parent:=@Finish;
 SendCurrQueue(@Node);
end;

Procedure TSAPIHandle.Send;
begin
 if not Assigned(Self) then
 begin
  Log(app_logic,1,['SAPIHandle.Send,Assigned(Self):',Self]);
  Exit;
 end;
 if FState then
 begin
  Log(app_logic,1,['SAPIHandle.Send,FState:',FState]);
  Exit;
 end;

 Node.Parent:=@Finish;

 if not Assigned(FAudioCon) then
 begin
  Log(app_logic,1,['SAPIHandle.Send,Assigned(FAudioCon):',FAudioCon]);
  Finish;
  Exit;
 end;

 if FAudioCon.Send(@Node) then
 begin
  FState:=True;
 end else
 begin
  Log(app_logic,1,['SAPIHandle.SendRequest=false']);
  BreakSend;
  Exit;
 end;

end;

Procedure TSAPIHandle.Finish;
begin
 FState:=False;
 if not Assigned(OnFinish) then
 begin
  Log(app_logic,1,['SAPIHandle.Send,Assigned(OnFinish):',TMethod(OnFinish).Code,TMethod(OnFinish).Data]);
  Exit;
 end;
 Log(app_logic,0,['SAPIHandle.Send,OnFinish:',TMethod(OnFinish).Code,TMethod(OnFinish).Data]);

 OnFinish();
end;

end.

