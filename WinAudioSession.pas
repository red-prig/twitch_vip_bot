{ Windows Audio per application volume interface

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

unit WinAudioSession;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,Windows,ActiveX;

type
 EDataFlow = (eRender,eCapture,eAll,EDataFlow_enum_count);

 ERole = (eConsole,eMultimedia,eCommunications,ERole_enum_count);

 TPropertyKey = record
   fmtid: TGUID;
   pid: DWORD;
 end;

const
 IID_MMDeviceEnumerator   :TGUID='{BCDE0395-E52F-467C-8E3D-C4579291692E}';
 IID_IAudioEndpointVolume :TGUID='{5CDF2C82-841E-4546-9722-0CF74078229A}';
 IID_IAudioSessionManager2:TGUID='{77AA99A0-1BD6-484F-8BC7-2C654C9A9B6F}';

 DEVICE_STATE_ACTIVE    =$00000001;
 DEVICE_STATE_DISABLED  =$00000002;
 DEVICE_STATE_NOTPRESENT=$00000004;
 DEVICE_STATE_UNPLUGGED =$00000008;
 DEVICE_STATEMASK_ALL   =$0000000F;

 PKEY_Device_FriendlyName:TPropertyKey=(fmtid:'{a45c254e-df1c-4efd-8020-67d146a850e0}';pid:14);

function PropVariantClear(var pvar:TPROPVARIANT):HRESULT; stdcall; external 'Ole32.dll';

type
  IAudioEndpointVolume = Interface(IUnknown)
   ['{5CDF2C82-841E-4546-9722-0CF74078229A}']

    Function RegisterControlChangeNotify(
              AudioEndPtVol: IUnknown): HRESULT; StdCall;
    Function UnregisterControlChangeNotify(
              AudioEndPtVol: IUnknown): HRESULT; StdCall;
    Function GetChannelCount(Out PInteger): HRESULT; StdCall;

    Function SetMasterVolumeLevel(
              fLevelDB: Single; pguidEventContext: PGUID): HRESULT; StdCall;
    Function SetMasterVolumeLevelScalar(
              fLevelDB: Single; pguidEventContext: PGUID): HRESULT; StdCall;

    Function GetMasterVolumeLevel(
              Out fLevelDB: Single): HRESULT; StdCall;
    Function GetMasterVolumeLevelScalar(
              Out fLevelDB: Single): HRESULT; StdCall;

    Function SetChannelVolumeLevel(
              nChannel: Integer; fLevelDB: Double; pguidEventContext: PGUID):
             HRESULT; StdCall;
    Function SetChannelVolumeLevelScalar(
              nChannel: Integer; fLevelDB: Double; pguidEventContext: PGUID):
             HRESULT; StdCall;

    Function GetChannelVolumeLevel(
              nChannel: Integer; Out fLevelDB: Double): HRESULT; StdCall;
    Function GetChannelVolumeLevelScalar(
              nChannel: Integer; Out fLevelDB: Double): HRESULT; StdCall;

    Function SetMute(
              bSetMute: Bool; pguidEventContext: PGUID): HRESULT; StdCall;
    Function GetMute(
              Out bGetMute: Bool): HRESULT; StdCall;

    Function GetVolumeStepInfo(
              pnStep: Integer; Out pnStepCount: Integer): HRESULT; StdCall;
    Function VolumeStepUp(
              pguidEventContext: PGUID): HRESULT; StdCall;
    Function VolumeStepDown(
              pguidEventContext: PGUID): HRESULT; StdCall;
    Function GetVolumeRange(
              Out pflVolumeMindB: Double; Out pflVolumeMaxdB: Double;
              Out pflVolumeIncrementdB: Double): HRESULT; StdCall;
    Function QueryHardwareSupport(
              Out pdwHardwareSupportMask): HRESULT; StdCall;
  End;

  IPropertyStore = interface(IUnknown)
    function GetCount(out cProps: DWORD): HRESULT; stdcall;
    function GetAt(iProp: DWORD; out key: TPropertyKey): HRESULT; stdcall;
    function GetValue(const key: TPropertyKey;
      out value: TPropVariant): HRESULT; stdcall;
  end;

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD;
      pActivationParams: PPropVariant;
      out EndpointVolume: IUnknown): HRESULT; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD;
      out Properties: IPropertyStore): HRESULT; stdcall;
    function GetId(out strId: LPWSTR): HRESULT; stdcall;
    function GetState(out State: DWORD): HRESULT; stdcall;
  end;

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out cDevices: UINT): HRESULT; stdcall;
    function Item(nDevice: UINT; out Device: IMMDevice): HRESULT; stdcall;
  end;

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: EDataFlow;
      dwStateMask: DWORD; out Devices: IMMDeviceCollection): HRESULT; stdcall;
    function GetDefaultAudioEndpoint(EDF: EDataFlow; role: ERole;
      out EndPoint: IMMDevice): HRESULT; stdcall;
    function GetDevice(pwstrId: LPWSTR; out EndPoint: IMMDevice): HRESULT; stdcall;
    function RegisterEndpointNotificationCallback(
      const Client: IUnknown): HRESULT; stdcall;
    function UnregisterEndpointNotificationCallback(
      const Client: IUnknown): HRESULT; stdcall;
  end;

  IAudioSessionControl2 = interface(IUnknown)
    ['{bfb7ff88-7239-4fc9-8fa2-07c950be9c6d}']
    function NotImpl0:UInt; stdcall;
    function GetDisplayName(out pRetVal:LPWSTR):HRESULT; stdcall;
    function NotImpl1:UInt; stdcall;
    function GetIconPath(out pRetVal:LPWSTR):HRESULT; stdcall;
    function NotImpl3:UInt; stdcall;
    function NotImpl4:UInt; stdcall;
    function NotImpl5:UInt; stdcall;
    function NotImpl6:UInt; stdcall;
    function NotImpl7:UInt; stdcall;
    function GetSessionIdentifier(out pRetVal:LPWSTR):HRESULT; stdcall;
    function GetSessionInstanceIdentifier(out pRetVal:LPWSTR):HRESULT; stdcall;
    function GetProcessId(out pRetVal:DWORD):HRESULT; stdcall;
    function IsSystemSoundsSession:HRESULT; stdcall;
    function SetDuckingPreference(optOut:WINBOOL):HRESULT; stdcall;
  end;

  IAudioSessionEnumerator = interface(IUnknown)
    ['{E2F5BB11-0570-40CA-ACDD-3AA01277DEE8}']
    function GetCount(out SessionCount:UINT): HRESULT; stdcall;
    function GetSession(nSession: UINT; out Session:IAudioSessionControl2): HRESULT; stdcall;
  end;

  IAudioSessionManager2 = interface(IUnknown)
    ['{77AA99A0-1BD6-484F-8BC7-2C654C9A9B6F}']
    function NotImpl1:UInt; stdcall;
    function NotImpl2:UInt; stdcall;
    function GetSessionEnumerator(out SessionEnum:IAudioSessionEnumerator):HRESULT; stdcall;
  end;

  ISimpleAudioVolume = interface(IUnknown)
    ['{87CE5498-68D6-44E5-9215-6DA47EF883D8}']
    function SetMasterVolume(fLevel:Single;EventContext:PGUID):HRESULT; stdcall;
    function GetMasterVolume(out pfLevel:Single):HRESULT; stdcall;
    function SetMute(bMute:BOOL;EventContext:PGUID):HRESULT; stdcall;
    function GetMute(out bMute:BOOL):HRESULT; stdcall;
  end;

  TCBEnumSessionsVolume=function(Const name:RawByteString;Volume:ISimpleAudioVolume):Boolean of object;
  TCBEnumDeviceAudio   =function(Device:IMMDevice):Boolean of object;

procedure SetVolume(Volume:ISimpleAudioVolume;percent:Byte);
function  GetVolume(Volume:ISimpleAudioVolume):Byte;
procedure SetMute(Volume:ISimpleAudioVolume;bMute:Boolean);
function  GetMute(Volume:ISimpleAudioVolume):Boolean;
function  GetDeviceName(Device:IMMDevice):RawByteString;
function  GetDeviceID(Device:IMMDevice):RawByteString;
function  GetDeviceVolume(Device:IMMDevice):IAudioEndpointVolume;
function  GetSessionAppName(Session:IAudioSessionControl2):RawByteString;
function  GetDefaultAudioEndpoint:IMMDevice;
procedure EnumDeviceAudio(dwStateMask:DWORD;cb:TCBEnumDeviceAudio);
function  EnumSessionsVolume(Device:IMMDevice;SystemSession:Boolean;cb:TCBEnumSessionsVolume):Boolean;
procedure EnumSessionsVolume(AllDevice,SystemSession:Boolean;cb:TCBEnumSessionsVolume);
procedure EnumSessionsVolume(Const id:RawByteString;SystemSession:Boolean;cb:TCBEnumSessionsVolume);
function  IsVaildDeviceID(Const id:RawByteString):Boolean;

implementation

procedure SetVolume(Volume:ISimpleAudioVolume;percent:Byte);
begin
 if Volume=nil then Exit;
 if percent>100 then percent:=100;
 Volume.SetMasterVolume(percent/100,nil);
end;

function GetVolume(Volume:ISimpleAudioVolume):Byte;
var
 pfLevel:Single;
begin
 Result:=0;
 if Volume=nil then Exit;
 pfLevel:=0;
 Volume.GetMasterVolume(pfLevel);
 Result:=Round(pfLevel*100);
end;

procedure SetMute(Volume:ISimpleAudioVolume;bMute:Boolean);
begin
 if Volume=nil then Exit;
 Volume.SetMute(bMute,nil);
end;

function GetMute(Volume:ISimpleAudioVolume):Boolean;
var
 bMute:BOOL;
begin
 Result:=True;
 if Volume=nil then Exit;
 bMute:=True;
 Volume.GetMute(bMute);
 Result:=bMute;
end;

function GetDeviceName(Device:IMMDevice):RawByteString;
var
 Prop:IPropertyStore;
 Name:TPROPVARIANT;
begin
 Result:='';
 if Device=nil then Exit;

 Prop:=nil;
 Device.OpenPropertyStore(STGM_READ,Prop);
 if Prop=nil then Exit;

 Name:=Default(TPROPVARIANT);
 if Prop.GetValue(PKEY_Device_FriendlyName,Name)<>S_OK then Exit;
 Prop:=nil;

 if Name.vt=VT_LPWSTR then
 begin
  Result:=UTF8Encode(WideString(Name.PWSZVAL));
 end;
 PropVariantClear(Name);
end;

function GetDeviceID(Device:IMMDevice):RawByteString;
var
 PWID:PWideChar;
begin
 Result:='';
 if Device=nil then Exit;

 PWID:=nil;
 if Device.GetId(PWID)<>S_OK then Exit;
 if PWID=nil then Exit;

 Result:=UTF8Encode(WideString(PWID));
 CoTaskMemFree(PWID);
end;

function GetDeviceVolume(Device:IMMDevice):IAudioEndpointVolume;
begin
 Result:=nil;
 if Device=nil then Exit;
 Device.Activate(IID_IAudioEndpointVolume,0,nil,IUnknown(Result));
end;

function GetDefaultAudioEndpoint:IMMDevice;
var
 deviceEnum:IMMDeviceEnumerator;
begin
 Result:=nil;
 deviceEnum:=nil;
 if CoCreateInstance(IID_MMDeviceEnumerator,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IMMDeviceEnumerator,deviceEnum)<>S_OK then Exit;
 if deviceEnum=nil then Exit;

 deviceEnum.GetDefaultAudioEndpoint(eRender,eMultimedia,Result);

 deviceEnum:=nil;
end;

procedure EnumDeviceAudio(dwStateMask:DWORD;cb:TCBEnumDeviceAudio);
var
 di,DevCount:UINT;
 deviceEnum:IMMDeviceEnumerator;
 Endpoints:IMMDeviceCollection;
 Device:IMMDevice;
begin
 if cb=nil then Exit;
 deviceEnum:=nil;
 if CoCreateInstance(IID_MMDeviceEnumerator,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IMMDeviceEnumerator,deviceEnum)<>S_OK then Exit;
 if deviceEnum=nil then Exit;

 Device:=nil;

 Endpoints:=nil;
 if deviceEnum.EnumAudioEndpoints(eRender,dwStateMask,Endpoints)<>S_OK then Exit;
 if Endpoints=nil then Exit;
 DevCount:=0;
 if Endpoints.GetCount(DevCount)<>S_OK then Exit;
 if DevCount<>0 then
  For di:=0 to DevCount-1 do
  begin
   Endpoints.Item(di,Device);
   if Device<>nil then
    if not cb(Device) then Break;
   Device:=nil;
  end;

 Device:=nil;
 Endpoints:=nil;
 deviceEnum:=nil;
end;

function GetSessionAppName(Session:IAudioSessionControl2):RawByteString;
var
 PWID:PWideChar;
 WS:WideString;
 i:Integer;
begin
 Result:='';
 if Session=nil then Exit;

 PWID:=nil;
 if Session.GetSessionInstanceIdentifier(PWID)<>S_OK then Exit;
 if (PWID=nil) then Exit;
 WS:=WideString(PWID);
 CoTaskMemFree(PWID);
 WS:=Trim(WS);
 if WS='' then Exit;

 i:=Pos('|',WS);
 if i<>0 then
 begin
  WS:=Copy(WS,i+1);
 end;

 i:=Pos('%b',WS);
 if i<>0 then
 begin
  WS:=Copy(WS,1,i-1);
 end;

 WS:=ExtractFileName(WS);
 WS:=ChangeFileExt(WS,WideString(''));

 Result:=UTF8Encode(WS);
end;

function EnumSessionsVolume(Device:IMMDevice;SystemSession:Boolean;cb:TCBEnumSessionsVolume):Boolean;
var
 si,SesCount:UINT;
 SessionManager:IAudioSessionManager2;
 SessionEnum:IAudioSessionEnumerator;
 Session1,Session2:IAudioSessionControl2;
 SimpleAudioVolume:ISimpleAudioVolume;
begin
 Result:=True;
 if cb=nil then Exit;
 if Device=nil then Exit;

 SessionManager:=nil;
 if Device.Activate(IID_IAudioSessionManager2,0,nil,IUnknown(SessionManager))<>S_OK then Exit;
 if SessionManager=nil then Exit;

 SessionEnum:=nil;
 SessionManager.GetSessionEnumerator(SessionEnum);
 SessionManager:=nil;

 SesCount:=0;
 if SessionEnum.GetCount(SesCount)<>S_OK then Exit;

 if SesCount<>0 then
  For si:=0 to SesCount-1 do
  begin
   Session1:=nil;
   if (SessionEnum.GetSession(si,Session1)=S_OK) then
   if (Session1<>nil) then
   begin
    Session2:=nil;
    if (Session1.QueryInterface(IAudioSessionControl2,Session2)=S_OK) then
    if (Session2<>nil) then
    begin
     if (Session2.IsSystemSoundsSession<>0) or SystemSession then
     begin
      SimpleAudioVolume:=nil;
      if (Session2.QueryInterface(ISimpleAudioVolume,SimpleAudioVolume)=S_OK) then
      if (SimpleAudioVolume<>nil) then
      begin
       if not cb(GetSessionAppName(Session2),SimpleAudioVolume) then
       begin
        Result:=False;
        Break;
       end;
      end;
     end;
    end;
   end;
  end;

 Session1:=nil;
 Session2:=nil;
 SessionEnum:=nil;
end;

procedure EnumSessionsVolume(AllDevice,SystemSession:Boolean;cb:TCBEnumSessionsVolume);
var
 di,DevCount:UINT;
 deviceEnum:IMMDeviceEnumerator;
 Endpoints:IMMDeviceCollection;
 Device:IMMDevice;
begin
 if cb=nil then Exit;
 deviceEnum:=nil;
 if CoCreateInstance(IID_MMDeviceEnumerator,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IMMDeviceEnumerator,deviceEnum)<>S_OK then Exit;
 if deviceEnum=nil then Exit;

 Device:=nil;
 if AllDevice then
 begin
  Endpoints:=nil;
  if deviceEnum.EnumAudioEndpoints(eRender,DEVICE_STATE_ACTIVE,Endpoints)<>S_OK then Exit;
  if Endpoints=nil then Exit;
  DevCount:=0;
  if Endpoints.GetCount(DevCount)<>S_OK then Exit;
  if DevCount<>0 then
   For di:=0 to DevCount-1 do
   begin
    Endpoints.Item(di,Device);
    EnumSessionsVolume(Device,SystemSession,cb);
    Device:=nil;
   end;
  Endpoints:=nil;
 end else
 begin
  if deviceEnum.GetDefaultAudioEndpoint(eRender,eMultimedia,Device)<>S_OK then Exit;
  EnumSessionsVolume(Device,SystemSession,cb);
  Device:=nil;
 end;

 deviceEnum:=nil;
end;

procedure EnumSessionsVolume(Const id:RawByteString;SystemSession:Boolean;cb:TCBEnumSessionsVolume);
var
 W:WideString;
 deviceEnum:IMMDeviceEnumerator;
 Device:IMMDevice;
begin
 if (cb=nil) or (id='') then Exit;
 deviceEnum:=nil;
 if CoCreateInstance(IID_MMDeviceEnumerator,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IMMDeviceEnumerator,deviceEnum)<>S_OK then Exit;
 if deviceEnum=nil then Exit;

 Device:=nil;
 W:=UTF8Decode(id);
 if deviceEnum.GetDevice(PWideChar(W),Device)<>S_OK then Exit;
 EnumSessionsVolume(Device,SystemSession,cb);
 Device:=nil;

 deviceEnum:=nil;
end;

function IsVaildDeviceID(Const id:RawByteString):Boolean;
var
 W:WideString;
 deviceEnum:IMMDeviceEnumerator;
 Device:IMMDevice;
begin
 Result:=False;
 if (id='') then Exit;

 deviceEnum:=nil;
 if CoCreateInstance(IID_MMDeviceEnumerator,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IMMDeviceEnumerator,deviceEnum)<>S_OK then Exit;
 if deviceEnum=nil then Exit;

 Device:=nil;
 W:=UTF8Decode(id);
 if deviceEnum.GetDevice(PWideChar(W),Device)<>S_OK then Exit;
 Result:=(Device<>nil);
 Device:=nil;
 deviceEnum:=nil;
end;

initialization
 CoInitialize(nil);

finalization
 CoUninitialize;

end.

