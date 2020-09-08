unit ufrmvol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  CheckLst,
  WinAudioSession;

type

  { TFrmVolParam }

  TFrmVolParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBVolEnable: TCheckBox;
    CBSystemSound: TCheckBox;
    CBSessions: TCheckListBox;
    CMBDevices: TComboBox;
    LDevices: TLabel;
    LApps: TLabel;
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
    procedure CBSessionsClickCheck(Sender: TObject);
    procedure CBSystemSoundClick(Sender: TObject);
    procedure CMBDevicesSelect(Sender: TObject);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure FormShow(Sender: TObject);
  private

  public
    FDeviceID:RawByteString;
    FExclude:TStringList;
    Procedure SetVolExclude(Const F:RawByteString);
    function  GetVolExclude:RawByteString;
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
    procedure OnForClear(const CurrentValue:string;const index:integer;Obj:TObject);
    function  OnEnumDevice(Device:IMMDevice):Boolean;
    function  OnSessions(Const fname:RawByteString;Volume:ISimpleAudioVolume):Boolean;
    procedure UpdateSessions;
  end;

var
  FrmVolParam: TFrmVolParam;

Function GetVolumeInfo(Const name:RawByteString;Volume:ISimpleAudioVolume):RawByteString;
Function GetSessionsStr:RawByteString;
Function FindSessionsStr(const name:RawByteString):ISimpleAudioVolume;

implementation

{$R *.lfm}

uses
 ULog,Main;

type
 TSessionList=object
   L:TStringList;
   function OnSessions(Const name:RawByteString;Volume:ISimpleAudioVolume):Boolean;
 end;

 TSessionFind=object
  S:RawByteString;
  R:ISimpleAudioVolume;
  function OnSessions(Const name:RawByteString;Volume:ISimpleAudioVolume):Boolean;
 end;


Function GetVolumeInfo(Const name:RawByteString;Volume:ISimpleAudioVolume):RawByteString;
begin
 if GetMute(Volume) then
  Result:=LowerCase(name)+':M'
 else
  Result:=LowerCase(name)+':'+IntToStr(GetVolume(Volume));
end;

function TSessionList.OnSessions(Const name:RawByteString;Volume:ISimpleAudioVolume):Boolean;
Var
 F:RawByteString;
begin
 Result:=True;
 F:=LowerCase(name);
 if vol_cmd.Exclude.IndexOf(F)=-1 then
  if L.IndexOfName(F)=-1 then
  begin
   L.Add(GetVolumeInfo(name,Volume))
  end;
end;

Function GetSessionsStr:RawByteString;
var
 SL:TSessionList;
begin
 SL.L:=TStringList.Create;
 SL.L.LineBreak:=', ';
 SL.L.NameValueSeparator:=':';

 case vol_cmd.Device of
  'all'    :EnumSessionsVolume(True ,vol_cmd.System,@SL.OnSessions);
  'default':EnumSessionsVolume(False,vol_cmd.System,@SL.OnSessions);
  else
            EnumSessionsVolume(vol_cmd.Device,vol_cmd.System,@SL.OnSessions);
 end;

 Result:=SL.L.Text;
 FreeAndNil(SL.L);
end;

function TSessionFind.OnSessions(Const name:RawByteString;Volume:ISimpleAudioVolume):Boolean;
begin
 if R<>nil then Exit(false);
 Result:=True;
 if LowerCase(name)=S then
 begin
  R:=Volume;
  Result:=False;
 end;
end;

Function FindSessionsStr(const name:RawByteString):ISimpleAudioVolume;
Var
 F:TSessionFind;
begin
 Result:=nil;
 if vol_cmd.Exclude.IndexOf(name)=-1 then
 begin
  F:=Default(TSessionFind);
  F.S:=LowerCase(name);

  case vol_cmd.Device of
   'all'    :EnumSessionsVolume(True ,vol_cmd.System,@F.OnSessions);
   'default':EnumSessionsVolume(False,vol_cmd.System,@F.OnSessions);
   else
             EnumSessionsVolume(vol_cmd.Device,vol_cmd.System,@F.OnSessions);
  end;

  Result:=F.R;
 end;
end;

Procedure TFrmVolParam.SetVolExclude(Const F:RawByteString);
var
 i:SizeInt;
 param,v:RawByteString;
begin
 if Vol_cmd.Exclude=nil then
 begin
  Vol_cmd.Exclude:=TStringList.Create;
  Vol_cmd.Exclude.LineBreak:=',';
  Vol_cmd.Exclude.Sorted:=True;
 end;
 Vol_cmd.Exclude.Clear;

 param:=F;
 repeat
  i:=System.IndexChar(PAnsiChar(param)^,Length(param),',');
  if i<>-1 then
  begin
   v:=Copy(param,1,i);
   param:=Copy(param,i+2,Length(param)-(i+1));
  end else
  begin
   v:=param;
   param:='';
  end;
  v:=Trim(v);
  if v<>'' then
  begin
   v:=LowerCase(v);
   if Vol_cmd.Exclude.IndexOf(v)=-1 then
   begin
    Vol_cmd.Exclude.Add(v);
   end;
  end;
 until (param='');
end;

function TFrmVolParam.GetVolExclude:RawByteString;
begin
 Result:='';
 if Vol_cmd.Exclude<>nil then
 begin
  Result:=Vol_cmd.Exclude.Text;
 end;
end;

Procedure TFrmVolParam.InitCfg;
begin
 case Vol_cmd.Enable of
  True :Config.WriteString('vol' ,'enable','1');
  False:Config.WriteString('vol' ,'enable','0');
 end;

 case Vol_cmd.System of
  True :Config.WriteString('vol' ,'system','1');
  False:Config.WriteString('vol' ,'system','0');
 end;

 Config.WriteString('vol','device' ,Vol_cmd.Device);
 Config.WriteString('vol','exclude',GetVolExclude);
end;

Procedure TFrmVolParam.LoadCfg;
begin
 Vol_cmd.Enable :=Trim(Config.ReadString('vol','enable','0'))='1';
 Vol_cmd.System :=Trim(Config.ReadString('vol','system','0'))='1';
 Vol_cmd.Device :=LowerCase(Trim(Config.ReadString('vol','device' ,Vol_cmd.Device)));
 SetVolExclude((Config.ReadString('vol','exclude',GetVolExclude)));
end;

Procedure TFrmVolParam.Open;
begin
 CBVolEnable.Checked  :=Vol_cmd.Enable;
 CBSystemSound.Checked:=Vol_cmd.System;
 FDeviceID            :=Vol_cmd.Device;

 if FExclude=nil then
 begin
  FExclude:=TStringList.Create;
  FExclude.Sorted:=True;
 end;
 FExclude.Assign(Vol_cmd.Exclude);

 if ShowModal=1 then
 begin

  Vol_cmd.Enable :=CBVolEnable.Checked;
  Vol_cmd.System :=CBSystemSound.Checked;
  Vol_cmd.Device :=FDeviceID;
  Vol_cmd.Exclude.Assign(FExclude);
  FreeAndNil(FExclude);

  try
   case Vol_cmd.Enable of
    True :Config.WriteString('vol' ,'enable','1');
    False:Config.WriteString('vol' ,'enable','0');
   end;

   case Vol_cmd.System of
    True :Config.WriteString('vol' ,'system','1');
    False:Config.WriteString('vol' ,'system','0');
   end;

   Config.WriteString('vol','device' ,Vol_cmd.Device);
   Config.WriteString('vol','exclude',GetVolExclude);
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;

 end;
end;

procedure TFrmVolParam.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmVolParam.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmVolParam.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmVolParam.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

type
 TObjStrId=class
  Id:RawByteString;
 end;

procedure TFrmVolParam.OnForClear(const CurrentValue:string;const index:integer;Obj:TObject);
begin
 FreeAndNil(Obj);
end;

function TFrmVolParam.OnEnumDevice(Device:IMMDevice):Boolean;
Var
 A:TObjStrId;
begin
 Result:=True;
 A:=TObjStrId.Create;
 A.Id:=GetDeviceID(Device);
 CMBDevices.AddItem(GetDeviceName(Device),A);
 if FDeviceID=A.Id then
  CMBDevices.ItemIndex:=CMBDevices.Items.Count-1;
end;

function TFrmVolParam.OnSessions(Const fname:RawByteString;Volume:ISimpleAudioVolume):Boolean;
Var
 L:RawByteString;
begin
 Result:=True;
 L:=LowerCase(fname);
 if CBSessions.Items.IndexOf(L)=-1 then
 begin
  CBSessions.AddItem(L,nil);
  CBSessions.Checked[CBSessions.Count-1]:=True;
  if (FExclude<>nil) and (FExclude.IndexOf(L)<>-1) then
  begin
   CBSessions.Checked[CBSessions.Count-1]:=False;
  end;
 end;
end;

procedure TFrmVolParam.CBSessionsClickCheck(Sender: TObject);
var
 F:RawByteString;
 i:Integer;
begin
 if (FExclude<>nil) then
 begin
  F:=CBSessions.Items[CBSessions.ItemIndex];
  case CBSessions.Checked[CBSessions.ItemIndex] of
   True :
    begin
     if (FExclude<>nil) then
     begin
      i:=FExclude.IndexOf(F);
      if (i<>-1) then
      begin
       FExclude.Delete(i);
      end;
     end;
    end;
   False:
    begin
     if (FExclude<>nil) then
     begin
      i:=FExclude.IndexOf(F);
      if (i=-1) then
      begin
       FExclude.Add(F);
      end;
     end;
    end;
  end;
 end;
end;

procedure TFrmVolParam.CBSystemSoundClick(Sender: TObject);
begin
 UpdateSessions;
end;

procedure TFrmVolParam.CMBDevicesSelect(Sender: TObject);
Var
 A:TObjStrId;
begin
 A:=TObjStrId(CMBDevices.Items.Objects[CMBDevices.ItemIndex]);
 if A<>nil then
 begin
  FDeviceID:=A.Id;
 end;
 UpdateSessions;
end;

procedure TFrmVolParam.FormShow(Sender: TObject);
Var
 A:TObjStrId;
begin
 CMBDevices.Items.ForEach(@OnForClear);
 CMBDevices.Clear;
 CMBDevices.ItemIndex:=-1;
 A:=TObjStrId.Create;
 A.Id:='all';
 CMBDevices.AddItem('Все',A);
 A:=TObjStrId.Create;
 A.Id:='default';
 CMBDevices.AddItem('По умолчанию',A);
 case FDeviceID of
  'all'    :CMBDevices.ItemIndex:=0;
  'default':CMBDevices.ItemIndex:=1;
 end;
 EnumDeviceAudio(DEVICE_STATE_ACTIVE,@OnEnumDevice);
 UpdateSessions;
end;

procedure TFrmVolParam.UpdateSessions;
begin
 CBSessions.Clear;
 case FDeviceID of
  'all'    :EnumSessionsVolume(True ,CBSystemSound.Checked,@OnSessions);
  'default':EnumSessionsVolume(False,CBSystemSound.Checked,@OnSessions);
  else
            EnumSessionsVolume(FDeviceID,CBSystemSound.Checked,@OnSessions);
 end;
end;

end.

