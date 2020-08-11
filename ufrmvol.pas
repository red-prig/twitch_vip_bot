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
    procedure OnForClear(const CurrentValue:string;const index:integer;Obj:TObject);
    function  OnEnumDevice(Device:IMMDevice):Boolean;
    function  OnSessions(Const fname:RawByteString;Volume:ISimpleAudioVolume):Boolean;
    procedure UpdateSessions;
  end;

var
  FrmVolParam: TFrmVolParam;

implementation

{$R *.lfm}

uses
 Main;

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

