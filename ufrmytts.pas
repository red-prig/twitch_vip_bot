unit UFrmYtts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, libportaudio, AudioEngine, ytts;

type

  { TFrmYtts }

  TFrmYtts = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CMBDevices: TComboBox;
    CMBVoice: TComboBox;
    CMBEmotion: TComboBox;
    CMBMode: TComboBox;
    LDevices: TLabel;
    LSpeed: TLabel;
    LVolume: TLabel;
    LVoice: TLabel;
    LEmotion: TLabel;
    LMode: TLabel;
    TBSpeed: TTrackBar;
    TBVolume: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure BtnCancelClick(Sender:TObject);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure TBSpeedChange(Sender: TObject);
    procedure TBVolumeChange(Sender: TObject);
  private

  public
    FPlayRef:SizeInt;

    Procedure YttsSetParam;
    Procedure LazyInitYtts;

    procedure OnEnumDevice(Index:PaDeviceIndex;Const _name:RawByteString);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
    procedure SetDevice(Index:PaDeviceIndex);
    function  GetDevice:PaDeviceIndex;
    procedure LoadDevice;
    function  GetTBSpeed:Currency;
    procedure SetTBSpeed(FSpeed:Currency);
    function  GetTBVolume:Single;
    procedure SetTBVolume(FVolume:Single);
    procedure OnStartPlay(Sender:TObject);
    procedure OnStopPlay(Sender:TObject);
    procedure add_to_chat_cmd(highlighted:Boolean;const cmd,param,msg:RawByteString);
  end;

var
 FrmYtts: TFrmYtts;

 Fytts:Tytts;

 ytts_param:record
  Fmode:Integer;
  FIndex:PaDeviceIndex;
  FVolume:Single;
  FVoice:TyVoice;
  FEmotion:TyEmotion;
  FSpeed:Currency;
 end;

implementation

{$R *.lfm}

uses
 uLog,
 main;

procedure TFrmYtts.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmYtts.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmYtts.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmYtts.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

procedure TFrmYtts.SetDevice(Index:PaDeviceIndex);
var
 i:Integer;
begin
 if CMBDevices.Items.Count>0 then
 For i:=0 to CMBDevices.Items.Count-1 do
  if (PtrInt(Index)=PtrInt(CMBDevices.Items.Objects[i])) then
  begin
   CMBDevices.ItemIndex:=i;
   Exit;
  end;
 CMBDevices.ItemIndex:=0;
end;

function TFrmYtts.GetDevice:PaDeviceIndex;
begin
 Result:=-1;
 if (CMBDevices.ItemIndex<>-1) then
 if (CMBDevices.Items.Count<>0) then
  Result:=PaDeviceIndex(PtrInt(CMBDevices.Items.Objects[CMBDevices.ItemIndex]));
end;

procedure TFrmYtts.OnEnumDevice(Index:PaDeviceIndex;Const _name:RawByteString);
begin
 CMBDevices.AddItem(_name,TObject(Pointer(Index)));
end;

procedure TFrmYtts.LoadDevice;
begin
 CMBDevices.Clear;
 CMBDevices.AddItem('По умолчанию',TObject(Pointer(-1)));
 EnumOutputDevice(@OnEnumDevice);
end;

procedure TFrmYtts.FormCreate(Sender: TObject);
var
 _Voice:TyVoice;
 _Emotion:TyEmotion;
begin
 For _Voice:=Low(TyVoice) to High(TyVoice) do
  Case _Voice of
   alyss :CMBVoice.AddItem('Элиc'  ,nil);
   jane  :CMBVoice.AddItem('Джейн' ,nil);
   oksana:CMBVoice.AddItem('Оксана',nil);
   omazh :CMBVoice.AddItem('Омаж'  ,nil);
   zahar :CMBVoice.AddItem('Захар' ,nil);
   ermil :CMBVoice.AddItem('Ермил' ,nil);
   alena :CMBVoice.AddItem('Алёна' ,nil);
   filipp:CMBVoice.AddItem('Филипп',nil);
  end;

 For _Emotion:=Low(TyEmotion) to High(TyEmotion) do
  Case _Emotion of
   good   :CMBEmotion.AddItem('Радостный'   ,nil);
   evil   :CMBEmotion.AddItem('Раздражённый',nil);
   neutral:CMBEmotion.AddItem('Нейтральный' ,nil);
  end;
 FPlayRef:=0;
end;


Procedure TFrmYtts.YttsSetParam;
begin
 if (Fytts<>nil) then
 begin
  Fytts.DeviceIndex:=ytts_param.FIndex;
  Fytts.Volume     :=ytts_param.FVolume;
  Fytts.Speed      :=ytts_param.FSpeed;
  Fytts.Voice      :=ytts_param.FVoice;
  Fytts.Emotion    :=ytts_param.FEmotion;
  Fytts.StartPlay  :=@OnStartPlay;
  Fytts.StopPlay   :=@OnStopPlay;
 end;
end;

Procedure TFrmYtts.LazyInitYtts;
begin
 frmMain.LazyInitAudioThread;
 if (Fytts=nil) then
 begin
  Fytts:=Tytts.Create;
  YttsSetParam;
 end;
end;

procedure LoadVoice(Const Value:RawByteString);
begin
 case LowerCase(Value) of
  'alyss' :ytts_param.FVoice:=alyss ;
  'jane'  :ytts_param.FVoice:=jane  ;
  'oksana':ytts_param.FVoice:=oksana;
  'omazh' :ytts_param.FVoice:=omazh ;
  'zahar' :ytts_param.FVoice:=zahar ;
  'ermil' :ytts_param.FVoice:=ermil ;
  'alena' :ytts_param.FVoice:=alena ;
  'filipp':ytts_param.FVoice:=filipp;
 end;
end;

Function SaveVoice:RawByteString;
begin
 Case ytts_param.FVoice of
  alyss :Result:='alyss' ;
  jane  :Result:='jane'  ;
  oksana:Result:='oksana';
  omazh :Result:='omazh' ;
  zahar :Result:='zahar' ;
  ermil :Result:='ermil' ;
  alena :Result:='alena' ;
  filipp:Result:='filipp';
 end;
end;

procedure LoadEmotion(Const Value:RawByteString);
begin
 case LowerCase(Value) of
  'good'   :ytts_param.FEmotion:=good;
  'evil'   :ytts_param.FEmotion:=evil;
  'neutral':ytts_param.FEmotion:=neutral;
 end;
end;

Function SaveEmotion:RawByteString;
begin
 Case ytts_param.FEmotion of
  good   :Result:='good';
  evil   :Result:='evil';
  neutral:Result:='neutral';
 end;
end;

Procedure TFrmYtts.InitCfg;
begin
 ytts_param.Fmode   :=0;
 ytts_param.FIndex  :=-1;
 ytts_param.FVolume :=0.5;
 ytts_param.FVoice  :=filipp;
 ytts_param.FEmotion:=neutral;
 ytts_param.FSpeed  :=1;
end;

Procedure TFrmYtts.LoadCfg;
begin
 InitCfg;

 ytts_param.Fmode:=StrToIntDef(Config.ReadString('ytts','mode',IntToStr(ytts_param.Fmode)),ytts_param.Fmode);
 if ytts_param.Fmode<0 then ytts_param.Fmode:=0;
 if ytts_param.Fmode>3 then ytts_param.Fmode:=0;

 ytts_param.FIndex :=StrToIntDef  (Config.ReadString('ytts','device',IntToStr  (ytts_param.FIndex)) ,ytts_param.FIndex);
 ytts_param.FVolume:=StrToFloatDef(Config.ReadString('ytts','volume',FloatToStrF(ytts_param.FVolume,ffFixed,2,2)),ytts_param.FVolume);

 LoadVoice  (Config.ReadString('ytts','voice',''));
 LoadEmotion(Config.ReadString('ytts','emotion',''));

 ytts_param.FSpeed:=StrToCurrDef(Config.ReadString('ytts','speed',CurrToStr(ytts_param.FSpeed)),ytts_param.FSpeed);

end;

Procedure TFrmYtts.Open;
begin
 LoadDevice;
 SetDevice(ytts_param.FIndex);
 CMBMode   .ItemIndex:=ytts_param.Fmode;
 CMBVoice  .ItemIndex:=Ord(ytts_param.FVoice);
 CMBEmotion.ItemIndex:=Ord(ytts_param.FEmotion);
 SetTBSpeed (ytts_param.FSpeed);
 SetTBVolume(ytts_param.FVolume);

 if ShowModal=1 then
 begin
  ytts_param.FIndex  :=GetDevice;
  ytts_param.Fmode   :=CMBMode.ItemIndex;
  ytts_param.FVoice  :=TyVoice(CMBVoice.ItemIndex);
  ytts_param.FEmotion:=TyEmotion(CMBEmotion.ItemIndex);
  ytts_param.FSpeed  :=GetTBSpeed;
  ytts_param.FVolume :=GetTBVolume;
  YttsSetParam;

  try
   Config.WriteString('ytts','mode'   ,IntToStr  (ytts_param.Fmode));
   Config.WriteString('ytts','device' ,IntToStr  (ytts_param.FIndex));
   Config.WriteString('ytts','volume' ,FloatToStrF(ytts_param.FVolume,ffFixed,2,2));
   Config.WriteString('ytts','voice'  ,SaveVoice);
   Config.WriteString('ytts','emotion',SaveEmotion);
   Config.WriteString('ytts','speed'  ,CurrToStr(ytts_param.FSpeed));
  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;

 end;
end;

function TFrmYtts.GetTBSpeed:Currency;
begin
 Result:=0.5+(TBSpeed.Position/10);
end;


procedure TFrmYtts.SetTBSpeed(FSpeed:Currency);
begin
 if FSpeed<0.5 then FSpeed:=0.5;
 if FSpeed>3.0 then FSpeed:=3.0;
 TBSpeed.Position:=Round((FSpeed-0.5)*10);
end;

procedure TFrmYtts.TBSpeedChange(Sender: TObject);
begin
 LSpeed.Caption:='Скорость: '+CurrToStr(GetTBSpeed);
end;

function TFrmYtts.GetTBVolume:Single;
begin
 Result:=TBVolume.Position/50;
end;

procedure TFrmYtts.SetTBVolume(FVolume:Single);
begin
 if FVolume<0 then FVolume:=0;
 if FVolume>1 then FVolume:=1;
 TBVolume.Position:=Round(FVolume*50);
end;

procedure TFrmYtts.TBVolumeChange(Sender: TObject);
begin
 LVolume.Caption:='Громкость: '+FloatToStrF(GetTBVolume,ffFixed,2,2);
end;

procedure TFrmYtts.OnStartPlay(Sender:TObject);
begin
 Inc(FPlayRef);
end;

procedure TFrmYtts.OnStopPlay(Sender:TObject);
begin
 Dec(FPlayRef);
end;

procedure TFrmYtts.add_to_chat_cmd(highlighted:Boolean;const cmd,param,msg:RawByteString);
Const
 maxqueue=2;
begin
 if frmMain.BtnPlay.CaptionState<>0 then Exit;
 Case ytts_param.Fmode of
  1:begin
     if (cmd<>'') and (cmd[1]<>'!') then
      if (FPlayRef<=maxqueue) or highlighted then
      begin
       LazyInitYtts;
       Fytts.push_msg(msg);
      end;
    end;
  2:begin
     if (cmd='!ytts') then
      if (param<>'') then
       if (FPlayRef<=maxqueue) or highlighted then
       begin
        LazyInitYtts;
        Fytts.push_msg(param);
       end;
    end;
  3:begin
     if highlighted then
      if (msg<>'') then
      begin
       LazyInitYtts;
       Fytts.push_msg(msg);
      end;
    end;
 end;
end;

end.

