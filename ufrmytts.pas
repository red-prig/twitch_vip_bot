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
    CMBDevicesY: TComboBox;
    CMBDevicesS: TComboBox;
    CMBEmotionY: TComboBox;
    CMBMode: TComboBox;
    CMBIntf: TComboBox;
    CMBVoiceY: TComboBox;
    CMBVoiceS: TComboBox;
    LDevicesY: TLabel;
    LDevicesS: TLabel;
    LEmotionY: TLabel;
    LMode: TLabel;
    LIntf: TLabel;
    LSpeedY: TLabel;
    LSpeedS: TLabel;
    LVoiceY: TLabel;
    LVoiceS: TLabel;
    LVolumeY: TLabel;
    LVolumeS: TLabel;
    PageControlTts: TPageControl;
    TBSpeedY: TTrackBar;
    TBSpeedS: TTrackBar;
    TBVolumeY: TTrackBar;
    TBVolumeS: TTrackBar;
    TSYtts: TTabSheet;
    TSSAPI: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure BtnCancelClick(Sender:TObject);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure TBSpeedSChange(Sender: TObject);
    procedure TBSpeedYChange(Sender: TObject);
    procedure TBVolumeSChange(Sender: TObject);
    procedure TBVolumeYChange(Sender: TObject);
  private

  public
    FPlayRef:SizeInt;

    Procedure YttsSetParam;
    Procedure LazyInitYtts;

    procedure OnEnumDeviceY(Index:PaDeviceIndex;Const _name:RawByteString);
    procedure OnEnumDeviceS(const _name:RawByteString;Voice:ISpeechObjectToken);
    procedure OnEnumVoicesS(const _name:RawByteString;Voice:ISpeechObjectToken);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
    procedure SetDeviceY(Index:PaDeviceIndex);
    function  GetDeviceY:PaDeviceIndex;
    procedure LoadDeviceY;
    procedure LoadDeviceS;
    procedure LoadVoicesS;
    procedure SetDeviceS(const _name:RawByteString);
    procedure SetVoicesS(const _name:RawByteString);
    function  GetTBSpeed:Currency;
    procedure SetTBSpeed(FSpeed:Currency);
    function  GetTBVolumeY:Single;
    procedure SetTBVolumeY(FVolume:Single);
    procedure SetTBVolumeS(FVolume:Integer);
    function  GetTBVolumeS:Integer;
    procedure OnStartPlay(Sender:TObject);
    procedure OnStopPlay(Sender:TObject);
    procedure push_msg(msg:RawByteString);
    procedure add_to_chat_cmd(highlighted:Boolean;const cmd,param,msg:RawByteString);
  end;

var
 FrmYtts: TFrmYtts;

 Fytts:Tytts;

 tts_mode:Integer;
 tts_intf:Integer;

 ytts_param:record
  FIndex:PaDeviceIndex;
  FVolume:Single;
  FVoice:TyVoice;
  FEmotion:TyEmotion;
  FSpeed:Currency;
 end;

 sapi_param:record
  FRate:Integer;
  FVolume:Integer;
  FVoiceName:RawByteString;
  FAudioOutputName:RawByteString;
 end;

implementation

{$R *.lfm}

uses
 WordFilter,
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

procedure TFrmYtts.SetDeviceY(Index:PaDeviceIndex);
var
 i:Integer;
begin
 if CMBDevicesY.Items.Count>0 then
 For i:=0 to CMBDevicesY.Items.Count-1 do
  if (PtrInt(Index)=PtrInt(CMBDevicesY.Items.Objects[i])) then
  begin
   CMBDevicesY.ItemIndex:=i;
   Exit;
  end;
 CMBDevicesY.ItemIndex:=0;
end;

function TFrmYtts.GetDeviceY:PaDeviceIndex;
begin
 Result:=-1;
 if (CMBDevicesY.ItemIndex<>-1) then
 if (CMBDevicesY.Items.Count<>0) then
  Result:=PaDeviceIndex(PtrInt(CMBDevicesY.Items.Objects[CMBDevicesY.ItemIndex]));
end;

procedure TFrmYtts.OnEnumDeviceY(Index:PaDeviceIndex;Const _name:RawByteString);
begin
 CMBDevicesY.AddItem(_name,TObject(Pointer(Index)));
end;

procedure TFrmYtts.LoadDeviceY;
begin
 CMBDevicesY.Clear;
 CMBDevicesY.AddItem('По умолчанию',TObject(Pointer(-1)));
 EnumOutputDevice(@OnEnumDeviceY);
end;

procedure TFrmYtts.OnEnumDeviceS(const _name:RawByteString;Voice:ISpeechObjectToken);
begin
 CMBDevicesS.AddItem(_name,TObject(1));
end;

procedure TFrmYtts.LoadDeviceS;
begin
 CMBDevicesS.Clear;
 CMBDevicesS.AddItem('По умолчанию',nil);
 CMBDevicesS.ItemIndex:=0;
 EnumSAPIAudioOutputs(@OnEnumDeviceS);
end;

procedure TFrmYtts.OnEnumVoicesS(const _name:RawByteString;Voice:ISpeechObjectToken);
begin
 CMBVoiceS.AddItem(_name,TObject(1));
end;

procedure TFrmYtts.LoadVoicesS;
begin
 CMBVoiceS.Clear;
 CMBVoiceS.AddItem('По умолчанию',nil);
 CMBVoiceS.ItemIndex:=0;
 EnumSAPIVoices(@OnEnumVoicesS);
end;

procedure TFrmYtts.SetDeviceS(const _name:RawByteString);
var
 i:Integer;
begin
 i:=CMBDevicesS.Items.IndexOf(_name);
 if (i<>-1) then CMBDevicesS.ItemIndex:=i;
end;

procedure TFrmYtts.SetVoicesS(const _name:RawByteString);
var
 i:Integer;
begin
 i:=CMBVoiceS.Items.IndexOf(_name);
 if (i<>-1) then CMBVoiceS.ItemIndex:=i;
end;

procedure TFrmYtts.FormCreate(Sender: TObject);
var
 _Voice:TyVoice;
 _Emotion:TyEmotion;
begin
 For _Voice:=Low(TyVoice) to High(TyVoice) do
  Case _Voice of
   alyss :CMBVoiceY.AddItem('Элиc'  ,nil);
   jane  :CMBVoiceY.AddItem('Джейн' ,nil);
   oksana:CMBVoiceY.AddItem('Оксана',nil);
   omazh :CMBVoiceY.AddItem('Омаж'  ,nil);
   zahar :CMBVoiceY.AddItem('Захар' ,nil);
   ermil :CMBVoiceY.AddItem('Ермил' ,nil);
   alena :CMBVoiceY.AddItem('Алёна' ,nil);
   filipp:CMBVoiceY.AddItem('Филипп',nil);
  end;

 For _Emotion:=Low(TyEmotion) to High(TyEmotion) do
  Case _Emotion of
   good   :CMBEmotionY.AddItem('Радостный'   ,nil);
   evil   :CMBEmotionY.AddItem('Раздражённый',nil);
   neutral:CMBEmotionY.AddItem('Нейтральный' ,nil);
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
 tts_mode:=0;
 tts_intf:=0;

 ytts_param.FIndex  :=-1;
 ytts_param.FVolume :=0.5;
 ytts_param.FVoice  :=filipp;
 ytts_param.FEmotion:=neutral;
 ytts_param.FSpeed  :=1;

 sapi_param.FRate:=0;
 sapi_param.FVolume:=100;
 sapi_param.FVoiceName:='';
 sapi_param.FAudioOutputName:='';

end;

Procedure TFrmYtts.LoadCfg;
begin
 InitCfg;

 tts_mode:=StrToIntDef(Config.ReadString('ytts','mode',IntToStr(tts_mode)),tts_mode);
 if tts_mode<0 then tts_mode:=0;
 if tts_mode>3 then tts_mode:=0;

 tts_intf:=StrToIntDef(Config.ReadString('ytts','intf',IntToStr(tts_intf)),tts_intf);
 if tts_intf<0 then tts_intf:=0;
 if tts_intf>1 then tts_intf:=1;

 ytts_param.FIndex :=StrToIntDef  (Config.ReadString('ytts','device',IntToStr  (ytts_param.FIndex)) ,ytts_param.FIndex);
 ytts_param.FVolume:=StrToFloatDef(Config.ReadString('ytts','volume',FloatToStrF(ytts_param.FVolume,ffFixed,2,2)),ytts_param.FVolume);

 LoadVoice  (Config.ReadString('ytts','voice',''));
 LoadEmotion(Config.ReadString('ytts','emotion',''));

 ytts_param.FSpeed:=StrToCurrDef(Config.ReadString('ytts','speed',CurrToStr(ytts_param.FSpeed)),ytts_param.FSpeed);

 sapi_param.FRate:=StrToIntDef(Config.ReadString('sapi','rate',IntToStr(sapi_param.FRate)),sapi_param.FRate);
 if sapi_param.FRate<-10 then sapi_param.FRate:=-10;
 if sapi_param.FRate>10  then sapi_param.FRate:=10;

 sapi_param.FVolume:=StrToIntDef(Config.ReadString('sapi','volume',IntToStr(sapi_param.FVolume)),sapi_param.FVolume);
 if sapi_param.FVolume<0   then sapi_param.FVolume:=0;
 if sapi_param.FVolume>100 then sapi_param.FVolume:=100;

 sapi_param.FVoiceName      :=Config.ReadString('sapi','voicename','');
 sapi_param.FAudioOutputName:=Config.ReadString('sapi','audiooutputname','');

end;

Procedure TFrmYtts.Open;
begin

 CMBMode.ItemIndex:=tts_mode;
 CMBIntf.ItemIndex:=tts_intf;

 LoadDeviceS;
 LoadVoicesS;

 SetDeviceS(sapi_param.FAudioOutputName);
 SetVoicesS(sapi_param.FVoiceName);

 TBSpeedS.Position:=sapi_param.FRate;
 TBSpeedSChange(nil);

 SetTBVolumeS(sapi_param.FVolume);

 LoadDeviceY;
 SetDeviceY(ytts_param.FIndex);
 CMBVoiceY  .ItemIndex:=Ord(ytts_param.FVoice);
 CMBEmotionY.ItemIndex:=Ord(ytts_param.FEmotion);
 SetTBSpeed (ytts_param.FSpeed);
 SetTBVolumeY(ytts_param.FVolume);

 if ShowModal=1 then
 begin
  tts_mode:=CMBMode.ItemIndex;
  tts_intf:=CMBIntf.ItemIndex;

  ytts_param.FIndex  :=GetDeviceY;
  ytts_param.FVoice  :=TyVoice(CMBVoiceY.ItemIndex);
  ytts_param.FEmotion:=TyEmotion(CMBEmotionY.ItemIndex);
  ytts_param.FSpeed  :=GetTBSpeed;
  ytts_param.FVolume :=GetTBVolumeY;
  YttsSetParam;

  sapi_param.FRate:=TBSpeedS.Position;
  sapi_param.FVolume:=GetTBVolumeS;

  sapi_param.FAudioOutputName:='';
  if (CMBDevicesS.ItemIndex>0) then
  begin
   sapi_param.FAudioOutputName:=CMBDevicesS.Items[CMBDevicesS.ItemIndex]
  end;

  sapi_param.FVoiceName:='';
  if (CMBVoiceS.ItemIndex>0) then
  begin
   sapi_param.FVoiceName:=CMBVoiceS.Items[CMBVoiceS.ItemIndex]
  end;

  try
   Config.WriteString('ytts','mode'   ,IntToStr  (tts_mode));
   Config.WriteString('ytts','intf'   ,IntToStr  (tts_intf));

   Config.WriteString('ytts','device' ,IntToStr  (ytts_param.FIndex));
   Config.WriteString('ytts','volume' ,FloatToStrF(ytts_param.FVolume,ffFixed,2,2));
   Config.WriteString('ytts','voice'  ,SaveVoice);
   Config.WriteString('ytts','emotion',SaveEmotion);
   Config.WriteString('ytts','speed'  ,CurrToStr(ytts_param.FSpeed));

   Config.WriteString('sapi','rate'           ,IntToStr(sapi_param.FRate));
   Config.WriteString('sapi','volume'         ,IntToStr(sapi_param.FVolume));
   Config.WriteString('sapi','voicename'      ,sapi_param.FVoiceName);
   Config.WriteString('sapi','audiooutputname',sapi_param.FAudioOutputName);

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
 Result:=0.5+(TBSpeedY.Position/10);
end;


procedure TFrmYtts.SetTBSpeed(FSpeed:Currency);
begin
 if FSpeed<0.5 then FSpeed:=0.5;
 if FSpeed>3.0 then FSpeed:=3.0;
 TBSpeedY.Position:=Round((FSpeed-0.5)*10);
end;

procedure TFrmYtts.TBSpeedYChange(Sender: TObject);
begin
 LSpeedY.Caption:='Скорость: '+CurrToStr(GetTBSpeed);
end;

procedure TFrmYtts.TBSpeedSChange(Sender: TObject);
begin
 LSpeedS.Caption:='Скорость: '+IntToStr(TBSpeedS.Position);
end;

function TFrmYtts.GetTBVolumeY:Single;
begin
 Result:=TBVolumeY.Position/50;
end;

procedure TFrmYtts.SetTBVolumeY(FVolume:Single);
begin
 if FVolume<0 then FVolume:=0;
 if FVolume>1 then FVolume:=1;
 TBVolumeY.Position:=Round(FVolume*50);
end;

procedure TFrmYtts.TBVolumeYChange(Sender: TObject);
begin
 LVolumeY.Caption:='Громкость: '+FloatToStrF(GetTBVolumeY,ffFixed,2,2);
end;

procedure TFrmYtts.SetTBVolumeS(FVolume:Integer);
begin
 if FVolume<0   then FVolume:=0;
 if FVolume>100 then FVolume:=100;
 TBVolumeS.Position:=FVolume div 2;
end;

function TFrmYtts.GetTBVolumeS:Integer;
begin
 Result:=TBVolumeS.Position*2;
end;

procedure TFrmYtts.TBVolumeSChange(Sender: TObject);
begin
 LVolumeS.Caption:='Громкость: '+IntToStr(GetTBVolumeS);
end;

procedure TFrmYtts.OnStartPlay(Sender:TObject);
begin
 Inc(FPlayRef);
end;

procedure TFrmYtts.OnStopPlay(Sender:TObject);
begin
 Dec(FPlayRef);
end;

type
 TSAPIHandle2=class(TSAPIHandle)
   function OnFinishSAPI:SizeInt;
 end;

function TSAPIHandle2.OnFinishSAPI:SizeInt;
begin
 Result:=0;
 Free;
 FrmYtts.OnStopPlay(nil);
end;

procedure TFrmYtts.push_msg(msg:RawByteString);
var
 H:TSAPIHandle2;
begin
 case tts_intf of
  0:begin
     LazyInitYtts;
     Fytts.push_msg(msg);
    end;
  1:begin
     frmMain.LazyInitAudioThread;

     H:=TSAPIHandle2.Create;
     H.OnFinish:=@H.OnFinishSAPI;
     H.AudioConnection:=FAudioThread;

     FiltredChar(PAnsiChar(msg),Length(msg));
     FiltredWords(PAnsiChar(msg),Length(msg));

     H.Text           :=msg;
     H.Rate           :=sapi_param.FRate;
     H.Volume         :=sapi_param.FVolume;
     H.VoiceName      :=sapi_param.FVoiceName;
     H.AudioOutputName:=sapi_param.FAudioOutputName;
     H.Send;
     OnStartPlay(nil);
    end;
 end;
end;

procedure TFrmYtts.add_to_chat_cmd(highlighted:Boolean;const cmd,param,msg:RawByteString);
Const
 maxqueue=2;
begin
 if frmMain.BtnPlay.CaptionState<>0 then Exit;
 Case tts_mode of
  1:begin
     if (cmd<>'') and (cmd[1]<>'!') then
      if (FPlayRef<=maxqueue) or highlighted then
      begin
       push_msg(msg);
      end;
    end;
  2:begin
     if (cmd='!ytts') then
      if (param<>'') then
       if (FPlayRef<=maxqueue) or highlighted then
       begin
        push_msg(param);
       end;
    end;
  3:begin
     if highlighted then
      if (msg<>'') then
      begin
       push_msg(msg);
      end;
    end;
 end;
end;

end.

