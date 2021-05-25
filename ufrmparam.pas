unit UFrmParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,LCLType,LCLIntf;

type

  { TFrmParam }

  TFrmParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBLoginMsg: TCheckBox;
    CBDontWelcomeMsg: TCheckBox;
    EdtLogin: TLabeledEdit;
    EdtChat: TLabeledEdit;
    EdtLogin2: TLabeledEdit;
    EdtPassword: TLabeledEdit;
    EdtPassword2: TLabeledEdit;
    TBView: TToggleBox;
    TBView2: TToggleBox;
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EdtKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure EdtUTF8KeyPress(Sender:TObject;var UTF8Key:TUTF8Char);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure TBViewChange(Sender:TObject);
    procedure TBViewChange2(Sender:TObject);
    procedure EdtNumKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
  private
    DigLastChar:Char;
    prev_dw:DWORD;
  public
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
  end;

var
  FrmParam: TFrmParam;

implementation

Uses
  ULog,Main;

{$R *.lfm}

Procedure TFrmParam.InitCfg;
begin
 Config.WriteString('base','zurl'   ,DefZURL);
 Config.WriteString('base','login'  ,'');
 Config.WriteString('base','oAuth'  ,'');
 Config.WriteString('base','chat'   ,base.chat);
end;

Procedure TFrmParam.LoadCfg;
begin
 base.chat     :=Trim(Config.ReadString('base','chat'   ,base.chat));
 base.login    :=Trim(Config.ReadString('base','login'  ,base.login));
 base.login2   :=Trim(Config.ReadString('base','login2' ,base.login2));
 base.useLogin2:=Trim(Config.ReadString('base','useLogin2','0'))='1';
 base.dontWelcome:=Trim(Config.ReadString('base','dontWelcome','0'))='1';
end;

Procedure TFrmParam.Open;
begin
 try
  EdtLogin.Text     :=base.login;
  EdtPassword.Text  :=Config.ReadString('base','oAuth','');
  EdtChat.Text      :=base.chat;
  EdtLogin2.Text    :=base.login2;
  EdtPassword2.Text :=Config.ReadString('base','oAuth2','');
  CBLoginMsg.Checked:=base.useLogin2;
  CBDontWelcomeMsg.Checked:=base.dontWelcome;
 except
  on E:Exception do
  begin
   DumpExceptionCallStack(E);
  end;
 end;

 if ShowModal=1 then
 begin
  base.chat     :=EdtChat.Text;
  base.login    :=EdtLogin.Text;
  base.login2   :=EdtLogin2.Text;
  base.useLogin2:=CBLoginMsg.Checked;
  base.dontWelcome:=CBDontWelcomeMsg.Checked;

  try
   Config.WriteString('base','login'   ,base.login);
   Config.WriteString('base','oAuth'   ,EdtPassword.Text);
   Config.WriteString('base','chat'    ,base.chat);
   Config.WriteString('base','login2'  ,base.login2);
   Config.WriteString('base','oAuth2'  ,EdtPassword2.Text);

   case base.useLogin2 of
    True :Config.WriteString('base','useLogin2','1');
    False:Config.WriteString('base','useLogin2','0');
   end;

   case base.dontWelcome of
    True :Config.WriteString('base','dontWelcome','1');
    False:Config.WriteString('base','dontWelcome','0');
   end;

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;

 EdtPassword.Text:='';
 EdtPassword2.Text:='';
end;

procedure TFrmParam.EdtKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 DigLastChar:=#0;
 if [ssAlt,ssCtrl]*Shift=[] then
 Case Key of
  ord(' '):Key:=0;
  96..105:DigLastChar:=Char(Key-48);
  ord('A')..ord('Z'):
  begin
    if (GetKeyState(VK_CAPITAL) and 1)<>0 then
    begin
     if ssShift in Shift then
      Shift:=Shift-[ssShift]
     else
      Shift:=Shift+[ssShift];
    end;
    if not (ssShift in Shift) then
     DigLastChar:=lowerCase(Char(Key))
    else
     DigLastChar:=Char(Key);
  end;
 end;
 FormKeyDown(Sender,Key,Shift);
end;

procedure TFrmParam.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmParam.EdtUTF8KeyPress(Sender:TObject;var UTF8Key:TUTF8Char);
begin
 if (DigLastChar<>#0) then UTF8Key:=DigLastChar;
end;

procedure TFrmParam.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

procedure TFrmParam.TBViewChange(Sender:TObject);
begin
 Case TToggleBox(Sender).Checked of
  True :EdtPassword.EchoMode:=emNormal;
  False:EdtPassword.EchoMode:=emPassword;
 end;
 Case TToggleBox(Sender).Checked of
  True :TToggleBox(Sender).Caption:='*';
  False:TToggleBox(Sender).Caption:='a';
 end;
end;

procedure TFrmParam.TBViewChange2(Sender:TObject);
begin
 Case TToggleBox(Sender).Checked of
  True :EdtPassword2.EchoMode:=emNormal;
  False:EdtPassword2.EchoMode:=emPassword;
 end;
 Case TToggleBox(Sender).Checked of
  True :TToggleBox(Sender).Caption:='*';
  False:TToggleBox(Sender).Caption:='a';
 end;
end;

procedure TFrmParam.EdtNumKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if [ssAlt,ssCtrl]*Shift=[] then
 Case Key of
  8,9,37,39:;
  ord('0')..ord('9'):;
  else
   Key:=0;
 end;
 FormKeyDown(Sender,Key,Shift);
end;

procedure TFrmParam.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmParam.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

end.

