unit UFrmVorRpg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  TaskManager,DbcEngine,DbcScript;

type

  { TFrmVorRpg }

  TFrmVorRpg = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBVorRpgEnable: TCheckBox;
    procedure BtnOkClick(Sender:TObject);
    procedure BtnCancelClick(Sender:TObject);
    procedure BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
  private
  public
    Procedure rpg_theif_vip(const s,dst_user,msg:RawByteString);
    Procedure OnGetUsers(Sender:TBaseTask);
    Procedure OnVorUsers(const s,dst_user,msg,d1,d2:RawByteString);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
  end;

var
  FrmVorRpg: TFrmVorRpg;

  FGetRpgUsersScript:TSQLScript;

implementation

uses
 Ulog,Main,UFrmVipParam;

{$R *.lfm}

type
 TUserPoints=record
  STR,LUK,DEF,CHR,AGL,LVL,PTS:Int64;
 end;

 TDbcScriptRpg=class(TDbcStatementScript)
  s,dst_user,msg:RawByteString;
 end;

Procedure TFrmVorRpg.rpg_theif_vip(const s,dst_user,msg:RawByteString);
var
 FDbcScript:TDbcScriptRpg;
begin
 FDbcScript:=TDbcScriptRpg.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FGetRpgUsersScript);
 FDbcScript.ExecuteScript;
 FDbcScript.s:=s;
 FDbcScript.dst_user:=dst_user;
 FDbcScript.msg:=msg;
 FDbcScript.Params.SetRawByteString('user1',dst_user);
 FDbcScript.Params.SetRawByteString('user2',msg);
 FDbcScript.Notify.Add(T_FIN,@OnGetUsers);
 FDbcScript.Start;
 FDbcScript.Release;
end;

Procedure TFrmVorRpg.OnGetUsers(Sender:TBaseTask);
Var
 user_f,data_f:SizeInt;
 u,d:RawByteString;
 d1,d2:RawByteString;
 i,c:SizeInt;
 ResultSet:TZResultSet;
begin
 ResultSet:=TDbcScriptRpg(Sender).ResultSet;

 d1:='';
 d2:='';

 if ResultSet<>nil then
 begin
  c:=0;
  if ResultSet.Last then
  begin
   c:=ResultSet.GetRow;
  end;
  if c>0 then
  begin
   user_f:=ResultSet.FindColumn('user');
   data_f:=ResultSet.FindColumn('data');
   For i:=1 to c do
   begin
    ResultSet.MoveAbsolute(i);

    u:=ResultSet.GetRawByteString(user_f);
    d:=ResultSet.GetRawByteString(data_f);

    if TDbcScriptRpg(Sender).dst_user=u then
    begin
     d1:=d;
    end else
    if TDbcScriptRpg(Sender).msg=u then
    begin
     d2:=d;
    end;

   end;
  end;
 end;

 OnVorUsers(TDbcScriptRpg(Sender).s,TDbcScriptRpg(Sender).dst_user,TDbcScriptRpg(Sender).msg,d1,d2);
end;

Procedure TFrmVorRpg.OnVorUsers(const s,dst_user,msg,d1,d2:RawByteString);
begin
 if (FrmVipParam.FindVipUser(dst_user)<>-1) then
 begin

 end else
 begin

 end;
end;

Procedure TFrmVorRpg.InitCfg;
begin
 vor_rpg.Enable:=False;
 case vor_rpg.Enable of
  True :Config.WriteString('vor_rpg' ,'enable','1');
  False:Config.WriteString('vor_rpg' ,'enable','0');
 end;
end;

Procedure TFrmVorRpg.LoadCfg;
begin
 vor_rpg.Enable:=Trim(Config.ReadString('vor_rpg','enable','0'))='1';
end;

Procedure TFrmVorRpg.Open;
begin
 CBVorRpgEnable.Checked :=vor_rpg.Enable;

 if ShowModal=1 then
 begin
  vor_rpg.Enable:=CBVorRpgEnable.Checked;

  try

   case vor_rpg.Enable of
    True :Config.WriteString('vor_rpg' ,'enable','1');
    False:Config.WriteString('vor_rpg' ,'enable','0');
   end;

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmVorRpg.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmVorRpg.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmVorRpg.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmVorRpg.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

end.

