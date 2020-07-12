unit UFrmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFrmAbout }

  TFrmAbout = class(TForm)
    LAbout: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FrmAbout: TFrmAbout;

implementation

{$R *.lfm}

{ TFrmAbout }

//C:\lazarus\packager\units\x86_64-win64

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
 LAbout.Caption:=LAbout.Caption+
 'Сборка от: '+{$I %DATE%}+#13#10+
 'FPC: '+{$I %FPCVERSION%}+#13#10+
 'Lazarus IDE: '+{$I C:\lazarus\ide\version.inc}+#13#10+
 {$I %FPCTARGETCPU%}+'/'+{$I %FPCTARGETOS%}
 ;

end;

end.

