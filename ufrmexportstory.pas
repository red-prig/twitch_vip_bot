unit ufrmexportstory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,

  TaskManager;

type

  { TFrmExportStory }

 TProgXCHG=object
  private
   type
    PBlock=^TBlock;
    TBlock=record
     Load,Count:NativeInt;
    end;
   Var
    FA,FB:TBlock;
    PFA,PFB:PBlock;
    Prev:TBlock;
  public
   Procedure Clear;
   Procedure Write(_Load,_Count:NativeInt);
   function  Read:TBlock;
 end;

  TFrmExportStory = class(TForm)
    LText: TLabel;
  private

  public
   Prog:TProgXCHG;
   Timer:TTimer;
   procedure OnTick(Sender:TObject);
   Procedure Open;
   Procedure onClose(Sender:TBaseTask);
  end;

var
  FrmExportStory: TFrmExportStory;

implementation

Uses
 main,

 ZDbcIntfs,
 DbcEngine,DbcScript,

 uLog,
 UJson,

 xlsbiff5,fpspreadsheet,fpsTypes;


{$R *.lfm}

type
 TDbcExortTask=class(TDbcQueryTask)
  protected
   FParams:TSQLVariables;
   FScript:TSQLScript;
   Procedure   OnQuery;
   function    GetParams:TSQLVariables;
   Procedure   SetParams(R:TSQLVariables);
  public
   FName:RawByteString;
   Constructor Create;  override;
   Procedure   Cleanup; override;
   Procedure   SetSctipt(Const FNew:TSQLScript);
   Procedure   ExecuteScript;
   Procedure   ExecuteScript(Stream:TStream);
   property    Params:TSQLVariables read GetParams write SetParams;
 end;

Procedure TProgXCHG.Clear;
begin
 Self:=Default(TProgXCHG);
 PFA:=@FA;
 PFB:=@FB;
end;

Procedure TProgXCHG.Write(_Load,_Count:NativeInt);
Var
 P:PBlock;
begin
 P:=System.InterLockedExchange(PFA,nil);
 if P<>nil then
 begin
  P^.Load:=_Load;
  P^.Count:=_Count;
  P:=System.InterLockedExchange(PFB,P);
  if P<>nil then
  begin
   P:=System.InterLockedExchange(PFA,P);
  end;
 end;
end;

function  TProgXCHG.Read:TBlock;
Var
 P:PBlock;

begin
 P:=System.InterLockedExchange(PFB,nil);
 if P<>nil then
 begin
  prev:=P^;
  P:=System.InterLockedExchange(PFB,P);
  if P<>nil then
  begin
   P:=System.InterLockedExchange(PFA,P);
  end;
 end;
 Result:=prev;
end;

Procedure TDbcExortTask.OnQuery;
Var
 MyWorkbook:TsWorkbook;
 Worksheet:TsWorksheet;
 FContext:TSQLContext;
 FGlobal:ZDbcIntfs.IZResultSet;
 I:TSQLScriptIterator;
 pos,count:NativeInt;
 Columns:SizeUInt;

 datetime_f,user_f,mes_f,cmd_f:SizeInt;

 ms:TPCharStream;

 function SaveHeader(FSheet:TsWorksheet;M:IZResultSetMetadata):SizeUInt;
 Var
  x:SizeUInt;
 begin
  FSheet.PageLayout.Orientation:=spoPortrait;
  FSheet.PageLayout.FitHeightToPages:=1;
  FSheet.PageLayout.FitWidthToPages :=1;
  FSheet.PageLayout.TopMargin   :=20;
  FSheet.PageLayout.LeftMargin  :=10;
  FSheet.PageLayout.RightMargin :=10;
  FSheet.PageLayout.BottomMargin:=10;

  if not Assigned(M) then Exit;

  datetime_f:=M.FindColumn('datetime');
  user_f    :=M.FindColumn('user');
  mes_f     :=M.FindColumn('mes');
  cmd_f     :=M.FindColumn('cmd');

  Result:=M.GetColumnCount;

  if Result<>0 then
  begin
   For x:=0 to 5 do
   begin
    FSheet.WriteBackgroundColor(0,x,$D0D0D0);
    FSheet.WriteBorders(0,x,[cbNorth,cbWest,cbEast,cbSouth]);
   end;
   FSheet.WriteText(0,0,'Дата');
   FSheet.WriteText(0,1,'Время');
   FSheet.WriteText(0,2,'ЛЕВ');
   FSheet.WriteText(0,3,'Награда');
   FSheet.WriteText(0,4,'Сообщение');
   FSheet.WriteText(0,5,'Команда');
  end;
 end;

 Procedure SaveRow(FSheet:TsWorksheet;Row,Columns:SizeUInt;R:ZDbcIntfs.IZResultSet);
 var
  FS:TFormatSettings;
  x:SizeUInt;
  D:TDateTime;
  msg2:TJson;
  msg:RawByteString;
 begin
  if Columns<>0 then
  begin
   FS:=DefaultFormatSettings;
   FS.ShortDateFormat:='dd/mm/yyyy';
   FS.DateSeparator:='.';
   FS.ShortTimeFormat:='hh:nn:ss';
   FS.LongTimeFormat:='hh:nn:ss';
   FS.TimeSeparator:=':';
   FS.ListSeparator:=' ';

   D:=R.GetTimestamp(datetime_f);
   FSheet.WriteText(Row,0,DateToStr(D,FS));
   FSheet.WriteText(Row,1,TimeToStr(D,FS));
   FSheet.WriteText(Row,2,R.GetRawByteString(user_f));

   msg:=R.GetRawByteString(mes_f);
   ms.SetNew(PAnsiChar(msg),Length(msg));
   msg2:=Default(TJson);
   try
    msg2:=TJson.New(ms);
   except
    on E:Exception do
    begin
     DumpExceptionCallStack(E);
    end;
   end;

   FSheet.WriteText(Row,3,Trim(fetch_reward(msg2)));
   FSheet.WriteText(Row,4,fetch_msg(msg2));

   FSheet.WriteText(Row,5,R.GetRawByteString(cmd_f));

   For x:=0 to 5 do
   begin
    FSheet.WriteBorders(Row,x,[cbNorth,cbWest,cbEast,cbSouth]);
   end;

  end;
 end;

begin
 FContext:=Default(TSQLContext);
 FContext.FVariables:=FParams;
 FContext.FConnection:=ZConnection;

 if Assigned(FHandle) then
  if Assigned(FHandle.DbcConnection) then
   FContext.FGlobalCache:=FHandle.DbcConnection.GetPreparedCache;

 I:=FScript.Excecute(FContext);
 While (I.Next) do
 begin
  if FHandle.isCancel then
  begin
   FContext.FGlobal:=nil;
   I.Close(false);
   Break;
  end;
 end;
 I.Close(true);

 FGlobal:=FContext.FGlobal;
 FContext.ClearCursors;

 count:=FContext.FVariables.GetAsInteger('count');
 if Assigned(FGlobal) then
 begin
  MyWorkbook:=TsWorkbook.Create;
  Worksheet:=MyWorkbook.AddWorksheet('История наград',False);

  Columns:=SaveHeader(Worksheet,FGlobal.GetMetadata);
  ms:=TPCharStream.Create(nil,0);

  While FGlobal.Next do
  begin
   pos:=FGlobal.GetRow;
   FrmExportStory.Prog.Write(pos,count);
   SaveRow(Worksheet,pos,Columns,FGlobal);
  end;

  FreeAndNil(ms);
  MyWorkbook.WriteToFile(FName,True,[]);
  MyWorkbook.Free;
 end;
end;

function  TDbcExortTask.GetParams:TSQLVariables;
begin
 Result:=nil;
 if FHandle.State then Exit;
 Result:=FParams;
end;

Procedure TDbcExortTask.SetParams(R:TSQLVariables);
begin
 if FHandle.State then Exit;
 FParams:=R;
end;

Constructor TDbcExortTask.Create;
begin
 inherited;
 FParams:=TSQLVariables.Create;
end;

Procedure TDbcExortTask.Cleanup;
begin
 FreeAndNil(FParams);
 inherited;
end;

Procedure TDbcExortTask.SetSctipt(Const FNew:TSQLScript);
begin
 if FHandle.State then Exit;
 FScript:=FNew;
end;

Procedure TDbcExortTask.ExecuteScript(Stream:TStream);
begin
 if FHandle.State then Exit;
 FScript.Parse(Stream);
 FHandle.OnDbcProc:=@OnQuery;
end;

Procedure TDbcExortTask.ExecuteScript;
begin
 FHandle.OnDbcProc:=@OnQuery;
end;

procedure TFrmExportStory.OnTick(Sender:TObject);
Var
 B:TProgXCHG.TBlock;
begin
 B:=Prog.Read;
 LText.Caption:=IntToStr(B.Load)+'/'+IntToStr(B.Count);
end;

Procedure TFrmExportStory.Open;
var
 FName:RawByteString;
 SaveDialog:TSaveDialog;
 FDbcScript:TDbcExortTask;
begin
 if Showing then Exit;

 SaveDialog:=TSaveDialog.Create(Self);
 SaveDialog.Title:='Экспорт истории в xls';
 SaveDialog.Filter:='*.XLS|*.XLS';
 SaveDialog.FileName:='';

 repeat
  if SaveDialog.Execute then
  begin
   FName:=SaveDialog.FileName;
   if FileExists(FName) then
   begin
    if QuestionDlg('Файл уже существует!',
                   'Заменить?',
                   mtInformation,
                   [mrYes,'Да',mrNo,'Нет'],
                   'Файл уже существует')=mrYes then
    begin
     Break;
    end;
   end else
   begin
    Break;
   end;
  end else
  begin
   SaveDialog.Free;
   Exit;
  end;
 until false;
 SaveDialog.Free;

 Prog.Clear;

 Timer:=TTimer.Create(Self);
 Timer.OnTimer:=@OnTick;
 Timer.Interval:=300;
 Timer.Enabled:=True;

 FDbcScript:=TDbcExortTask.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FExportStoryScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Notify.Add(T_FIN,@OnClose);
 FDbcScript.FName:=FName;
 FDbcScript.Start;
 FDbcScript.Release;

 ShowOnTop;
end;

Procedure TFrmExportStory.OnClose(Sender:TBaseTask);
begin
 FreeAndNil(Timer);
 Hide;
end;

end.

