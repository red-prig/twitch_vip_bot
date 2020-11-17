unit UFrmVipParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,MaskEdit, ComCtrls,
  TaskManager,DbcEngine,
  kgrids,
  ExtStringGrid,DbcScript;

type

  { TFrmVipParam }

  TFrmVipParam = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    CBVipEnable: TCheckBox;
    CBVorEnable: TCheckBox;
    CBVipExpired: TCheckBox;
    EdtMaxVips: TLabeledEdit;
    EdtPercent: TLabeledEdit;
    EdtTitle: TLabeledEdit;
    EdtVipDays: TLabeledEdit;
    EdtVorPercent: TLabeledEdit;
    EdtVorTitle: TLabeledEdit;
    GBVip: TGroupBox;
    GBVor: TGroupBox;
    GBList: TGroupBox;
    procedure BtnCancelClick(Sender:TObject);
    procedure BtnCancelKeyDown(Sender: TObject;var Key:Word;Shift:TShiftState);
    procedure BtnOkClick(Sender:TObject);
    procedure EdtPercentExit(Sender:TObject);
    procedure EdtPercentKeyPress(Sender:TObject;var Key:char);
    procedure EdtVipDaysExit(Sender: TObject);
    procedure EdtVipDaysKeyPress(Sender:TObject;var Key:char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
  private
    prev_perc:Byte;
    prev_dw:DWORD;
  public
    procedure UpdateStatusBarVips;
    procedure VipsEditorCreate(Sender:TObject;ACol,ARow:Integer;var AEditor:TWinControl);
    function  FindVipUser(Const FValue:RawByteString):Integer;
    function  FindPosVipUser(Const FValue:RawByteString):Integer;
    procedure getVipStat(var All,Tmp:RawByteString);
    function  getTmpVipList:TStringList;
    procedure getTmpVipList2(Var P,T:TStringList);
    function  getPermVipList:TStringList;
    function  DoVipInsert(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
    function  DoVipUpdate(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
    procedure DbUpdateVip_Time(const user:RawByteString;DT:TDateTime);
    procedure DbUpdateVip_user(const src_user,dst_user:RawByteString);
    procedure DbDeleteVip(const user:RawByteString);
    function  DoVipDelete(aRow:Integer):Boolean;
    procedure DeleteVip(aRow:Integer);
    Function  DoVipAddNew(DT:TDateTime;Const FUser:RawByteString):Boolean;
    procedure SetTimerVipExpired(m:Boolean);
    procedure OnBtnCheckVipClick(Sender:TObject);
    procedure OnBtnDeleteVipClick(Sender:TObject);
    procedure OnBtnInsertVipClick(Sender:TObject);
    procedure DeleteAndUnVip(aRow:Integer);
    procedure DeleteAndUnVip(Const FUser:RawByteString);
    procedure OnBtnUnVipClick(Sender:TObject);
    procedure OnBtnUpdateVipClick(Sender:TObject);
    Procedure OnListVips(Sender:TBaseTask);
    procedure vip_time_cmd(const user,cmd:RawByteString;param:RawByteString);
    procedure GridKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure OnBtnFind(Sender:TObject);
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
  end;

var
  FrmVipParam: TFrmVipParam;

  PanelVips:TPanel;
  GridVips:TDBStringGrid;
  StatusBarVips:TStatusBar;

  FListVipsScript  :TSQLScript;
  FAddVipsScript   :TSQLScript;
  FInsertVipsScript:TSQLScript;
  FUpdateVipsScript:TSQLScript;
  FDeleteVipsScript:TSQLScript;

function TryGetDateTime_US(const S:RawByteString;out Value:TDateTime):Boolean;
function TryGetDateTime_RU(const S:RawByteString;out Value:TDateTime):Boolean;
function DateTimeToStr_RU(DateTime:TDateTime):RawByteString;

implementation

Uses
  ULog,Main,ujson,DateUtils,Math;

{$R *.lfm}

function TryGetDateTime_US(const S:RawByteString;out Value:TDateTime):Boolean;
var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='yyyy/mm/dd';
 FS.DateSeparator:='.';
 FS.ShortTimeFormat:='hh:nn:ss';
 FS.LongTimeFormat:='hh:nn:ss';
 FS.TimeSeparator:=':';
 FS.ListSeparator:=' ';
 Result:=TryStrToDateTime2(S,Value,FS);
end;

function TryGetDateTime_RU(const S:RawByteString;out Value:TDateTime):Boolean;
var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='dd/mm/yyyy';
 FS.DateSeparator:='.';
 FS.ShortTimeFormat:='hh:nn:ss';
 FS.LongTimeFormat:='hh:nn:ss';
 FS.TimeSeparator:=':';
 FS.ListSeparator:=' ';
 Result:=TryStrToDateTime2(S,Value,FS);
end;

function DateTimeToStr_RU(DateTime:TDateTime):RawByteString;
var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='dd/mm/yyyy';
 FS.DateSeparator:='.';
 FS.ShortTimeFormat:='hh:nn:ss';
 FS.LongTimeFormat:='hh:nn:ss';
 FS.TimeSeparator:=':';
 FS.ListSeparator:=' ';
 Result:=DateTimeToStr(DateTime,FS);
end;

function GetDateTimeEnd(const Value:TDateTime):TDateTime;
begin
 Result:=IncDay(Value,vip_rnd.days);
end;

type
 TUserMaskEdit=class(TCustomMaskEdit)
  procedure ValidateEdit; override;
 end;

procedure TUserMaskEdit.ValidateEdit;
begin
 if (Text='    .  .     :  :  ') then
 begin
  Clear;
  Text:='';
 end else
 begin
  try
   inherited;
  except
   Clear;
   Text:='';
  end;
 end;
end;

procedure TFrmVipParam.VipsEditorCreate(Sender:TObject;ACol,ARow:Integer;var AEditor:TWinControl);
Var
 ME:TUserMaskEdit;
begin
 Case GridVips.GetColumnName(ACol) of
  'datebeg':
   begin
    ME:=TUserMaskEdit.Create(GridVips);
    ME.EditMask:='0000/00/00 00:00:00';
    ME.SpaceChar:='_';
    AEditor:=ME;
   end;
  'user'   :
  begin
   AEditor:=TEdit.Create(GridVips);
  end;
 end;
end;

function TFrmVipParam.FindVipUser(Const FValue:RawByteString):Integer;
Var
 i,u,C:Integer;
 v:RawByteString;
begin
 Result:=-1;
 C:=GridVips.RowCount;
 if C>1 then
 begin
  u:=GridVips.FindColumn('user');
  v:=LowerCase(FValue);
  if u<>-1 then
   For i:=1 to C-1 do
    if LowerCase(GridVips.Cells[u,i])=v then
     Exit(i);
 end;
end;

function TFrmVipParam.FindPosVipUser(Const FValue:RawByteString):Integer;
Var
 i,u,C:Integer;
 v:RawByteString;
begin
 Result:=-1;
 C:=GridVips.RowCount;
 if C>1 then
 begin
  u:=GridVips.FindColumn('user');
  v:=LowerCase(FValue);
  if u<>-1 then
   For i:=1 to C-1 do
    if Pos(v,LowerCase(GridVips.Cells[u,i]))<>0 then
     Exit(i);
 end;
end;

procedure TFrmVipParam.getVipStat(var All,Tmp:RawByteString);
var
 i:integer;
 s,e,r:SizeInt;
begin
 r:=0;
 s:=GridVips.RowCount;
 if (s>1) then
 begin
  Dec(s);
  e:=GridVips.FindColumn('dateend');
  if (e<>-1) then
   For i:=1 to s do
    if (GridVips.Cells[e,i]<>'') then
     Inc(r);
 end;
 All:=IntToStr(s);
 Tmp:=IntToStr(r);
end;

function TFrmVipParam.getTmpVipList:TStringList;
var
 i:integer;
 s,u,e:SizeInt;
begin
 Result:=TStringList.Create;
 Result.Sorted:=True;
 s:=GridVips.RowCount;
 if s>1 then
 begin
  u:=GridVips.FindColumn('user');
  e:=GridVips.FindColumn('dateend');
  if (u<>-1) and (e<>-1) then
   For i:=1 to s-1 do
    if (GridVips.Cells[e,i]<>'') then
     Result.AddObject(LowerCase(GridVips.Cells[u,i]),GridVips.Rows[i]);
 end;
end;

procedure TFrmVipParam.getTmpVipList2(Var P,T:TStringList);
Var
 N,D:TDateTime;
 i:integer;
 s,u,b:SizeInt;
begin
 P:=TStringList.Create;
 P.Sorted:=True;
 T:=TStringList.Create;
 T.Sorted:=True;
 s:=GridVips.RowCount;
 if s>1 then
 begin
  u:=GridVips.FindColumn('user');
  b:=GridVips.FindColumn('datebeg');
  N:=Now;
  if (u<>-1) and (b<>-1) then
   For i:=1 to s-1 do
    if TryGetDateTime_US(GridVips.Cells[b,i],D) then
    begin
     if DateTimeInRange(N,D,IncDay(D)) or
        DateTimeInRange(N,IncDay(GetDateTimeEnd(D),-1),GetDateTimeEnd(D)) then
     begin
      P.AddObject(LowerCase(GridVips.Cells[u,i]),GridVips.Rows[i]);
     end else
     begin
      T.AddObject(LowerCase(GridVips.Cells[u,i]),GridVips.Rows[i]);
     end;
    end;
 end;
end;

function TFrmVipParam.getPermVipList:TStringList;
var
 i:integer;
 s,u,e:SizeInt;
begin
 Result:=TStringList.Create;
 Result.Sorted:=True;
 s:=GridVips.RowCount;
 if s>1 then
 begin
  u:=GridVips.FindColumn('user');
  e:=GridVips.FindColumn('dateend');
  if (u<>-1) and (e<>-1) then
   For i:=1 to s-1 do
    if (GridVips.Cells[e,i]='') then
     Result.AddObject(LowerCase(GridVips.Cells[u,i]),GridVips.Rows[i]);
 end;
end;

function TFrmVipParam.DoVipInsert(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
Var
 FDbcScript:TDbcStatementScript;
 DT:TDateTime;
 T:RawByteString;

begin
 Result:=True;
 Case FName of
  'datebeg':
   begin
    if (FValue='    .  .     :  :  ') or (FValue='') then
    begin
     GridVips.FieldValue[FName    ,ARow]:='';
     GridVips.FieldValue['dateend',ARow]:='';
     Result:=False;
    end else
    begin
     if TryGetDateTime_US(FValue,DT) then
     begin

      FDbcScript:=TDbcStatementScript.Create;
      FDbcScript.Handle.DbcConnection:=DbcThread;
      FDbcScript.SetSctipt(FInsertVipsScript);
      FDbcScript.ExecuteScript;
      FDbcScript.Params.SetRawByteString(':field','datetime');
      FDbcScript.Params.SetAsDateTime('value',DT);
      FDbcScript.Start;
      FDbcScript.Release;

      GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr_US(GetDateTimeEnd(DT));

      GridVips.ResetRowInsert;
      UpdateStatusBarVips;
     end else
     begin
      Result:=False;
     end;
    end;

   end;
  'user'   :
  begin
   T:=Trim(FValue);
   if (T='') or (FindVipUser(T)<>-1) then
   begin
    Result:=False;
   end else
   begin
    GridVips.FieldValue[FName,ARow]:=T;
    FDbcScript:=TDbcStatementScript.Create;
    FDbcScript.Handle.DbcConnection:=DbcThread;
    FDbcScript.SetSctipt(FInsertVipsScript);
    FDbcScript.ExecuteScript;
    FDbcScript.Params.SetRawByteString(':field','user');
    FDbcScript.Params.SetRawByteString('value',T);
    FDbcScript.Start;
    FDbcScript.Release;

    GridVips.ResetRowInsert;
    UpdateStatusBarVips;
   end;
  end;
 end;

end;

function TFrmVipParam.DoVipUpdate(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
Var
 FDbcScript:TDbcStatementScript;
 DT:TDateTime;
 T:RawByteString;

begin
 Result:=True;
 Case FName of
  'datebeg':
   begin
    if (FValue='    .  .     :  :  ') or (FValue='') then
    begin

     DbUpdateVip_Time(GridVips.FieldValue['user',ARow],NAN);

     GridVips.FieldValue[FName    ,ARow]:='';
     GridVips.FieldValue['dateend',ARow]:='';

     GridVips.ResetRowInsert;
     Result:=False;
    end else
    begin
     if TryGetDateTime_US(FValue,DT) then
     begin

      DbUpdateVip_Time(GridVips.FieldValue['user',ARow],DT);

      GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr_US(GetDateTimeEnd(DT));

      GridVips.ResetRowInsert;
     end else
     begin
      Result:=False;
     end;
    end;

   end;
  'user'   :
  begin
   T:=Trim(FValue);
   if (T='') or (FindVipUser(T)<>-1) then
   begin
    Result:=False;
   end else
   begin

    DbUpdateVip_user(GridVips.FieldValue['user',ARow],T);

    GridVips.FieldValue[FName,ARow]:=T;
    GridVips.ResetRowInsert;
   end;
  end;
 end;

end;

procedure TFrmVipParam.DbUpdateVip_Time(const user:RawByteString;DT:TDateTime);
Var
 FDbcScript:TDbcStatementScript;
begin
 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FUpdateVipsScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString(':field','datetime');
 if IsNullValue(DT) then
 begin
  FDbcScript.Params.SetAsNull('value');
 end else
 begin
  FDbcScript.Params.SetAsDateTime('value',DT);
 end;
 FDbcScript.Params.SetRawByteString('user',user);
 FDbcScript.Start;
 FDbcScript.Release;
end;

procedure TFrmVipParam.DbUpdateVip_user(const src_user,dst_user:RawByteString);
Var
 FDbcScript:TDbcStatementScript;
begin
 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FUpdateVipsScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString(':field','user');
 FDbcScript.Params.SetRawByteString('value',dst_user);
 FDbcScript.Params.SetRawByteString('user' ,src_user);
 FDbcScript.Start;
 FDbcScript.Release;
end;

procedure TFrmVipParam.DbDeleteVip(const user:RawByteString);
Var
 FDbcScript:TDbcStatementScript;
begin
 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FDeleteVipsScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Params.SetRawByteString('user',user);
 FDbcScript.Start;
 FDbcScript.Release;
end;

function TFrmVipParam.DoVipDelete(aRow:Integer):Boolean;
begin
 Result:=QuestionDlg('Удаление из таблицы',
                     Format('Удалить из таблицы %s ?',[GridVips.FieldValue['user',ARow]]),
                    mtInformation,
                    [mrYes,'Да',mrNo,'Нет'],
                    'Удаление из таблицы')=mrYes;

 if Result then
 begin
  Result:=False;
  DeleteVip(aRow);
 end;

end;

procedure TFrmVipParam.DeleteVip(aRow:Integer);
begin
 if (aRow>=GridVips.FixedRows) and GridVips.RowValid(aRow) then
 begin
  DbDeleteVip(GridVips.FieldValue['user',ARow]);
  GridVips.DeleteRow(aRow);
  UpdateStatusBarVips;
 end;
end;

Function TFrmVipParam.DoVipAddNew(DT:TDateTime;Const FUser:RawByteString):Boolean;
Var
 ARow:Integer;
 FDbcScript:TDbcStatementScript;
begin
 Result:=(FUser<>'') and (FindVipUser(FUser)=-1);
 if not Result then Exit;

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.SetSctipt(FAddVipsScript);
 FDbcScript.ExecuteScript;

 if IsNullValue(DT) then
 begin
  FDbcScript.Params.SetAsNull      ('datetime');
 end else
 begin
  FDbcScript.Params.SetAsDateTime  ('datetime',DT);
 end;

 FDbcScript.Params.SetRawByteString('user'    ,FUser);
 FDbcScript.Start;
 FDbcScript.Release;

 ARow:=GridVips.RowCount;
 GridVips.InsertRow(ARow);

 if IsNullValue(DT) then
 begin
  GridVips.FieldValue['datebeg',ARow]:='';
  GridVips.FieldValue['dateend',ARow]:='';
 end else
 begin
  GridVips.FieldValue['datebeg',ARow]:=GetDateTimeStr_US(DT);
  GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr_US(GetDateTimeEnd(DT));
 end;

 GridVips.FieldValue['user'   ,ARow]:=FUser;

 UpdateStatusBarVips;

 if GridVips.HandleAllocated then
 begin
  //GridVips.ScrollModeVert:=smCell;
  //GridVips.ScrollModeVert:=smSmooth;
  GridVips.Columns[0].Extent:=GridVips.Columns[0].MinExtent;
 end;
end;

procedure TFrmVipParam.SetTimerVipExpired(m:Boolean);
begin
 Case m of
  True :
  begin
   if (vip_rnd.Timer=nil) then
   begin
    vip_rnd.Timer:=TTimer.Create(Self);
    vip_rnd.Timer.Interval:=30*60*1000; //30min
    vip_rnd.Timer.OnTimer:=@OnBtnCheckVipClick;
   end;
   vip_rnd.Timer.Enabled:=m;
  end;
  False:
  begin
   if (vip_rnd.Timer<>nil) then
   begin
    vip_rnd.Timer.Enabled:=m;
   end;
  end;
 end;
end;

procedure TFrmVipParam.OnBtnCheckVipClick(Sender:TObject);
var
 ND,DT:TDateTime;
 T:RawByteString;
 i,s,de,u:SizeInt;
 L:array of RawByteString;
begin
 if not frmmain.BtnInfo.Visible then
 begin
  if Sender is TButton then
  begin
   ShowMessage('Для выполнения операции залогинтесь!');
  end;
  Exit;
 end;

 SetLength(L,0);
 s:=GridVips.RowCount;
 if s>1 then
 begin
  de:=GridVips.FindColumn('dateend');
  u:=GridVips.FindColumn('user');
  ND:=Now;
  if (u<>-1) and (de<>-1) then
   For i:=1 to s-1 do
   begin
    T:=GridVips.Cells[de,i];
    if (T<>'') and TryGetDateTime_US(T,DT) then
    begin
     if ND>DT then
     begin
      SetLength(L,Length(L)+1);
      L[Length(L)-1]:=LowerCase(GridVips.Cells[u,i]);
     end;
    end;
   end;
 end;

 if Length(L)<>0 then
 For i:=0 to Length(L)-1 do
 begin
  DeleteAndUnVip(L[i]);
 end;

end;

procedure TFrmVipParam.OnBtnDeleteVipClick(Sender:TObject);
begin
 GridVips.DoDelete;
 UpdateStatusBarVips;
end;

procedure TFrmVipParam.OnBtnInsertVipClick(Sender:TObject);
begin
 GridVips.DoInsert;
 UpdateStatusBarVips;
end;

procedure TFrmVipParam.DeleteAndUnVip(aRow:Integer);
var
 user:RawByteString;
begin
 user:=GridVips.FieldValue['user',ARow];
 push_irc_msg(Format(vip_rnd.unvip_cmd,[user]));
 DeleteVip(aRow);
end;

procedure TFrmVipParam.DeleteAndUnVip(Const FUser:RawByteString);
var
 aRow:Integer;
begin
 aRow:=FindVipUser(FUser);
 push_irc_msg(Format(vip_rnd.unvip_cmd,[FUser]));
 DeleteVip(aRow);
end;

procedure TFrmVipParam.OnBtnUnVipClick(Sender:TObject);
var
 user:RawByteString;
 aRow:Integer;
begin
 if not frmmain.BtnInfo.Visible then
 begin
  if Sender is TButton then
  begin
   ShowMessage('Для выполнения операции залогинтесь!');
  end;
  Exit;
 end;

 aRow:=GridVips.Row;
 if (aRow>=GridVips.FixedRows) and GridVips.RowValid(aRow) then
 begin

  user:=GridVips.FieldValue['user',ARow];
  if QuestionDlg('Удалить випку на твиче?',
                 Format('Удалить випку у %s ?',[user]),
                 mtInformation,
                 [mrYes,'Да',mrNo,'Нет'],
                 'Удаление випки на твиче')=mrYes then
  begin
   DeleteAndUnVip(aRow);
  end;
 end;
end;

procedure TFrmVipParam.OnBtnUpdateVipClick(Sender:TObject);
begin
 if not frmmain.BtnInfo.Visible then
 begin
  if Sender is TButton then
  begin
   ShowMessage('Для выполнения операции залогинтесь!');
  end;
  Exit;
 end;

 push_irc_msg(vip_rnd.vip_list_cmd);
 Frmmain.wait_vip_update:=True;
end;


Procedure TFrmVipParam.OnListVips(Sender:TBaseTask);
Var
 datetime_f,user_f:SizeInt;
 i,c:SizeInt;
 ResultSet:TZResultSet;
begin
 ResultSet:=TDbcStatementScript(Sender).ResultSet;

 if ResultSet=nil then Exit;

 c:=0;
 if ResultSet.Last then
 begin
  c:=ResultSet.GetRow;
  GridVips.RowCount:=c+1;
 end;

 if c>0 then
 begin
  datetime_f:=ResultSet.FindColumn('datetime');
  user_f    :=ResultSet.FindColumn('user');
  For i:=1 to c do
  begin
   ResultSet.MoveAbsolute(i);

   if ResultSet.IsNull(datetime_f) then
   begin
    GridVips.FieldValue['datebeg',i]:='';
    GridVips.FieldValue['dateend',i]:='';
    GridVips.FieldValue['user'   ,i]:=ResultSet.GetRawByteString(user_f);
   end else
   begin
    GridVips.FieldValue['datebeg',i]:=GetDateTimeStr_US(ResultSet.GetDouble(datetime_f));
    GridVips.FieldValue['dateend',i]:=GetDateTimeStr_US(GetDateTimeEnd(ResultSet.GetDouble(datetime_f)));
    GridVips.FieldValue['user'   ,i]:=ResultSet.GetRawByteString(user_f);
   end;

  end;
 end;

 UpdateStatusBarVips;

 if GridVips.HandleAllocated then
 begin
  //GridVips.ScrollModeVert:=smCell;
  //GridVips.ScrollModeVert:=smSmooth;
  GridVips.Columns[0].Extent:=GridVips.Columns[0].MinExtent;
 end;
end;

procedure TFrmVipParam.vip_time_cmd(const user,cmd:RawByteString;param:RawByteString);
var
 i:Integer;
 D:TDateTime;
 cmd2,nick,datebeg,dateend:RawByteString;
 L:TStringList;
begin
 if LowerCase(cmd)<>'!vip' then Exit;

 cmd2:=FetchAny(param);
 nick:=FetchAny(param);
 cmd2:=LowerCase(Trim(cmd2));

 try

 Case cmd2 of
  'time':
     begin
      nick:=Trim(nick);
      nick:=Extract_nick(nick);
      i:=FindVipUser(nick);
      datebeg:=GridVips.FieldValue['datebeg',i];
      dateend:=GridVips.FieldValue['dateend',i];
      if (i<>-1) then
      begin
       if TryGetDateTime_US(datebeg,D) then
       begin
        datebeg:=DateTimeToStr_RU(D);
       end;
       if TryGetDateTime_US(dateend,D) then
       begin
        dateend:=DateTimeToStr_RU(D);
       end;
      end else
      begin
       nick:='???';
      end;
      if vip_rnd.viptime_get_info='' then
      begin
       vip_rnd.viptime_get_info:='@%s vip time %s: [%s - %s]';
      end;
      push_irc_msg(Format(vip_rnd.viptime_get_info,[user,nick,datebeg,dateend]));
     end;
  'settime':
     begin
      nick:=Trim(nick);
      nick:=Extract_nick(nick);
      i:=FindVipUser(nick);
      datebeg:='';
      dateend:='';
      if (i<>-1) then
      begin
       cmd2:=FetchAny(param);
       if (cmd2='') then
       begin
        DbUpdateVip_Time(GridVips.FieldValue['user',i],NAN);
        GridVips.FieldValue['datebeg',i]:='';
        GridVips.FieldValue['dateend',i]:='';
       end else
       if TryGetDateTime_RU(cmd2,D) then
       begin
        DbUpdateVip_Time(GridVips.FieldValue['user',i],D);
        GridVips.FieldValue['datebeg',i]:=GetDateTimeStr_US(D);
        GridVips.FieldValue['dateend',i]:=GetDateTimeStr_US(GetDateTimeEnd(D));
        datebeg:=DateTimeToStr_RU(D);
        dateend:=DateTimeToStr_RU(GetDateTimeEnd(D));
       end else
       begin
        datebeg:=GridVips.FieldValue['datebeg',i];
        dateend:=GridVips.FieldValue['dateend',i];
        if TryGetDateTime_US(datebeg,D) then
        begin
         datebeg:=DateTimeToStr_RU(D);
        end;
        if TryGetDateTime_US(dateend,D) then
        begin
         dateend:=DateTimeToStr_RU(D);
        end;
       end;
      end else
      begin
       nick:='???';
      end;
      if vip_rnd.viptime_get_info='' then
      begin
       vip_rnd.viptime_get_info:='@%s vip time %s: [%s - %s]';
      end;
      push_irc_msg(Format(vip_rnd.viptime_get_info,[user,nick,datebeg,dateend]));
     end;
  'info':
     begin
      datebeg:='';
      dateend:='';
      getVipStat(datebeg,dateend);
      if vip_rnd.vipinfo_get_info='' then
      begin
       vip_rnd.vipinfo_get_info:='@%s vip count: %s permanent: %s';
      end;
      push_irc_msg(Format(vip_rnd.vipinfo_get_info,[user,datebeg,dateend]));
     end;
  'perm':
     begin
      L:=getPermVipList;
      L.LineBreak:=',';
      nick:='@'+user+' '+L.Text;
      if (nick<>'') and (nick[Length(nick)]=',') then
       Delete(nick,Length(nick),1);
      FreeAndNil(L);
      push_irc_msg(nick);
     end;
  'tmp':
     begin
      L:=getTmpVipList;
      L.LineBreak:=',';
      nick:='@'+user+' '+L.Text;
      if (nick<>'') and (nick[Length(nick)]=',') then
       Delete(nick,Length(nick),1);
      FreeAndNil(L);
      push_irc_msg(nick);
     end;
  'update':
     begin
      push_irc_msg(vip_rnd.vip_list_cmd);
      Frmmain.wait_vip_update:=True;
     end;
  else
     begin
      push_irc_msg('@'+user+' !vip [time,settime,info,perm,tmp,update]');
     end;
 end;

 except
  on E:Exception do
  begin
   push_irc_msg(E.Message);
   DumpExceptionCallStack(E);
  end;
 end;
end;

procedure TFrmVipParam.GridKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if ssCtrl in Shift then
 begin
  Case Key of
   Word('F'):OnBtnFind(Sender);
  end;
 end;
end;

procedure TFrmVipParam.OnBtnFind(Sender:TObject);
Var
 Value:String;
 i:Integer;
begin
 Value:='';
 if InputQuery('Поиск','Найти ник:',Value) then
 begin
  i:=FindPosVipUser(Value);
  if (i=-1) then
  begin
   ShowMessage('Не найдено!');
  end else
  begin
   GridVips.SelectRow(i);
  end;
 end;
end;

procedure TFrmVipParam.UpdateStatusBarVips;
begin
 if StatusBarVips<>nil then
  StatusBarVips.SimpleText:='Всего: '+IntToStr(GridVips.RowCount-1);
end;

procedure TFrmVipParam.FormCreate(Sender: TObject);
Var
 FDbcScript:TDbcStatementScript;
 Btn,Tmp1,Tmp2:TButton;
begin
 PanelVips:=TPanel.Create(FrmMain);
 PanelVips.Align:=alClient;
 PanelVips.BevelInner:=bvNone;
 PanelVips.BevelOuter:=bvNone;

 StatusBarVips:=TStatusBar.Create(PanelVips);
 StatusBarVips.SimplePanel:=True;
 StatusBarVips.Parent:=PanelVips;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnInsertVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Добавить';
 Btn.Left:=5;
 Btn.Top :=5;
 Btn.Parent:=PanelVips;

 Tmp1:=Btn;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnDeleteVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Удалить';
 Btn.AnchorSide[akLeft].Side:=asrRight;
 Btn.AnchorSide[akLeft].Control:=Tmp1;
 Btn.Top :=5;
 Btn.BorderSpacing.Left:=10;
 Btn.Parent:=PanelVips;

 Tmp2:=Btn;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnUnVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Unvip';
 Btn.AnchorSide[akLeft].Side:=asrRight;
 Btn.AnchorSide[akLeft].Control:=Tmp2;
 Btn.Top :=5;
 Btn.BorderSpacing.Left:=10;
 Btn.Parent:=PanelVips;

 Tmp2:=Btn;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnFind;
 Btn.AutoSize:=True;
 Btn.Caption:='Поиск';
 Btn.AnchorSide[akLeft].Side:=asrRight;
 Btn.AnchorSide[akLeft].Control:=Tmp2;
 Btn.Top :=5;
 Btn.BorderSpacing.Left:=10;
 Btn.Parent:=PanelVips;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnUpdateVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Обновить с твича';
 Btn.Anchors:=[akTop,akRight];
 Btn.AnchorSide[akRight].Side:=asrRight;
 Btn.AnchorSide[akRight].Control:=PanelVips;
 Btn.Top :=5;
 Btn.BorderSpacing.Right:=5;
 Btn.Parent:=PanelVips;

 Tmp2:=Btn;

 Btn:=TButton.Create(PanelVips);
 Btn.OnClick:=@OnBtnCheckVipClick;
 Btn.AutoSize:=True;
 Btn.Caption:='Проверить на истечение';
 Btn.Anchors:=[akTop,akRight];
 Btn.AnchorSide[akRight].Side:=asrLeft;
 Btn.AnchorSide[akRight].Control:=Tmp2;
 Btn.Top :=5;
 Btn.BorderSpacing.Right:=10;
 Btn.Parent:=PanelVips;

 GridVips:=TDBStringGrid.Create(FrmMain);

 GridVips.Anchors:=[akTop,akLeft,akRight,akBottom];

 GridVips.AnchorSide[akTop]   .Side:=asrBottom;
 GridVips.AnchorSide[akTop]   .Control:=Tmp1;
 GridVips.AnchorSide[akLeft]  .Side:=asrLeft;
 GridVips.AnchorSide[akLeft]  .Control:=PanelVips;
 GridVips.AnchorSide[akRight] .Side:=asrRight;
 GridVips.AnchorSide[akRight] .Control:=PanelVips;
 GridVips.AnchorSide[akBottom].Side:=asrTop;
 GridVips.AnchorSide[akBottom].Control:=StatusBarVips;

 GridVips.BorderSpacing.Top:=5;

 GridVips.RowCount:=1;
 GridVips.Options:=GridVips.Options-[goRowSorting,goAlwaysShowEditor]+[goEditing];
 GridVips.ScrollBars:=ssVertical;
 GridVips.AddColumn('datebeg',' Получено ');
 GridVips.AddColumn('dateend',' Истекает ');
 GridVips.AddColumn('user'   ,' Ник ');
 GridVips.OnEditorCreate:=@VipsEditorCreate;
 GridVips.FOnDbInsert:=@DoVipInsert;
 GridVips.FOnDbUpdate:=@DoVipUpdate;
 GridVips.FOnDbDelete:=@DoVipDelete;
 GridVips.OnKeyDown:=@GridKeyDown;
 GridVips.Parent:=PanelVips;

 SetTimerVipExpired(vip_rnd.Auto_expired);

 FDbcScript:=TDbcStatementScript.Create;
 FDbcScript.Handle.DbcConnection:=DbcThread;
 FDbcScript.Notify.Add(T_FIN,@OnListVips);
 FDbcScript.SetSctipt(FListVipsScript);
 FDbcScript.ExecuteScript;
 FDbcScript.Start;
 FDbcScript.Release;

end;

Procedure TFrmVipParam.InitCfg;
begin
 vip_rnd.Auto_expired:=False;
 vip_rnd.Enable:=True;
 Config.WriteString('vip' ,'enable'  ,'1');
 Config.WriteString('vip' ,'title'   ,vip_rnd.title);
 Config.WriteString('vip' ,'msg_perc',IntToStr(vip_rnd.perc));
 Config.WriteString('vip' ,'days'    ,IntToStr(vip_rnd.days));
 Config.WriteString('vip' ,'max_vips',IntToStr(vip_rnd.max_vips));

 Config.WriteString('vip' ,'auto_expired','0');

 Config.WriteString('vip' ,'title_vor',vip_rnd.title_vor);
 Config.WriteString('vip' ,'perc_vor',IntToStr(vip_rnd.perc_vor));

 case vip_rnd.Enable_vor of
  True :Config.WriteString('vip' ,'enable_vor','1');
  False:Config.WriteString('vip' ,'enable_vor','0');
 end;
end;

Procedure TFrmVipParam.LoadCfg;
begin
 vip_rnd.Enable:=Trim(Config.ReadString('vip','enable','0'))='1';
 vip_rnd.title :=Trim(Config.ReadString('vip','title',vip_rnd.title));
 vip_rnd.perc  :=StrToDWORDDef(Config.ReadString('vip','msg_perc',IntToStr(vip_rnd.perc)),70);
 if vip_rnd.perc>100 then vip_rnd.perc:=100;

 vip_rnd.days:=StrToDWORDDef(Config.ReadString('vip','days',IntToStr(vip_rnd.days)),vip_rnd.days);
 vip_rnd.max_vips:=StrToDWORDDef(Config.ReadString('vip','max_vips',IntToStr(vip_rnd.max_vips)),vip_rnd.max_vips);

 vip_rnd.Auto_expired:=Config.ReadString('vip','auto_expired','1')='1';

 vip_rnd.Enable_vor:=Trim(Config.ReadString('vip','enable_vor','0'))='1';
 vip_rnd.title_vor :=Trim(Config.ReadString('vip','title_vor',vip_rnd.title_vor));
 vip_rnd.perc_vor  :=StrToDWORDDef(Config.ReadString('vip','perc_vor',IntToStr(vip_rnd.perc_vor)),1);
 if vip_rnd.perc_vor>100 then vip_rnd.perc_vor:=100;
end;

Procedure TFrmVipParam.Open;
begin
 CBVipEnable.Checked :=vip_rnd.Enable;
 EdtTitle.Text       :=vip_rnd.title;
 EdtPercent.Text     :=IntToStr(vip_rnd.perc);
 CBVipExpired.Checked:=vip_rnd.Auto_expired;
 EdtVipDays.Text     :=IntToStr(vip_rnd.days);
 EdtMaxVips.Text     :=IntToStr(vip_rnd.max_vips);

 EdtVorTitle.Text    :=vip_rnd.title_vor;
 EdtVorPercent.Text  :=IntToStr(vip_rnd.perc_vor);
 CBVorEnable.Checked :=vip_rnd.Enable_vor;

 if ShowModal=1 then
 begin
  vip_rnd.Enable:=CBVipEnable.Checked;
  vip_rnd.title :=Trim(EdtTitle.Text);
  vip_rnd.perc  :=StrToDWORDDef(EdtPercent.Text,70);
  if vip_rnd.perc>100 then vip_rnd.perc:=100;
  vip_rnd.days  :=StrToDWORDDef(EdtVipDays.Text,30);
  vip_rnd.max_vips:=StrToDWORDDef(EdtMaxVips.Text,0);

  vip_rnd.Auto_expired:=CBVipExpired.Checked;
  SetTimerVipExpired(vip_rnd.Auto_expired);

  vip_rnd.title_vor :=EdtVorTitle.Text;
  vip_rnd.perc_vor  :=StrToDWORDDef(EdtVorPercent.Text,1);
  if vip_rnd.perc_vor>100 then vip_rnd.perc_vor:=100;
  vip_rnd.Enable_vor:=CBVorEnable.Checked;

  try

   case vip_rnd.Enable of
    True :Config.WriteString('vip' ,'enable','1');
    False:Config.WriteString('vip' ,'enable','0');
   end;

   Config.WriteString('vip' ,'title'   ,vip_rnd.title);
   Config.WriteString('vip' ,'msg_perc',IntToStr(vip_rnd.perc));
   Config.WriteString('vip' ,'days'    ,IntToStr(vip_rnd.days));
   Config.WriteString('vip' ,'max_vips',IntToStr(vip_rnd.max_vips));

   case vip_rnd.Auto_expired of
    True :Config.WriteString('vip' ,'auto_expired','1');
    False:Config.WriteString('vip' ,'auto_expired','0');
   end;

   Config.WriteString('vip' ,'title_vor',vip_rnd.title_vor);
   Config.WriteString('vip' ,'perc_vor',IntToStr(vip_rnd.perc_vor));

   case vip_rnd.Enable_vor of
    True :Config.WriteString('vip' ,'enable_vor','1');
    False:Config.WriteString('vip' ,'enable_vor','0');
   end;

  except
   on E:Exception do
   begin
    DumpExceptionCallStack(E);
   end;
  end;
 end;
end;

procedure TFrmVipParam.EdtPercentKeyPress(Sender:TObject;var Key:char);
begin
 prev_perc:=StrToQWORDDef(TLabeledEdit(Sender).Text,70);
 if prev_perc>100 then prev_perc:=100;
 Case Key of
  #8,#9,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

procedure TFrmVipParam.EdtVipDaysExit(Sender: TObject);
var
 S,L:Integer;
 d:DWORD;
begin
 if not TryStrToDWord(TLabeledEdit(Sender).Text,d) then
 begin
  S:=TLabeledEdit(Sender).SelStart ;
  L:=TLabeledEdit(Sender).SelLength;
  TLabeledEdit(Sender).Text:=IntToStr(prev_dw);
  TLabeledEdit(Sender).SelStart :=S;
  TLabeledEdit(Sender).SelLength:=L;
 end;
end;

procedure TFrmVipParam.EdtVipDaysKeyPress(Sender:TObject;var Key:char);
begin
 prev_dw:=StrToDWORDDef(TLabeledEdit(Sender).Text,1);
 Case Key of
  #8,#9,#37,#39:;
  '0'..'9':;
  else
   Key:=#0;
 end;
end;

procedure TFrmVipParam.EdtPercentExit(Sender:TObject);
var
 S,L:Integer;
begin
 case StrToDWordDef(TLabeledEdit(Sender).Text,0) of
  1..100:;
  else
  begin
   S:=TLabeledEdit(Sender).SelStart ;
   L:=TLabeledEdit(Sender).SelLength;
   TLabeledEdit(Sender).Text:=IntToStr(prev_perc);
   TLabeledEdit(Sender).SelStart :=S;
   TLabeledEdit(Sender).SelLength:=L;
  end;
 end;
end;

procedure TFrmVipParam.BtnOkClick(Sender:TObject);
begin
 ModalResult:=mrOk;
 Hide;
end;

procedure TFrmVipParam.BtnCancelClick(Sender:TObject);
begin
 ModalResult:=mrCancel;
 Close;
end;

procedure TFrmVipParam.BtnCancelKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
 Case Key of
  13:BtnCancelClick(Sender);
 end;
end;

procedure TFrmVipParam.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 Case Key of
  13:BtnOkClick(Sender);
 end;
end;

end.

