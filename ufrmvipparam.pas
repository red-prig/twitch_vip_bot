unit UFrmVipParam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,MaskEdit,
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
    procedure VipsEditorCreate(Sender:TObject;ACol,ARow:Integer;var AEditor:TWinControl);
    function  FindVipUser(Const FValue:RawByteString):Integer;
    function  DoVipInsert(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
    function  DoVipUpdate(Const FName,FValue:RawByteString;ACol,ARow:Integer;AEditor:TWinControl):Boolean;
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
    Procedure InitCfg;
    Procedure LoadCfg;
    Procedure Open;
  end;

var
  FrmVipParam: TFrmVipParam;

  PanelVips:TPanel;
  GridVips:TDBStringGrid;

  FListVipsScript  :TSQLScript;
  FAddVipsScript   :TSQLScript;
  FInsertVipsScript:TSQLScript;
  FUpdateVipsScript:TSQLScript;
  FDeleteVipsScript:TSQLScript;

implementation

Uses
  ULog,Main,ujson,DateUtils;

{$R *.lfm}

function TryGetDateTime(const S:RawByteString;out Value:TDateTime):Boolean;
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
begin
 Result:=-1;
 C:=GridVips.RowCount;
 if C>1 then
 begin
  u:=GridVips.FindColumn('user');
  if u<>-1 then
   For i:=1 to C-1 do
    if GridVips.Cells[u,i]=FValue then
     Exit(i);
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
     if TryGetDateTime(FValue,DT) then
     begin

      FDbcScript:=TDbcStatementScript.Create;
      FDbcScript.Handle.DbcConnection:=DbcThread;
      FDbcScript.SetSctipt(FInsertVipsScript);
      FDbcScript.ExecuteScript;
      FDbcScript.Params.SetRawByteString(':field','datetime');
      FDbcScript.Params.SetAsDateTime('value',DT);
      FDbcScript.Start;
      FDbcScript.Release;

      GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr(GetDateTimeEnd(DT));

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

     FDbcScript:=TDbcStatementScript.Create;
     FDbcScript.Handle.DbcConnection:=DbcThread;
     FDbcScript.SetSctipt(FUpdateVipsScript);
     FDbcScript.ExecuteScript;
     FDbcScript.Params.SetRawByteString(':field','datetime');
     FDbcScript.Params.SetAsNull('value');
     FDbcScript.Params.SetRawByteString('user',GridVips.FieldValue['user',ARow]);
     FDbcScript.Start;
     FDbcScript.Release;

     GridVips.FieldValue[FName    ,ARow]:='';
     GridVips.FieldValue['dateend',ARow]:='';

     GridVips.ResetRowInsert;
     Result:=False;
    end else
    begin
     if TryGetDateTime(FValue,DT) then
     begin

      FDbcScript:=TDbcStatementScript.Create;
      FDbcScript.Handle.DbcConnection:=DbcThread;
      FDbcScript.SetSctipt(FUpdateVipsScript);
      FDbcScript.ExecuteScript;
      FDbcScript.Params.SetRawByteString(':field','datetime');
      FDbcScript.Params.SetAsDateTime('value',DT);
      FDbcScript.Params.SetRawByteString('user',GridVips.FieldValue['user',ARow]);
      FDbcScript.Start;
      FDbcScript.Release;

      GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr(GetDateTimeEnd(DT));

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

 if not Result then Exit;

 DbDeleteVip(GridVips.FieldValue['user',ARow]);
end;

procedure TFrmVipParam.DeleteVip(aRow:Integer);
begin
 if (aRow>=GridVips.FixedRows) and GridVips.RowValid(aRow) then
 begin
  DbDeleteVip(GridVips.FieldValue['user',ARow]);
  GridVips.DeleteRow(aRow);
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
  GridVips.FieldValue['datebeg',ARow]:=GetDateTimeStr(DT);
  GridVips.FieldValue['dateend',ARow]:=GetDateTimeStr(GetDateTimeEnd(DT));
 end;

 GridVips.FieldValue['user'   ,ARow]:=FUser;

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
    vip_rnd.Timer.Interval:={30*}60*1000; //30min
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
 i,s,u:SizeInt;
 L:array of Integer;
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
  u:=GridVips.FindColumn('dateend');
  ND:=Now;
  if (u<>-1) then
   For i:=1 to s-1 do
   begin
    T:=GridVips.Cells[u,i];
    if (T<>'') and TryGetDateTime(T,DT) then
    begin
     if ND>DT then
     begin
      SetLength(L,Length(L)+1);
      L[Length(L)-1]:=i;
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
end;

procedure TFrmVipParam.OnBtnInsertVipClick(Sender:TObject);
begin
 GridVips.DoInsert;
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
 aRow:=self.FindVipUser(FUser);
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
    GridVips.FieldValue['datebeg',i]:=GetDateTimeStr(ResultSet.GetDouble(datetime_f));
    GridVips.FieldValue['dateend',i]:=GetDateTimeStr(GetDateTimeEnd(ResultSet.GetDouble(datetime_f)));
    GridVips.FieldValue['user'   ,i]:=ResultSet.GetRawByteString(user_f);
   end;

  end;
 end;

 if GridVips.HandleAllocated then
 begin
  //GridVips.ScrollModeVert:=smCell;
  //GridVips.ScrollModeVert:=smSmooth;
  GridVips.Columns[0].Extent:=GridVips.Columns[0].MinExtent;
 end;
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
 GridVips.AnchorSide[akBottom].Side:=asrBottom;
 GridVips.AnchorSide[akBottom].Control:=PanelVips;

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

