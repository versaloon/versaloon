unit parameditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, inputdialog, vsprogparser, strparser;

type
  TParam_Warning = record
    mask:  array of BYTE;
    value: array of BYTE;
    msg:   string;
    ban:   boolean;
  end;

  TParam_Choice = record
    value: array of BYTE;
    text:  string;
  end;

  TParam_Setting = record
    name:      string;
    info:      string;
    format:    string;
    mask:      array of BYTE;
    use_checkbox: boolean;
    use_edit:  boolean;
    radix:     integer;
    shift:     integer;
    checked:   array of BYTE;
    unchecked: array of BYTE;
    enabled:   boolean;
    bytelen:   integer;
    choices:   array of TParam_Choice;
  end;

  TParam_Record = record
    init_value: QWord;
    warnings:   array of TParam_Warning;
    settings:   array of TParam_Setting;
  end;

  { TFormParaEditor }

  TFormParaEditor = class(TForm)
    btnOK:     TButton;
    btnCancel: TButton;
    pnlSettings: TPanel;
    pnlButton: TPanel;
    tInit:     TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure pnlButtonClick(Sender: TObject);
    procedure pnlSettingsClick(Sender: TObject);
    procedure SettingChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure CenterControl(ctl: TControl; ref: TControl);
    procedure AdjustComponentColor(Sender: TControl);
    procedure tInitTimer(Sender: TObject);
  private
    { private declarations }
    Param_Record:    TParam_Record;
    ParaEdtNameArr:  array of TEdit;
    ParaComboArr:    array of TComboBox;
    ParaCheckArr:    array of TCheckBox;
    ParaEdtValueArr: array of TEdit;
    Param_Name:      string;
    Param_Format:    string;
    Param_Value:     array of BYTE;
    Param_Bytelen:   integer;
    EnableWarning:   boolean;
    SettingParameter: boolean;
  public
    { public declarations }
    function ParseSettingMaskValue(Sender: TObject; var mask, value: array of BYTE): boolean;
    procedure ValueToSetting();
    procedure UpdateTitle();
    procedure GetResult(var buff: array of BYTE);
    procedure SetParameter(var buff: array of BYTE; format: string;
      title: string; warnEnabled: boolean);
    function ParseLine(line: string): boolean;
    procedure FreeRecord();
  end;

var
  FormParaEditor: TFormParaEditor;
  bCancel: boolean;

const
  LEFT_MARGIN: integer   = 10;
  RIGHT_MARGIN: integer  = 10;
  TOP_MARGIN: integer    = 10;
  BOTTOM_MARGIN: integer = 10;
  X_MARGIN: integer      = 4;
  Y_MARGIN: integer      = 4;
  ITEM_HEIGHT: integer   = 20;
  EDT_WIDTH: integer     = 100;
  COMBO_WIDTH: integer   = 400;
  SECTION_DIV_HEIGHT: integer = 20;

implementation

procedure BufferFunc_And(var src0, src1, dest: array of BYTE);
var
  i: integer;
begin
  for i := Low(dest) to High(dest) do
  begin
    dest[i] := src0[i] and src1[i];
  end;
end;

procedure BufferFunc_Or(var src, mask, dest: array of BYTE);
var
  i: integer;
begin
  for i := Low(dest) to High(dest) do
  begin
    dest[i] := src[i] or mask[i];
  end;
end;

procedure BufferFunc_Not(var dest: array of BYTE);
var
  i: integer;
begin
  for i := Low(dest) to High(dest) do
  begin
    dest[i] := not dest[i];
  end;
end;

procedure BufferFunc_SetValue(var dest: array of BYTE; value: BYTE);
var
  i: integer;
begin
  for i := Low(dest) to High(dest) do
  begin
    dest[i] := value;
  end;
end;

procedure BufferFunc_Copy(var src, dest: array of BYTE);
var
  i: integer;
begin
  for i := Low(dest) to High(dest) do
  begin
    dest[i] := src[i];
  end;
end;

function BufferFunc_NotZero(var src: array of BYTE): boolean;
var
  i: integer;
begin
  for i := Low(src) to High(src) do
  begin
    if src[i] <> 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function BufferFunc_Equal(var arr0, arr1: array of BYTE): boolean;
var
  i: integer;
begin
  for i := Low(arr0) to High(arr1) do
  begin
    if arr0[i] <> arr1[i] then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure BufferFunc_Shr(var arr: array of BYTE; shift: integer);
var
  i, cur_byte_idx: integer;
  cur_byte, next_byte: BYTE;
begin
  if shift <= 0 then
  begin
    Exit;
  end;

  cur_byte_idx := Low(arr) + (shift div 8);
  shift := shift mod 8;

  for i := Low(arr) to High(arr) do
  begin
    if cur_byte_idx <= High(arr) then
    begin
      cur_byte := arr[cur_byte_idx];
    end
    else
    begin
      cur_byte := 0;
    end;

    if (cur_byte_idx + 1) <= High(arr) then
    begin
      next_byte := arr[cur_byte_idx + 1];
    end
    else
    begin
      next_byte := 0;
    end;

    arr[i] := (cur_byte shr shift) or (next_byte shl (8 - shift));
    Inc(cur_byte_idx);
  end;
end;

procedure BufferFunc_Shl(var arr: array of BYTE; shift: integer);
var
  i, cur_byte_idx: integer;
  cur_byte, next_byte: BYTE;
begin
  if shift <= 0 then
  begin
    Exit;
  end;

  cur_byte_idx := High(arr) - (shift div 8);
  shift := shift mod 8;

  for i := High(arr) downto Low(arr) do
  begin
    if cur_byte_idx >= Low(arr) then
    begin
      cur_byte := arr[cur_byte_idx];
    end
    else
    begin
      cur_byte := 0;
    end;

    if (cur_byte_idx - 1) >= Low(arr) then
    begin
      next_byte := arr[cur_byte_idx - 1];
    end
    else
    begin
      next_byte := 0;
    end;

    arr[i] := (cur_byte shl shift) or (next_byte shr (8 - shift));
    Dec(cur_byte_idx);
  end;
end;




function GetStringLen_To_ByteLen(bytelen, radix: integer): integer;
begin
  if (radix < 2) or (radix > 16) or (BYTELEN_ACCORDING_TO_RADIX[radix] = 0) then
  begin
    Result := 0;
    exit;
  end;

  Result := bytelen * BYTELEN_ACCORDING_TO_RADIX[radix];
end;





procedure TFormParaEditor.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormParaEditor.AdjustComponentColor(Sender: TControl);
begin
  if Sender.Enabled then
  begin
    Sender.Color := clWindow;
  end
  else
  begin
    Sender.Color := clBtnFace;
  end;
end;

procedure TFormParaEditor.tInitTimer(Sender: TObject);
var
  i, section_num: integer;
  mask: array of BYTE;
  dest: array of BYTE;
begin
  (Sender as TTimer).Enabled := False;

  SetLength(dest, Param_Bytelen);
  SetLength(mask, Param_Bytelen);
  BufferFunc_SetValue(mask, 0);

  section_num := 0;

  for i := 0 to Length(Param_Record.settings) - 1 do
  begin
    BufferFunc_And(Param_Record.settings[i].mask, mask, dest);
    if BufferFunc_NotZero(dest) then
    begin
      Inc(section_num);
      BufferFunc_SetValue(mask, 0);
    end;
    BufferFunc_Or(mask, Param_Record.settings[i].mask, mask);

    ParaEdtNameArr[i].Top    := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) +
      section_num * SECTION_DIV_HEIGHT;
    ParaEdtNameArr[i].Left   := LEFT_MARGIN;
    ParaEdtNameArr[i].Width  := EDT_WIDTH;
    ParaEdtNameArr[i].Height := ITEM_HEIGHT;
    if Param_Record.settings[i].use_edit then
    begin
      ParaEdtValueArr[i].Top    :=
        TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) + section_num * SECTION_DIV_HEIGHT;
      ParaEdtValueArr[i].Width  := COMBO_WIDTH;
      ParaEdtValueArr[i].Left   := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaEdtValueArr[i].Height := ITEM_HEIGHT;
      CenterControl(ParaEdtValueArr[i], ParaEdtNameArr[i]);
    end
    else if Param_Record.settings[i].use_checkbox then
    begin
      ParaCheckArr[i].Top    := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) +
        section_num * SECTION_DIV_HEIGHT;
      ParaCheckArr[i].Left   := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaCheckArr[i].Height := ITEM_HEIGHT;
      CenterControl(ParaCheckArr[i], ParaEdtNameArr[i]);
    end
    else
    begin
      ParaComboArr[i].Top    := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT) +
        section_num * SECTION_DIV_HEIGHT;
      ParaComboArr[i].Left   := LEFT_MARGIN + EDT_WIDTH + X_MARGIN;
      ParaComboArr[i].Width  := COMBO_WIDTH;
      ParaComboArr[i].Height := ITEM_HEIGHT;
      CenterControl(ParaComboArr[i], ParaEdtNameArr[i]);
    end;
  end;
end;

procedure TFormParaEditor.SettingChange(Sender: TObject);
var
  mask, value: array of BYTE;
begin
  if SettingParameter then
  begin
    exit;
  end;

  SetLength(mask, Param_Bytelen);
  BufferFunc_SetValue(mask, 0);
  SetLength(value, Param_Bytelen);
  BufferFunc_SetValue(value, 0);

  if not ParseSettingMaskValue(Sender, mask, value) = False then
  begin
    BufferFunc_Not(mask);
    BufferFunc_And(Param_Value, mask, Param_Value);
    BufferFunc_Or(Param_Value, value, Param_Value);
  end;

  SettingParameter := True;
  ValueToSetting();
  SettingParameter := False;

  UpdateTitle();
end;

procedure TFormParaEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

procedure TFormParaEditor.pnlButtonClick(Sender: TObject);
begin
  ActiveControl := btnOK;
end;

procedure TFormParaEditor.pnlSettingsClick(Sender: TObject);
begin
  ActiveControl := btnOK;
end;

procedure TFormParaEditor.btnOKClick(Sender: TObject);
begin
  bCancel := False;
end;

procedure TFormParaEditor.btnCancelClick(Sender: TObject);
begin
  bCancel := True;
end;

procedure TFormParaEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: integer;
  dest: array of BYTE;
begin
  FormParaEditor.ActiveControl := btnOK;
  CanClose := True;

  SetLength(dest, Param_Bytelen);
  BufferFunc_SetValue(dest, 0);

  if not bCancel then
  begin
    bCancel := True;
    for i := low(Param_Record.warnings) to high(Param_Record.warnings) do
    begin
      BufferFunc_And(Param_Value, Param_Record.warnings[i].mask, dest);
      if BufferFunc_Equal(dest, Param_Record.warnings[i].Value) then
      begin
        if Param_Record.warnings[i].ban then
        begin
          CanClose := False;
          MessageDlg(Param_Record.warnings[i].msg, mtError, [mbOK], 0);
          break;
        end
        else if EnableWarning and (mrNo = MessageDlg(Param_Record.warnings[i].msg,
          mtWarning, [mbYes, mbNo], 0)) then
        begin
          CanClose := False;
          break;
        end;
      end;
    end;
  end;
end;

procedure TFormParaEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  FreeRecord();

  for i := low(ParaEdtNameArr) to high(ParaEdtNameArr) do
  begin
    if Assigned(ParaEdtNameArr[i]) then
    begin
      ParaEdtNameArr[i].Destroy;
    end;
  end;
  for i := low(ParaComboArr) to high(ParaComboArr) do
  begin
    if Assigned(ParaComboArr[i]) then
    begin
      ParaComboArr[i].Destroy;
    end;
  end;
  for i := low(ParaCheckArr) to high(ParaCheckArr) do
  begin
    if Assigned(ParaCheckArr[i]) then
    begin
      ParaCheckArr[i].Destroy;
    end;
  end;
  for i := low(ParaEdtValueArr) to high(ParaEdtValueArr) do
  begin
    if Assigned(ParaEdtValueArr[i]) then
    begin
      ParaEdtValueArr[i].Destroy;
    end;
  end;
  SetLength(ParaEdtNameArr, 0);
  SetLength(ParaComboArr, 0);
  SetLength(ParaCheckArr, 0);
  SetLength(ParaEdtValueArr, 0);

  CloseAction := caHide;
end;

procedure TFormParaEditor.GetResult(var buff: array of BYTE);
begin
  BufferFunc_Copy(Param_Value, buff);
end;

procedure TFormParaEditor.UpdateTitle();
var
  strTmp: String;
begin
  strTmp := '';
  if not strparser_solve(strTmp, Param_Format, Param_Value) then
  begin
    strTmp := strparser_ErrMsg;
  end;
  Caption := Param_Name + ': ' + strTmp;
end;

procedure TFormParaEditor.SetParameter(var buff: array of BYTE; format: string;
  title: string; warnEnabled: boolean);
begin
  Param_Bytelen := Length(buff);
  SetLength(Param_Value, Param_Bytelen);
  Move(buff[0], Param_Value[0], Param_Bytelen);
  Param_Format  := format;
  Param_Name    := title;
  EnableWarning := warnEnabled;
end;

function TFormParaEditor.ParseSettingMaskValue(Sender: TObject;
  var mask, value: array of BYTE): boolean;
var
  i: integer;
  strTmp: string;
begin
  Result := False;
  strTmp := '';

  if Sender is TComboBox then
  begin
    i := (Sender as TComboBox).Tag;
  end
  else if Sender is TCheckBox then
  begin
    i := (Sender as TCheckBox).Tag;
  end
  else if Sender is TEdit then
  begin
    i := (Sender as TEdit).Tag;
  end
  else
  begin
    // this component is not supported as a setting component
    exit;
  end;

  BufferFunc_Copy(Param_Record.settings[i].mask, mask);
  if Param_Record.settings[i].use_edit then
  begin
    strTmp := ParaEdtValueArr[i].Text;
    if not strparser_parse(strTmp, Param_Record.settings[i].format, value) then
    begin
      Exit;
    end;
    BufferFunc_Shl(value, Param_Record.settings[i].shift);
    BufferFunc_And(value, mask, value);
  end
  else if Param_Record.settings[i].use_checkbox then
  begin
    if ParaCheckArr[i].Checked then
    begin
      BufferFunc_Copy(Param_Record.settings[i].checked, value);
    end
    else
    begin
      BufferFunc_Copy(Param_Record.settings[i].unchecked, value);
    end;
  end
  else
  begin
    BufferFunc_Copy(Param_Record.settings[i].choices[ParaComboArr[i].ItemIndex].value, value);
  end;

  Result := True;
end;

procedure TFormParaEditor.ValueToSetting();
var
  i, j:  integer;
  value: array of BYTE;
  strTmp: String;
  found: boolean;
begin
  SettingParameter := True;
  strTmp := '';

  SetLength(value, Param_Bytelen);
  BufferFunc_SetValue(value, 0);

  for i := low(Param_Record.settings) to high(Param_Record.settings) do
  begin
    BufferFunc_And(Param_Value, Param_Record.settings[i].mask, value);
    if Param_Record.settings[i].use_edit then
    begin
      BufferFunc_Shr(value, Param_Record.settings[i].shift);
      strparser_solve(strTmp, Param_Record.settings[i].format,
        value);
      ParaEdtValueArr[i].Text := strTmp;
      ParaEdtValueArr[i].Hint := strTmp;
    end
    else if Param_Record.settings[i].use_checkbox then
    begin
      if BufferFunc_Equal(value, Param_Record.settings[i].checked) then
      begin
        ParaCheckArr[i].Checked := True;
      end
      else if BufferFunc_Equal(value, Param_Record.settings[i].unchecked) then
      begin
        ParaCheckArr[i].Checked := False;
      end
      else
      begin
        // unrecognized value
        ParaEdtNameArr[i].Color := clRed;
      end;
    end
    else
    begin
      found := False;
      for j := low(Param_Record.settings[i].choices)
        to high(Param_Record.settings[i].choices) do
      begin
        if BufferFunc_Equal(value, Param_Record.settings[i].choices[j].Value) then
        begin
          ParaEdtNameArr[i].Color := clWindow;
          ParaComboArr[i].ItemIndex := j;
          found := True;
          break;
        end;
      end;
      if not found then
      begin
        // there is an error
        ParaEdtNameArr[i].Color := clRed;
      end;
      ParaComboArr[i].Hint := ParaComboArr[i].Text;
    end;
  end;
  SettingParameter := False;
end;

procedure TFormParaEditor.FormShow(Sender: TObject);
var
  i, j: integer;
  settings_num, choices_num, section_num: integer;
  mask: array of BYTE;
  dest: array of BYTE;
begin
  bCancel := True;

  // create components according to Param_Record
  settings_num := Length(Param_Record.settings);
  SetLength(ParaEdtNameArr, settings_num);
  SetLength(ParaComboArr, settings_num);
  SetLength(ParaCheckArr, settings_num);
  SetLength(ParaEdtValueArr, settings_num);

  SetLength(dest, Param_Bytelen);
  SetLength(mask, Param_Bytelen);
  BufferFunc_SetValue(mask, 0);

  section_num := 0;

  SettingParameter := False;
  for i := 0 to settings_num - 1 do
  begin
    BufferFunc_And(Param_Record.settings[i].mask, mask, dest);
    if BufferFunc_NotZero(dest) then
    begin
      Inc(section_num);
      BufferFunc_SetValue(mask, 0);
    end;
    BufferFunc_Or(mask, Param_Record.settings[i].mask, mask);

    ParaEdtNameArr[i]      := TEdit.Create(Self);
    ParaEdtNameArr[i].Parent := pnlSettings;
    ParaEdtNameArr[i].Text := Param_Record.settings[i].Name;
    if Param_Record.settings[i].use_edit then
    begin
      ParaEdtNameArr[i].Text :=
        ParaEdtNameArr[i].Text + '(r:' + IntToStr(Param_Record.settings[i].radix) + ')';
    end;
    ParaEdtNameArr[i].Color    := clWindow;
    ParaEdtNameArr[i].Hint     := Param_Record.settings[i].info;
    ParaEdtNameArr[i].ShowHint := True;
    ParaEdtNameArr[i].ReadOnly := True;
    if Param_Record.settings[i].use_edit then
    begin
      ParaEdtValueArr[i]     := TEdit.Create(Self);
      ParaEdtValueArr[i].Parent := pnlSettings;
      ParaEdtValueArr[i].OnExit := @SettingChange;
      ParaEdtValueArr[i].Tag := i;
      ParaEdtValueArr[i].ShowHint := True;
      ParaEdtValueArr[i].Enabled := Param_Record.settings[i].Enabled;
      AdjustComponentColor(ParaEdtValueArr[i]);
    end
    else if Param_Record.settings[i].use_checkbox then
    begin
      ParaCheckArr[i]     := TCheckBox.Create(Self);
      ParaCheckArr[i].Parent := pnlSettings;
      ParaCheckArr[i].OnChange := @SettingChange;
      ParaCheckArr[i].Tag := i;
      ParaCheckArr[i].Enabled := Param_Record.settings[i].Enabled;
      ParaCheckArr[i].Caption := '';
      ParaCheckArr[i].Checked := False;
    end
    else
    begin
      ParaComboArr[i]     := TComboBox.Create(Self);
      ParaComboArr[i].Parent := pnlSettings;
      ParaComboArr[i].OnChange := @SettingChange;
      ParaComboArr[i].Style := csDropDownList;
      ParaComboArr[i].Tag := i;
      ParaComboArr[i].ShowHint := True;
      ParaComboArr[i].Enabled := Param_Record.settings[i].Enabled;
      AdjustComponentColor(ParaComboArr[i]);
      ParaComboArr[i].Clear;
      choices_num := Length(Param_Record.settings[i].choices);
      for j := 0 to choices_num - 1 do
      begin
        ParaComboArr[i].Items.Add(Param_Record.settings[i].choices[j].Text);
      end;
    end;
  end;

  i := TOP_MARGIN + BOTTOM_MARGIN + settings_num * (Y_MARGIN + ITEM_HEIGHT) +
    section_num * SECTION_DIV_HEIGHT;
  ClientHeight := i + pnlButton.Height;
  pnlSettings.ClientHeight := i;
  i := LEFT_MARGIN + RIGHT_MARGIN + EDT_WIDTH + X_MARGIN + COMBO_WIDTH;
  ClientWidth := i;
  pnlSettings.ClientWidth := i;
  pnlButton.ClientWidth := i;
  // center buttons
  btnOK.Left := (pnlButton.Width div 2 - btnOK.Width) div 2;
  btnCancel.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnOK.Width) div 2;
  UpdateTitle();

  ValueToSetting();
  UpdateShowing;

  tInit.Enabled := True;
end;

procedure TFormParaEditor.FreeRecord();
begin
  SetLength(Param_Record.warnings, 0);
  SetLength(Param_Record.settings, 0);
end;

function TFormParaEditor.ParseLine(line: string): boolean;
var
  i, j, num, dis: integer;
  strTmp: string;
begin
  Result := False;
  strTmp := '';

  if Pos('warning: ', line) = 1 then
  begin
    i := Length(Param_Record.warnings) + 1;
    SetLength(Param_Record.warnings, i);
    SetLength(Param_Record.warnings[i - 1].mask, Param_Bytelen);
    SetLength(Param_Record.warnings[i - 1].value, Param_Bytelen);

    GetParameter(line, 'mask', strTmp);
    if (strTmp <> '') and
      not strparser_parse(strTmp, Param_Format, Param_Record.warnings[i - 1].mask) then
    begin
      Exit;
    end;

    GetParameter(line, 'value', strTmp);
    if (strTmp <> '') and
      not strparser_parse(strTmp, Param_Format, Param_Record.warnings[i - 1].value) then
    begin
      Exit;
    end;

    GetParameter(line, 'msg', Param_Record.warnings[i - 1].msg);
    num := 0;
    GetParameter(line, 'ban', num);
    Param_Record.warnings[i - 1].ban := num > 0;
  end
  else if Pos('setting: ', line) = 1 then
  begin
    i := Length(Param_Record.settings) + 1;
    SetLength(Param_Record.settings, i);
    SetLength(Param_Record.settings[i - 1].mask, Param_Bytelen);
    SetLength(Param_Record.settings[i - 1].checked, Param_Bytelen);
    SetLength(Param_Record.settings[i - 1].unchecked, Param_Bytelen);
    SetLength(Param_Record.settings[i - 1].choices, 0);

    GetParameter(line, 'mask', strTmp);
    if (strTmp <> '') and
      not strparser_parse(strTmp, Param_Format, Param_Record.settings[i - 1].mask) then
    begin
      Exit;
    end;

    Param_Record.settings[i - 1].use_checkbox := False;
    GetParameter(line, 'checked', strTmp);
    if strTmp <> '' then
    begin
      if strparser_parse(strTmp, Param_Format, Param_Record.settings[i - 1].checked) then
      begin
        Param_Record.settings[i - 1].use_checkbox := True;
      end
      else
      begin
        Exit;
      end;
    end;

    GetParameter(line, 'unchecked', strTmp);
    if (strTmp <> '') then
    begin
      if strparser_parse(strTmp, Param_Format, Param_Record.settings[i - 1].unchecked) then
      begin
        Param_Record.settings[i - 1].use_checkbox := True;
      end
      else
      begin
        Exit;
      end;
    end;

    GetParameter(line, 'name', Param_Record.settings[i - 1].Name);
    Param_Record.settings[i - 1].bytelen := 0;
    GetParameter(line, 'bytelen', Param_Record.settings[i - 1].bytelen);
    Param_Record.settings[i - 1].radix := 0;
    GetParameter(line, 'radix', Param_Record.settings[i - 1].radix);
    if (Param_Record.settings[i - 1].radix < 2) or
      (Param_Record.settings[i - 1].radix > 16) then
    begin
      Param_Record.settings[i - 1].radix := 10;
    end;
    GetParameter(line, 'shift', Param_Record.settings[i - 1].shift);
    GetParameter(line, 'info', Param_Record.settings[i - 1].info);

    GetParameter(line, 'format', Param_Record.settings[i - 1].format);
    if (Param_Record.settings[i - 1].format = '') and
      (Param_Record.settings[i - 1].bytelen > 0) then
    begin
      strTmp := '%' + IntToStr(Param_Record.settings[i - 1].bytelen);

      case Param_Record.settings[i - 1].radix of
      0, 10:
        strTmp := strTmp + 'd';
      2:
        strTmp := strTmp + 'b';
      16:
        strTmp := strTmp + 'x';
      else
        begin
          Exit;
        end;
      end;

      Param_Record.settings[i - 1].format := strTmp;
    end;

    dis := 0;
    GetParameter(line, 'disabled', dis);
    if dis > 0 then
    begin
      Param_Record.settings[i - 1].Enabled := False;
    end
    else
    begin
      Param_Record.settings[i - 1].Enabled := True;
    end;
    num := 0;
    GetParameter(line, 'num_of_choices', num);
    if not Param_Record.settings[i - 1].use_checkbox and (num = 0) then
    begin
      Param_Record.settings[i - 1].use_edit := True;
    end
    else
    begin
      Param_Record.settings[i - 1].use_edit := False;
    end;
  end
  else if Pos('choice: ', line) = 1 then
  begin
    i := Length(Param_Record.settings);
    j := Length(Param_Record.settings[i - 1].choices) + 1;
    SetLength(Param_Record.settings[i - 1].choices, j);
    SetLength(Param_Record.settings[i - 1].choices[j - 1].value, Param_Bytelen);

    GetParameter(line, 'value', strTmp);
    if (strTmp <> '') and
      not strparser_parse(strTmp, Param_Format, Param_Record.settings[i - 1].choices[j - 1].value) then
    begin
      Exit;
    end;

    GetParameter(line, 'text', Param_Record.settings[i - 1].choices[j - 1].text);
  end;

  Result := True;
end;

initialization
  {$I parameditor.lrs}

end.

