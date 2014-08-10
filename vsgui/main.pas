unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, ExtCtrls, cli_caller, parameditor, Menus, Buttons, Spin,
  Synaser, com_setup, fileselector, hexeditor, XMLCfg, vsprogparser, vsprogtarget,
  vsprogprogrammer, inputdialog, FileUtil, texteditor, strparser;

type

  { TPollThread }

  TPollThread = class(TThread)
  private
    FTargetVoltage: integer;
    FProgrammerAbilities: string;
    FConnectOK: boolean;
    FAppPath: string;
    procedure Update;
  protected
    procedure Execute; override;
  public
    constructor Create();
    property AppPath: string Read FAppPath Write FAppPath;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    btnEditFuse: TButton;
    btnEditEE: TButton;
    btnEditUsrSig: TButton;
    btnOpenOCDRun: TButton;
    btnOpenOCDStop: TButton;
    btnUpdate: TButton;
    btnSVFRun: TButton;
    btnEditCali: TButton;
    btnOpenFile: TButton;
    btnSetPower: TButton;
    btnComSetup: TButton;
    btnEditInterface: TButton;
    btnEditTarget: TButton;
    btnEditScript: TButton;
    btnEditSpecialStr: TButton;
    btnBatchProgram: TButton;
    cbboxOpenOCDInterface: TComboBox;
    cbboxOpenOCDScript: TComboBox;
    cbboxOpenOCDTarget: TComboBox;
    chkboxSpecialStr: TCheckBox;
    chkboxNowarning: TCheckBox;
    chkboxCali: TCheckBox;
    chkboxNoconnect: TCheckBox;
    chkboxUsrSig: TCheckBox;
    chkboxMP:  TCheckBox;
    chkboxFuse: TCheckBox;
    chkboxEE:  TCheckBox;
    cbboxMode: TComboBox;
    cbboxCOM:  TComboBox;
    cbboxInputFile: TComboBox;
    cbboxPower: TComboBox;
    cbboxTargetType: TComboBox;
    dedtOpenOCD: TDirectoryEdit;
    dedtVSprog: TDirectoryEdit;
    edtSVFOption: TEdit;
    edtOpenOCDOption: TEdit;
    fneditSVFFile: TFileNameEdit;
    fnFW:      TFileNameEdit;
    gbChipName: TGroupBox;
    btnEditApp: TButton;
    btnEditLock: TButton;
    btnErase:  TButton;
    btnRead:   TButton;
    btnTargetDetect: TButton;
    btnVerify: TButton;
    btnWrite:  TButton;
    cbboxTarget: TComboBox;
    chkboxApp: TCheckBox;
    chkboxEraseBeforeWrite: TCheckBox;
    chkboxLock: TCheckBox;
    chkboxVerifyAfterWrite: TCheckBox;
    gbInputFile: TGroupBox;
    gbOperation: TGroupBox;
    gbOption:  TGroupBox;
    gbUpdate:  TGroupBox;
    gbOpenOCD: TGroupBox;
    gbSVFPlayer: TGroupBox;
    gbPower:   TGroupBox;
    gbSetting: TGroupBox;
    lbledtSpecialStr: TLabeledEdit;
    lblKHz:    TLabel;
    lblOpenOCDDir: TLabel;
    lblVSProgDir: TLabel;
    lblPowerUnit: TLabel;
    lbledtCali: TLabeledEdit;
    lbledtUsrSig: TLabeledEdit;
    lblSVFOption: TLabel;
    lblSVFFile: TLabel;
    lblOpenOCDOption: TLabel;
    lblOpenOCDInterface: TLabel;
    lblOpenOCDTarget: TLabel;
    lblOpenOCDScript: TLabel;
    lbledtAddr: TLabeledEdit;
    lbledtExtraPara: TLabeledEdit;
    lbledtLock: TLabeledEdit;
    lbledtFuse: TLabeledEdit;
    lblMode:   TLabel;
    lblInputFile: TLabel;
    memoAbout: TMemo;
    memoInfo:  TMemo;
    memoLog:   TMemo;
    miExit:    TMenuItem;
    odInputFile: TOpenDialog;
    pnlAbout:  TPanel;
    pgbarMain: TProgressBar;
    pnlMain:   TPanel;
    pcMain:    TPageControl;
    pmTray:    TPopupMenu;
    sbMain:    TStatusBar;
    sedtFreq:  TSpinEdit;
    shapeResult: TShape;
    tsBatchProgrammer: TTabSheet;
    tBlink: TTimer;
    tInit:     TTimer;
    tsVsprog:  TTabSheet;
    tsJTAG:    TTabSheet;
    tiMain:    TTrayIcon;
    tsAbout:   TTabSheet;
    xmlcfgMain: TXMLConfig;
    procedure btnEditAppClick(Sender: TObject);
    procedure btnEditCaliClick(Sender: TObject);
    procedure btnEditEEClick(Sender: TObject);
    procedure btnEditFuseClick(Sender: TObject);
    procedure btnEditInterfaceClick(Sender: TObject);
    procedure btnEditLockClick(Sender: TObject);
    procedure btnEditScriptClick(Sender: TObject);
    procedure btnEditTargetClick(Sender: TObject);
    procedure btnEditUsrSigClick(Sender: TObject);
    procedure btnEraseClick(Sender: TObject);
    procedure btnComSetupClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOpenOCDRunClick(Sender: TObject);
    procedure btnOpenOCDStopClick(Sender: TObject);
    procedure btnBatchProgramClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnSetPowerClick(Sender: TObject);
    procedure btnEditSpecialStrClick(Sender: TObject);
    procedure btnSVFRunClick(Sender: TObject);
    procedure btnTargetDetectClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure cbboxInputFileChange(Sender: TObject);
    procedure cbboxModeChange(Sender: TObject);
    procedure cbboxTargetChange(Sender: TObject);
    procedure cbboxTargetTypeChange(Sender: TObject);
    procedure dedtPathChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbledtExtraParaKeyPress(Sender: TObject; var Key: char);
    procedure miExitClick(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: boolean);
    procedure pcMainPageChanged(Sender: TObject);
    procedure sedtFreqEditingDone(Sender: TObject);
    procedure tBlinkTimer(Sender: TObject);
    procedure tiMainClick(Sender: TObject);
    procedure tInitTimer(Sender: TObject);

    procedure CenterControl(ctl: TControl; ref: TControl);
    procedure AdjustComponentColor(Sender: TControl);
    procedure ShowDebugLog();
    procedure HideDebugLog();
    procedure ComboBoxSetText(var combo: TComboBox; aText: string);
    function ComboBoxGetIdx(combo: TComboBox; aText: string): integer;
    procedure UpdateTitle();
    procedure LogInfo(info: string);
    procedure UpdateComboTargetFile();

    { VSProg declarations }

    procedure VSProg_GUIUpdateULCS(format: String; buff: array of BYTE;
      var edt: TLabeledEdit);
    procedure VSProg_GUIUpdateStr(Str: String; var edt: TLabeledEdit);
    procedure VSProg_GUIUpdateFuse(format: String; buff: array of BYTE);
    procedure VSProg_GUIUpdateLock(format: String; buff: array of BYTE);
    procedure VSProg_GUIUpdateUsrSig(format: String; buff: array of BYTE);
    procedure VSProg_GUIUpdateCali(format: String; buff: array of BYTE);
    procedure ShowTargetArea(AreaName: char; var Sender: TObject;
      parser: TParserFunc; var buffer: array of BYTE);
    function VSProg_PrepareToRun(aApplicationName: string): boolean;
    function VSProg_PrepareToRunCLI: boolean;
    function VSProg_PrepareToRunOpenOCD: boolean;
    function VSProg_RunAlgorithm(var caller: TCLI_Caller; parser: TParserFunc;
      result_num: integer; silent: boolean): boolean;
    function VSProg_SettingTargetParserCallback(var line: string): boolean;
    procedure VSProg_LogOutput(var line: string);
    procedure VSProg_LogProgress(ProgressInfo: TProgressInfoType; info: string);
    function VSProg_GUITargetSeriesPageInit(index: integer): boolean;
    procedure VSProg_GUITargetAreaInit;
    procedure VSProg_GUIModeInit;
    procedure VSProg_GUIFeatureInit(para: string; append: boolean);
    function VSProg_GetEnabledOperationString(): string;
    function VSProg_GetTargetDefineParameters(): string;
    function VSProg_AddEraseOperation(): boolean;
    function VSProg_AddWriteOperation(): boolean;
    function VSProg_AddVerifyOperation(): boolean;
    function VSProg_AddReadOperation(): boolean;
    procedure VSProg_PrepareMiscParameters(var caller: TCLI_Caller);
    procedure VSProg_PrepareBaseParameters(var caller: TCLI_Caller);
    procedure VSProg_PrepareOperationParameters(var caller: TCLI_Caller);
    { OpenOCD declarations }
    function OpenOCD_Init(): boolean;
  private
    { private declarations }
    bReadOperation: boolean;
    bOpenFileOK: boolean;
    JTAGPage_Init: boolean;
    TargetPage_Init: boolean;
    AboutPage_Init: boolean;
    Page_Init: boolean;
  public
    { public declarations }
    strProgrammerAbilities: string;
  end;

var
  FormMain:     TFormMain;
  PollThread:   TPollThread;
  VSProg_Taken_By_Polling: boolean;
  VSProg_Version: string;
  VSProg_Caller: TCLI_Caller;
  VSProg_Parser: TVSprog_Parser;
  VSProg_Targets: TVSProg_Targets;
  VSProg_Exists: boolean;
  OpenOCD_Exists: boolean;
  CurTargetChip: TTargetChip;
  CurTargetSeries: TTargetSeries;
  ComMode:      TComMode;
  PreviousTarget: string;
  CurProgrammerInfo: string;
  ProgrammerParameter: string;
  PreviousTargetIndex: integer;

const
  BATCH_PROGRAM_CMDLINE: string = '';
  DEBUG_LOG_SHOW: boolean = False;
  POLLTHREAD_EN: boolean = True;
  DISPLAY_ALL_COMPORT_WHEN_UPDATE = True;
  APP_STR: string     = 'Vsgui';
  VERSION_STR: string = '';
  {$IFDEF UNIX}
  VSPROG_STR: string  = 'vsprog';
  {$ELSE}
  VSPROG_STR: string  = 'vsprog.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  OPENOCD_STR: string = 'openocd';
  {$ELSE}
  OPENOCD_STR: string = 'openocd.exe';
  {$ENDIF}
  LOGMEMO_WIDTH: integer = 400;

implementation

{ TPollThread }

procedure TPollThread.Update;
begin
  if FConnectOK then
  begin
    CurProgrammerInfo := FormatFloat('0.0', FTargetVoltage / 1000) + 'V';
  end
  else
  begin
    CurProgrammerInfo := 'N.C.';
  end;
  FormMain.strProgrammerAbilities := FProgrammerAbilities;
  FormMain.UpdateTitle();
end;

procedure TPollThread.Execute;
var
  i: integer;
begin
  i := 0;
  while not Terminated do
  begin
    Sleep(50);
    Inc(i);
    if i < 20 then
    begin
      continue;
    end;
    i := 0;

    if not VSProg_Caller.Take() then
    begin
      // not available now
      continue;
    end;
    VSProg_Taken_By_Polling := True;

    FTargetVoltage := 0;
    VSProg_Caller.Application := FAppPath;
    VSProg_Caller.RemoveAllParameters();
    if ProgrammerParameter <> '' then
    begin
      VSProg_Caller.AddParameter('U' + ProgrammerParameter);
    end;
    VSProg_Caller.AddParameter('pversaloon');
    VSProg_Parser.Prepare();
    VSProg_Parser.ParserFunc      := @VSProg_Parser.ProgrammerParser;
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Caller.bProcessMessage := False;
    VSProg_Caller.Run(@VSProg_Parser.CommonParser, False, True);
    VSProg_Caller.bProcessMessage := True;
    VSProg_Parser.LogOutputEnable := True;
    if (not VSProg_Parser.HasError) and (VSProg_Parser.ResultStrings.Count > 1) then
    begin
      FConnectOK     := True;
      FProgrammerAbilities := VSProg_Parser.ResultStrings.Strings[0];
      FTargetVoltage := StrToInt(VSProg_Parser.ResultStrings.Strings[1]);
    end
    else
    begin
      FConnectOK     := False;
      FTargetVoltage := 0;
    end;

    if FConnectOK and (ProgrammerParameter = '') then
    begin
      if not VSProg_Caller.Take() then
      begin
        // not available now
        continue;
      end;
      VSProg_Caller.Application := FAppPath;
      VSProg_Caller.RemoveAllParameters();
      VSProg_Caller.AddParameter('L');
      VSProg_Parser.Prepare();
      VSProg_Parser.ParserFunc      := @VSProg_Programmer.ProgrammerParser;
      VSProg_Parser.LogOutputEnable := False;
      VSProg_Caller.bProcessMessage := False;
      VSProg_Caller.Run(@VSProg_Parser.CommonParser, False, True);
      VSProg_Caller.bProcessMessage := True;
      VSProg_Parser.LogOutputEnable := True;
      if not VSProg_Parser.HasError then
      begin
        ProgrammerParameter := VSProg_Programmer.SerialNumber;
      end;
    end;

    Synchronize(@Update);
    VSProg_Taken_By_Polling := False;
  end;
end;

constructor TPollThread.Create();
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

{ TFormMain }

function TFormMain.ComboBoxGetIdx(combo: TComboBox; aText: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to combo.Items.Count - 1 do
  begin
    if combo.Items.Strings[i] = aText then
    begin
      Result := i;
    end;
  end;
end;

procedure TFormMain.ComboBoxSetText(var combo: TComboBox; aText: string);
var
  i: integer;
begin
  i := ComboBoxGetIdx(combo, aText);
  if i >= 0 then
  begin
    combo.ItemIndex := i;
  end;
end;

procedure TFormMain.tInitTimer(Sender: TObject);
var
  ControlLeft: integer;
const
  poll_cnt: integer = 0;
begin
  Inc(poll_cnt);
  if poll_cnt > 3 then
  begin
    (Sender as TTimer).Enabled := False;
  end;

  // adjust size and position
  CenterControl(btnTargetDetect, cbboxTarget);
  CenterControl(lblInputFile, cbboxInputFile);
  CenterControl(btnOpenFile, cbboxInputFile);
  CenterControl(lblMode, cbboxMode);
  CenterControl(lbledtAddr, cbboxMode);
  CenterControl(btnComSetup, cbboxMode);
  CenterControl(sedtFreq, cbboxMode);
  CenterControl(lblKHz, cbboxMode);
  CenterControl(chkboxNoconnect, lbledtSpecialStr);
  CenterControl(chkboxNowarning, lbledtSpecialStr);
  CenterControl(lblOpenOCDInterface, cbboxOpenOCDInterface);
  CenterControl(lblOpenOCDTarget, cbboxOpenOCDTarget);
  CenterControl(lblOpenOCDScript, cbboxOpenOCDScript);
  CenterControl(lblOpenOCDOption, edtOpenOCDOption);
  CenterControl(lblSVFFile, fneditSVFFile);
  CenterControl(lblSVFOption, edtSVFOption);
  CenterControl(btnSVFRun, edtSVFOption);
  CenterControl(btnSetPower, cbboxPower);
  CenterControl(fnFW, cbboxCOM);
  CenterControl(btnUpdate, cbboxCOM);
  CenterControl(lblVSProgDir, dedtVSProg);
  CenterControl(lblOpenOCDDir, dedtOpenOCD);

  ControlLeft      := (pcMain.ActivePage.Width - gbOpenOCD.Width) div 2;
  gbOpenOCD.Left   := ControlLeft;
  gbSVFPlayer.Left := ControlLeft;
  gbPower.Left     := ControlLeft;
  gbChipName.Left  := ControlLeft;
  gbInputFile.Left := ControlLeft;
  gbOption.Left    := ControlLeft;
  gbOperation.Left := ControlLeft;
end;

procedure TFormMain.UpdateTitle();
var
  enable_str: string;
begin
  if pcMain.ActivePage.Enabled then
  begin
    enable_str := '';
  end
  else
  begin
    enable_str := '(*)';
  end;
  Caption := APP_STR + VERSION_STR + '-' + pcMain.ActivePage.Caption +
    enable_str + ' (' + VSProg_Version + ') ' + CurProgrammerInfo;
end;

function TFormMain.VSProg_RunAlgorithm(var caller: TCLI_Caller;
  parser: TParserFunc; result_num: integer; silent: boolean): boolean;
var
  strTmp: string;
begin
  Result := False;
  VSProg_Parser.Prepare();
  VSProg_Parser.ParserFunc := parser;
  tBlink.Enabled := True;
  caller.Run(@VSProg_Parser.CommonParser, False, True);
  tBlink.Enabled := False;
  VSProg_Parser.CallbackFunc    := nil;
  VSProg_Parser.LogOutputEnable := True;
  VSProg_Caller.Application     := '';

  if (VSProg_Parser.HasError) then
  begin
    shapeResult.Brush.Color := clRed;
    if not silent then
    begin
      Beep();
      strTmp := VSProg_TranslateError(VSProg_Parser.ErrorStr);
      if strTmp = '' then
      begin
        strTmp := VSProg_Parser.ErrorStr;
      end;
      MessageDlg('Error', strTmp, mtError, [mbOK], 0);
    end;
  end
  else if VSProg_Parser.ResultStrings.Count >= result_num then
  begin
    shapeResult.Brush.Color := clGreen;
    Result := True;
  end;
end;

procedure TFormMain.VSProg_LogOutput(var line: string);
begin
  memoLog.Lines.Add(line);
end;

procedure TFormMain.VSProg_LogProgress(ProgressInfo: TProgressInfoType; info: string);
begin
  case ProgressInfo of
    liStartSection:
    begin
      LogInfo(info);
      sbMain.Panels.Items[1].Text := '';
      pgbarMain.Position := 0;
    end;
    liStep:
    begin
      sbMain.Panels.Items[1].Text := sbMain.Panels.Items[1].Text + info;
      pgbarMain.StepIt;
    end;
    liEndSection:
    begin
      memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] :=
        memoInfo.Lines.Strings[memoInfo.Lines.Count - 1] + info;
    end;
  end;
end;

procedure TFormMain.VSProg_GUIUpdateStr(Str: String; var edt: TLabeledEdit);
begin
  edt.Text := Str;
end;

procedure TFormMain.VSProg_GUIUpdateULCS(format: String; buff: array of BYTE;
  var edt: TLabeledEdit);
var
  strTmp: String;
begin
  strTmp := '';
  if (Length(buff) > 0) and strparser_solve(strTmp, format, buff) then
  begin
    edt.Text := strTmp;
  end
  else
  begin
    edt.Text := '';
  end;
end;

procedure TFormMain.VSProg_GUIUpdateUsrSig(format: String; buff: array of BYTE);
begin
  VSProg_GUIUpdateULCS(format, buff, lbledtUsrSig);
end;

procedure TFormMain.VSProg_GUIUpdateCali(format: String; buff: array of BYTE);
begin
  VSProg_GUIUpdateULCS(format, buff, lbledtCali);
end;

procedure TFormMain.VSProg_GUIUpdateLock(format: String; buff: array of BYTE);
begin
  VSProg_GUIUpdateULCS(format, buff, lbledtLock);
end;

procedure TFormMain.VSProg_GUIUpdateFuse(format: String; buff: array of BYTE);
begin
  VSProg_GUIUpdateULCS(format, buff, lbledtFuse);
end;

 // Update Series Features
 // C: Commport, X: eXecute, F, Frequency
procedure TFormMain.VSProg_GUIFeatureInit(para: string; append: boolean);
var
  strTmp: string;
begin
  chkboxMP.Checked := False;
  chkboxMP.Enabled := (Pos('M', para) > 0);

  if Pos('F', para) > 0 then
  begin
    sedtFreq.Visible     := True;
    btnComSetup.Visible := False;
  end
  else
  begin
    sedtFreq.Visible := False;
  end;

  if Pos('C', para) > 0 then
  begin
    sedtFreq.Visible     := False;
    btnComSetup.Visible := True;

    // Set COMM Settings
    strTmp := CurTargetChip.ExtraStr;
    if strTmp <> '' then
    begin
      FormComSetup.ComInitPara(strTmp);
    end
    else
    begin
      strTmp := CurTargetSeries.SeriesInfo;
      if strTmp <> '' then
      begin
        FormComSetup.ComInitPara(strTmp);
      end;
    end;
  end
  else
  begin
    btnComSetup.Visible := False;
  end;

  lblKHz.Visible := sedtFreq.Visible;

  if Pos('X', para) > 0 then
  begin
    lbledtAddr.Enabled := True;
  end
  else if not append then
  begin
    lbledtAddr.Enabled := False;
    lbledtAddr.Text    := '';
  end;

  if Pos('A', para) > 0 then
  begin
    btnTargetDetect.Enabled := True;
  end
  else if not append then
  begin
    btnTargetDetect.Enabled := False;
  end;
end;

// Update Mode and correspong features
procedure TFormMain.VSProg_GUIModeInit;
var
  OrigMode: string;
  i: integer;
begin
  OrigMode := cbboxMode.Text;
  cbboxMode.Clear;

  for i := 1 to Length(CurTargetChip.Mode) do
  begin
    cbboxMode.Items.Add(CurTargetSeries.GetModeStrByModeChar(CurTargetChip.Mode[i]));
  end;

  if cbboxMode.Items.Count > 0 then
  begin
    i := ComboBoxGetIdx(cbboxMode, OrigMode);
    if i >= 0 then
    begin
      cbboxMode.ItemIndex := i;
    end
    else
    begin
      cbboxMode.ItemIndex := 0;
    end;
    cbboxMode.Enabled := True;
    cbboxModeChange(cbboxMode);
  end
  else
  begin
    cbboxMode.Enabled := False;
  end;
end;

// f: Flash, e: EEprom, l: Lock, u: Fuse, s: User Signature, c: Calibration Value
procedure TFormMain.VSProg_GUITargetAreaInit;
var
  index: integer;
  para:  string;
begin
  para := CurTargetChip.Areas;

  if Pos(FLASH_CHAR, para) > 0 then
  begin
    btnEditApp.Enabled := True;
    chkboxApp.Enabled  := True;
    //chkboxApp.Checked  := True;
    index := CurTargetChip.GetAreaIdx(FLASH_CHAR);
    if (index > 0) then
    begin
      SetLength(TargetAreaData.FlashData, CurTargetChip.TargetAreas[index].ByteLen);
      FillChar(TargetAreaData.FlashData[0], Length(TargetAreaData.FlashData),
        CurTargetChip.TargetAreas[index].DefaultValue);
    end;
  end
  else
  begin
    btnEditApp.Enabled := False;
    chkboxApp.Enabled  := False;
    chkboxApp.Checked  := False;
    SetLength(TargetAreaData.FlashData, 0);
  end;

  if Pos(EE_CHAR, para) > 0 then
  begin
    btnEditEE.Enabled := True;
    chkboxEE.Enabled  := True;
    //chkboxEE.Checked  := True;
    index := CurTargetChip.GetAreaIdx(EE_CHAR);
    if (index > 0) then
    begin
      SetLength(TargetAreaData.EEData, CurTargetChip.TargetAreas[index].ByteLen);
      FillChar(TargetAreaData.EEData[0], Length(TargetAreaData.EEData),
        CurTargetChip.TargetAreas[index].DefaultValue);
    end;
  end
  else
  begin
    btnEditEE.Enabled := False;
    chkboxEE.Enabled  := False;
    chkboxEE.Checked  := False;
    SetLength(TargetAreaData.EEData, 0);
  end;

  lbledtLock.Text    := '';
  lbledtLock.Enabled := False;
  if Pos(LOCK_CHAR, para) > 0 then
  begin
    btnEditLock.Enabled := True;
    chkboxLock.Enabled := True;
    //chkboxLock.Checked := True;
    index := CurTargetChip.GetAreaIdx(LOCK_CHAR);
    if index >= 0 then
    begin
      SetLength(TargetAreaData.LockData, CurTargetChip.TargetAreas[index].ByteLen);
      FillChar(TargetAreaData.LockData[0], Length(TargetAreaData.LockData),
        CurTargetChip.TargetAreas[index].DefaultValue);
      if not CurTargetChip.TargetAreas[index].InFile then
      begin
        lbledtLock.Enabled := True;
        VSProg_GUIUpdateLock(CurTargetChip.TargetAreas[index].Format,
          TargetAreaData.LockData);
      end;
    end;
  end
  else
  begin
    btnEditLock.Enabled := False;
    chkboxLock.Enabled  := False;
    chkboxLock.Checked  := False;
    SetLength(TargetAreaData.LockData, 0);
  end;

  lbledtFuse.Text    := '';
  lbledtFuse.Enabled := False;
  if Pos(FUSE_CHAR, para) > 0 then
  begin
    btnEditFuse.Enabled := True;
    chkboxFuse.Enabled := True;
    //chkboxFuse.Checked := True;
    index := CurTargetChip.GetAreaIdx(FUSE_CHAR);
    if index >= 0 then
    begin
      SetLength(TargetAreaData.FuseData, CurTargetChip.TargetAreas[index].ByteLen);
      FillChar(TargetAreaData.FuseData[0], Length(TargetAreaData.FuseData),
        CurTargetChip.TargetAreas[index].DefaultValue);
      if not CurTargetChip.TargetAreas[index].InFile then
      begin
        lbledtFuse.Enabled := True;
        VSProg_GUIUpdateFuse(CurTargetChip.TargetAreas[index].Format,
          TargetAreaData.FuseData);
      end;
    end;
  end
  else
  begin
    btnEditFuse.Enabled := False;
    chkboxFuse.Enabled  := False;
    chkboxFuse.Checked  := False;
    SetLength(TargetAreaData.FuseData, 0);
  end;

  lbledtUsrSig.Text    := '';
  lbledtUsrSig.Enabled := False;
  if Pos(USRSIG_CHAR, para) > 0 then
  begin
    btnEditUsrSig.Enabled := True;
    chkboxUsrSig.Enabled := True;
    //chkboxUsrSig.Checked := True;
    index := CurTargetChip.GetAreaIdx(USRSIG_CHAR);
    if index >= 0 then
    begin
      SetLength(TargetAreaData.UsrsigData, CurTargetChip.TargetAreas[index].ByteLen);
      FillChar(TargetAreaData.UsrsigData[0], Length(TargetAreaData.UsrsigData),
        CurTargetChip.TargetAreas[index].DefaultValue);
      if not CurTargetChip.TargetAreas[index].InFile then
      begin
        lbledtUsrSig.Enabled := True;
        VSProg_GUIUpdateUsrSig(CurTargetChip.TargetAreas[index].Format,
          TargetAreaData.UsrsigData);
      end;
    end;
  end
  else
  begin
    btnEditUsrSig.Enabled := False;
    chkboxUsrSig.Enabled  := False;
    chkboxUsrSig.Checked  := False;
    SetLength(TargetAreaData.UsrsigData, 0);
  end;

  lbledtCali.Text    := '';
  lbledtCali.Enabled := False;
  if Pos(CALI_CHAR, para) > 0 then
  begin
    btnEditCali.Enabled := True;
    chkboxCali.Enabled := True;
    //chkboxCali.Checked := True;
    index := CurTargetChip.GetAreaIdx(CALI_CHAR);
    if index >= 0 then
    begin
      SetLength(TargetAreaData.CaliData, CurTargetChip.TargetAreas[index].ByteLen);
      FillChar(TargetAreaData.CaliData[0], Length(TargetAreaData.CaliData),
        CurTargetChip.TargetAreas[index].DefaultValue);
      if not CurTargetChip.TargetAreas[index].InFile then
      begin
        lbledtCali.Enabled := True;
        VSProg_GUIUpdateCali(CurTargetChip.TargetAreas[index].Format,
          TargetAreaData.CaliData);
      end;
    end;
  end
  else
  begin
    btnEditCali.Enabled := False;
    chkboxCali.Enabled  := False;
    chkboxCali.Checked  := False;
    SetLength(TargetAreaData.CaliData, 0);
  end;

  TargetAreaData.StrData:= '';
  lbledtSpecialStr.Text := '';
  lbledtSpecialStr.Enabled := False;
  if Pos(SPECIALSTR_CHAR, para) > 0 then
  begin
    btnEditSpecialStr.Enabled := True;
    chkboxSpecialStr.Enabled := True;
    //chkboxSpecialStr.Checked := True;
    index := CurTargetChip.GetAreaIdx(SPECIALSTR_CHAR);
    if (index >= 0) and (not CurTargetChip.TargetAreas[index].InFile) then
    begin
      lbledtSpecialStr.Enabled := True;
    end;
  end
  else
  begin
    btnEditSpecialStr.Enabled := False;
    chkboxSpecialStr.Enabled := False;
    chkboxSpecialStr.Checked := False;
  end;
end;

function TFormMain.VSProg_GUITargetSeriesPageInit(index: integer): boolean;
var
  i: integer;
begin
  Result := True;

  VSProg_GUIFeatureInit(VSProg_Targets.TargetSeries[index].Feature, False);

  cbboxTarget.Clear;
  for i := 0 to VSProg_Targets.TargetSeries[index].ChipCount - 1 do
  begin
    cbboxTarget.Items.Add(VSProg_Targets.TargetSeries[index].TargetChips[i].Name);
  end;

  if (PreviousTargetIndex >= 0) and (PreviousTargetIndex < cbboxTarget.Items.Count) then
  begin
    cbboxTarget.ItemIndex := PreviousTargetIndex;
  end
  else
  begin
    cbboxTarget.ItemIndex := 0;
  end;

  gbChipName.Parent  := pcMain.ActivePage;
  gbInputFile.Parent := pcMain.ActivePage;
  gbOption.Parent    := pcMain.ActivePage;
  gbOperation.Parent := pcMain.ActivePage;

  cbboxTargetChange(cbboxTarget);
end;

procedure TFormMain.UpdateComboTargetFile();
var
  valid_file_num, i: integer;
begin
  valid_file_num := 0;
  cbboxInputFile.Clear;
  for i := low(TargetFile) to high(TargetFile) do
  begin
    if TargetFile[i].filename <> '' then
    begin
      cbboxInputFile.Items.Add(GetAreaFullName(TargetFile[i].target) +
        ':' + TargetFile[i].filename);
      Inc(valid_file_num);
    end;
  end;
  if valid_file_num > 1 then
  begin
    cbboxInputFile.Items.Insert(0, 'ALL');
  end;
  cbboxInputFile.ItemIndex := 0;
  cbboxInputFileChange(cbboxInputFile);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
  str_tmp: string;
  ser: TBlockSerial;
begin
  xmlcfgMain.FileName := GetUserDir + 'vsgui.xml';

  ProgrammerParameter := '';
  CurProgrammerInfo := 'N.C.';
  JTAGPage_Init   := True;
  TargetPage_Init := True;
  AboutPage_Init  := True;

  VSProg_Taken_By_Polling := False;
  bOpenFileOK    := False;
  bReadOperation := False;
  Page_Init      := False;

  // caller init
  VSProg_Caller := TCLI_Caller.Create();
  VSProg_Caller.Delimiter := '-';

  // parser init
  VSProg_Parser := TVSProg_Parser.Create;
  VSProg_Parser.LogProgressFunc := @VSProg_LogProgress;
  VSProg_Parser.LogOutputFunc := @VSProg_LogOutput;
  VSProg_Parser.CallbackFunc := nil;

  // target init
  VSProg_Targets := TVSProg_Targets.Create;

  // poll thread init
  if POLLTHREAD_EN then
  begin
    PollThread := TPollThread.Create;
    if Assigned(PollThread.FatalException) then
    begin
      raise PollThread.FatalException;
    end;
  end;

  // TrayIcon
  tiMain.Icon      := Application.Icon;
  tiMain.PopUpMenu := pmTray;
  tiMain.Show;

  // locate executables(vsprog and openocd)
  dedtVSProg.Directory  := xmlcfgMain.GetValue('vsprog_dir', Application.Location);
  dedtOpenOCD.Directory := xmlcfgMain.GetValue('openocd_dir', Application.Location);
  dedtPathChange(dedtVSProg);
  dedtPathChange(dedtOpenOCD);
  if not VSProg_Exists then
  begin
    Beep();
    MessageDlg('Error, missing vsprog',
      'Opps, Where is my vsprog? I cannot work without her.', mtError, [mbOK], 0);
  end;

  // get VSProg_Version
  VSProg_Version := '';
  if not VSProg_PrepareToRunCLI then
  begin
    VSProg_Version := 'No Exists';
  end
  else
  begin
    VSProg_Caller.AddParametersString('--version');
    VSProg_Parser.LogOutputEnable := False;
    if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.VersionParser, 1, False) then
    begin
      VSProg_Version := VSProg_Parser.ResultStrings.Strings[0];
    end;
  end;
  UpdateTitle();

  // get VSProg_Targets
  if VSProg_PrepareToRunCLI then
  begin
    VSProg_Caller.AddParameter('Starget');
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Parser.CallbackFunc    := @VSProg_Targets.TargetParser;
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.TargetParser, 0, True);
  end;
  // Create Items in cbboxTargetType
  cbboxTargetType.Items.Clear;
  for i := 0 to VSProg_Targets.SeriesCount - 1 do
  begin
    if Pos('svf', VSProg_Targets.TargetSeries[i].Name) = 0 then
    begin
      cbboxTargetType.Items.Add(UpperCase(VSProg_Targets.TargetSeries[i].Name));
    end;
  end;
  Page_Init := True;

  // Init COM
  cbboxCOM.Clear;
  for i := low(COMPORTS) to high(COMPORTS) do
  begin
    if DISPLAY_ALL_COMPORT_WHEN_UPDATE then
    begin
      cbboxCOM.Items.Add(COMPORTS[i]);
    end
    else
    begin
      ser := TBlockSerial.Create;
      try
        ser.Connect(COMPORTS[i]);
        if ser.LastError = 0 then
        begin
          cbboxCOM.Items.Add(COMPORTS[i]);
        end;
      finally
        ser.Free;
      end;
    end;
  end;
  if cbboxCOM.Items.Count > 0 then
  begin
    cbboxCOM.ItemIndex := 0;
  end;

  // load last settings
  str_tmp := xmlcfgMain.GetValue('activepage', 'About');
  for i := 0 to pcMain.PageCount - 1 do
  begin
    if pcMain.Page[i].Caption = str_tmp then
    begin
      pcMain.ActivePageIndex := i;
      break;
    end;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  VSProg_Caller.Stop();
  VSProg_Caller.Destroy;
  VSProg_Parser.Destroy;
  VSProg_Targets.Destroy;

  SetLength(TargetAreaData.FlashData, 0);
  SetLength(TargetAreaData.EEData, 0);
  SetLength(TargetAreaData.FuseData, 0);
  SetLength(TargetAreaData.LockData, 0);
  SetLength(TargetAreaData.CaliData, 0);
  SetLength(TargetAreaData.UsrsigData, 0);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pgbarMain.Height := sbMain.Height - 1;
  pgbarMain.Top    := sbMain.Top + 1;
  pgbarMain.Left   := sbMain.Panels.Items[0].Width + 2;
  pgbarMain.Width  := Width - sbMain.Panels.Items[0].Width - 2;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  // get existing programmer
  if VSProg_PrepareToRunCLI then
  begin
    VSProg_Caller.AddParameter('L');
    VSProg_Parser.LogOutputEnable := False;
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Programmer.ProgrammerParser, 0, True);
  end;
  if VSProg_Programmer.ProgrammerCount > 1 then
  begin
    VSProg_Programmer.ShowModal;
  end;
  ProgrammerParameter := VSProg_Programmer.SerialNumber;

  if POLLTHREAD_EN and VSProg_Exists and (PollThread <> nil) then
  begin
    PollThread.AppPath := dedtVSProg.Directory + VSPROG_STR;
    PollThread.Resume;
  end;

  FormResize(Sender);
  FormMain.Width := pnlMain.Width + LOGMEMO_WIDTH + 2;
  memoLog.Width  := LOGMEMO_WIDTH;

  // Load Setting
  JTAGPage_Init   := False;
  TargetPage_Init := False;
  AboutPage_Init  := False;
  pcMainPageChanged(pcMain);
  UpdateShowing;

  tInit.Enabled := True;
end;

procedure TFormMain.lbledtExtraParaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Pos('show debug log', lbledtExtraPara.Text) = 1 then
    begin
      ShowDebugLog();
      lbledtExtraPara.Text := '';
    end
    else if lbledtExtraPara.Text = 'exit' then
    begin
      Close;
    end;

    Key := #0;
  end;
end;

procedure TFormMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.pcMainChanging(Sender: TObject; var AllowChange: boolean);
begin
  AllowChange := (VSProg_Caller <> nil) and ((not VSProg_Caller.IsRunning) or
    VSProg_Taken_By_Polling);
end;

procedure TFormMain.ShowDebugLog();
begin
  Width := pnlMain.Width + LOGMEMO_WIDTH + 2;
  memoLog.AdjustSize;
end;

procedure TFormMain.HideDebugLog();
begin
  if not DEBUG_LOG_SHOW then
  begin
    Width := pnlMain.Width + 1;
  end;
end;

procedure TFormMain.pcMainPageChanged(Sender: TObject);
var
  strTmp:  string;
  intTmp:  integer;
  success: boolean;
  i, j:    integer;
begin
  if not Page_Init then
  begin
    exit;
  end;
  if (not pcMain.ActivePage.Enabled) or (VSProg_Caller.IsRunning and
    (not VSProg_Taken_By_Polling)) then
  begin
    UpdateTitle();
    exit;
  end;

  memoInfo.Clear;
  memoLog.Clear;

  // initialize GUI
  if pcMain.ActivePage = tsBatchProgrammer then
  begin
    HideDebugLog();

    btnBatchProgram.Enabled := VSProg_Exists;
  end
  else if pcMain.ActivePage = tsAbout then
  begin
    HideDebugLog();

    gbUpdate.Enabled := VSProg_Exists;

    // update settings
    if not AboutPage_Init then
    begin
      AboutPage_Init := True;
      fnFW.FileName  := xmlcfgMain.GetValue('fw/filename', '');
      ComboBoxSetText(cbboxCOM, xmlcfgMain.GetValue('fw/comm', ''));
    end;
  end
  else if pcMain.ActivePage = tsJTAG then
  begin
    ShowDebugLog();
    OpenOCD_Init();

    gbSVFPlayer.Enabled := VSprog_Exists;
    gbPower.Enabled     := VSprog_Exists;
    gbOpenOCD.Enabled   := OpenOCD_Exists;

    // update settings
    if not JTAGPage_Init then
    begin
      JTAGPage_Init := True;
      ComboBoxSetText(cbboxOpenOCDInterface, xmlcfgMain.GetValue(
        'openocd/interface', ''));
      ComboBoxSetText(cbboxOpenOCDTarget, xmlcfgMain.GetValue('openocd/target', ''));
      ComboBoxSetText(cbboxOpenOCDScript, xmlcfgMain.GetValue('openocd/script', ''));
      edtOpenOCDOption.Text := xmlcfgMain.GetValue('openocd/option', '');
      fneditSVFFile.FileName := xmlcfgMain.GetValue('svf/filename', '');
      edtSVFOption.Text := xmlcfgMain.GetValue('svf/option', '');
      ComboBoxSetText(cbboxPower, xmlcfgMain.GetValue('power/voltage', '0'));
    end;
  end
  else if pcMain.ActivePage = tsVSProg then
  begin
    HideDebugLog();
    if cbboxTargetType.Items.Count > 0 then
    begin
      if cbboxTargetType.ItemIndex < 0 then
      begin
        cbboxTargetType.ItemIndex := 0;
      end;
      cbboxTargetTypeChange(cbboxTargetType);

      if not TargetPage_Init then
      begin
        TargetPage_Init := True;
        strTmp  := xmlcfgMain.GetValue('target/series', '');
        ComboBoxSetText(cbBoxTargetType, UpperCase(strTmp));
        cbboxTargetTypeChange(cbboxTargetType);
        strTmp  := xmlcfgMain.GetValue('target/chip', '');
        success := False;
        for i := 0 to cbboxTarget.Items.Count - 1 do
        begin
          if strTmp = cbboxTarget.Items.Strings[i] then
          begin
            success := True;
            break;
          end;
        end;
        if success then
        begin
          ComboBoxSetText(cbboxTarget, xmlcfgMain.GetValue('target/chip', ''));
          cbboxTargetChange(cbboxTarget);
          if cbboxMode.Items.Count > 0 then
          begin
            ComboBoxSetText(cbboxMode, xmlcfgMain.GetValue('target/mode', ''));
            cbboxModeChange(cbboxMode);
          end;
          sedtFreq.Value := xmlcfgMain.GetValue('target/freq', 1000);
          if lbledtAddr.Enabled then
          begin
            lbledtAddr.Text := xmlcfgMain.GetValue('target/exe_addr', '');
          end;
          intTmp := xmlcfgMain.GetValue('target/files/number', 0);
          if Length(TargetFile) < intTmp then
          begin
            // Error here, try to recovery
            xmlcfgMain.DeletePath('target/files');
            xmlcfgMain.SetValue('target/files/number', 0);
            intTmp := 0;
          end;
          if intTmp > 0 then
          begin
            for i := 0 to intTmp - 1 do
            begin
              j := GetTargetFileIdx(xmlcfgMain.GetValue('target/files/' +
                IntToStr(i) + '/target', '')[1]);
              if j < 0 then
              begin
                // Error here, try to recovery
                xmlcfgMain.DeletePath('target/files');
                xmlcfgMain.SetValue('target/files/number', 0);
                intTmp := 0;
                break;
              end;
              TargetFile[j].filename :=
                xmlcfgMain.GetValue('target/files/' + IntToStr(i) + '/filename', '');
            end;
          end;
          UpdateComboTargetFile();
          ComboBoxSetText(cbboxInputFile, xmlcfgMain.GetValue('target/filename', ''));
          if lbledtFuse.Enabled then
          begin
            lbledtFuse.Text := xmlcfgMain.GetValue('target/fuse', '');
          end;
          if lbledtLock.Enabled then
          begin
            lbledtLock.Text := xmlcfgMain.GetValue('target/lock', '');
          end;
          if lbledtCali.Enabled then
          begin
            lbledtCali.Text := xmlcfgMain.GetValue('target/cali', '');
          end;
          if lbledtUsrSig.Enabled then
          begin
            lbledtUsrSig.Text := xmlcfgMain.GetValue('target/usrsig', '');
          end;
          if lbledtSpecialStr.Enabled then
          begin
            lbledtSpecialStr.Text := xmlcfgMain.GetValue('target/special_str', '')
          end;
          if chkboxNoconnect.Enabled then
          begin
            chkboxNoconnect.Checked := xmlcfgMain.GetValue('target/nc', False);
          end;
          if chkboxNowarning.Enabled then
          begin
            chkboxNowarning.Checked := xmlcfgMain.GetValue('target/nw', False);
          end;
          if chkboxApp.Enabled then
          begin
            chkboxApp.Checked := xmlcfgMain.GetValue('target/flashen', False);
          end;
          if chkboxEE.Enabled then
          begin
            chkboxEE.Checked := xmlcfgMain.GetValue('target/eepromen', False);
          end;
          if chkboxFuse.Enabled then
          begin
            chkboxFuse.Checked := xmlcfgMain.GetValue('target/fuseen', False);
          end;
          if chkboxLock.Enabled then
          begin
            chkboxLock.Checked := xmlcfgMain.GetValue('target/locken', False);
          end;
          if chkboxUsrSig.Enabled then
          begin
            chkboxUsrSig.Checked := xmlcfgMain.GetValue('target/usrsigen', False);
          end;
          if chkboxCali.Enabled then
          begin
            chkboxCali.Checked := xmlcfgMain.GetValue('target/calien', False);
          end;
          if chkboxSpecialStr.Enabled then
          begin
            chkboxSpecialStr.Checked := xmlcfgMain.GetValue('target/special_stren', False);
          end;
          if chkboxMP.Enabled then
          begin
            chkboxMP.Checked := xmlcfgMain.GetValue('target/mass', False);
          end;
          chkboxEraseBeforeWrite.Checked := xmlcfgMain.GetValue('target/ebw', False);
          chkboxVerifyAfterWrite.Checked := xmlcfgMain.GetValue('target/vaw', False);
          lbledtExtraPara.Text := xmlcfgMain.GetValue('target/extraparam', '');
          ComMode.comstr := xmlcfgMain.GetValue('commode/com', '');
          ComMode.baudrate := xmlcfgMain.GetValue('commode/baudrate', DEFAULT_BAUDRATE);
          ComMode.datalength := xmlcfgMain.GetValue('commode/datalength', DEFAULT_DATALENGTH);
          strTmp := xmlcfgMain.GetValue('commode/parity', DEFAULT_PARITYBIT_CHAR);
          ComMode.paritybit := strTmp[1];
          strTmp := xmlcfgMain.GetValue('commode/stop', DEFAULT_STOPBIT_CHAR);
          ComMode.stopbit := strTmp[1];
          strTmp := xmlcfgMain.GetValue('commode/handshake', DEFAULT_HANDSHAKE_CHAR);
          ComMode.handshake := strTmp[1];
          strTmp := xmlcfgMain.GetValue('commode/aux', DEFAULT_AUXPIN_CHAR);
          ComMode.auxpin := strTmp[1];
          FormComSetup.ComSetPara(ComMode);
        end;
      end;
    end
    else
    begin
      pcMain.ActivePage.Enabled := False;
    end;
  end;
  UpdateTitle();
end;

procedure TFormMain.sedtFreqEditingDone(Sender: TObject);
begin
  if sedtFreq.Text = '' then
  begin
    sedtFreq.Value := 0;
  end;
end;

procedure TFormMain.tBlinkTimer(Sender: TObject);
begin
  if shapeResult.Brush.Color <> clYellow then
  begin
    shapeResult.Brush.Color := clYellow;
  end
  else
  begin
    shapeResult.Brush.Color := clWhite;
  end;
end;

procedure TFormMain.btnTargetDetectClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_Caller.AddParameter('s' + cbboxTarget.Items.Strings[0]);
  VSProg_PrepareMiscParameters(VSProg_Caller);
  VSProg_Caller.AddParameter('oi');

  LogInfo('Running...');
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.AutoDetectParser, 1, False) then
  begin
    ComboBoxSetText(cbboxTarget, VSProg_Parser.ResultStrings.Strings[0]);
    cbboxTargetChange(cbboxTarget);
  end;
  LogInfo('Idle');
end;

procedure TFormMain.btnUpdateClick(Sender: TObject);
begin
  if cbboxCOM.Text = '' then
  begin
    Beep();
    MessageDlg('Error, no comm port found?', '......', mtError, [mbOK], 0);
    exit;
  end;
  if fnFW.FileName = '' then
  begin
    Beep();
    MessageDlg('Error', 'Please sellect FW Hex file.', mtError, [mbOK], 0);
    exit;
  end;

  if not VSProg_PrepareToRunCLI then
  begin
    Beep;
    MessageDlg('Error', 'Fail to run.', mtError, [mbOK], 0);
    exit;
  end;

  VSProg_Caller.AddParametersString('-G -Z -sstm32 -mi -C' +
    cbboxCOM.Text + ' ' + ' -x0x08002000 -oe -owf -I"' + fnFW.FileName + '"');
  LogInfo('Running...');
  if VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False) then
  begin
    MessageDlg('OK', 'FW Updated OK.', mtInformation, [mbOK], 0);
  end;
  LogInfo('Idle');
end;

function TFormMain.VSProg_GetTargetDefineParameters(): string;
begin
  if cbboxTarget.ItemIndex = 0 then
  begin
    Result := 's';
  end
  else
  begin
    Result := 'c';
  end;
  Result := Result + CurTargetChip.Name;
end;

function TFormMain.VSProg_GetEnabledOperationString(): string;
begin
  Result := '';
  if chkboxApp.Enabled and chkboxApp.Checked then
  begin
    Result := Result + FLASH_CHAR;
  end;
  if chkboxEE.Enabled and chkboxEE.Checked then
  begin
    Result := Result + EE_CHAR;
  end;
  if chkboxFuse.Enabled and chkboxFuse.Checked then
  begin
    Result := Result + FUSE_CHAR;
  end;
  if chkboxLock.Enabled and chkboxLock.Checked then
  begin
    Result := Result + LOCK_CHAR;
  end;
  if chkboxUsrSig.Enabled and chkboxUsrSig.Checked then
  begin
    Result := Result + USRSIG_CHAR;
  end;
  if chkboxCali.Enabled and chkboxCali.Checked then
  begin
    Result := Result + CALI_CHAR;
  end;
  if chkboxSpecialStr.Enabled and chkboxSpecialStr.Checked then
  begin
    Result := Result + SPECIALSTR_CHAR;
  end;
end;

function TFormMain.VSProg_AddEraseOperation(): boolean;
var
  para: string;
begin
  Result := False;
  para   := VSProg_GetEnabledOperationString();
  if para <> '' then
  begin
    Result := True;
    para   := 'oe' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
    end;
  end;
end;

function TFormMain.VSProg_AddWriteOperation(): boolean;
var
  para: string;
begin
  Result := False;
  para   := VSProg_GetEnabledOperationString();
  if para <> '' then
  begin
    Result := True;
    para   := 'ow' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
    end;
  end;
end;

function TFormMain.VSProg_AddVerifyOperation(): boolean;
var
  para: string;
begin
  Result := True;
  para   := VSProg_GetEnabledOperationString();
  if para <> '' then
  begin
    Result := True;
    para   := 'ov' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
    end;
  end;
end;

function TFormMain.VSProg_AddReadOperation(): boolean;
var
  para: string;
begin
  Result := True;
  para   := VSProg_GetEnabledOperationString();
  if para <> '' then
  begin
    Result := True;
    para   := 'or' + para;
    if Result then
    begin
      VSProg_Caller.AddParameter(para);
    end;
  end;
end;

procedure TFormMain.btnVerifyClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_PrepareOperationParameters(VSProg_Caller);
  if not VSProg_AddVerifyOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnEraseClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_PrepareOperationParameters(VSProg_Caller);
  if not VSProg_AddEraseOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add erase operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnComSetupClick(Sender: TObject);
begin
  FormComSetup.ShowModal;
  FormComSetup.GetComMode(ComMode);
end;

procedure TFormMain.btnOpenFileClick(Sender: TObject);
var
  file_num: integer;
begin
  bOpenFileOK := False;
  file_num    := Length(TargetFile);
  if file_num = 0 then
  begin
    exit;
  end
  else if file_num > 1 then
  begin
    FormFileSelector.Reset;
    if mrOk = FormFileSelector.ShowModal then
    begin
      UpdateComboTargetFile();
      bOpenFileOK := True;
    end;
  end
  else
  begin
    odInputFile.FileName := TargetFile[0].filename;
    if odInputFile.Execute then
    begin
      TargetFile[0].filename := odInputFile.FileName;
      UpdateComboTargetFile();
      bOpenFileOK := True;
    end;
  end;
end;

procedure TFormMain.btnOpenOCDRunClick(Sender: TObject);
var
  strTmp: string;
begin
  if not VSProg_PrepareToRunOpenOCD then
  begin
    exit;
  end;

  if edtOpenOCDOption.Text <> '' then
  begin
    VSProg_Caller.AddParametersString(edtOpenOCDOption.Text);
  end;
  if cbboxOpenOCDInterface.Text <> '' then
  begin
    strTmp := dedtOpenOCD.Directory + cbboxOpenOCDInterface.Text;
    strTmp := StringReplace(strTmp,'\','/',[rfReplaceAll]);
    VSProg_Caller.AddParameter('f "' + strTmp + '"');
  end;
  if cbboxOpenOCDTarget.Text <> '' then
  begin
    strTmp := dedtOpenOCD.Directory +cbboxOpenOCDTarget.Text;
    strTmp := StringReplace(strTmp,'\','/',[rfReplaceAll]);
    VSProg_Caller.AddParameter('f "' + strTmp + '"');
  end;
  if cbboxOpenOCDScript.Text <> '' then
  begin
    strTmp := dedtOpenOCD.Directory +cbboxOpenOCDScript.Text;
    strTmp := StringReplace(strTmp,'\','/',[rfReplaceAll]);
    VSProg_Caller.AddParameter('f "' + strTmp + '"');
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnOpenOCDStopClick(Sender: TObject);
begin
  if (VSProg_Caller <> nil) and (VSProg_Caller.Application =
    dedtOpenOCD.Directory + OPENOCD_STR) then
  begin
    VSProg_Caller.Stop();
  end;
end;

procedure TFormMain.btnBatchProgramClick(Sender: TObject);
begin
  if BATCH_PROGRAM_CMDLINE <> '' then
  begin
    if not VSProg_PrepareToRunCLI then
    begin
      exit;
    end;

    VSProg_Caller.AddParametersString(BATCH_PROGRAM_CMDLINE);
    LogInfo('Running...');
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
    LogInfo('Idle');
  end
  else
  begin
    btnWrite.Click;
  end;
end;

procedure TFormMain.ShowTargetArea(AreaName: char; var Sender: TObject;
  parser: TParserFunc; var buffer: array of BYTE);
var
  areaidx, fileidx: integer;
  targetdefined: string;
  format: string;
  strValue: string;
begin
  areaidx := CurTargetChip.GetAreaIdx(AreaName);
  if areaidx < 0 then
  begin
    exit;
  end;

  if CurTargetChip.TargetAreas[areaidx].InFile then
  begin
    fileidx := GetTargetFileIdx(AreaName);
    if (fileidx < 0) or (TargetFile[fileidx].filename = '') then
    begin
      Beep();
      MessageDlg('Error', 'No File Defined.', mtError, [mbOK], 0);
      exit;
    end;
    if not FileExistsUtf8(TargetFile[fileidx].filename) then
    begin
      Beep();
      MessageDlg('Error', '"' + TargetFile[fileidx].filename + '" not exists.',
        mtError, [mbOK], 0);
      exit;
    end;

    FormHexEditor.FileName := TargetFile[fileidx].filename;
    FormHexEditor.StartAddress := CurTargetChip.GetArea(AreaName).StartAddr;
    FormHexEditor.DataByteSize := CurTargetChip.GetArea(AreaName).ByteLen;
    FormHexEditor.DefaultData := CurTargetChip.GetArea(AreaName).DefaultValue;
    FormHexEditor.SegAddr := CurTargetChip.GetArea(AreaName).SegAddr;
    FormHexEditor.AddressOffset := 0;
    FormHexEditor.Target := GetAreaFullName(AreaName);

    FormHexEditor.ShowModal;
  end
  else
  begin
    if not (Sender is TLabeledEdit) then
    begin
      exit;
    end;
    targetdefined := VSProg_GetTargetDefineParameters();
    if targetdefined[1] = 's' then
    begin
      Beep();
      MessageDlg('Error', 'Please select a target chip.', mtError, [mbOK], 0);
      exit;
    end;

    format  := CurTargetChip.TargetAreas[areaidx].Format;

    if not chkboxNoconnect.Checked then
    begin
      // call 'vsprog -or' to read settings from target
      if not VSProg_PrepareToRunCLI then
      begin
        exit;
      end;
      VSProg_PrepareBaseParameters(VSProg_Caller);
      VSProg_Caller.AddParameter('or' + AreaName);
      if not VSProg_RunAlgorithm(VSProg_Caller, parser, 1, False) then
      begin
        exit;
      end;
      strValue := '';
      if AreaName = SPECIALSTR_CHAR then
      begin
        VSProg_GUIUpdateStr(VSProg_Parser.ResultStrings.Strings[0],TLabeledEdit(Sender));
      end
      else if strparser_parse(VSProg_Parser.ResultStrings.Strings[0], format,
        buffer) and strparser_solve(strValue, format, buffer) then
      begin
        TLabeledEdit(Sender).Text := strValue;
      end
      else
      begin
        // invalid data read
        Beep();
        MessageDlg('Error', 'Invalid data: ' + VSProg_Parser.ResultStrings.Strings[0],
          mtError, [mbOK], 0);
        exit;
      end;
    end;

    // Special_String is shown in the FormMain
    // No need to call FormParaEditor
    if AreaName = SPECIALSTR_CHAR then
    begin
      exit;
    end;

    FormParaEditor.SetParameter(buffer, format, GetAreaFullName(AreaName),
      not chkboxNowarning.Checked);

    // call 'vsprog -Ppara' to extract para settings
    if not VSProg_PrepareToRunCLI then
    begin
      exit;
    end;
    VSProg_PrepareBaseParameters(VSProg_Caller);
    VSProg_Caller.AddParameter('P' + AreaName);
    FormParaEditor.FreeRecord();
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Parser.CallbackFunc    := @VSProg_SettingTargetParserCallback;
    if not VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.SettingTargetInfoParser,
      0, False) then
    begin
      exit;
    end;

    if mrOk = FormParaEditor.ShowModal then
    begin
      // OK clicked, get value
      FormParaEditor.GetResult(buffer);
      VSProg_GUIUpdateULCS(format, buffer, TLabeledEdit(Sender));
    end;
  end;
end;

procedure TFormMain.btnEditAppClick(Sender: TObject);
begin
  ShowTargetArea(FLASH_CHAR, Sender, nil, TargetAreaData.FlashData);
end;

procedure TFormMain.btnEditCaliClick(Sender: TObject);
begin
  ShowTargetArea(CALI_CHAR, TObject(lbledtCali), @VSProg_Parser.CaliDataParser,
    TargetAreaData.CaliData);
end;

procedure TFormMain.btnEditEEClick(Sender: TObject);
begin
  ShowTargetArea(EE_CHAR, Sender, nil, TargetAreaData.EEData);
end;

procedure TFormMain.btnEditFuseClick(Sender: TObject);
begin
  ShowTargetArea(FUSE_CHAR, TObject(lbledtFuse), @VSProg_Parser.FuseDataParser,
    TargetAreaData.FuseData);
end;

procedure TFormMain.btnEditInterfaceClick(Sender: TObject);
begin
  TextFileName := dedtOpenOCD.Directory + cbboxOpenOCDInterface.Text;
  FormTextEditor.ShowModal;
end;

procedure TFormMain.btnEditLockClick(Sender: TObject);
begin
  ShowTargetArea(LOCK_CHAR, TObject(lbledtLock), @VSProg_Parser.LockDataParser,
    TargetAreaData.LockData);
end;

procedure TFormMain.btnEditScriptClick(Sender: TObject);
begin
  TextFileName := dedtOpenOCD.Directory + cbboxOpenOCDScript.Text;
  FormTextEditor.ShowModal;
end;

procedure TFormMain.btnEditTargetClick(Sender: TObject);
begin
  TextFileName := dedtOpenOCD.Directory + cbboxOpenOCDTarget.Text;
  FormTextEditor.ShowModal;
end;

procedure TFormMain.btnEditUsrSigClick(Sender: TObject);
begin
  ShowTargetArea(USRSIG_CHAR, TObject(lbledtUsrsig), @VSProg_Parser.UsrsigDataParser,
    TargetAreaData.UsrsigData);
end;

procedure TFormMain.btnReadClick(Sender: TObject);
begin
  // select output file
  btnOpenFile.Click;
  if not bOpenFileOK then
  begin
    exit;
  end;

  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;
  bReadOperation := True;
  VSProg_PrepareOperationParameters(VSProg_Caller);
  if not VSProg_AddReadOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add read operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    bReadOperation := False;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
  bReadOperation := False;
end;

procedure TFormMain.btnSetPowerClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_Caller.AddParameter('V"tvcc.set ' + cbboxPower.Text + '"');
  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, nil, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.btnEditSpecialStrClick(Sender: TObject);
begin
  ShowTargetArea(SPECIALSTR_CHAR, TObject(lbledtSpecialStr),
    @VSProg_Parser.SpecialStrParser, TargetAreaData.FuseData);
end;

procedure TFormMain.btnSVFRunClick(Sender: TObject);
begin
  if fneditSVFFile.FileName <> '' then
  begin
    if not VSProg_PrepareToRunCLI then
    begin
      exit;
    end;
    VSProg_Caller.AddParametersString('-G -ssvf_player -I"' +
      fneditSVFFile.FileName + '"');
    if edtSVFOption.Text <> '' then
    begin
      VSProg_Caller.AddParametersString(edtSVFOption.Text);
    end;

    LogInfo('Running...');
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
    LogInfo('Idle');
  end
  else
  begin
    Beep();
    MessageDlg('Error', 'Please input SVF file.', mtError, [mbOK], 0);
  end;
end;

procedure TFormMain.btnWriteClick(Sender: TObject);
begin
  if not VSProg_PrepareToRunCLI then
  begin
    exit;
  end;

  VSProg_PrepareOperationParameters(VSProg_Caller);

  if chkboxEraseBeforeWrite.Checked and not VSProg_AddEraseOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add erase operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  if not VSProg_AddWriteOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add write operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  if chkboxVerifyAfterWrite.Checked and not VSProg_AddVerifyOperation() then
  begin
    Beep();
    MessageDlg('Error', 'Fail to add verify operation', mtError, [mbOK], 0);
    VSProg_Caller.UnTake;
    exit;
  end;

  LogInfo('Running...');
  VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.OperationParser, 0, False);
  LogInfo('Idle');
end;

procedure TFormMain.cbboxInputFileChange(Sender: TObject);
begin
  cbboxInputFile.Hint := cbboxInputFile.Text;
end;

procedure TFormMain.cbboxModeChange(Sender: TObject);
begin
  if cbboxMode.Items.Count > 0 then
  begin
    VSProg_GUIFeatureInit(CurTargetSeries.Feature +
      CurTargetSeries.GetModeFeatureByModeChar(CurTargetChip.Mode[1 +
      cbboxMode.ItemIndex]), False);

    AdjustComponentColor(cbboxMode);
    AdjustComponentColor(lbledtFuse);
    AdjustComponentColor(lbledtLock);
    AdjustComponentColor(lbledtCali);
    AdjustComponentColor(lbledtUsrSig);
    AdjustComponentColor(lbledtSpecialStr);
    AdjustComponentColor(lbledtAddr);
    AdjustComponentColor(sedtFreq);
  end;
end;

procedure TFormMain.cbboxTargetChange(Sender: TObject);
var
  i: integer;
begin
  CurTargetChip := CurTargetSeries.TargetChips[cbboxTarget.ItemIndex];

  if CurTargetChip.AreaCount = 0 then
  begin
    // Parse Memory Info using '-D' option
    if not VSProg_PrepareToRunCli then
    begin
      exit;
    end;
    VSProg_Caller.AddParameter(VSProg_GetTargetDefineParameters);
    VSProg_Caller.AddParameter('Dall');
    VSProg_Parser.LogOutputEnable := False;
    VSProg_Parser.CallbackFunc    := @CurTargetChip.TargetAreaParser;
    VSProg_RunAlgorithm(VSProg_Caller, @VSProg_Parser.MemoryTargetInfoParser, 0, True);
  end;

  for i := 0 to CurTargetChip.AreaCount - 1 do
  begin
    if CurTargetChip.TargetAreas[i].InFile then
    begin
      AddTargetFile(CurTargetChip.TargetAreas[i].Name);
    end;
  end;

  PreviousTargetIndex := cbboxTarget.ItemIndex;
  VSProg_GUITargetAreaInit;
  VSProg_GUIModeInit;

  AdjustComponentColor(cbboxMode);
  AdjustComponentColor(lbledtFuse);
  AdjustComponentColor(lbledtLock);
  AdjustComponentColor(lbledtCali);
  AdjustComponentColor(lbledtUsrSig);
  AdjustComponentColor(lbledtSpecialStr);
  AdjustComponentColor(lbledtAddr);
  AdjustComponentColor(sedtFreq);
end;

procedure TFormMain.cbboxTargetTypeChange(Sender: TObject);
var
  target_idx: integer;
begin
  if PreviousTarget <> (Sender as TComboBox).Caption then
  begin
    SetLength(TargetFile, 0);
    cbboxInputFile.Clear;
    PreviousTargetIndex := 0;
  end;
  PreviousTarget := (Sender as TComboBox).Caption;

  target_idx := (Sender as TComboBox).ItemIndex;
  if target_idx < 0 then
  begin
    exit;
  end;

  CurTargetSeries := VSProg_Targets.TargetSeries[target_idx];
  if VSProg_GUITargetSeriesPageInit(target_idx) and
    (cbboxTarget.ItemIndex >= 0) then
  begin
    //
  end
  else
  begin
    gbInputFile.Enabled := False;
    gbOption.Enabled := False;
    gbOperation.Enabled := False;
  end;
end;

procedure TFormMain.dedtPathChange(Sender: TObject);
begin
  if ((Sender as TDirectoryEdit).Directory <> '') and
     ((Sender as TDirectoryEdit).Directory[Length(
    (Sender as TDirectoryEdit).Directory)] <> System.DirectorySeparator) then
  begin
    (Sender as TDirectoryEdit).Directory :=
      (Sender as TDirectoryEdit).Directory + System.DirectorySeparator;
  end;

  VSProg_Exists  := FileExistsUtf8(dedtVSProg.Directory + VSPROG_STR);
  OpenOCD_Exists := FileExistsUtf8(dedtOpenOCD.Directory + OPENOCD_STR);
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  if POLLTHREAD_EN and Assigned(PollThread) then
  begin
    PollThread.Terminate;
    Sleep(100);
    if Assigned(PollThread) then
    begin
//      PollThread.Free;
    end;
  end;

  // save settings to config file
  xmlcfgMain.SetValue('vsprog_dir', dedtVSProg.Directory);
  xmlcfgMain.SetValue('openocd_dir', dedtOpenOCD.Directory);
  xmlcfgMain.SetValue('activepage', pcMain.ActivePage.Caption);
  xmlcfgMain.SetValue('openocd/interface', cbboxOpenOCDInterface.Text);
  xmlcfgMain.SetValue('openocd/target', cbboxOpenOCDTarget.Text);
  xmlcfgMain.SetValue('openocd/script', cbboxOpenOCDScript.Text);
  xmlcfgMain.SetValue('openocd/option', edtOpenOCDOption.Text);
  xmlcfgMain.SetValue('svf/filename', fneditSVFFile.FileName);
  xmlcfgMain.SetValue('svf/option', edtSVFOption.Text);
  xmlcfgMain.SetValue('power/voltage', cbboxPower.Text);
  xmlcfgMain.SetValue('fw/filename', fnFW.FileName);
  xmlcfgMain.SetValue('fw/comm', cbboxCOM.Text);
  xmlcfgMain.SetValue('target/chip', cbboxTarget.Text);
  xmlcfgMain.SetValue('target/series', cbboxTargetType.Text);
  xmlcfgMain.SetValue('target/mode', cbboxMode.Text);
  xmlcfgMain.SetValue('target/filename', cbboxInputFile.Text);
  xmlcfgMain.SetValue('target/freq', sedtFreq.Value);
  if lbledtAddr.Enabled then
  begin
    xmlcfgMain.SetValue('target/exe_addr', lbledtAddr.Text);
  end;
  if Length(TargetFile) > 0 then
  begin
    xmlcfgMain.SetValue('target/files/number', Length(TargetFile));
    for i := low(TargetFile) to high(TargetFile) do
    begin
      xmlcfgMain.SetValue('target/files/' + IntToStr(i) + '/target',
        TargetFile[i].target);
      xmlcfgMain.SetValue('target/files/' + IntToStr(i) + '/filename',
        TargetFile[i].filename);
    end;
  end;
  if lbledtFuse.Enabled then
  begin
    xmlcfgMain.SetValue('target/fuse', lbledtFuse.Text);
  end;
  if lbledtLock.Enabled then
  begin
    xmlcfgMain.SetValue('target/lock', lbledtLock.Text);
  end;
  if lbledtCali.Enabled then
  begin
    xmlcfgMain.SetValue('target/cali', lbledtCali.Text);
  end;
  if lbledtUsrSig.Enabled then
  begin
    xmlcfgMain.SetValue('target/usrsig', lbledtUsrSig.Text);
  end;
  if lbledtSpecialStr.Enabled then
  begin
    xmlcfgMain.SetValue('target/special_str', lbledtSpecialStr.Text);
  end;
  if chkboxNoconnect.Enabled then
  begin
    xmlcfgMain.SetValue('target/nc', chkboxNoconnect.Checked);
  end;
  if chkboxNowarning.Enabled then
  begin
    xmlcfgMain.SetValue('target/nw', chkboxNowarning.Checked);
  end;
  if chkboxApp.Enabled then
  begin
    xmlcfgMain.SetValue('target/flashen', chkboxApp.Checked);
  end;
  if chkboxEE.Enabled then
  begin
    xmlcfgMain.SetValue('target/eepromen', chkboxEE.Checked);
  end;
  if chkboxFuse.Enabled then
  begin
    xmlcfgMain.SetValue('target/fuseen', chkboxFuse.Checked);
  end;
  if chkboxLock.Enabled then
  begin
    xmlcfgMain.SetValue('target/locken', chkboxLock.Checked);
  end;
  if chkboxUsrSig.Enabled then
  begin
    xmlcfgMain.SetValue('target/usrsigen', chkboxUsrSig.Checked);
  end;
  if chkboxCali.Enabled then
  begin
    xmlcfgMain.SetValue('target/calien', chkboxCali.Checked);
  end;
  if chkboxSpecialStr.Enabled then
  begin
    xmlcfgMain.SetValue('target/special_stren', chkboxSpecialStr.Checked);
  end;
  if chkboxMP.Enabled then
  begin
    xmlcfgMain.SetValue('target/mass', chkboxMP.Checked);
  end;
  xmlcfgMain.SetValue('target/ebw', chkboxEraseBeforeWrite.Checked);
  xmlcfgMain.SetValue('target/vaw', chkboxVerifyAfterWrite.Checked);
  xmlcfgMain.SetValue('target/extraparam', lbledtExtraPara.Text);
  FormComSetup.GetComMode(ComMode);
  xmlcfgMain.SetValue('commode/com', ComMode.comstr);
  xmlcfgMain.SetValue('commode/baudrate', ComMode.baudrate);
  xmlcfgMain.SetValue('commode/datalength', ComMode.datalength);
  xmlcfgMain.SetValue('commode/parity', ComMode.paritybit);
  xmlcfgMain.SetValue('commode/stop', ComMode.stopbit);
  xmlcfgMain.SetValue('commode/handshake', ComMode.handshake);
  xmlcfgMain.SetValue('commode/aux', ComMode.auxpin);
  xmlcfgMain.Flush;

  tiMain.Hide;
  CloseAction := caFree;
end;

procedure TFormMain.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormMain.AdjustComponentColor(Sender: TControl);
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

procedure TFormMain.LogInfo(info: string);
begin
  memoInfo.Lines.Add(info);
  sbMain.Panels.Items[0].Text := info;

  if info = 'Idle' then
  begin
    sbMain.Panels.Items[1].Text := '';
    pgbarmain.Position := 0;
  end;
end;

function TFormMain.VSProg_PrepareToRun(aApplicationName: string): boolean;
begin
  Result := False;
  if not FileExistsUtf8(aApplicationName) then
  begin
    exit;
  end;

  // take it first
  if not VSProg_Caller.Take() then
  begin
    if VSProg_Taken_By_Polling then
    begin
      // occupied by polling thread, wait a while
      {Sleep(100);
      if not VSProg_Caller.Take() then
      begin
        // give up ......
        exit;
      end;
      }
      while not VSProg_Caller.Take() do
      begin
      end;
    end
    else
    begin
      // other operation is on the way
      exit;
    end;
  end;

  VSProg_Caller.Application := Utf8ToAnsi(aApplicationName);
  VSProg_Caller.RemoveAllParameters();
  Result := True;
  memoLog.Clear;
  memoInfo.Clear;
end;

function TFormMain.VSProg_PrepareToRunOpenOCD: boolean;
begin
  Result := VSProg_PrepareToRun(dedtOpenOCD.Directory + OPENOCD_STR);
end;

function TFormMain.VSProg_PrepareToRunCLI: boolean;
begin
  Result := VSProg_PrepareToRun(dedtVSProg.Directory + VSPROG_STR);
end;

procedure TFormMain.VSProg_PrepareMiscParameters(var caller: TCLI_Caller);
var
  i: integer;
  io_file_opt, strTmp, strExt: string;
  areaTmp: TTargetArea;
  FakeArea: TFakeArea;
  addr, seg: cardinal;
  faddr, fseg: cardinal;
begin
  // COM Mode
  if btnComSetup.Visible then
  begin
    if ComMode.comstr = '' then
    begin
      FormComSetup.GetComMode(ComMode);
    end;
    caller.AddParameter('C "' + ComMode.comstr + ':' + IntToStr(ComMode.baudrate) +
      ' ' + IntToStr(ComMode.datalength) + ComMode.paritybit +
      ComMode.stopbit + ' ' + ComMode.handshake + ComMode.auxpin + '"');
  end;

  // input/output file
  if bReadOperation then
  begin
    io_file_opt := 'O';
  end
  else
  begin
    io_file_opt := 'I';
  end;

  if cbboxInputFile.Text = '' then
  begin

  end
  else if cbboxInputFile.Text <> 'ALL' then
  begin
    // enable selected input file
    i := Pos(':', cbboxInputFile.Text);
    areaTmp := CurTargetChip.GetArea(GetAreaShortName(
      Copy(cbboxInputFile.Text, 1, i - 1)));
    if areaTmp <> nil then
    begin
      addr     := areaTmp.StartAddr;
      seg      := areaTmp.SegAddr;
      faddr    := 0;
      fseg     := 0;
      FakeArea := CurTargetSeries.GetFakeArea(areaTmp.Name);
      if FakeArea <> nil then
      begin
        if FakeArea.FakeAddrEn then
        begin
          faddr := FakeArea.FakeAddr;
          Inc(addr, faddr);
        end;
        if FakeArea.FakeSegEn then
        begin
          fseg := FakeArea.FakeSeg;
          Inc(seg, fseg);
        end;
      end;

      strTmp := Copy(cbboxInputFile.Text, i + 1, Length(cbboxInputFile.Text) - i);
      strExt := LowerCase(ExtractFileExt(strTmp));
      if (strExt = '.hex') or (strExt = '.s19') then
      begin
        caller.AddParameter(io_file_opt + '"' + strTmp + '@' +
          IntToStr(fseg) + ',' + IntToStr(faddr) + '"');
      end
      else if strExt = '.bin' then
      begin
        caller.AddParameter(io_file_opt + '"' + strTmp + '@' +
          IntToStr(seg) + ',' + IntToStr(addr) + '"');
      end;
    end;
  end
  else
  begin
    // enable all input file
    for i := low(TargetFile) to high(TargetFile) do
    begin
      if TargetFile[i].filename <> '' then
      begin
        areaTmp := CurTargetChip.GetArea(TargetFile[i].target);
        if areaTmp <> nil then
        begin
          addr     := areaTmp.StartAddr;
          seg      := areaTmp.SegAddr;
          faddr    := 0;
          fseg     := 0;
          FakeArea := CurTargetSeries.GetFakeArea(areaTmp.Name);
          if FakeArea <> nil then
          begin
            if FakeArea.FakeAddrEn then
            begin
              faddr := FakeArea.FakeAddr;
              Inc(addr, faddr);
            end;
            if FakeArea.FakeSegEn then
            begin
              fseg := FakeArea.FakeSeg;
              Inc(seg, fseg);
            end;
          end;

          strExt := LowerCase(ExtractFileExt(TargetFile[i].filename));
          if (strExt = '.hex') or (strExt = '.s19') then
          begin
            caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
              '@' + IntToStr(fseg) + ',' + IntToStr(faddr) + '"');
          end
          else if strExt = '.bin' then
          begin
            caller.AddParameter(io_file_opt + '"' + TargetFile[i].filename +
              '@' + IntToStr(seg) + ',' + IntToStr(addr) + '"');
          end;
        end;
      end;
    end;
  end;

  // extra parameters
  if lbledtExtraPara.Text <> '' then
  begin
    caller.AddParametersString(lbledtExtraPara.Text);
  end;

  // Mode
  if cbboxMode.Enabled and (cbboxMode.Text <> '') then
  begin
    caller.AddParameter('m' + cbboxMode.Text[1]);
  end;

  // Frequency
  if sedtFreq.Visible and sedtFreq.Enabled and (sedtFreq.Value > 0) then
  begin
    caller.AddParameter('F' + IntToStr(sedtFreq.Value));
  end;

  // ProgrammerParameter
  if ProgrammerParameter <> '' then
  begin
    caller.AddParameter('U"' + ProgrammerParameter + '"');
  end;
end;

procedure TFormMain.VSProg_PrepareBaseParameters(var caller: TCLI_Caller);
begin
  caller.AddParameter(VSProg_GetTargetDefineParameters());

  VSProg_PrepareMiscParameters(caller);
end;

procedure TFormMain.VSProg_PrepareOperationParameters(var caller: TCLI_Caller);
begin
  // enable GUI mode
  caller.AddParameter('G');
  VSProg_PrepareBaseParameters(caller);

  // Fuse
  if lbledtFuse.Enabled and chkboxFuse.Enabled and chkboxFuse.Checked and
    (lbledtFuse.Text <> '') then
  begin
    caller.AddParameter('tu' + lbledtFuse.Text);
  end;

  // Lock
  if lbledtLock.Enabled and chkboxLock.Enabled and chkboxLock.Checked and
    (lbledtLock.Text <> '') then
  begin
    caller.AddParameter('tl' + lbledtLock.Text);
  end;

  // SpecialStr
  if lbledtSpecialStr.Enabled and chkboxSpecialStr.Enabled and chkboxSpecialStr.Checked and
    (lbledtSpecialStr.Text <> '') then
  begin
    caller.AddParameter('tt' + lbledtSpecialStr.Text);
  end;

  // Execute
  if lbledtAddr.Visible and lbledtAddr.Enabled and (lbledtAddr.Text <> '') then
  begin
    caller.AddParameter('x' + lbledtAddr.Text);
  end;

  // Mass-product support
  if chkboxMP.Enabled and chkboxMP.Checked then
  begin
    caller.AddParameter('M');
  end;
end;

procedure TFormMain.tiMainClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
end;

function TFormMain.VSProg_SettingTargetParserCallback(var line: string): boolean;
var
  dis: string;
begin
  Result := True;
  if Pos('setting: ', line) = 1 then
  begin
    // check disable
    dis := '';
    GetParameter(line, 'ban', dis);
    if (dis = '*') or (Pos(cbboxMode.Text[1], dis) > 0) then
    begin
      // current setting is disabled in current mode
      line := line + ', disabled = 1';
    end;
  end;

  FormParaEditor.ParseLine(line);
end;

{ OpenOCD functions }
function TFormMain.OpenOCD_Init(): boolean;
var
  SearchResult: TSearchRec;
  index: integer;
begin
  if (cbboxOpenOCDInterface.Items.Count = 0) and
    DirectoryExists(dedtOpenOCD.Directory + 'interface') then
  begin
    if FindFirst(dedtOpenOCD.Directory + 'interface' + System.DirectorySeparator +
      '*', (faAnyFile and not faDirectory), SearchResult) = 0 then
    begin
      if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
      begin
        cbboxOpenOCDInterface.Items.Add('interface' + System.DirectorySeparator +
          SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
        begin
          cbboxOpenOCDInterface.Items.Add('interface' + System.DirectorySeparator +
            SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDInterface.Items.Count > 0 then
    begin
      index := cbboxOpenOCDInterface.Items.IndexOf('interface' +
        System.DirectorySeparator + 'vsllink.cfg');
      if index > 0 then
      begin
        cbboxOpenOCDInterface.ItemIndex := index;
      end
      else
      begin
        cbboxOpenOCDInterface.ItemIndex := 0;
      end;
    end;
  end;

  if (cbboxOpenOCDTarget.Items.Count = 0) and
    DirectoryExists(dedtOpenOCD.Directory + 'target') then
  begin
    if FindFirst(dedtOpenOCD.Directory + 'target' + System.DirectorySeparator +
      '*', (faAnyFile and not faDirectory), SearchResult) = 0 then
    begin
      if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
      begin
        cbboxOpenOCDTarget.Items.Add('target' + System.DirectorySeparator +
          SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
        begin
          cbboxOpenOCDTarget.Items.Add('target' + System.DirectorySeparator +
            SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDTarget.Items.Count > 0 then
    begin
      cbboxOpenOCDTarget.ItemIndex := 0;
    end;
  end;

  if (cbboxOpenOCDScript.Items.Count = 0) and
    DirectoryExists(dedtOpenOCD.Directory + 'script') then
  begin
    if FindFirst(dedtOpenOCD.Directory + 'script' + System.DirectorySeparator +
      '*', (faAnyFile and not faDirectory), SearchResult) = 0 then
    begin
      if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
      begin
        cbboxOpenOCDScript.Items.Add('script' + System.DirectorySeparator +
          SearchResult.Name);
      end;
      while FindNext(SearchResult) = 0 do
      begin
        if LowerCase(ExtractFileExt(SearchResult.Name)) = '.cfg' then
        begin
          cbboxOpenOCDScript.Items.Add('script' + System.DirectorySeparator +
            SearchResult.Name);
        end;
      end;
    end;
    FindClose(SearchResult);

    if cbboxOpenOCDScript.Items.Count > 0 then
    begin
      cbboxOpenOCDScript.ItemIndex := 0;
    end;
  end;

  Result := True;
end;

initialization
  {$I main.lrs}

end.

