unit vsprogparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TLogOutputFunc    = procedure(var line: string) of object;
  TProgressInfoType = (liStartSection, liStep, liEndSection);
  TLogProgressFunc  = procedure(ProgressInfo: TProgressInfoType; info: string) of object;
  TParserFunc = function(var line: string): boolean of object;

  TVSProg_Info = record
    TargetVoltage: integer;
  end;

  { TVSProg_Parser }

  TVSProg_Parser = class(TObject)
    procedure Prepare();
    procedure SetLogOutputFunc(func: TLogOutputFunc);
    procedure SetLogProgressFunc(func: TLogProgressFunc);
    procedure SetCallbackFunc(func: TParserFunc);

    function CommonParser(line: string): boolean;
    function ErrorParser(var line: string): boolean;
    function VersionParser(var line: string): boolean;
    function TargetVoltageParser(var line: string): boolean;
    function OperationParser(var line: string): boolean;
    // used for -p
    function ProgrammerParser(var line: string): boolean;
    // used for parsing fuse/lock/calibration settings
    function SettingTargetInfoParser(var line: string): boolean;
    // used for parsing flash/eeprom information
    function MemoryTargetInfoParser(var line: string): boolean;
    function FuseDataParser(var line: string): boolean;
    function LockDataParser(var line: string): boolean;
    function CaliDataParser(var line: string): boolean;
    function UsrsigDataParser(var line: string): boolean;
    function SpecialStrParser(var line: string): boolean;
    function SupportParser(var line: string): boolean;
    function AutoDetectParser(var line: string): boolean;
    function TargetParser(var line: string): boolean;
  private
    FLogOutputEnable: boolean;
    FLogOutputFunc: TLogOutputFunc;
    FLogProgressEnable: boolean;
    FLogProgressFunc: TLogProgressFunc;
    FCallbackEnable: boolean;
    FCallbackFunc:  TParserFunc;
    FParserFunc:    TParserFunc;
    FFatalError:    boolean;
    FErrorStr:      string;
    FErrorNum:      integer;
    FResultStrings: TStringList;
    function ParseTargetData(var line: string; target: string; var result_str: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property ParserFunc: TParserFunc Read FParserFunc Write FParserFunc;
    property LogOutputEnable: boolean Read FLogOutputEnable Write FLogOutputEnable;
    property LogOutputFunc: TLogOutputFunc Read FLogOutputFunc Write SetLogOutputFunc;
    property LogProgressEnable: boolean Read FLogProgressEnable Write FLogProgressEnable;
    property LogProgressFunc: TLogProgressFunc
      Read FLogProgressFunc Write SetLogProgressFunc;
    property CallbackEnable: boolean Read FCallbackEnable Write FCallbackEnable;
    property CallbackFunc: TParserFunc Read FCallbackFunc Write SetCallbackFunc;
    property HasError: boolean Read FFatalError;
    property ErrorStr: string Read FErrorStr;
    property ResultStrings: TStringList Read FResultStrings;
  end;

  TVSProgErrorTranslator = record
    ErrStr: string;
    DisplayErrStr: string;
  end;
  function VSProg_TranslateError(ErrStr: string): string;

  function GetParameter(line, para_name: string; var Value: QWord): boolean;
  function GetParameter(line, para_name: string; var Value: integer): boolean;
  function GetParameter(line, para_name: string; var Value: cardinal): boolean;
  function GetParameter(line, para_name: string; var Value: string): boolean;

const
  EQUAL_STR: string      = ' = ';
  MAX_ERROR_LINES: integer = 10;
  VSProgErrorTranslator : array [0 .. 1] of TVSProgErrorTranslator =
  (
    (ErrStr: 'interface not supported: '; DisplayErrStr: 'Current interfaces not supported, get a VersaloonFull!!'),
    (ErrStr: ''; DisplayErrStr: '')
  );

implementation

function GetParameter(line, para_name: string; var Value: QWord): boolean;
var
  pos_start, pos_end: integer;
  str_tmp: string;
begin
  pos_start := Pos(para_name + EQUAL_STR, line);
  if pos_start > 0 then
  begin
    str_tmp := Copy(line, pos_start + Length(para_name + EQUAL_STR),
      Length(line) - pos_start);

    pos_end := Pos(',', str_tmp);
    if pos_end > 1 then
    begin
      str_tmp := Copy(str_tmp, 1, pos_end - 1);
    end;

    Value  := StrToQWord(str_tmp);
    Result := True;
  end
  else
  begin
    Value  := 0;
    Result := False;
  end;
end;

function GetParameter(line, para_name: string; var Value: string): boolean;
var
  pos_start, pos_end: integer;
  str_tmp: string;
begin
  pos_start := Pos(para_name + EQUAL_STR, line);
  if pos_start > 0 then
  begin
    str_tmp := Copy(line, pos_start + Length(para_name + EQUAL_STR),
      Length(line) - pos_start);

    pos_end := Pos(',', str_tmp);
    if pos_end > 1 then
    begin
      str_tmp := Copy(str_tmp, 1, pos_end - 1);
    end;

    Value  := str_tmp;
    Result := True;
  end
  else
  begin
    Value  := '';
    Result := False;
  end;
end;

function GetParameter(line, para_name: string; var Value: cardinal): boolean;
var
  pos_start, pos_end: integer;
  str_tmp: string;
begin
  pos_start := Pos(para_name + EQUAL_STR, line);
  if pos_start > 0 then
  begin
    str_tmp := Copy(line, pos_start + Length(para_name + EQUAL_STR),
      Length(line) - pos_start);

    pos_end := Pos(',', str_tmp);
    if pos_end > 1 then
    begin
      str_tmp := Copy(str_tmp, 1, pos_end - 1);
    end;

    Value  := StrToInt(str_tmp);
    Result := True;
  end
  else
  begin
    Value  := 0;
    Result := False;
  end;
end;

function GetParameter(line, para_name: string; var Value: integer): boolean;
var
  pos_start, pos_end: integer;
  str_tmp: string;
begin
  pos_start := Pos(para_name + EQUAL_STR, line);
  if pos_start > 0 then
  begin
    str_tmp := Copy(line, pos_start + Length(para_name + EQUAL_STR),
      Length(line) - pos_start);

    pos_end := Pos(',', str_tmp);
    if pos_end > 1 then
    begin
      str_tmp := Copy(str_tmp, 1, pos_end - 1);
    end;

    Value  := StrToInt(str_tmp);
    Result := True;
  end
  else
  begin
    Value  := 0;
    Result := False;
  end;
end;

function VSProg_TranslateError(ErrStr: string): string;
var
  strTmp: string;
  i: integer;
begin
  Result := '';
  for i := LOW(VSProgErrorTranslator) to HIGH(VSProgErrorTranslator) do
  begin
    strTmp := VSProgErrorTranslator[i].ErrStr;
    if (strTmp <> '') and (Pos(strTmp, ErrStr) > 0) then
    begin
      Result := VSProgErrorTranslator[i].DisplayErrStr;
      Exit;
    end;
  end;
end;

{ TVSProg_Parser }

constructor TVSProg_Parser.Create;
begin
  inherited Create;
  FResultStrings     := TStringList.Create;
  FLogProgressEnable := False;
  FLogOutputEnable   := False;
  FCallbackEnable    := False;
end;

destructor TVSProg_Parser.Destroy;
begin
  inherited Destroy;
  FResultStrings.Destroy;
end;

procedure TVSProg_Parser.SetLogOutputFunc(func: TLogOutputFunc);
begin
  FLogOutputFunc   := func;
  FLogOutputEnable := func <> nil;
end;

procedure TVSProg_Parser.SetLogProgressFunc(func: TLogProgressFunc);
begin
  FLogProgressFunc   := func;
  FLogProgressEnable := func <> nil;
end;

procedure TVSProg_Parser.SetCallbackFunc(func: TParserFunc);
begin
  FCallbackFunc   := func;
  FCallbackEnable := func <> nil;
end;

function TVSProg_Parser.ParseTargetData(var line: string; target: string;
  var result_str: string): boolean;
var
  pos_start: integer;
begin
  Result     := False;
  result_str := '';
  pos_start  := Pos(target + ' read is ', line);
  if pos_start > 0 then
  begin
    result_str := Copy(line, pos_start + Length(target + ' read is '),
      Length(line) - pos_start);
    Result     := True;
  end;
end;

procedure TVSProg_Parser.Prepare;
begin
  FResultStrings.Clear;
  FFatalError := False;
  FErrorNum   := 0;
  FErrorStr := '';
end;

function TVSProg_Parser.CommonParser(line: string): boolean;
begin
  if FLogOutputEnable and Assigned(FLogOutputFunc) then
  begin
    FLogOutputFunc(line);
  end;

  if not ErrorParser(line) then
  begin
    Result := False;
    exit;
  end;
  Result := True;

  if Assigned(FParserFunc) then
  begin
    Result := FParserFunc(line);
  end;

  if (Result <> False) and FCallbackEnable and Assigned(FCallbackFunc) then
  begin
    Result := FCallbackFunc(line);
  end;
end;

function TVSProg_Parser.ErrorParser(var line: string): boolean;
begin
  Result := True;
  if ((Pos('Error:', line) = 1) or (Pos('/****Bug****/:', line) = 1))
    and (FErrorNum < MAX_ERROR_LINES) then
  begin
//    if not FFatalError then
    begin
      FFatalError := True;
      FErrorStr   := FErrorStr + line + Char(10);
      Inc(FErrorNum);
    end;
    Result      := False;
  end;
end;

function TVSProg_Parser.VersionParser(var line: string): boolean;
begin
  Result := True;
  if Pos('vsprog', LowerCase(line)) = 1 then
  begin
    FResultStrings.Add(line);
  end;
end;

function TVSProg_Parser.TargetVoltageParser(var line: string): boolean;
var
  pos_start: integer;
  strTmp: string;
begin
  Result    := True;
  pos_start := Pos('Target runs at ', line);
  if pos_start > 0 then
  begin
    Inc(pos_start, Length('Target runs at '));
    strTmp := Copy(line, pos_start, Length(line) - pos_start);
    pos_start := Trunc(StrToFloat(strTmp) * 1000);
    FResultStrings.Add(IntToStr(pos_start));
  end;
end;

function TVSProg_Parser.OperationParser(var line: string): boolean;
var
  i: integer;
const
  operating: boolean = False;
begin
  Result := True;

  if ((Pos('writing', line) = 1) or (Pos('reading', line) = 1) or
    (Pos('verifying', line) = 1) or (Pos('erasing', line) = 1) or
    (Pos('checking', line) = 1) or (Pos('executing', line) = 1)) and
    (Length(line) > 9) then
  begin
    operating := True;
    if FLogProgressEnable and Assigned(FLogProgressFunc) then
    begin
      FLogProgressFunc(liStartSection, Copy(line, 1, Length(line) - 5));
    end;
  end;

  i := Pos('| ', line);
  if operating and (i > 0) then
  begin
    if FLogProgressEnable and Assigned(FLogProgressFunc) then
    begin
      FLogProgressFunc(liEndSection, Copy(line, i + 1, Length(line) - i));
    end;
    operating := False;
  end;

  if Pos('=', line) = 1 then
  begin
    i := 1;
    while line[i] = PChar('=') do
    begin
      if FLogProgressEnable and Assigned(FLogProgressFunc) then
      begin
        FLogProgressFunc(liStep, '=');
      end;
      Inc(i);
    end;
  end;
end;

function TVSProg_Parser.ProgrammerParser(var line: string): boolean;
var
  pos_start: integer;
  strTmp: string;
begin
  pos_start := Pos('USB_TO_XXX abilities: ', line);
  if pos_start > 0 then
  begin
    Inc(pos_start, Length('USB_TO_XXX abilities: '));
    strTmp := Copy(line, pos_start, Length(line) - pos_start);
    FResultStrings.Add(strTmp);
  end;
  Result := TargetVoltageParser(line);
end;

function TVSProg_Parser.SettingTargetInfoParser(var line: string): boolean;
begin
  Result := True;
  if (Pos('setting: ', line) = 1) or (Pos('warning: ', line) = 1) or
    (Pos('choice: ', line) = 1) then
  begin
    FResultStrings.Add(line);
  end;
end;

function TVSProg_Parser.MemoryTargetInfoParser(var line: string): boolean;
begin
  line   := line;
  Result := True;
end;

function TVSProg_Parser.SpecialStrParser(var line: string):boolean;
var
  tmpStr: string;
begin
  Result := True;
  tmpStr := '';
  if ParseTargetData(line, 'special_str', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.FuseDataParser(var line: string): boolean;
var
  tmpStr: string;
begin
  Result := True;
  tmpStr := '';
  if ParseTargetData(line, 'fuse', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.LockDataParser(var line: string): boolean;
var
  tmpStr: string;
begin
  Result := True;
  tmpStr := '';
  if ParseTargetData(line, 'lock', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.UsrsigDataParser(var line: string): boolean;
var
  tmpStr: string;
begin
  Result := True;
  tmpStr := '';
  if ParseTargetData(line, 'usrsig', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.CaliDataParser(var line: string): boolean;
var
  tmpStr: string;
begin
  Result := True;
  tmpStr := '';
  if ParseTargetData(line, 'calibration', tmpStr) then
  begin
    FResultStrings.Add(tmpStr);
  end;
end;

function TVSProg_Parser.SupportParser(var line: string): boolean;
begin
  line   := line;
  Result := True;
end;

function TVSProg_Parser.AutoDetectParser(var line: string): boolean;
var
  pos_start: integer;
begin
  Result    := True;
  pos_start := Pos(' found', line);
  if pos_start >= 8 then
  begin
    // chip found
    FResultStrings.Add(Copy(line, 9, pos_start - 9));
  end;
end;

function TVSProg_Parser.TargetParser(var line: string): boolean;
begin
  line := line;
  Result := True;
end;

end.

