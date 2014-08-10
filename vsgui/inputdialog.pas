unit inputdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type
  TInputType = (itNumeric, itLiteral);
  TStrCase   = (scBoth, scUpper, scLower);
  TNumRadix  = (nrBinary = 2, nrDecimal = 10, nrHexadecimal = 16);

  { TFormInputDialog }

  TFormInputDialog = class(TForm)
    btnOK:     TButton;
    btnCancel: TButton;
    edtInput:  TEdit;
    lblPrefix: TLabel;
    tInit: TTimer;
    procedure CenterControl(ctl: TControl; ref: TControl);
    procedure edtInputKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure tInitTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    InputType: TInputType;

    CommonMaxLength: integer;
    CommonPrefix: string;
    CommonCase: TStrCase;
    NumMin:   int64;
    NumMax:   int64;
    NumRadix: TNumRadix;

    function GetString: string;
    function GetNumber: integer;
  end;

function StrToIntRadix(sData: string; radix: integer): QWord;
function StrToIntRadix(sData: string; radix: integer; var off: integer): QWord;
function IntToStrRadix(aData, radix, minlen: integer): string;
function IntToStrRadix(aData: QWord; radix, minlen: integer): string;

var
  FormInputDialog: TFormInputDialog;

const
  HEX_PARSE_STR: string = '0123456789ABCDEF';
  BYTELEN_ACCORDING_TO_RADIX: array[2..16] of integer =
    (8, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2);

implementation

function IntToStrRadix(aData: QWord; radix, minlen: integer): string;
var
  t: integer;
  r: QWord;
begin
  Result := '';
  if (radix = 0) or (radix > 16) then
  begin
    exit;
  end;

  r := radix;
  repeat
    t := aData mod r;
    if t < 10 then
      Result := IntToStr(t) + Result
    else
      Result := InttoHex(t, 1) + Result;
    aData := aData div r;
  until (aData = 0);

  while Length(Result) < minlen do
  begin
    Result := '0' + Result;
  end;
end;

function IntToStrRadix(aData, radix, minlen: integer): string;
var
  t: integer;
begin
  Result := '';
  if (radix = 0) or (radix > 16) then
  begin
    exit;
  end;

  repeat
    t := aData mod radix;
    if t < 10 then
      Result := IntToStr(t) + Result
    else
      Result := InttoHex(t, 1) + Result;
    aData := aData div radix;
  until (aData = 0);

  while Length(Result) < minlen do
  begin
    Result := '0' + Result;
  end;
end;

function StrToIntRadix(sData: string; radix: integer): QWord;
var
  off: integer;
begin
  off := 0;
  Result := StrToIntRadix(sData, radix, off);
end;

function StrToIntRadix(sData: string; radix: integer; var off: integer): QWord;
var
  i: integer;
  r, v: QWord;
  strFirst2: string;
  strNumOnly: string;
begin
  Result := 0;
  off := 0;
  sData  := UpperCase(sData);
  strFirst2 := Copy(sData, 1, 2);
  if strFirst2 = '0X' then
  begin
    radix := 16;
    sData := Copy(sData, 3, Length(sData) - 2);
    v := Pos(sData[1], HEX_PARSE_STR);
    if (v >= 1) and (v <= radix) then
    begin
      Inc(off, 2);
    end;
  end;
  if (radix < 2) or (radix > 16) then
  begin
    exit;
  end;

  strNumOnly := '';
  for i := 1 to Length(sdata) do
  begin
    v := Pos(sData[i], HEX_PARSE_STR);
    if (v < 1) or (v > radix) then
    begin
      break;
    end;

    strNumOnly := strNumOnly + sData[i];
    Inc(off);
  end;

  r := 1;
  for i := Length(strNumOnly) downto 1 do
  begin
    Inc(Result, r * (Pos(strNumOnly[i], HEX_PARSE_STR) - 1));
    r := r * radix;
  end;
end;

{ TFormInputDialog }

procedure TFormInputDialog.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormInputDialog.edtInputKeyPress(Sender: TObject; var Key: char);
var
  Value, radix: integer;
  key_upper:    char;
begin
  if Key = char(8) then
  begin
    // backspace is OK
    exit;
  end
  else if CommonCase <> scBoth then
  begin
    if CommonCase = scUpper then
    begin
      Key := AnsiStrUpper(@Key)^;
    end
    else
    begin
      Key := AnsiStrLower(@Key)^;
    end;
  end;

  case InputType of
    itNumeric:
    begin
      // process radix
      radix     := integer(NumRadix);
      key_upper := AnsiStrUpper(@Key)^;
      Value     := Pos(key_upper, HEX_PARSE_STR);
      if (Value > radix) or (Value = 0) then
      begin
        Key := char(0);
        exit;
      end;
      if not ((NumMax = 0) and (NumMin = 0)) then
      begin
        // process Min Max value
      end;
    end;
    itLiteral:
    begin

    end;
  end;
end;

procedure TFormInputDialog.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

procedure TFormInputDialog.FormShow(Sender: TObject);
begin
  lblPrefix.Caption  := CommonPrefix;
  edtInput.MaxLength := CommonMaxLength;
  edtInput.Text      := '';

  UpdateShowing;

  tInit.Enabled := True;
end;

procedure TFormInputDialog.tInitTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;

  CenterControl(lblPrefix, edtInput);
end;

function TFormInputDialog.GetString: string;
begin
  Result := edtInput.Text;
end;

function TFormInputDialog.GetNumber: integer;
var
  str_result: string;
begin
  str_result := edtInput.Text;

  Result := StrToIntRadix(str_result, integer(NumRadix));
end;

initialization
  {$I inputdialog.lrs}

end.

