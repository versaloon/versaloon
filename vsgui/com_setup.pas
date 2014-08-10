unit com_setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Synaser, vsprogparser;

type
  TComMode = record
    comstr: string;
    baudrate: integer;
    datalength: integer;
    paritybit: char;
    stopbit: char;
    handshake: char;
    auxpin: char;
  end;

  { TFormComSetup }

  TFormComSetup = class(TForm)
    btnOK: TButton;
    cbboxBaudrate: TComboBox;
    cbboxCom: TComboBox;
    cbboxDataLength: TComboBox;
    cbboxParitybit: TComboBox;
    cbboxStopbit: TComboBox;
    cbboxHandshake: TComboBox;
    cbboxAuxPin: TComboBox;
    lblAuxPin: TLabel;
    lblBaudrate: TLabel;
    lblCom: TLabel;
    lblDataLength: TLabel;
    lblParitybit: TLabel;
    lblStopbit: TLabel;
    lblHandshake: TLabel;
    tInit: TTimer;
    procedure CenterControl(ctl: TControl; ref: TControl);
    procedure AdjustComponentColor(Sender: TControl);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure CheckComPort();
    procedure tInitTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ComSetPara(ComMode: TComMode);
    procedure ComInitPara(ComInitMode: TComMode);
    procedure ComInitPara(CommStr: string);
    procedure GetComMode(var ComMode: TComMode);
  end; 

var
  FormComSetup: TFormComSetup;

const
  DEFAULT_BAUDRATE: integer = 115200;
  DEFAULT_DATALENGTH: integer = 8;
  DEFAULT_PARITYBIT: integer = 0;
  DEFAULT_PARITYBIT_CHAR: char = 'N';
  DEFAULT_STOPBIT: integer = 1;
  DEFAULT_STOPBIT_CHAR: char = '1';
  DEFAULT_HANDSHAKE: integer = 0;
  DEFAULT_HANDSHAKE_CHAR: char = 'N';
  DEFAULT_AUXPIN: integer = 0;
  DEFAULT_AUXPIN_CHAR: char = 'N';
  {$IFDEF UNIX}
  COMPORTS: array[0..39] of string =
    ('/dev/ttyS0', '/dev/ttyS1', '/dev/ttyS2', '/dev/ttyS3', '/dev/ttyS4',
     '/dev/ttyS5', '/dev/ttyS6', '/dev/ttyS7', '/dev/ttyS8', '/dev/ttyS9',
     '/dev/ttyS10', '/dev/ttyS11', '/dev/ttyS12', '/dev/ttyS13', '/dev/ttyS14',
     '/dev/ttyS15', '/dev/ttyS16', '/dev/ttyS17', '/dev/ttyS18', '/dev/ttyS19',
     '/dev/ttyACM0', '/dev/ttyACM1', '/dev/ttyACM2', '/dev/ttyACM3', '/dev/ttyACM4',
     '/dev/ttyACM5', '/dev/ttyACM6', '/dev/ttyACM7', '/dev/ttyACM8', '/dev/ttyACM9',
     '/dev/ttyACM10', '/dev/ttyACM11', '/dev/ttyACM12', '/dev/ttyACM13', '/dev/ttyACM14',
     '/dev/ttyACM15', '/dev/ttyACM16', '/dev/ttyACM17', '/dev/ttyACM18', '/dev/ttyACM19');
  {$ELSE}{$IFDEF MSWINDOWS}
  COMPORTS: array[0..19] of string =
    ('COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9', 'COM10',
    'COM11', 'COM12', 'COM13', 'COM14', 'COM15', 'COM16', 'COM17', 'COM18', 'COM19', 'COM20');
  {$ENDIF}{$ENDIF}

implementation

procedure TFormComSetup.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormComSetup.AdjustComponentColor(Sender: TControl);
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

procedure TFormComSetup.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormComSetup.FormCreate(Sender: TObject);
begin
  cbboxBaudrate.Text := IntToStr(DEFAULT_BAUDRATE);
  cbboxDataLength.Text := IntToStr(DEFAULT_DATALENGTH);
  cbboxParitybit.ItemIndex := DEFAULT_PARITYBIT;
  cbboxStopbit.ItemIndex := DEFAULT_STOPBIT;
  cbboxHandshake.ItemIndex := DEFAULT_HANDSHAKE;
  cbboxAuxPin.ItemIndex := DEFAULT_AUXPIN;
end;

procedure TFormComSetup.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(27) then
  begin
    close;
  end;
end;

procedure TFormComSetup.CheckComPort();
var
  ser: TBlockSerial;
  index: integer;
  cur_comport: string;
begin
  cur_comport := cbboxCom.Text;
  cbboxCom.Clear;

  for index := 0 to Length(COMPORTS) - 1 do
  begin
    ser := TBlockSerial.Create;
    try
      ser.Connect(COMPORTS[index]);
      if ser.LastError = 0 then
      begin
        cbboxCOM.Items.Add(COMPORTS[index]);
      end;
    finally
      ser.Free;
    end;
  end;
  cbboxCOM.Items.Add('usbtocomm');

  if cbboxCOM.Items.IndexOf(cur_comport) > 0 then
  begin
    cbboxCOM.Text := cur_comport;
  end
  else
  begin
    cbboxCOM.ItemIndex := 0;
  end;
end;

procedure TFormComSetup.tInitTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;

  CenterControl(lblCom, cbboxCom);
  CenterControl(lblBaudrate, cbboxBaudrate);
  CenterControl(lblDataLength, cbboxDataLength);
  CenterControl(lblParitybit, cbboxParitybit);
  CenterControl(lblStopbit, cbboxStopbit);
  CenterControl(lblHandshake, cbboxHandshake);
  CenterControl(lblAuxPin, cbboxAuxPin);
end;

procedure TFormComSetup.FormShow(Sender: TObject);
begin
  CheckComPort;
  ActiveControl := cbboxCom;

  tInit.Enabled := True;
end;

procedure TFormComSetup.ComInitPara(CommStr: string);
var
  tmpComm: TComMode;
  str_tmp: string;
begin
  tmpComm.baudrate := DEFAULT_BAUDRATE;
  tmpComm.datalength := DEFAULT_DATALENGTH;
  tmpComm.paritybit := DEFAULT_PARITYBIT_CHAR;
  tmpComm.stopbit := DEFAULT_STOPBIT_CHAR;
  tmpComm.handshake := DEFAULT_HANDSHAKE_CHAR;
  tmpComm.auxpin := DEFAULT_AUXPIN_CHAR;
  str_tmp := '';

  GetParameter(CommStr, 'baudrate', tmpComm.baudrate);
  GetParameter(CommStr, 'datalength', tmpComm.datalength);
  GetParameter(CommStr, 'paritybit', str_tmp);
  if str_tmp <> '' then
  begin
    tmpComm.paritybit := str_tmp[1];
  end;
  GetParameter(CommStr, 'stopbit', str_tmp);
  if str_tmp <> '' then
  begin
    tmpComm.stopbit := str_tmp[1];
  end;
  GetParameter(CommStr, 'handshake', str_tmp);
  if str_tmp <> '' then
  begin
    tmpComm.handshake := str_tmp[1];
  end;
  GetParameter(CommStr, 'auxpin', str_tmp);
  if str_tmp <> '' then
  begin
    tmpComm.auxpin := str_tmp[1];
  end;
  ComInitPara(tmpComm);
end;

procedure TFormComSetup.ComInitPara(ComInitMode: TComMode);
begin
  cbboxBaudrate.Enabled := not (ComInitMode.baudrate > 0);
  AdjustComponentColor(cbboxBaudrate);

  cbboxDataLength.Enabled := not (ComInitMode.datalength > 0);
  AdjustComponentColor(cbboxDataLength);

  cbboxParitybit.Enabled := not (
     (ComInitMode.paritybit = Char('N'))
     or (ComInitMode.paritybit = Char('O'))
     or (ComInitMode.paritybit = Char('E')));
  AdjustComponentColor(cbboxParitybit);

  cbboxStopbit.Enabled := not (
     (ComInitMode.stopbit = Char('1'))
     or (ComInitMode.stopbit = Char('P'))
     or (ComInitMode.stopbit = Char('2')));
  AdjustComponentColor(cbboxStopbit);

  cbboxHandshake.Enabled := not (
     (ComInitMode.handshake = Char('N'))
     or (ComInitMode.handshake = Char('S'))
     or (ComInitMode.handshake = Char('H')));
  AdjustComponentColor(cbboxHandshake);

  cbboxAuxPin.Enabled := not (
     (ComInitMode.auxpin = Char('N'))
     or (ComInitMode.auxpin = Char('A')));
  AdjustComponentColor(cbboxAuxPin);

  ComSetPara(ComInitMode);
end;

procedure TFormComSetup.ComSetPara(ComMode: TComMode);
begin
  CheckComPort;

  if ComMode.baudrate > 0 then
  begin
    cbboxBaudrate.Text := IntToStr(ComMode.baudrate);
  end;

  if ComMode.datalength > 0 then
  begin
    cbboxDataLength.Text := IntToStr(ComMode.datalength);
  end;

  if ComMode.paritybit = Char('N') then
  begin
    cbboxParitybit.Text := 'None';
  end
  else if ComMode.paritybit = Char('O') then
  begin
    cbboxParitybit.Text := 'Odd';
  end
  else if ComMode.paritybit = Char('E') then
  begin
    cbboxParitybit.Text := 'Even';
  end;

  if ComMode.stopbit = Char('1') then
  begin
    cbboxStopbit.Text := '1';
  end
  else if ComMode.stopbit = Char('P') then
  begin
    cbboxStopbit.Text := '1.5';
  end
  else if ComMode.stopbit = Char('2') then
  begin
    cbboxStopbit.Text := '2';
  end;

  if ComMode.handshake = Char('N') then
  begin
    cbboxHandshake.Text := 'None';
  end
  else if ComMode.handshake = Char('S') then
  begin
    cbboxHandshake.Text := 'Software';
  end
  else if ComMode.handshake = Char('H') then
  begin
    cbboxHandshake.Text := 'Hardware';
  end;

  if ComMode.auxpin = Char('N') then
  begin
    cbboxAuxPin.Text := 'None';
  end
  else if ComMode.auxpin = Char('A') then
  begin
    cbboxAuxPin.Text := 'AuxPin';
  end;
end;

procedure TFormComSetup.GetComMode(var ComMode: TComMode);
begin
  ComMode.comstr := cbboxCom.Text;
  ComMode.baudrate := StrToInt(cbboxBaudrate.Text);
  ComMode.datalength := StrToInt(cbboxDataLength.Text);
  ComMode.paritybit := cbboxParitybit.Text[1];
  if cbboxStopbit.Text = '1' then
  begin
    ComMode.stopbit := Char('1');
  end
  else if cbboxStopbit.Text = '1.5' then
  begin
    ComMode.stopbit := Char('P');
  end
  else if cbboxStopbit.Text = '2' then
  begin
    ComMode.stopbit := Char('2');
  end;
  ComMode.handshake := cbboxHandshake.Text[1];
  ComMode.auxpin := cbboxAuxPin.Text[1];
end;

initialization
  {$I com_setup.lrs}

end.

