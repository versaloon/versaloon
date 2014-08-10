unit hexeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Grids, ActnList, parameditor, inputdialog,
  Contnrs, Math, findreplace, FileUtil;

type

  { TMemList TMemInfo }

  TMemInfo = class(TObject)
  private
    FStartAddr: cardinal;
    FByteSize:  cardinal;
    function GetTail: cardinal;
  public
    constructor Create(aStartAddr, aByteSize: cardinal);
    property StartAddr: cardinal Read FStartAddr Write FStartAddr;
    property ByteSize: cardinal Read FByteSize Write FByteSize;
    property Head: cardinal Read FStartAddr;
    property Tail: cardinal Read GetTail;
  end;

  TMemList = class(TObjectList)
  private
    function GetMemInfoItem(Index: integer): TMemInfo;
    procedure SetMemInfoItem(Index: integer; aMemInfoObject: TMemInfo);
  public
    constructor Create;
    function Add(aStartAddr, aByteSize: cardinal): integer;
    function Add(aMemInfo: TMemInfo): integer;
    function GetIndexByAddr(aAddr: cardinal): integer;
    property Items[Index: integer]: TMemInfo Read GetMemInfoItem Write SetMemInfoItem;
      default;
  end;

  { TFileParser }

  TReadFileFunc = function(hFile: TFileStream; var buffer;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
  TValidateFileFunc = function(hFile: TFileStream; var buffer;
    ChangeList: TMemList; default_byte: byte; bytesize, start_addr: cardinal;
    seg_offset, addr_offset: int64): boolean;
  TPrepareFileFunc = function(hFile: TFileStream; default_byte: byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;

  TFileParser = record
    ext:      string;
    ReadFile: TReadFileFunc;
    ValidateFile: TValidateFileFunc;
    PrepareFile: TPrepareFileFunc;
  end;

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    actGoto:    TAction;
    actSave:    TAction;
    actNext:    TAction;
    actReplace: TAction;
    actSearch:  TAction;
    alHotKey:   TActionList;
    btnSave:    TButton;
    btnExit:    TButton;
    btnSaveAs: TButton;
    lblMeasureSize: TLabel;
    pnlData:    TPanel;
    pnlButton:  TPanel;
    sdSaveAs: TSaveDialog;
    sgData:     TStringGrid;
    tGoto:      TTimer;
    tReplace:   TTimer;
    tNext:      TTimer;
    tSearch:    TTimer;
    tInit:      TTimer;
    procedure actGotoExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sgDataKeyPress(Sender: TObject; var Key: char);
    procedure sgDataPrepareCanvas(Sender: TObject; aCol, aRow: integer;
      aState: TGridDrawState);
    procedure sgDataSelectCell(Sender: TObject; aCol, aRow: integer;
      var CanSelect: boolean);
    procedure sgDataTopLeftChanged(Sender: TObject);
    procedure tGotoTimer(Sender: TObject);
    procedure tInitTimer(Sender: TObject);
    procedure DataBuffChangeRequire(addr: integer; aValue: byte);
    procedure DirtyRowsByAddr(FromAddr, ToAddr: cardinal);
    procedure DirtyRowsByAddr(Addr: cardinal);
    function DoSearch(FromAddr: cardinal; ForData: string; UpSearch: boolean): integer;
    procedure sgDataGoToAddr(Addr: cardinal; bHexSection: boolean);
    procedure sgDataDoSearch();
    procedure sgDataDoGoto();
    procedure sgDataDoReplace();
    procedure sgDataDoNext();
    procedure tNextTimer(Sender: TObject);
    procedure tReplaceTimer(Sender: TObject);
    procedure tSearchTimer(Sender: TObject);
  private
    { private declarations }
    DataBuff:   ansistring;
    DataBuffChangeList: TMemList;
    RowDirty:   array of boolean;
    StrGridInited: boolean;
    CurCellRow: integer;
    CurCellCol: integer;
    CurCellPos: integer;
    CurCellAddress: integer;
    CurCellAddressValid: boolean;
    CurFileParserIndex: integer;
    hFile:      TFileStream;
    ChangedFont: TFont;
    SearchInfo: TSearchInfo;
    procedure SetGridCanvasFont(aFont: TFont);
  public
    { public declarations }
    FileName:    string;
    Target:      string;
    SegAddr:     int64;
    AddressOffset: int64;
    StartAddress: cardinal;
    DataByteSize: cardinal;
    DefaultData: byte;
  end;

  THexFileLineInfo = record
    Addr:     cardinal;
    ByteSize: cardinal;
    DataType: byte;
    DataOffset: byte;
    FileLineLength: cardinal;
    EmptyLeadingByteLength: cardinal;
  end;

function ReadBinFile(hFile: TFileStream; var buffer; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
function ValidateBinFile(hFile: TFileStream; var buffer; ChangeList: TMemList;
  default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
function PrepareBinFile(hFile: TFileStream; default_byte: byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
function ReadHexFile(hFile: TFileStream; var buffer; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
function ValidateHexFile(hFile: TFileStream; var buffer; ChangeList: TMemList;
  default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
function PrepareHexFile(hFile: TFileStream; default_byte: byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
function ReadS19File(hFile: TFileStream; var buffer; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
function ValidateS19File(hFile: TFileStream; var buffer; ChangeList: TMemList;
  default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
function PrepareS19File(hFile: TFileStream; default_byte: byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;

var
  FormHexEditor: TFormHexEditor;

const
  FileParser: array[0..2] of TFileParser =
    (
    (ext: '.bin'; ReadFile: @ReadBinFile; ValidateFile: @ValidateBinFile; PrepareFile: @PrepareBinFile),
    (ext: '.hex'; ReadFile: @ReadHexFile; ValidateFile: @ValidateHexFile; PrepareFile: @PrepareHexFile),
    (ext: '.s19'; ReadFile: @ReadS19File; ValidateFile: @ValidateS19File; PrepareFile: @PrepareS19File)
    );
  BYTES_IN_ROW: integer  = 16;
  DIVIDER_WIDTH: integer = 20;



implementation

function IgnoreEmptyLine(hFile: TFileStream; var lastchar: char;
  var haslastchar: boolean): cardinal;
var
  ch: char;
begin
  Result      := 0;
  haslastchar := False;
  ch          := Char(0);
  repeat
    if hFile.Read(ch, 1) <> 1 then
    begin
      exit;
    end;
    Inc(Result);
  until (ch <> char(10)) and (ch <> char(13));
  lastchar    := ch;
  haslastchar := True;
end;

function ReadS19FileLine(hFile: TFileStream; var buff;
  var LineInfo: THexFileLineInfo): boolean;
var
  P:    PByte;
  ch:   char;
  checksum: byte;
  line: string;
  i:    integer;
  headread: boolean;
  FileLineLength: cardinal;
begin
  P      := @buff;
  Result := False;
  headread := False;
  ch     := Char(0);

  FileLineLength := IgnoreEmptyLine(hFile, ch, headread);
  if (FileLineLength = 0) or (not headread) then
  begin
    Result := True;
    LineInfo.EmptyLeadingByteLength := FileLineLength;
    LineInfo.FileLineLength := 0;
    exit;
  end;
  LineInfo.EmptyLeadingByteLength := FileLineLength - 1;

  // first char MUST be S
  if (ch <> 'S') then
  begin
    exit;
  end;

  // read line
  line := '';
  repeat
    if (hFile.Read(ch, 1) <> 1) then
    begin
      break;
    end
    else if (ch = char(10)) or (ch = char(13)) then
    begin
      Inc(FileLineLength);
      break;
    end
    else
    begin
      Inc(FileLineLength);
      line := line + ch;
    end;
  until False;
  if (Length(line) < 9) or ((Length(line) mod 2) = 0)
     or (line[1] < '0') or (line[1] > '9') then
  begin
    exit;
  end;

  LineInfo.DataType := byte(line[1]) - byte('0');
  LineInfo.ByteSize := byte(StrToIntRadix(Copy(line, 2, 2), 16));
  if Length(line) <> (3 + 2 * LineInfo.ByteSize) then
  begin
    exit;
  end;

  checksum := 0;
  for i := 0 to LineInfo.ByteSize do
  begin
    (P + i)^ := byte(StrToIntRadix(Copy(line, 4 + 2 * i, 2), 16));
    Inc(checksum, (P + i)^);
  end;
  // validity check
  if byte(checksum + LineInfo.ByteSize) <> $FF then
  begin
    exit;
  end;

  LineInfo.FileLineLength := FileLineLength;
  Result := True;
end;

function ReadS19File(hFile: TFileStream; var buffer; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
var
  buff: array[0 .. 260] of byte;
  data_addr: cardinal;
  LineInfo: THexFileLineInfo;
  P: PByte;
begin
  seg_offset := seg_offset;

  p      := @buffer;
  hFile.Position := 0;
  Result := False;

  LineInfo.Addr := 0;
  LineInfo.ByteSize := 0;
  LineInfo.DataOffset := 0;
  LineInfo.DataType := 0;
  LineInfo.EmptyLeadingByteLength := 0;
  LineInfo.FileLineLength := 0;

  FillChar(buff[0], Length(buff), 0);

  while True do
  begin
    if not ReadS19FileLine(hFile, buff, LineInfo) then
    begin
      exit;
    end;
    if LineInfo.FileLineLength = 0 then
    begin
      Result := True;
      exit;
    end;

    // process data
    case LineInfo.DataType of
      0://S0
      begin

      end;
      1://S1
      begin
        data_addr := buff[1] + (buff[0] shl 8);
        if not ((data_addr < start_addr) or
          (data_addr < (addr_offset + start_addr))) then
        begin
          Dec(data_addr, start_addr);
          Dec(LineInfo.ByteSize, 3);
          if (LineInfo.ByteSize + data_addr) > bytesize then
          begin
            if data_addr > bytesize then
            begin
              LineInfo.ByteSize := 0;
            end
            else
            begin
              LineInfo.ByteSize := bytesize - data_addr;
            end;
          end;
          Move(buff[2], (P + data_addr - addr_offset)^, LineInfo.ByteSize);
        end;
      end;
      2: //S2
      begin
        data_addr := buff[2] + (buff[1] shl 16) + (buff[0] shl 8);
        if not ((data_addr < start_addr) or
          (data_addr < (addr_offset + start_addr))) then
        begin
          Dec(data_addr, start_addr);
          Dec(LineInfo.ByteSize, 4);
          if (LineInfo.ByteSize + data_addr) > bytesize then
          begin
            if data_addr > bytesize then
            begin
              LineInfo.ByteSize := 0;
            end
            else
            begin
              LineInfo.ByteSize := bytesize - data_addr;
            end;
          end;
          Move(buff[3], (P + data_addr - addr_offset)^, LineInfo.ByteSize);
        end;
      end;
      3: //S3
      begin
        data_addr := buff[3] + (buff[2] shl 24) + (buff[1] shl 16) + (buff[0] shl 8);
        if not ((data_addr < start_addr) or
          (data_addr < (addr_offset + start_addr))) then
        begin
          Dec(data_addr, start_addr);
          Dec(LineInfo.ByteSize, 5);
          if (LineInfo.ByteSize + data_addr) > bytesize then
          begin
            if data_addr > bytesize then
            begin
              LineInfo.ByteSize := 0;
            end
            else
            begin
              LineInfo.ByteSize := bytesize - data_addr;
            end;
          end;
          Move(buff[4], (P + data_addr - addr_offset)^, LineInfo.ByteSize);
        end;
      end;
      7, 8, 9:
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function ValidateS19File(hFile: TFileStream; var buffer; ChangeList: TMemList;
  default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
begin
  hFile := hFile;
  ChangeList := ChangeList;
  default_byte := default_byte;
  bytesize := bytesize;
  start_addr := start_addr;
  seg_offset := seg_offset;
  addr_offset := addr_offset;
  Result := False;
end;

function PrepareS19File(hFile: TFileStream; default_byte: byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
begin
  hFile := hFile;
  default_byte := default_byte;
  bytesize := bytesize;
  start_addr := start_addr;
  seg_offset := seg_offset;
  addr_offset := addr_offset;
  Result := False;
end;

function ReadBinFile(hFile: TFileStream; var buffer; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
var
  P: PByte;
begin
  P := @buffer;
  seg_offset := seg_offset;
  start_addr := start_addr;

  hFile.Position := addr_offset;
  hFile.Read(P^, bytesize);
  Result := True;
end;

function ValidateBinFile(hFile: TFileStream; var buffer; ChangeList: TMemList;
  default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
var
  i: integer;
  P: PByte;
begin
  P      := @buffer;
  seg_offset := seg_offset;
  default_byte := default_byte;
  bytesize := bytesize;
  Result := False;

  for i := 0 to ChangeList.Count - 1 do
  begin
    if (addr_offset + ChangeList.Items[i].StartAddr + 1 - start_addr) > hFile.Size then
    begin
      continue;
    end
    else if (addr_offset + ChangeList.Items[i].StartAddr +
      ChangeList.Items[i].ByteSize - start_addr) > hFile.Size then
    begin
      ChangeList.Items[i].ByteSize :=
        hFile.Size - addr_offset - ChangeList.Items[i].StartAddr;
    end;

    hFile.Position := addr_offset + ChangeList.Items[i].StartAddr - start_addr;
    hFile.Write((P + ChangeList.Items[i].StartAddr - start_addr)^,
      ChangeList.Items[i].ByteSize);
  end;
  Result := True;
end;

function PrepareBinFile(hFile: TFileStream; default_byte: byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
var
  buf_tmp: AnsiString;
  i: integer;
begin
  start_addr := start_addr;
  seg_offset := seg_offset;
  addr_offset := addr_offset;

  SetLength(buf_tmp, bytesize);
  for i := 0 to bytesize - 1 do
  begin
    buf_tmp[1 + i] := Char(default_byte);
  end;

  if hFile.Size <> bytesize then
  begin
    hFile.Size := bytesize;
  end;
  hFile.Position := 0;
  hFile.Write(buf_tmp[1], bytesize);
  Result := True;
end;

function ReadHexFileLine(hFile: TFileStream; var buff;
  var LineInfo: THexFileLineInfo): boolean;
var
  P:    PByte;
  ch:   char;
  checksum: byte;
  line: string;
  i:    integer;
  headread: boolean;
  FileLineLength: cardinal;
begin
  P      := @buff;
  Result := False;
  headread := False;
  ch     := Char(0);

  FileLineLength := IgnoreEmptyLine(hFile, ch, headread);
  if (FileLineLength = 0) or (not headread) then
  begin
    Result := True;
    LineInfo.EmptyLeadingByteLength := FileLineLength;
    LineInfo.FileLineLength := 0;
    exit;
  end;
  LineInfo.EmptyLeadingByteLength := FileLineLength - 1;

  // first char MUST be :
  if (ch <> ':') then
  begin
    exit;
  end;

  // read line
  line := '';
  repeat
    if (hFile.Read(ch, 1) <> 1) then
    begin
      break;
    end
    else if (ch = char(10)) or (ch = char(13)) then
    begin
      Inc(FileLineLength);
      break;
    end
    else
    begin
      Inc(FileLineLength);
      line := line + ch;
    end;
  until False;
  if (Length(line) < 10) or ((Length(line) mod 2) = 1) then
  begin
    exit;
  end;
  i := 0;
  checksum := 0;
  while i < Length(line) do
  begin
    (P + i div 2)^ := byte(StrToIntRadix(Copy(line, i + 1, 2), 16));
    Inc(checksum, (P + i div 2)^);
    Inc(i, 2);
  end;
  i := i div 2;
  // validity check
  if ((P^ + 5) <> i) or (checksum <> 0) then
  begin
    exit;
  end;

  LineInfo.ByteSize := P^;
  LineInfo.Addr := ((P + 1)^ shl 8) + (P + 2)^;
  LineInfo.DataOffset := 4;
  LineInfo.DataType := (P + 3)^;
  LineInfo.FileLineLength := FileLineLength;
  Result := True;
end;

function ReadHexFile(hFile: TFileStream; var buffer; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
var
  buff: array[0 .. 260] of byte;
  seg_addr, ext_addr0, ext_addr1, data_addr: cardinal;
  LineInfo: THexFileLineInfo;
  P: PByte;
begin
  p      := @buffer;
  seg_addr := 0;
  ext_addr0 := 0;
  ext_addr1 := 0;
  hFile.Position := 0;
  Result := False;

  LineInfo.Addr := 0;
  LineInfo.ByteSize := 0;
  LineInfo.DataOffset := 0;
  LineInfo.DataType := 0;
  LineInfo.EmptyLeadingByteLength := 0;
  LineInfo.FileLineLength := 0;

  FillChar(buff[0], Length(buff), 0);

  while True do
  begin
    if not ReadHexFileLine(hFile, buff, LineInfo) then
    begin
      exit;
    end;
    if LineInfo.FileLineLength = 0 then
    begin
      Result := True;
      exit;
    end;

    // process data
    case LineInfo.DataType of
      0://htData
      begin
        data_addr := ext_addr0 + ext_addr1 + LineInfo.Addr;
        if not ((seg_addr <> seg_offset) or (data_addr < start_addr) or
          (data_addr < (addr_offset + start_addr))) then
        begin
          Dec(data_addr, start_addr);
          if (LineInfo.ByteSize + data_addr) > bytesize then
          begin
            if data_addr > bytesize then
            begin
              LineInfo.ByteSize := 0;
            end
            else
            begin
              LineInfo.ByteSize := bytesize - data_addr;
            end;
          end;
          Move(buff[4], (P + data_addr - addr_offset)^, LineInfo.ByteSize);
        end;
      end;
      1://htEOF
      begin
        Result := True;
        exit;
      end;
      2://htLinearAddr
      begin
        if (LineInfo.ByteSize <> 2) or (LineInfo.Addr <> 0) then
        begin
          exit;
        end;
        ext_addr0 := (buff[4] shl 12) + (buff[5] shl 4);
      end;
      3://htSegAddr
      begin
        if LineInfo.Addr <> 0 then
        begin
          exit;
        end;
        seg_addr := (buff[4] shl 8) + buff[5];
      end;
      4://htExtAddr
      begin
        if (LineInfo.ByteSize <> 2) or (LineInfo.Addr <> 0) then
        begin
          exit;
        end;
        ext_addr1 := (buff[4] shl 24) + (buff[5] shl 16);
      end;
    end;
  end;
end;

function ReadHexFileLineByAddr(hFile: TFileStream; var buff;
  addr, seg: cardinal; var LineInfo: THexFileLineInfo): int64;
var
  seg_addr, ext_addr, data_addr: cardinal;
  FileLinePos: int64;
  P: PByte;
begin
  P := @buff;
  seg_addr := 0;
  ext_addr := 0;
  hFile.Position := 0;
  FilelinePos := 0;

  while True do
  begin
    if (not ReadHexFileLine(hFile, P^, LineInfo)) or
      (LineInfo.FileLineLength = 0) then
    begin
      Result := -1;
    end;

    case LineInfo.DataType of
      0://htData
      begin
        data_addr := ext_addr + LineInfo.Addr;
        if not ((seg_addr <> seg) or (data_addr > addr) or
          (addr >= (data_addr + LineInfo.ByteSize))) then
        begin
          LineInfo.Addr := addr;
          LineInfo.DataOffset := LineInfo.DataOffset + (addr - data_addr);
          LineInfo.ByteSize := LineInfo.ByteSize - (addr - data_addr);
          Result := FilelinePos + LineInfo.EmptyLeadingByteLength;
          exit;
        end;
      end;
      1://htEOF
      begin
        Result := -1;
        exit;
      end;
      2://htSegAddr
      begin
        if LineInfo.ByteSize <> 2 then
        begin
          exit;
        end;
        seg_addr := ((P + 4)^ shl 8) + (P + 5)^;
      end;
      4://htExtAddr
      begin
        if LineInfo.ByteSize <> 2 then
        begin
          exit;
        end;
        ext_addr := ((P + 4)^ shl 24) + ((P + 5)^ shl 16);
      end;
    end;
    Inc(FileLinePos, LineInfo.FileLineLength);
  end;
end;

function ValidateHexFile(hFile: TFileStream; var buffer; ChangeList: TMemList;
  default_byte: byte; bytesize, start_addr: cardinal;
  seg_offset, addr_offset: int64): boolean;
var
  P:    PByte;
  i, j: integer;
  checksum: byte;
  cur_write_len: cardinal;
  buff: array[0 .. 260] of byte;
  FileLinePos: int64;
  LineInfo: THexFileLineInfo;
  str_tmp: string;
begin
  default_byte := default_byte;
  bytesize := bytesize;
  P := @buffer;

  LineInfo.Addr := 0;
  LineInfo.ByteSize := 0;
  LineInfo.DataOffset := 0;
  LineInfo.DataType := 0;
  LineInfo.EmptyLeadingByteLength := 0;
  LineInfo.FileLineLength := 0;

  FillChar(buff[0], Length(buff), 0);

  for i := 0 to ChangeList.Count - 1 do
  begin
    while ChangeList.Items[i].ByteSize > 0 do
    begin
      FileLinePos := ReadHexFileLineByAddr(hFile, buff, start_addr +
        addr_offset + ChangeList.Items[i].StartAddr, seg_offset, LineInfo);

      if FileLinePos >= 0 then
      begin
        // found
        cur_write_len := Min(LineInfo.ByteSize, ChangeList.Items[i].ByteSize);
        Move((P + LineInfo.Addr - start_addr)^, buff[LineInfo.DataOffset],
          cur_write_len);

        hFile.Position := FileLinePos + 1 + 2 * LineInfo.DataOffset;
        for j := LineInfo.DataOffset to LineInfo.DataOffset + cur_write_len - 1 do
        begin
          str_tmp := IntToHex(buff[j], 2);
          hFile.Write(str_tmp[1], 2);
        end;
        // write checksum
        checksum := 0;
        for j := 0 to LineInfo.DataOffset + LineInfo.ByteSize - 1 do
        begin
          Inc(checksum, buff[j]);
        end;
        checksum := $100 - checksum;
        str_tmp  := IntToHex(checksum, 2);
        hFile.Position := FileLinePos + 1 + 2 * (LineInfo.DataOffset +
          LineInfo.ByteSize);
        hFile.Write(str_tmp[1], 2);

        ChangeList.Items[i].ByteSize  :=
          ChangeList.Items[i].ByteSize - cur_write_len;
        ChangeList.Items[i].StartAddr :=
          ChangeList.Items[i].StartAddr + cur_write_len;
      end
      else
      begin
        // no expanding hex file
        ChangeList.Items[i].ByteSize := 0;
      end;
    end;
  end;
  Result := True;
end;

function PrepareHexFile(hFile: TFileStream; default_byte: byte;
    bytesize, start_addr: cardinal; seg_offset, addr_offset: int64): boolean;
begin
  hFile := hFile;
  default_byte := default_byte;
  bytesize := bytesize;
  start_addr := start_addr;
  seg_offset := seg_offset;
  addr_offset := addr_offset;

  Result := False;
end;

{ TMemInfo }

constructor TMemInfo.Create(aStartAddr, aByteSize: cardinal);
begin
  inherited Create;

  StartAddr := aStartAddr;
  ByteSize  := aByteSize;
end;

function TMemInfo.GetTail: cardinal;
begin
  if (StartAddr = 0) and (ByteSize = 0) then
  begin
    Result := 0;
  end
  else
  begin
    Result := StartAddr + ByteSize - 1;
  end;
end;

{ TMemList }

procedure TMemList.SetMemInfoItem(Index: integer; aMemInfoObject: TMemInfo);
begin
  Put(Index, Pointer(aMemInfoObject));
end;

function TMemList.GetMemInfoItem(Index: integer): TMemInfo;
begin
  Result := TMemInfo(inherited Get(Index));
end;

function TMemList.GetIndexByAddr(aAddr: cardinal): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].StartAddr <= aAddr) and
      ((Items[i].StartAddr + Items[i].ByteSize) > aAddr) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TMemList.Add(aStartAddr, aByteSize: cardinal): integer;
var
  i:     integer;
  aTail, bTail: cardinal;
  found: boolean;
begin
  if aByteSize = 0 then
  begin
    Result := -1;
    exit;
  end;

  aTail := aStartAddr + aByteSize - 1;
  // find a location to insert or merge
  found := False;
  for i := 0 to Count - 1 do
  begin
    if aStartAddr <= (Items[i].Tail + 1) then
    begin
      found := True;
      break;
    end;
  end;
  if not found then
  begin
    // add last
    Result := inherited Add(TMemInfo.Create(aStartAddr, aByteSize));
    exit;
  end;
  Result := i;
  // merge or insert
  if ((aTail + 1) < Items[Result].Head) then
  begin
    // insert before
    Insert(i, TmemInfo.Create(aStartAddr, aByteSize));
  end
  else
  begin
    // merge
    bTail := Items[Result].Tail;
    Items[Result].StartAddr := Min(Items[Result].StartAddr, aStartAddr);
    Items[Result].ByteSize :=
      1 + Max(bTail, aTail) - Items[Result].StartAddr;

    // try merge the descendant
    i := i + 1;
    while i < Count do
    begin
      if (Items[Result].Tail + 1) < Items[i].Head then
      begin
        break;
      end;

      // merge current
      Items[Result].ByteSize :=
        1 + Max(Items[Result].Tail, Items[i].Tail) - Items[Result].StartAddr;
      Delete(i);

      Inc(i);
    end;
  end;
end;

function TMemList.Add(aMemInfo: TMemInfo): integer;
begin
  Result := Add(aMemInfo.StartAddr, aMemInfo.ByteSize);
end;

constructor TMemList.Create;
begin
  inherited Create(True);
end;

{ TFormHexEditor }

procedure TFormHexEditor.DirtyRowsByAddr(Addr: cardinal);
begin
  DirtyRowsByAddr(Addr, Addr);
end;

procedure TFormHexEditor.DirtyRowsByAddr(FromAddr, ToAddr: cardinal);
begin
  if ToAddr < FromAddr then
  begin
    exit;
  end;

  FromAddr := FromAddr div BYTES_IN_ROW;
  ToAddr   := ToAddr div BYTES_IN_ROW;
  Dec(ToAddr, FromAddr);
  FillChar(RowDirty[FromAddr], Max(1, ToAddr), True);
end;

procedure TFormHexEditor.DataBuffChangeRequire(addr: integer; aValue: byte);
begin
  if DataBuff[1 + addr] <> char(aValue) then
  begin
    DataBuff[1 + addr] := char(aValue);
    DataBuffChangeList.Add(addr, 1);

    if (not btnSave.Enabled) and (DataBuffChangeList.Count > 0) then
    begin
      btnSave.Enabled := True;
    end;
  end;
end;

procedure TFormHexEditor.FormCreate(Sender: TObject);
begin
  ChangedFont := TFont.Create;
  ChangedFont.Color := clRed;
end;

procedure TFormHexEditor.FormDestroy(Sender: TObject);
begin
  ChangedFont.Destroy;
end;

procedure TFormHexEditor.btnSaveClick(Sender: TObject);
var
  i: integer;
begin
  if (CurFileParserIndex >= Length(FileParser)) then
  begin
    exit;
  end;

  if (FileParser[CurFileParserIndex].ValidateFile <> nil) and
    (not FileParser[CurFileParserIndex].ValidateFile(hFile, DataBuff[1],
    DataBuffChangeList, DefaultData, DataByteSize, StartAddress,
    SegAddr, AddressOffset)) then
  begin
    Beep();
    MessageDlg('Error', 'fail to write ' + FileName + '.', mtError, [mbOK], 0);
    exit;
  end;

  // Dirty corresponding lines
  for i := 0 to DataBuffChangeList.Count - 1 do
  begin
    DirtyRowsByAddr(DataBuffChangeList.Items[i].Head,
      DataBuffChangeList.Items[i].Tail);
  end;
  DataBuffChangeList.Clear;
  // update dirty color
  sgData.Invalidate;
  btnSave.Enabled := False;
end;

procedure TFormHexEditor.btnExitClick(Sender: TObject);
begin
  if DataBuffChangeList.Count > 0 then
  begin
    if mrYes = MessageDlg('Query', 'Data changed, Save?', mtConfirmation,
      [mbYes, mbNo], 0) then
    begin
      btnSave.Click;
    end;
  end;
end;

procedure TFormHexEditor.btnSaveAsClick(Sender: TObject);
var
  SaveAsFileName, SaveAsFileExt: String;
  SaveAsFileParserIndex, i: integer;
  hSaveAsFile: TFileStream;
  MemListTmp: TMemList;
  HandleTmp: THandle;
begin
  if sdSaveAs.Execute then
  begin
    SaveAsFileName := sdSaveAs.FileName;
    SaveAsFileExt := LowerCase(ExtractFileExt(SaveAsFileName));
    SaveAsFileParserIndex := 0;
    for i := low(FileParser) to high(FileParser) do
    begin
      if SaveAsFileExt = FileParser[i].ext then
      begin
        SaveAsFileParserIndex := i;
        break;
      end;
    end;

    try
      if not FileExistsUtf8(SaveAsFileName) then
      begin
        HandleTmp := FileCreate(Utf8ToAnsi(SaveAsFileName));
        if HandleTmp = NULL then
        begin
          Beep;
          MessageDlg('Error', 'fail to create ' + SaveAsFileName + '.', mtError, [mbOK], 0);
          Exit;
        end;
        FileClose(HandleTmp);
      end;

      MemListTmp := TMemList.Create;
      MemListTmp.Clear;
      MemListTmp.Add(StartAddress, DataByteSize);
      hSaveAsFile := TFileStream.Create(Utf8ToAnsi(SaveAsFileName), fmOpenReadWrite);

      if (FileParser[SaveAsFileParserIndex].PrepareFile <> nil) and
         (FileParser[SaveAsFileParserIndex].ValidateFile <> nil) then
      begin
        if (not FileParser[SaveAsFileParserIndex].PrepareFile(hSaveAsFile,
             DefaultData, DataByteSize, StartAddress, SegAddr, AddressOffset)) or
           (not FileParser[SaveAsFileParserIndex].ValidateFile(hSaveAsFile,
             DataBuff[1], MemListTmp, DefaultData, DataByteSize, StartAddress,
             SegAddr, AddressOffset)) then
        begin
          Beep();
          MessageDlg('Error', 'fail to write ' + SaveAsFileName + '.', mtError, [mbOK], 0);
        end
        else
        begin
          Beep();
          MessageDlg('OK', 'save to ' + SaveAsFileName + ' successes.', mtInformation, [mbOK], 0);
        end;
      end
      else
      begin
        Beep();
        MessageDlg('Error', 'Sellected file type not supported.', mtError, [mbOK], 0);
      end;
    finally
      if Assigned(hSaveAsFile) then
      begin
        hSaveAsFile.Free;
      end;
      if Assigned(MemListTmp) then
      begin
        MemListTmp.Destroy;
      end;
    end;
  end;
end;

procedure TFormHexEditor.actGotoExecute(Sender: TObject);
begin
  tGoto.Enabled := True;
end;

procedure TFormHexEditor.actNextExecute(Sender: TObject);
begin
  tNext.Enabled := True;
end;

procedure TFormHexEditor.actReplaceExecute(Sender: TObject);
begin
  tReplace.Enabled := True;
end;

procedure TFormHexEditor.actSaveExecute(Sender: TObject);
begin
  btnSave.Click;
end;

procedure TFormHexEditor.actSearchExecute(Sender: TObject);
begin
  tSearch.Enabled := True;
end;

function TFormHexEditor.DoSearch(FromAddr: cardinal; ForData: string;
  UpSearch: boolean): integer;
var
  strTmp:     ansistring;
  strForData: string;
  PSrc, PDst: PByte;
  i, step:    integer;
begin
  if UpSearch then
  begin
    SetLength(strForData, Length(ForData));
    PSrc := @ForData[Length(ForData)];
    PDst := @strForData[1];
    for i := 0 to Length(strForData) - 1 do
    begin
      PDst^ := PSrc^;
      Dec(PSrc);
      Inc(PDst);
    end;

    SetLength(strTmp, FromAddr + 1);
    step := -1;
  end
  else
  begin
    strForData := ForData;
    SetLength(strTmp, DataByteSize - FromAddr);
    step := 1;
  end;

  PSrc := @DataBuff[1];
  Inc(PSrc, FromAddr);
  PDst := @strTmp[1];
  for i := 0 to Length(strTmp) - 1 do
  begin
    PDst^ := PSrc^;
    PSrc  := PSrc + step;
    Inc(PDst);
  end;

  Result := Pos(strForData, strTmp);

  if (step < 0) and (Result > 0) then
  begin
    Result := -1 * Result - Length(ForData) + 1;
  end;
end;

procedure TFormHexEditor.sgDataGoToAddr(Addr: cardinal; bHexSection: boolean);
begin
  sgData.Row := 1 + Addr div BYTES_IN_ROW;
  sgData.Col := 1 + (Addr mod BYTES_IN_ROW);

  if (sgData.Row < sgData.TopRow) or
    (sgData.Row > (sgData.TopRow + sgData.VisibleRowCount - 1)) then
  begin
    sgData.TopRow := sgData.Row;
  end;

  if not bHexSection then
  begin
    sgData.Col := sgData.Col + BYTES_IN_ROW + 1;
  end;
end;

procedure TFormHexEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(hFile) then
  begin
    hFile.Free;
  end;
  if Assigned(DataBuffChangeList) then
  begin
    DataBuffChangeList.Destroy;
  end;
  CloseAction := caHide;
end;

procedure TFormHexEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

procedure TFormHexEditor.FormShow(Sender: TObject);
var
  i:   integer;
  ext: string;
  success: boolean;
begin
  SearchInfo.Action := saNone;
  btnSave.Enabled := False;
  SearchInfo.Action := saNone;
  DataBuffChangeList := TMemList.Create;
  StrGridInited := False;
  CurCellRow := 0;
  CurCellCol := 0;
  CurCellPos := 0;
  CurCellAddress := 0;
  CurCellAddressValid := False;
  sgData.Row := 0;
  sgData.Col := 0;
  Caption := Target + ' ' + FileName;
  sgData.RowCount := 0;
  sgData.TopRow := 0;
  ActiveControl := sgData;

  // prepare memory
  DataByteSize := (DataByteSize + (BYTES_IN_ROW - 1)) div BYTES_IN_ROW * BYTES_IN_ROW;
  if DataByteSize = 0 then
  begin
    // set default size
    DataByteSize := 256 * 1024;
  end;
  SetLength(DataBuff, DataByteSize);
  FillChar(DataBuff[1], DataByteSize, DefaultData);

  // open file and read
  if FileExistsUtf8(FileName) then
  begin
    ext := LowerCase(ExtractFileExt(FileName));
    CurFileParserIndex := 0;
    for i := low(FileParser) to high(FileParser) do
    begin
      if ext = FileParser[i].ext then
      begin
        CurFileParserIndex := i;
        break;
      end;
    end;
    success := True;
    try
      hFile := TFileStream.Create(Utf8ToAnsi(FileName), fmOpenReadWrite);
      if FileParser[CurFileParserIndex].ReadFile <> nil then
      begin
        if not FileParser[CurFileParserIndex].ReadFile(hFile,
          DataBuff[1], DataByteSize, StartAddress, SegAddr, AddressOffset) then
        begin
          success := False;
          Beep();
          MessageDlg('Error', 'fail to read ' + FileName + '.', mtError, [mbOK], 0);
        end;
      end;
    except
      begin
        success := False;
        Beep();
        MessageDlg('Error', 'File Open Failed:' + FileName, mtError, [mbOK], 0);
      end;
    end;
    if not success then
    begin
      Close;
      exit;
    end;
  end
  else
  begin
    Beep();
    MessageDlg('Error', FileName + ' not exists.', mtError, [mbOK], 0);
    Close;
    exit;
  end;

  // adjust GUI and display data
  sgData.RowCount := 1 + DataByteSize div BYTES_IN_ROW;
  SetLength(RowDirty, DataByteSize div BYTES_IN_ROW);
  DirtyRowsByAddr(0, DataByteSize);
  sgData.ColCount  := 2 + 2 * BYTES_IN_ROW;
  sgData.FixedRows := 1;
  sgData.FixedCols := 1;
  for i := 1 to BYTES_IN_ROW do
  begin
    sgData.Cells[i, 0] := UpperCase(IntToHex(i - 1, 1));
  end;
  sgData.Row := 1;
  sgData.Col := 1;

  tInit.Enabled := True;
end;

procedure TFormHexEditor.sgDataKeyPress(Sender: TObject; var Key: char);
var
  i:      integer;
  Value:  byte;
  IncPos: integer;
begin
  IncPos := 0;

  if CurCellAddressValid then
  begin
    // data input
    if CurCellCol > 1 + BYTES_IN_ROW then
    begin
      // input char data
      Value := byte(Key);
      Key   := char(0);
      // change data
      DataBuffChangeRequire(CurCellAddress, Value);
      IncPos := 2 + BYTES_IN_ROW;
    end
    else
    begin
      // input hex value
      Key := AnsiStrUpper(@Key)^;
      i   := Pos(Key, HEX_PARSE_STR);
      Key := char(0);
      if (i < 1) or (i > 16) then
      begin
        exit;
      end;
      Value := i - 1;
      // valid input, change data
      case CurCellPos of
        0:
        begin
          Value := (byte(DataBuff[1 + CurCellAddress]) and $0F) or (Value shl 4);
          DataBuffChangeRequire(CurCellAddress, Value);
          Inc(CurCellPos);
        end;
        1:
        begin
          Value := (byte(DataBuff[1 + CurCellAddress]) and $F0) or Value;
          DataBuffChangeRequire(CurCellAddress, Value);
          IncPos := 1;
        end;
      end;
    end;

    DirtyRowsByAddr(CurCellAddress);
    // increase address if required
    if (IncPos > 0) and (not (CurCellAddress = DataByteSize - 1)) then
    begin
      if (CurCellAddress mod BYTES_IN_ROW) = (BYTES_IN_ROW - 1) then
      begin
        SgData.Col := IncPos;
        sgData.Row := sgData.Row + 1;
      end
      else
      begin
        sgData.Col := sgData.Col + 1;
      end;
    end;

    // update dirty data
    sgDataTopLeftChanged(sgData);
  end;
end;

procedure TFormHexEditor.SetGridCanvasFont(aFont: TFont);
const
  lastFont: TFont = nil;
begin
  if (aFont <> nil) and (aFont <> lastFont) then
  begin
    sgData.Canvas.Font := aFont;
//    lastFont := aFont;
  end;
end;

procedure TFormHexEditor.sgDataPrepareCanvas(Sender: TObject;
  aCol, aRow: integer; aState: TGridDrawState);
var
  addr: cardinal;
begin
  if gdFixed in aState then
  begin
    SetGridCanvasFont((Sender as TStringGrid).TitleFont);
  end
  else if (aRow > 0) and (aCol > 0) then
  begin
    if aCol > (1 + BYTES_IN_ROW) then
    begin
      Dec(aCol, 1 + BYTES_IN_ROW);
    end;

    addr := (aRow - 1) * BYTES_IN_ROW + aCol - 1;
    if DataBuffChangeList.GetIndexByAddr(addr) >= 0 then
    begin
      SetGridCanvasFont(ChangedFont);
    end
    else
    begin
      SetGridCanvasFont((Sender as TStringGrid).Font);
    end;
  end;
end;

procedure TFormHexEditor.sgDataSelectCell(Sender: TObject; aCol, aRow: integer;
  var CanSelect: boolean);
begin
  if aCol = (1 + BYTES_IN_ROW) then
  begin
    CanSelect := False;
  end
  else if (CurCellRow <> aRow) or (CurCellCol <> aCol) then
  begin
    CanSelect  := True;
    CurCellPos := 0;
    CurCellRow := aRow;
    CurCellCol := aCol;

    if (sgData.ColCount > 0) and (sgData.RowCount > 0) then
    begin
      if aCol > 1 + BYTES_IN_ROW then
      begin
        Dec(aCol, BYTES_IN_ROW + 1);
      end;

      CurCellAddressValid := True;
      CurCellAddress      := (aRow - 1) * BYTES_IN_ROW + aCol - 1;
      SearchInfo.FromPos  := CurCellAddress;
      sgData.Cells[0, 0]  := IntToHex(StartAddress + CurCellAddress, 8);
    end;
  end;
end;

procedure TFormHexEditor.sgDataTopLeftChanged(Sender: TObject);
var
  i, j: integer;
begin
  // update data
  for i := sgData.TopRow to sgData.TopRow + sgData.VisibleRowCount do
  begin
    if (i < sgData.RowCount) and RowDirty[i - 1] then
    begin
      sgData.Cells[0, i] := UpperCase(IntToHex(StartAddress + (i - 1) * $10, 8));
      for j := 1 to BYTES_IN_ROW do
      begin
        sgData.Cells[j, i] := UpperCase(IntToHex(byte(DataBuff[(i - 1) * 16 + j]), 2));
      end;
      for j := 2 + BYTES_IN_ROW to 1 + 2 * BYTES_IN_ROW do
      begin
        sgData.Cells[j, i] := char(DataBuff[(i - 1) * 16 + (j - 1 - BYTES_IN_ROW)]);
      end;
      RowDirty[i - 1] := False;
    end;
  end;
end;

procedure TFormHexEditor.tGotoTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  sgDataDoGoto;
end;

procedure TFormHexEditor.sgDataDoGoto();
var
  goto_addr: cardinal;
begin
  FormInputDialog.CommonMaxLength := 8;
  FormInputDialog.CommonCase := scUpper;
  FormInputDialog.InputType := itNumeric;
  FormInputDialog.NumMax    := 0;
  FormInputDialog.NumMin    := 0;
  FormInputDialog.NumRadix  := nrHexadecimal;
  FormInputDialog.CommonPrefix := '0x';
  FormInputDialog.Caption   := 'Goto: input hex address';
  if FormInputDialog.ShowModal = mrOk then
  begin
    goto_addr := FormInputDialog.GetNumber;
    if (goto_addr >= StartAddress) and (goto_addr < (StartAddress + DataByteSize)) then
    begin
      Dec(goto_addr, StartAddress);
      sgDataGoToAddr(goto_addr, True);
      Beep;
    end;
  end;
end;

procedure TFormHexEditor.sgDataDoSearch();
var
  FirstSearch: boolean;
  DiffPos:     integer;
begin
  FirstSearch := SearchInfo.Action <> saNextSearch;
  if FirstSearch then
  begin
    FormFindReplace.IsReplace := False;
    FormFindReplace.ShowModal;
    if FormFindReplace.SearchInfo.Action <> saNone then
    begin
      // input data to search for
      SearchInfo := FormFindReplace.SearchInfo;
      if CurCellAddressValid then
      begin
        SearchInfo.FromPos := CurCellAddress;
      end
      else
      begin
        SearchInfo.FromPos := 0;
      end;
    end
    else
    begin
      exit;
    end;
  end;

  if SearchInfo.UpSearch then
  begin
    if SearchInfo.FromPos < 1 then
    begin
      DiffPos := 0;
    end
    else
    begin
      DiffPos := DoSearch(SearchInfo.FromPos - 1, SearchInfo.ForData,
        SearchInfo.UpSearch);
    end;
  end
  else
  begin
    if SearchInfo.FromPos > (DataByteSize - 1) then
    begin
      DiffPos := 0;
    end
    else
    begin
      DiffPos := DoSearch(SearchInfo.FromPos + 1, SearchInfo.ForData,
        SearchInfo.UpSearch);
    end;
  end;
  Beep;
  if DiffPos <> 0 then
  begin
    // found
    SearchInfo.FoundPos := SearchInfo.FromPos + DiffPos;
    sgDataGoToAddr(SearchInfo.FoundPos, True);
  end
  else
  begin
    // not found in the first search, show message
    MessageDlg('Error', SearchInfo.ForDataStr + ' not found.', mtError, [mbOK], 0);
  end;
end;

procedure TFormHexEditor.sgDataDoReplace();
var
  ReplaceOption: integer;
  FirstSearch: boolean;
  DiffPos:    integer;
  PSrc, PDst: PByte;
begin
  FirstSearch := SearchInfo.Action <> saNextReplace;
  if FirstSearch then
  begin
    FormFindReplace.IsReplace := True;
    FormFindReplace.ShowModal;
    if FormFindReplace.SearchInfo.Action <> saNone then
    begin
      // input data to search for
      SearchInfo := FormFindReplace.SearchInfo;
      // check valid, ForData shoud be the same size as WithData
      if Length(SearchInfo.ForData) <> Length(SearchInfo.WithData) then
      begin
        Beep();
        MessageDlg('Error', 'Replace String is not the same size.', mtError, [mbOK], 0);
      end;

      if CurCellAddressValid then
      begin
        SearchInfo.FromPos := CurCellAddress;
      end
      else
      begin
        SearchInfo.FromPos := 0;
      end;
    end
    else
    begin
      exit;
    end;
  end;

  if SearchInfo.UpSearch then
  begin
    if SearchInfo.FromPos < 1 then
    begin
      DiffPos := 0;
    end
    else
    begin
      DiffPos := DoSearch(SearchInfo.FromPos - 1, SearchInfo.ForData,
        SearchInfo.UpSearch);
    end;
  end
  else
  begin
    if SearchInfo.FromPos > (DataByteSize - 1) then
    begin
      DiffPos := 0;
    end
    else
    begin
      DiffPos := DoSearch(SearchInfo.FromPos + 1, SearchInfo.ForData,
        SearchInfo.UpSearch);
    end;
  end;
  Beep;
  if DiffPos <> 0 then
  begin
    // found
    SearchInfo.FoundPos := SearchInfo.FromPos + DiffPos;
    sgDataGoToAddr(SearchInfo.FoundPos, True);

    if SearchInfo.PromptDis then
    begin
      ReplaceOption := mrYesToAll;
    end
    else
    begin
      ReplaceOption := MessageDlg('Query', 'replace with ' +
        SearchInfo.WithData + '?', mtConfirmation, [mbYes, mbYesToAll, mbIgnore], 0);
    end;

    if (ReplaceOption = mrYes) or (ReplaceOption = mrYesToAll) then
    begin
      // replace current
      PSrc := @SearchInfo.WithData[1];
      PDst := @DataBuff[1 + SearchInfo.FoundPos];
      for DiffPos := 0 to Length(SearchInfo.WithData) - 1 do
      begin
        if (PDst + DiffPos)^ <> (PSrc + DiffPos)^ then
        begin
          DataBuffChangeList.Add(SearchInfo.FoundPos + DiffPos, 1);
          (PDst + DiffPos)^ := (PSrc + DiffPos)^;
        end;
      end;
      DirtyRowsByAddr(SearchInfo.FoundPos,
        SearchInfo.FoundPos - 1 + Length(SearchInfo.WithData));
      // Update Data
      sgDataTopLeftChanged(sgData);
    end;
    if (ReplaceOption = mrYesToAll) then
    begin
      // replace all
      SearchInfo.PromptDis := True;
      SearchInfo.Action    := saNextReplace;
      sgDataDoReplace();
      if SearchInfo.FoundPos = 0 then
      begin
        // has replaced
        exit;
      end;
    end;
  end
  else if FirstSearch or (not SearchInfo.PromptDis) then
  begin
    // not found in the first search, show message
    MessageDlg('Error', SearchInfo.ForDataStr + ' not found.', mtError, [mbOK], 0);
  end;
end;

procedure TFormHexEditor.sgDataDoNext();
begin
  case SearchInfo.Action of
    saNone:
      exit;
    saSearch:
    begin
      SearchInfo.Action := saNextSearch;
      sgDataDoSearch();
      SearchInfo.Action := saSearch;
    end;
    saReplace:
    begin
      SearchInfo.Action := saNextReplace;
      sgDataDoReplace();
      SearchInfo.Action := saReplace;
    end;
    saNextSearch:
    begin
      sgDataDoSearch();
    end;
    saNextReplace:
    begin
      sgDataDoReplace();
    end;
  end;
end;

procedure TFormHexEditor.tNextTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  sgDataDoNext;
end;

procedure TFormHexEditor.tReplaceTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  sgDataDoReplace;
end;

procedure TFormHexEditor.tSearchTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  sgDataDoSearch;
end;

procedure TFormHexEditor.tInitTimer(Sender: TObject);
var
  i: integer;
  tmpWidth, allWidth: integer;
begin
  (Sender as TTimer).Enabled := False;
  allWidth := 0;

  lblMeasureSize.Caption := '000000000';
  lblMeasureSize.AdjustSize;
  tmpWidth := lblMeasureSize.Width;
  sgData.ColWidths[0] := tmpWidth;
  Inc(allWidth, tmpWidth);

  lblMeasureSize.Caption := '000';
  lblMeasureSize.AdjustSize;
  tmpWidth := lblMeasureSize.Width;
  for i := 1 to BYTES_IN_ROW do
  begin
    sgData.ColWidths[i] := tmpWidth;
  end;
  Inc(allWidth, BYTES_IN_ROW * tmpWidth);

  sgData.ColWidths[BYTES_IN_ROW + 1] := DIVIDER_WIDTH;
  Inc(allWidth, DIVIDER_WIDTH);

  lblMeasureSize.Caption := '00';
  lblMeasureSize.AdjustSize;
  tmpWidth := lblMeasureSize.Width;
  for i := 2 + BYTES_IN_ROW to 1 + 2 * BYTES_IN_ROW do
  begin
    sgData.ColWidths[i] := tmpWidth;
  end;
  Inc(allWidth, BYTES_IN_ROW * tmpWidth);

  sgData.Align := alCustom;
  sgData.Width := allWidth;
  Width := pnlData.BevelWidth * 2 + sgData.Width + 20;
  sgData.Align := alClient;

  // Center Buttons
  btnSave.Left := (pnlButton.Width div 2 - btnSave.Width) div 2;
  btnExit.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnSave.Width) div 2;

  StrGridInited := True;
  sgDataTopLeftChanged(sgData);
  UpdateShowing();
end;

initialization
  {$I hexeditor.lrs}

end.

