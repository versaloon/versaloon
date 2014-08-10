unit fileselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, EditBtn, vsprogtarget;

type
  TTargetFile = record
    target:   char;
    filename: string;
  end;

  { TFormFileSelector }

  TFormFileSelector = class(TForm)
    btnOK:     TButton;
    btnCancel: TButton;
    pnlMain:   TPanel;
    pnlButton: TPanel;
    tInit: TTimer;
    procedure btnOKClick(Sender: TObject);
    procedure FileNameEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure CenterControl(ctl: TControl; ref: TControl);
    procedure tInitTimer(Sender: TObject);
  private
    { private declarations }
    FileNameLabelArr: array of TLabel;
    FileNameEdtArr:   array of TFileNameEdit;
    procedure AddFileSetting(target: char; filename: string);
  public
    { public declarations }
    procedure Reset();
  end;

  procedure RemoveTargetFile(TargetName: char);
  procedure AddTargetFile(TargetName: char);
  function GetTargetFileIdx(TargetName: char): integer;

var
  FormFileSelector: TFormFileSelector;
  TargetFile:  array of TTargetFile;

const
  LEFT_MARGIN: integer   = 10;
  RIGHT_MARGIN: integer  = 10;
  TOP_MARGIN: integer    = 10;
  BOTTOM_MARGIN: integer = 10;
  X_MARGIN: integer      = 4;
  Y_MARGIN: integer      = 4;
  ITEM_HEIGHT: integer   = 20;
  FILELABEL_WIDTH: integer = 100;
  FILEEDIT_WIDTH: integer = 400;

implementation

procedure RemoveTargetFile(TargetName: char);
var
  i, j:  integer;
  found: boolean;
begin
  found := False;
  for i := low(TargetFile) to high(TargetFile) do
  begin
    if TargetFile[i].target = TargetName then
    begin
      found := True;
    end;
  end;
  if found then
  begin
    for j := i to high(TargetFile) - 1 do
    begin
      TargetFile[j] := TargetFile[j + 1];
    end;
    SetLength(TargetFile, Length(TargetFile) - 1);
  end;
end;

procedure AddTargetFile(TargetName: char);
var
  i:     integer;
  found: boolean;
begin
  found := False;
  for i := low(TargetFile) to high(TargetFile) do
  begin
    if TargetFile[i].target = TargetName then
    begin
      found := True;
    end;
  end;
  if not found then
  begin
    SetLength(TargetFile, Length(TargetFile) + 1);
    i := Length(TargetFile) - 1;
    TargetFile[i].target := TargetName;
    TargetFile[i].filename := '';
  end;
end;

function GetTargetFileIdx(TargetName: char): integer;
var
  i: integer;
begin
  Result := -1;
  if Length(TargetFile) > 0 then
  begin
    for i := low(TargetFile) to high(TargetFile) do
    begin
      if TargetFile[i].target = TargetName then
      begin
        Result := i;
        exit;
      end;
    end;
  end;
end;

{ TFormFileSelector }

procedure TFormFileSelector.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormFileSelector.tInitTimer(Sender: TObject);
var
  i: integer;
begin
  (Sender as TTimer).Enabled := False;

  // center buttons
  btnOK.Left   := (pnlButton.Width div 2 - btnOK.Width) div 2;
  btnCancel.Left := pnlButton.Width div 2 + (pnlButton.Width div 2 - btnOK.Width) div 2;

  for i := low(TargetFile) to high(TargetFile) do
  begin
    FileNameLabelArr[i].Top  := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT);
    FileNameLabelArr[i].Left := LEFT_MARGIN;
    FileNameLabelArr[i].Width := FILELABEL_WIDTH;
    FileNameLabelArr[i].Height := ITEM_HEIGHT;

    FileNameEdtArr[i].Top  := TOP_MARGIN + i * (Y_MARGIN + ITEM_HEIGHT);
    FileNameEdtArr[i].Left := LEFT_MARGIN + X_MARGIN + FILELABEL_WIDTH;
    FileNameEdtArr[i].Width := FILEEDIT_WIDTH - FileNameEdtArr[i].ButtonWidth;
    FileNameEdtArr[i].Height := ITEM_HEIGHT;
    CenterControl(FileNameLabelArr[i], FileNameEdtArr[i]);
  end;
end;

procedure TFormFileSelector.Reset();
var
  i: integer;
begin
  for i := low(FileNameLabelArr) to high(FileNameLabelArr) do
  begin
    if Assigned(FileNameLabelArr[i]) then
    begin
      FileNameLabelArr[i].Destroy;
    end;
  end;
  SetLength(FileNameLabelArr, 0);

  for i := low(FileNameEdtArr) to high(FileNameEdtArr) do
  begin
    if Assigned(FileNameEdtArr[i]) then
    begin
      FileNameEdtArr[i].Destroy;
    end;
  end;
  SetLength(FileNameEdtArr, 0);
end;

procedure TFormFileSelector.AddFileSetting(target: char; filename: string);
var
  i:     integer;
  found: boolean;
  str:   string;
begin
  found := False;
  for i := low(FileNameLabelArr) to high(FileNameLabelArr) do
  begin
    if FileNameLabelArr[i].Caption = target then
    begin
      found := True;
    end;
  end;

  if found then
  begin
    FileNameEdtArr[i].FileName := filename;
  end
  else
  begin
    SetLength(FileNameEdtArr, Length(FileNameEdtArr) + 1);
    SetLength(FileNameLabelArr, Length(FileNameLabelArr) + 1);
    i := Length(FileNameLabelArr) - 1;

    FileNameLabelArr[i]      := TLabel.Create(Self);
    FileNameLabelArr[i].Parent := pnlMain;
    FileNameLabelArr[i].Caption := GetAreaFullName(target);
    FileNameLabelArr[i].Hint := target;
    FileNameLabelArr[i].ShowHint := True;

    FileNameEdtArr[i]      := TFileNameEdit.Create(Self);
    FileNameEdtArr[i].Parent := pnlMain;
    FileNameEdtArr[i].FileName := filename;
    FileNameEdtArr[i].Filter := 'HEX File|*.hex|BIN File|*.bin|S19 File|*.s19';
    FileNameEdtArr[i].Hint := filename;
    FileNameEdtArr[i].Flat := True;
    FileNameEdtArr[i].ShowHint := True;
    FileNameEdtArr[i].OnChange := @FileNameEditChange;
    FileNameEdtArr[i].OnEditingDone := @FileNameEditChange;

    str := LowerCase(ExtractFileExt(filename));
    if (str = '.hex') or (str = '') then
    begin
      FileNameEdtArr[i].FilterIndex := 1;
    end
    else if str = '.bin' then
    begin
      FileNameEdtArr[i].FilterIndex := 2;
    end
    else if str = '.s19' then
    begin
      FileNameEdtArr[i].FilterIndex := 3;
    end;
  end;
end;

procedure TFormFileSelector.FormDestroy(Sender: TObject);
begin
  Reset();
end;

procedure TFormFileSelector.FileNameEditChange(Sender: TObject);
var
  str: string;
begin
  (Sender as TFileNameEdit).Hint := (Sender as TFileNameEdit).FileName;

  str := LowerCase(ExtractFileExt((Sender as TFileNameEdit).filename));
  if (str = '.hex') or (str = '') then
  begin
    (Sender as TFileNameEdit).FilterIndex := 1;
  end
  else if str = '.bin' then
  begin
    (Sender as TFileNameEdit).FilterIndex := 2;
  end
  else if str = '.s19' then
  begin
    (Sender as TFileNameEdit).FilterIndex := 3;
  end;
end;

procedure TFormFileSelector.btnOKClick(Sender: TObject);
var
  i: integer;
begin
  // Update TargetFile
  for i := low(TargetFile) to high(TargetFile) do
  begin
    TargetFile[i].filename := FileNameEdtArr[i].FileName;
  end;
end;

procedure TFormFileSelector.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

procedure TFormFileSelector.FormShow(Sender: TObject);
var
  intTmp: integer;
begin
  for intTmp := low(TargetFile) to high(TargetFile) do
  begin
    AddFileSetting(TargetFile[intTmp].target, TargetFile[intTmp].filename);
  end;

  intTmp       := LEFT_MARGIN + RIGHT_MARGIN + FILELABEL_WIDTH +
    X_MARGIN + FILEEDIT_WIDTH;
  ClientWidth  := intTmp;
  ClientHeight := TOP_MARGIN + BOTTOM_MARGIN + Length(FileNameLabelArr) *
    (Y_MARGIN + ITEM_HEIGHT) + pnlButton.Height;
  pnlMain.Width := intTmp;
  pnlButton.Width := intTmp;

  UpdateShowing;

  tInit.Enabled := True;
end;

initialization
  {$I fileselector.lrs}

end.

