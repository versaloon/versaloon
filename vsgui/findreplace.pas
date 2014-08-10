unit findreplace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, inputdialog;

type

  TSearchAction = (saNone, saSearch, saReplace, saNextSearch, saNextReplace);

  TSearchInfo = record
    Action:      TSearchAction;
    PromptDis:   boolean;
    FromPos:     cardinal;
    FoundPos:    cardinal;
    UpSearch:    boolean;
    ForDataStr:  string;
    ForData:     string;
    WithDataStr: string;
    WithData:    string;
  end;

  { TFormFindReplace }

  TFormFindReplace = class(TForm)
    btnOK:      TButton;
    btnCancel:  TButton;
    cbboxDataType: TComboBox;
    cbboxSearch: TComboBox;
    cbboxReplace: TComboBox;
    chkboxUnicode: TCheckBox;
    chkboxUp:   TCheckBox;
    chkboxReplaceAll: TCheckBox;
    lblDataType: TLabel;
    lblReplace: TLabel;
    lblSearch:  TLabel;
    tInit: TTimer;
    procedure CenterControl(ctl: TControl; ref: TControl);
    procedure AdjustComponentColor(Sender: TControl);
    procedure btnOKClick(Sender: TObject);
    procedure cbboxDataTypeChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure tInitTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    SearchInfo: TSearchInfo;
    IsReplace:  boolean;
  end;

var
  FormFindReplace: TFormFindReplace;

implementation

function ConvertToHex(Src: string; var Dst: string): boolean;
var
  tmpStr: string;
  i:      integer;
begin
  Result := False;
  tmpStr := '';
  for i := 0 to Length(Src) - 1 do
  begin
    if Pos(UpperCase(Src[1 + i]), HEX_PARSE_STR) > 0 then
    begin
      tmpStr := tmpStr + Src[1 + i];
      if Length(tmpStr) >= 2 then
      begin
        Dst    := Dst + char(StrToIntRadix(tmpStr, 16));
        tmpStr := '';
      end;
    end
    else if (Src[1 + i] = ' ') then
    begin
      if tmpStr <> '' then
      begin
        Dst    := Dst + char(StrToIntRadix(tmpStr, 16));
        tmpStr := '';
      end;
    end
    else
    begin
      exit;
    end;
  end;
  Result := True;
end;

{ TFormFindReplace }

procedure TFormFindReplace.CenterControl(ctl: TControl; ref: TControl);
begin
  ctl.Top := ref.Top + (ref.Height - ctl.Height) div 2;
end;

procedure TFormFindReplace.AdjustComponentColor(Sender: TControl);
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

procedure TFormFindReplace.btnOKClick(Sender: TObject);
var
  i:    integer;
  tmp_unicode: WideString;
  PSrc: PByte;
begin
  if cbboxSearch.Text = '' then
  begin
    Beep();
    MessageDlg('Error', 'Search for what?', mtError, [mbOK], 0);
    exit;
  end;
  if (cbboxSearch.Items.IndexOf(cbboxSearch.Text) < 0) then
  begin
    cbboxSearch.Items.Add(cbboxSearch.Text);
  end;
  if (cbboxReplace.Items.IndexOf(cbboxReplace.Text) < 0) then
  begin
    cbboxReplace.Items.Add(cbboxReplace.Text);
  end;

  SearchInfo.PromptDis   := not chkboxReplaceAll.Checked;
  SearchInfo.UpSearch    := chkboxUp.Checked;
  SearchInfo.ForDataStr  := cbboxSearch.Text;
  SearchInfo.WithDataStr := cbboxReplace.Text;
  if cbboxDataType.Text = 'Text-string' then
  begin
    if chkboxUnicode.Checked then
    begin
      tmp_unicode := SearchInfo.ForDataStr;
      PSrc := @tmp_unicode[1];
      SearchInfo.ForData := '';
      for i := 0 to 2 * Length(tmp_unicode) - 1 do
      begin
        SearchInfo.ForData := SearchInfo.ForData + char(PSrc^);
        Inc(PSrc);
      end;

      tmp_unicode := SearchInfo.WithDataStr;
      PSrc := @tmp_unicode[1];
      SearchInfo.WithData := '';
      for i := 0 to 2 * Length(tmp_unicode) - 1 do
      begin
        SearchInfo.WithData := SearchInfo.WithData + char(PSrc^);
        Inc(PSrc);
      end;
    end
    else
    begin
      SearchInfo.ForData  := SearchInfo.ForDataStr;
      SearchInfo.WithData := SearchInfo.WithDataStr;
    end;
  end
  else if cbboxDataType.Text = 'Hex-values' then
  begin
    SearchInfo.ForData  := '';
    SearchInfo.WithData := '';
    if not ConvertToHex(SearchInfo.ForDataStr, SearchInfo.ForData) then
    begin
      Beep();
      MessageDlg('Error', 'fail to parse hex value.', mtError, [mbOK], 0);
    end;

    if not ConvertToHex(SearchInfo.WithDataStr, SearchInfo.WithData) then
    begin
      Beep();
      MessageDlg('Error', 'fail to parse hex value.', mtError, [mbOK], 0);
    end;
  end;

  if IsReplace then
  begin
    SearchInfo.Action := saReplace;
  end
  else
  begin
    SearchInfo.Action := saSearch;
  end;
  SearchInfo.FoundPos := 0;
  Close;
end;

procedure TFormFindReplace.cbboxDataTypeChange(Sender: TObject);
begin
  if (Sender as TComboBox).Text = 'Text-string' then
  begin
    chkboxUnicode.Enabled := True;
  end
  else if (Sender as TComboBox).Text = 'Hex-values' then
  begin
    chkboxUnicode.Enabled := False;
  end;
end;

procedure TFormFindReplace.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

procedure TFormFindReplace.FormShow(Sender: TObject);
begin
  SearchInfo.Action := saNone;
  if IsReplace then
  begin
    chkboxReplaceAll.Enabled := True;
    cbboxReplace.Enabled     := True;
  end
  else
  begin
    chkboxReplaceAll.Checked := False;
    chkboxReplaceAll.Enabled := False;
    cbboxReplace.Enabled     := False;
  end;
  AdjustComponentColor(cbboxReplace);
  cbboxReplace.ItemIndex := -1;
  cbboxSearch.ItemIndex  := -1;
  ActiveControl := cbboxSearch;

  tInit.Enabled := True;
end;

procedure TFormFindReplace.tInitTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;

  CenterControl(lblSearch, cbboxSearch);
  CenterControl(lblReplace, cbboxReplace);
  CenterControl(lblDataType, cbboxDataType);
end;

initialization
  {$I findreplace.lrs}

end.

