unit texteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TFormTextEditor }

  TFormTextEditor = class(TForm)
    memoText: TMemo;
    btnSave: TButton;
    btnClose: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure memoTextKeyPress(Sender: TObject; var Key: char);
    procedure UpdateTitle;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormTextEditor: TFormTextEditor;
  TextFileName: String;
  TextContentChanged: boolean;

implementation

{ TFormTextEditor }

procedure TFormTextEditor.UpdateTitle;
begin
  if TextContentChanged then
  begin
    Caption := 'TextEditor*: ' + TextFileName;
  end
  else
  begin
    Caption := 'TextEditor: ' + TextFileName;
  end;
end;

procedure TFormTextEditor.FormShow(Sender: TObject);
var
  tmpStrList: TStringList;
  i: integer;
begin
  if not FileExistsUtf8(TextFileName) then
  begin
    Beep;
    MessageDlg('Error', TextFileName + ' not exists.', mtError, [mbOK], 0);
    Close;
    Exit;
  end;

  TextContentChanged := False;
  UpdateTitle;
  memoText.Hint := TextFileName;

  tmpStrList := TStringList.Create;
  tmpStrList.LoadFromFile(TextFileName);

  memoText.Lines.Clear;
  for i := 0 to tmpStrList.Count - 1 do
  begin
    memoText.Lines.Add(tmpStrList.Strings[i]);
  end;

  tmpStrList.Destroy;
end;

procedure TFormTextEditor.memoTextKeyPress(Sender: TObject; var Key: char);
begin
  Key := Key;

  TextContentChanged := True;
  UpdateTitle;
end;

procedure TFormTextEditor.btnSaveClick(Sender: TObject);
begin
  if TextContentChanged then
  begin
    memoText.Lines.SaveToFile(TextFileName);
    TextContentChanged := False;
    UpdateTitle;
    Beep();
    MessageDlg('OK', 'save to ' + TextFileName + ' successes.', mtInformation, [mbOK], 0);
  end;
end;

procedure TFormTextEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(27) then
  begin
    close;
  end;
end;

initialization
  {$I texteditor.lrs}

end.

