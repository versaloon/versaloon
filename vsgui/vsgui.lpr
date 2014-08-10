program vsgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  LResources,
  main,
  cli_caller,
  parameditor,
  com_setup,
  fileselector,
  hexeditor,
  inputdialog,
  findreplace,
  vsprogparser,
  vsprogtarget,
  vsprogprogrammer,
  texteditor,
  strparser;

{$IFDEF WINDOWS}{$R vsgui.rc}{$ENDIF}

begin
  {$I vsgui.lrs}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormComSetup, FormComSetup);
  Application.CreateForm(TFormParaEditor, FormParaEditor);
  Application.CreateForm(TFormFileSelector, FormFileSelector);
  Application.CreateForm(TFormHexEditor, FormHexEditor);
  Application.CreateForm(TFormInputDialog, FormInputDialog);
  Application.CreateForm(TFormFindReplace, FormFindReplace);
  Application.CreateForm(TVSProg_Programmer, VSProg_Programmer);
  Application.CreateForm(TFormTextEditor, FormTextEditor);
  Application.Run;
end.

