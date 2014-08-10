unit cli_caller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, SyncObjs;

type
  TCLI_Callback = function(line: string): boolean of object;

  TCLI_Caller = class(TObject)
    function Take(): boolean;
    procedure UnTake();
    procedure AddParameter(para: string);
    procedure AddParametersString(para: string);
    procedure RemoveAllParameters();
    procedure Run(callback: TCLI_Callback; bView: boolean; bTimeout: boolean);
    procedure Stop();
    function IsRunning(): boolean;
  private
    { private declarations }
    FApplication: string;
    FParameter: string;
    FDelimiter: string;
    FProcessMessage: boolean;
    FP:     TProcess;
    FRunning: boolean;
    FTaken: boolean;
    FCriticalSection: TCriticalSection;
  public
    { public declarations }
    constructor Create;
    destructor Destroy; override;
    property Application: string Read FApplication Write FApplication;
    property Delimiter: string Read FDelimiter Write FDelimiter;
    property bProcessMessage: boolean Read FProcessMessage Write FProcessMessage;
  end;

const
  BUF_INC = 1024;

implementation

constructor TCLI_Caller.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FProcessMessage := True;
end;

destructor TCLI_Caller.Destroy;
begin
  inherited Destroy;
  FCriticalSection.Destroy;
  FP := nil;
end;

procedure TCLI_Caller.AddParameter(para: string);
begin
  FParameter := FParameter + ' ' + FDelimiter + para;
end;

procedure TCLI_Caller.AddParametersString(para: string);
begin
  FParameter := FParameter + ' ' + para;
end;

procedure TCLI_Caller.RemoveAllParameters();
begin
  FParameter := '';
end;

function TCLI_Caller.Take: boolean;
begin
  FCriticalSection.Enter;
  try
    Result := False;
    if not IsRunning then
    begin
      FTaken := True;
      Result := True;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TCLI_Caller.UnTake;
begin
  if ((FP = nil) or (not FP.Running)) and FRunning then
  begin
    FRunning := False;
  end;
  FTaken := False;
end;

function TCLI_Caller.IsRunning(): boolean;
begin
  if ((FP <> nil) and FP.Running) or FRunning or FTaken then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TCLI_Caller.Stop();
begin
  if (FP <> nil) and FP.Running then
  begin
    FP.Terminate(0);
  end;
end;

procedure TCLI_Caller.Run(callback: TCLI_Callback; bView: boolean; bTimeout: boolean);
var
  n, BytesRead: longint;
  dly:  integer;
  buff: string;
  i:    integer;
begin
  buff := '';
  dly  := 0;

  BytesRead := 0;
  FP := TProcess.Create(nil);
{$ifdef MSWINDOWS}
  FP.CommandLine := '"' + FApplication + '"' + Utf8ToAnsi(FParameter);
{$else}
  FP.CommandLine := FApplication + FParameter;
{$endif}
  if callback <> nil then
  begin
{$ifdef MSWINDOWS}
    callback(AnsiToUtf8(FP.CommandLine));
{$else}
    callback(FP.CommandLine);
{$endif}
  end;

  if not bView then
  begin
    FP.ShowWindow := swoHIDE;
  end;
  FP.Options := FP.Options + [poUsePipes, poStderrToOutPut] - [poWaitOnExit];

  try
    FRunning := True;
    FP.Execute;

    while FP.Running do
    begin
      // make sure we have room
      SetLength(buff, BytesRead + BUF_INC);

      // try reading it
      if FP.Output.NumBytesAvailable > 0 then
      begin
        dly := 0;
        n   := FP.Output.Read(buff[BytesRead + 1], BUF_INC);
        if n > 0 then
        begin
          Inc(BytesRead, n);

          if callback <> nil then
          begin
            SetLength(buff, BytesRead);

            i := 0;
            while (BytesRead > 0) and (i <= BytesRead) do
            begin
              if (buff[1 + i] = #10) or (buff[1 + i] = #13) then
              begin
{$ifdef MSWINDOWS}
                callback(AnsiToUtf8(Copy(buff, 1, i)));
{$else}
                callback(Copy(buff, 1, i));
{$endif}
                if ((buff[2 + i] = #10) or (buff[2 + i] = #13)) and
                  (buff[2 + i] <> buff[1 + i]) then
                begin
                  Inc(i);
                end;
                Delete(buff, 1, i + 1);
                Dec(BytesRead, i + 1);
                i := 0;
              end
              else
              begin
                Inc(i);
              end;
            end;
          end;
        end;
      end
      else
      begin
        // no data, wait 10 ms
        Inc(dly);
        bTimeout := bTimeout;
        //      if bTimeout and (dly > 10) then
        //      begin
        // Timeout
        //        P.Terminate(0);
        //      end;
        Sleep(10);
      end;
      if FProcessMessage then
      begin
        Forms.Application.ProcessMessages;
      end;
    end;

    // read last part, process finished
    repeat
      // make sure we have room
      SetLength(buff, BytesRead + BUF_INC);

      // try reading it
      n := FP.Output.Read(buff[BytesRead + 1], BUF_INC);
      if n > 0 then
      begin
        Inc(BytesRead, n);

        if callback <> nil then
        begin
          SetLength(buff, BytesRead);

          i := 0;
          while (BytesRead > 0) and (i <= BytesRead) do
          begin
            if (buff[1 + i] = #10) or (buff[1 + i] = #13) then
            begin
{$ifdef MSWINDOWS}
              callback(AnsiToUtf8(Copy(buff, 1, i)));
{$else}
              callback(Copy(buff, 1, i));
{$endif}
              if ((buff[2 + i] = #10) or (buff[2 + i] = #13)) and
                (buff[2 + i] <> buff[1 + i]) then
              begin
                Inc(i);
              end;
              Delete(buff, 1, i + 1);
              Dec(BytesRead, i + 1);
              i := 0;
            end
            else
            begin
              Inc(i);
            end;
          end;
        end;
      end;
    until n <= 0;
  finally
    FreeAndNil(FP);
  end;
  FRunning := False;
  FTaken   := False;
end;

end.

