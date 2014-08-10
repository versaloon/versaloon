unit strparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inputdialog;

type
    TArrayOfByte = array of BYTE;

  function strparser_parse(str, format: String; var buffer: array of BYTE): boolean;
  function strparser_solve(var str: String; format: String; var buffer: array of BYTE): boolean;

var
  strparser_ErrMsg: String;

const
  STRPARSER_DIV: String = '|_:; ';
  STRPARSER_DEFAULT_DIV: Char = ':';

implementation

function strparser_parse(str, format: String; var buffer: array of BYTE): boolean;
var
  format_parsed_len, str_parsed_len, buffer_parsed_len: integer;
  param: integer;
  value: QWord;
  strTmp: String;
  off: integer;
  radix: integer;
  parse_num: boolean;
  i: integer;
begin
  Result := False;
  format_parsed_len := 1;
  str_parsed_len    := 1;
  buffer_parsed_len := Low(buffer);
  off := 0;

  while format_parsed_len < Length(format) do
  begin
    case format[format_parsed_len] of
    '%':
      begin
        strTmp := Copy(format, format_parsed_len + 1, Length(format) - format_parsed_len);
        param := 0;
        param := StrToIntRadix(strTmp, 10, off);
        Inc(format_parsed_len, off + 1);

        parse_num := True;
        case format[format_parsed_len] of
        'i','I','d','D':
          begin
            radix := 10;
          end;
        'b','B':
          begin
            radix := 2;
          end;
        'x','X':
          begin
            radix := 16;
          end;
        'c','C':
          begin
            if buffer_parsed_len >= Length(buffer) then
            begin
              strparser_ErrMsg := 'Invalid buffer_size: ' + IntToStr(Length(buffer));
              Exit;
            end;

            buffer[buffer_parsed_len] := BYTE(str[str_parsed_len]);
            Inc(buffer_parsed_len);
            Inc(str_parsed_len);
            parse_num := False;
          end;
        's','S':
          begin
            while (str_parsed_len < Length(str)) and
              (not Pos(str[str_parsed_len], STRPARSER_DIV) > 0) do
            begin
              if buffer_parsed_len >= Length(buffer) then
              begin
                strparser_ErrMsg := 'Invalid buffer_size: ' + IntToStr(Length(buffer));
                Exit;
              end;

              buffer[buffer_parsed_len] := BYTE(str[str_parsed_len]);
              Inc(buffer_parsed_len);
              Inc(str_parsed_len);
            end;

            if buffer_parsed_len >= Length(buffer) then
            begin
              strparser_ErrMsg := 'Invalid buffer_size: ' + IntToStr(Length(buffer));
              Exit;
            end;

            buffer[buffer_parsed_len] := BYTE(0);
            Inc(buffer_parsed_len);
            parse_num := False;
          end
        else
          begin
            strparser_ErrMsg := 'Invalid type: ' + format[format_parsed_len];
            Exit;
          end;
        end;

        Inc(format_parsed_len, 1);
        if parse_num then
        begin
          if param = 0 then
          begin
            strparser_ErrMsg := 'Integer size not defined!';
            Exit;
          end;

          strTmp := Copy(str, str_parsed_len, Length(str) - str_parsed_len + 1);
          value := StrToIntRadix(strTmp, radix, off);
          if off = 0 then
          begin
            strparser_ErrMsg := 'Fail to parse integer: ' + strTmp;
            Exit;
          end;

          for i := 1 to param do
          begin
            if buffer_parsed_len >= Length(buffer) then
            begin
              strparser_ErrMsg := 'Invalid buffer_size: ' + IntToStr(Length(buffer));
              Exit;
            end;

            buffer[buffer_parsed_len] := BYTE((Value shr (8 * (i - 1))) and $FF);
            Inc(buffer_parsed_len);
          end;
          Inc(str_parsed_len, off);
        end;

        while (str_parsed_len < Length(str)) and
          (Pos(str[str_parsed_len], STRPARSER_DIV) > 0) do
        begin
          Inc(str_parsed_len);
        end
      end
     else
     begin
       if buffer_parsed_len >= Length(buffer) then
       begin
         strparser_ErrMsg := 'Invalid buffer_size: ' + IntToStr(Length(buffer));
         Exit;
       end;

       buffer[buffer_parsed_len] := BYTE(format[format_parsed_len]);
       Inc(buffer_parsed_len);
       Inc(format_parsed_len);
     end;
    end;
  end;

  Result := True;
end;

function strparser_solve(var str: String; format: String; var buffer: array of BYTE): boolean;
var
  format_parsed_len, buffer_parsed_len: integer;
  param: integer;
  value: QWord;
  strTmp: String;
  off: integer;
  radix: integer;
  parse_num: boolean;
  i, j: integer;
  str_min_len: integer;
  strPrefix: string;
begin
  Result := False;
  str := '';
  format_parsed_len := 1;
  buffer_parsed_len := Low(buffer);
  off := 0;

  while format_parsed_len < Length(format) do
  begin
    case format[format_parsed_len] of
    '%':
      begin
        strTmp := Copy(format, format_parsed_len + 1, Length(format) - format_parsed_len);
        param := 0;
        param := StrToIntRadix(strTmp, 10, off);
        Inc(format_parsed_len, off + 1);

        parse_num := True;
        case format[format_parsed_len] of
        'i','I','d','D':
          begin
            radix := 10;
            str_min_len := 0;
            strPrefix := '';
          end;
        'b','B':
          begin
            radix := 2;
            str_min_len := param * 8;
            strPrefix := '0b';
          end;
        'x','X':
          begin
            radix := 16;
            str_min_len := param * 2;
            strPrefix := '0x';
          end;
        'c','C':
          begin
            str := str + Char(buffer[buffer_parsed_len]);
            Inc(buffer_parsed_len);
            parse_num := False;
          end;
        's','S':
          begin
            while buffer[buffer_parsed_len] <> 0 do
            begin
              str := str + Char(buffer[buffer_parsed_len]);
              Inc(buffer_parsed_len);
            end;
            Inc(buffer_parsed_len);
            parse_num := False;
          end
        else
          begin
            strparser_ErrMsg := 'Invalid type: ' + format[format_parsed_len];
            Exit;
          end;
        end;

        Inc(format_parsed_len, 1);
        if parse_num then
        begin
          if param = 0 then
          begin
            strparser_ErrMsg := 'Integer size not defined!';
            Exit;
          end;

          str := str + strPrefix;
          if (radix = 16) and (param > 8) then
          begin
            j := buffer_parsed_len;
            for i := param downto 1 do
            begin
              value := buffer[j + i - 1];
              str := str + IntToStrRadix(value, radix, 2);
              Inc(buffer_parsed_len);
            end;
          end
          else
          begin
            value := 0;
            for i := 1 to param do
            begin
              value := value + (QWord(buffer[buffer_parsed_len]) shl (8 * (i - 1)));
              Inc(buffer_parsed_len);
            end;
            str := str + IntToStrRadix(value, radix, str_min_len);
          end;
        end;
        if format_parsed_len < Length(format) then
        begin
          str := str + STRPARSER_DEFAULT_DIV;
        end
      end
    else
      begin
        str := str + format[format_parsed_len];
        Inc(format_parsed_len);
      end;
    end;
  end;

  Result := True;
end;

end.

