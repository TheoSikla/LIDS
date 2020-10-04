unit utlConstants_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  sLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
               {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};

implementation

end.

