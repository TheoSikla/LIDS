unit utlConstants_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  sLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
               {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};

{ Filenames }
SETTINGS_FILE_NAME = 'settings.json';

{ Settings }
SETTINGS_NAME = 'settings';
RE_SIMULATE_SETTING_NAME = 'ReSimulate';
DEFAULT_SETTINGS = '{' + sLineBreak +
                   '  "' + SETTINGS_NAME + '": {' + sLineBreak +
                   '    "' + RE_SIMULATE_SETTING_NAME + '": False' +
                   '  }' + sLineBreak +
                   '}';

implementation

end.

