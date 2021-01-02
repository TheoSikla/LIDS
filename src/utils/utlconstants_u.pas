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
RE_SIMULATE_SETTING_NAME = 'cbxReSimulate';
RE_SIMULATE_MINIMUM_RECOVERED_NODE_COUNT_SETTING_NAME = 'edtReSimulateMinRecoveredNodeCount';
DEFAULT_SETTINGS = '{' + sLineBreak +
                   '  "' + SETTINGS_NAME + '": {' + sLineBreak +
                   '    "' + RE_SIMULATE_SETTING_NAME + '": False,' +
                   '    "' + RE_SIMULATE_MINIMUM_RECOVERED_NODE_COUNT_SETTING_NAME + '": 0' +
                   '  }' + sLineBreak +
                   '}';

{ Regex }
ADJACENCY_MATRIX_REGEX = '^[0-1]*$';
ADJACENCY_LIST_REGEX = '(^\d+:\d+(,\d+)*$)|(^\d+:$)';

implementation

end.

