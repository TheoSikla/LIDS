{
                Copyright (C) 2020 - 2021 Theodoros Siklafidis

    This file is part of LIDS.

    LIDS is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    LIDS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with LIDS. If not, see <https://www.gnu.org/licenses/>.
}

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

