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

unit utlMisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms;

procedure RandomizeSystem;

implementation
uses
  { Forms }
  frmMain_u;

procedure RandomizeSystem;
begin
  if frmMain.ckbUseSystemSeed.Checked then begin
    Randomize;
    RandSeed := RandSeed + MilliSecondsBetween(Now, Date);
    frmMain.edtSeed.Text := IntToStr(RandSeed);
    frmMain.RefreshGUI;
    Application.Processmessages;
  end
  else begin
    if frmMain.edtSeed.Text <> '' then begin
      RandSeed := StrToInt(frmMain.edtSeed.Text);
    end
    else
      RandSeed := 0;
      frmMain.edtSeed.Text := IntToStr(RandSeed);
  end;
end;

end.

