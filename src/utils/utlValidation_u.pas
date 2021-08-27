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

unit utlValidation_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls;

procedure ValidateFloat(Sender: TObject; var Key: char);
procedure ValidateInteger(Sender: TObject; var Key: char);

implementation
procedure ValidateFloat(Sender: TObject; var Key: char);
begin
  { Ensures that a TEdit field can only have a float input. }
  if (not (Key in ['0'..'9', '.', #8, #9])) OR ( (Key = '.') and (pos('.',TEdit(Sender).Text)>0) ) then Key := #0;
end;

procedure ValidateInteger(Sender: TObject; var Key: char);
begin
  { Ensures that a TEdit field can only have an integer input. }
  if (not (Key in ['0'..'9', #8, #9]))  then Key := #0;
end;
end.

