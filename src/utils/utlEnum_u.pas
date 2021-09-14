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

unit utlEnum_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  { Basic enumeration for available epidemic models }
  NumberOfAvailableModels = 9 {+ 1};
  SIR = 'SIR';
  SIS = 'SIS';
  SIQ = 'SIQ';
  SIQS = 'SIQS';
  SIQR = 'SIQR';
  SIRD = 'SIRD';
  MSIR = 'MSIR';
  SEIR = 'SEIR';
  SEIS = 'SEIS';
  MSEIR = 'MSEIR';
  AvailableModels: Array[0..NumberOfAvailableModels] of String =
                   (SIR, SIS, SIQ, SIQS, SIQR, SIRD, MSIR, SEIR, SEIS, MSEIR);

implementation

end.

