{
                Copyright (C) 2020 Theodoros Siklafidis

    This file is part of BVS.

    BVS is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BVS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BVS. If not, see <https://www.gnu.org/licenses/>.
}

unit utlEuler_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u,
  utlSIR_u;

type
  TCallback = Function: ArrayOfDouble;

function odeEuler(t, y0: ArrayOfDouble; N: Integer; beta, gamma: Double): ArrayOfArrayOfDouble;

implementation
  function odeEuler(t, y0: ArrayOfDouble; N: Integer; beta, gamma: Double): ArrayOfArrayOfDouble;
  var
    i, j: Integer;
    DiffEquations: ArrayOfArrayOfDouble;
    dXdts, x: ArrayOfDouble;
  begin
    {
      [ Description ]
      [************************************************************************]
      Euler method (also called forward Euler method) a first-order numerical
      procedure for solving ordinary differential equations (ODEs) with a given
      initial value.

      Notes:
            * Currently specific for SIR model.
      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param t: An array of a specified duration with equal parts.
      Param y0: The initial system state.
      Param N: The total population.
      Param beta  (β): The average number of contacts per person per time.
      Param gamma (γ): The recovery rate.

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var i, j: Counters.
      Var DiffEquations: Array of Arrays containing double numbers that holds
                         the whole system's state.

      Var dXdts: Calculated differential equation result returned from a
                 differential equation calculating function.

      Var x: Temporary array that holds the next system's state to be
             calculated.

      [************************************************************************]
    }

    SetLength(DiffEquations, Length(y0));
    SetLength(x, Length(y0));

    for i := 0 to Length(DiffEquations) - 1 do begin
      SetLength(DiffEquations[i], Length(t));
      DiffEquations[i][0] := y0[i];
    end;

    for i := 0 to Length(t) - 1 do begin
      for j := 0 to Length(DiffEquations) - 1 do begin
        x[j] := DiffEquations[j][i];
      end;

      dXdts := SIRDE(x, N, beta, gamma);

      for j := 0 to Length(dXdts) - 1 do begin
        DiffEquations[j][i + 1] := DiffEquations[j][i] + dXdts[j] * (t[i + 1] - t[i]);
      end;
    end;

    { Print the calculated differential equations }
    //for i:=0 to Length(DiffEquations) - 1 do begin
    //  for j := 0 to Length(t) - 1 do begin
    //    write(DiffEquations[i][j]:0:2);
    //    write(', ');
    //  end;
    //  writeln();
    //end;

    Result := DiffEquations;
  end;
end.

