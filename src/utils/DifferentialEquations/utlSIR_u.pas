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

unit utlSIR_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function SIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function SIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma: Double;
    S, I: Double;
    dSdt, dIdt, dRdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the SIR's (Susceptible - Infectious - Recovered) model
      differential equations given a y state.

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' S I R states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2
                       [N, beta, gamma]

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var N: The total population.
      Var beta  (β): The average number of contacts per person per time.
                     (Infection rate)

      Var gamma (γ): The recovery rate.
      Var S: Number of Susceptible individuals.
      Var I: Number of Infected individuals.
      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dIdt: Calculated differential equation result for Infected.
      Var dRdt: Calculated differential equation result for Recovered.

      [************************************************************************]

      [ Differential Equations ]
      [************************************************************************]
               _     _
       dS     | β*I*S |
       -- = - | ----- | ,
       dt     |_  N  _|

             _     _
       dI   | β*I*S |
       -- = | ----- | - (γ*I) ,
       dt   |_  N  _|


       dR
       -- = (γ*I)
       dt

      [************************************************************************]
    }

    S := y[0];
    I := y[1];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];

    dSdt := -(beta * S * I / N);
    dIdt := (beta * S * I / N) - (gamma * I);
    dRdt := gamma * I;

    Result := ArrayOfDouble.Create(dSdt, dIdt, dRdt);
  end;

end.
