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

unit utlSIRD_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function SIRDDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function SIRDDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma, mu: Double;
    S, I: Double;
    dSdt, dIdt, dRdt, dDdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the SIRD's (Susceptible - Infectious - Recovered - Deceased)
      model differential equations given a y state.

      <>=============<>            <>============<>            <>==============+========================<>
      ||             ||            ||            ||            || <>==========+<>    /  <>==========<>  ||
      || Susceptible ||  ___\`-._  ||  Infected  ||  ___\`-._  || || Recovered ||   /   || Deceased ||  ||
      ||             ||     /.-'   ||            ||     /.-'   || <>==========+<>  /    <>==========<>  ||
      <>=============<>            <>============<>            <>===============+=======================<>

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' S I R D states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2    3
                       [N, beta, gamma, mu]

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var N: The total population.

      Var beta  (β): The average number of contacts per person per time.
                     (Infection rate)
      Var gamma (γ): The recovery rate.
      Var mu (μ): Natural mortality rate.

      Var S: Number of Susceptible individuals.
      Var I: Number of Infected individuals.

      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dIdt: Calculated differential equation result for Infected.
      Var dRdt: Calculated differential equation result for Recovered.
      Var dDdt: Calculated differential equation result for Deceased.

      [************************************************************************]

      [ Differential Equations ]
      [************************************************************************]
               _     _
       dS     | β*I*S |
       -- = - | ----- | ,
       dt     |_  N  _|

             _     _
       dI   | β*I*S |
       -- = | ----- | - (γ*I) - (μ*I) ,
       dt   |_  N  _|


       dR
       -- = (γ*I) ,
       dt

       dD
       -- = (μ*I) ,
       dt

      [************************************************************************]
    }

    S := y[0];
    I := y[1];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];
    mu := extraArgs[3];

    dSdt := -(beta * S * I / N);
    dIdt := (beta * S * I / N) - (gamma * I) - (mu * I);
    dRdt := gamma * I;
    dDdt := mu * I;

    Result := ArrayOfDouble.Create(dSdt, dIdt, dRdt, dDdt);
  end;

end.
