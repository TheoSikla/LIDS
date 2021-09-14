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

unit utlMSIR_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function MSIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function MSIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma, mu, lambda, delta: Double;
    M, S, I, R: Double;
    dMdt, dSdt, dIdt, dRdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the MSIR's
      (Maternally derived immunity - Susceptible - Infectious - Recovered)
      model differential equations given a y state.

      <>============<>
      ||            ||            <>=============<>            <>============<>            <>===========<>
      || Maternally ||            ||             ||            ||            ||            ||           ||
      ||  derived   ||  ___\`-._  || Susceptible ||  ___\`-._  || Infectious ||  ___\`-._  || Recovered ||
      ||  immunity  ||     /.-'   ||             ||     /.-'   ||            ||     /.-'   ||           ||
      ||            ||            <>=============<>            <>============<>            <>===========<>
      <>============<>

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' M S I R states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2    3      4      5
                       [N, beta, gamma, mu, lambda, delta]

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var N: The total population.

      Var beta  (β): The average number of contacts per person per time.
                     (Infection rate)
      Var gamma (γ): The recovery rate.
      Var mu (μ): Natural mortality rate.
      Var lambda (Λ): Recruitment of the susceptible individuals (birth etc.)
      Var delta (δ): Disease mortality rate.

      Var M: Number of individuals with Maternally derived immunity.
      Var S: Number of Susceptible individuals.
      Var I: Number of Infected individuals.
      Var R: Number of Recovered individuals.

      Var dMdt: Calculated differential equation result for Maternally
                derived immunity.
      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dIdt: Calculated differential equation result for Infected.
      Var dRdt: Calculated differential equation result for Recovered.

      [************************************************************************]

      [ Differential Equations ]
      [************************************************************************]

       dM
       -- = Λ - (δ*M) - (μ*M) ,
       dt

                     _     _
       dS           | β*I*S |
       -- = (δ*M) - | ----- | - (μ*S) ,
       dt           |_  N  _|

             _     _
       dI   | β*I*S |
       -- = | ----- | - (γ*I) - (μ*I) ,
       dt   |_  N  _|


       dR
       -- = (γ*I) - (μ*R) ,
       dt

      [************************************************************************]
    }

    M := y[0];
    S := y[1];
    I := y[2];
    R := y[3];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];
    mu := extraArgs[3];
    lambda := extraArgs[4];
    delta := extraArgs[5];

    dMdt := lambda - (delta * M) - (mu * M);
    dSdt := (delta * M) - (beta * S * I / N) - (mu * S);
    dIdt := (beta * S * I / N) - (gamma * I) - (mu * I);
    dRdt := (gamma * I) - (mu * R);

    Result := ArrayOfDouble.Create(dMdt, dSdt, dIdt, dRdt);
  end;

end.
