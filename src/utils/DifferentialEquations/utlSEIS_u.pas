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

unit utlSEIS_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function SEISDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function SEISDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma, mu, lambda, epsilon: Double;
    S, E, I: Double;
    dSdt, dEdt, dIdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the SEIS's
      (Susceptible - Exposed - Infectious - Susceptible)
      model differential equations given a y state.

                     <>=============<>            <>=========<>            <>============<>
                     ||             ||            ||         ||            ||            ||
       _______\`-._  || Susceptible ||  ___\`-._  || Exposed ||  ___\`-._  || Infectious ||  ___\`-._ _______
      |       /.-'   ||             ||     /.-'   ||         ||     /.-'   ||            ||     /.-'         |
      |              <>=============<>            <>=========<>            <>============<>                  |
      |______________________________________________________________________________________________________|

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' S E I S states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2    3      4       5
                       [N, beta, gamma, mu, lambda, epsilon]

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var N: The total population.

      Var beta  (β): The average number of contacts per person per time.
                     (Infection rate)
      Var gamma (γ): The recovery rate.
      Var mu (μ): Natural mortality rate.
      Var lambda (Λ): Recruitment of the susceptible individuals (birth etc.)
      Var epsilon (ε): The rate at which exposed individuals become infectious.

      Var S: Number of Susceptible individuals.
      Var E: Number of Exposed individuals.
      Var I: Number of Infected individuals.

      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dEdt: Calculated differential equation result for Exposed.
      Var dIdt: Calculated differential equation result for Infected.

      [************************************************************************]

      [ Differential Equations ]
      [************************************************************************]
                 _     _
       dS       | β*I*S |
       -- = Λ - | ----- | - (μ*S) + (γ*I) ,
       dt       |_  N  _|

             _     _
       dE   | β*I*S |
       -- = | ----- | - (ε+μ)*E ,
       dt   |_  N  _|


       dI
       -- = (ε*E) - (γ+μ)*I ,
       dt

      [************************************************************************]
    }

    S := y[0];
    E := y[1];
    I := y[2];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];
    mu := extraArgs[3];
    lambda := extraArgs[4];
    epsilon := extraArgs[5];

    dSdt := lambda - (beta * S * I / N) - (mu * S) + (gamma * I);
    dEdt := (beta * S * I / N) - ((epsilon + mu) * E);
    dIdt := (epsilon * E) - ((gamma + mu) * I);

    Result := ArrayOfDouble.Create(dSdt, dEdt, dIdt);
  end;

end.
