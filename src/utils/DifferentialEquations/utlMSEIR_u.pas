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

unit utlMSEIR_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function MSEIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function MSEIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma, mu, lambda, delta, epsilon: Double;
    M, S, E, I, R: Double;
    dMdt, dSdt, dEdt, dIdt, dRdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the MSEIR's
      (Maternally derived immunity - Susceptible - Exposed - Infectious - Recovered)
      model differential equations given a y state.

      <>============<>
      ||            ||            <>=============<>            <>=========<>            <>============<>           <>===========<>
      || Maternally ||            ||             ||            ||         ||            ||            ||           ||           ||
      ||  derived   ||  ___\`-._  || Susceptible ||  ___\`-._  || Exposed ||  ___\`-._  || Infectious || ___\`-._  || Recovered ||
      ||  immunity  ||     /.-'   ||             ||     /.-'   ||         ||     /.-'   ||            ||    /.-'   ||           ||
      ||            ||            <>=============<>            <>=========<>            <>============<>           <>===========<>
      <>============<>

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' M S E I R states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2    3      4      5       6
                       [N, beta, gamma, mu, lambda, delta, epsilon]

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
      Var epsilon (ε): The rate at which exposed individuals become infectious.

      Var M: Number of individuals with Maternally derived immunity.
      Var S: Number of Susceptible individuals.
      Var E: Number of Exposed individuals.
      Var I: Number of Infected individuals.
      Var R: Number of Recovered individuals.

      Var dMdt: Calculated differential equation result for Maternally
                derived immunity.
      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dEdt: Calculated differential equation result for Exposed.
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
       dE   | β*I*S |
       -- = | ----- | - (ε+μ)*E ,
       dt   |_  N  _|


       dI
       -- = (ε*E) - (γ+μ)*I ,
       dt


       dR
       -- = (γ*I) - (μ*R) ,
       dt

      [************************************************************************]
    }

    M := y[0];
    S := y[1];
    E := y[2];
    I := y[3];
    R := y[4];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];
    mu := extraArgs[3];
    lambda := extraArgs[4];
    delta := extraArgs[5];
    epsilon := extraArgs[6];

    dMdt := lambda - (delta * M) - (mu * M);
    dSdt := (delta * M) - (beta * S * I / N) - (mu * S);
    dEdt := (beta * S * I / N) - ((epsilon + mu) * E);
    dIdt := (epsilon * E) - ((gamma + mu) * I);
    dRdt := (gamma * I) - (mu * R);

    Result := ArrayOfDouble.Create(dMdt, dSdt, dEdt, dIdt, dRdt);
  end;

end.
