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

unit utlSEIR_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function SEIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function SEIRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma, mu, lambda, alpha: Double;
    S, E, I, R: Double;
    dSdt, dEdt, dIdt, dRdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the SEIR's
      (Susceptible - Exposed - Infectious - Recovered)
      model differential equations given a y state.

      <>=============<>            <>=========<>            <>============<>            <>===========<>
      ||             ||            ||         ||            ||            ||            ||           ||
      || Susceptible ||  ___\`-._  || Exposed ||  ___\`-._  || Infectious ||  ___\`-._  || Recovered ||
      ||             ||     /.-'   ||         ||     /.-'   ||            ||     /.-'   ||           ||
      <>=============<>            <>=========<>            <>============<>            <>===========<>

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' S E I R states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2    3      4      5
                       [N, beta, gamma, mu, lambda, alpha]

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var N: The total population.

      Var beta  (β): The average number of contacts per person per time.
                     (Infection rate)
      Var gamma (γ): The recovery rate.
      Var mu (μ): Natural mortality rate.
      Var lambda (Λ): Recruitment of the susceptible individuals (birth etc.)
      Var alpha (α): Average incubation period (α¯¹).

      Var S: Number of Susceptible individuals.
      Var E: Number of Exposed individuals.
      Var I: Number of Infected individuals.
      Var R: Number of Recovered individuals.

      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dEdt: Calculated differential equation result for Exposed.
      Var dIdt: Calculated differential equation result for Infected.
      Var dRdt: Calculated differential equation result for Recovered.

      [************************************************************************]

      [ Differential Equations ]
      [************************************************************************]
                             _     _
       dS                   | β*I*S |
       -- = (Λ*N) - (μ*S) - | ----- | ,
       dt                   |_  N  _|

             _     _
       dE   | β*I*S |
       -- = | ----- | - (μ+α)*E ,
       dt   |_  N  _|


       dI
       -- = (α*E) - (γ+μ)*I ,
       dt


       dR
       -- = (γ*I) - (μ*R) ,
       dt

      [************************************************************************]
    }

    S := y[0];
    E := y[1];
    I := y[2];
    R := y[3];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];
    mu := extraArgs[3];
    lambda := extraArgs[4];
    alpha := extraArgs[5];

    dSdt := (lambda * N) - (mu * S) - (beta * S * I / N);
    dEdt := (beta * S * I / N) - ((mu + alpha) * E);
    dIdt := (alpha * E) - ((gamma + mu) * I);
    dRdt := (gamma * I) - (mu * R);

    Result := ArrayOfDouble.Create(dSdt, dEdt, dIdt, dRdt);
  end;

end.
