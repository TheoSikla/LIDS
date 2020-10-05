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

unit utlSIQS_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function SIQSDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function SIQSDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma, mu, lambda, delta, delta1, kappa, zeta: Double;
    S, I, Q: Double;
    dSdt, dIdt, dQdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the SIQ's (Susceptible - Infectious - Quarantined - Susceptible)
      model differential equations given a y state.


                     <>=============<>            <>==========<>            <>=============<>
                     ||             ||            ||          ||            ||             ||
       _______\`-._  || Susceptible ||  ___\`-._  || Infected ||  ___\`-._  || Quarantined ||  ___\`-._ ______
      |       /.-'   ||             ||     /.-'   ||          ||     /.-'   ||             ||     /.-'        |
      |              <>=============<>            <>==========<>            <>=============<>                 |
      |_______________________________________________________________________________________________________|

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' S I Q states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2    3     4       5      6      7     8
                       [N, beta, gamma, mu, lambda, delta, delta1 kappa, zeta]

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
      Var delta1 (δ1): Disease mortality rate of quarantined individuals.
      Var Kappa (κ): The rate where the infected individuals become quarantined.
      Var Zeta (ζ): The rate where quarantined individuals become susceptible
                    again.

      Var S: Number of Susceptible individuals.
      Var I: Number of Infected individuals.
      Var Q: Number of Quarantined individuals.

      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dIdt: Calculated differential equation result for Infected.
      Var dQdt: Calculated differential equation result for Quarantined.

      [************************************************************************]

      [ Differential Equations ]
      [************************************************************************]
                 _     _
       dS       | β*I*S |
       -- = Λ - | ----- | - (μ*S) + (γ*I) + (ζ*Q) ,
       dt       |_  N  _|

             _     _
       dI   | β*I*S |
       -- = | ----- | - (μ+δ+κ+γ)*I ,
       dt   |_  N  _|


       dQ
       -- = (κ*I) - (μ+δ1+ζ)*Q
       dt

      [************************************************************************]
    }

    S := y[0];
    I := y[1];
    Q := y[2];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];
    mu := extraArgs[3];
    lambda := extraArgs[4];
    delta := extraArgs[5];
    delta1 := extraArgs[6];
    kappa := extraArgs[7];
    zeta := extraArgs[8];

    dSdt := lambda - (beta * S * I / N) - (mu * S) + (gamma * I) + (zeta * Q);
    dIdt := (beta * S * I / N) - ((mu + delta + kappa + gamma) * I);
    dQdt := (kappa * I) - ((mu + delta1 + zeta) * Q);

    Result := ArrayOfDouble.Create(dSdt, dIdt, dQdt);
  end;

end.
