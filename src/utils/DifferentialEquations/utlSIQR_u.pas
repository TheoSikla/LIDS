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

unit utlSIQR_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function SIQRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function SIQRDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, gamma, mu, lambda, delta, delta1, kappa, zeta: Double;
    S, I, Q, R: Double;
    dSdt, dIdt, dQdt, dRdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the SIQR's (Susceptible - Infectious - Quarantined - Recovered)
      model differential equations given a y state.


      <>=============<>            <>==========<>            <>=============<>            <>===========<>
      ||             ||            ||          ||            ||             ||            ||           ||
      || Susceptible ||  ___\`-._  || Infected ||  ___\`-._  || Quarantined ||  ___\`-._  || Recovered ||
      ||             ||     /.-'   ||          ||     /.-'   ||             ||     /.-'   ||           ||
      <>=============<>            <>==========<>            <>=============<>            <>===========<>

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' S I Q R states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1     2    3      4      5      6       7     8
                       [N, beta, gamma, mu, lambda, delta, delta1, kappa, zeta]

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var N: The total population.

      Var beta  (β): The average number of contacts per person per time.
                     (Infection rate)
      Var gamma (γ): The recovery rate.
      Var mu (μ): Natural mortality rate.
      Var lambda (Λ): Recruitment of the susceptible individuals (birth etc.)
      Var delta (δ): Disease mortality rate of infectious individuals.
      Var delta1 (δ1): Disease mortality rate of quarantined individuals.
      Var Kappa (κ): The rate where the infected individuals become quarantined.
      Var Zeta (ζ): The rate where quarantined individuals become susceptible
                    again.

      Var S: Number of Susceptible individuals.
      Var I: Number of Infected individuals.
      Var Q: Number of Quarantined individuals.
      Var R: Number of Recovered individuals.

      Var dSdt: Calculated differential equation result for Susceptibles.
      Var dIdt: Calculated differential equation result for Infected.
      Var dQdt: Calculated differential equation result for Quarantined.
      Var dRdt: Calculated differential equation result for Recovered.

      [************************************************************************]

      [ Differential Equations ]
      [************************************************************************]
                 _     _
       dS       | β*I*S |
       -- = Λ - | ----- | - (μ*S) ,
       dt       |_  N  _|

             _     _
       dI   | β*I*S |
       -- = | ----- | - (γ+κ+μ+δ)*I ,
       dt   |_  N  _|


       dQ
       -- = (κ*I) - (ζ+μ+δ1)*Q ,
       dt

       dR
       -- = (γ*I) + (ζ*Q) - (μ*R)
       dt

      [************************************************************************]
    }

    S := y[0];
    I := y[1];
    Q := y[2];
    R := y[3];

    N := extraArgs[0];
    beta := extraArgs[1];
    gamma := extraArgs[2];
    mu := extraArgs[3];
    lambda := extraArgs[4];
    delta := extraArgs[5];
    delta1 := extraArgs[6];
    kappa := extraArgs[7];
    zeta := extraArgs[8];

    dSdt := lambda - (beta * S * I / N) - (mu * S);
    dIdt := (beta * S * I / N) - ((gamma + kappa + mu + delta) * I);
    dQdt := (kappa * I) - ((zeta + mu + delta1) * Q);
    dRdt := (gamma * I) + (zeta * Q) - (mu * R);

    Result := ArrayOfDouble.Create(dSdt, dIdt, dQdt, dRdt);
  end;

end.
