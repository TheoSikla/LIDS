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

unit utlSIQ_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Forms }
  { Classes }
  { Utilities }
  utlTypes_u;

function SIQDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;

implementation
  function SIQDE(y, extraArgs: ArrayOfDouble): ArrayOfDouble;
  var
    N, beta, mu, lambda, delta, kappa: Double;
    S, I, Q: Double;
    dSdt, dIdt, dQdt: Double;
  begin
    {
      [ Description ]
      [************************************************************************]
      Calculate the SIQ's (Susceptible - Infectious - Quarantined) model
      differential equations given a y state.


      <>=============<>            <>==========<>            <>=============<>
      ||             ||            ||          ||            ||             ||
      || Susceptible ||  ___\`-._  || Infected ||  ___\`-._  || Quarantined ||
      ||             ||     /.-'   ||          ||     /.-'   ||             ||
      <>=============<>            <>==========<>            <>=============<>

      [************************************************************************]

      [ Parameters ]
      [************************************************************************]
      Param y: Array of double containing the 'to be calculated' S I Q states.
      Param extraArgs: An array of double containing the extra arguments that
                       are needed in order to perform calculations. The correct
                       form of the array should be:
                        0    1   2     3       4      5
                       [N, beta, mu, lambda, delta, kappa]

      [************************************************************************]

      [ Variables ]
      [************************************************************************]
      Var N: The total population.

      Var beta  (β): The average number of contacts per person per time.
                     (Infection rate)
      Var mu (μ): Natural mortality rate.
      Var lambda (Λ): Recruitment of the susceptible individuals (birth etc.)
      Var delta (δ): Disease mortality rate.
      Var Kappa (κ): The rate where the infected individuals become quarantined.

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
       -- = Λ - | ----- | - (μ*S) ,
       dt       |_  N  _|

                 _     _
       dI       | β*I*S |
       -- = Λ + | ----- | - (μ+δ+κ)*I ,
       dt       |_  N  _|


       dQ
       -- = (κ*I) - (μ+δ)*Q
       dt

      [************************************************************************]
    }

    S := y[0];
    I := y[1];
    Q := y[2];

    N := extraArgs[0];
    beta := extraArgs[1];
    mu := extraArgs[2];
    lambda := extraArgs[3];
    delta := extraArgs[4];
    kappa := extraArgs[5];

    dSdt := lambda - (beta * S * I / N) - (mu * S);
    dIdt := lambda + (beta * S * I / N) - ((mu + delta + kappa) * I);
    dQdt := (kappa * I) - ((mu + delta) * Q);

    Result := ArrayOfDouble.Create(dSdt, dIdt, dQdt);
  end;

end.
