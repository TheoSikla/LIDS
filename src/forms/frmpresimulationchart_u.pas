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

unit frmPreSimulationChart_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries,
  { Forms }
  frmMain_u,
  { Classes }
  { Utilities }
  utlArray_u,
  utlTypes_u,
  utlEuler_u;

type

  { TfrmPreSimulationChart }

  TfrmPreSimulationChart = class(TForm)
    chtPreSimulation: TChart;
    chtPreSimulationI: TLineSeries;
    chtPreSimulationR: TLineSeries;
    chtPreSimulationS: TLineSeries;
    procedure CalculatePreSimulation;
    procedure FormClose(Sender: TObject);
    procedure ClearPreSimulationChart;
  private

  public

  end;

var
  frmPreSimulationChart: TfrmPreSimulationChart;

implementation

procedure TfrmPreSimulationChart.CalculatePreSimulation;
var
  OdeEulerResult: ArrayOfArrayOfDouble;
  t, y0: ArrayOfDouble;
  i, days: Integer;
begin
  days := StrToInt(frmMain.edtDays.Text);
  SetLength(t, days);
  t := linspace(0, days, days);

  SetLength(y0, 3);
  y0[1] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[2] := 0;                                                 // R
  y0[0] := Length(frmMain.Nodes) - y0[1] - y0[2];             // S

  self.chtPreSimulation.Visible := true;
  OdeEulerResult := odeEuler(t, y0, Length(frmMain.Nodes),
                             StrToFloat(frmMain.edtBeta.Text),
                             StrToFloat(frmMain.edtGamma.Text));

  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationI.Title := 'Infectious';
  self.chtPreSimulationR.Title := 'Recovered';
  for i := 0 to days - 1 do
  begin
    self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[0][i]); // S
    self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[1][i]); // I
    self.chtPreSimulationR.AddXY(t[i], OdeEulerResult[2][i]); // R
  end;
end;

procedure TfrmPreSimulationChart.ClearPreSimulationChart;
var
  i: Integer;
begin
  for i := 0 to self.chtPreSimulation.SeriesCount - 1 do
  begin
    if self.chtPreSimulation.Series[i] is TLineSeries then
      (self.chtPreSimulation.Series[i] as TLineSeries).Clear;
  end;
end;

procedure TfrmPreSimulationChart.FormClose(Sender: TObject);
begin
  self.ClearPreSimulationChart;
end;

{$R *.lfm}

end.

