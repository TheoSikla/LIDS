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
  utlEuler_u,
  utlEnum_u,
  utlConstants_u;

type

  { TfrmPreSimulationChart }

  TfrmPreSimulationChart = class(TForm)
    chtPreSimulation: TChart;
    chtPreSimulationI: TLineSeries;
    chtPreSimulationD: TLineSeries;
    chtPreSimulationE: TLineSeries;
    chtPreSimulationQ: TLineSeries;
    chtPreSimulationM: TLineSeries;
    chtPreSimulationR: TLineSeries;
    chtPreSimulationS: TLineSeries;
    procedure CalculatePreSimulation;
    procedure FormClose(Sender: TObject);
    procedure ClearPreSimulationChart;
    procedure PrepareSIR(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareSIS(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareSIQ(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareSIQS(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareSIRD(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareMSIR(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareSEIR(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareSEIS(var y0, extraArgs: ArrayOfDouble);
    procedure PrepareMSEIR(var y0, extraArgs: ArrayOfDouble);
  private

  public

  end;

var
  frmPreSimulationChart: TfrmPreSimulationChart;

implementation

procedure TfrmPreSimulationChart.CalculatePreSimulation;
var
  OdeEulerResult: ArrayOfArrayOfDouble;
  t, y0, extraArgs: ArrayOfDouble;
  i, days: Integer;
  model: String;
begin
  SetLength(y0, 0);
  SetLength(extraArgs, 0);

  days := StrToInt(frmMain.edtDays.Text);
  SetLength(t, days);
  t := linspace(0, days, days);

  model := frmMain.cbxAvailableModels.Items[frmMain.cbxAvailableModels.ItemIndex];
  { Initiate based on the model }
  case model of
    SIR: self.PrepareSIR(y0, extraArgs);
    SIS: self.PrepareSIS(y0, extraArgs);
    SIQ: self.PrepareSIQ(y0, extraArgs);
    SIQS: self.PrepareSIQS(y0, extraArgs);
    SIRD: self.PrepareSIRD(y0, extraArgs);
    MSIR: self.PrepareMSIR(y0, extraArgs);
    SEIR: self.PrepareSEIR(y0, extraArgs);
    SEIS: self.PrepareSEIS(y0, extraArgs);
    MSEIR: self.PrepareMSEIR(y0, extraArgs);
  end;

  { Apply Euler to the model's differential equations }
  OdeEulerResult := odeEuler(model, t, y0, extraArgs);

  { Fill data to the appropriate axes }
  case model of
    SIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[0][i]); // S
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[1][i]); // I
          self.chtPreSimulationR.AddXY(t[i], OdeEulerResult[2][i]); // R
        end;
      end;

    SIS: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[0][i]); // S
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[1][i]); // I
        end;
      end;

    SIQ, SIQS: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[0][i]); // S
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[1][i]); // I
          self.chtPreSimulationQ.AddXY(t[i], OdeEulerResult[2][i]); // Q
        end;
      end;

    SIRD: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[0][i]); // S
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[1][i]); // I
          self.chtPreSimulationR.AddXY(t[i], OdeEulerResult[2][i]); // R
          self.chtPreSimulationD.AddXY(t[i], OdeEulerResult[3][i]); // D
        end;
      end;

    MSIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationM.AddXY(t[i], OdeEulerResult[0][i]); // M
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[1][i]); // S
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[2][i]); // I
          self.chtPreSimulationR.AddXY(t[i], OdeEulerResult[3][i]); // R
        end;
      end;

    SEIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[0][i]); // S
          self.chtPreSimulationE.AddXY(t[i], OdeEulerResult[1][i]); // E
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[2][i]); // I
          self.chtPreSimulationR.AddXY(t[i], OdeEulerResult[3][i]); // R
        end;
      end;

    SEIS: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[0][i]); // S
          self.chtPreSimulationE.AddXY(t[i], OdeEulerResult[1][i]); // E
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[2][i]); // I
        end;
      end;

    MSEIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtPreSimulationM.AddXY(t[i], OdeEulerResult[0][i]); // M
          self.chtPreSimulationS.AddXY(t[i], OdeEulerResult[1][i]); // S
          self.chtPreSimulationE.AddXY(t[i], OdeEulerResult[2][i]); // E
          self.chtPreSimulationI.AddXY(t[i], OdeEulerResult[3][i]); // I
          self.chtPreSimulationR.AddXY(t[i], OdeEulerResult[4][i]); // R
        end;
      end;

    end;

  self.chtPreSimulation.Visible := true;
end;

procedure TfrmPreSimulationChart.ClearPreSimulationChart;
var
  i: Integer;
begin
  for i := 0 to self.chtPreSimulation.SeriesCount - 1 do
  begin
    if self.chtPreSimulation.Series[i] is TLineSeries then
      begin
        (self.chtPreSimulation.Series[i] as TLineSeries).Clear;
        (self.chtPreSimulation.Series[i] as TLineSeries).Active := False;
      end;
  end;
end;

procedure TfrmPreSimulationChart.PrepareSIR(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 3);
  y0[1] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[2] := 0;                                                 // R
  y0[0] := Length(frmMain.Nodes) - y0[1] - y0[2];             // S

  SetLength(extraArgs, 3);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma

  { Arrange Line Series Color }
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationI.SeriesColor := clMaroon;
  self.chtPreSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationI.Title := 'Infected';
  self.chtPreSimulationR.Title := 'Recovered';

  { Activate Line Series }
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationI.Active := True;
  self.chtPreSimulationR.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareSIS(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 2);
  y0[1] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[0] := Length(frmMain.Nodes) - y0[1];                     // S

  SetLength(extraArgs, 3);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma

  { Arrange Line Series Color }
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationI.SeriesColor := clMaroon;

  { Define Line Series Titles }
  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationI.Title := 'Infected';

  { Activate Line Series }
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationI.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareSIQ(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 3);
  y0[1] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[2] := 0;                                                 // Q
  y0[0] := Length(frmMain.Nodes) - y0[1] - y0[2];             // S

  SetLength(extraArgs, 6);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtMu.Text);             // Mu
  extraArgs[3] := StrToFloat(frmMain.edtLambda.Text);         // Lambda
  extraArgs[4] := StrToFloat(frmMain.edtDelta.Text);          // Delta
  extraArgs[5] := StrToFloat(frmMain.edtKappa.Text);          // Kappa

  { Arrange Line Series Color }
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationI.SeriesColor := clMaroon;
  self.chtPreSimulationQ.SeriesColor := TColor($F59D81);

  { Define Line Series Titles }
  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationI.Title := 'Infected';
  self.chtPreSimulationQ.Title := 'Quarantined';

  { Activate Line Series }
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationI.Active := True;
  self.chtPreSimulationQ.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareSIQS(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 3);
  y0[1] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[2] := 0;                                                 // Q
  y0[0] := Length(frmMain.Nodes) - y0[1] - y0[2];             // S

  SetLength(extraArgs, 9);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma
  extraArgs[3] := StrToFloat(frmMain.edtMu.Text);             // Mu
  extraArgs[4] := StrToFloat(frmMain.edtLambda.Text);         // Lambda
  extraArgs[5] := StrToFloat(frmMain.edtDelta.Text);          // Delta
  extraArgs[6] := StrToFloat(frmMain.edtDelta1.Text);         // Delta1
  extraArgs[7] := StrToFloat(frmMain.edtKappa.Text);          // Kappa
  extraArgs[8] := StrToFloat(frmMain.edtZeta.Text);           // Zeta

  { Arrange Line Series Color }
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationI.SeriesColor := clMaroon;
  self.chtPreSimulationQ.SeriesColor := TColor($F59D81);

  { Define Line Series Titles }
  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationI.Title := 'Infected';
  self.chtPreSimulationQ.Title := 'Quarantined';

  { Activate Line Series }
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationI.Active := True;
  self.chtPreSimulationQ.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareSIRD(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 4);
  y0[1] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[2] := 0;                                                 // R
  y0[0] := Length(frmMain.Nodes) - y0[1] - y0[2];             // S
  y0[3] := 0;                                                 // D

  SetLength(extraArgs, 4);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma
  extraArgs[3] := StrToFloat(frmMain.edtMu.Text);             // Mu

  { Arrange Line Series Color }
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationI.SeriesColor := clOlive;
  self.chtPreSimulationR.SeriesColor := clGreen;
  self.chtPreSimulationD.SeriesColor := clMaroon;

  { Define Line Series Titles }
  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationI.Title := 'Infected';
  self.chtPreSimulationR.Title := 'Recovered';
  self.chtPreSimulationD.Title := 'Deceased';

  { Activate Line Series }
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationI.Active := True;
  self.chtPreSimulationR.Active := True;
  self.chtPreSimulationD.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareMSIR(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 4);
  y0[0] := StrToFloat(frmMain.edtMaternallyDerivedImmunity.Text);  // M
  y0[2] := StrToFloat(frmMain.edtInitialInfected.Text);            // I
  y0[3] := 0;                                                      // R
  { Susceptible (y0[1]) = N - Maternally derived immunity - Infected - Recovered }
  y0[1] := Length(frmMain.Nodes) - y0[0] - y0[2] - y0[3];          // S

  SetLength(extraArgs, 6);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma
  extraArgs[3] := StrToFloat(frmMain.edtMu.Text);             // Mu
  extraArgs[4] := StrToFloat(frmMain.edtLambda.Text);         // Lambda
  extraArgs[5] := StrToFloat(frmMain.edtDelta.Text);          // Delta

  { Arrange Line Series Color }
  self.chtPreSimulationM.SeriesColor := clFuchsia;
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationI.SeriesColor := clOlive;
  self.chtPreSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtPreSimulationM.Title := 'Maternally' + sLineBreak + 'derived' + sLineBreak + 'immunity';
  self.chtPreSimulationS.Title := ' ' + sLineBreak + 'Susceptible';
  self.chtPreSimulationI.Title := ' ' + sLineBreak + 'Infected';
  self.chtPreSimulationR.Title := ' ' + sLineBreak + 'Recovered';

  { Activate Line Series }
  self.chtPreSimulationM.Active := True;
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationI.Active := True;
  self.chtPreSimulationR.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareSEIR(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 4);
  y0[3] := 0;                                                 // R
  y0[2] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[1] := 0;                                                 // E
  y0[0] := Length(frmMain.Nodes) - y0[1] - y0[2] - y0[3];     // S

  SetLength(extraArgs, 6);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma
  extraArgs[3] := StrToFloat(frmMain.edtMu.Text);             // Mu
  extraArgs[4] := StrToFloat(frmMain.edtLambda.Text);         // Lambda
  extraArgs[5] := StrToFloat(frmMain.edtAlpha.Text);          // Alpha

  { Arrange Line Series Color }
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationE.SeriesColor := clOlive;
  self.chtPreSimulationI.SeriesColor := clMaroon;
  self.chtPreSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationE.Title := 'Exposed';
  self.chtPreSimulationI.Title := 'Infected';
  self.chtPreSimulationR.Title := 'Recovered';

  { Activate Line Series }
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationE.Active := True;
  self.chtPreSimulationI.Active := True;
  self.chtPreSimulationR.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareSEIS(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 3);
  y0[2] := StrToFloat(frmMain.edtInitialInfected.Text);       // I
  y0[1] := 0;                                                 // E
  y0[0] := Length(frmMain.Nodes) - y0[1] - y0[2];             // S

  SetLength(extraArgs, 6);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma
  extraArgs[3] := StrToFloat(frmMain.edtMu.Text);             // Mu
  extraArgs[4] := StrToFloat(frmMain.edtLambda.Text);         // Lambda
  extraArgs[5] := StrToFloat(frmMain.edtEpsilon.Text);        // Epsilon

  { Arrange Line Series Color }
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationE.SeriesColor := clOlive;
  self.chtPreSimulationI.SeriesColor := clMaroon;

  { Define Line Series Titles }
  self.chtPreSimulationS.Title := 'Susceptible';
  self.chtPreSimulationE.Title := 'Exposed';
  self.chtPreSimulationI.Title := 'Infected';

  { Activate Line Series }
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationE.Active := True;
  self.chtPreSimulationI.Active := True;
end;

procedure TfrmPreSimulationChart.PrepareMSEIR(var y0, extraArgs: ArrayOfDouble);
begin
  SetLength(y0, 5);
  y0[0] := StrToFloat(frmMain.edtMaternallyDerivedImmunity.Text);  // M
  y0[4] := 0;                                                      // R
  y0[3] := StrToFloat(frmMain.edtInitialInfected.Text);            // I
  y0[2] := 0;                                                      // E
  { Susceptible (y0[1]) = N - Maternally derived immunity - Exposed - Infected - Recovered }
  y0[1] := Length(frmMain.Nodes) - y0[0] - y0[2] - y0[3] - y0[4];  // S

  SetLength(extraArgs, 7);
  extraArgs[0] := Length(frmMain.Nodes);                      // N
  extraArgs[1] := StrToFloat(frmMain.edtBeta.Text);           // Beta
  extraArgs[2] := StrToFloat(frmMain.edtGamma.Text);          // Gamma
  extraArgs[3] := StrToFloat(frmMain.edtMu.Text);             // Mu
  extraArgs[4] := StrToFloat(frmMain.edtLambda.Text);         // Lambda
  extraArgs[5] := StrToFloat(frmMain.edtDelta.Text);          // Delta
  extraArgs[6] := StrToFloat(frmMain.edtEpsilon.Text);        // Epsilon

  { Arrange Line Series Color }
  self.chtPreSimulationM.SeriesColor := clFuchsia;
  self.chtPreSimulationS.SeriesColor := clNavy;
  self.chtPreSimulationE.SeriesColor := clOlive;
  self.chtPreSimulationI.SeriesColor := clMaroon;
  self.chtPreSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtPreSimulationM.Title := 'Maternally' + sLineBreak + 'derived' + sLineBreak + 'immunity';
  self.chtPreSimulationS.Title := ' ' + sLineBreak + 'Susceptible';
  self.chtPreSimulationE.Title := ' ' + sLineBreak + 'Exposed';
  self.chtPreSimulationI.Title := ' ' + sLineBreak + 'Infected';
  self.chtPreSimulationR.Title := ' ' + sLineBreak + 'Recovered';

  { Activate Line Series }
  self.chtPreSimulationM.Active := True;
  self.chtPreSimulationS.Active := True;
  self.chtPreSimulationE.Active := True;
  self.chtPreSimulationI.Active := True;
  self.chtPreSimulationR.Active := True;
end;

procedure TfrmPreSimulationChart.FormClose(Sender: TObject);
begin
  self.ClearPreSimulationChart;
end;

{$R *.lfm}

end.

