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

unit frmSimulationChart_u;

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
  utlEnum_u,
  utlConstants_u;

type

  { TfrmSimulationChart }

  TfrmSimulationChart = class(TForm)
    chtSimulation: TChart;
    chtSimulationI: TLineSeries;
    chtSimulationD: TLineSeries;
    chtSimulationE: TLineSeries;
    chtSimulationQ: TLineSeries;
    chtSimulationM: TLineSeries;
    chtSimulationR: TLineSeries;
    chtSimulationS: TLineSeries;
    procedure CalculateSimulation(SamplingResult: TArrayOfArrayOfWord);
    procedure FormClose(Sender: TObject);
    procedure ClearSimulationChart;
    procedure FormResize(Sender: TObject);
    procedure PrepareSIR;
    procedure PrepareSIS;
    procedure PrepareSIQ;
    procedure PrepareSIQS;
    procedure PrepareSIQR;
    procedure PrepareSIRD;
    procedure PrepareMSIR;
    procedure PrepareSEIR;
    procedure PrepareSEIS;
    procedure PrepareMSEIR;
  private

  public

  end;

var
  frmSimulationChart: TfrmSimulationChart;

implementation

procedure TfrmSimulationChart.CalculateSimulation(SamplingResult: TArrayOfArrayOfWord);
var
  t: ArrayOfDouble;
  i, days: Integer;
  model: String;
begin
  days := StrToInt(frmMain.edtDays.Text) + 2;
  SetLength(t, days);
  t := linspace(0, days, days);

  model := frmMain.cbxAvailableModels.Items[frmMain.cbxAvailableModels.ItemIndex];
  { Initiate based on the model }
  case model of
    SIR: self.PrepareSIR;
    SIS: self.PrepareSIS;
    SIQ: self.PrepareSIQ;
    SIQS: self.PrepareSIQS;
    SIQR: self.PrepareSIQR;
    SIRD: self.PrepareSIRD;
    MSIR: self.PrepareMSIR;
    SEIR: self.PrepareSEIR;
    SEIS: self.PrepareSEIS;
    MSEIR: self.PrepareMSEIR;
  end;

  { Fill data to the appropriate axes }
  case model of
    SIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][0]); // S
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][1]); // I
          self.chtSimulationR.AddXY(t[i], SamplingResult[i][2]); // R
        end;
      end;

    SIS: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][0]); // S
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][1]); // I
        end;
      end;

    SIQ, SIQS: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][0]); // S
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][1]); // I
          self.chtSimulationQ.AddXY(t[i], SamplingResult[i][2]); // Q
        end;
      end;

    SIQR: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][0]); // S
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][1]); // I
          self.chtSimulationQ.AddXY(t[i], SamplingResult[i][2]); // Q
          self.chtSimulationR.AddXY(t[i], SamplingResult[i][3]); // R
        end;
      end;

    SIRD: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][0]); // S
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][1]); // I
          self.chtSimulationR.AddXY(t[i], SamplingResult[i][2]); // R
          self.chtSimulationD.AddXY(t[i], SamplingResult[i][3]); // D
        end;
      end;

    MSIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationM.AddXY(t[i], SamplingResult[i][0]); // M
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][1]); // S
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][2]); // I
          self.chtSimulationR.AddXY(t[i], SamplingResult[i][3]); // R
        end;
      end;

    SEIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][0]); // S
          self.chtSimulationE.AddXY(t[i], SamplingResult[i][1]); // E
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][2]); // I
          self.chtSimulationR.AddXY(t[i], SamplingResult[i][3]); // R
        end;
      end;

    SEIS: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][0]); // S
          self.chtSimulationE.AddXY(t[i], SamplingResult[i][1]); // E
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][2]); // I
        end;
      end;

    MSEIR: begin
      for i := 0 to days - 1 do
        begin
          self.chtSimulationM.AddXY(t[i], SamplingResult[i][0]); // M
          self.chtSimulationS.AddXY(t[i], SamplingResult[i][1]); // S
          self.chtSimulationE.AddXY(t[i], SamplingResult[i][2]); // E
          self.chtSimulationI.AddXY(t[i], SamplingResult[i][3]); // I
          self.chtSimulationR.AddXY(t[i], SamplingResult[i][4]); // R
        end;
      end;

    end;

  self.chtSimulation.Visible := true;
end;

procedure TfrmSimulationChart.ClearSimulationChart;
var
  i: Integer;
begin
  for i := 0 to self.chtSimulation.SeriesCount - 1 do
  begin
    if self.chtSimulation.Series[i] is TLineSeries then
      begin
        (self.chtSimulation.Series[i] as TLineSeries).Clear;
        (self.chtSimulation.Series[i] as TLineSeries).Active := False;
      end;
  end;
end;

procedure TfrmSimulationChart.FormResize(Sender: TObject);
begin
  self.chtSimulation.Height := self.Height * 99 div 100;
  self.chtSimulation.Width := self.Width * 99 div 100;
end;

procedure TfrmSimulationChart.PrepareSIR;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationI.SeriesColor := clMaroon;
  self.chtSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationI.Title := 'Infected';
  self.chtSimulationR.Title := 'Recovered';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationR.Active := True;
end;

procedure TfrmSimulationChart.PrepareSIS;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationI.SeriesColor := clMaroon;

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationI.Title := 'Infected';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationI.Active := True;
end;

procedure TfrmSimulationChart.PrepareSIQ;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationI.SeriesColor := clMaroon;
  self.chtSimulationQ.SeriesColor := TColor($F59D81);

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationI.Title := 'Infected';
  self.chtSimulationQ.Title := 'Quarantined';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationQ.Active := True;
end;

procedure TfrmSimulationChart.PrepareSIQS;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationI.SeriesColor := clMaroon;
  self.chtSimulationQ.SeriesColor := TColor($F59D81);

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationI.Title := 'Infected';
  self.chtSimulationQ.Title := 'Quarantined';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationQ.Active := True;
end;

procedure TfrmSimulationChart.PrepareSIQR;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationI.SeriesColor := clMaroon;
  self.chtSimulationQ.SeriesColor := TColor($F59D81);
  self.chtSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationI.Title := 'Infected';
  self.chtSimulationQ.Title := 'Quarantined';
  self.chtSimulationR.Title := 'Recovered';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationQ.Active := True;
  self.chtSimulationR.Active := True;
end;

procedure TfrmSimulationChart.PrepareSIRD;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationI.SeriesColor := clOlive;
  self.chtSimulationR.SeriesColor := clGreen;
  self.chtSimulationD.SeriesColor := clMaroon;

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationI.Title := 'Infected';
  self.chtSimulationR.Title := 'Recovered';
  self.chtSimulationD.Title := 'Deceased';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationR.Active := True;
  self.chtSimulationD.Active := True;
end;

procedure TfrmSimulationChart.PrepareMSIR;
begin
  { Arrange Line Series Color }
  self.chtSimulationM.SeriesColor := clFuchsia;
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationI.SeriesColor := clOlive;
  self.chtSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtSimulationM.Title := 'Maternally' + sLineBreak + 'derived' + sLineBreak + 'immunity';
  self.chtSimulationS.Title := ' ' + sLineBreak + 'Susceptible';
  self.chtSimulationI.Title := ' ' + sLineBreak + 'Infected';
  self.chtSimulationR.Title := ' ' + sLineBreak + 'Recovered';

  { Activate Line Series }
  self.chtSimulationM.Active := True;
  self.chtSimulationS.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationR.Active := True;
end;

procedure TfrmSimulationChart.PrepareSEIR;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationE.SeriesColor := clOlive;
  self.chtSimulationI.SeriesColor := clMaroon;
  self.chtSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationE.Title := 'Exposed';
  self.chtSimulationI.Title := 'Infected';
  self.chtSimulationR.Title := 'Recovered';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationE.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationR.Active := True;
end;

procedure TfrmSimulationChart.PrepareSEIS;
begin
  { Arrange Line Series Color }
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationE.SeriesColor := clOlive;
  self.chtSimulationI.SeriesColor := clMaroon;

  { Define Line Series Titles }
  self.chtSimulationS.Title := 'Susceptible';
  self.chtSimulationE.Title := 'Exposed';
  self.chtSimulationI.Title := 'Infected';

  { Activate Line Series }
  self.chtSimulationS.Active := True;
  self.chtSimulationE.Active := True;
  self.chtSimulationI.Active := True;
end;

procedure TfrmSimulationChart.PrepareMSEIR;
begin
  { Arrange Line Series Color }
  self.chtSimulationM.SeriesColor := clFuchsia;
  self.chtSimulationS.SeriesColor := clNavy;
  self.chtSimulationE.SeriesColor := clOlive;
  self.chtSimulationI.SeriesColor := clMaroon;
  self.chtSimulationR.SeriesColor := clGreen;

  { Define Line Series Titles }
  self.chtSimulationM.Title := 'Maternally' + sLineBreak + 'derived' + sLineBreak + 'immunity';
  self.chtSimulationS.Title := ' ' + sLineBreak + 'Susceptible';
  self.chtSimulationE.Title := ' ' + sLineBreak + 'Exposed';
  self.chtSimulationI.Title := ' ' + sLineBreak + 'Infected';
  self.chtSimulationR.Title := ' ' + sLineBreak + 'Recovered';

  { Activate Line Series }
  self.chtSimulationM.Active := True;
  self.chtSimulationS.Active := True;
  self.chtSimulationE.Active := True;
  self.chtSimulationI.Active := True;
  self.chtSimulationR.Active := True;
end;

procedure TfrmSimulationChart.FormClose(Sender: TObject);
begin
  self.ClearSimulationChart;
end;

{$R *.lfm}

end.

