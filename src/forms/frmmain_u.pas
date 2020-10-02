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

unit frmMain_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, TAGraph,
  { Forms }
  { Classes }
  clNode_u,
  { Utilities }
  utlFile_u,
  utlValidation_u;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnImportDialog: TOpenDialog;
    btnSimulate: TButton;
    edtDays: TEdit;
    edtBeta: TEdit;
    edtGamma: TEdit;
    edtInitialInfected: TEdit;
    frmTimer: TTimer;
    lblDays: TLabel;
    lblBeta: TLabel;
    lblGamma: TLabel;
    lblInitialInfected: TLabel;
    mnuMainMenu: TMainMenu;
    mnuFileOpen: TMenuItem;
    mnuFile: TMenuItem;

    procedure RefreshGUI;
    procedure FormCreate(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure edtFloatKeyPress(Sender: TObject; var Key: char);
    procedure edtIntegerKeyPress(Sender: TObject; var Key: char);
    procedure edtKeyUpEnter(Sender: TObject; var Key: char);
    procedure preparePreSimulationChart;
    function validatePreSimulationChart: Boolean;
  private

  public
    Nodes: Array of TNode;

  end;

var
  frmMain: TfrmMain;

implementation

uses
  { Forms }
  frmSimulation_u,
  frmPreSimulationChart_u;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.mnuFileOpenClick(Sender: TObject);
var
  filename: string;
begin
  self.btnSimulate.Enabled := False;

  if btnImportDialog.Execute then
  begin
    filename := btnImportDialog.Filename;
  end;

  if filename <> '' then begin
    if Length(Nodes) > 0 then frmSimulation.ResetShapes;
    Nodes := LoadGRATISAdjacencyMaxtrixFile(filename);
    frmSimulation.RenderShapes;

    if self.validatePreSimulationChart AND (Length(Nodes) > 0)
       then self.preparePreSimulationChart;
  end;

  if Length(Nodes) > 0 then self.btnSimulate.Enabled := True;

end;

procedure TfrmMain.btnSimulateClick(Sender: TObject);
begin
  if self.btnSimulate.IsEnabled then begin
     frmSimulation.Show;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetLength(Nodes, 0);
end;

procedure TfrmMain.RefreshGUI;
begin
  self.Update;
end;

procedure TfrmMain.preparePreSimulationChart;
begin
  frmPreSimulationChart.ClearPreSimulationChart;
  frmPreSimulationChart.CalculatePreSimulation;
  frmPreSimulationChart.Show;
end;

procedure TfrmMain.edtIntegerKeyPress(Sender: TObject; var Key: char);
begin
  ValidateInteger(Sender, Key);
end;

procedure TfrmMain.edtFloatKeyPress(Sender: TObject; var Key: char);
begin
  ValidateFloat(Sender, Key);
end;

procedure TfrmMain.edtKeyUpEnter(Sender: TObject; var Key: char);
begin
  { Prepare the pre simulation chart if all conditions are met. }
  { Key #13 represents the Enter key }
  if (Key = #13) AND self.validatePreSimulationChart AND
     (Length(self.Nodes) > 0) then
  begin
    self.preparePreSimulationChart;
  end;
end;

function TfrmMain.validatePreSimulationChart: Boolean;
begin
  { If all the required TEdits contain a value then return True. }
  Result := True;
  if (self.edtDays.Text = '') OR (self.edtBeta.Text = '') OR
     (self.edtGamma.Text = '') OR (self.edtInitialInfected.Text = '')
     then Result := False;

end;

end.

