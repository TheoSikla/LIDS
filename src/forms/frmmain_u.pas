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
  Menus, TAGraph, typinfo,
  { Forms }
  { Classes }
  clNode_u,
  { Utilities }
  utlFile_u,
  utlValidation_u,
  utlEnum_u;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnImportDialog: TOpenDialog;
    btnSimulate: TButton;
    cbxAvailableModels: TComboBox;
    edtN: TEdit;
    edtDelta1: TEdit;
    edtEpsilon: TEdit;
    edtDays: TEdit;
    edtBeta: TEdit;
    edtAlpha: TEdit;
    edtZeta: TEdit;
    edtKappa: TEdit;
    edtGamma: TEdit;
    edtInitialInfected: TEdit;
    edtMaternallyDerivedImmunity: TEdit;
    edtMu: TEdit;
    edtLambda: TEdit;
    edtDelta: TEdit;
    frmTimer: TTimer;
    lblN: TLabel;
    lblDelta1: TLabel;
    lblEpsilon: TLabel;
    lblDays: TLabel;
    lblBeta: TLabel;
    lblAlpha: TLabel;
    lblZeta: TLabel;
    lblKappa: TLabel;
    lblGamma: TLabel;
    lblMaternallyDerivedImmunity: TLabel;
    lblMu: TLabel;
    lblInitialInfected: TLabel;
    lblLambda: TLabel;
    lblDelta: TLabel;
    mnuFileClose: TMenuItem;
    mnuMainMenu: TMainMenu;
    mnuFileOpen: TMenuItem;
    mnuFile: TMenuItem;

    procedure cbxAvailableModelsChange(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure RefreshGUI;
    procedure FormCreate(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure edtFloatKeyPress(Sender: TObject; var Key: char);
    procedure edtIntegerKeyPress(Sender: TObject; var Key: char);
    procedure edtKeyUpEnter(Sender: TObject; var Key: char);
    procedure preparePreSimulationChart;
    procedure registerAvailableModels;
    function validatePreSimulationChart: Boolean;
    function getN: Integer;
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
    self.edtN.Enabled := False;
    self.mnuFileClose.Enabled := True;

    if self.validatePreSimulationChart then self.preparePreSimulationChart;
  end;

  if Length(Nodes) > 0 then self.btnSimulate.Enabled := True;

end;

procedure TfrmMain.btnSimulateClick(Sender: TObject);
begin
  { Do not allow the close file function to be invokable while simulating }
  self.mnuFileClose.Enabled := False;
  if self.btnSimulate.IsEnabled then begin
     frmSimulation.Show;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetLength(Nodes, 0);
  self.registerAvailableModels;
end;

procedure TfrmMain.RefreshGUI;
begin
  self.Update;
end;

procedure TfrmMain.cbxAvailableModelsChange(Sender: TObject);
begin
  case self.cbxAvailableModels.Items[self.cbxAvailableModels.ItemIndex] of
    SIR, SIS: begin
       self.edtGamma.Enabled := True;

       self.edtMu.Enabled := False;
       self.edtLambda.Enabled := False;
       self.edtDelta.Enabled := False;
       self.edtMaternallyDerivedImmunity.Enabled := False;
       self.edtAlpha.Enabled := False;
       self.edtEpsilon.Enabled := False;
       self.edtKappa.Enabled := False;
       self.edtDelta1.Enabled := False;
       self.edtZeta.Enabled := False;
    end;

    SIQ: begin
       self.edtMu.Enabled := True;
       self.edtLambda.Enabled := True;
       self.edtDelta.Enabled := True;
       self.edtKappa.Enabled := True;

       self.edtGamma.Enabled := False;
       self.edtMaternallyDerivedImmunity.Enabled := False;
       self.edtAlpha.Enabled := False;
       self.edtEpsilon.Enabled := False;
       self.edtDelta1.Enabled := False;
       self.edtZeta.Enabled := False;
    end;

    SIQS, SIQR: begin
       self.edtGamma.Enabled := True;
       self.edtMu.Enabled := True;
       self.edtLambda.Enabled := True;
       self.edtDelta.Enabled := True;
       self.edtDelta1.Enabled := True;
       self.edtKappa.Enabled := True;
       self.edtZeta.Enabled := True;

       self.edtMaternallyDerivedImmunity.Enabled := False;
       self.edtAlpha.Enabled := False;
       self.edtEpsilon.Enabled := False;
    end;

    SIRD: begin
       self.edtGamma.Enabled := True;
       self.edtMu.Enabled := True;

       self.edtLambda.Enabled := False;
       self.edtDelta.Enabled := False;
       self.edtMaternallyDerivedImmunity.Enabled := False;
       self.edtAlpha.Enabled := False;
       self.edtEpsilon.Enabled := False;
       self.edtKappa.Enabled := False;
       self.edtDelta1.Enabled := False;
       self.edtZeta.Enabled := False;
    end;

    MSIR: begin
       self.edtGamma.Enabled := True;
       self.edtMu.Enabled := True;
       self.edtLambda.Enabled := True;
       self.edtDelta.Enabled := True;
       self.edtMaternallyDerivedImmunity.Enabled := True;

       self.edtAlpha.Enabled := False;
       self.edtEpsilon.Enabled := False;
       self.edtKappa.Enabled := False;
       self.edtDelta1.Enabled := False;
       self.edtZeta.Enabled := False;
    end;

    SEIR: begin
       self.edtGamma.Enabled := True;
       self.edtMu.Enabled := True;
       self.edtLambda.Enabled := True;
       self.edtAlpha.Enabled := True;

       self.edtDelta.Enabled := False;
       self.edtMaternallyDerivedImmunity.Enabled := False;
       self.edtEpsilon.Enabled := False;
       self.edtKappa.Enabled := False;
       self.edtDelta1.Enabled := False;
       self.edtZeta.Enabled := False;
    end;

    SEIS: begin
       self.edtGamma.Enabled := True;
       self.edtMu.Enabled := True;
       self.edtLambda.Enabled := True;
       self.edtEpsilon.Enabled := True;

       self.edtDelta.Enabled := False;
       self.edtMaternallyDerivedImmunity.Enabled := False;
       self.edtAlpha.Enabled := False;
       self.edtKappa.Enabled := False;
       self.edtDelta1.Enabled := False;
       self.edtZeta.Enabled := False;
    end;

    MSEIR: begin
       self.edtGamma.Enabled := True;
       self.edtMu.Enabled := True;
       self.edtLambda.Enabled := True;
       self.edtEpsilon.Enabled := True;
       self.edtDelta.Enabled := True;
       self.edtMaternallyDerivedImmunity.Enabled := True;

       self.edtAlpha.Enabled := False;
       self.edtKappa.Enabled := False;
       self.edtDelta1.Enabled := False;
       self.edtZeta.Enabled := False;
    end;

  end;
end;

procedure TfrmMain.mnuFileCloseClick(Sender: TObject);
begin
  if frmSimulation.Visible then frmSimulation.Close;
  if Length(Nodes) > 0 then frmSimulation.ResetShapes;
  self.edtN.Enabled := True;
  self.mnuFileClose.Enabled := False;
  self.btnSimulate.Enabled := False;
end;

procedure TfrmMain.preparePreSimulationChart;
begin
  frmPreSimulationChart.ClearPreSimulationChart;
  frmPreSimulationChart.CalculatePreSimulation;
  frmPreSimulationChart.Show;
end;

procedure TfrmMain.registerAvailableModels;
var
  i: Integer;
begin
  for i := Low(AvailableModels) to High(AvailableModels) do
  self.cbxAvailableModels.Items.Add(AvailableModels[i]);

  self.cbxAvailableModels.ItemIndex := 0;
  cbxAvailableModelsChange(self.cbxAvailableModels);
  self.cbxAvailableModels.Style := csDropDownList;
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
  if (Key = #13) AND self.validatePreSimulationChart then
    self.preparePreSimulationChart;
end;

function TfrmMain.getN: Integer;
begin
  Result := 0;
  if self.edtN.Text <> '' then Result := StrToInt(self.edtN.Text);
  if Length(frmMain.Nodes) > 0 then Result := Length(frmMain.Nodes);
end;

function TfrmMain.validatePreSimulationChart: Boolean;
begin
  { If all the required TEdits contain a value then return True. }
  Result := False;
  if self.getN <> 0.0 then
  begin
    case self.cbxAvailableModels.Items[self.cbxAvailableModels.ItemIndex] of
    SIR, SIS: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtGamma.Text <> '') AND (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    SIQ: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtMu.Text <> '') AND (self.edtLambda.Text <> '') AND
              (self.edtDelta.Text <> '') AND (self.edtKappa.Text <> '') AND
              (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    SIQS, SIQR: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtGamma.Text <> '') AND (self.edtMu.Text <> '') AND
              (self.edtLambda.Text <> '') AND (self.edtDelta.Text <> '') AND
              (self.edtDelta1.Text <> '') AND (self.edtKappa.Text <> '') AND
              (self.edtZeta.Text <> '') AND (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    SIRD: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtGamma.Text <> '') AND (self.edtMu.Text <> '')
              AND (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    MSIR: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtGamma.Text <> '') AND (self.edtMu.Text <> '') AND
              (self.edtLambda.Text <> '') AND (self.edtDelta.Text <> '') AND
              (self.edtMaternallyDerivedImmunity.Text <> '') AND
              (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    SEIR: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtGamma.Text <> '') AND (self.edtMu.Text <> '') AND
              (self.edtLambda.Text <> '') AND (self.edtAlpha.Text <> '') AND
              (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    SEIS: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtGamma.Text <> '') AND (self.edtMu.Text <> '') AND
              (self.edtLambda.Text <> '') AND (self.edtEpsilon.Text <> '') AND
              (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    MSEIR: begin
           if (self.edtDays.Text <> '') AND (self.edtBeta.Text <> '') AND
              (self.edtGamma.Text <> '') AND (self.edtMu.Text <> '') AND
              (self.edtLambda.Text <> '') AND (self.edtEpsilon.Text <> '') AND
              (self.edtDelta.Text <> '') AND
              (self.edtMaternallyDerivedImmunity.Text <> '') AND
              (self.edtInitialInfected.Text <> '')
                then Result := True;
         end;

    end;
  end;
end;

end.

