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

unit frmSettings_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, fpjson,
  { Utilities }
  utlFile_u,
  utlConstants_u,
  utlValidation_u;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnCancel: TButton;
    btnApply: TButton;
    cbxReSimulate: TCheckBox;
    edtReSimulateMinRecoveredNodeCount: TEdit;
    lblMinRecoveredSIR: TLabel;
    lblSimulation: TLabel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure cbxReSimulateChange(Sender: TObject);
    procedure edtReSimulateMinRecoveredNodeCountKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadSettings;
  private

  public

  end;

var
  frmSettings: TfrmSettings;

implementation

procedure TfrmSettings.LoadSettings;
var
  i, j: Integer;
  JsonData: TJSONData;
  JsonObject, settings: TJSONObject;
begin
  { Generate default settings if they do not exist }
  if not FileExists(SETTINGS_FILE_NAME) then FileHandler.WriteToJsonFile(SETTINGS_FILE_NAME, GetJSON(DEFAULT_SETTINGS));
  { Load the settings }
  try
    JsonData := FileHandler.LoadJsonFile(SETTINGS_FILE_NAME);
    JsonObject := TJSONObject(JsonData);
    settings := JsonObject.Find(SETTINGS_NAME) as TJSONObject;
    for i := 0 to settings.Count - 1 do begin
      for j := 0 to self.ComponentCount - 1 do begin
        if self.Components[j].name = settings.Names[i] then begin
          if self.Components[j] is TCheckBox then (self.Components[j] as TCheckbox).Checked := settings.FindPath(TJSONObject(settings).Names[i]).AsBoolean
          else if self.Components[j] is TEdit then (self.Components[j] as TEdit).Text := settings.FindPath(TJSONObject(settings).Names[i]).AsString;
        end;
      end;
    end;
  except on E:Exception do begin end;
  end;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  inherited;
  self.LoadSettings; // Load Application Settings
  if self.cbxReSimulate.Checked then self.edtReSimulateMinRecoveredNodeCount.Enabled := True
  else self.edtReSimulateMinRecoveredNodeCount.Enabled := False;
end;

procedure TfrmSettings.FormClose(Sender: TObject);
begin
  self.LoadSettings;
  inherited;
end;

procedure TfrmSettings.btnCancelClick(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmSettings.btnApplyClick(Sender: TObject);
var
 JsonObject, settings: TJSONObject;
begin
  { Load the default settings }
  JsonObject := TJSONObject(GetJSON(DEFAULT_SETTINGS));

  { Get the new settings }
  settings := JsonObject.Find(SETTINGS_NAME) as TJSONObject;
  settings.Booleans[RE_SIMULATE_SETTING_NAME] := self.cbxReSimulate.Checked;
  if self.edtReSimulateMinRecoveredNodeCount.Text <> '' then settings.Strings[RE_SIMULATE_MINIMUM_RECOVERED_NODE_COUNT_SETTING_NAME] := self.edtReSimulateMinRecoveredNodeCount.Text
  else settings.Strings[RE_SIMULATE_MINIMUM_RECOVERED_NODE_COUNT_SETTING_NAME] := '0';

  { Write the new settings }
  FileHandler.WriteToJsonFile(SETTINGS_FILE_NAME, TJSONData(JsonObject));

  self.LoadSettings; // Load Application Settings
end;

procedure TfrmSettings.cbxReSimulateChange(Sender: TObject);
begin
  if self.cbxReSimulate.Checked then begin
    self.edtReSimulateMinRecoveredNodeCount.Enabled := True;
  end
  else self.edtReSimulateMinRecoveredNodeCount.Enabled := False;
end;

procedure TfrmSettings.edtReSimulateMinRecoveredNodeCountKeyPress(Sender: TObject; var Key: char
  );
begin
  ValidateInteger(Sender, Key);
end;

{$R *.lfm}

end.

