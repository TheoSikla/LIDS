unit frmSettings_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, fpjson,
  strutils,
  { Utilities }
  utlFile_u,
  utlConstants_u;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnCancel: TButton;
    btnApply: TButton;
    cbxReSimulate: TCheckBox;
    lblSimulation: TLabel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
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
        if AnsiContainsStr(self.Components[j].name, settings.Names[i]) then begin
          if self.Components[j] is TCheckBox then (self.Components[j] as TCheckbox).Checked := settings.FindPath(TJSONObject(settings).Names[i]).AsBoolean;
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

  { Write the new settings }
  FileHandler.WriteToJsonFile(SETTINGS_FILE_NAME, TJSONData(JsonObject));
end;

{$R *.lfm}

end.

