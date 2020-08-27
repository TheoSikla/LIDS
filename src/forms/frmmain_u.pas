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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  { Forms }
  frmSimulation_u,
  { Classes }
  clNode_u,
  { Utilities }
  utlFile_u;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnImport: TButton;
    btnImportDialog: TOpenDialog;
    btnSimulate: TButton;
    procedure btnImportClick(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    Nodes: Array of TNode;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnImportClick(Sender: TObject);
var
  filename: string;
  i: Integer;
begin
  if Length(Nodes) > 0 then
  begin
    for i:=0 to Length(Nodes) - 1 do begin
      Nodes[i].FreeShape;
      FreeAndNil(Nodes[i]);
    end;
  end;

  SetLength(Nodes, 0);

  if btnImportDialog.Execute then
  begin
    filename := btnImportDialog.Filename;
  end;

  Nodes := LoadGRATISAdjacencyMaxtrixFile(filename);
end;

procedure TfrmMain.btnSimulateClick(Sender: TObject);
begin
  frmSimulation.Show;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetLength(Nodes, 0);
end;

end.

