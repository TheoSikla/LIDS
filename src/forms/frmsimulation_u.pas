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

unit frmSimulation_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLIntf,
  math;

type

  { TfrmSimulation }

  TfrmSimulation = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure Show;
  private

  public

  end;

var
  frmSimulation: TfrmSimulation;

implementation

{$R *.lfm}

uses
  { Forms }
  frmMain_u;

{ TfrmSimulation }

procedure TfrmSimulation.FormCreate(Sender: TObject);
begin
end;

procedure TfrmSimulation.Show;
var
  i: Integer;
  ShapeWidth: Integer;
  ShapeHeight: Integer;
  ShapeTopPossition: Integer = 0;
  ShapeLeftPossition: Integer = 0;
  HorizontalObjectCount: Integer;
  VerticalObjectCount: Integer;
  ShapePtr: ^TShape;
  Shapes: Array of TShape;
begin
  inherited;

  ShapeWidth := Floor(Sqrt(self.Width * self.Height / Length(frmMain.Nodes)));
  HorizontalObjectCount := Floor(self.Width / ShapeWidth);
  VerticalObjectCount := Floor(self.Height / ShapeWidth);

  while HorizontalObjectCount * VerticalObjectCount < Length(frmMain.Nodes) do
  begin
      Dec(ShapeWidth);
      HorizontalObjectCount := Floor(self.Width / ShapeWidth);
      VerticalObjectCount := Floor(self.Height / ShapeWidth);
  end;

  ShapeHeight := ShapeWidth;

  SetLength(Shapes, Length(frmMain.Nodes));

  if Assigned(frmMain.Nodes) and (Length(frmMain.Nodes) > 0) then begin

    for i := 0 to Length(frmMain.Nodes) - 1 do begin

        if ShapeLeftPossition >= (self.Width - ShapeWidth + 1) then begin
          ShapeTopPossition := ShapeTopPossition + ShapeHeight;
          ShapeLeftPossition := 0;
        end;

        Shapes[i] := TShape.Create(self);
        Shapes[i].Name := 'shp' + IntToStr(i);
        Shapes[i].Parent := self;
        Shapes[i].Shape := stCircle;
        Shapes[i].Height := ShapeHeight;
        Shapes[i].Width := ShapeWidth;
        Shapes[i].Left := ShapeLeftPossition;
        Shapes[i].Top := ShapeTopPossition;
        Shapes[i].Brush.Color := clWhite;
        Shapes[i].Visible := true;

        frmMain.Nodes[i].SetShape(@Shapes[i]);

        ShapeLeftPossition := ShapeLeftPossition + ShapeWidth;
    end;

  end;

end;

end.

