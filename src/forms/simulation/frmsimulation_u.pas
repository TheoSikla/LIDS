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
  math, DateUtils, fgl,
  { Classes }
  clNode_u,
  utlArray_u,
  utlTypes_u;

type
  TShapePointerList = array[0..MaxListSize - 1] of ^TShape;
  TAppenderType = specialize TAppender<Word>;
  ArrayOfWord = Array of Word;
  ArrayOfArrayOfWord = Array of ArrayOfWord;
  TAppenderArrayOfWordType = specialize TAppender<ArrayOfWord>;

  { TfrmSimulation }

  TfrmSimulation = class(TForm)
   frmSmlInvoker: TTimer;

    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Show;
    procedure RenderShapes;
    procedure ResetShapes;
    procedure RestoreNodes;
    function InfectRandomNodes(NumberOfNodesToInfect: Word): TWordList;
    procedure InfectNode(NodeToBeInfected: TNode; InfectorNodeId: Word);
  private

  public
    TShapePtrArray: TShapePointerList;
    appender: TAppenderType;
    appenderTAppenderArrayOfWordType: TAppenderArrayOfWordType;

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
{ During the initial simulation, gather all pointers of the TShape objects
  to the relative list that will hold those pointers during each simulation. }

//var
//  i: Integer;
//
begin
//  if not Assigned(TShapePtrArray[0]) then begin
//    for i := 0 to self.ComponentCount - 1 do begin
//        TShapePtrArray[i] := self.FieldAddress('shp' + IntToStr(i));
//    end;
//  end;
end;

procedure TfrmSimulation.FormClose(Sender: TObject);
begin
  { Make sure all Nodes and their Shapes are restored to their original state. }
  self.RestoreNodes;
end;

procedure TfrmSimulation.Show;
begin
  inherited;
  frmSimulation.frmSmlInvoker.Enabled := True;
end;

procedure TfrmSimulation.RenderShapes;
var
  i: Integer;
  ShapeWidth: Integer;
  ShapeHeight: Integer;
  ShapeTopPossition: Integer = 0;
  ShapeLeftPossition: Integer = 0;
  HorizontalObjectCount: Integer;
  VerticalObjectCount: Integer;
begin
  { Define the width and height of each TShape object given the 2d dimensions of
    the simulation window. }

  { ************************************************************************** }
  ShapeWidth := Floor(Sqrt(self.Width * self.Height / frmMain.Nodes.Count));
  HorizontalObjectCount := Floor(self.Width / ShapeWidth);
  VerticalObjectCount := Floor(self.Height / ShapeWidth);

  while HorizontalObjectCount * VerticalObjectCount < frmMain.Nodes.Count do
  begin
      Dec(ShapeWidth);
      HorizontalObjectCount := Floor(self.Width / ShapeWidth);
      VerticalObjectCount := Floor(self.Height / ShapeWidth);
  end;

  ShapeHeight := ShapeWidth;
  { ************************************************************************** }

  { Render all the TShape objects needed for the simulation, using the
    dimensions that were calculated above. A small padding is used to the right
    of the simulation window in order to ensure that all TShape objects will
    gracefully appear inside the simulation window. }

  if Assigned(frmMain.Nodes) and (frmMain.Nodes.Count > 0) then begin

    for i := 0 to frmMain.Nodes.Count - 1 do begin

        if ShapeLeftPossition >= (self.Width - ShapeWidth + 1) then
        begin
          ShapeTopPossition := ShapeTopPossition + ShapeHeight;
          ShapeLeftPossition := 0;
        end;

        self.TShapePtrArray[i]^.Height := ShapeHeight;
        self.TShapePtrArray[i]^.Width := ShapeWidth;
        self.TShapePtrArray[i]^.Left := ShapeLeftPossition;
        self.TShapePtrArray[i]^.Top := ShapeTopPossition;
        self.TShapePtrArray[i]^.Visible := true;

        frmMain.Nodes[i].PtrShape := self.TShapePtrArray[i];

        ShapeLeftPossition := ShapeLeftPossition + ShapeWidth;
    end;

  end;

end;

procedure TfrmSimulation.ResetShapes;
{ Make sure that every TShape object that became visible during the simulation
  is now again not visible. }

//var
//  i: Integer;

begin
  //for i := 0 to frmMain.Nodes.Count - 1 do begin
  //  self.TShapePtrArray[i]^.Visible := false;
  //  self.TShapePtrArray[i]^.Brush.Color := clMedGray;
  //end;
  frmMain.Nodes.Clear;
end;

function TfrmSimulation.InfectRandomNodes(NumberOfNodesToInfect: Word): TWordList;
var
  i: Integer;
  NodeToBeInfected: Word;

begin
  Result := TWordList.Create;

  for i := 0 to NumberOfNodesToInfect - 1 do begin;
    { Infect the first node/s }
    NodeToBeInfected := Random(frmMain.Nodes.Count);

    while frmMain.Nodes[NodeToBeInfected].Neighbors.Count = 0 do begin
      NodeToBeInfected := Random(frmMain.Nodes.Count);
    end;

    frmMain.Nodes[NodeToBeInfected].Infect(NodeToBeInfected);
    Result.Add(NodeToBeInfected);
  end;
end;

procedure TfrmSimulation.InfectNode(NodeToBeInfected: TNode; InfectorNodeId: Word);
begin
  NodeToBeInfected.Infect(InfectorNodeId);
end;

procedure TfrmSimulation.RestoreNodes;
var
  i: Integer;
begin
  for i := 0 to frmMain.Nodes.Count - 1 do frmMain.Nodes[i].Restore;
end;

end.

