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

unit clNode_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics;

type
  ArrayOfWord = Array of Word;
  PtrOfTShape = ^TShape;

type
  TNode = class(TObject)
    private
      FId: Word;
      FIsSusceptible: Boolean;
      FIsInfected: Boolean;
      FIsRecovered: Boolean;
      FInfectedByNode: Word;
      FNeighbors: ArrayOfWord;
      FPtrShape: PtrOfTShape;
    protected
      { protected declarations here }
    public
      constructor Create(
        AId: Word;
        ANeighbors: String;
        AIsSusceptible: Boolean = false;
        AIsInfected: Boolean = false;
        AIsRecovered: Boolean = false);

      destructor Destroy; override;

      { Getters }
      function GetId(): Word;
      function GetNeighbors(): ArrayOfWord;
      function GetIsSusceptible(): Boolean;
      function GetIsInfected(): Boolean;
      function GetIsRecovered(): Boolean;
      function GetShape(): PtrOfTShape;
      function GetInfectedByNode(): Word;

      { Setters }
      procedure SetIsSusceptible(AValue: Boolean);
      procedure SetIsInfected(AValue: Boolean);
      procedure SetIsRecovered(AValue: Boolean);
      procedure SetShape(AShape: PtrOfTShape);
      procedure SetInfectedByNode(AValue: Word);

      property InfectedByNode: Word read GetInfectedByNode write SetInfectedByNode;

      procedure Infect(InfectorNode: Word);
      procedure Restore;

    published
      { published declarations here }
  end;

implementation
  constructor TNode.Create(
    AId: Word;
    ANeighbors: String;
    AIsSusceptible: Boolean = false;
    AIsInfected: Boolean = false;
    AIsRecovered: Boolean = false);
  var
    c: Char;
    i: Word;
    index: Word;
  begin
     Fid := AId;
     FIsSusceptible := AIsSusceptible;
     FIsInfected := AIsInfected;
     FIsRecovered := AIsRecovered;

     SetLength(FNeighbors, Length(ANeighbors));
     i := 0;
     index := 0;
     for c in ANeighbors do begin
       if c = '1' then begin
         FNeighbors[i] := index;
         Inc(i);
       end;
       Inc(index);
     end;
  end;

  destructor TNode.Destroy;
  begin
     inherited; // Also call parent class destroyer
  end;

  { Getters }
  function TNode.GetId: Word;
    begin
       result := Fid;
    end;

  function TNode.GetIsSusceptible: Boolean;
    begin
      result := FIsSusceptible;
    end;

  function TNode.GetIsInfected: Boolean;
    begin
      result := FIsInfected;
    end;

  function TNode.GetIsRecovered: Boolean;
    begin
      result := FIsRecovered;
    end;

  function TNode.GetNeighbors: ArrayOfWord;
    begin
      result := FNeighbors;
    end;

  function TNode.GetShape: PtrOfTShape;
    begin
      result := FPtrShape;
    end;

  function TNode.GetInfectedByNode: Word;
    begin
      result := FInfectedByNode;
    end;

  { Setters }
  procedure TNode.SetIsSusceptible(AValue: Boolean);
    begin
      FIsSusceptible := AValue;
    end;

  procedure TNode.SetIsInfected(AValue: Boolean);
    begin
      FIsInfected := AValue;
    end;

  procedure TNode.SetIsRecovered(AValue: Boolean);
    begin
      FIsRecovered := AValue;
    end;

  procedure TNode.SetShape(AShape: PtrOfTShape);
    begin
      FPtrShape := AShape;
    end;

  procedure TNode.SetInfectedByNode(AValue: Word);
    begin
      FInfectedByNode := AValue;
    end;

  procedure TNode.Infect(InfectorNode: Word);
  begin
    self.SetIsInfected(True);
    self.SetInfectedByNode(InfectorNode);
    self.GetShape()^.Brush.Color := clRed;
  end;

  procedure TNode.Restore;
  begin
    self.SetIsInfected(False);
    self.SetInfectedByNode(0);
    self.GetShape()^.Brush.Color := clMedGray;
  end;

end.

