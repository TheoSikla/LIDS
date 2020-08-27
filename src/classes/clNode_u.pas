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
  Classes, SysUtils, ExtCtrls;

type
  ArrayOfBoolean = Array of Boolean;
  PtrOfTShape = ^TShape;

type
  TNode = class(TObject)
    private
      FId: Word;
      FIsSusceptible: Boolean;
      FIsInfected: Boolean;
      FIsRecovered: Boolean;
      FNeighbors: ArrayOfBoolean;
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
      function GetNeighbors(): ArrayOfBoolean;
      function GetIsSusceptible(): Boolean;
      function GetIsInfected(): Boolean;
      function GetIsRecovered(): Boolean;
      function GetShape(): PtrOfTShape;

      { Setters }
      procedure SetIsSusceptible(AValue: Boolean);
      procedure SetIsInfected(AValue: Boolean);
      procedure SetIsRecovered(AValue: Boolean);
      procedure SetShape(AShape: PtrOfTShape);

      { Misc }
      procedure FreeShape;

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
  begin
     Fid := AId;
     FIsSusceptible := AIsSusceptible;
     FIsInfected := AIsInfected;
     FIsRecovered := AIsRecovered;

     SetLength(FNeighbors, Length(ANeighbors));
     i := 0;
     for c in ANeighbors do begin
       FNeighbors[i] := StrToBool(c);
       Inc(i);
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

  function TNode.GetNeighbors: ArrayOfBoolean;
    begin
      result := FNeighbors;
    end;

  function TNode.GetShape: PtrOfTShape;
    begin
      result := FPtrShape;
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

  { Misc }
  procedure TNode.FreeShape;
  begin
    FreeAndNil(FPtrShape^);
  end;

end.

