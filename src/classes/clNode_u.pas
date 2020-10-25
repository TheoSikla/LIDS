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
  Classes, SysUtils, ExtCtrls, Graphics, fgl;

type
  ArrayOfWord = Array of Word;
  PtrOfTShape = ^TShape;
  TWordList = specialize TFPGList<Word>;

type
  TNode = class(TObject)
    private
      FId: Word;
      FIsSusceptible: Boolean;
      FIsInfected: Boolean;
      FIsRecovered: Boolean;
      FInfectedByNode: Word;
      FNeighbors: TWordList;
      FPtrShape: PtrOfTShape;
    protected
      { protected declarations here }
    public
      constructor Create(
        AId: Word;
        ANeighbors: TWordList = Nil;
        AIsSusceptible: Boolean = false;
        AIsInfected: Boolean = false;
        AIsRecovered: Boolean = false);

      destructor Destroy; override;

      property Id: Word read FId write FId;
      property IsSusceptible: Boolean read FIsSusceptible write FIsSusceptible;
      property IsInfected: Boolean read FIsInfected write FIsInfected;
      property IsRecovered: Boolean read FIsRecovered write FIsRecovered;
      property InfectedByNode: Word read FInfectedByNode write FInfectedByNode;
      property Neighbors: TWordList read FNeighbors;
      property PtrShape: PtrOfTShape read FPtrShape write FPtrShape;

      function GetNumberOfNeighbors(): Word;

      procedure Infect(InfectorNode: Word);
      procedure Recover;
      procedure Restore;

    published
      { published declarations here }
  end;

implementation
  constructor TNode.Create(
    AId: Word;
    ANeighbors: TWordList = Nil;
    AIsSusceptible: Boolean = false;
    AIsInfected: Boolean = false;
    AIsRecovered: Boolean = false);
  begin
     Fid := AId;
     FIsSusceptible := AIsSusceptible;
     FIsInfected := AIsInfected;
     FIsRecovered := AIsRecovered;
     FNeighbors := TWordList.Create;
     FNeighbors := ANeighbors;
  end;

  destructor TNode.Destroy;
  begin
     inherited; // Also call parent class destroyer
  end;

  function TNode.GetNumberOfNeighbors(): Word;
  begin
    Result := self.Neighbors.Count;
  end;

  procedure TNode.Infect(InfectorNode: Word);
  begin
    self.IsSusceptible := False;
    self.IsInfected := True;
    self.IsRecovered := False;

    self.InfectedByNode := InfectorNode;
    self.PtrShape^.Brush.Color := clRed;
  end;

  procedure TNode.Recover;
  begin
    self.IsSusceptible := False;
    self.IsInfected := False;
    self.IsRecovered := True;
  end;

  procedure TNode.Restore;
  begin
    self.IsSusceptible := False;
    self.IsInfected := False;
    self.IsRecovered := False;

    self.InfectedByNode := 0;
    self.PtrShape^.Brush.Color := clMedGray;
  end;

end.

