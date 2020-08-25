unit clNode_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ArrayOfBoolean = Array of Boolean;
type
  TNode = class(TObject)
    private
      FId: Word;
      FIsSusceptible: Boolean;
      FIsInfected: Boolean;
      FIsRecovered: Boolean;
      FNeighbors: ArrayOfBoolean;
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

      { Setters }
      procedure SetIsSusceptible(AValue: Boolean);
      procedure SetIsInfected(AValue: Boolean);
      procedure SetIsRecovered(AValue: Boolean);

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

end.

