unit clNode_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNode = class(TObject)
    private
      FId: Word;
      FIsSusceptible: Boolean;
      FIsInfected: Boolean;
      FIsRecovered: Boolean;
    protected
      { protected declarations here }
    public
      constructor Create(AId: Word;
        AIsSusceptible: Boolean = false;
        AIsInfected: Boolean = false;
        AIsRecovered: Boolean = false);

      destructor Destroy; override;

      { Getters }
      function GetId(): Word;
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
  constructor TNode.Create(AId: Word;
    AIsSusceptible: Boolean = false;
    AIsInfected: Boolean = false;
    AIsRecovered: Boolean = false);
  begin
     Fid := AId;
     FIsSusceptible := AIsSusceptible;
     FIsInfected := AIsInfected;
     FIsRecovered := AIsRecovered;
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

