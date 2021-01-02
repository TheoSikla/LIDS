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

unit utlFile_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, Dialogs, fpjson, jsonparser,
  clNode_u, utlArray_u, utlTypes_u, utlConstants_u;

type
  TAppenderType = specialize TAppender<TNode>;
  ArrayOfTNodeType = Array of TNode;

  TFileHandler = class(TObject)
    public
      function IsAdjacencyMaxtrixOrListFile(filename, Atype: String): Boolean;
      function LoadAdjacencyMaxtrix(filename: String): TListOfTNode;
      function LoadAdjacencyList(filename: String): TListOfTNode;
      procedure WriteStringToFile(filename: string; data: String);
      procedure WriteToJsonFile(filename: string; data: TJSONData);
      function LoadJsonFile(filename: string): TJSONData;


  end;

var
  FileHandler: TFileHandler;

implementation
  uses
  { Forms }
  frmMain_u;

function TFileHandler.IsAdjacencyMaxtrixOrListFile(filename, Atype: String): Boolean;
var
  tfIn: TextFile;
  s: String;
  RegexObj: TRegExpr;
begin
  Result := False;
  AssignFile(tfIn, filename);
  RegexObj := TRegExpr.Create;
  if Atype = 'matrix' then begin
    RegexObj.Expression := ADJACENCY_MATRIX_REGEX;
  end
  else
    RegexObj.Expression := ADJACENCY_LIST_REGEX;

  try
    try
      reset(tfIn);
      if not eof(tfIn) then
         begin
           readln(tfIn, s);
           if RegexObj.Exec(s) then Result := True;
         end;
    except
      on E: EInOutError do
       writeln('File handling error occurred. Details: ', E.Message);
    end;

  finally
    CloseFile(tfIn);
    RegexObj.Free;
  end;

end;

function TFileHandler.LoadAdjacencyMaxtrix(filename: String): TListOfTNode;
var
  tfIn: TextFile;
  s: String;
  i: Integer;
  lineLength: Integer;
  RegexObj: TRegExpr;
  Nodes: TListOfTNode;
  appender: TAppenderType;
  obj: TNode;
  Neighbors: TWordList;
  c: char;
  RowIndex, ColumnIndex: Word;
begin
  // Set the name of the file that will be read
  AssignFile(tfIn, filename);

  // Create a regex to validate file structure
  RegexObj := TRegExpr.Create;
  RegexObj.Expression := ADJACENCY_MATRIX_REGEX;

  Nodes := TListOfTNode.Create;     // Initialize list
  appender := TAppenderType.Create; // Initialize instance

  frmMain.NumberOfEdges := 0;
  frmMain.AvgNumberOfNeighbors := 0;
  i := 0; RowIndex := 0; ColumnIndex := 0;
  try
    {
      Embed the file handling in a try/except block to handle
      errors gracefully
    }
    try
      // Open the file for reading
      reset(tfIn);

      // Grab the first line's length
      if not eof(tfIn) then
         begin
           readln(tfIn, s);
           lineLength := Length(s);

           Neighbors := TWordList.Create;

           for c in s do begin
             if c = '1' then
                begin
                  Neighbors.Add(ColumnIndex);
                  if (ColumnIndex > RowIndex) then Inc(frmMain.NumberOfEdges);
                end;
             Inc(ColumnIndex);
           end;
           ColumnIndex := 0;
           Inc(RowIndex);

           frmMain.AvgNumberOfNeighbors += Neighbors.Count;

           obj := TNode.Create(i, Neighbors);
           Inc(i);
           Nodes.Add(obj);
         end;

      // Keep reading lines until the end of the file is reached
      while not eof(tfIn) do
      begin
        readln(tfIn, s);

        // Validate each line of the file
        if not RegexObj.Exec(s) or (Length(s) <> lineLength) then
           begin
             MessageDlg('Error', 'Invalid file format', mtError, [mbOK], 0);
             Break; // Jump to the 'finally' block
           end;

        Neighbors := TWordList.Create;

        for c in s do begin
          if c = '1' then
             begin
               Neighbors.Add(ColumnIndex);
               if (ColumnIndex > RowIndex) then Inc(frmMain.NumberOfEdges);
             end;
          Inc(ColumnIndex);
        end;
        ColumnIndex := 0;
        Inc(RowIndex);

        frmMain.AvgNumberOfNeighbors += Neighbors.Count;

        obj := TNode.Create(i, Neighbors);
        Inc(i);
        Nodes.Add(obj);
        //writeln('Node: ' + IntToStr(i) + ' has {' + IntToStr(Neighbors.Count) + '} neighbors'); { Debug }
      end;

      frmMain.AvgNumberOfNeighbors := frmMain.AvgNumberOfNeighbors div Nodes.Count;
      //writeln('Average number of neighbors: ' + IntToStr(frmMain.AvgNumberOfNeighbors)); { Debug }

    except
      on E: EInOutError do
       writeln('File handling error occurred. Details: ', E.Message);
    end;

  finally
    CloseFile(tfIn);      // Close the file
    RegexObj.Free;        // Free the regex object
    FreeAndNil(appender); // Free the generic appender instance

    result := Nodes;
  end;

end;

function TFileHandler.LoadAdjacencyList(filename: String): TListOfTNode;
var
  tfIn: TextFile;
  s, neighbor: String;
  RegexObj: TRegExpr;
  Nodes: TListOfTNode;
  obj: TNode;
  Neighbors: TWordList;
  colonSplit, commaSplit: TStringArray;
begin
  AssignFile(tfIn, filename);

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := ADJACENCY_LIST_REGEX;

  Nodes := TListOfTNode.Create;

  frmMain.NumberOfEdges := 0;
  frmMain.AvgNumberOfNeighbors := 0;
  try
    try
      reset(tfIn);

      while not eof(tfIn) do
      begin
        readln(tfIn, s);

        // Validate each line of the file
        if not RegexObj.Exec(s) then
           begin
             MessageDlg('Error', 'Invalid file format', mtError, [mbOK], 0);
             Break; // Jump to the 'finally' block
           end;

        Neighbors := TWordList.Create;

        colonSplit := s.Split(':');
        commaSplit := colonSplit[1].Split(',');
        if not (commaSplit[0] = '') then begin
          for neighbor in commaSplit do begin
            Neighbors.Add(StrToInt(neighbor));
          end;
        end;

        frmMain.AvgNumberOfNeighbors += Neighbors.Count;

        obj := TNode.Create(StrToInt(colonSplit[0]), Neighbors);
        Nodes.Add(obj);
        //writeln('Node: ' + colonSplit[0] + ' has {' + IntToStr(Neighbors.Count) + '} neighbors'); { Debug }
      end;

      frmMain.AvgNumberOfNeighbors := frmMain.AvgNumberOfNeighbors div Nodes.Count;
      //writeln('Average number of neighbors: ' + IntToStr(frmMain.AvgNumberOfNeighbors)); { Debug }

    except
      on E: EInOutError do
       writeln('File handling error occurred. Details: ', E.Message);
    end;

  finally
    CloseFile(tfIn); // Close the file
    RegexObj.Free;   // Free the regex object

    result := Nodes;
  end;

end;

procedure TFileHandler.WriteStringToFile(filename: string; data: String);
var
  tfOut: TextFile;
begin
  try
    AssignFile(tfOut, filename);
    Rewrite(tfOut);
    writeln(tfOut, data);
    closefile(tfOut);
  except
    on E:Exception do
  end;
end;

procedure TFileHandler.WriteToJsonFile(filename: string; data: TJSONData);
begin
  self.WriteStringToFile(filename, data.FormatJSON);
end;

function TFileHandler.LoadJsonFile(filename: string): TJSONData;
var
  tfIn: TextFile;
  s, JsonString: String;
begin
  s := '';
  JsonString := '';

  try
    try
      AssignFile(tfIn, filename);
      reset(tfIn);

      while not eof(tfIn) do begin
        readln(tfIn, s);
        JsonString += s;
      end;

      Result := GetJSON(JsonString);
    except
      on E: EInOutError do
       writeln('File handling error occurred. Details: ', E.Message);
    end;
  finally
      CloseFile(tfIn);
  end;

end;

end.

