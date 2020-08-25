unit utlFile_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, Dialogs, clNode_u, utlArray_u;

type
  TAppenderType = specialize TAppender<TNode>;

{ Publicly accessible functions }
Function LoadGRATISAdjacencyMaxtrixFile(filename: String): Boolean;

implementation

  Function LoadGRATISAdjacencyMaxtrixFile(filename: String): Boolean;
  var
    tfIn: TextFile;
    s: String;
    i: Integer;
    lineLength: Integer;
    RegexObj: TRegExpr;
    nodes: Array of TNode;
    appender: TAppenderType;
    obj: TNode;
  begin
    // Set the name of the file that will be read
    AssignFile(tfIn, filename);

    // Create a regex to validate file structure
    RegexObj := TRegExpr.Create;
    RegexObj.Expression := '^[0-1]*$';

    SetLength(nodes, 0);              // Initialize list
    appender := TAppenderType.Create; // Initialize instance

    i := 0;
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
             obj := TNode.Create(i, s);
             Inc(i);
             appender.Append(nodes, obj);
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

          obj := TNode.Create(i, s);
          Inc(i);
          appender.Append(nodes, obj);
        end;

      except
        on E: EInOutError do
         writeln('File handling error occurred. Details: ', E.Message);
      end;

    finally
      CloseFile(tfIn);      // Close the file
      RegexObj.Free;        // Free the regex object
      FreeAndNil(appender); // Free the generic appender instance

      result := true;
    end;

  end;

end.

