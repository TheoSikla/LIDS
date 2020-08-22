unit utlFile_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, Dialogs;

{ Publicly accessible functions }
Function LoadGRATISAdjacencyMaxtrixFile(filename: String): Boolean;

implementation

  Function LoadGRATISAdjacencyMaxtrixFile(filename: String): Boolean;
  var
    tfIn: TextFile;
    s: String;
    lineLength: Integer;
    RegexObj: TRegExpr;
  begin
    // Set the name of the file that will be read
    AssignFile(tfIn, filename);

    // Create a regex to validate file structure
    RegexObj := TRegExpr.Create;
    RegexObj.Expression := '^[0-1]*$';

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

          writeln(s);
        end;

      except
        on E: EInOutError do
         writeln('File handling error occurred. Details: ', E.Message);
      end;

    finally
      CloseFile(tfIn); // Close the file
      RegexObj.Free;   // Free the regex object
      result := true;
    end;

  end;

end.

