unit frmMain_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, utlFile_u;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnImport: TButton;
    btnImportDialog: TOpenDialog;
    procedure btnImportClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnImportClick(Sender: TObject);
var
  filename: string;
begin
  if btnImportDialog.Execute then
  begin
    filename := btnImportDialog.Filename;
  end;
  LoadGRATISAdjacencyMaxtrixFile(filename);
end;

end.

