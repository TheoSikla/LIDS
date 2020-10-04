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

program bvs;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg,
  { Forms }
  frmMain_u,
  frmSimulation_u,
  frmPreSimulationChart_u,
  { Classes }
  clNode_u,
  { Utilities }
  utlConstants_u,
  utlFile_u,
  utlArray_u, utlEnum_u, utlTypes_u, utlValidation_u,
  utlEuler_u,
  utlSIR_u,
  utlSIS_u,
  utlSIRD_u,
  utlMSIR_u;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSimulation, frmSimulation);
  Application.CreateForm(Tfrmpresimulationchart, frmpresimulationchart);
  Application.Run;
end.

