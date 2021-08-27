{
                Copyright (C) 2020 - 2021 Theodoros Siklafidis

    This file is part of LIDS.

    LIDS is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    LIDS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with LIDS. If not, see <https://www.gnu.org/licenses/>.
}

unit AlgSIR_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { Classes }
  clNode_u,
  utlTypes_u,
  utlMisc;

procedure SIRALG(days, NumberOfInitialInfectedNodes: Word;
                beta, gamma: Double;
                ProbabilityOfInfection: Byte;
                var SamplingResult: TArrayOfArrayOfWord);

implementation
uses
  { Forms }
  frmMain_u,
  frmSimulation_u,
  frmSettings_u;

procedure SIRALG(days, NumberOfInitialInfectedNodes: Word;
                beta, gamma: Double;
                ProbabilityOfInfection: Byte;
                var SamplingResult: TArrayOfArrayOfWord);
var
  i, j, k, day: Word;
  pos: Integer;
  exitedWhileSimulating: Boolean;

  InitialInfectedNodes, Neighbors, Susceptible, Infected, Recovered: TWordList;

  TestingNode, NodeToBeRecovered, NodesInfectedByNodePerDay,
  NumberOfNodesInfectedPerDay, NumOfMaxNeighborsToTest, NumOfMaxNodesPerDay: Word;

begin
  {
            <>======================================================<>
            ||                                                      ||
            ||   Simulation algorithm for the SIR epidemic model.   ||
            ||                                                      ||
            <>======================================================<>

    [ Algorithm ]
    [**************************************************************************]
      The algorithm is constructed with the following steps:
      1) Initialize the Susceptible, Infected and Recovered lists.

      2) Make all nodes Susceptible add them to the Susceptible list.

      3) Infect an initial random node.

      4) For each day of the simulation.

      5) For each node pick a random number of neighbors and infect them with
         probability pos. If the maximum number of nodes that can be infected
         in one day is reached, go to the next day.

      6) At the end of each day make sure that γ*I nodes recover from the
         decease.

    [**************************************************************************]

    [ Variables ]
    [**************************************************************************]
        i, j, k, day: Counters.

        days: Number of days that the simulation lasts.
        firstInfected: Index of the first infected node.
        TestingNode: Index of node to be tested.
        NodeToBeRecovered: Index of node to be recovered.

        ProbabilityOfInfection: The probability of infection specified
                                by the user.
        pos: Probability of Infection.

        Neighbors: List with the contact indexes of each node.
        Susceptible: List that holds the indexes of the susceptible nodes.
        Infected: List that holds the indexes of the infected nodes.
        Recovered: List that holds the indexes of the recovered nodes.

        SamplingResult: List that holds the samplings taken from the Susceptible,
                        Infected, Recovered lists, at the end of each day.

        beta: The average number of contacts per person per time.
              (Infection rate)
        gamma: The recovery rate.

        NodesInfectedByNodePerDay: Number of nodes that were infected by a node
                                   in one day.
        NumberOfNodesInfectedPerDay: Number of nodes that were infected
                                     in one day.

        NumOfMaxNeighborsToTest: Number of maximum neighbors to be tested
                                 for each node.
        NumOfMaxNodesPerDay: Number of maximum nodes that is allowed to
                             be infected per day.

    [**************************************************************************]
  }

  frmSimulation.frmSmlInvoker.Enabled := False;
  exitedWhileSimulating := False;

  { Initialize Variable Lists }
  InitialInfectedNodes := TWordList.Create;
  Neighbors := TWordList.Create;
  Susceptible := TWordList.Create;
  Infected := TWordList.Create;
  Recovered := TWordList.Create;

  { Append all nodes to initial Susceptible state }
  for i := 0 to frmMain.Nodes.Count - 1 do begin
    frmMain.Nodes[i].MakeSusceptible;
    Susceptible.Add(i);
  end;

  InitialInfectedNodes := frmSimulation.InfectRandomNodes(NumberOfInitialInfectedNodes);

  for i := 0 to NumberOfInitialInfectedNodes - 1 do begin
    Susceptible.Remove(InitialInfectedNodes[i]);
    Infected.Add(InitialInfectedNodes[i]);
  end;

  { Take an initial sampling }
  SamplingResult.Add(ArrayOfWord.Create(Susceptible.Count, Infected.Count, Recovered.Count));

  for Day := 0 to Days do begin

    if not (Susceptible.Count = 0) and not (Infected.Count = 0) then begin
      NumberOfNodesInfectedPerDay := 0;

      //writeln('Day: ' + IntToStr(Day)); { Debug }

      for i := 0 to frmMain.Nodes.Count - 1 do begin
        NodesInfectedByNodePerDay := 0;
        Neighbors := frmMain.Nodes[i].Neighbors;

        if Neighbors.Count > 0 then begin
          NumOfMaxNodesPerDay := Random(Round(Neighbors.Count * beta));
          NumOfMaxNeighborsToTest := NumOfMaxNodesPerDay;
          if NumOfMaxNeighborsToTest = 0 then NumOfMaxNeighborsToTest := 1;

          //writeln('Number of neighbors to be tested by node ' + IntToStr(i) + ' at day ' + IntToStr(day) + ' are ' + IntToStr(NumOfMaxNeighborsToTest)); { Debug }

          for j := 0 to NumOfMaxNeighborsToTest do begin
            pos := Random(100);
            if pos <= ProbabilityOfInfection then begin
              { Pick a random Neighbor }
              TestingNode := Neighbors[Random(Neighbors.Count)];

              if not frmMain.Nodes[TestingNode].IsInfected and
                 not frmMain.Nodes[TestingNode].IsRecovered and
                 frmMain.Nodes[i].IsInfected then begin

                frmSimulation.InfectNode(frmMain.Nodes[TestingNode], frmMain.Nodes[i].Id);

                Susceptible.Remove(TestingNode);
                Infected.Add(TestingNode);

                Inc(NodesInfectedByNodePerDay);
                //writeln('Node ' + IntToStr(frmMain.Nodes[TestingNode].Id) + ' has been infected by node ' + IntToStr(frmMain.Nodes[i].Id)); { Debug }
              end;
            end;
          end; { End j }

          //{ In case the simulation was forced closed before finishing set the
          //  exitedWhileSimulating flag to true. }
          //if not frmSimulation.Showing then begin
          //  exitedWhileSimulating := True;
          //  break;
          //end;

          { If there are no infected nodes stop the simulation. }
          if Infected.Count = 0 then break;

          NumberOfNodesInfectedPerDay += NodesInfectedByNodePerDay;
          //writeln('Node: ' + IntToStr(i) + ' infected: ' + IntToStr(NodesInfectedByNodePerDay) + ' nodes, at day: ' + IntToStr(day)); { Debug }

          if NumberOfNodesInfectedPerDay >= NumOfMaxNodesPerDay then break;

        end;

        end; { End i }

      end;

    //writeln('At Day: ' + IntToStr(day) + ', { ' + IntToStr(NumberOfNodesInfectedPerDay) + ' } where infected.'); { Debug }

    { Recover FIFO (First In First Out) Style }
    for k := 0 to Round(Infected.Count * gamma) do begin
      if (Infected.Count > 0) then begin
         NodeToBeRecovered := Infected[0];
         frmMain.Nodes[NodeToBeRecovered].Recover;

         Infected.Remove(NodeToBeRecovered);
         Recovered.Add(NodeToBeRecovered);
      end
      else break;
    end;

    SamplingResult.Add(ArrayOfWord.Create(Susceptible.Count, Infected.Count, Recovered.Count));

    if frmMain.CancelTriggered then break;

  end; { End Day }

  { Repeat the simulation if needed }
  if (Recovered.Count < StrToInt(frmSettings.edtReSimulateMinRecoveredNodeCount.Text)) and
     frmMain.ckbUseSystemSeed.Checked and
     frmSettings.cbxReSimulate.Checked and
     not frmMain.CancelTriggered then begin

       SamplingResult.Clear;
       frmSimulation.RestoreNodes; // Restore the Nodes
       RandomizeSystem; // Randomize System
       SIRALG(days, NumberOfInitialInfectedNodes, beta, gamma, ProbabilityOfInfection, SamplingResult); // Repeat the simulation
  end
  else if not frmMain.CancelTriggered then begin
    { Take missing samplings }
    for i := 0 to days - SamplingResult.Count do
      SamplingResult.Add(ArrayOfWord.Create(Susceptible.Count, Infected.Count, Recovered.Count));

    { Take a final sampling }
    SamplingResult.Add(ArrayOfWord.Create(Susceptible.Count, Infected.Count, Recovered.Count));

    //{ If the exitedWhileSimulating is set to true invoke the FormClose action. }
    //if exitedWhileSimulating then self.FormClose(self);

    { Print the samples } { Debug }
    //for i:=0 to SamplingResult.Count - 1 do begin
    //  write(IntToStr(i) + ': ');
    //  for j := 0 to 3 - 1 do begin
    //    write(SamplingResult[i][j]:2);
    //    write(', ');
    //  end;
    //  writeln();
    //end;

    SamplingResult.DeleteRange(days + 2, SamplingResult.Count - 1);

  end;
end;

end.

