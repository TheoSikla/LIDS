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

unit utlTypes_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, clNode_u;

{ Globally available types }
type
  ArrayOfDouble = Array of Double;
  ArrayOfWord = Array of Word;
  ArrayOfArrayOfDouble = Array of ArrayOfDouble;
  TListOfTNode = specialize TFPGList<TNode>;
  TWordList = specialize TFPGList<Word>;
  TArrayOfArrayOfWord = specialize TFPGList<ArrayOfWord>;

implementation

end.

