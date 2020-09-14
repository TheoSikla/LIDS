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

unit utlArray_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TAppender<T> = class(TObject)
      type
        ArrayOfT = Array of T;
      public
        constructor Create;
        destructor Destroy; override;

        procedure Append(var Arr: ArrayOfT; AValue: T);
        procedure Delete(var Arr: ArrayOfT; const Index: Cardinal);
        procedure DeleteByValue(var Arr: ArrayOfT; const AValue: T);
  end;

implementation
  constructor TAppender.Create;
  begin
    inherited;
  end;

  destructor TAppender.Destroy;
  begin
    inherited;
  end;

  procedure TAppender.Append(var Arr: ArrayOfT; AValue: T);
    begin
      SetLength(Arr, Length(Arr)+1);
      Arr[High(Arr)] := AValue;
    end;

  procedure TAppender.Delete(var Arr: ArrayOfT; const Index: Cardinal);
  var
    ALength: Cardinal;
    TailElements: Cardinal;

  begin
    ALength := Length(Arr);
    Assert(ALength > 0);
    Assert(Index < ALength);
    Finalize(Arr[Index]);
    TailElements := ALength - Index;
    if TailElements > 0 then
      Move(Arr[Index + 1], Arr[Index], SizeOf(T) * TailElements);
    Initialize(Arr[ALength - 1]);
    SetLength(Arr, ALength - 1);
  end;

  procedure TAppender.DeleteByValue(var Arr: ArrayOfT; const AValue: T);
  var
    i: Integer;
    ALength: Cardinal;
    TailElements: Cardinal;
    temp: T;
    Index: Word;

  begin
    for i := 0 to Length(Arr) do begin
      if Arr[i] = AValue then begin
        Index := i;

        ALength := Length(Arr);
        Assert(ALength > 0);
        Assert(Index < ALength);
        Finalize(Arr[Index]);
        TailElements := ALength - Index;
        if TailElements > 0 then
          Move(Arr[Index + 1], Arr[Index], SizeOf(T) * TailElements);
        Initialize(Arr[ALength - 1]);
        SetLength(Arr, ALength - 1);
      end;
      break;
    end;
  end;

end.

