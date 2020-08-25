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
end.

