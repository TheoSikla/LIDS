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

