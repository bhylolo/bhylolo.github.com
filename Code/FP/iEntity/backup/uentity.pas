unit uEntity;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TValue }

  TValue = class(TObject)
  protected
    function ReadValue(R: TReader): TValue; virtual;
    function WriteValue(W: TWriter): TValue; virtual;
  end;

  { TValueList }

  TValueList = class(TValue)
  private
    FList: TObjectList;
    function GetItem(const AIndex: integer): TValue;
  protected
    function ReadValue(R: TReader): TValue; override;
    function WriteValue(W: TWriter): TValue; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Count(): integer;
    property Item[const AIndex: integer]: TValue read GetItem; default;
  end;

implementation

{ TValueList }

function TValueList.GetItem(const AIndex: integer): TValue;
begin
  Result := FList[AIndex] as TValue;
end;

function TValueList.ReadValue(R: TReader): TValue;
begin
  Result := inherited ReadValue(R);
end;

function TValueList.WriteValue(W: TWriter): TValue;
var
  I: integer;
begin
  Result := inherited WriteValue(W);
  W.WriteListBegin;
  w.WriteInteger(FList.Count);
  for I := 0 to Pred(Count) do
    Item[I].WriteValue(W);
  w.WriteListEnd;
end;

procedure TValueList.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TObjectList.Create(True);
end;

procedure TValueList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FreeAndNil(FList);
end;

function TValueList.Count(): integer;
begin
  Result := FList.Count;
end;

{ TValue }

function TValue.ReadValue(R: TReader): TValue;
begin
  Result := Self;
end;

function TValue.WriteValue(W: TWriter): TValue;
begin
  Result := Self;
end;

end.
