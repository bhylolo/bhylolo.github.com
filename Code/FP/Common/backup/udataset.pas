unit uDataset;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
interface

uses
  Classes, SysUtils, Contnrs;

type
  TRow = class;
  TGetRow = procedure(ARow: TRow) is nested;
  { TRow }

  TRow = class(TObject)
  private
    FRowList: TObjectList;
    FData: TMemoryStream;
    function GetItem(AIndex: integer): TRow;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Count(): integer;
    property Item[AIndex: integer]: TRow read GetItem; default;
    procedure Foreach(AGetter: TGetRow; ARecursive: boolean);
  end;

implementation

{ TRow }

function TRow.GetItem(AIndex: integer): TRow;
begin
  if Assigned(FRowList) then
    Result := FRowList[AIndex] as TRow
  else
    Result := nil;
end;

procedure TRow.AfterConstruction;
begin
  inherited AfterConstruction;
  FData := TMemoryStream.Create;
end;

procedure TRow.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(FRowList) then
    FreeAndNil(FRowList);
  FreeAndNil(FData);
end;

function TRow.Count(): integer;
begin
  if Assigned(FRowList) then
    Result := FRowList.Count
  else
    Result := -1;
end;

procedure TRow.Foreach(AGetter: TGetRow; ARecursive: boolean);
var
  I: integer;
begin
  for I := 0 to Pred(Count) do
  begin
    AGetter(Item[I]);
    if ARecursive then
      Item[I].Foreach(AGetter, ARecursive);
  end;
end;

end.
