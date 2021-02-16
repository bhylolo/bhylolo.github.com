unit uMind;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TMindObject = class;
  TGetMindObject = procedure(M: TMindObject) is nested;
  { TMindObject }

  TMindObject = class(TComponent)
  protected
    function GetChildOwner: TComponent; override;
    function GetChildParent: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    function GetParentComponent: TComponent; override;

    procedure Foreach(AGetter: TGetMindObject);
  end;

  TMind = class(TMindObject)
  private
    FDetail: TStrings;
    FText: string;
    procedure SetDetail(AValue: TStrings);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Text: string read FText write FText;
    property Detail: TStrings read FDetail write SetDetail;
  end;

  TMindProject = class(TMindObject)

  end;

implementation

{ TMind }

procedure TMind.SetDetail(AValue: TStrings);
begin
  if FDetail = AValue then
    Exit;
  with FDetail do
    try
      BeginUpdate;
      if Assigned(AValue) then
        Assign(AValue)
      else
        Clear;
    finally
      EndUpdate;
    end;
end;

procedure TMind.AfterConstruction;
begin
  inherited AfterConstruction;
  FDetail := TStringList.Create;
end;

procedure TMind.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FreeAndNil(FDetail);
end;

{ TMindObject }

function TMindObject.GetChildOwner: TComponent;
begin
  Result := Self;
end;

function TMindObject.GetChildParent: TComponent;
begin
  Result := Self;
end;

procedure TMindObject.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: integer;
begin
  for I := 0 to Pred(ComponentCount) do
    Proc(Components[I]);
end;

function TMindObject.GetParentComponent: TComponent;
begin
  Result := Owner;
end;

procedure TMindObject.Foreach(AGetter: TGetMindObject);
var
  I: integer;
begin
  for I := 0 to Pred(ComponentCount) do
    if Components[I] is TMindObject then
      AGetter(Components[I] as TMindObject);
end;

end.
