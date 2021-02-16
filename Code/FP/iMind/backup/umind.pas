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

  TMindProject = class(TMindObject)

  end;

implementation

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
