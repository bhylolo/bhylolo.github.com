unit uETL;

interface

uses
  classes, sysutils, uComponentContainers, contnrs;

type
  TETLObject = class(TComponentContainer)

  end;

  TETLFieldType = (ftUnknown, ftInteger, ftNumber, ftString, ftDateTime);

  TETLField = class(TETLObject)
  private
    FFieldName: string;
    FFieldType: TETLFieldType;
    FFormat: string;
  published
    property FieldName: string read FFieldName write FFieldName;
    property FieldType: TETLFieldType read FFieldType write FFieldType;
    property Format: string read FFormat write FFormat;
  end;

  TETLTable = class(TETLObject)
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure ForeachFields(AGetter: TProc<TETLField>); overload;
    function ForeachFields(AGetter: TPredicate<TETLField>): Boolean; overload;
    function AddField(AName: string): TETLField;
    procedure ClearFields();
  end;

  TetlCSVTable = class(TETLTable)
  private
    FFileName: TFileName;
  published
    property FileName: TFileName read FFileName write FFileName;

  end;

implementation

{ TETLTable }

function TETLTable.AddField(AName: string): TETLField;
begin
  Result := TETLField.Create(Self);
  Result.Name := Self.AuditName('f');
end;

procedure TETLTable.AfterConstruction;
begin
  inherited;

end;

procedure TETLTable.BeforeDestruction;
begin
  inherited;

end;

procedure TETLTable.ClearFields;
var
  I: Integer;
begin
  for I := Self.ComponentCount - 1 downto 0 do
    if Components[I] is TETLField then
      Components[I].Free;
end;

function TETLTable.ForeachFields(AGetter: TPredicate<TETLField>): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.ComponentCount - 1 do
    if (Components[I] is TETLField) and AGetter(TETLField(Components[I])) then
      Exit(true);
end;

procedure TETLTable.ForeachFields(AGetter: TProc<TETLField>);
begin
  ForeachFields(
    function(F: TETLField): Boolean
    begin
      Result := False;
      AGetter(F);
    end);
end;

end.
