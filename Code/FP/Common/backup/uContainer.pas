unit uContainer;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
interface

uses
  Classes, SysUtils, fgl, uUtilities, contnrs;

type

  { TOwnedPersistent }

  TOwnedPersistent = class(TPersistent)
  private
    FOwner: TPersistent;
    FChildren: TObjectList;
    function GetChildren(): TObjectList;
  protected
    function CheckChild(AChild: TOwnedPersistent): boolean;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function GetOwner: TPersistent; override;
    procedure SetOwner(AOwner: TOwnedPersistent); virtual;
    procedure AddChild(AChild: TOwnedPersistent);
    procedure RemoveChild(AChild: TOwnedPersistent);
  end;

  { TComponentContainer }

  TComponentContainer = class(TComponent)
  private
    FUpdating: boolean;
  protected
    function GetChildOwner: TComponent; override;
    function GetChildParent: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetUpdating(AUpdating: boolean);
  public
    function GetParentComponent: TComponent; override;
    function AuditName(const AName: string; const AReplaced: char = '_';
      AOwner: TComponent = nil): string;
    procedure BeginUpdate();
    procedure EndUpdate();
  end;

  { TComponentCollection }

  TComponentCollection = class(TComponentContainer)
  private
    FItemClass: TComponentClass;
    FItemList: TComponentList;
    FIndexList: TStringList;
    function GetItem(AIndex: integer): TComponent;
  protected
    function GetIndexList(AGetKey: specialize TFunc1<TComponent, string>): TStringList;
    procedure ReadItemClass(AReader: TReader);
    procedure WriteItemClass(AWriter: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    property ItemClass: TComponentClass read FItemClass write FItemClass;
    property Item[AIndex: integer]: TComponent read GetItem;
    function ItemCount: integer;
    constructor Create(AOwner: TComponent; AItemClass: TComponentClass);
    destructor Destroy; override;
    function AddItem(AItemClass: TComponentClass;
      AAutoSetName: boolean): TComponent;
    function AddorGet(const AKey: string; AGetKey: specialize TFunc1<TComponent, string>;
      AAfterCreate: specialize TProc2<TComponent, string>): TComponent; virtual;
    function GetByKey(const AKey: string; AGetKey: specialize TFunc1<TComponent, string>;
      AGetter: specialize TProc1<TComponent>): boolean; overload;
    function GetByKey(const AKey: string;
      AGetKey: specialize TFunc1<TComponent, string>): TComponent; overload;
  published
    function Add(): TComponent;
  end;

  { TGenericCollection }

  generic TGenericCollection<T: TCollectionItem> = class(TOwnedCollection)
  private
    function GetItems(Index: integer): T;
    procedure SetItems(Index: integer; const Value: T);
  public
    constructor Create(AOwner: TPersistent);
    function Add(): T;
    function BinSearch(ACompareLeft: specialize TFunc1<T, integer >;
      out AFoundIndex: integer): boolean;
    property Items[Index: integer]: T read GetItems write SetItems; default;

  end;

  { TWrapper }

  generic TWrapper<T> = class(TComponentContainer)
  private
    FHost: T;
    FCaption: string;
  public
    property Host: T read FHost write FHost;
    property Caption: string read FCaption write FCaption;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AHost: T); overload; virtual;
  end;

  { TObjectWrapper }

  TObjectWrapper = class(specialize TWrapper<TObject>)
  private
    FOwned: boolean;
  public
    constructor Create(AOwner: TComponent; AHost: TObject; AOwned: boolean);
      overload;
    procedure BeforeDestruction; override;
    property Owned: boolean read FOwned write FOwned;

  end;

  { TInterfaceWrapper }

  generic TInterfaceWrapper<T: IInterface> = class(specialize TWrapper<T>)
  public
    procedure BeforeDestruction; override;
  end;

implementation

{ TOwnedPersistent }

function TOwnedPersistent.GetChildren(): TObjectList;
begin
  if not Assigned(FChildren) then
    FChildren := TObjectList.Create(True);
  Result := FChildren;
end;

function TOwnedPersistent.CheckChild(AChild: TOwnedPersistent): boolean;
begin
  Result := Assigned(AChild) and (AChild <> Self);
end;

constructor TOwnedPersistent.Create(AOwner: TPersistent);
begin
  inherited Create();
  if Assigned(AOwner) and (AOwner is TOwnedPersistent) then
    SetOwner(AOwner as TOwnedPersistent);
end;

destructor TOwnedPersistent.Destroy;
var
  I: integer;
begin
  inherited Destroy;
  SetOwner(nil);
  if Assigned(FChildren) then
  begin
    for I := 0 to Pred(FChildren.Count) do
      (FChildren[I] as TOwnedPersistent).FOwner := nil;
    FreeAndNil(FChildren);
  end;
end;

function TOwnedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TOwnedPersistent.SetOwner(AOwner: TOwnedPersistent);
begin
  if (AOwner = FOwner) or (AOwner = Self) then
    Exit;
  if Assigned(FOwner) and (FOwner is TOwnedPersistent) then
    TOwnedPersistent(FOwner).RemoveChild(Self);
  FOwner := AOwner;
  if Assigned(FOwner) and (FOwner is TOwnedPersistent) then
    TOwnedPersistent(FOwner).AddChild(Self);
end;

procedure TOwnedPersistent.AddChild(AChild: TOwnedPersistent);
begin
  if CheckChild(AChild) and (GetChildren.IndexOf(AChild) < 0) then
  begin
    GetChildren.Add(AChild);
    AChild.FOwner := Self;
  end;
end;

procedure TOwnedPersistent.RemoveChild(AChild: TOwnedPersistent);
begin
  if Assigned(FChildren) and (AChild.FOwner = Self) then
    with FChildren do
      if IndexOf(AChild) >= 0 then
      begin
        Extract(AChild);
        AChild.FOwner := nil;
      end;
end;

{ TComponentCollection }

function TComponentCollection.GetItem(AIndex: integer): TComponent;
begin
  if Assigned(FItemList) then
    Result := FItemList[AIndex]
  else
    Result := nil;
end;

function TComponentCollection.GetIndexList(AGetKey: specialize TFunc1<
  TComponent, string>): TStringList;
var
  I: integer;
begin
  if not Assigned(FIndexList) then
  begin
    FIndexList := TStringList.Create;
    with FIndexList do
    begin
      OwnsObjects := False;
      CaseSensitive := False;
      Duplicates := dupIgnore;
    end;
    if Assigned(AGetKey) then
      for I := 0 to Pred(ItemCount) do
        FIndexList.AddObject(AGetKey(Item[I]), Item[I]);
  end;
  Result := FIndexList;
end;

procedure TComponentCollection.ReadItemClass(AReader: TReader);
var
  LClassName: string;
  LClass: TPersistentClass;
begin
  LClassName := AReader.ReadString;
  LClass := GetClass(LClassName);
  if Assigned(LClass) and LClass.InheritsFrom(TComponent) then
    FItemClass := TComponentClass(LClass);
end;

procedure TComponentCollection.WriteItemClass(AWriter: TWriter);
begin
  if Assigned(FItemClass) then
    AWriter.WriteString(FItemClass.ClassName);
end;

procedure TComponentCollection.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  filer.DefineProperty('ItemClass', @ReadItemClass, @WriteItemClass,
    Assigned(FItemClass));
end;

procedure TComponentCollection.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: integer;
begin
  inherited Notification(AComponent, Operation);
  if (AComponent.Owner = Self) and Assigned(FItemClass) and
    AComponent.InheritsFrom(FItemClass) then
    case operation of
      opInsert:
      begin
        if not Assigned(FItemList) then
          FItemList := TComponentList.Create(False);
        if FItemList.IndexOf(AComponent) < 0 then
          FItemList.Add(AComponent);
      end;
      opRemove:
        if Assigned(FIndexList) then
        begin
          I := FIndexList.IndexOfObject(AComponent);
          if I >= 0 then
            FIndexList.Delete(I);
        end;
    end;
end;

function TComponentCollection.ItemCount: integer;
begin
  if Assigned(FItemList) then
    Result := FItemList.Count
  else
    Result := 0;
end;

constructor TComponentCollection.Create(AOwner: TComponent;
  AItemClass: TComponentClass);
begin
  inherited Create(AOwner);
  FItemClass := AItemClass;
end;

destructor TComponentCollection.Destroy;
begin
  inherited Destroy;
  if Assigned(FItemList) then
    FreeAndNil(FItemList);
  if Assigned(FIndexList) then
    FreeAndNil(FIndexList);
end;

function TComponentCollection.AddItem(AItemClass: TComponentClass;
  AAutoSetName: boolean): TComponent;
begin
  if not Assigned(AItemClass) then
    AItemClass := FItemClass;
  if not Assigned(AItemClass) then
    AItemClass := TComponent;

  Result := AItemClass.Create(Self);
  if AAutoSetName then
    Result.Name := AuditName(StringStart(AItemClass.ClassName, 2));
end;

function TComponentCollection.AddorGet(const AKey: string;
  AGetKey: specialize  TFunc1<TComponent, string>;
  AAfterCreate: specialize TProc2<TComponent, string  >): TComponent;
begin
  Result := GetByKey(AKey, AGetKey);
  if not Assigned(Result) then
    with GetIndexList(AGetKey) do
    begin
      Result := Self.Add();
      if Assigned(AAfterCreate) then
        AAfterCreate(Result, AKey);
      AddObject(AKey, Result);
    end;
end;

function TComponentCollection.GetByKey(const AKey: string;
  AGetKey: specialize  TFunc1<TComponent, string>;
  AGetter: specialize TProc1<TComponent>): boolean;
var
  LItem: TComponent;
begin
  LItem := GetByKey(AKey, AGetKey);
  Result := Assigned(LItem);
  if Result and Assigned(AGetter) then
    AGetter(LItem);
end;

function TComponentCollection.GetByKey(const AKey: string;
  AGetKey: specialize  TFunc1<TComponent, string>): TComponent;
var
  I: integer;
begin
  with GetIndexList(AGetKey) do
  begin
    I := IndexOf(AKey);
    if I >= 0 then
      Result := Objects[I] as TComponent
    else
      Result := nil;
  end;
end;

function TComponentCollection.Add(): TComponent;
begin
  Result := AddItem(nil, True);
end;

{ TInterfaceWrapper }

procedure TInterfaceWrapper.BeforeDestruction;
begin
  //FHost := nil;
  inherited BeforeDestruction;
end;

{ TObjectWrapper }

constructor TObjectWrapper.Create(AOwner: TComponent; AHost: TObject; AOwned: boolean);
begin
  inherited Create(AOwner, AHost);
  FOwned := AOwned;
end;

procedure TObjectWrapper.BeforeDestruction;
begin
  if FOwned and Assigned(FHost) then
    FreeAndNil(FHost);
  inherited BeforeDestruction;
end;

{ TGenericCollection }

function TGenericCollection.GetItems(Index: integer): T;
begin
  Result := inherited Items[Index] as T;
end;

procedure TGenericCollection.SetItems(Index: integer; const Value: T);
begin
  inherited Items[Index] := Value;
end;

constructor TGenericCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, T);
end;

function TGenericCollection.Add(): T;
begin
  Result := inherited Add as T;
end;

function TGenericCollection.BinSearch(ACompareLeft: specialize TFunc1<T, integer  >;
  out AFoundIndex: integer): boolean;
var
  L, H, mid, cmp: integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := ACompareLeft(Items[mid]);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  AFoundIndex := L;
end;

{ TWrapper }

constructor TWrapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TWrapper.Create(AOwner: TComponent; AHost: T);
begin
  Create(AOwner);
  FHost := AHost;
end;

{ TComponentContainer }

function TComponentContainer.AuditName(const AName: string;
  const AReplaced: char; AOwner: TComponent): string;
const
  FMT: string = '%s%d';
var
  I: integer;
begin
  Result := AName;
  if Result <> '' then
  begin
    if Result[1] in ['0' .. '9'] then
      Result := '_' + Result;
    for I := 1 to Length(Result) do
      if (Ord(Result[I]) < 128) and
        (not (Result[I] in ['0' .. '9', '_', 'A' .. 'Z', 'a' .. 'z'])) then
        Result[I] := AReplaced;
  end;

  if not Assigned(AOwner) then
    AOwner := Self;

  if AOwner.FindComponent(Result) <> nil then
  begin
    I := 1;
    while AOwner.FindComponent(Format(FMT, [Result, I])) <> nil do
      Inc(I);
    Result := Format(FMT, [Result, I]);
  end;
end;

procedure TComponentContainer.BeginUpdate();
begin
  SetUpdating(True);
end;

procedure TComponentContainer.EndUpdate();
begin
  SetUpdating(False);
end;

function TComponentContainer.GetChildOwner: TComponent;
begin
  Result := Self;
end;

function TComponentContainer.GetChildParent: TComponent;
begin
  Result := Self;
end;

procedure TComponentContainer.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
    Proc(Self.Components[I]);
end;

procedure TComponentContainer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not FUpdating then
    inherited Notification(AComponent, Operation);
  //else if (Operation = opInsert) and (AComponent.Owner = Self) and
  //  (AComponent is TComponentContainer) then
  //  TComponentContainer(AComponent).SetUpdating(FUpdating);
end;

procedure TComponentContainer.SetUpdating(AUpdating: boolean);
var
  I: integer;
begin
  FUpdating := AUpdating;
  for I := 0 to Pred(ComponentCount) do
    if Components[I] is TComponentContainer then
      TComponentContainer(Components[I]).SetUpdating(AUpdating);
end;

function TComponentContainer.GetParentComponent: TComponent;
begin
  Result := Owner;
end;

end.
