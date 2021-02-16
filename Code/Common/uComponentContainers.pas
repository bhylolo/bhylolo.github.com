unit uComponentContainers;

interface

uses
  Classes, SysUtils, System.Generics.Collections, System.TypInfo, System.RTTI;

type
  TComponentContainer = class(TComponent)
  protected
    FUpdating: Boolean;
    function GetChildOwner: TComponent; override;
    function GetChildParent: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    procedure SetUpdating(AUpdating: Boolean);
    function GetParentComponent: TComponent; override;
    function AuditName(const AName: string; const AReplaced: Char = '_';
      AOwner: TComponent = nil): string;
    procedure AfterConstruction; override;
  end;

  TGenericCollection<T: TCollectionItem> = class(TOwnedCollection)
  private
    FPropName: string;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    constructor Create(AOwner: TPersistent; ItemClass: T); overload;
    constructor Create(AOwner: TPersistent); overload;
    function GetNamePath: string; override;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    procedure Iterator(AGetter: TProc<T>);
    function Insert(Index: Integer): T;
    function Search(ACompare: TFunc<T, Integer>; var AIndex: Integer): Boolean;
  published
    function Add(): T;
  end;

  TWrapper<T> = class(TComponent)
  private
    FHost: T;
    FCaption: string;
  public
    property Host: T read FHost write FHost;
    property Caption: string read FCaption write FCaption;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AHost: T); overload;
    function SetName(const AName: string): TWrapper<T>;
    function SetCaption(const ACaption: string): TWrapper<T>;
    function InitHost(AAction: TProc<T>): TWrapper<T>;
  end;

  TObjectWrapper = class(TWrapper<TObject>)
  private
    FOwned: Boolean;
  public
    constructor Create(AOwner: TComponent; AHost: TObject;
      AOwned: Boolean); overload;
    procedure BeforeDestruction; override;
    property Owned: Boolean read FOwned write FOwned;
  end;

  TInterfaceWrapper<T: IInterface> = class(TWrapper<T>)
  public
    procedure BeforeDestruction; override;
  end;

  TDelegationItem = class(TCollectionItem)
  public const
    ProcName: string = 'Proc';
  public type
    TCallState = (csStateless, csSuccess, csFail);
  private
    FCallState: TCallState;
  protected
    procedure FreeNotification(AObject: TObject);
    procedure RemoveFreeNotification(AObject: TObject);
    function GetDisplayName: string; override;
  public
    procedure Call(); virtual;
    function ProcData(): TObject;
  published
    property CallState: TDelegationItem.TCallState read FCallState
      write FCallState;
    procedure Delete();
  end;

  TDelegation<T: TDelegationItem> = class(TComponentContainer)
  private
    FDelegations: TGenericCollection<T>;
    procedure SetDelegations(const Value: TGenericCollection<T>);
    procedure ReadDelegations(R: TReader);
    procedure WriteDelegations(W: TWriter);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Delegations: TGenericCollection<T> read FDelegations
      write SetDelegations;

    procedure Call(); virtual;
  end;

  TObjectProc = procedure of object;

  TProcDelegationItem = class(TDelegationItem)
  private
    FProc: TObjectProc;
    procedure SetProc(const Value: TObjectProc);
  public
    procedure Call; override;
  published
    property Proc: TObjectProc read FProc write SetProc;
  end;

  TProcDelegation = class(TDelegation<TProcDelegationItem>)

  end;

  TOwnedPersistent = class(TPersistent)
  private
    FChildrenList: TObjectList<TPersistent>;
    FOwned: Boolean;
    FOwner: TPersistent;
    function GetChildrenList(): TObjectList<TPersistent>;
    function GetChildren(AIndex: Integer): TPersistent;
    procedure SetOwned(const Value: Boolean);
    function GetParent: TPersistent;
    procedure SetParent(const Value: TPersistent);
  protected
    function GetOwner: TPersistent; override;
  public
    function Count(): Integer;
    destructor Destroy; override;
    property Parent: TPersistent read GetParent write SetParent;
    property Children[AIndex: Integer]: TPersistent read GetChildren;
    property Owned: Boolean read FOwned write SetOwned;
    procedure AddChild(AChild: TPersistent);
    procedure RemoveChild(AChild: TPersistent);

  end;

implementation

{ TComponentContainer }

procedure TComponentContainer.AfterConstruction;
begin
  inherited;
  FUpdating := False;
end;

function TComponentContainer.AuditName(const AName: string;
  const AReplaced: Char; AOwner: TComponent): string;
const
  FMT: string = '%s%d';
var
  I: Integer;
begin
  Result := AName;
  if Result <> '' then
  begin
    if Result[1] in ['0' .. '9'] then
      Result := '_' + Result;
    for I := 1 to Length(Result) do
      if (ord(Result[I]) < 128) and
        (not(Result[I] in ['0' .. '9', '_', 'A' .. 'Z', 'a' .. 'z'])) then
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

function TComponentContainer.GetChildOwner: TComponent;
begin
  Result := Self;
end;

function TComponentContainer.GetChildParent: TComponent;
begin
  Result := Self;
end;

procedure TComponentContainer.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
    Proc(Self.Components[I]);
end;

function TComponentContainer.GetParentComponent: TComponent;
begin
  Result := Owner;
end;

procedure TComponentContainer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not FUpdating then
    inherited;
end;

procedure TComponentContainer.SetUpdating(AUpdating: Boolean);
var
  I: Integer;
begin
  if FUpdating = AUpdating then
    Exit;
  FUpdating := AUpdating;
  for I := 0 to Self.ComponentCount - 1 do
    if Components[I] is TComponentContainer then
      TComponentContainer(Components[I]).SetUpdating(AUpdating);
end;

procedure TComponentContainer.ValidateInsert(AComponent: TComponent);
begin
  inherited;
  if AComponent is TComponentContainer then
    TComponentContainer(AComponent).FUpdating := Self.FUpdating;
end;

{ TGenericCollection<T> }

function TGenericCollection<T>.Add: T;
begin
  Result := inherited Add as T;
end;

constructor TGenericCollection<T>.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, T);
end;

constructor TGenericCollection<T>.Create(AOwner: TPersistent; ItemClass: T);
begin
  inherited Create(AOwner, ItemClass);
end;

function TGenericCollection<T>.GetItem(Index: Integer): T;
begin
  Result := inherited GetItem(Index) as T;
end;

function TGenericCollection<T>.GetNamePath: string;
var
  LContext: TRttiContext;
  LProp: TRttiProperty;
begin
  if (FPropName = '') and (Owner <> nil) then
  begin
    LContext := TRttiContext.Create;
    try
      for LProp in LContext.GetType(Owner.ClassType).GetProperties do
        if (LProp is TRttiInstanceProperty) and (LProp.Visibility = mvPublished)
          and (LProp.PropertyType.TypeKind = tkClass) then
        begin
          if GetObjectProp(Owner, TRttiInstanceProperty(LProp).PropInfo) = Self
          then
          begin
            FPropName := LProp.Name;
            Break;
          end;
        end;
    finally
      LContext.Free;
    end;
    PropName := FPropName;
  end;
  Result := inherited GetNamePath;
end;

function TGenericCollection<T>.Insert(Index: Integer): T;
begin
  Result := Self.Add;
  Result.Index := Index;
end;

procedure TGenericCollection<T>.Iterator(AGetter: TProc<T>);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    AGetter(Items[I]);
end;

function TGenericCollection<T>.Search(ACompare: TFunc<T, Integer>;
  var AIndex: Integer): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
begin
  if Count = 0 then
  begin
    AIndex := 0;
    Exit(False);
  end;

  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := ACompare(Items[mid]);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  AIndex := L;
end;

procedure TGenericCollection<T>.SetItem(Index: Integer; const Value: T);
begin
  inherited SetItem(Index, Value);
end;

{ TObjectWrapper }

procedure TObjectWrapper.BeforeDestruction;
begin
  inherited;
  if FOwned and Assigned(FHost) then
    FreeAndNil(FHost);
end;

constructor TObjectWrapper.Create(AOwner: TComponent; AHost: TObject;
  AOwned: Boolean);
begin
  inherited Create(AOwner, AHost);
  FOwned := AOwned;
end;

{ TWrapper<T> }

constructor TWrapper<T>.Create(AOwner: TComponent);
begin
  inherited;

end;

constructor TWrapper<T>.Create(AOwner: TComponent; AHost: T);
begin
  Create(AOwner);
  FHost := AHost;
end;

function TWrapper<T>.InitHost(AAction: TProc<T>): TWrapper<T>;
begin
  Result := Self;
  if Assigned(AAction) then
    AAction(Host);
end;

function TWrapper<T>.SetCaption(const ACaption: string): TWrapper<T>;
begin
  FCaption := ACaption;
  Result := Self;
end;

function TWrapper<T>.SetName(const AName: string): TWrapper<T>;
begin
  Name := AName;
  Result := Self;
end;

{ TInterfaceWrapper<T> }

procedure TInterfaceWrapper<T>.BeforeDestruction;
begin
  inherited;
  Host := nil;
end;

{ TDelegation<T> }

procedure TDelegation<T>.AfterConstruction;
begin
  inherited;
  FDelegations := TGenericCollection<T>.Create(Self);
end;

procedure TDelegation<T>.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FDelegations);
end;

procedure TDelegation<T>.Call;
begin
  FDelegations.Iterator(
    procedure(D: T)
    begin
      D.Call();
    end);
end;

procedure TDelegation<T>.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Delegations', ReadDelegations, WriteDelegations, True);
end;

procedure TDelegation<T>.Notification(AComponent: TComponent;
Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  case Operation of
    opInsert:
      ;
    opRemove:
      with Delegations do
        for I := Count - 1 downto 0 do
          if Items[I].ProcData = AComponent then
            Delete(I);
  end;
end;

procedure TDelegation<T>.ReadDelegations(R: TReader);
begin
  if R.ReadValue = vaCollection then
    R.ReadCollection(FDelegations);
end;

procedure TDelegation<T>.SetDelegations(const Value: TGenericCollection<T>);
begin
  if FDelegations = Value then
    Exit;
  with FDelegations do
    try
      BeginUpdate;
      Clear;
      if Assigned(Value) then
        Assign(Value);
    finally
      EndUpdate;
    end;
end;

procedure TDelegation<T>.WriteDelegations(W: TWriter);
begin
  W.WriteCollection(FDelegations);
end;

{ TProcDelegationItem }

procedure TProcDelegationItem.Call;
begin
  inherited;
  if Assigned(FProc) then
  begin
    FProc();
    FCallState := csSuccess;
  end;
end;

procedure TProcDelegationItem.SetProc(const Value: TObjectProc);
begin
  if Assigned(FProc) then
    RemoveFreeNotification(TObject(TMethod(FProc).Data));

  FProc := Value;

  if Assigned(FProc) then
    FreeNotification(TObject(TMethod(FProc).Data));
end;

{ TDelegationItem }

procedure TDelegationItem.Call;
begin
  FCallState := csStateless;
end;

procedure TDelegationItem.Delete;
begin
  Free;
end;

procedure TDelegationItem.FreeNotification(AObject: TObject);
begin
  if Assigned(AObject) and (AObject is TComponent) and (Collection.Owner <> nil)
    and (Collection.Owner is TComponent) then
    TComponent(AObject).FreeNotification(TComponent(Collection.Owner));
end;

function TDelegationItem.GetDisplayName: string;
var
  LProc: TMethod;
  LPropInfo: PPropInfo;
begin
  Result := '';
  LProc := GetMethodProp(Self, ProcName);
  with LProc do
    if Assigned(Data) and Assigned(Code) and (TObject(Data) is TPersistent) then
      Result := TPersistent(Data).GetNamePath + '.' + TObject(Data)
        .MethodName(Code)
    else
    begin
      LPropInfo := GetPropInfo(Self, ProcName);
      if Assigned(LPropInfo) then
        Result := LPropInfo^.PropType^^.Name;
    end;
end;

function TDelegationItem.ProcData: TObject;
var
  LProc: TMethod;
begin
  LProc := GetMethodProp(Self, ProcName);
  with LProc do
    if Assigned(Data) and Assigned(Code) then
      Result := TObject(Data)
    else
      Result := nil;
end;

procedure TDelegationItem.RemoveFreeNotification(AObject: TObject);
begin
  if Assigned(AObject) and (AObject is TComponent) and (Collection.Owner <> nil)
    and (Collection.Owner is TComponent) then
    TComponent(AObject).RemoveFreeNotification(TComponent(Collection.Owner));
end;

{ TOwnedPersistent }

procedure TOwnedPersistent.AddChild(AChild: TPersistent);
begin
  if Assigned(AChild) and (AChild <> Self) then
    with GetChildrenList do
      if IndexOf(AChild) < 0 then
      begin

        if (AChild is TOwnedPersistent) and
          Assigned(TOwnedPersistent(AChild).FOwner) and
          (TOwnedPersistent(AChild).FOwner is TOwnedPersistent) then
          TOwnedPersistent(TOwnedPersistent(AChild).FOwner).RemoveChild(AChild);

        Add(AChild);

        if AChild is TOwnedPersistent then
          TOwnedPersistent(AChild).FOwner := Self;
      end;
end;

function TOwnedPersistent.Count: Integer;
begin
  if Assigned(FChildrenList) then
    Result := FChildrenList.Count
  else
    Result := 0;
end;

destructor TOwnedPersistent.Destroy;
begin

  inherited;
  if Assigned(FChildrenList) then
    FreeAndNil(FChildrenList);
end;

function TOwnedPersistent.GetChildren(AIndex: Integer): TPersistent;
begin
  if Assigned(FChildrenList) then
    Result := FChildrenList[AIndex]
  else
    Result := nil;
end;

function TOwnedPersistent.GetChildrenList: TObjectList<TPersistent>;
begin
  if not Assigned(FChildrenList) then
    FChildrenList := TObjectList<TPersistent>.Create(FOwned);
  Result := FChildrenList;
end;

function TOwnedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TOwnedPersistent.GetParent: TPersistent;
begin
  Result := GetOwner;
end;

procedure TOwnedPersistent.RemoveChild(AChild: TPersistent);
begin
  if Assigned(AChild) and Assigned(FChildrenList) then
  begin
    FChildrenList.Extract(AChild);
    if AChild is TOwnedPersistent then
      TOwnedPersistent(AChild).FOwner := nil;
  end;
end;

procedure TOwnedPersistent.SetOwned(const Value: Boolean);
begin
  FOwned := Value;
  if Assigned(FChildrenList) then
    FChildrenList.OwnsObjects := FOwned;
end;

procedure TOwnedPersistent.SetParent(const Value: TPersistent);
begin
  if (Value = FOwner) or (Value = Self) then
    Exit;
  if Assigned(FOwner) and (FOwner is TOwnedPersistent) then
    TOwnedPersistent(FOwner).RemoveChild(Self);
  FOwner := Value;
  if Assigned(FOwner) and (FOwner is TOwnedPersistent) then
    TOwnedPersistent(FOwner).AddChild(Self);
end;

initialization

RegisterClasses([TObjectWrapper, TProcDelegationItem, TProcDelegation]);

finalization

UnRegisterClasses([TObjectWrapper, TProcDelegationItem, TProcDelegation]);

end.
