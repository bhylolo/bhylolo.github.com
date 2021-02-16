unit fmObjectInspector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, strutils,
  Vcl.ComCtrls, System.TypInfo, System.RTTI, VirtualTrees,
  System.Generics.Collections, Vcl.StdCtrls, fmObjectTree, DesignIntf,
  uUtilities;

type
  TFrameObjectInspector = class(TFrame)
    TabControl1: TTabControl;
    FrameObjectTree1: TFrameObjectTree;
    procedure TabControl1Change(Sender: TObject);
    procedure FrameObjectTree1VTreeEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure FrameObjectTree1VTreeCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  private
    { Private declarations }
    FTypeKinds: TTypeKinds;
    procedure SetTypeKinds(const Value: TTypeKinds);
    function GetHost: TPersistent;

  public
    property Host: TPersistent read GetHost;
    property TypeKinds: TTypeKinds read FTypeKinds write SetTypeKinds;
    procedure SetHost(ADesigner: IDesigner; const Value: TPersistent);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TRTTIMemberLink<T: TRttiMember> = class(TTypeLink<T>)
  private
    FHost: TObject;
    FDesigner: IDesigner;
    FMemberName: string;
  protected
    function GetMemberValue: string; virtual;
    procedure SetMemberValue(const Value: string); virtual;
  public
    property Host: TObject read FHost write FHost;
    property Designer: IDesigner read FDesigner;
    property MemberName: string read FMemberName write FMemberName;
    property MemberValue: string read GetMemberValue write SetMemberValue;

    function GetNodeText(AColumn: Integer; AColumnLink: TColumnLink)
      : string; override;

    constructor Create(ADesigner: IDesigner; AParentLink: TNodeLink;
      AHost: TObject; ARTTIInfo: T; const APropName: string); virtual;
  end;

  TProcedureLink = class(TRTTIMemberLink<TRttiMethod>)
  protected
    FResult: string;
    function CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
      : TWinControl; override;
    function SetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    function GetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    procedure SetBounds(R: TRect); override; stdcall;

    function GetMemberValue: string; override;
    procedure OnButtonClick(ASender: TObject); virtual;
  end;

  TPropLinkClass = class of TPropLink;

  TPropLink = class(TRTTIMemberLink<TRttiInstanceProperty>)
  private
    function GetPropInfo: PPropInfo;
  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;

    function CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
      : TWinControl; override;
    function SetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    function GetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
  public
    constructor Create(ADesigner: IDesigner; AParentLink: TNodeLink;
      AHost: TObject; ARTTIInfo: TRttiInstanceProperty;
      const APropName: string); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property PropInfo: PPropInfo read GetPropInfo;
  end;

  TIntPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;

  end;

  TCharPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;

  end;

  TEnumPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
    function CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
      : TWinControl; override;
    function GetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    function SetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
  public

  end;

  TFloatPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
  end;

  TStrPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
  end;

  TBoolPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
    function CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
      : TWinControl; override;
    function GetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    function SetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
  end;

  TSetPropLink = class(TPropLink)
  public type
    TSetItemLink = class(TBoolPropLink)
    protected
      FValue: Integer;
      function GetMemberValue: string; override;
      procedure SetMemberValue(const Value: string); override;
    end;
  protected
    function GetSetValue(): TIntegerSet;
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
  public
    procedure CreateChildrenNodes(ANodeCreator: TFunNodeLink); override;
  end;

  TObjectPropLink = class(TPropLink)
  private
    FContext: TRttiContext;
    function GetParentHost: TObject;
  protected
    function GetMemberValue: string; override;
  public
    property ParentHost: TObject read GetParentHost;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure IterateChildrenObject(AGetter: TProc<TRttiInstanceProperty>;
      ARecursive: Boolean); override;
    procedure CreateChildrenNodes(ANodeCreator: TFunNodeLink); override;

  end;

  TPersistentPropLink = class(TObjectPropLink)

  end;

  TComponentPropLink = class(TObjectPropLink)
  protected
    function CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
      : TWinControl; override;
    function GetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    function SetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;

    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
    procedure LoadValueList(AValueList: TStrings); virtual;
  public
    procedure CreateChildrenNodes(ANodeCreator: TFunNodeLink); override;

  end;

  TMethodPropLink = class(TPropLink)
  protected
    function CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
      : TWinControl; override;
    function GetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    function SetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;

    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
    procedure LoadValueList(AValueList: TStrings); virtual;
  end;

  TWCharPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
  end;

  TWStrPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
  end;

  TVariantPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
  end;

  TIntfPropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
  end;

  TInt64PropLink = class(TPropLink)

  protected
    function GetMemberValue: string; override;
    procedure SetMemberValue(const Value: string); override;
  end;

  THostLink = class(TRTTIMemberLink<TRttiMember>)
  private
    FContext: TRttiContext;
  public
    property Host;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CreateColumns(AColumnLinkList: TColumnLink); override;
    procedure CreateChildrenNodes(ANodeCreator: TFunNodeLink); override;
    procedure IterateChildrenObject(AGetter: TProc<TRttiMember>;
      ARecursive: Boolean); override;
  end;

  TClassTypeLink = class(TRTTIMemberLink<TRttiMember>)
  private
    FContext: TRttiContext;
    FHostClass: TPersistentClass;
    procedure SetHostClass(const Value: TPersistentClass);
  protected
    function GetMemberValue: string; override;

  public
    constructor Create(ADesigner: IDesigner; AParentLink: TNodeLink;
      AHostClass: TPersistentClass);
    property HostClass: TPersistentClass read FHostClass write SetHostClass;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CreateColumns(AColumnLinkList: TColumnLink); override;
    procedure CreateChildrenNodes(ANodeCreator: TFunNodeLink); override;
    procedure IterateChildrenObject(AGetter: TProc<TRttiMember>;
      ARecursive: Boolean); override;
  end;

  TFileNamePropLink = class(TStrPropLink)
  protected
    function CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
      : TWinControl; override;
    function SetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    function GetEditorValue(AEditor: TWinControl; AColumn: Integer;
      AColumnLink: TColumnLink): Boolean; override;
    procedure SetBounds(R: TRect); override; stdcall;

    function GetMemberValue: string; override;
    procedure OnButtonClick(ASender: TObject); virtual;
  end;

procedure RegisterPropLink(PropertyType: PTypeInfo; ComponentClass: TClass;
  const PropertyName: string; EditorClass: TPropLinkClass);

implementation

{ Property Editor registration }

type
  TPropertyMapperFunc = function(ObjClass: TPersistentClass;
    PropInfo: PPropInfo): TPropLinkClass;

  PPropertyClassRec = ^TPropertyClassRec;

  TPropertyClassRec = record
    Group: Integer;
    PropertyType: PTypeInfo;
    PropertyName: string;
    ComponentClass: TClass;
    ClassGroup: TPersistentClass;
    EditorClass: TPropLinkClass;
  end;

  PPropertyMapperRec = ^TPropertyMapperRec;

  TPropertyMapperRec = record
    Group: Integer;
    Mapper: TPropertyMapperFunc;
  end;

const
  PropClassMap: array [System.TypInfo.TTypeKind] of TPropLinkClass = (nil,
    // tkUnknown
    TIntPropLink, // tkInteger
    TCharPropLink, // tkChar
    TEnumPropLink, // tkEnumeration
    TFloatPropLink, // tkFloat
    TStrPropLink, // tkString
    TSetPropLink, // tkSet
    TObjectPropLink, // tkClass
    TMethodPropLink, // tkMethod
    TWCharPropLink, // tkWChar
    TStrPropLink, // tkLString
    TWStrPropLink, // tkWString
    TVariantPropLink, // tkVariant
    nil, // tkArray
    nil, // tkRecord
    TIntfPropLink, // tkInterface
    TInt64PropLink, // tkInt64
    nil, // tkDynArray
    TStrPropLink, // tkUString
    nil, // tkClassRef
    nil, // tkPointer
    nil, // tkProcedure
    nil // tkMRecord
    );

var
  PropertyClassList: TList = nil;
  PropertyMapperList: TList = nil;

procedure RegisterPropLink(PropertyType: PTypeInfo; ComponentClass: TClass;
  const PropertyName: string; EditorClass: TPropLinkClass);
var
  P: PPropertyClassRec;
begin
  if PropertyClassList = nil then
    PropertyClassList := TList.Create;
  New(P);
  P.Group := CurrentGroup;
  P.PropertyType := PropertyType;
  P.ComponentClass := ComponentClass;
  P.PropertyName := '';
  P.ClassGroup := nil;
  if Assigned(ComponentClass) then
    P^.PropertyName := PropertyName;
  P.EditorClass := EditorClass;
  PropertyClassList.Insert(0, P);
end;

procedure SetPropertyEditorGroup(EditorClass: TPropLinkClass;
  GroupClass: TPersistentClass);
var
  P: PPropertyClassRec;
  I: Integer;
begin
  for I := 0 to PropertyClassList.Count - 1 do
  begin
    P := PropertyClassList[I];
    if P^.EditorClass = EditorClass then
    begin
      P^.ClassGroup := ClassGroupOf(GroupClass);
      Exit;
    end;
  end;
  // Ignore it if the EditorClass is not found.
end;

function InterfaceInheritsFrom(Child, Parent: PTypeData): Boolean;
begin
  while (Child <> nil) and (Child <> Parent) and (Child^.IntfParent <> nil) do
    Child := GetTypeData(Child^.IntfParent^);
  Result := (Child <> nil) and (Child = Parent);
end;

function GetPropClass(PropInfo: PPropInfo; ObjClass: TPersistentClass)
  : TPropLinkClass;
var
  PropType: PTypeInfo;
  P, C: PPropertyClassRec;
  I: Integer;
begin
  if PropertyMapperList <> nil then
  begin
    for I := 0 to PropertyMapperList.Count - 1 do
      with PPropertyMapperRec(PropertyMapperList[I])^ do
      begin
        Result := Mapper(ObjClass, PropInfo);
        if Result <> nil then
          Exit;
      end;
  end;
  PropType := PropInfo^.PropType^;
  I := 0;
  C := nil;
  while I < PropertyClassList.Count do
  begin
    P := PropertyClassList[I];

    if ((P^.PropertyType = PropType) { or
        ((P^.PropertyType^.Kind = PropType.Kind) and
        (P^.PropertyType^.Name = PropType.Name)
        ) }
      ) or // compatible class type
      ((PropType^.Kind = tkClass) and (P^.PropertyType^.Kind = tkClass) and
      GetTypeData(PropType)^.ClassType.InheritsFrom(GetTypeData(P^.PropertyType)
      ^.ClassType)) or // compatible interface type
      ((PropType^.Kind = tkInterface) and (P^.PropertyType^.Kind = tkInterface)
      and InterfaceInheritsFrom(GetTypeData(PropType),
      GetTypeData(P^.PropertyType))) then
      if ((P^.ComponentClass = nil) or (ObjClass.InheritsFrom(P^.ComponentClass)
        )) and ((P^.ClassGroup = nil) or (P^.ClassGroup = ClassGroupOf(ObjClass)
        )) and ((P^.PropertyName = '') or (CompareText(GetPropName(PropInfo),
        P^.PropertyName) = 0)) then
        if (C = nil) or // see if P is better match than C
          ((C^.ComponentClass = nil) and (P^.ComponentClass <> nil)) or
          ((C^.PropertyName = '') and (P^.PropertyName <> '')) or
        // P's proptype match is exact, but C's isn't
          ((C^.PropertyType <> PropType) and (P^.PropertyType = PropType)) or
        // P's proptype is more specific than C's proptype
          ((P^.PropertyType <> C^.PropertyType) and ((
          // P has a more specific class type than C.
          (P^.PropertyType^.Kind = tkClass) and
          (C^.PropertyType^.Kind = tkClass) and GetTypeData(P^.PropertyType)
          ^.ClassType.InheritsFrom(GetTypeData(C^.PropertyType)^.ClassType)) or
          // P has a more specific interface type than C.
          ((P^.PropertyType^.Kind = tkInterface) and
          (C^.PropertyType^.Kind = tkInterface) and
          InterfaceInheritsFrom(GetTypeData(P^.PropertyType),
          GetTypeData(C^.PropertyType))))) or
        // P's component class is more specific than C's component class
          ((P^.ComponentClass <> nil) and (C^.ComponentClass <> nil) and
          (P^.ComponentClass <> C^.ComponentClass) and
          (P^.ComponentClass.InheritsFrom(C^.ComponentClass))) then
          C := P;
    Inc(I);
  end;
  if C <> nil then
    Result := C^.EditorClass
  else
    Result := PropClassMap[PropType^.Kind];
end;

procedure FreeEditorGroup();
var
  I: Integer;
  P: PPropertyClassRec;
  M: PPropertyMapperRec;
begin
  // Release all property editors associated with the group
  if Assigned(PropertyClassList) then
  begin
    I := PropertyClassList.Count - 1;
    while I > -1 do
    begin
      P := PropertyClassList[I];

      PropertyClassList.Delete(I);
      Dispose(P);

      Dec(I);
    end;
  end;

  // Release all property mappers associated with the group
  if Assigned(PropertyMapperList) then
    if PropertyMapperList <> nil then
      for I := PropertyMapperList.Count - 1 downto 0 do
      begin
        M := PropertyMapperList[I];

        PropertyMapperList.Delete(I);
        Dispose(M);

      end;
end;

{$R *.dfm}

procedure TFrameObjectInspector.AfterConstruction;
begin
  inherited;
  FrameObjectTree1.ShowRoot := False;
  FTypeKinds := tkProperties - [tkProcedure];

  with TabControl1 do
    if (Tabs.Count > 0) and (TabIndex <> 0) then
      TabIndex := 0;
end;

procedure TFrameObjectInspector.BeforeDestruction;
begin
  inherited;
  SetHost(nil, nil);
end;

procedure TFrameObjectInspector.FrameObjectTree1VTreeCompareNodes
  (Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
begin
  FrameObjectTree1.VTreeCompareNodes(Sender, Node1, Node2, Column, Result);
end;

procedure TFrameObjectInspector.FrameObjectTree1VTreeEditing
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  Allowed := Column <> 0;
end;

function TFrameObjectInspector.GetHost: TPersistent;
begin
  Result := nil;
  if Assigned(FrameObjectTree1) and Assigned(FrameObjectTree1.RootLink) then
    Result := (FrameObjectTree1.RootLink as THostLink).Host as TPersistent;
end;

procedure TFrameObjectInspector.SetHost(ADesigner: IDesigner;
  const Value: TPersistent);
begin
  if Assigned(FrameObjectTree1) then
    with FrameObjectTree1 do
      if Self.Host <> Value then
        if Assigned(ADesigner) and Assigned(Value) then
          UpdateTree(
            procedure()
            var
              LRootLink: TNodeLink;
            begin
              LRootLink := SetRootLink(THostLink.Create(ADesigner, nil, Value,
                nil, ''));
              if Assigned(LRootLink) then
                with LRootLink do
                begin
                  Self.SetTypeKinds(Self.FTypeKinds);
                  Expanded[True] := True;
                end;
            end)
        else
          SetRootLink(nil);
end;

procedure TFrameObjectInspector.SetTypeKinds(const Value: TTypeKinds);
begin
  Self.FTypeKinds := Value;

  with FrameObjectTree1 do
    if RootLink <> nil then
      UpdateTree(
        procedure()
        begin
          RootLink.IterateChildrenLinks(
            procedure(N: TNodeLink)
            begin
              if N is TProcedureLink then
              begin
                if tkProcedure in Self.FTypeKinds then
                  N.Node^.States := N.Node^.States + [vsVisible]
                else
                  N.Node^.States := N.Node^.States - [vsVisible];
              end
              else if (N is TPropLink) and Assigned(TPropLink(N).PropInfo) then
                if (TPropLink(N).PropInfo^.PropType^^.Kind in Self.FTypeKinds)
                then
                  N.Node^.States := N.Node^.States + [vsVisible]
                else
                  N.Node^.States := N.Node^.States - [vsVisible];
            end, True);
        end);
end;

procedure TFrameObjectInspector.TabControl1Change(Sender: TObject);
begin
  case (Sender as TTabControl).TabIndex of
    0:
      TypeKinds := tkProperties - [tkProcedure];
    1:
      TypeKinds := tkMethods;
    2:
      TypeKinds := [tkProcedure];
  else
    TypeKinds := tkProperties;
  end;
end;

{ TPropLink }

procedure TPropLink.AfterConstruction;
begin
  inherited;

end;

procedure TPropLink.BeforeDestruction;
begin
  inherited;
  Data := nil;
end;

constructor TPropLink.Create(ADesigner: IDesigner; AParentLink: TNodeLink;
AHost: TObject; ARTTIInfo: TRttiInstanceProperty; const APropName: string);
begin
  inherited Create(ADesigner, AParentLink, AHost, ARTTIInfo, APropName);
end;

function TPropLink.CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
  : TWinControl;
begin
  Result := nil;
  if AColumn = 1 then
  begin
    Result := TEdit.Create(Tree);
    with Result as TEdit do
    begin
      OnKeyDown := Self.EditKeyDown;
      OnKeyUp := Self.EditKeyUp;
    end;
  end;
end;

function TPropLink.GetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  if AColumn = 1 then
    MemberValue := (AEditor as TEdit).Text;
  Result := True;
end;

function TPropLink.GetMemberValue: string;
begin
  Result := '';
end;

function TPropLink.GetPropInfo: PPropInfo;
begin
  if Assigned(Data) then
    Result := Self.Data.PropInfo
  else
    Result := nil;
end;

function TPropLink.SetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
  if AColumn = 1 then
    (AEditor as TEdit).Text := MemberValue;
end;

procedure TPropLink.SetMemberValue(const Value: string);
begin
  inherited;

end;

{ TIntPropLink }

function TIntPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := IntToStr(GetOrdProp(Host, PropInfo))
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

procedure TIntPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetOrdProp(Host, PropInfo, StrToInt(Value));
end;

{ TCharPropLink }

function TCharPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := char(GetOrdProp(Host, PropInfo))
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := 'Char';
end;

procedure TCharPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetOrdProp(Host, PropInfo, Ord(Value[1]));
end;

{ TEnumPropLink }

function TEnumPropLink.CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
  : TWinControl;
var
  T: PTypeData;
  I: Integer;
  S: string;
begin
  Result := TComboBox.Create(Tree);
  with Result as TComboBox do
  begin
    Parent := Tree;
    OnKeyDown := Self.EditKeyDown;
    OnKeyUp := Self.EditKeyUp;
  end;

  T := GetTypeData(GetTypeData(PropInfo^.PropType^)^.BaseType^);
  if T^.minvalue >= 0 then
    for I := T^.minvalue to T^.maxvalue do
    begin
      S := GetEnumName(PropInfo^.PropType^, I);
      if S <> '' then
        TComboBox(Result).Items.Append(S);
    end;

end;

function TEnumPropLink.GetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  if AColumn = 1 then
    MemberValue := (AEditor as TComboBox).Text;
  Result := True;
end;

function TEnumPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := GetEnumProp(Host, PropInfo)
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

function TEnumPropLink.SetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
  if AColumn = 1 then
    (AEditor as TComboBox).Text := MemberValue;
end;

procedure TEnumPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetEnumProp(Host, PropInfo, Value);
end;

{ TFloatPropLink }

function TFloatPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := FloatToStr(GetFloatProp(Host, PropInfo))
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

procedure TFloatPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetFloatProp(Host, PropInfo, StrToFloat(Value));
end;

{ TStrPropLink }

function TStrPropLink.GetMemberValue: string;
begin
  try
    if Assigned(PropInfo) then
      if Assigned(Host) then
        Result := GetStrProp(Host, PropInfo)
      else
        Result := GetTypeName(PropInfo^.PropType^)
    else
      Result := '';
  except
    on E: Exception do
      Result := MemberName;
  end;

end;

procedure TStrPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetStrProp(Host, PropInfo, Value);
end;

{ TSetPropLink }

procedure TSetPropLink.CreateChildrenNodes(ANodeCreator: TFunNodeLink);
var
  I: Integer;
  LTypeInfo: PTypeInfo;
  LLink: TSetItemLink;
begin
  if Assigned(PropInfo) then
  begin
    LTypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
    with GetTypeData(LTypeInfo)^ do
      for I := minvalue to maxvalue do
      begin
        LLink := TSetItemLink.Create(Self.FDesigner, Self, Self.FHost,
          Self.Data, GetSetElementName(LTypeInfo, I));
        LLink.FValue := I;
        ANodeCreator(LLink);
      end;
  end;
end;

function TSetPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := GetSetProp(Host, PropInfo, True)
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

function TSetPropLink.GetSetValue: TIntegerSet;
begin
  if Assigned(Host) and Assigned(PropInfo) then
    Result := TIntegerSet(GetOrdProp(Host, PropInfo))
  else
    Result := [];
end;

procedure TSetPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetSetProp(Host, PropInfo, Value);
end;

{ TObjectPropLink }

procedure TObjectPropLink.AfterConstruction;
begin
  inherited;
  FContext := TRttiContext.Create;

end;

procedure TObjectPropLink.BeforeDestruction;
begin
  inherited;
  FContext.Free;
end;

procedure TObjectPropLink.CreateChildrenNodes(ANodeCreator: TFunNodeLink);
begin
  IterateChildrenObject(
    procedure(P: TRttiInstanceProperty)
    var
      LSubObj: TObject;
    begin
      LSubObj := Self.Host;
      if P.PropInfo^.PropType^^.Kind = tkClass then
        LSubObj := GetObjectProp(Self.Host, P.PropInfo);

      if Self.Host is TPersistent then
        ANodeCreator(GetPropClass(P.PropInfo,
          TPersistentClass(Self.Host.ClassType)).Create(Self.FDesigner, Self,
          LSubObj, P, P.Name));
    end, False);
end;

function TObjectPropLink.GetMemberValue: string;
begin
  if Assigned(Host) then
    if Host is TComponent then
      Result := TComponent(Host).Name
    else if Host is TPersistent then
      Result := '...';
end;

function TObjectPropLink.GetParentHost: TObject;
begin
  Result := nil;
  if Assigned(ParentLink) then
    if ParentLink is TPropLink then
      Result := TPropLink(ParentLink).Host
    else if ParentLink is TProcedureLink then
      Result := TProcedureLink(ParentLink).Host
    else if ParentLink is THostLink then
      Result := THostLink(ParentLink).Host;
end;

procedure TObjectPropLink.IterateChildrenObject
  (AGetter: TProc<TRttiInstanceProperty>; ARecursive: Boolean);
var
  I: Integer;
  LProp: TRttiProperty;
begin
  if Assigned(FHost) then
    for LProp in FContext.GetType(FHost.ClassType).GetProperties do
      if (LProp is TRttiInstanceProperty) and (LProp.Visibility = mvPublished)
      then
        AGetter(TRttiInstanceProperty(LProp));
end;

{ TMethodPropLink }

function TMethodPropLink.CreateEditor(AColumn: Integer;
AColumnLink: TColumnLink): TWinControl;
begin
  Result := nil;
  if AColumn = 1 then
  begin
    Result := TComboBox.Create(Tree);
    with Result as TComboBox do
    begin
      Parent := Tree;
      OnKeyDown := Self.EditKeyDown;
      OnKeyUp := Self.EditKeyUp;
      LoadValueList(Items);
    end;
  end;
end;

function TMethodPropLink.GetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
  if AColumn = 1 then
    MemberValue := (AEditor as TComboBox).Text;
end;

function TMethodPropLink.GetMemberValue: string;
var
  M: TMethod;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
    begin
      M := GetMethodProp(Host, PropInfo);
      if Assigned(M.Code) and Assigned(M.Data) then
        // Result := TObject(M.Data).MethodName(M.Code);
        Result := FDesigner.GetMethodName(M);
    end
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

procedure TMethodPropLink.LoadValueList(AValueList: TStrings);
begin
  if Assigned(FDesigner) then
  begin
    IterateObjects(FHost, FDesigner.Root,
      procedure(O: TObject)
      begin
        IterateMethods(O.ClassType,
          procedure(M: TRttiMethod)
          var
            LMethod: TMethod;
          begin
            with LMethod do
            begin
              Data := O;
              Code := M.CodeAddress;
            end;
            AValueList.Append(FDesigner.GetMethodName(LMethod));
          end);
      end);
  end;
end;

function TMethodPropLink.SetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
  if AColumn = 1 then
    (AEditor as TComboBox).Text := MemberValue;
end;

procedure TMethodPropLink.SetMemberValue(const Value: string);
begin
  // if (Trim(Value) <> '') and Assigned(Host) and Assigned(PropInfo) then
  // SetMethodProp(Host, PropInfo, TMethod(FDesigner.Root.MethodAddress(Value)));
end;

{ TWCharPropLink }

function TWCharPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := widechar(GetOrdProp(Host, PropInfo))
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

procedure TWCharPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetOrdProp(Host, PropInfo, Ord(widestring(Value)[1]));
end;

{ TWStrPropLink }

function TWStrPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := GetWideStrProp(Host, PropInfo)
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

procedure TWStrPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetWideStrProp(Host, PropInfo, Value);
end;

{ TVariantPropLink }

function TVariantPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := VarToStr(GetVariantProp(Host, PropInfo))
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

procedure TVariantPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetVariantProp(Host, PropInfo, Value);
end;

{ TIntfPropLink }

function TIntfPropLink.GetMemberValue: string;
var
  LIntf: IInterface;
  LCompIntf: IInterfaceComponentReference;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
    begin
      LIntf := GetInterfaceProp(Host, PropInfo);
      if Assigned(LIntf) and (LIntf.QueryInterface(IInterfaceComponentReference,
        LCompIntf) = S_OK) and (LCompIntf.GetComponent() <> nil) then
        Result := LCompIntf.GetComponent.Name;
    end
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

{ TInt64PropLink }

function TInt64PropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := IntToStr(GetInt64Prop(Host, PropInfo))
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

procedure TInt64PropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetInt64Prop(Host, PropInfo, StrToInt64(Value));
end;

{ THostLink }

procedure THostLink.AfterConstruction;
begin
  inherited;
  FContext := TRttiContext.Create;

  if (MemberName = '') and Assigned(FHost) then
    MemberName := FHost.ClassName;
end;

procedure THostLink.BeforeDestruction;
begin
  inherited;
  FContext.Free;
end;

procedure THostLink.CreateChildrenNodes(ANodeCreator: TFunNodeLink);
begin
  IterateChildrenObject(
    procedure(P: TRttiMember)
    var
      LSubObj: TObject;
    begin
      if P is TRttiMethod then
        ANodeCreator(TProcedureLink.Create(Self.FDesigner, Self, Self.FHost,
          P as TRttiMethod, P.Name))
      else if P is TRttiInstanceProperty then
        with P as TRttiInstanceProperty do
        begin
          LSubObj := Self.Host;
          if PropInfo^.PropType^^.Kind = tkClass then
            LSubObj := GetObjectProp(Self.Host, PropInfo);

          if Self.Host is TPersistent then
            ANodeCreator(GetPropClass(PropInfo,
              TPersistentClass(Self.Host.ClassType)).Create(Self.FDesigner,
              Self, LSubObj, P as TRttiInstanceProperty, P.Name));
        end;
    end, False);
end;

procedure THostLink.CreateColumns(AColumnLinkList: TColumnLink);
begin
  inherited;
  with AColumnLinkList do
  begin
    AddLink('Name').Width := 150;
    with AddLink('Value') do
    begin
      Width := 200;
      Editing := True;
    end;
  end;
end;

procedure THostLink.IterateChildrenObject(AGetter: TProc<TRttiMember>;
ARecursive: Boolean);
var
  M: TRttiMember;
begin
  if Assigned(FHost) then
    with FContext.GetType(FHost.ClassType) do
    begin
      for M in GetProperties do
        if M.Visibility = mvPublished then
          AGetter(M);

      for M in GetMethods do
        if M.Visibility = mvPublished then
          AGetter(M);
    end;
end;

{ TBoolPropLink }

function TBoolPropLink.CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
  : TWinControl;
begin
  Result := nil;
  if AColumn = 1 then
  begin
    Result := TComboBox.Create(Tree);
    with Result as TComboBox do
    begin
      Parent := Tree;
      Items.Append('True');
      Items.Append('False');
      OnKeyDown := Self.EditKeyDown;
      OnKeyUp := Self.EditKeyUp;
    end;
  end;
end;

function TBoolPropLink.GetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  MemberValue := (AEditor as TComboBox).Text;
  Result := True;
end;

function TBoolPropLink.GetMemberValue: string;
begin
  if Assigned(PropInfo) then
    if Assigned(Host) then
      Result := BoolToStr(GetOrdProp(Host, PropInfo) <> 0, True)
    else
      Result := GetTypeName(PropInfo^.PropType^)
  else
    Result := '';
end;

function TBoolPropLink.SetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  (AEditor as TComboBox).Text := MemberValue;
  Result := True;
end;

procedure TBoolPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(Host) and Assigned(PropInfo) then
    SetOrdProp(Host, PropInfo, Ord(StrToBool(Value)));
end;

{ TSetPropLink.TSetItemLink }

function TSetPropLink.TSetItemLink.GetMemberValue: string;
begin
  Result := BoolToStr(FValue in TSetPropLink(ParentLink).GetSetValue, True);
end;

procedure TSetPropLink.TSetItemLink.SetMemberValue(const Value: string);
var
  LSet: TIntegerSet;
begin
  if Assigned(Host) then
    with ParentLink as TSetPropLink do
    begin
      LSet := GetSetValue;
      if StrToBool(Value) then
        Include(LSet, FValue)
      else
        Exclude(LSet, FValue);
      SetOrdProp(Host, PropInfo, Integer(LSet));
    end;
end;

{ TComponentPropLink }

procedure TComponentPropLink.CreateChildrenNodes(ANodeCreator: TFunNodeLink);
begin
  (*
    donot create children links
  *)
end;

function TComponentPropLink.CreateEditor(AColumn: Integer;
AColumnLink: TColumnLink): TWinControl;
begin
  Result := nil;
  if AColumn = 1 then
  begin
    Result := TComboBox.Create(Tree);
    with Result as TComboBox do
    begin
      Parent := Tree;
      OnKeyDown := Self.EditKeyDown;
      OnKeyUp := Self.EditKeyUp;
      LoadValueList(Items);
    end;
  end;
end;

function TComponentPropLink.GetEditorValue(AEditor: TWinControl;
AColumn: Integer; AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
  if AColumn = 1 then
    MemberValue := (AEditor as TComboBox).Text;
end;

function TComponentPropLink.GetMemberValue: string;
begin
  if Assigned(FDesigner) and Assigned(Host) and (Host is TComponent) then
    Result := FDesigner.GetComponentName(TComponent(Host))
  else
    Result := 'TComponent';
end;

procedure TComponentPropLink.LoadValueList(AValueList: TStrings);
var
  LClass: TClass;
begin
  if Assigned(ParentHost) and Assigned(PropInfo) and Assigned(FDesigner) then
  begin
    LClass := GetObjectPropClass(ParentHost, PropInfo);
    if Assigned(LClass) then
    begin
      if FDesigner.Root.InheritsFrom(LClass) then
        AValueList.Append(FDesigner.GetComponentName(FDesigner.Root));

      IterateComponent(FDesigner.Root,
        procedure(C: TComponent)
        begin
          if C.InheritsFrom(LClass) then
            AValueList.Append(FDesigner.GetComponentName(C));
        end, True);
    end;
  end;
end;

function TComponentPropLink.SetEditorValue(AEditor: TWinControl;
AColumn: Integer; AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
  if AColumn = 1 then
    (AEditor as TComboBox).Text := MemberValue;
end;

procedure TComponentPropLink.SetMemberValue(const Value: string);
begin
  if Assigned(FDesigner) and Assigned(ParentHost) and Assigned(PropInfo) then
  begin
    if Trim(Value) = '' then
      FHost := nil
    else
      FHost := FDesigner.GetComponent(Value);
    SetObjectProp(ParentHost, PropInfo, FHost);
  end;
end;

{ TRTTIMemberLink<T> }

constructor TRTTIMemberLink<T>.Create(ADesigner: IDesigner;
AParentLink: TNodeLink; AHost: TObject; ARTTIInfo: T; const APropName: string);
begin
  inherited Create(AParentLink, ARTTIInfo);
  FDesigner := ADesigner;
  FHost := AHost;
  FMemberName := APropName;
end;

function TRTTIMemberLink<T>.GetMemberValue: string;
begin
  Result := '';
end;

function TRTTIMemberLink<T>.GetNodeText(AColumn: Integer;
AColumnLink: TColumnLink): string;
begin
  case AColumn of
    - 1, 0:
      Result := MemberName;
    1:
      Result := MemberValue;
  else
    Result := MemberName;
  end;
end;

procedure TRTTIMemberLink<T>.SetMemberValue(const Value: string);
begin

end;

{ TProcedureLink }

function TProcedureLink.CreateEditor(AColumn: Integer; AColumnLink: TColumnLink)
  : TWinControl;
begin
  Result := nil;
  if AColumn = 1 then
  begin
    Result := TButton.Create(Tree);
    with Result as TButton do
    begin
      Caption := '...';
      OnClick := Self.OnButtonClick;
    end;
  end;
end;

function TProcedureLink.GetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
end;

function TProcedureLink.GetMemberValue: string;
begin
  if FResult = '' then
    FResult := '...';
  Result := FResult;
end;

procedure TProcedureLink.OnButtonClick(ASender: TObject);
begin
  if Assigned(FHost) and (not Data.IsStatic) and (not Data.IsClassMethod) and
    (not Data.IsConstructor) and (not Data.IsDestructor) and
    (length(Data.GetParameters) = 0) then
  begin
    with Data.Invoke(FHost, []) do
      if not IsEmpty then
        FResult := ToString();
  end;
end;

procedure TProcedureLink.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  if not Assigned(Editor) then
    Exit;
  Tree.Header.Columns.GetColumnBounds(FActivedColumn, Dummy, R.Right);
  with Editor do
  begin
    BoundsRect := R;
    Width := R.Height + 2;
    Height := R.Height + 2;
  end;
end;

function TProcedureLink.SetEditorValue(AEditor: TWinControl; AColumn: Integer;
AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
end;

{ TFileNamePropLink }

function TFileNamePropLink.CreateEditor(AColumn: Integer;
AColumnLink: TColumnLink): TWinControl;
begin
  Result := nil;
  if AColumn = 1 then
  begin
    Result := TButton.Create(Tree);
    with Result as TButton do
    begin
      Caption := '...';
      OnClick := Self.OnButtonClick;
    end;
  end;
end;

function TFileNamePropLink.GetEditorValue(AEditor: TWinControl;
AColumn: Integer; AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
end;

function TFileNamePropLink.GetMemberValue: string;
begin
  Result := inherited GetMemberValue;
  if Result = '' then
    Result := '.';
end;

procedure TFileNamePropLink.OnButtonClick(ASender: TObject);
begin
  with TOpenDialog.Create(Tree) do
    try
      if execute then
        MemberValue := FileName;
    finally
      Free;
    end;
end;

procedure TFileNamePropLink.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  if not Assigned(Editor) then
    Exit;
  Tree.Header.Columns.GetColumnBounds(FActivedColumn, Dummy, R.Right);
  with Editor do
  begin
    BoundsRect := R;
    Width := R.Height + 2;
    Height := R.Height + 2;
    Left := R.Right - R.Height - 2;
  end;
end;

function TFileNamePropLink.SetEditorValue(AEditor: TWinControl;
AColumn: Integer; AColumnLink: TColumnLink): Boolean;
begin
  Result := True;
end;

{ TClassTypeLink }

procedure TClassTypeLink.AfterConstruction;
begin
  inherited;
  FContext := TRttiContext.Create;

  if (MemberName = '') and Assigned(FHostClass) then
    MemberName := FHostClass.ClassName;
end;

procedure TClassTypeLink.BeforeDestruction;
begin
  inherited;
  FContext.Free;
end;

constructor TClassTypeLink.Create(ADesigner: IDesigner; AParentLink: TNodeLink;
AHostClass: TPersistentClass);
begin
  inherited Create(ADesigner, AParentLink, nil, nil, '');
  HostClass := AHostClass;
end;

procedure TClassTypeLink.CreateChildrenNodes(ANodeCreator: TFunNodeLink);
begin
  IterateChildrenObject(
    procedure(P: TRttiMember)
    var
      LClassProp: TClass;
    begin
      if P is TRttiMethod then
        ANodeCreator(TProcedureLink.Create(Self.FDesigner, Self, Self.FHost,
          P as TRttiMethod, P.Name))
      else if P is TRttiInstanceProperty then
        with P as TRttiInstanceProperty do
        begin
          LClassProp := nil;
          if PropInfo^.PropType^^.Kind = tkClass then
            LClassProp := GetObjectPropClass(PropInfo);

          ANodeCreator(GetPropClass(PropInfo, Self.HostClass)
            .Create(Self.FDesigner, Self, nil,
            P as TRttiInstanceProperty, P.Name));
        end;
    end, False);
end;

procedure TClassTypeLink.CreateColumns(AColumnLinkList: TColumnLink);
begin
  inherited;
  with AColumnLinkList do
  begin
    AddLink('Name').Width := 150;
    with AddLink('Type') do
    begin
      Width := 200;
      Editing := True;
    end;
  end;
end;

function TClassTypeLink.GetMemberValue: string;
begin
  if Assigned(FHostClass) then
    Result := FHostClass.ClassName
  else
    Result := '';
end;

procedure TClassTypeLink.IterateChildrenObject(AGetter: TProc<TRttiMember>;
ARecursive: Boolean);
var
  M: TRttiMember;
begin
  if Assigned(FHostClass) then
    with FContext.GetType(FHostClass) do
    begin
      for M in GetProperties do
        if M.Visibility = mvPublished then
          AGetter(M);

      for M in GetMethods do
        if M.Visibility = mvPublished then
          AGetter(M);
    end;
end;

procedure TClassTypeLink.SetHostClass(const Value: TPersistentClass);
begin
  FHostClass := Value;
end;

initialization

RegisterPropLink(TPersistent.ClassInfo, TPersistent, '', TPersistentPropLink);
RegisterPropLink(TComponent.ClassInfo, TComponent, '', TComponentPropLink);
RegisterPropLink(TypeInfo(Boolean), TObject, '', TBoolPropLink);
RegisterPropLink(TypeInfo(TFileName), TObject, '', TFileNamePropLink);

finalization

FreeEditorGroup;

end.
