unit fmobjecttree;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, VirtualTrees, Contnrs, fgl, LMessages;

type
  TColumnLink = class;
  TNodeLink = class;
  TObjectLink = class;
  TNodeLinkClass = class of TNodeLink;
  TObjectLinkClass = class of TObjectLink;
  TColumnLinkClass = class of TColumnLink;
  TFunNodeLink = function(L: TNodeLink): TNodeLink of object;
  TGetNodeLink = procedure(L: TNodeLink) is nested;
  TPredicateNodeLink = function(L: TNodeLink): boolean is nested;
  TPredicateObjectLink = function(L: TObjectLink; O: TObject): boolean is nested;

  TNodeLinkList = class(specialize TFPGObjectList<TNodeLink>)

  end;


  TFrameObjectTree = class(TFrame)
    VTree: TVirtualStringTree;
    procedure VTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: integer);
    procedure VTreeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: boolean);
    procedure VTreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VTreeCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure VTreeAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTreeRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  private
    FRootObject: TObject;
    FRootLink: TNodeLink;
    FOwnedRootObject: boolean;
    FExpandDepth: integer;
    FSelectedLinkList: specialize TFPGObjectList<TNodeLink>;
    FOnAddToSelection: TGetNodeLink;
    FOnRemoveFromSelection: TGetNodeLink;
    FShowRoot: boolean;
  protected
    procedure StretchVTImages(); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property ExpandDepth: integer read FExpandDepth write FExpandDepth;
    property OwnedRootObject: boolean read FOwnedRootObject write FOwnedRootObject;
    property RootLink: TNodeLink read FRootLink;
    property ShowRoot: boolean read FShowRoot write FShowRoot;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function SetRootLink(const Value: TNodeLink): TNodeLink;
    function SetRootObject(ARoot: TObject;
      ALinkClass: TObjectLinkClass = nil): TNodeLink;
    function GetRootObject(): TObject;
    function Rebuild(): TNodeLink;
    (*
      iterate all links and use getter as a predicate to see if it is a valid rule for each element
    *)
    function IterateLinks(AGetter: TPredicateNodeLink): boolean; overload;
    function IterateLinks(AGetter: TPredicateNodeLink;
      ALinkClass: TNodeLinkClass): boolean; overload;
    function IterateObjects(AGetter: TPredicateObjectLink): boolean;

    property OnAddToSelection: TGetNodeLink read FOnAddToSelection
      write FOnAddToSelection;
    property OnRemoveFromSelection: TGetNodeLink
      read FOnRemoveFromSelection write FOnRemoveFromSelection;

    function GetNodeLink(AGetter: TGetNodeLink; ANode: PVirtualNode;
      ATree: TVirtualStringTree = nil; ALinkClass: TNodeLinkClass = nil): boolean;
      overload;
    function GetNodeLink(ANode: PVirtualNode;
      ATree: TVirtualStringTree = nil): TNodeLink; overload;
    (*
      get all selected nodes with a predicate, if no selection, it returns false.
    *)
    function GetSelections(AGetter: TPredicateNodeLink;
      ALinkClass: TNodeLinkClass = nil): boolean;
    function SelectedCount(): integer;
    function GetSelectedObject(): TObject; overload;
    function GetSelectedObject(AGetter: TPredicateObjectLink): boolean; overload;
  end;

  TNexus = class(TComponent)
  private
    FOnFreeNotify: TGetChildProc;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    property OnFreeNotify: TGetChildProc read FOnFreeNotify write FOnFreeNotify;
  end;

  { -M }
  TNodeLink = class(TInterfacedPersistent, IVTEditLink)
  public
    type
    TMoveKind = (mkInsertBefore, // insert node before destination as sibling
      mkInsertAfter, // insert node after destionation as sibling
      mkAddChildFirst, // add node as first child of destination
      mkAddChildLast // add node as last child of destination
    );
  private
    FNode: PVirtualNode;
    FTree: TVirtualStringTree;
    FParentLink: TNodeLink;
    FChildrenLinkList: TNodeLinkList;
    FColumnLinkList: TColumnLink;
    FImageIndex: integer;
    FEditor: TWinControl; // One of the property editor classes.
    FNexus: TNexus;
    procedure SetParentLink(AValue: TNodeLink);
    function GetChildrenLinkList: TNodeLinkList;
    function GetColumnLinkList: TColumnLink;
    function GetExpanded(ARecursive: boolean): boolean;
    procedure SetExpanded(ARecursive: boolean; const Value: boolean);
    procedure SetEditor(const Value: TWinControl);
    function GetSelected: boolean;
    procedure SetSelected(const Value: boolean);
    procedure SetImageIndex(const Value: integer);

  protected
    { IVTEditLink }
    FActivedColumn: integer;
    FActivedColumnLink: TColumnLink;
    // Called when editing actually starts.
    function BeginEdit: boolean; stdcall;
    // Called when editing has been cancelled by the tree.
    function CancelEdit: boolean; stdcall;
    // Called when editing has been finished by the tree.
    function EndEdit: boolean; stdcall;
    // Called after creation to allow a setup.
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): boolean; stdcall;
    // Called to get the current size of the edit window
    // (only important if the edit resizes itself).
    function GetBounds: TRect; stdcall;
    // Used to forward messages to the edit window(s)-
    procedure ProcessMessage(var Message: TLMessage); stdcall;
    // Called to place the editor.
    procedure SetBounds(R: TRect); virtual; stdcall;

    function CreateEditor(AColumn: integer;
      AColumnLink: TColumnLink): TWinControl; virtual;
    function SetEditorValue(AEditor: TWinControl; AColumn: integer;
      AColumnLink: TColumnLink): boolean; virtual;
    function GetEditorValue(AEditor: TWinControl; AColumn: integer;
      AColumnLink: TColumnLink): boolean; virtual;
    procedure EditKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState); virtual;
    procedure EditKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState); virtual;

    procedure SetNode(const Value: PVirtualNode); virtual;

    function BuildChildrenCondition(): boolean; virtual;
    function MoveTo(ATargetLink: TNodeLink; AMoveKind: TMoveKind): boolean;
      virtual;
  public
    constructor Create(AParentLink: TNodeLink);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Node: PVirtualNode read FNode write SetNode;
    property Tree: TVirtualStringTree read FTree;
    property Editor: TWinControl read FEditor write SetEditor;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property Expanded[ARecursive: boolean]: boolean read GetExpanded write SetExpanded;
    property Selected: boolean read GetSelected write SetSelected;
    procedure SetFocus(ASelect: boolean = True);
    property ParentLink: TNodeLink read FParentLink write SetParentLink;
    property ChildrenLinkList:specialize TFPGObjectList<TNodeLink> read GetChildrenLinkList;
    property ColumnLinkList: TColumnLink read GetColumnLinkList;

    function MoveUp(): TNodeLink; virtual;
    function MoveDown(): TNodeLink; virtual;
    function MoveToFirst(): TNodeLink; virtual;
    function MoveToLast(): TNodeLink; virtual;

    function GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
      virtual;
    function GetNodeStaticText(AColumn: integer; AColumnLink: TColumnLink): string;
      virtual;
    procedure CreateChildrenNodes(ANodeCreator: TFunNodeLink); virtual;
    function CreateSubTreeNode(ASubLink: TNodeLink): TNodeLink;
    procedure CreateColumns(AColumnLinkList: TColumnLink); virtual;
    (*
      there are 2 version of iterator,
      1. recursive iterator
      2. recursive iterator with a predicate
    *)
    procedure IterateChildrenLinks(AGetter: TGetNodeLink;
      ARecursive: boolean); overload;
    function IterateChildrenLinks(AGetter: TPredicateNodeLink;
      ARecursive: boolean = True): boolean; overload;
    procedure Update(AAction: TProc);
    function Rebuild(ADepth: integer = -1): TNodeLink;
    procedure BuildTree(ADepth: integer = -1);
    procedure BuildColumn();
    procedure Clear();
  end;

  TColumnLink = class(TObject)
  private
    FOnAfterCreateColumn: TProc<TColumnLink>;
    FOnClickColumn: TProc<TColumnLink>;
    FOnClickColumnButton: TProc<TColumnLink>;
    FOnClickColumnIcon: TProc<TColumnLink>;
    FWidth: integer;
    FEditing: boolean;
    FColumnLinkClass: TColumnLinkClass;
    FAlignment: TAlignment;
    FCaptionAlignment: TAlignment;
  protected
    FColumnLinks: TObjectList<TColumnLink>;
    FParentColumnLink: TColumnLink;
    FData: TObject;
    FCaption: string;
    FTreeHeader: TVTHeader;
    FTreeColumn: TVirtualTreeColumn;
    function GetColumnLinks: TObjectList<TColumnLink>;
    procedure SetCaption(const Value: string);
    function GetColumnLink(AIndex: integer): TColumnLink; virtual;
    procedure ClickColumn();
    procedure ClickColumnButton();
    procedure ClickColumnIcon();
  public
    property ColumnLink[AIndex: integer]: TColumnLink read GetColumnLink; default;
    function Count(): integer;
    procedure Clear();
    property ColumnLinkClass: TColumnLinkClass
      read FColumnLinkClass write FColumnLinkClass;
    property Data: TObject read FData write FData;
    property Caption: string read FCaption write SetCaption;
    property Width: integer read FWidth write FWidth;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property CaptionAlignment: TAlignment read FCaptionAlignment
      write FCaptionAlignment;
    property Editing: boolean read FEditing write FEditing;
    property TreeHeader: TVTHeader read FTreeHeader;
    property TreeColumn: TVirtualTreeColumn read FTreeColumn;
    property OnAfterCreateColumn: TProc<TColumnLink>
      read FOnAfterCreateColumn write FOnAfterCreateColumn;
    property OnClickColumn: TProc<TColumnLink> read FOnClickColumn write FOnClickColumn;
    property OnClickColumnButton: TProc<TColumnLink>
      read FOnClickColumnButton write FOnClickColumnButton;
    property OnClickColumnIcon: TProc<TColumnLink>
      read FOnClickColumnIcon write FOnClickColumnIcon;

    constructor Create(AParentLink: TColumnLink);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function AddLink(const ACaption: string; AData: TObject = nil): TColumnLink;
    procedure IterateColumnLink(AGetter: TProc<TColumnLink>;
      ARecursive: boolean); overload;
    procedure IterateColumnLink(AGetter: TProc<TColumnLink, integer>); overload;
  end;

  TTypeLink<T> = class(TNodeLink)
  private
    FData: T;
  public
    property Data: T read FData write FData;
    procedure IterateChildrenObject(AGetter: TProc<T>; ARecursive: boolean);
      virtual; abstract;
    constructor Create(AParentLink: TNodeLink; const AData: T); virtual;

  end;

  TObjectLink = class(TTypeLink<TObject>)
  public
    constructor Create(AParentLink: TNodeLink; const AData: TObject); override;
    function GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
      override;
    procedure CreateChildrenNodes(ANodeCreator: TFunNodeLink); override;
    procedure IterateChildrenObject(AGetter: TProc<TObject>;
      ARecursive: boolean); override;
    function MoveDown: TNodeLink; override;
    function MoveUp: TNodeLink; override;
  end;

  TPersistentLink = class(TObjectLink)

  end;

  TComponentLink = class(TPersistentLink)
  public
    procedure IterateChildrenObject(AGetter: TProc<TObject>;
      ARecursive: boolean); override;
    function GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
      override;
  end;

  TComponentAndPropsLink = class(TComponentLink)
  public
    procedure IterateChildrenObject(AGetter: TProc<TObject>;
      ARecursive: boolean); override;
  end;

  TWinControlLink = class(TComponentLink)
  public
    procedure IterateChildrenObject(AGetter: TProc<TObject>;
      ARecursive: boolean); override;
  end;

  TCollectionLink = class(TPersistentLink)
  protected
    FPropName: string;
  public
    procedure IterateChildrenObject(AGetter: TProc<TObject>;
      ARecursive: boolean); override;
    function GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
      override;
  end;

  TCollectionItemLink = class(TPersistentLink)

  public
    function GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
      override;
  end;

  TInterfaceLinkClass = class of TInterfaceLink;

  TInterfaceLink = class(TTypeLink<IInterface>)
  public
    destructor Destroy; override;
  end;

  (*
    RegisterLink is to bind class type or interface id to link type, and use class or interface id to unregister.
    If binding link class, FindLinkClass will search the best match link according to giving.
  *)
procedure RegisterLink(AClass: TClass; ALinkerClass: TObjectLinkClass);
  overload;
procedure UnregisterLink(AClasses: array of TClass); overload;
function FindLinkClass(AClass: TClass;
  ADefaultLinkClass: TObjectLinkClass): TObjectLinkClass; overload;

procedure RegisterLink(AIntf: TGUID; ALinkerClass: TInterfaceLinkClass); overload;
procedure UnregisterLink(AIntfs: array of TGUID); overload;
function FindLinkClass(AGuid: TGUID;
  ADefaultLinkClass: TInterfaceLinkClass): TInterfaceLinkClass; overload;

procedure StretchImageList(AImages: TCustomImageList; AWidth, AHeight: integer);

implementation

{$R *.dfm}

var
  ClassRegister: TDictionary<TClass, TObjectLinkClass>;
  InterfaceRegister: TDictionary<TGUID, TInterfaceLinkClass>;

const
  MaskBackground: array [boolean] of TColor = (clWhite, clBlack);
  crColorPick = -100;

procedure StretchReplace(List: TCustomImageList; Index: integer;
  Image, Mask: TBitmap; MaskColor: TColor = clDefault);
var
  NewImage, NewMask: TBitmap;
begin
  NewImage := TBitmap.Create;
  try
    NewImage.Assign(Image);
    NewImage.SetSize(List.Width, List.Height);
    NewImage.Canvas.Brush.Color := clBlack;
    Image.TransparentColor := clnone;
    NewImage.Canvas.FillRect(Rect(0, 0, List.Width, List.Height));
    NewImage.Canvas.StretchDraw(Rect(0, 0, List.Width, List.Height), Image);
    if MaskColor <> clDefault then
      NewImage.TransparentColor := MaskColor;
    if Mask <> nil then
    begin
      NewMask := TBitmap.Create;
      try
        NewMask.SetSize(List.Width, List.Height);
        NewMask.Canvas.Brush.Color := MaskBackground[Mask = nil];
        NewMask.Canvas.FillRect(Rect(0, 0, List.Width, List.Height));
        NewMask.Canvas.StretchDraw(Rect(0, 0, List.Width, List.Height), Mask);
        List.Replace(Index, NewImage, NewMask);
      finally
        NewMask.Free;
      end;
    end
    else if MaskColor <> clDefault then
      List.ReplaceMasked(Index, NewImage, MaskColor)
    else
      List.Replace(Index, NewImage, nil);
  finally
    NewImage.Free;
  end;
end;

procedure GetImages(ImageList: TImageList; Index: integer; Image, Mask: TBitmap);
var
  R: TRect;
begin
  R := Rect(0, 0, ImageList.Width, ImageList.Height);
  Image.PixelFormat := pf32bit;
  Image.AlphaFormat := afIgnored;
  Image.SetSize(ImageList.Width, ImageList.Height);
  Mask.PixelFormat := pf1bit;
  Mask.SetSize(ImageList.Width, ImageList.Height);

  Image.Canvas.Brush.Color := clBlack;
  Image.Canvas.FillRect(R);
  ImageList_Draw(ImageList.Handle, Index, Image.Canvas.Handle, 0, 0,
    ILD_NORMAL);

  Mask.Canvas.Brush.Color := clWhite;
  Mask.Canvas.FillRect(R);
  ImageList_Draw(ImageList.Handle, Index, Mask.Canvas.Handle, 0, 0, ILD_MASK);
end;

procedure StretchImageList(AImages: TCustomImageList; AWidth, AHeight: integer);
var
  I: integer;
  LImages: TImageList;
  Image, Mask: TBitmap;
begin
  LImages := TImageList.CreateSize(AImages.Width, AImages.Height);
  try
    LImages.Assign(AImages);
    AImages.SetSize(AWidth, AHeight);

    Image := TBitmap.Create;
    Mask := TBitmap.Create;
    try
      for I := 0 to LImages.Count - 1 do
      begin
        GetImages(LImages, I, Image, Mask);
        AImages.Add(nil, nil);
        StretchReplace(AImages, I, Image, Mask);
      end;
    finally
      Mask.Free;
      Image.Free;
    end;
  finally
    LImages.Free;
  end;
end;

function GetClassRegister: TDictionary<TClass, TObjectLinkClass>;
begin
  if not Assigned(ClassRegister) then
    ClassRegister := TDictionary<TClass, TObjectLinkClass>.Create(10);
  Result := ClassRegister;
end;

function GetInterfaceRegister: TDictionary<TGUID, TInterfaceLinkClass>;
begin
  if not Assigned(InterfaceRegister) then
    InterfaceRegister := TDictionary<TGUID, TInterfaceLinkClass>.Create(10);
  Result := InterfaceRegister;
end;

procedure RegisterLink(AClass: TClass; ALinkerClass: TObjectLinkClass);
  overload;
begin
  GetClassRegister.AddOrSetValue(AClass, ALinkerClass);
end;

procedure RegisterLink(AIntf: TGUID; ALinkerClass: TInterfaceLinkClass); overload;
begin
  GetInterfaceRegister.AddOrSetValue(AIntf, ALinkerClass);
end;

procedure UnregisterLink(AClasses: array of TClass);
var
  I: integer;
begin
  if Assigned(ClassRegister) then
    for I := Low(AClasses) to High(AClasses) do
      ClassRegister.Remove(AClasses[I]);
end;

procedure UnregisterLink(AIntfs: array of TGUID);
var
  I: integer;
begin
  if Assigned(InterfaceRegister) then
    for I := Low(AIntfs) to High(AIntfs) do
      InterfaceRegister.Remove(AIntfs[I]);
end;

function FindLinkClass(AClass: TClass;
  ADefaultLinkClass: TObjectLinkClass): TObjectLinkClass;
var
  L: TPair<TClass, TObjectLinkClass>;
  GCount, C: integer;
begin
  Result := ADefaultLinkClass;
  GCount := -1;
  for L in GetClassRegister do
  begin
    C := CountGenerations(L.Key, AClass);
    case C of
      0:
      begin
        Result := L.Value;
        Exit;
      end;
      -1:
      begin

      end;
      else
        if (GCount = -1) or (GCount > C) then
        begin
          GCount := C;
          Result := L.Value;
        end;
    end;
  end;
end;

function FindLinkClass(AGuid: TGUID;
  ADefaultLinkClass: TInterfaceLinkClass): TInterfaceLinkClass;
begin
  if not GetInterfaceRegister.TryGetValue(AGuid, Result) then
    Result := ADefaultLinkClass;
end;

{ TNodeLink }

procedure TNodeLink.AfterConstruction;
begin
  inherited;
  FImageIndex := -1;
end;

procedure TNodeLink.BeforeDestruction;
begin
  inherited;
  if Assigned(FChildrenLinkList) then
  begin
    Clear;
    FreeAndNil(FChildrenLinkList);
  end;
  if Assigned(FTree) and Assigned(FNode) then
  begin
    FTree.DeleteNode(FNode);
    Node := nil;
  end;
  ParentLink := nil;
  Editor := nil;
  if Assigned(FNexus) then
    FreeAndNil(FNexus);
end;

function TNodeLink.BeginEdit: boolean;
begin
  Result := True;
  if Assigned(FEditor) then
  begin
    FEditor.Show;
    FEditor.SetFocus;
  end;
end;

function TNodeLink.BuildChildrenCondition: boolean;
begin
  Result := ChildrenLinkList.Count = 0;
end;

procedure TNodeLink.BuildColumn;
begin
  CreateColumns(ColumnLinkList);
  if Assigned(FColumnLinkList) and Assigned(FTree) and
    (TVTHeaderOption.hoVisible in FTree.Header.Options) then
  begin
    FTree.Header.Columns.Clear;
    FColumnLinkList.IterateColumnLink(
      procedure (C: TColumnLink; I: integer)
      begin
        with C do
        begin
          FTreeHeader := Self.FTree.Header;
          FTreeColumn := Self.FTree.Header.Columns.Add;
          FTreeColumn.Text := Caption;
          FTreeColumn.Tag := I;
          FTreeColumn.CaptionAlignment := CaptionAlignment;
          FTreeColumn.Alignment := Alignment;
          if Width > 0 then
            FTreeColumn.Width := Width;
        end;
        if Assigned(C.FOnAfterCreateColumn) then
          C.FOnAfterCreateColumn(C);
      end);
  end;
end;

procedure TNodeLink.BuildTree(ADepth: integer);
begin
  if Assigned(FTree) and (ADepth <> 0) then
  begin
    if BuildChildrenCondition then
      CreateChildrenNodes(CreateSubTreeNode);

    if ADepth > 0 then
      Dec(ADepth);

    IterateChildrenLinks(
      procedure (L: TNodeLink)
      begin
        L.BuildTree(ADepth);
      end, False);
  end;
end;

function TNodeLink.CancelEdit: boolean;
begin
  Result := True;
  if Assigned(FEditor) then
  begin
    FEditor.Hide;
  end;
end;

procedure TNodeLink.Clear;
begin
  // if Assigned(FChildrenLinkList) then
  // begin
  // IterateChildrenLinks(
  // procedure(N: TNodeLink)
  // begin
  // N.FParentLink := nil;
  // N.FNode := nil;
  // end, True);
  // FChildrenLinkList.Clear;
  // end;
  // if Assigned(FTree) and Assigned(FNode) then
  // FTree.DeleteChildren(FNode);
  if Assigned(FChildrenLinkList) then
    Update(
      procedure ()
      begin
        while FChildrenLinkList.Count > 0 do
        begin
          // FChildrenLinkList.Last.FParentLink:= nil;
          FChildrenLinkList.Last.Free;
        end;
      end);
end;

constructor TNodeLink.Create(AParentLink: TNodeLink);
begin
  ParentLink := AParentLink;
end;

procedure TNodeLink.CreateChildrenNodes(ANodeCreator: TFunNodeLink);
begin

end;

procedure TNodeLink.CreateColumns(AColumnLinkList: TColumnLink);
begin

end;

function TNodeLink.CreateEditor(AColumn: integer;
  AColumnLink: TColumnLink): TWinControl;
begin
  Result := nil;
end;

function TNodeLink.CreateSubTreeNode(ASubLink: TNodeLink): TNodeLink;
begin
  if ASubLink.ParentLink <> Self then
    ASubLink.ParentLink := Self;
  with ASubLink do
  begin
    FTree := Self.FTree;
    if not Assigned(FNode) then
      Node := Self.FTree.AddChild(Self.FNode, ASubLink);
  end;
  Result := ASubLink;
end;

procedure TNodeLink.EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  CanAdvance: boolean;

begin
  CanAdvance := True;

  case Key of
    VK_ESCAPE:
    begin
      Key := 0; // ESC will be handled in EditKeyUp()
    end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;

    VK_UP, VK_DOWN:
    begin
      // Consider special cases before finishing edit mode.
      CanAdvance := Shift = [];
      if FEditor is TComboBox then
        CanAdvance := CanAdvance and not TComboBox(FEditor).DroppedDown;
      if FEditor is TDateTimePicker then
        CanAdvance := CanAdvance and not TDateTimePicker(FEditor).DroppedDown;

      if CanAdvance then
      begin
        // Forward the keypress to the tree. It will asynchronously change the focused node.
        PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    end;
  end;
end;

procedure TNodeLink.EditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
    begin
      FTree.CancelEditNode;
      Key := 0;
    end;
  end;
end;

function TNodeLink.EndEdit: boolean;
begin
  Result := True;
  if Assigned(FEditor) then
  begin
    Result := GetEditorValue(FEditor, FActivedColumn, FActivedColumnLink);
    if Result then
    begin
      FEditor.Hide;
      Tree.SetFocus;
    end;
  end;
end;

function TNodeLink.GetBounds: TRect;
begin
  if not Assigned(FEditor) then
    Exit;
  Result := FEditor.BoundsRect;
end;

function TNodeLink.GetChildrenLinkList: TObjectList<TNodeLink>;
begin
  if not Assigned(FChildrenLinkList) then
    FChildrenLinkList := TObjectList<TNodeLink>.Create(True);
  Result := FChildrenLinkList;
end;

function TNodeLink.GetColumnLinkList: TColumnLink;
begin
  if not Assigned(FColumnLinkList) then
  begin
    FColumnLinkList := TColumnLink.Create(nil);
    if Assigned(FTree) then
      FColumnLinkList.FTreeHeader := FTree.Header;
  end;
  Result := FColumnLinkList;
end;

function TNodeLink.GetEditorValue(AEditor: TWinControl;
  AColumn: integer; AColumnLink: TColumnLink): boolean;
begin
  Result := True;
end;

function TNodeLink.GetExpanded(ARecursive: boolean): boolean;
begin
  Result := Assigned(FTree) and Assigned(FNode) and FTree.Expanded[FNode];
end;

function TNodeLink.GetNodeStaticText(AColumn: integer; AColumnLink: TColumnLink): string;
begin
  Result := '';
end;

function TNodeLink.GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
begin
  Result := '';
end;

function TNodeLink.GetSelected: boolean;
begin
  Result := Assigned(FTree) and Assigned(FNode) and FTree.Selected[FNode];
end;

function TNodeLink.IterateChildrenLinks(AGetter: TPredicate<TNodeLink>;
  ARecursive: boolean): boolean;
var
  I: integer;
begin
  Result := False;
  if Assigned(FChildrenLinkList) then
    for I := 0 to FChildrenLinkList.Count - 1 do
      if AGetter(FChildrenLinkList[I]) or (ARecursive and
        FChildrenLinkList[I].IterateChildrenLinks(AGetter)) then
        Exit(True);
end;

function TNodeLink.MoveDown: TNodeLink;
var
  I: integer;
begin
  Result := Self;
  if Assigned(ParentLink) then
  begin
    I := ParentLink.ChildrenLinkList.IndexOf(Self);
    Inc(I);
    if I < ParentLink.ChildrenLinkList.Count then
      MoveTo(ParentLink.ChildrenLinkList[I], mkInsertAfter);
  end;
end;

function TNodeLink.MoveTo(ATargetLink: TNodeLink; AMoveKind: TMoveKind): boolean;
var
  I: integer;
begin
  Result := Assigned(FTree) and Assigned(FNode) and Assigned(ATargetLink) and
    (ATargetLink <> Self);

  if Result then
    Update(
      procedure ()
      begin
        case AMoveKind of
          mkInsertBefore:
          begin
            if Assigned(ATargetLink.ParentLink) then
            begin
              if Assigned(FParentLink) then
                FParentLink.ChildrenLinkList.Extract(Self);
              FParentLink := ATargetLink.ParentLink;
              I := FParentLink.ChildrenLinkList.IndexOf(ATargetLink);
              FParentLink.ChildrenLinkList.Insert(I, Self);
              FTree.MoveTo(Self.FNode, ATargetLink.FNode,
              amInsertBefore, False);
            end;
          end;
          mkInsertAfter:
          begin
            if Assigned(ATargetLink.ParentLink) then
            begin
              if Assigned(FParentLink) then
                FParentLink.ChildrenLinkList.Extract(Self);
              FParentLink := ATargetLink.ParentLink;
              I := FParentLink.ChildrenLinkList.IndexOf(ATargetLink);
              FParentLink.ChildrenLinkList.Insert(I + 1, Self);
              FTree.MoveTo(Self.FNode, ATargetLink.FNode,
              amInsertAfter, False);
            end;
          end;
          mkAddChildFirst:
            if ParentLink <> ATargetLink then
            begin
              if Assigned(FParentLink) then
                FParentLink.ChildrenLinkList.Extract(Self);
              FParentLink := ATargetLink;
              FParentLink.ChildrenLinkList.Insert(0, Self);
              FTree.MoveTo(Self.FNode, ATargetLink.FNode,
              amAddChildFirst, False);
            end;
          mkAddChildLast:
            if ParentLink <> ATargetLink then
            begin
              ParentLink := ATargetLink;
              FTree.MoveTo(Self.FNode, ATargetLink.FNode,
              amAddChildLast, False);
            end;
        end;
        if Assigned(FParentLink) then
          FTree.InvalidateChildren(FParentLink.FNode, True);
      end);
end;

function TNodeLink.MoveToFirst: TNodeLink;
begin
  Result := Self;
  if Assigned(ParentLink) then
    MoveTo(ParentLink, TMoveKind.mkAddChildFirst);
end;

function TNodeLink.MoveToLast: TNodeLink;
begin
  Result := Self;
  if Assigned(ParentLink) then
    MoveTo(ParentLink, TMoveKind.mkAddChildLast);
end;

function TNodeLink.MoveUp: TNodeLink;
var
  I: integer;
begin
  Result := Self;
  if Assigned(ParentLink) then
  begin
    I := ParentLink.ChildrenLinkList.IndexOf(Self);
    Dec(I);
    if I >= 0 then
      MoveTo(ParentLink.ChildrenLinkList[I], mkInsertBefore);
  end;
end;

procedure TNodeLink.IterateChildrenLinks(AGetter: TProc<TNodeLink>; ARecursive: boolean);
var
  I: integer;
begin
  if Assigned(FChildrenLinkList) then
    for I := 0 to FChildrenLinkList.Count - 1 do
    begin
      AGetter(FChildrenLinkList[I]);
      if ARecursive then
        FChildrenLinkList[I].IterateChildrenLinks(AGetter, ARecursive);
    end;
end;

function TNodeLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean;
begin
  FActivedColumn := Column;

  Editor := CreateEditor(FActivedColumn, FActivedColumnLink);
  Result := Assigned(FEditor);
  if Result then
  begin
    FEditor.Parent := FTree;
    SetEditorValue(FEditor, FActivedColumn, FActivedColumnLink);
  end;
end;

procedure TNodeLink.ProcessMessage(var Message: TMessage);
begin
  if Assigned(FEditor) then
    FEditor.WindowProc(Message);
end;

function TNodeLink.Rebuild(ADepth: integer): TNodeLink;
begin
  Result := Self;
  Update(
    procedure ()
    begin
      ChildrenLinkList.Clear;
      BuildTree(ADepth);
    end);
end;

procedure TNodeLink.SetBounds(R: TRect);
var
  Dummy: integer;
begin
  if not Assigned(FEditor) then
    Exit;
  Tree.Header.Columns.GetColumnBounds(FActivedColumn, Dummy, R.Right);
  FEditor.BoundsRect := R;
  FEditor.Height := R.Height + 2;
end;

procedure TNodeLink.SetEditor(const Value: TWinControl);
begin
  if FEditor = Value then
    Exit;

  if Assigned(FEditor) then
  begin
    if Assigned(FNexus) then
      FEditor.RemoveFreeNotification(FNexus);
    FEditor.Free;
  end;

  FEditor := Value;

  if Assigned(FEditor) then
  begin
    if not Assigned(FNexus) then
    begin
      FNexus := TNexus.Create(nil);
      FNexus.OnFreeNotify := procedure (C: TComponent)
        begin
          if C = Self.FEditor then
            Editor := nil;
        end;

    end;
    FEditor.FreeNotification(FNexus);
  end;
end;

function TNodeLink.SetEditorValue(AEditor: TWinControl;
  AColumn: integer; AColumnLink: TColumnLink): boolean;
begin
  Result := True;
end;

procedure TNodeLink.SetExpanded(ARecursive: boolean; const Value: boolean);
begin
  if Assigned(FTree) and Assigned(FNode) then
    with FTree do
      try
        FTree.BeginUpdate;
        if ARecursive then
          case Value of
            True:
              FTree.FullExpand(FNode);
            False:
              FTree.FullCollapse(FNode);
          end
        else
          FTree.Expanded[FNode] := Value;
      finally
        FTree.EndUpdate;
      end;
end;

procedure TNodeLink.SetFocus(ASelect: boolean);
begin
  if Assigned(FTree) and Assigned(FNode) then
  begin
    FTree.FocusedNode := FNode;
    if ASelect then
      SetSelected(ASelect);
  end;
end;

procedure TNodeLink.SetImageIndex(const Value: integer);
begin
  FImageIndex := Value;
end;

procedure TNodeLink.SetNode(const Value: PVirtualNode);
begin
  if FNode = Value then
    Exit;
  FNode := Value;
end;

procedure TNodeLink.SetParentLink(AValue: TNodeLink);
begin
  if AValue = FParentLink then
    Exit;
  if Assigned(FParentLink) then
    FParentLink.ChildrenLinkList.ExtractItem(Self, TDirection.FromEnd);
  FParentLink := AValue;
  if Assigned(FParentLink) then
    FParentLink.ChildrenLinkList.Add(Self);
end;

procedure TNodeLink.SetSelected(const Value: boolean);
begin
  if Assigned(FTree) and Assigned(FNode) then
    FTree.Selected[FNode] := Value;
end;

procedure TNodeLink.Update(AAction: TProc);
begin
  if Assigned(FTree) then
    with FTree do
      try
        BeginUpdate;
        AAction();
      finally
        EndUpdate;
      end
  else
    AAction();
end;

{ TFrameObjectTree }

procedure TFrameObjectTree.AfterConstruction;
begin
  inherited;
  StretchVTImages;
  FOwnedRootObject := False;
  FExpandDepth := -1;
  FShowRoot := True;
  FSelectedLinkList := TObjectList<TNodeLink>.Create(False);
end;

procedure TFrameObjectTree.BeforeDestruction;
begin
  SetRootObject(nil, nil);
  FreeAndNil(FSelectedLinkList);
  inherited;

end;

function TFrameObjectTree.GetNodeLink(AGetter: TProc<TNodeLink>; ANode: PVirtualNode;
  ATree: TVirtualStringTree; ALinkClass: TNodeLinkClass): boolean;
var
  L: TNodeLink;
begin
  L := GetNodeLink(ANode, ATree);
  Result := Assigned(L);
  if Result and Assigned(AGetter) and ((ALinkClass = nil) or
    L.InheritsFrom(ALinkClass)) then
    AGetter(L);
end;

function TFrameObjectTree.GetNodeLink(ANode: PVirtualNode;
  ATree: TVirtualStringTree): TNodeLink;
begin
  Result := nil;
  if not Assigned(ATree) then
    ATree := VTree;
  if Assigned(ANode) and (ATree.GetNodeData(ANode) <> nil) and
    (TObject(ATree.GetNodeData(ANode)^) is TNodeLink) then
    Result := TNodeLink(ATree.GetNodeData(ANode)^);
end;

function TFrameObjectTree.GetRootObject: TObject;
begin
  Result := FRootObject;
end;

function TFrameObjectTree.GetSelectedObject
  (AGetter: TFunc<TNodeLink, TObject, boolean>): boolean;
begin
  Result := GetSelections(function (L: TNodeLink): boolean
    begin
      Result := (L is TObjectLink) and AGetter(L, TObjectLink(L).Data);
    end, TObjectLink);
end;

function TFrameObjectTree.GetSelectedObject: TObject;
var
  O: TObject;
begin
  O := nil;
  GetSelections(
    function (L: TNodeLink): boolean
    begin
      Result := L is TObjectLink;
      if Result then
        O := TObjectLink(L).Data;
    end, TObjectLink);
  Result := O;
end;

function TFrameObjectTree.GetSelections(AGetter: TPredicate<TNodeLink>;
  ALinkClass: TNodeLinkClass): boolean;
var
  I: integer;
begin
  Result := False;
  if ALinkClass = nil then
    ALinkClass := TNodeLink;

  with FSelectedLinkList do
    for I := 0 to Count - 1 do
      if (Items[I] is ALinkClass) and AGetter(Items[I]) then
        Exit(True);
end;

function TFrameObjectTree.IterateLinks(AGetter: TPredicate<TNodeLink>;
  ALinkClass: TNodeLinkClass): boolean;
begin
  Result := IterateLinks(function (L: TNodeLink): boolean
    begin
      Result := L.InheritsFrom(ALinkClass) and AGetter(L);
    end);
end;

function TFrameObjectTree.IterateObjects
  (AGetter: TFunc<TObjectLink, TObject, boolean>): boolean;
begin
  Result := IterateLinks(function (L: TNodeLink): boolean
    begin
      Result := AGetter(TObjectLink(L), TObjectLink(L).Data);
    end, TObjectLink);
end;

function TFrameObjectTree.IterateLinks(AGetter: TPredicate<TNodeLink>): boolean;
begin
  Result := Assigned(FRootLink) and AGetter(FRootLink);
  if (not Result) and Assigned(FRootLink) then
    Result := FRootLink.IterateChildrenLinks(AGetter);
end;

procedure TFrameObjectTree.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  case Operation of
    opInsert:
      ;
    opRemove:
      if FRootObject = AComponent then
        SetRootObject(nil);
  end;
end;

function TFrameObjectTree.Rebuild(): TNodeLink;
begin
  if Assigned(FRootLink) then
    UpdateTree(
      procedure ()
      begin
        with FRootLink do
        begin
          ColumnLinkList.Clear;
          Clear;
          BuildColumn;
          BuildTree(FExpandDepth);
        end;
      end);
  Result := FRootLink;
end;

function TFrameObjectTree.SetRootObject(ARoot: TObject;
  ALinkClass: TObjectLinkClass): TNodeLink;
begin
  SetRootLink(nil);
  if Assigned(FRootObject) then
  begin
    if FRootObject is TComponent then
      TComponent(FRootObject).RemoveFreeNotification(Self);
    if FOwnedRootObject then
      FreeAndNil(FRootObject);
  end;

  FRootObject := ARoot;
  if Assigned(FRootObject) then
  begin
    if FRootObject is TComponent then
      TComponent(FRootObject).FreeNotification(Self);
    if Assigned(ALinkClass) then
      SetRootLink(ALinkClass.Create(nil, FRootObject))
    else
      SetRootLink(FindLinkClass(FRootObject.ClassType, TObjectLink).Create(nil,
        FRootObject));
  end;
  Result := RootLink;
end;

procedure TFrameObjectTree.StretchVTImages;
var
  LScale: extended;
  LWidth, LHeight: integer;
begin
  LScale := Screen.PixelsPerInch / 96;
  if LScale >= 1.5 then
    with VTree do
    begin
      LWidth := Round(16 * LScale);
      LHeight := Round(16 * LScale);
      if Assigned(CheckImages) and (CheckImages.Width < LWidth) or
        (CheckImages.Height < LHeight) then
        StretchImageList(CheckImages, LWidth, LHeight);
    end;
end;

function TFrameObjectTree.SelectedCount: integer;
begin
  if Assigned(FSelectedLinkList) then
    Result := FSelectedLinkList.Count
  else
    Result := 0;
end;

function TFrameObjectTree.SetRootLink(const Value: TNodeLink): TNodeLink;
begin
  Result := Value;
  if Value = FRootLink then
    Exit;
  UpdateTree(
    procedure ()
    begin
      if Assigned(FRootLink) then
      begin
        FreeAndNil(FRootLink);
      end;
      with Self.VTree do
      begin
        Clear;
        Header.Columns.Clear;
      end;
      FRootLink := Value;
      if Assigned(FRootLink) then
        with FRootLink do
        begin
          FTree := Self.VTree;
          if Self.ShowRoot then
            Node := FTree.AddChild(nil, Self.FRootLink)
          else
            Node := nil;
          BuildColumn;
          BuildTree(FExpandDepth);
        end;
    end);
end;

procedure TFrameObjectTree.UpdateTree(AAction: TProc);
begin
  if Assigned(AAction) then
    with VTree do
      try
        BeginUpdate;
        AAction();
      finally
        EndUpdate;
      end;
end;

procedure TFrameObjectTree.VTreeAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  L: TNodeLink;
begin
  L := GetNodeLink(Node);
  if Assigned(FSelectedLinkList) and Assigned(L) and
    (FSelectedLinkList.IndexOf(L) < 0) then
    FSelectedLinkList.Add(L);

  if Assigned(FOnAddToSelection) then
    FOnAddToSelection(L);
end;

procedure TFrameObjectTree.VTreeCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  S1, S2: string;
begin
  S1 := '';
  S2 := '';
  Self.VTree.OnGetText(Sender, Node1, Column, ttNormal, S1);
  Self.VTree.OnGetText(Sender, Node2, Column, ttNormal, S2);
  Result := CompareText(S1, S2);
end;

procedure TFrameObjectTree.VTreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; out EditLink: IVTEditLink);
var
  LLink: TNodeLink;
begin
  if Assigned(RootLink) then
    with RootLink.ColumnLinkList do
      if (Count > 0) and (Column < Count) then
      begin
        LLink := TNodeLink(Sender.GetNodeData(Node)^);
        LLink.FActivedColumn := Column;
        LLink.FActivedColumnLink := ColumnLink[Column];
        EditLink := LLink;
      end;
end;

procedure TFrameObjectTree.VTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: boolean);
begin
  if Assigned(RootLink) then
    with RootLink.ColumnLinkList do
      if (Count > 0) and (Column < Count) then
        Allowed := Allowed and ColumnLink[Column].Editing;
end;

procedure TFrameObjectTree.VTreeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if ExpandDepth > 0 then
    try
      Sender.BeginUpdate;
      TNodeLink(Sender.GetNodeData(Node)^).BuildTree(2);
    finally
      Sender.EndUpdate;
    end;
end;

procedure TFrameObjectTree.VTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: integer);
begin
  NodeDataSize := sizeof(TNodeLink);
end;

procedure TFrameObjectTree.VTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  case TextType of
    ttNormal:
      CellText := TNodeLink(Sender.GetNodeData(Node)^)
        .GetNodeText(Column, FRootLink.ColumnLinkList[Column]);
    ttStatic:
      CellText := TNodeLink(Sender.GetNodeData(Node)^).GetNodeStaticText(Column,
        FRootLink.ColumnLinkList[Column]);
  end;
end;

procedure TFrameObjectTree.VTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if (HitInfo.Column >= 0) and (HitInfo.Column < Sender.Columns.Count) and
    Assigned(FRootLink) then
    with FRootLink.ColumnLinkList[Sender.Columns[HitInfo.Column].Tag] do
      if hhiOnCheckbox in HitInfo.HitPosition then
        ClickColumnButton
      else if hhiOnIcon in HitInfo.HitPosition then
        ClickColumnIcon
      else if hhiOnColumn in HitInfo.HitPosition then
        ClickColumn;
end;

procedure TFrameObjectTree.VTreeRemoveFromSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(FOnRemoveFromSelection) then
    FOnRemoveFromSelection(GetNodeLink(Node));
  if Assigned(FSelectedLinkList) then
    FSelectedLinkList.Remove(GetNodeLink(Node));
end;

{ TObjectLink }

constructor TObjectLink.Create(AParentLink: TNodeLink; const AData: TObject);
begin
  inherited Create(AParentLink, AData);
end;

procedure TObjectLink.CreateChildrenNodes(ANodeCreator: TFunNodeLink);
begin
  IterateChildrenObject(
    procedure (O: TObject)
    begin
      ANodeCreator(FindLinkClass(O.ClassType, TObjectLink).Create(Self, O));
    end, False);
end;

function TObjectLink.GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
begin
  Result := FData.ClassName;
end;

procedure TObjectLink.IterateChildrenObject(AGetter: TProc<TObject>; ARecursive: boolean);
begin

end;

function TObjectLink.MoveDown: TNodeLink;
begin
  Result := Self;
  Update(
    procedure ()
    begin
      if Data is TComponent then
        with Data as TComponent do
        begin
          ComponentIndex := ComponentIndex + 1;
          inherited MoveDown;
        end
      else if Data is TCollectionItem then
        with Data as TCollectionItem do
        begin
          Index := Index + 1;
          inherited MoveDown;
        end;
    end);
end;

function TObjectLink.MoveUp: TNodeLink;
begin
  Result := Self;
  Update(
    procedure ()
    begin
      if Data is TComponent then
        with Data as TComponent do
        begin
          ComponentIndex := ComponentIndex - 1;
          inherited MoveUp;
        end
      else if Data is TCollectionItem then
        with Data as TCollectionItem do
        begin
          Index := Index - 1;
          inherited MoveUp;
        end;
    end);
end;

{ TComponentLink }

function TComponentLink.GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
begin
  Result := (FData as TComponent).Name;
end;

procedure TComponentLink.IterateChildrenObject(AGetter: TProc<TObject>;
  ARecursive: boolean);
begin
  IterateComponent(FData as TComponent,
    procedure (O: TComponent)
    begin
      AGetter(O);
    end, ARecursive);
end;

{ TColumnLink }

function TColumnLink.AddLink(const ACaption: string; AData: TObject): TColumnLink;
begin
  Result := FColumnLinkClass.Create(Self);
  with Result do
  begin
    FCaption := ACaption;
    FData := AData;
    FTreeHeader := Self.FTreeHeader;
  end;
  GetColumnLinks.Add(Result);
end;

procedure TColumnLink.AfterConstruction;
begin
  inherited;
  FWidth := -1;
  FColumnLinkClass := TColumnLink;
  FOnAfterCreateColumn := nil;
  FOnClickColumn := nil;
  FOnClickColumnButton := nil;
  FOnClickColumnIcon := nil;
end;

procedure TColumnLink.BeforeDestruction;
var
  I: integer;
begin
  inherited;
  if Assigned(FTreeHeader) and Assigned(FTreeColumn) then
    with FTreeHeader.Columns do
      for I := 0 to Count - 1 do
        if Items[I] = FTreeColumn then
        begin
          Delete(I);
          Break;
        end;

  if Assigned(FColumnLinks) then
    FreeAndNil(FColumnLinks);
end;

procedure TColumnLink.Clear;
begin
  if Assigned(FColumnLinks) then
    FColumnLinks.Clear;
end;

procedure TColumnLink.ClickColumn;
begin
  if Assigned(FOnClickColumn) then
    FOnClickColumn(Self);
end;

procedure TColumnLink.ClickColumnButton;
begin
  if Assigned(FOnClickColumnButton) then
    FOnClickColumnButton(Self);
end;

procedure TColumnLink.ClickColumnIcon;
begin
  if Assigned(FOnClickColumnIcon) then
    FOnClickColumnIcon(Self);
end;

function TColumnLink.Count: integer;
begin
  if Assigned(FColumnLinks) then
    Result := FColumnLinks.Count
  else
    Result := 0;
end;

constructor TColumnLink.Create(AParentLink: TColumnLink);
begin
  FParentColumnLink := AParentLink;
end;

function TColumnLink.GetColumnLink(AIndex: integer): TColumnLink;
begin
  if Assigned(FColumnLinks) and (AIndex >= 0) and (AIndex < FColumnLinks.Count) then
    Result := FColumnLinks[AIndex]
  else
    Result := nil;
end;

function TColumnLink.GetColumnLinks: TObjectList<TColumnLink>;
begin
  if not Assigned(FColumnLinks) then
    FColumnLinks := TObjectList<TColumnLink>.Create(True);
  Result := FColumnLinks;
end;

procedure TColumnLink.IterateColumnLink(AGetter: TProc<TColumnLink, integer>);
var
  I: integer;
begin
  if Assigned(FColumnLinks) then
    for I := 0 to FColumnLinks.Count - 1 do
      AGetter(FColumnLinks[I], I);
end;

procedure TColumnLink.IterateColumnLink(AGetter: TProc<TColumnLink>; ARecursive: boolean);
var
  I: integer;
begin
  if Assigned(FColumnLinks) then
    for I := 0 to FColumnLinks.Count - 1 do
    begin
      AGetter(FColumnLinks[I]);
      if ARecursive then
        FColumnLinks[I].IterateColumnLink(AGetter, ARecursive);
    end;
end;

procedure TColumnLink.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

{ TWinControlLink }

procedure TWinControlLink.IterateChildrenObject(AGetter: TProc<TObject>;
  ARecursive: boolean);
var
  I: integer;
begin
  with Data as TWinControl do
  begin
    for I := 0 to controlcount - 1 do
      AGetter(Controls[I]);
    for I := 0 to ComponentCount - 1 do
      if not (Components[I] is TControl) then
        AGetter(Components[I]);
  end;
end;

{ TCollectionLink }

function TCollectionLink.GetNodeText(AColumn: integer; AColumnLink: TColumnLink): string;
begin
  with Data as TCollection do
    if (Self.FPropName = '') and (Owner <> nil) then
      IterateProps(Owner, [tkClass],
        procedure (P: PPropInfo)
        begin
          if GetObjectProp(Owner, P) = Self.Data then
            FPropName := P^.Name;
        end);
  Result := FPropName;
end;

procedure TCollectionLink.IterateChildrenObject(AGetter: TProc<TObject>;
  ARecursive: boolean);
var
  I: integer;
begin
  with Data as TCollection do
    for I := 0 to Count - 1 do
      AGetter(Items[I]);
end;

{ TCollectionItemLink }

function TCollectionItemLink.GetNodeText(AColumn: integer;
  AColumnLink: TColumnLink): string;
begin
  Result := (Data as TCollectionItem).DisplayName;
end;

{ TTypeLink<T> }

constructor TTypeLink<T>.Create(AParentLink: TNodeLink; const AData: T);
begin
  inherited Create(AParentLink);
  FData := AData;
end;

{ TInterfaceLink }

destructor TInterfaceLink.Destroy;
begin

  inherited;
  FData := nil;
end;

{ TComponentAndPropsLink }

procedure TComponentAndPropsLink.IterateChildrenObject(AGetter: TProc<TObject>;
  ARecursive: boolean);
// var
// LProp: TRttiProperty;
begin
  IterateProps(Data, [tkClass],
    procedure (P: PPropInfo)
    begin
      AGetter(GetObjectProp(Data, P^.Name));
    end);
  // for LProp in RTTIContext.GetType(Data.ClassType).GetProperties do
  // if (LProp.Visibility = mvPublished) and
  // (LProp.PropertyType.TypeKind = tkClass) then
  // AGetter(GetObjectProp(Data, LProp.Name));
  inherited;

end;

initialization

  RegisterClasses([TFrameObjectTree]);

  RegisterLink(TObject, TObjectLink);
  RegisterLink(TPersistent, TPersistentLink);
  RegisterLink(TComponent, TComponentLink);
  RegisterLink(TWinControl, TWinControlLink);
  RegisterLink(TCollection, TCollectionLink);
  RegisterLink(TCollectionItem, TCollectionItemLink);

finalization

  UnRegisterClasses([TFrameObjectTree]);

  UnregisterLink([TObject, TPersistent, TComponent, TWinControl, TCollection,
    TCollectionItem]);

  if Assigned(ClassRegister) then
    FreeAndNil(ClassRegister);
  if Assigned(InterfaceRegister) then
    FreeAndNil(InterfaceRegister);

end.
