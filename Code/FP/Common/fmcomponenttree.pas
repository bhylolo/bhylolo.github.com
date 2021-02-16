unit fmComponentTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComponentTreeView, PropEdits, PropEditUtils;

type

  { TFrameComponentTree }

  TFrameComponentTree = class(TFrame)
  private
    FComponentTree: TComponentTreeView;
    function GetComponentTree: TComponentTreeView;
  protected
    procedure OnTreeHookSetSelection(const ASelection: TPersistentSelectionList);
    procedure OnLookupRootChanged();
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property ComponentTree: TComponentTreeView read GetComponentTree;
  end;

implementation

//type
//  TPersistentAccess = class(TPersistent);

{$R *.lfm}

function InternalGetLookupRoot(APersistent: TPersistent): TPersistent;
begin
  if APersistent = Application then
    Result := GlobalDesignHook.LookupRoot
  else
    Result := nil;
  //Result := APersistent;
  //while Assigned(Result) do
  //  if Result = GlobalDesignHook.LookupRoot then
  //    Break
  //  else if Result is TComponent then
  //    Result := (Result as TComponent).Owner
  //  else if Result is TCollection then
  //    Result := (Result as TCollection).Owner
  //  else if Result is TCollectionItem then
  //    Result := (Result as TCollectionItem).Collection
  //  else
  //    Result := TPersistentAccess(Result).GetOwner;
end;

{ TFrameComponentTree }

function TFrameComponentTree.GetComponentTree: TComponentTreeView;
begin
  if not Assigned(FComponentTree) then
  begin
    FComponentTree := TComponentTreeView.Create(Self);
    InsertControl(FComponentTree);
    FComponentTree.Name := 'ComponentTree';
    FComponentTree.Align := alClient;
    FComponentTree.PropertyEditorHook := GlobalDesignHook;

    if Assigned(GlobalDesignHook) then
      with GlobalDesignHook do
      begin
        //AddHandlerSetSelection(@OnTreeHookSetSelection);
        AddHandlerChangeLookupRoot(@OnLookupRootChanged);
      end;
  end;
  Result := FComponentTree;
end;

procedure TFrameComponentTree.OnTreeHookSetSelection(
  const ASelection: TPersistentSelectionList);
begin
  if Assigned(ComponentTree) then
    ComponentTree.Selection := ASelection;
end;

procedure TFrameComponentTree.OnLookupRootChanged();
begin
  if Assigned(ComponentTree) then
    ComponentTree.RebuildComponentNodes;
end;

procedure TFrameComponentTree.AfterConstruction;
begin
  inherited AfterConstruction;
  ComponentTree;
end;

procedure TFrameComponentTree.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  ComponentTree.PropertyEditorHook := nil;
end;

initialization
  //RegisterGetLookupRoot(@InternalGetLookupRoot);

finalization;
  //UnregisterGetLookupRoot(@InternalGetLookupRoot);
end.
