unit DockForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, DesignMenus,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DesignIntf, DesignerTypes;

type
  TDockableForm = class(TForm, IDesignNotification, IDesignWindow, IHostForm)
  private
    FDesigner: IDesigner;
  protected
    { IDesignNotification }
    procedure ItemDeleted(const ADesigner: IDesigner;
      AItem: TPersistent); virtual;
    procedure ItemInserted(const ADesigner: IDesigner;
      AItem: TPersistent); virtual;
    procedure ItemsModified(const ADesigner: IDesigner); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); virtual;
    procedure DesignerOpened(const ADesigner: IDesigner;
      AResurrecting: Boolean); virtual;
    procedure DesignerClosed(const ADesigner: IDesigner;
      AGoingDormant: Boolean); virtual;
    { IDesignWindow }
    procedure WindowHide;
    procedure WindowShow;
    { IHostForm }
    // procedure BringToFront;
    procedure CheckPosChanged;
    procedure DeinitializeDesigner(ARoot: TComponent);
    function GetCanPrint: Boolean;
    function GetCaption: string;
    function GetDesignerState: TDesignerState;
    function GetFont: TPersistent;
    function GetForm: TComponent;
    function GetFormImage: TObject;
    function GetScrollPos(Horiz: Boolean): Integer;
    function GetVisible: Boolean;
    function GetWindowState: TShowState;
    procedure HideWindow;
    function IsMenuKey(var Message: TWMKey): Boolean;
    procedure SetCaption(const ACaption: string);
    procedure SetDesigner(const ADesigner: IInterface);
    procedure SetDesigning(DesignMode: Boolean); overload;
    // procedure Show;
    procedure ShowWindow(AShowState: TShowState);
    procedure SetFormDefaults(ARoot: TComponent; const ARootName: string;
      X, Y: Integer; Scale: Boolean);
    procedure Unmodify;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // property Designer: IDesigner read FDesigner write SetDesigner;
  end;

  TCustomModule = class(TBaseCustomModule, ICustomModule)
  public
    { ICustomModule }
    function GetAttributes: TCustomModuleAttributes; virtual;
    procedure ExecuteVerb(Index: Integer); virtual;
    function GetVerb(Index: Integer): string; virtual;
    function GetVerbCount: Integer; virtual;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); virtual;
    procedure Saving; virtual;
    procedure ValidateComponent(Component: TComponent); virtual;
    function ValidateComponentClass(ComponentClass: TComponentClass)
      : Boolean; virtual;
    function Nestable: Boolean; virtual;
  end;

var
  DockableForm: TDockableForm;

implementation

{$R *.dfm}
{ TDockableForm }

procedure TDockableForm.AfterConstruction;
begin
  inherited;
  RegisterDesignNotification(Self);
end;

procedure TDockableForm.BeforeDestruction;
begin
  inherited;
  UnregisterDesignNotification(Self);
end;

procedure TDockableForm.CheckPosChanged;
begin

end;

procedure TDockableForm.DeinitializeDesigner(ARoot: TComponent);
begin

end;

procedure TDockableForm.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin

end;

procedure TDockableForm.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin

end;

function TDockableForm.GetCanPrint: Boolean;
begin

end;

function TDockableForm.GetCaption: string;
begin
  Result := Self.Caption;
end;

function TDockableForm.GetDesignerState: TDesignerState;
begin

end;

function TDockableForm.GetFont: TPersistent;
begin
  Result := Self.Font;
end;

function TDockableForm.GetForm: TComponent;
begin
  Result := Self;
end;

function TDockableForm.GetFormImage: TObject;
begin

end;

function TDockableForm.GetScrollPos(Horiz: Boolean): Integer;
begin

end;

function TDockableForm.GetVisible: Boolean;
begin
  Result := Self.Visible;
end;

function TDockableForm.GetWindowState: TShowState;
begin

end;

procedure TDockableForm.HideWindow;
begin

end;

function TDockableForm.IsMenuKey(var Message: TWMKey): Boolean;
begin

end;

procedure TDockableForm.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TDockableForm.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TDockableForm.ItemsModified(const ADesigner: IDesigner);
begin

end;

procedure TDockableForm.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin

end;

procedure TDockableForm.SetCaption(const ACaption: string);
begin
  Self.Caption := ACaption;
end;

procedure TDockableForm.SetDesigner(const ADesigner: IInterface);
begin
  if Assigned(ADesigner) and (ADesigner.QueryInterface(IDesigner, FDesigner)
    = S_OK) then
  begin

  end;
end;

procedure TDockableForm.SetDesigning(DesignMode: Boolean);
begin
  Self.SetDesigning(DesignMode, True);
end;

procedure TDockableForm.SetFormDefaults(ARoot: TComponent;
  const ARootName: string; X, Y: Integer; Scale: Boolean);
begin

end;

procedure TDockableForm.ShowWindow(AShowState: TShowState);
begin

end;

procedure TDockableForm.Unmodify;
begin

end;

procedure TDockableForm.WindowHide;
begin

end;

procedure TDockableForm.WindowShow;
begin

end;

{ TCustomModule }

procedure TCustomModule.ExecuteVerb(Index: Integer);
begin

end;

function TCustomModule.GetAttributes: TCustomModuleAttributes;
begin
  Result := [];
end;

function TCustomModule.GetVerb(Index: Integer): string;
begin

end;

function TCustomModule.GetVerbCount: Integer;
begin
  Result := 0;
end;

function TCustomModule.Nestable: Boolean;
begin
  Result := False;
end;

procedure TCustomModule.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin

end;

procedure TCustomModule.Saving;
begin

end;

procedure TCustomModule.ValidateComponent(Component: TComponent);
begin

end;

function TCustomModule.ValidateComponentClass(ComponentClass
  : TComponentClass): Boolean;
begin
  Result := True;
end;

end.
