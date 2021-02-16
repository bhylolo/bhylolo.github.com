unit fmBaseDesigner;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, toolsapi, DesignIntf,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TFrameBaseDesigner = class(TFrame, IDesignNotification)
  private
    FDesigner: IDesigner;
  protected
    procedure SetDesigner(const Value: IDesigner); virtual;
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
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Designer: IDesigner read FDesigner write SetDesigner;
  end;

var
  FrameBaseDesigner: TFrameBaseDesigner;

implementation

{$R *.dfm}
{ TFrameBaseDesigner }

procedure TFrameBaseDesigner.AfterConstruction;
begin
  inherited;
  RegisterDesignNotification(Self);
end;

procedure TFrameBaseDesigner.BeforeDestruction;
begin
  inherited;
  UnregisterDesignNotification(Self);
end;

procedure TFrameBaseDesigner.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin

end;

procedure TFrameBaseDesigner.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin

end;

procedure TFrameBaseDesigner.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TFrameBaseDesigner.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TFrameBaseDesigner.ItemsModified(const ADesigner: IDesigner);
begin

end;

procedure TFrameBaseDesigner.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin

end;

procedure TFrameBaseDesigner.SetDesigner(const Value: IDesigner);
begin
  if FDesigner = Value then
    Exit;
  FDesigner := Value;
end;

end.
