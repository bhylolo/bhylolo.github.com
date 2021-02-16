unit fmObjectGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, fmObjectTree, VirtualTrees;

type
  TFrameObjectGrid = class(TFrameObjectTree)
  private
    { Private declarations }
  public
    procedure AfterConstruction; override;
  end;

var
  FrameObjectGrid: TFrameObjectGrid;

implementation

{$R *.dfm}
{ TFrameObjectGrid }

procedure TFrameObjectGrid.AfterConstruction;
begin
  inherited;
  ShowRoot := False;
end;

end.
