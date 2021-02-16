unit fmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls,
  StdCtrls, PropEdits, PropEditUtils, fmInspector, fmComponentTree;

type

  { TFormMain }

  TFormMain = class(TForm)
    Button1: TButton;
    FrameComponentTree1: TFrameComponentTree;
    FrameInspector1: TFrameInspector;
    Panel1: TPanel;
    Splitter1: TSplitter;
  private

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.AfterConstruction;
begin
  inherited AfterConstruction;
  GlobalDesignHook.LookupRoot := Self;
end;

procedure TFormMain.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

end.
