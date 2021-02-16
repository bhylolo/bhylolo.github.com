unit fmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  ActnList, VirtualTrees, RTTIGrids;

type

  { TFormMain }

  TFormMain = class(TForm)
    acAddRoot: TAction;
    acAddChild: TAction;
    acAddSibling: TAction;
    acDeleteNode: TAction;
    ActionList1: TActionList;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure acAddChildUpdate(Sender: TObject);
    procedure acAddRootUpdate(Sender: TObject);
    procedure acAddSiblingUpdate(Sender: TObject);
    procedure acDeleteNodeUpdate(Sender: TObject);

  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.acAddRootUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (ActiveControl is TVirtualStringTree);
end;

procedure TFormMain.acAddSiblingUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (ActiveControl is TVirtualStringTree) and
    ((ActiveControl as TVirtualStringTree).SelectedCount > 0);
end;

procedure TFormMain.acDeleteNodeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (ActiveControl is TVirtualStringTree) and
    ((ActiveControl as TVirtualStringTree).SelectedCount > 0);
end;

procedure TFormMain.acAddChildUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (ActiveControl is TVirtualStringTree) and
    ((ActiveControl as TVirtualStringTree).SelectedCount > 0);
end;

end.
