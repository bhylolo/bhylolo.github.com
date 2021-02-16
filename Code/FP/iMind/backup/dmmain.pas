unit dmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, Controls, StdActns, VirtualTrees;

type

  { TDataModuleMain }

  TDataModuleMain = class(TDataModule)
    acAddRoot: TAction;
    acAddChild: TAction;
    acAddSibling: TAction;
    acDeleteNode: TAction;
    ActionListMain: TActionList;
    EditCut1: TEditCut;
    ImageListMain: TImageList;

  private

  public

  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{$R *.lfm}

end.

