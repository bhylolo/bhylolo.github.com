unit fmInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, RTTIGrids, PropEdits, PropEditUtils;

type

  { TFrameInspector }

  TFrameInspector = class(TFrame)
    TIPropertyGrid1: TTIPropertyGrid;
  private

  public

  end;

implementation

{$R *.lfm}
initialization
  if not Assigned(GlobalDesignHook) then
    GlobalDesignHook := TPropertyEditorHook.Create(nil);

  RegisterPropertyEditor(TypeInfo(string), nil, 'FileName', TFileNamePropertyEditor);

finalization
  if Assigned(GlobalDesignHook) then
    FreeAndNil(GlobalDesignHook);
end.

