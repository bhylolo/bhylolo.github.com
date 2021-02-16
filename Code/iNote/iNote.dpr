program iNote;

uses
  Vcl.Forms,
  fmMain in 'fmMain.pas' {FormMain},
  fmBaseDesigner in 'fmBaseDesigner.pas' {FrameBaseDesigner},
  DockForm in '..\Common\DockForm.pas' {DockableForm},
  PersonalityConst in '..\Common\PersonalityConst.pas',
  uUtils in '..\Common\uUtils.pas',
  uComponentContainers in '..\Common\uComponentContainers.pas',
  fmObjectTree in '..\Common\fmObjectTree.pas' {FrameObjectTree: TFrame},
  uDesigner in '..\Common\uDesigner.pas',
  dmMain in 'dmMain.pas' {DataModuleMain: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModuleMain, DataModuleMain);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
