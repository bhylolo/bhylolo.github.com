program iEntity;

uses
  Vcl.Forms,
  fmMain in 'fmMain.pas' {FormMain},
  fmObjectTree in '..\Common\fmObjectTree.pas' {FrameObjectTree: TFrame},
  uComponentContainers in '..\Common\uComponentContainers.pas',
  uUtils in '..\Common\uUtils.pas',
  dmMain in 'dmMain.pas' {DataModuleMain: TDataModule},
  fmChild in 'fmChild.pas' {FormChild},
  uETL in '..\Common\uETL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModuleMain, DataModuleMain);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
