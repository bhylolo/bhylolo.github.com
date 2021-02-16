unit dmMain;

interface

uses
  System.SysUtils, System.Classes, System.Actions, Vcl.ActnList,
  System.ImageList, Vcl.ImgList, Vcl.Controls;

type
  TDataModuleMain = class(TDataModule)
    ImageListMain: TImageList;
    ActionListMain: TActionList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
