unit dmMain;

interface

uses
  System.SysUtils, System.Classes, System.Actions, Vcl.ActnList,
  System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.Menus;

type
  TDataModuleMain = class(TDataModule)
    ilMain: TImageList;
    alMain: TActionList;
    pmNew: TPopupMenu;
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
