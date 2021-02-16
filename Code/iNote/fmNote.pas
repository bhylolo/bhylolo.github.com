unit fmNote;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,DockForm,
  Vcl.ComCtrls;

type
  TFormNote = class(TDockableForm)
    reNote: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNote: TFormNote;

implementation

{$R *.dfm}

end.
