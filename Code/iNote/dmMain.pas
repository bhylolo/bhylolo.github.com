unit dmMain;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  System.Actions, Vcl.ActnList, Vcl.StdActns, Vcl.BandActn, Vcl.ExtActns,
  Vcl.Bind.Navigator, Vcl.ListActns, Vcl.DBClientActns, Vcl.DBActns, Vcl.Menus;

type
  TDataModuleMain = class(TDataModule)
    al: TActionList;
    il: TImageList;
    Action1: TAction;
    ControlAction1: TControlAction;
    DatasetFirst1: TDataSetFirst;
    DatasetPrior1: TDataSetPrior;
    DatasetNext1: TDataSetNext;
    DatasetLast1: TDataSetLast;
    DatasetInsert1: TDataSetInsert;
    DatasetDelete1: TDataSetDelete;
    DatasetEdit1: TDataSetEdit;
    DatasetPost1: TDataSetPost;
    DatasetCancel1: TDataSetCancel;
    DatasetRefresh1: TDataSetRefresh;
    DatasetClientDataSetApply1: TClientDataSetApply;
    DatasetClientDataSetRevert1: TClientDataSetRevert;
    DatasetClientDataSetUndo1: TClientDataSetUndo;
    DialogOpenPicture1: TOpenPicture;
    DialogSavePicture1: TSavePicture;
    DialogColorSelect1: TColorSelect;
    DialogFontEdit1: TFontEdit;
    DialogPrintDlg1: TPrintDlg;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    FileOpen1: TFileOpen;
    FileOpenWith1: TFileOpenWith;
    FileSaveAs1: TFileSaveAs;
    FilePrintSetup1: TFilePrintSetup;
    FilePageSetup1: TFilePageSetup;
    FileRun1: TFileRun;
    FileExit1: TFileExit;
    BrowseForFolder1: TBrowseForFolder;
    FormatRichEditBold1: TRichEditBold;
    FormatRichEditItalic1: TRichEditItalic;
    FormatRichEditUnderline1: TRichEditUnderline;
    FormatRichEditStrikeOut1: TRichEditStrikeOut;
    FormatRichEditBullets1: TRichEditBullets;
    FormatRichEditAlignLeft1: TRichEditAlignLeft;
    FormatRichEditAlignRight1: TRichEditAlignRight;
    FormatRichEditAlignCenter1: TRichEditAlignCenter;
    HelpContents1: THelpContents;
    HelpTopicSearch1: THelpTopicSearch;
    HelpOnHelp1: THelpOnHelp;
    HelpContextAction1: THelpContextAction;
    InternetBrowseURL1: TBrowseURL;
    InternetDownLoadURL1: TDownLoadURL;
    InternetSendMail1: TSendMail;
    StaticListAction1: TStaticListAction;
    VirtualListAction1: TVirtualListAction;
    ListControlCopySelection1: TListControlCopySelection;
    ListControlDeleteSelection1: TListControlDeleteSelection;
    ListControlSelectAll1: TListControlSelectAll;
    ListControlClearSelection1: TListControlClearSelection;
    ListControlMoveSelection1: TListControlMoveSelection;
    LiveBindingsBindNavigateFirst1: TBindNavigateFirst;
    LiveBindingsBindNavigatePrior1: TBindNavigatePrior;
    LiveBindingsBindNavigateNext1: TBindNavigateNext;
    LiveBindingsBindNavigateLast1: TBindNavigateLast;
    LiveBindingsBindNavigateInsert1: TBindNavigateInsert;
    LiveBindingsBindNavigateDelete1: TBindNavigateDelete;
    LiveBindingsBindNavigateEdit1: TBindNavigateEdit;
    LiveBindingsBindNavigatePost1: TBindNavigatePost;
    LiveBindingsBindNavigateCancel1: TBindNavigateCancel;
    LiveBindingsBindNavigateRefresh1: TBindNavigateRefresh;
    LiveBindingsBindNavigateApplyUpdates1: TBindNavigateApplyUpdates;
    LiveBindingsBindNavigateCancelUpdates1: TBindNavigateCancelUpdates;
    SearchFind1: TSearchFind;
    SearchFindNext1: TSearchFindNext;
    SearchReplace1: TSearchReplace;
    SearchFindFirst1: TSearchFindFirst;
    TabPreviousTab1: TPreviousTab;
    TabNextTab1: TNextTab;
    CustomizeActionBars1: TCustomizeActionBars;
    WindowClose1: TWindowClose;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowMinimizeAll1: TWindowMinimizeAll;
    WindowArrange1: TWindowArrange;
    mmMain: TMainMenu;
    File2: TMenuItem;
    New2: TMenuItem;
    Open2: TMenuItem;
    Save2: TMenuItem;
    SaveAs2: TMenuItem;
    Print2: TMenuItem;
    PrintSetup2: TMenuItem;
    Exit2: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Edit2: TMenuItem;
    Undo2: TMenuItem;
    Repeat2: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    PasteSpecial2: TMenuItem;
    Find2: TMenuItem;
    Replace2: TMenuItem;
    GoTo2: TMenuItem;
    Links2: TMenuItem;
    Object2: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    Window1: TMenuItem;
    NewWindow1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    Hide1: TMenuItem;
    Show1: TMenuItem;
    N12: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    Index1: TMenuItem;
    Commands1: TMenuItem;
    Procedures1: TMenuItem;
    Keyboard1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Tutorial1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    Search1: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Replace1: TMenuItem;
    FindFirst1: TMenuItem;
    Italic1: TMenuItem;
    Strikeout1: TMenuItem;
    Underline1: TMenuItem;
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
