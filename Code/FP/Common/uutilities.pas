unit uUtilities;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fgl, typinfo, rttiutils, strutils, Forms,
  Controls, LCLType, LMessages, PropEditUtils;

type

  //TGetStr = reference to procedure(AIndex: Integer; const AStr: string);
  TGetStr = procedure(AIndex: integer; const AStr: string) is nested;
  TProc = procedure() is nested;
  TPredicate = function(): boolean is nested;
  generic TProc1<T> = procedure(Arg: T) is nested;
  generic TProc2<T1, T2> = procedure(Arg1: T1; Arg2: T2) is nested;
  generic TProc3<T1, T2, T3> = procedure(Arg1: T1; Arg2: T2; Arg3: T3) is nested;
  generic TPredicate1<T> = function(Arg: T): boolean is nested;
  generic TFunc0<T> = function(): T is nested;
  generic TFunc1<T1, T2> = function(Arg: T1): T2 is nested;
  generic TFunc2<T1, T2, T3> = function(Arg1: T1; Arg2: T2): T3 is nested;

  TStreamOriginalFormat = (sofUnknown, sofBinary, sofDFM, sofLFM);
  { TComponentNexus }

  TComponentNexus = class(TComponent)
  private
    FHost: TObject;
    FOnHostFree: TNotifyEvent;
    procedure SetHost(AValue: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Host: TObject read FHost write SetHost;
    property OnHostFree: TNotifyEvent read FOnHostFree write FOnHostFree;
    constructor Create(AOwner: TComponent; AHost: TObject; AOnHostFree: TNotifyEvent);
  end;

  { TReaderEx }

  TReaderEx = class(TReader)
  private
    FSelf: TComponent;
  public
    procedure AfterConstruction; override;
    procedure SetMethodPropertyEx(Reader: TReader; Instance: TPersistent;
      PropInfo: PPropInfo; const TheMethodName: string; var Handled: boolean);
    procedure PropertyNotFoundEx(Reader: TReader; Instance: TPersistent;
      var APropName: string; IsPath: boolean; var Handled, Skip: boolean);
    procedure ReferenceNameEx(Reader: TReader; var Name: string);
    function FindComponentEx(Reader: TReader; const AName: string): TComponent;
    procedure SetNameEx(Reader: TReader; Component: TComponent; var Name: string);
  end;

  TWriterEx = class(TWriter)

  end;

  { TThreadEx }
  TStrExecuteStatusCallBack = procedure(const AData: string;
    ReportStatus: TThreadReportStatus) of object;
  TStrStatusNotifyCallBack = procedure(Sender: TThread; const AData: string;
    const status: string) of object;
  TStrNotifyCallBack = procedure(Sender: TObject; const AData: string) of object;

  TThreadEx = class(TThread)
  public
    class function ExecuteInThread(AMethod: TStrExecuteStatusCallBack;
      AOnStatus: TStrStatusNotifyCallBack; const AData: string = '';
      AOnTerminate: TStrNotifyCallBack = nil): TThread; overload; static;
  end;

  { TObjectDesigner }

  TObjectDesigner = class(TIDesigner)
  protected
    FSelectionList: TPersistentSelectionList;
  public
    property SelectionList: TPersistentSelectionList read FSelectionList;
    constructor Create(ARoot: TComponent);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function IsDesignMsg(Sender: TControl; var Message: TLMessage): boolean;
      override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure Modified; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintGrid; override;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); override;
    function GetShiftState: TShiftState; override;
    procedure SelectOnlyThisComponent(AComponent: TComponent); override;
    function UniqueName(const BaseName: string): string; override;
    procedure PrepareFreeDesigner(AFreeComponent: boolean); override;
  end;

function UniqName(AOwner: TComponent; APrefix: string): string;

function GetUltimateOwner(AObject: TObject): TPersistent; overload;
function GetUltimateOwner(AObject: TObject;
  ACast: specialize TFunc1<TObject, TPersistent>): TPersistent; overload;

function IsAncestor(AAncestor, AChild: TObject): boolean;

procedure IterateComponents(AObject: TComponent; AGetter: specialize TProc1<TObject>;
  ARecursive: boolean);

function StringStart(const AStr: string; AStart: integer): string;
procedure SplitALine(ALine: PChar; AGetter: TGetStr; const ASpliter: char = ',';
  const AQuoted: char = #0; ABreakLine: boolean = True);
procedure ForeachLines(const AStream: TStream; AGetter: TGetStr); overload;
procedure ForeachLines(const AStr: string; AGetter: TGetStr); overload;
procedure ForeachCSVColumns(const AStr: string; AGetter: TGetStr;
  const ASpliter: char = ','; const AQuoted: char = #0);

procedure StreamAccessor(W: specialize TProc1<TWriter>; R: specialize TProc1<TReader>);
procedure SavePersistent(AObject: TPersistent; AStream: TStream);
procedure SaveComponent(ARoot: TComponent; AStream: TStream);
procedure LoadPersistent(AObject: TPersistent; AStream: TStream);
procedure CopyPersistent(ASource, ATarget: TPersistent);
procedure LoadComponent(ARoot: TComponent; AStream: TStream;
  AGetter: specialize TProc1<TComponent>; AOwnObject: boolean = True); overload;
procedure LoadComponent(ARoot: TComponent; const ADFM: string;
  AGetter: specialize TProc1<TComponent>; AOwnObject: boolean = True); overload;
function LoadComponent(AFileName: string; AFormat: TStreamOriginalFormat): TComponent;
  overload;
function LoadComponent(AStream: TStream; AFormat: TStreamOriginalFormat): TComponent;
  overload;
function FormatDFM(const ADFM: string): string;

procedure ForeachProps(AClass: TClass; AGetter: specialize TProc1<PPropInfo>;
  ATypeKinds: TTypeKinds = tkAny);
procedure ForeachMethods(AClass: TClass;
  AGetter: specialize TProc3<TClass, string, Pointer>);
procedure ForeachFields(AClass: TClass; AGetter: specialize TProc1<string>);

function AS2US(const AIn: ansistring): UnicodeString;
function US2AS(const AIn: UnicodeString): ansistring;
function Char8DateToDate(AChart8Date: integer): TDate;

implementation

type

  { TPersistentNexus }

  TPersistentNexus = class(TComponent)
  private
    FHost: TPersistent;
  published
    property Host: TPersistent read FHost write FHost;
  end;

  { TSimpleStrStatusProcThread }

  TSimpleStrStatusProcThread = class(TThread)
  private
    FExecuteMethod: TStrExecuteStatusCallBack;
    FCallOnTerminate: TStrNotifyCallBack;
    FStatus: string;
    FOnStatus: TStrStatusNotifyCallBack;
    FData: string;
  protected
    procedure Execute; override;
    procedure DoStatus;
    procedure SetStatus(const AStatus: string);
    procedure TerminateCallBack(Sender: TObject);
  public
    constructor Create(ExecuteMethod: TStrExecuteStatusCallBack;
      const AData: string; AOnStatus: TStrStatusNotifyCallBack;
      AOnterminate: TStrNotifyCallBack);
  end;

function AS2US(const AIn: ansistring): UnicodeString;
var
  LBytes: TBytes;
begin
  with TEncoding.GetEncoding(936) do
    try
      SetLength(LBytes, Length(AIn));
      Move(PAnsiChar(@AIn[1])^, LBytes[0], Length(AIn));
      Result := GetString(LBytes);
    finally
      Free;
    end;
end;

function US2AS(const AIn: UnicodeString): ansistring;
var
  LBytes: TBytes;
begin
  with TEncoding.GetEncoding(936) do
    try
      LBytes := GetBytes(AIn);
      SetLength(Result, Length(LBytes));
      //fpc_zeromem(PAnsiChar(Result), Length(LBytes));
      Move(LBytes[0], PAnsiChar(Result)^, Length(LBytes));
    finally
      Free;
    end;
end;

function Char8DateToDate(AChart8Date: integer): TDate;
var
  Y, M, D: integer;
begin
  if AChart8Date = 0 then
    Result := 0
  else
  begin
    Y := AChart8Date div 10000;
    M := (AChart8Date mod 10000) div 100;
    D := AChart8Date mod 100;
    Result := EncodeDate(Y, M, D);
  end;
end;

procedure ForeachLines(const AStream: TStream; AGetter: TGetStr);
var
  LBuff: TBytes;
  LBOMLength: integer;
  LEncoding: TEncoding;
  LStr: string;

begin
  with AStream do
  begin
    Position := 0;
    LBOMLength := 0;
    LEncoding := nil;
    SetLength(LBuff, size);
    ReadBuffer(pointer(LBuff)^, size);
    LBOMLength := TEncoding.GetBufferEncoding(LBuff, LEncoding, TEncoding.ANSI);

    LStr := LEncoding.GetString(LBuff, LBOMLength, length(LBuff) - LBOMLength);

    ForeachLines(LStr, AGetter);
  end;
end;

procedure ForeachLines(const AStr: string; AGetter: TGetStr);
var
  I: integer;
  S, P: PChar;
begin
  I := 0;
  S := @AStr[1];
  while S^ <> #0 do
  begin
    P := S;
    while not (P^ in [#0, #10, #13]) do
      Inc(P);
    AGetter(I, Copy(S, 0, P - S));
    while P^ in [#10, #13] do
      Inc(P);
    S := P;
    Inc(I);
  end;
end;

procedure ForeachCSVColumns(const AStr: string; AGetter: TGetStr;
  const ASpliter: char; const AQuoted: char);
begin
  SplitALine(@AStr[1], AGetter, ASpliter, AQuoted, True);
end;

procedure StreamAccessor(W: specialize TProc1<TWriter>; R: specialize TProc1<  TReader>);
var
  LMem: TMemoryStream;
  LW: TWriter;
  LR: TReader;
begin
  LMem := TMemoryStream.Create;
  try
    LW := TWriter.Create(LMem, 1024);
    try
      W(LW);
    finally
      LW.Free;
    end;
    LMem.Position := 0;
    LR := TReader.Create(LMem, 1024);
    try
      R(LR);
    finally
      LR.Free;
    end;
  finally
    LMem.Free;
  end;

end;

procedure SavePersistent(AObject: TPersistent; AStream: TStream);
var
  LNexus: TPersistentNexus;
begin
  if Assigned(AObject) and Assigned(AStream) then
  begin
    LNexus := TPersistentNexus.Create(nil);
    try
      LNexus.Host := AObject;
      with TWriter.Create(AStream, 1024) do
        try
          WriteRootComponent(LNexus);
        finally
          Free;
        end;
      LNexus.Host := nil;
    finally
      LNexus.Free;
    end;
  end;
end;

procedure SaveComponent(ARoot: TComponent; AStream: TStream);

  procedure SetName(O: TObject);
  begin
    if O is TComponent then
      with O as TComponent do
        Name := UniqName(Owner, ClassName);
  end;

begin
  if Assigned(ARoot) and Assigned(AStream) then
  begin
    IterateComponents(ARoot, @SetName, True);
    with TWriterEx.Create(AStream, 2046) do
      WriteRootComponent(ARoot);
  end;
end;

procedure LoadPersistent(AObject: TPersistent; AStream: TStream);
var
  LNexus: TPersistentNexus;
begin
  if Assigned(AObject) and Assigned(AStream) then
  begin
    LNexus := TPersistentNexus.Create(nil);
    try
      LNexus.Host := AObject;
      with TReader.Create(AStream, 1024) do
        try
          ReadRootComponent(LNexus);
        finally
          Free;
        end;
      LNexus.Host := nil;
    finally
      LNexus.Free;
    end;
  end;
end;

procedure CopyPersistent(ASource, ATarget: TPersistent);
var
  LMem: TMemoryStream;
begin
  LMem := TMemoryStream.Create;
  try
    SavePersistent(ASource, LMem);
    LMem.Position := 0;
    LoadPersistent(ATarget, LMem);
  finally
    LMem.Free;
  end;
end;

procedure LoadComponent(ARoot: TComponent; AStream: TStream;
  AGetter: specialize TProc1<TComponent>; AOwnObject: boolean);
begin
  with TReaderEx.Create(AStream, 2048) do
    try
      ARoot := ReadRootComponent(ARoot);
      AGetter(ARoot);
      if AOwnObject then
        FreeAndNil(ARoot);
    finally
      Free;
    end;
end;

procedure LoadComponent(ARoot: TComponent; const ADFM: string;
  AGetter: specialize TProc1<TComponent>; AOwnObject: boolean);
var
  LText: TStringStream;
  LMem: TMemoryStream;
begin
  LMem := TMemoryStream.Create;
  try
    LText := TStringStream.Create(ADFM);
    try
      LText.Position := 0;
      ObjectTextToBinary(LText, LMem);
    finally
      LText.Free;
    end;
    LMem.Position := 0;
    LoadComponent(ARoot, LMem, AGetter, AOwnObject);
  finally
    LMem.Free;
  end;
end;

function LoadComponent(AFileName: string; AFormat: TStreamOriginalFormat): TComponent;
var
  LFile: TFileStream;
begin
  Result := nil;
  if FileExists(AFileName) then
  begin
    LFile := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyWrite);
    try
      LFile.Position := 0;
      Result := LoadComponent(LFile, AFormat);
    finally
      LFile.Free;
    end;
  end;
end;

function LoadComponent(AStream: TStream; AFormat: TStreamOriginalFormat): TComponent;
var
  LStream: TStream;
  LResult: TComponent;

  procedure Get(C: TComponent);
  begin
    LResult := C;
  end;

begin
  LResult := nil;
  case AFormat of
    sofBinary: LoadComponent(nil, AStream, @Get, False);
    sofDFM, sofLFM:
      try
        LStream := TMemoryStream.Create;
        ObjectTextToBinary(AStream, LStream);
        LStream.Position := 0;
        LoadComponent(nil, LStream, @Get, False);
      finally
        LStream.Free;
      end;
  end;
  Result := LResult;
end;

function FormatDFM(const ADFM: string): string;
var
  LInput, LOutput: TStringStream;
  LBin: TMemoryStream;
begin
  LBin := TMemoryStream.Create;
  try
    LInput := TStringStream.Create(ADFM);
    try
      LInput.Position := 0;
      ObjectTextToBinary(LInput, LBin);
    finally
      LInput.Free;
    end;
    LOutput := TStringStream.Create('');
    try
      LBin.Position := 0;
      ObjectBinaryToText(LBin, LOutput);
      Result := LOutput.DataString;
    finally
      LOutput.Free;
    end;
  finally
    LBin.Free;
  end;
end;

type
  tmethodnamerec = packed record
    Name: pshortstring;
    addr: codepointer;
  end;

  tmethodnametable = packed record
    Count: dword;
    entries: packed array[0..0] of tmethodnamerec;
  end;

  pmethodnametable = ^tmethodnametable;

procedure ForeachProps(AClass: TClass; AGetter: specialize TProc1<PPropInfo>;
  ATypeKinds: TTypeKinds);
var
  I: integer;
  LList: PPropList;
  LCount: integer;
  LSize: integer;
begin
  if Assigned(AClass) then
    try
      LCount := GetPropList(AClass.ClassInfo, ATypeKinds, nil);
      LSize := LCount * SizeOf(Pointer);
      if LSize > 0 then
      begin
        GetMem(LList, LSize);
        GetPropList(AClass.ClassInfo, ATypeKinds, LList);
        for I := 0 to Pred(LCount) do
          AGetter(LList^[I]);
      end;
    finally
      if LSize > 0 then
        FreeMem(LList, LSize);
    end;
end;

procedure ForeachMethods(AClass: TClass;
  AGetter: specialize TProc3<TClass, string, Pointer>);
var
  methodtable: pmethodnametable;
  i: dword;
  ovmt: PVmt;
begin
  if Assigned(AClass) then
  begin
    ovmt := PVmt(AClass);
    while assigned(ovmt) do
    begin
      methodtable := pmethodnametable(ovmt^.vMethodTable);
      if assigned(methodtable) then
      begin
        for i := 0 to methodtable^.Count - 1 do
          AGetter(TClass(ovmt), methodtable^.entries[i].Name^,
            methodtable^.entries[i].addr);
      end;
      ovmt := ovmt^.vParent;
    end;
  end;
end;

procedure ForeachFields(AClass: TClass; AGetter: specialize TProc1<string>);
type
  PFieldInfo = ^TFieldInfo;

  TFieldInfo =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
    FieldOffset: PtrUInt;
    ClassTypeIndex: word;
    Name: ShortString;
  end;

  PFieldTable = ^TFieldTable;

  TFieldTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
    FieldCount: word;
    ClassTable: Pointer;
     { should be array[Word] of TFieldInfo;  but
       Elements have variant size! force at least proper alignment }
    Fields: array[0..0] of TFieldInfo
  end;

var
  ovmt: PVmt;
  FieldTable: PFieldTable;
  FieldInfo: PFieldInfo;
  i: longint;

begin
  if Assigned(AClass) then
  begin
    ovmt := PVmt(AClass);
    while ovmt <> nil do
    begin
      FieldTable := PFieldTable(ovmt^.vFieldTable);
      if FieldTable <> nil then
      begin
        FieldInfo := @FieldTable^.Fields[0];
        for i := 0 to FieldTable^.FieldCount - 1 do
        begin
          AGetter(FieldInfo^.Name);
          FieldInfo := PFieldInfo(PByte(@FieldInfo^.Name) + 1 +
            Length(FieldInfo^.Name));
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
          { align to largest field of TFieldInfo }
          FieldInfo := Align(FieldInfo, SizeOf(PtrUInt));
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
        end;
      end;
      { Try again with the parent class type }
      ovmt := ovmt^.vParent;
    end;
  end;
end;

function UniqName(AOwner: TComponent; APrefix: string): string;
var
  I: integer;
begin
  if APrefix = '' then
    Result := ''
  else
  begin
    for I := 1 to Length(APrefix) do
      if not (APrefix[i] in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) then
        APrefix[i] := '_';
    if not (APrefix[1] in ['A'..'Z', '_', 'a'..'z']) then
      APrefix := '_' + APrefix;
    if (not Assigned(AOwner)) or (AOwner.FindComponent(APrefix) = nil) then
      Result := APrefix
    else
    begin
      I := 1;
      while AOwner.FindComponent(Format('%s%d', [APrefix, I])) <> nil do
        Inc(I);
      Result := Format('%s%d', [APrefix, I]);
    end;
  end;
end;

function GetUltimateOwner(AObject: TObject): TPersistent;
begin
  Result := nil;
  if Assigned(AObject) then
    if AObject is TComponent then
      Result := TComponent(AObject).Owner
    else if AObject is TCollection then
      Result := TCollection(AObject).Owner
    else if AObject is TCollectionItem then
      Result := TCollectionItem(AObject).Collection;
end;

function GetUltimateOwner(AObject: TObject;
  ACast: specialize TFunc1<TObject, TPersistent>): TPersistent;
begin
  Result := GetUltimateOwner(AObject);
  if (not Assigned(Result)) and Assigned(ACast) then
    Result := ACast(AObject);
end;

function IsAncestor(AAncestor, AChild: TObject): boolean;
begin
  Result := Assigned(AAncestor) and Assigned(AChild) and
    ((AAncestor = AChild) or IsAncestor(AAncestor, GetUltimateOwner(AChild)));
end;

procedure IterateComponents(AObject: TComponent; AGetter: specialize TProc1<TObject>;
  ARecursive: boolean);
var
  I: integer;
begin
  if Assigned(AObject) then
    with AObject do
      for I := 0 to ComponentCount - 1 do
      begin
        AGetter(Components[I]);
        if ARecursive then
          IterateComponents(Components[I], AGetter, ARecursive);
      end;
end;

function StringStart(const AStr: string; AStart: integer): string;
begin
  if (AStart > 0) and (AStart < length(AStr)) then
    Result := string(PChar(@AStr[AStart]))
  else
    Result := '';
end;

procedure SplitALine(ALine: PChar; AGetter: TGetStr; const ASpliter: char;
  const AQuoted: char; ABreakLine: boolean);
var
  P: PChar;
  I: integer;
  LEndChars: set of char;
begin
  if Assigned(ALine) then
  begin
    I := 0;
    if ABreakLine or (AQuoted = #0) then
    begin
      LEndChars := [#0, #10, #13];
      while ALine^ in [#10, #13] do
        Inc(ALine);
    end
    else
      LEndChars := [#0];
    while not (ALine^ in LEndChars) do
    begin
      P := ALine;
      if (AQuoted <> #0) and (P^ = AQuoted) then
        Inc(P);
      while not (P^ in LEndChars) do
        if P^ in [ASpliter, AQuoted] then
          Break
        else
          Inc(P);
      AGetter(I, Copy(ALine, 0, P - ALine));
      if P^ = ASpliter then
        Inc(P)
      else if (AQuoted <> #0) and (P^ = AQuoted) then
        Inc(P);
      Inc(I);
      ALine := P;
    end;
  end;
end;

{ TSimpleStrStatusProcThread }

procedure TSimpleStrStatusProcThread.Execute;
begin
  FreeOnTerminate := True;
  FExecuteMethod(FData, @SetStatus);
end;

procedure TSimpleStrStatusProcThread.DoStatus;
begin
  FOnStatus(Self, FData, FStatus);
end;

procedure TSimpleStrStatusProcThread.SetStatus(const AStatus: string);
begin
  if (AStatus = FStatus) then
    exit;
  FStatus := AStatus;
  if Assigned(FOnStatus) then
    Synchronize(@DoStatus);
end;

procedure TSimpleStrStatusProcThread.TerminateCallBack(Sender: TObject);
begin
  if Assigned(FCallOnTerminate) then
    FCallOnTerminate(Sender, FData);
end;

constructor TSimpleStrStatusProcThread.Create(ExecuteMethod: TStrExecuteStatusCallBack;
  const AData: string; AOnStatus: TStrStatusNotifyCallBack;
  AOnterminate: TStrNotifyCallBack);
begin
  FExecuteMethod := ExecuteMethod;
  FCallOnTerminate := AOnTerminate;
  FData := AData;
  if Assigned(FCallOnTerminate) then
    OnTerminate := @TerminateCallBack;
  FOnStatus := AOnStatus;
  FStatus := '';
  inherited Create(False);
end;

{ TThreadEx }

class function TThreadEx.ExecuteInThread(AMethod: TStrExecuteStatusCallBack;
  AOnStatus: TStrStatusNotifyCallBack; const AData: string;
  AOnTerminate: TStrNotifyCallBack): TThread;
begin
  Result := TSimpleStrStatusProcThread.Create(AMethod, AData, AOnStatus, AOnTerminate);
end;

{ TReaderEx }

procedure TReaderEx.SetMethodPropertyEx(Reader: TReader; Instance: TPersistent;
  PropInfo: PPropInfo; const TheMethodName: string; var Handled: boolean);
var
  LCurrent: TPersistent;
  S, P: PChar;
  LName: string;
  LMethod: TMethod;
begin
  with LMethod do
  begin
    Code := nil;
    Data := nil;
  end;
  Handled := False;

  if TheMethodName <> '' then
  begin
    S := PChar(TheMethodName);
    //Current := Reader.Owner;
    LCurrent := nil;
    while S^ <> #0 do
    begin
      P := S;
      while not (P^ in ['.', #0]) do
        Inc(P);
      if P = S then
        Break;
      SetString(LName, S, p - s);
      if P^ = #0 then
      begin
        if not Assigned(LCurrent) then
          LCurrent := Instance;
        with LMethod do
        begin
          Code := LCurrent.MethodAddress(LName);
          Data := LCurrent;
        end;
        Handled := Assigned(LMethod.Code);
        if Handled then
          SetMethodProp(Instance, PropInfo, LMethod);
      end
      else if SameText(LName, 'root') then
        LCurrent := Reader.Root
      else if SameText('parent', LName) then
        LCurrent := Reader.Parent
      else if SameText('owner', LName) then
        LCurrent := Reader.Owner
      else if SameText('self', LName) then
        LCurrent := Instance
      else if Assigned(LCurrent) and (LCurrent is TComponent) then
      begin
        LCurrent := (LCurrent as TComponent).FindComponent(LName);
        if not Assigned(LCurrent) then
          Break;
      end;
      while p^ = '.' do
        Inc(P);
      s := p;
    end;
  end;
end;

procedure TReaderEx.PropertyNotFoundEx(Reader: TReader; Instance: TPersistent;
  var APropName: string; IsPath: boolean; var Handled, Skip: boolean);
begin
  Skip := True;
end;

procedure TReaderEx.ReferenceNameEx(Reader: TReader; var Name: string);
var
  S, P: PChar;
  LName: string;
begin
  if Name <> '' then
    with TStringList.Create do
      try
        Delimiter := '.';
        S := PChar(Name);
        while S^ <> #0 do
        begin
          P := S;
          while not (P^ in ['.', #0]) do
            Inc(P);
          if P = S then
            Exit;
          SetString(LName, S, P - S);
          if SameText('root', LName) then
            Append(IfThen(Assigned(Reader.Root) and (Reader.Root.Name <> ''),
              Reader.Root.Name, 'root'))
          else if SameText('parent', LName) then
            Append(IfThen(Assigned(Reader.Parent) and (Reader.Parent.Name <> ''),
              Reader.Parent.Name, 'parent'))
          else if SameText('owner', LName) then
            Append(IfThen(Assigned(Reader.Owner) and (Reader.Owner.Name <> ''),
              Reader.Owner.Name, 'owner'))
          else if SameText('self', LName) then
            Append(IfThen(Assigned(FSelf) and (FSelf.Name <> ''), FSelf.Name, 'self'))
          else
            Append(LName);
          while p^ = '.' do
            Inc(P);
          S := P;
        end;
        Name := DelimitedText;
      finally
        Free;
      end;
end;

function TReaderEx.FindComponentEx(Reader: TReader; const AName: string): TComponent;
var
  LCurrent: TComponent;
  S, P: PChar;
  LName: string;
begin
  Result := nil;

  LCurrent := Reader.Root;
  if AName <> '' then
  begin
    S := @AName[1];
    while S^ <> #0 do
    begin
      P := S;
      while not (P^ in [#0, '.']) do
        Inc(P);
      if P = S then
        Exit;
      SetString(LName, S, P - S);
      if SameText('root', LName) then
        LCurrent := Reader.Root
      else if SameText('parent', LName) then
        LCurrent := Reader.Parent
      else if SameText('owner', LName) then
        LCurrent := Reader.Owner
      else if SameText('self', LName) then
        LCurrent := FSelf
      else if Assigned(LCurrent) then
        LCurrent := (LCurrent as TComponent).FindComponent(LName);
      if not Assigned(LCurrent) then
        Break;
      while P^ = '.' do
        Inc(P);
      S := P;
    end;
  end;
  Result := LCurrent as TComponent;
end;

procedure TReaderEx.SetNameEx(Reader: TReader; Component: TComponent; var Name: string);
begin
  FSelf := Component;
end;

procedure TReaderEx.AfterConstruction;
begin
  inherited AfterConstruction;
  OnSetMethodProperty := @SetMethodPropertyEx;
  OnPropertyNotFound := @PropertyNotFoundEx;
  OnReferenceName := @ReferenceNameEx;
  OnSetName := @SetNameEx;
end;

{ TComponentNexus }

procedure TComponentNexus.SetHost(AValue: TObject);
begin
  if FHost = AValue then
    Exit;
  if Assigned(FHost) and (FHost is TComponent) then
    TComponent(FHost).RemoveFreeNotification(Self);
  FHost := AValue;
  if Assigned(FHost) and (FHost is TComponent) then
    TComponent(FHost).FreeNotification(Self);
end;

procedure TComponentNexus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = TOperation.opRemove) and (FHost = AComponent) and
    Assigned(FOnHostFree) then
    FOnHostFree(AComponent);
end;

constructor TComponentNexus.Create(AOwner: TComponent; AHost: TObject;
  AOnHostFree: TNotifyEvent);
begin
  inherited Create(AOwner);
  Host := AHost;
  OnHostFree := AOnHostFree;
end;

{ TObjectDesigner }

constructor TObjectDesigner.Create(ARoot: TComponent);
begin
  inherited Create;
  FLookupRoot := ARoot;
end;

procedure TObjectDesigner.AfterConstruction;
begin
  inherited AfterConstruction;
  FSelectionList := TPersistentSelectionList.Create;
end;

procedure TObjectDesigner.BeforeDestruction;
begin
  inherited BeforeDestruction;
  PrepareFreeDesigner(True);
  FreeAndNil(FSelectionList);
end;

function TObjectDesigner.IsDesignMsg(Sender: TControl; var Message: TLMessage): boolean;

begin
  Result := False;
end;

procedure TObjectDesigner.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin

end;

procedure TObjectDesigner.Modified;
begin

end;

procedure TObjectDesigner.Notification(AComponent: TComponent; Operation: TOperation);
begin

end;

procedure TObjectDesigner.PaintGrid;
begin

end;

procedure TObjectDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin

end;

function TObjectDesigner.GetShiftState: TShiftState;
begin

end;

procedure TObjectDesigner.SelectOnlyThisComponent(AComponent: TComponent);
begin

end;

function TObjectDesigner.UniqueName(const BaseName: string): string;
begin
  Result := uUtilities.UniqName(LookupRoot, BaseName);
end;

procedure TObjectDesigner.PrepareFreeDesigner(AFreeComponent: boolean);
begin
  if AFreeComponent and Assigned(FLookupRoot) then
    FreeAndNil(FLookupRoot);
end;

initialization
  RegisterClasses([TPersistentNexus]);

finalization
  UnRegisterClasses([TPersistentNexus]);
end.
