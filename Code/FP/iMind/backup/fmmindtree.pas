unit fmmindtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, VirtualTrees, ActiveX;

type

  { TFrameMindTree }

  TFrameMindTree = class(TFrame)
    VTree: TVirtualStringTree;
    procedure VTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: integer);
    procedure VTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VTreeLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure VTreeSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
  private
    FModified: boolean;
    procedure SetModified(AValue: boolean);

  public
    property Modified: boolean read FModified write SetModified;
  end;

  TMind = record
    Title: string;
    Details: TStrings;
  end;

implementation

{$R *.lfm}

{ TFrameMindTree }

procedure TFrameMindTree.VTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: integer);
begin
  NodeDataSize := SizeOf(TMind);
end;

procedure TFrameMindTree.VTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  with TMind(Sender.GetNodeData(Node)^) do
    if (Sender as TVirtualStringTree).Header.Columns.Count = 0 then
      CellText := Title
    else
      case Column of
        0: CellText := Title;
        else
          CellText := '';
      end;
end;

procedure TFrameMindTree.VTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  with TMind(Sender.GetNodeData(Node)^) do
    if InitialStates * [ivsReInit] <> [] then
    begin
      if not Assigned(Details) then
        Details := TStringList.Create;
    end;
end;

procedure TFrameMindTree.VTreeLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
begin
  with TMind(Sender.GetNodeData(Node)^) do
  begin
    Title := Stream.ReadAnsiString;
    if not Assigned(Details) then
      Details := TStringList.Create;
    Details.Text := Stream.ReadAnsiString;
  end;
end;

procedure TFrameMindTree.VTreeSaveNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
begin
  with TMind(Sender.GetNodeData(Node)^) do
  begin
    Stream.WriteAnsiString(Title);
    Stream.WriteAnsiString(Details.Text);
  end;
end;

procedure TFrameMindTree.SetModified(AValue: boolean);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
end;

procedure TFrameMindTree.VTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  with TMind(Sender.GetNodeData(Node)^) do
  begin
    Title := '';
    if Assigned(Details) then
      FreeAndNil(Details);
  end;
end;

end.
