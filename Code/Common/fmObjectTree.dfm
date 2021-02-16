object FrameObjectTree: TFrameObjectTree
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object VTree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    ClipboardFormats.Strings = (
      'CSV'
      'Plain text'
      'Unicode text'
      'Virtual Tree Data')
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    TabOrder = 0
    OnAddToSelection = VTreeAddToSelection
    OnCompareNodes = VTreeCompareNodes
    OnCreateEditor = VTreeCreateEditor
    OnEditing = VTreeEditing
    OnExpanded = VTreeExpanded
    OnGetText = VTreeGetText
    OnGetNodeDataSize = VTreeGetNodeDataSize
    OnHeaderClick = VTreeHeaderClick
    OnRemoveFromSelection = VTreeRemoveFromSelection
    Columns = <>
  end
end
