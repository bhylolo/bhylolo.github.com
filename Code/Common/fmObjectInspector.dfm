object FrameObjectInspector: TFrameObjectInspector
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Properties'
      'Events'
      'Procedures')
    TabIndex = 0
    OnChange = TabControl1Change
    inline FrameObjectTree1: TFrameObjectTree
      Left = 4
      Top = 24
      Width = 312
      Height = 212
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 312
      ExplicitHeight = 212
      inherited VTree: TVirtualStringTree
        Width = 312
        Height = 212
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick, toEditOnDblClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus]
        OnCompareNodes = FrameObjectTree1VTreeCompareNodes
        OnEditing = FrameObjectTree1VTreeEditing
        ExplicitWidth = 312
        ExplicitHeight = 212
      end
    end
  end
end
