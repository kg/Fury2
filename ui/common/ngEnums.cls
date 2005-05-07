VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngEnums"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Enum ngToolButtonTextAlignment
    btaLeft
    btaRight
    btaTop
    btaBottom
End Enum

Public Enum ngToolButtonStyles
    bsyNormal
    bsyCheck
    bsyGroup
    bsySeparator
End Enum

Public Enum ngToolButtonStates
    bstNormal
    bstHighlighted
    bstPressed
    bstChecked
    bstDisabled
End Enum

Public Enum ngToolbarColors
    tbcBackground
    tbcText
    tbcTint
    tbcBorder
    tbcGlow
    tbcHighlight
    tbcHighlightText
    tbcHighlightTint
    tbcHighlightBorder
    tbcHighlightGlow
    tbcPressed
    tbcPressedText
    tbcPressedTint
    tbcPressedBorder
    tbcPressedGlow
    tbcChecked
    tbcCheckedText
    tbcCheckedTint
    tbcCheckedBorder
    tbcCheckedGlow
    tbcDisabled
    tbcDisabledText
    tbcDisabledTint
    tbcDisabledBorder
    tbcDisabledGlow
    tbcSeparator
    tbc_max
End Enum

Public Enum ngToolbarMetrics
    tbmButtonMargin
    tbmImageMargin
    tbmTextMargin
    tbmSeparatorMargin
    tbmSeparatorSize
    tbm_max
End Enum

Public Enum ngToolbarOrientations
    tboHorizontal
    tboVertical
End Enum

Public Enum ngToolButtonOrientations
    borHorizontal
    borVertical
End Enum

Public Enum ngListBoxColors
    lbcBackground
    lbcText
    lbcTint
    lbcBorder
    lbcGlow
    lbcHighlight
    lbcHighlightText
    lbcHighlightTint
    lbcHighlightBorder
    lbcHighlightGlow
    lbc_max
End Enum

Public Enum ngListBoxMetrics
    lbmItemMargin
    lbmImageMargin
    lbmTextMargin
    lbm_max
End Enum

Public Enum ngListItemTextAlignment
    ltaLeft
    ltaRight
    ltaTop
    ltaBottom
End Enum

Public Enum ngResourceTypes
    rtyUnknown
    rtyImage
    rtyText
    rtyScript
    rtyIcon
End Enum

Public Enum ngTabStripMetrics
    tsmTabHorizontalMargin
    tsmTabVerticalMargin
    tsmImageMargin
    tsmTextMargin
    tsm_max
End Enum

Public Enum ngTabTextAlignment
    tsaLeft
    tsaRight
End Enum

Public Enum ngTabStates
    tstNormal
    tstHighlighted
    tstSelected
End Enum

Public Enum ngTabStripColors
    tscBackground
    tscText
    tscFill
    tscBorder
    tscHighlight
    tscHighlightText
    tscHighlightFill
    tscHighlightBorder
    tscSelected
    tscSelectedText
    tscSelectedFill
    tscSelectedBorder
    tsc_max
End Enum

Public Enum ngMenuItemTextAlignment
    mtaLeft
    mtaRight
End Enum

Public Enum ngMenuItemStyles
    msyNormal
    msyCheck
    msyGroup
    msySeparator
End Enum

Public Enum ngMenuItemStates
    mstNormal
    mstHighlighted
    mstPressed
    mstChecked
    mstDisabled
End Enum

Public Enum ngMenuColors
    mncBackground
    mncText
    mncTint
    mncBorder
    mncGlow
    mncHighlight
    mncHighlightText
    mncHighlightTint
    mncHighlightBorder
    mncHighlightGlow
    mncPressed
    mncPressedText
    mncPressedTint
    mncPressedBorder
    mncPressedGlow
    mncChecked
    mncCheckedText
    mncCheckedTint
    mncCheckedBorder
    mncCheckedGlow
    mncDisabled
    mncDisabledText
    mncDisabledTint
    mncDisabledBorder
    mncDisabledGlow
    mncSeparator
    mnc_max
End Enum

Public Enum ngMenuMetrics
    mnmItemMargin
    mnmImageMargin
    mnmTextMargin
    mnmSeparatorMargin
    mnmSeparatorSize
    mnm_max
End Enum

