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

Public Enum ngToolButtonOrientations
    borHorizontal
    borVertical
End Enum

Public Enum ngListBoxTextAlignment
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
