VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cMenuManager"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    ngIDE (Fury² Game Creation System Next-Generation Editor)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Private WithEvents l_mnuMenu As cPopupMenu
Attribute l_mnuMenu.VB_VarHelpID = -1
Private l_imlImageList As vbalImageList
Private l_objEventHandler As Object
Private l_booOpen As Boolean, l_lngOpenIndex As Long

Public Property Get EventHandler() As Object
    Set EventHandler = l_objEventHandler
End Property

Public Property Set EventHandler(NewHandler As Object)
    Set l_objEventHandler = NewHandler
End Property

Public Property Get ImageList() As vbalImageList
    Set ImageList = l_imlImageList
End Property

Public Property Set ImageList(NewImageList As vbalImageList)
    Set l_imlImageList = NewImageList
End Property

Public Property Get Menu() As cPopupMenu
    Set Menu = l_mnuMenu
End Property

Friend Property Set Menu(NewMenu As cPopupMenu)
    Set l_mnuMenu = NewMenu
End Property

Private Sub l_mnuMenu_Click(ItemNumber As Long)
On Error Resume Next
    l_booOpen = False
    l_objEventHandler.Menu_Click l_mnuMenu, ItemNumber
End Sub

Private Sub l_mnuMenu_InitPopupMenu(ParentItemNumber As Long)
On Error Resume Next
    l_booOpen = True
    l_lngOpenIndex = ParentItemNumber
    l_objEventHandler.Menu_Initialize l_mnuMenu, ParentItemNumber
End Sub

Private Sub l_mnuMenu_MenuExit()
On Error Resume Next
    l_booOpen = False
End Sub

Private Sub l_mnuMenu_UnInitPopupMenu(ParentItemNumber As Long)
On Error Resume Next
    l_objEventHandler.Menu_UnInitialize l_mnuMenu, ParentItemNumber
End Sub
