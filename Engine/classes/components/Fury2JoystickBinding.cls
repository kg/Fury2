VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2JoystickBinding"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
'
'    Engine (Fury² Game Creation System Runtime Engine)
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
Implements IVirtualFileSaveable

Private Const c_Low = 19660
Private Const c_High = 45874

Public Parent As Fury2Joystick

Private mvarPressed As Boolean
Private mvarName As String
Private mvarControlName As String
Private mvarBindType As Long
Private mvarBindIndex As Long
Private mvarEnabled As Boolean
Private mvarBindControl As Fury2InputControl

Public Sub Rebind()
On Error Resume Next
    Set mvarBindControl = m_Engine.Controls(mvarControlName)
End Sub

Public Property Let Name(ByVal vData As String)
    mvarName = vData
End Property

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Set BindControl(ByVal vData As Fury2InputControl)
    Set mvarBindControl = vData
    mvarControlName = vData.Name
End Property

Public Property Get BindControl() As Fury2InputControl
    Set BindControl = mvarBindControl
End Property

Public Property Let LastValue(ByVal vData As Boolean)
End Property

Public Property Get LastValue() As Boolean
Dim m_lngValue As Long
    With Parent
        Select Case mvarBindType
        Case BT_Button
            m_lngValue = .OldButtons(mvarBindIndex)
            If m_lngValue > 0 Then LastValue = True
        Case BT_AxisPositive
            m_lngValue = .OldAxes(mvarBindIndex)
            If m_lngValue >= 0 And m_lngValue >= c_High Then LastValue = True
        Case BT_AxisNegative
            m_lngValue = .OldAxes(mvarBindIndex)
            If m_lngValue >= 0 And m_lngValue <= c_Low Then LastValue = True
        Case BT_POVHat
            m_lngValue = .OldPOVs(mvarBindIndex)
            If m_lngValue >= 0 Then
                LastValue = CBool((m_lngValue \ 1000) = (mvarBindIndex))
            End If
        Case Else
            LastValue = False
        End Select
    End With
End Property

Public Property Let Value(ByVal vData As Boolean)
End Property

Public Property Get Value() As Boolean
On Error Resume Next
Dim m_lngValue As Long
    If Parent Is Nothing Then Exit Property
    With Parent
        Select Case mvarBindType
        Case BT_Button
            m_lngValue = .Buttons(mvarBindIndex)
            If m_lngValue > 0 Then
                Value = True
            End If
        Case BT_AxisPositive
            m_lngValue = .Axes(mvarBindIndex)
            If m_lngValue >= 0 And m_lngValue >= c_High Then
                Value = True
            End If
        Case BT_AxisNegative
            m_lngValue = .Axes(mvarBindIndex)
            If m_lngValue >= 0 And m_lngValue <= c_Low Then
                Value = True
            End If
        Case BT_POVHat
            m_lngValue = .POVs(mvarBindIndex)
            If m_lngValue >= 0 Then
                Value = CBool((m_lngValue \ 1000) = (mvarBindIndex))
            End If
        Case Else
            Value = False
        End Select
    End With
End Property

Public Property Let Enabled(ByVal vData As Boolean)
    mvarEnabled = vData
End Property

Public Property Get Enabled() As Boolean
    Enabled = mvarEnabled
End Property

Public Property Let BindIndex(ByVal vData As Long)
    mvarBindIndex = vData
End Property

Public Property Get BindIndex() As Long
    BindIndex = mvarBindIndex
End Property

Public Property Let BindType(ByVal vData As BindTypes)
    mvarBindType = vData
End Property

Public Property Get BindType() As BindTypes
    BindType = mvarBindType
End Property

Public Function Update() As Boolean
On Error Resume Next
Dim l_booValue As Boolean
    l_booValue = Value
    If l_booValue <> LastValue Then
        If l_booValue Then
            If Not mvarPressed Then
                mvarPressed = True
                BindControl.Activate
            End If
        Else
            If mvarPressed Then
                mvarPressed = False
                BindControl.Deactivate
            End If
        End If
    End If
End Function

Public Function Binding() As String
On Error Resume Next
    Select Case mvarBindType
    Case BT_Button
        Binding = Parent.ButtonName(mvarBindIndex)
    Case BT_AxisPositive
        Binding = "+" + Parent.AxisName(mvarBindIndex)
    Case BT_AxisNegative
        Binding = "-" + Parent.AxisName(mvarBindIndex)
    Case BT_POVHat
        Binding = Parent.POVName(mvarBindIndex)
    Case Else
        Binding = "None"
    End Select
End Function

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_ctlControl As Fury2InputControl
    With File
        mvarPressed = False
        .Load mvarName
        .Load mvarBindType
        .Load mvarBindIndex
        .Load mvarEnabled
        .Load mvarControlName
        For Each l_ctlControl In m_Engine.Controls
            If Trim(LCase(l_ctlControl.Name)) = Trim(LCase(mvarControlName)) Then
                Set mvarBindControl = l_ctlControl
            End If
        Next l_ctlControl
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save mvarName
        .Save mvarBindType
        .Save mvarBindIndex
        .Save mvarEnabled
        .Save mvarControlName
    End With
End Sub
