VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Joystick"
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
Implements Fury2Object
Implements Fury2Plugin

Private Const c_Low = 19660
Private Const c_High = 45874

Public Enum BindTypes
    BT_Null
    BT_Button
    BT_AxisPositive
    BT_AxisNegative
    BT_POVHat
End Enum

Public Enum Axes
    AX_Null
    AX_X
    AX_Y
    AX_Z
    AX_Slider
    AX_AX
    AX_AY
    AX_AZ
    AX_ASlider
    AX_ARX
    AX_ARY
    AX_ARZ
    AX_FX
    AX_FY
    AX_FZ
    AX_FSlider
    AX_FRX
    AX_FRY
    AX_FRZ
    AX_RX
    AX_RY
    AX_RZ
    AX_VRX
    AX_VRY
    AX_VRZ
    AX_VX
    AX_VY
    AX_VZ
    AX_VSlider
    AX_Max
End Enum

Dim m_Caps As DIDEVCAPS
Dim m_DX8 As DirectX8
Dim m_DI8 As DirectInput8
Dim m_DID As DirectInputDevice8
Dim m_DIE As DirectInputEnumDevices8
Dim m_DS As DIJOYSTATE2, m_OldDS As DIJOYSTATE2
Dim m_lngCallback As Long
Dim m_lngBindIndex As Long
Private m_mnuBindings As Fury2Menu
Private m_msgText As Fury2MessageWindow
Private m_msgPrompt As Fury2MessageWindow
Public Engine As Fury2Engine
Public LogKeypresses As Boolean
Public Bindings As Fury2JoystickBindings

Implements DirectXEvent8

Public Function AddBinding(Name, Optional BoundControl As Fury2InputControl = Nothing) As Fury2JoystickBinding
On Error Resume Next
Dim m_jbBinding As Fury2JoystickBinding
    Set m_jbBinding = New Fury2JoystickBinding
    With m_jbBinding
        .SetEngine Engine
        .Name = CStr(Name)
        Set .BindControl = BoundControl
        .Enabled = True
    End With
    Bindings.Add m_jbBinding
    Set AddBinding = m_jbBinding
End Function

Public Function Axes(ByVal Index As Long) As Long
On Error Resume Next
    If m_DID Is Nothing Then Axes = -1: Exit Function
    If Index >= m_Caps.lAxes Then Axes = -1: Exit Function
    With m_DS
        Axes = CLng(Choose(CLng(Index), .X, .Y, .Z, .slider, .ax, .ay, .az, .aslider, .arx, .ary, .arz, _
        .fx, .fy, .fz, .fslider, .frx, .fry, .frz, .Rx, .Ry, .rz, .vrx, .vry, .vrz, .vx, .vy, .vz, .vslider))
    End With
    If Err <> 0 Then Axes = -1
End Function

Public Function AxisName(ByVal Axis As Long) As String
On Error Resume Next
    If Axis < 1 Or Axis >= AX_Max Then
        AxisName = "Unknown Axis"
    Else
        AxisName = CStr(Choose(CLng(Axis), "X Axis", "Y Axis", "Z Axis", "Slider", _
        "A X Axis", "A Y Axis", "A Z Axis", "A Slider", "AR X Axis", "AR Y Axis", "AR Z Axis", _
        "F X Axis", "F Y Axis", "F Z Axis", "F Slider", "FR X Axis", "FR Y Axis", "FR Z Axis", _
        "R X Axis", "R Y Axis", "R Z Axis", "VR X Axis", "VR Y Axis", "VR Z Axis", _
        "V X Axis", "V Y Axis", "V Z Axis", "V Slider"))
    End If
End Function

Public Property Get BindingMenu() As Fury2Menu
    Set BindingMenu = m_mnuBindings
End Property

Public Function Bound(ByVal BindType As Long, ByVal BindIndex As Long) As Boolean
On Error Resume Next
Dim l_bndBinding As Fury2JoystickBinding
    For Each l_bndBinding In Bindings
        If l_bndBinding.BindType = BindType Then
            If l_bndBinding.BindIndex = BindIndex Then
                Bound = True
                Exit Function
            End If
        End If
    Next l_bndBinding
End Function

Public Function ButtonName(ByVal Button As Long) As String
On Error Resume Next
    ButtonName = CStr("Button " + CStr(CLng(Button + 1)))
End Function

Public Function Buttons(ByVal Button As Long) As Long
On Error Resume Next
    If m_DID Is Nothing Then Buttons = -1: Exit Function
    If Button >= m_Caps.lButtons Then Buttons = -1: Exit Function
    With m_DS
        Buttons = .Buttons(CLng(Button))
    End With
End Function

Public Sub CancelConfig()
On Error Resume Next
    With Engine
        .EnableObjectEvents = False
        .UnHookEvents m_msgPrompt
        Bindings(m_lngBindIndex).BindType = BT_Null
        Bindings(m_lngBindIndex).BindIndex = 0
        m_lngBindIndex = 0
        .EnableObjectEvents = True
    End With
    UpdateConfig
    m_mnuBindings.Active = True
End Sub

Public Sub ChangeConfig(Index)
On Error Resume Next
Dim m_lngIndex As Long
    m_lngIndex = CLng(Index)
    If m_lngIndex = -32767 Then
        HideConfig
        Exit Sub
    End If
    m_lngBindIndex = m_lngIndex
    m_mnuBindings.Active = False
    With Bindings(m_lngIndex)
        m_msgPrompt.Text = "Please hit the key, button, or axis you wish to use for:" + vbCrLf + _
        .Name + vbCrLf + _
        "Or hit the Cancel button to reset it."
    End With
    With Engine
        .EnableObjectEvents = False
        .HookEvents m_msgPrompt
        .EnableObjectEvents = True
    End With
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set Bindings = New Fury2JoystickBindings
    Set Bindings.Parent = Me
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set Bindings = Nothing
    If m_lngCallback <> 0 Then
        m_DX8.DestroyEvent m_lngCallback
    End If
    If m_DID Is Nothing Then
    Else
        m_DID.Unacquire
        'm_DID.SetEventNotification 0
    End If
    Set m_DID = Nothing
    Set m_DI8 = Nothing
    Set m_DX8 = Nothing
End Sub

Public Property Get DescriptionWindow() As Fury2MessageWindow
    Set DescriptionWindow = m_msgText
End Property

Private Sub DirectXEvent8_DXCallback(ByVal eventid As Long)
On Error Resume Next
    Poll
End Sub

Private Sub Fury2Plugin_ControlActivate(Control As Engine.Fury2InputControl)
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Engine.Fury2InputControl)
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
End Property

Private Property Set Fury2Plugin_Engine(ByVal RHS As Engine.Fury2Engine)
    Set Engine = RHS
End Property

Private Property Get Fury2Plugin_Engine() As Engine.Fury2Engine
    Set Fury2Plugin_Engine = Engine
End Property

Private Sub Fury2Plugin_Init()
End Sub

Private Sub Fury2Plugin_KeyDown(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_KeyPress(KeyAscii As Long)
End Sub

Private Sub Fury2Plugin_KeyUp(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_MapLoad(Filename As String)
End Sub

Private Sub Fury2Plugin_MouseDown(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Private Sub Fury2Plugin_Redraw()
End Sub

Private Sub Fury2Plugin_Shutdown()
End Sub

Private Sub Fury2Plugin_Startup()
    Startup
End Sub

Private Sub Fury2Plugin_SyncUpdate()
    SyncUpdate
End Sub

Private Sub Fury2Plugin_Unload()
End Sub

Private Sub Fury2Plugin_Update()
End Sub

Public Sub HideConfig()
On Error Resume Next
    With Engine
        .EnableObjectEvents = False
        .UnHookEvents m_mnuBindings
        .UnHookEvents m_msgText
        .EnableObjectEvents = True
        .PopEventHooks
    End With
End Sub

Private Sub InitConfig()
On Error Resume Next
    With Engine
        .EnableObjectEvents = False
        Set m_mnuBindings = Engine.CreateMenu(Array("Bindings"), F2Rect(10, 50, Engine.ScreenWidth - 10, Engine.ScreenHeight - 10, True), "UpdateJoystickConfig", "HideJoystickConfig")
        Set m_msgText = Engine.MessageBox("Binding Description", F2Rect(10, 10, Engine.ScreenWidth - 10, 50, True), , , , , , False)
        Set m_msgPrompt = Engine.MessageBox("Prompt", F2Rect(10, (Engine.ScreenHeight * 0.25), Engine.ScreenWidth - 20, Engine.ScreenHeight * 0.5, False), , "CancelJoystickConfig", , , , False)
        m_msgText.Active = False
        m_msgPrompt.Active = True
        m_mnuBindings.Active = True
        m_msgText.Enabled = False
        .EnableObjectEvents = True
    End With
End Sub

Public Function OldAxes(ByVal Index As Long) As Long
On Error Resume Next
    If m_DID Is Nothing Then OldAxes = -1: Exit Function
    If Index >= m_Caps.lAxes Then OldAxes = -1: Exit Function
    With m_OldDS
        OldAxes = CLng(Choose(CLng(Index), .X, .Y, .Z, .slider, .ax, .ay, .az, .aslider, .arx, .ary, .arz, _
        .fx, .fy, .fz, .fslider, .frx, .fry, .frz, .Rx, .Ry, .rz, .vrx, .vry, .vrz, .vx, .vy, .vz, .vslider))
    End With
    If Err <> 0 Then OldAxes = -1
End Function

Public Function OldButtons(ByVal Button As Long) As Long
On Error Resume Next
    If m_DID Is Nothing Then OldButtons = -1: Exit Function
    If Button >= m_Caps.lButtons Then OldButtons = -1: Exit Function
    With m_OldDS
        OldButtons = .Buttons(CLng(Button))
    End With
End Function

Public Function OldPOVs(ByVal Index As Long) As Long
On Error Resume Next
    If m_DID Is Nothing Then OldPOVs = -1: Exit Function
    If Index >= m_Caps.lPOVs Then OldPOVs = -1: Exit Function
    With m_OldDS
        OldPOVs = .POV(Index)
    End With
End Function

Friend Function OldState() As DIJOYSTATE2
    OldState = m_OldDS
End Function

Sub Poll()
On Error Resume Next
Dim m_lngItem As Long
Dim m_booDone As Boolean
    ProfileStart "Joystick Update"
    m_DID.Poll
    m_DID.GetDeviceStateJoystick2 m_DS
    If m_lngBindIndex = 0 Then
        ' Normal polling mode
        Bindings.Update
    Else
        ' Binding change mode
        For m_lngItem = 0 To m_Caps.lButtons - 1
            If Bound(BT_Button, m_lngItem) Then
            Else
                If Buttons(m_lngItem) > 0 Then
                    Bindings(m_lngBindIndex).BindType = BT_Button
                    Bindings(m_lngBindIndex).BindIndex = m_lngItem
                    m_booDone = True
                End If
            End If
        Next m_lngItem
        For m_lngItem = 1 To AX_Max - 1
            Select Case Axes(m_lngItem)
            Case -1
            Case Is >= c_High
                If Bound(BT_AxisPositive, m_lngItem) Then
                Else
                    Bindings(m_lngBindIndex).BindType = BT_AxisPositive
                    Bindings(m_lngBindIndex).BindIndex = m_lngItem
                    m_booDone = True
                End If
            Case Is <= c_Low
                If Bound(BT_AxisNegative, m_lngItem) Then
                Else
                    Bindings(m_lngBindIndex).BindType = BT_AxisNegative
                    Bindings(m_lngBindIndex).BindIndex = m_lngItem
                    m_booDone = True
                End If
            Case Else
            End Select
        Next m_lngItem
    End If
    If m_booDone Then
        With Engine
            .EnableObjectEvents = False
            .UnHookEvents m_msgPrompt
            m_lngBindIndex = 0
            .EnableObjectEvents = True
        End With
        m_mnuBindings.Active = True
        UpdateConfig
    End If
    ProfileStop "Joystick Update"
End Sub

Public Function POVName(ByVal Direction As Long) As String
On Error Resume Next
    POVName = "POV Hat @ " + CStr(CLng(Direction / 1000)) + " Degrees"
End Function

Public Function POVs(ByVal Index As Long) As Long
On Error Resume Next
    If m_DID Is Nothing Then POVs = -1: Exit Function
    If Index >= m_Caps.lPOVs Then POVs = -1: Exit Function
    With m_DS
        POVs = .POV(Index)
    End With
End Function

Public Property Get PromptWindow() As Fury2MessageWindow
    Set PromptWindow = m_msgPrompt
End Property

Public Sub ShowConfig()
On Error Resume Next
    InitConfig
    UpdateConfig
    With Engine
        .PushEventHooks
        .EnableObjectEvents = False
        .HookEvents m_mnuBindings
        .HookEvents m_msgText
        m_msgText.Active = False
        .EnableObjectEvents = True
    End With
End Sub

Sub Startup()
On Error Resume Next
Dim DIJoyPropRange As DIPROPRANGE
Dim DIJoyPropDead As DIPROPLONG
Dim DIJoyPropSaturation As DIPROPLONG
    Set m_DX8 = New DirectX8
    Set m_DI8 = m_DX8.DirectInputCreate
    Set m_DIE = m_DI8.GetDIDevices(DI8DEVTYPE_JOYSTICK, DIEDFL_ATTACHEDONLY)
    If m_DIE.GetCount = 0 Then
        Engine.DebugOut "No joysticks installed. Joystick plugin inactive."
        Exit Sub
    Else
        Engine.DebugOut "Using joystick 1: " + m_DIE.GetItem(1).GetProductName
    End If
    Set m_DID = m_DI8.CreateDevice(m_DIE.GetItem(1).GetGuidInstance)
    m_DID.SetCommonDataFormat DIFORMAT_JOYSTICK2
    m_DID.SetCooperativeLevel 0, DISCL_BACKGROUND Or DISCL_NONEXCLUSIVE
'    m_lngCallback = m_DX7.CreateEvent(Me)
'    m_DID.SetEventNotification m_lngCallback
    m_DID.Acquire
    With DIJoyPropDead
        .lData = 1000
        .lHow = DIPH_BYOFFSET
        .lObj = DIJOFS_X
        m_DID.SetProperty "DIPROP_DEADZONE", DIJoyPropDead
        .lObj = DIJOFS_Y
        m_DID.SetProperty "DIPROP_DEADZONE", DIJoyPropDead
    End With
    With DIJoyPropSaturation
        .lData = 9500
        .lHow = DIPH_BYOFFSET
        .lObj = DIJOFS_X
        m_DID.SetProperty "DIPROP_SATURATION", DIJoyPropSaturation
        .lObj = DIJOFS_Y
        m_DID.SetProperty "DIPROP_SATURATION", DIJoyPropSaturation
    End With
    With DIJoyPropRange
        .lHow = DIPH_DEVICE
        .lMin = 0
        .lMax = 10000
        m_DID.SetProperty "DIPROP_RANGE", DIJoyPropRange
    End With
    m_DID.GetCapabilities m_Caps
    m_DID.GetDeviceStateJoystick2 m_DS
    m_OldDS = m_DS
End Sub

Friend Function State() As DIJOYSTATE2
    State = m_DS
End Function

Sub SyncUpdate()
On Error Resume Next
    m_OldDS = m_DS
    Poll
End Sub

Public Sub UpdateConfig()
On Error Resume Next
Dim m_lngBindings As Long, m_bndBinding As Fury2JoystickBinding
    With m_mnuBindings
        .Clear
        If Bindings.Count > 0 Then
            m_lngBindings = 1
            For Each m_bndBinding In Bindings
                .Add Bindings(m_lngBindings).Name + " = " + Bindings(m_lngBindings).Binding, , "ChangeJoystickConfig"
                m_lngBindings = m_lngBindings + 1
            Next m_bndBinding
        End If
        .Add "Close Configuration Menu", , "ChangeJoystickConfig", -32767
    End With
End Sub

Private Sub Fury2Plugin_OutputPluginChanged()
End Sub

