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

Public Engine As Fury2Engine
Public Bindings As Fury2JoystickBindings
Attribute Bindings.VB_VarDescription = "The joystick's control bindings."
Private m_ddcCaps As DIDEVCAPS
Private m_devDevice As DirectInputDevice8
Private m_djsState As DIJOYSTATE2, m_djsOldState As DIJOYSTATE2
Private m_lngCallback As Long
Private m_lngBindIndex As Long
Private m_mnuBindings As Fury2Menu
Private m_msgText As Fury2MessageWindow
Private m_msgPrompt As Fury2MessageWindow
Private m_booEnabled As Boolean
Private m_lngLow As Long
Private m_lngHigh As Long

Implements DirectXEvent8

Public Property Get Enabled() As Boolean
Attribute Enabled.VB_Description = "If True, this joystick will poll for updates and fire events."
    Enabled = m_booEnabled
End Property

Public Property Let Enabled(ByVal NewValue As Boolean)
    m_booEnabled = NewValue
End Property

Public Function AddBinding(Name, Optional BoundControl As Fury2InputControl = Nothing) As Fury2JoystickBinding
Attribute AddBinding.VB_Description = "Creates a new joystick binding."
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
Attribute Axes.VB_Description = "Returns the state of a joystick axis."
On Error Resume Next
    If m_devDevice Is Nothing Then Axes = -1: Exit Function
    If Index >= m_ddcCaps.lAxes Then Axes = -1: Exit Function
    With m_djsState
        Axes = CLng(Choose(CLng(Index), .X, .Y, .Z, .Slider, .ax, .ay, .az, .aslider, .arx, .ary, .arz, _
        .fx, .fy, .fz, .fslider, .frx, .fry, .frz, .Rx, .Ry, .rz, .vrx, .vry, .vrz, .vx, .vy, .vz, .vslider))
    End With
    If Err <> 0 Then Axes = -1
End Function

Public Function AxisName(ByVal Axis As Long) As String
Attribute AxisName.VB_Description = "Returns the name of a joystick axis."
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
Attribute BindingMenu.VB_Description = "Creates a menu containing joystick bindings."
    Set BindingMenu = m_mnuBindings
End Property

Public Function Bound(ByVal BindType As Long, ByVal BindIndex As Long) As Boolean
Attribute Bound.VB_Description = "Returns True if the specified binding is enabled."
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
Attribute ButtonName.VB_Description = "Returns the name of a button."
On Error Resume Next
    ButtonName = CStr("Button " + CStr(CLng(Button + 1)))
End Function

Public Function Buttons(ByVal Button As Long) As Long
Attribute Buttons.VB_Description = "Returns the state of a button."
On Error Resume Next
    If m_devDevice Is Nothing Then Buttons = -1: Exit Function
    If Button >= m_ddcCaps.lButtons Then Buttons = -1: Exit Function
    With m_djsState
        Buttons = .Buttons(CLng(Button))
    End With
End Function

Public Sub CancelConfig()
Attribute CancelConfig.VB_Description = "Cancels the joystick configuration mode."
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

Public Sub ChangeConfig(ByVal Index As Long)
Attribute ChangeConfig.VB_Description = "Changes a joystick configuration."
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
    m_booEnabled = True
    Set Bindings = New Fury2JoystickBindings
    Set Bindings.Parent = Me
    m_lngLow = 3000
    m_lngHigh = 7000
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set Bindings = Nothing
    If m_lngCallback <> 0 Then
        Engine.DirectX.DestroyEvent m_lngCallback
    End If
    If m_devDevice Is Nothing Then
    Else
        m_devDevice.Unacquire
    End If
    Set m_devDevice = Nothing
End Sub

Public Property Get DescriptionWindow() As Fury2MessageWindow
Attribute DescriptionWindow.VB_Description = "Returns a message window describing the selected joystick binding."
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
End Sub

Private Sub Fury2Plugin_SyncUpdate()
    SyncUpdate
End Sub

Private Sub Fury2Plugin_Unload()
End Sub

Private Sub Fury2Plugin_Update()
End Sub

Public Sub HideConfig()
Attribute HideConfig.VB_Description = "Hides the joystick configuration."
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
Attribute OldAxes.VB_Description = "Returns the previous state of an axis."
On Error Resume Next
    If m_devDevice Is Nothing Then OldAxes = -1: Exit Function
    If Index >= m_ddcCaps.lAxes Then OldAxes = -1: Exit Function
    With m_djsOldState
        OldAxes = CLng(Choose(CLng(Index), .X, .Y, .Z, .Slider, .ax, .ay, .az, .aslider, .arx, .ary, .arz, _
        .fx, .fy, .fz, .fslider, .frx, .fry, .frz, .Rx, .Ry, .rz, .vrx, .vry, .vrz, .vx, .vy, .vz, .vslider))
    End With
    If Err <> 0 Then OldAxes = -1
End Function

Public Function OldButtons(ByVal Button As Long) As Long
Attribute OldButtons.VB_Description = "Returns the previous state of a button."
On Error Resume Next
    If m_devDevice Is Nothing Then OldButtons = -1: Exit Function
    If Button >= m_ddcCaps.lButtons Then OldButtons = -1: Exit Function
    With m_djsOldState
        OldButtons = .Buttons(CLng(Button))
    End With
End Function

Public Function OldPOVs(ByVal Index As Long) As Long
Attribute OldPOVs.VB_Description = "Returns the previous state of a point-of-view hat."
On Error Resume Next
    If m_devDevice Is Nothing Then OldPOVs = -1: Exit Function
    If Index >= m_ddcCaps.lPOVs Then OldPOVs = -1: Exit Function
    With m_djsOldState
        OldPOVs = .POV(Index)
    End With
End Function

Friend Function OldState() As DIJOYSTATE2
    OldState = m_djsOldState
End Function

Sub Poll()
Attribute Poll.VB_Description = "Polls for updates."
On Error Resume Next
Dim m_lngItem As Long
Dim m_booDone As Boolean
    m_devDevice.Poll
    m_devDevice.GetDeviceStateJoystick2 m_djsState
    If m_lngBindIndex = 0 Then
        ' Normal polling mode
        Bindings.Update
    Else
        ' Binding change mode
        For m_lngItem = 0 To m_ddcCaps.lButtons - 1
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
            Case Is >= m_lngHigh
                If Bound(BT_AxisPositive, m_lngItem) Then
                Else
                    Bindings(m_lngBindIndex).BindType = BT_AxisPositive
                    Bindings(m_lngBindIndex).BindIndex = m_lngItem
                    m_booDone = True
                End If
            Case Is <= m_lngLow
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
End Sub

Public Function POVName(ByVal Direction As Long) As String
Attribute POVName.VB_Description = "Returns the name of a point-of-view hat."
On Error Resume Next
    POVName = "POV Hat @ " + CStr(CLng(Direction / 1000)) + " Degrees"
End Function

Public Function POVs(ByVal Index As Long) As Long
Attribute POVs.VB_Description = "Returns the state of a point-of-view hat."
On Error Resume Next
    If m_devDevice Is Nothing Then POVs = -1: Exit Function
    If Index >= m_ddcCaps.lPOVs Then POVs = -1: Exit Function
    With m_djsState
        POVs = .POV(Index)
    End With
End Function

Public Property Get PromptWindow() As Fury2MessageWindow
Attribute PromptWindow.VB_Description = "Returns a prompt window."
    Set PromptWindow = m_msgPrompt
End Property

Public Sub ShowConfig()
Attribute ShowConfig.VB_Description = "Shows the joystick configuration."
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

Friend Sub Acquire(ByVal Joystick As DirectInputDevice8)
On Error Resume Next
Dim DIJoyPropRange As DIPROPRANGE
Dim DIJoyPropDead As DIPROPLONG
Dim DIJoyPropSaturation As DIPROPLONG
    Set m_devDevice = Joystick
    m_devDevice.SetCommonDataFormat DIFORMAT_JOYSTICK2
    m_devDevice.SetCooperativeLevel 0, DISCL_BACKGROUND Or DISCL_NONEXCLUSIVE
    m_devDevice.Acquire
    With DIJoyPropDead
        .lData = 1000
        .lHow = DIPH_BYOFFSET
        .lObj = DIJOFS_X
        m_devDevice.SetProperty "DIPROP_DEADZONE", DIJoyPropDead
        .lObj = DIJOFS_Y
        m_devDevice.SetProperty "DIPROP_DEADZONE", DIJoyPropDead
    End With
    With DIJoyPropSaturation
        .lData = 9500
        .lHow = DIPH_BYOFFSET
        .lObj = DIJOFS_X
        m_devDevice.SetProperty "DIPROP_SATURATION", DIJoyPropSaturation
        .lObj = DIJOFS_Y
        m_devDevice.SetProperty "DIPROP_SATURATION", DIJoyPropSaturation
    End With
    With DIJoyPropRange
        .lHow = DIPH_DEVICE
        .lMin = 0
        .lMax = 10000
        m_devDevice.SetProperty "DIPROP_RANGE", DIJoyPropRange
    End With
    m_devDevice.GetCapabilities m_ddcCaps
    m_devDevice.GetDeviceStateJoystick2 m_djsState
    m_djsOldState = m_djsState
End Sub

Friend Function State() As DIJOYSTATE2
    State = m_djsState
End Function

Sub SyncUpdate()
On Error Resume Next
    m_djsOldState = m_djsState
    Poll
End Sub

Public Sub UpdateConfig()
Attribute UpdateConfig.VB_Description = "Updates configuration."
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

Public Property Get AxisLow() As Long
Attribute AxisLow.VB_Description = "Returns the minimum value of an axis."
    AxisLow = m_lngLow
End Property

Public Property Get AxisHigh() As Long
Attribute AxisHigh.VB_Description = "Returns the maximum value of an axis."
    AxisHigh = m_lngHigh
End Property

