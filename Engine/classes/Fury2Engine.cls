VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Engine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Game Engine"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Attribute VB_Ext_KEY = "Member0" ,"SoundEngine"
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

Option Explicit

Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Integer

Private Type Fury2Picture
    Visible As Boolean
    DestRect As Fury2Rect
    SourceRect As Fury2Rect
    Picture As Object
    EffectMode As Long
    Alpha As Single
    FadeSpeed As Single
    EndFadeAt As Single
    Fading As Boolean
    EndFadeScript As String
    EffectColor As Long
    StartRect As Fury2Rect
    EndRect As Fury2Rect
    AnimationStart As Single
    AnimationEnd As Single
    EndAnimationScript As String
    Tile As Boolean
End Type

Private Type Fury2Hotspot
    Enabled As Boolean
    x1 As Long
    y1 As Long
    x2 As Long
    y2 As Long
    ClickEvent As Fury2Event
    EnterEvent As Fury2Event
    LeaveEvent As Fury2Event
    Hovering As Boolean
End Type

Option Compare Text

Implements IVirtualFileSaveable
Public Mouse As Fury2Mouse

Public ErrorOccurred As Boolean

Private KeyBindings(0 To 255) As Variant
Private BindState(0 To 255) As Boolean

Private m_booErrorsOff As Boolean, m_booErrorOccurred As Boolean

Private m_booBreak As Boolean, m_booResetClock As Boolean

Public DisableUserInput As Boolean

Public ConsoleText As String, ConsoleHeight As Long, ConsoleOffset As Single, ConsoleSliding As Boolean, ConsoleEndOffset As Single, ConsoleStartOffset As Single, ConsoleEndTime As Single, ConsoleStartTime As Single, ConsoleCaretPos As Long

Public ConsoleVisible As Boolean

Public Joystick As Fury2Joystick

Public ScriptLanguage As String

Public HideLog As Boolean

Public DisableCameraBuffers As Boolean

Public CachedNames As Collection

Public ControlStyle As Long
Attribute ControlStyle.VB_VarDescription = "Specifies the style of keyboard control used by the Fury2Controller object."

Public MaxPathWait As Long
Attribute MaxPathWait.VB_VarDescription = "The maximum number of updates a sprite will wait for a waypoint to be completed before skipping onto the next waypoint."

Public DefaultMessageTop As Single, SmartMessageBoxPositioning As Boolean

Public EnableObjectEvents As Boolean
Attribute EnableObjectEvents.VB_VarDescription = "If this flag is set to True, Fury2Menu and Fury2MessageWindow objects fire events when opened and closed."

Public DefaultFont As String, DefaultWindowSkin As String, DefaultCursor As String

Public RangedProjectileChecks As Boolean
Attribute RangedProjectileChecks.VB_VarMemberFlags = "40"

Private SavedPictures() As Fury2Picture, SavedActivePictureSlots As Long
Private SavedHotspots() As Fury2Hotspot
Private OnscreenPictures() As Fury2Picture
Private Hotspots() As Fury2Hotspot

Public TouchDelay As Long

Public VelocityMultiplier As Single, AnimationMultiplier As Single
Attribute VelocityMultiplier.VB_VarDescription = "The global Movement Speed multiplier. Default value is 1.0."
Attribute AnimationMultiplier.VB_VarDescription = "The global Animation Speed multiplier. Default value is 1.0."

Private Type Fury2QueueEvent
    Type As Long
    SParam1 As Single
    SParam2 As Single
    SParam3 As Single
    SParam4 As Single
    LParam1 As Long
    LParam2 As Long
    LParam3 As Long
    LParam4 As Long
End Type

Private Type Fury2SubthreadState
    Name As String
    Running As Boolean
End Type

Public ClearMapBG As Boolean
Attribute ClearMapBG.VB_VarDescription = "Clears the background of camera buffers every frame even when not necessary."

Private EventQueue() As Fury2QueueEvent

Public EventQueueLength As Long
Attribute EventQueueLength.VB_VarMemberFlags = "40"

Public DisableMoveQueue As Boolean
Attribute DisableMoveQueue.VB_VarMemberFlags = "40"
Public DefaultBGMVolume As Single
Attribute DefaultBGMVolume.VB_VarDescription = "The default volume for BGMs."

Public Party As Fury2Party
Attribute Party.VB_VarDescription = "The current party."

Public HarassCPU As Boolean
Attribute HarassCPU.VB_VarMemberFlags = "40"

Public GameTime As Long, GameTimePaused As Boolean
Attribute GameTime.VB_VarDescription = "The current time in-game."

Public ActivationDistance As Long, ScaleBlocking As Boolean
Attribute ActivationDistance.VB_VarDescription = "The distance in pixels that is allowed for sprite activation to occur. Default value is 12."
Private m_booSmoothScaling As Boolean

Public AutoThrottle As Boolean, DisableMovement As Boolean
Attribute AutoThrottle.VB_VarMemberFlags = "40"
Attribute AutoThrottle.VB_VarDescription = "This flag automatically drops the engine's speed when CPU usage goes above 75%. (Disabled)"

Public CurrentBGM As String, CurrentBGMHandle As Long, OldBGM As String
Attribute CurrentBGM.VB_VarDescription = "Returns the filename of the current BGM."
Attribute CurrentBGMHandle.VB_VarDescription = "Returns the index of the current BGM. Can be used to get a SoundHandle for the BGM."
Attribute OldBGM.VB_VarDescription = "The BGM played previously."

Public AccurateRotation As Boolean

Public EnableTimers As Boolean
Attribute EnableTimers.VB_VarDescription = "The default velocity of jumps."

Public Configuration As Fury2ConfigurationFile

Private EventHookStack As Collection

Public ActivePictureSlots As Long
Attribute ActivePictureSlots.VB_VarMemberFlags = "40"
Attribute ActivePictureSlots.VB_VarDescription = "The number of picture slots currently in use."

Public WindowSkin As Fury2WindowSkin
Public MenuCursor As Fury2Graphic
Public MouseCursor As Fury2Image
Public Font As Fury2Font
Private m_objConstants As Fury2ScriptConstants
Private m_objTransitions As Fury2Transitions
Private m_colTimerEvents As Fury2TimerEvents
#If EnableBehaviors = 1 Then
Private m_objBehavior As Fury2BehaviorConstructor
#End If

Public MessageQueue As Fury2Collection
Attribute MessageQueue.VB_VarDescription = "The current message box queue."

Public MessageWindows As Fury2Collection
Attribute MessageWindows.VB_VarDescription = "The message windows currently active."

Public SpriteObjectCount As Long, AreaObjectCount As Long, MapObjectCount As Long, TimerEventObjectCount As Long
Attribute SpriteObjectCount.VB_VarMemberFlags = "40"
Public HTTPRequestCount As Long

Public IObjThis As Object
Attribute IObjThis.VB_VarMemberFlags = "40"

Private m_imgBackbuffer As Fury2Image

Private m_objOutputPlugin As Object
Public OutputPlugin As String, VSync As Boolean
Attribute OutputPlugin.VB_VarDescription = "The name of the output plugin."

Public VelocityDecay As Single
Attribute VelocityDecay.VB_VarDescription = "The speed at which a sprite's velocity decays."

Public DisableSound As Boolean
Attribute DisableSound.VB_VarDescription = "This flag disables the sound engine entirely (It does not even load)."
Public SoundListener As Fury2Sprite

Public StartMap As String, StartX As Single, StartY As Single, StartLayer As Long
Attribute StartMap.VB_VarDescription = "The map loaded at startup."
Attribute StartX.VB_VarDescription = "The X of the player on the startup map."
Attribute StartY.VB_VarDescription = "The X of the player on the startup map."
Attribute StartLayer.VB_VarDescription = "The default layer for the player sprite(s)."

Public HideConfiguration As Boolean
Attribute HideConfiguration.VB_VarDescription = "This flag controls whether the configuration dialog is shown at startup, and must be set in settings.vbs to have any effect."

Public ClearEveryFrame As Boolean, BackgroundColor As Long
Attribute ClearEveryFrame.VB_VarDescription = "This flag causes the engine to clear the backbuffer to black every frame. Usually not necessary if a camera is visible, so defaults to off to increase speed."

Public Controls As Fury2Collection
Public RepeatDelay As Long

Private m_colPluginStack As Collection
Private m_colReversePluginStack As Collection

Private m_Icon As String

Private m_Flags As Collection, m_FlagNames As Collection

Public Timers As Fury2Collection

Public OutputGamma As Long
Attribute OutputGamma.VB_VarDescription = "The gamma value used for screen output. (Slow! Use Camera.Gamma instead.)"

Public DirectRender As Boolean
Public SpecialFX As Boolean, GameName As String, DesiredFramerate As Long
Attribute SpecialFX.VB_VarDescription = "The detail level setting for the engine. This automatically controls certain effects, like transitions."
Public WindowCaption As String, BalanceFramerate As Boolean
Attribute WindowCaption.VB_VarDescription = "The caption of the output window."
Attribute BalanceFramerate.VB_VarDescription = "This flag enables framerate balancing. Framerate is set by the 'DesiredFramerate' property."
Public ShowFPS As Boolean, ShowTitlebar As Boolean
Attribute ShowFPS.VB_VarDescription = "This flag controls whether the current FPS is shown in the titlebar."
Attribute ShowTitlebar.VB_VarDescription = "This flag controls whether the titlebar is shown."
Public ScreenWidth As Long, ScreenHeight As Long, BitDepth As Long, Fullscreen As Boolean, ScreenScaleRatio As Single, ScreenScaleMode As Long
Attribute ScreenWidth.VB_VarDescription = "The width of the screen (output)."
Public FPS As Long, MaxFrameskip As Long
Attribute FPS.VB_VarDescription = "Returns the current actual rendering framerate."
Public LogToClipboard As Boolean

Public Terminating As Boolean, TerminateEngine As Boolean

Private m_KeyState(0 To 255) As Boolean
Private m_KeyMap(0 To 255) As Byte

Public Paused As Boolean
Attribute Paused.VB_VarDescription = "This flag pauses the entire engine's updating routines."
Public Running As Boolean
Attribute Running.VB_VarDescription = "This flag determines whether the engine is running. Try setting it to False for some fun!"
Private WithEvents m_ScriptEngine As ScriptEngine
Attribute m_ScriptEngine.VB_VarHelpID = -1
Private m_SoundEngine As SoundEngine
Attribute m_SoundEngine.VB_VarHelpID = -1
Private m_Images As Collection
Private mvarMaps As Fury2Maps
Private mvarCameras As Fury2Cameras
Private m_Graphics As Fury2GEGlobal, m_Globals As Fury2Globals
Public FrameCount As Long
Attribute FrameCount.VB_VarDescription = "Returns the number of frames that have been rendered since the engine was started."

Private Subthreads() As Fury2SubthreadState
Public SubthreadCount As Long

Public Map As Fury2Map
Attribute Map.VB_VarDescription = "Returns the current map being updated."

'Public ScriptContext As String
Private m_strScriptContext() As String
Private m_lngContextDepth As Long
Private m_colContextStack As New Collection

Public KeysEntered As String, TrapKeys As Boolean

Public DefaultTransition As Long, DefaultTransitionLength As Single
Attribute DefaultTransition.VB_VarDescription = "The default transition used for map changes."

Public RedrawHook As Fury2Event, UpdateHook As Fury2Event
Attribute RedrawHook.VB_VarMemberFlags = "40"
Public MouseDownHook As Fury2Event, MouseMoveHook As Fury2Event, MouseUpHook As Fury2Event, MouseDblClickHook As Fury2Event
Public SavedMaps As Fury2Collection
Attribute SavedMaps.VB_VarDescription = "The saved map state array."

Public Filesystem As Fury2Filesystem

Public CurrentGame As String

Public LastFrameStart As Double, LastFrameEnd As Double
Public LastSecond As Double, FrameBuildup As Double
Public FrameLength As Double
Private NextFPS As Long
Private OldVolume As Single

Private RanStartEvent As Boolean

Private m_imgMouseBuffer As Fury2Image
Public ConsoleItems As Fury2Collection
Private m_objNotify As Object, m_booNotifyNoLogging As Boolean

Public LogDebugMessages As Boolean
Public Debugger As Fury2Debugger
Public Halted As Boolean
Public EditMode As Boolean

Public Sub SetNotify(ByRef Obj As Object)
On Error Resume Next
    Set m_objNotify = Obj
    m_booNotifyNoLogging = False
End Sub

Sub AddObject(ByRef Name As String, ByVal Obj As Object)
Attribute AddObject.VB_Description = "Adds a command object to the script engine."
On Error Resume Next
    m_ScriptEngine.AddObject Name, Obj
End Sub

Function AddTextElement(Text, ByVal X As Long, ByVal Y As Long, Optional ByVal Realtime As Boolean = False) As Fury2TextElement
Attribute AddTextElement.VB_Description = "Creates a new text element and adds it to the plugin stack."
On Error Resume Next
Dim m_teNew As Fury2TextElement
    Set m_teNew = New Fury2TextElement
    With m_teNew
        .Realtime = Realtime
        .Text = ToString(Text)
        .Color = F2White
        .Alpha = 1
        Set .Font = Me.Font
        Set .Rectangle = F2Rect(X, Y, Backbuffer.Width, Backbuffer.Height, True)
    End With
    HookEvents m_teNew
    Set AddTextElement = m_teNew
End Function

Function AddTextElementRealtime(Text, ByVal X As Long, ByVal Y As Long) As Fury2TextElement
Attribute AddTextElementRealtime.VB_MemberFlags = "40"
On Error Resume Next
    Set AddTextElementRealtime = AddTextElement(Text, X, Y, True)
End Function

Public Function Alphabet() As String
Attribute Alphabet.VB_Description = "Returns a string of letters useful for testing."
    Alphabet = "abcdefghijklmnopqrstuvwxyz"
End Function

Public Function AngleBetween(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single) As Single
Attribute AngleBetween.VB_Description = "Returns an angle (0-360) usable to navigate to a point."
Dim Rx As Single, Ry As Single
On Error Resume Next
    Rx = (x2 - x1)
    Ry = (y2 - y1)
    If Rx = 0 And Ry = 0 Then
        AngleBetween = 0
        Exit Function
    ElseIf Ry = 0 Then
        If Rx > 0 Then
            AngleBetween = 90
        Else
            AngleBetween = 270
        End If
        Exit Function
    ElseIf Rx = 0 Then
        If Ry > 0 Then
            AngleBetween = 180
        Else
            AngleBetween = 0
        End If
        Exit Function
    End If
    If Ry < 0 And Rx < 0 Then
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (270)
    ElseIf Ry >= 0 And Rx < 0 Then
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (270)
    ElseIf Ry < 0 And Rx >= 0 Then
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (90)
    Else
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (90)
    End If
    AngleBetween = NormalizeAngle(AngleBetween)
End Function

Public Function AngleInRange(ByVal Angle As Single, ByVal Minimum As Single, ByVal Maximum As Single) As Boolean
On Error Resume Next
Dim AngleDelta As Single
    Minimum = NormalizeAngle(Minimum)
    Maximum = NormalizeAngle(Maximum)
    If Minimum > Maximum Then
        AngleDelta = Minimum - Maximum
        Minimum = NormalizeAngle(Minimum + AngleDelta)
        Maximum = NormalizeAngle(Maximum + AngleDelta)
        Angle = NormalizeAngle(Angle + AngleDelta)
    Else
        Angle = NormalizeAngle(Angle)
    End If
    AngleInRange = (Angle >= Minimum) And (Angle <= Maximum)
End Function

Public Sub AnimatePicture(ByVal Slot As Long, Optional ByVal Length As Single = 1, Optional ByVal NewX As Long = -32767, Optional ByVal NewY As Long = -32767, Optional ByVal NewWidth As Long = -32767, Optional ByVal NewHeight As Long = -32767, Optional ByRef EndScript = "")
On Error Resume Next
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With OnscreenPictures(Slot)
        .AnimationStart = HiTimer
        .AnimationEnd = HiTimer + Length
        Set .StartRect = .DestRect.Copy
        Set .EndRect = .DestRect.Copy
        If NewX <> -32767 Then .EndRect.RelLeft = NewX
        If NewY <> -32767 Then .EndRect.RelTop = NewY
        If NewWidth <> -32767 Then .EndRect.Width = NewWidth
        If NewHeight <> -32767 Then .EndRect.Height = NewHeight
        .EndAnimationScript = ToString(EndScript)
    End With
End Sub

Sub ArrayAdd(Arr As Variant, Value)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long
Dim m_varNull As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    Err.Clear
    If (VarType(Value) And vbArray) = vbArray Then
        m_lngUB = m_lngUB + (UBound(Value) - LBound(Value) + 1)
        ReDim Preserve Arr(m_lngLB To m_lngUB) As Variant
        ArrayCopy Arr, Value, m_lngUB - (UBound(Value) - LBound(Value) + 1), 0, (UBound(Value) - LBound(Value) + 1)
    Else
        m_lngUB = m_lngUB + 1
        ReDim Preserve Arr(m_lngLB To m_lngUB) As Variant
        ArrayAssign Arr(m_lngUB), Value
    End If
    Err.Clear
End Sub

Sub ArrayAssign(ByRef aTo, ByRef aFrom)
On Error Resume Next
    If VarType(aFrom) = vbObject Then
        Set aTo = aFrom
    Else
        aTo = aFrom
    End If
End Sub

Sub ArrayCopy(Target As Variant, Source As Variant, Optional ByVal Position As Long = 0, Optional ByVal SourcePosition As Long = 0, Optional ByVal Count As Long = -1)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngSourceLB As Long, m_lngSourceUB As Long
Dim m_lngCopy As Long
    Err.Clear
    If (VarType(Target) And vbArray) <> vbArray Then Exit Sub
    If (VarType(Source) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Target)
    m_lngUB = UBound(Target)
    m_lngSourceLB = LBound(Source)
    m_lngSourceUB = UBound(Source)
    If Count = -1 Then Count = (m_lngSourceUB - m_lngSourceLB + 1)
    If Position < m_lngLB Then
        Count = Count - (m_lngLB - Position)
        Position = m_lngLB
    End If
    If Position > m_lngUB Then
        Count = 1
        Position = m_lngUB
    End If
    If SourcePosition < m_lngSourceLB Then
        Count = Count - (m_lngSourceLB - SourcePosition)
        SourcePosition = m_lngSourceLB
    End If
    If SourcePosition > m_lngSourceUB Then
        Count = 1
        SourcePosition = m_lngSourceUB
    End If
    If Count > (m_lngUB - Position) + 1 Then
        Count = (m_lngUB - Position) + 1
    End If
    If Count > (m_lngSourceUB - SourcePosition) + 1 Then
        Count = (m_lngSourceUB - SourcePosition) + 1
    End If
    If Count < 1 Then
        Exit Sub
    End If
    For m_lngCopy = 0 To Count - 1
        ArrayAssign Target(m_lngCopy + Position), Source(m_lngCopy + SourcePosition)
    Next m_lngCopy
    Err.Clear
End Sub

Function ArrayCount(Arr As Variant, Value) As Long
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    ArrayCount = 0
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Function
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Function
    For m_lngItems = m_lngLB To m_lngUB
        If Arr(m_lngItems) = Value Then
            ArrayCount = ArrayCount + 1
        End If
    Next m_lngItems
    Err.Clear
End Function

Function ArrayFind(Arr As Variant, ByRef Value) As Long
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
Dim l_objValue As Object
    ArrayFind = -1
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Function
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Function
    Set l_objValue = Value
    If l_objValue Is Nothing Then
        For m_lngItems = m_lngLB To m_lngUB
            If Arr(m_lngItems) = Value Then
                ArrayFind = m_lngItems
                Exit Function
            End If
        Next m_lngItems
    Else
        For m_lngItems = m_lngLB To m_lngUB
            If Arr(m_lngItems) Is l_objValue Then
                ArrayFind = m_lngItems
                Exit Function
            End If
        Next m_lngItems
    End If
    Err.Clear
End Function

Sub ArrayInsert(Arr As Variant, Value, ByVal Index As Long)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then
        ReDim Arr(0 To Index)
        m_lngUB = Index
        ArrayAssign Arr(m_lngUB), Value
        Exit Sub
    End If
    Err.Clear
    If Index < m_lngLB Then Exit Sub
    If Index > (m_lngUB + 1) Then
        ReDim Preserve Arr(m_lngLB To Index)
        m_lngUB = Index
    Else
        ReDim Preserve Arr(m_lngLB To m_lngUB + 1)
        m_lngUB = m_lngUB + 1
    End If
    If Index = m_lngLB Then
        For m_lngItems = m_lngUB - 1 To m_lngLB Step -1
            ArrayAssign Arr(m_lngItems + 1), Arr(m_lngItems)
        Next m_lngItems
    ElseIf Index = m_lngUB Then
    Else
        For m_lngItems = m_lngUB - 1 To Index Step -1
            ArrayAssign Arr(m_lngItems + 1), Arr(m_lngItems)
        Next m_lngItems
    End If
    ArrayAssign Arr(Index), Value
    Err.Clear
End Sub

Function ArrayJoin(ParamArray Arrays() As Variant)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long
Dim m_lngArrays As Long, m_lngSize As Long
Dim m_varArray As Variant, m_lngPosition As Long
    m_lngLB = LBound(Arrays)
    m_lngUB = UBound(Arrays)
    m_lngSize = 0
    For m_lngArrays = m_lngLB To m_lngUB
        m_lngSize = m_lngSize + (UBound(Arrays(m_lngArrays)) - LBound(Arrays(m_lngArrays)) + 1)
    Next m_lngArrays
    m_varArray = Array()
    ReDim m_varArray(0 To m_lngSize - 1)
    m_lngPosition = 0
    For m_lngArrays = m_lngLB To m_lngUB
        ArrayCopy m_varArray, Arrays(m_lngArrays), m_lngPosition, 0
        m_lngPosition = m_lngPosition + (UBound(Arrays(m_lngArrays)) - LBound(Arrays(m_lngArrays)) + 1)
    Next m_lngArrays
    ArrayJoin = m_varArray
End Function

Sub ArrayRemove(Arr As Variant, ByVal Index As Long)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If Index < m_lngLB Then Exit Sub
    If Index > m_lngUB Then Exit Sub
    If m_lngLB >= m_lngUB Then
        Erase Arr
        Exit Sub
    End If
    If Index = m_lngLB Then
        For m_lngItems = m_lngLB + 1 To m_lngUB
            Arr(m_lngItems - 1) = Arr(m_lngItems)
        Next m_lngItems
    ElseIf Index = m_lngUB Then
    Else
        For m_lngItems = Index + 1 To m_lngUB
            Arr(m_lngItems - 1) = Arr(m_lngItems)
        Next m_lngItems
    End If
    ReDim Preserve Arr(m_lngLB To m_lngUB - 1)
    Err.Clear
End Sub

Sub ArrayReplace(Arr As Variant, Match, Replace)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    For m_lngItems = m_lngLB To m_lngUB
        If Arr(m_lngItems) = Match Then ArrayAssign Arr(m_lngItems), Replace
    Next m_lngItems
    Err.Clear
End Sub

Sub ArrayReplaceEx(Arr As Variant, MatchFunction As String, ReplaceFunction As String)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    For m_lngItems = m_lngLB To m_lngUB
        If CBool(m_ScriptEngine.Exec(MatchFunction, Arr(m_lngItems))) Then
            m_ScriptEngine.Exec ReplaceFunction, Arr(m_lngItems)
        End If
    Next m_lngItems
    Err.Clear
End Sub

Sub ArrayResize(Arr As Variant, ByVal NewSize)
On Error Resume Next
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    ReDim Preserve Arr(0 To CLng(NewSize) - 1)
    Err.Clear
End Sub

Function ArraySlice(Source As Variant, ByVal Length As Long, Optional ByVal Start As Long = 0)
On Error Resume Next
Dim l_varNew As Variant
    ReDim l_varNew(0 To Length - 1)
    ArrayCopy l_varNew, Source, 0, Start, Length
    ArraySlice = l_varNew
    Err.Clear
End Function

Sub ArraySort(Arr As Variant, Optional ByVal Descending As Boolean = False)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim P As Long, K As Long, H As Long, i As Long, J As Long
Dim Temp As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If m_lngCount < 2 Then Exit Sub
    For P = m_lngLB To ClipValue(m_lngUB - 1, m_lngLB, m_lngUB)
        H = P
        If Descending Then
            For K = P + 1 To m_lngUB
                If Arr(K) > Arr(H) Then H = K
            Next K
        Else
            For K = P + 1 To m_lngUB
                If Arr(K) < Arr(H) Then H = K
            Next K
        End If
        If P <> H Then
            i = H
            J = P
            ArrayAssign Temp, Arr(i)
            ArrayAssign Arr(i), Arr(J)
            ArrayAssign Arr(J), Temp
        End If
    Next P
    Err.Clear
End Sub

Sub ArraySortEx(Arr As Variant, Optional TestFunction As String)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim P As Long, K As Long, H As Long, i As Long, J As Long
Dim Temp As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If m_lngCount < 2 Then Exit Sub
    For P = m_lngLB To ClipValue(m_lngUB - 1, m_lngLB, m_lngUB)
        H = P
        For K = P + 1 To m_lngUB
            If CBool(m_ScriptEngine.Exec(TestFunction, Arr(K), Arr(H))) Then H = K
        Next K
        If P <> H Then
            i = H
            J = P
            Temp = Arr(i)
            Arr(i) = Arr(J)
            Arr(J) = Temp
        End If
    Next P
    Err.Clear
End Sub

Function ArraySum(Arr As Variant) As Variant
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
Dim m_varValue As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Function
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Function
    For m_lngItems = m_lngLB To m_lngUB
        m_varValue = m_varValue + Arr(m_lngItems)
    Next m_lngItems
    ArraySum = m_varValue
    Err.Clear
End Function

Public Property Get Backbuffer() As Fury2Image
Attribute Backbuffer.VB_Description = "The engine's backbuffer."
On Error Resume Next
    Set Backbuffer = m_imgBackbuffer
End Property

Sub BeginTrace()
    #If DebugFeatures = 1 Then
    m_booTrace = True
    #End If
End Sub

#If EnableBehaviors = 1 Then
Public Property Get Behavior() As Fury2BehaviorConstructor
    Set Behavior = m_Behavior
End Property
#End If

Public Property Get BGMVolume() As Single
On Error Resume Next
    BGMVolume = m_SoundEngine.Handles(CurrentBGMHandle).Volume
End Property

Public Property Let BGMVolume(ByVal Volume As Single)
On Error Resume Next
    m_SoundEngine.Handles(CurrentBGMHandle).Volume = Volume
    Err.Clear
End Property

Public Function NullEvent() As Fury2Event
On Error Resume Next
'    Set NullEvent = New Fury2Event
'    NullEvent.SetEngine Me
End Function

Public Function BindEvent(ByRef Method, Optional Object = Nothing, Optional BindParameters As Variant = Nothing) As Fury2Event
On Error Resume Next
    If VarType(Method) = vbString Then
        If Len(Trim(ToString(Method))) = 0 Then Exit Function
        Method = ToString(Method)
        If InStr(Method, " ") Then
            BindParameters = m_ScriptEngine.Eval("Array(" & Mid(Method, InStr(Method, " ") + 1) & ")")
            Method = Left(Method, InStr(Method, " ") - 1)
        End If
        If VarType(Object) = vbString Then
        Else
            If Object Is Nothing Then
                Set Object = m_ScriptEngine.Namespace
            End If
        End If
        Set BindEvent = New Fury2Event
        BindEvent.SetEngine Me
        BindEvent.Bind ToString(Method), Object, BindParameters
        If Debugger Is Nothing Then Else Debugger.EventBound BindEvent
    Else
        If Method Is Nothing Then
            Set BindEvent = NullEvent
        ElseIf TypeOf Method Is Fury2Event Then
            Set BindEvent = Method
        End If
        Exit Function
    End If
End Function

Sub BindKey(KeyCode, Binding)
Attribute BindKey.VB_Description = "Attaches a default script to a key. Key bindings are executed right after the Engine_KeyDown event, but only if the event and all plugins don't block it."
On Error Resume Next
Dim m_lngKeyCode As Long
Dim l_ctlControl As Fury2InputControl
Dim l_evtEvent As Fury2Event
    If VarType(KeyCode) = vbString Then
        m_lngKeyCode = Asc(ToString(KeyCode))
    Else
        m_lngKeyCode = CLng(KeyCode)
    End If
    If m_lngKeyCode < 1 Then Exit Sub
    If m_lngKeyCode > 255 Then Exit Sub
    If VarType(Binding) = vbObject Then
        If TypeOf Binding Is Fury2InputControl Then
            Set l_ctlControl = Binding
            Set KeyBindings(m_lngKeyCode) = l_ctlControl
        ElseIf TypeOf Binding Is Fury2Event Then
            Set l_evtEvent = Binding
            Set KeyBindings(m_lngKeyCode) = l_evtEvent
        End If
    ElseIf VarType(Binding) = vbString Then
        KeyBindings(m_lngKeyCode) = ToString(Binding)
    End If
    Err.Clear
End Sub

Public Sub Break(Optional ByVal Name As String = "")
On Error Resume Next
Dim l_lngSubthreads As Long
    If Trim(Name) = "" Then
        m_booBreak = True
    Else
        For l_lngSubthreads = UBound(Subthreads) To LBound(Subthreads) Step -1
            With Subthreads(l_lngSubthreads)
                If LCase(Trim(.Name)) = LCase(Trim(Name)) Then
                    If .Running Then
                        .Running = False
                        Exit For
                    End If
                End If
            End With
        Next l_lngSubthreads
    End If
End Sub

Function CacheScript(Filename As String, Script As String) As String
On Error Resume Next
Dim m_csScript As Fury2ScriptCache
    CacheScript = FindScript(Filename)
    If Len(CacheScript) < 1 Then
        Set m_csScript = New Fury2ScriptCache
        m_csScript.Name = VariableName(Filename)
        m_csScript.Filename = Filename
        CachedNames.Add m_csScript, LCase(Trim(Filename))
        CacheScript = m_csScript.Name
        ScriptEngine.AddCode Script
    End If
End Function

Function CallByName(ByVal Obj As Object, ByRef Member As String, ByVal CallType As Long, ParamArray Parameters() As Variant)
On Error Resume Next
    Select Case UBound(Parameters)
    Case 0
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0))
    Case 1
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0), Parameters(1))
    Case 2
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0), Parameters(1), Parameters(2))
    Case 3
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0), Parameters(1), Parameters(2), Parameters(3))
    Case 4
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4))
    Case 5
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5))
    Case 6
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6))
    Case 7
        CallByName = VBA.CallByName(Obj, Member, CallType, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6), Parameters(7))
    Case Else
        CallByName = VBA.CallByName(Obj, Member, CallType)
    End Select
End Function

Public Property Get Camera() As Fury2Camera
Attribute Camera.VB_Description = "The default camera. (Equal to Cameras(1))"
    Set Camera = Cameras(1)
End Property

Public Property Get Cameras() As Fury2Cameras
Attribute Cameras.VB_Description = "The engine's camera array. Usually contains one camera."
    Set Cameras = mvarCameras
End Property

Public Sub ChangePicture(ByVal Slot As Long, ByVal NewPic As Object)
Attribute ChangePicture.VB_Description = "Changes the picture in a picture slot."
On Error Resume Next
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With OnscreenPictures(Slot)
        Set .Picture = NewPic
    End With
End Sub

Public Function CharIsPrintable(ByVal Index As Long) As Boolean
On Error Resume Next
    CharIsPrintable = False
    Select Case Index
    Case Else
        If Index >= 32 Then
            CharIsPrintable = True
        Else
            CharIsPrintable = False
        End If
    End Select
    Err.Clear
End Function

Friend Sub SetMouseLocation(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
Dim l_lngBorderWidth As Long, l_lngTitleHeight As Long, l_lngOldMode As Long
Dim l_lngWidth As Long, l_lngHeight As Long
    With m_objOutputPlugin.Window
        l_lngOldMode = .ScaleMode
        .ScaleMode = 1
        l_lngBorderWidth = (.Width - .ScaleWidth) \ 2
        l_lngTitleHeight = (.Height - .ScaleHeight) - (l_lngBorderWidth * 2)
        .ScaleMode = l_lngOldMode
        SetCursorPos (X * (.ScaleWidth / ScreenWidth)) + (l_lngBorderWidth / Screen.TwipsPerPixelX) + (.Left / Screen.TwipsPerPixelX), (Y * (.ScaleHeight / ScreenHeight)) + ((l_lngBorderWidth + l_lngTitleHeight) / Screen.TwipsPerPixelY) + (.Top / Screen.TwipsPerPixelY)
    End With
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    m_booIDE = InIDE
    RanStartEvent = False
    ReDim OnscreenPictures(0 To c_lngMaxPictureSlot)
    ReDim SavedPictures(0 To c_lngMaxPictureSlot)
    ReDim Hotspots(0 To c_lngMaxHotspot)
    ReDim SavedHotspots(0 To c_lngMaxHotspot)
    ReDim m_strScriptContext(0 To 0)
    m_strScriptContext(0) = "Fury²"
    DefaultTransition = TR_None
    DefaultTransitionLength = 1
    PushPictures
    PushHotspots
    ReDim EventQueue(0 To c_lngMaxEventQueue)
    DefaultBGMVolume = 0.65
    ActivationDistance = c_lngDefaultActivationDistance
    MaxPathWait = c_lngDefaultMaxPathWait
    ScriptLanguage = "VBScript"
    MaxFrameskip = c_lngMaxFrameskip
    OutputPlugin = "GDI"
    SubthreadCount = -1
    VelocityMultiplier = 1
    AnimationMultiplier = 1
    VelocityDecay = c_sngDefaultVelocityDecay
    RepeatDelay = c_lngDefaultRepeatDelay
    EnableTimers = True
    ClearMapBG = True
    StartLayer = 1
    TouchDelay = 20
    BitDepth = 32
    DisableCameraBuffers = True
    DisableSound = False
    SpecialFX = True
    ScreenWidth = 320
    ScreenHeight = 240
    ScreenScaleRatio = 1
    ShowTitlebar = True
    ShowFPS = True
    HarassCPU = True
    ResetKeyMappings
    InitDefaultControls
    Set Filesystem = New Fury2Filesystem
    Set Filesystem.OutputHook = Me
    Filesystem.Root = "."
    Set Configuration = New Fury2ConfigurationFile
    Configuration.SetEngine Me
    Configuration.InitDefaultSettings
    Set CachedNames = New Collection
    Set EventHookStack = New Collection
    Set Timers = F2Collection()
    Set MessageQueue = F2Collection()
    Set MessageWindows = F2Collection()
    Set Mouse = New Fury2Mouse
    Mouse.SetEngine Me
    Set m_colPluginStack = New Collection
    Set m_colReversePluginStack = New Collection
    Set m_Images = New Collection
    Set m_Graphics = New Fury2GEGlobal
    Set m_Globals = New Fury2Globals
    Set mvarMaps = New Fury2Maps
    Set mvarMaps.Engine = Me
    Set m_objConstants = New Fury2ScriptConstants
    Set m_objTransitions = New Fury2Transitions
    m_objTransitions.SetEngine Me
    Set m_colTimerEvents = New Fury2TimerEvents
    m_colTimerEvents.SetEngine Me
#If EnableBehaviors = 1 Then
    Set m_Behavior = New Fury2BehaviorConstructor
#End If
    Set mvarCameras = New Fury2Cameras
    Set m_SoundEngine = New SoundEngine
    Set m_SoundEngine.m_Engine = Me
    m_SoundEngine.Init
    Set Font = New Fury2Font
    Font.SetEngine Me
    Set WindowSkin = New Fury2WindowSkin
    WindowSkin.SetEngine Me
    Set m_Flags = New Collection
    Set m_FlagNames = New Collection
    Set Party = New Fury2Party
    Party.SetEngine Me
    Set SavedMaps = F2Collection()
    Paused = False
    If EditMode Then
        Running = False
        InitScriptEngine
    Else
        Running = True
        InitScriptEngine
    End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    EmptyImageCache
    m_SoundEngine.Shutdown
    Set ScriptEngine = Nothing
    Set m_SoundEngine = Nothing
    Set Filesystem = Nothing
    Set mvarCameras = Nothing
    Set m_Flags = Nothing
    Set m_FlagNames = Nothing
    Set SavedMaps = Nothing
    Set Party = Nothing
    Set Font = Nothing
    Set WindowSkin = Nothing
    Set EventHookStack = Nothing
    Set Timers = Nothing
    Set MessageQueue = Nothing
    Set MessageWindows = Nothing
    Set m_colPluginStack = Nothing
    Set m_colReversePluginStack = Nothing
    Set m_Images = Nothing
    Set m_Graphics = Nothing
    Set m_Globals = Nothing
    Set mvarMaps = Nothing
    Set m_objConstants = Nothing
    Set m_objTransitions = Nothing
    Set m_colTimerEvents = Nothing
    Set Configuration = Nothing
End Sub

Public Sub ClearConsole()
On Error Resume Next
    ConsoleItems.Clear
    Err.Clear
End Sub

Public Property Get ClipboardText() As String
On Error Resume Next
    If Clipboard.GetFormat(vbCFText) Then
        ClipboardText = Clipboard.GetText(vbCFText)
    End If
    Err.Clear
End Property

Public Property Let ClipboardText(ByRef NewText As String)
On Error Resume Next
    Clipboard.Clear
    Clipboard.SetText NewText, vbCFText
    Err.Clear
End Property

Public Function ClipInteger(ByVal Number As Long, ByVal Min As Long, ByVal Max As Long) As Long
On Error Resume Next
    ClipInteger = ClipValue(Number, Min, Max)
End Function

Public Function ClipNumber(ByVal Number As Double, ByVal Min As Double, ByVal Max As Double) As Double
Attribute ClipNumber.VB_Description = "Clips a number into the range of Min-Max."
On Error Resume Next
Dim l_dblMin As Double, l_dblMax As Double
    ClipNumber = Number
    If ClipNumber < Min Then ClipNumber = Min
    If ClipNumber > Max Then ClipNumber = Max
End Function

Public Property Get Colin() As Variant
On Error Resume Next
    Colin = "Once the creator learns a VB data type other than variant for parameters (no joke!) then we'll be talking :p"
End Property

Public Sub ContextLevelAdd(ByRef Name As String)
Attribute ContextLevelAdd.VB_MemberFlags = "40"
On Error Resume Next
    #If DebugFeatures = 1 Then
    m_lngContextDepth = m_lngContextDepth + 1
    ReDim Preserve m_strScriptContext(0 To m_lngContextDepth)
    m_strScriptContext(m_lngContextDepth) = Name
    If m_booTrace = True Then
        Trace ScriptContext
    End If
    #End If
End Sub

Public Sub ContextLevelChange(ByRef NewValue As String)
Attribute ContextLevelChange.VB_MemberFlags = "40"
On Error Resume Next
    #If DebugFeatures = 1 Then
        m_strScriptContext(m_lngContextDepth) = NewValue
    #End If
End Sub

Public Sub ContextLevelRemove()
Attribute ContextLevelRemove.VB_MemberFlags = "40"
On Error Resume Next
    #If DebugFeatures = 1 Then
    m_strScriptContext(m_lngContextDepth) = ""
    m_lngContextDepth = m_lngContextDepth - 1
    ReDim Preserve m_strScriptContext(0 To m_lngContextDepth)
    If m_booTrace = True Then
        Trace ScriptContext
    End If
    #End If
End Sub

Public Sub ControlActivate(Control As Fury2InputControl)
On Error Resume Next
Dim Obj As Object
Dim PObj As Fury2Plugin
    If (Not Control.State) Then ContextLevelRemove: Exit Sub
    If Control.Handled Then
        Debug.Print "Recieved already handled input event: " & Control.Name & " Activated"
        Exit Sub
    End If
    Control.Handled = True
    ContextLevelAdd "ControlActivate"
    Control.Consumed = False
    Err.Clear
    For Each Obj In m_colPluginStack
        Set PObj = Nothing
        Set PObj = Obj
        Err.Clear
        If PObj Is Nothing Then
            Obj.ControlActivate Control
        Else
            PObj.ControlActivate Control
        End If
        If Control.Consumed Then
            Control.Consumed = False
            ContextLevelRemove
            Exit Sub
        End If
        If Not Control.State Then ContextLevelRemove: Exit Sub
    Next Obj
    Err.Clear
    m_ScriptEngine.Exec "Engine_ControlActivate", Control
    Err.Clear
    ContextLevelRemove
End Sub

Public Sub ControlDeactivate(Control As Fury2InputControl)
On Error Resume Next
Dim Obj As Object
Dim PObj As Fury2Plugin
    If Control.State Then ContextLevelRemove: Exit Sub
    If Control.Handled Then
        Debug.Print "Recieved already handled input event: " & Control.Name & " Deactivated"
        Exit Sub
    End If
    Control.Handled = True
    ContextLevelAdd "ControlDeactivate"
    Control.Consumed = False
    For Each Obj In m_colPluginStack
        Set PObj = Nothing
        Set PObj = Obj
        If PObj Is Nothing Then
            Obj.ControlDeactivate Control
        Else
            PObj.ControlDeactivate Control
        End If
        If Control.Consumed Then Control.Consumed = False: ContextLevelRemove: Exit Sub
    Next Obj
    Err.Clear
    m_ScriptEngine.Exec "Engine_ControlDeactivate", Control
    Err.Clear
    ContextLevelRemove
End Sub

Public Function CreateHardwareImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    Set CreateHardwareImage = m_objOutputPlugin.CreateImage(Width, Height)
    If CreateHardwareImage Is Nothing Then
        Set CreateHardwareImage = F2Image(Width, Height)
    End If
End Function

Public Function CreateMenu(Items, Optional ByVal Area As Fury2Rect = Nothing, Optional SelectEvent = Nothing, Optional CloseEvent = Nothing, Optional MoveEvent = Nothing, Optional ByVal Cursor As Fury2Graphic = Nothing, Optional ByVal WindowSkin As Fury2WindowSkin = Nothing) As Fury2Menu
On Error Resume Next
Dim NewMenu As Fury2Menu
Dim m_lngItems As Long
    Set NewMenu = New Fury2Menu
    With NewMenu
        Set .Engine = Me
        Err.Clear
        If Area Is Nothing Then
            Set .Rectangle = F2Rect(5, 5 + ((ScreenHeight - 10) * DefaultMessageTop * 0.6), ScreenWidth - 10, (ScreenHeight - 10) * 0.4, False)
        Else
            Set .Rectangle = Area
        End If
        Set .Font = Me.Font
        Set .TitleFont = Me.Font
        If UBound(Items) >= 0 Then
            For m_lngItems = LBound(Items) To UBound(Items)
                .Add ToString(Items(m_lngItems)), , BindEvent(SelectEvent)
            Next m_lngItems
        End If
        If Cursor Is Nothing Then
            Set .Cursor = Me.MenuCursor
        Else
            Set .Cursor = Cursor
        End If
        If WindowSkin Is Nothing Then
            Set .WindowSkin = Me.WindowSkin
        Else
            Set .WindowSkin = WindowSkin
        End If
        .Alpha = 1
        .WindowSkin.AdjustRectangle .Rectangle
        Set .SelectEvent = BindEvent(SelectEvent)
        Set .CloseEvent = BindEvent(CloseEvent)
        Set .MoveEvent = BindEvent(MoveEvent)
    End With
    Set CreateMenu = NewMenu
End Function

Public Function CreateTimer(ByRef Name As String, ByVal Delay As Long, ByRef FireCode As String, Optional ByVal GameOnly As Boolean = True, Optional ByVal Realtime As Boolean = False) As Fury2Timer
Attribute CreateTimer.VB_Description = "Creates a timer object."
On Error Resume Next
    If Timers.Count >= c_lngMaxTimers Then Exit Function
    Set CreateTimer = New Fury2Timer
    With CreateTimer
        .SetEngine Me
        .Delay = Delay
        .FireCode = FireCode
        .GameOnly = GameOnly
        .Realtime = Realtime
        .Enabled = True
    End With
    Timers.Add CreateTimer, Name
    Err.Clear
End Function

Function CreateTimerEvent(ByVal BeginTime As Long, ByVal EndTime As Long, ByVal Script As String, Optional ByVal RepeatDelay As Long = 0, Optional ByVal ActivationType As Long = F2TE_Normal)
On Error Resume Next
Dim m_teNew As Fury2TimerEvent
    Set m_teNew = New Fury2TimerEvent
    With m_teNew
        .SetEngine Me
        .BeginTime = BeginTime
        .EndTime = EndTime
        .ScriptSource = Script
        .RepeatDelay = RepeatDelay
        If RepeatDelay > 0 Then
            .Repeat = True
        End If
        .ActivationType = ActivationType
    End With
    Set CreateTimerEvent = m_teNew
End Function

Function DataType(ByRef Data As Variant) As String
On Error Resume Next
Dim l_lngType As Long, l_strArray As String
    DataType = "Unknown"
    l_lngType = VarType(Data)
    If (l_lngType And vbArray) = vbArray Then
        l_lngType = l_lngType And (Not vbArray)
        l_strArray = "Array Of "
    End If
    Select Case l_lngType
    Case vbString
        DataType = l_strArray & "String"
    Case vbSingle
        DataType = l_strArray & "Single"
    Case vbDouble
        DataType = l_strArray & "Double"
    Case vbCurrency
        DataType = l_strArray & "Currency"
    Case vbBoolean
        DataType = l_strArray & "Boolean"
    Case vbByte
        DataType = l_strArray & "Byte"
    Case vbInteger
        DataType = l_strArray & "Integer"
    Case vbLong
        DataType = l_strArray & "Long"
    Case vbObject
        DataType = l_strArray & "Object (" & TypeName(Data) & ")"
    Case Else
    End Select
End Function

Public Sub DebugBreak()
On Error Resume Next
    Stop
End Sub

Sub DefocusWindow()
Attribute DefocusWindow.VB_MemberFlags = "40"
On Error Resume Next
    SetTopmost m_objOutputPlugin.Window, False
    Err.Clear
End Sub

Public Sub DelayEvent(ByVal Delay As Long, ByRef Code As String, Optional ByVal GameOnly As Boolean = True)
Attribute DelayEvent.VB_Description = "Causes an event to be fired after a certain amount of time. Similar to SetTimeout() in web browsers."
On Error Resume Next
Dim NewTimer As Fury2Timer
    If Timers.Count >= c_lngMaxTimers Then Exit Sub
    Set NewTimer = New Fury2Timer
    Timers.Add NewTimer
    With NewTimer
        .SetEngine Me
        .Delay = CLng(Delay)
        .FireCode = ToString(Code)
        .GameOnly = CBool(GameOnly)
        .OneShot = True
        .Realtime = False
        .Enabled = True
    End With
End Sub

Public Sub DestroyAllMenus()
Attribute DestroyAllMenus.VB_Description = "Removes all currently active menus."
On Error Resume Next
Dim m_lngItems As Long
    If m_colPluginStack.Count = 0 Then Exit Sub
    m_lngItems = 1
    Do While m_lngItems <= m_colPluginStack.Count
        If TypeOf m_colPluginStack.Item(m_lngItems) Is Fury2Menu Then
            UnHookEvents m_colPluginStack.Item(m_lngItems)
        Else
            m_lngItems = m_lngItems + 1
        End If
    Loop
    Err.Clear
End Sub

Public Sub DestroyAllMessageWindows()
Attribute DestroyAllMessageWindows.VB_Description = "Removes all currently active message windows."
On Error Resume Next
Dim m_lngItems As Long
    If m_colPluginStack.Count = 0 Then Exit Sub
    m_lngItems = 1
    Do While m_lngItems <= m_colPluginStack.Count
        If TypeOf m_colPluginStack.Item(m_lngItems) Is Fury2MessageWindow Then
            UnHookEvents m_colPluginStack.Item(m_lngItems)
        Else
            m_lngItems = m_lngItems + 1
        End If
    Loop
    Err.Clear
End Sub

Public Sub DestroyAllTextElements()
Attribute DestroyAllTextElements.VB_Description = "Removes all currently active text elements."
On Error Resume Next
Dim m_lngItems As Long
    If m_colPluginStack.Count = 0 Then Exit Sub
    m_lngItems = 1
    Do While m_lngItems <= m_colPluginStack.Count
        If TypeOf m_colPluginStack.Item(m_lngItems) Is Fury2TextElement Then
            UnHookEvents m_colPluginStack.Item(m_lngItems)
        Else
            m_lngItems = m_lngItems + 1
        End If
    Loop
    Err.Clear
End Sub

Public Sub DestroyAllTimers()
Attribute DestroyAllTimers.VB_Description = "Removes all currently active timers."
On Error Resume Next
    Set Timers = New Collection
End Sub

Public Sub DestroyTimer(ByRef ID)
Attribute DestroyTimer.VB_Description = "Destroys a timer."
On Error Resume Next
    If VarType(ID) = vbString Then
        Timers.Remove CStr(ID)
    ElseIf VarType(ID) = vbLong Then
        Timers.Remove CLng(ID)
    ElseIf TypeOf ID Is Fury2Timer Then
        Timers.Remove Timers.Find(ID)
    End If
    Err.Clear
End Sub

Public Property Get Digits() As String
Attribute Digits.VB_Description = "Returns a string of numbers useful for testing."
    Digits = "0123456789"
End Property

Public Sub DisableAllHotspots()
Attribute DisableAllHotspots.VB_Description = "Disables all hotspots."
On Error Resume Next
Dim m_lngIndex As Long
    For m_lngIndex = 0 To c_lngMaxHotspot
        Hotspots(m_lngIndex).Enabled = False
    Next m_lngIndex
    Err.Clear
End Sub

Public Sub DisableHotspot(ByVal Index As Long)
Attribute DisableHotspot.VB_Description = "Disables a hotspot."
On Error Resume Next
    With Hotspots(Index)
        .Enabled = False
    End With
    Err.Clear
End Sub

Function DistanceBetween(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single) As Single
Attribute DistanceBetween.VB_Description = "Returns the distance between two points."
Dim Rx As Single, Ry As Single
On Error Resume Next
    Rx = (x2 - x1)
    Ry = (y2 - y1)
    DistanceBetween = Sqr((Rx * Rx) + (Ry * Ry))
End Function

Public Sub EnableHotspot(ByVal Index As Long)
Attribute EnableHotspot.VB_Description = "Enables a hotspot."
On Error Resume Next
    With Hotspots(Index)
        .Enabled = True
    End With
    Err.Clear
End Sub

Sub EndTrace()
    #If DebugFeatures = 1 Then
    m_booTrace = False
    #End If
End Sub

Public Sub ErrorMessage(ByRef Error As String, ByRef Details As String)
Attribute ErrorMessage.VB_Description = "Displays an error message in the debugging log."
Attribute ErrorMessage.VB_MemberFlags = "40"
On Error Resume Next
    ErrorOccurred = True
    If Debugger Is Nothing Then
        TextOut Error & vbCrLf & Details
    Else
        Debugger.Error Error, Details
    End If
End Sub

Function Evaluate(ByRef Expr)
On Error Resume Next
    Err.Clear
    Evaluate = m_ScriptEngine.Eval(ToString(Expr))
    If (Err.Number = 438) Then
        Set Evaluate = m_ScriptEngine.Eval(ToString(Expr))
    End If
End Function

Private Sub Event_KeyDown(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object, m_lngKeyCode As Long
Dim PObj As Fury2Plugin
    If ConsoleVisible Then Exit Sub
    ContextLevelAdd "KeyDown"
    m_lngKeyCode = CLng(Evt.LParam1)
    For Each Obj In m_colPluginStack
        Set PObj = Nothing
        Set PObj = Obj
        If PObj Is Nothing Then
            Obj.KeyDown m_lngKeyCode, CLng(Evt.LParam2)
        Else
            PObj.KeyDown m_lngKeyCode, CLng(Evt.LParam2)
        End If
        If m_lngKeyCode = 0 Then ContextLevelRemove: Exit Sub
    Next Obj
    Err.Clear
    m_ScriptEngine.Exec "Engine_KeyDown", m_lngKeyCode, CLng(Evt.LParam2)
    If m_lngKeyCode = 0 Then ContextLevelRemove: Exit Sub
    If (Not ConsoleVisible) And (Not TrapKeys) Then
        If Evt.LParam3 <> 1 Then
            If VarType(KeyBindings(m_lngKeyCode)) = vbString Then
                Err.Clear
                If BindState(m_lngKeyCode) Then
                Else
                    BindState(m_lngKeyCode) = True
                    If Not ConsoleVisible Then m_ScriptEngine.AddCode ToString(KeyBindings(m_lngKeyCode))
                    BindState(m_lngKeyCode) = False
                End If
                Err.Clear
            Else
                If TypeOf KeyBindings(m_lngKeyCode) Is Fury2Event Then
                    KeyBindings(m_lngKeyCode).Invoke
                ElseIf TypeOf KeyBindings(m_lngKeyCode) Is Fury2InputControl Then
                    If KeyBindings(m_lngKeyCode).State Then
                        KeyBindings(m_lngKeyCode).Deactivate
                    End If
                    KeyBindings(m_lngKeyCode).Activate
                End If
            End If
        Else
        End If
    End If
    ContextLevelRemove
End Sub

Private Sub Event_KeyUp(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object, m_lngKeyCode As Long
    ContextLevelAdd "KeyUp"
    m_lngKeyCode = CLng(Evt.LParam1)
    Dim PObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set PObj = Nothing
        Set PObj = Obj
        If PObj Is Nothing Then
            Obj.KeyUp m_lngKeyCode, CLng(Evt.LParam2)
        Else
            PObj.KeyUp m_lngKeyCode, CLng(Evt.LParam2)
        End If
        If m_lngKeyCode = 0 Then ContextLevelRemove: Exit Sub
    Next Obj
    m_ScriptEngine.Exec "Engine_KeyUp", CLng(Evt.LParam1), CLng(Evt.LParam2)
    If VarType(KeyBindings(m_lngKeyCode)) = vbObject Then
        If TypeOf KeyBindings(m_lngKeyCode) Is Fury2InputControl Then
            KeyBindings(m_lngKeyCode).Deactivate
        End If
    End If
    ContextLevelRemove
End Sub

Private Sub Event_MouseDown(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object
Dim m_lngX As Long, m_lngY As Long
Dim m_colObjects As Collection, m_lngObjects As Long
Dim l_lngHotspots As Long, l_booInside As Boolean
    Set m_colObjects = New Collection
    ContextLevelAdd "MouseDown"
    m_lngX = Evt.LParam3
    m_lngY = Evt.LParam4
    Dim PObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set PObj = Nothing
        Set PObj = Obj
        If PObj Is Nothing Then
            Obj.MouseDown Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        Else
            PObj.MouseDown Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        End If
        If m_lngX = -32767 Or m_lngY = -32767 Then ContextLevelRemove: Exit Sub
    Next Obj
    For l_lngHotspots = LBound(Hotspots) To UBound(Hotspots)
        With Hotspots(l_lngHotspots)
            If .Enabled Then
                l_booInside = m_lngX >= .x1 And m_lngX < .x2 And m_lngY >= .y1 And m_lngY < .y2
                If .Hovering Then
                    If l_booInside Then
                        .ClickEvent.Invoke
                    Else
                        .Hovering = False
                        .LeaveEvent.Invoke
                    End If
                Else
                    If l_booInside Then
                        .EnterEvent.Invoke
                        .Hovering = True
                        .ClickEvent.Invoke
                    End If
                End If
            End If
        End With
    Next l_lngHotspots
    If Not (MouseDownHook Is Nothing) Then
        MouseDownHook.Invoke Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
    End If
    ContextLevelRemove
End Sub

Private Sub Event_MouseMove(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object
Dim m_lngX As Long, m_lngY As Long
Dim l_lngHotspots As Long, l_booInside As Boolean
    ContextLevelAdd "MouseMove"
    m_lngX = Evt.LParam3
    m_lngY = Evt.LParam4
    Dim PObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set PObj = Nothing
        Set PObj = Obj
        If PObj Is Nothing Then
            Obj.MouseMove Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        Else
            PObj.MouseMove Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        End If
        If m_lngX = -32767 Or m_lngY = -32767 Then ContextLevelRemove: Exit Sub
    Next Obj
    For l_lngHotspots = LBound(Hotspots) To UBound(Hotspots)
        With Hotspots(l_lngHotspots)
            If .Enabled Then
                l_booInside = m_lngX >= .x1 And m_lngX < .x2 And m_lngY >= .y1 And m_lngY < .y2
                If .Hovering Then
                    If l_booInside Then
                    Else
                        .Hovering = False
                        .LeaveEvent.Invoke
                    End If
                Else
                    If l_booInside Then
                        .EnterEvent.Invoke
                        .Hovering = True
                    End If
                End If
            End If
        End With
    Next l_lngHotspots
    If Not (MouseMoveHook Is Nothing) Then
        MouseMoveHook.Invoke Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
    End If
    ContextLevelRemove
End Sub

Private Sub Event_MouseUp(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object
Dim m_lngX As Long, m_lngY As Long
Dim m_colObjects As Collection, m_lngObjects As Long
Dim m_lngHotspots As Long
    Set m_colObjects = New Collection
    ContextLevelAdd "MouseUp"
    m_lngX = Evt.LParam3
    m_lngY = Evt.LParam4
    Dim PObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set PObj = Nothing
        Set PObj = Obj
        If PObj Is Nothing Then
            Obj.MouseUp Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        Else
            PObj.MouseUp Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        End If
        If m_lngX = -32767 Or m_lngY = -32767 Then ContextLevelRemove: Exit Sub
    Next Obj
    If Not (MouseUpHook Is Nothing) Then
        MouseUpHook.Invoke Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
    End If
    ContextLevelRemove
End Sub

Public Property Get EventHooks() As Collection
On Error Resume Next
    Set EventHooks = m_colPluginStack
End Property

#If EnableBehaviors = 1 Then
Public Function F2Behavior(BType) As Fury2Behavior
On Error Resume Next
Dim m_facBehavior As fury2behaviorfactory
    Set m_facBehavior = New fury2behaviorfactory
    m_facBehavior.BehaviorType = CLng(BType)
    m_facBehavior.Instantiate
    Set F2Behavior = m_facBehavior.TrueObject
End Function
#End If

Public Function F2Camera() As Fury2Camera
On Error Resume Next
    Set F2Camera = New Fury2Camera
    F2Camera.SetEngine Me
End Function

Public Function F2Collection(Optional ValueArray, Optional ByVal AddNames As Boolean = False) As Fury2Collection
Attribute F2Collection.VB_Description = "Creates a collection object."
On Error Resume Next
Dim l_lngIndex As Long
    Set F2Collection = New Fury2Collection
    F2Collection.SetEngine Me
    If IsMissing(ValueArray) Then
    Else
        If (VarType(ValueArray) And vbArray) = vbArray Then
            For l_lngIndex = LBound(ValueArray) To UBound(ValueArray)
                If AddNames Then
                    F2Collection.Add ValueArray(l_lngIndex), ValueArray(l_lngIndex).Name
                Else
                    F2Collection.Add ValueArray(l_lngIndex)
                End If
            Next l_lngIndex
        End If
    End If
End Function

Public Function F2Control(ByVal Name As String) As Fury2InputControl
On Error Resume Next
    Set F2Control = New Fury2InputControl
    F2Control.SetEngine Me
    F2Control.Name = Name
End Function

Public Function F2Controller(ByVal Obj, Optional ByVal Camera As Fury2Camera = Nothing) As Fury2Controller
Attribute F2Controller.VB_Description = "Creates a controller object."
On Error Resume Next
Dim m_f2cController As Fury2Controller
    If Obj Is Nothing Then
        Err.Clear
        Err.Raise 10241, "F2Controller", "Passed empty object to F2Controller."
        Exit Function
    End If
    Set m_f2cController = New Fury2Controller
    With m_f2cController
        Set .Obj = Obj
        Set .Camera = Camera
        Err.Clear
    End With
    Set F2Controller = m_f2cController
    Set m_f2cController = Nothing
End Function

Public Function F2Dispatcher() As Fury2EventDispatcher
    Set F2Dispatcher = New Fury2EventDispatcher
End Function

Public Function F2Font() As Fury2Font
On Error Resume Next
    Set F2Font = New Fury2Font
    F2Font.SetEngine Me
End Function

Public Function F2Graphic(Images, Optional ByVal XCenter As Long = -32767, Optional ByVal YCenter As Long = -32767, Optional ByVal BlitMode As SFXBlitModes = -1, Optional ByVal LoopMode As Fury2LoopModes = F2LM_Restart, Optional ByVal AnimationDelay As Long = 5) As Fury2Graphic
On Error Resume Next
Dim l_gfcGraphic As Fury2Graphic
Dim l_imgImage As Fury2Image
Dim l_lngImages As Long
    Set l_gfcGraphic = New Fury2Graphic
    With l_gfcGraphic
        .SetEngine Me
        Set l_imgImage = Images
        If Err <> 0 Then
            Err.Clear
            If (VarType(Images) And vbArray) = vbArray Then
                For l_lngImages = LBound(Images) To UBound(Images)
                    Set l_imgImage = Nothing
                    Set l_imgImage = Images(l_lngImages)
                    If Not (l_imgImage Is Nothing) Then .Images.Add l_imgImage
                Next l_lngImages
                Err.Clear
                Set l_imgImage = Images(LBound(Images))
            Else
                Exit Function
            End If
        Else
            .Images.Add l_imgImage
        End If
        If XCenter = -32767 Then
            XCenter = l_imgImage.Width / 2
        End If
        If YCenter = -32767 Then
            YCenter = l_imgImage.Height / 2
        End If
        .XCenter = XCenter
        .YCenter = YCenter
        If BlitMode = BlitMode_Default Then
            If l_imgImage.AlphaChannel Then
                BlitMode = BlitMode_SourceAlpha
            Else
                BlitMode = BlitMode_Matte
            End If
        End If
        .LoopMode = LoopMode
        .AnimationDelay = AnimationDelay
        .BlitMode = BlitMode
    End With
    Err.Clear
    Set F2Graphic = l_gfcGraphic
End Function

Public Function F2Map(Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal Layers As Long = 1, Optional ByVal Tileset As Fury2Tileset = Nothing) As Fury2Map
Attribute F2Map.VB_Description = "Creates a map object."
On Error Resume Next
Dim m_newMap As Fury2Map
    Set m_newMap = New Fury2Map
    m_newMap.SetEngine Me
    If Width = -1 Then
    Else
        m_newMap.CreateNew "Map", Width, Height, Layers, Tileset
        m_newMap.Init
    End If
    Set F2Map = m_newMap
End Function

Public Function F2Slider() As Fury2Slider
    Set F2Slider = New Fury2Slider
    Set F2Slider.Engine = Me
End Function

Public Function F2SoundObject(ByVal Filename As String, Optional ByVal Repeat As Boolean = True) As Fury2SoundObject
On Error Resume Next
Dim l_sndSound As Fury2SoundObject
    Set l_sndSound = New Fury2SoundObject
    l_sndSound.Sound = Filename
    l_sndSound.Repeat = Repeat
    Set F2SoundObject = l_sndSound
    Err.Clear
End Function

Public Function F2TextElement() As Fury2TextElement
Attribute F2TextElement.VB_Description = "Creates a text element object."
On Error Resume Next
    Set F2TextElement = New Fury2TextElement
    Set F2TextElement.Font = Me.Font
End Function

Public Function F2TileAnimation(ByVal Tile As Long, ByVal Delay As Long, ParamArray Frames() As Variant) As Fury2TileAnimation
Attribute F2TileAnimation.VB_Description = "Creates a tile animation object."
On Error Resume Next
Dim m_anmNewAnim As Fury2TileAnimation
Dim m_lngAddFrames As Long
Dim m_intFrames() As Integer
    Set m_anmNewAnim = New Fury2TileAnimation
    With m_anmNewAnim
        .Tile = CInt(Tile)
        .SetDelay CLng(Delay)
        ReDim m_intFrames(0 To UBound(Frames) - LBound(Frames))
        For m_lngAddFrames = 0 To (UBound(Frames) - LBound(Frames))
            m_intFrames(m_lngAddFrames) = CInt(Frames(m_lngAddFrames + LBound(Frames)))
        Next m_lngAddFrames
        .Sequence = m_intFrames()
    End With
    Set F2TileAnimation = m_anmNewAnim
    Set m_anmNewAnim = Nothing
End Function

'Public Function F2ParticleEffect() As Fury2ParticleEffect
'    Set F2ParticleEffect = New Fury2ParticleEffect
'End Function

Public Function F2Tileset(Optional ByRef Filename As String = "", Optional ByVal TileWidth As Long = 16, Optional ByVal TileHeight As Long = 16, Optional ByVal MaskColor As Long = c_lngNullColor) As Fury2Tileset
Attribute F2Tileset.VB_Description = "Creates a tileset object."
On Error Resume Next
Dim m_NewTileset As Fury2Tileset
    Set m_NewTileset = New Fury2Tileset
    With m_NewTileset
        .SetEngine Me
        .TileWidth = TileWidth
        .TileHeight = TileHeight
        .MatteColor = MaskColor
        If Trim(Filename) = "" Then
        Else
            .Filename = Filename
        End If
    End With
    Set F2Tileset = m_NewTileset
    Set m_NewTileset = Nothing
End Function

Public Function F2WindowSkin() As Fury2WindowSkin
On Error Resume Next
    Set F2WindowSkin = New Fury2WindowSkin
    F2WindowSkin.SetEngine Me
End Function

Public Sub FadePicture(ByVal Slot As Long, Optional ByVal StartAlpha As Single = -1, Optional ByVal EndAlpha As Single = -1, Optional ByVal Speed As Single = 3.33333317812224E-02, Optional ByRef EndScript As String = "")
Attribute FadePicture.VB_Description = "Fades a picture slot in or out."
On Error Resume Next
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With OnscreenPictures(Slot)
        If Not StartAlpha = -1 Then .Alpha = StartAlpha
        If Not EndAlpha = -1 Then .EndFadeAt = EndAlpha
        If .EndFadeAt < .Alpha Then
            .FadeSpeed = -Speed
        Else
            .FadeSpeed = Speed
        End If
        .EndFadeScript = EndScript
        .Fading = True
    End With
End Sub

Public Sub FastQuit()
Attribute FastQuit.VB_MemberFlags = "40"
    PostQuitMessage 0
End Sub

Function FindScript(ByRef Filename As String) As String
On Error Resume Next
Dim m_csScript As Fury2ScriptCache
    Set m_csScript = CachedNames(LCase(Trim(Filename)))
    If m_csScript Is Nothing Then
    Else
        FindScript = m_csScript.Name
    End If
    Set m_csScript = Nothing
End Function

Public Property Get Flag(ByRef Index As String) As Variant
Attribute Flag.VB_Description = "An array of global game settings."
On Error Resume Next
    Flag = m_Flags(LCase(Index))
    Err.Clear
End Property

Public Property Let Flag(ByRef Index As String, ByVal NewValue As Variant)
On Error Resume Next
Dim m_Value As Variant
    Err.Clear
    Index = LCase(Index)
    m_Value = m_Flags(Index)
    If Err.Number <> 0 Then
        m_Flags.Add NewValue, Index
        m_FlagNames.Add Index
    Else
        m_Flags.Remove Index
        m_Flags.Add NewValue, Index
    End If
    Err.Clear
End Property

Public Property Get flags(ByRef Index As String) As Variant
Attribute flags.VB_MemberFlags = "40"
    flags = Flag(Index)
End Property

Public Property Let flags(ByRef Index As String, ByVal NewValue As Variant)
    Flag(Index) = NewValue
End Property

Public Sub FlipScreen()
Attribute FlipScreen.VB_Description = "Forces the engine to copy the backbuffer to the screen."
On Error Resume Next
    If Running Then Else Exit Sub
    If m_objOutputPlugin.Closed Then GFXEngine_Closed
    If m_objOutputPlugin.Window.WindowState = 1 Then
    Else
        If Running Then
            m_objOutputPlugin.VSync = VSync
            If MouseCursor Is Nothing Then
            Else
                If (m_imgMouseBuffer.Width <> MouseCursor.Width) Or (m_imgMouseBuffer.Height <> MouseCursor.Height) Then
                    m_imgMouseBuffer.Resize MouseCursor.Width, MouseCursor.Height
                End If
                m_imgMouseBuffer.Blit m_imgMouseBuffer.Rectangle, F2Rect(g_lngMouseX, g_lngMouseY, MouseCursor.Width, MouseCursor.Height, False), m_imgBackbuffer
                m_imgBackbuffer.Blit F2Rect(g_lngMouseX, g_lngMouseY, MouseCursor.Width, MouseCursor.Height, False), MouseCursor.Rectangle, MouseCursor, 1, BlitMode_SourceAlpha
            End If
            m_objOutputPlugin.Flip
            If MouseCursor Is Nothing Then
            Else
                m_imgBackbuffer.Blit F2Rect(g_lngMouseX, g_lngMouseY, MouseCursor.Width, MouseCursor.Height, False), m_imgMouseBuffer.Rectangle, m_imgMouseBuffer
            End If
        End If
    End If
End Sub

Public Sub FlushImageCache()
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown, m_lngCount As Long
Dim m_imgObj As Fury2Image, m_lngObj As Long
    If m_Images.Count < 1 Then Exit Sub
    m_lngObj = 1
    DebugOut "Flushing image cache"
    Do While m_lngObj < m_Images.Count
        Set m_objObj = Nothing
        Set m_objObj = m_Images(m_lngObj)
        If m_objObj Is Nothing Then
            m_lngObj = m_lngObj + 1
        Else
            m_objObj.AddRef
            m_lngCount = m_objObj.Release
            Set m_objObj = Nothing
            If m_lngCount <= 2 Then
                Set m_imgObj = m_Images(m_lngObj)
                Set m_imgObj = Nothing
                m_Images.Remove m_lngObj
            Else
                m_lngObj = m_lngObj + 1
            End If
        End If
    Loop
End Sub

Public Sub EmptyImageCache()
On Error Resume Next
Dim l_imgImage As Fury2Image
    If m_Images.Count < 1 Then Exit Sub
    DebugOut "Emptying image cache"
    Do While m_Images.Count > 0
        Set l_imgImage = m_Images(1)
        l_imgImage.Deallocate
        Set l_imgImage = Nothing
        m_Images.Remove 1
    Loop
End Sub

Sub FocusWindow()
Attribute FocusWindow.VB_MemberFlags = "40"
On Error Resume Next
    SetTopmost m_objOutputPlugin.Window, m_objOutputPlugin.Fullscreen
    m_objOutputPlugin.Window.SetFocus
    Err.Clear
End Sub

Sub ForEach(Arr As Variant, ByVal ExecScript As String)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    For m_lngItems = m_lngLB To m_lngUB
        ScriptEngine.Exec ExecScript, Arr(m_lngItems)
    Next m_lngItems
    Err.Clear
End Sub

Public Function FormatNumber(ByVal Value As Double, ByRef Mask As String)
Attribute FormatNumber.VB_Description = "Formats a number into a string."
On Error Resume Next
    FormatNumber = Format(Value, Mask)
End Function

Friend Sub FreedImage(ByVal Obj As Fury2Image)
On Error Resume Next
Dim m_lngImages As Long
    If m_Images.Count <= 0 Then Exit Sub
    For m_lngImages = 1 To m_Images.Count
        If m_Images(m_lngImages) Is Obj Then m_Images.Remove m_lngImages: Exit Sub
    Next m_lngImages
End Sub

Public Sub FreeImage(Handle)
Attribute FreeImage.VB_Description = "Unloads an image."
On Error Resume Next
    m_Images(Handle).Free
    If VarType(Handle) = vbString Then
        m_Images.Remove Handle
    Else
        m_Images.Add Nothing, , Handle
        m_Images.Remove Handle + 1
    End If
End Sub

Friend Sub Game(Optional ByVal Name As String = "")
On Error Resume Next
Dim FrameStart As Double, FrameEnd As Double, Elapsed As Double
Dim UpdateStart As Double, UpdateEnd As Double
Dim UpdateMaxLength As Double, UpdateOverflowCount As Long
Dim NextFrame As Double
Dim FramesElapsed As Long, Frames As Long
Dim l_lngThreadID As Long
    Halted = False
    SubthreadCount = SubthreadCount + 1
    ReDim Preserve Subthreads(0 To SubthreadCount)
    l_lngThreadID = SubthreadCount
    With Subthreads(l_lngThreadID)
        .Name = Name
        .Running = True
    End With
    If LastFrameStart = 0 Then LastFrameStart = HiTimer
    If LastFrameEnd = 0 Then LastFrameEnd = HiTimer
    If LastSecond = 0 Then LastSecond = HiTimer
    UpdateMaxLength = 0.8 / DesiredFramerate
    Profiler.ProfileReset
    If l_lngThreadID = 0 Then
        ProfileStart "Game"
        ContextLevelAdd "Game"
    Else
        If l_lngThreadID = 1 Then
            ProfileStop "Game"
        Else
            ProfileStop "Subthread #" & l_lngThreadID - 1
        End If
        ProfileStart "Subthread #" & l_lngThreadID
        ContextLevelAdd "Subthread #" & l_lngThreadID
    End If
    If Not RanStartEvent Then
        RanStartEvent = True
        m_objNotify.Start
        If Not (Debugger Is Nothing) Then Debugger.DebugGameStart
        ContextLevelAdd "Engine_Start"
        ScriptEngine.Exec "Engine_Start"
        ContextLevelRemove
    End If
    If Not (Debugger Is Nothing) Then Debugger.DebugSubthreadStart
    Do While Running
        If (m_booBreak = True) And (SubthreadCount > 0) Then
            m_booBreak = False
            m_booResetClock = True
            Exit Do
        End If
        If Subthreads(l_lngThreadID).Running = False Then
            m_booResetClock = True
            Exit Do
        End If
        If Terminating Then Exit Do
        LastFrameStart = FrameStart
        FrameStart = HiTimer
        FrameLength = 1# / CDbl(DesiredFramerate)
        NextFrame = LastFrameStart + FrameLength
        Elapsed = Elapsed + (FrameStart - LastFrameStart)
        If BalanceFramerate Then
            FramesElapsed = FramesElapsed + Floor((Elapsed) / FrameLength)
            If FramesElapsed > MaxFrameskip Then FramesElapsed = MaxFrameskip
            Elapsed = Elapsed - ((Floor((Elapsed) / FrameLength)) * FrameLength)
            If Elapsed < 0 Then Elapsed = 0
        Else
            FramesElapsed = 1
            Elapsed = 0
        End If
        If m_booResetClock Then
            m_booResetClock = False
            FramesElapsed = 1
            Elapsed = 0
        End If
        
        If FramesElapsed >= 1 Then
            
            Do While FramesElapsed > 0
                If Not (Debugger Is Nothing) Then Debugger.DebugFrameStart
                If Halted Then
                    If Not (Debugger Is Nothing) Then Debugger.DebugHalted
                    Do While Halted
                        DoEvents
                    Loop
                    m_booResetClock = True
                    If Not (Debugger Is Nothing) Then Debugger.DebugUnhalted
                End If
                FramesElapsed = FramesElapsed - 1
                UpdateStart = HiTimer
                Update
                If Terminating Then Exit Do
                UpdateEnd = HiTimer
                If (UpdateEnd - UpdateStart) > UpdateMaxLength Then
                    UpdateOverflowCount = UpdateOverflowCount + 1
                End If
                UpdatePictures
                If Terminating Then Exit Do
                If Not (Debugger Is Nothing) Then Debugger.DebugFrameEnd
            Loop
            
            If Terminating Then Exit Do
            UpdateTimers
            If Terminating Then Exit Do
            UpdateEvents
            If Terminating Then Exit Do
            
            Redraw
            If Terminating Then Exit Do
        
            FlipScreen
            If Terminating Then Exit Do
        
            NextFPS = NextFPS + 1
            
        End If
        
        FrameEnd = HiTimer
        
        If BalanceFramerate And (UpdateOverflowCount > (DesiredFramerate / 4)) Then
            BalanceFramerate = False
            DebugOut "Framerate balancing auto-disabled (game too slow)"
        End If
        
        If (FrameEnd - LastSecond) >= 1# Then
            UpdateOverflowCount = 0
            If l_lngThreadID = 0 Then
                ProfileStop "Game"
            Else
                ProfileStop "Subthread #" & l_lngThreadID
            End If
            Profiler.ProfileUpdate
            Profiler.ProfileReset
            frmProfile.ProfileTextHeight = ProfileTextCurrentHeight
            frmProfile.Form_Resize
            If l_lngThreadID = 0 Then
                ProfileStart "Game"
            Else
                ProfileStart "Subthread #" & l_lngThreadID
            End If
            LastSecond = FrameEnd
            FPS = NextFPS
            NextFPS = 0
            If ShowFPS Then
                m_objOutputPlugin.Window.Caption = WindowCaption & " (" & FPS & " FPS)"
            End If
        End If
        
        DoEvents
        If Me.HarassCPU Then Else SleepEx 1, True
        
        LastFrameEnd = FrameEnd
    Loop
    SubthreadCount = SubthreadCount - 1
    ReDim Preserve Subthreads(0 To SubthreadCount)
    m_booBreak = False
    ContextLevelRemove
    If Not (Debugger Is Nothing) Then Debugger.DebugSubthreadEnd
    Err.Clear
End Sub

Public Function GetFile(ByRef Filename As String) As String
On Error Resume Next
    GetFile = Filesystem.File(Filename).Text
End Function

Public Function GetPictureAlpha(ByVal Slot As Long) As Single
On Error Resume Next
    GetPictureAlpha = OnscreenPictures(Slot).Alpha
End Function

Public Function GetPictureHeight(ByVal Slot As Long) As Long
On Error Resume Next
    GetPictureHeight = OnscreenPictures(Slot).DestRect.Height
End Function

Public Function GetPictureImage(ByVal Slot As Long) As Fury2Image
On Error Resume Next
    Set GetPictureImage = OnscreenPictures(Slot).Picture
End Function

Public Function GetPictureRectangle(ByVal Slot As Long) As Fury2Rect
On Error Resume Next
    Set GetPictureRectangle = OnscreenPictures(Slot).DestRect
End Function

Public Function GetPictureSourceRectangle(ByVal Slot As Long) As Fury2Rect
On Error Resume Next
    Set GetPictureSourceRectangle = OnscreenPictures(Slot).SourceRect
End Function

Public Function GetPictureTile(ByVal Slot As Long) As Boolean
On Error Resume Next
    GetPictureTile = OnscreenPictures(Slot).Tile
End Function

Public Function GetPictureWidth(ByVal Slot As Long) As Long
On Error Resume Next
    GetPictureWidth = OnscreenPictures(Slot).DestRect.Width
End Function

Public Function GetPictureX(ByVal Slot As Long) As Long
On Error Resume Next
    GetPictureX = OnscreenPictures(Slot).DestRect.Left
End Function

Public Function GetPictureY(ByVal Slot As Long) As Long
On Error Resume Next
    GetPictureY = OnscreenPictures(Slot).DestRect.Top
End Function

Public Function GetReferenceCount(ByVal Obj As Object) As Long
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown
    Set m_objObj = Obj
    If Err <> 0 Then Exit Function
    m_objObj.AddRef
    GetReferenceCount = m_objObj.Release
End Function

Public Function GetUserInput(Optional ByRef Prompt As String = "Input:") As String
On Error Resume Next
    GetUserInput = InputBox(Prompt)
End Function

Public Sub GFXEngine_Closed()
Attribute GFXEngine_Closed.VB_MemberFlags = "40"
    If Running = True Then
        Running = False
    End If
    Terminating = True
    TerminateEngine = True
End Sub

Public Sub GFXEngine_DblClick()
On Error Resume Next
    If Not (MouseDblClickHook Is Nothing) Then
        MouseDblClickHook.Invoke g_lngMouseX, g_lngMouseY
    End If
End Sub

Public Sub ConsoleCommand(ByRef Command As String)
On Error Resume Next
    If Left(Trim(Command), 1) = "?" Then
        Err.Clear
        ScriptEngine.AddCode ScriptEngine.Language.GenerateSubCall("TextOut", ScriptEngine.Language.GenerateFunctionCall("ToString", Mid(Command, 2) & ", true"))
    Else
        Err.Clear
        ScriptEngine.AddCode Command
    End If
End Sub

Friend Sub ConsoleInsert(ByRef Text As String)
On Error Resume Next
Dim l_strLeft As String, l_strRight As String
    l_strLeft = Mid(ConsoleText, 1, ConsoleCaretPos - 1)
    l_strRight = Mid(ConsoleText, ConsoleCaretPos)
    ConsoleText = l_strLeft + Text + l_strRight
    ConsoleCaretPos = ConsoleCaretPos + Len(Text)
    Err.Clear
End Sub

Friend Sub ConsoleBackspace()
On Error Resume Next
Dim l_strLeft As String, l_strRight As String
    l_strLeft = Mid(ConsoleText, 1, ConsoleCaretPos - 2)
    l_strRight = Mid(ConsoleText, ConsoleCaretPos)
    ConsoleText = l_strLeft + l_strRight
    ConsoleCaretPos = ConsoleCaretPos - 1
End Sub

Friend Sub ConsoleDelete()
On Error Resume Next
Dim l_strLeft As String, l_strRight As String
    l_strLeft = Mid(ConsoleText, 1, ConsoleCaretPos - 1)
    l_strRight = Mid(ConsoleText, ConsoleCaretPos + 1)
    ConsoleText = l_strLeft + l_strRight
End Sub

Public Sub GFXEngine_KeyDown(ByVal KeyCode As Integer, ByVal Shift As Integer, Optional ByVal Force As Boolean = False)
Attribute GFXEngine_KeyDown.VB_MemberFlags = "40"
On Error Resume Next
Dim l_lngType As Long
    Select Case KeyCode
    Case 192 ' Tilda
        If Shift = 1 Then
            If m_objOutputPlugin.Fullscreen Then
            Else
                TraceOpen
            End If
        Else
            If ConsoleVisible Then
                HideConsole
            Else
                ShowConsole
            End If
        End If
    Case 67 ' C
        If Shift = 2 Then ' Ctrl
            If ConsoleVisible Then
                Clipboard.Clear
                Clipboard.SetText ConsoleText
            Else
                Clipboard.Clear
                Clipboard.SetText KeysEntered
            End If
        End If
    Case 86 ' V
        If Shift = 2 Then ' Ctrl
            If ConsoleVisible Then
                ConsoleInsert Clipboard.GetText
            Else
                KeysEntered = KeysEntered + Clipboard.GetText
            End If
        End If
    Case 46
        If ConsoleVisible Then
            ConsoleDelete
        End If
    Case 8
        If Shift = 2 Then ' Ctrl
            If ConsoleVisible Then
                ConsoleText = ""
                ConsoleCaretPos = 0
            Else
                KeysEntered = ""
            End If
        End If
    Case vbKeyLeft
        If ConsoleVisible Then
            ConsoleCaretPos = ConsoleCaretPos - 1
        End If
    Case vbKeyRight
        If ConsoleVisible Then
            ConsoleCaretPos = ConsoleCaretPos + 1
        End If
    Case Else
    End Select
    If (DisableUserInput) And (Not Force) Then Exit Sub
    If m_KeyState(m_KeyMap(KeyCode)) = True Then
        l_lngType = 1 ' Repeated key
    Else
        m_KeyState(m_KeyMap(KeyCode)) = True
    End If
    If BindState(m_KeyMap(KeyCode)) Then
    Else
        QueueEvent 0, , , CLng(m_KeyMap(KeyCode)), CLng(Shift), l_lngType
    End If
End Sub

Public Sub GFXEngine_KeyPress(ByVal KeyAscii As Integer, Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim l_strInput As String
Dim l_strBuffer As String
Dim l_strTemp As String
    If ConsoleVisible Then
        l_strBuffer = ConsoleText
    Else
        If (DisableUserInput) And (Not Force) Then Exit Sub
        l_strBuffer = KeysEntered
    End If
    If KeyAscii = 96 Or KeyAscii = 127 Then
    ElseIf KeyAscii >= 32 Then
        l_strInput = Chr(KeyAscii)
        If ConsoleVisible Then
            ConsoleInsert l_strInput
        ElseIf TrapKeys Then
            l_strBuffer = l_strBuffer + l_strInput
            ContextLevelAdd "Engine_KeyPress"
            ScriptEngine.Exec "Engine_KeyPress", KeyAscii
            ContextLevelRemove
        End If
    ElseIf KeyAscii = 13 Then
        If ConsoleVisible Then
            m_booErrorOccurred = False
            ContextLevelAdd "Console"
            ConsoleCommand ConsoleText
            ContextLevelRemove
            If (Err.Number = 0) And (Not m_booErrorOccurred) Then
                ConsoleText = ""
                ConsoleCaretPos = 1
            Else
                Err.Clear
            End If
        ElseIf TrapKeys Then
            ContextLevelAdd "Engine_KeyPress"
            ScriptEngine.Exec "Engine_KeyPress", KeyAscii
            ContextLevelRemove
        End If
    ElseIf KeyAscii = 8 Then
        If ConsoleVisible Then
            ConsoleBackspace
        ElseIf TrapKeys Then
            l_strBuffer = Left(l_strBuffer, Len(l_strBuffer) - 1)
            ContextLevelAdd "Engine_KeyPress"
            ScriptEngine.Exec "Engine_KeyPress", KeyAscii
            ContextLevelRemove
        End If
    Else
        If ConsoleVisible Then
        ElseIf TrapKeys Then
            ContextLevelAdd "Engine_KeyPress"
            ScriptEngine.Exec "Engine_KeyPress", KeyAscii
            ContextLevelRemove
        End If
    End If
    If ConsoleVisible Then
    ElseIf TrapKeys Then
        KeysEntered = l_strBuffer
    ElseIf KeyAscii = 96 Or KeyAscii = 127 Then
    Else
        Dim Obj As Object, m_lngKeyAscii As Long
        Dim PObj As Fury2Plugin
        m_lngKeyAscii = KeyAscii
        For Each Obj In m_colReversePluginStack
            If Not (Obj Is Nothing) Then
                Set PObj = Nothing
                Set PObj = Obj
                If PObj Is Nothing Then
                    Obj.KeyPress m_lngKeyAscii
                Else
                    PObj.KeyPress m_lngKeyAscii
                End If
                If m_lngKeyAscii = 0 Then: Exit Sub
            End If
        Next Obj
        Err.Clear
    End If
End Sub

Public Sub GFXEngine_KeyUp(ByVal KeyCode As Integer, ByVal Shift As Integer, Optional ByVal Force As Boolean = False)
Attribute GFXEngine_KeyUp.VB_MemberFlags = "40"
On Error Resume Next
    m_KeyState(m_KeyMap(KeyCode)) = False
    If DisableUserInput Then Exit Sub
    QueueEvent 1, , , CLng(m_KeyMap(KeyCode)), CLng(Shift)
End Sub

Public Sub GFXEngine_MouseDown(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single, Optional ByVal Force As Boolean = False)
Attribute GFXEngine_MouseDown.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
    If (DisableUserInput) And (Not Force) Then Exit Sub
    m_lngX = (X) / (m_objOutputPlugin.Window.ScaleWidth / ScreenWidth)
    m_lngY = (Y) / (m_objOutputPlugin.Window.ScaleHeight / ScreenHeight)
    g_lngMouseX = m_lngX
    g_lngMouseY = m_lngY
    g_lngMouseButtons = Button
    QueueEvent 2, X, Y, CLng(Button), CLng(Shift), m_lngX, m_lngY
End Sub

Public Sub GFXEngine_MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single, Optional ByVal Force As Boolean = False)
Attribute GFXEngine_MouseMove.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
    If (DisableUserInput) And (Not Force) Then Exit Sub
    m_lngX = (X) / (m_objOutputPlugin.Window.ScaleWidth / ScreenWidth)
    m_lngY = (Y) / (m_objOutputPlugin.Window.ScaleHeight / ScreenHeight)
    If (g_lngMouseX = m_lngX) And (g_lngMouseY = m_lngY) Then Exit Sub
    g_lngMouseX = m_lngX
    g_lngMouseY = m_lngY
    QueueEvent 3, X, Y, CLng(Button), CLng(Shift), m_lngX, m_lngY
End Sub

Public Sub GFXEngine_MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single, Optional ByVal Force As Boolean = False)
Attribute GFXEngine_MouseUp.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
    If (DisableUserInput) And (Not Force) Then Exit Sub
    m_lngX = (X) / (m_objOutputPlugin.Window.ScaleWidth / ScreenWidth)
    m_lngY = (Y) / (m_objOutputPlugin.Window.ScaleHeight / ScreenHeight)
    g_lngMouseX = m_lngX
    g_lngMouseY = m_lngY
    g_lngMouseButtons = 0
    QueueEvent 4, X, Y, CLng(Button), CLng(Shift), m_lngX, m_lngY
End Sub

Public Sub GFXEngine_Paint()
Attribute GFXEngine_Paint.VB_MemberFlags = "40"
On Error Resume Next
    If m_imgBackbuffer Is Nothing Then
        m_objOutputPlugin.ClearScreen 0
    Else
        m_imgBackbuffer.Dirty = True
        m_objOutputPlugin.Flip
    End If
End Sub

Sub GoFullscreen()
On Error Resume Next
    Err.Clear
    Transitions.FadeOut 1, , True
    Fullscreen = True
    m_objOutputPlugin.GoFullscreen
    Transitions.FadeIn 1, , True
End Sub

Sub GoWindowed()
On Error Resume Next
    Err.Clear
    Transitions.FadeOut 1, , True
    Fullscreen = False
    m_objOutputPlugin.GoWindowed
    Transitions.FadeIn 1, , True
End Sub

Public Function GrabScreen(Optional ByVal ForceRedraw As Boolean = True) As Fury2Image
On Error Resume Next
Dim l_imgCopy As Fury2Image
    If ForceRedraw Then
        Redraw
    End If
    Set l_imgCopy = F2Image(Backbuffer.Width, Backbuffer.Height)
    l_imgCopy.Copy Backbuffer
    Set GrabScreen = l_imgCopy
End Function

Public Sub HideAllPictures()
Attribute HideAllPictures.VB_Description = "Hides all active picture slots."
On Error Resume Next
Dim m_lngSlot As Long
    ActivePictureSlots = 0
    For m_lngSlot = 0 To c_lngMaxPictureSlot
        With OnscreenPictures(m_lngSlot)
            .Visible = False
            Set .Picture = Nothing
            .EndFadeScript = ""
            .Fading = False
        End With
    Next m_lngSlot
    FlushImageCache
End Sub

Sub HideConsole()
On Error Resume Next
    #If DebugFeatures = 1 Then
        Me.ConsoleStartOffset = ConsoleOffset
        Me.ConsoleEndOffset = ConsoleHeight
        Me.ConsoleStartTime = HiTimer
        Me.ConsoleEndTime = HiTimer + 0.33
        Me.ConsoleSliding = True
        ConsoleVisible = False
    #End If
    ConsoleText = ""
    Err.Clear
End Sub

Public Sub HidePicture(ByVal Slot As Long)
Attribute HidePicture.VB_Description = "Hides a picture slot."
On Error Resume Next
Dim m_lngSlot As Long
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With OnscreenPictures(Slot)
        .Visible = False
        Set .Picture = Nothing
        .EndFadeScript = ""
        .Fading = False
    End With
    For m_lngSlot = 0 To c_lngMaxPictureSlot
        With OnscreenPictures(m_lngSlot)
            If .Visible Then ActivePictureSlots = m_lngSlot + 1
        End With
    Next m_lngSlot
    Err.Clear
End Sub

Public Function HiTimer() As Double
Attribute HiTimer.VB_Description = "Returns an extremely accurate timer value."
Dim PCounter As Currency, PFrequency As Currency
    Call QueryPerformanceCounter(PCounter)
    Call QueryPerformanceFrequency(PFrequency)
    HiTimer = PCounter / PFrequency
End Function

Public Sub HookEvent(ByRef Name As String, ByVal EventHook)
Attribute HookEvent.VB_Description = "Redirects an engine event to a script function."
On Error Resume Next
    Select Case LCase(Trim(Name))
    Case "redraw"
        Set RedrawHook = BindEvent(EventHook)
    Case "update"
        Set UpdateHook = BindEvent(EventHook)
    Case "mousedown"
        Set MouseDownHook = BindEvent(EventHook)
    Case "mousemove"
        Set MouseMoveHook = BindEvent(EventHook)
    Case "mousedblclick"
        Set MouseDblClickHook = BindEvent(EventHook)
    Case "mouseup"
        Set MouseUpHook = BindEvent(EventHook)
    End Select
End Sub

Public Function HookEvents(ByVal Obj As Object, Optional ByVal Priority As Boolean = False) As Boolean
Attribute HookEvents.VB_Description = "Hooks an object into the engine's event system. Usually used for plugins."
On Error Resume Next
Dim l_plgObj As Fury2Plugin
    If Running Then Else Exit Function
    If Obj Is Nothing Then Exit Function
    Err.Clear
    If TypeOf Obj Is Fury2Plugin Then
        Set l_plgObj = Obj
        Set l_plgObj.Engine = Me
    Else
        Set Obj.Engine = Me
    End If
    If Err.Number <> 0 Then
        DebugOut "HookEvents failed on " & ToString(Obj)
    Else
        UnHookEvents Obj
        Obj.Startup
        If l_plgObj Is Nothing Then Else l_plgObj.Init
        If m_colPluginStack.Count > 0 Then
            If Priority Then
                m_colPluginStack.Add Obj
                m_colReversePluginStack.Add Obj, , 1
            Else
                m_colPluginStack.Add Obj, , 1
                m_colReversePluginStack.Add Obj
            End If
        Else
            m_colPluginStack.Add Obj
            m_colReversePluginStack.Add Obj
        End If
        HookEvents = True
    End If
    Err.Clear
End Function

Function HTTPGet(ByVal URL As String, Optional ByVal Port As Long = 80, Optional ByRef CompleteEvent = "", Optional ByRef ProgressEvent = "") As Fury2HTTPRequest
On Error Resume Next
Dim l_reqRequest As Fury2HTTPRequest
    Set l_reqRequest = New Fury2HTTPRequest
    With l_reqRequest
        .SetEngine Me
        Set .CompleteEvent = BindEvent(CompleteEvent)
        Set .ProgressEvent = BindEvent(ProgressEvent)
        .Go URL, Port
    End With
    Set HTTPGet = l_reqRequest
End Function

Public Property Get Icon() As String
Attribute Icon.VB_MemberFlags = "40"
    Icon = m_Icon
End Property

Public Property Let Icon(NewIcon As String)
On Error Resume Next
    m_Icon = NewIcon
    If Trim(m_Icon) = "" Or (Filesystem.FileExists(m_Icon) = False) Then
        SetAppIcon m_objOutputPlugin.Window
    Else
        SetWindowIcon m_objOutputPlugin.Window, Filesystem.File(m_Icon).GetRealFilename
    End If
End Property

Function IIf(ByVal Expression As Boolean, ByRef TruePart, Optional ByRef FalsePart = 0)
On Error Resume Next
    IIf = VBA.IIf(Expression, TruePart, FalsePart)
End Function

Friend Function iLoadImage(ByVal Filename As String, Optional ByVal IgnoreErrors As Boolean = False) As Long
On Error Resume Next
Dim m_lngImages As Long
Dim m_imgImage As Fury2Image
Dim m_strName As String
Dim m_filFile As Fury2File
Static m_lngHere As Long
    m_lngHere = m_lngHere + 1
    Err.Clear
    Filename = Filesystem.AutoSelectExtension(Filename, "png;f2g;tga;jpg;bmp;gif")
    m_strName = Filename
    If EditMode Then
    Else
        If m_Images.Count > 0 Then
            For m_lngImages = 1 To m_Images.Count
                If m_Images(m_lngImages) Is Nothing Then
                Else
                    If m_Images(m_lngImages).Name = LCase(Trim(Filename)) Then
                        iLoadImage = m_lngImages
                        m_lngHere = m_lngHere - 1
                        Exit Function
                    End If
                End If
            Next m_lngImages
        End If
        If Filesystem.FileExists(Filename) Then
            Set m_filFile = Filesystem.File(Filename)
            Set m_imgImage = F2LoadImage(m_filFile.GetRealFilename)
        Else
        End If
        If m_imgImage Is Nothing Then
            If IgnoreErrors Then
            Else
                If m_booErrorsOff Then
                Else
                    CriticalError "LoadImage", "LoadImage", "Unable to load image: """ + Filename + """"
                End If
            End If
            Exit Function
        End If
        SetImageTag m_imgImage.Handle, 1, 1234
        m_imgImage.Name = LCase(Trim(m_strName))
        m_Images.Add m_imgImage, LCase(Trim(m_strName))
        iLoadImage = m_Images.Count
        m_lngHere = m_lngHere - 1
        Err.Clear
    End If
End Function

Public Property Get Images()
Attribute Images.VB_Description = "The image cache."
On Error Resume Next
    Set Images = m_Images
End Property

Public Sub Include(ByVal Filename As String, Optional ByVal Require As Boolean = True)
Attribute Include.VB_Description = "Loads a script file."
On Error Resume Next
Dim l_strText As String
Dim l_strModule As String
    Err.Clear
    If Filesystem.FileExists(Filename + ".f2sf") Then
        Filename = Filename + ".f2sf"
    ElseIf Filesystem.FileExists(Filename + ".f2script") Then
        Filename = Filename + ".f2script"
    ElseIf Filesystem.FileExists(Filename + ".f2cfg") Then
        Filename = Filename + ".f2cfg"
    ElseIf Filesystem.FileExists(Filename + ".f2config") Then
        Filename = Filename + ".f2config"
    ElseIf (Filesystem.FileExists(Filename + ".vbs")) And (ScriptEngine.Language.Name = "VBScript") Then
        Filename = Filename + ".vbs"
    ElseIf (Filesystem.FileExists(Filename + ".js")) And (ScriptEngine.Language.Name = "JScript") Then
        Filename = Filename + ".js"
    ElseIf (Filesystem.FileExists(Filename + ".py")) And (ScriptEngine.Language.Name = "Python") Then
        Filename = Filename + ".py"
    End If
    If Require Then
        If Not Filesystem.FileExists(Filename) Then
            If m_booErrorsOff Then
            Else
                CriticalError "Include", "Include", "Script not found: " + ToString(Filename)
            End If
            Exit Sub
        End If
    Else
        If Not Filesystem.FileExists(Filename) Then
            Exit Sub
        End If
    End If
    ContextLevelAdd Filename
    l_strModule = VariableName(Filesystem.GetTitle(Filename))
    m_ScriptEngine.AddCode Filesystem.File(Filename).Text
    If Err <> 0 Then
        Err.Clear
'        m_ScriptEngine.Error.Clear
    End If
    ContextLevelRemove
End Sub

Public Sub InitDefaultControls()
On Error Resume Next
    Set Controls = F2Collection()
    Controls.Add F2Control("OK"), "OK"
    Controls.Add F2Control("Cancel"), "Cancel"
    Controls.Add F2Control("Quit"), "Quit"
    Controls.Add F2Control("Left"), "Left"
    Controls.Add F2Control("Up"), "Up"
    Controls.Add F2Control("Right"), "Right"
    Controls.Add F2Control("Down"), "Down"
    Controls("Left").Repeat = True
    Controls("Up").Repeat = True
    Controls("Right").Repeat = True
    Controls("Down").Repeat = True
    Controls("OK").Repeat = True
    Controls("Cancel").Repeat = True
    BindKey vbKeyA, Controls("OK")
    BindKey vbKeyS, Controls("Cancel")
    BindKey vbKeyLeft, Controls("Left")
    BindKey vbKeyUp, Controls("Up")
    BindKey vbKeyRight, Controls("Right")
    BindKey vbKeyDown, Controls("Down")
    BindKey vbKeyEscape, Controls("Quit")
End Sub

Public Sub InitJoystick()
On Error Resume Next
'    UnHookEvents Joystick
'    Set Joystick = Nothing
'    Set Joystick = New Fury2Joystick
'    HookEvents Joystick
End Sub

Public Sub InitScriptEngine()
On Error Resume Next
    Set m_ScriptEngine = New ScriptEngine
    Set m_ScriptEngine.Language = m_ScriptEngine.LoadLanguage(ScriptLanguage)
    m_ScriptEngine.Init 0
    m_ScriptEngine.AddObject "Engine", Me
    m_ScriptEngine.AddObject "Graphics", m_Graphics
    m_ScriptEngine.AddObject "Constants", m_objConstants
End Sub

Public Sub iTerminate()
On Error Resume Next
    Class_Terminate
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
        .WriteSegment_End
    End With
End Sub

Function JoinX(Arr As Variant, Optional ByVal Delimiter As String = " ", Optional ByVal QuoteStrings As Boolean = False) As String
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    JoinX = ""
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Function
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Function
    For m_lngItems = m_lngLB To m_lngUB
        JoinX = JoinX + ToString(Arr(m_lngItems), True)
        If m_lngItems < m_lngUB Then
            JoinX = JoinX + Delimiter
        End If
    Next m_lngItems
    Err.Clear
End Function

Public Property Get KeyMapping(ByVal Index As Long) As Long
On Error Resume Next
    KeyMapping = m_KeyMap(Index)
End Property

Public Property Let KeyMapping(ByVal Index As Long, ByVal NewValue As Long)
On Error Resume Next
    m_KeyMap(Index) = NewValue
End Property

Public Property Get KeyState(ByVal Key As Long) As Boolean
Attribute KeyState.VB_Description = "Returns the state of a key."
On Error Resume Next
    If (Key < 0) Or (Key > 255) Then Exit Property
    KeyState = m_KeyState(Key)
End Property

Public Property Let KeyState(ByVal Key As Long, ByVal Value As Boolean)
On Error Resume Next
    If (Key < 0) Or (Key > 255) Then Exit Property
    If m_KeyState(Key) <> Value Then
        m_KeyState(Key) = Value
        If VarType(KeyBindings(Key) = vbString) Then
        Else
            If Value Then
                KeyBindings(Key).Activate
            Else
                KeyBindings(Key).Deactivate
            End If
        End If
    End If
End Property

Function LoadBrush(ByVal Filename As String) As Object
On Error Resume Next
Dim m_brsBrush As Fury2Brush
Dim m_glbGlobals As Fury2Globals
    If Filesystem.FileExists(Filename) Then Else Exit Function
    Set m_glbGlobals = New Fury2Globals
    Set m_brsBrush = New Fury2Brush
    If LoadFromFile(m_brsBrush, Filesystem.File(Filename)) Then
        Set LoadBrush = m_brsBrush
    End If
    Set m_glbGlobals = Nothing
    Set m_brsBrush = Nothing
End Function

Public Sub LoadCursor(ByVal Filename As String)
Attribute LoadCursor.VB_MemberFlags = "40"
On Error Resume Next
    If Filesystem.FileExists(Filename) Then
        Filename = (Filesystem.File(Filename).GetRealFilename)
        Set m_objOutputPlugin.Window.MouseIcon = stdole.LoadPicture(CStr(Filename))
        m_objOutputPlugin.Window.MousePointer = 99
    End If
End Sub

Public Function LoadFlags(ByVal File As VirtualFile)
On Error Resume Next
Dim l_lngFlags As Long, l_lngCount As Long
Dim l_strName As String, l_varValue As Variant, l_lngType As Long, l_strType As String
    Set File = File
    If File Is Nothing Then Exit Function
    With File
        .ReadSegment_Begin
            .Load l_lngCount
            If .EOS Or .EOF Or (l_lngCount < 1) Then
            Else
                For l_lngFlags = 1 To l_lngCount
                    .Load l_strName
                    .Load l_lngType
                    Select Case l_lngType
                    Case vbString
                        l_varValue = ""
                    Case vbLong
                        l_varValue = CLng(0)
                    Case vbInteger
                        l_varValue = CInt(0)
                    Case vbBoolean
                        l_varValue = CBool(False)
                    Case vbSingle
                        l_varValue = CSng(0)
                    Case vbDouble
                        l_varValue = CDbl(0)
                    Case vbObject
                        .Load l_strType
                        l_varValue = Nothing
                        Set l_varValue = ScriptEngine.Eval("New " & l_strType)
                    End Select
                    .Load l_varValue
                    m_FlagNames.Add LCase(l_strName)
                    m_Flags.Add l_varValue, LCase(l_strName)
                Next l_lngFlags
            End If
        .ReadSegment_End
    End With
End Function

Public Function LoadFont(ByVal Filename As String) As Fury2Font
On Error Resume Next
    Set LoadFont = New Fury2Font
    LoadFont.SetEngine Me
    LoadFont.Load Filename
End Function

Public Function LoadFromFile(ByRef Obj As Object, ByVal File As Object) As Boolean
Attribute LoadFromFile.VB_MemberFlags = "40"
On Error Resume Next
Dim l_glbGlobals As New Fury2Globals
    LoadFromFile = l_glbGlobals.LoadFromFile(Obj, File)
End Function

Public Function LoadGame(ByVal Game As String) As Boolean
On Error Resume Next
Dim l_varScripts As Variant, l_lngScripts As Long
Dim l_varPackages As Variant, l_lngPackages As Long
Dim l_objObject As Object
    ContextLevelAdd "LoadGame(""" & Game & """)"
    DebugOut "Loading game """ & Game & """"
    Set ConsoleItems = F2Collection()
    Class_Terminate
    Class_Initialize
    If Trim(Game) = "" Then
        If m_booIDE Then
            If Right(App.Path, 1) = "\" Then
                SystemRoot = App.Path + "..\..\binary\"
            Else
                SystemRoot = App.Path + "\..\..\binary\"
            End If
        Else
            If Right(App.Path, 1) = "\" Then
                SystemRoot = App.Path
            Else
                SystemRoot = App.Path + "\"
            End If
        End If
    Else
        SystemRoot = FixPath(Game)
    End If
    Filesystem.Root = RootPath
'    Filesystem.LoadPackage "game"
    Configuration.Load "game.f2config"
    Configuration.Load "global.f2config"
    ParseConfiguration
    InitScriptEngine
    If Not EditMode Then
        Include "constants", False
        Include "settings.vbs", False
        Include "game.f2cfg", False
        Include "global.f2cfg", False
        ScriptEngine.AddObject "Enums", New Fury2GEEnums
        ScriptEngine.AddObject "Types", New Fury2GETypes
        ScriptEngine.AddObject "Globals", New Fury2Globals
        If Not (HideConfiguration) Then
            Load frmConfigure
            frmConfigure.Cancelled = False
            Set frmConfigure.Engine = Me
            frmConfigure.LoadSettings
            frmConfigure.Show
            frmConfigure.SetFocus
            Do While frmConfigure.Visible
                If GetActiveWindow <> frmConfigure.hwnd Then frmConfigure.SetFocus
                Call SleepEx(1, True)
                DoEvents
            Loop
            If frmConfigure.Cancelled Then
                ContextLevelRemove
                Terminating = True
                TerminateEngine = True
                Exit Function
            End If
            Unload frmConfigure
        End If
        If InIDE Then
            If MsgBox("Disable sound?", vbYesNo) = vbYes Then DisableSound = True
        End If
        If ScreenWidth = 0 Then ScreenWidth = 320
        If ScreenHeight = 0 Then ScreenHeight = 240
        If ScreenWidth < 4 Or ScreenHeight < 4 Then
            CriticalError "LoadGame", "Sanity Check", "ScreenWidth and ScreenHeight must be > 4"
            ContextLevelRemove
            Exit Function
        End If
        ScreenWidth = ClipValue(ScreenWidth, c_lngMinScreenWidth, c_lngMaxScreenWidth)
        ScreenHeight = ClipValue(ScreenHeight, c_lngMinScreenHeight, c_lngMaxScreenHeight)
        ConsoleHeight = ScreenHeight / 2
        ConsoleOffset = ConsoleHeight
        If Trim(WindowCaption) = "" Then WindowCaption = "Unknown Game"
        Err.Clear
        SwitchOutputPlugin OutputPlugin
        If m_objOutputPlugin Is Nothing Then
            CriticalError "LoadGame", "LoadGFXPlugin """ + CStr(OutputPlugin) + """", "Unable to load output plugin"
            ContextLevelRemove
            Exit Function
        End If
        Err.Clear
        If Trim(Configuration.Settings("Icon")) <> "" Then
            Icon = Configuration.Settings("Icon")
        End If
        Set m_imgMouseBuffer = New Fury2Image
        Set m_imgBackbuffer = m_objOutputPlugin.Backbuffer
        m_imgBackbuffer.Name = "Backbuffer"
        If m_imgBackbuffer Is Nothing Then
            CriticalError "LoadGame", "Set m_imgBackbuffer = m_objOutputPlugin.Backbuffer", "Failed to initialize backbuffer"
            ContextLevelRemove
            Exit Function
        End If
        If m_objOutputPlugin.SupportsGamma Then
        Else
        End If
        Dim l_newMap As Fury2Map
        Set l_newMap = New Fury2Map
        l_newMap.SetEngine Me
        Maps.Add l_newMap
        Cameras.Add New Fury2Camera
        Cameras(1).SetEngine Me
        Cameras(1).Reposition 0, 0, ScreenWidth, ScreenHeight
        Cameras(1).Map = 1
    End If
    Err.Clear
    l_varPackages = Split(Filesystem.File("packages.f2config").Text, vbCrLf)
    l_lngPackages = UBound(l_varPackages)
    If Err = 0 Then
        For l_lngPackages = LBound(l_varPackages) To UBound(l_varPackages)
'            Filesystem.LoadPackage Trim(l_varPackages(l_lngPackages))
        Next l_lngPackages
    End If
    If Not EditMode Then
        Include "party", False
        Include "system", False
        Include "joystick", False
        Err.Clear
        l_varScripts = Split(Filesystem.File("scripts.f2config").Text, vbCrLf)
        l_lngScripts = UBound(l_varScripts)
        If Err = 0 Then
            For l_lngScripts = LBound(l_varScripts) To UBound(l_varScripts)
                Include Trim(l_varScripts(l_lngScripts))
            Next l_lngScripts
        End If
        If m_booIDE Then DebugOut "Starting game"
        m_objNotify.Init
        Include "init", False
        ContextLevelAdd "Engine_Init"
        ScriptEngine.Exec "Engine_Init"
        ContextLevelRemove
        Running = True
        m_objOutputPlugin.Window.SetFocus
        If m_objOutputPlugin.Fullscreen Then SetTopmost m_objOutputPlugin.Window, True
        LoadProgress "Game", 0
        For Each l_objObject In EventHooks
            l_objObject.Init
        Next l_objObject
        LoadProgress "Game", 10
        Set Font = New Fury2Font
        Font.SetEngine Me
        If Trim(LCase(DefaultFont)) = "none" Then
        ElseIf DefaultFont <> "" Then
            If Font.Load(Replace(DefaultFont, "ttf:", "ttf;", , , vbTextCompare)) Then
                Err.Clear
            Else
                CriticalError "LoadGame", "Load Default Font", "Unable to load default font:" + vbCrLf + DefaultFont
                ContextLevelRemove
                Exit Function
            End If
        Else
            DebugOut "No default font specified"
        End If
        LoadProgress "Game", 15
        Set WindowSkin = New Fury2WindowSkin
        WindowSkin.SetEngine Me
        If Trim(LCase(DefaultWindowSkin)) = "none" Then
        ElseIf DefaultWindowSkin <> "" Then
            If WindowSkin.Load(DefaultWindowSkin) Then
                Err.Clear
            Else
                CriticalError "LoadGame", "Load Default Window Skin", "Unable to load default window skin:" + vbCrLf + DefaultWindowSkin
                ContextLevelRemove
                Exit Function
            End If
        Else
            DebugOut "No default window skin specified"
        End If
        Set MenuCursor = New Fury2Graphic
        MenuCursor.SetEngine Me
        If Trim(LCase(DefaultCursor)) = "none" Then
        ElseIf DefaultCursor <> "" Then
            Set MenuCursor = F2Graphic(LoadImage(Trim(Split(DefaultCursor, "@")(0))), Split(Split(DefaultCursor, "@")(1), ",")(0), Split(Split(DefaultCursor, "@")(1), ",")(1))
        Else
            DebugOut "No default cursor specified"
        End If
        LoadProgress "Game", 20
        If Trim(StartMap) = "" Then
        Else
            LoadMap StartMap, StartX, StartY, StartLayer
            LoadProgress "Game", 80
            Cameras(1).Map = 1
            Cameras(1).Init
            Set Map = Maps(1)
        End If
        Cameras(1).Redraw
        LoadProgress "Game", 90
        FlushImageCache
        LoadProgress "Game", 100
        m_objOutputPlugin.Window.SetFocus
        Include "start", False
        Err.Clear
        GameTime = 0
        CurrentGame = Game
        Cameras.Dirty
        EventQueueLength = 0
        ResetKeys
        LoadGame = True
    End If
    ContextLevelRemove
End Function

Public Function LoadImage(ByVal Filename As String, Optional ByVal IgnoreErrors As Boolean = False) As Fury2Image
Attribute LoadImage.VB_Description = "Loads an image file into the cache and returns an image object."
On Error Resume Next
Dim m_imgPicture As Fury2Image
    If Trim(CStr(Filename)) = "" Then Exit Function
    Err.Clear
    Set m_imgPicture = Nothing
    If EditMode Then
        Filename = Filesystem.AutoSelectExtension(Filename, "png;f2g;tga;jpg;bmp;gif")
        If Filesystem.FileExists(Filename) Then
            Set m_imgPicture = F2LoadImage(Filesystem.File(Filename).GetRealFilename)
            m_imgPicture.Name = Filename
        End If
    Else
        Filename = LCase(Trim(Filesystem.AutoSelectExtension(Filename, "png;f2g;tga;jpg;bmp;gif")))
        Set m_imgPicture = m_Images(Filename)
        If (m_imgPicture Is Nothing) Or (Err.Number <> 0) Then
            Err.Clear
            Set m_imgPicture = m_Images(Me.iLoadImage(Filename, IgnoreErrors))
        End If
        If m_imgPicture Is Nothing Then
            'If EditMode = False Then CriticalError "LoadImage", "LoadImage", "Unable to load image: """ + filename + """"
        End If
    End If
    Set LoadImage = m_imgPicture
End Function

Public Function LoadImages(ParamArray Filenames() As Variant) As Variant
On Error Resume Next
Dim l_imgImages As Variant
Dim l_lngImages As Long
    If UBound(Filenames) = LBound(Filenames) Then
        ReDim l_imgImages(LBound(Filenames(0)) To UBound(Filenames(0)))
        For l_lngImages = LBound(Filenames(0)) To UBound(Filenames(0))
            Set l_imgImages(l_lngImages) = LoadImage(Filenames(0)(l_lngImages))
        Next l_lngImages
    Else
        ReDim l_imgImages(LBound(Filenames) To UBound(Filenames))
        For l_lngImages = LBound(Filenames) To UBound(Filenames)
            Set l_imgImages(l_lngImages) = LoadImage(Filenames(l_lngImages))
        Next l_lngImages
    End If
    LoadImages = l_imgImages
End Function

Public Sub LoadKeyMappings(ByVal File As VirtualFile)
On Error Resume Next
Dim l_lngKey As Long
    If Err <> 0 Then Exit Sub
    With File
        .ReadSegment_Begin
            For l_lngKey = 0 To 255
                .Load m_KeyMap(l_lngKey)
            Next l_lngKey
        .ReadSegment_End
    End With
End Sub

Public Sub LoadMap(ByVal Filename As String, Optional ByVal LoadX As Single = vbNull, Optional ByVal LoadY As Single = vbNull, Optional ByVal Layer As Long = -1, Optional ByVal Index As Long = 1)
Attribute LoadMap.VB_Description = "Loads a map."
On Error Resume Next
Dim m_gblGlobal As Fury2Globals
Dim m_lngSprites As Long, m_lngLayers As Long
Dim m_mapNew As Fury2Map
Dim l_mapOld As Fury2Map
Dim m_strOldFn As String
    Set m_gblGlobal = New Fury2Globals
    ContextLevelAdd "LoadMap(""" + Filename + """, " + CStr(Index) + ")"
    Err.Clear
    Set l_mapOld = Maps(Index)
    Err.Clear
    m_strOldFn = Trim(CStr(Filename))
    Filename = (CStr(Filename))
    Filename = Filesystem.AutoSelectExtension(Filename, "f2m;f2map")
    If Not Filesystem.FileExists(Filename) Then
        CriticalError "LoadMap", "Filesystem.FileExists(Filename)", "Map not found: " + CStr(Filename)
    Else
        Set m_mapNew = New Fury2Map
        m_mapNew.SetEngine Me
        If LoadFromFile(m_mapNew, Filesystem.File(Filename)) Then
        Else
            CriticalError "LoadMap", "LoadFromFile(m_mapNew, Filesystem.File(filename))", "Map Load Failed"
        End If
        m_mapNew.Filename = Filename
        mvarMaps.Remove Index
        mvarMaps.Add m_mapNew, , Index
        If Camera.Map = 0 Then
            Camera.Map = Index
        End If
        m_mapNew.Init False
        Set Map = m_mapNew
        m_mapNew.SetEngine Me
        m_mapNew.Load
        If m_mapNew.Sprites.Count > 0 Then
            For m_lngLayers = 1 To m_mapNew.Layers.Count
                With m_mapNew.Layers(m_lngLayers)
                    For m_lngSprites = 1 To .Sprites.Count
                            Set IObjThis = .Sprites(m_lngSprites)
                            ContextLevelAdd "Layers(" & m_lngLayers & ").Sprites(""" + IObjThis.Name + """).Script.Start"
                            .Sprites(m_lngSprites).Script.Start
                            ContextLevelRemove
                    Next m_lngSprites
                End With
            Next m_lngLayers
        End If
        Party.InitMap CLng(Index), CSng(LoadX), CSng(LoadY), CLng(Layer)
        m_ScriptEngine.Exec "Engine_MapLoad", CLng(Index), CSng(LoadX), CSng(LoadY), CLng(Layer)
        m_ScriptEngine.Exec "Engine_MapLoad", CLng(Index), CSng(LoadX), CSng(LoadY)
        m_ScriptEngine.Exec "Engine_MapLoad", CLng(Index)
        m_ScriptEngine.Exec "Engine_MapLoad"
        MapLoad m_strOldFn
        DebugOut "Map """ & Filename & """ loaded"
        If Debugger Is Nothing Then Else Debugger.MapSwitch Filename
    End If
    Cameras.Dirty
    Set Map = Maps(Index)
    l_mapOld.Unload
    l_mapOld.Free
    FlushImageCache
    ContextLevelRemove
    m_booTrace = False
End Sub

Public Sub LoadMapState(ByVal Index As Long, ByRef Name As String)
Attribute LoadMapState.VB_Description = "Loads the current state of a map."
On Error Resume Next
    Maps.Remove Index
    Maps.Add SavedMaps(Name), , Index
    SavedMaps.Remove Name
End Sub

Public Function LoadPlugin(ByRef DLLFile As String, ByRef PluginName As String) As Object
Attribute LoadPlugin.VB_Description = "Loads a plugin from a DLL."
On Error Resume Next
Dim l_filFile As Fury2File
    If Filesystem.FileExists(DLLFile) Then
        Set l_filFile = Filesystem.File(DLLFile)
    ElseIf FileLen(App.Path & "\" & Replace(DLLFile, "/", "\")) > 0 Then
        ' In engine dir
    Else
        CriticalError "LoadPlugin", "File check", "Unable to load plugin """ & PluginName & """, """ & DLLFile & """ not found"
    End If
    Err.Clear
    Set LoadPlugin = CreateObject(PluginName)
    If LoadPlugin Is Nothing Or Err.Number <> 0 Then
        If l_filFile Is Nothing Then Exit Function
        Err.Clear
        RegisterDLL l_filFile.GetRealFilename
        DoEvents
        Set LoadPlugin = CreateObject(PluginName)
        If LoadPlugin Is Nothing Or Err.Number <> 0 Then
        Else
            Set LoadPlugin.Engine = Me
            LoadPlugin.Plugin_Load
            DebugOut "Plugin " & ToString(LoadPlugin) & " loaded"
        End If
    Else
        Set LoadPlugin.Engine = Me
        LoadPlugin.Plugin_Load
        DebugOut "Plugin " & ToString(LoadPlugin) & " loaded"
    End If
    If LoadPlugin Is Nothing Then
        CriticalError "LoadPlugin", "Error check", "Unable to load plugin """ & PluginName & """"
    End If
    Err.Clear
End Function

Friend Sub LoadProgress(LoadType As String, Progress As Single)
On Error Resume Next
'    ContextLevelAdd "Engine_LoadProgress"
    Err.Clear
    m_ScriptEngine.Exec "Engine_LoadProgress", LoadType, Progress
'    ContextLevelRemove
End Sub

Function LoadSprite(ByVal Filename As String, Optional ByVal Index = 1) As Fury2Sprite
On Error Resume Next
Dim m_sprSprites As Fury2Sprites
    Err.Clear
    Set m_sprSprites = LoadSprites(Filename)
    If m_sprSprites Is Nothing Then Exit Function
    Set LoadSprite = m_sprSprites(Index)
    Set m_sprSprites = Nothing
    If Err <> 0 Then
        Set LoadSprite = Nothing
    End If
End Function

Function LoadSprites(ByVal Filename As String) As Fury2Sprites
Attribute LoadSprites.VB_Description = "Loads sprites from a sprite collection."
On Error Resume Next
Dim m_sprSprites As Fury2Sprites
Dim m_glbGlobals As Fury2Globals
    Filename = Filesystem.AutoSelectExtension(Filename, "f2sprites;f2sprite")
    If Not Filesystem.FileExists(Filename) Then Exit Function
    ContextLevelAdd "LoadSprites(""" & Filename & """)"
    Set m_glbGlobals = New Fury2Globals
    Set m_sprSprites = New Fury2Sprites
    m_sprSprites.SetEngine Me
    If LoadFromFile(m_sprSprites, Filesystem.File(Filename)) Then
        Set LoadSprites = m_sprSprites
    Else
        DebugOut "Unable to load sprite collection """ & Filename & """"
    End If
    Set m_glbGlobals = Nothing
    Set m_sprSprites = Nothing
    ContextLevelRemove
End Function

Public Function LoadWindowSkin(ByVal Filename As String) As Fury2WindowSkin
On Error Resume Next
    Set LoadWindowSkin = New Fury2WindowSkin
    LoadWindowSkin.SetEngine Me
    LoadWindowSkin.Load Filename
End Function

Private Sub m_ScriptEngine_Error(ByVal LineNumber As Long, ByVal Character As Long, Description As String)
On Error Resume Next
Dim l_strError As String
Dim l_strDesc As String
    l_strDesc = Description
    l_strError = "  Line " & CStr(LineNumber) & ", Character " & Character & VBA.IIf(Trim(ScriptContext) <> "", vbCrLf & "  Context: " & ScriptContext, "")
    If m_booErrorsOff Then
        m_booErrorOccurred = True
        ErrorMessage l_strDesc, l_strError
    Else
        m_booErrorOccurred = True
        ErrorMessage l_strDesc, l_strError
    End If
End Sub

Private Sub m_ScriptEngine_ExecutionBegin()
On Error Resume Next
End Sub

Private Sub m_ScriptEngine_ExecutionEnd()
On Error Resume Next
End Sub

Public Sub MapLoad(ByVal Filename As String)
Attribute MapLoad.VB_MemberFlags = "40"
On Error Resume Next
Dim Obj As Object
    For Each Obj In m_colPluginStack
        Obj.MapLoad CStr(Filename)
    Next Obj
End Sub

Public Property Get Maps() As Fury2Maps
Attribute Maps.VB_Description = "The maps currently loaded in the engine. There is usually only one, but it is possible to have multiple maps running at once."
    Set Maps = mvarMaps
End Property

Public Function Maximum(ParamArray Values() As Variant) As Variant
On Error Resume Next
Dim l_lngItems As Long, l_varBiggest As Variant
    l_varBiggest = Empty
    For l_lngItems = LBound(Values) To UBound(Values)
        If (l_varBiggest = Empty) Or (l_varBiggest < Values(l_lngItems)) Then
            l_varBiggest = Values(l_lngItems)
        End If
    Next l_lngItems
    Maximum = l_varBiggest
End Function

Public Function MessageBox(Text, Optional ByVal Area As Fury2Rect = Nothing, Optional ByVal Portrait As Object = Nothing, Optional CloseEvent = Nothing, Optional ByVal TextColor As Long = -1, Optional ByVal Alpha As Single = 1, Optional ByVal Queue As Boolean = False, Optional ByVal Hook As Boolean = True) As Fury2MessageWindow
Attribute MessageBox.VB_Description = "Opens a message box."
On Error Resume Next
Dim NewMessage As Fury2MessageWindow
    Set NewMessage = New Fury2MessageWindow
    With NewMessage
        Set .Engine = Me
        If Queue Then
            If MessageQueue.Count > 0 Then
                .First = False
                MessageQueue(MessageQueue.Count).Last = False
                .Last = True
            Else
                .First = True
                .Last = True
            End If
            .Queued = True
        End If
        If Area Is Nothing Then
            Set .Rectangle = F2Rect(5, 5 + ((ScreenHeight - 10) * DefaultMessageTop * 0.6), ScreenWidth - 10, (ScreenHeight - 10) * 0.4, False)
        Else
            Set .Rectangle = Area
        End If
        .Alpha = Alpha
        .Text = ToString(Text)
        If Portrait Is Nothing Then
        Else
            Set .Image = Portrait
        End If
        Set .Font = Me.Font
        .TextColor = TextColor
        Set .ClickEvent = NullEvent
        Set .CloseEvent = BindEvent(CloseEvent)
        If WindowSkin Is Nothing Then
            Set .WindowSkin = Me.WindowSkin
        Else
            Set .WindowSkin = WindowSkin
        End If
        .WindowSkin.AdjustRectangle .Rectangle
'        .ShadowText = DefaultFontShadow
    End With
    If Hook Then
        If Queue Then
            MessageQueue.Add NewMessage
            If MessageQueue.Count = 1 Then HookEvents NewMessage
        Else
            MessageWindows.Add NewMessage
            HookEvents NewMessage
        End If
    End If
    Set MessageBox = NewMessage
End Function

Public Function MessageBoxQueue(Text, Optional ByVal Area As Fury2Rect = Nothing, Optional ByVal Portrait As Object = Nothing, Optional CloseEvent = Nothing, Optional ByVal TextColor As Long = -1, Optional ByVal Alpha As Single = 1, Optional ByVal Hook As Boolean = True) As Fury2MessageWindow
On Error Resume Next
    Set MessageBoxQueue = MessageBox(Text, Area, Portrait, CloseEvent, TextColor, Alpha, True, Hook)
End Function

Public Function Minimum(ParamArray Values() As Variant) As Variant
On Error Resume Next
Dim l_lngItems As Long, l_varSmallest As Variant
    l_varSmallest = Empty
    For l_lngItems = LBound(Values) To UBound(Values)
        If (l_varSmallest = Empty) Or (l_varSmallest > Values(l_lngItems)) Then
            l_varSmallest = Values(l_lngItems)
        End If
    Next l_lngItems
    Minimum = l_varSmallest
End Function

Public Sub MovePicture(ByVal Slot As Long, Optional ByVal NewX As Long = -32767, Optional ByVal NewY As Long = -32767, Optional ByVal NewEffectMode As SFXBlitModes = BlitMode_Default, Optional ByVal NewAlpha As Single = -1)
Attribute MovePicture.VB_Description = "Repositions a picture slot."
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With OnscreenPictures(Slot)
        If NewX <> -32767 Then
            m_lngWidth = .DestRect.Width
            .DestRect.Left = NewX
            .DestRect.Right = NewY + m_lngWidth
        End If
        If NewY <> -32767 Then
            m_lngHeight = .DestRect.Height
            .DestRect.Top = NewY
            .DestRect.Bottom = NewY + m_lngHeight
        End If
        If NewAlpha <> -1 Then .Alpha = CSng(NewAlpha)
        If NewEffectMode <> BlitMode_Default Then .EffectMode = CLng(NewEffectMode) + 1
    End With
End Sub

Public Sub Mute()
On Error Resume Next
    OldVolume = SoundEngine.Volume
    SoundEngine.Volume = 0
End Sub

Public Function NormalizeAngle(ByVal Angle As Single) As Single
Dim N As Single
    N = Angle / 360
    NormalizeAngle = Angle - (Floor(N) * 360)
End Function

Public Sub Notify_CriticalError()
Attribute Notify_CriticalError.VB_MemberFlags = "40"
    CriticalError "Unknown", "Unknown"
End Sub

Public Function PadString(Text, ByVal Length As Long, Optional ByVal PaddingCharacter As String = " ") As String
Attribute PadString.VB_Description = "Pads a string to a certain length."
On Error Resume Next
Dim m_strText As String, m_lngLength As Long
    m_strText = ToString(Text)
    m_lngLength = Len(m_strText)
    If m_lngLength < Length Then
        PadString = String(Length - Len(m_strText), PaddingCharacter) + m_strText
    ElseIf m_lngLength > Length Then
        PadString = Right(m_strText, Length)
    Else
        PadString = m_strText
    End If
End Function

Public Function PadStringR(Text, ByVal Length As Long, Optional ByVal PaddingCharacter As String = " ") As String
Attribute PadStringR.VB_Description = "Pads a string to a certain length, padding on the right."
On Error Resume Next
Dim m_strText As String, m_lngLength As Long
    m_strText = ToString(Text)
    m_lngLength = Len(m_strText)
    If m_lngLength < Length Then
        PadStringR = m_strText + String(Length - Len(m_strText), PaddingCharacter)
    ElseIf m_lngLength > Length Then
        PadStringR = Left(m_strText, Length)
    Else
        PadStringR = m_strText
    End If
End Function

Public Sub Panic()
Attribute Panic.VB_MemberFlags = "40"
    TerminateProcess GetCurrentProcess, 1
End Sub

Public Function ParseCodes(ByRef Str As String) As String
On Error Resume Next
    ParseCodes = ScriptEngine.ParseCodes(Str)
End Function

Friend Sub ParseConfiguration()
On Error Resume Next
Dim l_varValues, l_strSetting As String
    With Configuration
    
        l_varValues = Split(.Settings("Screen Mode"), "@")
        BitDepth = CLng(l_varValues(1))
        l_varValues = Split(l_varValues(0), "x")
        ScreenWidth = CLng(l_varValues(0))
        ScreenHeight = CLng(l_varValues(1))
        ScreenScaleRatio = CSng(.Settings("Screen Scale Ratio"))
        
        Fullscreen = .ToBoolean("Fullscreen")
        VSync = .ToBoolean("Vertical Sync")
        
        Icon = .Settings("Icon")
        GameName = .Settings("Title")
        WindowCaption = .Settings("Title")
        
        If Trim(.Settings("Framerate") = "~") Then
            DesiredFramerate = 60
            BalanceFramerate = False
        Else
            DesiredFramerate = CLng(Split(.Settings("Framerate"), "fps")(0))
            BalanceFramerate = True
        End If
        
        SpecialFX = .ToBoolean("Special FX")
        OutputPlugin = .Settings("Output Plugin")
        VSync = .ToBoolean("Vertical Sync")
        
        ShowTitlebar = .ToBoolean("Show Titlebar")
        ShowFPS = .ToBoolean("Show Framerate")
        
        StartMap = .Settings("Default Map")
        l_varValues = Split(.Settings("Default Start Position"), "@")
        StartLayer = CLng(l_varValues(0))
        l_varValues = Split(l_varValues(1), ",")
        StartX = CSng(l_varValues(0))
        StartY = CSng(l_varValues(1))
        
        DefaultWindowSkin = .Settings("Default Window Skin")
        DefaultCursor = .Settings("Default Cursor")
        DefaultFont = .Settings("Default Font")
        DefaultTransition = Which(LCase(Trim(.Settings("Default Transition"))), "fade", "crossfade", "smooth fade left", "smooth fade right", "smooth fade up", "smooth fade down", "slide left", "slide right", "slide up", "slide down", "circle in")
        DefaultTransitionLength = CSng(.Settings("Default Transition Length"))
        
        DisableSound = Not .ToBoolean("Enable Sound")
        
        ScriptLanguage = .Settings("Language")
        
        DefaultBGMVolume = CSng(.Settings("Music Volume"))
                
        HideConfiguration = Not .ToBoolean("Show Configuration Dialog")
        
    End With
End Sub

Public Function ParseString(ByRef Str)
Attribute ParseString.VB_Description = "Parses inline scripts in a string."
On Error Resume Next
Dim m_strText As String, m_strConstant As String, m_strValue As String
Dim m_strOldText As String
Dim m_lngStartPos As Long, m_lngLength As Long, m_lngPos As Long, m_lngRelPos
Dim m_lngRelStartPos As Long
    ContextLevelAdd "ParseString"
    m_strText = ToString(Str)
    m_strOldText = m_strText
    m_lngPos = 1
    m_lngRelPos = 1
    m_lngStartPos = InStr(m_lngPos, m_strOldText, "<%=")
    m_lngRelStartPos = InStr(m_lngRelPos, m_strText, "<%=")
    If m_lngStartPos <> 0 Then
        Do While m_lngStartPos <> 0
            If m_lngRelPos > Len(m_strText) Then Exit Do
            If InStr(m_lngRelPos, m_strText, "%>") = 0 Then Exit Do
            m_lngLength = InStr(m_lngPos, m_strOldText, "%>") - (m_lngStartPos + 3)
            m_strConstant = Mid(m_strOldText, m_lngStartPos + 3, m_lngLength)
            m_strValue = "<error>"
            m_strValue = ToString(m_ScriptEngine.Eval(m_strConstant))
            If (Err.Number <> 0) Then m_strValue = "<error>"
            m_strText = Left(m_strText, m_lngRelStartPos - 1) + m_strValue + Mid(m_strText, m_lngRelStartPos + m_lngLength + 5)
            m_lngPos = m_lngStartPos + m_lngLength + 5
            m_lngRelPos = m_lngRelStartPos + Len(m_strValue)
            m_lngStartPos = InStr(m_lngPos, m_strOldText, "<%=")
            m_lngRelStartPos = InStr(m_lngRelPos, m_strText, "<%=")
        Loop
    End If
    ParseString = m_strText
    ContextLevelRemove
End Function

Public Function PathTarget(ByVal x1 As Single, ByVal y1 As Single, ByVal Angle As Single, ByVal Distance As Single, Optional ByRef x2, Optional ByRef y2)
Attribute PathTarget.VB_Description = "Takes the current location (x1 and y1), angle, and speed, and places the estimated next point in x2 and y2."
On Error Resume Next
Dim m_sngAngle As Single
Dim m_sngX As Single, m_sngY As Single
Dim m_sngDist As Single
    If Angle = -1 Then
        x2 = x1
        y2 = y1
        PathTarget = Array(x1, y1)
        Exit Function
    End If
    m_sngDist = CSng(Distance)
    m_sngAngle = NormalizeAngle(Angle) * c_dblRadian
    m_sngX = x1 + (Sin(m_sngAngle) * m_sngDist)
    m_sngY = y1 + -(Cos(m_sngAngle) * m_sngDist)
    PathTarget = Array(m_sngX, m_sngY)
    x2 = m_sngX
    y2 = m_sngY
End Function

Sub PlayBGM(Filename As String, Optional ByVal Fade As Boolean = True)
Attribute PlayBGM.VB_Description = "Loads a BGM and plays it, stopping any previously playing BGMs."
On Error Resume Next
    If CurrentBGMHandle <> 0 Then StopBGM False
    If m_SoundEngine.EnableMusic Then
        CurrentBGM = Filename
        CurrentBGMHandle = m_SoundEngine.Load(Filename, True)
        m_SoundEngine.Handles(CurrentBGMHandle).Play
        If Fade Then
            m_SoundEngine.Handles(CurrentBGMHandle).Volume = 0
            FadeBGM , DefaultBGMVolume
        Else
            m_SoundEngine.Handles(CurrentBGMHandle).Volume = DefaultBGMVolume
        End If
    End If
    Err.Clear
End Sub

Public Property Get Player() As Object
On Error Resume Next
    Set Player = Party.Item(Party.Leader)
End Property

Sub PlaySample(ByVal Filename As String)
On Error Resume Next
Dim m_Handle As Long
    m_Handle = SoundEngine.LoadSample(Filename, False)
    SoundEngine.Handles(m_Handle).Play
    Err.Clear
End Sub

Sub PopBGM(Optional ByVal Crossfade As Boolean = True)
On Error Resume Next
    SwitchBGM OldBGM, Crossfade
    OldBGM = ""
    Err.Clear
End Sub

Public Sub PopContext()
Attribute PopContext.VB_MemberFlags = "40"
On Error Resume Next
    #If DebugFeatures = 1 Then
        ScriptContext = CStr(m_colContextStack(m_colContextStack.Count))
        m_colContextStack.Remove m_colContextStack.Count
    #End If
End Sub

Public Sub PopEventHooks()
On Error Resume Next
Dim l_objObject As Object
    If EventHookStack.Count > 0 Then
        Set m_colPluginStack = EventHookStack.Item(EventHookStack.Count)
        Set m_colReversePluginStack = New Collection
        For Each l_objObject In m_colPluginStack
            If (m_colReversePluginStack.Count = 0) Then
                m_colReversePluginStack.Add l_objObject
            Else
                m_colReversePluginStack.Add l_objObject, , 1
            End If
        Next l_objObject
        EventHookStack.Remove EventHookStack.Count
    End If
End Sub

Sub PopHotspots()
On Error Resume Next
    Hotspots = SavedHotspots
End Sub

Sub PopPictures()
On Error Resume Next
Dim l_lngSlot As Long
    For l_lngSlot = 0 To c_lngMaxPictureSlot
        OnscreenPictures(l_lngSlot) = SavedPictures(l_lngSlot)
    Next l_lngSlot
    ActivePictureSlots = SavedActivePictureSlots
End Sub

Sub PushBGM(ByVal Filename As String, Optional ByVal Crossfade As Boolean = True)
Attribute PushBGM.VB_MemberFlags = "40"
On Error Resume Next
    OldBGM = CurrentBGM
    SwitchBGM Filename, Crossfade
    Err.Clear
End Sub

Public Sub PushContext(ByRef NewValue As String)
Attribute PushContext.VB_MemberFlags = "40"
On Error Resume Next
    #If DebugFeatures = 1 Then
    m_colContextStack.Add ScriptContext
    ScriptContext = NewValue
    If m_booTrace = True Then
        Trace ScriptContext
    End If
    #End If
End Sub

Public Sub PushEventHooks()
On Error Resume Next
    EventHookStack.Add m_colPluginStack
    Set m_colPluginStack = New Collection
    Set m_colReversePluginStack = New Collection
End Sub

Sub PushHotspots()
On Error Resume Next
    SavedHotspots = Hotspots
End Sub

Sub PushPictures()
On Error Resume Next
Dim l_lngSlot As Long
    For l_lngSlot = 0 To c_lngMaxPictureSlot
        SavedPictures(l_lngSlot) = OnscreenPictures(l_lngSlot)
    Next l_lngSlot
    SavedActivePictureSlots = ActivePictureSlots
End Sub

Friend Sub QueueEvent(ByVal EventType As Long, Optional ByVal SParam1 As Single = 0, Optional ByVal SParam2 As Single = 0, Optional ByVal LParam1 As Long = 0, Optional ByVal LParam2 As Long = 0, Optional ByVal LParam3 As Long = 0, Optional ByVal LParam4 As Long = 0)
On Error Resume Next
Dim Evt As Fury2QueueEvent
    If EventQueueLength >= c_lngMaxEventQueue Then Exit Sub
    If Halted Then Exit Sub
    EventQueueLength = EventQueueLength + 1
    With Evt
        .Type = EventType
        .SParam1 = SParam1
        .SParam2 = SParam2
        .LParam1 = LParam1
        .LParam2 = LParam2
        .LParam3 = LParam3
        .LParam4 = LParam4
    End With
    EventQueue(EventQueueLength - 1) = Evt
End Sub

Public Sub Quit()
Attribute Quit.VB_Description = "Shuts down the engine and unloads it from memory."
On Error Resume Next
    ContextLevelAdd "Quit"
    GFXEngine_Closed
    ContextLevelRemove
End Sub

Public Function Random(Optional ByVal Minimum As Single = 0, Optional ByVal Maximum As Single = 1, Optional ByVal FloatingPoint As Boolean = True)
On Error Resume Next
    If CBool(FloatingPoint) Then
        Random = (Rnd * (Maximum - Minimum)) + Minimum
    Else
        Random = CLng(FRound((Rnd * (FRound(Maximum) - FRound(Minimum))) + FRound(Minimum)))
    End If
End Function

Public Function Range(ByVal StartValue As Long, ByVal EndValue As Long) As Variant
On Error Resume Next
Dim l_varValue As Variant
Dim l_lngIndex As Long
    ReDim l_varValue(0 To (EndValue - StartValue))
    For l_lngIndex = LBound(l_varValue) To UBound(l_varValue)
        l_varValue(l_lngIndex) = l_lngIndex + StartValue
    Next l_lngIndex
    Range = l_varValue
End Function

Public Sub Redraw()
Attribute Redraw.VB_Description = "Forces the engine to redraw."
On Error Resume Next
Dim l_lngObjects As Long, l_booBehind As Boolean
Dim l_lngCount As Long
Dim PObj As Fury2Plugin, Obj As Object
    If Running Then Else Exit Sub
    If Halted Then Exit Sub
    ProfileStart "Redraw"
    ContextLevelAdd "Redraw"
    If ClearEveryFrame Then m_imgBackbuffer.Clear BackgroundColor
    Set m_imgBackbuffer.ClipRectangle = m_imgBackbuffer.Rectangle
    RedrawCameras
    If m_colReversePluginStack.Count > 0 Then
        For Each Obj In m_colReversePluginStack
            Set PObj = Nothing
            Set PObj = Obj
            Err.Clear
            If Not (PObj Is Nothing) Then
                l_booBehind = False
                l_booBehind = PObj.DrawBehind
                If l_booBehind Then PObj.Redraw
            End If
        Next Obj
    End If
    RedrawPictures
    If m_colReversePluginStack.Count > 0 Then
        For Each Obj In m_colReversePluginStack
            If Not (Obj Is Nothing) Then
                Set PObj = Nothing
                Set PObj = Obj
                Err.Clear
                If PObj Is Nothing Then
                    Obj.Redraw
                Else
                    l_booBehind = False
                    l_booBehind = PObj.DrawBehind
                    If Not l_booBehind Then PObj.Redraw
                End If
            End If
        Next Obj
    End If
    If Not (RedrawHook Is Nothing) Then
        RedrawHook.Invoke
    End If
    RedrawConsole
    ProfileStop "Redraw"
    ContextLevelRemove
End Sub

Friend Sub RedrawCameras()
Dim m_camCamera As Fury2Camera
    ContextLevelAdd "Cameras"
    For Each m_camCamera In Cameras
        m_camCamera.FlushBuffer
        m_camCamera.Refresh
    Next m_camCamera
    ContextLevelRemove
End Sub

Public Sub RedrawConsole()
On Error Resume Next
Dim l_lngItems As Long, l_rctSize As Fury2Rect, l_strText As String, l_lngY As Long, l_lngHeight As Long
Dim l_lngTopColor As Long, l_lngBottomColor As Long
Dim l_rctConsole As Fury2Rect, l_rctTextbox As Fury2Rect
    If ConsoleOffset >= ConsoleHeight Then Exit Sub
    ConsoleCaretPos = ClipValue(ConsoleCaretPos, 1, Len(ConsoleText) + 1)
    With Backbuffer
        Set l_rctConsole = F2Rect(0, -ConsoleOffset, ScreenWidth, ConsoleHeight, False)
        Set .ClipRectangle = .Rectangle
        .ClippedSetClipRectangle l_rctConsole
        l_lngHeight = ClipValue(Font.TextHeight(ConsoleText, .Width - 2) + (Font.FullHeight - Font.Height), Font.FullHeight, ConsoleHeight) + 2
        l_lngTopColor = F2RGB(180, 180, 180, 192)
        l_lngBottomColor = F2RGB(110, 110, 110, 192)
        .GradientFill l_rctConsole, Array(l_lngTopColor, l_lngTopColor, l_lngBottomColor, l_lngBottomColor), RenderMode_SourceAlpha
        If ConsoleItems.Count > 0 Then
            l_lngY = l_rctConsole.Bottom - l_lngHeight - 4
            For l_lngItems = ConsoleItems.Count To 1 Step -1
                l_strText = ConsoleItems(l_lngItems)
                Set l_rctSize = Font.GetTextRectangle(l_strText, .Width - 4)
                Font.Draw Backbuffer, l_strText, F2Rect(2, l_lngY - l_rctSize.Height, .Width - 4, l_rctSize.Height + (Font.FullHeight - Font.Height), False), F2White, 1
                l_lngY = l_lngY - l_rctSize.Height
                If l_lngY <= 0 Then Exit For
            Next l_lngItems
        End If
        Set l_rctTextbox = F2Rect(l_rctConsole.Left, l_rctConsole.Bottom - l_lngHeight - 2, l_rctConsole.Right, l_rctConsole.Bottom, True)
        Set .ClipRectangle = .Rectangle
        .ClippedSetClipRectangle l_rctTextbox
        .GradientFill l_rctTextbox, Array(F2RGB(0, 0, 0, 32), F2RGB(0, 0, 0, 32), F2RGB(0, 0, 0, 96), F2RGB(0, 0, 0, 96)), RenderMode_SourceAlpha
        Font.Draw Backbuffer, ConsoleText, F2Rect(2, l_rctConsole.Bottom - l_lngHeight, .Width - 4, l_lngHeight + (Font.FullHeight - Font.Height) - 2, False), , , Font.Options(0, 0, 0, 0, 0, ConsoleCaretPos, F2White)
        Set .ClipRectangle = .Rectangle
    End With
End Sub

Public Sub RedrawPictures()
On Error Resume Next
Dim l_lngPictures As Long, l_objPic As Fury2Image, l_objGraph As Fury2Graphic
    ProfileStart "RedrawPictures"
    If ActivePictureSlots > 0 Then
        For l_lngPictures = 0 To ActivePictureSlots - 1
            With OnscreenPictures(l_lngPictures)
                If .Visible Then
                    If (.Picture Is Nothing) Or (.DestRect Is Nothing) Or (.SourceRect Is Nothing) Then
                    Else
                        If TypeOf .Picture Is Fury2Image Then
                            Set l_objPic = .Picture
                        ElseIf TypeOf .Picture Is Fury2Graphic Then
                            Set l_objGraph = .Picture
                            Set l_objPic = l_objGraph.CurrentImage
                        End If
                        If .Tile Then
                            If .EffectMode = 0 Then
                                m_imgBackbuffer.TileBlit .DestRect, l_objPic
                            ElseIf .Alpha > 0 Then
                                m_imgBackbuffer.TileBlit .DestRect, l_objPic, .Alpha, .EffectMode - 1, .EffectColor
                            End If
                        Else
                            If .EffectMode = 0 Then
                                m_imgBackbuffer.Blit .DestRect, .SourceRect, l_objPic, , , , IIf(SmoothScaling, ResampleMode_Bilinear, ResampleMode_Linear)
                            ElseIf .Alpha > 0 Then
                                m_imgBackbuffer.Blit .DestRect, .SourceRect, l_objPic, .Alpha, .EffectMode - 1, .EffectColor, IIf(SmoothScaling, ResampleMode_Bilinear, ResampleMode_Linear)
                            End If
                        End If
                    End If
                End If
            End With
        Next l_lngPictures
    End If
    ProfileStop "RedrawPictures"
End Sub

Public Function RegisterDLL(ByVal Filename As String) As Boolean
Attribute RegisterDLL.VB_Description = "Installs a DLL."
On Error Resume Next
    RegisterDLL = RegisterServer(Filename, True)
End Function

Sub ReleaseScriptEngine()
Attribute ReleaseScriptEngine.VB_MemberFlags = "40"
On Error Resume Next
    m_ScriptEngine.Obj.Timeout = 250
    Sleep 250
End Sub

Public Function ReloadGame() As Boolean
On Error Resume Next
    Terminating = True
End Function

Public Sub ReLoadPicture(ByVal Filename As String)
Attribute ReLoadPicture.VB_Description = "Unloads a picture from the cache and loads it again."
On Error Resume Next
    Filename = LCase(Trim(Filename))
    m_Images.Remove Filename
    LoadPicture Filename
End Sub

Sub ResetHotspots()
Attribute ResetHotspots.VB_Description = "Resets all hotspots."
On Error Resume Next
Dim l_lngHotspot As Long
    For l_lngHotspot = 0 To c_lngMaxHotspot
        Hotspots(l_lngHotspot).Enabled = False
    Next l_lngHotspot
    Erase Hotspots
    ReDim Hotspots(0 To c_lngMaxHotspot)
End Sub

Public Sub ResetKeyMappings()
On Error Resume Next
Dim l_lngKey As Long
    For l_lngKey = 0 To 255
        m_KeyMap(l_lngKey) = CByte(l_lngKey)
    Next l_lngKey
End Sub

Public Sub ResetKeys()
On Error Resume Next
Dim l_lngKey As Long
    For l_lngKey = 0 To 255
        m_KeyState(l_lngKey) = False
    Next l_lngKey
End Sub

Sub ResetPictures()
Attribute ResetPictures.VB_Description = "Resets all picture slots."
On Error Resume Next
    Erase OnscreenPictures
    ReDim OnscreenPictures(0 To c_lngMaxPictureSlot)
End Sub

Public Sub ResetTiming()
On Error Resume Next
    m_booResetClock = True
End Sub

Public Sub ResizeOutput(ByVal X As Long, ByVal Y As Long)
Attribute ResizeOutput.VB_Description = "Resizes the output window."
On Error Resume Next
    Err.Clear
    m_objOutputPlugin.ScaleRatio = ScreenScaleRatio
    m_objOutputPlugin.ResizeWindow X, Y
    Err.Clear
End Sub

Public Sub ResizePicture(ByVal Slot As Long, Optional ByVal NewWidth As Long = -32767, Optional ByVal NewHeight As Long = -32767)
Attribute ResizePicture.VB_Description = "Resizes a picture slot. (Not the actual  picture being displayed - just the coordinates)"
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With OnscreenPictures(Slot)
        If NewWidth <> -32767 Then
            .DestRect.Width = NewWidth
            .SourceRect.Width = NewWidth
        End If
        If NewHeight <> -32767 Then
            .DestRect.Height = NewHeight
            .SourceRect.Height = NewHeight
        End If
    End With
End Sub

'Function GridSprite(Image As String, Optional Width = 0, Optional Height = 0, Optional MaskColor = -32767, Optional AnimDelay = 6) As Fury2Sprite
'On Error Resume Next
'Dim l_imgImage As Fury2Image
'Dim l_lngPoses As Long, l_lngFrames As Long
'    Set l_imgImage = LoadImage(Image)
'    If l_imgImage Is Nothing Then Exit Function
'    If Height < 1 Or Width < 1 Then Exit Function
'    'If (l_imgImage.Width Mod Width) <> 0 Then Exit Function
'    'If (l_imgImage.Height Mod Height) <> 0 Then Exit Function
'    Set GridSprite = New Fury2Sprite
'    With GridSprite
'        .Name = "Grid Sprite"
'        .WalkSpeed = 1
'        .TrueMovement = True
'        .AnimateWhileMoving = True
'        If l_imgImage.AlphaChannel Then
'            .Effect = 1
'        End If
'        .States(1).DirectionPoses = l_imgImage.Height \ Height
'        With .Poses
'            For l_lngPoses = 1 To l_imgImage.Height \ Height
'                .AddPose 0, 0, F2LM_PingPong, "Pose " & l_lngPoses
'                With .Item(l_lngPoses)
'                    .Blocking.Width = CLng(Width)
'                    .Blocking.Height = CLng(Height)
'                    .Frames.AddFrame Image, F2Rect(0, (l_lngPoses - 1) * Height, Width, Height, False), (Width - 1) / 2, (Height - 1) / 2, AnimDelay, CLng(MaskColor)
'                    For l_lngFrames = 1 To l_imgImage.Width \ Width
'                        .Frames.AddFrame Image, F2Rect((l_lngFrames - 1) * Width, (l_lngPoses - 1) * Height, Width, Height, False), (Width - 1) / 2, (Height - 1) / 2, AnimDelay, CLng(MaskColor)
'                    Next l_lngFrames
'                End With
'            Next l_lngPoses
'        End With
'    End With
'End Function
'
Function RM2kSprite(ByVal Charset As String, Optional ByVal Char As Long = 0, Optional ByVal AnimDelay As Long = 6) As Fury2Sprite
On Error Resume Next
Dim m_lngXOffset As Long, m_lngYOffset As Long
Dim m_lngMaskColor As Long
Dim m_lngPoses As Long
    m_lngXOffset = (Char Mod 4) * 72
    m_lngYOffset = (Char \ 4) * 128
    m_lngMaskColor = LoadImage(Charset).GetPixel(0, 0)
    Set RM2kSprite = New Fury2Sprite
    With RM2kSprite
        .Name = "RM2k Sprite"
        .Solid = True
        .States.Add New Fury2State
        .States(1).DirectionPoses = 4
        .States(1).MaxVelocity = 2
        .States(1).PoseOffset = 0
        .States(1).Name = "Normal"
        .AnimateWhileMoving = True
        With .Poses
            For m_lngPoses = 1 To 4
                .AddPose 0, 0, 1, "Pose"
                With .Item(m_lngPoses)
                    .Blocking.Width = 16
                    .Blocking.Height = 12
                    .Frames.AddFrame Charset, F2Rect(m_lngXOffset + 24, m_lngYOffset, 24, 32, False), 12, 32, AnimDelay, m_lngMaskColor
                    .Frames.AddFrame Charset, F2Rect(m_lngXOffset, m_lngYOffset, 24, 32, False), 12, 32, AnimDelay, m_lngMaskColor
                    .Frames.AddFrame Charset, F2Rect(m_lngXOffset + 24, m_lngYOffset, 24, 32, False), 12, 32, AnimDelay, m_lngMaskColor
                    .Frames.AddFrame Charset, F2Rect(m_lngXOffset + 48, m_lngYOffset, 24, 32, False), 12, 32, AnimDelay, m_lngMaskColor
                    .Frames.AddFrame Charset, F2Rect(m_lngXOffset + 24, m_lngYOffset, 24, 32, False), 12, 32, AnimDelay, m_lngMaskColor
                End With
                m_lngYOffset = m_lngYOffset + 32
            Next m_lngPoses
            With .Item(1)
                .Name = "Up"
                .XVelocity = 0
                .YVelocity = -1
            End With
            With .Item(2)
                .Name = "Right"
                .XVelocity = 1
                .YVelocity = 0
            End With
            With .Item(3)
                .Name = "Down"
                .XVelocity = 0
                .YVelocity = 1
            End With
            With .Item(4)
                .Name = "Left"
                .XVelocity = -1
                .YVelocity = 0
            End With
        End With
    End With
End Function
'
'Function StaticSprite(Image As String, Optional Width = 0, Optional Height = 0, Optional MaskColor = -32767) As Fury2Sprite
'On Error Resume Next
'Dim m_imgImage As Fury2Image
'    Set m_imgImage = LoadImage(Image)
'    Set StaticSprite = New Fury2Sprite
'    With StaticSprite
'        .Name = "Static Sprite"
'        .WalkSpeed = 1
'        .TrueMovement = True
'        .States(1).DirectionPoses = 1
'        .AnimateWhileMoving = False
'        With .Poses
'            .AddPose 0, 0, 0, "Normal"
'            With .Item(1)
'                .Blocking.Width = CLng(Width)
'                .Blocking.Height = CLng(Height)
'                .Frames.AddFrame Image, m_imgImage.Rectangle, (m_imgImage.Width - 1) / 2, (m_imgImage.Height - 1) / 2, 0, CLng(MaskColor)
'            End With
'        End With
'    End With
'End Function
'
'Function StaticRotatingSprite(Path As String, Optional Width = 0, Optional Height = 0, Optional MaskColor = -32767, Optional Frames As Variant) As Fury2Sprite
'On Error Resume Next
'Dim m_imgImage As Fury2Image
'Dim m_lngFrames As Long
'    If IsMissing(Frames) Then Set StaticRotatingSprite = StaticSprite(Path, Width, Height, MaskColor): Exit Function
'    Set StaticRotatingSprite = New Fury2Sprite
'    With StaticRotatingSprite
'        .Name = "Static Rotating Sprite"
'        .WalkSpeed = 1
'        .TrueMovement = True
'        .States(1).DirectionPoses = (UBound(Frames) - LBound(Frames)) + 1
'        .AnimateWhileMoving = False
'        With .Poses
'            For m_lngFrames = LBound(Frames) To UBound(Frames)
'                .AddPose 0, 0, 0, "Angle #" + CStr(m_lngFrames)
'                With .Item(.Count)
'                    .Blocking.Width = CLng(Width)
'                    .Blocking.Height = CLng(Height)
'                    Set m_imgImage = LoadImage(Path + CStr(Frames(m_lngFrames)))
'                    .Frames.AddFrame Path + CStr(Frames(m_lngFrames)), m_imgImage.Rectangle, (m_imgImage.Width - 1) / 2, (m_imgImage.Height - 1) / 2, 0, CLng(MaskColor)
'                End With
'            Next m_lngFrames
'        End With
'    End With
'End Function

Public Function RotatePoint(ByVal X As Single, ByVal Y As Single, ByVal Angle As Single)
On Error Resume Next
Dim l_sngTheta As Single, l_sngR As Single
    l_sngR = Sqr((X * X) + (Y * Y))
    l_sngTheta = Atn(Y / X)
    If X < 0 Then
        l_sngTheta = l_sngTheta + c_dblPi
    End If
    l_sngTheta = l_sngTheta + Angle * c_dblRadian
    RotatePoint = Array(l_sngR * Sin(l_sngTheta), l_sngR * -Cos(l_sngTheta))
End Function

Public Sub RunCode(ByRef Code)
On Error Resume Next
    ContextLevelAdd "RunCode"
    m_ScriptEngine.AddCode ToString(Code)
    If Err <> 0 Then
        Err.Clear
'        m_ScriptEngine.Error.Clear
    End If
    ContextLevelRemove
End Sub

Public Function SaveFlags(ByVal File As VirtualFile)
On Error Resume Next
Dim l_filFile As VirtualFile
Dim l_lngFlags As Long
    Set l_filFile = File
    If l_filFile Is Nothing Then Exit Function
    With l_filFile
        .WriteSegment_Begin
            .Save CLng(m_FlagNames.Count)
            For l_lngFlags = 1 To m_FlagNames.Count
                .Save CStr(m_FlagNames(l_lngFlags))
                .Save CLng(VarType(m_Flags(CStr(m_FlagNames(l_lngFlags)))))
                .Save CStr(m_Flags(CStr(m_FlagNames(l_lngFlags))))
            Next l_lngFlags
        .WriteSegment_End
    End With
End Function

Public Sub SaveKeyMappings(ByVal File As VirtualFile)
On Error Resume Next
Dim m_vfFile As VirtualFile
Dim l_lngKey As Long
    Set m_vfFile = File
    If Err <> 0 Then Exit Sub
    With m_vfFile
        .WriteSegment_Begin
            For l_lngKey = 0 To 255
                .Save m_KeyMap(l_lngKey)
            Next l_lngKey
        .WriteSegment_End
    End With
End Sub

Public Sub SaveMapState(ByVal Index As Long, ByRef Name As String)
Attribute SaveMapState.VB_Description = "Saves the current state of a map."
On Error Resume Next
    SavedMaps.Add Maps(Index), Name
End Sub

Public Function SaveToFile(ByRef Obj As Object, ByRef File As Object) As Boolean
On Error Resume Next
Dim l_glbGlobals As New Fury2Globals
    SaveToFile = l_glbGlobals.SaveToFile(Obj, File)
End Function

Public Property Get ScriptContext() As String
On Error Resume Next
    ScriptContext = Join(m_strScriptContext, ">")
End Property

Public Property Let ScriptContext(ByRef NewContext As String)
On Error Resume Next
    m_strScriptContext = Split(NewContext, ">")
    m_lngContextDepth = UBound(m_strScriptContext)
End Property

Public Property Get ScriptEngine() As ScriptEngine
Attribute ScriptEngine.VB_MemberFlags = "40"
On Error Resume Next
    Set ScriptEngine = m_ScriptEngine
End Property

Public Sub SetHotspot(ByVal Index As Long, Optional ByVal Left As Long = 0, Optional ByVal Top As Long = 0, Optional ByVal Width As Long = 0, Optional ByVal Height As Long = 0, Optional ByRef ClickEvent = Nothing, Optional ByRef EnterEvent = Nothing, Optional ByRef LeaveEvent = Nothing)
Attribute SetHotspot.VB_Description = "Creates a hotspot."
On Error Resume Next
    With Hotspots(CLng(Index))
        .Enabled = True
        .x1 = Left
        .y1 = Top
        .x2 = Left + Width
        .y2 = Top + Height
        Set .ClickEvent = BindEvent(ClickEvent)
        Set .EnterEvent = BindEvent(EnterEvent)
        Set .LeaveEvent = BindEvent(LeaveEvent)
    End With
    Err.Clear
End Sub

Friend Sub SetIObjThis()
On Error Resume Next
    m_ScriptEngine.AddCode m_ScriptEngine.Language.GenerateAssignment("Object", "Engine" & m_ScriptEngine.Language.MethodOperator & "IObjThis", True)
End Sub

Public Sub SetPictureBlitMode(ByVal Slot As Long, ByVal BlitMode As SFXBlitModes)
On Error Resume Next
    OnscreenPictures(Slot).EffectMode = BlitMode + 1
End Sub

Public Sub SetPictureImage(ByVal Slot As Long, ByRef Image As Object)
On Error Resume Next
    Set OnscreenPictures(Slot).Picture = Image
End Sub

Public Sub SetPictureRectangle(ByVal Slot As Long, ByVal Rectangle As Fury2Rect)
On Error Resume Next
    Set OnscreenPictures(Slot).DestRect = Rectangle
End Sub

Public Sub SetPictureSourceRectangle(ByVal Slot As Long, ByVal Rectangle As Fury2Rect)
On Error Resume Next
    Set OnscreenPictures(Slot).SourceRect = Rectangle
End Sub

Public Sub SetPictureTile(ByVal Slot As Long, ByVal Tile As Boolean)
On Error Resume Next
    OnscreenPictures(Slot).Tile = Tile
End Sub

Sub ShowConsole()
    #If DebugFeatures = 1 Then
        ConsoleVisible = True
        Me.ConsoleStartOffset = ConsoleOffset
        Me.ConsoleEndOffset = 0
        Me.ConsoleStartTime = HiTimer
        Me.ConsoleEndTime = HiTimer + 0.33
        Me.ConsoleSliding = True
    #End If
End Sub

Public Sub ShowPicture(ByVal Slot As Long, ByVal Pic As Object, ByVal X As Long, ByVal Y As Long, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal x2 As Long = 0, Optional ByVal y2 As Long = 0, Optional ByVal EffectMode As SFXBlitModes = BlitMode_Default, Optional ByVal Alpha As Single = 1, Optional ByVal EffectColor As Long = 0, Optional ByVal Tile As Boolean = False)
Attribute ShowPicture.VB_Description = "Shows a picture in a specified slot at a specified location, similar to RM2k's Show Picture command."
On Error Resume Next
Dim m_lngSlot As Long
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    If Slot + 1 > ActivePictureSlots Then ActivePictureSlots = Slot + 1
    With OnscreenPictures(Slot)
        Set .DestRect = F2Rect(X, Y, IIf(Width = -1, Pic.Width, Width), IIf(Height = -1, Pic.Height, Height), False)
        Set .SourceRect = F2Rect(x2, y2, IIf(Width = -1, Pic.Width, Width), IIf(Height = -1, Pic.Height, Height), False)
        If EffectMode = BlitMode_Default Then
            If TypeOf Pic Is Fury2Graphic Then
                .EffectMode = Pic.BlitMode
            ElseIf TypeOf Pic Is Fury2Image Then
                If Pic.AlphaChannel Then
                    .EffectMode = CLng(BlitMode_SourceAlpha) + 1
                Else
                    .EffectMode = CLng(BlitMode_Matte) + 1
                End If
            End If
        Else
            .EffectMode = CLng(EffectMode) + 1
        End If
        .EffectColor = EffectColor
        .Alpha = Alpha
        Set .Picture = Pic
        .EndFadeScript = ""
        .Visible = True
        .Tile = Tile
    End With
    For m_lngSlot = 0 To c_lngMaxPictureSlot
        With OnscreenPictures(m_lngSlot)
            If .Visible Then ActivePictureSlots = m_lngSlot + 1
        End With
    Next m_lngSlot
End Sub

Public Property Get SmoothScaling() As Boolean
On Error Resume Next
    SmoothScaling = m_booSmoothScaling
End Property

Public Property Let SmoothScaling(ByVal Value As Boolean)
On Error Resume Next
    m_booSmoothScaling = Value
    If m_booSmoothScaling Then
        SetDefaultSampleFunction GetBilinearScaler()
    Else
        SetDefaultSampleFunction GetLinearScaler()
    End If
End Property

Public Property Get SoundEngine() As SoundEngine
Attribute SoundEngine.VB_Description = "The sound engine."
On Error Resume Next
    Set SoundEngine = m_SoundEngine
End Property

Function FadeBGM(Optional ByVal FromVolume As Single = -1, Optional ByVal ToVolume As Single = -1, Optional ByVal Length As Single = 1) As Fury2SoundFader
On Error Resume Next
Dim l_fadFader As Fury2SoundFader
    Set l_fadFader = New Fury2SoundFader
    Set l_fadFader.Handle = SoundEngine.Handles(CurrentBGMHandle)
    Set l_fadFader.Slider = F2Slider
    l_fadFader.VolumeStart = IIf(FromVolume < 0, l_fadFader.Handle.Volume, FromVolume)
    l_fadFader.VolumeEnd = IIf(ToVolume < 0, l_fadFader.Handle.Volume, ToVolume)
    l_fadFader.Slider.Start Length
    HookEvents l_fadFader
    Set FadeBGM = l_fadFader
End Function

Sub StopBGM(Optional ByVal Fade As Boolean = True)
Attribute StopBGM.VB_Description = "Stops the currently playing BGM."
On Error Resume Next
    If Fade Then
        FadeBGM(, 0).FreeWhenDone = True
    Else
        If CurrentBGM <> "" And CurrentBGMHandle <> 0 Then
            m_SoundEngine.Handles(CurrentBGMHandle).Kill
            m_SoundEngine.Handles(CurrentBGMHandle).Free
        End If
    End If
    CurrentBGM = ""
    CurrentBGMHandle = 0
    Err.Clear
End Sub

Public Sub Subthread(Optional ByVal Name As String = "")
On Error Resume Next
    Game Name
End Sub

Public Sub Swap(ByRef One, ByRef Two)
On Error Resume Next
Dim Temp
    If VarType(Two) = vbObject Then
        Set Temp = Two
    Else
        Temp = Two
    End If
    If VarType(One) = vbObject Then
        Set Two = One
    Else
        Two = One
    End If
    If VarType(Temp) = vbObject Then
        Set One = Temp
    Else
        One = Temp
    End If
End Sub

Sub SwitchBGM(ByRef Filename As String, Optional ByVal Crossfade As Boolean = True)
Attribute SwitchBGM.VB_Description = "Changes the currently playing BGM. If the BGM specified is already playing, nothing happens."
On Error Resume Next
    If Trim(LCase(CurrentBGM)) = Trim(LCase(Filename)) Then Exit Sub
    StopBGM Crossfade
    PlayBGM Filename, Crossfade
    Err.Clear
End Sub

Public Sub SwitchMap(ByRef Filename As String, Optional ByVal LoadX As Single = vbNull, Optional ByVal LoadY As Single = vbNull, Optional ByVal Layer As Long = -1, Optional ByVal Index As Long = 1)
Attribute SwitchMap.VB_Description = "Switches from the current map to a new one."
On Error Resume Next
Dim m_imgOld As Fury2Image
    ContextLevelAdd "SwitchMap(""" + Filename + """, " + CStr(Index) + ")"
    If DefaultTransition <> TR_None Then Set m_imgOld = m_imgBackbuffer.Duplicate
    Transition DefaultTransition, , 0
    LoadMap Filename, LoadX, LoadY, Layer, Index
    Sync
    Redraw
    Transition DefaultTransition, , 1
    Transition DefaultTransition, , m_imgOld
    Set m_imgOld = Nothing
    ContextLevelRemove
    Err.Clear
End Sub

Function LoadOutputPlugin(ByRef NewPlugin As String)
On Error Resume Next
Dim l_objTemp As Object
    Err.Clear
    DebugOut "Loading output plugin """ & NewPlugin & """"
    If Not (Debugger Is Nothing) Then Set l_objTemp = Debugger.DebugCreateOutputPlugin()
    If l_objTemp Is Nothing Then
        Err.Clear
        Set l_objTemp = CreateObject("video_" + CStr(Trim(NewPlugin)) + "." + CStr(Trim(NewPlugin)) + "Engine")
        If Err.Number <> 0 Then
            Err.Clear
            Call RegisterServer(RootPath + "video_" + NewPlugin + ".dll", False)
            Call RegisterServer(RootPath + "video_" + NewPlugin + ".dll", True)
            Set l_objTemp = CreateObject("video_" + CStr(Trim(NewPlugin)) + "." + CStr(Trim(NewPlugin)) + "Engine")
            If Err.Number <> 0 Then CriticalError "LoadOutputPlugin """ + CStr(NewPlugin) + """", "CreateObject()", Err.Description + "(Error #" + CStr(Err.Number) + ")"
        End If
    End If
    If l_objTemp Is Nothing Then Exit Function
    l_objTemp.HookEvents Me
    SetAppIcon l_objTemp.Window
    Set LoadOutputPlugin = l_objTemp
    Set l_objTemp = Nothing
End Function

Sub SwitchOutputPlugin(ByRef NewPlugin As String)
On Error Resume Next
    If m_objOutputPlugin Is Nothing Then
    Else
        Err.Clear
        Transitions.FadeOut 1, , True
        m_objOutputPlugin.HookEvents Nothing
        m_objOutputPlugin.Shutdown
        Set m_objOutputPlugin = Nothing
    End If
    OutputPlugin = NewPlugin
    Set m_objOutputPlugin = LoadOutputPlugin(OutputPlugin)
    If m_objOutputPlugin Is Nothing Then
        CriticalError "SwitchOutputPlugin", "SwitchOutputPlugin """ + CStr(OutputPlugin) + """", "Unable to load output plugin": Exit Sub
    End If
    Err.Clear
    m_objOutputPlugin.HookEvents Me
    m_objOutputPlugin.ScaleRatio = ScreenScaleRatio
    m_objOutputPlugin.Init ScreenWidth, ScreenHeight, 32, IIf(ShowTitlebar, WindowCaption, ""), Not Fullscreen, ScreenWidth, ScreenHeight, 0
    m_objOutputPlugin.VSync = VSync
    If Trim(CStr(Icon)) = "" Then
    Else
        Icon = Icon
    End If
    Err.Clear
    Set m_imgBackbuffer = m_objOutputPlugin.Backbuffer
    m_imgBackbuffer.Name = "Backbuffer"
    Dim l_plgPlugin As Fury2Plugin, l_objPlugin As Object
    For Each l_objPlugin In m_colPluginStack
        Err.Clear
        Set l_plgPlugin = Nothing
        Set l_plgPlugin = l_objPlugin
        l_plgPlugin.OutputPluginChanged
        l_objPlugin.OutputPluginChanged
    Next l_objPlugin
    Err.Clear
    If SubthreadCount >= 0 Then Transitions.FadeIn 1, , True
End Sub

Sub Sync()
On Error Resume Next
Dim m_booPaused As Boolean
Dim m_camCamera As Fury2Camera
    m_booPaused = Paused
    Paused = False
    Cameras.Init
    Cameras.Dirty
    Camera.Refresh
    Update
    Paused = m_booPaused
End Sub

Public Sub DebugOut(ByRef Text)
On Error Resume Next
Dim l_strCBoard As String
Dim l_strText As String
    If Debugger Is Nothing Then Else Debugger.DebugOut ToString(Text)
    If Not LogDebugMessages Then Exit Sub
    l_strText = CStr(Text)
    If LogToClipboard Then
        l_strCBoard = Clipboard.GetText
        Clipboard.Clear
        Clipboard.SetText l_strCBoard + vbCrLf + "> " + l_strText
    End If
    #If DebugFeatures = 1 Then
    If Not (ConsoleItems Is Nothing) Then
        If InStr(l_strText, vbCrLf) Then
            Dim l_varLines As Variant
            Dim l_lngLines As Long
            l_varLines = Split(l_strText, vbCrLf)
            For l_lngLines = LBound(l_varLines) To UBound(l_varLines)
                ConsoleItems.Add CStr(l_varLines(l_lngLines))
            Next l_lngLines
        Else
            ConsoleItems.Add l_strText
        End If
        Do While ConsoleItems.Count > 100
            ConsoleItems.Remove 1
        Loop
    End If
    Debug.Print l_strText
    If (m_objNotify Is Nothing) Or (m_booNotifyNoLogging) Then
    Else
        Err.Clear
        m_objNotify.LogOutput l_strText
        If Err <> 0 Then m_booNotifyNoLogging = True
    End If
    #End If
End Sub

Public Sub TextOut(ByRef Text)
Attribute TextOut.VB_Description = "Outputs text to the engine log."
On Error Resume Next
Dim l_strCBoard As String
Dim l_strText As String
    If Debugger Is Nothing Then Else Debugger.DebugTextOut ToString(Text)
    l_strText = CStr(Text)
    If LogToClipboard Then
        l_strCBoard = Clipboard.GetText
        Clipboard.Clear
        Clipboard.SetText l_strCBoard + vbCrLf + "> " + l_strText
    End If
    #If DebugFeatures = 1 Then
    If Not (ConsoleItems Is Nothing) Then
        If InStr(l_strText, vbCrLf) Then
            Dim l_varLines As Variant
            Dim l_lngLines As Long
            l_varLines = Split(l_strText, vbCrLf)
            For l_lngLines = LBound(l_varLines) To UBound(l_varLines)
                ConsoleItems.Add CStr(l_varLines(l_lngLines))
            Next l_lngLines
        Else
            ConsoleItems.Add l_strText
        End If
        Do While ConsoleItems.Count > 100
            ConsoleItems.Remove 1
        Loop
    End If
    Debug.Print l_strText
    If (m_objNotify Is Nothing) Or (m_booNotifyNoLogging) Then
    Else
        Err.Clear
        m_objNotify.LogOutput l_strText
        If Err <> 0 Then m_booNotifyNoLogging = True
    End If
    #End If
End Sub

Public Property Get TimerEvents() As Fury2Collection
    Set TimerEvents = m_colTimerEvents
End Property

Function ToBoolean(Value) As Boolean
On Error Resume Next
    Err.Clear
    ToBoolean = CBool(Value)
    If VarType(Value) = vbObject Then
        If Err = 0 Then Exit Function
        Err.Clear
        ToBoolean = Value.ToBoolean
        If Err = 0 Then Exit Function
        Err.Clear
        ToBoolean = Value.Class_ToBoolean
        If Err = 0 Then Exit Function
        Err.Clear
        ToBoolean = Value.ToNumber
        If Err = 0 Then Exit Function
        Err.Clear
        ToBoolean = Value.Class_ToNumber
        If Err = 0 Then Exit Function
        Err.Clear
        ToBoolean = Value.ToInteger
        If Err = 0 Then Exit Function
        Err.Clear
        ToBoolean = Value.Class_ToInteger
        If Err = 0 Then Exit Function
        Err.Raise 10241, "ToBoolean()", "Unable to convert object to boolean"
    End If
End Function

Function ToInteger(Value) As Long
On Error Resume Next
    If (VarType(Value) = vbLong) Then
        ToInteger = Value
        Exit Function
    ElseIf (VarType(Value) = vbSingle) Then
        ToInteger = Value
        Exit Function
    ElseIf (VarType(Value) = vbDouble) Then
        ToInteger = Value
        Exit Function
    End If
    Err.Clear
    ToInteger = CLng(Value)
    If VarType(Value) = vbObject Then
        If Err = 0 Then Exit Function
        Err.Clear
        ToInteger = Value.ToInteger
        If Err = 0 Then Exit Function
        Err.Clear
        ToInteger = Value.Class_ToInteger
        If Err = 0 Then Exit Function
        Err.Clear
        ToInteger = Value.ToNumber
        If Err = 0 Then Exit Function
        Err.Clear
        ToInteger = Value.Class_ToNumber
        If Err = 0 Then Exit Function
        Err.Clear
        ToInteger = Value.ToBoolean
        If Err = 0 Then Exit Function
        Err.Clear
        ToInteger = Value.Class_ToBoolean
        If Err = 0 Then Exit Function
        Err.Raise 10241, "ToInteger()", "Unable to convert object to integer"
    End If
End Function

Function ToNumber(Value) As Double
On Error Resume Next
    If (VarType(Value) = vbDouble) Then
        ToNumber = Value
        Exit Function
    ElseIf (VarType(Value) = vbSingle) Then
        ToNumber = Value
        Exit Function
    ElseIf (VarType(Value) = vbLong) Then
        ToNumber = Value
        Exit Function
    End If
    ToNumber = CDbl(Value)
    If VarType(Value) = vbObject Then
        If Err = 0 Then Exit Function
        Err.Clear
        ToNumber = Value.ToNumber
        If Err = 0 Then Exit Function
        Err.Clear
        ToNumber = Value.Class_ToNumber
        If Err = 0 Then Exit Function
        Err.Clear
        ToNumber = Value.ToInteger
        If Err = 0 Then Exit Function
        Err.Clear
        ToNumber = Value.Class_ToInteger
        If Err = 0 Then Exit Function
        Err.Clear
        ToNumber = Value.ToBoolean
        If Err = 0 Then Exit Function
        Err.Clear
        ToNumber = Value.Class_ToBoolean
        If Err = 0 Then Exit Function
        Err.Raise 10241, "ToNumber()", "Unable to convert object to number"
    End If
End Function

Function ToString(Value, Optional ByVal QuoteStrings As Boolean = False) As String
On Error Resume Next
Dim l_vtType As VariantTypeConstants
    l_vtType = VarType(Value)
    If l_vtType = vbEmpty Then
        ToString = "{Empty}"
    ElseIf l_vtType = vbNull Then
        ToString = "{Null}"
    ElseIf (l_vtType And vbArray) = vbArray Then
        If UBound(Value) > 64 Then
            ToString = "{Array}"
        Else
            If QuoteStrings Then
                ToString = "{" + JoinX(Value, ", ", QuoteStrings) + "}"
            Else
                ToString = JoinX(Value, , QuoteStrings)
            End If
        End If
    ElseIf (l_vtType = vbSingle) Or (l_vtType = vbDouble) Then
        ToString = Format(Value, "##############0.0##############")
    ElseIf l_vtType = vbString Then
        If QuoteStrings Then
            ToString = """" + Value + """"
        Else
            ToString = Value
        End If
    ElseIf l_vtType = vbObject Then
        If Value Is Nothing Then
            ToString = "{Nothing}"
        Else
            Err.Clear
            ToString = "{Object}"
            ToString = IIf(QuoteStrings, """", "") & CStr(Value.ToString()) & IIf(QuoteStrings, """", "")
            If Err = 0 Then Exit Function
            Err.Clear
            ToString = IIf(QuoteStrings, """", "") & CStr(Value.Class_ToString()) & IIf(QuoteStrings, """", "")
            If Err = 0 Then Exit Function
            Err.Clear
            ToString = CStr(ToNumber(Value))
            If Err = 0 Then Exit Function
            Err.Clear
            ToString = "{Object: " + TypeName(Value) + "}"
        End If
    ElseIf l_vtType = vbUserDefinedType Then
        ToString = "{UDT}"
    Else
        ToString = "{Unknown}"
        Err.Clear
        ToString = CStr(Value)
        If Err <> 0 Then ToString = "{Unknown}"
    End If
End Function

Sub Trace(ByRef Text)
Attribute Trace.VB_Description = "Outputs text to the debugging window."
On Error Resume Next
    Set frmTrace.Engine = Me
    If frmTrace.Visible Then
    Else
        frmTrace.Show
    End If
    If Len(frmTrace.txtTrace.Text) > 4096 Then
        frmTrace.txtTrace.Text = Left(frmTrace.txtTrace.Text, InStrRev(frmTrace.txtTrace.Text, vbCrLf) - 1)
    End If
    frmTrace.txtTrace.Text = ToString(Text) + vbCrLf + frmTrace.txtTrace.Text
    DoEvents
End Sub

Sub TraceOpen()
Attribute TraceOpen.VB_Description = "Opens the debugging window."
On Error Resume Next
    frmTrace.Show
End Sub

Sub Transition(ByVal TransitionIndex As Fury2TransitionTypes, Optional ByVal Speed As Single = -1, Optional Param)
On Error Resume Next
    Select Case TransitionIndex
    Case Fury2TransitionTypes.TR_None
    Case Fury2TransitionTypes.TR_Fade
        If IsMissing(Param) Then
            Transitions.FadeOut Speed
            Transitions.FadeIn Speed
        ElseIf VarType(Param) = vbObject Then
        Else
            If CLng(Param) = 0 Then
                Transitions.FadeOut Speed
            ElseIf CLng(Param) = 1 Then
                Transitions.FadeIn Speed
            End If
        End If
    #If EnableAllTransitions Then
    Case Fury2TransitionTypes.TR_SlideLeft
        If IsMissing(Param) Then
            Transitions.SlideLeft Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SlideLeft Speed, Param
        End If
    Case Fury2TransitionTypes.TR_SlideRight
        If IsMissing(Param) Then
            Transitions.SlideRight Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SlideRight Speed, Param
        End If
    Case Fury2TransitionTypes.TR_SlideUp
        If IsMissing(Param) Then
            Transitions.SlideUp Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SlideUp Speed, Param
        End If
    Case Fury2TransitionTypes.TR_SlideDown
        If IsMissing(Param) Then
            Transitions.SlideDown Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SlideDown Speed, Param
        End If
    Case Fury2TransitionTypes.TR_SmoothFadeLeft
        If IsMissing(Param) Then
            Transitions.SmoothFadeLeft Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SmoothFadeLeft Speed, Param
        End If
    Case Fury2TransitionTypes.TR_SmoothFadeRight
        If IsMissing(Param) Then
            Transitions.SmoothFadeRight Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SmoothFadeRight Speed, Param
        End If
    Case Fury2TransitionTypes.TR_SmoothFadeUp
        If IsMissing(Param) Then
            Transitions.SmoothFadeUp Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SmoothFadeUp Speed, Param
        End If
    Case Fury2TransitionTypes.TR_SmoothFadeDown
        If IsMissing(Param) Then
            Transitions.SmoothFadeDown Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.SmoothFadeDown Speed, Param
        End If
    Case Fury2TransitionTypes.TR_CircleIn
        If IsMissing(Param) Then
            Transitions.CircleIn Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.CircleIn Speed, Param
        End If
    #End If
'    Case Fury2TransitionTypes.TR_Crossfade
    Case Else
        If IsMissing(Param) Then
            Transitions.Crossfade Speed
        ElseIf VarType(Param) = vbObject Then
            Transitions.Crossfade Speed, Param
        End If
    End Select
End Sub

Public Property Get Transitions() As Fury2Transitions
Attribute Transitions.VB_Description = "The transition library."
    Set Transitions = m_objTransitions
End Property

Public Sub TriggerInputEvent(EventName As String, ParamArray Args() As Variant)
On Error Resume Next
Dim l_booOldState As Boolean
Dim l_sngMultiplier As Single
    l_booOldState = DisableUserInput
    l_sngMultiplier = (m_objOutputPlugin.Window.ScaleWidth / ScreenWidth)
    Select Case LCase(Trim(EventName))
    Case "mousedown"
        GFXEngine_MouseDown CInt(Args(0)), CInt(Args(1)), CSng(Args(2)) * l_sngMultiplier, CSng(Args(3)) * l_sngMultiplier, True
    Case "mousemove"
        GFXEngine_MouseMove CInt(Args(0)), CInt(Args(1)), CSng(Args(2)) * l_sngMultiplier, CSng(Args(3)) * l_sngMultiplier, True
    Case "mouseup"
        GFXEngine_MouseUp CInt(Args(0)), CInt(Args(1)), CSng(Args(2)) * l_sngMultiplier, CSng(Args(3)) * l_sngMultiplier, True
    Case "keydown"
        GFXEngine_KeyDown CInt(Args(0)), CInt(Args(1)), True
    Case "keypress"
        GFXEngine_KeyPress CInt(Args(0)), True
    Case "keyup"
        GFXEngine_KeyUp CInt(Args(0)), CInt(Args(1)), True
    Case Else
    End Select
    UpdateEvents
End Sub

Function TTFont(ByVal Name As String, Optional ByVal Size As Single = 9, Optional ByVal Bold As Boolean = False, Optional ByVal Italic As Boolean = False) As StdFont
On Error Resume Next
    Set TTFont = New StdFont
    With TTFont
        .Name = Name
        .Size = Size
        .Bold = Bold
        .Italic = Italic
    End With
End Function

Public Sub UnHookAll()
Attribute UnHookAll.VB_Description = "Unhooks all plugins from the event system."
On Error Resume Next
Dim CheckHooks As Long
    Do While m_colPluginStack.Count > 0
        UnHookEvents m_colPluginStack(1)
    Loop
    Set m_colPluginStack = New Collection
    Set m_colReversePluginStack = New Collection
    Err.Clear
End Sub

Public Sub UnHookEvents(ByVal Obj As Object)
Attribute UnHookEvents.VB_Description = "Unhooks a plugin from the event system."
On Error Resume Next
Dim l_lngObject As Long
    If m_colReversePluginStack.Count >= 1 Then
        For l_lngObject = 1 To m_colReversePluginStack.Count
            Err.Clear
            If m_colReversePluginStack(l_lngObject) Is Obj Then
                If Err.Number = 0 Then
                    m_colReversePluginStack.Remove l_lngObject
                End If
                Exit For
            End If
        Next l_lngObject
    End If
    If m_colPluginStack.Count >= 1 Then
        For l_lngObject = 1 To m_colPluginStack.Count
            Err.Clear
            If m_colPluginStack(l_lngObject) Is Obj Then
                If Err.Number = 0 Then
                    m_colPluginStack.Remove l_lngObject
                    'Obj.Hide
                    Obj.Unload
                End If
                Exit For
            End If
        Next l_lngObject
    End If
    Err.Clear
End Sub

Sub UnloadAllMaps()
Attribute UnloadAllMaps.VB_Description = "Unloads all loaded maps."
On Error Resume Next
Dim m_lngMaps As Long
Dim m_lngCameras As Long
    DebugOut "Unloading all maps"
    If Maps.Count > 0 Then
        For m_lngMaps = 1 To Maps.Count
            Set Map = Maps(m_lngMaps)
            With Map
                .Unload
                .Free
            End With
            FlushImageCache
            If Cameras.Count > 0 Then
                For m_lngCameras = 1 To Cameras.Count
                    If Cameras(m_lngCameras).ViewingMap Is Maps(m_lngMaps) Then
                        Cameras(m_lngCameras).Map = 0
                    End If
                Next m_lngCameras
            End If
            Maps.Remove m_lngMaps
            m_lngMaps = m_lngMaps - 1
            If m_lngMaps > Maps.Count Then Exit For
            If Maps.Count = 0 Then Exit For
        Next m_lngMaps
    End If
End Sub

Public Sub Unmute()
On Error Resume Next
    SoundEngine.Volume = OldVolume
End Sub

Public Function UnregisterDLL(Filename) As Boolean
Attribute UnregisterDLL.VB_Description = "Uninstalls a DLL."
On Error Resume Next
    UnregisterDLL = RegisterServer(CStr(Filename), False)
End Function

Public Sub Update()
Attribute Update.VB_Description = "Forces an engine update."
On Error Resume Next
Dim m_sngTimer As Single
Dim m_lngTimers As Long
Dim m_lngHooks As Long
Dim l_camCamera As Fury2Camera
Dim PObj As Fury2Plugin, Obj As Object
Dim m_lngMaps As Long, l_mapMap As Fury2Map
Static m_sngLastTimer As Single
    If Running Then Else Exit Sub
    If Halted Then Exit Sub
    If m_objOutputPlugin.Closed Then GFXEngine_Closed
    ContextLevelAdd "Update"
    ProfileStart "Update"
    FrameCount = FrameCount + 1
    UpdateKeyboard
    Mouse.Update
    If Not Paused Then
        m_sngTimer = HiTimer
        If Int(m_sngTimer) <> Int(m_sngLastTimer) Then
            m_sngLastTimer = m_sngTimer
            If GameTimePaused Then
            Else
                ContextLevelAdd "Engine_Tick"
                ProfileStart "Engine_Tick"
                GameTime = GameTime + 1
                ScriptEngine.Exec "Engine_Tick"
                ProfileStop "Engine_Tick"
                ContextLevelRemove
            End If
        End If
        ProfileStart "Timer Events"
        m_colTimerEvents.Update
        If Terminating Then Exit Sub
        For Each l_mapMap In Maps
            l_mapMap.TimerEvents.Update
            If Terminating Then Exit Sub
        Next l_mapMap
        ProfileStop "Timer Events"
        If Maps.Count > 0 Then
            Set Map = Maps(1)
            m_lngMaps = 1
            For Each Map In Maps
                If Map.Layers.Count > 0 Then
                    ContextLevelAdd "Maps(" + CStr(m_lngMaps) + ")"
                    ProfileStart "Map.Update"
                    Map.Update
                    ProfileStop "Map.Update"
                    ContextLevelRemove
                End If
                m_lngMaps = m_lngMaps + 1
                If Terminating Then Exit Sub
            Next Map
            Set Map = Maps(1)
        End If
        If Cameras.Count > 0 Then
            For Each l_camCamera In Cameras
                l_camCamera.Update
            Next l_camCamera
        End If
        If m_colPluginStack.Count > 0 Then
            m_lngHooks = 1
            ProfileStart "EventHooks.Update"
            For Each Obj In m_colPluginStack
                If Not (Obj Is Nothing) Then
                    Set PObj = Nothing
                    Set PObj = Obj
                    If PObj Is Nothing Then
                        Obj.Update
                    Else
                        PObj.Update
                    End If
                    Err.Clear
                    m_lngHooks = m_lngHooks + 1
                    If Terminating Then Exit Sub
                End If
            Next Obj
            ProfileStop "EventHooks.Update"
        End If
        If Not (UpdateHook Is Nothing) Then
            UpdateHook.Invoke
        End If
    End If
    If ConsoleSliding Then
        ConsoleOffset = (ConsoleStartOffset) + ((ConsoleEndOffset - ConsoleStartOffset) * ((HiTimer - Me.ConsoleStartTime) / (Me.ConsoleEndTime - Me.ConsoleStartTime)))
        If ConsoleOffset < 0 Then ConsoleOffset = 0
        If ConsoleOffset > ConsoleHeight Then ConsoleOffset = ConsoleHeight
    End If
    ProfileStop "Update"
    ContextLevelRemove
End Sub

Public Sub UpdateBackbuffer()
Attribute UpdateBackbuffer.VB_Description = "Copies all cameras to the backbuffer and renders all picture slots."
On Error Resume Next
    Redraw
End Sub

Friend Sub UpdateEvents()
On Error Resume Next
Dim m_lngQueue As Long
Dim m_lngCount As Long
Dim l_lngBindings As Long, l_lngType As Long
Dim l_lngProcessedCount As Long
Dim m_ctlControl As Fury2InputControl
    ProfileStart "Process Input"
    ContextLevelAdd "Controls"
        For Each m_ctlControl In Controls
            m_ctlControl.TriggerRepeat
        Next m_ctlControl
    ContextLevelRemove
    If EventQueueLength < 1 Then
        ProfileStop "Process Input"
        Exit Sub
    End If
    ContextLevelAdd "Events"
    m_lngCount = EventQueueLength
    Do Until EventQueueLength <= 0 Or m_lngQueue > EventQueueLength
        With EventQueue(m_lngQueue)
            l_lngType = .Type
            .Type = -1
            Select Case l_lngType
            Case 0 ' KeyDown
                Event_KeyDown EventQueue(m_lngQueue)
                l_lngProcessedCount = l_lngProcessedCount + 1
            Case 1 ' KeyUp
                Event_KeyUp EventQueue(m_lngQueue)
                l_lngProcessedCount = l_lngProcessedCount + 1
            Case 2 ' MouseDown
                Event_MouseDown EventQueue(m_lngQueue)
                l_lngProcessedCount = l_lngProcessedCount + 1
            Case 3 ' MouseMove
                Event_MouseMove EventQueue(m_lngQueue)
                l_lngProcessedCount = l_lngProcessedCount + 1
            Case 4 ' MouseUp
                Event_MouseUp EventQueue(m_lngQueue)
                l_lngProcessedCount = l_lngProcessedCount + 1
            Case -1 ' None
            Case Else ' ?
            End Select
        End With
        m_lngQueue = m_lngQueue + 1
        If Terminating Then Exit Do
    Loop
    EventQueueLength = EventQueueLength - l_lngProcessedCount
    ContextLevelRemove
    Err.Clear
    ProfileStop "Process Input"
End Sub

Public Sub UpdateKeyboard()
On Error Resume Next
Dim l_lngKeys As Long
Dim l_booState As Boolean
    If InIDE Then Exit Sub
    ContextLevelAdd "UpdateKeyboard"
    For l_lngKeys = 1 To 254
        l_booState = CBool(Abs(GetAsyncKeyState(l_lngKeys)))
        If l_booState <> m_KeyState(m_KeyMap(l_lngKeys)) Then
            If l_booState = False Then
                m_KeyState(m_KeyMap(l_lngKeys)) = False
                If Not DisableUserInput Then QueueEvent 1, , , CLng(m_KeyMap(l_lngKeys)), 0, 0
            End If
        End If
    Next l_lngKeys
    ContextLevelRemove
End Sub

Public Sub UpdatePictures()
Attribute UpdatePictures.VB_MemberFlags = "40"
On Error Resume Next
Dim m_sngX As Single, m_sngY As Single, m_sngW As Single, m_sngH As Single
Dim m_sngProgress As Single, m_sngProgress2 As Single
Dim m_lngPictures As Long, l_objGraph As Fury2Graphic
    ProfileStart "UpdatePictures"
    For m_lngPictures = 0 To c_lngMaxPictureSlot
        With OnscreenPictures(m_lngPictures)
            If .Picture Is Nothing Then
            Else
                If TypeOf .Picture Is Fury2Graphic Then
                    Set l_objGraph = .Picture
                    l_objGraph.Update
                End If
            End If
            If .Fading Then
                .Alpha = .Alpha + .FadeSpeed
                If .FadeSpeed > 0 Then
                    If .Alpha >= .EndFadeAt Then
                        .Alpha = .EndFadeAt
                        .Fading = False
                        .FadeSpeed = 0
                        If (.EndFadeScript = "%Hide%") Then
                            HidePicture m_lngPictures
                        Else
                            If Trim(.EndFadeScript) <> "" Then
                                ' Debug.Print "Called " & .EndFadeScript & "(" & m_lngPictures & ")"
                                m_ScriptEngine.AddCode .EndFadeScript
                            End If
                        End If
                    End If
                ElseIf .FadeSpeed < 0 Then
                    If .Alpha <= .EndFadeAt Then
                        .Alpha = .EndFadeAt
                        If Alpha <= 0 Then .Visible = False
                        .Fading = False
                        .FadeSpeed = 0
                        If (.EndFadeScript = "%Hide%") Then
                            HidePicture m_lngPictures
                        Else
                            If Trim(.EndFadeScript) <> "" Then
                                ' Debug.Print "Called " & .EndFadeScript & "(" & m_lngPictures & ")"
                                m_ScriptEngine.AddCode .EndFadeScript
                            End If
                        End If
                    End If
                End If
            End If
            If .AnimationStart <> 0 Then
                m_sngProgress = (HiTimer - .AnimationStart) / (.AnimationEnd - .AnimationStart)
                If m_sngProgress > 1 Then m_sngProgress = 1
                If m_sngProgress < 0 Then m_sngProgress = 0
                m_sngProgress2 = 1 - m_sngProgress
                If m_sngProgress >= 1 Then
                    Set .DestRect = .EndRect
                    Set .EndRect = Nothing
                    Set .StartRect = Nothing
                    .AnimationStart = 0
                    .AnimationEnd = 0
                    If .EndAnimationScript <> "" Then
                        m_ScriptEngine.AddCode .EndAnimationScript
                    End If
                Else
                    m_sngX = (.StartRect.Left * (m_sngProgress2)) + (.EndRect.Left * m_sngProgress)
                    m_sngY = (.StartRect.Top * (m_sngProgress2)) + (.EndRect.Top * m_sngProgress)
                    m_sngW = (.StartRect.Right * (m_sngProgress2)) + (.EndRect.Right * m_sngProgress)
                    m_sngH = (.StartRect.Bottom * (m_sngProgress2)) + (.EndRect.Bottom * m_sngProgress)
                    .DestRect.SetValues CLng(m_sngX), CLng(m_sngY), CLng(m_sngW), CLng(m_sngH)
                End If
            End If
        End With
    Next m_lngPictures
    ProfileStop "UpdatePictures"
End Sub

Public Sub UpdateTimers()
Attribute UpdateTimers.VB_MemberFlags = "40"
On Error Resume Next
Dim m_tmrTimer As Fury2Timer
Dim m_lngTimers As Long
Dim m_lngHooks As Long
Dim Obj As Object, PObj As Fury2Plugin
    If EnableTimers Then
        If Timers.Count > 0 Then
            ContextLevelAdd "Timers"
            ProfileStart "Timers"
            m_lngTimers = 1
            For Each m_tmrTimer In Timers
                With m_tmrTimer
                    .Update
                    If .OneShot = True And .Enabled = False Then
                        Timers.Remove m_lngTimers
                        If m_lngTimers = Timers.Count + 1 Then Exit For
                        m_lngTimers = m_lngTimers - 1
                    End If
                End With
                m_lngTimers = m_lngTimers + 1
            Next m_tmrTimer
            ProfileStop "Timers"
            ContextLevelRemove
        End If
    End If
    If m_colPluginStack.Count > 0 Then
        ContextLevelAdd "SyncUpdate"
        ProfileStart "EventHooks.SyncUpdate"
        For Each Obj In m_colPluginStack
            Err.Clear
            Set PObj = Nothing
            Set PObj = Obj
            If PObj Is Nothing Then
                Obj.SyncUpdate
            Else
                PObj.SyncUpdate
            End If
        Next Obj
        ProfileStop "EventHooks.SyncUpdate"
        ContextLevelRemove
    End If
    Err.Clear
End Sub

Function VariableName(Name) As String
On Error Resume Next
Dim m_lngChars As Long
Dim m_strOut As String
Dim m_bytIn() As Byte
    m_bytIn = CStr(Name)
    For m_lngChars = LBound(m_bytIn) To UBound(m_bytIn)
        Select Case m_bytIn(m_lngChars)
        Case 32, 58, 46, 39
            m_strOut = m_strOut + "_"
        Case 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 47
        Case 59, 60, 61, 62, 63, 64
        Case 91, 92, 93, 94, 96
        Case 48, 49, 50, 51, 52, 53, 54, 55, 56, 57 ' Numbers not allowed as first character
            If Len(m_strOut) >= 1 Then
                m_strOut = m_strOut + Chr(m_bytIn(m_lngChars))
            End If
        Case Is < 32 ' Below space not allowed
        Case Is > 122 ' Extended characters not allowed
        Case Else
            m_strOut = m_strOut + Chr(m_bytIn(m_lngChars))
        End Select
    Next m_lngChars
    If Len(m_strOut) > c_lngMaxNameLength Then
        m_strOut = Right(m_strOut, c_lngMaxNameLength)
    End If
    VariableName = m_strOut
End Function

Public Property Get Version() As String
Attribute Version.VB_Description = "Returns the version of the engine."
    Version = "Fury² " + Choose(App.Major + 1, "Alpha", "Beta", "Release") + " " + CStr(App.Minor) + " Build " + CStr(App.Revision)
End Property

Public Sub Wait(Time)
Attribute Wait.VB_Description = "Waits a certain number of seconds."
Dim StartTime As Double, EndTime As Double
Dim i As Long
    ContextLevelAdd "Engine.Wait(" + Format(Time, "###.####") + "s)"
    StartTime = HiTimer
    EndTime = HiTimer + CDbl(Time)
    Do While (HiTimer < EndTime) And (Running = True) And (m_booBreak <> True)
        i = i + 1: If i >= 50 Then ContextLevelChange "Engine.Wait(" + Format(EndTime - HiTimer, "#.###") + "s)": i = 0
        If HarassCPU Then Else Call SleepEx(1, True)
        If Halted Then
            If Not (Debugger Is Nothing) Then Debugger.DebugHalted
            Do While Halted
                DoEvents
            Loop
            m_booResetClock = True
            If Not (Debugger Is Nothing) Then Debugger.DebugUnhalted
        Else
            DoEvents
        End If
    Loop
    m_booResetClock = True
    ContextLevelRemove
End Sub

Public Function Which(Value As Variant, ParamArray Options() As Variant) As Long
On Error Resume Next
Dim l_lngValues As Long
    Which = 0
    For l_lngValues = LBound(Options) To UBound(Options)
        If Options(l_lngValues) Is Value Then
            Which = (l_lngValues - LBound(Options)) + 1
            Exit For
        ElseIf Options(l_lngValues) = Value Then
            Which = (l_lngValues - LBound(Options)) + 1
            Exit For
        End If
    Next l_lngValues
End Function

Public Function WrapInteger(Number, Min, Max) As Long
On Error Resume Next
    WrapInteger = WrapValue(CLng(Number), CLng(Min), CLng(Max))
End Function

' Hack
Public Function WrapNumber(Number, Min, Max) As Long
On Error Resume Next
    WrapNumber = WrapValue(CLng(Number), CLng(Min), CLng(Max))
End Function

'Public Function LoadDictionary(ByVal Filename As String) As Fury2Dictionary
'On Error Resume Next
'Dim l_strFile As String
'Dim l_lngPosition As Long
'    If Filesystem.FileExists(Filename) Then
'        l_strFile = Filesystem.File(Filename).Text
'    End If
'End Function

Public Function YesNo(BVal) As String
Attribute YesNo.VB_Description = "Returns 'Yes' or 'No' based on the value of a boolean."
On Error Resume Next
    If CBool(BVal) Then
        YesNo = "Yes"
    Else
        YesNo = "No"
    End If
End Function

Public Sub Yield(Optional ByVal Sync As Boolean = True)
Attribute Yield.VB_Description = "Pauses the engine and allows other applications and system processes to execute. Useful in long processing loops."
On Error Resume Next
    If Running Then Else Exit Sub
    If Running = True Then DoEvents
    If HarassCPU Then Else Call SleepEx(1, True)
    If Sync Then UpdateTimers
End Sub

Friend Sub Init(Parameters As String)
On Error Resume Next
    Randomize Timer
    m_booMouseVisible = True
    
    If LoadGame(Parameters) Then
        EngineLoop
    Else
        If EditMode Then
        Else
            MsgBox "Unable to load game!"
            Shutdown
        End If
    End If
End Sub

Friend Sub EngineLoop()
On Error Resume Next
    m_objOutputPlugin.Window.SetFocus
    If (ErrorOccurred) And (Debugger Is Nothing) Then ShowConsole
    Cameras.Dirty
    m_objNotify.Begin
    Do
        Game
        If Not (Debugger Is Nothing) Then Debugger.DebugGameEnd
        If TerminateEngine Then
            Exit Do
        ElseIf Terminating Then
            Terminating = False
            DebugOut "Reloading game"
            LoadGame CurrentGame
            Game
        Else
            Exit Do
        End If
    Loop
    Shutdown
End Sub

Public Sub Shutdown()
On Error Resume Next
Dim m_frmForm As Form
Dim m_lngForms As Long
Dim m_lngWait As Long
Dim Obj As Object
    Halted = False
    ScriptContext = "Shutdown"
    StopBGM
    SoundEngine.FreeAll
    If m_booIDE Then DebugOut "Ending game"
    UnloadAllMaps
    ScriptEngine.Exec "Engine_Shutdown"
    For m_lngWait = 1 To 50
        SleepEx 1, True
        DoEvents
    Next m_lngWait
    ReleaseScriptEngine
    m_objOutputPlugin.HookEvents Nothing
    m_objOutputPlugin.Shutdown
    SoundEngine.Shutdown
    Set m_objOutputPlugin = Nothing
    Set m_imgBackbuffer = Nothing
    For Each Obj In EventHooks
        Obj.Shutdown
        Set Obj = Nothing
    Next Obj
    ShowCursor True
    EmptyImageCache
    For m_lngForms = Forms.Count To 0 Step -1
        Unload Forms(m_lngForms)
    Next m_lngForms
    m_objNotify.Quit
End Sub

Public Sub CriticalError(Source As String, Location As String, Optional Description As String = "Unknown Error")
On Error Resume Next
Dim m_lngHandle As Long
Dim m_strError As String
Dim l_frmError As frmCriticalError
    Halted = True
    If EditMode Then
        If Err.Number <> 0 Then
            m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
            Err.Description + "(" + CStr(Err.Number) + ")" + vbCrLf + _
            "From: """ + Err.Source + """" + vbCrLf + _
            "In english: """ + Description + """" + vbCrLf + _
            "Context: " + ScriptContext + vbCrLf + _
            "Frames rendered: " + CStr(FrameCount)
        Else
            m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
            "In english: """ + Description + """" + vbCrLf + _
            "Context: " + ScriptContext + vbCrLf + _
            "Frames rendered: " + CStr(FrameCount)
        End If
'        MsgBox m_strError, vbInformation, "Error"
        Exit Sub
    End If
    If m_booIDE Then Stop
    If Debugger Is Nothing Then
    Else
        Debugger.CriticalError Source, Location, Description
        Exit Sub
    End If
    If m_objOutputPlugin.Fullscreen Then m_objOutputPlugin.GoWindowed
    If Err.Number <> 0 Then
        m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
        Err.Description + "(" + CStr(Err.Number) + ")" + vbCrLf + _
        "From: """ + Err.Source + """" + vbCrLf + _
        "In english: """ + Description + """" + vbCrLf + _
        "Context: " + ScriptContext + vbCrLf + _
        "Frames rendered: " + CStr(FrameCount)
    Else
        m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
        "In english: """ + Description + """" + vbCrLf + _
        "Context: " + ScriptContext + vbCrLf + _
        "Frames rendered: " + CStr(FrameCount)
    End If
    m_lngHandle = FreeFile
    Open App.Path + IIf(Right(App.Path, 1) = "\", "", "\") + "error.wtf" For Append As #m_lngHandle
    Print #m_lngHandle, m_strError
    Close #m_lngHandle
    Mouse.Visible = True
    Set l_frmError = New frmCriticalError
    Load l_frmError
    Set l_frmError.Engine = Me
    l_frmError.Show
    SetTopmost l_frmError, True
    l_frmError.lblInfo.Caption = m_strError
    l_frmError.SetFocus
    DoEvents
    Do While Halted
        DoEvents
        Sleep (1)
    Loop
End Sub

Private Sub m_ScriptEngine_InterruptPoll(Interrupt As Boolean)
On Error Resume Next
    Debugger.InterruptPoll Interrupt
    DoEvents
End Sub
