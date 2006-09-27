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

Option Compare Text
Option Explicit

Implements Fury2Object
Implements Fury2VideoEventSink
Implements IVirtualFileSaveable

Private Declare Function GetForegroundWindow Lib "user32" () As Long

' Local types
Public Type Fury2Picture
    Visible As Boolean
    DestRect As Fury2Rect
    SourceRect As Fury2Rect
    Picture As Object
    EffectMode As Long
    Alpha As Single
    Rotation As Single
    FadeSpeed As Double
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
    pObj As Long
End Type

Public Type Fury2Hotspot
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
    TimeStamp As Double
End Type

Private Type Fury2SubthreadState
    Name As String
    Running As Boolean
End Type

' Public objects/singletons
Public Arrays As Fury2Arrays
Attribute Arrays.VB_VarDescription = "Contains utility functions for working with arrays."
Public Math As Fury2Math
Attribute Math.VB_VarDescription = "Contains utility functions for working with numbers and performing arithmetic."
Public Strings As Fury2Strings
Attribute Strings.VB_VarDescription = "The default string table."
Public Mouse As Fury2Mouse
Attribute Mouse.VB_VarDescription = "The mouse."
Public Keyboard As Fury2Keyboard
Attribute Keyboard.VB_VarDescription = "The keyboard."
Public Joysticks As Fury2Collection
Attribute Joysticks.VB_VarDescription = "All of the joysticks currently connected to the computer."
Public Party As Fury2Party
Attribute Party.VB_VarDescription = "The current party object."
Public Configuration As Fury2ConfigurationFile
Attribute Configuration.VB_VarDescription = "The current configuration."
Public WindowSkin As Fury2WindowSkin
Attribute WindowSkin.VB_VarDescription = "The default window skin used for menus and message boxes."
Public MenuCursor As Fury2Graphic
Attribute MenuCursor.VB_VarDescription = "The default cursor used for menus."
Public MouseCursor As Fury2Image
Attribute MouseCursor.VB_VarDescription = "The image drawn to represent the mouse cursor."
Public Font As Fury2Font
Attribute Font.VB_VarDescription = "The default font used for message boxes, menus, textelements, the console, and other objects."
Public Controls As Fury2Collection
Attribute Controls.VB_VarDescription = "All of the input controls currently defined."
Public Timers As Fury2Collection
Attribute Timers.VB_VarDescription = "All of the timers currently defined."
Public Map As Fury2Map
Attribute Map.VB_VarDescription = "The map currently being updated."
Public SavedMaps As Fury2Collection
Attribute SavedMaps.VB_VarDescription = "All of the maps currently saved in memory."
Public Filesystem As Fury2Filesystem
Attribute Filesystem.VB_VarDescription = "The engine's filesystem."
Public ConsoleItems As Fury2Collection
Attribute ConsoleItems.VB_VarDescription = "All of the text currently in the console."
Public Debugger As Fury2Debugger
Attribute Debugger.VB_VarDescription = "The debugger currently attached to the engine, if any."
Public SoundListener As Fury2Sprite
Attribute SoundListener.VB_VarDescription = "The sprite that the sound engine's listener is currently attached to, if any."
Public IObjThis As Object
Public MessageWindows As Fury2Collection
Attribute MessageWindows.VB_VarDescription = "All of the message windows that are currently open."
Public MessageQueue As Fury2Collection
Attribute MessageQueue.VB_VarDescription = "All of the message windows that are currently waiting to open."
Public ActionQueue As Fury2Collection
Attribute ActionQueue.VB_VarDescription = "A queue of Fury2Event objects that will be executed at the end of the current frame."
Public BGMStack As Fury2Collection
Attribute BGMStack.VB_VarDescription = "A list of all the BGMs that have been pushed onto the stack by PushBGM."
Public CurrentBGM As Sound
Attribute CurrentBGM.VB_VarDescription = "The sound currently playing as a BGM, if any."
Public CurrentBGMChannel As Channel
Attribute CurrentBGMChannel.VB_VarDescription = "The channel that the current BGM is playing on, if any."
Public CurrentBGMFader As Fury2SoundFader
Attribute CurrentBGMFader.VB_VarDescription = "The fader that is currently controlling the BGM volume, if any."
Public ScreenshotOverlay As Fury2Image
Attribute ScreenshotOverlay.VB_VarDescription = "The image that should be overlaid on screenshots when saved."

' Event hooks
Public RedrawHook As Fury2Event, UpdateHook As Fury2Event, LostFocusHook As Fury2Event, GotFocusHook As Fury2Event
Public MouseDownHook As Fury2Event, MouseMoveHook As Fury2Event, MouseUpHook As Fury2Event, MouseDblClickHook As Fury2Event

' Private/protected game objects/engine objects
Private m_objVideoDriver As Object
Private m_objOutputPlugin As Fury2VideoDriver
Private m_imgBackbuffer As Fury2Image
Private WithEvents m_ScriptEngine As ScriptEngine
Attribute m_ScriptEngine.VB_VarHelpID = -1
Private m_SoundEngine As SoundEngine
Private m_Images As Collection
Private m_Tilesets As Collection
Private mvarMaps As Fury2Maps
Private mvarCameras As Fury2Cameras
Private m_Graphics As Fury2GEGlobal
Private m_Globals As Fury2Globals
Private m_colPluginStack As Collection
Private m_colReversePluginStack As Collection
Private m_Flags As Collection
Private m_FlagNames As Collection
Private Hosts As Fury2Collection
Private Videos As Fury2Collection
Private m_objConstants As Fury2ScriptConstants
Private m_objTransitions As Fury2Transitions
Private m_colTimerEvents As Fury2TimerEvents
Private EventHookStack As Collection
Private m_dxDirectX As DirectX8
Private m_diDirectInput As DirectInput8
Private ScriptCache As Collection
Private m_colContextStack As New Collection
Private m_imgMouseBuffer As Fury2Image
Private WithEvents m_imeIMEEngine As IMEEngine
Attribute m_imeIMEEngine.VB_VarHelpID = -1

' Non-game configuration variables
Public ScreenshotPattern As String
Attribute ScreenshotPattern.VB_VarDescription = "The filename pattern to use when saving screenshots."
Public LogDebugMessages As Boolean
Attribute LogDebugMessages.VB_VarDescription = "Logs debug messages to the Console if True."
Public LogToClipboard As Boolean
Attribute LogToClipboard.VB_VarDescription = "Logs console text to the clipboard if True."
Public LogToFile As Boolean
Attribute LogToFile.VB_VarDescription = "Logs console text to a logfile if True."
Public EditMode As Boolean
Attribute EditMode.VB_VarDescription = "If True, the engine is in Editor mode."
Public HarassCPU As Boolean
Attribute HarassCPU.VB_VarDescription = "If False, the engine will attempt to use as little CPU power as necessary. This may reduce framerates and impact the effectiveness of framerate balancing."
Public HideLog As Boolean

' Game configuration variables
Public Strict As Boolean
Public DisableUserInput As Boolean
Attribute DisableUserInput.VB_VarDescription = "Disables keyboard and mouse input from the user."
Public ScriptLanguage As String
Attribute ScriptLanguage.VB_VarDescription = "The name of the script language currently being used."
Public DisableCameraBuffers As Boolean
Public DefaultMessageTop As Single
Attribute DefaultMessageTop.VB_VarDescription = "The default Y position for new message boxes (0.0 - 1.0)."
Public SmartMessageBoxPositioning As Boolean
Attribute SmartMessageBoxPositioning.VB_VarDescription = "If True, new message boxes will attempt to position themselves in an intelligent place based on the player's location."
Public EnableObjectEvents As Boolean
Public DefaultFont As String
Attribute DefaultFont.VB_VarDescription = "Specifies a default font to load at startup."
Public DefaultWindowSkin As String
Attribute DefaultWindowSkin.VB_VarDescription = "Specifies a default window skin to load at startup."
Public DefaultCursor As String
Attribute DefaultCursor.VB_VarDescription = "Specifies a default menu cursor to load at startup."
Public DisableMovement As Boolean
Attribute DisableMovement.VB_VarDescription = "Disables player movement."
Public EnableTimers As Boolean
Attribute EnableTimers.VB_VarDescription = "Enables timers."
Public ClearMapBG As Boolean
Attribute ClearMapBG.VB_VarDescription = "Causes cameras to clear themselves before rendering maps."
Public DisableMoveQueue As Boolean
Public DefaultBGMVolume As Single
Attribute DefaultBGMVolume.VB_VarDescription = "The default volume to use when playing BGMs (0.0 - 1.0)"
Public DefaultSampleVolume As Single
Attribute DefaultSampleVolume.VB_VarDescription = "The default volume to use when playing samples (0.0 - 1.0)"
Public GameTimePaused As Boolean
Attribute GameTimePaused.VB_VarDescription = "Pauses the GameTime so that it will not continue increasing."
Public ActivationDistance As Single
Attribute ActivationDistance.VB_VarDescription = "Specifies the default distance for an object to search when looking for another object to activate."
Public ScaleBlocking As Boolean
Attribute ScaleBlocking.VB_VarDescription = "Specifies that a sprite's blocking should scale with its ScaleLevel, instead of just its displayed image."
Public VelocityDecay As Single
Attribute VelocityDecay.VB_VarDescription = "Controls the rate at which objects accelerate and decelerate when under the control of a path or controller object."
Public OutputPlugin As String
Attribute OutputPlugin.VB_VarDescription = "The name of the current output plugin."
Public VSync As Boolean
Attribute VSync.VB_VarDescription = "If True, the current output plugin will attempt to perform Vertical Sync if possible. Will lower framerates but prevent tearing."
Public DisableSound As Boolean
Attribute DisableSound.VB_VarDescription = "Disables the sound engine."
Public StartMap As String
Attribute StartMap.VB_VarDescription = "The default map to load at startup."
Public StartX As Single
Attribute StartX.VB_VarDescription = "The default X location to start the player at."
Public StartY As Single
Attribute StartY.VB_VarDescription = "The default Y location to start the player at."
Public StartLayer As Long
Attribute StartLayer.VB_VarDescription = "The default layer to start the player on."
Public HideConfiguration As Boolean
Attribute HideConfiguration.VB_VarDescription = "If True, the configuration dialog will not appear at startup."
Public ClearEveryFrame As Boolean
Attribute ClearEveryFrame.VB_VarDescription = "If True, before Redrawing the engine will Clear the backbuffer."
Public ClearBackgroundColor As Long
Attribute ClearBackgroundColor.VB_VarDescription = "Specifies what color the Backbuffer should clear to every frame."
Public RepeatDelay As Single
Public GameName As String
Attribute GameName.VB_VarDescription = "Specifies the name of the current game."
Public DesiredFramerate As Long
Attribute DesiredFramerate.VB_VarDescription = "Specifies the target framerate for the framerate balancer."
Public WindowCaption As String
Attribute WindowCaption.VB_VarDescription = "Specifies the caption for the game window."
Public BalanceFramerate As Boolean
Attribute BalanceFramerate.VB_VarDescription = "If True, the game engine will attempt to process updates at DesiredFramerate frames per second, and will skip redrawing when necessary to do so."
Public ShowFPS As Boolean
Attribute ShowFPS.VB_VarDescription = "If True, the current framerate will be displayed in the game window."
Public ShowTitlebar As Boolean
Attribute ShowTitlebar.VB_VarDescription = "If True, the game window will show a titlebar."
Public ScreenWidth As Long
Attribute ScreenWidth.VB_VarDescription = "Specifies the width of the Backbuffer."
Public ScreenHeight As Long
Attribute ScreenHeight.VB_VarDescription = "Specifies the height of the Backbuffer."
Public Fullscreen As Boolean
Attribute Fullscreen.VB_VarDescription = "If True, the game will attempt to run fullscreen."
Public ScreenScaleRatio As Single
Attribute ScreenScaleRatio.VB_VarDescription = "Specifies the level of output scaling to apply to the backbuffer. Values below 1.0 are not guaranteed to work."
Public ScreenScaleMode As Long
Attribute ScreenScaleMode.VB_VarDescription = "Specifies what type of scaling to use when drawing the backbuffer, if the ScreenScaleRatio is not 1.0. Valid values are: 0=Linear, 1=Bilinear, 2=Scale2x"
Public MaxFrameskip As Long
Attribute MaxFrameskip.VB_VarDescription = "The maximum number of updates that can be performed for every Redraw."
Public TrapKeys As Boolean
Attribute TrapKeys.VB_VarDescription = "If True, keyboard input will be buffered into KeysEntered instead of being processed normally."
Public DefaultTransition As Fury2Event
Attribute DefaultTransition.VB_VarDescription = "Specifies a default transition handler to use when switching maps. This handler is called before and after a map change."
Public DefaultTransitionLength As Single
Attribute DefaultTransitionLength.VB_VarDescription = "Specifies the default length (in seconds) for transitions."
Public DefaultControllerAngles As Variant
Attribute DefaultControllerAngles.VB_VarDescription = "Specifies the movement angles to use by default when creating a Fury2Controller."
Public DefaultControllerStyle As Fury2ControllerStyles
Attribute DefaultControllerStyle.VB_VarDescription = "Specifies the default style to use when creating a Fury2Controller."
Public ConsoleEnabled As Boolean
Attribute ConsoleEnabled.VB_VarDescription = "If True, the player can open a debugging console by pressing the ` key."
Public PictureDrawPosition As Long

' Private state/configuration variables
Private m_dblLastFlushTime As Double
Private m_sngConsoleOffset As Single
Private m_booConsoleSliding As Boolean
Private m_sngConsoleEndOffset As Single
Private m_sngConsoleStartOffset As Single
Private m_sngConsoleEndTime As Single
Private m_sngConsoleStartTime As Single
Private m_lngConsoleCaretPosition As Long
Private m_lngInterruptTimer As Long
Private m_booErrorsOff As Boolean, m_booErrorOccurred As Boolean
Private m_booBreak As Boolean, m_booResetClock As Boolean
Private m_booSubthreadStarted As Boolean
Private m_booHaltScripts As Boolean
Private m_lngLastWindow As Long
Private m_picSavedPictures() As Fury2Picture
Private m_lngSavedActivePictures As Long
Private m_hsSavedHotspots() As Fury2Hotspot
Private m_picPictures() As Fury2Picture
Private m_hsHotspots() As Fury2Hotspot
Private m_evtEventQueue() As Fury2QueueEvent
Private m_lngEventQueueLength As Long
Private m_booSmoothScaling As Boolean
Private m_booENetInitialized As Boolean
Private m_strIcon As String
Private m_strScriptContext() As String
Private m_lngContextDepth As Long
Private m_thrSubthreads() As Fury2SubthreadState
Private m_sngLastFrameStart As Double
Private m_sngLastFrameEnd As Double
Private m_sngLastSecond As Double
'Private m_sngFrameBuildup As Double
Private m_sngFrameLength As Double
Private m_lngFPSAccumulator As Long
Private m_sngMutePreviousVolume As Single
Private m_booRanStartEvent As Boolean
Private m_booFresh As Boolean
Private m_dblTimeOffset As Double
Private m_objNotify As Object
Private m_booNotifyNoLogging As Boolean
Private m_booIMEOpen As Boolean
Private m_lngIMECursorPos As Long
Private m_strIMEText As String

' Public state variables
Public ErrorOccurred As Boolean
Attribute ErrorOccurred.VB_VarDescription = "If True, an error has occurred."
Public VelocityMultiplier As Single
Attribute VelocityMultiplier.VB_VarDescription = "Specifies a constant multiplier to apply to the speed of all movement."
Public AnimationMultiplier As Single
Attribute AnimationMultiplier.VB_VarDescription = "Specifies a constant multiplier to apply to the speed of all animation."
Public GameTime As Long
Attribute GameTime.VB_VarDescription = "The current time in seconds since the game has started."
Public ActivePictureSlots As Long
Attribute ActivePictureSlots.VB_VarDescription = "The number of picture slots currently in use."
Public ScriptObjectCount As Long
Attribute ScriptObjectCount.VB_VarDescription = "The number of script objects currently defined."
Public ScriptBlockCount As Long
Public HTTPRequestCount As Long
Attribute HTTPRequestCount.VB_VarDescription = "The number of HTTP requests currently in use."
Public FPS As Long
Attribute FPS.VB_VarDescription = "The current framerate."
Public Terminating As Boolean
Attribute Terminating.VB_VarDescription = "If True, the game engine is shutting down."
Public TerminateEngine As Boolean
Attribute TerminateEngine.VB_VarDescription = "If True, the game engine is preparing to shut down."
Public Paused As Boolean
Attribute Paused.VB_VarDescription = "If True, the game engine will not perform updates."
Public Running As Boolean
Attribute Running.VB_VarDescription = "If True, the game is running."
Public FrameCount As Long
Attribute FrameCount.VB_VarDescription = "The number of frames that have been rendered since the game started."
Public SubthreadCount As Long
Attribute SubthreadCount.VB_VarDescription = "The number of subthreads currently active."
Public KeysEntered As String
Attribute KeysEntered.VB_VarDescription = "The buffer that recieves text input if TrapKeys is True."
Public CurrentGame As String
Attribute CurrentGame.VB_VarDescription = "The game that is currently running."
Public Halted As Boolean
Attribute Halted.VB_VarDescription = "If True, the engine is halted for some reason (most likely an error)."
Public ConsoleText As String
Attribute ConsoleText.VB_VarDescription = "The text currently being entered into the console by the user."
Private ConsoleScroll As Long
Private ConsoleHeight As Long
Public ConsoleVisible As Boolean
Attribute ConsoleVisible.VB_VarDescription = "If True, the console is open."

' Private runtime constants
Private m_curCPUFrequency As Currency

Function AddTextElement(Text, ByVal X As Long, ByVal Y As Long, Optional ByVal Realtime As Boolean = False) As Fury2TextElement
Attribute AddTextElement.VB_Description = "Creates a new text element and adds it to the plugin stack."
On Error Resume Next
Dim m_teNew As Fury2TextElement
    Set m_teNew = New Fury2TextElement
    With m_teNew
        .Realtime = Realtime
        .Text = ToString(Text)
        If Not Realtime Then
            .Text = ParseString(Text)
        End If
        .Color = F2White
        .Alpha = 1
        Set .Font = Me.Font
        Set .Rectangle = F2Rect(X, Y, Backbuffer.Width, Backbuffer.Height, True)
    End With
    HookEvents m_teNew
    Set AddTextElement = m_teNew
End Function

Function AddTextElementRealtime(Text, ByVal X As Long, ByVal Y As Long) As Fury2TextElement
Attribute AddTextElementRealtime.VB_Description = "Creates a new text element that is updated every frame and adds it to the plugin stack."
Attribute AddTextElementRealtime.VB_MemberFlags = "40"
On Error Resume Next
    Set AddTextElementRealtime = AddTextElement(Text, X, Y, True)
End Function

Public Function BindEvent(ByRef Method, Optional ByVal Object = Nothing, Optional ByRef BindParameters As Variant = Nothing) As Fury2Event
Attribute BindEvent.VB_Description = "Creates a Fury2Event object and binds it to a given method of a given object. Also can optionally bind one or more arguments to pass to the method when it is invoked. Bound parameters are passed after normal parameters."
On Error Resume Next
    If VarType(Method) = vbString Then
        If Len(Trim(ToString(Method))) = 0 Then Exit Function
        Method = ToString(Method)
        If InStr(Method, "(") Then
            BindParameters = m_ScriptEngine.Eval("Array" & Mid(Method, InStr(Method, "(")))
            Method = left(Method, InStr(Method, "(") - 1)
        ElseIf InStr(Method, " ") Then
            BindParameters = m_ScriptEngine.Eval("Array(" & Mid(Method, InStr(Method, " ") + 1) & ")")
            Method = left(Method, InStr(Method, " ") - 1)
        End If
        If InStr(Method, ".") Then
            Object = left(Method, InStr(Method, ".") - 1)
            Method = Mid(Method, InStr(Method, ".") + 1)
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

Function CallByName(ByVal Obj As Object, ByVal Member As String, ByVal CallType As Long, ParamArray Parameters() As Variant)
Attribute CallByName.VB_Description = "Calls a given method of a given object by name, and optionally passes parameters."
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

Public Function CreateAddress(ByVal Host As String, ByVal Port As Long) As VBENetAddress
Attribute CreateAddress.VB_Description = "Creates a network address."
On Error Resume Next
Dim l_adrAddress As VBENetAddress
    If m_booENetInitialized Then
        Set l_adrAddress = New VBENetAddress
        l_adrAddress.Host = Host
        l_adrAddress.Port = Port
        Set CreateAddress = l_adrAddress
    End If
End Function

Public Function CreateAddressFromString(ByVal Text As String) As VBENetAddress
Attribute CreateAddressFromString.VB_Description = "Creates a network address from an address string."
On Error Resume Next
Dim l_adrAddress As VBENetAddress
    If m_booENetInitialized Then
        Set l_adrAddress = New VBENetAddress
        l_adrAddress.Text = Text
        Set CreateAddressFromString = l_adrAddress
    End If
End Function

Public Function CreateClientHost(Optional ByVal ServerCount As Long = 1, Optional ByVal IncomingBandwidthLimit As Long = 0, Optional ByVal OutgoingBandwidthLimit As Long = 0) As VBENetHost
Attribute CreateClientHost.VB_Description = "Creates a network host."
On Error Resume Next
Dim l_hosHost As VBENetHost
    If m_booENetInitialized Then
        Set l_hosHost = New VBENetHost
        l_hosHost.SetEngine Me
        If l_hosHost.Create(Nothing, ServerCount, IncomingBandwidthLimit, OutgoingBandwidthLimit) Then
            Hosts.Add l_hosHost
            Set CreateClientHost = l_hosHost
        End If
    Else
        DebugOut "Unable to create client; ENet not available!"
    End If
End Function

Public Function CreateHardwareImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
Attribute CreateHardwareImage.VB_Description = "Creates a hardware-accelerated image, if available."
On Error Resume Next
    Set CreateHardwareImage = m_objOutputPlugin.CreateImage(Width, Height)
    If CreateHardwareImage Is Nothing Then
        Set CreateHardwareImage = F2Image(Width, Height)
    End If
End Function

Public Function CreateMenu(Items, Optional ByVal Area As Fury2Rect = Nothing, Optional SelectEvent = Nothing, Optional CloseEvent = Nothing, Optional MoveEvent = Nothing, Optional ByVal Cursor As Fury2Graphic = Nothing, Optional ByVal WindowSkin As Fury2WindowSkin = Nothing) As Fury2Menu
Attribute CreateMenu.VB_Description = "Creates a new menu object."
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

Public Function CreatePacket(Optional ByVal Length As Long = 0, Optional ByVal Reliable As Boolean = False) As VBENetPacket
Attribute CreatePacket.VB_Description = "Creates a network packet."
On Error Resume Next
Dim l_pktPacket As VBENetPacket
    If m_booENetInitialized Then
        Set l_pktPacket = New VBENetPacket
        If Length > 0 Then
            l_pktPacket.Create Length, Abs(CLng(Reliable))
        End If
        Set CreatePacket = l_pktPacket
    End If
End Function

Public Function CreatePacketFromFile(ByVal File As VirtualFile, Optional ByVal Reliable As Boolean = False)
Attribute CreatePacketFromFile.VB_Description = "Creates a network packet containing the contents of a file."
On Error Resume Next
Dim l_pktPacket As VBENetPacket
    If m_booENetInitialized Then
        Set l_pktPacket = New VBENetPacket
        If File.Length > 0 Then
            l_pktPacket.Create File.Length, Abs(CLng(Reliable))
            CopyMemory ByVal l_pktPacket.DataPointer, ByVal File.Pointer, File.Length
        End If
        Set CreatePacketFromFile = l_pktPacket
    End If
End Function

Public Function CreateServerHost(ByVal Address As VBENetAddress, Optional ByVal ClientCount As Long = 32, Optional ByVal IncomingBandwidthLimit As Long = 0, Optional ByVal OutgoingBandwidthLimit As Long = 0) As VBENetHost
Attribute CreateServerHost.VB_Description = "Creates a network host configured to be a server."
On Error Resume Next
Dim l_hosHost As VBENetHost
    If m_booENetInitialized Then
        Set l_hosHost = New VBENetHost
        l_hosHost.SetEngine Me
        If l_hosHost.Create(Address, ClientCount, IncomingBandwidthLimit, OutgoingBandwidthLimit) Then
            Hosts.Add l_hosHost
            Set CreateServerHost = l_hosHost
        End If
    Else
        DebugOut "Unable to create server; ENet not available!"
    End If
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

Function DataType(ByRef Data As Variant) As String
Attribute DataType.VB_Description = "Returns the data type of a value."
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

Function DistanceBetween(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single) As Single
Attribute DistanceBetween.VB_Description = "Returns the distance between two points."
Dim Rx As Single, Ry As Single
On Error Resume Next
    Rx = (x2 - x1)
    Ry = (y2 - y1)
    DistanceBetween = Sqr((Rx * Rx) + (Ry * Ry))
End Function

Function Evaluate(ByRef Expr) As Variant
Attribute Evaluate.VB_Description = "Evaluates an expression and returns its value."
On Error Resume Next
    Err.Clear
    VarCopy m_ScriptEngine.Eval(ToString(Expr)), Evaluate
End Function

Function ResolveName(ByRef Name) As Object
Attribute ResolveName.VB_Description = "Attempts to resolve a name into an object."
On Error Resume Next
    Set ResolveName = m_ScriptEngine.ResolveName(Name)
End Function

Public Function F2Camera(Optional ByVal Rectangle As Fury2Rect = Nothing, Optional ByVal Map As Long = 1) As Fury2Camera
Attribute F2Camera.VB_Description = "Creates a Fury2Camera object."
On Error Resume Next
    If Rectangle Is Nothing Then Set Rectangle = Backbuffer.Rectangle
    Set F2Camera = New Fury2Camera
    F2Camera.Map = Map
    F2Camera.Reposition Rectangle.left, Rectangle.top, Rectangle.Width, Rectangle.Height, False
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
Attribute F2Control.VB_Description = "Creates a Fury2InputControl object."
On Error Resume Next
    Set F2Control = New Fury2InputControl
    F2Control.SetEngine Me
    F2Control.Name = Name
End Function

Public Function F2Controller(ByVal Obj, Optional ByVal Camera As Fury2Camera = Nothing) As Fury2Controller
Attribute F2Controller.VB_Description = "Creates a controller object, and attaches it to a sprite or camera."
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
        .Angles = DefaultControllerAngles
        .Style = DefaultControllerStyle
        Err.Clear
    End With
    Set F2Controller = m_f2cController
    Set m_f2cController = Nothing
End Function

Public Function F2Counter() As Fury2Counter
    Set F2Counter = New Fury2Counter
On Error Resume Next
    Set F2Counter.Engine = Me
End Function

Public Function F2Dispatcher() As Fury2EventDispatcher
    Set F2Dispatcher = New Fury2EventDispatcher
On Error Resume Next
End Function

Public Function F2Font() As Fury2Font
Attribute F2Font.VB_Description = "Creates a new Fury2Font object."
On Error Resume Next
    Set F2Font = New Fury2Font
    F2Font.SetEngine Me
End Function

Public Function F2Graphic(Images, Optional ByVal XCenter As Long = -32767, Optional ByVal YCenter As Long = -32767, Optional ByVal BlitMode As SFXBlitModes = -1, Optional ByVal LoopMode As Fury2LoopModes = F2LM_Restart, Optional ByVal AnimationDelay As Long = 5) As Fury2Graphic
Attribute F2Graphic.VB_Description = "Creates a new Fury2Graphic object, optionally filling it with one or more image frames and setting parameters."
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
'        Graphics.Add l_gfcGraphic
'        RemoveRef l_gfcGraphic
    End With
    Err.Clear
    Set F2Graphic = l_gfcGraphic
End Function

Public Function F2Map(Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal Layers As Long = 1, Optional ByVal Tileset As Fury2Tileset = Nothing) As Fury2Map
Attribute F2Map.VB_Description = "Creates a new empty Fury2Map object, optionally specifying a size and tileset."
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

Public Function F2ParticleEngine() As Fury2ParticleEngine
Attribute F2ParticleEngine.VB_Description = "Creates a new Fury2ParticleEngine object."
On Error Resume Next
    Set F2ParticleEngine = New Fury2ParticleEngine
    F2ParticleEngine.SetEngine Me
End Function

Public Function F2Sequence() As Fury2Sequence
Attribute F2Sequence.VB_Description = "Creates a new Fury2Sequence object."
On Error Resume Next
    Set F2Sequence = New Fury2Sequence
    F2Sequence.SetEngine Me
End Function

Public Function F2Slider() As Fury2Slider
Attribute F2Slider.VB_Description = "Creates a new Fury2Slider object."
On Error Resume Next
    Set F2Slider = New Fury2Slider
    Set F2Slider.Engine = Me
End Function

Public Function F2SoundObject(ByVal Filename As String, Optional ByVal Repeat As Boolean = True) As Fury2SoundObject
Attribute F2SoundObject.VB_Description = "Creates a new Fury2SoundObject from a given sound file."
On Error Resume Next
Dim l_sndSound As Fury2SoundObject
    Set l_sndSound = New Fury2SoundObject
    l_sndSound.Sound = Filename
    l_sndSound.Repeat = Repeat
    Set F2SoundObject = l_sndSound
    Err.Clear
End Function

Public Function F2Sprite() As Fury2Sprite
Attribute F2Sprite.VB_Description = "Creates a new, empty Fury2Sprite object."
On Error Resume Next
Dim l_sprNew As Fury2Sprite
    Set l_sprNew = New Fury2Sprite
    l_sprNew.SetEngine Me
    Set F2Sprite = l_sprNew
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
Attribute F2WindowSkin.VB_Description = "Creates a new, empty Fury2WindowSkin object."
On Error Resume Next
    Set F2WindowSkin = New Fury2WindowSkin
    F2WindowSkin.SetEngine Me
End Function

Function FadeBGM(Optional ByVal FromVolume As Single = -1, Optional ByVal ToVolume As Single = -1, Optional ByVal Length As Double = 1) As Fury2SoundFader
Attribute FadeBGM.VB_Description = "Fades the current BGM to a new volume value. If no FromVolume is specified, the current volume is used. If no ToVolume is specified, the DefaultBGMVolume is used. If no Length is specified, the DefaultTransitionLength is used."
On Error Resume Next
Dim l_fadFader As Fury2SoundFader
    If CurrentBGMChannel Is Nothing Then Exit Function
    Set l_fadFader = CurrentBGMFader
    If Not (l_fadFader Is Nothing) Then
        If l_fadFader.Channel Is CurrentBGMChannel Then
            UnHookEvents l_fadFader
            Set CurrentBGMFader = Nothing
        End If
    End If
    Set l_fadFader = Nothing
    Set l_fadFader = New Fury2SoundFader
    Set l_fadFader.Channel = CurrentBGMChannel
    Set l_fadFader.Slider = F2Slider
    l_fadFader.VolumeStart = IIf(FromVolume < 0, l_fadFader.Channel.Volume, FromVolume)
    l_fadFader.VolumeEnd = IIf(ToVolume < 0, l_fadFader.Channel.Volume, ToVolume)
    l_fadFader.Slider.Start Length
    HookEvents l_fadFader
    Set FadeBGM = l_fadFader
    Set CurrentBGMFader = l_fadFader
    Err.Clear
End Function

Public Function CompileScriptBlock(ByVal Source As String, Optional ByVal Arguments As String = "") As Fury2Event
On Error Resume Next
Dim l_csScript As Fury2ScriptCache
Dim l_strSource As String
Dim l_strSourceL As String
Dim l_strArgs As String
    l_strSource = Source
    l_strSourceL = LCase(Trim(l_strSource))
    l_strArgs = Arguments
    For Each l_csScript In ScriptCache
        If LCase(Trim(l_csScript.Source)) = l_strSourceL Then
            Set CompileScriptBlock = BindEvent(CStr(l_csScript.Name))
            If CompileScriptBlock.MethodID = -1 Then
                Set CompileScriptBlock = Nothing
            Else
                Exit Function
            End If
        End If
    Next l_csScript
    ContextLevelAdd "Script"
    Set l_csScript = New Fury2ScriptCache
    ScriptBlockCount = ScriptBlockCount + 1
    l_csScript.Name = "SB_" & ScriptBlockCount
    l_csScript.Source = l_strSource
    With ScriptEngine.Language
        l_strSource = .GenerateFunction(l_csScript.Name, l_strArgs, l_strSource)
        ScriptEngine.AddCode l_strSource
    End With
    Set CompileScriptBlock = BindEvent(CStr(l_csScript.Name))
    If CompileScriptBlock.MethodID = -1 Then
        Set CompileScriptBlock = Nothing
    Else
        ScriptCache.Add l_csScript
    End If
    ContextLevelRemove
End Function

Public Function GenerateScriptObject(ByVal Source As String) As Object
Attribute GenerateScriptObject.VB_Description = "Creates an object from a specified object script."
On Error Resume Next
Dim l_csScript As Fury2ScriptCache
Dim l_strSource As String
Dim l_colDataVars As Collection
Dim l_strLines() As String, l_lngLine As Long
Dim l_strLine As String, l_strToken As String, l_strArgs As String
Dim l_strVars() As String, l_lngVars As Long
Dim l_strData As String, l_strVar As String
Dim l_strSourceL As String
    l_strSource = Source
    ' Parse data members
    Set l_colDataVars = New Collection
    l_strLines = ScriptEngine.Language.SplitLines(l_strSource)
    For l_lngLine = LBound(l_strLines) To UBound(l_strLines)
        l_strLine = Trim(l_strLines(l_lngLine))
        If InStr(l_strLine, " ") Then
            l_strToken = left(l_strLine, InStr(l_strLine, " ") - 1)
            l_strArgs = Mid(l_strLine, InStr(l_strLine, " ") + 1)
            Select Case LCase(l_strToken)
            Case "publicdata", "privatedata"
                l_strVars = Split(l_strArgs, ",")
                For l_lngVars = LBound(l_strVars) To UBound(l_strVars)
                    l_colDataVars.Add l_strVars(l_lngVars)
                Next l_lngVars
            End Select
        End If
    Next l_lngLine
    If (l_colDataVars.Count > 0) Then
        With ScriptEngine.Language
            l_strData = ""
            For l_lngVars = 1 To l_colDataVars.Count
                l_strVar = l_colDataVars(l_lngVars)
                l_strData = l_strData + .GenerateSubCall("File" + .MethodOperator + "SaveVariant", l_strVar)
            Next l_lngVars
            l_strSource = l_strSource + vbCrLf + .GenerateClassMethod("Object_Save", "File", l_strData)
            l_strData = ""
            For l_lngVars = 1 To l_colDataVars.Count
                l_strVar = l_colDataVars(l_lngVars)
                l_strData = l_strData + .GenerateAssignment(l_strVar, .GenerateFunctionCall("File" + .MethodOperator + "LoadVariant", ""), False)
            Next l_lngVars
            l_strSource = l_strSource + vbCrLf + .GenerateClassMethod("Object_Load", "File", l_strData)
        End With
    End If
    l_strSourceL = LCase(Trim(l_strSource))
    For Each l_csScript In ScriptCache
        If l_csScript.Source = l_strSourceL Then
            Set GenerateScriptObject = ScriptEngine.Eval(ScriptEngine.Language.GenerateInstantiation(l_csScript.Name))
            Exit Function
        End If
    Next l_csScript
    ContextLevelAdd "Object"
    Set l_csScript = New Fury2ScriptCache
    ScriptObjectCount = ScriptObjectCount + 1
    l_csScript.Name = "SO_" & ScriptObjectCount
    l_csScript.Source = l_strSource
    ScriptCache.Add l_csScript
    With ScriptEngine.Language
        ScriptEngine.AddCode .GenerateClass(l_csScript.Name, l_strSource)
    End With
    Set GenerateScriptObject = ScriptEngine.Eval(ScriptEngine.Language.GenerateInstantiation(l_csScript.Name))
    ContextLevelRemove
End Function

Public Function GetFile(ByRef Filename As String) As String
Attribute GetFile.VB_Description = "Reads the contents of a file."
On Error Resume Next
    GetFile = Filesystem.File(Filename).Text
End Function

Friend Sub PictureSlotDestroyed(ByVal Slot As Long)
On Error Resume Next
    m_picPictures(Slot).pObj = 0
End Sub

Public Function NewPictureSlot() As Fury2PictureSlot
On Error Resume Next
    Set NewPictureSlot = GetPictureSlot(ActivePictureSlots)
End Function

Public Function GetPictureSlot(ByVal Slot As Long) As Fury2PictureSlot
On Error Resume Next
Dim l_sltSlot As Fury2PictureSlot
Dim l_lngZero As Long
    If m_picPictures(Slot).pObj Then
        CopyMemory ByVal VarPtr(l_sltSlot), ByVal VarPtr(m_picPictures(Slot).pObj), 4
        Set GetPictureSlot = l_sltSlot
        CopyMemory ByVal VarPtr(l_sltSlot), ByVal VarPtr(l_lngZero), 4
    Else
        Set l_sltSlot = New Fury2PictureSlot
        l_sltSlot.SetEngine Me
        l_sltSlot.SetIndex Slot
        CopyMemory ByVal VarPtr(m_picPictures(Slot).pObj), ByVal VarPtr(l_sltSlot), 4
        Set GetPictureSlot = l_sltSlot
    End If
End Function

Public Function GetPictureAlpha(ByVal Slot As Long) As Single
Attribute GetPictureAlpha.VB_Description = "Returns the opacity of a picture slot."
On Error Resume Next
    GetPictureAlpha = m_picPictures(Slot).Alpha
End Function

Public Function GetPictureBlitMode(ByVal Slot As Long) As SFXBlitModes
On Error Resume Next
    GetPictureBlitMode = m_picPictures(Slot).EffectMode - 1
End Function

Public Function GetPictureHeight(ByVal Slot As Long) As Long
Attribute GetPictureHeight.VB_Description = "Returns the width of a picture slot."
On Error Resume Next
    GetPictureHeight = m_picPictures(Slot).DestRect.Height
End Function

Public Function GetPictureImage(ByVal Slot As Long) As Fury2Image
Attribute GetPictureImage.VB_Description = "Returns the image placed in a picture slot."
On Error Resume Next
    Set GetPictureImage = m_picPictures(Slot).Picture
End Function

Public Function GetPictureGraphic(ByVal Slot As Long) As Fury2Graphic
On Error Resume Next
    Set GetPictureGraphic = m_picPictures(Slot).Picture
End Function

Public Function GetPictureRotation(ByVal Slot As Long) As Single
On Error Resume Next
    GetPictureRotation = m_picPictures(Slot).Rotation
End Function

Public Function GetPictureRectangle(ByVal Slot As Long) As Fury2Rect
Attribute GetPictureRectangle.VB_Description = "Returns the rectangle of a picture slot."
On Error Resume Next
    Set GetPictureRectangle = m_picPictures(Slot).DestRect
End Function

Public Function GetPictureSourceRectangle(ByVal Slot As Long) As Fury2Rect
Attribute GetPictureSourceRectangle.VB_Description = "Returns the source rectangle of a picture slot."
On Error Resume Next
    Set GetPictureSourceRectangle = m_picPictures(Slot).SourceRect
End Function

Public Function GetPictureTile(ByVal Slot As Long) As Boolean
Attribute GetPictureTile.VB_Description = "Returns the tiling state of a picture slot."
On Error Resume Next
    GetPictureTile = m_picPictures(Slot).Tile
End Function

Public Function GetPictureWidth(ByVal Slot As Long) As Long
Attribute GetPictureWidth.VB_Description = "Returns the width of a picture slot."
On Error Resume Next
    GetPictureWidth = m_picPictures(Slot).DestRect.Width
End Function

Public Function GetPictureX(ByVal Slot As Long) As Long
Attribute GetPictureX.VB_Description = "Returns the X coordinate of a picture slot (top-left)."
On Error Resume Next
    GetPictureX = m_picPictures(Slot).DestRect.left
End Function

Public Function GetPictureY(ByVal Slot As Long) As Long
Attribute GetPictureY.VB_Description = "Returns the Y coordinate of a picture slot (top-right)."
On Error Resume Next
    GetPictureY = m_picPictures(Slot).DestRect.top
End Function

Public Function GetReferenceCount(ByVal Obj As Object) As Long
Attribute GetReferenceCount.VB_Description = "Returns an estimate of the number of references currently held to a given object."
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown
    Set m_objObj = Obj
    If Err <> 0 Then Exit Function
    m_objObj.AddRef
    GetReferenceCount = m_objObj.Release
End Function

Public Function GetUserInput(Optional ByRef Prompt As String = "Input:") As String
Attribute GetUserInput.VB_Description = "Displays a Win32 input box requesting that the user input some text."
On Error Resume Next
    GetUserInput = InputBox(Prompt)
End Function

Public Function GrabScreen(Optional ByVal ForceRedraw As Boolean = True) As Fury2Image
Attribute GrabScreen.VB_Description = "Grabs a copy of the current contents of the screen and stores it in a new image. If ForceRedraw is True, the screen is redrawn first."
On Error Resume Next
Dim l_imgCopy As Fury2Image
    If ForceRedraw Then
        Redraw
    End If
    Set l_imgCopy = F2Image(Backbuffer.Width, Backbuffer.Height)
    l_imgCopy.Copy Backbuffer
    Set GrabScreen = l_imgCopy
End Function

Public Function HiTimer() As Double
Attribute HiTimer.VB_Description = "Returns a timer value that can be used for timing purposes."
On Error Resume Next
    HiTimer = SoftFX.GetTime() - m_dblTimeOffset
End Function

Public Function HookEvents(ByVal Obj As Object, Optional ByVal Priority As Boolean = False) As Boolean
Attribute HookEvents.VB_Description = "Hooks an object into the engine's event system so that it recieves notification of events."
On Error Resume Next
Dim l_plgObj As Fury2Plugin
    If Obj Is Nothing Then Exit Function
    Err.Clear
    If TypeOf Obj Is Fury2Plugin Then
        Set l_plgObj = Obj
        Set l_plgObj.Engine = Me
    Else
        Set Obj.Engine = Me
    End If
    If Err.Number <> 0 Then
        If LCase(Trim(TypeName(Obj))) = "vbscripttypeinfo" Then
        Else
            DebugOut "Warning: HookEvents called on " & ToString(Obj) & ", has no Engine property"
        End If
    End If
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
    Err.Clear
End Function

Function IIf(ByVal Expression As Boolean, ByRef TruePart, Optional ByRef FalsePart = 0)
Attribute IIf.VB_Description = "If Expression is True, TruePart is returned, otherwise FalsePart is returned."
On Error Resume Next
    IIf = VBA.IIf(Expression, TruePart, FalsePart)
End Function

Friend Function iLoadImage(ByVal Filename As String, Optional ByVal IgnoreErrors As Boolean = False, Optional ByRef IsNew As Boolean = False) As Long
On Error Resume Next
Dim m_lngImages As Long
Dim m_imgImage As Fury2Image
Dim m_strName As String
Dim m_filFile As Fury2File
Dim l_strSuffix As String
Static m_lngHere As Long
    DebugOut "Loading image """ & Filename & """"
    m_lngHere = m_lngHere + 1
    Err.Clear
    If InStr(Filename, "%") Then
        l_strSuffix = Mid(Filename, InStrRev(Filename, "%"))
        Filename = left(Filename, InStrRev(Filename, "%") - 1)
    End If
    Filename = Filesystem.AutoSelectExtension(Filename, "png;f2g;tga;jpg;bmp;gif")
    m_strName = Filename
    If m_Images.Count > 0 Then
        For m_lngImages = 1 To m_Images.Count
            If m_Images(m_lngImages) Is Nothing Then
            Else
                If m_Images(m_lngImages).Name = LCase(Trim(Filename)) Then
                    iLoadImage = m_lngImages
                    m_lngHere = m_lngHere - 1
                    IsNew = False
                    Exit Function
                End If
            End If
        Next m_lngImages
    End If
    If Filesystem.FileExists(Filename) Then
        Set m_filFile = Filesystem.File(Filename)
        Set m_imgImage = F2LoadImage(m_filFile.GetRealFilename + l_strSuffix)
        m_imgImage.Optimize
        m_imgImage.Name = Filename
        IsNew = True
    Else
    End If
    If m_imgImage Is Nothing Then
        If IgnoreErrors Then
        Else
            If m_booErrorsOff Then
            Else
                If Not EditMode Then CriticalError "LoadImage", "LoadImage", "Unable to load image: """ + Filename + """"
            End If
        End If
        Exit Function
    End If
    SetImageTag m_imgImage.Handle, 1, 1234
'    m_imgImage.Name = LCase(Trim(m_strName))
    m_Images.Add m_imgImage, LCase(Trim(m_strName))
    iLoadImage = m_Images.Count
    m_lngHere = m_lngHere - 1
    Err.Clear
End Function

Public Function InstantiateByName(ByRef Name As String) As Object
Attribute InstantiateByName.VB_Description = "Creates a new instance of a given type and returns it."
On Error Resume Next
Dim l_objItem As Object
Dim l_parItem As Fury2Party
    Select Case LCase(Trim(Name))
    Case "fury2party"
        Set l_parItem = New Fury2Party
        l_parItem.SetEngine Me
        Set l_objItem = l_parItem
    Case Else
    End Select
    If l_objItem Is Nothing Then
        ScriptEngine.AddCode "Set Engine.IObjThis = " & ScriptEngine.Language.GenerateInstantiation(Trim(LCase(Name)))
        Set l_objItem = IObjThis
        Set IObjThis = Nothing
        If l_objItem Is Nothing Then
            Err.Clear
            Set l_objItem = CreateObject("Engine." & Name)
            If l_objItem Is Nothing Then
                Err.Clear
                Set l_objItem = CreateObject("libGraphics." & Name)
            End If
        End If
    End If
    Set InstantiateByName = l_objItem
End Function

Function JoinX(Arr As Variant, Optional ByVal Delimiter As String = " ", Optional ByVal QuoteStrings As Boolean = False) As String
Attribute JoinX.VB_Description = "Joins together the contents of an array into a string, inserting Delimiter between each item, and if QuoteStrings is true, strings will have quotes placed around them to keep them seperate."
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

Function LoadBrush(ByVal Filename As String) As Object
Attribute LoadBrush.VB_Description = "Loads a Fury2Brush from a file."
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

Public Function LoadFlags(ByVal File As VirtualFile)
Attribute LoadFlags.VB_Description = "Loads the current state of the Flags array from a file."
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

Public Function CreateDefaultCursor() As Fury2Graphic
Attribute CreateDefaultCursor.VB_Description = "Creates the default menu cursor image that is used if no cursor file has been used. This image is created on demand."
On Error Resume Next
Dim l_imgArrow As Fury2Image
Dim l_lngSize As Long
    l_lngSize = 9 * 2
    Set l_imgArrow = F2Image(l_lngSize, l_lngSize)
    l_imgArrow.AlphaChannel = True
    l_imgArrow.Clear 0
    l_imgArrow.AntiAliasConvexPolygon Array(Array(0, 0), Array(l_lngSize - 1, l_lngSize / 2), Array(0, l_lngSize - 1)), F2Black, RenderMode_Merge
    l_imgArrow.AntiAliasConvexPolygon Array(Array(2, 2), Array(l_lngSize - 3, l_lngSize / 2), Array(2, l_lngSize - 3)), F2White, RenderMode_Merge
    Set CreateDefaultCursor = F2Graphic(l_imgArrow.Resample(l_lngSize \ 2, l_lngSize \ 2, ResampleMode_Bilinear_High_Quality), , , BlitMode_SourceAlpha)
End Function

Public Function LoadFont(ByVal Filename As String) As Fury2Font
Attribute LoadFont.VB_Description = "Loads a font from a .f2font file."
On Error Resume Next
    Set LoadFont = New Fury2Font
    LoadFont.SetEngine Me
    LoadFont.Load Filename
End Function

Public Function LoadSequence(ByVal Filename As String) As Fury2Sequence
On Error Resume Next
    Set LoadSequence = New Fury2Sequence
    LoadSequence.SetEngine Me
    If LoadFromFile(LoadSequence, Filesystem.File(Filename)) = False Then
        LoadSequence.Free
        Set LoadSequence = Nothing
    End If
End Function

Public Function LoadFromFile(ByVal Obj As Object, ByVal File As Object) As Boolean
Attribute LoadFromFile.VB_Description = "Loads a given object from a given file. If the load was successful, True is returned."
Attribute LoadFromFile.VB_MemberFlags = "40"
On Error Resume Next
Dim l_glbGlobals As New Fury2Globals
    LoadFromFile = l_glbGlobals.LoadFromFile(Obj, File, BindEvent("InternalLoadProgress", Me, "File"))
End Function

Public Function LoadGame(ByVal Game As String) As Boolean
Attribute LoadGame.VB_Description = "Loads a new game from a given path. If the load failed, False is returned."
On Error Resume Next
Dim l_varScripts As Variant, l_lngScripts As Long
Dim l_varPackages As Variant, l_lngPackages As Long
Dim l_objObject As Object, l_varTest As Variant
Dim l_strError As String
    ContextLevelAdd "LoadGame"
    DebugOut "Loading game """ & Game & """"
    Set ConsoleItems = F2Collection()
    If Not m_booFresh Then
        DebugOut "Resetting internal data structures"
        Class_Terminate
        Class_Initialize
    End If
    m_booFresh = False
    DebugOut "Initializing filesystem"
    If Trim(Game) = "" Then
        If m_booIDE Then
            If right(App.Path, 1) = "\" Then
                SystemRoot = App.Path + "..\..\binary\"
            Else
                SystemRoot = App.Path + "\..\..\binary\"
            End If
        Else
            If right(App.Path, 1) = "\" Then
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
    If Filesystem Is Nothing Then
        MsgBox "Unable to intialize filesystem!", vbCritical, "Error"
        Exit Function
    End If
    DebugOut "Loading configuration files"
    Configuration.Load "game.f2config"
    Configuration.Load "global.f2config"
    ParseConfiguration
    DebugOut "Initializing script engine"
    l_strError = InitScriptEngine
    If Len(l_strError) > 0 Then
        MsgBox "Unable to initialize script engine!" & vbCrLf & l_strError, vbCritical, "Error"
        Exit Function
    End If
    DebugOut "Initializing network library"
    InitENet
    DebugOut "Initializing video library"
    AVIFileInit
    If EditMode Then
        Err.Clear
        l_varPackages = Split(Filesystem.File("editor_packages.f2config").Text, vbCrLf)
        l_lngPackages = UBound(l_varPackages)
        If Err = 0 Then
            For l_lngPackages = LBound(l_varPackages) To UBound(l_varPackages)
    '            Filesystem.LoadPackage Trim(l_varPackages(l_lngPackages))
            Next l_lngPackages
        End If
        DebugOut "Initializing sound engine"
        InitSoundEngine
        If SoundEngine Is Nothing Then
            MsgBox "Unable to initialize sound engine!", vbCritical, "Error"
        End If
    Else
        DebugOut "Initializing input engine"
        InitInput
        Include "constants", False
        Include "settings.vbs", False
        Include "game.f2cfg", False
        Include "global.f2cfg", False
        If Not (HideConfiguration) Then
            Load frmConfigure
            frmConfigure.Cancelled = False
            Set frmConfigure.Engine = Me
            frmConfigure.Caption = "Configure " & GameName
            frmConfigure.LoadSettings
            frmConfigure.Show
            frmConfigure.SetFocus
            Do While frmConfigure.Visible
                If Not InIDE Then
                    If GetActiveWindow <> frmConfigure.hWnd Then frmConfigure.SetFocus
                End If
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
        DebugOut "Initializing sound engine"
        InitSoundEngine
        If SoundEngine Is Nothing Then
            MsgBox "Unable to initialize sound engine!", vbCritical, "Error"
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
        m_sngConsoleOffset = ConsoleHeight
        If Trim(WindowCaption) = "" Then WindowCaption = Version
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
        DebugOut "Initializing game data"
        Dim l_newMap As Fury2Map
        Set l_newMap = New Fury2Map
        l_newMap.SetEngine Me
        Maps.Add l_newMap
        Cameras.Add New Fury2Camera
        Cameras(1).SetEngine Me
        Cameras(1).Reposition 0, 0, ScreenWidth, ScreenHeight
        Cameras(1).Map = 1
        Err.Clear
        DebugOut "Loading game packages"
        l_varPackages = Split(Filesystem.File("packages.f2config").Text, vbCrLf)
        l_lngPackages = UBound(l_varPackages)
        If Err = 0 Then
            For l_lngPackages = LBound(l_varPackages) To UBound(l_varPackages)
    '            Filesystem.LoadPackage Trim(l_varPackages(l_lngPackages))
            Next l_lngPackages
        End If
        DebugOut "Loading game scripts"
        Include "party", False
        Include "system", False
        Include "joystick", False
        For Each l_objObject In Joysticks
            HookEvents l_objObject
        Next l_objObject
        Err.Clear
        l_varScripts = Split(Filesystem.File("scripts.f2config").Text, vbCrLf)
        l_lngScripts = UBound(l_varScripts)
        If Err = 0 Then
            For l_lngScripts = LBound(l_varScripts) To UBound(l_varScripts)
                Include Trim(l_varScripts(l_lngScripts))
            Next l_lngScripts
        End If
        DebugOut "Starting game"
        m_dblTimeOffset = 0
        m_dblTimeOffset = HiTimer
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
    End If
    LoadProgress "Game", 10
    DebugOut "Initializing font"
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
    DebugOut "Initializing window skin"
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
    DebugOut "Initializing menu cursor"
    Set MenuCursor = New Fury2Graphic
    MenuCursor.SetEngine Me
    If Trim(LCase(DefaultCursor)) = "none" Then
    ElseIf DefaultCursor <> "" Then
        Set MenuCursor = LoadGraphic(DefaultCursor)
    Else
        Set MenuCursor = CreateDefaultCursor()
        DebugOut "No default cursor specified"
    End If
    LoadProgress "Game", 20
    If EditMode Then
        DebugOut "Loading editor scripts"
        Err.Clear
        l_varScripts = Split(Filesystem.File("editor_scripts.f2config").Text, vbCrLf)
        l_lngScripts = UBound(l_varScripts)
        If Err = 0 Then
            For l_lngScripts = LBound(l_varScripts) To UBound(l_varScripts)
                Include Trim(l_varScripts(l_lngScripts))
            Next l_lngScripts
        End If
    Else
        DebugOut "Loading start map"
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
        m_lngEventQueueLength = 0
        ResetKeys
        LoadGame = True
    End If
    ContextLevelRemove
    DebugOut "Game loaded"
End Function

Public Function LoadGraphic(ByVal Filename As String) As Fury2Graphic
On Error Resume Next
    If InStr(Filename, "@") Then
        Set LoadGraphic = F2Graphic(LoadImage(Trim(Split(DefaultCursor, "@")(0))), Split(Split(DefaultCursor, "@")(1), ",")(0), Split(Split(DefaultCursor, "@")(1), ",")(1))
    Else
        Set LoadGraphic = New Fury2Graphic
        LoadGraphic.SetEngine Me
        LoadGraphic.Load Filename
    End If
End Function

Public Function LoadImage(ByVal Filename As String, Optional ByVal IgnoreErrors As Boolean = False) As Fury2Image
Attribute LoadImage.VB_Description = "Loads an image file into the cache and returns a reference to it if successful. If IgnoreErrors is False, a failed image load will halt the game."
On Error Resume Next
Dim m_imgPicture As Fury2Image
Dim m_imgSecondRef As Fury2Image
    If Trim(CStr(Filename)) = "" Then Exit Function
    Err.Clear
    Set m_imgPicture = Nothing
    Filename = LCase(Trim(Filesystem.AutoSelectExtension(Filename, "png;tga;jpg;bmp;gif;pcx")))
    Set m_imgPicture = m_Images(Filename)
    If (m_imgPicture Is Nothing) Or (Err.Number <> 0) Then
        Err.Clear
        If Filesystem.FileExists(Filename) Then
            Set m_imgPicture = m_Images(Me.iLoadImage(Filename, IgnoreErrors))
        Else
            Set m_imgPicture = Nothing
        End If
    End If
    m_imgPicture.Name = Filename
    If m_imgPicture.Handle = 0 Then Set m_imgPicture = Nothing
    If m_imgPicture Is Nothing Then
        If (EditMode = False) And (IgnoreErrors = False) Then CriticalError "LoadImage", "LoadImage", "Unable to load image: """ + Filename + """"
    End If
    Set LoadImage = m_imgPicture
    Set m_imgSecondRef = LoadImage
    If EditMode Then
        If (SoftFX.GetTime() - m_dblLastFlushTime) > 60 Then
            m_dblLastFlushTime = SoftFX.GetTime
            FlushImageCache
        End If
    Else
        If (SoftFX.GetTime() - m_dblLastFlushTime) > 600 Then
            m_dblLastFlushTime = SoftFX.GetTime
            FlushImageCache
        End If
    End If
End Function

Public Function LoadImages(ParamArray Filenames() As Variant) As Variant
Attribute LoadImages.VB_Description = "Loads one or more images and returns them in an array."
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

Public Function LoadMapFile(ByVal Filename As String) As Fury2Map
Attribute LoadMapFile.VB_Description = "Loads a map from a .f2map file and returns it."
On Error Resume Next
    Set LoadMapFile = New Fury2Map
    LoadMapFile.SetEngine Me
    LoadFromFile LoadMapFile, Filesystem.File(Filename).Load()
    LoadMapFile.Filename = Filename
End Function

Function LoadOutputPlugin(ByRef NewPlugin As String) As Object
Attribute LoadOutputPlugin.VB_Description = "Loads a new output plugin."
On Error Resume Next
Dim l_objTemp As Object
    Err.Clear
    DebugOut "Loading output plugin """ & NewPlugin & """"
    If Not (Debugger Is Nothing) Then Set l_objTemp = Debugger.DebugCreateOutputPlugin()
    If l_objTemp Is Nothing Then
        Err.Clear
        If (InIDE() = False) And (Compromise.IsSupported() = 1) Then
            If Compromise.Register(App.Path + "\video_" + NewPlugin + ".dll") = 0 Then
                If Compromise.Register(RootPath + "\video_" + NewPlugin + ".dll") = 0 Then
                End If
            End If
        Else
        End If
        Set l_objTemp = CreateObject("Video_" + CStr(Trim(NewPlugin)) + "." + CStr(Trim(NewPlugin)) + "Engine")
        If Err.Number <> 0 Then
            If Err.Number <> 0 Then CriticalError "LoadOutputPlugin """ + CStr(NewPlugin) + """", "CreateObject()", Err.Description + "(Error #" + CStr(Err.Number) + ")"
        End If
    End If
    If l_objTemp Is Nothing Then Exit Function
    l_objTemp.HookEvents Me
    SetAppIcon l_objTemp.Window
    m_lngLastWindow = l_objTemp.Window.hWnd
    Set LoadOutputPlugin = l_objTemp
    Set l_objTemp = Nothing
End Function

Public Function LoadPlugin(ByRef DLLFile As String, ByRef PluginName As String) As Object
Attribute LoadPlugin.VB_Description = "Loads a plugin object from a DLL."
On Error Resume Next
Dim l_filFile As Fury2File
    DebugOut "Loading plugin """ & DLLFile & """"
    If Filesystem.FileExists(DLLFile) Then
        Set l_filFile = Filesystem.File(DLLFile)
        If (InIDE() = False) And (Compromise.IsSupported() = 1) Then Compromise.Register l_filFile.GetRealFilename
    ElseIf FileLen(App.Path & "\" & Replace(DLLFile, "/", "\")) > 0 Then
        ' In engine dir
        If (InIDE() = False) And (Compromise.IsSupported() = 1) Then Compromise.Register App.Path & "\" & Replace(DLLFile, "/", "\")
    Else
        CriticalError "LoadPlugin", "File check", "Unable to load plugin """ & PluginName & """, """ & DLLFile & """ not found"
    End If
    Err.Clear
    Set LoadPlugin = CreateObject(PluginName)
    If LoadPlugin Is Nothing Or Err.Number <> 0 Then
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

Function LoadSample(ByRef Filename As String) As Sound
Attribute LoadSample.VB_Description = "Loads a Sample from a sound file and returns it."
On Error Resume Next
    Set LoadSample = SoundEngine.Load(Filename, "Sample")
    LoadSample.Volume = DefaultSampleVolume
End Function

Function LoadSprite(ByVal Filename As String, Optional ByVal Index = 1) As Fury2Sprite
Attribute LoadSprite.VB_Description = "Loads a single Fury2Sprite from a sprite collection and returns it. You can specify which sprite to load using the Index argument. Note that the entire collection will be loaded before the sprite can be accessed, so if other sprites in this collection have scripts, those scripts may execute."
On Error Resume Next
Dim m_sprSprites As Fury2Sprites
Dim l_sprSprite As Fury2Sprite
    Err.Clear
    Set m_sprSprites = LoadSprites(Filename)
    If m_sprSprites Is Nothing Then Exit Function
    Set LoadSprite = m_sprSprites(Index)
    For Each l_sprSprite In m_sprSprites
        If l_sprSprite Is LoadSprite Then
        Else
            l_sprSprite.Free
        End If
    Next l_sprSprite
    m_sprSprites.Clear
    Set m_sprSprites = Nothing
    If Err <> 0 Then
        Set LoadSprite = Nothing
    End If
End Function

Function LoadSprites(ByVal Filename As String) As Fury2Sprites
Attribute LoadSprites.VB_Description = "Loads a sprite collection from a .f2sprites file and returns it."
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

Public Function LoadTileset(ByVal Filename As String) As Fury2Tileset
Attribute LoadTileset.VB_Description = "Loads a tileset and returns it."
On Error Resume Next
    Set LoadTileset = m_Tilesets(LCase(Trim(Filename)))
    If LoadTileset Is Nothing Then
        Set LoadTileset = New Fury2Tileset
        LoadTileset.SetEngine Me
        LoadFromFile LoadTileset, Filesystem.File(Filename).Load()
        m_Tilesets.Add LoadTileset, LCase(Trim(Filename))
    End If
End Function

Public Function LoadUserData(ByVal Filename As String, Optional ByVal Data As Object = Nothing) As Object
Attribute LoadUserData.VB_Description = "Loads a UserData object from a file. If an existing object is passed into Data, it will be overwritten."
On Error Resume Next
Dim l_vfFile As VirtualFile
Dim l_strType As String
Dim l_objData As Object
    Set l_vfFile = Filesystem.File(Filename).Load
    Set LoadUserData = LoadUserDataFromFile(l_vfFile, Data)
    Err.Clear
End Function

Public Function LoadUserDataFromFile(ByVal File As VirtualFile, Optional ByVal Data As Object = Nothing) As Object
Attribute LoadUserDataFromFile.VB_Description = "Loads a UserData object from a file. If an existing object is passed into Data, it will be overwritten."
On Error Resume Next
Dim l_strType As String
    File.ReadSegment_Begin
        File.Load l_strType
        If (Data Is Nothing) Then
            Set Data = InstantiateByName(l_strType)
        End If
        File.Load Data
    File.ReadSegment_End
    Set Data.Engine = Me
    Set LoadUserDataFromFile = Data
    Err.Clear
End Function

Public Function LoadVideo(ByVal Filename As String) As Fury2Video
Attribute LoadVideo.VB_Description = "Loads a video clip from a file and returns it."
On Error Resume Next
Dim l_vidNew As Fury2Video
    Set l_vidNew = Videos(Filename)
    If l_vidNew Is Nothing Then
        Set l_vidNew = New Fury2Video
        l_vidNew.SetEngine Me
        If l_vidNew.Load(Filename) Then
            Err.Clear
            Videos.Add l_vidNew, Filename
            Set LoadVideo = l_vidNew
        End If
    End If
    Err.Clear
End Function

Public Function LoadWindowSkin(ByVal Filename As String) As Fury2WindowSkin
Attribute LoadWindowSkin.VB_Description = "Loads a window skin from a file and returns it."
On Error Resume Next
    Set LoadWindowSkin = New Fury2WindowSkin
    LoadWindowSkin.SetEngine Me
    LoadWindowSkin.Load Filename
End Function

Public Function Maximum(ParamArray Values() As Variant) As Variant
Attribute Maximum.VB_Description = "Returns the largest value of the values passed."
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
Attribute MessageBox.VB_Description = "Creates a new message box, calls HookEvents on it if Hook is True, and then returns it. If Queue is true, the message box will be queued if an existing message box is already open."
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
Attribute MessageBoxQueue.VB_Description = "Creates a new message box, calls HookEvents on it if Hook is True, and then returns it. The message box will be queued if an existing message box is already open."
On Error Resume Next
    Set MessageBoxQueue = MessageBox(Text, Area, Portrait, CloseEvent, TextColor, Alpha, True, Hook)
End Function

Public Function Minimum(ParamArray Values() As Variant) As Variant
Attribute Minimum.VB_Description = "Returns the smallest value of the values passed."
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

Public Function NullEvent() As Fury2Event
Attribute NullEvent.VB_Description = "Returns a null Fury2Event object that will do nothing when invoked."
On Error Resume Next
'    Set NullEvent = New Fury2Event
'    NullEvent.SetEngine Me
End Function

Public Function ParseCodes(ByVal Str As String) As String
Attribute ParseCodes.VB_Description = "Parses a string that contains substitution codes (<%=expression%>) into a normal string."
On Error Resume Next
    ParseCodes = ScriptEngine.ParseCodes(Str)
End Function

Public Function ParseString(ByRef Str, Optional ByRef Variables = Nothing)
Attribute ParseString.VB_Description = "Parses inline scripts in a string. Optional replacement values can be specified."
On Error Resume Next
Dim l_strText As String, l_strConstant As String, l_strValue As String
Dim l_strOldText As String
Dim l_lngStartPos As Long, l_lngLength As Long, l_lngPos As Long, l_lngRelPos
Dim l_lngRelStartPos As Long
Dim l_varVariables As Variant, l_lngVariables As Long, l_booFound As Boolean
    ContextLevelAdd "ParseString"
    If (VarType(Variables) And vbArray) = vbArray Then
        l_varVariables = Variables
    Else
        l_varVariables = Array(Array("", ""))
    End If
    l_strText = ToString(Str)
    l_strOldText = l_strText
    l_lngPos = 1
    l_lngRelPos = 1
    l_lngStartPos = InStr(l_lngPos, l_strOldText, "<%=")
    l_lngRelStartPos = InStr(l_lngRelPos, l_strText, "<%=")
    If l_lngStartPos <> 0 Then
        Do While l_lngStartPos <> 0
            If l_lngRelPos > Len(l_strText) Then Exit Do
            If InStr(l_lngRelPos, l_strText, "%>") = 0 Then Exit Do
            l_lngLength = InStr(l_lngPos, l_strOldText, "%>") - (l_lngStartPos + 3)
            l_strConstant = Mid(l_strOldText, l_lngStartPos + 3, l_lngLength)
            l_strValue = "<error>"
            l_booFound = False
            Err.Clear
            For l_lngVariables = LBound(l_varVariables) To UBound(l_varVariables)
                If LCase(Trim(l_varVariables(l_lngVariables)(0))) = LCase(Trim(l_strConstant)) Then
                    l_strValue = l_varVariables(l_lngVariables)(1)
                    l_booFound = True
                    Exit For
                End If
            Next l_lngVariables
            If l_booFound Then
                If (Err.Number <> 0) Then l_strValue = "<error>"
            Else
                Err.Clear
                l_strValue = ToString(m_ScriptEngine.Eval(l_strConstant))
                If (Err.Number <> 0) Then l_strValue = "<error>"
            End If
            l_strText = left(l_strText, l_lngRelStartPos - 1) + l_strValue + Mid(l_strText, l_lngRelStartPos + l_lngLength + 5)
            l_lngPos = l_lngStartPos + l_lngLength + 5
            l_lngRelPos = l_lngRelStartPos + Len(l_strValue)
            l_lngStartPos = InStr(l_lngPos, l_strOldText, "<%=")
            l_lngRelStartPos = InStr(l_lngRelPos, l_strText, "<%=")
        Loop
    End If
    ParseString = l_strText
    ContextLevelRemove
End Function

Function PlaySample(ByVal Filename As String) As Channel
Attribute PlaySample.VB_Description = "Loads a sample from a file and plays it. Returns the channel it was played on."
On Error Resume Next
Dim l_sndSample As Sound
    Set l_sndSample = LoadSample(Filename)
    Set PlaySample = l_sndSample.Play
    Err.Clear
End Function

Public Function RegisterDLL(ByVal Filename As String) As Boolean
Attribute RegisterDLL.VB_Description = "Installs a COM DLL."
On Error Resume Next
    RegisterDLL = RegisterServer(Filename, True)
End Function

Public Function ReloadGame() As Boolean
Attribute ReloadGame.VB_Description = "Reloads the current game. Returns False if unsuccessful."
On Error Resume Next
    Terminating = True
End Function
'
Function RM2kSprite(ByVal Charset As String, Optional ByVal Char As Long = 0, Optional ByVal AnimDelay As Long = 6) As Fury2Sprite
Attribute RM2kSprite.VB_Description = "Creates a new sprite from an RPG Maker 200x spriteset and returns it."
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
                .AddPose 1, "Pose"
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
            End With
            With .Item(2)
                .Name = "Right"
            End With
            With .Item(3)
                .Name = "Down"
            End With
            With .Item(4)
                .Name = "Left"
            End With
        End With
    End With
End Function

Public Function SaveFlags(ByVal File As VirtualFile)
Attribute SaveFlags.VB_Description = "Saves the current Flags array to a file."
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

Public Function SaveToFile(ByVal Obj As Object, ByVal File As Object) As Boolean
Attribute SaveToFile.VB_Description = "Saves a given object to a File. If the save was successful, True is returned."
On Error Resume Next
Dim l_glbGlobals As New Fury2Globals
    SaveToFile = l_glbGlobals.SaveToFile(Obj, File, BindEvent("InternalSaveProgress", Me, "File"))
End Function

Public Sub SaveImage(ByVal Image As Fury2Image, ByVal Filename As String)
Attribute SaveImage.VB_Description = "Saves an image to a PNG file."
On Error Resume Next
    Image.SavePNG Filesystem.CreateFile(Filename).GetRealFilename
End Sub

Function ToBoolean(Value) As Boolean
Attribute ToBoolean.VB_Description = "Attempts to convert Value to a boolean and returns the result."
On Error Resume Next
    ToBoolean = CBool(Value)
End Function

Function ToInteger(Value) As Long
Attribute ToInteger.VB_Description = "Attempts to convert Value to an integer and returns the result."
On Error Resume Next
    ToInteger = CLng(Value)
End Function

Function ToNumber(Value) As Double
Attribute ToNumber.VB_Description = "Attempts to convert Value to a floating-point number and returns the result."
On Error Resume Next
    ToNumber = CDbl(Value)
End Function

Function ToString(Value, Optional ByVal QuoteStrings As Boolean = False) As String
Attribute ToString.VB_Description = "Attempts to convert Value to a string and returns the result. If QuoteStrings is specified, strings within arrays will have quotes surrounding them."
On Error Resume Next
Dim l_vtType As VariantTypeConstants
    l_vtType = VarType(Value)
    If l_vtType = vbEmpty Then
        ToString = "{Empty}"
    ElseIf l_vtType = vbNull Then
        ToString = "{Null}"
    ElseIf (l_vtType And vbArray) = vbArray Then
        If UBound(Value) > 256 Then
            ToString = "{Array}"
        Else
            If QuoteStrings Then
                ToString = "(" + JoinX(Value, ", ", QuoteStrings) + ")"
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

Function TTFont(ByVal Name As String, Optional ByVal Size As Single = 9, Optional ByVal Bold As Boolean = False, Optional ByVal Italic As Boolean = False) As StdFont
Attribute TTFont.VB_Description = "Creates a TrueType Font object that can be used to create a Fury2Font."
On Error Resume Next
    Set TTFont = New StdFont
    With TTFont
        .Name = Name
        .Size = Size
        .Bold = Bold
        .Italic = Italic
    End With
End Function

Public Function UnregisterDLL(Filename) As Boolean
Attribute UnregisterDLL.VB_Description = "Uninstalls a COM DLL."
On Error Resume Next
    UnregisterDLL = RegisterServer(CStr(Filename), False)
End Function

Function VariableName(Name) As String
Attribute VariableName.VB_Description = "Converts a string into a valid identifier that can be used as the name of a class, sub, function, or variable."
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
        m_strOut = right(m_strOut, c_lngMaxNameLength)
    End If
    VariableName = m_strOut
End Function

Public Function Which(Value As Variant, ParamArray Options() As Variant) As Long
Attribute Which.VB_Description = "Selects a value from a set of options, based on an index."
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

'Public Function LoadDictionary(ByVal Filename As String) As Fury2Dictionary
'On Error Resume Next
'Dim l_strFile As String
'Dim l_lngPosition As Long
'    If Filesystem.FileExists(Filename) Then
'        l_strFile = Filesystem.File(Filename).Text
'    End If
'End Function

Public Function YesNo(ByVal BVal As Boolean) As String
Attribute YesNo.VB_Description = "Returns either ""Yes"" or ""No"" based on the value of BVal."
On Error Resume Next
    If BVal Then
        YesNo = "Yes"
    Else
        YesNo = "No"
    End If
End Function

Public Property Set Backbuffer(ByVal NewBuffer As Fury2Image)
On Error Resume Next
    Set m_imgBackbuffer = NewBuffer
End Property

Public Property Get Backbuffer() As Fury2Image
Attribute Backbuffer.VB_Description = "The engine's backbuffer."
On Error Resume Next
    Set Backbuffer = m_imgBackbuffer
End Property

Public Property Get BGMVolume() As Single
Attribute BGMVolume.VB_Description = "Returns the current volume of the current BGM."
On Error Resume Next
    BGMVolume = CurrentBGMChannel.Volume
End Property

Public Property Get Camera() As Fury2Camera
Attribute Camera.VB_Description = "The default camera. (Equal to Cameras(1))"
On Error Resume Next
    Set Camera = Cameras(1)
End Property

Public Property Get Cameras() As Fury2Cameras
Attribute Cameras.VB_Description = "The engine's camera array. Usually contains one camera."
On Error Resume Next
    Set Cameras = mvarCameras
End Property

Public Property Get ClipboardText() As String
Attribute ClipboardText.VB_Description = "The current text of the clipboard."
On Error Resume Next
    If Clipboard.GetFormat(vbCFText) Then
        ClipboardText = Clipboard.GetText(vbCFText)
    End If
    Err.Clear
End Property

Friend Property Get DirectInput() As DirectInput8
On Error Resume Next
    Set DirectInput = m_diDirectInput
End Property

Friend Property Get DirectX() As DirectX8
On Error Resume Next
    Set DirectX = m_dxDirectX
End Property

Public Property Get EventHooks() As Collection
Attribute EventHooks.VB_Description = "The event hooks currently active."
On Error Resume Next
    Set EventHooks = m_colPluginStack
End Property

Public Property Get Flag(ByVal Index As String) As Variant
Attribute Flag.VB_Description = "An array of global game settings."
On Error Resume Next
    Flag = m_Flags(LCase(Index))
    Err.Clear
End Property

Public Property Get Flags(ByVal Index As String) As Variant
Attribute Flags.VB_Description = "An array of global game settings. Same as Flag()."
Attribute Flags.VB_MemberFlags = "40"
On Error Resume Next
    Flags = Flag(Index)
End Property

Public Property Get GFXEngine() As Object
Attribute GFXEngine.VB_Description = "The current video driver."
Attribute GFXEngine.VB_MemberFlags = "40"
On Error Resume Next
    Set GFXEngine = m_objOutputPlugin
End Property

Public Property Get Icon() As String
Attribute Icon.VB_Description = "The icon displayed in the game window's title bar."
Attribute Icon.VB_MemberFlags = "40"
On Error Resume Next
    Icon = m_strIcon
End Property

Public Property Get Images()
Attribute Images.VB_Description = "The image cache."
On Error Resume Next
    Set Images = m_Images
End Property

Public Property Get IME() As IMEEngine
Attribute IME.VB_Description = "The current IME, if any. Used for international text input support."
On Error Resume Next
    Set IME = m_imeIMEEngine
End Property

Public Property Get Joystick() As Fury2Joystick
Attribute Joystick.VB_Description = "The default joystick (same as Joysticks(1), if any joysticks are installed)."
On Error Resume Next
    Set Joystick = Joysticks(1)
End Property

Public Property Get KeyState(ByVal Key As Long) As Boolean
Attribute KeyState.VB_Description = "Returns the state of a key."
On Error Resume Next
    KeyState = Keyboard.State(Key)
End Property

Public Property Get Maps() As Fury2Maps
Attribute Maps.VB_Description = "The maps currently loaded in the engine. There is usually only one, but it is possible to have multiple maps running at once."
On Error Resume Next
    Set Maps = mvarMaps
End Property

Public Property Get Player() As Object
Attribute Player.VB_Description = "Returns the PartyMember that is leading the current Party, if any."
On Error Resume Next
    Set Player = Party.Item(Party.Leader)
End Property

Public Property Get ScriptContext() As String
Attribute ScriptContext.VB_Description = "Returns the current script execution context."
On Error Resume Next
    ScriptContext = Join(m_strScriptContext, ">")
End Property

Public Property Get ScriptEngine() As ScriptEngine
Attribute ScriptEngine.VB_Description = "The script engine."
Attribute ScriptEngine.VB_MemberFlags = "40"
On Error Resume Next
    Set ScriptEngine = m_ScriptEngine
End Property

Public Property Get SmoothScaling() As Boolean
Attribute SmoothScaling.VB_Description = "If True, sprites will be scaled using bilinear filtering."
On Error Resume Next
    SmoothScaling = m_booSmoothScaling
End Property

Public Property Get SoundEngine() As SoundEngine
Attribute SoundEngine.VB_Description = "The sound engine."
On Error Resume Next
    Set SoundEngine = m_SoundEngine
End Property

Public Property Get Time() As Double
Attribute Time.VB_Description = "The current time."
On Error Resume Next
    Time = HiTimer
End Property

Public Property Get TimerEvents() As Fury2Collection
On Error Resume Next
    Set TimerEvents = m_colTimerEvents
End Property

Public Property Get Transitions() As Fury2Transitions
Attribute Transitions.VB_Description = "The transition library. Contains various commands that can be used to perform transitions."
On Error Resume Next
    Set Transitions = m_objTransitions
End Property

Public Property Get Version() As String
Attribute Version.VB_Description = "Returns the version of the engine."
On Error Resume Next
    Version = "Fury² " + Choose(App.Major + 1, "Alpha", "Beta", "Release") + " " + Format(App.Minor, "00") + " Build " + Format(App.Revision, "00000")
End Property

Public Property Get VideoDriver() As Object
Attribute VideoDriver.VB_Description = "The current video driver."
On Error Resume Next
    Set VideoDriver = m_objVideoDriver
End Property

Public Property Let BGMVolume(ByVal Volume As Single)
Attribute BGMVolume.VB_Description = "The current volume of the current BGM."
On Error Resume Next
    CurrentBGM.Volume = Volume
    CurrentBGMChannel.Volume = Volume
    Err.Clear
End Property

Public Property Let ClipboardText(ByRef NewText As String)
Attribute ClipboardText.VB_Description = "The text currently contained in the clipboard."
On Error Resume Next
    Clipboard.Clear
    Clipboard.SetText NewText, vbCFText
    Err.Clear
End Property

Public Property Let Flag(ByVal Index As String, ByVal NewValue As Variant)
Attribute Flag.VB_Description = "An array of global game settings."
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

Public Property Let Flags(ByVal Index As String, ByVal NewValue As Variant)
Attribute Flags.VB_Description = "An array of global game settings. Same as Flag()."
On Error Resume Next
    Flag(Index) = NewValue
End Property

Public Property Let Icon(NewIcon As String)
On Error Resume Next
    m_strIcon = NewIcon
    If m_objOutputPlugin Is Nothing Then Exit Property
    If Trim(m_strIcon) = "" Or (Filesystem.FileExists(m_strIcon) = False) Then
        SetAppIcon m_objOutputPlugin.Window
    Else
        Dim l_filFile As Fury2File
        Set l_filFile = Filesystem.File(m_strIcon)
        If l_filFile Is Nothing Then Exit Property
        SetWindowIcon m_objOutputPlugin.Window, l_filFile.GetRealFilename
    End If
End Property

Public Property Let KeyState(ByVal Key As Long, ByVal Value As Boolean)
On Error Resume Next
    Keyboard.State(Key) = Value
End Property

Public Property Let ScriptContext(ByVal NewContext As String)
On Error Resume Next
    m_strScriptContext = Split(NewContext, ">")
    m_lngContextDepth = UBound(m_strScriptContext)
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

Sub AddObject(ByVal Name As String, ByVal Obj As Object)
Attribute AddObject.VB_Description = "Adds a command object to the script engine."
On Error Resume Next
    m_ScriptEngine.AddObject Name, Obj
End Sub

Private Sub AddRef(ByRef Obj As IShellFolderEx_TLB.IUnknown)
On Error Resume Next
    Obj.AddRef
End Sub

Public Sub AdjustGameClock(ByVal Amount As Double)
Attribute AdjustGameClock.VB_Description = "Adjusts the value of the game clock by a certain amount."
On Error Resume Next
    m_dblTimeOffset = m_dblTimeOffset - Amount
    ResetTiming
End Sub

Public Sub AnimatePicture(ByVal Slot As Long, Optional ByVal Length As Double = 1, Optional ByVal NewX As Long = -32767, Optional ByVal NewY As Long = -32767, Optional ByVal NewWidth As Long = -32767, Optional ByVal NewHeight As Long = -32767, Optional ByRef EndScript = "")
Attribute AnimatePicture.VB_Description = "Animates the position and size of a picture slot over time."
On Error Resume Next
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With m_picPictures(Slot)
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

Sub BeginTrace()
On Error Resume Next
    #If DebugFeatures = 1 Then
    m_booTrace = True
    #End If
End Sub

Sub BindKey(ByVal KeyCode As Long, ByVal Binding As Variant)
Attribute BindKey.VB_Description = "Attaches a default script to a key. Key bindings are executed right after the Engine_KeyDown event, but only if the event and all plugins don't block it."
On Error Resume Next
    Select Case VarType(Binding)
    Case vbString
        Set Keyboard.Binding(KeyCode) = BindEvent(Binding)
    Case vbObject
        Set Keyboard.Binding(KeyCode) = Binding
    Case Else
        DebugOut "Unable to bind key " & KeyCode & " to " & ToString(Binding, True)
    End Select
End Sub

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Fury2Engine: " & GameName
End Function

Public Sub Break(Optional ByVal Name As String = "")
Attribute Break.VB_Description = "Stops the execution of a given subthread (if no subthread name is specified, the current subthread is stopped)."
On Error Resume Next
Dim l_lngSubthreads As Long
    If Trim(Name) = "" Then
        DebugOut "Breaking subthread"
        m_booBreak = True
    Else
        For l_lngSubthreads = UBound(m_thrSubthreads) To LBound(m_thrSubthreads) Step -1
            With m_thrSubthreads(l_lngSubthreads)
                If LCase(Trim(.Name)) = LCase(Trim(Name)) Then
                    If .Running Then
                        DebugOut "Breaking subthread #" & l_lngSubthreads & " (" & .Name & ")"
                        .Running = False
                        Exit For
                    End If
                End If
            End With
        Next l_lngSubthreads
    End If
End Sub

Public Sub ChangePicture(ByVal Slot As Long, ByVal NewPic As Object)
Attribute ChangePicture.VB_Description = "Changes the picture in a picture slot."
On Error Resume Next
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With m_picPictures(Slot)
        Set .Picture = NewPic
    End With
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    InitVariables
    InitLog

    If EditMode Then
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
    AVIFileExit
    DebugOut "Destroying engine"
End Sub

Public Sub ClearConsole()
Attribute ClearConsole.VB_Description = "Clears the contents of the console."
On Error Resume Next
    ConsoleScroll = 0
    ConsoleItems.Clear
    Err.Clear
End Sub

Friend Sub ConsoleBackspace()
On Error Resume Next
Dim l_strLeft As String, l_strRight As String
    l_strLeft = Mid(ConsoleText, 1, m_lngConsoleCaretPosition - 2)
    l_strRight = Mid(ConsoleText, m_lngConsoleCaretPosition)
    ConsoleText = l_strLeft + l_strRight
    m_lngConsoleCaretPosition = ClipValue(m_lngConsoleCaretPosition - 1, 1, Len(ConsoleText) + 1)
End Sub

Public Sub ConsoleCommand(ByVal Command As String)
Attribute ConsoleCommand.VB_Description = "Executes a command as if it was entered into the console."
On Error Resume Next
    If left(Trim(Command), 1) = "?" Then
        Err.Clear
        ScriptEngine.AddCode ScriptEngine.Language.GenerateSubCall("TextOut", Mid(Command, 2))
    Else
        Err.Clear
        ScriptEngine.AddCode Command
    End If
End Sub

Friend Sub ConsoleDelete()
On Error Resume Next
Dim l_strLeft As String, l_strRight As String
    l_strLeft = Mid(ConsoleText, 1, m_lngConsoleCaretPosition - 1)
    l_strRight = Mid(ConsoleText, m_lngConsoleCaretPosition + 1)
    ConsoleText = l_strLeft + l_strRight
End Sub

Friend Sub ConsoleInsert(ByVal Text As String)
On Error Resume Next
Dim l_strLeft As String, l_strRight As String
    If m_lngConsoleCaretPosition < 1 Then m_lngConsoleCaretPosition = 1
    l_strLeft = Mid(ConsoleText, 1, m_lngConsoleCaretPosition - 1)
    l_strRight = Mid(ConsoleText, m_lngConsoleCaretPosition)
    ConsoleText = l_strLeft + Text + l_strRight
    m_lngConsoleCaretPosition = ClipValue(m_lngConsoleCaretPosition + Len(Text), 1, Len(ConsoleText) + 1)
    Err.Clear
End Sub

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
Attribute ControlActivate.VB_Description = "Simulates activation of an InputControl."
On Error Resume Next
Dim Obj As Object
Dim pObj As Fury2Plugin
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
        Set pObj = Nothing
        Set pObj = Obj
        Err.Clear
        If pObj Is Nothing Then
            Obj.ControlActivate Control
        Else
            pObj.ControlActivate Control
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
Attribute ControlDeactivate.VB_Description = "Simulates deactivation of an InputControl."
On Error Resume Next
Dim Obj As Object
Dim pObj As Fury2Plugin
    If Control.State Then ContextLevelRemove: Exit Sub
    If Control.Handled Then
        Debug.Print "Recieved already handled input event: " & Control.Name & " Deactivated"
        Exit Sub
    End If
    Control.Handled = True
    ContextLevelAdd "ControlDeactivate"
    Control.Consumed = False
    For Each Obj In m_colPluginStack
        Set pObj = Nothing
        Set pObj = Obj
        If pObj Is Nothing Then
            Obj.ControlDeactivate Control
        Else
            pObj.ControlDeactivate Control
        End If
        If Control.Consumed Then Control.Consumed = False: ContextLevelRemove: Exit Sub
    Next Obj
    Err.Clear
    m_ScriptEngine.Exec "Engine_ControlDeactivate", Control
    Err.Clear
    ContextLevelRemove
End Sub

Public Sub CriticalError(Source As String, Location As String, Optional Description As String = "Unknown Error")
Attribute CriticalError.VB_Description = "Displays a critical error message and halts the engine."
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
    Open App.Path + IIf(right(App.Path, 1) = "\", "", "\") + "error.wtf" For Append As #m_lngHandle
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

Public Sub DebugBreak()
On Error Resume Next
    Stop
End Sub

Public Sub DebugOut(ByRef Text)
Attribute DebugOut.VB_Description = "Logs text to the debugger, if one is attached to the game."
On Error Resume Next
Dim l_strCBoard As String
Dim l_strText As String
    l_strText = CStr(Text)
    If Debugger Is Nothing Then Else Debugger.DebugOut l_strText
    LogPrint l_strText
    If Not LogDebugMessages Then Exit Sub
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
    #End If
End Sub

Sub DefocusWindow()
Attribute DefocusWindow.VB_MemberFlags = "40"
On Error Resume Next
    SetTopmost m_objOutputPlugin.Window, False
    Err.Clear
End Sub

Friend Sub DeInitENet()
On Error Resume Next
Dim l_hosHost As VBENetHost
    If m_booENetInitialized Then
        For Each l_hosHost In Hosts
            l_hosHost.DisconnectAll
            l_hosHost.Flush
            l_hosHost.Free
        Next l_hosHost
        enet_deinitialize
        m_booENetInitialized = False
    End If
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
    Set Timers = F2Collection()
End Sub

Public Sub DestroyTimer(ByRef ID)
Attribute DestroyTimer.VB_Description = "Destroys a timer."
On Error Resume Next
    If VarType(ID) = vbString Then
        Timers.Remove CStr(ID)
    ElseIf TypeOf ID Is Fury2Timer Then
        Timers.Remove Timers.Find(ID)
    Else
        Timers.Remove CLng(ID)
    End If
    Err.Clear
End Sub

Public Sub DisableAllHotspots()
Attribute DisableAllHotspots.VB_Description = "Disables all hotspots."
On Error Resume Next
Dim m_lngIndex As Long
    For m_lngIndex = 0 To c_lngMaxHotspot
        m_hsHotspots(m_lngIndex).Enabled = False
    Next m_lngIndex
    Err.Clear
End Sub

Public Sub DisableHotspot(ByVal Index As Long)
Attribute DisableHotspot.VB_Description = "Disables a hotspot."
On Error Resume Next
    With m_hsHotspots(Index)
        .Enabled = False
    End With
    Err.Clear
End Sub

Public Sub DisableIME()
Attribute DisableIME.VB_Description = "Disables the IME."
On Error Resume Next
    m_imeIMEEngine.Enabled = False
End Sub

Public Sub EmptyImageCache()
Attribute EmptyImageCache.VB_Description = "Empties out the image cache."
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

Public Sub EnableHotspot(ByVal Index As Long)
Attribute EnableHotspot.VB_Description = "Enables a hotspot."
On Error Resume Next
    With m_hsHotspots(Index)
        .Enabled = True
    End With
    Err.Clear
End Sub

Public Sub EnableIME()
Attribute EnableIME.VB_Description = "Enables the IME."
On Error Resume Next
    m_imeIMEEngine.Enabled = True
End Sub

Sub EndTrace()
    #If DebugFeatures = 1 Then
    m_booTrace = False
    #End If
End Sub

Friend Sub EngineLoop()
On Error Resume Next
    If Not InIDE Then
        m_objOutputPlugin.Window.SetFocus
    End If
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

Public Sub ErrorMessage(ByVal Error As String, ByVal Details As String)
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

Private Sub Event_KeyDown(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object, m_lngKeyCode As Long
Dim pObj As Fury2Plugin
    If ConsoleVisible Then Exit Sub
    If Not Keyboard.Enabled Then Exit Sub
    ContextLevelAdd "KeyDown"
    m_lngKeyCode = CLng(Evt.LParam1)
    For Each Obj In m_colPluginStack
        Set pObj = Nothing
        Set pObj = Obj
        If pObj Is Nothing Then
            Obj.KeyDown m_lngKeyCode, CLng(Evt.LParam2)
        Else
            pObj.KeyDown m_lngKeyCode, CLng(Evt.LParam2)
        End If
        If m_lngKeyCode = 0 Then ContextLevelRemove: Exit Sub
    Next Obj
    Err.Clear
    m_ScriptEngine.Exec "Engine_KeyDown", m_lngKeyCode, CLng(Evt.LParam2)
    If m_lngKeyCode = 0 Then ContextLevelRemove: Exit Sub
    If (Not ConsoleVisible) And (Not TrapKeys) Then
        If Evt.LParam3 = 1 Then
            ' Repeated press
            Keyboard.KeyDown m_lngKeyCode, True
        Else
            ' First press
            Keyboard.KeyDown m_lngKeyCode, True
        End If
'            If VarType(KeyBindings(m_lngKeyCode)) = vbString Then
'                Err.Clear
'                If BindState(m_lngKeyCode) Then
'                Else
'                    BindState(m_lngKeyCode) = True
'                    If Not ConsoleVisible Then m_ScriptEngine.AddCode ToString(KeyBindings(m_lngKeyCode))
'                    BindState(m_lngKeyCode) = False
'                End If
'                Err.Clear
'            Else
'                If TypeOf KeyBindings(m_lngKeyCode) Is Fury2Event Then
'                    KeyBindings(m_lngKeyCode).Invoke
'                ElseIf TypeOf KeyBindings(m_lngKeyCode) Is Fury2InputControl Then
'                    If KeyBindings(m_lngKeyCode).State Then
'                        KeyBindings(m_lngKeyCode).Deactivate
'                    End If
'                    KeyBindings(m_lngKeyCode).Activate
'                End If
'            End If
    End If
    ContextLevelRemove
End Sub

Private Sub Event_KeyUp(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object, m_lngKeyCode As Long
    ContextLevelAdd "KeyUp"
    m_lngKeyCode = CLng(Evt.LParam1)
    Keyboard.KeyUp m_lngKeyCode
    Dim pObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set pObj = Nothing
        Set pObj = Obj
        If pObj Is Nothing Then
            Obj.KeyUp m_lngKeyCode, CLng(Evt.LParam2)
        Else
            pObj.KeyUp m_lngKeyCode, CLng(Evt.LParam2)
        End If
        If m_lngKeyCode = 0 Then ContextLevelRemove: Exit Sub
    Next Obj
    m_ScriptEngine.Exec "Engine_KeyUp", CLng(Evt.LParam1), CLng(Evt.LParam2)
    ContextLevelRemove
End Sub

Private Sub Event_MouseDown(Evt As Fury2QueueEvent)
On Error Resume Next
Dim Obj As Object
Dim m_lngX As Long, m_lngY As Long
Dim m_varX As Variant, m_varY As Variant
Dim m_colObjects As Collection, m_lngObjects As Long
Dim l_lngHotspots As Long, l_booInside As Boolean
    If Not Mouse.Enabled Then Exit Sub
    Set m_colObjects = New Collection
    ContextLevelAdd "MouseDown"
    m_lngX = Evt.LParam3
    m_lngY = Evt.LParam4
    m_varX = Evt.LParam3
    m_varY = Evt.LParam4
    Dim pObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set pObj = Nothing
        Set pObj = Obj
        If pObj Is Nothing Then
            Obj.MouseDown Evt.LParam1, Evt.LParam2, m_varX, m_varY
            If m_varX = -32767 Or m_varY = -32767 Then ContextLevelRemove: Exit Sub
        Else
            pObj.MouseDown Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
            If m_lngX = -32767 Or m_lngY = -32767 Then ContextLevelRemove: Exit Sub
        End If
    Next Obj
    For l_lngHotspots = LBound(m_hsHotspots) To UBound(m_hsHotspots)
        With m_hsHotspots(l_lngHotspots)
            If .Enabled Then
                l_booInside = m_lngX >= .x1 And m_lngX < .x2 And m_lngY >= .y1 And m_lngY < .y2
                If .Hovering Then
                    If l_booInside Then
                        If Not (.ClickEvent Is Nothing) Then .ClickEvent.Invoke
                    Else
                        .Hovering = False
                        If Not (.LeaveEvent Is Nothing) Then .LeaveEvent.Invoke
                    End If
                Else
                    If l_booInside Then
                        If Not (.EnterEvent Is Nothing) Then .EnterEvent.Invoke
                        .Hovering = True
                        If Not (.ClickEvent Is Nothing) Then .ClickEvent.Invoke
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
Dim m_varX As Variant, m_varY As Variant
Dim l_lngHotspots As Long, l_booInside As Boolean
    If Not Mouse.Enabled Then Exit Sub
    ContextLevelAdd "MouseMove"
    m_lngX = Evt.LParam3
    m_lngY = Evt.LParam4
    m_varX = Evt.LParam3
    m_varY = Evt.LParam4
    Dim pObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set pObj = Nothing
        Set pObj = Obj
        If pObj Is Nothing Then
            Obj.MouseMove Evt.LParam1, Evt.LParam2, m_varX, m_varY
            If m_varX = -32767 Or m_varY = -32767 Then ContextLevelRemove: Exit Sub
        Else
            pObj.MouseMove Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        End If
        If m_lngX = -32767 Or m_lngY = -32767 Then ContextLevelRemove: Exit Sub
    Next Obj
    For l_lngHotspots = LBound(m_hsHotspots) To UBound(m_hsHotspots)
        With m_hsHotspots(l_lngHotspots)
            If .Enabled Then
                l_booInside = m_lngX >= .x1 And m_lngX < .x2 And m_lngY >= .y1 And m_lngY < .y2
                If .Hovering Then
                    If l_booInside Then
                    Else
                        .Hovering = False
                        If Not (.LeaveEvent Is Nothing) Then .LeaveEvent.Invoke
                    End If
                Else
                    If l_booInside Then
                        If Not (.EnterEvent Is Nothing) Then .EnterEvent.Invoke
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
Dim m_varX As Variant, m_varY As Variant
Dim m_colObjects As Collection, m_lngObjects As Long
Dim m_lngHotspots As Long
    If Not Mouse.Enabled Then Exit Sub
    Set m_colObjects = New Collection
    ContextLevelAdd "MouseUp"
    m_lngX = Evt.LParam3
    m_lngY = Evt.LParam4
    m_varX = Evt.LParam3
    m_varY = Evt.LParam4
    Dim pObj As Fury2Plugin
    For Each Obj In m_colPluginStack
        Set pObj = Nothing
        Set pObj = Obj
        If pObj Is Nothing Then
            Obj.MouseUp Evt.LParam1, Evt.LParam2, m_varX, m_varY
            If m_varX = -32767 Or m_varY = -32767 Then ContextLevelRemove: Exit Sub
        Else
            pObj.MouseUp Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
        End If
        If m_lngX = -32767 Or m_lngY = -32767 Then ContextLevelRemove: Exit Sub
    Next Obj
    If Not (MouseUpHook Is Nothing) Then
        MouseUpHook.Invoke Evt.LParam1, Evt.LParam2, m_lngX, m_lngY
    End If
    ContextLevelRemove
End Sub

Public Sub FadePicture(ByVal Slot As Long, Optional ByVal StartAlpha As Single = -1, Optional ByVal EndAlpha As Single = -1, Optional ByVal Speed As Double = 3.33333317812224E-02, Optional ByRef EndScript As String = "")
Attribute FadePicture.VB_Description = "Fades a picture slot in or out."
On Error Resume Next
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With m_picPictures(Slot)
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

Public Sub FlipScreen()
Attribute FlipScreen.VB_Description = "Forces the engine to copy the backbuffer to the screen."
On Error Resume Next
    If Not (Running) Then Exit Sub
    If m_objOutputPlugin Is Nothing Then Exit Sub
    If m_objOutputPlugin.Closed Then Fury2VideoEventSink_Closed
    If m_objOutputPlugin.Window.WindowState = 1 Then
    Else
        If Running Then
            ' m_objOutputPlugin.VSync = VSync
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
Attribute FlushImageCache.VB_Description = "Removes unused images from the image cache."
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown, m_lngCount As Long
Dim m_imgObj As Fury2Image, m_lngObj As Long
Dim m_tsObj As Fury2Tileset
Dim l_lngImagesFlushed As Long, l_lngTilesetsFlushed As Long
    If m_Images.Count > 0 Then
        m_lngObj = 1
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
                    m_imgObj.Deallocate
                    Set m_imgObj = Nothing
                    m_Images.Remove m_lngObj
                    l_lngImagesFlushed = l_lngImagesFlushed + 1
                Else
                    m_lngObj = m_lngObj + 1
                End If
            End If
        Loop
        If l_lngImagesFlushed > 0 Then DebugOut "Flushed " & l_lngImagesFlushed & " cached image(s)"
    End If
    If m_Tilesets.Count > 0 Then
        m_lngObj = 1
        Do While m_lngObj < m_Tilesets.Count
            Set m_objObj = Nothing
            Set m_objObj = m_Tilesets(m_lngObj)
            If m_objObj Is Nothing Then
                m_lngObj = m_lngObj + 1
            Else
                m_objObj.AddRef
                m_lngCount = m_objObj.Release
                Set m_objObj = Nothing
                If m_lngCount <= 2 Then
                    Set m_tsObj = m_Tilesets(m_lngObj)
                    m_tsObj.Free
                    Set m_tsObj = Nothing
                    m_Tilesets.Remove m_lngObj
                    l_lngTilesetsFlushed = l_lngTilesetsFlushed + 1
                Else
                    m_lngObj = m_lngObj + 1
                End If
            End If
        Loop
        If l_lngTilesetsFlushed > 0 Then DebugOut "Flushed " & l_lngTilesetsFlushed & " cached tileset(s)"
    End If
End Sub

Sub FocusWindow()
Attribute FocusWindow.VB_MemberFlags = "40"
On Error Resume Next
    SetTopmost m_objOutputPlugin.Window, m_objOutputPlugin.Fullscreen
    If Not InIDE Then
        m_objOutputPlugin.Window.SetFocus
    End If
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

Public Sub Fury2VideoEventSink_Closed()
Attribute Fury2VideoEventSink_Closed.VB_MemberFlags = "40"
On Error Resume Next
    ShutdownIME
    If Running = True Then
        Running = False
    End If
    Terminating = True
    TerminateEngine = True
End Sub

Public Sub Fury2VideoEventSink_DblClick()
On Error Resume Next
    If Not (MouseDblClickHook Is Nothing) Then
        MouseDblClickHook.Invoke g_lngMouseX, g_lngMouseY
    End If
End Sub

Public Sub Fury2VideoEventSink_GotFocus()
On Error Resume Next
    If Not GotFocusHook Is Nothing Then
        GotFocusHook.Invoke
    End If
End Sub

Private Sub Fury2VideoEventSink_KeyDown(ByVal KeyCode As Integer, ByVal Shift As Integer)
On Error Resume Next
    Fury2VideoEventSink_KeyDown2 KeyCode, Shift
End Sub

Public Sub Fury2VideoEventSink_KeyDown2(ByVal KeyCode As Integer, ByVal Shift As Integer, Optional ByVal Force As Boolean = False)
Attribute Fury2VideoEventSink_KeyDown2.VB_MemberFlags = "40"
On Error Resume Next
Dim l_lngType As Long
Dim l_evtBinding As Fury2Event, l_booActive As Boolean
    Select Case KeyCode
    Case 192 ' Tilde
        If Shift = 1 Then
            If m_objOutputPlugin.Fullscreen Then
            Else
                TraceOpen
            End If
        Else
            If ConsoleEnabled Then
                If ConsoleVisible Then
                    HideConsole
                Else
                    ShowConsole
                End If
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
    Case vbKeyDelete
        If ConsoleVisible Then
            ConsoleDelete
        End If
    Case 13
        If Shift = 1 Then ' Shift
            If ConsoleVisible Then
                ConsoleInsert vbCrLf
            Else
                KeysEntered = KeysEntered + vbCrLf
            End If
            KeyCode = 0
        Else
            If ConsoleVisible Then
                m_booErrorOccurred = False
                ContextLevelAdd "Console"
                ConsoleCommand ConsoleText
                ContextLevelRemove
                If (Err.Number = 0) And (Not m_booErrorOccurred) Then
                    ConsoleText = ""
                    m_lngConsoleCaretPosition = 1
                Else
                    Err.Clear
                End If
            End If
        End If
    Case vbKeyBack
        If Shift = 2 Then ' Ctrl
            If ConsoleVisible Then
                ConsoleText = ""
                m_lngConsoleCaretPosition = 1
            Else
                KeysEntered = ""
            End If
        End If
    Case vbKeyUp
        If ConsoleVisible Then
            ConsoleScroll = ConsoleScroll + 1
            If ConsoleScroll > ConsoleItems.Count Then ConsoleScroll = ConsoleItems.Count
        End If
    Case vbKeyDown
        If ConsoleVisible Then
            ConsoleScroll = ConsoleScroll - 1
            If ConsoleScroll < 0 Then ConsoleScroll = 0
        End If
    Case vbKeyHome
        If ConsoleVisible Then
            m_lngConsoleCaretPosition = 1
        End If
    Case vbKeyEnd
        If ConsoleVisible Then
            m_lngConsoleCaretPosition = Len(ConsoleText) + 1
        End If
    Case vbKeyLeft
        If ConsoleVisible Then
            m_lngConsoleCaretPosition = m_lngConsoleCaretPosition - 1
            If m_lngConsoleCaretPosition < 1 Then
                m_lngConsoleCaretPosition = 1
            End If
        End If
    Case vbKeyRight
        If ConsoleVisible Then
            m_lngConsoleCaretPosition = m_lngConsoleCaretPosition + 1
            If m_lngConsoleCaretPosition > Len(ConsoleText) + 1 Then m_lngConsoleCaretPosition = Len(ConsoleText) + 1
        End If
    Case Else
    End Select
    If (DisableUserInput) And (Not Force) Then Exit Sub
    If Keyboard.State(KeyCode) = True Then
        l_lngType = 1 ' Repeated key
    End If
    Set l_evtBinding = Keyboard.Binding(KeyCode)
    If l_evtBinding Is Nothing Then
        l_booActive = False
    Else
        l_booActive = (l_evtBinding.Blocked) And (l_evtBinding.Blocking)
    End If
    If (l_booActive) Then
    Else
        QueueEvent 0, , , CLng(KeyCode), CLng(Shift), l_lngType
    End If
End Sub

Private Sub Fury2VideoEventSink_KeyPress(ByVal KeyAscii As Long)
On Error Resume Next
    Fury2VideoEventSink_KeyPress2 KeyAscii
End Sub

Public Sub Fury2VideoEventSink_KeyPress2(ByVal KeyAscii As Long, Optional ByVal Force As Boolean = False)
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
        l_strInput = ChrW(KeyAscii)
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
        ElseIf TrapKeys Then
            ContextLevelAdd "Engine_KeyPress"
            ScriptEngine.Exec "Engine_KeyPress", KeyAscii
            ContextLevelRemove
        End If
    ElseIf KeyAscii = 8 Then
        If ConsoleVisible Then
            ConsoleBackspace
        ElseIf TrapKeys Then
            l_strBuffer = left(l_strBuffer, Len(l_strBuffer) - 1)
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
        Dim pObj As Fury2Plugin
        m_lngKeyAscii = KeyAscii
        For Each Obj In m_colReversePluginStack
            If Not (Obj Is Nothing) Then
                Set pObj = Nothing
                Set pObj = Obj
                If pObj Is Nothing Then
                    Obj.KeyPress m_lngKeyAscii
                Else
                    pObj.KeyPress m_lngKeyAscii
                End If
                If m_lngKeyAscii = 0 Then: Exit Sub
            End If
        Next Obj
        Err.Clear
    End If
End Sub

Private Sub Fury2VideoEventSink_KeyUp(ByVal KeyCode As Integer, ByVal Shift As Integer)
On Error Resume Next
    Fury2VideoEventSink_KeyUp2 KeyCode, Shift
End Sub

Public Sub Fury2VideoEventSink_KeyUp2(ByVal KeyCode As Integer, ByVal Shift As Integer, Optional ByVal Force As Boolean = False)
Attribute Fury2VideoEventSink_KeyUp2.VB_MemberFlags = "40"
On Error Resume Next
    If DisableUserInput Then Exit Sub
    QueueEvent 1, , , CLng(KeyCode), CLng(Shift)
End Sub

Public Sub Fury2VideoEventSink_LostFocus()
On Error Resume Next
    If Not LostFocusHook Is Nothing Then
        LostFocusHook.Invoke
    End If
End Sub

Private Sub Fury2VideoEventSink_MouseDown(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    Fury2VideoEventSink_MouseDown2 Button, Shift, X, Y
End Sub

Public Sub Fury2VideoEventSink_MouseDown2(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single, Optional ByVal Force As Boolean = False)
Attribute Fury2VideoEventSink_MouseDown2.VB_MemberFlags = "40"
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

Private Sub Fury2VideoEventSink_MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    Fury2VideoEventSink_MouseMove2 Button, Shift, X, Y
End Sub

Public Sub Fury2VideoEventSink_MouseMove2(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single, Optional ByVal Force As Boolean = False)
Attribute Fury2VideoEventSink_MouseMove2.VB_MemberFlags = "40"
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

Private Sub Fury2VideoEventSink_MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    Fury2VideoEventSink_MouseUp2 Button, Shift, X, Y
End Sub

Public Sub Fury2VideoEventSink_MouseUp2(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single, Optional ByVal Force As Boolean = False)
Attribute Fury2VideoEventSink_MouseUp2.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
    If (DisableUserInput) And (Not Force) Then Exit Sub
    m_lngX = (X) / (m_objOutputPlugin.Window.ScaleWidth / ScreenWidth)
    m_lngY = (Y) / (m_objOutputPlugin.Window.ScaleHeight / ScreenHeight)
    g_lngMouseX = m_lngX
    g_lngMouseY = m_lngY
    g_lngMouseButtons = g_lngMouseButtons Xor Button
    QueueEvent 4, X, Y, CLng(Button), CLng(Shift), m_lngX, m_lngY
End Sub

Public Sub Fury2VideoEventSink_Paint()
Attribute Fury2VideoEventSink_Paint.VB_MemberFlags = "40"
On Error Resume Next
End Sub

Private Sub Fury2VideoEventSink_Resize()
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
    ReDim Preserve m_thrSubthreads(0 To SubthreadCount)
    l_lngThreadID = SubthreadCount
    With m_thrSubthreads(l_lngThreadID)
        .Name = Name
        .Running = True
    End With
    If m_sngLastFrameStart = 0 Then m_sngLastFrameStart = HiTimer
    If m_sngLastFrameEnd = 0 Then m_sngLastFrameEnd = HiTimer
    If m_sngLastSecond = 0 Then m_sngLastSecond = HiTimer
    UpdateMaxLength = 0.8 / DesiredFramerate
    If l_lngThreadID = 0 Then
        ContextLevelAdd "Game"
    Else
        ContextLevelAdd "Subthread #" & l_lngThreadID
    End If
    If Not m_booRanStartEvent Then
        m_booRanStartEvent = True
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
        If m_thrSubthreads(l_lngThreadID).Running = False Then
            m_booResetClock = True
            Exit Do
        End If
        If Terminating Then Exit Do
        m_sngLastFrameStart = FrameStart
        FrameStart = HiTimer
        m_sngFrameLength = 1# / CDbl(DesiredFramerate)
        NextFrame = m_sngLastFrameStart + m_sngFrameLength
        Elapsed = Elapsed + (FrameStart - m_sngLastFrameStart)
        If Elapsed < 0 Then Elapsed = 0
        If BalanceFramerate Then
            FramesElapsed = FramesElapsed + Floor((Elapsed) / m_sngFrameLength)
            If m_booSubthreadStarted Then
                FramesElapsed = 1
            End If
            If FramesElapsed > MaxFrameskip Then FramesElapsed = MaxFrameskip
            Elapsed = Elapsed - ((Floor((Elapsed) / m_sngFrameLength)) * m_sngFrameLength)
            If Elapsed < 0 Then Elapsed = 0
        Else
            FramesElapsed = 1
            Elapsed = 0
        End If
        If m_booResetClock Then
            m_booResetClock = False
            m_sngLastSecond = HiTimer
            m_lngFPSAccumulator = 0
            FramesElapsed = 1
            Elapsed = 0
        End If

        If FramesElapsed >= 1 Then

            m_booSubthreadStarted = False
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
                UpdateENet
                If Terminating Then Exit Do
                UpdateEnd = HiTimer
                If (UpdateEnd - UpdateStart) > UpdateMaxLength Then
                    UpdateOverflowCount = UpdateOverflowCount + 1
                End If
                UpdatePictures
                If Terminating Then Exit Do
                If Not (Debugger Is Nothing) Then Debugger.DebugFrameEnd
                If m_booSubthreadStarted Then Exit Do
            Loop

            If Terminating Then Exit Do
            UpdateTimers
            If Terminating Then Exit Do
            Mouse.Update
            UpdateEvents
            UpdateKeyboard
            If Terminating Then Exit Do
            UpdateWindow
            If Terminating Then Exit Do

            Redraw
            If Terminating Then Exit Do

            FlipScreen
            If Terminating Then Exit Do

            UpdateActionQueue
            If Terminating Then Exit Do

            m_lngFPSAccumulator = m_lngFPSAccumulator + 1

        End If

        FrameEnd = HiTimer

        If BalanceFramerate And (UpdateOverflowCount > DesiredFramerate) Then
            BalanceFramerate = False
            TextOut "Framerate balancing auto-disabled (game too slow)"
        End If

        If (FrameEnd - m_sngLastSecond) >= 1# Then
            UpdateOverflowCount = 0
            frmProfile.ProfileTextHeight = ProfileTextCurrentHeight
            frmProfile.Form_Resize
            m_sngLastSecond = FrameEnd
            FPS = m_lngFPSAccumulator
            m_lngFPSAccumulator = 0
            If ShowFPS Then
                If Fullscreen Then
                    SetWindowText m_objOutputPlugin.Window.hWnd, WindowCaption & " (" & FPS & " FPS)"
                Else
                    m_objOutputPlugin.Window.Caption = WindowCaption & " (" & FPS & " FPS)"
                End If
            End If
        End If

        DoEvents
        If Me.HarassCPU Then Else SleepEx 1, True

        m_sngLastFrameEnd = FrameEnd
    Loop
    SubthreadCount = SubthreadCount - 1
    ReDim Preserve m_thrSubthreads(0 To SubthreadCount)
    m_booBreak = False
    ContextLevelRemove
    If Not (Debugger Is Nothing) Then Debugger.DebugSubthreadEnd
    Err.Clear
End Sub

Sub GoFullscreen()
Attribute GoFullscreen.VB_Description = "Attempts to switch to fullscreen mode."
On Error Resume Next
    Err.Clear
    Transitions.FadeOut 1
    Fullscreen = True
    m_objOutputPlugin.GoFullscreen
    Transitions.FadeIn 1
End Sub

Sub GoWindowed()
Attribute GoWindowed.VB_Description = "Attempts to switch to windowed mode."
On Error Resume Next
    Err.Clear
    Transitions.FadeOut 1
    Fullscreen = False
    m_objOutputPlugin.GoWindowed
    Transitions.FadeIn 1
End Sub

Friend Sub GraphicDestroyed(ByRef Obj As Fury2Graphic)
On Error Resume Next
'    Graphics.Remove Graphics.Find(Obj)
End Sub

Public Sub HideAllPictures()
Attribute HideAllPictures.VB_Description = "Hides all active picture slots."
On Error Resume Next
Dim m_lngSlot As Long
    ActivePictureSlots = 0
    For m_lngSlot = 0 To c_lngMaxPictureSlot
        With m_picPictures(m_lngSlot)
            .Visible = False
            Set .Picture = Nothing
            .EndFadeScript = ""
            .Fading = False
        End With
    Next m_lngSlot
    FlushImageCache
End Sub

Sub HideConsole()
Attribute HideConsole.VB_Description = "Hides the console, if it's open."
On Error Resume Next
    #If DebugFeatures = 1 Then
        m_sngConsoleStartOffset = m_sngConsoleOffset
        m_sngConsoleEndOffset = ConsoleHeight
        m_sngConsoleStartTime = HiTimer
        m_sngConsoleEndTime = HiTimer + 0.33
        m_booConsoleSliding = True
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
    With m_picPictures(Slot)
        .Visible = False
        Set .Picture = Nothing
        .EndFadeScript = ""
        .Fading = False
    End With
    For m_lngSlot = 0 To c_lngMaxPictureSlot
        With m_picPictures(m_lngSlot)
            If .Visible Then ActivePictureSlots = m_lngSlot + 1
        End With
    Next m_lngSlot
    Err.Clear
End Sub

Public Sub HookEvent(ByVal Name As String, ByVal EventHook)
Attribute HookEvent.VB_Description = "Attaches an engine event to a script function or Fury2Event."
On Error Resume Next
    Select Case LCase(Trim(Name))
    Case "redraw"
        Set RedrawHook = BindEvent(EventHook)
    Case "update"
        Set UpdateHook = BindEvent(EventHook)
    Case "lostfocus"
        Set LostFocusHook = BindEvent(EventHook)
    Case "gotfocus"
        Set GotFocusHook = BindEvent(EventHook)
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

Public Sub Include(ByVal Filename As String, Optional ByVal Require As Boolean = True)
Attribute Include.VB_Description = "Loads a script file and runs it. If Require is True, a failed Include will halt the engine."
On Error Resume Next
Dim l_strText As String
Dim l_strModule As String
    Err.Clear
    If Filesystem.FileExists(Filename + ".f2script") Then
        Filename = Filename + ".f2script"
    ElseIf Filesystem.FileExists(Filename + ".f2sf") Then
        Filename = Filename + ".f2sf"
    ElseIf Filesystem.FileExists(Filename + ".f2cfg") Then
        Filename = Filename + ".f2cfg"
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

Public Sub InitDefaultControls()
Attribute InitDefaultControls.VB_Description = "Initializes the default control bindings (arrow keys to move, A to select, S to cancel)"
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
End Sub

Friend Sub InitDirectInput()
On Error Resume Next
    Set m_dxDirectX = New DirectX8
    Set m_diDirectInput = m_dxDirectX.DirectInputCreate()
End Sub

Friend Sub InitENet()
On Error Resume Next
Dim l_lngResult As Long
    m_booENetInitialized = False
    Err.Clear
    l_lngResult = -1
    l_lngResult = enet_initialize()
    If l_lngResult = 0 Then
        m_booENetInitialized = True
    Else
        m_booENetInitialized = False
    End If
End Sub

Friend Sub InitIME()
On Error Resume Next
    Set m_imeIMEEngine = New IMEEngine
    #If EnableIME = 1 Then
        m_imeIMEEngine.Hook m_objOutputPlugin.Window.hWnd
    #End If
End Sub

Friend Sub InitInput()
On Error Resume Next
    If EditMode Then Exit Sub
    InitDirectInput
    InitDefaultControls
    InitKeyboard
    InitJoysticks
End Sub

Friend Sub InitJoysticks()
On Error Resume Next
Dim l_deEnum As DirectInputEnumDevices8
Dim l_diDevice As DirectInputDeviceInstance8
Dim l_devDevice As DirectInputDevice8
Dim l_lngDevices As Long
Dim l_joyJoystick As Fury2Joystick
    Joysticks.Clear
    If DirectInput Is Nothing Then
        TextOut "DirectInput 8 not available. Joysticks disabled."
        Exit Sub
    End If
    Set l_deEnum = DirectInput.GetDIDevices(DI8DEVTYPE_JOYSTICK, DIEDFL_ATTACHEDONLY)
    TextOut "Detected #[C0C0FF]" & l_deEnum.GetCount & "#[] joysticks."
    For l_lngDevices = 1 To l_deEnum.GetCount
        Set l_diDevice = l_deEnum.GetItem(l_lngDevices)
        Set l_devDevice = DirectInput.CreateDevice(l_diDevice.GetGuidInstance)
        Set l_joyJoystick = New Fury2Joystick
        l_joyJoystick.Acquire l_devDevice
        Joysticks.Add l_joyJoystick
    Next l_lngDevices
    If Joysticks.Count > 0 Then
        Set l_joyJoystick = Joysticks(1)
        With l_joyJoystick
            .Bindings.Clear
            With .AddBinding("Up", Controls("Up"))
                .BindType = BT_AxisNegative
                .BindIndex = AX_Y
            End With
            With .AddBinding("Down", Controls("Down"))
                .BindType = BT_AxisPositive
                .BindIndex = AX_Y
            End With
            With .AddBinding("Left", Controls("Left"))
                .BindType = BT_AxisNegative
                .BindIndex = AX_X
            End With
            With .AddBinding("Right", Controls("Right"))
                .BindType = BT_AxisPositive
                .BindIndex = AX_X
            End With
            With .AddBinding("OK", Controls("OK"))
                .BindType = BT_Button
                .BindIndex = 1
            End With
            With .AddBinding("Cancel", Controls("Cancel"))
                .BindType = BT_Button
                .BindIndex = 2
            End With
        End With
    End If
End Sub

Friend Sub InitKeyboard()
On Error Resume Next
    BindKey vbKeyA, Controls("OK")
    BindKey vbKeyS, Controls("Cancel")
    BindKey vbKeyLeft, Controls("Left")
    BindKey vbKeyUp, Controls("Up")
    BindKey vbKeyRight, Controls("Right")
    BindKey vbKeyDown, Controls("Down")
    BindKey vbKeyEscape, Controls("Quit")
End Sub

Friend Sub InitLog()
On Error Resume Next
Dim l_lngHandle As Long
    If LogToFile Then
        l_lngHandle = FreeFile
        Open "fury2.log" For Append As #l_lngHandle
            Print #l_lngHandle, String(64, "-")
            Print #l_lngHandle, Version & " loaded at " & Now
        Close #l_lngHandle
    End If
End Sub

Friend Function InitScriptEngine() As String
On Error Resume Next
Dim l_varTest As Variant
    Set m_ScriptEngine = New ScriptEngine
    If m_ScriptEngine Is Nothing Then
        InitScriptEngine = "Unable to create object. " & Err.Description
        Exit Function
    End If
    Set m_ScriptEngine.Language = m_ScriptEngine.LoadLanguage(ScriptLanguage)
    Load frmScriptSink
    frmScriptSink.Visible = False
    m_ScriptEngine.Init frmScriptSink.hWnd
    m_ScriptEngine.AddObject "Engine", Me
    Using m_Graphics, "GraphicsEngine"
    Using m_objConstants, "Constants"
    Using New Fury2GEEnums, "Enums"
    Using New Fury2GETypes, "Types"
    Using New Fury2Globals, "Globals"
    If Strict Then
    Else
        Using Arrays, "Arrays"
        Using Math, "Math"
        Using Strings, "Strings"
    End If
    If EditMode Then m_ScriptEngine.AddObject "Editor", m_objNotify, False, False, True
    l_varTest = Evaluate("5 + 5")
    If CLng(l_varTest) <> 10& Then
        InitScriptEngine = "Sanity test failed. Arithmetic error."
        Exit Function
    End If
    l_varTest = Evaluate("Engine.Version")
    If (CStr(l_varTest) <> Version) Then
        InitScriptEngine = "Sanity test failed. Interop error."
        Exit Function
    End If
End Function

Friend Sub InitSoundEngine()
On Error Resume Next
    Set m_SoundEngine = New SoundEngine
    Set m_SoundEngine.m_Engine = Me
    m_SoundEngine.Enabled = Not Me.DisableSound
    m_SoundEngine.Init
End Sub

Friend Sub InitNamespaces()
On Error Resume Next
    Set Arrays = New Fury2Arrays
    Set Math = New Fury2Math
    Set Strings = New Fury2Strings
End Sub

Friend Sub InitVariables()
On Error Resume Next
    ReDim m_picPictures(0 To c_lngMaxPictureSlot)
    ReDim m_picSavedPictures(0 To c_lngMaxPictureSlot)
    ReDim m_hsHotspots(0 To c_lngMaxHotspot)
    ReDim m_hsSavedHotspots(0 To c_lngMaxHotspot)
    ReDim m_strScriptContext(0 To 0)
    ReDim m_evtEventQueue(0 To c_lngMaxEventQueue)

    Set Mouse = New Fury2Mouse
    Mouse.SetEngine Me
    Set Keyboard = New Fury2Keyboard
    Keyboard.SetEngine Me
    Set Joysticks = F2Collection()
    Set ScriptCache = New Collection
    Set EventHookStack = New Collection
    Set Timers = F2Collection()
    Set MessageQueue = F2Collection()
    Set MessageWindows = F2Collection()
    Set ActionQueue = F2Collection()
    Set Hosts = F2Collection()
    Set Videos = F2Collection()
    Set BGMStack = F2Collection()
    Set m_colPluginStack = New Collection
    Set m_colReversePluginStack = New Collection
    Set m_Images = New Collection
    Set m_Tilesets = New Collection
    Set m_Graphics = New Fury2GEGlobal
    Set m_Globals = New Fury2Globals
    Set mvarMaps = New Fury2Maps
    Set mvarMaps.Engine = Me
    Set m_objConstants = New Fury2ScriptConstants
    Set m_objTransitions = New Fury2Transitions
    m_objTransitions.SetEngine Me
    If Not (DefaultTransition Is Nothing) Then
        If TypeOf DefaultTransition.Object Is Fury2Transitions Then
            Set DefaultTransition.Object = m_objTransitions
            DefaultTransition.ResolveMethod
        End If
    End If
    Set m_colTimerEvents = New Fury2TimerEvents
    m_colTimerEvents.SetEngine Me
    Set Filesystem = New Fury2Filesystem
    Set Configuration = New Fury2ConfigurationFile
    Configuration.SetEngine Me
    Set mvarCameras = New Fury2Cameras
    Set Font = New Fury2Font
    Font.SetEngine Me
    Set WindowSkin = New Fury2WindowSkin
    WindowSkin.SetEngine Me
    Set m_Flags = New Collection
    Set m_FlagNames = New Collection
    Set Party = New Fury2Party
    Party.SetEngine Me
    Set SavedMaps = F2Collection()

    InitNamespaces

    Call QueryPerformanceFrequency(m_curCPUFrequency)
    m_dblTimeOffset = 0
    m_booRanStartEvent = False
    ScriptObjectCount = 0
    m_strScriptContext(0) = "Fury²"
    DefaultTransition = TR_None
    DefaultTransitionLength = 1
    m_booFresh = True
    m_booIDE = InIDE
    DefaultBGMVolume = 0.65
    DefaultSampleVolume = 1
    ActivationDistance = c_lngDefaultActivationDistance
    ScriptLanguage = "VBScript"
    MaxFrameskip = c_lngMaxFrameskip
    OutputPlugin = "GDI"
    ScreenshotPattern = "/screenshot_*.png"
    SubthreadCount = -1
    VelocityMultiplier = 1
    AnimationMultiplier = 1
    VelocityDecay = c_sngDefaultVelocityDecay
    RepeatDelay = c_sngDefaultRepeatDelay
    EnableTimers = True
    ClearMapBG = True
    StartLayer = 1
    DisableCameraBuffers = True
    DisableSound = False
    ScreenWidth = 320
    ScreenHeight = 240
    ScreenScaleRatio = 1
    ShowTitlebar = True
    ShowFPS = True
    ConsoleEnabled = True
    HarassCPU = True
    Paused = False
    Running = Not EditMode
    PictureDrawPosition = 1
    LogToFile = True
    LogToClipboard = False
    DefaultControllerAngles = Array(0, 45, 90, 135, 180, 225, 270, 315)
    DefaultControllerStyle = F2CS_Direct

    PushPictures
    PushHotspots

    Set Filesystem.OutputHook = Me
    Filesystem.Root = "."
    Configuration.InitDefaultSettings
End Sub

Public Property Get InIDE() As Boolean
Attribute InIDE.VB_Description = "If True, the engine is currently running in an IDE."
On Error Resume Next
    InIDE = mdlFury2.InIDE
End Property

Public Sub iTerminate()
Attribute iTerminate.VB_MemberFlags = "40"
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

Public Sub LoadCursor(ByVal Filename As String)
Attribute LoadCursor.VB_MemberFlags = "40"
On Error Resume Next
    If Filesystem.FileExists(Filename) Then
        Filename = (Filesystem.File(Filename).GetRealFilename)
        Set m_objOutputPlugin.Window.MouseIcon = stdole.LoadPicture(CStr(Filename))
        m_objOutputPlugin.Window.MousePointer = 99
    End If
End Sub

Public Sub LoadMap(ByVal FilenameOrMap, Optional ByVal LoadX As Single = vbNull, Optional ByVal LoadY As Single = vbNull, Optional ByVal Layer As Long = -1, Optional ByVal Index As Long = 1)
Attribute LoadMap.VB_Description = "Loads a map into the game engine. The specified location/layer will be passed to the MapLoad event."
On Error Resume Next
Dim m_gblGlobal As Fury2Globals
Dim m_lngSprites As Long, m_lngLayers As Long
Dim m_mapNew As Fury2Map
Dim l_mapOld As Fury2Map
Dim m_strOldFn As String
Dim l_strFilename As String
    Set m_gblGlobal = New Fury2Globals
    DebugOut "Loading map """ & ToString(FilenameOrMap) & """"
    ContextLevelAdd "LoadMap(""" + ToString(FilenameOrMap) + """, " + CStr(Index) + ")"
    Err.Clear
    Set l_mapOld = Maps(Index)
    Err.Clear
    If VarType(FilenameOrMap) = vbString Then
        l_strFilename = CStr(FilenameOrMap)
        m_strOldFn = Trim(CStr(l_strFilename))
        l_strFilename = Filesystem.AutoSelectExtension(l_strFilename, "f2m;f2map")
        If Not Filesystem.FileExists(l_strFilename) Then
            CriticalError "LoadMap", "Filesystem.FileExists(l_strFilename)", "Map not found: " + CStr(l_strFilename)
        Else
            Set m_mapNew = New Fury2Map
            m_mapNew.SetEngine Me
            If LoadFromFile(m_mapNew, Filesystem.File(l_strFilename)) Then
            Else
                CriticalError "LoadMap", "LoadFromFile(m_mapNew, Filesystem.File(filename))", "Map Load Failed"
            End If
            m_mapNew.Filename = l_strFilename
        End If
    Else
        Set m_mapNew = FilenameOrMap
    End If
    If m_mapNew Is Nothing Then
        CriticalError "LoadMap", "", "No valid map provided"
    Else
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
        DebugOut "Party.InitMap"
        Party.InitMap CLng(Index), CSng(LoadX), CSng(LoadY), CLng(Layer)
        m_ScriptEngine.Exec "Engine_MapLoad", CLng(Index), CSng(LoadX), CSng(LoadY), CLng(Layer)
        m_ScriptEngine.Exec "Engine_MapLoad", CLng(Index), CSng(LoadX), CSng(LoadY)
        m_ScriptEngine.Exec "Engine_MapLoad", CLng(Index)
        m_ScriptEngine.Exec "Engine_MapLoad"
        MapLoad m_strOldFn
        DebugOut "Map """ & ToString(FilenameOrMap) & """ loaded"
        If Debugger Is Nothing Then Else Debugger.MapSwitch ToString(FilenameOrMap)
    End If
    Cameras.Dirty
    Set Map = Maps(Index)
    l_mapOld.Unload
    l_mapOld.Free
    FlushImageCache
    ContextLevelRemove
    m_booTrace = False
End Sub

Public Sub LoadMapState(ByVal Index As Long, ByVal Name As String)
Attribute LoadMapState.VB_Description = "Loads the current state of a map."
On Error Resume Next
    Maps.Remove Index
    Maps.Add SavedMaps(Name), , Index
    SavedMaps.Remove Name
End Sub

Public Sub InternalLoadProgress(ByVal Progress As Double, Optional ByVal LoadType As String = "Game")
On Error Resume Next
    LoadProgress LoadType, 100 * Progress
End Sub

Public Sub InternalSaveProgress(ByVal Progress As Double, Optional ByVal LoadType As String = "Game")
On Error Resume Next
    LoadProgress LoadType, 100 * Progress
End Sub

Friend Sub LoadProgress(LoadType As String, Progress As Single)
On Error Resume Next
'    ContextLevelAdd "Engine_LoadProgress"
    Err.Clear
    m_ScriptEngine.Exec "Engine_LoadProgress", LoadType, Progress
'    ContextLevelRemove
End Sub

Friend Sub LogPrint(ByVal Text As String)
On Error Resume Next
Dim l_strCBoard As String
Dim l_lngHandle As Long
    If LogToClipboard Then
        l_strCBoard = Clipboard.GetText
        Clipboard.Clear
        Clipboard.SetText l_strCBoard + vbCrLf + "> " + Text
    End If
    If LogToFile Then
        l_lngHandle = FreeFile
        Open App.Path & "\fury2.log" For Append As #l_lngHandle
            Print #l_lngHandle, Text
        Close #l_lngHandle
    End If
    Debug.Print Text
    If (m_objNotify Is Nothing) Or (m_booNotifyNoLogging) Then
    Else
        Err.Clear
        m_objNotify.LogOutput Text
        If Err <> 0 Then m_booNotifyNoLogging = True
    End If
End Sub

Private Sub m_imeIMEEngine_CompositionBegin()
On Error Resume Next
    m_booIMEOpen = True
    m_strIMEText = ""
End Sub

Private Sub m_imeIMEEngine_CompositionComplete()
On Error Resume Next
Dim l_lngChars As Long
Dim l_lngChar As Long
    m_strIMEText = m_imeIMEEngine.Text
    If Len(m_strIMEText) > 0 Then
        For l_lngChars = 1 To Len(m_strIMEText)
            l_lngChar = AscW(Mid(m_strIMEText, l_lngChars, 1))
            Me.Fury2VideoEventSink_KeyPress2 l_lngChar
        Next l_lngChars
    End If
    m_booIMEOpen = False
    m_strIMEText = ""
End Sub

Private Sub m_imeIMEEngine_CompositionUpdate()
On Error Resume Next
    m_booIMEOpen = True
    m_strIMEText = m_imeIMEEngine.Text & vbCrLf & m_imeIMEEngine.GuideLine
    m_lngIMECursorPos = m_imeIMEEngine.CursorPos
End Sub

Private Sub m_imeIMEEngine_LanguageChanged()
On Error Resume Next
End Sub

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

Private Sub m_ScriptEngine_InterruptPoll(Interrupt As Boolean)
On Error Resume Next
    If m_booHaltScripts Then Interrupt = True
    If Terminating Then
        m_lngInterruptTimer = m_lngInterruptTimer + 1
        If m_lngInterruptTimer >= 5 Then
            Interrupt = True
        End If
    Else
        m_lngInterruptTimer = 0
    End If
    If EditMode Then
        DoEvents
    Else
        If Debugger Is Nothing Then
        Else
            Debugger.InterruptPoll Interrupt
        End If
    End If
End Sub

Public Sub MapLoad(ByVal Filename As String)
Attribute MapLoad.VB_MemberFlags = "40"
On Error Resume Next
Dim Obj As Object
    For Each Obj In m_colPluginStack
        Obj.MapLoad CStr(Filename)
    Next Obj
End Sub

Public Sub MovePicture(ByVal Slot As Long, Optional ByVal NewX As Long = -32767, Optional ByVal NewY As Long = -32767, Optional ByVal NewEffectMode As SFXBlitModes = BlitMode_Default, Optional ByVal NewAlpha As Single = -1)
Attribute MovePicture.VB_Description = "Repositions a picture slot."
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With m_picPictures(Slot)
        If NewX <> -32767 Then
            m_lngWidth = .DestRect.Width
            .DestRect.left = NewX
            .DestRect.right = NewY + m_lngWidth
        End If
        If NewY <> -32767 Then
            m_lngHeight = .DestRect.Height
            .DestRect.top = NewY
            .DestRect.bottom = NewY + m_lngHeight
        End If
        If NewAlpha <> -1 Then .Alpha = CSng(NewAlpha)
        If NewEffectMode <> BlitMode_Default Then .EffectMode = CLng(NewEffectMode) + 1
    End With
End Sub

Public Sub Mute()
Attribute Mute.VB_Description = "Silences the sound engine."
On Error Resume Next
    m_sngMutePreviousVolume = SoundEngine.MasterVolume
    SoundEngine.MasterVolume = 0
End Sub

Public Sub Notify_CriticalError()
Attribute Notify_CriticalError.VB_MemberFlags = "40"
    CriticalError "Unknown", "Unknown"
End Sub

Public Sub Panic()
Attribute Panic.VB_MemberFlags = "40"
    TerminateProcess GetCurrentProcess, 1
End Sub

Public Sub Transition(Optional ByVal Length As Double = -1, Optional ByVal Param = Nothing)
Attribute Transition.VB_Description = "Invokes the default transition handler."
On Error Resume Next
    If Length = -1 Then
        Length = DefaultTransitionLength
    End If
    DefaultTransition.Invoke Length, Param
End Sub

Friend Sub ParseConfiguration()
On Error Resume Next
Dim l_varValues, l_strSetting As String
    With Configuration

        l_varValues = Split(.Settings("Screen Mode"), "@")
        l_varValues = Split(l_varValues(0), "x")
        ScreenWidth = CLng(l_varValues(0))
        ScreenHeight = CLng(l_varValues(1))
        ScreenScaleRatio = CSng(.Settings("Screen Scale Ratio"))
        Select Case LCase(Trim(.Settings("Output Scaling Mode")))
        Case "bilinear"
            ScreenScaleMode = 1
        Case "scale2x"
            ScreenScaleMode = 2
        Case Else
            ScreenScaleMode = 0
        End Select

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
        SetDefaultTransition Trim(.Settings("Default Transition"))
        DefaultTransitionLength = CSng(.Settings("Default Transition Length"))

        DisableSound = Not .ToBoolean("Enable Sound")

        ScriptLanguage = .Settings("Language")

        DefaultBGMVolume = CSng(.Settings("Music Volume"))
        DefaultSampleVolume = CSng(.Settings("Sound Volume"))

        HideConfiguration = Not .ToBoolean("Show Configuration Dialog")

        Strict = .ToBoolean("Strict")

    End With
End Sub

Sub PlayBGM(ByVal Filename, Optional ByVal Fade As Boolean = True)
Attribute PlayBGM.VB_Description = "Loads a BGM and plays it, stopping any previously playing BGMs."
On Error Resume Next
    If Not (CurrentBGM Is Nothing) Then StopBGM False
    If VarType(Filename) = vbObject Then
        Set CurrentBGM = Filename
    Else
        DebugOut "Loading BGM """ & Filename & """"
        Set CurrentBGM = SoundEngine.Load(CStr(Filename))
    End If
    CurrentBGM.LoopMode = LoopMode_Normal
    CurrentBGM.Loopcount = -1
    Set CurrentBGMChannel = CurrentBGM.Play(True)
    If Not (CurrentBGMChannel Is Nothing) Then
        CurrentBGMChannel.Priority = 0
        If Fade Then
            CurrentBGMChannel.Volume = 0
            FadeBGM , DefaultBGMVolume
        Else
            CurrentBGMChannel.Volume = DefaultBGMVolume
        End If
        CurrentBGMChannel.Paused = False
    End If
    SoundEngine.FlushCache
    Err.Clear
End Sub

Sub PopBGM(Optional ByVal Crossfade As Boolean = True)
Attribute PopBGM.VB_Description = "Pops a BGM from the BGM stack."
On Error Resume Next
Dim l_sndOld As Sound
    Set l_sndOld = BGMStack.Item(BGMStack.Count)
    BGMStack.Remove BGMStack.Count
    SwitchBGM l_sndOld, Crossfade
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
Attribute PopHotspots.VB_Description = "Pops a set of hotspots from the hotspot stack."
On Error Resume Next
    m_hsHotspots = m_hsSavedHotspots
End Sub

Sub PopPictures()
Attribute PopPictures.VB_Description = "Pops a set of picture slots from the picture stack."
On Error Resume Next
Dim l_lngSlot As Long
    For l_lngSlot = 0 To c_lngMaxPictureSlot
        m_picPictures(l_lngSlot) = m_picSavedPictures(l_lngSlot)
    Next l_lngSlot
    ActivePictureSlots = m_lngSavedActivePictures
End Sub

Sub PushBGM(ByVal Filename As String, Optional ByVal Crossfade As Boolean = True)
Attribute PushBGM.VB_Description = "Pushes the current BGM onto the stack, and begins playing a new BGM. The current BGM can be restored with PopBGM."
On Error Resume Next
    BGMStack.Add CurrentBGM
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
Attribute PushHotspots.VB_Description = "Pushes the current hotspots onto the hotspot stack."
On Error Resume Next
    m_hsSavedHotspots = m_hsHotspots
End Sub

Sub PushPictures()
Attribute PushPictures.VB_Description = "Pushes the current picture slots onto the picture stack."
On Error Resume Next
Dim l_lngSlot As Long
    For l_lngSlot = 0 To c_lngMaxPictureSlot
        m_picSavedPictures(l_lngSlot) = m_picPictures(l_lngSlot)
    Next l_lngSlot
    m_lngSavedActivePictures = ActivePictureSlots
End Sub

Sub SetPictures(ByRef NewPictures() As Fury2Picture, ByVal Count As Long)
On Error Resume Next
Dim l_lngSlot As Long
    For l_lngSlot = 0 To c_lngMaxPictureSlot
        m_picPictures(l_lngSlot) = NewPictures(l_lngSlot)
    Next l_lngSlot
    ActivePictureSlots = Count
End Sub

Sub SetHotspots(ByRef NewHotspots() As Fury2Hotspot)
On Error Resume Next
    m_hsHotspots = NewHotspots()
    ReDim Preserve m_hsHotspots(0 To c_lngMaxHotspot)
End Sub

Function GetPictures() As Fury2Picture()
On Error Resume Next
    GetPictures = m_picPictures()
End Function

Function GetHotspots() As Fury2Hotspot()
On Error Resume Next
    GetHotspots = m_hsHotspots()
End Function

Friend Sub QueueEvent(ByVal EventType As Long, Optional ByVal SParam1 As Single = 0, Optional ByVal SParam2 As Single = 0, Optional ByVal LParam1 As Long = 0, Optional ByVal LParam2 As Long = 0, Optional ByVal LParam3 As Long = 0, Optional ByVal LParam4 As Long = 0)
On Error Resume Next
Dim Evt As Fury2QueueEvent
    If m_lngEventQueueLength >= c_lngMaxEventQueue Then Exit Sub
    If Halted Then Exit Sub
    m_lngEventQueueLength = m_lngEventQueueLength + 1
    With Evt
        .Type = EventType
        .SParam1 = SParam1
        .SParam2 = SParam2
        .LParam1 = LParam1
        .LParam2 = LParam2
        .LParam3 = LParam3
        .LParam4 = LParam4
        .TimeStamp = HiTimer
    End With
    m_evtEventQueue(m_lngEventQueueLength - 1) = Evt
End Sub

Public Sub Quit()
Attribute Quit.VB_Description = "Shuts down the current game, and then shuts down the engine."
On Error Resume Next
    ContextLevelAdd "Quit"
    Fury2VideoEventSink_Closed
    ContextLevelRemove
End Sub

Public Sub Redraw()
Attribute Redraw.VB_Description = "Forces the engine to redraw."
On Error Resume Next
Dim l_lngObjects As Long, l_booBehind As Boolean
Dim l_lngCount As Long
Dim pObj As Fury2Plugin, Obj As Object
'    If (Not Running) And (Not EditMode) Then Exit Sub
    If Halted Then Exit Sub
    ContextLevelAdd "Redraw"
    If ClearEveryFrame Then m_imgBackbuffer.Clear ClearBackgroundColor
    m_imgBackbuffer.ResetClipRectangle
    If PictureDrawPosition = -1 Then RedrawPictures
    RedrawCameras
    If PictureDrawPosition = 0 Then RedrawPictures
    If m_colReversePluginStack.Count > 0 Then
        For Each Obj In m_colReversePluginStack
            Set pObj = Nothing
            Set pObj = Obj
            Err.Clear
            If Not (pObj Is Nothing) Then
                l_booBehind = False
                l_booBehind = pObj.DrawBehind
                If l_booBehind Then pObj.Redraw
            End If
        Next Obj
    End If
    If PictureDrawPosition = 1 Then RedrawPictures
    If m_colReversePluginStack.Count > 0 Then
        For Each Obj In m_colReversePluginStack
            If Not (Obj Is Nothing) Then
                Set pObj = Nothing
                Set pObj = Obj
                Err.Clear
                If pObj Is Nothing Then
                    Obj.Redraw
                Else
                    l_booBehind = False
                    l_booBehind = pObj.DrawBehind
                    If Not l_booBehind Then pObj.Redraw
                End If
            End If
        Next Obj
    End If
    If PictureDrawPosition = 2 Then RedrawPictures
    If Not (RedrawHook Is Nothing) Then
        RedrawHook.Invoke
    End If
    If PictureDrawPosition = 3 Then RedrawPictures
    RedrawConsole
    RedrawIME
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
Dim l_lngStart As Long
    If m_sngConsoleOffset >= ConsoleHeight Then Exit Sub
    m_lngConsoleCaretPosition = ClipValue(m_lngConsoleCaretPosition, 1, Len(ConsoleText) + 1)
    With Backbuffer
        Set l_rctConsole = F2Rect(0, -m_sngConsoleOffset, ScreenWidth, ConsoleHeight, False)
        .ResetClipRectangle
        .ClippedSetClipRectangle l_rctConsole
        l_lngHeight = ClipValue(Font.TextHeight(ConsoleText, .Width - 2, False) + (Font.FullHeight - Font.Height), Font.FullHeight, ConsoleHeight) + 2
        l_lngTopColor = F2RGB(180, 180, 180, 192)
        l_lngBottomColor = F2RGB(110, 110, 110, 192)
        .GradientFill l_rctConsole, Array(l_lngTopColor, l_lngTopColor, l_lngBottomColor, l_lngBottomColor), RenderMode_SourceAlpha
        If ConsoleItems.Count > 0 Then
            l_lngY = l_rctConsole.bottom - l_lngHeight - 4
            l_lngStart = ConsoleItems.Count - ConsoleScroll
            If l_lngStart >= 1 Then
                For l_lngItems = l_lngStart To 1 Step -1
                    l_strText = ConsoleItems(l_lngItems)
                    Set l_rctSize = Font.GetTextRectangle(l_strText, .Width - 4)
                    Font.Draw Backbuffer, l_strText, F2Rect(2, l_lngY - l_rctSize.Height, .Width - 4, l_rctSize.Height + (Font.FullHeight - Font.Height), False), F2White, 1
                    l_lngY = l_lngY - l_rctSize.Height
                    If l_lngY <= 0 Then Exit For
                Next l_lngItems
            End If
        End If
        Set l_rctTextbox = F2Rect(l_rctConsole.left, l_rctConsole.bottom - l_lngHeight - 2, l_rctConsole.right, l_rctConsole.bottom, True)
        .ResetClipRectangle
        .ClippedSetClipRectangle l_rctTextbox
        .GradientFill l_rctTextbox, Array(F2RGB(0, 0, 0, 32), F2RGB(0, 0, 0, 32), F2RGB(0, 0, 0, 96), F2RGB(0, 0, 0, 96)), RenderMode_SourceAlpha
        Font.Draw Backbuffer, ConsoleText, F2Rect(2, l_rctConsole.bottom - l_lngHeight, .Width - 4, l_lngHeight + (Font.FullHeight - Font.Height) - 2, False), , , Font.Options(0, 0, 0, 0, 0, m_lngConsoleCaretPosition, F2White, , False)
        .ResetClipRectangle
    End With
End Sub

Public Sub RedrawIME()
Attribute RedrawIME.VB_Description = "Redraws the IME's input display."
On Error Resume Next
Dim l_rctText As Fury2Rect
Dim l_rctCandidates As Fury2Rect
Dim l_strCandidates() As String
Dim l_lngCandidates As Long, l_lngY As Long, l_lngHeight As Long
    If m_booIMEOpen Then
        Set l_rctText = Font.GetTextRectangle(m_strIMEText, Backbuffer.Width, False)
        l_rctText.Translate (Backbuffer.Width - l_rctText.Width) \ 2, Backbuffer.Height - (l_rctText.Height + 5)
        l_rctText.Adjust 2, 2
        Backbuffer.Fill l_rctText, F2RGB(0, 0, 0, 192), RenderMode_SourceAlpha
        Backbuffer.Box l_rctText, F2White
        l_rctText.Adjust -2, -2
        l_rctText.Width = l_rctText.Width + 1
        Font.Draw Backbuffer, m_strIMEText, l_rctText, F2White, 1, Font.Options(0, 0, 0, 0, 0, m_lngIMECursorPos + 1, F2White, , False)
        l_strCandidates = m_imeIMEEngine.Candidates
        l_lngCandidates = -1
        Err.Clear
        l_lngCandidates = UBound(l_strCandidates)
        If Err <> 0 Or l_lngCandidates < 0 Then
        Else
            Set l_rctCandidates = Font.GetTextRectangle(Join(l_strCandidates, vbCrLf), Backbuffer.Width, False)
            l_rctCandidates.Translate (Backbuffer.Width - l_rctCandidates.Width) \ 2, l_rctText.top - l_rctCandidates.Height - 5
            l_rctCandidates.Adjust 2, 2
            Backbuffer.Fill l_rctCandidates, F2RGB(0, 0, 0, 192), RenderMode_SourceAlpha
            Backbuffer.Box l_rctCandidates, F2White
            l_rctCandidates.Adjust -2, -2
            l_lngY = l_rctCandidates.top
            For l_lngCandidates = 0 To UBound(l_strCandidates)
                If l_lngCandidates = m_imeIMEEngine.SelectedCandidate Then
                    Backbuffer.Fill F2Rect(l_rctCandidates.left, l_lngY, l_rctCandidates.Width, Font.FullHeight, False), F2RGB(32, 63, 240, 220), RenderMode_SourceAlpha
                End If
                Font.Draw Backbuffer, l_strCandidates(l_lngCandidates), F2Rect(l_rctCandidates.left, l_lngY, l_rctCandidates.right, l_rctCandidates.bottom, True), F2White, 1, Font.Options(0, 0, 0, 0, 0, 0, 0, 0, False), l_lngHeight
                l_lngY = l_lngY + (l_lngHeight - Font.FullHeight + Font.Height)
            Next l_lngCandidates
        End If
    End If
End Sub

Public Sub RedrawPictures()
Attribute RedrawPictures.VB_Description = "Redraws all active picture slots."
On Error Resume Next
Dim l_lngPictures As Long, l_objPic As Fury2Image, l_objGraph As Fury2Graphic
Dim l_lngScaler As Long
    If ActivePictureSlots > 0 Then
        For l_lngPictures = 0 To ActivePictureSlots - 1
            With m_picPictures(l_lngPictures)
                If .Visible Then
                    If (.Picture Is Nothing) Or (.DestRect Is Nothing) Or (.SourceRect Is Nothing) Then
                    Else
                        l_lngScaler = IIf(SmoothScaling, ResampleMode_Bilinear, ResampleMode_Linear)
                        If TypeOf .Picture Is Fury2Image Then
                            Set l_objPic = .Picture
                        ElseIf TypeOf .Picture Is Fury2Graphic Then
                            Set l_objGraph = .Picture
                            Set l_objPic = l_objGraph.CurrentImage
                        End If
                        Dim l_sngScale As Single
                        l_sngScale = ((.DestRect.Width / .SourceRect.Width) + (.DestRect.Height / .SourceRect.Height)) / 2
                        If l_objGraph Is Nothing Then
                            If .Rotation <> 0 Then
                                l_objPic.Draw m_imgBackbuffer, .DestRect.XCenter, .DestRect.YCenter, .Alpha, l_sngScale, .Rotation, .EffectMode - 1, .EffectColor, l_lngScaler
                            Else
                                If .Tile Then
                                    If .EffectMode = 0 Then
                                        m_imgBackbuffer.TileBlit .DestRect, l_objPic
                                    ElseIf .Alpha > 0 Then
                                        m_imgBackbuffer.TileBlit .DestRect, l_objPic, .Alpha, .EffectMode - 1, .EffectColor
                                    End If
                                Else
                                    If .EffectMode = 0 Then
                                        m_imgBackbuffer.Blit .DestRect, .SourceRect, l_objPic, , , , l_lngScaler
                                    ElseIf .Alpha > 0 Then
                                        m_imgBackbuffer.Blit .DestRect, .SourceRect, l_objPic, .Alpha, .EffectMode - 1, .EffectColor, l_lngScaler
                                    End If
                                End If
                            End If
                        Else
                            l_objGraph.Draw m_imgBackbuffer, .DestRect.left, .DestRect.top, .Alpha, l_sngScale, .Rotation
                        End If
                    End If
                End If
            End With
        Next l_lngPictures
    End If
End Sub

Sub ReleaseScriptEngine()
Attribute ReleaseScriptEngine.VB_MemberFlags = "40"
On Error Resume Next
    Set m_ScriptEngine = Nothing
    Unload frmScriptSink
End Sub

Public Sub ReLoadPicture(ByVal Filename As String)
Attribute ReLoadPicture.VB_Description = "Unloads a picture from the cache and loads it again."
On Error Resume Next
    Filename = LCase(Trim(Filename))
    m_Images.Remove Filename
    LoadPicture Filename
End Sub

Private Sub RemoveRef(ByRef Obj As IShellFolderEx_TLB.IUnknown)
On Error Resume Next
    Obj.Release
End Sub

Sub ResetHotspots()
Attribute ResetHotspots.VB_Description = "Resets all hotspots."
On Error Resume Next
Dim l_lngHotspot As Long
    For l_lngHotspot = 0 To c_lngMaxHotspot
        m_hsHotspots(l_lngHotspot).Enabled = False
    Next l_lngHotspot
    Erase m_hsHotspots
    ReDim m_hsHotspots(0 To c_lngMaxHotspot)
End Sub

Public Sub ResetKeys()
Attribute ResetKeys.VB_Description = "Resets the state of all keyboard keys."
On Error Resume Next
    Keyboard.Reset
End Sub

Sub ResetPictures()
Attribute ResetPictures.VB_Description = "Resets all picture slots."
On Error Resume Next
    Erase m_picPictures
    ReDim m_picPictures(0 To c_lngMaxPictureSlot)
End Sub

Public Sub ResetTiming()
Attribute ResetTiming.VB_Description = "Resets the timing logic used by the framerate counter and framerate balancer."
On Error Resume Next
Dim l_ctlControl As Fury2InputControl
    For Each l_ctlControl In Controls
        l_ctlControl.ResetRepeat
    Next l_ctlControl
    m_booResetClock = True
End Sub

Public Sub ResizeOutput(ByVal X As Long, ByVal Y As Long)
Attribute ResizeOutput.VB_Description = "Resizes the output window to a new size."
On Error Resume Next
    Err.Clear
    m_objOutputPlugin.ScaleRatio = ScreenScaleRatio
    m_objOutputPlugin.ScaleMode = ScreenScaleMode
    m_objOutputPlugin.ResizeWindow X, Y
    Err.Clear
End Sub

Public Sub ResizePicture(ByVal Slot As Long, Optional ByVal NewWidth As Long = -32767, Optional ByVal NewHeight As Long = -32767)
Attribute ResizePicture.VB_Description = "Resizes a picture slot. (Not the actual picture being displayed - just the coordinates)"
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    With m_picPictures(Slot)
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

Public Sub RunCode(ByRef Code)
Attribute RunCode.VB_Description = "Runs a block of script code."
On Error Resume Next
    ContextLevelAdd "RunCode"
    m_ScriptEngine.AddCode ToString(Code)
    If Err <> 0 Then
        Err.Clear
'        m_ScriptEngine.Error.Clear
    End If
    ContextLevelRemove
End Sub

Public Sub SaveMapState(ByVal Index As Long, ByVal Name As String)
Attribute SaveMapState.VB_Description = "Saves the current state of a map."
On Error Resume Next
    SavedMaps.Add Maps(Index), Name
End Sub

Public Sub SaveUserData(ByVal Filename As String, ByVal Data As Object)
Attribute SaveUserData.VB_Description = "Saves an object's data to a file that can be later loaded by the engine or editor as UserData."
On Error Resume Next
Dim l_vfFile As VirtualFile
Dim l_strType As String
Dim l_objData As Object
    Set l_vfFile = F2File
    SaveUserDataToFile l_vfFile, Data
    Filesystem.CreateFile(Filename).Save l_vfFile
    Err.Clear
End Sub

Public Sub SaveUserDataToFile(ByVal File As VirtualFile, ByVal Data As Object)
Attribute SaveUserDataToFile.VB_Description = "Saves an object's data to a file that can be later loaded by the engine or editor as UserData."
On Error Resume Next
Dim l_strType As String
Dim l_objData As Object
    File.WriteSegment_Begin
        l_strType = TypeName(Data)
        File.Save l_strType
        File.Save Data
    File.WriteSegment_End
    Err.Clear
End Sub

Public Sub SetHotspot(ByVal Index As Long, Optional ByVal left As Long = 0, Optional ByVal top As Long = 0, Optional ByVal Width As Long = 0, Optional ByVal Height As Long = 0, Optional ByRef ClickEvent = Nothing, Optional ByRef EnterEvent = Nothing, Optional ByRef LeaveEvent = Nothing)
Attribute SetHotspot.VB_Description = "Creates a hotspot."
On Error Resume Next
    With m_hsHotspots(CLng(Index))
        .Enabled = True
        .x1 = left
        .y1 = top
        .x2 = left + Width
        .y2 = top + Height
        Set .ClickEvent = BindEvent(ClickEvent)
        Set .EnterEvent = BindEvent(EnterEvent)
        Set .LeaveEvent = BindEvent(LeaveEvent)
        If (Mouse.X >= .x1) And (Mouse.Y >= .y1) And (Mouse.X < .x2) And (Mouse.Y < .y2) Then
            If Not (.EnterEvent Is Nothing) Then .EnterEvent.Invoke
            .Hovering = True
        Else
            .Hovering = False
        End If
    End With
    Err.Clear
End Sub

Friend Sub SetIObjThis()
On Error Resume Next
    m_ScriptEngine.AddCode m_ScriptEngine.Language.GenerateAssignment("Object", "Engine" & m_ScriptEngine.Language.MethodOperator & "IObjThis", True)
End Sub

Friend Sub SetMouseLocation(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
Dim l_lngBorderWidth As Long, l_lngTitleHeight As Long, l_lngOldMode As Long
Dim l_lngWidth As Long, l_lngHeight As Long
    With m_objOutputPlugin.Window
        If GetForegroundWindow() = .hWnd Then
            l_lngOldMode = .ScaleMode
            .ScaleMode = 1
            l_lngBorderWidth = (.Width - .ScaleWidth) \ 2
            l_lngTitleHeight = (.Height - .ScaleHeight) - (l_lngBorderWidth * 2)
            .ScaleMode = l_lngOldMode
            SetCursorPos (X * (.ScaleWidth / ScreenWidth)) + (l_lngBorderWidth / Screen.TwipsPerPixelX) + (.left / Screen.TwipsPerPixelX), (Y * (.ScaleHeight / ScreenHeight)) + ((l_lngBorderWidth + l_lngTitleHeight) / Screen.TwipsPerPixelY) + (.top / Screen.TwipsPerPixelY)
        Else
            g_lngMouseX = X
            g_lngMouseY = Y
        End If
    End With
End Sub

Public Sub SetNotify(ByRef Obj As Object)
On Error Resume Next
    Set m_objNotify = Obj
    m_booNotifyNoLogging = False
End Sub

Public Sub SetPictureBlitMode(ByVal Slot As Long, ByVal BlitMode As SFXBlitModes)
Attribute SetPictureBlitMode.VB_Description = "Sets the blend mode of a picture slot."
On Error Resume Next
    m_picPictures(Slot).EffectMode = BlitMode + 1
End Sub

Public Sub SetPictureAlpha(ByVal Slot As Long, ByVal Alpha As Single)
On Error Resume Next
    m_picPictures(Slot).Alpha = Alpha
End Sub

Public Sub SetPictureRotation(ByVal Slot As Long, ByVal Rotation As Single)
On Error Resume Next
    m_picPictures(Slot).Rotation = Rotation
End Sub

Public Sub SetPictureImage(ByVal Slot As Long, ByRef Image As Object)
Attribute SetPictureImage.VB_Description = "Sets the image occupying a picture slot."
On Error Resume Next
    Set m_picPictures(Slot).Picture = Image
End Sub

Public Sub SetPictureRectangle(ByVal Slot As Long, ByVal Rectangle As Fury2Rect)
Attribute SetPictureRectangle.VB_Description = "Sets the rectangle of a picture slot."
On Error Resume Next
    Set m_picPictures(Slot).DestRect = Rectangle
End Sub

Public Sub SetPictureSourceRectangle(ByVal Slot As Long, ByVal Rectangle As Fury2Rect)
Attribute SetPictureSourceRectangle.VB_Description = "Sets the source rectangle of a picture slot."
On Error Resume Next
    Set m_picPictures(Slot).SourceRect = Rectangle
End Sub

Public Sub SetPictureTile(ByVal Slot As Long, ByVal Tile As Boolean)
Attribute SetPictureTile.VB_Description = "Sets the tiling mode of a picture slot."
On Error Resume Next
    m_picPictures(Slot).Tile = Tile
End Sub

Sub ShowConsole()
Attribute ShowConsole.VB_Description = "Shows the console."
On Error Resume Next
    #If DebugFeatures = 1 Then
        If ConsoleEnabled Then
            ConsoleScroll = 0
            ConsoleVisible = True
            m_sngConsoleStartOffset = m_sngConsoleOffset
            m_sngConsoleEndOffset = 0
            m_sngConsoleStartTime = HiTimer
            m_sngConsoleEndTime = HiTimer + 0.33
            m_booConsoleSliding = True
        End If
    #End If
End Sub

Public Sub ShowPicture(ByVal Slot As Long, ByVal Pic As Object, Optional ByVal X As Long = 0, Optional ByVal Y As Long = 0, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal x2 As Long = 0, Optional ByVal y2 As Long = 0, Optional ByVal EffectMode As SFXBlitModes = BlitMode_Default, Optional ByVal Alpha As Single = 1, Optional ByVal EffectColor As Long = 0, Optional ByVal Tile As Boolean = False)
Attribute ShowPicture.VB_Description = "Shows a picture in a specified slot at a specified location, similar to RM2k's Show Picture command."
On Error Resume Next
Dim m_lngSlot As Long
    If Slot < 0 Or Slot > c_lngMaxPictureSlot Then Exit Sub
    If Slot + 1 > ActivePictureSlots Then ActivePictureSlots = Slot + 1
    If Width = -1 Then Width = Pic.Width
    If Height = -1 Then Height = Pic.Height
    With m_picPictures(Slot)
        If EffectMode = BlitMode_Default Then
            If TypeOf Pic Is Fury2Graphic Then
                X = X - Pic.XCenter
                Y = Y - Pic.YCenter
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
        Set .DestRect = F2Rect(X, Y, Width, Height, False)
        Set .SourceRect = F2Rect(x2, y2, Width, Height, False)
        .EffectColor = EffectColor
        .Alpha = Alpha
        .Rotation = 0
        Set .Picture = Pic
        .EndFadeScript = ""
        .Visible = True
        .Tile = Tile
    End With
    For m_lngSlot = 0 To c_lngMaxPictureSlot
        With m_picPictures(m_lngSlot)
            If .Visible Then ActivePictureSlots = m_lngSlot + 1
        End With
    Next m_lngSlot
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
    SoundEngine.FlushCache
    ShutdownIME
    If m_booIDE Then DebugOut "Ending game"
    UnloadAllMaps
    ScriptEngine.Exec "Engine_Shutdown"
    m_booHaltScripts = True
    For m_lngWait = 1 To 10
        SleepEx 1, True
        DoEvents
    Next m_lngWait
    SoundEngine.Shutdown
    ReleaseScriptEngine
    DeInitENet
    m_objOutputPlugin.HookEvents Nothing
    m_objOutputPlugin.Shutdown
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

Friend Sub ShutdownIME()
On Error Resume Next
    If m_imeIMEEngine Is Nothing Then Exit Sub
    #If EnableIME = 1 Then
        m_imeIMEEngine.Unhook m_objOutputPlugin.Window.hWnd
    #End If
    Set m_imeIMEEngine = Nothing
End Sub

Sub StopBGM(Optional ByVal Fade As Boolean = True)
Attribute StopBGM.VB_Description = "Stops the currently playing BGM. If Fade is True, the BGM will fade out before stopping."
On Error Resume Next
    If Fade Then
        Set FadeBGM(, 0).EndEvent = BindEvent("Kill", CurrentBGMChannel)
    Else
        CurrentBGMChannel.Kill
    End If
    Set CurrentBGM = Nothing
    Set CurrentBGMChannel = Nothing
    Err.Clear
End Sub

Public Sub Subthread(Optional ByVal Name As String = "")
Attribute Subthread.VB_Description = "Begins a new subthread, optionally with a name. This function will not return until Break has been called to halt the subthread."
On Error Resume Next
    DebugOut "Starting subthread"
    m_booSubthreadStarted = True
    Game Name
End Sub

Public Sub Swap(ByRef One, ByRef Two)
Attribute Swap.VB_Description = "Swaps the contents of two specified variables."
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

Sub SwitchBGM(ByVal BGM, Optional ByVal Crossfade As Boolean = True)
Attribute SwitchBGM.VB_Description = "Changes the currently playing BGM. If the BGM specified is already playing, nothing happens. If Crossfade is True, the two BGMs will fade in/out smoothly."
On Error Resume Next
Dim l_sndBGM As Sound
    If VarType(BGM) = vbObject Then
        Set l_sndBGM = BGM
        If l_sndBGM Is CurrentBGM Then
            Err.Clear
            Exit Sub
        End If
        StopBGM Crossfade
        PlayBGM BGM, Crossfade
    Else
        BGM = CStr(BGM)
        Set l_sndBGM = SoundEngine.Load(BGM)
        If l_sndBGM Is CurrentBGM Then
            Err.Clear
            Exit Sub
        End If
        If (CurrentBGM Is Nothing) Then
        Else
            If BGM = CurrentBGM.Filename Then
                Err.Clear
                Exit Sub
            End If
'            If l_sndBGM.Name = CurrentBGM.Name Then Exit Sub
        End If
        StopBGM Crossfade
        PlayBGM l_sndBGM, Crossfade
    End If
    Err.Clear
End Sub

Public Sub SetDefaultTransition(ByVal NewTransition As String)
Attribute SetDefaultTransition.VB_Description = "Changes the default transition to a new one, by name."
On Error Resume Next
Dim l_strParts As Variant
    l_strParts = Split(NewTransition, " ")
    If UBound(l_strParts) > 0 Then
        Set DefaultTransition = BindEvent("Do" & l_strParts(0), Transitions, Arrays.Slice(l_strParts, UBound(l_strParts) - LBound(l_strParts), 1))
    Else
        Set DefaultTransition = BindEvent("Do" & l_strParts(0), Transitions)
    End If
End Sub

Public Sub SwitchMap(ByVal FilenameOrMap, Optional ByVal LoadX As Single = vbNull, Optional ByVal LoadY As Single = vbNull, Optional ByVal Layer As Long = -1, Optional ByVal Index As Long = 1)
Attribute SwitchMap.VB_Description = "Switches from the current map to a new one. The specified location/layer will be passed to the MapLoad event. The default transition will be invoked to smooth the change between maps."
On Error Resume Next
Dim m_imgOld As Fury2Image
    ContextLevelAdd "SwitchMap(""" + ToString(FilenameOrMap) + """, " + CStr(Index) + ")"
    If DefaultTransition <> TR_None Then Set m_imgOld = m_imgBackbuffer.Duplicate
    Transition -1, 0
    LoadMap FilenameOrMap, LoadX, LoadY, Layer, Index
    Sync
    Redraw
    Transition -1, 1
    Set m_imgOld = Nothing
    ContextLevelRemove
    Err.Clear
End Sub

Sub SwitchOutputPlugin(ByVal NewPlugin As String)
Attribute SwitchOutputPlugin.VB_Description = "Switches to a new output plugin by name."
On Error Resume Next
    If m_objOutputPlugin Is Nothing Then
    Else
        ShutdownIME
        Err.Clear
        Transitions.FadeOut 1
        m_objOutputPlugin.HookEvents Nothing
        m_objOutputPlugin.Shutdown
        Set m_objOutputPlugin = Nothing
        Set m_objVideoDriver = Nothing
    End If
    OutputPlugin = NewPlugin
    Set m_objVideoDriver = LoadOutputPlugin(OutputPlugin)
    Set m_objOutputPlugin = m_objVideoDriver
    If m_objOutputPlugin Is Nothing Then
        CriticalError "SwitchOutputPlugin", "SwitchOutputPlugin """ + CStr(OutputPlugin) + """", "Unable to load output plugin": Exit Sub
    End If
    Err.Clear
    m_objOutputPlugin.HookEvents Me
    m_objOutputPlugin.ScaleRatio = ScreenScaleRatio
    m_objOutputPlugin.ScaleMode = ScreenScaleMode
    m_objOutputPlugin.Init ScreenWidth, ScreenHeight, IIf(ShowTitlebar, WindowCaption, ""), Not Fullscreen
    m_objOutputPlugin.ScaleMode = ScreenScaleMode
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
    InitIME
    Err.Clear
    If SubthreadCount >= 0 Then Transitions.FadeIn 1
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

Public Sub TakeScreenshot()
Attribute TakeScreenshot.VB_Description = "Takes a screenshot and saves it."
On Error Resume Next
Dim l_imgScreen As Fury2Image
Dim l_lngIndex As Long
Dim l_strFilename As String
    Set l_imgScreen = GrabScreen(False)
    l_imgScreen.FillChannel l_imgScreen.Rectangle, 3, 255
    If ScreenshotOverlay Is Nothing Then
    Else
        l_imgScreen.Blit F2Rect(l_imgScreen.Width - ScreenshotOverlay.Width, l_imgScreen.Height - ScreenshotOverlay.Height, ScreenshotOverlay.Width, ScreenshotOverlay.Height, False), , ScreenshotOverlay, 1, BlitMode_SourceAlpha
    End If
    Do
        l_strFilename = Replace(ScreenshotPattern, "*", Format(l_lngIndex, "000"))
        If Filesystem.FileExists(l_strFilename) Then
            l_lngIndex = l_lngIndex + 1
        Else
            l_imgScreen.SavePNG Filesystem.CreateFile(l_strFilename).GetRealFilename()
            Exit Do
        End If
    Loop
End Sub

Public Sub TextOut(ByRef Text)
Attribute TextOut.VB_Description = "Outputs text to the console."
On Error Resume Next
Dim l_strText As String
    If Debugger Is Nothing Then Else Debugger.DebugTextOut ToString(Text)
    l_strText = ToString(Text)
    LogPrint l_strText
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
    #End If
End Sub

Sub Trace(ByRef Text)
Attribute Trace.VB_Description = "Outputs text to the debugging trace window."
On Error Resume Next
    Set frmTrace.Engine = Me
    If frmTrace.Visible Then
    Else
        frmTrace.Show
    End If
    If Len(frmTrace.txtTrace.Text) > 4096 Then
        frmTrace.txtTrace.Text = left(frmTrace.txtTrace.Text, InStrRev(frmTrace.txtTrace.Text, vbCrLf) - 1)
    End If
    frmTrace.txtTrace.Text = ToString(Text) + vbCrLf + frmTrace.txtTrace.Text
    DoEvents
End Sub

Sub TraceOpen()
Attribute TraceOpen.VB_Description = "Opens the debugging window."
On Error Resume Next
    frmTrace.Show
End Sub

Public Sub TriggerInputEvent(EventName As String, ParamArray Args() As Variant)
Attribute TriggerInputEvent.VB_Description = "Triggers an input event by name, with specified arguments."
On Error Resume Next
Dim l_booOldState As Boolean
Dim l_sngMultiplier As Single
    l_booOldState = DisableUserInput
    l_sngMultiplier = (m_objOutputPlugin.Window.ScaleWidth / ScreenWidth)
    Select Case LCase(Trim(EventName))
    Case "mousedown"
        Fury2VideoEventSink_MouseDown2 CInt(Args(0)), CInt(Args(1)), CSng(Args(2)) * l_sngMultiplier, CSng(Args(3)) * l_sngMultiplier, True
    Case "mousemove"
        Fury2VideoEventSink_MouseMove2 CInt(Args(0)), CInt(Args(1)), CSng(Args(2)) * l_sngMultiplier, CSng(Args(3)) * l_sngMultiplier, True
    Case "mouseup"
        Fury2VideoEventSink_MouseUp2 CInt(Args(0)), CInt(Args(1)), CSng(Args(2)) * l_sngMultiplier, CSng(Args(3)) * l_sngMultiplier, True
    Case "keydown"
        Fury2VideoEventSink_KeyDown2 CInt(Args(0)), CInt(Args(1)), True
    Case "keypress"
        Fury2VideoEventSink_KeyPress2 CInt(Args(0)), True
    Case "keyup"
        Fury2VideoEventSink_KeyUp2 CInt(Args(0)), CInt(Args(1)), True
    Case Else
    End Select
    UpdateEvents
End Sub

Public Function GetHookedObjects() As Fury2Collection
On Error Resume Next
Dim l_colNew As Fury2Collection
Dim l_objObject As Object
    Set l_colNew = F2Collection()
    For Each l_objObject In m_colReversePluginStack
        l_colNew.Add l_objObject
    Next l_objObject
    Set GetHookedObjects = l_colNew
End Function

Public Sub SetHookedObjects(ByVal NewHooks As Fury2Collection)
On Error Resume Next
Dim l_objObject As Object
    Set m_colPluginStack = New Collection
    Set m_colReversePluginStack = New Collection
    For Each l_objObject In NewHooks
        If m_colPluginStack.Count > 0 Then
            m_colPluginStack.Add l_objObject, , 1
        Else
            m_colPluginStack.Add l_objObject
        End If
        m_colReversePluginStack.Add l_objObject
    Next l_objObject
End Sub

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
Attribute Unmute.VB_Description = "Unmutes the sound system."
On Error Resume Next
    SoundEngine.MasterVolume = m_sngMutePreviousVolume
End Sub

Public Sub Update()
Attribute Update.VB_Description = "Forces an engine update."
On Error Resume Next
Dim m_sngTimer As Single
Dim m_lngTimers As Long
Dim m_lngHooks As Long
Dim l_camCamera As Fury2Camera
Dim pObj As Fury2Plugin, Obj As Object
Dim m_lngMaps As Long, l_mapMap As Fury2Map
Static m_sngLastTimer As Single
    If (Not Running) And (Not EditMode) Then Exit Sub
    If Halted Then Exit Sub
    If m_objOutputPlugin.Closed Then Fury2VideoEventSink_Closed
    ContextLevelAdd "Update"
    FrameCount = FrameCount + 1
    If Not Paused Then
        m_sngTimer = HiTimer
        If Int(m_sngTimer) <> Int(m_sngLastTimer) Then
            m_sngLastTimer = m_sngTimer
            If GameTimePaused Then
            Else
                ContextLevelAdd "Engine_Tick"
                GameTime = GameTime + 1
                ScriptEngine.Exec "Engine_Tick"
                ContextLevelRemove
            End If
        End If
        m_colTimerEvents.Update
        If Terminating Then Exit Sub
        For Each l_mapMap In Maps
            l_mapMap.TimerEvents.Update
            If Terminating Then Exit Sub
        Next l_mapMap
        If Maps.Count > 0 Then
            Set Map = Maps(1)
            m_lngMaps = 1
            For Each Map In Maps
                If Map.Layers.Count > 0 Then
                    ContextLevelAdd "Maps(" + CStr(m_lngMaps) + ")"
                    Map.Update
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
            For Each Obj In m_colPluginStack
                If Not (Obj Is Nothing) Then
                    Set pObj = Nothing
                    Set pObj = Obj
                    If pObj Is Nothing Then
                        Obj.Update
                    Else
                        pObj.Update
                    End If
                    Err.Clear
                    m_lngHooks = m_lngHooks + 1
                    If Terminating Then Exit Sub
                End If
            Next Obj
        End If
        If Not (UpdateHook Is Nothing) Then
            UpdateHook.Invoke
        End If
    End If
    If Not (SoundListener Is Nothing) Then
        SoundEngine.SetListenerAttributes Array(SoundListener.X, SoundListener.Y, SoundListener.Z), Array(SoundListener.XVelocity * DesiredFramerate, SoundListener.YVelocity * DesiredFramerate, SoundListener.ZVelocity * DesiredFramerate), SoundListener.Orientation
    End If
    SoundEngine.Update
    If m_booConsoleSliding Then
        m_sngConsoleOffset = (m_sngConsoleStartOffset) + ((m_sngConsoleEndOffset - m_sngConsoleStartOffset) * ((HiTimer - m_sngConsoleStartTime) / (m_sngConsoleEndTime - m_sngConsoleStartTime)))
        If m_sngConsoleOffset < 0 Then
            m_sngConsoleOffset = 0
            m_booConsoleSliding = False
        End If
        If m_sngConsoleOffset > ConsoleHeight Then
            m_sngConsoleOffset = ConsoleHeight
            m_booConsoleSliding = False
        End If
    End If
    ContextLevelRemove
End Sub

Public Sub UpdateBackbuffer()
Attribute UpdateBackbuffer.VB_Description = "Copies all cameras to the backbuffer and renders all picture slots."
On Error Resume Next
    Redraw
End Sub

Friend Sub UpdateENet()
On Error Resume Next
Dim l_hosHost As VBENetHost, l_lngIndex As Long
Dim l_lngCount As Long
    If Not m_booENetInitialized Then Exit Sub
    ContextLevelAdd "ENet"
        For Each l_hosHost In Hosts
            l_hosHost.Poll
            l_lngCount = (l_hosHost.Peers.Count + IIf(l_hosHost.TimeSynchronizer Is Nothing, 0, 1))
            l_lngCount = GetReferenceCount(l_hosHost) - l_lngCount
            If l_lngCount <= 5 Then
                DebugOut "Freeing ENet Host"
                l_hosHost.DisconnectAll
                l_hosHost.Flush
                l_hosHost.Free
                l_lngIndex = Hosts.Find(l_hosHost)
                Hosts.Remove l_lngIndex
            End If
        Next l_hosHost
    ContextLevelRemove
End Sub

Friend Sub UpdateEvents()
On Error Resume Next
Dim m_lngQueue As Long
Dim m_lngCount As Long
Dim l_lngBindings As Long, l_lngType As Long
Dim l_lngProcessedCount As Long
Dim m_ctlControl As Fury2InputControl
Dim l_evtEvent As Fury2QueueEvent
    ContextLevelAdd "Controls"
        For Each m_ctlControl In Controls
            m_ctlControl.TriggerRepeat
        Next m_ctlControl
    ContextLevelRemove
    If m_lngEventQueueLength < 1 Then
        Exit Sub
    End If
    ContextLevelAdd "Events"
    m_lngCount = m_lngEventQueueLength
    Do Until m_lngEventQueueLength <= 0 Or m_lngQueue > m_lngEventQueueLength
        l_evtEvent = m_evtEventQueue(m_lngQueue)
        m_evtEventQueue(m_lngQueue).Type = -1
        m_evtEventQueue(m_lngQueue).LParam1 = 0
        m_evtEventQueue(m_lngQueue).LParam2 = 0
        m_evtEventQueue(m_lngQueue).LParam3 = 0
        m_evtEventQueue(m_lngQueue).LParam4 = 0
        If (l_evtEvent.Type <> -1) Then
            l_lngProcessedCount = l_lngProcessedCount + 1
        End If
        With l_evtEvent
            l_lngType = .Type
            Select Case l_lngType
            Case 0 ' KeyDown
                Event_KeyDown l_evtEvent
            Case 1 ' KeyUp
                Event_KeyUp l_evtEvent
            Case 2 ' MouseDown
                Event_MouseDown l_evtEvent
            Case 3 ' MouseMove
                Event_MouseMove l_evtEvent
            Case 4 ' MouseUp
                Event_MouseUp l_evtEvent
            Case -1 ' None
            End Select
        End With
        m_lngQueue = m_lngQueue + 1
        If Terminating Then Exit Do
    Loop
    m_lngEventQueueLength = m_lngEventQueueLength - l_lngProcessedCount
    ContextLevelRemove
    Err.Clear
End Sub

Public Sub UpdateKeyboard()
Attribute UpdateKeyboard.VB_Description = "Updates the state of the keyboard."
On Error Resume Next
    'If InIDE Then Exit Sub
    ContextLevelAdd "UpdateKeyboard"
    Keyboard.Update
    ContextLevelRemove
End Sub

Public Sub UpdatePictures()
Attribute UpdatePictures.VB_MemberFlags = "40"
On Error Resume Next
Dim m_sngX As Single, m_sngY As Single, m_sngW As Single, m_sngH As Single
Dim m_sngProgress As Single, m_sngProgress2 As Single
Dim m_lngPictures As Long, l_objGraph As Fury2Graphic
    If ActivePictureSlots < 1 Then Exit Sub
    For m_lngPictures = 0 To ActivePictureSlots
        With m_picPictures(m_lngPictures)
            If .Picture Is Nothing Then
            Else
                If TypeOf .Picture Is Fury2Graphic Then
                    Set l_objGraph = .Picture
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
                    m_sngX = (.StartRect.left * (m_sngProgress2)) + (.EndRect.left * m_sngProgress)
                    m_sngY = (.StartRect.top * (m_sngProgress2)) + (.EndRect.top * m_sngProgress)
                    m_sngW = (.StartRect.right * (m_sngProgress2)) + (.EndRect.right * m_sngProgress)
                    m_sngH = (.StartRect.bottom * (m_sngProgress2)) + (.EndRect.bottom * m_sngProgress)
                    .DestRect.SetValues CLng(m_sngX), CLng(m_sngY), CLng(m_sngW), CLng(m_sngH)
                End If
            End If
        End With
    Next m_lngPictures
End Sub

Public Sub UpdateActionQueue()
Attribute UpdateActionQueue.VB_Description = "Processes any actions in the action queue."
On Error Resume Next
Dim l_lngCount As Long
Dim l_evtAction As Fury2Event
    l_lngCount = ActionQueue.Count
    Do While l_lngCount > 0
        Set l_evtAction = Nothing
        Set l_evtAction = ActionQueue(1)
        ActionQueue.Remove 1
        l_evtAction.Invoke
        l_lngCount = l_lngCount - 1
    Loop
End Sub

Public Sub UpdateTimers()
Attribute UpdateTimers.VB_MemberFlags = "40"
On Error Resume Next
Dim m_tmrTimer As Fury2Timer
Dim m_lngTimers As Long
Dim m_lngHooks As Long
Dim Obj As Object, pObj As Fury2Plugin
Dim l_gphGraphic As Fury2Graphic
Dim l_vidVideo As Fury2Video
    If EnableTimers Then
        If Timers.Count > 0 Then
            ContextLevelAdd "Timers"
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
            ContextLevelRemove
        End If
    End If
    For Each l_vidVideo In Videos
        l_vidVideo.Update
    Next l_vidVideo
    If m_colPluginStack.Count > 0 Then
        ContextLevelAdd "SyncUpdate"
        For Each Obj In m_colPluginStack
            Err.Clear
            Set pObj = Nothing
            Set pObj = Obj
            If pObj Is Nothing Then
                Obj.SyncUpdate
            Else
                pObj.SyncUpdate
            End If
        Next Obj
        ContextLevelRemove
    End If
    Err.Clear
End Sub

Friend Sub UpdateWindow()
On Error Resume Next
Dim l_lngWindow As Long, l_lngGameWindow
    l_lngWindow = GetForegroundWindow()
    l_lngGameWindow = m_objOutputPlugin.Window.hWnd
    If l_lngWindow <> m_lngLastWindow Then
        m_lngLastWindow = l_lngWindow
        If l_lngWindow <> l_lngGameWindow Then
            Fury2VideoEventSink_LostFocus
        Else
            Fury2VideoEventSink_GotFocus
        End If
    End If
End Sub

Public Sub Wait(Time)
Attribute Wait.VB_Description = "Waits a certain number of seconds. This function will not return until it has finished waiting."
On Error Resume Next
Dim StartTime As Double, EndTime As Double
Dim I As Long
    ContextLevelAdd "Engine.Wait(" + Format(Time, "###.####") + "s)"
    StartTime = HiTimer
    EndTime = HiTimer + CDbl(Time)
    Do While (HiTimer < EndTime) And (Running = True) And (m_booBreak <> True)
        I = I + 1: If I >= 50 Then ContextLevelChange "Engine.Wait(" + Format(EndTime - HiTimer, "#.###") + "s)": I = 0
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

Public Sub Yield(Optional ByVal Sync As Boolean = True)
Attribute Yield.VB_Description = "Pauses the engine for a short amount of time and allows other applications and system processes to execute. Useful in long processing loops."
On Error Resume Next
    If Running Then Else Exit Sub
    If Running = True Then DoEvents
    If HarassCPU Then Else Call SleepEx(1, True)
    If Sync Then UpdateTimers
    UpdateWindow
End Sub

Public Sub Using(ByVal Namespace, Optional ByVal Alias As String = "")
Attribute Using.VB_Description = "Adds the contents of an object into the global namespace."
On Error Resume Next
Dim l_objObject As Object
    If VarType(Namespace) = vbString Then
        If Len(Alias) < 1 Then
            Alias = VariableName(Namespace)
        End If
        Set l_objObject = m_ScriptEngine.Eval(CStr(Namespace))
    Else
        Set l_objObject = Namespace
    End If
    m_ScriptEngine.AddObject Alias, l_objObject, True, True, True
End Sub

Friend Function InvokeMember(ByVal Obj As Object, ByVal Member As Long, Optional ByRef Args As Variant) As Variant
On Error Resume Next
Dim l_varArgs() As Variant
    l_varArgs = Args
    Err.Clear
    InvokeMember = m_ScriptEngine.InvokeMember(Obj, Member, VbMethod, l_varArgs)
End Function

