VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Area"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Area Object (Scriptable)"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
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

Implements IInspectable
Implements Fury2Object
Implements IVirtualFileSaveable

Private mvarScriptFailed As Boolean
Public ScriptSource As String
Attribute ScriptSource.VB_VarDescription = "The source code of the object's script."
Public Index As Long
Attribute Index.VB_VarDescription = "The area's numeric index in the parent Fury2Areas collection."
Private m_objScript As Object
Private mvarX As Long
Private mvarY As Long
Private mvarWidth As Long
Private mvarHeight As Long
Private mvarMap As Fury2Map
Private mvarMoveInterval As Long
Private mvarRandomChance As Single
Private mvarNamePattern As String
Private mvarName As String
Private m_objScript_Activate As Boolean
Private m_objScript_Enter As Boolean
Private m_objScript_Move As Boolean
Private m_objScript_Leave As Boolean
Private m_objScript_Load As Boolean
Private Type StringUDT
    Length As Long
    Text As String
End Type
Private m_lngID_Activate As Long
Private m_lngID_Enter As Long
Private m_lngID_Move As Long
Private m_lngID_Leave As Long
Private m_lngID_Load As Long

Friend Sub BindScript()
On Error Resume Next
    m_lngID_Activate = mvarMap.Engine.ScriptEngine.ResolveMember(m_objScript, "Activate")
    m_lngID_Load = mvarMap.Engine.ScriptEngine.ResolveMember(m_objScript, "Load")
    m_lngID_Enter = mvarMap.Engine.ScriptEngine.ResolveMember(m_objScript, "Enter")
    m_lngID_Move = mvarMap.Engine.ScriptEngine.ResolveMember(m_objScript, "Move")
    m_lngID_Leave = mvarMap.Engine.ScriptEngine.ResolveMember(m_objScript, "Leave")
End Sub

Public Function Duplicate() As Fury2Area
On Error Resume Next
    Set Duplicate = New Fury2Area
    With Duplicate
        .ScriptSource = ScriptSource
        .X = X
        .Y = Y
        .Width = Width
        .Height = Height
        .MoveInterval = MoveInterval
        .RandomChance = RandomChance
        .NamePattern = NamePattern
        .Name = Name
    End With
End Function

Public Sub Free()
On Error Resume Next
    Set mvarMap = Nothing
    m_objScript.F2OInit Nothing, Nothing
    Set m_objScript = Nothing
End Sub

Public Function PointInside(ByVal X As Long, ByVal Y As Long) As Boolean
Attribute PointInside.VB_Description = "Returns true if the specified location is inside the area."
On Error Resume Next
    If X < mvarX Then Exit Function
    If Y < mvarY Then Exit Function
    If X >= (mvarX + mvarWidth) Then Exit Function
    If Y >= (mvarY + mvarHeight) Then Exit Function
    PointInside = True
End Function
    
Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Area: " & mvarName
End Function

Friend Sub SetParent(Parent As Fury2Map)
    Set mvarMap = Parent
End Sub
    
Function SupportsAction(ByVal Name As String) As Boolean
Attribute SupportsAction.VB_Description = "Returns True if the object's script supports a given action."
On Error Resume Next
    With mvarMap.Engine.ScriptEngine.Language
        Select Case LCase(Trim(Name))
        Case "enter"
            SupportsAction = m_objScript_Enter
        Case "leave"
            SupportsAction = m_objScript_Leave
        Case "move"
            SupportsAction = m_objScript_Move
        Case "activate"
            SupportsAction = m_objScript_Activate
        Case Else
            SupportsAction = .ImplementsMethod(ScriptSource, Name)
        End Select
    End With
    Err.Clear
End Function
    
Function Action(Optional ByVal Activator As Object, Optional ByVal ActionName As String = "Activate") As Boolean
Attribute Action.VB_Description = "Invokes a given action on the object's script."
On Error Resume Next
Dim l_lngID As Long
    mvarMap.Engine.ContextLevelAdd "Area_" + ActionName
    ActionName = LCase(Trim(ActionName))
    Select Case ActionName
    Case "enter"
        If Not m_objScript_Enter Then
            Action = False
            mvarMap.Engine.ContextLevelRemove
            Exit Function
        End If
        l_lngID = m_lngID_Enter
    Case "leave"
        If Not m_objScript_Leave Then
            Action = False
            mvarMap.Engine.ContextLevelRemove
            Exit Function
        End If
        l_lngID = m_lngID_Leave
    Case "activate"
        If Not m_objScript_Activate Then
            Action = False
            mvarMap.Engine.ContextLevelRemove
            Exit Function
        End If
        l_lngID = m_lngID_Activate
    Case "move"
        If Not m_objScript_Move Then
            Action = False
            mvarMap.Engine.ContextLevelRemove
            Exit Function
        End If
        l_lngID = m_lngID_Move
    Case Else
    End Select
    If Not (Script Is Nothing) Then
        Action = True
        If l_lngID <> -1 Then
            Err.Clear
            mvarMap.Engine.InvokeMember m_objScript, l_lngID, Array(Activator)
            If Err.Number <> 0 Then
                Err.Clear
                mvarMap.Engine.InvokeMember m_objScript, l_lngID
                If Err <> 0 Then
                    Action = False
                End If
            End If
        Else
            Err.Clear
            CallByName Script, ActionName, VbMethod, Activator
            If Err.Number <> 0 Then
                Err.Clear
                CallByName Script, ActionName, VbMethod
                If Err.Number <> 0 Then Action = False
            End If
        End If
    End If
    mvarMap.Engine.ContextLevelRemove
End Function

Public Property Get Script() As Object
Attribute Script.VB_Description = "The object's script."
On Error Resume Next
Dim l_strScript As String, l_strName As String
    Err.Clear
    If Trim(ScriptSource) = "" Then Exit Property
    If m_objScript Is Nothing Then
        If (mvarScriptFailed = False) Then
            Err.Clear
            If left(Trim(m_strScript), 2) = "<@" Then
                l_strScript = Trim(m_strScript)
                l_strName = Mid(l_strScript, 3, Len(l_strScript) - 3)
                Set m_objScript = mvarMap.Engine.ScriptEngine.EvalObject(l_strName)
                m_objScript_Load = m_objScript.ImplementsMethod("Load")
                m_objScript_Activate = m_objScript.ImplementsMethod("Activate")
                m_objScript_Enter = m_objScript.ImplementsMethod("Enter")
                m_objScript_Move = m_objScript.ImplementsMethod("Move")
                m_objScript_Leave = m_objScript.ImplementsMethod("Leave")
                BindScript
            Else
                With mvarMap.Engine.ScriptEngine.Language
                    l_strScript = .GenerateClassVariable("Object") & .LineTerminator & _
                    .GenerateClassVariable("Map") & .LineTerminator & _
                    .GenerateClassMethod("F2OInit", Array("newThis", "newMap"), _
                    .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "newThis", True) & .LineTerminator & _
                    .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "newMap", True) _
                    ) & .LineTerminator & ScriptSource
                    m_objScript_Load = .ImplementsMethod(l_strScript, "Load")
                    m_objScript_Activate = .ImplementsMethod(l_strScript, "Activate")
                    m_objScript_Enter = .ImplementsMethod(l_strScript, "Enter")
                    m_objScript_Move = .ImplementsMethod(l_strScript, "Move")
                    m_objScript_Leave = .ImplementsMethod(l_strScript, "Leave")
                    Set m_objScript = mvarMap.Engine.GenerateScriptObject(l_strScript)
                    BindScript
                End With
            End If
            Set mvarMap.Engine.IObjThis = Me
            m_objScript.F2OInit Me, mvarMap
            m_objScript.Initialize
            If (m_objScript Is Nothing) Then
                mvarMap.Engine.DebugOut "Unable to load script for area """ & Name & """"
                mvarScriptFailed = True
            End If
        End If
    End If
    Set Script = m_objScript
    Err.Clear
End Property

Public Property Let X(ByVal vData As Long)
Attribute X.VB_Description = "The X coordinate of the area, in pixels."
    mvarX = vData
End Property

Public Property Get X() As Long
    X = mvarX
End Property

Public Property Let Y(ByVal vData As Long)
Attribute Y.VB_Description = "The Y coordinate of the area, in pixels."
    mvarY = vData
End Property

Public Property Get Y() As Long
    Y = mvarY
End Property

Public Property Let Width(ByVal vData As Long)
Attribute Width.VB_Description = "The width of the area, in pixels."
    mvarWidth = vData
End Property

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Let Height(ByVal vData As Long)
Attribute Height.VB_Description = "The height of the area, in pixels."
    mvarHeight = vData
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Public Property Set Rectangle(vData As Fury2Rect)
Attribute Rectangle.VB_Description = "The rectangle of the area."
    With vData
        mvarX = .left
        mvarY = .top
        mvarWidth = .Width
        mvarHeight = .Height
    End With
End Property

Public Property Get MoveInterval() As Long
Attribute MoveInterval.VB_Description = "Determines how often (in frames) individual Move events are fired."
    MoveInterval = mvarMoveInterval
End Property

Public Property Let MoveInterval(ByVal NewInterval As Long)
    mvarMoveInterval = NewInterval
End Property

Public Property Get RandomChance() As Single
Attribute RandomChance.VB_Description = "Specifies a random chance for the Move event to fire."
    RandomChance = mvarRandomChance
End Property

Public Property Let RandomChance(ByVal NewChance As Single)
    mvarRandomChance = NewChance
End Property

Public Property Get Name() As String
Attribute Name.VB_Description = "The name of the area."
    Name = mvarName
End Property

Public Property Let Name(ByVal NewName As String)
    mvarName = NewName
End Property

Public Property Get NamePattern() As String
Attribute NamePattern.VB_Description = "Specifies an optional mask to apply to sprites before firing events for them."
    NamePattern = mvarNamePattern
End Property

Public Property Let NamePattern(ByVal NewNamePattern As String)
    mvarNamePattern = NewNamePattern
End Property

Public Property Get Rectangle() As Fury2Rect
    Set Rectangle = F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False)
End Property

Friend Sub DualIntersect(ByRef Rectangle1 As Rectangle, ByRef Rectangle2 As Rectangle, ByRef Value1 As Boolean, ByRef Value2 As Boolean)
On Error Resume Next
    With Rectangle1
        If mvarX <= (.left + .Width) Then
            If mvarY <= (.top + .Height) Then
                If (mvarX + mvarWidth) >= .left Then
                    If (mvarY + mvarHeight) >= .top Then
                        Value1 = True
                    End If
                End If
            End If
        End If
    End With
    With Rectangle2
        If mvarX <= (.left + .Width) Then
            If mvarY <= (.top + .Height) Then
                If (mvarX + mvarWidth) >= .left Then
                    If (mvarY + mvarHeight) >= .top Then
                        Value2 = True
                    End If
                End If
            End If
        End If
    End With
End Sub

Friend Property Get Script_Activate() As Boolean
On Error Resume Next
    Script_Activate = m_objScript_Activate
End Property

Friend Property Get Script_Enter() As Boolean
On Error Resume Next
    Script_Enter = m_objScript_Enter
End Property

Friend Property Get Script_Move() As Boolean
On Error Resume Next
    Script_Move = m_objScript_Move
End Property

Friend Property Get Script_Leave() As Boolean
On Error Resume Next
    Script_Leave = m_objScript_Leave
End Property

Private Sub Class_Initialize()
    mvarRandomChance = 1
    mvarName = "Untitled"
    mvarNamePattern = "*"
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
            .Load mvarX
            .Load mvarY
            .Load mvarWidth
            .Load mvarHeight
            .Load ScriptSource
            .Load mvarRandomChance
            .Load mvarName
            If mvarWidth < 0 Then
                mvarX = mvarX + mvarWidth
                mvarWidth = -mvarWidth
            End If
            If mvarHeight < 0 Then
                mvarY = mvarY + mvarHeight
                mvarHeight = -mvarHeight
            End If
            .Load mvarMoveInterval
            If .EOS Then
                mvarNamePattern = "*"
            Else
                .Load mvarNamePattern
            End If
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarX
            .Save mvarY
            .Save mvarWidth
            .Save mvarHeight
            .Save ScriptSource
            .Save mvarRandomChance
            .Save mvarName
            .Save mvarMoveInterval
            .Save mvarNamePattern
        .WriteSegment_End
    End With
End Sub

