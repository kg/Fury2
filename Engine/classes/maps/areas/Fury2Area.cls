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
Public Index As Long
Private mvarScript As Object
Private mvarX As Long
Private mvarY As Long
Private mvarWidth As Long
Private mvarHeight As Long
Private mvarMap As Fury2Map
Private mvarRandomChance As Single
Private mvarName As String
Private mvarScript_Activate As Boolean
Private mvarScript_Enter As Boolean
Private mvarScript_Move As Boolean
Private mvarScript_Leave As Boolean
Private mvarScript_Load As Boolean
Private Type StringUDT
    Length As Long
    Text As String
End Type

Public Function PointInside(ByVal X As Long, ByVal Y As Long) As Boolean
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
On Error Resume Next
    With m_Engine.ScriptEngine.Language
        Select Case LCase(Trim(Name))
        Case "enter"
            SupportsAction = mvarScript_Enter
        Case "leave"
            SupportsAction = mvarScript_Leave
        Case "move"
            SupportsAction = mvarScript_Move
        Case "activate"
            SupportsAction = mvarScript_Activate
        Case Else
            SupportsAction = .ImplementsMethod(ScriptSource, Name)
        End Select
    End With
    Err.Clear
End Function
    
Function Action(Optional ByRef Activator As Object, Optional ByVal ActionName As String = "Activate") As Boolean
On Error Resume Next
    m_Engine.ContextLevelAdd "Area_" + ActionName
    ActionName = LCase(Trim(ActionName))
    Select Case ActionName
    Case "enter"
        If Not mvarScript_Enter Then
            Action = False
            m_Engine.ContextLevelRemove
            Exit Function
        End If
    Case "leave"
        If Not mvarScript_Leave Then
            Action = False
            m_Engine.ContextLevelRemove
            Exit Function
        End If
    Case "activate"
        If Not mvarScript_Activate Then
            Action = False
            m_Engine.ContextLevelRemove
            Exit Function
        End If
    Case "move"
        If Not mvarScript_Move Then
            Action = False
            m_Engine.ContextLevelRemove
            Exit Function
        End If
    Case Else
    End Select
    Action = True
    If Script Is Nothing Then Else CallByName Script, ActionName, VbMethod, Activator
    If Err.Number <> 0 Then
        Err.Clear
        If Script Is Nothing Then Else CallByName Script, ActionName, VbMethod
        If Err.Number <> 0 Then Action = False
    End If
    m_Engine.ContextLevelRemove
End Function

Public Property Get Script() As Object
On Error Resume Next
Dim AreaName As String
Dim l_strScript As String
    Err.Clear
    If Trim(ScriptSource) = "" Then Exit Property
    If mvarScript Is Nothing Then
        If (mvarScriptFailed = False) Then
            Err.Clear
            m_Engine.AreaObjectCount = m_Engine.AreaObjectCount + 1
            AreaName = m_Engine.VariableName(mvarMap.Filename + "_A" & Index)
            With m_Engine.ScriptEngine.Language
                l_strScript = .GenerateClassVariable("Object") & .LineTerminator & _
                .GenerateClassVariable("Map") & .LineTerminator & _
                .GenerateClassMethod("F2OInit", Array("T", "M"), _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "T", True) & .LineTerminator & _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "M", True) _
                ) & .LineTerminator & ScriptSource
                mvarScript_Load = .ImplementsMethod(l_strScript, "Load")
                mvarScript_Activate = .ImplementsMethod(l_strScript, "Activate")
                mvarScript_Enter = .ImplementsMethod(l_strScript, "Enter")
                mvarScript_Move = .ImplementsMethod(l_strScript, "Move")
                mvarScript_Leave = .ImplementsMethod(l_strScript, "Leave")
                m_Engine.CacheScript mvarMap.Filename + "_A" & Index, .GenerateClass(AreaName, l_strScript)
                m_Engine.ScriptEngine.AddCode .GenerateAssignment("New" & AreaName, .GenerateInstantiation(AreaName), True)
                Set mvarScript = m_Engine.ScriptEngine.Eval("New" & AreaName)
            End With
            Set m_Engine.IObjThis = Me
            mvarScript.F2OInit Me, mvarMap
            mvarScript.Initialize
            If (Err.Number <> 0) Or (mvarScript Is Nothing) Then mvarScriptFailed = True
        End If
    End If
    Set Script = mvarScript
    Err.Clear
End Property

Public Property Let X(vData As Long)
    mvarX = vData
End Property

Public Property Get X() As Long
    X = mvarX
End Property

Public Property Let Y(vData As Long)
    mvarY = vData
End Property

Public Property Get Y() As Long
    Y = mvarY
End Property

Public Property Let Width(vData As Long)
    mvarWidth = vData
End Property

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Let Height(vData As Long)
    mvarHeight = vData
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Public Property Set Rectangle(vData As Fury2Rect)
    With vData
        mvarX = .Left
        mvarY = .Top
        mvarWidth = .Width
        mvarHeight = .Height
    End With
End Property

Public Property Get RandomChance() As Single
    RandomChance = mvarRandomChance
End Property

Public Property Let RandomChance(NewChance As Single)
    mvarRandomChance = NewChance
End Property

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Let Name(NewName As String)
    mvarName = NewName
End Property

Public Property Get Rectangle() As Fury2Rect
    Set Rectangle = F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False)
End Property

Friend Sub DualIntersect(ByRef Rectangle1 As Rectangle, ByRef Rectangle2 As Rectangle, ByRef Value1 As Boolean, ByRef Value2 As Boolean)
On Error Resume Next
    With Rectangle1
        Value1 = False
        If mvarX > (.Left + .Width) Then
        Else
            If mvarY > (.Top + .Height) Then
            Else
                If (mvarX + mvarWidth) < .Left Then
                Else
                    If (mvarY + mvarHeight) < .Top Then
                    Else
                        Value1 = True
                    End If
                End If
            End If
        End If
    End With
    With Rectangle2
        Value2 = False
        If mvarX > (.Left + .Width) Then
        Else
            If mvarY > (.Top + .Height) Then
            Else
                If (mvarX + mvarWidth) < .Left Then
                Else
                    If (mvarY + mvarHeight) < .Top Then
                    Else
                        Value2 = True
                    End If
                End If
            End If
        End If
    End With
End Sub

Friend Property Get Script_Activate() As Boolean
On Error Resume Next
    Script_Activate = mvarScript_Activate
End Property

Friend Property Get Script_Enter() As Boolean
On Error Resume Next
    Script_Enter = mvarScript_Enter
End Property

Friend Property Get Script_Move() As Boolean
On Error Resume Next
    Script_Move = mvarScript_Move
End Property

Friend Property Get Script_Leave() As Boolean
On Error Resume Next
    Script_Leave = mvarScript_Leave
End Property

Private Sub Class_Initialize()
    mvarRandomChance = 1
    mvarName = "Untitled"
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
        .WriteSegment_End
    End With
End Sub

