VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2AIBehavior"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² AI Behavior Object (Scriptable)"
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

Option Explicit
Implements Fury2Object
Implements IVirtualFileSaveable
Public Engine As Fury2Engine
Public Condition As String
Public Class As Fury2SpriteClasses
Public ExcludedClass As Fury2SpriteClasses
Public Target As Object
Public TargetInitializer As String
Public Distance As Single
Public WanderDelay As Long
Public ScriptDelay As Long
Public Chain As Fury2AIBehaviors
Private mvarScript As String
Private mvarScriptObject As Object
Private mvarScriptFailed As Boolean
Private mvarWanderDelay As Long
Private mvarScriptDelay As Long
Private mvarType As Fury2AIBehaviorTypes

Friend Sub RecreateScriptObject()
On Error Resume Next
Dim AIName As String
Dim TheMap As Fury2Map
Dim l_strScript As String
    Err.Clear
    If Trim(mvarScript) = "" Then Exit Sub
    If mvarScriptObject Is Nothing Then
        If (mvarScriptFailed = False) Then
            Err.Clear
            m_Engine.AIObjectCount = m_Engine.AIObjectCount + 1
            AIName = "OAI" + CStr(m_Engine.AIObjectCount)
            With m_Engine.ScriptEngine.Language
                l_strScript = .GenerateClassVariable("Object") & .LineTerminator & _
                .GenerateClassVariable("Map") & .LineTerminator & _
                .GenerateClassVariable("Target") & .LineTerminator & _
                .GenerateClassVariable("Chain") & .LineTerminator & _
                .GenerateClassMethod("F2OInit", Array("T", "M", "S", "C"), _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "T", True) & .LineTerminator & _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "M", True) & .LineTerminator & _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Target", "S", True) & .LineTerminator & _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Chain", "C", True) _
                ) & .LineTerminator & mvarScript
                m_Engine.ScriptEngine.AddCode .GenerateClass(AIName, l_strScript)
                m_Engine.ScriptEngine.AddCode .GenerateAssignment("New" & AIName, .GenerateInstantiation(AIName), True)
                Set mvarScriptObject = m_Engine.ScriptEngine.Eval("New" & AIName)
            End With
            Set m_Engine.IObjThis = Me
            Set TheMap = Nothing
            TheMap = Target.Map
            mvarScriptObject.F2OInit Me, TheMap, Target, Chain
            mvarScriptObject.Initialize
            If (Err.Number <> 0) Or (mvarScriptObject Is Nothing) Then mvarScriptFailed = True
        End If
    End If
    Err.Clear
End Sub

Public Function Duplicate() As Fury2AIBehavior
On Error Resume Next
    Set Duplicate = New Fury2AIBehavior
    With Duplicate
        .AIType = mvarType
        .WanderDelay = mvarWanderDelay
        .Script = Script
        .Distance = Distance
        .TargetInitializer = TargetInitializer
        Set .Target = Target
        .Class = Class
        .Condition = Condition
        .ExcludedClass = ExcludedClass
        Set .Engine = Engine
    End With
End Function

Public Function Class_Options() As Variant
    Class_Options = Array("Normal", "Party Member", "NPC", "Monster", "Object", "Projectile", "Effect", "Item", "Sign")
End Function

Public Function MemberList() As Variant
    If mvarType = F2AI_Script Then
        MemberList = Array("AIType", "Condition", "ScriptSource")
    ElseIf mvarType = F2AI_StayClose Then
        MemberList = Array("AIType", "Condition", "TargetInitializer", "Class", "Distance")
    ElseIf mvarType = F2AI_StayFar Then
        MemberList = Array("AIType", "Condition", "TargetInitializer", "Class", "Distance")
    ElseIf mvarType = F2AI_Wander Then
        MemberList = Array("AIType", "Condition", "WanderDelay")
    Else
        MemberList = Array("AIType", "Condition")
    End If
End Function

Public Property Get Script() As Object
    If mvarScriptObject Is Nothing Then
        RecreateScriptObject
    End If
    Set Script = mvarScriptObject
End Property

Public Property Get ScriptSource() As String
    ScriptSource = mvarScript
End Property

Public Property Let ScriptSource(RHS As String)
    If RHS <> mvarScript Then
        mvarScript = RHS
    End If
End Property

Public Function GetName() As String
On Error Resume Next
    GetName = Choose(mvarType + 1, "None", "Script", "Stay Close", "Stay Far", "Wander", "Seek")
End Function

Public Function AIType_Options() As Variant
    AIType_Options = Array("None", "Script", "Stay Close", "Stay Far", "Wander", "Seek")
End Function

Public Property Get AIType() As Fury2AIBehaviorTypes
On Error Resume Next
    AIType = mvarType
End Property

Public Property Let AIType(NewType As Fury2AIBehaviorTypes)
On Error Resume Next
    mvarType = NewType
End Property

Public Sub Update(Obj As Fury2Sprite)
On Error Resume Next
Dim m_booCondition As Boolean
Dim m_sngAngle As Single
Dim m_sprNeighbor As Fury2Sprite
Dim m_lngNeighbors As Long
Dim m_sngSmallestDistance As Single, m_lngSmallestIndex As Long, m_sngDistance As Single
Dim m_sngX As Single, m_sngY As Single
    Select Case LCase(Trim(Condition))
    Case "true", "1", "-1", "", "always"
        m_booCondition = True
    Case "never", "0", "false", "error"
        m_booCondition = False
    Case Else
        m_booCondition = CBool(mvarScriptObject.Condition)
    End Select
    If m_booCondition = False Then Exit Sub
    Select Case mvarType
    Case F2AI_None
        Exit Sub
    Case F2AI_Script
        If mvarScript = "" Then Exit Sub
        If ScriptDelay = -1 Then Exit Sub
        mvarScriptDelay = mvarScriptDelay + 1
        If mvarScriptDelay >= ScriptDelay Then
            mvarScriptDelay = 0
            If mvarScriptObject Is Nothing Then RecreateScriptObject
            Err.Clear
            mvarScriptObject.Update
            If Err <> 0 Then Condition = "Error"
        End If
    Case F2AI_StayClose
        m_sngSmallestDistance = Distance
        m_lngSmallestIndex = -1
        If Target Is Nothing Then
            Set m_sprNeighbor = Obj.Parent.Layers(Obj.Layer).Sprites.GetFarthest(Obj.Index, False, Class, ExcludedClass, m_sngDistance)
            If (m_sprNeighbor Is Nothing) Or (m_sngDistance <= Distance) Then
            Else
                With m_sprNeighbor
                    Obj.Path.Reset
                    m_Engine.PathTarget Obj.X, Obj.y, m_Engine.AngleBetween(Obj.X, Obj.y, .X, .y), (m_sngDistance - Distance) + (Rnd * 3), m_sngX, m_sngY
                    Obj.Path.Add m_sngX, m_sngY
                    Obj.FollowPath = True
                End With
            End If
        Else
            m_sngDistance = m_Engine.DistanceBetween(Obj.X, Obj.y, Target.X, Target.y)
            If (m_sngDistance - Distance) >= 1 Then
                Obj.Path.Reset
                m_Engine.PathTarget Obj.X, Obj.y, m_Engine.AngleBetween(Obj.X, Obj.y, Target.X, Target.y), (m_sngDistance - Distance) + (Rnd * 3), m_sngX, m_sngY
                Obj.Path.Add m_sngX, m_sngY
                Obj.FollowPath = True
            End If
        End If
    Case F2AI_StayFar
        m_sngSmallestDistance = Distance
        m_lngSmallestIndex = -1
        If Target Is Nothing Then
            Set m_sprNeighbor = Obj.Parent.Layers(Obj.Layer).Sprites.GetClosest(Obj.Index, False, Class, ExcludedClass, m_sngDistance)
            If m_sprNeighbor Is Nothing Or (m_sngDistance >= Distance) Then
            Else
                With m_sprNeighbor
                Obj.Path.Reset
                'm_Engine.PathTarget .x, .y, m_Engine.AngleBetween(Obj.x, Obj.y, .x, .y) - 180, m_sngSmallestDistance, m_sngX, m_sngY
                m_sngX = .X - Obj.X
                m_sngY = .y - Obj.y
                Obj.Path.Add (Obj.X - m_sngX) * 1.05, (Obj.y - m_sngY) * 1.05
                Obj.FollowPath = True
                End With
            End If
        Else
            With Target
                Obj.Path.Reset
                'm_Engine.PathTarget .x, .y, m_Engine.AngleBetween(Obj.x, Obj.y, .x, .y) - 180, m_sngSmallestDistance, m_sngX, m_sngY
                m_sngX = .X - Obj.X
                m_sngY = .y - Obj.y
                Obj.Path.Add (Obj.X - m_sngX) * 1.05, (Obj.y - m_sngY) * 1.05
                Obj.FollowPath = True
            End With
        End If
    Case F2AI_Wander
        mvarWanderDelay = mvarWanderDelay + 1
        If mvarWanderDelay >= WanderDelay Then
            mvarWanderDelay = 0
            Obj.Angle = Rnd * 359
            Obj.Velocity = Rnd * Obj.WalkSpeed
        End If
    Case F2AI_Seek
        m_sngSmallestDistance = 999999999
        m_lngSmallestIndex = -1
        If Target Is Nothing Then
            For m_lngNeighbors = 1 To Obj.Parent.Sprites.Count
                Set m_sprNeighbor = Obj.Parent.Sprites(m_lngNeighbors)
                If (m_sprNeighbor Is Obj) Or (m_sprNeighbor Is Nothing) Then
                Else
                    With m_sprNeighbor
                        If .Class = Class Then
                            m_sngDistance = m_Engine.DistanceBetween(Obj.X, Obj.y, .X, .y)
                            If m_sngDistance < m_sngSmallestDistance Then
                                m_sngSmallestDistance = m_sngDistance
                                m_lngSmallestIndex = m_lngNeighbors
                            End If
                        End If
                    End With
                End If
            Next m_lngNeighbors
            If m_lngSmallestIndex = -1 Then
            Else
                With Obj.Parent.Sprites(m_lngSmallestIndex)
                    Obj.Path.Reset
                    Obj.Path.Add .X, .y
                    Obj.FollowPath = True
                End With
            End If
        Else
            Obj.Path.Reset
            Obj.Path.Add Target.X, Target.y
            Obj.FollowPath = True
        End If
    Case Else
        Exit Sub
    End Select
End Sub

Private Sub Class_Initialize()
    Class = -1
    ExcludedClass = -1
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load mvarType
        .Load Condition
        .Load Class
        If mvarType = F2AI_None Then
        ElseIf mvarType = F2AI_Script Then
            .Load mvarScript
            .Load ScriptDelay
        ElseIf mvarType = F2AI_StayClose Then
            .Load Distance
            .Load TargetInitializer
        ElseIf mvarType = F2AI_StayFar Then
            .Load Distance
            .Load TargetInitializer
        ElseIf mvarType = F2AI_Wander Then
            .Load WanderDelay
        ElseIf mvarType = F2AI_Seek Then
        Else
        End If
        .Load TargetInitializer
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save mvarType
        .Save Condition
        .Save Class
        If mvarType = F2AI_None Then
        ElseIf mvarType = F2AI_Script Then
            .Save mvarScript
            .Save ScriptDelay
        ElseIf mvarType = F2AI_StayClose Then
            .Save Distance
            .Save TargetInitializer
        ElseIf mvarType = F2AI_StayFar Then
            .Save Distance
            .Save TargetInitializer
        ElseIf mvarType = F2AI_Wander Then
            .Save WanderDelay
        ElseIf mvarType = F2AI_Seek Then
        Else
        End If
        .Save TargetInitializer
    End With
End Sub

