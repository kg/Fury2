VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ScriptBehavior"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Scripted Object Behavior"
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
Implements Fury2Behavior
Implements Fury2Object
Implements IVirtualFileSaveable
Public Engine As Fury2Engine
Public Obj As Object
Public Script As Object
Public ScriptSource As String

Private Sub InitScript()
On Error Resume Next
Dim BehaviorName As String
Dim l_strScript As String
    Err.Clear
    If Trim(ScriptSource) = "" Then Exit Sub
    If Script Is Nothing Then
        Err.Clear
        m_Engine.BehaviorObjectCount = m_Engine.BehaviorObjectCount + 1
        BehaviorName = "IObjBehavior" + CStr(m_Engine.BehaviorObjectCount)
        With m_Engine.ScriptEngine.Language
            l_strScript = .GenerateClassVariable("Behavior") & .LineTerminator & _
            .GenerateClassVariable("Object") & .LineTerminator & _
            .GenerateClassMethod("F2OInit", Array("T", "O"), _
            .GenerateAssignment(.ThisKeyword & .MethodOperator & "Behavior", "T", True) & .LineTerminator & _
            .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "O", True) & .LineTerminator _
            ) & .LineTerminator & ScriptSource
            m_Engine.ScriptEngine.AddCode .GenerateClass(BehaviorName, l_strScript)
            m_Engine.ScriptEngine.AddCode .GenerateAssignment("New" & BehaviorName, .GenerateInstantiation(BehaviorName), True)
            Set Script = m_Engine.ScriptEngine.Eval("New" & BehaviorName)
        End With
        Set m_Engine.IObjThis = Me
        Script.F2OInit Me, Obj
        Script.Initialize
    End If
End Sub

Private Sub Fury2Behavior_Attach(Obj As Object)
    Behavior_Attach Me, Obj
End Sub

Private Sub Fury2Behavior_Detach()
    Behavior_Detach Me
End Sub

Private Property Set Fury2Behavior_Engine(ByVal RHS As Fury2Engine)
    Set Engine = RHS
End Property

Private Property Get Fury2Behavior_Engine() As Fury2Engine
    Set Fury2Behavior_Engine = Engine
End Property

Private Property Get Fury2Behavior_O() As Object
    Set Fury2Behavior_O = Me
End Property

Private Property Set Fury2Behavior_Obj(RHS As Object)
    Set Obj = RHS
End Property

Private Property Get Fury2Behavior_Obj() As Object
    Set Fury2Behavior_Obj = Obj
End Property

Private Sub Fury2Behavior_Update()
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
    With File
        .Save ScriptSource
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
    With File
        .Load ScriptSource
    End With
End Sub
