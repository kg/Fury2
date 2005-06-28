VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2PoseFrame"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Pose::Frame Object"
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
Implements IInspectable
Implements Fury2Object
Implements IVirtualFileSaveable

Private mvarRectangle As Fury2Rect
Private mvarXCenter As Single
Private mvarYCenter As Single
Private mvarFilename As String
Private mvarMatteColor As Long
Private mvarDelay As Long
Private mvarImage As Fury2Image
Private mvarScript As String
Private mvarParent As Fury2Pose
Private mvarIndex As Long

Friend Sub SetIndex(ByVal Index As Long)
On Error Resume Next
    mvarIndex = Index
End Sub

Public Property Get Index() As Long
    Index = mvarIndex
End Property

Public Property Get Parent() As Fury2Pose
    Set Parent = mvarParent
End Property

Friend Sub SetParent(ByRef Parent As Fury2Pose)
On Error Resume Next
    Set mvarParent = Parent
End Sub

Public Sub RunScript()
On Error Resume Next
    If Len(mvarScript) < 1 Then Exit Sub
    With mvarParent.Parent.Engine.ScriptEngine
        Set mvarParent.Parent.Engine.IObjThis = mvarParent.Parent
        .AddCode .Language.GenerateAssignment("Object", "IObjThis", True)
        .AddCode mvarScript
        Set mvarParent.Parent.Engine.IObjThis = Nothing
        .AddCode .Language.GenerateAssignment("Object", "IObjThis", True)
    End With
End Sub

Public Property Get Script() As String
Attribute Script.VB_Description = "*The script to execute when the frame is played."
    Script = mvarScript
End Property

Public Property Let Script(ByRef Value As String)
    mvarScript = Value
End Property

Public Sub LoadGraphics()
On Error Resume Next
Dim l_varVariables As Variant
Dim l_strFilename As String
    l_varVariables = Array(Array("i", mvarIndex), Array("index", mvarIndex), _
        Array("p", mvarParent.Index), Array("pose", mvarParent.Index), _
        Array("pn", mvarParent.Name), Array("posename", mvarParent.Name), _
        Array("n", mvarParent.Parent.Name), Array("name", mvarParent.Parent.Name))
    l_strFilename = mvarParent.Parent.Engine.ParseString(mvarFilename, l_varVariables)
    If Not mvarImage Is Nothing Then
        If mvarImage.Name <> Filename Then
            Set mvarImage = Nothing
        End If
    End If
    If mvarImage Is Nothing Then
        Set mvarImage = mvarParent.Parent.Engine.LoadImage(l_strFilename)
    End If
    If mvarImage Is Nothing Then
        mvarParent.Parent.Engine.CriticalError "Fury2PoseFrame.LoadGraphics", "Unable to load frame: " + l_strFilename
    Else
        mvarImage.MatteColor = mvarMatteColor
    End If
End Sub

Sub Free()
Attribute Free.VB_MemberFlags = "40"
On Error Resume Next
    Set mvarImage = Nothing
End Sub

Public Function Duplicate() As Fury2PoseFrame
Attribute Duplicate.VB_Description = "Creates a copy of the frame."
On Error Resume Next
    Set Duplicate = New Fury2PoseFrame
    With Duplicate
        .MatteColor = mvarMatteColor
        Set .Rectangle = mvarRectangle.Copy
        Set .Image = mvarImage
        .SetFilename mvarFilename
        .XCenter = mvarXCenter
        .YCenter = mvarYCenter
        .Delay = mvarDelay
        .Script = mvarScript
    End With
End Function

Friend Sub SetImage(Img As Fury2Image)
    Set mvarImage = Img
End Sub

Public Property Set Image(RHS As Fury2Image)
    Set mvarImage = RHS
End Property

Public Property Get Image() As Fury2Image
Attribute Image.VB_Description = "*The image object that contains the frame."
    Set Image = mvarImage
End Property

Public Property Let MatteColor(ByVal vData As Long)
Attribute MatteColor.VB_Description = "The color that is shown as transparent in non-alpha-channel blit modes.{Color}"
    mvarMatteColor = vData
    If mvarImage Is Nothing Then Else mvarImage.MatteColor = mvarMatteColor
End Property

Public Property Get MatteColor() As Long
    MatteColor = mvarMatteColor
End Property

Public Property Let MaskColor(ByVal vData As Long)
Attribute MaskColor.VB_Description = "*"
Attribute MaskColor.VB_MemberFlags = "40"
    mvarMatteColor = vData
    If mvarImage Is Nothing Then Else mvarImage.MatteColor = mvarMatteColor
End Property

Public Property Get MaskColor() As Long
    MaskColor = mvarMatteColor
End Property

Friend Sub SetFilename(ByRef Filename As String)
On Error Resume Next
    mvarFilename = Filename
End Sub

Public Property Let Filename(ByVal vData As String)
Attribute Filename.VB_Description = "The name of the file the frame is stored in.{ImagePath}"
On Error Resume Next
    If mvarFilename = vData Then Exit Property
    mvarFilename = vData
    Set mvarImage = Nothing
    LoadGraphics
End Property

Public Property Get Filename() As String
    Filename = mvarFilename
End Property

Public Property Let YCenter(ByVal vData As Single)
Attribute YCenter.VB_Description = "The Y coordinate of the 'center' or 'alignment point' for the image."
    mvarYCenter = vData
End Property

Public Property Get YCenter() As Single
    YCenter = mvarYCenter
End Property

Public Property Let Delay(ByVal vData As Long)
Attribute Delay.VB_Description = "The number of updates this frame will last."
    mvarDelay = vData
End Property

Public Property Get Delay() As Long
    Delay = mvarDelay
End Property

Public Property Let XCenter(ByVal vData As Single)
Attribute XCenter.VB_Description = "The X coordinate of the 'center' or 'alignment point' for the image."
    mvarXCenter = vData
End Property

Public Property Get XCenter() As Single
    XCenter = mvarXCenter
End Property

Public Property Set Rectangle(ByVal vData As Fury2Rect)
Attribute Rectangle.VB_Description = "~The coordinates of the frame inside the image it is stored in."
    Set mvarRectangle = vData
End Property

Public Property Get Rectangle() As Fury2Rect
    Set Rectangle = mvarRectangle
End Property

Private Sub Class_Initialize()
    Set mvarRectangle = New Fury2Rect
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
        .Load mvarMatteColor
        .Load mvarFilename
        .Load mvarRectangle
        .Load mvarXCenter
        .Load mvarYCenter
        .Load mvarDelay
        .Load mvarScript
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
        .Save mvarMatteColor
        .Save mvarFilename
        .Save mvarRectangle
        .Save mvarXCenter
        .Save mvarYCenter
        .Save mvarDelay
        .Save mvarScript
        .WriteSegment_End
    End With
End Sub
