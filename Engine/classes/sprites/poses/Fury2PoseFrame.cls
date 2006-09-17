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

Private WithEvents mvarSecondaryImages As Fury2Collection
Attribute mvarSecondaryImages.VB_VarHelpID = -1
Private m_siSecondaryImageParams() As SpriteSecondaryImage
Private mvarRectangle As Fury2Rect
Private mvarXCenter As Single
Private mvarYCenter As Single
Private mvarFilename As String
Private mvarMatteColor As Long
Private mvarDelay As Long
Private mvarImage As Fury2Image
Private mvarScript As String
Private mvarScriptBlock As Fury2Event
Private mvarParent As WeakReference
Private mvarIndex As Long

Public Function AddSecondaryImage(Optional ByRef Filename As String = "", Optional ByVal ImageType As Fury2SecondaryImageTypes = F2SI_Overlay) As Fury2SecondaryImage
Attribute AddSecondaryImage.VB_Description = "Adds a new secondary image to this frame."
On Error Resume Next
Dim l_siImage As Fury2SecondaryImage
    Set l_siImage = New Fury2SecondaryImage
    l_siImage.SetParent Me
    l_siImage.ImageType = ImageType
    l_siImage.Filename = Filename
    mvarSecondaryImages.Add l_siImage
    Set AddSecondaryImage = l_siImage
End Function

Friend Property Get SecondaryImagePointer() As Long
On Error Resume Next
    SecondaryImagePointer = VarPtr(m_siSecondaryImageParams(0))
End Property

Friend Sub SetIndex(ByVal Index As Long)
On Error Resume Next
    mvarIndex = Index
End Sub

Public Property Get Index() As Long
Attribute Index.VB_Description = "The index of this frame."
    Index = mvarIndex
End Property

Public Property Get Parent() As Fury2Pose
Attribute Parent.VB_Description = "The pose this frame belongs to."
    Set Parent = WRGetPose(mvarParent)
End Property

Friend Sub SetParent(ByRef Parent As Fury2Pose)
On Error Resume Next
    WRSet mvarParent, Parent
End Sub

Public Sub RunScript(Optional ByVal Object As Fury2Sprite = Nothing)
Attribute RunScript.VB_Description = "Runs this frame's script."
On Error Resume Next
    If Len(mvarScript) < 1 Then Exit Sub
    If Object Is Nothing Then
        Set Object = WRGetPose(mvarParent).Parent
    End If
    If mvarScriptBlock Is Nothing Then
        Set mvarScriptBlock = Object.Engine.CompileScriptBlock(mvarScript, "Object")
    End If
    mvarScriptBlock.Invoke Object
End Sub

Public Property Get Script() As String
Attribute Script.VB_Description = "*The script to execute when the frame is played."
    Script = mvarScript
End Property

Public Property Let Script(ByRef Value As String)
On Error Resume Next
    mvarScript = Value
    Set mvarScriptBlock = Nothing
End Property

Public Sub LoadGraphics(Optional ByVal Object As Fury2Sprite = Nothing)
On Error Resume Next
Dim l_varVariables As Variant
Dim l_strFilename As String
Dim l_siImage As Fury2SecondaryImage
Dim l_posParent As Fury2Pose
    If mvarParent.pObj = 0 Then Exit Sub
    Set l_posParent = WRGetPose(mvarParent)
    If Object Is Nothing Then
        Set Object = l_posParent.Parent
    End If
    Set mvarSecondaryImages.Engine = Object.Engine
    l_varVariables = Array(Array("i", mvarIndex), Array("Index", mvarIndex), _
        Array("p", l_posParent.Index), Array("pose", l_posParent.Index), _
        Array("pn", l_posParent.Name), Array("posename", l_posParent.Name), _
        Array("n", Object.Name), Array("name", Object.Name))
    l_strFilename = Object.Engine.ParseString(mvarFilename, l_varVariables)
    If Not mvarImage Is Nothing Then
        If mvarImage.Name <> Filename Then
            Set mvarImage = Nothing
        End If
    End If
    If mvarImage Is Nothing Then
        Set mvarImage = Object.Engine.LoadImage(l_strFilename)
    End If
    If mvarImage Is Nothing Then
        Object.Engine.CriticalError "Fury2PoseFrame.LoadGraphics", "Unable to load frame: " + l_strFilename
    Else
        mvarImage.MatteColor = mvarMatteColor
    End If
    For Each l_siImage In mvarSecondaryImages
        l_siImage.LoadGraphics
    Next l_siImage
    RefreshSecondaryImages
End Sub

Sub Free()
Attribute Free.VB_MemberFlags = "40"
On Error Resume Next
Dim l_siImage As Fury2SecondaryImage
    Set mvarScriptBlock = Nothing
    For Each l_siImage In mvarSecondaryImages
        l_siImage.Free
    Next l_siImage
    mvarSecondaryImages.Clear
    Set mvarSecondaryImages = Nothing
    WRFree mvarParent
    Set mvarImage = Nothing
End Sub

Public Function Duplicate() As Fury2PoseFrame
Attribute Duplicate.VB_Description = "Creates a copy of the frame."
On Error Resume Next
Dim l_siImage As Fury2SecondaryImage
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
        For Each l_siImage In SecondaryImages
            .SecondaryImages.Add l_siImage.Duplicate
        Next l_siImage
    End With
End Function

Friend Sub SetImage(Img As Fury2Image)
    Set mvarImage = Img
End Sub

Public Property Set Image(RHS As Fury2Image)
Attribute Image.VB_Description = "The image of this frame."
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

Public Property Get SecondaryImages() As Fury2Collection
Attribute SecondaryImages.VB_Description = "The secondary images attached to this frame."
On Error Resume Next
    Set SecondaryImages = mvarSecondaryImages
End Property

Public Property Set SecondaryImages(ByVal NewValue As Fury2Collection)
On Error Resume Next
    Set mvarSecondaryImages = NewValue
    RefreshSecondaryImages
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set mvarRectangle = New Fury2Rect
    Set mvarSecondaryImages = New Fury2Collection
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngCount As Long, l_lngIndex As Long
Dim l_siImage As Fury2SecondaryImage
    With File
        .ReadSegment_Begin
            .Load mvarMatteColor
            .Load mvarFilename
            .Load mvarRectangle
            .Load mvarXCenter
            .Load mvarYCenter
            .Load mvarDelay
            .Load mvarScript
            .Load l_lngCount
            mvarSecondaryImages.Clear
            If l_lngCount > 0 Then
                .ReadSegment_Begin
                For l_lngIndex = 1 To l_lngCount
                    Set l_siImage = New Fury2SecondaryImage
                    l_siImage.SetParent Me
                    .Load l_siImage
                    mvarSecondaryImages.Add l_siImage
                Next l_lngIndex
                .ReadSegment_End
            End If
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngCount As Long, l_lngIndex As Long
Dim l_siImage As Fury2SecondaryImage
    With File
        .WriteSegment_Begin
            .Save mvarMatteColor
            .Save mvarFilename
            .Save mvarRectangle
            .Save mvarXCenter
            .Save mvarYCenter
            .Save mvarDelay
            .Save mvarScript
            .Save mvarSecondaryImages.Count
            If mvarSecondaryImages.Count > 0 Then
                .WriteSegment_Begin
                For l_lngIndex = 1 To mvarSecondaryImages.Count
                    Set l_siImage = mvarSecondaryImages(l_lngIndex)
                    .Save l_siImage
                Next l_lngIndex
                .WriteSegment_End
            End If
        .WriteSegment_End
    End With
End Sub

Friend Sub InitSecondaryImages()
On Error Resume Next
Dim l_siImage As Fury2SecondaryImage
    For Each l_siImage In mvarSecondaryImages
        l_siImage.SetParent Me
        l_siImage.LoadGraphics
    Next l_siImage
End Sub

Friend Sub RefreshSecondaryImages()
On Error Resume Next
Dim l_siImage As Fury2SecondaryImage
Dim l_lngIndex As Long
    If mvarSecondaryImages Is Nothing Then Exit Sub
    ReDim m_siSecondaryImageParams(0 To mvarSecondaryImages.Count - 1)
    l_lngIndex = 0
    For Each l_siImage In mvarSecondaryImages
        If Not (l_siImage Is Nothing) Then
            l_siImage.SetIndex l_lngIndex + 1
            m_siSecondaryImageParams(l_lngIndex).ImageType = l_siImage.ImageType
            If Not (l_siImage.Image Is Nothing) Then
                m_siSecondaryImageParams(l_lngIndex).pImage = l_siImage.Image.Handle
            Else
                m_siSecondaryImageParams(l_lngIndex).pImage = 0
            End If
        End If
        l_lngIndex = l_lngIndex + 1
    Next l_siImage
End Sub

Private Sub mvarSecondaryImages_Add(ByVal Index As Long)
On Error Resume Next
    RefreshSecondaryImages
End Sub

Private Sub mvarSecondaryImages_AddNew(Output As Object)
On Error Resume Next
    Set Output = Me.AddSecondaryImage()
End Sub

Private Sub mvarSecondaryImages_Clear()
On Error Resume Next
    RefreshSecondaryImages
End Sub

Private Sub mvarSecondaryImages_Remove(ByVal Index As Long)
On Error Resume Next
    RefreshSecondaryImages
End Sub

