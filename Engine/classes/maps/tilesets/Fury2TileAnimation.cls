VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2TileAnimation"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Tile Animation Object"
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

Implements Fury2Object
Implements IVirtualFileSaveable
Private mvarSequence() As Integer
Private mvarDelays() As Integer
Private mvarDelayCounter As Long
Private mvarCurrentFrame As Long
Private mvarTile As Integer
Private mvarParent As Fury2MapLayer

Friend Sub SetParent(ByRef Layer As Fury2MapLayer)
On Error Resume Next
    Set mvarParent = Layer
End Sub

Public Property Get Layer() As Fury2MapLayer
On Error Resume Next
    Set Layer = mvarParent
End Property

Public Function Length() As Long
On Error Resume Next
    Length = UBound(mvarSequence) + 1
End Function

Public Sub Resize(Length)
Dim m_lngLength As Long
    m_lngLength = CLng(Length)
    ReDim Preserve mvarSequence(0 To m_lngLength - 1)
    ReDim Preserve mvarDelays(0 To m_lngLength - 1)
End Sub

Public Function AverageDelay() As Single
On Error Resume Next
Dim m_lngFrames As Long
Dim m_sngValue As Single
    For m_lngFrames = LBound(mvarDelays) To UBound(mvarDelays)
        m_sngValue = m_sngValue + mvarDelays(m_lngFrames)
    Next m_lngFrames
    AverageDelay = m_sngValue / ((UBound(mvarDelays) - LBound(mvarDelays)) + 1)
End Function

Public Sub SetDelay(Value)
On Error Resume Next
Dim m_lngFrames As Long
Dim m_intValue As Integer
    m_intValue = CInt(Value)
    For m_lngFrames = LBound(mvarDelays) To UBound(mvarDelays)
        mvarDelays(m_lngFrames) = m_intValue
    Next m_lngFrames
End Sub

Public Property Let Tile(ByVal vData As Integer)
On Error Resume Next
    mvarTile = vData
End Property

Public Property Get Tile() As Integer
On Error Resume Next
    Tile = mvarTile
End Property

Public Property Let CurrentFrame(ByVal vData As Long)
On Error Resume Next
    mvarCurrentFrame = vData
End Property

Public Property Get CurrentFrame() As Long
On Error Resume Next
    CurrentFrame = mvarCurrentFrame
End Property

Public Property Let Delay(Index As Long, NewValue As Integer)
On Error Resume Next
    mvarDelays(Index) = NewValue
End Property

Public Property Get Delay(Index As Long) As Integer
On Error Resume Next
    Delay = mvarDelays(Index)
End Property

Public Property Let Sequence(ByRef vData() As Integer)
On Error Resume Next
    mvarSequence() = vData
    ReDim Preserve mvarDelays(LBound(mvarSequence) To UBound(mvarSequence))
End Property

Public Property Get Sequence() As Integer()
On Error Resume Next
    Sequence = mvarSequence()
End Property

Public Property Get Frame(Index As Long) As Integer
On Error Resume Next
    Frame = mvarSequence(Index)
End Property

Public Property Let Frame(Index As Long, NewValue As Integer)
On Error Resume Next
    mvarSequence(Index) = NewValue
End Property

Public Property Get Pointer(Optional Frame As Long = 0) As Long
    Pointer = VarPtr(mvarSequence(Frame))
End Property

Sub Update()
On Error Resume Next
    mvarDelayCounter = mvarDelayCounter + 1
    If mvarDelayCounter > mvarDelays(mvarCurrentFrame) Then
        mvarDelayCounter = 0
        mvarCurrentFrame = mvarCurrentFrame + 1
        If mvarCurrentFrame > UBound(mvarSequence) Then mvarCurrentFrame = LBound(mvarSequence)
        mvarParent.Map.Engine.Cameras.Dirty
    End If
End Sub

Private Sub Class_Initialize()
    ReDim mvarSequence(0 To 0)
    ReDim mvarDelays(0 To 0)
End Sub

Private Sub Class_Terminate()
    Erase mvarSequence
    Erase mvarDelays
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim m_lngLength As Long
    With File
        .ReadSegment_Begin
            .Load mvarTile
            .Load mvarCurrentFrame
            .Load m_lngLength
            ReDim mvarSequence(0 To m_lngLength)
            ReDim mvarDelays(0 To m_lngLength)
            .RawLoad VarPtr(mvarSequence(0)), (m_lngLength + 1) * 2
            .RawLoad VarPtr(mvarDelays(0)), (m_lngLength + 1) * 2
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim m_lngLength As Long
    With File
        .WriteSegment_Begin
            .Save mvarTile
            .Save mvarCurrentFrame
            m_lngLength = UBound(mvarSequence)
            .Save m_lngLength
            .RawSave VarPtr(mvarSequence(0)), (m_lngLength + 1) * 2
            .RawSave VarPtr(mvarDelays(0)), (m_lngLength + 1) * 2
        .WriteSegment_End
    End With
End Sub

