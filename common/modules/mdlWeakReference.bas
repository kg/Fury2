Attribute VB_Name = "mdlWeakReference"
Option Explicit
Public Type tVar
    v As Variant
    vt As VbVarType
End Type

Public Type WeakReference
    pObj As Long
End Type

Public Function VarCopy(ByRef Source As Variant, ByRef Destination As Variant)
On Error Resume Next
Dim vt As VbVarType
    vt = VarType(Source)
    If (vt And vbObject) = vbObject Then
        Set Destination = Source
    Else
        Destination = Source
    End If
End Function

Public Function Encapsulate(ByRef Value As Variant) As tVar
On Error Resume Next
Dim l_tvResult As tVar
    l_tvResult.vt = VarType(Value)
    If (l_tvResult.vt And vbObject) = vbObject Then
        Set l_tvResult.v = Value
    Else
        l_tvResult.v = Value
    End If
    Encapsulate = l_tvResult
End Function

Public Sub WRSet(ByRef WR As WeakReference, ByRef NewObject As Object)
On Error Resume Next
    CopyMemory ByVal VarPtr(WR.pObj), ByVal VarPtr(NewObject), 4
End Sub

Public Function WRIsNull(ByRef WR As WeakReference) As Boolean
On Error Resume Next
    WRIsNull = (WR.pObj = 0)
End Function

Public Sub WRFree(ByRef WR As WeakReference)
On Error Resume Next
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(WR.pObj), ByVal VarPtr(l_lngZero), 4
End Sub

Public Function WRGet(ByRef WR As WeakReference) As Object
On Error Resume Next
Dim l_objObject As Object
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(WR.pObj), 4
    Set WRGet = l_objObject
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(l_lngZero), 4
End Function

#If fury2 = 1 Then

Public Function WRGetSprites(ByRef WR As WeakReference) As Fury2Sprites
On Error Resume Next
Dim l_objObject As Fury2Sprites
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(WR.pObj), 4
    Set WRGetSprites = l_objObject
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(l_lngZero), 4
End Function

Public Function WRGetSprite(ByRef WR As WeakReference) As Fury2Sprite
On Error Resume Next
Dim l_objObject As Fury2Sprite
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(WR.pObj), 4
    Set WRGetSprite = l_objObject
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(l_lngZero), 4
End Function

Public Function WRGetPoses(ByRef WR As WeakReference) As Fury2Poses
On Error Resume Next
Dim l_objObject As Fury2Poses
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(WR.pObj), 4
    Set WRGetPoses = l_objObject
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(l_lngZero), 4
End Function

Public Function WRGetPose(ByRef WR As WeakReference) As Fury2Pose
On Error Resume Next
Dim l_objObject As Fury2Pose
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(WR.pObj), 4
    Set WRGetPose = l_objObject
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(l_lngZero), 4
End Function

Public Function WRGetSequence(ByRef WR As WeakReference) As Fury2Sequence
On Error Resume Next
Dim l_objObject As Fury2Sequence
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(WR.pObj), 4
    Set WRGetSequence = l_objObject
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(l_lngZero), 4
End Function

Public Function WRGetKeyframe(ByRef WR As WeakReference) As Fury2SequenceKeyframe
On Error Resume Next
Dim l_objObject As Fury2SequenceKeyframe
Dim l_lngZero As Long
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(WR.pObj), 4
    Set WRGetKeyframe = l_objObject
    CopyMemory ByVal VarPtr(l_objObject), ByVal VarPtr(l_lngZero), 4
End Function

#End If
