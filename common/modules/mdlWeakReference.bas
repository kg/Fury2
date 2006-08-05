Attribute VB_Name = "mdlWeakReference"
Option Explicit
Public Type WeakReference
    pObj As Long
End Type

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


