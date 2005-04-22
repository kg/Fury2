VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngListItems"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Generic Collection Object"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Obj"
Attribute VB_Ext_KEY = "Member0" ,"Obj"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements IVirtualFileSaveable

Private m_booDisableEvents As Boolean
Private m_lstParent As ngListBox
Private m_colCollection As Collection

Friend Sub Free()
On Error Resume Next
Dim l_liItem As ngListItem
    Set m_lstParent = Nothing
    For Each l_liItem In m_colCollection
        l_liItem.Free
    Next l_liItem
End Sub

Friend Sub Reindex()
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_lngIndex As Long
    If m_lstParent Is Nothing Then Exit Sub
    l_lngIndex = 1
    For Each l_liItem In m_colCollection
        l_liItem.Index = l_lngIndex
        l_lngIndex = l_lngIndex + 1
    Next l_liItem
End Sub

Friend Sub SetParent(Parent As ngListBox)
On Error Resume Next
    Set m_lstParent = Parent
End Sub

Public Function AddNew(Optional ByRef Text As String = "", Optional ByRef Key As String = "", Optional ByRef Image As Variant = Nothing, Optional ByVal TextAlignment As ngListItemTextAlignment = ltaLeft, Optional ByVal AtIndex As Long = -1) As ngListItem
On Error Resume Next
Dim l_liNew As ngListItem
Dim l_imgImage As Fury2Image
Dim l_picImage As StdPicture
    Set l_liNew = New ngListItem
    With l_liNew
        .Key = Key
        .Text = Text
        If VarType(Image) = vbString Then
            Set .Image = m_lstParent.ResourceFile(Replace(m_lstParent.ResourcePattern, "*", Image)).LoadImage()
        ElseIf VarType(Image) = vbObject Then
            If TypeOf Image Is Fury2Image Then
                Set l_imgImage = Image
                Set .Image = l_imgImage
            End If
        End If
        .TextAlignment = TextAlignment
    End With
    Add l_liNew
    Set AddNew = l_liNew
End Function

Public Sub Add(ByVal Obj As ngListItem, Optional ByVal AtIndex As Long = -1)
On Error Resume Next
Dim l_strKey As String
    l_strKey = Obj.Key
    If Len(l_strKey) = 0 Then
        If AtIndex = -1 Then
            m_colCollection.Add Obj
        Else
            If (m_colCollection.Count = 0) Or (AtIndex > m_colCollection.Count) Then
                m_colCollection.Add Obj
            ElseIf AtIndex <= 1 Then
                m_colCollection.Add Obj, , 1
            ElseIf m_colCollection.Count > AtIndex Then
                m_colCollection.Add Obj, , AtIndex
            Else
                m_colCollection.Add Obj, , , AtIndex - 1
            End If
        End If
    Else
        If AtIndex = -1 Then
            m_colCollection.Add Obj, l_strKey
        Else
            If (m_colCollection.Count = 0) Or (AtIndex > m_colCollection.Count) Then
                m_colCollection.Add Obj, l_strKey
            ElseIf AtIndex <= 1 Then
                m_colCollection.Add Obj, l_strKey, 1
            ElseIf m_colCollection.Count > AtIndex Then
                m_colCollection.Add Obj, l_strKey, AtIndex
            Else
                m_colCollection.Add Obj, l_strKey, , AtIndex - 1
            End If
        End If
    End If
    If m_lstParent Is Nothing Then Exit Sub
    Obj.SetParent m_lstParent
    If Not m_booDisableEvents Then
        Reindex
        m_lstParent.Reflow
    End If
    Err.Clear
End Sub

Public Property Get Item(ByVal IndexOrKey) As ngListItem
Attribute Item.VB_UserMemId = 0
On Error Resume Next
    If VarType(IndexOrKey) = vbString Then
        IndexOrKey = CStr(IndexOrKey)
    End If
    Err.Clear
    Set Item = m_colCollection(IndexOrKey)
    Err.Clear
End Property

Public Function Find(ByVal Obj As ngListItem) As Long
On Error Resume Next
Dim l_lngItems As Long, l_liItem As ngListItem
    If m_colCollection.Count <= 0 Then Exit Function
    l_lngItems = 1
    For Each l_liItem In m_colCollection
        If l_liItem Is Obj Then
            Find = l_lngItems
            Exit Function
        End If
        l_lngItems = l_lngItems + 1
    Next l_liItem
End Function

Public Property Get Count() As Long
On Error Resume Next
    Count = m_colCollection.Count
End Property

Public Sub Remove(ByVal IndexOrKey)
On Error Resume Next
    If VarType(IndexOrKey) = vbString Then
        IndexOrKey = CStr(IndexOrKey)
    ElseIf VarType(IndexOrKey) = vbObject Then
        IndexOrKey = Find(IndexOrKey)
    Else
        IndexOrKey = CLng(IndexOrKey)
    End If
    m_colCollection.Remove IndexOrKey
    If Not m_booDisableEvents Then
        Reindex
        m_lstParent.Reflow
    End If
End Sub

Public Sub Move(ByVal FromIndex As Long, ByVal ToIndex As Long)
On Error Resume Next
Dim l_liItem As Object
    If FromIndex < 1 Or FromIndex > m_colCollection.Count Then Exit Sub
    If ToIndex < 1 Or ToIndex > (m_colCollection.Count + 1) Then Exit Sub
    If FromIndex = ToIndex Then Exit Sub
    Set l_liItem = m_colCollection.Item(FromIndex)
    m_booDisableEvents = True
    Remove FromIndex
    If (FromIndex < ToIndex) Then
'        Trust me, this works - even though it doesn't make sense unless you think about it
'        ToIndex = ToIndex - 1
    End If
    Add l_liItem, ToIndex
    m_booDisableEvents = False
    Reindex
    m_lstParent.Reflow
End Sub

Public Sub Clear()
On Error Resume Next
Dim l_liItem As ngListItem
    For Each l_liItem In m_colCollection
        l_liItem.Free
    Next l_liItem
    Set m_colCollection = New Collection
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
On Error Resume Next
    Set NewEnum = m_colCollection.[_NewEnum]
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colCollection = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set m_colCollection = Nothing
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

