VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Font"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
Implements IInspectable
Implements IVirtualFileSaveable

Public Merge As Boolean
Attribute Merge.VB_VarDescription = "*Enables/disables Merge rendering mode."
Private m_lngFullHeight As Long
Private m_lngCharCount As Long
Private m_colCharImages As Collection
Private m_cpmCharParams() As CharacterParam
Private m_lngCharMappings() As Long
Private m_fpmFontParam As FontParam
Private m_strTitle As String
Private m_strAuthor As String
Private m_strCodeset As String
Private m_strFilename As String
Private m_booUpperCaseOnly As Boolean
Private m_lngTabStops() As Long
Private m_engEngine As Fury2Engine
Private m_rctText As Fury2Rect
Private m_colSubfonts As Collection
Private m_sfSubfonts() As SubFontParam
Private m_strSubfontNames() As String
Private m_bytFontBuffer() As Byte

Public Sub Free()
On Error Resume Next
    Do While m_colCharImages.Count > 0
        m_colCharImages.Remove 1
    Loop
    Do While m_colSubfonts.Count > 0
        m_colSubfonts.Remove 1
    Loop
    Erase m_cpmCharParams
    Erase m_lngCharMappings
    Erase m_lngTabStops
    Erase m_sfSubfonts
    Erase m_strSubfontNames
    Erase m_bytFontBuffer
    m_fpmFontParam.MapCount = 0
    m_fpmFontParam.SubFontCount = 0
End Sub

Public Sub AddSubFont(ByVal Font As Fury2Font, Optional ByVal Name As String = "")
On Error Resume Next
    If Len(Name) < 1 Then
        Name = Font.Title
    End If
    m_colSubfonts.Add Font, Name
    ReDim Preserve m_sfSubfonts(0 To m_colSubfonts.Count)
    ReDim Preserve m_strSubfontNames(0 To m_colSubfonts.Count)
    m_sfSubfonts(m_colSubfonts.Count).Font = Font.Pointer
    m_strSubfontNames(m_colSubfonts.Count) = Name & Chr(0)
    m_sfSubfonts(m_colSubfonts.Count).Name = StrPtr(m_strSubfontNames(m_colSubfonts.Count))
    m_fpmFontParam.SubFontCount = m_colSubfonts.Count + 1
    m_fpmFontParam.SubFonts = VarPtr(m_sfSubfonts(0))
End Sub

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
Attribute Engine.VB_Description = "*"
    Set Engine = m_engEngine
End Property

Public Property Get TabStops() As Variant
On Error Resume Next
Dim l_varTabStops As Variant
Dim l_lngStops As Long
    ReDim l_varTabStops(0 To UBound(m_lngTabStops))
    For l_lngStops = LBound(m_lngTabStops) To UBound(m_lngTabStops)
        l_varTabStops(l_lngStops) = m_lngTabStops(l_lngStops)
    Next l_lngStops
    TabStops = l_varTabStops
End Property

Public Property Let TabStops(Value As Variant)
On Error Resume Next
Dim l_lngStops As Long
    ReDim m_lngTabStops(0 To UBound(Value))
    For l_lngStops = LBound(Value) To UBound(Value)
        m_lngTabStops(l_lngStops) = CLng(Value(l_lngStops))
    Next l_lngStops
End Property

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Font: " & m_strTitle
End Function

Friend Sub SetData(CharCount As Long, CharImages As Collection, CharParams() As CharacterParam, CharMappings() As Long, FontParam As FontParam, Title As String, Author As String, Codeset As String, Filename As String)
On Error Resume Next
    m_lngCharCount = CharCount
    Set m_colCharImages = CharImages
    m_cpmCharParams() = CharParams()
    m_lngCharMappings() = CharMappings()
    m_fpmFontParam = FontParam
    m_strTitle = Title
    m_strAuthor = Author
    m_strCodeset = Codeset
    m_strFilename = Filename
End Sub

Public Function DuplicateHandle() As Fury2Font
On Error Resume Next
    Set DuplicateHandle = New Fury2Font
    With DuplicateHandle
        .SetEngine Engine
        .SetData m_lngCharCount, m_colCharImages, m_cpmCharParams, m_lngCharMappings, m_fpmFontParam, m_strTitle, m_strAuthor, m_strCodeset, m_strFilename
    End With
End Function

Public Function Duplicate() As Fury2Font
On Error Resume Next
Dim l_colNewImages As Collection, l_lngImages As Long, l_lngParams As Long, l_cpmNewParams() As CharacterParam
    Set Duplicate = New Fury2Font
    With Duplicate
        .SetEngine Engine
        Set l_colNewImages = New Collection
        l_cpmNewParams = m_cpmCharParams
        If m_colCharImages.Count > 0 Then
            For l_lngImages = 1 To m_colCharImages.Count
                l_colNewImages.Add m_colCharImages(l_lngImages).Duplicate
            Next l_lngImages
        End If
        For l_lngParams = LBound(l_cpmNewParams) To UBound(l_cpmNewParams) - 1
            l_cpmNewParams(l_lngParams).pImage = l_colNewImages(l_lngParams).Handle
        Next l_lngParams
        .SetData m_lngCharCount, l_colNewImages, l_cpmNewParams, m_lngCharMappings, m_fpmFontParam, m_strTitle, m_strAuthor, m_strCodeset, m_strFilename
    End With
End Function

Public Property Get Title() As String
Attribute Title.VB_MemberFlags = "200"
    Title = m_strTitle
End Property

Public Property Let Title(NewTitle As String)
    m_strTitle = NewTitle
End Property

Public Property Get Author() As String
    Author = m_strAuthor
End Property

Public Property Let Author(NewAuthor As String)
    m_strAuthor = NewAuthor
End Property

Public Property Get Codeset() As String
Attribute Codeset.VB_Description = "*"
    Codeset = m_strCodeset
End Property

Public Property Let Codeset(NewCodeset As String)
    m_strCodeset = NewCodeset
End Property

Public Function GetTextRectangle(Text, Optional ByVal MaximumWidth As Long = -1, Optional ByVal EnableColorCodes As Boolean = True) As Fury2Rect
On Error Resume Next
Dim l_lngMaxWidth As Long
Dim l_rctArea As Rectangle
Dim l_optOptions As TextParam
Dim l_lngRefCount As Long
    l_lngMaxWidth = MaximumWidth
    If l_lngMaxWidth < 1 Then l_lngMaxWidth = 32767
    l_rctArea.Width = l_lngMaxWidth
    l_rctArea.Height = 32767
    l_optOptions.Opacity = 255
    l_optOptions.EnableColorCodes = CByte(CLng(Abs(EnableColorCodes)))
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    If m_booUpperCaseOnly Then
        Call SoftFX.RenderText(Replace(UCase(m_Globals.ToString(Text)), ChrW(0), ""), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    Else
        Call SoftFX.RenderText(Replace(m_Globals.ToString(Text), ChrW(0), ""), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    End If
    If m_rctText Is Nothing Then
        Set m_rctText = F2Rect(0, 0, l_optOptions.Width, (l_optOptions.Height - Height + FullHeight), False)
    Else
        l_lngRefCount = m_engEngine.GetReferenceCount(m_rctText)
        If l_lngRefCount > 4 Then
            Set m_rctText = F2Rect(0, 0, l_optOptions.Width, (l_optOptions.Height - Height + FullHeight), False)
        Else
            m_rctText.SetValues 0, 0, l_optOptions.Width, (l_optOptions.Height - Height + FullHeight)
        End If
    End If
    Set GetTextRectangle = m_rctText
End Function

Public Function TextWidth(Text, Optional ByVal MaxWidth As Long = -1, Optional ByVal EnableColorCodes As Boolean = True) As Long
On Error Resume Next
Dim l_strText As String
Dim l_rctArea As Rectangle
Dim l_lngMaxWidth As Long, l_lngMaxHeight As Long
Dim l_lngTotalWidth As Long
Dim l_optOptions As TextParam
    If m_lngCharCount < 1 Then Exit Function
    If Err <> 0 Then Exit Function
    l_lngMaxWidth = MaxWidth ' + 1
    If l_lngMaxWidth < 1 Then l_lngMaxWidth = 32767
    l_lngMaxHeight = 32767
    l_rctArea.Width = l_lngMaxWidth
    l_rctArea.Height = l_lngMaxHeight
    l_optOptions.Opacity = 255
    l_optOptions.EnableColorCodes = CByte(CLng(Abs(EnableColorCodes)))
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    If m_booUpperCaseOnly Then
        Call SoftFX.RenderText(Replace(UCase(m_Globals.ToString(Text)), ChrW(0), ""), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    Else
        Call SoftFX.RenderText(Replace(m_Globals.ToString(Text), ChrW(0), ""), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    End If
    TextWidth = l_optOptions.Width
End Function

Public Function TextHeight(Text, Optional ByVal MaxWidth As Long = -1, Optional ByVal EnableColorCodes As Boolean = True) As Long
On Error Resume Next
Dim l_strText As String
Dim l_rctArea As Rectangle
Dim l_lngMaxWidth As Long, l_lngMaxHeight As Long
Dim l_optOptions As TextParam
    If m_lngCharCount < 1 Then Exit Function
    If Err <> 0 Then Exit Function
    l_lngMaxWidth = MaxWidth ' + 1
    If l_lngMaxWidth < 1 Then l_lngMaxWidth = 32767
    l_lngMaxHeight = 32767
    l_rctArea.Width = l_lngMaxWidth
    l_rctArea.Height = l_lngMaxHeight
    l_optOptions.Opacity = 255
    l_optOptions.EnableColorCodes = CByte(CLng(Abs(EnableColorCodes)))
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    If m_booUpperCaseOnly Then
        Call SoftFX.RenderText(Replace(UCase(m_Globals.ToString(Text)), ChrW(0), ""), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    Else
        Call SoftFX.RenderText(Replace(m_Globals.ToString(Text), ChrW(0), ""), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    End If
    TextHeight = l_optOptions.Height
End Function

Public Property Get Height() As Long
    Height = m_fpmFontParam.BaseHeight
End Property

Public Property Let Height(NewValue As Long)
    m_fpmFontParam.BaseHeight = NewValue
End Property

Public Property Get FullHeight() As Long
On Error Resume Next
    FullHeight = m_lngFullHeight
End Property

Public Property Let FullHeight(ByVal NewValue As Long)
On Error Resume Next
    m_lngFullHeight = NewValue
End Property

Public Property Get CharacterValue(Index) As Integer
Attribute CharacterValue.VB_Description = "*"
On Error Resume Next
    CharacterValue = m_cpmCharParams(Index - 1).MapValue
End Property

Public Property Let CharacterValue(Index, Value As Integer)
On Error Resume Next
    m_cpmCharParams(Index - 1).MapValue = Value
End Property

Public Property Get Character(ByVal Index As Long) As Fury2Image
Attribute Character.VB_Description = "*"
On Error Resume Next
    Set Character = m_colCharImages(Index)
End Property

Public Property Set Character(ByVal Index As Long, ByVal NewImage As Fury2Image)
On Error Resume Next
    m_colCharImages.Remove Index
    If Index < m_colCharImages.Count Then
        m_colCharImages.Add NewImage, , Index
    ElseIf Index > 1 Then
        m_colCharImages.Add NewImage, , , Index - 1
    Else
        m_colCharImages.Add NewImage
    End If
End Property

Public Property Get CharacterCount() As Long
    CharacterCount = m_lngCharCount
End Property

Public Property Get Filename() As String
    Filename = m_strFilename
End Property

Public Property Get EnhancedDraw() As Boolean
Attribute EnhancedDraw.VB_Description = "*"
    EnhancedDraw = m_fpmFontParam.EffectMode = 5
End Property

Public Property Let EnhancedDraw(NewValue As Boolean)
    m_fpmFontParam.EffectMode = CLng(IIf(NewValue, 5, 4))
End Property

Private Sub ParseOptions(ByRef TheArray As Variant, ByRef Output As TextParam)
On Error Resume Next
    With Output
        .Opacity = 255
        If UBound(TheArray) >= 0 Then .Scroll_X = CLng(TheArray(0))
        If UBound(TheArray) >= 1 Then .Scroll_Y = CLng(TheArray(1))
        If UBound(TheArray) >= 2 Then .Selection_Start = CLng(TheArray(2))
        If UBound(TheArray) >= 3 Then .Selection_End = CLng(TheArray(3))
        If UBound(TheArray) >= 4 Then .Selection_Color = CLng(TheArray(4))
        If UBound(TheArray) >= 5 Then .Caret_Position = CLng(TheArray(5))
        If UBound(TheArray) >= 6 Then .Caret_Color = CLng(TheArray(6))
        If UBound(TheArray) >= 7 Then .MaxChars = CLng(TheArray(7))
        If UBound(TheArray) >= 8 Then .EnableColorCodes = CByte(Abs(CLng(TheArray(8)))) Else .EnableColorCodes = True
    End With
End Sub

Public Function Options(Optional ByVal XScroll As Long = 0, Optional ByVal YScroll As Long = 0, _
    Optional ByVal SelectionStart As Long = 0, Optional ByVal SelectionEnd As Long = 0, _
    Optional ByVal SelectionColor As Long = 0, Optional ByVal CaretPosition As Long = 0, _
    Optional ByVal CaretColor As Long = 0, Optional ByVal MaxCharacters As Long = 0, Optional ByVal EnableColorCodes As Boolean = True) As Variant
    Options = Array(XScroll, YScroll, SelectionStart, SelectionEnd, SelectionColor, CaretPosition, CaretColor, MaxCharacters, EnableColorCodes)
End Function

Public Function IndexFromPoint(ByRef Text, ByVal Rectangle As Fury2Rect, ByVal X As Long, ByVal Y As Long, Optional Options As Variant) As Long
On Error Resume Next
Dim l_strText As String
Dim l_rctRectangle As Fury2Rect
Dim l_rctArea As Rectangle
Dim l_optOptions As TextParam
    If m_lngCharCount < 1 Then Exit Function
    If WrapMode > 1 Then WrapMode = 0
    If Err <> 0 Then Exit Function
    If m_booUpperCaseOnly Then
        l_strText = UCase(m_Globals.ToString(Text))
    Else
        l_strText = m_Globals.ToString(Text)
    End If
    If Err <> 0 Then Exit Function
    Set l_rctRectangle = Rectangle
    If Err <> 0 Then Exit Function
    ParseOptions Options, l_optOptions
    l_optOptions.Opacity = 255
    l_optOptions.CharFromPoint = 1
    l_optOptions.CharFromPoint_X = X
    l_optOptions.CharFromPoint_Y = Y
    If SoftFX.RenderText(l_strText, 0, l_rctRectangle.GetRectangle, m_fpmFontParam, l_optOptions) Then
        IndexFromPoint = l_optOptions.CharFromPoint
    End If
End Function

Public Function Draw(ByVal Surface As Fury2Image, ByRef Text, ByVal Rectangle As Fury2Rect, Optional ByVal Color, Optional ByVal Alpha As Single = 1, Optional Options As Variant, Optional ByRef HeightOut) As Long
On Error Resume Next
Dim l_imgSurface As Fury2Image
Dim l_strText As String
Dim l_rctRectangle As Fury2Rect
Dim l_lngColor As Long
Dim l_rctArea As Rectangle
Dim l_optOptions As TextParam
    If m_lngCharCount < 1 Then Exit Function
    Set l_imgSurface = Surface
    If WrapMode > 1 Then WrapMode = 0
    If Err <> 0 Then Exit Function
    If m_booUpperCaseOnly Then
        l_strText = Replace(UCase(m_Globals.ToString(Text)), ChrW(0), "")
    Else
        l_strText = Replace(m_Globals.ToString(Text), ChrW(0), "")
    End If
    If Err <> 0 Then Exit Function
    Set l_rctRectangle = Rectangle
    If Err <> 0 Then Exit Function
    l_lngColor = m_fpmFontParam.FillColor
    If IsMissing(Color) Then
    Else
        m_fpmFontParam.FillColor = CLng(Color)
    End If
    If Merge Then
        m_fpmFontParam.EffectMode = 1
    Else
        m_fpmFontParam.EffectMode = 0
    End If
'    m_fpmFontParam.FillColor = SetAlpha(m_fpmFontParam.FillColor, GetAlpha(m_fpmFontParam.FillColor) * CSng(Alpha))
    If IsMissing(Options) Then
        ParseOptions Array(), l_optOptions
    Else
        ParseOptions Options, l_optOptions
    End If
    l_optOptions.Opacity = Alpha * 255
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    If l_imgSurface Is Nothing Then
        Call SoftFX.RenderText(l_strText, 0, l_rctRectangle.GetRectangle, m_fpmFontParam, l_optOptions)
    Else
        Call SoftFX.RenderText(l_strText, l_imgSurface.Handle, l_rctRectangle.GetRectangle, m_fpmFontParam, l_optOptions)
    End If
    m_fpmFontParam.FillColor = l_lngColor
    Draw = l_optOptions.Lines
    If l_optOptions.Height > 0 Then
        HeightOut = (l_optOptions.Height - m_fpmFontParam.BaseHeight) + m_lngFullHeight
    Else
        HeightOut = 0
    End If
    l_imgSurface.Dirty = True
End Function

Public Function DrawCentered(ByVal Surface As Fury2Image, ByRef Text, ByVal Rectangle As Fury2Rect, Optional ByVal Color, Optional ByVal Alpha As Single = 1, Optional Options As Variant, Optional HeightOut) As Long
On Error Resume Next
Dim l_imgSurface As Fury2Image
Dim l_strText As String
Dim l_rctRectangle As Fury2Rect
Dim l_lngColor As Long
Dim l_rctArea As Rectangle
Dim l_optOptions As TextParam
Dim l_lngXO As Long, l_lngYO As Long
    If m_lngCharCount < 1 Then Exit Function
    Set l_imgSurface = Surface
    If WrapMode > 1 Then WrapMode = 0
    If Err <> 0 Then Exit Function
    If m_booUpperCaseOnly Then
        l_strText = Replace(UCase(m_Globals.ToString(Text)), ChrW(0), "")
    Else
        l_strText = Replace(m_Globals.ToString(Text), ChrW(0), "")
    End If
    If Err <> 0 Then Exit Function
    Set l_rctRectangle = Rectangle
    If Err <> 0 Then Exit Function
    If IsMissing(Options) Then
        ParseOptions Array(), l_optOptions
    Else
        ParseOptions Options, l_optOptions
    End If
    l_optOptions.Opacity = CSng(Alpha) * 255
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    Call SoftFX.RenderText(l_strText, 0, l_rctRectangle.GetRectangle, m_fpmFontParam, l_optOptions)
    With l_rctRectangle
        l_lngXO = (.Width - l_optOptions.Width) \ 2
        l_lngYO = (.Height - (l_optOptions.Height - m_fpmFontParam.BaseHeight + m_lngFullHeight)) \ 2
        Set l_rctRectangle = F2Rect(.left + l_lngXO, .top + l_lngYO, .right, .bottom)
    End With
    l_lngColor = m_fpmFontParam.FillColor
    If IsMissing(Color) Then
    Else
        m_fpmFontParam.FillColor = CLng(Color)
    End If
    If Merge Then
        m_fpmFontParam.EffectMode = 1
    Else
        m_fpmFontParam.EffectMode = 0
    End If
    If IsMissing(Options) Then
    Else
        ParseOptions Options, l_optOptions
    End If
    l_optOptions.Opacity = CSng(Alpha) * 255
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    Call SoftFX.RenderText(l_strText, l_imgSurface.Handle, l_rctRectangle.GetRectangle, m_fpmFontParam, l_optOptions)
    If l_optOptions.Height > 0 Then
        HeightOut = (l_optOptions.Height - m_fpmFontParam.BaseHeight) + m_lngFullHeight
    Else
        HeightOut = 0
    End If
    m_fpmFontParam.FillColor = l_lngColor
    DrawCentered = l_optOptions.Lines
    l_imgSurface.Dirty = True
End Function

Public Property Get Alpha() As Single
Attribute Alpha.VB_Description = "*"
On Error Resume Next
    Alpha = CLng((m_fpmFontParam.Alpha / 255) * 250) / 250
End Property

Public Property Let Alpha(NewValue As Single)
On Error Resume Next
    m_fpmFontParam.Alpha = NewValue * 255
End Property

Public Property Get Color() As Long
Attribute Color.VB_Description = "*"
    Color = m_fpmFontParam.FillColor
End Property

Public Property Let Color(NewValue As Long)
    m_fpmFontParam.FillColor = NewValue
End Property

Public Property Get ShadowColor() As Long
Attribute ShadowColor.VB_Description = "{Color}"
    ShadowColor = m_fpmFontParam.ShadowColor
End Property

Public Property Let ShadowColor(NewValue As Long)
    m_fpmFontParam.ShadowColor = NewValue
End Property

Public Property Get WrapMode() As Fury2WrapModes
    WrapMode = m_fpmFontParam.WrapMode
End Property

Public Property Let WrapMode(ByVal NewValue As Fury2WrapModes)
    m_fpmFontParam.WrapMode = NewValue
End Property

Public Property Get UpperCaseOnly() As Boolean
    UpperCaseOnly = m_booUpperCaseOnly
End Property

Public Property Let UpperCaseOnly(NewValue As Boolean)
    m_booUpperCaseOnly = NewValue
End Property

Friend Function Pointer() As Long
On Error Resume Next
    Pointer = VarPtr(m_fpmFontParam)
End Function

Sub RemoveCharacter(Index As Long)
On Error Resume Next
Dim m_lngMoveBack As Long
    m_colCharImages.Remove Index
    If Index < m_lngCharCount Then
        For m_lngMoveBack = Index - 1 To m_lngCharCount - 1
            m_cpmCharParams(m_lngMoveBack) = m_cpmCharParams(m_lngMoveBack + 1)
        Next m_lngMoveBack
    End If
    m_lngCharCount = m_lngCharCount - 1
    ReDim Preserve m_cpmCharParams(0 To m_lngCharCount - 1)
    Refresh
End Sub

Sub AddCharacter(Image As Fury2Image, Optional MapValue As Long = 0)
On Error Resume Next
    m_lngCharCount = m_lngCharCount + 1
    m_colCharImages.Add Image
    ReDim Preserve m_cpmCharParams(0 To m_lngCharCount - 1)
    m_cpmCharParams(m_lngCharCount - 1).MapValue = CInt(MapValue)
    Refresh
End Sub

'Sub Refresh(Optional ResetMappings As Boolean = False)
'On Error Resume Next
'Dim l_lngChars As Long, l_lngMaxHeight As Long, l_lngMinHeight As Long
'    'm_lngFullHeight = 0
'    Err.Clear
'    l_lngChars = UBound(m_cpmCharParams)
'    If l_lngChars <= 0 Or Err <> 0 Then m_lngCharCount = 0: Exit Sub
'    If m_lngCharCount < 1 Then Exit Sub
'    ReDim m_lngCharMappings(0 To c_lngMaxCharMappings - 1)
'    ReDim Preserve m_cpmCharParams(1 To m_lngCharCount)
'    For l_lngChars = LBound(m_cpmCharParams) To UBound(m_cpmCharParams) - 1
'        With m_cpmCharParams(l_lngChars)
'            If (.MapValue <= 0) Or (ResetMappings) Then
'                .MapValue = l_lngChars
'            End If
'            m_lngCharMappings(.MapValue) = VarPtr(m_cpmCharParams(l_lngChars))
'            .pImage = m_colCharImages(l_lngChars).Handle
'            With m_colCharImages(l_lngChars)
'                If (m_cpmCharParams(l_lngChars).YOffset + .Height) > l_lngMaxHeight Then
'                    l_lngMaxHeight = (m_cpmCharParams(l_lngChars).YOffset + .Height)
'                End If
'            End With
'        End With
'    Next l_lngChars
'    If m_lngFullHeight <= 0 Then
'        m_lngFullHeight = l_lngMaxHeight
'    End If
'    With m_fpmFontParam
'        .MapCount = UBound(m_lngCharMappings)
'        .MapPointer = VarPtr(m_lngCharMappings(0))
'    End With
'End Sub

Function CreateFixed(ByVal Width As Long, ByVal Height As Long, Optional ByVal Count As Long = 256) As Boolean
On Error Resume Next
Dim l_lngChars As Long
Dim l_imgCharacter As Fury2Image
    ReDim m_lngTabStops(0 To 0)
    m_strFilename = ""
    m_strTitle = "Font"
    m_lngCharCount = CLng(Count)
    ReDim m_cpmCharParams(0 To m_lngCharCount - 1)
    Set m_colCharImages = New Collection
    If m_lngCharCount < 1 Then Exit Function
    For l_lngChars = 1 To m_lngCharCount
        Set l_imgCharacter = F2Image(CLng(Width), CLng(Height))
        m_colCharImages.Add l_imgCharacter
        With m_cpmCharParams(l_lngChars - 1)
            .XIncrement = CLng(Width)
            .MapValue = l_lngChars
        End With
        Set l_imgCharacter = Nothing
    Next l_lngChars
    m_fpmFontParam.EffectMode = 5
    m_fpmFontParam.BaseMode = 0
    m_lngFullHeight = Height
    Refresh
    CreateFixed = True
    Err.Clear
End Function

Sub AddDropShadow(ByVal Color As Long, Optional ByVal XOffset As Long = 0, Optional ByVal YOffset As Long = 0, Optional ByVal BlurLevel As Single = 1)
On Error Resume Next
Dim l_imgCharacter As Fury2Image
Dim l_imgClone As Fury2Image
Dim l_imgShadow As Fury2Image
Dim l_fltFilter As Fury2ConvolutionFilter
Dim l_lngCharacter As Long
    If BlurLevel > 0 Then Set l_fltFilter = F2GaussianBlurFilter(BlurLevel)
    For Each l_imgCharacter In m_colCharImages
        Set l_imgClone = l_imgCharacter.Duplicate
        Set l_imgShadow = l_imgCharacter.Duplicate
        l_imgCharacter.Resize l_imgCharacter.Width + (BlurLevel * 2), l_imgCharacter.Height + (BlurLevel * 2)
        l_imgCharacter.Clear 0
        l_imgShadow.Clear 0
        l_fltFilter.Filter l_imgShadow, l_imgClone, , , RenderMode_Normal
        l_imgShadow.FillChannel l_imgShadow.Rectangle, 0, GetBlue(Color)
        l_imgShadow.FillChannel l_imgShadow.Rectangle, 1, GetGreen(Color)
        l_imgShadow.FillChannel l_imgShadow.Rectangle, 2, GetRed(Color)
        l_imgCharacter.Blit F2Rect(XOffset, YOffset, l_imgCharacter.Width, l_imgCharacter.Height, False), , l_imgShadow, , BlitMode_Merge
        l_imgCharacter.Blit , , l_imgClone, , BlitMode_Merge
        l_lngCharacter = l_lngCharacter + 1
    Next l_imgCharacter
    Err.Clear
End Sub

Sub AddOutline(ByVal Color As Long, Optional ByVal Thickness As Long = 1)
On Error Resume Next
Dim l_imgCharacter As Fury2Image
Dim l_imgClone As Fury2Image
Dim l_lngCharacter As Long
Dim l_lngThick As Long, l_lngX As Long, l_lngY As Long
    For Each l_imgCharacter In m_colCharImages
        Set l_imgClone = l_imgCharacter.Duplicate
        l_imgCharacter.Resize l_imgCharacter.Width + (Thickness * 2), l_imgCharacter.Height + (Thickness * 2)
        l_imgCharacter.Clear 0
        For l_lngThick = 1 To Thickness
            For l_lngX = -1 To 1
                For l_lngY = -1 To 1
                    l_imgCharacter.Blit F2Rect(Thickness + (l_lngThick * l_lngX), Thickness + (l_lngThick * l_lngY), l_imgClone.Width, l_imgClone.Height, False), , l_imgClone, , BlitMode_Font_Merge, Color
                Next l_lngY
            Next l_lngX
        Next l_lngThick
        l_imgCharacter.Blit F2Rect(Thickness, Thickness, l_imgClone.Width, l_imgClone.Height, False), , l_imgClone, , BlitMode_Merge
        l_lngCharacter = l_lngCharacter + 1
    Next l_imgCharacter
    Err.Clear
End Sub

Public Sub AdjustGamma(ByVal Gamma As Single)
On Error Resume Next
Dim l_imgCharacter As Fury2Image
Dim l_lngCharacter As Long
    For Each l_imgCharacter In m_colCharImages
        l_imgCharacter.AdjustGamma Gamma
        l_lngCharacter = l_lngCharacter + 1
    Next l_imgCharacter
    Err.Clear
End Sub

Public Sub Recolor(ByVal Color As Long)
On Error Resume Next
Dim l_imgCharacter As Fury2Image
Dim l_lngCharacter As Long
    For Each l_imgCharacter In m_colCharImages
        l_imgCharacter.AdjustChannelGamma 3, GetAlpha(Color) / 255
        l_imgCharacter.AdjustRGBGamma GetRed(Color) / 255, GetGreen(Color) / 255, GetBlue(Color) / 255
        l_lngCharacter = l_lngCharacter + 1
    Next l_imgCharacter
    Err.Clear
End Sub

Public Property Get CharacterWidth(ByVal Character As Long) As Long
On Error Resume Next
    CharacterWidth = m_cpmCharParams(Character - 1).XIncrement
End Property

Public Property Let CharacterWidth(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character - 1).XIncrement = Value
End Property

Public Property Get CharacterHeight(ByVal Character As Long) As Long
On Error Resume Next
    CharacterHeight = m_cpmCharParams(Character - 1).YIncrement
End Property

Public Property Let CharacterHeight(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character - 1).YIncrement = Value
End Property

Public Property Get CharacterXOffset(ByVal Character As Long) As Long
On Error Resume Next
    CharacterXOffset = m_cpmCharParams(Character - 1).XOffset
End Property

Public Property Let CharacterXOffset(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character - 1).XOffset = Value
End Property

Public Property Get CharacterYOffset(ByVal Character As Long) As Long
On Error Resume Next
    CharacterYOffset = m_cpmCharParams(Character - 1).YOffset
End Property

Public Property Let CharacterYOffset(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character - 1).YOffset = Value
End Property

Function ImportTTF(ByVal Font As StdFont, Optional ByVal Unicode As Boolean = False, Optional ByRef CharacterSet As String = "") As Boolean
On Error Resume Next
Dim l_lngChars As Long, l_lngCharIndices As Long, l_lngCode As Long
Dim l_imgNullChar As Fury2Image
Dim m_imgChar As Fury2Image
Dim m_lngMaxHeight As Long
Dim l_lngDC As Long, l_lngDeskWindow As Long, l_lngDeskDC As Long
Dim l_varAttr As Variant, l_lngBaseline As Long
Dim l_glbGlobal As Fury2GEGlobal
Dim l_lngGlyphs() As Long
Dim l_lngMapCount As Long, l_booFound As Boolean
Dim l_lngPointer As Long
    l_lngPointer = Screen.MousePointer
    Screen.MousePointer = 11
    m_lngFullHeight = 0
    Set l_glbGlobal = New Fury2GEGlobal
    Set l_imgNullChar = l_glbGlobal.GetGlyphBitmap(Font, 0)
    If (Unicode) And (Len(CharacterSet) > 0) Then
        ReDim l_lngGlyphs(0 To 15)
        l_lngGlyphs(0) = vbCr
        l_lngGlyphs(1) = vbLf
        l_lngGlyphs(2) = 32
        l_lngGlyphs(3) = 9
        l_lngMapCount = 4
        For l_lngChars = 1 To Len(CharacterSet)
            l_lngCode = AscW(Mid(CharacterSet, l_lngChars, 1))
            l_booFound = False
            For l_lngCharIndices = 0 To l_lngMapCount - 1
                If l_lngGlyphs(l_lngCharIndices) = l_lngCode Then
                    l_booFound = True
                    Exit For
                End If
            Next l_lngCharIndices
            If l_booFound Then
                ' Duplicate!
            Else
                If l_lngMapCount > UBound(l_lngGlyphs) Then
                    ReDim Preserve l_lngGlyphs(0 To UBound(l_lngGlyphs) + 16)
                End If
                l_lngGlyphs(l_lngMapCount) = l_lngCode
                l_lngMapCount = l_lngMapCount + 1
            End If
        Next l_lngChars
        ReDim Preserve l_lngGlyphs(0 To l_lngMapCount - 1)
    Else
        l_lngGlyphs = l_glbGlobal.GetGlyphList(Font, Unicode)
    End If
    m_lngCharCount = UBound(l_lngGlyphs) + 1
    If m_lngCharCount > c_lngMaxGlyphs Then
        m_lngCharCount = c_lngMaxGlyphs
    End If
'    Debug.Print "Glyph Count: " & m_lngCharCount
    l_lngMapCount = m_lngCharCount + 1
    If l_lngMapCount < 256 Then l_lngMapCount = 256
    For l_lngCharIndices = LBound(l_lngGlyphs) To UBound(l_lngGlyphs)
        If l_lngGlyphs(l_lngCharIndices) >= l_lngMapCount Then
            l_lngMapCount = l_lngGlyphs(l_lngCharIndices) + 1
        End If
    Next l_lngCharIndices
    ReDim m_lngCharMappings(0 To l_lngMapCount)
    ReDim m_cpmCharParams(0 To m_lngCharCount - 1)
    Set m_colCharImages = New Collection
    For l_lngCharIndices = 0 To m_lngCharCount - 1
        l_lngChars = l_lngGlyphs(l_lngCharIndices)
        If l_lngChars = 9 Then
            ' Tab
            Set m_imgChar = l_glbGlobal.GetGlyphBitmap(Font, 32)
        Else
            Set m_imgChar = l_glbGlobal.GetGlyphBitmap(Font, l_lngChars)
        End If
        If m_imgChar.Compare(, , l_imgNullChar) = 1 Then
            Set m_imgChar = Nothing
        End If
        m_cpmCharParams(l_lngCharIndices).MapValue = l_lngChars
        m_lngCharMappings(l_lngChars) = VarPtr(m_cpmCharParams(l_lngCharIndices))
        If m_imgChar Is Nothing Then
            m_colCharImages.Add Nothing
        Else
            If l_lngChars < 256 Then
                If m_imgChar.Tag(3) > m_lngMaxHeight Then m_lngMaxHeight = m_imgChar.Tag(3)
            End If
            m_cpmCharParams(l_lngCharIndices).pImage = m_imgChar.Handle
            m_colCharImages.Add m_imgChar
            Set m_imgChar = Nothing
        End If
    Next l_lngCharIndices
    For l_lngChars = 1 To m_lngCharCount
        Set m_imgChar = m_colCharImages(l_lngChars)
        If m_imgChar Is Nothing Then
        Else
            ' (GlyphOriginX, GlyphOriginY, BlackBoxX, BlackBoxY, CellIncX, CellIncY, MaxWidth, MaxHeight)
            l_varAttr = m_imgChar.Tag
            With m_imgChar
                .AlphaChannel = True
                If l_lngGlyphs(l_lngChars - 1) = 9 Then
                    m_cpmCharParams(l_lngChars - 1).XIncrement = CLng(l_varAttr(4) * 8)
                Else
                    m_cpmCharParams(l_lngChars - 1).XIncrement = CLng(l_varAttr(4))
                End If
                m_cpmCharParams(l_lngChars - 1).XOffset = l_varAttr(0)
                l_lngBaseline = l_varAttr(3) - l_varAttr(1)
                m_cpmCharParams(l_lngChars - 1).YOffset = (l_lngBaseline) - (l_varAttr(7) - m_lngMaxHeight)
                m_fpmFontParam.BaseHeight = IIf(CLng(l_varAttr(7)) > m_fpmFontParam.BaseHeight, CLng(l_varAttr(7)), m_fpmFontParam.BaseHeight)
                .Optimize
                If m_cpmCharParams(l_lngChars - 1).MapValue < 256 Then
                    If (Abs(m_cpmCharParams(l_lngChars - 1).YOffset) + .Height) > m_lngFullHeight Then
                        m_lngFullHeight = (Abs(m_cpmCharParams(l_lngChars - 1).YOffset) + .Height)
                    End If
                End If
            End With
            Set m_imgChar = Nothing
        End If
    Next l_lngChars
    m_lngFullHeight = m_lngFullHeight
    With m_fpmFontParam
        .EffectMode = 5
        .FillColor = F2White
        .ShadowColor = 0
        .WrapMode = 2
        .Alpha = 255
        .BaseMode = 0
        .MapCount = UBound(m_lngCharMappings)
        .MapPointer = VarPtr(m_lngCharMappings(0))
    End With
    Title = Font.Name & " " & CStr(Font.Size) & "pt"
    ImportTTF = True
    Err.Clear
    Screen.MousePointer = l_lngPointer
End Function

Sub Refresh(Optional ResetMappings As Boolean = False)
On Error Resume Next
Dim l_lngChars As Long, l_lngMaxHeight As Long, l_lngMinHeight As Long
Dim l_lngMax As Long
    'm_lngFullHeight = 0
    Err.Clear
    l_lngChars = UBound(m_cpmCharParams)
    If l_lngChars <= 0 Or Err <> 0 Then m_lngCharCount = 0: Exit Sub
    If m_lngCharCount < 1 Then Exit Sub
    For l_lngChars = LBound(m_cpmCharParams) To UBound(m_cpmCharParams)
        If m_cpmCharParams(l_lngChars).MapValue > l_lngMax Then
            l_lngMax = m_cpmCharParams(l_lngChars).MapValue
        End If
    Next l_lngChars
    ReDim m_lngCharMappings(0 To l_lngMax)
    ReDim Preserve m_cpmCharParams(0 To m_lngCharCount - 1)
    For l_lngChars = LBound(m_cpmCharParams) To UBound(m_cpmCharParams)
        With m_cpmCharParams(l_lngChars)
            If (.MapValue <= 0) Or (ResetMappings) Then
                .MapValue = l_lngChars
            End If
            m_lngCharMappings(.MapValue) = VarPtr(m_cpmCharParams(l_lngChars))
            With m_colCharImages(l_lngChars + 1)
                m_cpmCharParams(l_lngChars).pImage = .Handle
                If (m_cpmCharParams(l_lngChars).YOffset + .Height) > l_lngMaxHeight Then
                    l_lngMaxHeight = (m_cpmCharParams(l_lngChars).YOffset + .Height)
                End If
            End With
        End With
    Next l_lngChars
    If m_lngFullHeight <= 0 Then
        m_lngFullHeight = l_lngMaxHeight
    End If
    With m_fpmFontParam
        .MapCount = UBound(m_lngCharMappings) + 1
        .MapPointer = VarPtr(m_lngCharMappings(0))
    End With
End Sub

Function ImportImage(ByVal Filename As String, ByVal Width As Long, ByVal Height As Long, Optional ByVal FirstCharacter As Long = 1) As Boolean
On Error Resume Next
Dim l_imgFile As Fury2Image
Dim l_lngX As Long, l_lngY As Long, l_imgChar As Fury2Image
    Set l_imgFile = Engine.LoadImage(Filename)
    If l_imgFile Is Nothing Then ImportImage = False: Exit Function
    If (Width < 1) Or (Height < 1) Then ImportImage = False: Exit Function
    m_lngFullHeight = Height
    m_lngCharCount = (l_imgFile.Width \ Width) * (l_imgFile.Height \ Height)
    ReDim m_lngCharMappings(0 To c_lngMaxCharMappings - 1)
    ReDim m_cpmCharParams(0 To m_lngCharCount - 1)
    Set m_colCharImages = New Collection
    With l_imgFile
        Do Until (l_lngY + Height) > .Height
            l_lngX = 0
            Do Until (l_lngX + Width) > .Width
                Set l_imgChar = .Subimage(F2Rect(l_lngX, l_lngY, Width, Height, False))
                l_imgChar.Optimize
                m_colCharImages.Add l_imgChar
                With m_cpmCharParams(m_colCharImages.Count - 1)
                    .MapValue = FirstCharacter + (m_colCharImages.Count - 1)
                    .pImage = l_imgChar.Handle
                    .XIncrement = Width
                    .XOffset = 0
                    .YIncrement = Height
                    .YOffset = 0
                End With
                l_lngX = l_lngX + Width
            Loop
            l_lngY = l_lngY + Height
        Loop
    End With
    Set l_imgFile = Nothing
    With m_fpmFontParam
        .EffectMode = 5
        .FillColor = F2White
        .ShadowColor = 0
        .WrapMode = 2
        .Alpha = 255
        .BaseMode = 1
        .BaseHeight = Height
        .MapCount = UBound(m_lngCharMappings)
        .MapPointer = VarPtr(m_lngCharMappings(0))
    End With
    Refresh
    ImportImage = True
End Function

Function SaveRFN(ByVal Filename As String) As Boolean
On Error Resume Next
Dim l_vfFont As VirtualFile
Dim l_imgChar As Fury2Image
Dim l_lngChars As Long
    Err.Clear
    Set l_vfFont = F2File
    With l_vfFont
        .SaveCString ".rfn"
        .Skip -1
        .Save CInt(2)
        .Save CInt(m_colCharImages.Count + 1)
        .Skip 248
        .Save CInt(1)
        .Save CInt(1)
        .Skip 28
        .Save CLng(0)
        For Each l_imgChar In m_colCharImages
            If (l_imgChar Is Nothing) Then
                .Save CInt(1)
                .Save CInt(1)
                .Skip 28
                .Save CLng(0)
            ElseIf (l_imgChar.Width <= 0) Or (l_imgChar.Height <= 0) Then
                .Save CInt(1)
                .Save CInt(1)
                .Skip 28
                .Save CLng(0)
            Else
                With l_imgChar
                    l_vfFont.Save CInt(.Width)
                    l_vfFont.Save CInt(.Height)
                    l_vfFont.Skip 28
                    .SwapChannels Red, Blue
                    l_vfFont.RawSave .Pointer(0, 0), .Width * .Height * 4&
                    .SwapChannels Red, Blue
                End With
            End If
        Next l_imgChar
        .SaveFile CStr(Filename)
    End With
    SaveRFN = Err = 0
End Function

Function ImportRFN(ByVal Filename) As Boolean
On Error Resume Next
Dim l_vfFont As VirtualFile
Dim l_intVersion As Integer
Dim l_intCount As Integer
Dim l_lngChars As Long
Dim l_intWidth As Integer, l_intHeight As Integer
Dim l_lngHeight As Long
Dim l_imgCharacter As Fury2Image
Dim l_bytValues() As Byte
    ReDim m_lngTabStops(0 To 0)
    If VarType(Filename) = vbString Then
        m_strFilename = Filename
        m_strTitle = Filename
        Set l_vfFont = Engine.Filesystem.File(Filename).Load
    ElseIf VarType(Filename) = vbObject Then
        Set l_vfFont = Filename
        m_strFilename = "Object"
        m_strTitle = "Object"
    End If
    With l_vfFont
        .Skip 4
        .Load l_intVersion
        .Load l_intCount
        If l_intVersion <> 2 Then
            Engine.DebugOut "Unsupported version of RFN format: " + Filename
            Exit Function
        End If
        If (l_intCount < 1) Or (l_intCount > 256) Then
            Engine.DebugOut "Corrupted RFN file: " + Filename
            Exit Function
        End If
        .Skip 248
        ReDim m_cpmCharParams(0 To l_intCount - 1)
        m_lngCharCount = l_intCount - 1
        Set m_colCharImages = New Collection
        For l_lngChars = 0 To l_intCount - 1
            .Load l_intWidth
            .Load l_intHeight
            If (l_intWidth < 1) Or (l_intHeight < 1) Or (l_intWidth > 255) Or (l_intHeight > 255) Then
                Engine.DebugOut "Corrupted RFN file: " + Filename
                Exit Function
            End If
            .Skip 28
            Set l_imgCharacter = F2Image(l_intWidth, l_intHeight)
            If l_lngChars > 0 Then
                m_colCharImages.Add l_imgCharacter
            End If
            Select Case l_intVersion
            Case 1
                ' Grayscale
'                ReDim l_bytValues(0 To l_intWidth - 1, 0 To l_intHeight - 1)
'                .RawLoad VarPtr(l_bytValues(0, 0)), (CLng(l_intWidth) * l_intHeight)
'                SoftFX.PadData l_imgCharacter.Pointer(0, 0), VarPtr(l_bytValues(0, 0)), 1, 3, (CLng(l_intWidth) * l_intHeight)
'                Erase l_bytValues
'                l_imgCharacter.LoadChannel l_imgCharacter, f2aAlpha, f2aRed
'                l_imgCharacter.FillChannel f2aRed, 255
'                l_imgCharacter.FillChannel f2aGreen, 255
'                l_imgCharacter.FillChannel f2aBlue, 255
'                l_imgCharacter.AlphaChannel = False
            Case 2
                ' RGBA
                .RawLoad l_imgCharacter.Pointer(0, 0), (CLng(l_intWidth) * CLng(l_intHeight) * 4&)
                If l_intHeight > l_lngHeight Then l_lngHeight = l_intHeight
                l_imgCharacter.SwapChannels Red, Blue
                l_imgCharacter.AlphaChannel = True
            Case Else
                ' Unknown format
                Engine.DebugOut "Unknown version of RFN format: " + Filename
                Exit Function
            End Select
            l_imgCharacter.Optimize
            If l_lngChars > 0 Then
                With m_cpmCharParams(l_lngChars - 1)
                    .pImage = l_imgCharacter.Handle
                    .XIncrement = l_imgCharacter.Width
                    .MapValue = l_lngChars
                End With
            End If
            Set l_imgCharacter = Nothing
        Next l_lngChars
    End With
    m_fpmFontParam.BaseHeight = l_lngHeight
    m_lngFullHeight = l_lngHeight
    If l_intVersion = 1 Then
        m_fpmFontParam.EffectMode = 4
    ElseIf l_intVersion = 2 Then
        m_fpmFontParam.EffectMode = 5
    End If
    m_fpmFontParam.BaseMode = 1
    Refresh
    ImportRFN = True
    Err.Clear
End Function

Function Load(ByVal Filename As String) As Boolean
On Error Resume Next
Dim l_strExtension As String
    If InStr(Filename, ";") Then
        l_strExtension = LCase(Trim(left(Filename, InStr(Filename, ";") - 1)))
        Select Case l_strExtension
        Case "ttf", "ttfont", "truetype", "font"
            Load = ImportTTF(Engine.TTFont(Mid(Filename, InStr(Filename, ";") + 1)))
        Case "uttf", "uttfont", "utruetype", "ufont"
            Load = ImportTTF(Engine.TTFont(Mid(Filename, InStr(Filename, ";") + 1)), True)
        Case Else
            Load = False
        End Select
    Else
        Filename = Engine.Filesystem.AutoSelectExtension(Filename, "f2fnt;f2font;rfn")
        If Engine.Filesystem.FileExists(Filename) Then
        Else
            Exit Function
        End If
        m_lngCharCount = 0
        Set m_colCharImages = Nothing
        If InStr(Filename, ".") Then
            l_strExtension = LCase(Trim(Mid(Filename, InStrRev(Filename, ".") + 1)))
        End If
        Select Case l_strExtension
        Case "rfn"
            Load = ImportRFN(Filename)
        Case "f2fnt", "f2font"
            Load = Engine.LoadFromFile(Me, Engine.Filesystem.File(Filename))
        Case Else
                Load = False
        End Select
    End If
End Function

Function Save(ByVal Filename As String) As Boolean
On Error Resume Next
    Save = Engine.SaveToFile(Me, Engine.Filesystem.CreateFile(Filename))
End Function

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngChars As Long
Dim l_intValue As Integer
Dim l_lngValue As Long
Dim l_lngWidth As Long, l_lngHeight As Long, l_lngSize As Long, l_lngPointer As Long
Dim l_imgChar As Fury2Image
Dim l_lngVersion As Long
    m_lngCharCount = 0
    l_lngVersion = 1
    ReDim m_lngTabStops(0 To 0)
    With File
        .Load m_lngCharCount
        If m_lngCharCount < 1 Then Exit Sub
        .ReadSegment_Begin
            .Load m_strTitle
            .Load m_strAuthor
            .Load m_strCodeset
            If .EOS Then
            Else
                .Load l_lngVersion
            End If
            If .EOS Then
            Else
                .Load Merge
            End If
        .ReadSegment_End
        .ReadSegment_Begin
            .RawLoad VarPtr(m_fpmFontParam), Len(m_fpmFontParam)
            If Not .EOS Then .Load m_lngFullHeight
        .ReadSegment_End
        ReDim m_cpmCharParams(0 To m_lngCharCount - 1)
        .ReadSegment_Begin
            Set m_colCharImages = New Collection
            If l_lngVersion > 3 Then
                For l_lngChars = 0 To m_lngCharCount - 1
                    .ReadSegment_Begin
                        .Load l_lngValue
                        m_cpmCharParams(l_lngChars).MapValue = l_lngValue
                        .Load l_intValue
                        m_cpmCharParams(l_lngChars).XIncrement = l_intValue
                        .Load l_intValue
                        m_cpmCharParams(l_lngChars).YIncrement = l_intValue
                        .Load l_intValue
                        m_cpmCharParams(l_lngChars).XOffset = l_intValue
                        .Load l_intValue
                        m_cpmCharParams(l_lngChars).YOffset = l_intValue
                    .ReadSegment_End
                Next l_lngChars
                .ReadSegment_Begin
                    .Load l_lngValue
                    If l_lngValue > 0 Then
                        ReDim m_bytFontBuffer(0 To l_lngValue)
                        .ReadSegment_Begin
                            .RawLoad ByVal VarPtr(m_bytFontBuffer(0)), l_lngValue
                        .ReadSegment_End
                        .ReadSegment_Begin
                            l_lngPointer = VarPtr(m_bytFontBuffer(0))
                            For l_lngChars = 0 To m_lngCharCount - 1
                                .Load l_lngWidth
                                .Load l_lngHeight
                                l_lngSize = l_lngWidth * l_lngHeight * 4
                                If (l_lngSize <= 0) Then
                                    m_colCharImages.Add Nothing
                                Else
                                    Set l_imgChar = F2ImageFromPointer(l_lngPointer, l_lngWidth, l_lngHeight)
                                    l_imgChar.Optimize
                                    m_colCharImages.Add l_imgChar
                                    l_lngPointer = AddUInts(l_lngPointer, l_lngSize)
                                End If
                            Next l_lngChars
                        .ReadSegment_End
                    Else
                        .ReadSegment_Begin
                        .ReadSegment_End
                        .ReadSegment_Begin
                        .ReadSegment_End
                        Erase m_bytFontBuffer
                    End If
                .ReadSegment_End
            Else
                For l_lngChars = 0 To m_lngCharCount - 1
                    If l_lngVersion > 1 Then
                        .ReadSegment_Begin
                    End If
                    If l_lngVersion > 2 Then
                        .Load l_intValue
                        If l_intValue > 0 Then
                            Set l_imgChar = New Fury2Image
                            .Load l_imgChar
                        Else
                            Set l_imgChar = Nothing
                        End If
                    Else
                        Set l_imgChar = New Fury2Image
                        .Load l_imgChar
                    End If
                    If l_imgChar Is Nothing Then
                    Else
                        If l_imgChar.Width > 255 Or l_imgChar.Height > 255 Then
                            Engine.CriticalError "Fury2Font", "Load Character Images", "Corrupted Font Data"
                            Exit Sub
                        End If
                    End If
                    .Load l_lngValue
                    m_cpmCharParams(l_lngChars).MapValue = l_lngValue
                    .Load l_intValue
                    m_cpmCharParams(l_lngChars).XIncrement = l_intValue
                    .Load l_intValue
                    m_cpmCharParams(l_lngChars).YIncrement = l_intValue
                    If l_lngVersion > 1 Then
                        .Load l_intValue
                        m_cpmCharParams(l_lngChars).XOffset = l_intValue
                        .Load l_intValue
                        m_cpmCharParams(l_lngChars).YOffset = l_intValue
                        .ReadSegment_End
                    End If
                    l_imgChar.Optimize
                    m_colCharImages.Add l_imgChar
                Next l_lngChars
            End If
        .ReadSegment_End
        Refresh
        Err.Clear
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngChars As Long
Dim l_lngWidth As Long, l_lngHeight As Long, l_lngSize As Long
Dim l_lngTotalSize As Long, l_lngPointer As Long
    With File
        .Save m_lngCharCount
        .WriteSegment_Begin
            .Save m_strTitle
            .Save m_strAuthor
            .Save m_strCodeset
            .Save CLng(4)
            .Save Merge
        .WriteSegment_End
        .WriteSegment_Begin
            .RawSave VarPtr(m_fpmFontParam), Len(m_fpmFontParam)
            .Save m_lngFullHeight
        .WriteSegment_End
        .WriteSegment_Begin
            If m_colCharImages.Count > 0 Then
                For l_lngChars = 1 To m_colCharImages.Count
                    .WriteSegment_Begin
                        .Save m_cpmCharParams(l_lngChars - 1).MapValue
                        .Save m_cpmCharParams(l_lngChars - 1).XIncrement
                        .Save m_cpmCharParams(l_lngChars - 1).YIncrement
                        .Save m_cpmCharParams(l_lngChars - 1).XOffset
                        .Save m_cpmCharParams(l_lngChars - 1).YOffset
                    .WriteSegment_End
                Next l_lngChars
                .WriteSegment_Begin
                    For l_lngChars = 1 To m_colCharImages.Count
                        If m_colCharImages(l_lngChars) Is Nothing Then
                        Else
                            With m_colCharImages(l_lngChars)
                                l_lngSize = .Width * .Height * 4
                                l_lngTotalSize = l_lngTotalSize + l_lngSize
                            End With
                        End If
                    Next l_lngChars
                    .Save l_lngTotalSize
'                    ReDim Preserve m_bytFontBuffer(0 To l_lngTotalSize)
'                    l_lngPointer = VarPtr(m_bytFontBuffer(0))
                    .WriteSegment_Begin
                        ' Buffer
                        For l_lngChars = 1 To m_colCharImages.Count
                            If m_colCharImages(l_lngChars) Is Nothing Then
                            Else
                                With m_colCharImages(l_lngChars)
                                    l_lngSize = .Width * .Height * 4
'                                    If (l_lngPointer = .Pointer(0, 0)) Then
'                                    Else
'                                        CopyMemory ByVal l_lngPointer, ByVal .Pointer(0, 0), l_lngSize
'                                    End If
                                    l_lngPointer = AddUInts(l_lngPointer, l_lngSize)
                                    File.RawSave .Pointer(0, 0), l_lngSize
                                End With
                            End If
                        Next l_lngChars
'                        .RawSaveCompressed VarPtr(m_bytFontBuffer(0)), l_lngTotalSize
'                        Erase m_bytFontBuffer
                    .WriteSegment_End
                    .WriteSegment_Begin
                        ' Character bitmap entries
                        For l_lngChars = 1 To m_colCharImages.Count
                            If m_colCharImages(l_lngChars) Is Nothing Then
                                File.Save CLng(0)
                                File.Save CLng(0)
                            Else
                                With m_colCharImages(l_lngChars)
                                    File.Save CLng(.Width)
                                    File.Save CLng(.Height)
                                End With
                            End If
                        Next l_lngChars
                    .WriteSegment_End
                .WriteSegment_End
            End If
'            If m_colCharImages.Count > 0 Then
'                For l_lngChars = 1 To m_colCharImages.Count
'                    .WriteSegment_Begin
'                        If m_colCharImages(l_lngChars) Is Nothing Then
'                            .Save 0
'                        Else
'                            .Save 1
'                            .Save m_colCharImages(l_lngChars)
'                        End If
'                        .Save m_cpmCharParams(l_lngChars - 1).MapValue
'                        .Save m_cpmCharParams(l_lngChars - 1).XIncrement
'                        .Save m_cpmCharParams(l_lngChars - 1).YIncrement
'                        .Save m_cpmCharParams(l_lngChars - 1).XOffset
'                        .Save m_cpmCharParams(l_lngChars - 1).YOffset
'                    .WriteSegment_End
'                Next l_lngChars
'            End If
        .WriteSegment_End
        Err.Clear
    End With
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colSubfonts = New Collection
    ReDim m_lngTabStops(0 To 0)
    ReDim m_sfSubfonts(0 To 0)
    ReDim m_strSubfontNames(0 To 0)
    m_sfSubfonts(0).Font = VarPtr(m_fpmFontParam)
    m_strSubfontNames(0) = Chr(0)
    m_sfSubfonts(0).Name = StrPtr(m_strSubfontNames(0))
    m_fpmFontParam.SubFontCount = 1
    m_fpmFontParam.SubFonts = VarPtr(m_sfSubfonts(0))
    Color = F2White
End Sub
