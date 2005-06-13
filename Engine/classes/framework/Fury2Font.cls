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

Public Function GetTextRectangle(Text, Optional ByVal MaximumWidth As Long = -1) As Fury2Rect
On Error Resume Next
Dim l_lngMaxWidth As Long
Dim l_rctArea As Rectangle
Dim l_optOptions As TextParam
Dim l_lngRefCount As Long
    l_lngMaxWidth = MaximumWidth
    If l_lngMaxWidth < 1 Then l_lngMaxWidth = 32767
    l_rctArea.Width = l_lngMaxWidth
    l_rctArea.Height = 32767
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    If m_booUpperCaseOnly Then
        Call SoftFX.RenderText(UCase(m_Globals.ToString(Text)), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    Else
        Call SoftFX.RenderText(m_Globals.ToString(Text), 0, l_rctArea, m_fpmFontParam, l_optOptions)
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

Public Function TextWidth(Text, Optional ByVal MaxWidth As Long = -1) As Long
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
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    If m_booUpperCaseOnly Then
        Call SoftFX.RenderText(UCase(m_Globals.ToString(Text)), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    Else
        Call SoftFX.RenderText(m_Globals.ToString(Text), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    End If
    TextWidth = l_optOptions.Width
End Function

Public Function TextHeight(Text, Optional ByVal MaxWidth As Long = -1) As Long
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
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    If m_booUpperCaseOnly Then
        Call SoftFX.RenderText(UCase(m_Globals.ToString(Text)), 0, l_rctArea, m_fpmFontParam, l_optOptions)
    Else
        Call SoftFX.RenderText(m_Globals.ToString(Text), 0, l_rctArea, m_fpmFontParam, l_optOptions)
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
    CharacterValue = m_cpmCharParams(Index).MapValue
End Property

Public Property Let CharacterValue(Index, Value As Integer)
On Error Resume Next
    m_cpmCharParams(Index).MapValue = Value
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
        If UBound(TheArray) >= 0 Then .Scroll_X = CLng(TheArray(0))
        If UBound(TheArray) >= 1 Then .Scroll_Y = CLng(TheArray(1))
        If UBound(TheArray) >= 2 Then .Selection_Start = CLng(TheArray(2))
        If UBound(TheArray) >= 3 Then .Selection_End = CLng(TheArray(3))
        If UBound(TheArray) >= 4 Then .Selection_Color = CLng(TheArray(4))
        If UBound(TheArray) >= 5 Then .Caret_Position = CLng(TheArray(5))
        If UBound(TheArray) >= 6 Then .Caret_Color = CLng(TheArray(6))
        If UBound(TheArray) >= 7 Then .MaxChars = CLng(TheArray(7))
    End With
End Sub

Public Function Options(Optional ByVal XScroll As Long = 0, Optional ByVal YScroll As Long = 0, _
    Optional ByVal SelectionStart As Long = 0, Optional ByVal SelectionEnd As Long = 0, _
    Optional ByVal SelectionColor As Long = 0, Optional ByVal CaretPosition As Long = 0, _
    Optional ByVal CaretColor As Long = 0, Optional ByVal MaxCharacters As Long = 0) As Variant
    Options = Array(XScroll, YScroll, SelectionStart, SelectionEnd, SelectionColor, CaretPosition, CaretColor, MaxCharacters)
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
        l_strText = UCase(m_Globals.ToString(Text))
    Else
        l_strText = m_Globals.ToString(Text)
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
    m_fpmFontParam.FillColor = SetAlpha(m_fpmFontParam.FillColor, GetAlpha(m_fpmFontParam.FillColor) * CSng(Alpha))
    If IsMissing(Options) Then
    Else
        ParseOptions Options, l_optOptions
    End If
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    Call SoftFX.RenderText(l_strText, l_imgSurface.Handle, l_rctRectangle.GetRectangle, m_fpmFontParam, l_optOptions)
    Draw = l_optOptions.Lines
    If l_optOptions.Height > 0 Then
        HeightOut = (l_optOptions.Height - m_fpmFontParam.BaseHeight) + m_lngFullHeight
    Else
        HeightOut = 0
    End If
    l_imgSurface.Dirty = True
    m_fpmFontParam.FillColor = l_lngColor
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
        l_strText = UCase(m_Globals.ToString(Text))
    Else
        l_strText = m_Globals.ToString(Text)
    End If
    If Err <> 0 Then Exit Function
    Set l_rctRectangle = Rectangle
    If Err <> 0 Then Exit Function
    ParseOptions Options, l_optOptions
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
    m_fpmFontParam.FillColor = SetAlpha(m_fpmFontParam.FillColor, GetAlpha(m_fpmFontParam.FillColor) * CSng(Alpha))
    If IsMissing(Options) Then
    Else
        ParseOptions Options, l_optOptions
    End If
    l_optOptions.TabStopCount = UBound(m_lngTabStops) + 1
    l_optOptions.TabStopPointer = VarPtr(m_lngTabStops(0))
    Call SoftFX.RenderText(l_strText, l_imgSurface.Handle, l_rctRectangle.GetRectangle, m_fpmFontParam, l_optOptions)
    If l_optOptions.Height > 0 Then
        HeightOut = (l_optOptions.Height - m_fpmFontParam.BaseHeight) + m_lngFullHeight
    Else
        HeightOut = 0
    End If
    DrawCentered = l_optOptions.Lines
    l_imgSurface.Dirty = True
    m_fpmFontParam.FillColor = l_lngColor
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
        For m_lngMoveBack = Index To m_lngCharCount - 1
            m_cpmCharParams(m_lngMoveBack) = m_cpmCharParams(m_lngMoveBack + 1)
        Next m_lngMoveBack
    End If
    m_lngCharCount = m_lngCharCount - 1
    ReDim Preserve m_cpmCharParams(1 To m_lngCharCount)
    Refresh
End Sub

Sub AddCharacter(Image As Fury2Image, Optional MapValue As Long = 0)
On Error Resume Next
    m_lngCharCount = m_lngCharCount + 1
    m_colCharImages.Add Image
    ReDim Preserve m_cpmCharParams(1 To m_lngCharCount)
    m_cpmCharParams(m_lngCharCount).MapValue = CInt(MapValue)
    Refresh
End Sub

Sub Refresh(Optional ResetMappings As Boolean = False)
On Error Resume Next
Dim l_lngChars As Long, l_lngMaxHeight As Long, l_lngMinHeight As Long
    'm_lngFullHeight = 0
    Err.Clear
    l_lngChars = UBound(m_cpmCharParams)
    If l_lngChars <= 0 Or Err <> 0 Then m_lngCharCount = 0: Exit Sub
    If m_lngCharCount < 1 Then Exit Sub
    ReDim m_lngCharMappings(0 To c_lngMaxCharMappings - 1)
    ReDim Preserve m_cpmCharParams(1 To m_lngCharCount)
    For l_lngChars = LBound(m_cpmCharParams) To UBound(m_cpmCharParams) - 1
        With m_cpmCharParams(l_lngChars)
            If (.MapValue <= 0) Or (ResetMappings) Then
                .MapValue = l_lngChars
            End If
            m_lngCharMappings(.MapValue) = VarPtr(m_cpmCharParams(l_lngChars))
            .pImage = m_colCharImages(l_lngChars).Handle
            With m_colCharImages(l_lngChars)
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
        .MapCount = UBound(m_lngCharMappings)
        .MapPointer = VarPtr(m_lngCharMappings(0))
    End With
End Sub

Function CreateFixed(ByVal Width As Long, ByVal Height As Long, Optional ByVal Count As Long = 256) As Boolean
On Error Resume Next
Dim l_lngChars As Long
Dim l_imgCharacter As Fury2Image
    ReDim m_lngTabStops(0 To 0)
    m_strFilename = ""
    m_strTitle = "Font"
    m_lngCharCount = CLng(Count)
    ReDim m_cpmCharParams(1 To m_lngCharCount)
    Set m_colCharImages = New Collection
    If m_lngCharCount < 1 Then Exit Function
    For l_lngChars = 1 To m_lngCharCount
        Set l_imgCharacter = F2Image(CLng(Width), CLng(Height))
        m_colCharImages.Add l_imgCharacter
        With m_cpmCharParams(l_lngChars)
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
        l_imgCharacter.Resize l_imgCharacter.Width + 1, l_imgCharacter.Height + 1
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

Public Sub AdjustGamma(ByVal Gamma As Long)
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
        l_imgCharacter.AdjustChannelGamma 3, GetAlpha(Color)
        l_imgCharacter.AdjustRGBGamma GetRed(Color), GetGreen(Color), GetBlue(Color)
        l_lngCharacter = l_lngCharacter + 1
    Next l_imgCharacter
    Err.Clear
End Sub

Public Property Get CharacterWidth(ByVal Character As Long) As Long
On Error Resume Next
    CharacterWidth = m_cpmCharParams(Character).XIncrement
End Property

Public Property Let CharacterWidth(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character).XIncrement = Value
End Property

Public Property Get CharacterHeight(ByVal Character As Long) As Long
On Error Resume Next
    CharacterHeight = m_cpmCharParams(Character).YIncrement
End Property

Public Property Let CharacterHeight(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character).YIncrement = Value
End Property

Public Property Get CharacterXOffset(ByVal Character As Long) As Long
On Error Resume Next
    CharacterXOffset = m_cpmCharParams(Character).XOffset
End Property

Public Property Let CharacterXOffset(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character).XOffset = Value
End Property

Public Property Get CharacterYOffset(ByVal Character As Long) As Long
On Error Resume Next
    CharacterYOffset = m_cpmCharParams(Character).YOffset
End Property

Public Property Let CharacterYOffset(ByVal Character As Long, ByVal Value As Long)
On Error Resume Next
    m_cpmCharParams(Character).YOffset = Value
End Property

Function ImportTTF(ByVal Font As StdFont) As Boolean
On Error Resume Next
Dim l_lngChars As Long
Dim l_imgNullChar As Fury2Image
Dim m_imgChar As Fury2Image
Dim m_lngMaxHeight As Long
Dim l_lngDC As Long, l_lngDeskWindow As Long, l_lngDeskDC As Long
Dim l_varAttr As Variant, l_lngBaseline As Long
    m_lngFullHeight = 0
    m_lngCharCount = 250
    Set l_imgNullChar = libGraphics.GetGlyphBitmap(Font, 0)
    Engine.DebugOut "Importing TrueType font"
    ReDim m_lngCharMappings(0 To c_lngMaxCharMappings - 1)
    ReDim m_cpmCharParams(1 To m_lngCharCount)
    Set m_colCharImages = New Collection
    For l_lngChars = 1 To 250
        If l_lngChars = 9 Then
            ' Tab
            Set m_imgChar = libGraphics.GetGlyphBitmap(Font, 32)
        Else
            Set m_imgChar = libGraphics.GetGlyphBitmap(Font, l_lngChars)
        End If
        If m_imgChar.Compare(, , l_imgNullChar) = 1 Then
            Set m_imgChar = Nothing
        End If
        m_cpmCharParams(l_lngChars).MapValue = l_lngChars
        m_lngCharMappings(l_lngChars) = VarPtr(m_cpmCharParams(l_lngChars))
        If m_imgChar Is Nothing Then
            m_colCharImages.Add Nothing
        Else
            If m_imgChar.Tag(3) > m_lngMaxHeight Then m_lngMaxHeight = m_imgChar.Tag(3)
            m_cpmCharParams(l_lngChars).pImage = m_imgChar.Handle
            m_colCharImages.Add m_imgChar
            Set m_imgChar = Nothing
        End If
    Next l_lngChars
    For l_lngChars = 2 To 250
        Set m_imgChar = m_colCharImages(l_lngChars)
        If m_imgChar Is Nothing Then
        Else
            ' (GlyphOriginX, GlyphOriginY, BlackBoxX, BlackBoxY, CellIncX, CellIncY, MaxWidth, MaxHeight)
            l_varAttr = m_imgChar.Tag
            With m_imgChar
                .AlphaChannel = True
                If l_lngChars = 9 Then
                    m_cpmCharParams(l_lngChars).XIncrement = CLng(l_varAttr(4) * 8)
                Else
                    m_cpmCharParams(l_lngChars).XIncrement = CLng(l_varAttr(4))
                End If
                m_cpmCharParams(l_lngChars).XOffset = l_varAttr(0)
                l_lngBaseline = l_varAttr(3) - l_varAttr(1)
                m_cpmCharParams(l_lngChars).YOffset = (l_lngBaseline) - (l_varAttr(7) - m_lngMaxHeight)
                m_fpmFontParam.BaseHeight = CLng(l_varAttr(7))
                .Optimize
            End With
            Set m_imgChar = Nothing
        End If
        With m_colCharImages(l_lngChars)
            If (Abs(m_cpmCharParams(l_lngChars).YOffset) + .Height) > m_lngFullHeight Then
                m_lngFullHeight = (Abs(m_cpmCharParams(l_lngChars).YOffset) + .Height)
            End If
        End With
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
End Function

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
    ReDim m_cpmCharParams(1 To m_lngCharCount)
    Set m_colCharImages = New Collection
    With l_imgFile
        Do Until (l_lngY + Height) > .Height
            l_lngX = 0
            Do Until (l_lngX + Width) > .Width
                Set l_imgChar = .Subimage(F2Rect(l_lngX, l_lngY, Width, Height, False))
                l_imgChar.Optimize
                m_colCharImages.Add l_imgChar
                With m_cpmCharParams(m_colCharImages.Count)
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
        ReDim m_cpmCharParams(1 To l_intCount - 1)
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
                With m_cpmCharParams(l_lngChars)
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
        ReDim m_cpmCharParams(1 To m_lngCharCount)
        .ReadSegment_Begin
            Set m_colCharImages = New Collection
            For l_lngChars = 1 To m_lngCharCount - 1
                If l_lngVersion > 1 Then
                    .ReadSegment_Begin
                End If
                Set l_imgChar = New Fury2Image
                .Load l_imgChar
                If l_imgChar.Width > 255 Or l_imgChar.Height > 255 Then Engine.CriticalError "Fury2Font", "Load Character Images", "Corrupted Font Data"
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
        .ReadSegment_End
        Refresh
        Err.Clear
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
Dim l_lngChars As Long
    With File
        .Save m_lngCharCount
        .WriteSegment_Begin
            .Save m_strTitle
            .Save m_strAuthor
            .Save m_strCodeset
            .Save CLng(2)
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
                        .Save m_colCharImages(l_lngChars)
                        .Save m_cpmCharParams(l_lngChars).MapValue
                        .Save m_cpmCharParams(l_lngChars).XIncrement
                        .Save m_cpmCharParams(l_lngChars).YIncrement
                        .Save m_cpmCharParams(l_lngChars).XOffset
                        .Save m_cpmCharParams(l_lngChars).YOffset
                    .WriteSegment_End
                Next l_lngChars
            End If
        .WriteSegment_End
        Err.Clear
    End With
End Sub

Private Sub Class_Initialize()
    ReDim m_lngTabStops(0 To 0)
End Sub
