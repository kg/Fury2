Attribute VB_Name = "mdlIME"
Option Explicit
Public Declare Function ImmAssociateContext Lib "Imm32" (ByVal Window As Long, ByVal Context As Long) As Long
Public Declare Function ImmCreateContext Lib "Imm32" () As Long
Public Declare Function ImmDestroyContext Lib "Imm32" (ByVal Context As Long) As Long
Public Declare Function ImmGetContext Lib "Imm32" (ByVal Window As Long) As Long
Public Declare Function ImmReleaseContext Lib "Imm32" (ByVal Context As Long) As Long
Public Declare Function ImmSetOpenStatus Lib "Imm32" (ByVal Context As Long, ByVal OpenStatus As Long) As Long
Public Declare Function ImmGetCompositionString Lib "Imm32" Alias "ImmGetCompositionStringW" (ByVal Context As Long, ByVal Index As Long, ByVal Buffer As Long, ByVal Length As Long) As Long
Public Declare Function ImmGetDescription Lib "Imm32" Alias "ImmGetDescriptionW" (ByVal Locale As Long, ByVal StringPointer As Long, ByVal StringLength As Long) As Long
Public Declare Function ImmGetGuideLine Lib "Imm32" Alias "ImmGetGuideLineW" (ByVal Context As Long, ByVal Index As Long, ByVal StringPointer As Long, ByVal StringLength As Long) As Long
Public Declare Function ImmGetCandidateListCount Lib "Imm32" (ByVal Context As Long, ByRef Out As Long) As Long
Public Declare Function ImmGetCandidateList Lib "Imm32" Alias "ImmGetCandidateListW" (ByVal Context As Long, ByVal Index As Long, ByVal Pointer As Long, ByVal Length As Long) As Long

Public Const WM_INPUTLANGCHANGEREQUEST   As Long = &H50
Public Const WM_INPUTLANGCHANGE          As Long = &H51
Public Const WM_IME_STARTCOMPOSITION         As Long = &H10D
Public Const WM_IME_ENDCOMPOSITION           As Long = &H10E
Public Const WM_IME_COMPOSITION              As Long = &H10F
Public Const WM_IME_KEYLAST                  As Long = &H10F
Public Const WM_IME_SETCONTEXT               As Long = &H281
Public Const WM_IME_NOTIFY                   As Long = &H282
Public Const WM_IME_CONTROL                  As Long = &H283
Public Const WM_IME_COMPOSITIONFULL          As Long = &H284
Public Const WM_IME_SELECT                   As Long = &H285
Public Const WM_IME_CHAR                     As Long = &H286
Public Const WM_IME_KEYDOWN                  As Long = &H290
Public Const WM_IME_KEYUP                    As Long = &H291
Public Const WM_IME_REQUEST                  As Long = &H288

Public Const IMR_COMPOSITIONWINDOW           As Long = &H1
Public Const IMR_CANDIDATEWINDOW             As Long = &H2
Public Const IMR_COMPOSITIONFONT             As Long = &H3
Public Const IMR_RECONVERTSTRING             As Long = &H4
Public Const IMR_CONFIRMRECONVERTSTRING      As Long = &H5
Public Const IMR_QUERYCHARPOSITION           As Long = &H6
Public Const IMR_DOCUMENTFEED                As Long = &H7

Public Const IMN_CLOSESTATUSWINDOW As Long = 1
Public Const IMN_OPENSTATUSWINDOW As Long = 2
Public Const IMN_CHANGECANDIDATE As Long = 3
Public Const IMN_CLOSECANDIDATE As Long = 4
Public Const IMN_OPENCANDIDATE As Long = 5
Public Const IMN_SETCONVERSIONMODE As Long = 6
Public Const IMN_SETSENTENCEMODE As Long = 7
Public Const IMN_SETOPENSTATUS As Long = 8
Public Const IMN_SETCANDIDATEPOS As Long = 9
Public Const IMN_SETCOMPOSITIONFONT As Long = 10
Public Const IMN_SETCOMPOSITIONWINDOW As Long = 11
Public Const IMN_SETSTATUSWINDOWPOS As Long = 12
Public Const IMN_GUIDELINE As Long = 13
Public Const IMN_PRIVATE As Long = 14

Public Const GCS_COMPREADSTR As Long = 1
Public Const GCS_COMPREADATTR As Long = 2
Public Const GCS_COMPREADCLAUSE As Long = 4
Public Const GCS_COMPSTR As Long = 8
Public Const GCS_COMPATTR As Long = 16
Public Const GCS_COMPCLAUSE As Long = 32
Public Const GCS_CURSORPOS As Long = 128
Public Const GCS_DELTASTART As Long = 256
Public Const GCS_RESULTREADSTR As Long = 512
Public Const GCS_RESULTREADCLAUSE As Long = 1024
Public Const GCS_RESULTSTR As Long = 2048
Public Const GCS_RESULTCLAUSE As Long = 4096

Public Const GGL_STRING As Long = 3

Public Type CANDIDATELISTHeader
    Size As Long
    Style As Long
    Count As Long
    Selection As Long
    PageStart As Long
    PageSize As Long
End Type

Public Type IMECHARPOSITION
    Size As Long
    CharPos As Long
'    pt As PointAPI
    LineHeight As Long
'    DocumentRect as Rect
End Type

Sub ActivateIME(ByVal Handler As Object, ByVal Window As Long)
On Error Resume Next
End Sub

Sub DeactivateIME(ByVal Handler As Object, ByVal Window As Long)
On Error Resume Next
End Sub

