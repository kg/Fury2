VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cClipboardViewer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Implements ISubclass

Private Declare Function SetClipboardViewer Lib "user32" (ByVal hWnd As Long) As Long
Private Declare Function ChangeClipboardChain Lib "user32" (ByVal hWnd As Long, ByVal hWndNext As Long) As Long
Private Declare Function SendMessageByString Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As String) As Long
Private Declare Function SendMessageByLong Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Private Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long

Private Const WM_DESTROY = &H2
Private Const WM_CHANGECBCHAIN = &H30D
Private Const WM_DRAWCLIPBOARD = &H308

' To Report API errors:
Private Const FORMAT_MESSAGE_ALLOCATE_BUFFER = &H100
Private Const FORMAT_MESSAGE_ARGUMENT_ARRAY = &H2000
Private Const FORMAT_MESSAGE_FROM_HMODULE = &H800
Private Const FORMAT_MESSAGE_FROM_STRING = &H400
Private Const FORMAT_MESSAGE_FROM_SYSTEM = &H1000
Private Const FORMAT_MESSAGE_IGNORE_INSERTS = &H200
Private Const FORMAT_MESSAGE_MAX_WIDTH_MASK = &HFF
Private Declare Function FormatMessage Lib "kernel32" Alias "FormatMessageA" (ByVal dwFlags As Long, lpSource As Any, ByVal dwMessageId As Long, ByVal dwLanguageId As Long, ByVal lpBuffer As String, ByVal nSize As Long, Arguments As Long) As Long

Private m_hWnd As Long
Private m_hWndNextViewer As Long
Private m_bInClipboardChangeNotification As Boolean

Public Event ClipboardChanged()

Public Sub InitClipboardChangeNotification(hWndA As Long)
    StopClipboardChangeNotification
    m_hWnd = hWndA
    If (m_hWnd <> 0) Then
        ' Attach Clipboard 'viewer' notification messages
        AttachMessage Me, m_hWnd, WM_CHANGECBCHAIN
        AttachMessage Me, m_hWnd, WM_DRAWCLIPBOARD
        AttachMessage Me, m_hWnd, WM_DESTROY
        ' Place me in the clipboard viewer notification chain:
        m_hWndNextViewer = SetClipboardViewer(m_hWnd)
        m_bInClipboardChangeNotification = True
    End If
End Sub
Public Sub StopClipboardChangeNotification()
    If (m_bInClipboardChangeNotification) Then
        If (m_hWnd <> 0) Then
            ' Stop subclassing for clipboard messages:
            DetachMessage Me, m_hWnd, WM_CHANGECBCHAIN
            DetachMessage Me, m_hWnd, WM_DRAWCLIPBOARD
            DetachMessage Me, m_hWnd, WM_DESTROY
            ' Take myself out of the clipboard chain:
            ChangeClipboardChain m_hWnd, m_hWndNextViewer
            Debug.Print WinAPIError(Err.LastDllError)
        End If
        m_hWnd = 0
    End If
    m_bInClipboardChangeNotification = False
End Sub

Public Function WinAPIError(ByVal lLastDLLError As Long) As String
Dim sBuff As String
Dim lCount As Long
   
   ' Return the error message associated with LastDLLError:
   sBuff = String$(256, 0)
   lCount = FormatMessage( _
      FORMAT_MESSAGE_FROM_SYSTEM Or FORMAT_MESSAGE_IGNORE_INSERTS, _
      0, lLastDLLError, 0&, sBuff, Len(sBuff), ByVal 0)
   If lCount Then
      WinAPIError = Left$(sBuff, lCount)
   End If
   
End Function


Private Sub Class_Terminate()
   StopClipboardChangeNotification
End Sub

Private Function ISubclass_WindowProc(ByVal hWnd As Long, _
                                      ByVal iMsg As Long, _
                                      ByVal wParam As Long, _
                                      ByVal lParam As Long) As Long
   Select Case iMsg
   Case WM_CHANGECBCHAIN
      ' If the next viewer window is closing, repair the chain:
      m_hWndNextViewer = lParam
      If (m_hWndNextViewer <> 0) Then
         ' Otherwise if there is a next window, pass the message on:
         SendMessageByLong m_hWndNextViewer, iMsg, wParam, lParam
      End If
      ISubclass_WindowProc = 0
        
   Case WM_DRAWCLIPBOARD
      ' the content of the clipboard has changed.
      ' We raise a ClipboardChanged message and pass the message on:
      RaiseEvent ClipboardChanged
      If (m_hWndNextViewer <> 0) Then
         SendMessageByLong m_hWndNextViewer, iMsg, wParam, lParam
      End If
      ISubclass_WindowProc = 0
   
   Case WM_DESTROY
      StopClipboardChangeNotification
      
   End Select
   
End Function

Private Property Get ISubclass_MsgResponse() As EMsgResponse
   ISubclass_MsgResponse = emrPostProcess
End Property

Private Property Let ISubclass_MsgResponse(ByVal emrA As EMsgResponse)
   
End Property



