VERSION 5.00
Begin VB.UserControl MDIActiveX 
   ClientHeight    =   810
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   1830
   InvisibleAtRuntime=   -1  'True
   Picture         =   "MDIActiveX.ctx":0000
   ScaleHeight     =   810
   ScaleWidth      =   1830
   ToolboxBitmap   =   "MDIActiveX.ctx":0B82
End
Attribute VB_Name = "MDIActiveX"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "MDI Kindfenster in ActiveX-DLLs erstellen - Einfach eine Instanz in die MDIForm und in die Form der ActiveX-DLL laden. dort KeyPreview auf True setzten - Fertig!"
Option Explicit
'-------------------------------------------------------------'
' Copyright (c)2000 by Michael Wallner                        '
' http://wallner.homepage.com                                 '
' eMail: michael.wallner@iname.com                            '
'-------------------------------------------------------------'
' MDIActiveX Control Version 1.0.3                            '
'                                                             '
' Module: Definitionen                                        '
'         Funktionen                                          '
'-------------------------------------------------------------'

'---------------------'
'Private definitionen '
'---------------------'
Private m_booDead As Boolean

Private m_inMDI As Boolean              'Control run in MDIForm
Private m_inUserMode As Boolean         'Control run in UserMode
Private m_hMenu As Long                 'handle menu of the activex form
Private m_hFileMenu As Long             'handel windowlist menu of the activex form

'MDIChild Forms
Private m_State As Long                 'current window is maximized ?
Private m_hWndParent As Long            'handle to 'real' mdi child window
Private m_hWndForm As Long              'handle ActiveX mdi form (control parentform)
Private m_ignorekey As Boolean
Private WithEvents myForm As Form       'parent form
Attribute myForm.VB_VarHelpID = -1
Private m_lngHWnd As Long

'MDIForm
Private m_MDIClient As Long             'handle MDICLient window
Private WithEvents myMDIForm As MDIForm 'parent form
Attribute myMDIForm.VB_VarHelpID = -1


'---------------------'
'Public Properties    '
'---------------------'

'eine Kollection aller geladenen MDIChild formularen welche mit MDIActiveX erstellt wurden
'a collection of all MDIChild forms created with the mdiactivex control
Public Forms As Collection

Public Property Get hwnd() As Long
    hwnd = m_hWndParent
End Property

Public Property Get ActiveWindow() As Long
    ActiveWindow = SendMessage(m_MDIClient, WM_MDIGETACTIVE, 0, ByVal 0)
End Property

'Handle auf das Menü des ActiveX Formulares
'handle to the axtivex form menu
Property Get hMenu() As Long            'get menu handle
    hMenu = m_hMenu
End Property

'version of the control
Property Get Version() As String
    Version = App.Major & "." & App.Minor & "." & App.Revision
End Property

'copyright information
Property Get Copyright() As String
    Copyright = "Copyright (c)2000 by Michael Wallner http://wallner.homepage.com"
End Property

'gibt die MDIForm des Projketes zurück
'get the MDIForm of the project
Property Get MDIForm() As Object
    Dim hParent As Long
    Dim f As Form
    Dim pointer As Long

    If m_inMDI Then
        Err.Raise vbObjectError + 1050, "MDIActiveX Extender Control", "Not in MDIChild!"
        Exit Property
    End If

    hParent = GetParent(GetParent(m_hWndParent))
    If hParent <> 0 Then
        pointer = GetProp(hParent, "pMDIFrame")
        If pointer <> 0 Then
            CopyMemory f, pointer, 4
        End If
    End If
    Set MDIForm = f
    If pointer <> 0 Then
        CopyMemory f, 0&, 4
    End If
End Property

'Gibt das active MFIChild Fromular zurück- egal ob normales oder mit MDIActiveX erstelltes
'Get the active childform (normal MDIChild or an activex form with mdiactivex control
Property Get ActiveForm() As Object
    Dim hChild As Long
    Dim f As Form
    Dim pointer As Long

    If Not m_inMDI Then
        Err.Raise vbObjectError + 1051, "MDIActiveX Extender Control", "Not in MDIForm!"
        Exit Property
    End If

    hChild = SendMessage(m_MDIClient, WM_MDIGETACTIVE, 0, ByVal 0&)
    If hChild <> 0 Then
        'Get pointer to the form object
        pointer = GetProp(hChild, "pMDIChild")
        If pointer <> 0 Then
            'copy adress to object pointer
            CopyMemory f, pointer, 4
        Else
            'pointer=0 --> normal MDIChild --> return MDIForm property!
            Set f = myMDIForm.ActiveForm
        End If
    End If
    Set ActiveForm = f                          'Return object

    If pointer <> 0 Then
        'if work with copymemory then take back!
        CopyMemory f, 0&, 4
    End If
End Property

Public Property Get Visible() As Boolean
    ' NYI
    Visible = True
End Property

Public Property Let Visible(Value As Boolean)
    ' NYI
    If m_booDead Then Exit Property
    If Value Then
        ShowWindow m_hWndParent, 5
    Else
        ShowWindow m_hWndParent, 0
    End If
End Property

Public Sub SetFocus()
    If m_booDead Then Exit Sub
    ShowWindow ActiveWindow, 0
    ShowWindow m_hWndParent, 3
    ShowWindow m_lngHWnd, 5
    APISetFocus m_lngHWnd
    SendMessage GetParent(m_hWndParent), WM_MDIACTIVATE, m_hWndParent, ByVal 0
    SendMessage GetParent(m_hWndParent), WM_MDIMAXIMIZE, m_hWndParent, ByVal 0
    SendMessage m_hWndParent, WM_SIZE, 0, ByVal 0
    SendMessage m_lngHWnd, WM_SIZE, 0, ByVal 0
End Sub

'Gibt WindowState des ActiveX Formulars in welchem das Control geladen wurde oder
'WindowState des activen MDIChild Fensters (wenn in MDIForm geladen) zurück
'get the windowstate of the activex form (when control in an activex form)
'or the windowstate of the actual mdichild (when control in the MDIForm)
Property Get WindowState() As Long
    Dim hWindow As Long

    If m_inMDI = False Then
        hWindow = m_hWndParent
    Else
        hWindow = SendMessage(m_MDIClient, WM_MDIGETACTIVE, 0, ByVal 0&)
    End If
    WindowState = 0
    If hWindow <> 0 Then
        If IsZoomed(hWindow) Then
            WindowState = 2
        ElseIf IsIconic(hWindow) Then
            WindowState = 1
        Else
            WindowState = 0
        End If
    End If
End Property

'Setzt WindowState des ActiveX Formulars in welchem das Control geladen wurde oder
'setzt den WindowState des activen MDIChildFensters (wenn in MDIForm geladen)
'set the windowstate of the activex form (when control in an activex form)
'or the windowstate of the actual mdichild (when control in the MDIForm)
Property Let WindowState(State As Long)
    If m_booDead Then Exit Property
    Dim hWindow As Long

    If m_inMDI Then
        hWindow = SendMessage(m_MDIClient, WM_MDIGETACTIVE, 0, ByVal 0&)
    Else
        hWindow = m_hWndParent
    End If

    If hWindow <> 0 Then
        If State = 2 Then
            SendMessage GetParent(hWindow), WM_MDIMAXIMIZE, hWindow, ByVal 0&
        ElseIf State = 1 Then
            ShowWindow hWindow, SW_MINIMIZE
        Else
            SendMessage GetParent(hWindow), WM_MDIRESTORE, hWindow, ByVal 0&
        End If
    End If
End Property

'Ab Version 1.0.2 wird diese Methode nicht mehr benötigt!
'can be ignored since version 1.0.2
Public Sub EvalKeyDown(KeyCode As Integer, Shift As Integer)
End Sub


'---------------------'
'Private functions    '
'---------------------'

'Init wenn das Control in einer MDIForm geladen wird
'Init if control is in the MDIForm of the project
Private Sub InitParent()
    m_inMDI = True
    If m_inUserMode Then
        Set Forms = New Collection
        Set myMDIForm = UserControl.Extender.Parent
        SetProp myMDIForm.hwnd, "pMDIFrame", ObjPtr(myMDIForm)
        m_MDIClient = GetWindow(m_hWndForm, GW_CHILD)     'MDIClient Fenster
        m_lngHWnd = m_MDIClient
        'start subclassing
        SubStartMDI m_MDIClient
        SubStartMDI m_hWndForm
        myRegisterClass                                   'register WindowClass
    End If
End Sub

'Init wenn das Control in ein Formular einer ActiveX-DLL geladen wird
'Init if control is in an activex form
Private Sub InitChild()
    Dim hMDI As Long
    Dim lStyle As Long
    Dim hwnd As Long
    Dim hIcon As Long

    m_inMDI = False
    If m_inUserMode Then
        Set myForm = UserControl.Extender.Parent
        'find MDIFrame window...
        hMDI = GetMDIFrame
        If hMDI = 0 Then
            MsgBox "Can't find MDIForm!"
            Unload UserControl.Extender.Parent          '!!!!!
            Exit Sub
        End If
        hIcon = SendMessage(myForm.hwnd, WM_GETICON, 0, ByVal 0&)
        SendMessage GetWindow(hMDI, GW_CHILD), WM_MDIGETACTIVE, 0, m_State
        'create MDIChild Window...
        m_hWndParent = CreateMDIChild(hMDI, UserControl.Extender.Parent.Caption)
        m_lngHWnd = m_hWndParent
        If m_hWndParent = 0 Then
            MsgBox "Can't create MDIChild window!"
            Unload UserControl.Extender.Parent          '!!!!!
            Exit Sub
        End If
        'start subclassing  activex form
        SubStart m_hWndForm
    End If

End Sub

'init control
Private Sub Init()
    On Error Resume Next

    m_hWndForm = UserControl.Extender.Parent.hwnd

    If UserControl.Ambient.UserMode Then
        m_inUserMode = True
    Else
        m_inUserMode = False
    End If

    'test if parent is Form or MDIForm...
    If TypeOf UserControl.Extender.Parent Is MDIForm Then
        InitParent
    ElseIf TypeOf UserControl.Extender.Parent Is Form Then
        InitChild
    Else
        MsgBox "This Control is designed for Visual Basic!"
    End If


End Sub

'damit beim öffnen einer Form der Focus richtig gesetzt wird,
'ist diese Procedur nötig
'event activex form activate
Private Sub myForm_Activate()
    Dim hParent As Long
    'set focus to the right control!
    hParent = GetParent(myForm.hwnd)
    If hParent = SendMessage(GetParent(hParent), WM_MDIGETACTIVE, 0, ByVal 0&) Then
        SendMessage hParent, WM_NCACTIVATE, 1, ByVal 0& 'activate form
    End If
End Sub

'Kombinationen mit der Strg und Alt Taste müssen besonders behandelt werden!
'event activex keydown
'ctrl+F6, ctrl+F4 and alt+ accelerator keys!
Private Sub myForm_KeyDown(KeyCode As Integer, Shift As Integer)
    ' what the hell is going on in this stupid function
    Dim lParam As Long
    'Ctrl+F6
    If KeyCode = 117 And (Shift = 2 Or Shift = 3) Then
        If Shift = 2 Then
            PostMessage GetParent(m_hWndForm), WM_SYSCOMMAND, SC_NEXTWINDOW, ByVal 0&
        Else
            PostMessage GetParent(m_hWndForm), WM_SYSCOMMAND, SC_PREVWINDOW, ByVal 0&
        End If
        'Ctrl + F4
'    ElseIf KeyCode = 115 And (Shift = 2 Or Shift = 3) Then
'        PostMessage GetParent(m_hWndForm), WM_CLOSE, 0, ByVal 0&
'        'Alt + ? menu accelerator!
'    ElseIf Shift = 4 And KeyCode <> 18 And KeyCode <> 115 And KeyCode <> 32 Then
'        lParam = ((KeyCode Or &H2000) * &H10000) Or &H1
'        PostMessage GetParent(GetParent(m_hWndForm)), WM_SYSCHAR, KeyCode, lParam
'        'Alt + Space
    ElseIf Shift = 4 And KeyCode = 32 Then
        m_ignorekey = True
'    ElseIf Shift <> 0 Then
'        'if there is no form  menu  --> send to 'real' MDIChild Window
'        If GetProp(GetParent(m_hWndForm), "hMDIMenu") = 0 Then
'            Debug.Print "an Parrent", KeyCode, Shift
'            PostMessage GetParent(GetParent(m_hWndForm)), WM_KEYDOWN, KeyCode, 0
'            m_ignorekey = True
'        End If
    End If

End Sub

'wurde ein Tastendruck an die MDIForm weitergeleitet,
'so muß er in der ActiveX Form unterdrückt werden
'event activex form keypress
Private Sub myForm_KeyPress(KeyAscii As Integer)
    'if key is send to main window -> ignore key or we will get a beep!
    If m_ignorekey Then
        m_ignorekey = False
        KeyAscii = 0
    End If
End Sub

'wurde die Activex Form geladen, so muß sie erst zu einer MDIChild From gemacht werden!
'event activex form load
'after the form is loaded, there's a lot of work to do!
Private Sub myForm_Load()
    Dim hParent As Long
    Dim hIcon As Long
    Dim ctl As Control

    'Jetzt gibt es bereits ein Menu (falls die Form ein eigenes Menu hat!)
    'this ist the rigth moment to look for a menu!
    'if there is a form menu
    m_hMenu = GetMenu(myForm.hwnd)
    'and a WindowList
    m_hFileMenu = FindWindowMenu(m_hMenu)

    'Fenster zu Kindfenster machen
    MakeChild myForm.hwnd                  'change WindowStyle(ex)
    ParentNotify myForm.hwnd               'erase WS_EX_NOPARENTNOTIFY styles
    SetProp m_hWndParent, "pMDIChild", ObjPtr(myForm)
    SetParent myForm.hwnd, m_hWndParent    'change Parent
    Resize myForm.hwnd                     'correct size
    hParent = GetParent(myForm.hwnd)
    'Speichere die Menuhandles des Fensters
    SetProp hParent, "hMDIMenu", m_hMenu        'store handle menu
    SetProp hParent, "hFileMenu", m_hFileMenu   'store handle windowlist

    'set icon
    hIcon = SendMessage(myForm.hwnd, WM_GETICON, 0, ByVal 0&)
    SendMessage m_hWndParent, WM_SETICON, 0, ByVal hIcon

    'die Form zu der Forms Auflistung in der MDIForm hinzufügen
    'add the form to the forms collection of the mdiactivex control in the MDIForm
    For Each ctl In Me.MDIForm.Controls
        If TypeOf ctl Is MDIActiveX Then
            'found MDIActivex.ocx
            ctl.Forms.Add myForm, CStr(m_hWndParent)
            Exit For
        End If
    Next

    'War die zuletzt aktive Form maximiert?
    'was the active mdichild window maximized?
    If m_State = 1 Then
        'yes --> maximize me
        SendMessage GetParent(m_hWndParent), WM_MDIMAXIMIZE, m_hWndParent, ByVal 0&
    End If

    'Aktiviere die gerade geladene Form
    'activate the activex form
    SendMessage hParent, WM_NCACTIVATE, 1, ByVal 0& 'activate form

End Sub

Private Sub myForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    m_booDead = True
    Dim ctl As Control
    If m_inUserMode Then
        If m_inMDI Then
        Else
            myForm.Visible = False
            'delete from the forms collection in the MDIForm
            For Each ctl In Me.MDIForm.Controls
                If TypeOf ctl Is MDIActiveX Then
                    ctl.Forms.Remove CStr(m_hWndParent)
                    Exit For
                End If
            Next
            SubStop m_hWndForm
        End If
    End If
End Sub

Private Sub myForm_Unload(Cancel As Integer)
On Error Resume Next
    Dim ctl As Control
    If m_inUserMode Then
        If m_inMDI Then
        Else
            SetWindowText m_hWndParent, ""
            ShowWindow m_hWndParent, 0
'            DestroyWindow m_hWndParent
        End If
    End If
End Sub

'event MDIForm load
Private Sub myMDIForm_Load()
    'Original Menuhandle der Applikation sichern
    'save the menu handle
    g_oldhMenu = GetMenu(myMDIForm.hwnd)
    m_hMenu = g_oldhMenu
End Sub

'event MDIForm unload
Private Sub myMDIForm_Unload(Cancel As Integer)
    Dim hChild As Long
    Dim hChild1 As Long

    'erstes MDIChild suchen
    'find first MDIChild window
    hChild = GetWindow(m_MDIClient, GW_CHILD)
    While hChild <> 0
        'save handle
        hChild1 = hChild
        'Schließe das Fenster / close first MDIChild
        SendMessage hChild, WM_CLOSE, 0, ByVal 0
        'neues erstes MDIChild finden / find new first MDIChild window
        hChild = GetWindow(m_MDIClient, GW_CHILD)
        'ist 'neue' = 'altem' ---> abbrechen!
        'if this is the same window ---> Cancel!
        If hChild = hChild1 Then
            Cancel = True
            Exit Sub
        End If
        'else perform new MDIChild window
    Wend

End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
    Init
End Sub

Private Sub UserControl_Resize()
    UserControl.Height = UserControl.ScaleX(30, vbPixels, UserControl.ScaleMode)
    UserControl.Width = UserControl.ScaleY(32, vbPixels, UserControl.ScaleMode)
End Sub

Private Sub UserControl_Terminate()
    Dim ctl As Control
    If m_inUserMode Then
        If m_inMDI Then
            SubStop m_MDIClient
            SubStop m_hWndForm
            myUnregisterClass
            RemoveProp myMDIForm.hwnd, "pMDIFrame"
        Else
        End If
    End If
End Sub

'suche das Menü mit der Eigenschaft WindowList = true
'find the windowList menu in a child window
Private Function FindWindowMenu(hMenu As Long) As Long
    Dim c As Control
    Dim m As Menu
    Dim Caption As String
    Dim i As Long
    Dim cMenu As Long
    Dim hSubMenu As Long
    Dim mCaption As String
    Dim Result As Long

    Caption = ""

    'Find VB-Menu with WindowList
    For Each c In myForm.Controls
        If TypeOf c Is Menu Then
            Set m = c
            If m.WindowList Then
                Caption = m.Caption
                Exit For
            End If
        End If
    Next

    If Caption <> "" Then
        cMenu = GetMenuItemCount(m_hMenu)         'number of main menus
        For i = 0 To cMenu - 1
            hSubMenu = GetSubMenu(m_hMenu, i)     'get submenu Handle
            mCaption = String(255, 0)
            'get menu caption
            Result = GetMenuString(m_hMenu, i, mCaption, 254, MF_BYPOSITION)
            mCaption = Left$(mCaption, Result)
            If mCaption = Caption Then            'caption = caption.vb-menu?
                FindWindowMenu = hSubMenu         'yes --> return handle
                Exit For                          'and exit loop
            End If
        Next
    End If

End Function
