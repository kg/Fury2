Attribute VB_Name = "mdlAccelerators"
Option Explicit

Sub InitAccelerators()
On Error Resume Next
    With g_edEditor.AcceleratorManager
        .AddAccelerator vbKeyO, g_edEditor, "File_Open", True
        .AddAccelerator vbKeyS, g_edEditor, "File_Save", True
        .AddAccelerator vbKeyS, g_edEditor, "File_SaveAs", True, True
        .AddAccelerator vbKeyX, g_edEditor, "Action_Cut", True
        .AddAccelerator vbKeyC, g_edEditor, "Action_Copy", True
        .AddAccelerator vbKeyV, g_edEditor, "Action_Paste", True
        .AddAccelerator vbKeyZ, g_edEditor, "Action_Undo", True
        .AddAccelerator vbKeyZ, g_edEditor, "Action_Redo", True, True
        .AddAccelerator vbKeyA, g_edEditor, "Action_SelectAll", True
        .AddAccelerator vbKeyD, g_edEditor, "Action_SelectNone", True
        .AddAccelerator vbKeyDelete, g_edEditor, "Action_Delete"
        .AddAccelerator vbKeyF4, g_edEditor, "Action_CloseWindow", True
        .AddAccelerator vbKeyF6, g_edEditor, "Action_NextWindow", True
        .AddAccelerator vbKeyF8, g_edEditor, "Show_FileSidebar"
        .AddAccelerator vbKeyF9, g_edEditor, "Game_Play"
    End With
End Sub
