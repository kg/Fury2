VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "APIFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

'
'   Based on APIFile.cls from Planet-Source-Code
'   Author: Jugwine
'

Private Const COPY_ERROR = vbObjectError + 1000
Private Const DEL_ERROR = vbObjectError + 1001
Private Const MOVE_ERROR = vbObjectError + 1002
Private Const RENAME_ERROR = vbObjectError + 1003

'Copies the files specified in the pFrom member to the location
'specified in the pTo member.
Private Const FO_COPY = &H2&

'Deletes the files specified in pFrom (pTo is ignored.)
Private Const FO_DELETE = &H3&

'Moves the files specified in pFrom to the location specified in pTo.
Private Const FO_MOVE = &H1&

'Renames the files specified in pFrom.
Private Const FO_RENAME = &H4&

'Preserve Undo information.
Private Const FOF_ALLOWUNDO = &H40&

'Not currently implemented.
Private Const FOF_CONFIRMMOUSE = &H2&

'handle to the parent window for the progress dialog box.
Private Const FOF_CREATEPROGRESSDLG = &H0&

'Perform the operation on files only if a wildcard file
'name (*.*) is specified.
Private Const FOF_FILESONLY = &H80&

'The pTo member specifies multiple destination files (one for each source file)
'rather than one directory where all source files are to be deposited.
Private Const FOF_MULTIDESTFILES = &H1&

'Respond with Yes to All for any dialog box that is displayed.
Private Const FOF_NOCONFIRMATION = &H10&

'Does not confirm the creation of a new directory if the
'operation requires one to be created.
Private Const FOF_NOCONFIRMMKDIR = &H200&

'Give the file being operated on a new name in a move, copy, or
'rename operation if a file with the target name already exists.
Private Const FOF_RENAMEONCOLLISION = &H8&

'Does not display a progress dialog box.
Private Const FOF_SILENT = &H4&

'Displays a progress dialog box but does not show the file names.
Private Const FOF_SIMPLEPROGRESS = &H100&

'If FOF_RENAMEONCOLLISION is specified, the hNameMappings
'member will be filled in if any files were renamed.
Private Const FOF_WANTMAPPINGHANDLE = &H20&

' The SHFILOPSTRUCT is not double-word aligned. If no steps are
' taken, the last 3 variables will not be passed correctly. This
' has no impact unless the progress title needs to be changed.
   
Private Type SHFILEOPSTRUCT
    hwnd As Long
    wFunc As Long
    pFrom As String
    pTo As String
    fFlags As Integer
    fAnyOperationsAborted As Long
    hNameMappings As Long
    lpszProgressTitle As String
End Type
   
Private Declare Function SHFileOperation Lib "Shell32.dll" _
    Alias "SHFileOperationA" (lpFileOp As Any) As Long
    
'local variable(s) to hold property value(s)
Private mvarNoConfirm As Boolean 'local copy
Private mvarNoConfirmMakeDir As Boolean 'local copy
Private mvarRenameOnCollision As Boolean 'local copy
Private mvarSilent As Boolean 'local copy
Private mvarSimpleProgress As Boolean 'local copy

Public Property Let SimpleProgress(ByVal vData As Boolean)
Attribute SimpleProgress.VB_Description = "If true, the method will show the progress box without the source and target filenames, if false, it will show the full progress box.  Ignored by RenameFile method."
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.SimpleProgress = 5
    mvarSimpleProgress = vData
End Property

Public Property Get SimpleProgress() As Boolean
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.SimpleProgress
    SimpleProgress = mvarSimpleProgress
End Property



Public Property Let Silent(ByVal vData As Boolean)
Attribute Silent.VB_Description = "If true, the method will not show any progress dialog box, if false, it will show the standard or simple progress box, based on the SimpleProgress property setting."
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.Silent = 5
    mvarSilent = vData
End Property

Public Property Get Silent() As Boolean
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Silent
    Silent = mvarSilent
End Property



Public Property Let RenameOnCollision(ByVal vData As Boolean)
Attribute RenameOnCollision.VB_Description = "If true, the method will automatically rename the target file when it's name is already in use.  If false, it will prompt for overwrite or auto-overwrite, based on the NoConfirm property setting.  Ignored by DeleteFile method."
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.RenameOnCollision = 5
    mvarRenameOnCollision = vData
End Property

Public Property Get RenameOnCollision() As Boolean
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.RenameOnCollision
    RenameOnCollision = mvarRenameOnCollision
End Property



Public Property Let NoConfirmMakeDir(ByVal vData As Boolean)
Attribute NoConfirmMakeDir.VB_Description = "If true, the method will automatically make a new directory if the target directory does not exist.  If false, it will prompt the user to create a new directory.  Ignored by methods RenameFile and DeleteFile."
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.NoConfirmMakeDir = 5
    mvarNoConfirmMakeDir = vData
End Property

Public Property Get NoConfirmMakeDir() As Boolean
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.NoConfirmMakeDir
    NoConfirmMakeDir = mvarNoConfirmMakeDir
End Property



Public Property Let NoConfirm(ByVal vData As Boolean)
Attribute NoConfirm.VB_Description = "If true the method will answer ""yes to all"" for any confirmation questions (such as overwrite, etc).  If false, it will present all questions to the user.  Ignored by RenameFile method."
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.NoConfirm = 5
    mvarNoConfirm = vData
End Property

Public Property Get NoConfirm() As Boolean
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.NoConfirm
    NoConfirm = mvarNoConfirm
End Property



Public Sub CopyFile(Filename As String, ToDir As String)
Attribute CopyFile.VB_Description = "Copies file.  Takes arguments: FileName - the source file name as string, and ToDir - the path which the file should be copied to as string."
    'On Error Resume Next
 
    Dim FileStruct As SHFILEOPSTRUCT
    Dim X As Long
    Dim P As Boolean
    Dim strNoConfirm As Integer, strNoConfirmMakeDir As Integer, strRenameOnCollision As Integer
    Dim strSilent As Integer, strSimpleProgress As Integer
    If NoConfirm = True Then
        strNoConfirm = FOF_NOCONFIRMATION
    Else
        strNoConfirm = 0
    End If
    If NoConfirmMakeDir = True Then
        strNoConfirmMakeDir = FOF_NOCONFIRMMKDIR
    Else
        strNoConfirmMakeDir = 0
    End If
    If RenameOnCollision = True Then
        strRenameOnCollision = FOF_RENAMEONCOLLISION
    Else
        strRenameOnCollision = 0
    End If
    If Silent = True Then
        strSilent = FOF_SILENT
    Else
        strSilent = 0
    End If
    If SimpleProgress = True Then
        strSimpleProgress = FOF_SIMPLEPROGRESS
    Else
        strSimpleProgress = 0
    End If
    
    P = DoesFileExist(Filename)
    If P = True Then
        FileStruct.pFrom = Filename
        FileStruct.pTo = ToDir
        FileStruct.fFlags = strNoConfirm + strNoConfirmMakeDir + strRenameOnCollision + strSilent + strSimpleProgress
                
        FileStruct.wFunc = FO_COPY
        X = SHFileOperation(FileStruct)
    Else
        Err.Raise COPY_ERROR, "FileOperations::CopyFile", "APIFileOperations.CopyFile cannot execute because the file to copy from (" & Filename & ") cannot be found."
        'Err.Raise COPY_ERROR, "FileOperations::CopyFile", Err.Description
    End If
End Sub

Public Sub DeleteFile(Filename As String)
Attribute DeleteFile.VB_Description = "Deletes the file.  Takes argument: FileName - the file to be deleted as string."
    'On Error Resume Next
 
    Dim FileStruct As SHFILEOPSTRUCT
    Dim X As Long
    Dim P As Boolean
    Dim strNoConfirm As Integer, strNoConfirmMakeDir As Integer, strRenameOnCollision As Integer
    Dim strSilent As Integer, strSimpleProgress As Integer
    If NoConfirm = True Then
        strNoConfirm = FOF_NOCONFIRMATION
    Else
        strNoConfirm = 0
    End If
    If Silent = True Then
        strSilent = FOF_SILENT
    Else
        strSilent = 0
    End If
    If SimpleProgress = True Then
        strSimpleProgress = FOF_SIMPLEPROGRESS
    Else
        strSimpleProgress = 0
    End If
    
    P = DoesFileExist(Filename)
    If P = True Then
        FileStruct.pFrom = Filename
        FileStruct.fFlags = strNoConfirm + strSilent + strSimpleProgress
        FileStruct.wFunc = FO_DELETE
        X = SHFileOperation(FileStruct)
    Else
        'Err.Raise DEL_ERROR, "FileOperations::CopyFile", "APIFileOperations.DeleteFile cannot execute because the file to be deleted (" & Filename & ") cannot be found."
        'Err.Raise DEL_ERROR, "FileOperations::DeleteFile", Err.Description
    End If
End Sub

Public Sub MoveFile(Filename As String, DestName As String)
Attribute MoveFile.VB_Description = "Moves the file.  Takes arguments: FileName - Source file for move as string, DestName - target path for move as string."
    'On Error Resume Next
 
    Dim FileStruct As SHFILEOPSTRUCT
    Dim P As Boolean
    Dim X As Long
    Dim strNoConfirm As Integer, strNoConfirmMakeDir As Integer, strRenameOnCollision As Integer
    Dim strSilent As Integer, strSimpleProgress As Integer
    If NoConfirm = True Then
        strNoConfirm = FOF_NOCONFIRMATION
    Else
        strNoConfirm = 0
    End If
    If NoConfirmMakeDir = True Then
        strNoConfirmMakeDir = FOF_NOCONFIRMMKDIR
    Else
        strNoConfirmMakeDir = 0
    End If
    If RenameOnCollision = True Then
        strRenameOnCollision = FOF_RENAMEONCOLLISION
    Else
        strRenameOnCollision = 0
    End If
    If Silent = True Then
        strSilent = FOF_SILENT
    Else
        strSilent = 0
    End If
    If SimpleProgress = True Then
        strSimpleProgress = FOF_SIMPLEPROGRESS
    Else
        strSimpleProgress = 0
    End If
    
    P = DoesFileExist(Filename)
    If P = True Then
        FileStruct.pFrom = Filename
        FileStruct.pTo = DestName
     
        FileStruct.fFlags = strNoConfirm + strNoConfirmMakeDir + strRenameOnCollision + strSilent + strSimpleProgress
        FileStruct.wFunc = FO_MOVE
        X = SHFileOperation(FileStruct)
    Else
        Err.Raise MOVE_ERROR, "FileOperations::CopyFile", "APIFileOperations.MoveFile cannot execute because the file to move (" & Filename & ") cannot be found."
        'Err.Raise MOVE_ERROR, "FileOperations::MoveFile", Err.Description
    End If

End Sub

Public Sub RenameFile(Filename As String, Target As String)
Attribute RenameFile.VB_Description = "Renames a file.  Takes arguments: Filename - source filename for rename as string, Target - new filename as string."
    'On Error Resume Next
 
    Dim FileStruct As SHFILEOPSTRUCT
    Dim P As Boolean
    Dim X As Long
    Dim strNoConfirm As Integer, strNoConfirmMakeDir As Integer, strRenameOnCollision As Integer
    Dim strSilent As Integer, strSimpleProgress As Integer
    
    If RenameOnCollision = True Then
        strRenameOnCollision = FOF_RENAMEONCOLLISION
    Else
        strRenameOnCollision = 0
    End If
    If Silent = True Then
        strSilent = FOF_SILENT
    Else
        strSilent = 0
    End If
        
    P = DoesFileExist(Filename)
    If P = True Then
        FileStruct.pFrom = Filename
        FileStruct.pTo = Target
     
        FileStruct.fFlags = strRenameOnCollision + strSilent
        FileStruct.wFunc = FO_RENAME
        X = SHFileOperation(FileStruct)
    Else
        Err.Raise RENAME_ERROR, "FileOperations::CopyFile", "APIFileOperations.CopyFile cannot execute because the file to be renamed (" & Filename & ") cannot be found."
        'Err.Raise RENAME_ERROR, "FileOperation::RenameFile", Err.Description
    End If
End Sub

Friend Function DoesFileExist(NameOfFile As String) As Boolean
       
If Trim(Dir(NameOfFile)) = "" Then
    DoesFileExist = False
Else
    DoesFileExist = True
End If
    
End Function

Private Sub Class_Initialize()

mvarNoConfirm = False
mvarNoConfirmMakeDir = False
mvarRenameOnCollision = True
mvarSilent = False
mvarSimpleProgress = False

End Sub
