Attribute VB_Name = "mdlRegSvr"
'
'   ::fury2 regsvr substitute module::
'

Public Declare Function GetProcAddress _
 Lib "kernel32" _
 (ByVal hModule As Long, _
  ByVal lpProcName As String) _
 As Long

Public Declare Function LoadLibrary _
  Lib "kernel32" Alias "LoadLibraryA" _
  (ByVal lpLibFileName As String) As Long

Public Declare Function CreateThread Lib "kernel32" _
(lpThreadAttributes As Long, ByVal dwStackSize As Long, _
lpStartAddress As Long, lpParameter As Any, ByVal dwCreationFlags As Long, _
lpThreadId As Long) As Long

Public Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Public Declare Function GetExitCodeThread Lib "kernel32" (ByVal hThread As Long, lpExitCode As Long) As Long
Public Declare Sub ExitThread Lib "kernel32" (ByVal dwExitCode As Long)
Public Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
Public Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long

Function VBRegSvr32(ByVal sServerPath As String, _
                                         Optional fRegister = True) As Boolean
Dim hMod As Long            ' module handle
Dim lpfn As Long            ' reg/unreg function address
Dim lpThreadId As Long      ' dummy var that get's filled
Dim hThread As Long         ' thread handle
Dim fSuccess As Boolean     ' if things worked
Dim dwExitCode As Long      ' thread's exit code if it doesn't finish

  Screen.MousePointer = vbHourglass

  ' Load the server into memeory
  hMod = LoadLibrary(sServerPath)
      
  ' Get the specified function's address and our msgbox string.
  If fRegister Then
      lpfn = GetProcAddress(hMod, "DllRegisterServer")
  Else
      lpfn = GetProcAddress(hMod, "DllUnregisterServer")
  End If
    
  ' If we got a function address...
  If lpfn Then
      ' Create an alive thread and execute the function.
      hThread = CreateThread(ByVal 0, 0, ByVal lpfn, ByVal 0, 0, lpThreadId)
        
      ' If we got the thread handle...
      If hThread Then
          ' Wait 10 secs for the thread to finish (the function may take a while...)
          fSuccess = (WaitForSingleObject(hThread, 10000) = WAIT_OBJECT_0)
          
          ' If it didn't finish in 5 seconds...
          If Not fSuccess Then
              ' Something unlikely happened, lose the thread.
              Call GetExitCodeThread(hThread, dwExitCode)
              Call ExitThread(dwExitCode)
          End If
    
      ' Lose the thread handle
      Call CloseHandle(hThread)
      End If   ' hThread
  End If   ' lpfn
    
  ' Free server if we loaded it.
  If hMod Then Call FreeLibrary(hMod)
    
  Screen.MousePointer = vbDefault
    
  If fSuccess Then
      VBRegSvr32 = True
  Else
      MsgBox ("Error: " & sServerPath)
      VBRegSvr32 = False
  End If
  
End Function

