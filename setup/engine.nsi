SetCompressor lzma
SetOverwrite ifnewer
SetDatablockOptimize on
SetDateSave on
XPStyle on

Name "Fury² Engine"
Var ALREADY_INSTALLED
!define NAME "Fury² Engine"
!define VERSION "0.8"

!include Library.nsh
!include "MUI.nsh"

  !define MUI_ABORTWARNING

  !insertmacro MUI_PAGE_WELCOME
  !define MUI_LICENSEPAGE_TEXT_TOP "Please read over the changelog and license information before continuing."
  !define MUI_LICENSEPAGE_TEXT_BOTTOM "Click Next to continue."
  !define MUI_LICENSEPAGE_BUTTON "&Next >"
  !insertmacro MUI_PAGE_LICENSE "C:\Documents and Settings\Kevin\My Documents\Projects\fury2\docs\changelog.txt"
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

  !insertmacro MUI_LANGUAGE "English"

  Icon "res\icon.ico"
  UninstallIcon "res\icon.ico"

  OutFile "..\..\binary\setup\fury2_beta_${VERSION}.exe"

  InstallDir "$PROGRAMFILES\Fury²"

;--------------------------------
;Installer Sections

Section "-Engine"
	SetOutPath "$INSTDIR"
    StrCpy $ALREADY_INSTALLED 0
    IfFileExists "$INSTDIR\Uninstall_Engine.exe" old_installation new_installation
     StrCpy $ALREADY_INSTALLED 1
    new_installation:
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayName" "${NAME}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "UninstallString" "$INSTDIR\Uninstall_Engine.exe"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayVersion" "${VERSION}"
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoModify" 1
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoRepair" 1
	WriteUninstaller "$INSTDIR\Uninstall_Engine.exe"
    old_installation:

    IfFileExists $SYSDIR\msvbvm60.dll VBVMFound
        Call ConnectInternet
        StrCpy $2 "$TEMP\vbrun60sp5.exe"
        NSISdl::download http://fury2.luminance.org/downloads/vbrun60sp5.exe $2
        Pop $0
        StrCmp $0 success success
            DetailPrint "download failed: $0"
            Abort
        success:
            ExecWait '"$2"'
            Delete $2
    VBVMFound:

	SetOutPath "$INSTDIR\sys"
	File "J:\development\binary\sys\compressed\fury².exe"
	File "J:\development\binary\sys\compressed\engine.dll"
	File "J:\development\binary\sys\compressed\graphics.dll"
	File "J:\development\binary\sys\compressed\video.dll"
	File "J:\development\binary\sys\compressed\sound2.dll"
	File "J:\development\binary\sys\compressed\packages2.dll"
	File "J:\development\binary\sys\compressed\filesystem.dll"
	File "J:\development\binary\sys\compressed\script2.dll"
	File "J:\development\binary\sys\compressed\scriptengine.dll"
	File "J:\development\binary\sys\compressed\corona.dll"
	File "J:\development\binary\sys\compressed\softfx.dll"
	File "J:\development\binary\sys\compressed\glfx.dll"
	File "J:\development\binary\sys\compressed\fmodex.dll"
	File "J:\development\binary\sys\compressed\Video_GDI.dll"
	File "J:\development\binary\sys\compressed\Video_DirectDraw.dll"
	File "J:\development\binary\sys\compressed\Video_OpenGL.dll"
	File "J:\development\binary\sys\compressed\uikit.dll"
	File "J:\development\binary\sys\compressed\http.dll"
	File "J:\development\binary\sys\compressed\vbenet.dll"

   !insertmacro InstallLib REGDLL $ALREADY_INSTALLED REBOOT_PROTECTED    "J:\development\binary\sys\win32\vbscript.dll" "$SYSDIR\vbscript.dll" "$SYSDIR"

    SetOutPath "$INSTDIR\sys\"
	RegDLL "$INSTDIR\sys\graphics.dll"
	RegDLL "$INSTDIR\sys\video.dll"
	RegDLL "$INSTDIR\sys\engine.dll"
	RegDLL "$INSTDIR\sys\sound2.dll"
	RegDLL "$INSTDIR\sys\packages2.dll"
	RegDLL "$INSTDIR\sys\script2.dll"
	RegDLL "$INSTDIR\sys\scriptengine.dll"
	RegDLL "$INSTDIR\sys\filesystem.dll"
	RegDLL "$INSTDIR\sys\Video_GDI.dll"
	RegDLL "$INSTDIR\sys\Video_DirectDraw.dll"
	RegDLL "$INSTDIR\sys\Video_OpenGL.dll"
	RegDLL "$INSTDIR\sys\uikit.dll"
	RegDLL "$INSTDIR\sys\http.dll"

    SetOutPath "$INSTDIR\sys\shaders\"
    File /nonfatal "J:\development\binary\sys\shaders\*.fs"
    File /nonfatal "J:\development\binary\sys\shaders\*.vs"
    File /nonfatal "J:\development\binary\sys\shaders\*.sh"

    SetOutPath "$INSTDIR\"
SectionEnd

Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\Fury²"
  CreateShortCut "$SMPROGRAMS\Fury²\Fury² Engine.lnk" "$INSTDIR\sys\fury².exe" "" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Uninstall ${NAME}.lnk" "$INSTDIR\uninstall_engine.exe"
SectionEnd

Function ConnectInternet

  Push $R0

    ClearErrors
    Dialer::AttemptConnect
    IfErrors noie3

    Pop $R0
    StrCmp $R0 "online" connected
      MessageBox MB_OK|MB_ICONSTOP "Cannot connect to the internet."
      Quit

    noie3:

    ; IE3 not installed
    MessageBox MB_OK|MB_ICONINFORMATION "Please connect to the internet now."

    connected:

  Pop $R0

FunctionEnd

Section "Uninstall"

  RmDir /r "$SMPROGRAMS\Fury²"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"

	UnRegDLL "$INSTDIR\sys\http.dll"
	UnRegDLL "$INSTDIR\sys\uikit.dll"
	UnRegDLL "$INSTDIR\sys\video.dll"
	UnRegDLL "$INSTDIR\sys\graphics.dll"
	UnRegDLL "$INSTDIR\sys\sound2.dll"
	UnRegDLL "$INSTDIR\sys\packages2.dll"
	UnRegDLL "$INSTDIR\sys\script2.dll"
	UnRegDLL "$INSTDIR\sys\scriptengine.dll"
	UnRegDLL "$INSTDIR\sys\filesystem.dll"
	UnRegDLL "$INSTDIR\sys\video_gdi.dll"
	UnRegDLL "$INSTDIR\sys\video_directdraw.dll"
	UnRegDLL "$INSTDIR\sys\video_opengl.dll"
	UnRegDLL "$INSTDIR\sys\engine.dll"

    RmDir /r "$INSTDIR\sys"
    RmDir /r "$INSTDIR"
SectionEnd
