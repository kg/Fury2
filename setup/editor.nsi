SetCompressor lzma
SetOverwrite ifnewer
SetDatablockOptimize on
SetDateSave on
XPStyle on

Name "Fury² Editor"
Var ALREADY_INSTALLED
!define NAME "Fury² Editor"
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

  OutFile "..\..\binary\setup\fury2_editor_beta_${VERSION}.exe"

  InstallDir "$PROGRAMFILES\Fury²"

;--------------------------------
;Installer Sections

Section "-Editor"
	SetOutPath "$INSTDIR"
    StrCpy $ALREADY_INSTALLED 0
    IfFileExists "$INSTDIR\Uninstall_Editor.exe" old_installation new_installation
     StrCpy $ALREADY_INSTALLED 1
    new_installation:
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayName" "${NAME}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "UninstallString" "$INSTDIR\Uninstall.exe"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayVersion" "${VERSION}"
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoModify" 1
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoRepair" 1
	WriteUninstaller "$INSTDIR\Uninstall_Editor.exe"
    old_installation:
    DeleteRegKey HKLM "Software\Squared Interactive\ngIDE"

    IfFileExists $INSTDIR\sys\engine.dll EngineFound
        Call ConnectInternet
        StrCpy $2 "$TEMP\fury2_editor_installer.exe"
        NSISdl::download http://fury2.luminance.org/downloads/fury2_beta_${VERSION}.exe $2
        Pop $0
        StrCmp $0 success success
            DetailPrint "download failed: $0"
            Abort
        success:
            ExecWait '"$2"'
            Delete $2
    EngineFound:

    SetOutPath "$INSTDIR\sys"
	File "J:\development\binary\sys\compressed\ngIDE.exe"
	File "J:\development\binary\sys\ngIDE.exe.manifest"
	File "J:\development\binary\sys\compressed\ngInterfaces.dll"
	File "J:\development\binary\sys\compressed\ngCommon.dll"
	File "J:\development\binary\sys\compressed\ng.dll"
	File "J:\development\binary\sys\compressed\tk.dll"
	File "J:\development\binary\sys\compressed\debugger.dll"

	SetOutPath "$INSTDIR\sys\editor"
    !insertmacro InstallLib REGDLL $ALREADY_INSTALLED REBOOT_PROTECTED    "J:\development\binary\sys\win32\tlbinf32.dll" "$SYSDIR\tlbinf32.dll" "$SYSDIR"
	File "J:\development\binary\sys\compressed\corona.dll"
	File "J:\development\binary\sys\compressed\softfx.dll"
    File "J:\development\binary\sys\editor\*.dll"
    File "J:\development\binary\sys\editor\*.ocx"

    RegDLL "$INSTDIR\sys\editor\SSubTmr6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    RegDLL "$INSTDIR\sys\editor\MDIActiveX.ocx"
    RegDLL "$INSTDIR\sys\editor\cFScroll.ocx"
    RegDLL "$INSTDIR\sys\editor\cmcs21.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalDTab6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalIml6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalODCL6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalSBar6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalTreeView6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalEdit.ocx"
    RegDLL "$INSTDIR\sys\editor\cNewMenu6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalMDITabs6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalMDISplit6.dll"
    RegDLL "$INSTDIR\sys\editor\ngUI.ocx"

    SetOutPath "$INSTDIR\sys\resources"
	File "J:\development\binary\sys\resources\*.zip"
    SetOutPath "$INSTDIR\sys\icons"
	File "J:\development\binary\sys\icons\*.ico"
    SetOutPath "$INSTDIR\sys\"
	RegDLL "$INSTDIR\sys\ngInterfaces.dll"
	RegDLL "$INSTDIR\sys\ngCommon.dll"
	RegDLL "$INSTDIR\sys\ng.dll"
	RegDLL "$INSTDIR\sys\tk.dll"
	RegDLL "$INSTDIR\sys\debugger.dll"

	SetOutPath "$INSTDIR\"
SectionEnd

Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\Fury²"
  CreateShortCut "$SMPROGRAMS\Fury²\Fury² Editor.lnk" "$INSTDIR\sys\ngIDE.exe" "" "$INSTDIR\sys\ngIDE.exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Uninstall ${NAME}.lnk" "$INSTDIR\uninstall_editor.exe"
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

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"
  DeleteRegKey HKLM "Software\Squared Interactive\ngIDE"

	UnRegDLL "$INSTDIR\sys\debugger.dll"
	UnRegDLL "$INSTDIR\sys\tk.dll"
	UnRegDLL "$INSTDIR\sys\ng.dll"
	UnRegDLL "$INSTDIR\sys\ngInterfaces.dll"
	UnRegDLL "$INSTDIR\sys\ngCommon.dll"
    UnRegDLL "$INSTDIR\sys\editor\SSubTmr6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    UnRegDLL "$INSTDIR\sys\editor\MDIActiveX.ocx"
    UnRegDLL "$INSTDIR\sys\editor\cFScroll.ocx"
    UnRegDLL "$INSTDIR\sys\editor\cmcs21.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalEdit.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalCbEx.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalDTab6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalIml6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalODCL6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalSBar6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalTreeView6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\cNewMenu6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalMDITabs6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalMDISplit6.dll"
    UnRegDLL "$INSTDIR\sys\editor\tlbinf32.dll"
    RmDir /r "$INSTDIR\sys\resources"
    RmDir /r "$INSTDIR\sys\editor"

    RmDir /r "$INSTDIR\sys"
    RmDir /r "$INSTDIR"
SectionEnd
