SetCompressor lzma
SetOverwrite ifnewer
SetDatablockOptimize on
SetDateSave on
XPStyle on

Name "Fury²"
!define NAME "Fury²"

!include "MUI.nsh"

;--------------------------------
;Configuration

  !define MUI_ABORTWARNING

  !insertmacro MUI_PAGE_WELCOME
  !define MUI_LICENSEPAGE_TEXT_TOP "Please read over the changelog and license information before continuing."
  !define MUI_LICENSEPAGE_TEXT_BOTTOM "Click Next to continue."
  !define MUI_LICENSEPAGE_BUTTON "&Next >"
  !insertmacro MUI_PAGE_LICENSE "Z:\projects\fury2\docs\changelog.txt"
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

  ;Language
  !insertmacro MUI_LANGUAGE "English"
  
  Icon "icon.ico"
  UninstallIcon "icon.ico"

  ;General
  OutFile "fury2_beta_03.exe"

  ;Folder-selection page
  InstallDir "$PROGRAMFILES\${NAME}"

;--------------------------------
;Installer Sections

Section "-Engine"
	SetOutPath "$INSTDIR"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayName" "${NAME}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "UninstallString" "$INSTDIR\Uninstall.exe"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayVersion" "0.2"
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoModify" 1
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoRepair" 1
	WriteUninstaller "$INSTDIR\Uninstall.exe"
                
    SetOutPath "$SYSDIR"
    File "J:\development\binary\sys\compressed\vbscript.dll"
    RegDLL "$SYSDIR\vbscript.dll"
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
	File "J:\development\binary\sys\compressed\sound.dll"
	File "J:\development\binary\sys\compressed\packages.dll"
	File "J:\development\binary\sys\compressed\filesystem.dll"
	File "J:\development\binary\sys\compressed\script2.dll"
	File "J:\development\binary\sys\compressed\scriptengine.dll"
	File "J:\development\binary\sys\compressed\softfx.dll"
	File "J:\development\binary\sys\compressed\fmod.dll"
	File "J:\development\binary\sys\compressed\video_gdi.dll"
	File "J:\development\binary\sys\compressed\uikit.dll"
        
    SetOutPath "$INSTDIR\sys\"
	RegDLL "$INSTDIR\sys\graphics.dll"
	RegDLL "$INSTDIR\sys\engine.dll"
	RegDLL "$INSTDIR\sys\sound.dll"
	RegDLL "$INSTDIR\sys\packages.dll"
	RegDLL "$INSTDIR\sys\script2.dll"
	RegDLL "$INSTDIR\sys\scriptengine.dll"
	RegDLL "$INSTDIR\sys\filesystem.dll"
	RegDLL "$INSTDIR\sys\video_gdi.dll"
	RegDLL "$INSTDIR\sys\uikit.dll"

	SetOutPath "$INSTDIR\"
SectionEnd

Section "Editor"                
  DeleteRegKey HKLM "Software\Squared Interactive\ngIDE"

    SetOutPath "$SYSDIR"
    File "J:\development\binary\sys\editor\tlbinf32.dll"
    RegDLL "$SYSDIR\tlbinf32.dll"

	SetOutPath "$INSTDIR\sys"
	File "J:\development\binary\sys\compressed\ngIDE.exe"
	File "J:\development\binary\sys\ngIDE.exe.manifest"
	File "J:\development\binary\sys\compressed\ngInterfaces.dll"
	File "J:\development\binary\sys\compressed\ngCommon.dll"
	File "J:\development\binary\sys\compressed\ng.dll"
    
	SetOutPath "$INSTDIR\sys\editor"
    File "J:\development\binary\sys\editor\*.*"
    
    RegDLL "$INSTDIR\sys\editor\SSubTmr6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    RegDLL "$INSTDIR\sys\editor\MDIActiveX.ocx"
    RegDLL "$INSTDIR\sys\editor\cFScroll.ocx"
    RegDLL "$INSTDIR\sys\editor\cmcs21.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalDkTb6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalDTab6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalIml6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalODCL6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalSBar6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalScrb6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalTbar6.ocx"
    RegDLL "$INSTDIR\sys\editor\vbalTreeView6.ocx"
    RegDLL "$INSTDIR\sys\editor\cNewMenu6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalMDITabs6.dll"
    RegDLL "$INSTDIR\sys\editor\vbalMDISplit6.dll"
    
    SetOutPath "$INSTDIR\sys\"
	RegDLL "$INSTDIR\sys\ngInterfaces.dll"
	RegDLL "$INSTDIR\sys\ngCommon.dll"
	RegDLL "$INSTDIR\sys\ng.dll"

	SetOutPath "$INSTDIR\"
SectionEnd

Section "Example Game"
	SetOutPath "$INSTDIR\Example Game"
    File /nonfatal /r "Z:\projects\fury2\docs\Test Game\*.*"
SectionEnd

Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\${NAME}"
  IfFileExists "$INSTDIR\sys\ngIDE.exe" EditorInstalled EditorNotInstalled
EditorInstalled:
  CreateShortCut "$SMPROGRAMS\${NAME}\${NAME} Editor.lnk" "$INSTDIR\sys\ngIDE.exe" "" "$INSTDIR\sys\ngIDE.exe"
EditorNotInstalled:
  IfFileExists "$INSTDIR\example game\game.f2config" ExampleInstalled ExampleNotInstalled
ExampleInstalled:
  CreateShortCut "$SMPROGRAMS\${NAME}\${NAME} Example Game.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\example game" "$INSTDIR\sys\fury².exe"
ExampleNotInstalled:
  CreateShortCut "$SMPROGRAMS\${NAME}\${NAME} Engine.lnk" "$INSTDIR\sys\fury².exe" "" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\${NAME}\Uninstall ${NAME}.lnk" "$INSTDIR\uninstall.exe"
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
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!
  
  RmDir /r "$SMPROGRAMS\${NAME}"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"
  DeleteRegKey HKLM "Software\Squared Interactive\ngIDE"

	UnRegDLL "$INSTDIR\sys\uikit.dll"
	UnRegDLL "$INSTDIR\sys\graphics.dll"
	UnRegDLL "$INSTDIR\sys\sound.dll"
	UnRegDLL "$INSTDIR\sys\packages.dll"
	UnRegDLL "$INSTDIR\sys\script.dll"
	UnRegDLL "$INSTDIR\sys\scriptengine.dll"
	UnRegDLL "$INSTDIR\sys\filesystem.dll"
	UnRegDLL "$INSTDIR\sys\video_gdi.dll"
	UnRegDLL "$INSTDIR\sys\engine.dll"

    IfFileExists "$INSTDIR\sys\ngIDE.exe" UninstallEditor SkipEditor
UninstallEditor:
	UnRegDLL "$INSTDIR\sys\ng.dll"
	UnRegDLL "$INSTDIR\sys\ngInterfaces.dll"
	UnRegDLL "$INSTDIR\sys\ngCommon.dll"
    UnRegDLL "$INSTDIR\sys\editor\SSubTmr6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    UnRegDLL "$INSTDIR\sys\editor\MDIActiveX.ocx"
    UnRegDLL "$INSTDIR\sys\editor\cFScroll.ocx"
    UnRegDLL "$INSTDIR\sys\editor\cmcs21.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalCbEx.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalDkTb6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalDTab6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalIml6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalODCL6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalSBar6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalScrb6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalTbar6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\vbalTreeView6.ocx"
    UnRegDLL "$INSTDIR\sys\editor\cNewMenu6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalHook6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalMDITabs6.dll"
    UnRegDLL "$INSTDIR\sys\editor\vbalMDISplit6.dll"
    RmDir /r "$INSTDIR\sys\editor"
SkipEditor:

    RmDir /r "$INSTDIR\sys"
    RmDir /r "$INSTDIR"
SectionEnd