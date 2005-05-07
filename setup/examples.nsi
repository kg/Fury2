SetCompressor lzma
SetOverwrite ifnewer
SetDatablockOptimize on
SetDateSave on
XPStyle on

Name "Fury² Examples"
Var ALREADY_INSTALLED
!define NAME "Fury² Examples"
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

  OutFile "..\..\binary\setup\fury2_examples_beta_${VERSION}.exe"

  InstallDir "$PROGRAMFILES\Fury²"

;--------------------------------
;Installer Sections

Section "-Example Games"
	SetOutPath "$INSTDIR"
    StrCpy $ALREADY_INSTALLED 0
    IfFileExists "$INSTDIR\Uninstall_Examples.exe" old_installation new_installation
     StrCpy $ALREADY_INSTALLED 1
    new_installation:
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayName" "${NAME}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "UninstallString" "$INSTDIR\Uninstall_Examples.exe"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayVersion" "${VERSION}"
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoModify" 1
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoRepair" 1
	WriteUninstaller "$INSTDIR\Uninstall_Examples.exe"
    old_installation:

    IfFileExists $INSTDIR\sys\engine.dll EngineFound
        DetailPrint "Engine not installed."
        StrCpy $2 "$EXEDIR\fury2_beta_${VERSION}.exe"
        IfFileExists $2 setupfound
        DetailPrint "Attempting to download engine..."
        Call ConnectInternet
        NSISdl::download http://fury2.luminance.org/downloads/fury2_beta_${VERSION}.exe $2
        Pop $0
        StrCmp $0 success success
            DetailPrint "Engine download failed: $0"
            Abort
        setupfound:
            DetailPrint "Engine installer found."
        success:
            DetailPrint "Installing engine."
            ExecWait '"$2"'
            Delete $2
    EngineFound:

	SetOutPath "$INSTDIR\Examples\"
    File /nonfatal /r "C:\Documents and Settings\Kevin\My Documents\Projects\fury2\docs\Examples\*.*"
SectionEnd

Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\Fury²"
  CreateDirectory "$SMPROGRAMS\Fury²\Examples"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\AVI Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\avi" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Basic Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\basic" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Chat Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\chat" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Explosion Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\explosion" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Fury² Intro.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\intro" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\HTTP Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\http" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Menus Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\menus" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Paint Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\paint" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Rain Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\rain" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Reflect Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\reflect" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Examples\Reveal Example.lnk" "$INSTDIR\sys\fury².exe" "$INSTDIR\examples\reveal" "$INSTDIR\sys\fury².exe"
  CreateShortCut "$SMPROGRAMS\Fury²\Uninstall ${NAME}.lnk" "$INSTDIR\uninstall_examples.exe"
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

  RmDir /r "$SMPROGRAMS\${NAME}"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"

  RmDir /r "$INSTDIR\Examples"
  RmDir /r "$INSTDIR"
SectionEnd
