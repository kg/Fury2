SetCompressor lzma
SetOverwrite ifnewer
SetDatablockOptimize on
SetDateSave on
XPStyle on

Name "Fury² Web Installer"
Var ALREADY_INSTALLED
!define NAME "Fury²"
!define VERSION "0.9"
!define WEBSITE "http://luminance.org/fury2/downloads"

!include Library.nsh
!include "MUI.nsh"

  !define MUI_ABORTWARNING

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_LANGUAGE "English"

  Icon "res\webicon.ico"

  OutFile "..\..\binary\setup\fury2_websetup.exe"

  InstallDir "$PROGRAMFILES\Fury²"

;--------------------------------
;Installer Sections

Section "Engine"
    StrCpy $2 "$EXEDIR\fury2_beta_${VERSION}.exe"
    IfFileExists $2 enginesetupfound
    DetailPrint "Attempting to download engine..."
    Call ConnectInternet
    NSISdl::download ${WEBSITE}/fury2_beta_${VERSION}.exe $2
    Pop $0
    StrCmp $0 success essuccess
        DetailPrint "Engine download failed: $0"
        Abort
    enginesetupfound:
        DetailPrint "Engine installer found."
    essuccess:
        DetailPrint "Installing engine."
        ExecWait '"$2"'
SectionEnd

Section "Editor"
    StrCpy $2 "$EXEDIR\fury2_editor_beta_${VERSION}.exe"
    IfFileExists $2 editorsetupfound
    DetailPrint "Attempting to download editor..."
    Call ConnectInternet
    NSISdl::download ${WEBSITE}/fury2_editor_beta_${VERSION}.exe $2
    Pop $0
    StrCmp $0 success edssuccess
        DetailPrint "Editor download failed: $0"
        Abort
    editorsetupfound:
        DetailPrint "Editor installer found."
    edssuccess:
        DetailPrint "Installing editor."
        ExecWait '"$2"'
SectionEnd

Section "Examples"
    StrCpy $2 "$EXEDIR\fury2_examples_beta_${VERSION}.exe"
    IfFileExists $2 examplessetupfound
    DetailPrint "Attempting to download examples..."
    Call ConnectInternet
    NSISdl::download ${WEBSITE}/fury2_examples_beta_${VERSION}.exe $2
    Pop $0
    StrCmp $0 success exssuccess
        DetailPrint "Examples download failed: $0"
        Abort
    examplessetupfound:
        DetailPrint "Examples installer found."
    exssuccess:
        DetailPrint "Installing examples."
        ExecWait '"$2"'
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
