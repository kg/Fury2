SetCompressor zlib
SetCompress force
SetOverwrite ifnewer
SetDatablockOptimize on
SetDateSave on

!define MUI_PRODUCT "Fury² Game Creation System" ;Define your own software name here
!define MUI_VERSION "Alpha 0.01" ;Define your own software version here

!include "${NSISDIR}\Contrib\Modern UI\System.nsh"

;--------------------------------
;Configuration

  !define MUI_WELCOMEPAGE

  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  
  !define MUI_ABORTWARNING
  
  !define MUI_UNINSTALLER
  !define MUI_UNCONFIRMPAGE

  !define MUI_BGCOLOR 0xFFFFFF
  !define MUI_HEADERBITMAP "res\header.bmp"
  !define MUI_SPECIALBITMAP "res\sidebar.bmp"

  ;Language
  !insertmacro MUI_LANGUAGE "English"
  
  Icon "res\install.ico"
  UninstallIcon "res\uninstall.ico"

  ;General
  OutFile "..\..\binary\setup\alpha00_full.exe"

  ;License page
  LicenseData "..\..\documentation\licenses\lgpl.txt"

  ;Descriptions
  LangString DESC_SecMain ${LANG_ENGLISH} "Install the Fury² GCS."

  ;Folder-selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"

;--------------------------------
;Installer Sections

Section "-Fury² Game Creation System"
	SetOutPath "$INSTDIR"
	WriteRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/${MUI_PRODUCT}" "DisplayName" "${MUI_PRODUCT}"
	WriteRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/${MUI_PRODUCT}" "UninstallString" "$INSTDIR\Uninstall.exe"
	WriteRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/${MUI_PRODUCT}" "DisplayVersion" "${MUI_VERSION}"
	WriteRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/${MUI_PRODUCT}" "NoModify" 1
	WriteRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/${MUI_PRODUCT}" "NoRepair" 1
	WriteUninstaller "$INSTDIR\Uninstall.exe"

	SetOutPath "$INSTDIR\binary\sys"
	File "..\..\binary\sys\fury².exe"
	File "..\..\binary\sys\engine.dll"
	File "..\..\binary\sys\graphics.dll"
	File "..\..\binary\sys\sound.dll"
	File "..\..\binary\sys\packages.dll"
	File "..\..\binary\sys\filesystem.dll"
	File "..\..\binary\sys\script.dll"
	File "..\..\binary\sys\softfx.dll"
	File "..\..\binary\sys\fmod.dll"
	File "..\..\binary\sys\video_gdi.dll"
	File "..\..\binary\sys\editor.exe"
	File "..\..\binary\sys\cmcs21.ocx"

	RegDLL "$INSTDIR\binary\sys\graphics.dll"
	RegDLL "$INSTDIR\binary\sys\sound.dll"
	RegDLL "$INSTDIR\binary\sys\packages.dll"
	RegDLL "$INSTDIR\binary\sys\script.dll"
	RegDLL "$INSTDIR\binary\sys\filesystem.dll"
	RegDLL "$INSTDIR\binary\sys\video_gdi.dll"
	RegDLL "$INSTDIR\binary\sys\engine.dll"
	RegDLL "$INSTDIR\binary\sys\cmcs21.ocx"

	SetOutPath "$INSTDIR\data\res\"
	File "..\..\data\res\*.*"
	SetOutPath "$INSTDIR\data\Game Templates\Default\"
	File "..\..\data\Game Templates\Default\*.*"
	
SectionEnd

Section "Documentation"
SectionEnd

Section "Source Code"
SectionEnd

Section "Examples"
	SetOutPath "$INSTDIR\games\test (vbscript)"
	File "J:\test\*.*"
	SetOutPath "$INSTDIR\games\test (javascript)"
	File "J:\testjs\*.*"
SectionEnd

Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\${MUI_PRODUCT}"
  CreateShortCut "$SMPROGRAMS\${MUI_PRODUCT}\${MUI_PRODUCT}.lnk" "$INSTDIR\binary\sys\editor.exe" "" "$INSTDIR\binary\sys\editor.exe" 0
SectionEnd

;Display the Finish header
;Insert this macro after the sections if you are not using a finish page
!insertmacro MUI_SECTIONS_FINISHHEADER

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecMain} $(DESC_SecMain)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  DeleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/${MUI_PRODUCT}"

  ;Display the Finish header
  !insertmacro MUI_UNFINISHHEADER

SectionEnd