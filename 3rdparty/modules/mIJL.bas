Attribute VB_Name = "mIntelJPEGLibrary"
Option Explicit
' ==================================================================================
' Filename:    mIntelJPEGLibrary.bas
' Author:      Steve McMahon
' Date:        April 07, 2003
' Requires: IJL11.DLL (Intel)
'
' An interface to Intel's IJL (Intel JPG Library) for use in VB.
'
' Copyright © 1999 Steve McMahon for vbAccelerator
' http://vbaccelerator.com/
'
' Modifications
' 07 Apr 2003  KTG
' * Changed for use in graphics library. Saving function moved to Fury2Image.SaveJPG,
'    everything else is gone except type declarations
' 16 Jan 2000  SPM
' * Modified to declares to access v1.1. of Intel's IJL DLL
' * SaveJPG - if you were overwriting an existing JPG, the file could never be
'   reduced in size, only increased.  Old bytes were simply left trailing as
'   an unnecessary payload at the JPG.  The JPG could be loaded, but this was
'   not ideal.  The new version ensures the file size is always set.
' * LoadJPGFromPtr - new function, allows you to read a JPG from a memory
'   address (e.g. resource etc)
'
' Copyright.
' IJL.DLL is a copyright © Intel, which is a registered trade mark of the Intel
' Corporation.
'
' ==================================================================================

' IJL Declares:
Public Enum IJLERR
  '// The following "error" values indicate an "OK" condition.
  IJL_OK = 0
  IJL_INTERRUPT_OK = 1
  IJL_ROI_OK = 2

  '// The following "error" values indicate an error has occurred.
  IJL_EXCEPTION_DETECTED = -1
  IJL_INVALID_ENCODER = -2
  IJL_UNSUPPORTED_SUBSAMPLING = -3
  IJL_UNSUPPORTED_BYTES_PER_PIXEL = -4
  IJL_MEMORY_ERROR = -5
  IJL_BAD_HUFFMAN_TABLE = -6
  IJL_BAD_QUANT_TABLE = -7
  IJL_INVALID_JPEG_PROPERTIES = -8
  IJL_ERR_FILECLOSE = -9
  IJL_INVALID_FILENAME = -10
  IJL_ERROR_EOF = -11
  IJL_PROG_NOT_SUPPORTED = -12
  IJL_ERR_NOT_JPEG = -13
  IJL_ERR_COMP = -14
  IJL_ERR_SOF = -15
  IJL_ERR_DNL = -16
  IJL_ERR_NO_HUF = -17
  IJL_ERR_NO_QUAN = -18
  IJL_ERR_NO_FRAME = -19
  IJL_ERR_MULT_FRAME = -20
  IJL_ERR_DATA = -21
  IJL_ERR_NO_IMAGE = -22
  IJL_FILE_ERROR = -23
  IJL_INTERNAL_ERROR = -24
  IJL_BAD_RST_MARKER = -25
  IJL_THUMBNAIL_DIB_TOO_SMALL = -26
  IJL_THUMBNAIL_DIB_WRONG_COLOR = -27
  IJL_RESERVED = -99

End Enum

Public Enum IJLIOTYPE
  IJL_SETUP = -1&
  ''// Read JPEG parameters (i.e., height, width, channels,
  ''// sampling, etc.) from a JPEG bit stream.
  IJL_JFILE_READPARAMS = 0&
  IJL_JBUFF_READPARAMS = 1&
  ''// Read a JPEG Interchange Format image.
  IJL_JFILE_READWHOLEIMAGE = 2&
  IJL_JBUFF_READWHOLEIMAGE = 3&
  ''// Read JPEG tables from a JPEG Abbreviated Format bit stream.
  IJL_JFILE_READHEADER = 4&
  IJL_JBUFF_READHEADER = 5&
  ''// Read image info from a JPEG Abbreviated Format bit stream.
  IJL_JFILE_READENTROPY = 6&
  IJL_JBUFF_READENTROPY = 7&
  ''// Write an entire JFIF bit stream.
  IJL_JFILE_WRITEWHOLEIMAGE = 8&
  IJL_JBUFF_WRITEWHOLEIMAGE = 9&
  ''// Write a JPEG Abbreviated Format bit stream.
  IJL_JFILE_WRITEHEADER = 10&
  IJL_JBUFF_WRITEHEADER = 11&
  ''// Write image info to a JPEG Abbreviated Format bit stream.
  IJL_JFILE_WRITEENTROPY = 12&
  IJL_JBUFF_WRITEENTROPY = 13&
  ''// Scaled Decoding Options:
  ''// Reads a JPEG image scaled to 1/2 size.
  IJL_JFILE_READONEHALF = 14&
  IJL_JBUFF_READONEHALF = 15&
  ''// Reads a JPEG image scaled to 1/4 size.
  IJL_JFILE_READONEQUARTER = 16&
  IJL_JBUFF_READONEQUARTER = 17&
  ''// Reads a JPEG image scaled to 1/8 size.
  IJL_JFILE_READONEEIGHTH = 18&
  IJL_JBUFF_READONEEIGHTH = 19&
  ''// Reads an embedded thumbnail from a JFIF bit stream.
  IJL_JFILE_READTHUMBNAIL = 20&
  IJL_JBUFF_READTHUMBNAIL = 21&

End Enum

Public Type JPEG_CORE_PROPERTIES_VB ' Sadly, due to a limitation in VB (UDT variable count)
                                     ' we can't encode the full JPEG_CORE_PROPERTIES structure
  UseJPEGPROPERTIES As Long                      '// default = 0

  '// DIB specific I/O data specifiers.
  DIBBytes As Long ';                  '// default = NULL 4
  DIBWidth As Long ';                  '// default = 0 8
  DIBHeight As Long ';                 '// default = 0 12
  DIBPadBytes As Long ';               '// default = 0 16
  DIBChannels As Long ';               '// default = 3 20
  DIBColor As Long ';                  '// default = IJL_BGR 24
  DIBSubsampling As Long  ';            '// default = IJL_NONE 28

  '// JPEG specific I/O data specifiers.
  JPGFile As Long 'LPTSTR              JPGFile;                32   '// default = NULL
  JPGBytes As Long ';                  '// default = NULL 36
  JPGSizeBytes As Long ';              '// default = 0 40
  JPGWidth As Long ';                  '// default = 0 44
  JPGHeight As Long ';                 '// default = 0 48
  JPGChannels As Long ';               '// default = 3
  JPGColor As Long           ';                  '// default = IJL_YCBCR
  JPGSubsampling As Long  ';            '// default = IJL_411
  JPGThumbWidth As Long ' ;             '// default = 0
  JPGThumbHeight As Long ';            '// default = 0

  '// JPEG conversion properties.
  cconversion_reqd As Long ';          '// default = TRUE
  upsampling_reqd As Long ';           '// default = TRUE
  jquality As Long ';                  '// default = 75.  100 is my preferred quality setting.

  '// Low-level properties - 20,000 bytes.  If the whole structure
  ' is written out then VB fails with an obscure error message
  ' "Too Many Local Variables" !
  '
  ' These all default if they are not otherwise specified so there
  ' is no trouble to just assign a sufficient buffer in memory:
  jprops(0 To 19999) As Byte

End Type


Public Declare Function ijlInit Lib "ijl11.dll" (jcprops As Any) As Long
Public Declare Function ijlFree Lib "ijl11.dll" (jcprops As Any) As Long
Public Declare Function ijlRead Lib "ijl11.dll" (jcprops As Any, ByVal ioType As Long) As Long
Public Declare Function ijlWrite Lib "ijl11.dll" (jcprops As Any, ByVal ioType As Long) As Long
Public Declare Function ijlGetLibVersion Lib "ijl11.dll" () As Long
Public Declare Function ijlGetErrorString Lib "ijl11.dll" (ByVal code As Long) As Long

' Win32 Declares
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, lpvSource As Any, ByVal cbCopy As Long)
Private Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Private Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long
Private Const GMEM_DDESHARE = &H2000
Private Const GMEM_DISCARDABLE = &H100
Private Const GMEM_DISCARDED = &H4000
Private Const GMEM_FIXED = &H0
Private Const GMEM_INVALID_HANDLE = &H8000
Private Const GMEM_LOCKCOUNT = &HFF
Private Const GMEM_MODIFY = &H80
Private Const GMEM_MOVEABLE = &H2
Private Const GMEM_NOCOMPACT = &H10
Private Const GMEM_NODISCARD = &H20
Private Const GMEM_NOT_BANKED = &H1000
Private Const GMEM_NOTIFY = &H4000
Private Const GMEM_SHARE = &H2000
Private Const GMEM_VALID_FLAGS = &H7F72
Private Const GMEM_ZEROINIT = &H40
Private Const GPTR = (GMEM_FIXED Or GMEM_ZEROINIT)

' Stuff for replacing a file when you have to Kill the original:
Private Const MAX_PATH = 260
Private Type FILETIME
   dwLowDateTime As Long
   dwHighDateTime As Long
End Type
Private Type WIN32_FIND_DATA
   dwFileAttributes As Long
   ftCreationTime As FILETIME
   ftLastAccessTime As FILETIME
   ftLastWriteTime As FILETIME
   nFileSizeHigh As Long
   nFileSizeLow As Long
   dwReserved0 As Long
   dwReserved1 As Long
   cFileName As String * MAX_PATH
   cAlternate As String * 14
End Type
Private Declare Function lopen Lib "kernel32" Alias "_lopen" (ByVal lpPathName As String, ByVal iReadWrite As Long) As Long
Private Declare Function lclose Lib "kernel32" Alias "_lclose" (ByVal hFile As Long) As Long
Private Const OF_WRITE = &H1
Private Const OF_SHARE_DENY_WRITE = &H20
Private Const GENERIC_WRITE = &H40000000
Private Const GENERIC_READ = &H80000000
Private Const FILE_SHARE_WRITE = &H2
Private Const CREATE_ALWAYS = 2
Private Const FILE_BEGIN = 0
Private Const SECTION_MAP_WRITE = &H2
