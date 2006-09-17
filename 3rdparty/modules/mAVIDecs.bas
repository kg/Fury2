Attribute VB_Name = "mAVIDecs"
Option Explicit
'/**************************************************************************
' *
' *  mAVIDecs - Declarations for using Win32 AVIFile functions in VB
' *             Created 9/4/99 By Ray Mercer
' *
' *             Version 1.0 uploaded to www.shrinkwrapvb.com on
' *             2/14/2000 by Ray Mercer.  Please refer people to the
' *             latest version on the web rather than redistributing
' *             this file itself.
' *             Latest version at http://www.shrinkwrapvb.com
' *
' *  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
' *  KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
' *  IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
' *  PURPOSE.
' *
' *  Copyright (C) 1999 - 2000 by Ray Mercer.  All Rights Reserved.
' *  Please see the licensing and distribution page at Shrinkwrap Visual Basic
' *  for more information about copyright restrictions.
' *
' **************************************************************************/

' /**************************************************************************
' *
' *  AVIFile* Declares (converted from C function prototypes)
' *
' ***************************************************************************/

Public Declare Function mmioStringToFOURCC Lib "winmm.dll" Alias "mmioStringToFOURCCA" (ByVal sz As String, ByVal uFlags As Long) As Long 'returns fourcc
'note* functions that return MMRESULT return 0 on success
'note* mmioOpen()'s second parameter is declared as Any instead of as MMIOINFO because we sometimes pass a ByVal NULL (0&) which causes VB to choke
' mmioDescend()'s third parameter is As Any instead of As MMCKINFO for the same reason
'Private Declare Function mmioOpen Lib "winmm.dll" Alias "mmioOpenA" (ByVal szFileName As String, ByRef lpmmioinfo As Any, ByVal dwOpenFlags As Long) As Long   'returns hmmio
'Private Declare Function mmioDescend Lib "winmm.dll" (ByVal hmmio As Long, ByRef lpck As MMCKINFO, ByRef lpckParent As Any, ByVal wFlags As Long) As Long   'MMRESULT           );
'Private Declare Function mmioRead Lib "winmm.dll" (ByVal hmmio As Long, ByVal pBuf As Long, ByVal lenBuf As Long) As Long 'returns num bytes read
'Private Declare Function mmioClose Lib "winmm.dll" (ByVal hmmio As Long, ByVal wFlags As Long) As Long   ' MMRESULT
Public Declare Function VideoForWindowsVersion Lib "msvfw32.dll" () As Long
'Public Declare Function AVIFileInfo Lib "avifil32.dll" (ByVal pfile As Long, pfi As AVI_FILE_INFO, ByVal lSize As Long) As Long 'HRESULT
Public Declare Sub AVIFileInit Lib "avifil32.dll" ()
'note! - the ppfile argument is ByRef because it is a pointer to a pointer :-)
Public Declare Function AVIFileOpen Lib "avifil32.dll" (ByRef ppfile As Long, ByVal szFile As String, ByVal uMode As Long, ByVal pclsidHandler As Long) As Long  'HRESULT
Public Declare Function AVIFileInfo Lib "avifil32.dll" (ByVal pfile As Long, pfi As AVI_FILE_INFO, ByVal lSize As Long) As Long 'HRESULT
Public Declare Function AVIFileCreateStream Lib "avifil32.dll" Alias "AVIFileCreateStreamA" _
                                        (ByVal pfile As Long, ByRef ppavi As Long, ByRef psi As AVI_STREAM_INFO) As Long

'Careful! this function is awkward to use in VB
' the only way to make it work is to pass a pointer to a AVI_COMPRESS_OPTIONS UDT (last parameter) ByRef
' This means in your code you should Dim a long variable and get a pointer to your AVI_COMPRESS_OPTIONS UDT
' using VarPtr() - e.g. mylong = VarPtr(myUDT) - and then pass the mylong BYREF
' this will give you a pointer to a pointer to an (array of) UDT (yech!) -Ray
Public Declare Function AVISaveOptions Lib "avifil32.dll" (ByVal hWnd As Long, _
                                                        ByVal uiFlags As Long, _
                                                        ByVal nStreams As Long, _
                                                        ByRef ppavi As Long, _
                                                        ByRef ppOptions As Long) As Long 'TRUE if user pressed OK, False if cancel, or error if error
'This is actually the AVISaveV function aliased to be called as AVISave from VB because
'AVISave seems to be compiled using CDECL calling convention ;-(
'ALSO - see note above AVISaveOptions declare - this function also requires a pointer to a pointer to (an array of) UDT
Public Declare Function AVISave Lib "avifil32.dll" Alias "AVISaveVA" (ByVal szFile As String, _
                                                                        ByVal pclsidHandler As Long, _
                                                                        ByVal lpfnCallback As Long, _
                                                                        ByVal nStreams As Long, _
                                                                        ByRef ppaviStream As Long, _
                                                                        ByRef ppCompOptions As Long) As Long
'See note above AVISaveOptions declare - this function also requires a pointer to a pointer to (an array of) UDT
Public Declare Function AVISaveOptionsFree Lib "avifil32.dll" (ByVal nStreams As Long, _
                                                        ByRef ppOptions As Long) As Long

Public Declare Function AVIMakeCompressedStream Lib "avifil32.dll" (ByRef ppsCompressed As Long, _
                                                                ByVal psSource As Long, _
                                                                ByRef lpOptions As AVI_COMPRESS_OPTIONS, _
                                                                ByVal pclsidHandler As Long) As Long '

Public Declare Function AVIStreamSetFormat Lib "avifil32.dll" (ByVal pavi As Long, _
                                                                ByVal lPos As Long, _
                                                                ByRef lpFormat As Any, _
                                                                ByVal cbFormat As Long) As Long

Public Declare Function AVIStreamWrite Lib "avifil32.dll" (ByVal pavi As Long, _
                                                        ByVal lStart As Long, _
                                                        ByVal lSamples As Long, _
                                                        ByVal lpBuffer As Long, _
                                                        ByVal cbBuffer As Long, _
                                                        ByVal dwFlags As Long, _
                                                        ByRef plSampWritten As Long, _
                                                        ByRef plBytesWritten As Long) As Long
Public Declare Function AVIStreamReadFormat Lib "avifil32.dll" (ByVal pAVIStream As Long, _
                                                                ByVal lPos As Long, _
                                                                ByVal lpFormatBuf As Long, _
                                                                ByRef sizeBuf As Long) As Long

Public Declare Function AVIStreamRead Lib "avifil32.dll" (ByVal pAVIStream As Long, _
                                                            ByVal lStart As Long, _
                                                            ByVal lSamples As Long, _
                                                            ByVal lpBuffer As Long, _
                                                            ByVal cbBuffer As Long, _
                                                            ByRef pBytesWritten As Long, _
                                                            ByRef pSamplesWritten As Long) As Long
Public Declare Function AVIStreamGetFrameOpen Lib "avifil32.dll" (ByVal pAVIStream As Long, _
                                                                ByRef bih As Any) As Long 'returns pointer to GETFRAME object on success (or NULL on error)
Public Declare Function AVIStreamGetFrame Lib "avifil32.dll" (ByVal pGetFrameObj As Long, _
                                                                ByVal lPos As Long) As Long 'returns pointer to packed DIB on success (or NULL on error)
Public Declare Function AVIStreamGetFrameClose Lib "avifil32.dll" (ByVal pGetFrameObj As Long) As Long ' returns zero on success (error number) after calling this function the GETFRAME object pointer is invalid

                                                            
Public Declare Function AVIFileGetStream Lib "avifil32.dll" (ByVal pfile As Long, ByRef ppaviStream As Long, ByVal fccType As Long, ByVal lParam As Long) As Long
Public Declare Function AVIMakeFileFromStreams Lib "avifil32.dll" (ByRef ppfile As Long, ByVal nStreams As Long, ByVal pAVIStreamArray As Long) As Long

Public Declare Function AVIStreamInfo Lib "avifil32.dll" (ByVal pAVIStream As Long, ByRef psi As AVI_STREAM_INFO, ByVal lSize As Long) As Long
Public Declare Function AVIStreamStart Lib "avifil32.dll" (ByVal pavi As Long) As Long
Public Declare Function AVIStreamLength Lib "avifil32.dll" (ByVal pavi As Long) As Long
Public Declare Function AVIStreamRelease Lib "avifil32.dll" (ByVal pavi As Long) As Long 'ULONG
Public Declare Function AVIStreamClose Lib "avifil32.dll" Alias "AVIStreamRelease" (ByVal pavi As Long) As Long 'ULONG
Public Declare Function AVIFileRelease Lib "avifil32.dll" (ByVal pfile As Long) As Long
Public Declare Function AVIFileClose Lib "avifil32.dll" Alias "AVIFileRelease" (ByVal pfile As Long) As Long
Public Declare Sub AVIFileExit Lib "avifil32.dll" ()

'/****************************************************************************
' *
' *  Clipboard routines
' *
' ***************************************************************************/
Public Declare Function AVIMakeStreamFromClipboard Lib "avifil32.dll" (ByVal cfFormat As Long, ByVal hGlobal As Long, ByRef ppstream As Long) As Long
Public Declare Function AVIPutFileOnClipboard Lib "avifil32.dll" (ByVal pAVIFile As Long) As Long
Public Declare Function AVIGetFromClipboard Lib "avifil32.dll" (ByRef ppAVIFile As Long) As Long
Public Declare Function AVIClearClipboard Lib "avifil32.dll" () As Long

'Public Declare Function GetObjectAPI Lib "gdi32.dll" Alias "GetObjectA" (ByVal hGDIObj As Long, ByVal sizBuf As Long, ByRef lpBuf As Any) As Long
'Public Declare Function CreateCompatibleDC Lib "gdi32.dll" (ByVal hdc As Long) As Long
'Public Declare Function DeleteDC Lib "gdi32.dll" (ByVal hdc As Long) As Long 'BOOL
'Public Declare Function GetDIBits Lib "gdi32.dll" (ByVal hdc As Long, ByVal hBmp As Long, _
'                                                    ByVal startScan As Long, ByVal numScanLines As Long, _
'                                                    ByVal lpvBits As Long, ByRef BI As BITMAPINFOHEADER, ByVal uUsage As Long) As Long

'Public Declare Function LoadBitmapByNumber Lib "user32.dll" Alias "LoadBitmapA" (ByVal hInstance As Long, ByVal lpResID As Long) As Long 'hBitmap
'Public Declare Function DeleteObject Lib "gdi32.dll" (ByVal hObject As Long) As Long 'BOOL


' /**************************************************************************
' *
' *  AVIFile* Types (UDTS converted from C structs)
' *
' ***************************************************************************/
Private Const BMP_MAGIC_COOKIE As Integer = 19778 'this is equivalent to ascii string "BM"
Public Type BITMAPFILEHEADER '14 bytes
        bfType As Integer '"magic cookie" - must be "BM" (19778)
        bfSize As Long
        bfReserved1 As Integer
        bfReserved2 As Integer
        bfOffBits As Long
End Type
'//BITMAP DEFINES (from mmsystem.h)
Public Type BitmapInfoHeader '40 bytes
   biSize As Long
   biWidth As Long
   biHeight As Long
   biPlanes As Integer
   biBitCount As Integer
   biCompression As Long
   biSizeImage As Long
   biXPelsPerMeter As Long
   biYPelsPerMeter As Long
   biClrUsed As Long
   biClrImportant As Long
End Type

Public Type BITMAPINFOHEADER_MJPEG '68 bytes
   biSize As Long
   biWidth As Long
   biHeight As Long
   biPlanes As Integer
   biBitCount As Integer
   biCompression As Long
   biSizeImage As Long
   biXPelsPerMeter As Long
   biYPelsPerMeter As Long
   biClrUsed As Long
   biClrImportant As Long
   '/* extended BITMAPINFOHEADER fields */
   biExtDataOffset As Long
   '/* compression-specific fields */
   '/* these fields are defined for 'JPEG' and 'MJPG' */
   JPEGSize As Long
   JPEGProcess As Long
   '/* Process specific fields */
   JPEGColorSpaceID As Long
   JPEGBitsPerSample As Long
   JPEGHSubSampling As Long
   JPEGVSubSampling As Long
End Type
        
Public Type AVI_RECT
    left As Long
    top As Long
    right As Long
    bottom As Long
End Type

Public Type AVI_STREAM_INFO
    fccType As Long
    fccHandler As Long
    dwFlags As Long
    dwCaps As Long
    wPriority As Integer
    wLanguage As Integer
    dwScale As Long
    dwRate As Long
    dwStart As Long
    dwLength As Long
    dwInitialFrames As Long
    dwSuggestedBufferSize As Long
    dwQuality As Long
    dwSampleSize As Long
    rcFrame As AVI_RECT
    dwEditCount  As Long
    dwFormatChangeCount As Long
    szName As String * 64
End Type

'for use with AVIFIleInfo
Public Type AVI_FILE_INFO  '108 bytes?
    dwMaxBytesPerSecond As Long
    dwFlags As Long
    dwCaps As Long
    dwStreams As Long
    dwSuggestedBufferSize As Long
    dwWidth As Long
    dwHeight As Long
    dwScale As Long
    dwRate As Long
    dwLength As Long
    dwEditCount As Long
    szFileType As String * 64
End Type

Public Type AVI_COMPRESS_OPTIONS
    fccType As Long            '/* stream type, for consistency */
    fccHandler As Long         '/* compressor */
    dwKeyFrameEvery As Long    '/* keyframe rate */
    dwQuality As Long          '/* compress quality 0-10,000 */
    dwBytesPerSecond As Long   '/* bytes per second */
    dwFlags As Long            '/* flags... see below */
    lpFormat As Long           '/* save format */
    cbFormat As Long
    lpParms As Long            '/* compressor options */
    cbParms As Long
    dwInterleaveEvery As Long  '/* for non-video streams only */
End Type

' /**************************************************************************
' *
' *  AVIFile* Constants (converted from C defines)
' *
' ***************************************************************************/
Global Const AVIERR_OK As Long = 0&

'AVIERR MACRO BREAKDOWN (You gotta love those Windows error codes ;-()
'FROM WINERROR.H
'#define SEVERITY_ERROR   1
'#define FACILITY_ITF     4
'MAKE_SCODE(SEVERITY_ERROR, FACILITY_ITF, 0x4000 + error)
'#define MAKE_AVIERR(error)  MAKE_SCODE(sev,fac,code) \ ((SCODE) (((unsigned long)(sev)<<31) | ((unsigned long)(fac)<<16) | ((unsigned long)(code))) )
'Since we can't use the error macro from VB, we need these defined a little more concretely:
'(IOW - Man! those C -Programmers are lazy! ;-)
Private Const SEVERITY_ERROR    As Long = &H80000000
Private Const FACILITY_ITF      As Long = &H40000
Private Const AVIERR_BASE       As Long = &H4000

'#define AVIERR_UNSUPPORTED      MAKE_AVIERR(101)
'#define AVIERR_BADFORMAT        MAKE_AVIERR(102)
'#define AVIERR_MEMORY           MAKE_AVIERR(103)
'#define AVIERR_INTERNAL         MAKE_AVIERR(104)
Global Const AVIERR_BADFLAGS    As Long = SEVERITY_ERROR Or FACILITY_ITF Or (AVIERR_BASE + 105) '-2147205015
Global Const AVIERR_BADPARAM    As Long = SEVERITY_ERROR Or FACILITY_ITF Or (AVIERR_BASE + 106) '-2147205014
Global Const AVIERR_BADSIZE     As Long = SEVERITY_ERROR Or FACILITY_ITF Or (AVIERR_BASE + 107) '-2147205013
'#define AVIERR_BADHANDLE        MAKE_AVIERR(108)
'#define AVIERR_FILEREAD         MAKE_AVIERR(109)
'#define AVIERR_FILEWRITE        MAKE_AVIERR(110)
'#define AVIERR_FILEOPEN         MAKE_AVIERR(111)
'#define AVIERR_COMPRESSOR       MAKE_AVIERR(112)
'#define AVIERR_NOCOMPRESSOR     MAKE_AVIERR(113)
'#define AVIERR_READONLY     MAKE_AVIERR(114)
'#define AVIERR_NODATA       MAKE_AVIERR(115)
'#define AVIERR_BUFFERTOOSMALL   MAKE_AVIERR(116)
'#define AVIERR_CANTCOMPRESS MAKE_AVIERR(117)
Global Const AVIERR_USERABORT   As Long = SEVERITY_ERROR Or FACILITY_ITF Or (AVIERR_BASE + 198) '-2147204922
'#define AVIERR_ERROR            MAKE_AVIERR(199)

'// Flags for dwFlags
Global Const AVIFILEINFO_HASINDEX         As Long = &H10
Global Const AVIFILEINFO_MUSTUSEINDEX     As Long = &H20
Global Const AVIFILEINFO_ISINTERLEAVED    As Long = &H100
Global Const AVIFILEINFO_WASCAPTUREFILE   As Long = &H10000
Global Const AVIFILEINFO_COPYRIGHTED      As Long = &H20000

'// Flags for dwCaps
Global Const AVIFILECAPS_CANREAD          As Long = &H1
Global Const AVIFILECAPS_CANWRITE         As Long = &H2
Global Const AVIFILECAPS_ALLKEYFRAMES     As Long = &H10
Global Const AVIFILECAPS_NOCOMPRESSION    As Long = &H20

'//
'// Defines for the dwFlags field of the AVICOMPRESSOPTIONS struct
'// Each of these flags determines if the appropriate field in the structure
'// (dwInterleaveEvery, dwBytesPerSecond, and dwKeyFrameEvery) is payed
'// attention to.  See the autodoc in avisave.c for details.
'//
Global Const AVICOMPRESSF_INTERLEAVE     As Long = &H1           '// interleave
Global Const AVICOMPRESSF_DATARATE       As Long = &H2           '// use a data rate
Global Const AVICOMPRESSF_KEYFRAMES      As Long = &H4           '// use keyframes
Global Const AVICOMPRESSF_VALID          As Long = &H8           '// has valid data?

Global Const OF_READ  As Long = &H0
Global Const OF_WRITE As Long = &H1
'#define OF_READWRITE        0x00000002
'#define OF_SHARE_COMPAT     0x00000000
'#define OF_SHARE_EXCLUSIVE  0x00000010
Global Const OF_SHARE_DENY_WRITE As Long = &H20
'#define OF_SHARE_DENY_READ  0x00000030
'#define OF_SHARE_DENY_NONE  0x00000040
'#define OF_PARSE            0x00000100
'#define OF_DELETE           0x00000200
'#define OF_VERIFY           0x00000400
'#define OF_CANCEL           0x00000800
Global Const OF_CREATE As Long = &H1000
'#define OF_PROMPT           0x00002000
'#define OF_EXIST            0x00004000
'#define OF_REOPEN           0x00008000

Global Const AVIIF_KEYFRAME  As Long = &H10

'/* DIB color table identifiers */
Global Const DIB_RGB_COLORS  As Long = 0    '/* color table in RGBs */
Global Const DIB_PAL_COLORS  As Long = 1    '/* color table in palette indices */

'/* constants for the biCompression field */
Global Const BI_RGB          As Long = 0
Global Const BI_RLE8         As Long = 1
Global Const BI_RLE4         As Long = 2
Global Const BI_BITFIELDS    As Long = 3

'Stream types for use in VB (translated from C macros)
Global Const streamtypeVIDEO       As Long = 1935960438 'equivalent to: mmioStringToFOURCC("vids", 0&)
Global Const streamtypeAUDIO       As Long = 1935963489 'equivalent to: mmioStringToFOURCC("auds", 0&)
Global Const streamtypeMIDI        As Long = 1935960429 'equivalent to: mmioStringToFOURCC("mids", 0&)
Global Const streamtypeTEXT        As Long = 1937012852 'equivalent to: mmioStringToFOURCC("txts", 0&)

'// For GetFrame::SetFormat - use the best format for the display
Global Const AVIGETFRAMEF_BESTDISPLAYFMT  As Long = 1

'// defines for uiFlags (AVISaveOptions)
Global Const ICMF_CHOOSE_KEYFRAME           As Long = &H1     '// show KeyFrame Every box
Global Const ICMF_CHOOSE_DATARATE           As Long = &H2     '// show DataRate box
Global Const ICMF_CHOOSE_PREVIEW            As Long = &H4     '// allow expanded preview dialog
Global Const ICMF_CHOOSE_ALLCOMPRESSORS     As Long = &H8     '// don't only show those that
                                                              '// can handle the input format
                                                              '// or input data

' /**************************************************************************
' *
' *  'STANDARD WIN32 API DECLARES, UDTs and Constants
' *
' ***************************************************************************/
Private Declare Function SetRect Lib "user32.dll" _
    (ByRef lprc As AVI_RECT, ByVal xLeft As Long, ByVal yTop As Long, ByVal xRight As Long, ByVal yBottom As Long) As Long 'BOOL
Private Declare Function GetProcessHeap Lib "kernel32.dll" () As Long 'handle
Private Declare Function HeapAlloc Lib "kernel32.dll" (ByVal hHeap As Long, ByVal dwFlags As Long, ByVal dwBytes As Long) As Long 'Pointer to mem
Private Declare Function HeapFree Lib "kernel32.dll" (ByVal hHeap As Long, ByVal dwFlags As Long, ByVal lpMem As Long) As Long 'BOOL
Private Declare Sub CopyMemory Lib "kernel32.dll" Alias "RtlMoveMemory" (ByRef dest As Any, ByRef src As Any, ByVal dwLen As Long)

Private Const HEAP_ZERO_MEMORY As Long = &H8

Global gfAbort As Boolean 'allows user to abort an AVI Save operation (see Callback function below)

Public Function AVISaveCallback(ByVal nPercent As Long) As Long 'should return C BOOL
'AVISave callback function prototype:
'typedef BOOL (FAR PASCAL * AVISAVECALLBACK)(int);
'
'
'//Display user feedback here using nPercent
'
'
'DoEvents 'allows user to cancel
'If gfAbort = True Then
'    AVISaveCallback = AVIERR_USERABORT 'abort file write
'Else
'    AVISaveCallback = AVIERR_OK 'continue saving file
'End If

End Function

' /**************************************************************************
' *
' *  'UTILITY FUNCTIONS FOR WORKING WITH AVI FILES
' *
' ***************************************************************************/
Public Sub DebugPrintAVIStreamInfo(asi As AVI_STREAM_INFO)
Debug.Print ""
Debug.Print "**** AVI_STREAM_INFO (START) ****"
 With asi
    Debug.Print "fccType = " & .fccType
    Debug.Print "fccHandler = " & .fccHandler
    Debug.Print "dwFlags = " & .dwFlags
    Debug.Print "dwCaps = " & .dwCaps
    Debug.Print "wPriority = " & .wPriority
    Debug.Print "wLanguage = " & .wLanguage
    Debug.Print "dwScale = " & .dwScale
    Debug.Print "dwRate = " & .dwRate
    Debug.Print "dwStart = " & .dwStart
    Debug.Print "dwLength = " & .dwLength
    Debug.Print "dwInitialFrames = " & .dwInitialFrames
    Debug.Print "dwSuggestedBufferSize = " & .dwSuggestedBufferSize
    Debug.Print "dwQuality = " & .dwQuality
    Debug.Print "dwSampleSize = " & .dwSampleSize
    Debug.Print "rcFrame.left = " & .rcFrame.left
    Debug.Print "rcFrame.top = " & .rcFrame.top
    Debug.Print "rcFrame.right = " & .rcFrame.right
    Debug.Print "rcFrame.bottom = " & .rcFrame.bottom
    Debug.Print "dwEditCount = " & .dwEditCount
    Debug.Print "dwFormatChangeCount = " & .dwFormatChangeCount
    Debug.Print "szName = " & .szName
 End With
 Debug.Print "**** AVI_STREAM_INFO (END) ****"
 Debug.Print ""
End Sub

Public Sub DebugPrintAVIFileInfo(afi As AVI_FILE_INFO)
Debug.Print "**** AVI_FILE_INFO (START) ****"
 With afi
    Debug.Print "dwMaxBytesPerSecond = " & .dwMaxBytesPerSecond
    Debug.Print "dwFlags = " & .dwFlags
    Debug.Print "dwCaps = " & .dwCaps
    Debug.Print "dwStreams = " & .dwStreams
    Debug.Print "dwSuggestedBufferSize = " & .dwSuggestedBufferSize
    Debug.Print "dwWidth = " & .dwWidth
    Debug.Print "dwHeight = " & .dwHeight
    Debug.Print "dwScale = " & .dwScale
    Debug.Print "dwRate = " & .dwRate
    Debug.Print "dwLength = " & .dwLength
    Debug.Print "dwEditCount = " & .dwEditCount
    Debug.Print "szFileType = " & .szFileType
 End With
 Debug.Print "**** AVI_FILE_INFO (END) ****"
 Debug.Print ""
End Sub
