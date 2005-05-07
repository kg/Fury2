Attribute VB_Name = "mdlGLFX"
Option Explicit
Public Declare Function GLInit Lib "GLFX" Alias "_GLInit@8" (ByVal HWND As Long, ByVal HDC As Long) As Long
Public Declare Function GLShutdown Lib "GLFX" Alias "_GLShutdown@0" () As Long
Public Declare Function GLSetOutputSize Lib "GLFX" Alias "_GLSetOutputSize@8" (ByVal Width As Long, ByVal Height As Long) As Long
Public Declare Function GLSetScaleMode Lib "GLFX" Alias "_GLSetScaleMode@4" (ByVal ScaleMode As Long) As Long
Public Declare Function GLFlip Lib "GLFX" Alias "_GLFlip@0" () As Long
Public Declare Function GLCopySurface Lib "GLFX" Alias "_GLCopySurface@8" (ByVal FromImage As Long, ByVal ToImage As Long) As Long
Public Declare Function GLInstallAllocateHook Lib "GLFX" Alias "_GLInstallAllocateHook@0" () As Long
Public Declare Function GLUninstallAllocateHook Lib "GLFX" Alias "_GLUninstallAllocateHook@0" () As Long

