@echo off
echo Building Libraries...
echo libGraphics
%VB6% "libraries\libGraphics\libGraphics.vbp" /make
echo libSound
%VB6% "libraries\libSound\libSound.vbp" /make
echo libFilesystem
%VB6% "libraries\libFilesystem\libFilesystem.vbp" /make
echo libScript2
%VB6% "libraries\libScript2\libScript2.vbp" /make