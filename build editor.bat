@echo off
echo Building Editor...
echo libDebugger
%VB6% "libraries\libDebugger\libDebugger.vbp" /make
echo ngCommon
%VB6% "libraries\ngCommon\ngCommon.vbp" /make
echo ngIDE
%VB6% "ngIDE\ngIDE.vbp" /make
