@echo off
echo Building Output Plugins...
echo Video_GDI
%VB6% "plugins\video\gdi\GDI.vbp" /make
echo Video_DirectDraw
%VB6% "plugins\video\directdraw\DirectDraw.vbp" /make
