@echo off
echo Building Editor Plugins...
echo ngPlugins
%VB6% "plugins\ngPlugins\ngPlugins.vbp" /make
echo tk
%VB6% "plugins\tk\tk.vbp" /make