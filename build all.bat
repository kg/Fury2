@echo off
echo Building Everything...
Call "build libraries.bat"
Call "build engine.bat"
Call "build plugins.bat"
Call "build output plugins.bat"
Call "build editor.bat"
Call "build editor plugins.bat"
Call "build documentation generator.bat"