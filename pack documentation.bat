@echo off
cd DocGen\output
7z a -tzip "Documentation.zip" Documentation\*.* -mx=9 -r -bd