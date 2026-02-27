@echo off
call "C:\Delphi XE16\BIN\rsvars.bat"
msbuild StrEditor.dproj /t:Build /p:Config=AI /p:Platform=Win32

