@echo off
call "C:\Delphi XE16\bin\rsvars.bat"
msbuild "StrEditor.dproj" /p:Configuration=Release /p:Platform=Win32 /t:Build /v:q

