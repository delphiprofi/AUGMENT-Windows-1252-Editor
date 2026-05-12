@echo off
call "C:\Delphi XE16\BIN\rsvars.bat"
msbuild Tests\Unittests.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
