@echo off
REM ============================================
REM GitHub Release erstellen mit gh CLI
REM ============================================
REM Voraussetzung: GitHub CLI installieren
REM   winget install GitHub.cli
REM   oder: https://cli.github.com/
REM
REM Nach Installation einmalig authentifizieren:
REM   gh auth login
REM ============================================

setlocal

REM Version aus Parameter oder manuell setzen
if "%1"=="" (
    echo Usage: Create-GitHubRelease.bat [version]
    echo Example: Create-GitHubRelease.bat 1.7.4
    exit /b 1
)

set VERSION=%1
set ZIP_FILE=StrEditor-v%VERSION%-Windows-x86.zip
set RELEASE_NOTES=RELEASE-NOTES-v%VERSION%.md

REM Prüfen ob gh installiert ist
where gh >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: GitHub CLI (gh) nicht gefunden!
    echo.
    echo Installiere mit: winget install GitHub.cli
    echo Oder download: https://cli.github.com/
    exit /b 1
)

REM Prüfen ob ZIP existiert
if not exist "%ZIP_FILE%" (
    echo ERROR: %ZIP_FILE% nicht gefunden!
    echo Bitte zuerst Build-Release.bat ausführen.
    exit /b 1
)

REM Prüfen ob Release Notes existieren
if not exist "%RELEASE_NOTES%" (
    echo WARNING: %RELEASE_NOTES% nicht gefunden!
    echo Release wird ohne Notes erstellt.
    set RELEASE_NOTES=
)

echo.
echo ============================================
echo GitHub Release v%VERSION% erstellen
echo ============================================
echo ZIP: %ZIP_FILE%
echo Notes: %RELEASE_NOTES%
echo.

REM Git Tag erstellen (falls nicht vorhanden)
git tag -l "v%VERSION%" | findstr "v%VERSION%" >nul
if %ERRORLEVEL% NEQ 0 (
    echo Tag v%VERSION% erstellen...
    git tag -a v%VERSION% -m "v%VERSION%"
    git push origin v%VERSION%
)

REM GitHub Release erstellen
echo.
echo Release auf GitHub erstellen...
if "%RELEASE_NOTES%"=="" (
    gh release create v%VERSION% "%ZIP_FILE%" --title "StrEditor v%VERSION%" --generate-notes
) else (
    gh release create v%VERSION% "%ZIP_FILE%" --title "StrEditor v%VERSION%" --notes-file "%RELEASE_NOTES%"
)

if %ERRORLEVEL% EQU 0 (
    echo.
    echo ============================================
    echo SUCCESS: Release v%VERSION% erstellt!
    echo https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor/releases/tag/v%VERSION%
    echo ============================================
) else (
    echo.
    echo ERROR: Release konnte nicht erstellt werden!
)

endlocal

