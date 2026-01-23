# ============================================
# GitHub Release erstellen mit gh CLI
# ============================================
# Voraussetzung: GitHub CLI installieren
#   winget install GitHub.cli
#   oder: https://cli.github.com/
#
# Nach Installation einmalig authentifizieren:
#   gh auth login
# ============================================

param(
    [Parameter(Mandatory=$false)]
    [string]$Version,
    
    [switch]$DryRun,
    [switch]$Force
)

# Version aus StrEditor.CommandLine.pas lesen falls nicht angegeben
if (-not $Version) {
    $versionLine = Get-Content "StrEditor.CommandLine.pas" | Select-String "WriteLn\( 'StrEditor v"
    if ($versionLine) {
        $Version = $versionLine -replace ".*v(\d+\.\d+\.\d+).*", '$1'
        Write-Host "Version aus Code erkannt: $Version" -ForegroundColor Cyan
    } else {
        Write-Host "ERROR: Version nicht erkannt. Bitte mit -Version angeben." -ForegroundColor Red
        exit 1
    }
}

$ZipFile = "StrEditor-v$Version-Windows-x86.zip"
$ReleaseNotes = "RELEASE-NOTES-v$Version.md"
$RepoUrl = "https://github.com/delphiprofi/AUGMENT-Windows-1252-Editor"

Write-Host ""
Write-Host "============================================" -ForegroundColor Green
Write-Host "GitHub Release v$Version erstellen" -ForegroundColor Green
Write-Host "============================================" -ForegroundColor Green

# Prüfen ob gh installiert ist
if (-not (Get-Command gh -ErrorAction SilentlyContinue)) {
    Write-Host "ERROR: GitHub CLI (gh) nicht gefunden!" -ForegroundColor Red
    Write-Host ""
    Write-Host "Installiere mit:" -ForegroundColor Yellow
    Write-Host "  winget install GitHub.cli"
    Write-Host "  oder: https://cli.github.com/"
    Write-Host ""
    Write-Host "Nach Installation authentifizieren mit:"
    Write-Host "  gh auth login"
    exit 1
}

# Prüfen ob ZIP existiert
if (-not (Test-Path $ZipFile)) {
    Write-Host "ERROR: $ZipFile nicht gefunden!" -ForegroundColor Red
    Write-Host "Bitte zuerst Build-Release.bat ausführen und ZIP erstellen."
    exit 1
}

Write-Host "ZIP: $ZipFile" -ForegroundColor Cyan
Write-Host "Notes: $ReleaseNotes" -ForegroundColor Cyan

# Prüfen ob Release bereits existiert
$existingRelease = gh release view "v$Version" 2>&1
if ($LASTEXITCODE -eq 0 -and -not $Force) {
    Write-Host ""
    Write-Host "WARNING: Release v$Version existiert bereits!" -ForegroundColor Yellow
    Write-Host "Verwende -Force um zu überschreiben."
    exit 1
}

if ($DryRun) {
    Write-Host ""
    Write-Host "[DRY-RUN] Würde folgendes ausführen:" -ForegroundColor Yellow
    Write-Host "  git tag -a v$Version -m 'v$Version'"
    Write-Host "  git push origin v$Version"
    Write-Host "  gh release create v$Version '$ZipFile' --title 'StrEditor v$Version' --notes-file '$ReleaseNotes'"
    exit 0
}

# Git Tag erstellen (falls nicht vorhanden)
$existingTag = git tag -l "v$Version"
if (-not $existingTag) {
    Write-Host ""
    Write-Host "Tag v$Version erstellen..." -ForegroundColor Cyan
    git tag -a "v$Version" -m "v$Version"
    git push origin "v$Version"
}

# GitHub Release erstellen
Write-Host ""
Write-Host "Release auf GitHub erstellen..." -ForegroundColor Cyan

if ($Force) {
    # Altes Release löschen
    gh release delete "v$Version" --yes 2>$null
}

if (Test-Path $ReleaseNotes) {
    gh release create "v$Version" $ZipFile --title "StrEditor v$Version" --notes-file $ReleaseNotes
} else {
    Write-Host "WARNING: $ReleaseNotes nicht gefunden, verwende auto-generated notes" -ForegroundColor Yellow
    gh release create "v$Version" $ZipFile --title "StrEditor v$Version" --generate-notes
}

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "============================================" -ForegroundColor Green
    Write-Host "SUCCESS: Release v$Version erstellt!" -ForegroundColor Green
    Write-Host "$RepoUrl/releases/tag/v$Version" -ForegroundColor Cyan
    Write-Host "============================================" -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "ERROR: Release konnte nicht erstellt werden!" -ForegroundColor Red
    exit 1
}

