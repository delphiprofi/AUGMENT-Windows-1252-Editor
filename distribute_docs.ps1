# Verteilt str-replace-editor.md auf alle Projekte

$source = ".augment\rules\str-replace-editor.md"
$sourceFullPath = Resolve-Path $source

Write-Host "Quelle: $sourceFullPath"
Write-Host ""

# Lese die Liste der Ziel-Pfade
$locations = Get-Content "AUGMENT\str-replace-editor-locations.txt" | Where-Object { $_.Trim() -ne "" }

$copied = 0
$skipped = 0
$errors = 0

foreach ($location in $locations) {
    # Überspringe die Quelle selbst
    if ($location -eq $sourceFullPath) {
        Write-Host "SKIP: $location (ist die Quelle)"
        $skipped++
        continue
    }

    # Prüfe ob Ziel existiert
    $targetDir = Split-Path $location -Parent
    if (-not (Test-Path $targetDir)) {
        Write-Host "ERROR: Verzeichnis existiert nicht: $targetDir"
        $errors++
        continue
    }

    # Kopiere Datei
    try {
        Copy-Item $source $location -Force
        Write-Host "OK: $location"
        $copied++
    }
    catch {
        Write-Host "ERROR: $location - $($_.Exception.Message)"
        $errors++
    }
}

Write-Host ""
Write-Host "Zusammenfassung:"
Write-Host "  Kopiert: $copied"
Write-Host "  Übersprungen: $skipped"
Write-Host "  Fehler: $errors"

