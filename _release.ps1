# Release-Package aktualisieren
Copy-Item -Path 'C:\Delphi XE16\bin\StrEditor.exe' -Destination 'Release-Package\' -Force
Copy-Item -Path 'README.md' -Destination 'Release-Package\' -Force
Copy-Item -Path 'CHANGELOG.md' -Destination 'Release-Package\' -Force
Copy-Item -Path 'LICENSE.md' -Destination 'Release-Package\' -Force
Copy-Item -Path 'DOC\INTEGRATION.md' -Destination 'Release-Package\DOC\' -Force
Copy-Item -Path 'DOC\AGENT-COOKBOOK.md' -Destination 'Release-Package\DOC\' -Force
Write-Host 'Release-Package aktualisiert' -ForegroundColor Green

# ZIP erstellen
$zipName = 'StrEditor-v1.10.4-Windows-x86.zip'
if (Test-Path $zipName) { Remove-Item $zipName -Force }
Compress-Archive -Path 'Release-Package\*' -DestinationPath $zipName
Write-Host "ZIP erstellt: $zipName" -ForegroundColor Green

