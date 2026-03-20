# Release Workflow

## Release checklist

Before publishing a release, verify the following:

1. Update docs in both the repository and `Release-Package/`
2. Ensure these files are present in the release package:
   - `README.md`
   - `README.de.md`
   - `MCP-SERVER.md`
   - `CHANGELOG.md`
   - `LICENSE.md`
   - `DOC/INTEGRATION.md`
   - `DOC/AGENT-COOKBOOK.md`
   - `DOC/QUICKSTART.md`
   - `DOC/QUICKSTART.de.md`
3. Rebuild `StrEditor-v<version>-Windows-x86.zip`
4. Verify the ZIP contains the expected documentation files
5. Update `RELEASE-NOTES-v<version>.md`
6. Commit, tag, push, then create/update the GitHub release

## Packaging

Use `_release.ps1` to refresh `Release-Package/` and rebuild the ZIP.

## Publish

Typical sequence:

1. `git add ...`
2. `git commit -m "vX.Y.Z: <summary>"`
3. `git tag -a vX.Y.Z -m "vX.Y.Z: <summary>"`
4. `git push origin main`
5. `git push origin vX.Y.Z`
6. `gh release create vX.Y.Z "StrEditor-vX.Y.Z-Windows-x86.zip" --title "StrEditor vX.Y.Z" --notes-file "RELEASE-NOTES-vX.Y.Z.md"`

If the tag or release already exists, create a new patch version instead of force-rewriting published history.