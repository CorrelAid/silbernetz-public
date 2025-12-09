
@echo off
setlocal enabledelayedexpansion

echo Update den Code...
git pull

if %ERRORLEVEL% equ 0 (
    echo Code updated successfully!

    :: Write out current head commit id to current.txt
    for /f %%i in ('git rev-parse HEAD') do set GITHASH=%%i
    for /f %%i in ('powershell -NoProfile -Command "(Get-Date).ToString(\"yyyy-MM-dd\")"') do set ISODATE=%%i

    echo %ISODATE%, %GITHASH% >> deployed_versions.txt

    rem Get git hash (suppress error output). usebackq + backticks form.
    for /f "usebackq delims=" %%H in (`git rev-parse --verify HEAD 2^>nul`) do set "GITHASH=%%H"

    if not defined GITHASH (
    echo ERROR: Could not get git hash. Make sure you're in a git repo and git is in PATH.
    endlocal
    exit /b 1
    )

    rem Get ISO date via PowerShell (locale independent)
    for /f "usebackq delims=" %%D in (`powershell -NoProfile -Command "(Get-Date).ToString('yyyy-MM-dd')"`) do set "ISODATE=%%D"

    if not defined ISODATE (
    rem fallback: use %date% (may be locale dependent)
    set "ISODATE=%date%"
    )

    rem Append to versions.txt using delayed expansion so variables expand at runtime
    >>deployed_versions.txt echo !ISODATE!, !GITHASH!

    endlocal

) else (
    echo Code did not update successfully!
)


pause
