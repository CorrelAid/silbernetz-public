
@echo off
setlocal enabledelayedexpansion

echo Update den Code...
git pull

if %ERRORLEVEL% equ 0 (
    echo Code updated successfully!

    :: Write out current head commit id to current.txt
    for /f %%i in ('git rev-parse HEAD') do set GITHASH=%%i
    for /f %%i in ('powershell -NoProfile -Command "(Get-Date).ToString(\"yyyy-MM-dd\")"') do set ISODATE=%%i

    echo %ISODATE%, %GITHASH% >> versions.txt
) else (
    echo Code did not update successfully!
)

pause
