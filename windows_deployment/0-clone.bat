@echo off
set REPO_URL=https://github.com/CorrelAid/silbernetz-public.git
set TARGET_DIR=%USERPROFILE%\Documents\silbernetz_dashboard

if not exist "%TARGET_DIR%" (
    echo Creating directory %TARGET_DIR%...
    mkdir "%TARGET_DIR%"
)

echo Cloning repository into %TARGET_DIR%...
git clone %REPO_URL% "%TARGET_DIR%"

if %ERRORLEVEL% equ 0 (
    echo Repository cloned successfully!
) else (
    echo Failed to clone repository.
)

pause
