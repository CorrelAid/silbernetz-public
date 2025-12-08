
@echo off
setlocal enabledelayedexpansion

echo Update den Code...
git pull

if %ERRORLEVEL% equ 0 (
    echo Code updated successfully!
    :: Read the content of test.txt
    set /p content=<test.txt
    :: Remove any trailing whitespace or newline characters
    set "content=%content:~0,-1%"
    :: Define the new filename
    set "newfilename=versions\%content%.txt"
    :: Create the new file and write the content into it
    echo %content% > "%newfilename%"

    :: Write out current head commit id to current.txt
    echo git rev-parse HEAD > current.txt
) else (
    echo Code did not update successfully!
)

pause
