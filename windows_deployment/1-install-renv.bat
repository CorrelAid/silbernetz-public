@echo off
set APP_DIR=%~dp0..
"C:\Program Files\R\R-5.2.0\bin\Rscript.exe" -e "renv::restore()"
pause