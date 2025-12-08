@echo off
set APP_DIR=%~dp0..
"C:\Program Files\R\R-4.4.0\bin\Rscript.exe" -e "shiny::runApp('%APP_DIR%', launch.browser = TRUE)"
pause