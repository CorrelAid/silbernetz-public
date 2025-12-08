@echo off
set APP_DIR=%~dp0..
"C:\Program Files\R\R-5.2.0\bin\Rscript.exe" -e "shiny::runApp('%APP_DIR%', port = 3000, launch.browser = TRUE)"
pause