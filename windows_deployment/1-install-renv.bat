@echo off
set APP_DIR=%~dp0..
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "setwd('%APP_DIR%'); if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::install()"
pause