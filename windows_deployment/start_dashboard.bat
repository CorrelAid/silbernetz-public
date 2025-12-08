set APP_DIR="%HOMEDRIVE%%HOMEPATH%\Documents\silbernetz-dashboard\"
echo %APP_DIR%
set "APP_DIR_R=%APP_DIR:\=/%"
cd ..

"C:\Programme\R\R-4.5.2\bin\Rscript.exe" -e "shiny::runApp('app.R', port = 3000, launch.browser = TRUE)"
pause