set APP_DIR="%HOMEDRIVE%%HOMEPATH%\Documents\silbernetz-dashboard\"
echo app dir is: 
echo %APP_DIR%
echo trzing to cat
type %APP_DIR%\app.R
set "APP_DIR_R=%APP_DIR:\=/%"
cd ..
dir

"C:\Programme\R\R-4.5.2\bin\Rscript.exe" -e "shiny::runApp('app.R', port = 3000, launch.browser = TRUE)"
pause