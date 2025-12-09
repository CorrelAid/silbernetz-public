cd ..

"%HOMEDRIVE%%HOMEPATH%\AppData\Local\Programs\R\R-4.5.2\bin\Rscript.exe" -e "shiny::runApp('app.R', port = 3000, launch.browser = TRUE)"
pause