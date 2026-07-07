@echo off
rem Double-click to start the ethisbudget app without opening RStudio.
rem Runs the Shiny app with the Windows R installation; the browser opens
rem automatically when the app is ready. Closing this window stops the app.
setlocal
cd /d "%~dp0"

set "RSCRIPT="
for /f "delims=" %%p in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%p"
if not defined RSCRIPT for /f "tokens=2,*" %%a in ('reg query "HKCU\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RSCRIPT=%%b\bin\Rscript.exe"
if not defined RSCRIPT for /f "tokens=2,*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RSCRIPT=%%b\bin\Rscript.exe"
if not defined RSCRIPT (
  echo Could not find R. Is it installed on Windows?
  pause
  exit /b 1
)

echo Starting ethisbudget with "%RSCRIPT%" ...
echo (first run on a new machine may pause here to install R packages)
"%RSCRIPT%" -e "if (!requireNamespace('shiny', quietly = TRUE)) { message('Restoring R packages - one-time setup, this can take a while...'); renv::restore(prompt = FALSE) }"
echo The app opens in your browser shortly. Keep this window open while working;
echo close it (or press Ctrl+C) to stop the app.
"%RSCRIPT%" -e "shiny::runApp('app.R', port = 4242, launch.browser = TRUE)"
pause
