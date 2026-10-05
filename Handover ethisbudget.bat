@echo off
rem Double-click to write an anonymised handover file for grant writing.
rem A folder picker opens; pick your budget data folder. The summary
rem (handover_<date>.md) and the local-only key (handover_<date>_KEY.txt)
rem are written into that same folder. No Shiny app needed.
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

echo Writing handover file with "%RSCRIPT%" ...
"%RSCRIPT%" handover.R %*
echo.
pause
