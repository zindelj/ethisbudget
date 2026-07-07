@echo off
rem ============================================================
rem  ethisbudget installer / updater  (standalone)
rem  Double-click to install the app on this machine, or re-run
rem  any time to update to the latest version. Safe to repeat.
rem  What it does:
rem    1. installs R 4.4.3 if no R is found on this machine
rem    2. downloads the latest app from GitHub into Documents\ethisbudget
rem    3. creates a desktop shortcut with the app icon
rem  It never touches the budget data folder.
rem ============================================================
setlocal
set "REPO_ZIP=https://github.com/zindelj/ethisbudget/archive/refs/heads/main.zip"
set "DEST=%USERPROFILE%\Documents\ethisbudget"

rem --- 1/3: R ---------------------------------------------------
set "RSCRIPT="
for /f "delims=" %%p in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%p"
if not defined RSCRIPT for /f "tokens=2,*" %%a in ('reg query "HKCU\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RSCRIPT=%%b\bin\Rscript.exe"
if not defined RSCRIPT for /f "tokens=2,*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RSCRIPT=%%b\bin\Rscript.exe"
if defined RSCRIPT (
  echo [1/3] R found: "%RSCRIPT%"
) else (
  echo [1/3] R not found - downloading R 4.4.3 ^(about 85 MB^)...
  curl.exe -fL -o "%TEMP%\R-4.4.3-win.exe" https://cran.r-project.org/bin/windows/base/old/4.4.3/R-4.4.3-win.exe
  if errorlevel 1 (
    echo Download failed - check the internet connection, or install R manually
    echo from https://cran.r-project.org and re-run this installer.
    pause
    exit /b 1
  )
  echo        installing R 4.4.3 ^(a Windows permission prompt may appear^)...
  "%TEMP%\R-4.4.3-win.exe" /VERYSILENT /NORESTART
  if errorlevel 1 (echo R installation failed or was cancelled. & pause & exit /b 1)
  del "%TEMP%\R-4.4.3-win.exe" >nul 2>&1
)

rem --- 2/3: app files -------------------------------------------
echo [2/3] Downloading the latest app version from GitHub...
set "WORK=%TEMP%\ethisbudget_install"
rmdir /s /q "%WORK%" >nul 2>&1
mkdir "%WORK%"
curl.exe -fL -o "%WORK%\app.zip" %REPO_ZIP%
if errorlevel 1 (
  echo Download failed - check the internet connection ^(and that the
  echo repository is public^).
  pause
  exit /b 1
)
tar -xf "%WORK%\app.zip" -C "%WORK%"
if errorlevel 1 (echo Could not unpack the downloaded app. & pause & exit /b 1)
echo        installing to "%DEST%" ...
robocopy "%WORK%\ethisbudget-main" "%DEST%" /E /NFL /NDL /NJH /NJS /NP
if errorlevel 8 (echo Copy failed. & pause & exit /b 1)
rmdir /s /q "%WORK%" >nul 2>&1

rem --- 3/3: desktop shortcut ------------------------------------
echo [3/3] Creating desktop shortcut...
powershell -NoProfile -Command ^
 "$s = (New-Object -ComObject WScript.Shell).CreateShortcut([Environment]::GetFolderPath('Desktop') + '\ethisbudget.lnk');" ^
 "$s.TargetPath = 'C:\Windows\System32\cmd.exe';" ^
 "$s.Arguments  = '/c \"\"%DEST%\Launch ethisbudget.bat\"\"';" ^
 "$s.WorkingDirectory = '%DEST%';" ^
 "$s.IconLocation = '%DEST%\ethisbudget.ico';" ^
 "$s.Save()"

echo.
echo Done. Start the app from the new 'ethisbudget' desktop shortcut
echo (right-click it to pin to the taskbar). The first app start on a
echo new machine takes a while: R packages are installed once.
echo To update later: just double-click this file again.
pause
