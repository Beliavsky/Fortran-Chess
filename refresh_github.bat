@echo off
setlocal EnableExtensions

pushd "%~dp0" >nul 2>&1
if errorlevel 1 (
    echo Failed to enter the repository directory.
    exit /b 1
)

if not exist "upload_needed_files_to_github.bat" (
    echo Missing upload_needed_files_to_github.bat in:
    echo   %CD%
    popd >nul
    exit /b 1
)

call "upload_needed_files_to_github.bat"
set "SCRIPT_EXIT=%ERRORLEVEL%"

popd >nul
exit /b %SCRIPT_EXIT%
