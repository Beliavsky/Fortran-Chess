@echo off
setlocal EnableExtensions

pushd "%~dp0" >nul 2>&1
if errorlevel 1 (
    echo Failed to enter the repository directory.
    exit /b 1
)

if not exist "push_build_files_to_github.bat" (
    echo Missing push_build_files_to_github.bat in:
    echo   %CD%
    popd >nul
    exit /b 1
)

call "push_build_files_to_github.bat" github-beliavsky main
set "SCRIPT_EXIT=%ERRORLEVEL%"

popd >nul
exit /b %SCRIPT_EXIT%
