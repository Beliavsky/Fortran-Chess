@echo off
setlocal EnableExtensions

pushd "%~dp0" >nul 2>&1
if errorlevel 1 (
    echo Failed to enter the repository directory.
    exit /b 1
)

del /q *.o *.mod 2>nul

echo Removed local .o and .mod files from:
echo   %cd%

popd >nul
exit /b 0
