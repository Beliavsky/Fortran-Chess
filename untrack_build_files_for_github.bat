@echo off
setlocal EnableExtensions

pushd "%~dp0" >nul 2>&1
if errorlevel 1 (
    echo Failed to enter the repository directory.
    exit /b 1
)

git rev-parse --is-inside-work-tree >nul 2>&1
if errorlevel 1 (
    echo This script must be run from inside a git repository.
    popd >nul
    exit /b 1
)

set "FOUND_ANY="
for /f "usebackq delims=" %%I in (`git ls-files *.o *.mod`) do (
    set "FOUND_ANY=1"
    git rm --cached "%%I"
    if errorlevel 1 (
        echo Failed to untrack %%I
        popd >nul
        exit /b 1
    )
)

if not defined FOUND_ANY (
    echo No tracked .o or .mod files were found.
    popd >nul
    exit /b 0
)

echo.
echo Tracked .o and .mod files have been removed from the Git index.
echo They will be deleted from GitHub after you commit and push.
echo Local copies were kept. Run remove_build_files.bat if you also want to delete them locally.

popd >nul
exit /b 0
