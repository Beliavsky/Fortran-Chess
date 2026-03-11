@echo off
setlocal EnableExtensions EnableDelayedExpansion

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

for /f "usebackq delims=" %%I in (`git branch --show-current`) do set "CURRENT_BRANCH=%%I"
if not defined CURRENT_BRANCH (
    echo Could not determine the current branch. Detached HEAD is not supported by this script.
    popd >nul
    exit /b 1
)

set "TARGET_REMOTE=%~1"
if not defined TARGET_REMOTE (
    git remote get-url github-beliavsky >nul 2>&1
    if errorlevel 1 (
        set "TARGET_REMOTE=origin"
    ) else (
        set "TARGET_REMOTE=github-beliavsky"
    )
)

set "TARGET_BRANCH=%~2"
if not defined TARGET_BRANCH (
    if /I "%TARGET_REMOTE%"=="github-beliavsky" (
        set "TARGET_BRANCH=main"
    ) else (
        set "TARGET_BRANCH=%CURRENT_BRANCH%"
    )
)

echo Current branch: %CURRENT_BRANCH%
echo Target remote:  %TARGET_REMOTE%
echo Remote branch:  %TARGET_BRANCH%
echo.
echo Staging files needed to build and run the project...
echo.

call :stage_if_exists ".gitignore"
call :stage_if_exists "Makefile"
call :stage_if_exists "README.md"
call :stage_if_exists "%~nx0"

for %%F in (*.f90) do (
    if exist "%%F" git add -- "%%F"
)

for %%F in (book_*.txt) do (
    if exist "%%F" git add -- "%%F"
)

if exist "assets" git add -- "assets"

call :remove_if_tracked "chess.exe"
call :remove_if_tracked "chessgui.exe"
call :remove_if_tracked "games.pgn"
call :remove_if_tracked "search_debug.log"
call :remove_if_tracked "gui_debug.log"
call :remove_if_tracked "temp.pgn"
call :remove_if_tracked "temp_temp.txt"
call :remove_if_tracked "#book_black.txt.asv#"
call :remove_if_tracked "fortran-chess"
call :remove_if_tracked "tmp_book_error_test"
call :remove_if_tracked "tmp_book_fix_accept"
call :remove_if_tracked "tmp_book_fix_decline"
call :remove_if_tracked "tmp_probe_bestmove.exe"
call :remove_if_tracked "tmp_probe_bestmove.pgn"
call :remove_if_tracked "tmp_probe_qxa3.exe"
call :remove_if_tracked "tmp_probe_qxa3.pgn"

echo Staged build/runtime files:
git diff --cached --name-status
echo.

git diff --cached --quiet
if errorlevel 1 goto have_changes

echo No staged build/runtime changes were found.
popd >nul
exit /b 0

:have_changes
choice /M "Commit and push the staged build/runtime files"
if errorlevel 2 (
    echo Push cancelled.
    popd >nul
    exit /b 0
)

set "COMMIT_MESSAGE=Add Windows GUI, opening-book validation/fixes, autoplay/takeback/suggest commands, PGN/logging improvements, TimeControl/threefold support, configurable GUI depth, and search fixes"
set /p "COMMIT_MESSAGE=Commit message [Add Windows GUI, opening-book validation/fixes, autoplay/takeback/suggest commands, PGN/logging improvements, TimeControl/threefold support, configurable GUI depth, and search fixes]: "
if "!COMMIT_MESSAGE!"=="" set "COMMIT_MESSAGE=Add Windows GUI, opening-book validation/fixes, autoplay/takeback/suggest commands, PGN/logging improvements, TimeControl/threefold support, configurable GUI depth, and search fixes"

git commit -m "!COMMIT_MESSAGE!"
if errorlevel 1 (
    echo git commit failed.
    popd >nul
    exit /b 1
)

git push -u %TARGET_REMOTE% HEAD:%TARGET_BRANCH%
if errorlevel 1 (
    echo Push failed.
    popd >nul
    exit /b 1
)

echo.
echo Push completed successfully.

popd >nul
exit /b 0

:stage_if_exists
if exist "%~1" git add -- "%~1"
exit /b 0

:remove_if_tracked
git ls-files --error-unmatch -- "%~1" >nul 2>&1
if errorlevel 1 exit /b 0
git rm --cached -r --ignore-unmatch -- "%~1" >nul
exit /b 0
