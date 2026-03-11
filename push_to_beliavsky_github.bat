@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "TARGET_REMOTE=github-beliavsky"
set "TARGET_URL=https://github.com/Beliavsky/Fortran-Chess"
set "TARGET_BRANCH=main"

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

echo Current branch: %CURRENT_BRANCH%
echo Target remote:  %TARGET_REMOTE%
echo Target repo:    %TARGET_URL%
echo Remote branch:  %TARGET_BRANCH%
echo.

git status --short
echo.

set "WORKTREE_DIRTY="
for /f "usebackq delims=" %%I in (`git status --porcelain`) do set "WORKTREE_DIRTY=1"

if defined WORKTREE_DIRTY (
    choice /M "Commit the current working tree before pushing"
    if errorlevel 2 goto skip_commit

    set "COMMIT_MESSAGE=Sync local repo to Beliavsky/Fortran-Chess"
    set /p "COMMIT_MESSAGE=Commit message [Sync local repo to Beliavsky/Fortran-Chess]: "
    if "!COMMIT_MESSAGE!"=="" set "COMMIT_MESSAGE=Sync local repo to Beliavsky/Fortran-Chess"

    git add -A
    if errorlevel 1 (
        echo git add failed.
        popd >nul
        exit /b 1
    )

    git commit -m "!COMMIT_MESSAGE!"
    if errorlevel 1 (
        echo git commit did not succeed. Resolve that and run the script again.
        popd >nul
        exit /b 1
    )
)

:skip_commit
git remote get-url %TARGET_REMOTE% >nul 2>&1
if errorlevel 1 (
    git remote add %TARGET_REMOTE% %TARGET_URL%
) else (
    git remote set-url %TARGET_REMOTE% %TARGET_URL%
)
if errorlevel 1 (
    echo Failed to configure remote %TARGET_REMOTE%.
    popd >nul
    exit /b 1
)

echo.
echo Pushing %CURRENT_BRANCH% to %TARGET_REMOTE% as %TARGET_BRANCH%...
git push -u %TARGET_REMOTE% HEAD:%TARGET_BRANCH%
if errorlevel 1 (
    echo Push failed.
    popd >nul
    exit /b 1
)

echo.
echo Push completed successfully.
echo Remote %TARGET_REMOTE% now points to %TARGET_URL%.

popd >nul
exit /b 0
