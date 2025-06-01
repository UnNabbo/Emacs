@echo off
setlocal enabledelayedexpansion

mkdir user

REM Get script folder and convert backslashes to forward slashes
set "MyPath=%~dp0"
set "MyPath=%MyPath:\=/%"

REM Remove trailing slash if present
if "!MyPath:~-1!"=="/" set "MyPath=!MyPath:~0,-1!"

REM Create .emacs file in current directory

echo (setq user-init-file "!MyPath!/init.el") > .emacs
echo (setq user-emacs-directory "!MyPath!/user") >> .emacs
echo (load user-init-file) >> .emacs
echo (defun open-user-init-file () >> .emacs
echo   "Open the user's init file in the current buffer." >> .emacs
echo   (interactive) >> .emacs
echo   (find-file user-init-file)) >> .emacs


REM Ensure HOME is defined, else fall back to USERPROFILE
if not defined HOME set "HOME=%USERPROFILE%"

REM Move .emacs to HOME directory
move /Y ".emacs" "%HOME%\" > nul

endlocal
