@echo off

if "%PROCESSOR_ARCHITECTURE%"=="AMD64" (
    start "" "%~dp0sbnf-x86_64-pc-windows-msvc.exe" %*
) else (
    start "" "%~dp0sbnf-i686-pc-windows-msvc.exe" %*
)

:end
