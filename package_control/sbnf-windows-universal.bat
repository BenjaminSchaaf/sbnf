@echo off

if "%PROCESSOR_ARCHITECTURE%"=="AMD64" (
    "%~dp0sbnf-x86_64-pc-windows-msvc.exe" %*
) else (
    "%~dp0sbnf-i686-pc-windows-msvc.exe" %*
)

:end
