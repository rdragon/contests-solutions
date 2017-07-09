:: Simple 2D Command-Line Utility for Windows

@echo off

:: The installed version
set S2D_VERSION=0.4.1

:: Compile a C file
if "%~1"=="build" (
  if "%~2"=="" (
    echo Error: no input files
    exit /b 0
  )
  cl %2 /Fo%~np2 /Fe%~np2 /I %LOCALAPPDATA%\simple2d /link /LIBPATH %LOCALAPPDATA%\simple2d\simple2d.lib /SUBSYSTEM:CONSOLE
  del %~np2.obj
  exit /b 0
)

:: Output includes and libraries
if "%~1"=="--libs" (
  echo /I %LOCALAPPDATA%\simple2d /link /LIBPATH %LOCALAPPDATA%\simple2d\simple2d.lib /SUBSYSTEM:CONSOLE
  exit /b 0
)

:: Print current version
if "%~1"=="-v"        goto :print_version
if "%~1"=="--version" goto :print_version

:: If no matches, print usage message
call :print_usage


exit /b 0
:: End main script, start functions

:print_version
echo %S2D_VERSION%
exit /b 0

:print_usage
echo Simple 2D is a simple, open-source 2D graphics engine for everyone.
echo.
echo Usage: simple2d [--libs] [-v^|--version]
echo                 ^<command^> ^<options^>
echo.
echo Summary of commands and options:
echo   build         Compiles a `.c` file
echo   --libs        Outputs libraries needed to compile Simple 2D apps
echo   -v^|--version  Prints the installed version
echo.
exit /b 0
