@echo off
setlocal enabledelayedexpansion

rem ================================================================
rem  STRICT IBM MAINFRAME COBOL BUILD SCRIPT
rem  Emulates z/OS COBOL compiler options and JCL condition codes
rem ================================================================

rem Check parameter
if "%1"=="" (
    echo ERROR: NO PROGRAM SPECIFIED
    echo USAGE: build program_name [without extension]
    exit /b 16
)

rem Set project-specific paths (mimics JCL DD statements)
set COB_COPY_DIR=.\COPYLIB
set COB_LIBRARY_PATH=.\LOADLIB
set PROGRAM=%1

rem Create datasets (directories) if missing
if not exist COPYLIB mkdir COPYLIB
if not exist LOADLIB mkdir LOADLIB  
if not exist LISTING mkdir LISTING
if not exist SYSOUT mkdir SYSOUT

rem Clean previous outputs
if exist %PROGRAM%.exe del %PROGRAM%.exe
if exist LISTING\%PROGRAM%.lst del LISTING\%PROGRAM%.lst

echo ========================================
echo  COMPILING %PROGRAM%
echo  TIME: %DATE% %TIME%
echo ========================================

rem Strict IBM mainframe compilation
rem Mimics COBOL compiler options: RENT,APOST,NODYNAM,TEST,XREF
cobc -x ^
     -std=ibm-strict ^
     -fixed ^
     -Wall ^
     -fmax-errors=30 ^
     -fdefaultbyte=00 ^
     -fnotrunc ^
     -fcomplex-odo ^
     -fperform-osvs ^
     -fmove-ibm ^
     -fsign=ASCII ^
     -ffold-copy=UPPER ^
     -ffold-call=UPPER ^
     -fimplicit-init ^
     -fintrinsics=ALL ^
     -ftrap ^
     -debug ^
     -fdebugging-line ^
     -fsource-location ^
     -fstack-check ^
     -fbinary-size=2-4-8 ^
     -fbinary-byteorder=big-endian ^
     -t LISTING\%PROGRAM%.lst ^
     -T LISTING\%PROGRAM%.c ^
     %PROGRAM%.cbl 2> SYSOUT\%PROGRAM%.err

set RC=!errorlevel!

rem ========================================
rem  CONDITION CODE CHECKING (JCL STYLE)
rem ========================================

if !RC! equ 0 (
    echo.
    echo COMPILATION SUCCESSFUL - RC=0000
    echo LOAD MODULE: %PROGRAM%.exe
    goto :check_warnings
)

if !RC! leq 4 (
    echo.
    echo WARNING CONDITION - RC=0004
    echo CHECK LISTING FOR DETAILS
    type SYSOUT\%PROGRAM%.err | findstr /i "warning"
    goto :check_warnings
)

if !RC! leq 8 (
    echo.
    echo ERROR CONDITION - RC=0008
    echo COMPILATION FAILED
    type SYSOUT\%PROGRAM%.err | findstr /i "error"
    goto :abend
)

if !RC! geq 12 (
    echo.
    echo SEVERE ERROR - RC=0012
    echo SYSTEM ABEND
    type SYSOUT\%PROGRAM%.err
    goto :abend
)

:check_warnings
rem Scan for specific mainframe-relevant warnings
findstr /i "truncat" SYSOUT\%PROGRAM%.err >nul 2>&1
if !errorlevel! equ 0 (
    echo.
    echo ALERT: TRUNCATION WARNING DETECTED
    echo This would cause data loss on mainframe
)

findstr /i "overlap" SYSOUT\%PROGRAM%.err >nul 2>&1
if !errorlevel! equ 0 (
    echo.
    echo ALERT: OVERLAPPING STORAGE DETECTED  
    echo This causes S0C4 ABEND on mainframe
)

rem Display cross-reference if requested
if "%2"=="XREF" (
    echo.
    echo CROSS REFERENCE LISTING:
    echo ========================
    cobc -x -std=ibm-strict -fixed -Xref %PROGRAM%.cbl
)

rem Run test execution if compilation successful
if exist %PROGRAM%.exe (
    echo.
    echo ========================================
    echo  TEST EXECUTION
    echo ========================================
    %PROGRAM%.exe
    echo.
    echo EXECUTION RC=!errorlevel!
)

goto :end

:abend
echo.
echo *** ABEND - SEE SYSOUT FOR DIAGNOSTICS ***
echo.
type SYSOUT\%PROGRAM%.err
exit /b !RC!

:end
echo.
echo ========================================
echo  JOB COMPLETED
echo ========================================
endlocal
exit /b !RC!