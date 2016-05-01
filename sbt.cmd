@REM sbt launcher script
@setlocal enabledelayedexpansion

@echo off

set ERROR_CODE=0
set SBT_LAUNCH_JAR=%~dp0%sbt-launch.jar

@REM Detect if we were double clicked, although theoretically A user could manually run cmd /c
for %%x in (%cmdcmdline%) do if %%~x==/c set DOUBLECLICKED=1

@REM We use the value of the JAVACMD environment variable if defined
set _JAVACMD=%JAVACMD%

if "%_JAVACMD%"=="" (
  if not "%JAVA_HOME%"=="" (
    if exist "%JAVA_HOME%\bin\java.exe" set "_JAVACMD=%JAVA_HOME%\bin\java.exe"

    @@REM if there is a java home set we make sure it is the first picked up when invoking 'java'
    SET "PATH=%JAVA_HOME%\bin;%PATH%"
  )
)

if "%_JAVACMD%"=="" set _JAVACMD=java

@REM Detect if this java is ok to use.
for /F %%j in ('"%_JAVACMD%" -version  2^>^&1') do (
  if %%~j==java set JAVAINSTALLED=1
  if %%~j==openjdk set JAVAINSTALLED=1
)

@REM Detect the same thing about javac
if "%_JAVACCMD%"=="" (
  if not "%JAVA_HOME%"=="" (
    if exist "%JAVA_HOME%\bin\javac.exe" set "_JAVACCMD=%JAVA_HOME%\bin\javac.exe"
  )
)
if "%_JAVACCMD%"=="" set _JAVACCMD=javac
for /F %%j in ('"%_JAVACCMD%" -version 2^>^&1') do (
  if %%~j==javac set JAVACINSTALLED=1
)

@REM Check what Java version is being used
for /f "tokens=3" %%g in ('java -version 2^>^&1 ^| findstr /i "version"') do (
  set JAVA_VERSION=%%g
)

@REM Strips away the " characters
set JAVA_VERSION=%JAVA_VERSION:"=%

@REM Make sure Java 8 is installed
for /f "delims=. tokens=1-3" %%v in ("%JAVA_VERSION%") do (
  set MAJOR=%%v
  set MINOR=%%w
  set BUILD=%%x

  if "!MINOR!" GEQ "8" (
    set HASJAVA8=true
  )
)

@REM BAT has no logical or, so we do it OLD SCHOOL! Oppan Redmond Style
set JAVAOK=true
if not defined JAVAINSTALLED set JAVAOK=false
if not defined JAVACINSTALLED set JAVAOK=false
if not defined HASJAVA8 set JAVAOK=false

if "%JAVAOK%"=="false" (
  echo.
  echo A Java 8 JDK is not installed or can't be found.
  if not "%JAVA_HOME%"=="" (
    echo JAVA_HOME = "%JAVA_HOME%"
  )
  echo.
  echo Please go to
  echo   http://www.oracle.com/technetwork/java/javase/downloads/index.html
  echo and download a valid Java 8 JDK and install before running sbt.
  echo.
  echo If you think this message is in error, please check
  echo your environment variables to see if "java.exe" and "javac.exe" are
  echo available via JAVA_HOME or PATH.
  echo.
  if defined DOUBLECLICKED pause
  exit /B 1
)

if "%~1"=="shell" (
  set CMDS=
) else (
  if "%~1"=="" (
    set CMDS=default
  ) else (
    set CMDS=%~1
  )
)

set SBT_OPTS=-Xms512M -Xmx1024M -Xss1M -XX:MetaspaceSize=64M -XX:MaxMetaspaceSize=256M -XX:+CMSClassUnloadingEnabled

@REM Checks if the command contains spaces to know if it should be wrapped in quotes or not
set NON_SPACED_CMD=%_JAVACMD: =%

@REM Run sbt
if "%_JAVACMD%"=="%NON_SPACED_CMD%" %_JAVACMD% %SBT_OPTS% -jar "%SBT_LAUNCH_JAR%" %CMDS%
if NOT "%_JAVACMD%"=="%NON_SPACED_CMD%" "%_JAVACMD%" %SBT_OPTS% -jar "%SBT_LAUNCH_JAR%" %CMDS%

if ERRORLEVEL 1 goto error
goto end

:error
set ERROR_CODE=1

:end

@endlocal

exit /B %ERROR_CODE%