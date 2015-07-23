@echo off
rem -------------------------------------------------------------------------
rem VDBBuilder start script for Windows
rem -------------------------------------------------------------------------
rem
rem Uncomment JPDA settings for remote socket debugging
rem set "JAVA_OPTS=%JAVA_OPTS% -agentlib:jdwp=transport=dt_socket,address=8787,server=y,suspend=y"

@if not "%ECHO%" == ""  echo %ECHO%
@if "%OS%" == "Windows_NT" setlocal

if "%OS%" == "Windows_NT" (
  set "DIRNAME=%~dp0%"
) else (
  set DIRNAME=.\
)

pushd "%DIRNAME%"
set "RESOLVED_VDBBUILDER_HOME=%CD%"
popd

if "x%VDBBUILDER_HOME%" == "x" (
  set "VDBBUILDER_HOME=%RESOLVED_VDBBUILDER_HOME%" 
)

pushd "%VDBBUILDER_HOME%"
set "SANITIZED_VDBBUILDER_HOME=%CD%"
popd

if "%RESOLVED_VDBBUILDER_HOME%" NEQ "%SANITIZED_VDBBUILDER_HOME%" (
    echo WARNING VDBBUILDER_HOME may be pointing to a different installation - unpredictable results may occur.
)

set DIRNAME=

if "%OS%" == "Windows_NT" (
  set "PROGNAME=%~nx0%"
) else (
  set "PROGNAME=jdr.bat"
)

rem Setup JVM
if "x%JAVA_HOME%" == "x" (
  set  JAVA=java
  echo JAVA_HOME is not set. Unexpected results may occur.
  echo Set JAVA_HOME to the directory of your local JDK to avoid this message.
) else (
  set "JAVA=%JAVA_HOME%\bin\java"
)

rem Set default classpath
if "x%VDBBUILDER_CLASSPATH%" == "x" (
  set  "VDBBUILDER_CLASSPATH=%VDBBUILDER_HOME%\lib\*;%VDBBUILDER_HOME%\vdbbuilder\*"
)

set MAINCLASS=org.komodo.shell.DefaultKomodoShell

"%JAVA%" %JAVA_OPTS% -cp "%VDBBUILDER_CLASSPATH%" "%MAINCLASS%" %*

:END
