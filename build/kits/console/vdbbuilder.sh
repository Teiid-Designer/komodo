#!/bin/bash

# ------------------------------------------------------
# VDBBuilder start script
# ------------------------------------------------------
#
# Uncomment JPDA settings for remote socket debugging
#JAVA_OPTS="$JAVA_OPTS -agentlib:jdwp=transport=dt_socket,address=8787,server=y,suspend=y"

DIRNAME=`dirname "$0"`

# OS specific support (must be 'true' or 'false').
cygwin=false;
if  [ `uname|grep -i CYGWIN` ]; then
    cygwin=true;
fi

# For Cygwin, ensure paths are in UNIX format before anything is touched
if $cygwin ; then
    [ -n "$VDBBUILDER_HOME" ] &&
        VDBBUILDER_HOME=`cygpath --unix "$VDBBUILDER_HOME"`
    [ -n "$JAVA_HOME" ] &&
        JAVA_HOME=`cygpath --unix "$JAVA_HOME"`
    [ -n "$JAVAC_JAR" ] &&
        JAVAC_JAR=`cygpath --unix "$JAVAC_JAR"`
fi

# Setup VDBBUILDER_HOME
RESOLVED_VDBBUILDER_HOME=`cd "$DIRNAME"; pwd`
if [ "x$VDBBUILDER_HOME" = "x" ]; then
    # get the full path (without any relative bits)
    VDBBUILDER_HOME=$RESOLVED_VDBBUILDER_HOME
else
 SANITIZED_VDBBUILDER_HOME=`cd "$VDBBUILDER_HOME"; pwd`
 if [ "$RESOLVED_VDBBUILDER_HOME" != "$SANITIZED_VDBBUILDER_HOME" ]; then
   echo "WARNING VDBBUILDER_HOME may be pointing to a different installation - unpredictable results may occur."
   echo ""
 fi
fi
export VDBBUILDER_HOME

# Setup the JVM
if [ "x$JAVA" = "x" ]; then
    if [ "x$JAVA_HOME" != "x" ]; then
        JAVA="$JAVA_HOME/bin/java"
    else
        JAVA="java"
    fi
fi

if [ "x$VDBBUILDER_CLASSPATH" = "x" ]; then
    VDBBUILDER_CLASSPATH="$VDBBUILDER_HOME/lib/*:$VDBBUILDER_HOME/vdbbuilder/*"
fi

# For Cygwin, switch paths to Windows format before running java
if $cygwin; then
    JAVA_HOME=`cygpath --path --windows "$JAVA_HOME"`
    VDBBUILDER_CLASSPATH=`cygpath --path --windows "$VDBBUILDER_CLASSPATH"`
fi

MAINCLASS="org.komodo.shell.DefaultKomodoShell"

# ----------------------------------------
# No script args - Interactive Mode
# ----------------------------------------
if [ $# -eq 0 ]
  then
eval \"$JAVA\" $JAVA_OPTS -cp \"${VDBBUILDER_CLASSPATH}\" \"${MAINCLASS}\"
fi

# ----------------------------------------
# Script args - append and pass to shell
# ----------------------------------------
if [ $# -ne 0 ]
then
args=("$@")

#echo Number of arguments: $#
#echo First argument: ${args[0]}
#echo secnd argument: ${args[1]}

params="${args[0]} ${args[1]}"
#echo PARAMS: $params


eval \"$JAVA\" $JAVA_OPTS -cp \"${VDBBUILDER_CLASSPATH}\" \"${MAINCLASS}\" $params
fi
