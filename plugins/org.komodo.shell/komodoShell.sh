#!/bin/sh

CLI_OPTS=""
while [ "$#" -gt 0 ]
do
    case "$1" in
      *)
          CLI_OPTS="$CLI_OPTS \"$1\""
          ;;
    esac
    shift
done


DIRNAME=`dirname "$0"`

# OS specific support (must be 'true' or 'false').
cygwin=false;
darwin=false;
linux=false;
case "`uname`" in
    CYGWIN*)
        cygwin=true
        ;;

    Darwin*)
        darwin=true
        ;;

    Linux)
        linux=true
        ;;
esac

# For Cygwin, ensure paths are in UNIX format before anything is touched
if $cygwin ; then
    [ -n "$JAVA_HOME" ] &&
        JAVA_HOME=`cygpath --unix "$JAVA_HOME"`
    [ -n "$JAVAC_JAR" ] &&
        JAVAC_JAR=`cygpath --unix "$JAVAC_JAR"`
fi

# Setup the JVM
if [ "x$JAVA" = "x" ]; then
    if [ "x$JAVA_HOME" != "x" ]; then
        JAVA="$JAVA_HOME/bin/java"
    else
        JAVA="java"
    fi
fi

# For Cygwin, switch paths to Windows format before running java
if $cygwin; then
    JAVA_HOME=`cygpath --path --windows "$JAVA_HOME"`
fi

if $darwin ; then
    # Add the apple gui packages for the gui client
    JAVA_OPTS="$JAVA_OPTS -Djboss.modules.system.pkgs=com.apple.laf,com.apple.laf.resources"
else
    # Add base package for L&F
    JAVA_OPTS="$JAVA_OPTS -Djboss.modules.system.pkgs=com.sun.java.swing"
fi

LOG_CONF=`echo $JAVA_OPTS | grep "logging.configuration"`
if [ "x$LOG_CONF" = "x" ]; then
    JAVA_OPTS="$JAVA_OPTS \"-Dlogging.configuration=file:$JBOSS_HOME/bin/jboss-cli-logging.properties\""
else
    echo "logging.configuration already set in JAVA_OPTS"
fi

CLASSPATH=./target/*:../libs/org.komodo.shell.lib/lib/*:../org.komodo.shell-api/target/*:../org.komodo.spi/target/*:../org.komodo.utils/target/*:../org.komodo.relational/target/*

eval \"$JAVA\" $JAVA_OPTS -cp \"${CLASSPATH}\" org.komodo.shell.KomodoShell "$CLI_OPTS"

