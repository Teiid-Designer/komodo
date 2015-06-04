#!/bin/bash

<<<<<<< HEAD:build/kits/console/vdbbuilder.sh
CLASSPATH="lib/*:vdbbuilder/*"
=======
CLASSPATH="lib/*:vdbuilder/*"
>>>>>>> I made the following changes to the shell::build/kits/console/vdbuilder.sh
MAINCLASS="org.komodo.shell.DefaultKomodoShell"

# Uncomment these JPDA settings for remote socket debugging
#JAVA_OPTS="-agentlib:jdwp=transport=dt_socket,address=8787,server=y,suspend=y"

# ----------------------------------------
# No script args - Interactive Mode
# ----------------------------------------
if [ $# -eq 0 ]
  then
java $JAVA_OPTS -cp "$CLASSPATH" "$MAINCLASS"
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

java $JAVA_OPTS -cp "$CLASSPATH" "$MAINCLASS" $params
fi
