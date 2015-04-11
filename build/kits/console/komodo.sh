#!/bin/bash

CLASSPATH="lib/*:komodo/*"
MAINCLASS="org.komodo.shell.DefaultKomodoShell"

# ----------------------------------------
# No script args - Interactive Mode
# ----------------------------------------
if [ $# -eq 0 ]
  then
java -cp "$CLASSPATH" "$MAINCLASS"
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

java -cp "$CLASSPATH" "$MAINCLASS" $params
fi
