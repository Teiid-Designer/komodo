#!/bin/bash

CLASSPATH="lib/*:komodo/*"

# ----------------------------------------
# No script args - Interactive Mode
# ----------------------------------------
if [ $# -eq 0 ]
  then
java -cp "$CLASSPATH" org.komodo.shell.KomodoShell
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

java -cp "$CLASSPATH" org.komodo.shell.KomodoShell $params
fi
