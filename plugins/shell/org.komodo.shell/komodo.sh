#!/bin/bash

SCRIPTDIR=$(dirname $0)
cd $SCRIPTDIR

# ----------------------------------------
# No script args - Interactive Mode
# ----------------------------------------
if [ $# -eq 0 ]
  then
java -cp ./target/org.komodo.shell-1.0.0-SNAPSHOT.jar:../org.komodo.shell-api/target/org.komodo.shell-api-1.0.0-SNAPSHOT.jar:../../org.komodo.utils/target/org.komodo.utils-1.0.0-SNAPSHOT.jar:./lib/aesh-0.33.13.jar org.komodo.shell.KomodoShell
#mvn -Dexec.mainClass=org.komodo.shell.KomodoShell -Dexec.args="$params" exec:java
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

java -cp ./target/org.komodo.shell-1.0.0-SNAPSHOT.jar:../org.komodo.shell-api/target/org.komodo.shell-api-1.0.0-SNAPSHOT.jar:../../org.komodo.utils/target/org.komodo.utils-1.0.0-SNAPSHOT.jar:./lib/aesh-0.33.13.jar org.komodo.shell.KomodoShell $params
#mvn -Dexec.mainClass=org.komodo.shell.KomodoShell -Dexec.args="$params" exec:java
fi
