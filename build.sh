#!/bin/bash

#################
#
# Show help and exit
#
#################
function show_help {
	echo "Usage: $0 [-d] [-s] [-q] [-h]"
	echo "-d - enable maven debugging"
	echo "-s - skip test execution"
	echo "-q - skip integration test execution"
  echo ""
  echo "To passthrough additional arguments straight to maven, enter '--' followed by the extra arguments"
  exit 1
}

#
# This script should be executed from the directory
# it is located in. Try and stop alternatives
#
SCRIPT=`basename "$0"`

if [ ! -f $SCRIPT ]; then
  echo "This script must be executed from the same directory it is located in"
  exit 1
fi

#
# We must be in the same directory as the script so work
# out the root directory and find its absolute path
#
SCRIPT_DIR=`pwd`

echo "Script directory = $SCRIPT_DIR"

#
# Set root directory to be its parent since we are downloading
# lots of stuff and the relative path to the parent pom is
# ../build/parent/pom.xml
#
ROOT_DIR="$SCRIPT_DIR/.."

#
# By default debug is turned off
#
DEBUG=0

#
# Determine the command line options
#
while getopts ":dhsq" opt;
do
	case $opt in
	d) DEBUG=1 ;;
	h) show_help ;;
  s) SKIP=1 ;;
  q) INT_SKIP=1 ;;
	esac
done
shift "$(expr $OPTIND - 1)"

EXTRA_ARGS="$@"

#
# Source directory containing komodo codebase
# Should be the same directory as the build script location
#
SRC_DIR="${SCRIPT_DIR}"

#
# Maven repository to use.
# Ensure it only contains komodo related artifacts and
# does not clutter up user's existing $HOME/.m2 repository
#
#LOCAL_REPO="${ROOT_DIR}/m2-repository"
LOCAL_REPO="${HOME}/.m2/repository"

#
# Maven command
#
MVN="mvn clean install"

#
# Turn on dedugging if required
#
if [ "${DEBUG}" == "1" ]; then
  MVN_FLAGS="-e -X -U"
fi

#
# Skip tests
#
if [ "${SKIP}" == "1" ]; then
  SKIP_FLAG="-DskipTests"
  INTEGRATION_SKIP_FLAG="-Dintegration.skipTests=true"
fi

if [ "${INT_SKIP}" == "1" ]; then
  INTEGRATION_SKIP_FLAG="-Dintegration.skipTests=true"
fi

#
# Maven options
# -D maven.repo.local : Assign the $LOCAL_REPO as the target repository
#
MVN_FLAGS="${MVN_FLAGS} -s settings.xml -Dmaven.repo.local=${LOCAL_REPO} ${SKIP_FLAG} ${INTEGRATION_SKIP_FLAG}"

echo "==============="

# Build and test the komodo codebase
echo "Build and install the komodo plugins"
cd "${SRC_DIR}"
echo "Executing ${MVN} ${MVN_FLAGS} ${EXTRA_ARGS}"
${MVN} ${MVN_FLAGS} ${EXTRA_ARGS}
