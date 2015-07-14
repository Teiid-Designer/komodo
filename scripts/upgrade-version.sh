#!/bin/bash

#################
#
# Show help and exit
#
#################
function show_help {
	echo "Usage: $0 <new version>"
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
# Get the new specified version
#
VERSION="$1"
if [ -z "$VERSION" ]; then
  show_help
fi

#
# We must be in the same directory as the script so work
# out the root directory and find its absolute path
#
SCRIPT_DIR=`pwd`

#
# Set root directory to be its parent since we are downloading
# lots of stuff and the relative path to the parent pom is
# ../build/parent/pom.xml
#
ROOT_DIR="$SCRIPT_DIR/.."

#
# Maven repository to use.
# Ensure it only contains komodo related artifacts and
# does not clutter up user's existing $HOME/.m2 repository
#
LOCAL_REPO="${ROOT_DIR}/../m2-repository"

echo "VERSION = $VERSION"

#
# By default debug is turned off
#
DEBUG=0

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
fi

#
# Maven options
# -P <profiles> : The profiles to be used for downloading jbosstools artifacts
# -D maven.repo.local : Assign the $LOCAL_REPO as the target repository
#
MVN_FLAGS="${MVN_FLAGS} -P target-platform,multiple.target -Dmaven.repo.local=${LOCAL_REPO} -Dno.jbosstools.site -Dtycho.localArtifacts=ignore ${SKIP_FLAG}"

#
# Maven command
#
MVN="mvn ${MVN_FLAGS} org.eclipse.tycho:tycho-versions-plugin:set-version -DnewVersion=${VERSION}"

echo "==============="

cd "${ROOT_DIR}"
echo "Executing ${MVN}"
#${MVN}

echo "==============="
echo "Changing version in build/pom.xml"
sed -i "s/<org\.komodo\.version>.*<\/org\.komodo\.version>/<org\.komodo\.version>${VERSION}<\/org\.komodo\.version>/" "${ROOT_DIR}/build/pom.xml"
