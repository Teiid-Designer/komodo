#!/bin/bash

#################
#
# Show help and exit
#
#################
function show_help {
        echo "Usage: $0 [-v] [-h]"
        echo "-v - teiid version"
        echo "-q - skip integration test execution"
  exit 1
}

#
# This script should be executed from the directory
# it is located in. Try and stop alternatives
#
SCRIPT=`basename "$0"`

if [ ! -f $SCRIPT ]; then
  echo "Error: This script must be executed from the same directory it is located in"
  exit 1
fi

#
# We must be in the same directory as the script so work                                                                                                
# out the root directory and find its absolute path                                                                                                     
#                                                                                                                                                       
SCRIPT_DIR=`pwd`

echo "Script directory = $SCRIPT_DIR"

#
# Determine the command line options
#
while getopts "v:m:h" opt;
do
        case $opt in
        v) VERSION=$OPTARG ;;
        m) MAJMIN=$OPTARG ;;
        h) show_help ;;
        *) show_help ;;
        esac
done

if [ -z "$VERSION" ]; then
  echo "Error: Teiid version is not specified"
  exit 1
fi

#
# Check version is Major.Minor.Micro
#
DOTS=`echo "$VERSION" | grep -o "\." | grep -c "\."`
if [[ $DOTS -lt 2 ]]; then
  echo "Error: Please use a full version, ie. X.Y.Z"
  exit 1
fi

if [ -z "$MAJMIN" ]; then
  MAJMIN=`echo $VERSION | awk -F  "." '{print $1"."$2}'`
fi

echo "Full version: $VERSION"
echo "Major/Minor version: $MAJMIN"

#
# Copy the template
#
cp -rf teiid-template "teiid-$MAJMIN"

#
# Update the pom to the new version
#
sed -i "s/{FULL_VERSION_TO_REPLACE}/$VERSION/g" "teiid-$MAJMIN"/pom.xml
sed -i "s/{MAJMIN_VERSION_TO_REPLACE}/$MAJMIN/g" "teiid-$MAJMIN"/pom.xml

#
# Try building it
#
cd "teiid-$MAJMIN"
mvn clean install

