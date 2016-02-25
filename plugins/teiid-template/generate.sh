#!/bin/bash

#################
#
# Show help and exit
#
#################
function show_help {
        echo "Usage: $0 [-v] [-h]"
        echo "-v - teiid version"
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
while getopts "v:h" opt;
do
        case $opt in
        v) VERSION=$OPTARG ;;
        h) show_help ;;
        *) show_help ;;
        esac
done

if [ -z "$VERSION" ]; then
  echo "Error: Teiid version is not specified"
  exit 1
fi

mvn verify -D required-teiid-version=$VERSION

