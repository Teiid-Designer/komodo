#!/bin/bash

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

#
# Set root directory to be its parent since we are downloading
# lots of stuff and the relative path to the parent pom is
# ../build/parent/pom.xml
#
ROOT_DIR="$SCRIPT_DIR/.."

#
# Git directory
#
GIT_DIR="$ROOT_DIR/.git"

#
# Hooks package directory
#
HOOKS_DIR="$SCRIPT_DIR/hooks"

if [ ! -d "$HOOKS_DIR" ]; then
  echo "No hooks directory to install from"
  exit 1
fi

if [ ! -d "$GIT_DIR" ]; then
  echo "Cannot find the git config directory"
  exit 1
fi

if [ ! -d "$GIT_DIR/hooks" ]; then
  mkdir "$GIT_DIR/hooks"
fi

echo "Installing hooks to $GIT_DIR/hooks"
cp -rf "$HOOKS_DIR/"* "$GIT_DIR/hooks/"
