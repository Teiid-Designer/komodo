#!/bin/sh

type xjc > /dev/null 2>&1 || {
  echo >&2 "I require 'xjc' but it's not installed.  Aborting.";
  exit 1;
}

xjc teiid-vdb.xsd -d src -b bindings.xml -extension -Xinject-code -Xannotate
