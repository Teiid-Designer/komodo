#!/bin/sh

DEST="$WORKSPACE/vdb-builder-sources/build/target"

JB_DIR="$WORKSPACE/maven-repo/org/jboss/as/jboss-as-dist/7.5.0.Final-redhat-15"
JB="jboss-as-dist-7.5.0.Final-redhat-15.zip"
JBOSS_DIST="jboss-eap-6.4"
JB_DEPLOYMENT="$JBOSS_DIST/standalone/deployments/"
JB_CONFIG="$JBOSS_DIST/standalone/configuration/"

JB_USER_PROPS_NAME="application-users.properties"
JB_USER_PROPS="$WORKSPACE/vdb-builder-sources/server/komodo-rest/src/test/resources/credentials/$JB_USER_PROPS_NAME"
JB_ROLES_PROPS_NAME="application-roles.properties"
JB_ROLES_PROPS="$WORKSPACE/vdb-builder-sources/server/komodo-rest/src/test/resources/credentials/$JB_ROLES_PROPS_NAME"

TEIID_DIR="$WORKSPACE/maven-repo/org/jboss/teiid/teiid/${TEIID_VERSION}"
TEIID="teiid-${TEIID_VERSION}-jboss-dist.zip"

VDBBUILDER_NAME="vdb-builder.war"
VDBBUILDER="$WORKSPACE/vdb-builder-sources/server/komodo-rest/target/$VDBBUILDER_NAME"
VDBBENCH_NAME="ds-builder.war"
VDBBENCH="$WORKSPACE/vdb-bench-sources/vdb-bench-war/target/$VDBBENCH_NAME"

VDBBUILDER_DIST="vdb-builder-jboss"

if [ ! -d $DEST ]; then
  echo "No Destination directory ... aborting"
  exit 1
fi

#
# Change directory to destination
#
cd $DEST

#
# Copy jboss to the destination
#
cp $JB_DIR/$JB .

#
# Unzip jboss in destination directory
#
unzip $JB

#
# Remove the jboss dist zip
#
rm -f $JB

if [ ! -d $JBOSS_DIST ]; then
  echo "Jboss zip extraction failed on unexpected distribution"
  exit 1
fi

#
# Copy teiid distribution
#
cp $TEIID_DIR/$TEIID $JBOSS_DIST/

#
# Change to JBOSS_DIST to allow unzipping
#
cd $JBOSS_DIST

#
# Unzip teiid dist zip
#
unzip $TEIID

#
# Remove the teiid dist zip
#
rm -f $TEIID

#
# Return to destination
#
cd $DEST

if [ ! -f "$JBOSS_DIST/standalone/configuration/standalone-teiid.xml" ]; then
  echo "Teiid zip extraction failed on jboss distribution"
  exit 1
fi

#
# Copy in the default credentials
#
cp $JB_USER_PROPS $JB_CONFIG
if [ ! -f $JB_CONFIG/$JB_USER_PROPS_NAME ]; then
  echo "Failed to copy vdb-builder teiid user credentials to jboss-dist"
  exit 1
fi

cp $JB_ROLES_PROPS $JB_CONFIG
if [ ! -f $JB_CONFIG/$JB_ROLES_PROPS_NAME ]; then
  echo "Failed to copy vdb-builder teiid roles credentials to jboss-dist"
  exit 1
fi

#
# Deploy the war files
#
cp $VDBBUILDER $JB_DEPLOYMENT/
if [ ! -f $JB_DEPLOYMENT/$VDBBUILDER_NAME ]; then
  echo "Failed to copy vdb-builder to jboss distribution"
  exit 1
fi

cp $VDBBENCH $JB_DEPLOYMENT/
if [ ! -f $JB_DEPLOYMENT/$VDBBENCH_NAME ]; then
  echo "Failed to copy vdb-bench to jboss distribution"
  exit 1
fi

#
# Rename the distribution and rezip
#
mv $JBOSS_DIST $VDBBUILDER_DIST
zip -r "$VDBBUILDER_DIST.zip" $VDBBUILDER_DIST

if [ ! -f "$VDBBUILDER_DIST.zip" ]; then
  echo "Failed to zip up vdb-builder-jboss distribution"
  exit 1
fi
