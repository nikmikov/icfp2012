#!/bin/sh

SUB_ID="96563296"

FILENAME="icfp-${SUB_ID}.tgz"

COPYFILES="install README PACKAGES-TESTING maps src icfp2012.cabal mksubmission.sh LICENSE"

echo "creating archive... ${FILENAME}"

cp -a  $COPYFILES  dist/build/lifter

tar  -czf $FILENAME -C  dist/build/lifter $COPYFILES lifter


