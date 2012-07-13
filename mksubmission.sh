#!/bin/sh

SUB_ID="96563296"

FILENAME="icfp-${SUB_ID}.tgz"

echo "creating archive... ${FILENAME}"

cp install PACKAGES-TESTING dist/build/lifter

tar  -czf $FILENAME -C dist/build/lifter lifter install PACKAGES-TESTING


