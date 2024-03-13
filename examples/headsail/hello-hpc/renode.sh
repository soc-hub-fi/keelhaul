#!/bin/sh
set -x # echo on

BASEDIR=$(dirname "$0")
BIN=${BIN=$1}

renode --console -e "set bin @$BIN; include @$BASEDIR/../../../vp/headsail/run_hpc.resc"