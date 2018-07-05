#!/usr/bin/env bash

BUILD_LOCATION=_build
GEOGRAPHICLIB_DEST=Geographiclib

function fail_check
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

function DownloadGeographiclib()
{
    local repo=https://git.code.sf.net/p/geographiclib/code
    local rev=majic/release/1.49

	  echo "repo=$repo rev=$rev"

	  mkdir -p $BUILD_LOCATION
	  pushd $BUILD_LOCATION

	  if [ ! -d "$GEOGRAPHICLIB_DEST" ]; then
	      fail_check git clone $repo $GEOGRAPHICLIB_DEST
	  fi

	  pushd $GEOGRAPHICLIB_DEST
	  fail_check git checkout $rev
	  popd
	  popd
}

function BuildGeographiclib()
{
	  OS=$(uname -s)
	  KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

	  pushd $BUILD_LOCATION
	  pushd $GEOGRAPHICLIB_DEST

    case $OS in
        Darwin)
            CPP_FLAGS="-msse4.2 -O3"
            ;;
        *)
            CPP_FLAGS="-fPIC -msse4.2 -mpclmul -O3"
            ;;
    esac

    mkdir -p BUILD
    pushd BUILD

	  fail_check cmake ..
    fail_check cmake -D GEOGRAPHICLIB_LIB_TYPE=STATIC .
	  fail_check make

    popd
	  popd
	  popd
}

DownloadGeographiclib
BuildGeographiclib
