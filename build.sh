#!/bin/bash

set -eux

target=${1:-${target:-"null"}}

readlink=$(which readlink)
[[ $(uname -s) == Darwin ]] && readlink=$(which greadlink)

# Location of PWD and package source directory.
pkg_root=`dirname $(${readlink} -f $0)`

target=$(echo $target | tr [:upper:] [:lower:])
if [[ "$target" =~ ^(wcoss2|hera|orion)$ ]]; then
  source $pkg_root/versions/build.ver
  if [[ ! "$target" =~ ^(wcoss2)$ ]]; then
    # WCOSS2 wants these hardwired in build.ver
    # This makes the build and porting inflexible
    # as well as this script to have if-blocks for WCOSS2
    # These variables have no meaning on any other machine
    unset intel_ver
    unset craype_ver
    unset cray_mpich_ver
  fi
  set +x
  module use $pkg_root/modulefiles
  module load bufrtranjb_$target
  module list
  set -x
  # WCOSS2 likes to call the bin/ directory exec/
  if [[ "$target" =~ ^(wcoss2)$ ]]; then
    CMAKE_OPTS+=" -DCMAKE_INSTALL_BINDIR=exec"
  fi
fi

# Determine install path.
INSTALL_PREFIX=${INSTALL_PREFIX:-$pkg_root/install}

  exit
# Create a build directory and cd into it.
[[ -d build  ]] && rm -rf build
mkdir -p build && cd build

# build and install.
cmake -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX ${CMAKE_OPTS:-} ..
make -j ${BUILD_JOBS:-2} VERBOSE=${BUILD_VERBOSE:-}
make install

exit 0
