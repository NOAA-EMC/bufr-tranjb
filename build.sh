#!/bin/bash

set -eux

INSTALL_TARGET=${INSTALL_TARGET:-"wcoss2"}
INSTALL_PREFIX=${INSTALL_PREFIX:-"../install"}

# Location of PWD and package source directory.
pkg_root=`dirname $(readlink -f $0)`

target=$(echo $INSTALL_TARGET | tr [:upper:] [:lower:])
if [[ "$target" =~ ^(wcoss2|hera|orion)$ ]]; then
  source $pkg_root/versions/build.ver
  set +x
  module purge
  module use $pkg_root/modulefiles
  module load bufrtranjb_$target
  module list
  # To use local version of bufr library, modify and uncomment next 2 lines
  #module unload bufr
  #export bufr_ROOT=/lfs/h2/emc/obsproc/noscrub/Shelley.Melchior/install/install/bufr
  set -x
fi

# Create a build directory and cd into it.
[[ -d build  ]] && rm -rf build
mkdir -p build && cd build

# build and install.
cmake -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX -DCMAKE_INSTALL_BINDIR=exec ..
make -j ${BUILD_JOBS:-6} VERBOSE=${BUILD_VERBOSE:-}
make install

exit 0
