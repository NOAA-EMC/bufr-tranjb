#!/bin/bash

module purge
. ../versions/build.ver
module load envvar
module load PrgEnv-intel
module load intel/${intel_ver:?}
module load bufr/${bufr_ver:?}
module load w3nco/${w3nco_ver:?}
module load craype/${craype_ver:?}
module load cray-mpich/${cray_mpich_ver:?}
module load w3emc/${w3emc_ver:?}

cd bufr_tranjb.fd
make clean
make
make install

cd ..
cd tocsbufr.fd
make clean
make

cd ..
cd bufr_cword.fd
make clean
make
make install
