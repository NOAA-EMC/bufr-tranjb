help([[
Load environment for running bufr-tranjb on Hera
]])

local pkgName    = myModuleName()
local pkgVersion = myModuleVersion()
local pkgNameVer = myModuleFullName()

conflict(pkgName)

load("cmake/3.20.1")

prepend_path("/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")
load("hpc/1.1.0")
load("hpc-intel/18.0.5.274")

-- Load common modules for this package
load("bufrtranjb_common")

setenv("FC", "ifort")

whatis("Name: ".. pkgName)
whatis("Version: ".. pkgVersion)
whatis("Category: Application")
whatis("Description: bufr-tranjb application environment")
