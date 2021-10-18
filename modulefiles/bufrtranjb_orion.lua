help([[
Load environment for running bufr-tranjb on Orion
]])

local pkgName    = myModuleName()
local pkgVersion = myModuleVersion()
local pkgNameVer = myModuleFullName()

conflict(pkgName)

load("cmake/3.18.1")

prepend_path("/apps/contrib/NCEP/libs/hpc-stack/modulefiles/stack")
load("hpc/1.1.0")
load("hpc-intel/2018.4")

-- Load common modules for this package
load("bufrtranjb_common")

setenv("FC", "ifort")

whatis("Name: ".. pkgName)
whatis("Version: ".. pkgVersion)
whatis("Category: Application")
whatis("Description: bufr-tranjb application environment")
