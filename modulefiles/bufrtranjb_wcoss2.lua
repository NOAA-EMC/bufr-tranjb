help([[
Load environment to build bufr-tranjb on WCOSS2
]])

intel_ver=os.getenv("intel_ver") or "default"

load("envvar")
load("PrgEnv-intel")
load(pathJoin("intel", intel_ver))

-- Load common modules for this package
load("bufrtranjb_common")

whatis("Description: bufr-tranjb build environment")
