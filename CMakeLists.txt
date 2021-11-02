cmake_minimum_required(VERSION 3.18)

# Get the version from the VERSION file.
file(STRINGS "VERSION" pVersion)

project(
  bufrtranjb
  VERSION ${pVersion}
  LANGUAGES Fortran)

include(GNUInstallDirs)

# Build type
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

# Build and install code.
add_subdirectory(sorc)

# Install utility scripts
add_subdirectory(ush)

# Install modulefile for this package.
add_subdirectory(modulefiles)
