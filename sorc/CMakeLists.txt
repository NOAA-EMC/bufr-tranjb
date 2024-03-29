# Set sorc/ -wide compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check -check noarg_temp_created -check nopointer -fp-stack-check -fstack-protector-all -fpe0 -debug -ftrapuv")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -ggdb -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check")
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch -fallow-invalid-boz")
  endif()
endif()

# Find packages.
find_package(bacio REQUIRED)
find_package(w3emc REQUIRED)
find_package(bufr REQUIRED)
find_package(sp REQUIRED)
find_package(sigio REQUIRED)

if(bacio_VERSION GREATER_EQUAL 2.5.0)
  set(bacio_name bacio)
else()
  set(bacio_name bacio_4)
endif()

# Build code.
add_subdirectory(bufr_cword.fd)
add_subdirectory(bufr_tranjb.fd)
add_subdirectory(tocsbufr.fd)
