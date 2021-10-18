list(APPEND fortran_src
  tocsbufr.f
)

set(exe_name tocsbufr.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  bacio::bacio_4
  w3emc::w3emc_4
  bufr::bufr_4
  sp::sp_4
  sigio::sigio)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
