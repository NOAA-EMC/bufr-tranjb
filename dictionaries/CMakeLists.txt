list(APPEND dictionaries_files
  rstprod_bufr_tank.tbl
  )
install(PROGRAMS ${dictionaries_files}
        DESTINATION "dictionaries")
