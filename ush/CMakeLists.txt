list(APPEND ush_files
  build.sh
  bufr_cword.sh
  bufr_tranjb.sh
  )
install(PROGRAMS ${ush_files}
        DESTINATION "ush")
