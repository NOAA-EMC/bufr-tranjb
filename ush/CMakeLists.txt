list(APPEND ush_files
  bufr_cword.sh
  bufr_tranjb.sh
  run_apxdx.sh
  )
install(PROGRAMS ${ush_files}
        DESTINATION "ush")
