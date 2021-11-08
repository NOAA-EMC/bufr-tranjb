list(APPEND ush_files
  bufr_cword.sh
  bufr_tranjb.sh
  )
install(PROGRAMS ${ush_files}
        DESTINATION "ush")
