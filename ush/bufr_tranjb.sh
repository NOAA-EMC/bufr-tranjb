#!/bin/ksh
# Run under ksh

####  UNIX Script Documentation Block
#
# Script name:   bufr_tranjb.sh   Puts BUFR data files into BUFR database tanks
#
# RFC contact:  Nadiga    org: NP22        date: 2019-10-09
#
# Abstract: This script controls the transfer of BUFR data files into the BUFR
#   database tank files. The input BUFR file will be appended onto the
#   applicable BUFR database tank file if the BUFR database tank file already
#   exists.  Otherwise it will be written into the newly-created BUFR database
#   tank file.  The script parameters point it to the database parent directory
#   path and the BUFR input file.  Output from the transfer program BUFR_TRANJB
#   is scrolled into the top of the logfile, tranjb.out, in the database parent
#   directory path.  An errlog.out file is also located there.
#
# Script history log:
# 1996-09-06  J. Woollen  Original version for implementation.
# 1996-12-10  J. Woollen  Modified error exit so errlog file is smaller.
# 1996-12-11  J. Woollen  Added time stamp to heading with input filename.
# 1997-12-15  Bill Facey  Convert to IBM.
# 1998-05-13  Bill Facey  Add new variable "cword" that controls the
#        execution of the script defined in CWORD.  For unblocked BUFR files
#        (e.g., all decoder files and NESDIS ATOVS retrievals) cword should be
#        set to "yes" (default) in order to FORTRAN block them because this
#        platform uses BUFRLIB with FORTRAN blocked BUFR files (tanks).  If the
#        BUFR file is already FORTRAN blocked (e.g., all SHEF and NESDIS data
#        except for ATOVS retrievals) cword must be  exported as "NO" or "no". 
# 1999-02-02  Bill Facey  Modified to give individual users the options of
#        receiving a return code from the script and to turn off the time
#        screening of the incoming BUFR data.
# 2000-03-13  Bill Facey  Modified to generate a return code unless otherwise
#        specified.
# 2001-04-26  D. Keyser   Modified to accept the imported script variable
#        "EXECbufr" to set the path to the bufr_tranjb executable (if not
#        found, still defaults to "/nwprod/exec").
# 2001-02-13  D. Keyser   Modified to increase the maximum number of lines in
#        the output file /dcom/us007003/tranjb.out from 20K to 80K.  The error
#        log file /dcom/us007003/errlog.out is now limited to 80K lines and new
#        output is scrolled from the beginning of the file rather than appended
#        to the end of the file.
# 2001-06-05  D. Keyser   Modified to post a message to the joblog file prior
#        to the execution of TRANX.
# 2003-08-19  D. Keyser   Add new variable "CHGRP_RSTPROD" which when "YES"
#        (default) forces TRANX to check to see if a BUFR database file
#        qualifies as a "RESTRICTED" data type.  Also now allows the variable
#        "CWORD" to be imported with a value different than the default
#        "/nwprod/ush/cwordsh"
# 2005-01-25  D. Keyser   Modified to run in the working directory (DATA)
# 2005-11-01  D. Keyser   Removed echo of SCREEN into temporary file,
#        removed stdin of this file into TRANX since TRANX now obtains value of
#        SCREEN directly via call to GETENV.  Now writes error for zero-length
#        or non-existent file to the top of file errlog.out instead of to the
#        bottom (to be consistent with BUFR_TRANJB errors)
# 2007-08-24  D. Keyser   Add new variable "MESSAGE_LENGTH" which when imported
#        with a value of "10001" or larger forces TRANX to open all new output
#        BUFR messages with length $MESSAGE_LENGTH bytes {maximum value for
#        MESSAGE_LENGTH is 50000 (i.e., 50,000 byte messages opened}.  If
#        MESSAGE_LENGTH is not imported here, it is set to the default value of
#        "-99999".  If the value for MESSAGE_LENGTH passed into TRANX is less
#        than "10001" (e.g., the default value of "-99999"), TRANX opens all
#        new output BUFR messages with the default BUFRLIB length of 10,000
#        bytes. Also, variable "CHGRP_RSTPROD" can now be imported with a
#        value of "ALL" which forces TRANX to set all input BUFR database files
#        to "RESTRICTED" data types (regardless of whether or not they
#        explicitly qualify to be a "RESTRICTED" data type when "CHGRP_RSTPROD"
#        is imported as "YES").
# 2008-01-02  D. Keyser   Added a new section which, when new script variable
#        COPYFILES is imported as YES, attempts to copy individual files pulled
#        over from the remote unix machine to directory
#        $tank_dir/YYYYMMDD/wbufbul (obtaining YYYYMMDD from the date qualifier
#        in the remote filename), after their translation into NCEP BUFR and
#        subsequent standardization into WMO BUFR (Note: This works only for
#        remote files with date qualifiers the following forms: *.Dyyddd.S*.*,
#        *.Dyyddd.T*.*, *.Dyyyymmddhh.T*.* and *.Dyyyymmddhh.S*.*).  If script
#        variable SENDDBN is imported as YES, a DBN alert will be submitted to
#        post files to a remote machine (currently only possible for WindSat
#        files).
# 2010-03-08  D. Keyser   Modified to provide more diagnostic information about
#        the success or failure of $CWORD (if it runs here).  If $CWORD either
#        1) fails - or - 2) completes normally but is unable to block one or
#        more input BUFR messages (for whatever reason), this script will now
#        post a message indicating such to both the $log and $err files and
#        also to the production joblog file (if present). Modified to increase
#        the maximum number of lines in the output file (normally
#        /dcom/us007003/tranjb.out) from 80K to 200K.
# 2012-08-06  J. Ator     Increased MESSAGE_LENGTH maximum from 50K to 200K
# 2012-10-18  D. Keyser   Modified to run on WCOSS. Specifically, replaces
#        CCS script variables XLFUNIT_n with FORTn (where n is the unit number
#        connected to the filename defined by the variable FORTn) - needed
#        because ifort uses FORTn.  Also, cwordsh (when run) now unblocks files
#        rather blocks them.  This script is now set to run under ksh shell as
#        the default.
# 2014-01-17  D. Keyser   This script renamed from tranjb to bufr_tranjb.sh.
#        Now includes hostname as well as process id in temporary filenames
#        where only process id was present before.  $EXECobsproc_satingest
#        replaces $EXECbufr as the directory path to the executable
#        bufr_tranjb.  USH script cwordsh renamed to bufr_cword.sh and moved
#        from directory path $USHbufr to directory path
#        $USHobsproc_shared_bufr_cword.  Extra condition on test for successful
#        bufr_cword.sh run: not only must it have a rc=0, there must also be a
#        non-empty unblocked output file produced.  Changes to account for the
#        fact that there are now three choices for the directory path to the
#        bufrtab.xxx files rather than just two choices.  Added information to
#        docblock and new comments.  Updated some existing comments.  Changed
#        all "date" commands to "date -u" since WCOSS should always present
#        date in UTC.  Run times listed in output log file are now correctly
#        stamped as UTC (before these UTC times were incorrectly listed as
#        LST).
# 2015-08-27  D. Keyser
#          - Imported environment variable $CWORDush is set only if environment
#        variable $cword is imported as 'yes' (since it is not otherwise
#        needed).  Also imported environment variable $CWORDX is now set when
#        $cword is imported as 'yes' even though is is also set, with same
#        default, inside $CWORDush (needed because $CWORDX is now invoked
#        further down in this script and we want it to use the value set here,
#        if indeed it is set here, see below),
#          - The following changes are in response to a change in bufr_tranjb
#        source code to replace the hardwired, obsolete horizontal structure
#        form of ush script cwordsh (/nwprod/ush/cwordsh) with the imported
#        variable $CWORDush to now define the path to the ush script in the
#        SYSTEM call for the case where an incomplete BUFR message is
#        encountered at the end of the tank file (i.e., the tank is corrupted)
#        during the appending process and must be repaired.  This allows for a
#        transition to the new vertical structure form of bufr_cword.sh (as in
#        production), and provides for the use of other versions of this script
#        (e.g. in checkout). 
#        {Note: This repair logic, added in 2010, likely will not be invoked
#               because the change to add C-language I/O in BUFRLIB version
#               10.2.0 forces corrupted BUFR messages to be skipped in the tank
#               reading (and appending) process.  It is retained in the rare
#               case there is still a problem coming out of the appending
#               process.}
#            - If either $CWORDush or $CWORDX is not imported or not set at a
#              prior point in this script, and if imported environment variable
#              $obsproc_shared_bufr_cword_ver (production version number for
#              obsproc_shared/bufr_cword) is not set,
#              $obsproc_shared_bufr_cword_ver is set to current production
#              version in /nwprod/versions/obsproc_satingest.ver.
#            - If imported environment variable $CWORDush is not imported or
#              not set at a prior point in this script, it is set to current
#              production path to ush script bufr_cword.sh which makes use of
#              $obsproc_shared_bufr_cword_ver.
#            - If imported environment variable $CWORDX is not imported or not
#              set at a prior point in this script, it is set to current
#              production path to executable bufr_cword which makes use of
#              $obsproc_shared_bufr_cword_ver.
#          - Script Docblock is updated to account for changes above and to
#            improve informational content.
# 2015-11-05  D. Keyser   If the BUFR file being ingested here was generated
#        from a concatenation of individual BUFR files pulled from the remote
#        server, the names of these individual files that contributed to the
#        concatenation will be listed in the stdout when this script is
#        executed (applicable only to satingest run type).
# 2016-04-20  D. Keyser/D. Stokes   Updated documentation on MESSAGE_LENGTH
#        since it can now be imported with a value less than 10000.  Remove
#        upper limit on MESSAGE_LENGTH (before it was set to 200000).  If
#        MESSAGE_LENGTH is not imported, then do nothing (before it was set
#        to -99999) (let bufr_tranjb take care of the default case).
#        Also added information for new condition code 93.
# 2016-11-14  D. Keyser   Add documentation for a new variable
#        "BORG_REMAP_xx102" which, when imported as a character string
#        containing no more that 16 different 4-character bulletin originators
#        (BORG), allows selected subsets with any of these BORG's to be remapped
#        from b001/xx102 or b001/xx103 to b001/xx002 as a (temporary) workaround
#        in response to the termination of many TAC BUFR buoy reports that had
#        been written to b001/xx002 on 11/1/16. Once we are ready to handle the
#        BUFR-feed in tank b001/xx102 this variable can be removed.
# 2018-12-06  Y. Ling  Updated to run on phase 3 machine. 
# 2019-10-09  S. Nadiga Modified to shift the Y2K windowing technique that
#        converts 2-digit years to 4-digit
# 2021-09  A. Richert  Porting to WCOSS2. date2jday accessed through PATH.
#        CWORD-related variables must be sufficiently defined (no hardcoded
#        fallback path).
#         
#
# Usage: bufr_tranjb.sh  <tank_dir>  <bufrfile>
#
#   Script parameters:
#                 $1: tank_dir - root of directory path to output BUFR database
#                                tank file (e.g., "/dcom/us007003"); also path
#                                to directory containing first choice for
#                                location of BUFR mnemonic table regardless of
#                                run type
#                 $2: bufrfile - full path to the BUFR file to be ingested into
#                                the BUFR database tank file (bufrfile will be
#                                appended onto BUFR database tank file if the
#                                BUFR database tank file already exists,
#                                otherwise it will be written into the newly-
#                                created BUFR database tank file)
#
#   Modules and files referenced in this script:
#     scripts    : date2jday.sh (in $PATH, from prod_util)
#                  $CWORDush
#                  $UTILROOT/ush/postmsg
#                  $UTILROOT/ush/prep_step
#                  $UTILROOT/ush/startmsg
#     data cards : none
#     executables: $TRANX
# ##YL*****                 tocsbufr
#                  $CWORDX
#                  (set here but invoked by $CWORDush)
#                  $DBNROOT/bin/dbn_alert
#   Modules and files referenced in executable $TRANX:
#     scripts    : $CWORDush
#     executables: $CWORDX
#                  (set here but invoked by $CWORDush)
#
# Remarks:
#
#   Imported Variables that must be passed in:
#      DATA                 - path to current working directory
#
#   Imported Variables that must be passed in under certain conditions:
#      USHobsproc_shared_bufr_cword
#                           - path to obsproc_shared bufr_cword ush directory
#                             containing bufr_cword.sh
#                             (invoked only if cword=yes and $CWORDush is not
#                              imported)
#      EXECobsproc_shared_bufr_cword/bufr_cword
#                           - path to obsproc_shared bufr_cword executable
#                             directory containing bufr_cword
#                             (invoked only if cword=yes and $CWORDX is not
#                              imported)
#      DBNROOT              - root path to dbn_alert (e.g.,
#                             "/iodprod/dbnet_siphon")
#                             (invoked only if $SENDDBN is "YES")
#      utilexec             - path to utility executable directory containing
#                             tocsbufr
#                             (invoked only if $COPYFILES is "YES" and $TOCSBUFR is not imported)
#      job                  - string indicating job name
#                             (invoked only if $SENDDBN is "YES")
#
#   Imported Variables that can be passed in:
#      log                  - full path name of the file to which standard
#                             output from program BUFR_TRANJB should be
#                             directed
#                             (default = '$tank_dir/tranjb.out')
#      CWORDush             - path to ush script bufr_cword.sh
#                                for case when cword is imported as yes:
#                             (default =
#                             '$USHobsproc_shared_bufr_cword/bufr_cword.sh')
#                                otherwise:
#                             (default =
#                             '/$NWROOT/obsproc_shared/bufr_cword.${obsproc_shared_bufr_cword_ver}/ush/bufr_cword')
#      CWORDX               - path to executable bufr_cword
#                                for case when cword is imported as yes:
#                             (default =
#                             '$EXECobsproc_shared_bufr_cword/bufr_cword')
#                                otherwise:
#                             (default =
#                             '/$NWROOT/obsproc_shared/bufr_cword.${obsproc_shared_bufr_cword_ver}/exec/bufr_cword.sh')
#      SENDDBN              - if set to "YES", issue dbnet_alert
#                             (currently used only to post WindSat files to
#                             remote machine )
#                             (invoked only if imported)
#      jlogfile             - path to joblog file
#                             (invoked only if imported)
#      COPYFILES            - if set to "YES", attempts to copy input BUFR file
#                             ($bufrfile) to directory
#                             $tank_dir/<YYYYMMDD>/wbufbul (obtaining YYYYMMDD
#                             from the date qualifier in $bufrfile), but only
#                             after $bufrfile is standardized into WMO BUFR
#                             (Note: This works only for certain date
#                                    qualifier patterns in $bufrfile)
#                             (invoked only if imported)
#      FIXobsproc_satingest - path to obsproc_satingest fix directory
#                             containing second choice for location of BUFR
#                             mnemonic table for satellite ingest run type
#                             (invoked only if first choice for location
#                             of BUFR mnemonic table yields no results)
#                                - or -
#                             containing third choice for location of BUFR
#                             mnemonic table for decoder run type (invoked only
#                             if first and second choices for location of BUFR
#                             mnemonic table yield no results)
#                             {no default - if not imported, location of
#                             BUFR mnemonic table for satellite ingest run type
#                             falls to third choice, $FIXbufr (see below)}
#      FIXbufr              - path to non-obsproc fix directory, here
#                             containing second choice for location of
#                             BUFR mnemonic table for decoder run type (invoked
#                             only if first choice for location of BUFR
#                             mnemonic table yields no results)
#                                - or -
#                             containing third choice for location of BUFR
#                             mnemonic table for satellite ingest run type
#                             (invoked only if first and second choices for
#                             location of BUFR mnemonic table yield no results)
#                             {no default - if not imported, location of
#                             BUFR mnemonic table for decoder run type
#                             falls to third choice, $FIXobsproc_satingest (see
#                             above)}
#      TRANX                - path to executable bufr_tranjb
#                             (default = '$EXECobsproc_satingest/bufr_tranjb')
#      give_rc              - if set to "YES" or "yes", this script will exit
#                             with return code 1 in the event bufr_cword
#                             processing failed, or it will exit with the
#                             non-zero return code coming out of program
#                             BUFR_TRANJB in the event that it terminated
#                             abnormally; any other value for $give_rc forces
#                             this script to always exit with return code 0
#                             regardless of the status of bufr_cword processing
#                             or status of program BUFR_TRANJB
#                             {Note: this script will always exit with return
#                                    code 99 if more or less than two arguments
#                                    (script parameters) are passed into it}
#                             (default = 'YES')
#      SCREEN               - if set to "OFF" or 'off' no time screening is
#                             performed (all BUFR messages will be processed
#                             regardless of their date); otherwise, either each
#                             input subset date (when $SUBDATE_CHECK is "YES")
#                             or each input message date (when $SUBDATE_CHECK
#                             is'NO') is checked agaist the current time and
#                             if the subset/message date is either more than 10
#                             days prior to the current time or more than 12
#                             hours after the current time, the subset/message
#                             is skipped over and not written out
#                             (default = 'YES')
#      SUBDATE_CHECK        - if set to "YES", then each subset is unpacked
#                             from the current input BUFR message and its date
#                             is checked in order to ensure that all database
#                             tank files have message dates with the same year,
#                             month, day and hour as each subset within the
#                             message (in this case subsets are written back
#                             into new BUFR messages which are then appended to
#                             the BUFR database tank files); if set to "NO",
#                             then an assumption is made that all subsets in
#                             the current input BUFR message have the same
#                             year, month, day and hour as the message date
#                             itself {in this case messages are simply appended
#                             directly (as is) to the BUFR database tank files}
#                             (Note: This variable is not invoked in this
#                                    script, but it can be imported into it and
#                                    then passed into program BUFR_TRANJB which
#                                    reads it via a call to
#                                    GET_ENVIRONMENT_VARIABLE)
#                             (default = 'YES' for uncompressed BUFR messages)
#                             (default = 'NO' for compressed BUFR messages)
#      CHGRP_RSTPROD        - if set to "YES", will check to see if a newly-
#                             created BUFR database tank file qualifies as a
#                             "RESTRICTED" data type; if set to "ALL", will
#                             automatically set all newly-created BUFR database
#                             tank files to be "RESTRICTED" (a "RESTRICTED"
#                             database tank file's group is set to "rstprod"
#                             and its permission is set to "640" so that it can
#                             only be read by users in the rstprod group)
#                             (default = 'YES')
#      MXMSGL               - if set, the value will override the setting
#                             of the BUFRLIB variable of the same name,
#                             defining the maximum length of a BUFR message that
#                             can be read or written by the BUFRLIB software.
#                             (integer. only first 8 digits will be recognized)
#      MESSAGE_LENGTH       - upper limit to length (in bytes) of each new
#                             output BUFR message opened for appending to the
#                             BUFR database tank file (bufr_tranjb will use this
#                             to override BUFRLIB default maximum bufr message
#                             length if necessary); there is technically no 
#                             minimum value for MESSAGE_LENGTH but it makes
#                             sense not to set this less than 2500
#                             {default (when this is not set) is the default
#                             BUFRLIB message length upper limit}
#      MAXSS                - if set, the value will override the setting
#                             of the BUFRLIB variable of the same name,
#                             defining the max number of data values in
#                             an uncompressed bufr subset.
#                             (integer. only first 8 digits will be recognized)
#      cword                - if set to "yes", always execute $CWORDush to
#                             unblock the input BUFR file; otherwise, do not
#                             execute $CWORDush (assumes input BUFR file is
#                             already unblocked)
#                             (default = 'yes')
#vvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
#      BORG_REMAP_xx102     - if set, character string (up to 80 characters)
#                             containing no more than 16 different 4-character
#                             bulletin originators (BORG) (separated by white
#                             space) for which remapping from b001/xx102 or
#                             b001/xx103 to b001/xx002 may occur (e.g.,
#                             BORG_REMAP_xx102='LFPW LFVW KARS').  If this is
#                             not set, then no remapping from b001/xx102 or
#                             b001/xx103 to b001/xx002 will occur. This is a
#                             (temporary) workaround in response to the
#                             termination of many TAC BUFR buoy reports that had
#                             been written to b001/xx002 on 11/1/16. Once we are
#                             ready to handle the BUFR-feed in tank b001/xx102
#                             this variable can be removed.
#^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
#
#
#   Condition codes:
#     0 - no problem encountered, or $give_rc is either 'YES" or 'yes'
#   > 0 - some problem encountered
#     Specifically:   1 - this script: bufr_cword processing failed
#                         (unless $give_rc is neither 'YES' nor 'yes')
#                    93 - program BUFR_TRANJB: attempt to set MXMSGL failed
#                         (unless $give_rc is neither 'YES' nor 'yes')
#                    94 - program BUFR_TRANJB: attempt to set MAXSS failed
#                         (unless $give_rc is neither 'YES' nor 'yes')
#                    95 - program BUFR_TRANJB: a message read from input BUFR
#                         file has an invalid message type
#                         (unless $give_rc is neither 'YES' nor 'yes')
#                    96 - program BUFR_TRANJB: one or more subsets read in from
#                         input BUFR file do not have an internal date
#                         (unless $give_rc is neither 'YES' nor 'yes')
#                    97 - program BUFR_TRANJB: unable to allocate arrays
#                         (unless $give_rc is neither 'YES' nor 'yes')
#                    98 - program BUFR_TRANJB: BUFR mnemonic table cannot be
#                         located in either first ($tank_dir), second
#                         ($FIXobsproc_satingest for satingest run type,
#                         $FIXbufr for decoder run type) or third ($FIXbufr for
#                         satingest run type, $FIXobsproc_satingest for decoder
#                         run type) choice location
#                         (unless $give_rc is neither 'YES' nor 'yes')
#                    99 - this script: more or less than two arguments (script
#                         parameters) are passed into this script
#                         (regardless of value for $give_rc)
#
#
# Attributes:
#
#   Language: ksh script
#   Machine:  NCEP WCOSS
#
####
 
[ $# -ne 2 ] && { echo "$0: <tank_dir> <file>"; exit 99; }
 
set -xa

host=$(hostname -s)

tank_dir=$1
bufrfile=$2

#  unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`

[ -z "$CHGRP_RSTPROD" ] && CHGRP_RSTPROD=YES
[ -z "$SCREEN" ] && SCREEN=YES
[ -z "${give_rc}" ] && give_rc=YES
[ -z "${cword}" ] && cword=yes
RC_FLAG=` echo $give_rc | tr '[a-z]' '[A-Z]' `

tmp=$tank_dir/tranjb.out.$host.$$
TRANX=${TRANX:?}

err=$tank_dir/errlog.out
log=${log:-$tank_dir/tranjb.out}
 
#  just exit quietly if the input file doesn't exist
#  -------------------------------------------------
#  -------------------------------------------------
if [ ! -s $bufrfile ]; then
   sleep 10
   if [ ! -s $bufrfile ]; then
      if [ ! -f $bufrfile ]; then
         msg="$bufrfile does not exist"
      else
         msg="$bufrfile has zero length"
      fi
      tmp_err=$tank_dir/errlog.out.$host.$$
      echo "****************************************************************\
*****************************************************************" > $tmp_err
      echo "****************************************************************\
*****************************************************************" >> $tmp_err
      echo "`date -u`" >> $tmp_err
      echo "Input file $msg -- bufr_tranjb.sh cannot run" >> $tmp_err
      [ -s $err ] || >$err
      head -n 80000 $err | cat >> $tmp_err
      mv $tmp_err $err
      rm $bufrfile
      exit 0
   fi
fi
 
#  put a filename header at the top of the ingest report
#  -----------------------------------------------------
 
echo "*******************************************************************\
**************************************************************" >> $tmp
echo "*******************************************************************\
**************************************************************" >> $tmp
echo "-------------------------------------------------------------------\
--------------" >> $tmp
echo "`date -u +%T` UTC $bufrfile"                                              >> $tmp

if [ -s $DATA/orbitlist_fname_cat ]; then
   echo "   - this is a concatenation of files:" >> $tmp
   cat $DATA/orbitlist_fname_cat | {
   read line
   rc=$?
   while [ $rc -eq 0 ] ; do
      set -A FILEINFO $line
      echo "     $FILEINFO" >> $tmp
      read line
      rc=$?
   done }
fi

echo "-------------------------------------------------------------------\
--------------" >> $tmp
#
# check to see if we need to run bufr_cword.sh to unblock the BUFR file 
err_cword_warning=0
if [ $cword = yes ]; then
   CWORDush=${CWORDush:-$USHobsproc_shared_bufr_cword/bufr_cword.sh}
   CWORDX=${CWORDX:-$EXECobsproc_shared_bufr_cword/bufr_cword}
   $CWORDush unblk $bufrfile $tank_dir/F8_unblocked.$host.$$ | \
    tee $tank_dir/bufr_cword.out.$host.$$
   err_cword=$?
   if [ $err_cword -eq 0 -a -s $tank_dir/F8_unblocked.$host.$$ ]; then
      echo "---> File successfully unblocked by BUFR_CWORD processing and \
copied to $tank_dir/F8_unblocked.$host.$$" >> $tmp
      msg=`grep "***WARNING" $tank_dir/bufr_cword.out.$host.$$`
      err_grep=$?
      if [ $err_grep -eq 0 ]; then
         err_cword_warning=1
         echo
         echo "$msg" >> $tmp
         echo
         msg=`echo $msg, input file: $bufrfile.`
         [ -n "$jlogfile" ] && $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      fi
      rm $tank_dir/bufr_cword.out.$host.$$
   else 
      echo "###> File UNSUCCESSFULLY unblocked by BUFR_CWORD processing, NOT \
copied to $tank_dir/F8_unblocked.$host.$$" >> $tmp
      msg="***WARNING: BUFR_CWORD processing FAILED - No input BUFR messages \
were unblocked into output file - no output file created"
      msg=`echo $msg, input file: $bufrfile.`
      [ -n "$jlogfile" ] && $UTILROOT/ush/postmsg "$jlogfile" "$msg"

#  scroll the report into the log file(s) and exit
#  -----------------------------------------------
      tmp_err=$tank_dir/errlog.out.$host.$$
      cp $tmp $tmp_err
      [ -s $err ] || >$err
      head -n 80000 $err | cat >> $tmp_err
      cp $tmp_err $err
      [ -s $log ] || >$log
      head -n 200000 $log | cat >> $tmp
      mv $tmp $log
      rm $tmp_err $tank_dir/bufr_cword.out.$host.$$ 
      rm $tank_dir/F8_unblocked.$host.$$

#  test to see if return code from script is desired.
#  --------------------------------------------------
      [ $RC_FLAG = YES ] && exit 1
      exit 0
   fi
else
   cp $bufrfile $tank_dir/F8_unblocked.$host.$$
   errcp=$?
   if [ $errcp -eq 0 ]; then
      echo "---> Pre-unblocked file successfully copied to \
$tank_dir/F8_unblocked.$host.$$" >> $tmp
   else
      echo "###> Pre-unblocked file UNSUCCESSFULLY copied to \
$tank_dir/F8_unblocked.$host.$$ - no output file created" >> $tmp
   fi
fi
#  run the program BUFR_TRANJB
#  ---------------------------

pgm=`basename  $TRANX`
msg="$pgm has BEGUN"
[ -n "$jlogfile" ] && $UTILROOT/ush/postmsg "$jlogfile" "$msg"

export FORT8=$tank_dir/F8_unblocked.$host.$$

#-----------------------------------------------------------------------------
# It is possible that program BUFR_TRANJB may need to execute bufr_cword.sh
#  via a SYSTEM call, so make sure values for $CWORDush and $CWORDX are set
#  and exported here (if they have not already been imported to this script
#  or set above in this script)
#-----------------------------------------------------------------------------
if [ -z "$CWORDush" -o -z "$CWORDX" ]; then
  echo 'FATAL ERROR: CWORDush and/or CWORDX not set. Make sure obsproc_shared_bufr_cword module is loaded.' 1>&2
  exit 1
fi
#-----------------------------------------------------------------------------

$TRANX >> $tmp 2>&1
rc=$?


if [ -n "$COPYFILES" -a -n "$DATA" ]; then
   if [ $COPYFILES = YES ]; then

###############################################################################
# This section attempts to copy individual files pulled over from the remote
#  unix machine to directory $tank_dir/YYYYMMDD/wbufbul (obtaining YYYYMMDD
#  from the date qualifier in the remote filename), but only after their
#  translation into NCEP BUFR and their subsequent standardization into WMO
#  BUFR
#
# Note: This works only for remote files with date qualifiers in the following
#       form: *.Dyyddd.S*.*
#             *.Dyyddd.T*.*
#             *.Dyyyymmddhh.T*.*
#             *.Dyyyymmddhh.S*.*
###############################################################################

      iflag=0
      bufrfile_base=`basename  $bufrfile | awk -F[.]tmpout '{print $1}' | \
cut -c12-`

      msg="attempt to standardize and save this NCEP BUFR file"
      [ -n "$jlogfile" ] && $UTILROOT/ush/postmsg "$jlogfile" "$msg"

#  Convert NCEP BUFR file to "standard" WMO BUFR file
#  --------------------------------------------------
      export pgm=tocsbufr
      . $UTILROOT/ush/prep_step
      export FORT11=$tank_dir/F8_unblocked.$host.$$
      export FORT51=$tank_dir/bufrout.$host.$$
      $UTILROOT/ush/startmsg
##     $utilexec/tocsbufr << EOF
      ${TOCSBUFR:-$utilexec/tocsbufr} << EOF
 &INPUT
  BULHED="DUMMY ",KWBX="DUMM",
  NCEP2STD=.TRUE.,
  SEPARATE=.TRUE.,
 /
EOF
      err_tocsbufr=$?
      if [ $err_tocsbufr -eq 0 ]; then

#  If conversion successful, determine YYYYMMDD from date qualifier in file
#   name (to get correct path $tank_dir/YYYYMMDD/wbufbul to copy file to)
#  ------------------------------------------------------------------------

         yyyyddd=0  # initialize YYYYMMDD as ZERO
         for char in S T; do
            date_qual=\
`echo $bufrfile_base | awk -F[.]D '{print $2}' | awk -F[.]$char '{print $1}'`
            if [ "${#date_qual}" -eq '5' ]; then

#      .... come here if 5-character date qualifier in filename, make sure it
#            is numeric (thus in the form YYDDD)
#           -----------------------------------------------------------------

               if [ "$(echo $date_qual | grep "^[[:digit:]]*$")" ]; then

#      ........ make sure YY and DDD pass sanity checks
#               ---------------------------------------

                  ddd=`echo $date_qual | cut -c3-5`
                  if [ $ddd -eq 0 -o $ddd -gt 366 ];then
                     iflag=3 # day-of-year in date qualifier in invalid
                  else
                     yy=`echo $date_qual | cut -c1-2`

#     ............ convert 2-digit to 4-digit yr using Y2K windowing technique
#                   (yy=41-99, assume 1941-1999; yy=00-40, assume 2000-2040)
#                  -----------------------------------------------------------

                     if [ $yy -gt 40 ]; then
                        yyyyddd=19${yy}${ddd}
                     else
                        yyyyddd=20${yy}${ddd}
                     fi

#     ............ convert year and day-of-year to year, month and day
#                  ---------------------------------------------------
                     if [ -z $(type -P date2jday.sh) ]; then echo 'FATAL ERROR: date2jday.sh not in $PATH' ; exit 1 ; fi
                     yyyymmdd=`date2jday.sh $yyyyddd`
                     break  # have YYYYMMDD, all done
                  fi
               else
                  iflag=2 # date qualifier is not all numeric
               fi
            elif [ "${#date_qual}" -eq '10' ]; then

#      .... come here if 10-character date qualifier in filename, make sure it
#            is numeric (thus in the form YYYYMMDDHH)
#           ------------------------------------------------------------------

               if [ "$(echo $date_qual | grep "^[[:digit:]]*$")" ]; then

#      ........ make sure YYYY, MM, DD and HH pass sanity checks
#               -----------------------------------------------

                  yyyy=`echo $date_qual | cut -c1-4`
                  mm=`echo $date_qual | cut -c5-6`
                  dd=`echo $date_qual | cut -c7-8`
                  hh=`echo $date_qual | cut -c9-10`
                  if [ $yyyy -lt 1900 -o  $yyyy -gt 2100 ];then
                     iflag=3 # year invalid
                  elif [ $mm -lt 1 -o  $mm -gt 12 ];then
                     iflag=3 # month invalid
                  elif [ $dd -lt 1 -o  $dd -gt 31 ];then
                     iflag=3 # day-of-month invalid
                  elif [ $hh -lt 0 -o  $hh -gt 23 ];then
                     iflag=3 # hour invalid
                  else
                     yyyymmdd=${yyyy}${mm}${dd}${hh}
                     break  # have YYYYMMDD, all done
                  fi
               else
                  iflag=2 # date qualifier is not all numeric
               fi
            else
               iflag=1 # date qualifier is neither 5- nor 10-characters
            fi
         done

         if [ $yyyymmdd -ne 0 ]; then
            mkdir -m 775 -p $tank_dir/$yyyymmdd/wbufbul
            mv $tank_dir/bufrout.$host.$$ \
             $tank_dir/$yyyymmdd/wbufbul/$bufrfile_base
            msg="$bufrfile_base successfully standardized and saved in \
$tank_dir/$yyyymmdd/wbufbul"
            [ -n "$jlogfile" ] && $UTILROOT/ush/postmsg "$jlogfile" "$msg"
            if [ -n "$SENDDBN" ]; then
               if [ $SENDDBN = YES ]; then

#      ........ post WindSat files to remote machine via DBN alert
#               --------------------------------------------------

                  $DBNROOT/bin/dbn_alert BUFR WINDSAT $job \
                   $tank_dir/$yyyymmdd/wbufbul/$bufrfile_base
               fi
            fi
         else
            if [ $iflag -eq 1 ]; then
               msg="***WARNING: $bufrfile_base standardized but not saved \
because target directory could not be obtained (date qualifier neither 5- nor 10-char)"
            elif [ $iflag -eq 2 ]; then
               msg="***WARNING: $bufrfile_base standardized but not saved \
because target directory could not be obtained (date qualifier is not numeric)"
            elif [ $iflag -eq 3 ]; then
               msg="***WARNING: $bufrfile_base standardized but not saved \
because target directory could not be obtained (date qualifier out of range)"
            elif [ $iflag -eq 0 ]; then
               msg="***WARNING: $bufrfile_base standardized but not saved \
because target directory could not be obtained (no valid date qualifier found)"
            fi
            [ -n "$jlogfile" ] && $UTILROOT/ush/postmsg "$jlogfile" "$msg"
         fi
      else
         msg="***WARNING: standardization of NCEP BUFR file FAILED, thus it \
is not saved"
         [ -n "$jlogfile" ] && $UTILROOT/ush/postmsg "$jlogfile" "$msg"
      fi
###############################################################################
   fi
fi


#  scroll the report into the log file(s) and exit
#  -----------------------------------------------
if [ $rc -ne 0 -o $err_cword_warning -ne 0 ]; then
   tmp_err=$tank_dir/errlog.out.$host.$$
   cp $tmp $tmp_err
   [ -s $err ] || >$err
   head -n 80000 $err | cat >> $tmp_err
   cp $tmp_err $err
fi

[ -s $log ] || >$log
head -n 200000 $log | cat >> $tmp
mv $tmp $log
rm $tank_dir/F8_unblocked.$host.$$ $tmp_err

#  test to see if return code from script is desired.
#  --------------------------------------------------
[ $RC_FLAG = YES ] && exit $rc
exit 0 
