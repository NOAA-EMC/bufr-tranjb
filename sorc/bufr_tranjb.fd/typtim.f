C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TYPTIM
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2016-12-01
C
C ABSTRACT: DETERMINES THE OUTPUT BUFR "TANK" FILENAME THIS SUBSET
C   (REPORT) WILL BE WRITTEN TO, BASED ON THE SUBSET'S MESSAGE TYPE,
C   MESSAGE SUBTYPE AND DATE.  IF THE FILENAME HAS ALREADY BEEN
C   ASSIGNED A UNIT NUMBER (FROM A PREVIOUS CALL TO THIS SUBROUTINE)
C   THEN IT SIMPLY EXITS.  OTHERWISE, THE FILE IS ASSIGNED A UNIT
C   NUMBER AND OPENED VIA THE BUFRLIB ROUTINE OPENBF (EITHER AS A
C   NEW FILE ENCODING THE BUFR TABLE INTERNAL TO THE INPUT BUFR FILE,
C   OR AS AN EXISTING FILE WITH ITS OWN INTERNAL BUFR TABLE, IN THE
C   LATTER CASE THIS SUBSET WILL LATER BE APPENDED TO THIS FILE).
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 2002-03-12  L. SAGER   -- STORES OCEANOGRAPHIC DATA (BUFR TYPE 031)
C     IN DAILY FILES (LIKE ALL OTHER BUFR TYPES) IF THE BUFR SUBTYPE IS
C     .GE. 100 SINCE FUTURE NAVOCEANO TOPEX IGDR PROCESSED DATA
C     (SUBTYPE 100 AND UP) WILL NEED TO BE LOADED INTO DAILY FILES (ALL
C     OTHER OCEANOGRAPHIC DATA SUBTYPES CONTINUE TO BE LOADED INTO
C     MONTHLY FILES)
C 2002-04-08  D. KEYSER  -- ADDED DOCBLOCKS, ADDED COMMENTS,
C     STREAMLINED; IMPROVED ALL STANDARD OUTPUT PRINT
C 2003-08-13  D. KEYSER/J. ATOR -- WHEN A NEW "TANK" IS CREATED, CHECKS
C     TO SEE IF IT QUALIFIES AS A "RESTRICTED" DATA TYPE - IF SO,
C     CHANGES GROUP TO "rstprod" AND PERMISSION TO "640"
C 2004-10-19  D. KEYSER  -- USES FULL PATH NAMES FOR DATABASE AND BUFR
C     MNEMONIC TABLE FILES IN OPEN AND INQUIRE STATEMENTS AND IN
C     ARGUMENT IN CALLS TO SUBROUTINE SYSTEM (BEFORE USED ONLY FILE
C     NAMES AND ASSUMED EXECUTING SCRIPT WAS RUNNING IN DATABASE PARENT
C     DIRECTORY); ADDED CANADIAN AMDAR (NC004009) TO LIST OF TYPES
C     WHICH QUALIFIES AS "RESTRICTED" (MEANING GROUP WILL BE CHANGED TO
C     "rstprod" AND PERMISSION WILL BE SET TO "640")
C 2007-04-20  D. KEYSER --  MODIFIED LOGIC SO THAT NEW TANKS b255/xx161
C     (MADIS SNOW FEED SNOW DATA) AND b255/xx131 (MADIS HYDRO FEED
C     DENVER URBAN DRAINAGE AND FLOOD CONTROL) DO NOT DEFAULT TO BEING
C     RESTRICTED DATA TYPES SINCE (THESE DATA ARE NOT RESTRICTED BY
C     THEIR PROVIDERS), PREVIOUSLY ALL TANKS IN b255 DIRECTORY WERE
C     UNILATERALLY SET TO RESTRICTED
C 2007-08-24 D. KEYSER -- MODIFIED TO ALLOW USER TO CONTROL SIZE OF
C     NEW OUTPUT BUFR MESSAGES (IF SOME SIZE LARGER THAN THE DEFAULT OF
C     10,000 BYTES IS DESIRED) VIA THE SCRIPT ENVIRONMENT VARIABLE
C     "MESSAGE_LENGTH" OBTAINED VIA CALL TO GETENV (MESSAGE_LENGTH
C     DEFAULTS TO -99999, ANY VALUE LESS THAN 10001 MEANS DO NOT
C     INCREASE MESSAGE LENGTH BEYOND BUFRLIB DEFAULT OF 10,000 BYTES),
C     MAXIMUM VALUE FOR MESSAGE_LENGTH IS 50000 (50,000 BYTE MESSAGES);
C     VARIABLE "CHGRP_RSTPROD" CAN NOW BE IMPORTED WITH A VALUE OF
C     "ALL" WHICH FORCES THIS CODE TO SET ANY NEWLY CREATED TANK FILES
C     TO "RESTRICTED" (I.E., CHANGES GROUP TO "rstprod" AND PERMISSION
C     TO "640") REGARDLESS OF WHETHER OR NOT THEY EXPLICITLY QUALIFY TO
C     BE "RESTRICTED" WHEN "CHGRP_RSTPROD" IS OTHERWISE IMPORTED AS
C     "YES"; ADDED AIRDAT TAMDAR (NC004010) TO LIST OF TYPES WHICH
C     QUALIFIES AS "RESTRICTED" WHEN "CHGRP_RSTPROD" IS "YES" (MEANING
C     GROUP WILL BE CHANGED TO "rstprod" AND PERMISSION WILL BE SET TO
C     "640")
C 2007-11-21 D. KEYSER -- ADDED SURFACE SHIP (NC001001) TO LIST OF
C     TYPES WHICH QUALIFIES AS "RESTRICTED" WHEN "CHGRP_RSTPROD" IS
C     "YES" (MEANING GROUP WILL BE CHANGED TO "rstprod" AND PERMISSION
C     WILL BE SET TO "640") (NEEDED BECAUSE OF SHIP CALL SIGN MASKING
C     AFTER ~ 12/1/2007)
C 2008-09-12 D. KEYSER -- ADDED MADIS TAMDAR-MESABA (PREVIOUSLY ALL
C     CARRIERS) (NC004008), MADIS TAMDAR-PENAIR (NC004012) AND MADIS
C     TAMDAR-CHAUTAUQUA (NC004013) TO LIST OF TYPES WHICH QUALIFY AS
C     "RESTRICTED" WHEN "CHGRP_RSTPROD" IS "YES" (MEANING GROUP WILL BE
C     CHANGED TO "rstprod" AND PERMISSION WILL BE SET TO "640") (NEEDED
C     BECAUSE OF ALL TAMDAR DATA FROM MADIS FEED IS NOW CONSIDERED TO
C     BE RESTRICTED)
C 2008-11-18 D. KEYSER -- ADDED SHORT- AND LONG-RANGE LIGHTNING DATA
C     FROM VAISALA VIA NOAAPORT (NC007001 AND NC007002, RESP.) TO LIST
C     OF TYPES WHICH QUALIFY AS "RESTRICTED" WHEN "CHGRP_RSTPROD" IS
C     "YES" (MEANING GROUP WILL BE CHANGED TO "rstprod" AND PERMISSION
C     WILL BE SET TO "640") (NEEDED BECAUSE OF THESE NEW DATA ARE
C     CONSIDERED TO BE RESTRICTED)
C 2010-01-29 D. KEYSER -- FORCES BUFR DICTIONARY MESSAGES TO ALWAYS BE
C     CREATED WITH EDITION NUMBER 3 VIA CALL TO "PKVS01", REGARDLESS OF
C     THE EDITION NUMBER OF THE DATA MESSAGES (WHICH MAY NOW BE 4 IN
C     SOME CASES)
C 2010-05-21 J. ATOR   -- ATTEMPTS TO REPAIR AN EXISTING TANK IF IT IS
C     FOUND TO BE CORRUPTED
C 2012-09-26 J. WOOLLEN -- INCREASED THE FILE CACHE TO MAXIMUM VALUE OF
C     31 FILES AT A TIME
C 2012-09-26 D. KEYSER -- ADDED CANADIAN RADAR (NC006080, NC006081,
C     NC006082, ... , NC006101, NC006102, NC006103 AND NC006110,
C     NC006111, NC006112, ... , NC006131, NC006132, NC006133) TO LIST
C     OF TYPES WHICH QUALIFIES AS "RESTRICTED" (MEANING GROUP WILL BE
C     CHANGED TO "rstprod" AND PERMISSION WILL BE SET TO "640") (NEEDED
C     BECAUSE OF THESE NEW DATA ARE CONSIDERED TO BE RESTRICTED)
C 2015-08-27 D. KEYSER --
C       - Replaces hardwired, obsolete horizontal structure form of ush
C     script cwordsh (/nwprod/ush/cwordsh) with imported variable
C     $CWORDush to now define the path to the ush script in the SYSTEM
C     call for the case where an incomplete BUFR message is encountered
C     at the end of the tank file (i.e., the tank is corrupted) during
C     the appending process and must be repaired.  This allows for a
C     transition to the new vertical structure form of bufr_cword.sh
C     (as in production), and provides for the use of other versions of
C     this script (e.g. in checkout).  Both $CWORDush and $CWORDX (the
C     path to the executable bufr_cword, invoked inside $CWORDush) must
C     be defined in an upstream parent script (and, in fact, they are
C     both set in bufr_tranjb.sh, which executes this program, if not
C     already set upstream of that).  The default for both $CWORDush
C     and $CWORDX, used in production, is the path to the current
C     production versions of ush script bufr_cword.sh and executable
C     bufr_cword, respectively.
C         - The bufr_cword processing invoked above now unblocks,
C     rather than blocks the file in the corrupt tank repair process
C     since, by default, BUFR files are now unblocked on WCOSS.
C         - Updated the information send to stdout and stderr via
C           this processing (more complete).
C         - Note: This repair logic, added in 2010, likely will not be
C                 invoked because the change to add C-language I/O in
C                 BUFRLIB version 10.2.0 forces corrupted BUFR messages
C                 to be skipped in the tank reading (and appending)
C                 process.  It is retained in the rare case there is
C                 still a problem coming out of the appending process.
C       - Added the following to the list of types which qualify as
C     "restricted" when "CHGRP_RSTPROD" is "YES" (meaning group will be
C     changed to "rstprod" and permission will be set to "640"):
C          NC000100 - SYNOPTIC - FIXED LAND (NATIVE BUFR) (WMO RES 40)
C          NC001101 - SURFACE MARINE SHIP, RESTRICTED (NATIVE BUFR)
C          NC004011 - KOREAN AMDAR (NATIVE BUFR)
C          NC004103 - AMDAR (NATIVE BUFR)
C 2016-04-20 D. STOKES/D. KEYSER --
C     - Updated logic to override default BUFR message length upper
C       limit for new messages.  Now, any positive value of integer 
C       variable IMESSAGE_LENGTH will trigger a call to BUFRLIB routine
C       MAXOUT. (Previously MAXOUT was only called if IMESSAGE_LENGTH 
C       was greater than 10000).
C 2016-04-28 JWhiting --
C     - Added the following to the list of types which qualify as
C       "restricted" when "CHGRP_RSTPROD" is "YES" (meaning group will 
C       be changed to "rstprod" and permission will be set to "640"):
C          NC000020 - Wind energy nacelle, restricted
C          NC002020 - Wind energy tower, restricted
C          NC012004 - Ground-based GNSS (GPS, etc.) data
C 2016-05-03 JWhiting --
C     - Removed extraneous/obsolete logic testing for specific /dcom & 
C       /dcomdev directories, allowing for more flexible developer 
C       testing.
C     - Removed reference to FLNEW variable (containing CDATE value of 
C       unknown length) in restricted tank specifications.
C 2016-05-09 JWhiting --
C     - Added the following to the list of types which qualify as
C       "restricted" when "CHGRP_RSTPROD" is "YES" (meaning group will 
C       be changed to "rstprod" and permission will be set to "640"):
C          NC021242 - Megha-Tropiques SAPHIR L1A2 brightness temps
C                     (future ingest, not currently being received)
C 2016-11-07 D. Keyser --
C       Open file with unit number 81 for output in the event remapping
C       from tank b001/xx102 or b001/xx103 to b001/xx002 may occur.
C       This allows for a (temporary) workaround in response to the
C       termination of many TAC BUFR buoy reports that had been written
C       to b001/xx002 on 11/1/16. Once we are ready to handle the BUFR-
C       feed in tanks b001/xx102 and b001/xx103 this logic can be
C       removed.
C 2016-12-01 D. Keyser --
C     - Corrected a bug introduced in 2016-11-07 change which resulted
C       in an abort in the remapping when a rejected date was found for
C       a report targeted for b001/xx102 tank, this due to the file
C       associated with remapped b001/xx002 tank (unit 81) not being
C       connected to the BUFRLIB software when it was expected to be such.
C     - Corrected a bug introduced in 2016-11-07 change which resulted
C       in some subsets remapped from tanks b001/xx102 or b001/xx103 to
C       tank b001/xx002 being written to the wrong tank date for
C       b001/xx002.
C 2019-12-04 C. Hill --
C       Open file with unit number 81 for output in the event remapping
C       from tank b002/xx101 to b002/xx001 may occur. This allows for
C       a (temporary) workaround in response to the termination of
C       CMA TAC radiosonde reports that had been written to b002/xx001
C       up until 01/15/20. Once we are ready to handle the BUFR-
C       feed in tanks b002/xx101 this logic can be removed.
C 2021-11-29 J. Ator --
C     - Added the following to the list of types which qualify as
C       "restricted" when "CHGRP_RSTPROD" is "YES" (meaning group will 
C       be changed to "rstprod" and permission will be set to "640"):
C          NC003010 - GPS radio occultation
C 2024-02-12 M. Weiss --
C     Added Common block RESTRICT containing parameters listing "all"
C     restricted BUFR tanks via the RSTPROD_BUFR_TANK.TBL file.
C     Eliminated the CHGRP_RSTPROD.EQ.'ALL' option. Also eliminated
C     the MTYP/MSBT IF block. See the "CHANGE GROUP & PERMISSION ON
C     RESTRICTED TANKS" code listing.
C 2024-03-28  M. Weiss -- 
C     Separated SUBPROGRAM TYPTIM from bufr_tranjb.f.
C---------------------------------------------------------------------
C
C USAGE:    CALL TYPTIM(MTYP,MSBT,CDATE,IERR)
C   INPUT ARGUMENT LIST:
C     MTYP     - BUFR MESSAGE TYPE FOR THIS SUBSET
C     MSBT     - BUFR MESSAGE SUBTYPE FOR THIS SUBSET
C     CDATE    - CHARACTER*8 DATE FOR THIS SUBSET (IN FORM YYYYMMDD)
C
C   OUTPUT ARGUMENT LIST:
C     IERR     - RETURN CODE (=0 - OUTPUT FILE ALREADY ASSIGNED A
C              - UNIT NUMBER AND OPENED IN A PREVIOUS CALL,
C              - =1 -  OUTPUT FILE ASSIGNED A UNIT NUMBER AND OPENED
C              -  IN THIS CALL)
C
C   OUTPUT FILES:
C     UNIT "IFBFR" - OUTPUT BUFR "TANK" FILE
C     UNIT 06      - STANDARD OUTPUT PRINT
C
C REMARKS: ENTRY CLCASH CLOSES ALL POSSIBLE OUTPUT BUFR "TANK" FILES
C     AND RESETS THE CACHE (I.E., REMOVES ALL ASSOCIATION BETWEEN
C     FORTRAN UNIT NUMBERS AND OUTPUT FILENAMES).
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE TYPTIM(MTYP,MSBT,CDATE,IERR)
 
      PARAMETER (NFBFR=31)
      PARAMETER (MXTANKS=500)

      COMMON /LUNITS/ INBFR,IFBFR,LFBFR,LFUNT,CTABLEA(50:49+NFBFR),
     $                FLBFR(NFBFR)
      COMMON /KOUNTS/ IRD(49:49+NFBFR),IWT(49:49+NFBFR),
     $                ISK(2,49:49+NFBFR),IFL(2,50:49+NFBFR),IDAT(8),
     $                IHHMM,CREJ(100,50:49+NFBFR),CBAD(100,50:49+NFBFR),
     $                ISKM(50:49+NFBFR),IFLM(50:49+NFBFR)
      COMMON /HOMEDC/ IEDTN,IMESSAGE_LENGTH,TANK_DIR,BUFR_FIX_DIR,
     $                SATINGEST_FIX_DIR,CHGRP_RSTPROD,SUBDATE_CHECK,
     $                RUN_TYPE
      COMMON /RESTRICT/ IRTANK_TOT, MTYP_RLIST (MXTANKS),
     +                  MSBT_RLIST (MXTANKS)

      CHARACTER*500 TANK_DIR,BUFR_FIX_DIR,SATINGEST_FIX_DIR
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
      common /borg_check/BORG_REMAP_xx102,BORG_REMAP_002101
      character*80 BORG_REMAP_xx102
      character*8   cdate_last_81
      character*4  BORG_REMAP_002101
      data cdate_last_81/'xxxxxxxx'/
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C^^^^^ remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]

      CHARACTER*22  FLBFR,DRNEW,FLNEW
      CHARACTER*9   RUN_TYPE
      CHARACTER*8   CDATE,CTABLEA
      CHARACTER*3   MTYP,MSBT,CHGRP_RSTPROD,SUBDATE_CHECK,
     +              MTYP_RLIST,MSBT_RLIST
      INTEGER *4    IDIR
      LOGICAL       EXIST

      IERR = 0

C  SEE WHICH TYPE FILE THIS REPORT GOES INTO AND ASSEMBLE A FILENAME
C  -----------------------------------------------------------------
       
      IF(MTYP.EQ.'031'.AND.MSBT(1:1).EQ.'0') THEN
             ! Oceanographic data (BUFR type 031) with subtype < 100
             !  written to monthly tank files
         FLNEW = CDATE(1:6) // '/b' // MTYP // '/xx' // MSBT
         DRNEW = CDATE(1:6) // '/b' // MTYP
      ELSE
             ! All other types and subtypes written to daily tank files
         FLNEW = CDATE // '/b' // MTYP // '/xx' // MSBT
         DRNEW = CDATE // '/b' // MTYP
      ENDIF
 
C  SEE IF THE FILE IS ALREADY ASSIGNED - IF SO JUST RETURN
C  -------------------------------------------------------
 
      DO I=1,NFBFR
         IF(FLNEW.EQ.FLBFR(I)) THEN
            IFBFR = LFUNT+I
cppppp
ccc   print *, '$$$ changing to unit number ',ifbfr,' - file already ',
ccc  $ 'assigned - FLNEW = ',FLNEW
cppppp
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
            if((BORG_REMAP_xx102(1:1).ne.' '.and.
     $         mtyp.eq.'001'.and.(msbt.eq.'102'.or.msbt.eq.'103'))) then

c  before returning here check for two things, either of which will
c   necessitate opening file 81 and connecting it to BUFRLIB software in
c   the event remapping from tank b001/xx102 or b001/xx103 to b001/xx002
c   may occur:
c     1) unit 81 is currently NOT connected to BUFRLIB software
c        (otherwise a BUFRLIB abort could occur next time subr. remap is
c         called)
c     2) unit 81 IS currently connected to BUFRLIB software BUT the date
c        associated with the tank file it is connected to (from the last
c        time unit 81 was opened) does not agree with the date of the
c        subset (here targeted for tank b001/xx102 or b001/xx103)
c        (otherwise a remapped subset would be written into a tank file
c         having a date not agreeing with the current subset date)
c          - in this case unit 81 must first be closed to BUFRLIB
c            software since it will be reopened below)

               call status(81,LUN,IL,IM)
               if(il.eq.0 .or. cdate.ne.cdate_last_81) then
                  if(il.eq.1) call closbf(81)
                  go to 400
               endif
            endif
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
Cvvvvv remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
            if (BORG_REMAP_002101(1:1).ne.' '.and.
     $          mtyp.eq.'002'.and.msbt.eq.'101') then
               call status(81,LUN,IL,IM)
               if(il.eq.0 .or. cdate.ne.cdate_last_81) then
                  if(il.eq.1) call closbf(81)
                  go to 400
               endif
            endif            
C^^^^^ remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
            RETURN
         ENDIF
      ENDDO

C  UPDATE THE CACHE WITH ANOTHER FILE
C  ----------------------------------
 
      LFBFR = MAX(MOD(LFBFR+1,NFBFR+1),1)
      FLBFR(LFBFR) = FLNEW
      IFBFR = LFUNT+LFBFR
      CTABLEA(IFBFR) = 'NC' // MTYP // MSBT
cppppp
ccc   print *, '$$$ changing to unit number ',ifbfr,' - updating ',
ccc  $ 'the cache  - FLNEW = ',FLNEW
cppppp
      CALL CLOSBF(IFBFR)
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
c always close unit 81 (even if empty)
      call closbf(81)
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C^^^^^ remapping b002/xx101 ---> b002/xx001 [dependancy]    [CH 11/2019]

      IERR = 1
 
C  CHECK FOR EXISTING DIRECTORY PATH, IF DIRECTORY NOT PRESENT MAKE IT
C  -------------------------------------------------------------------
 
      IF(CDATE.NE.'00000000')  THEN
         CALL SYSTEM('mkdir -p '//trim(TANK_DIR)//'/'//DRNEW)
         INQUIRE(FILE=trim(TANK_DIR)//'/'//FLNEW,EXIST=EXIST)
         IF(EXIST) THEN
 
C  OPEN EXISTING "TANK" FILE
C  -------------------------
 
            OPEN(IFBFR,FILE=trim(TANK_DIR)//'/'//FLNEW,
     $       FORM='UNFORMATTED')
            PRINT'("   OPENING   BUFR TANK  ",A,"/",A," IN UNIT",I3)',
     $       trim(TANK_DIR),trim(FLBFR(LFBFR)),IFBFR
            CALL OPENBF(IFBFR,'APX',INBFR)

C  Check to see if there was a problem in BUFRLIB routine POSAPX (e.g.,
C   a corrupt message was found at the bottom of the exsiting tank) -
C   if so, attempt to repair the tank
C  --------------------------------------------------------------------

            IF(IGETSC(IFBFR).EQ.-1) THEN
              PRINT'("    ###WARNING: BUFR TANK ",A,"/",A," CORRUPTED,",
     $          " ATTEMPT REPAIR - RUN TIME:",I3.2,"/",I2.2,"/",I4,
     $          " AT ",I4.4,"Z")', trim(TANK_DIR),trim(FLBFR(LFBFR)),
     $          IDAT(2),IDAT(3),IDAT(1),IHHMM
               CALL CLOSBF(IFBFR)
              PRINT'(//"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")'
               CALL SYSTEM('set -x; mv -f '//trim(TANK_DIR)//'/'//
     $          FLNEW//' '//trim(TANK_DIR)//'/corrupt_'//CDATE//'_'//
     $          MTYP//'_'//MSBT)
               CALL SYSTEM('set -x; $CWORDush unblk '//trim(TANK_DIR)//
     $          '/corrupt_'//CDATE//'_'//MTYP//'_'//MSBT//' '//
     $          trim(TANK_DIR)//'/'//FLNEW)
              PRINT'("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"//)'
               OPEN(IFBFR,FILE=trim(TANK_DIR)//'/'//FLNEW,
     $          FORM='UNFORMATTED')
               CALL OPENBF(IFBFR,'APX',INBFR)
               IF(IGETSC(IFBFR).EQ.0) THEN
                  PRINT'("    ----------> BUFR TANK ",A,"/",A,
     $             " REPAIRED - RUN TIME: ",I3.2,"/",I2.2,"/",I4," AT ",
     $             I4.4,"Z")',trim(TANK_DIR),trim(FLBFR(LFBFR)),IDAT(2),
     $             IDAT(3),IDAT(1),IHHMM
               ELSE
                  PRINT'("    ###WARNING: CORRUPTED BUFR TANK ",A,"/",A,
     $            " COULD NOT BE REPAIRED - RUN TIME:",I3.2,"/",I2.2,
     $            "/",I4," AT ",I4.4,"Z")', trim(TANK_DIR),
     $            trim(FLBFR(LFBFR)),IDAT(2),IDAT(3),IDAT(1),IHHMM
               ENDIF
            ENDIF
            IF(IMESSAGE_LENGTH.GT.0)  CALL MAXOUT(IMESSAGE_LENGTH)
         ELSE
 
C  OPEN NEW "TANK" FILE
C  --------------------
 
            OPEN(IFBFR,FILE=trim(TANK_DIR)//'/'//FLNEW,
     $       FORM='UNFORMATTED')
            PRINT'("   CREATING  BUFR TANK  ",A,"/",A," IN UNIT",I3)',
     $       trim(TANK_DIR),trim(FLBFR(LFBFR)),IFBFR

C  Dictionary messages in tank file are always created with edition 3
C  ------------------------------------------------------------------

            IF(IEDTN.NE.3.AND.SUBDATE_CHECK.NE.'NO')
     $       CALL PKVS01('BEN',3)
            CALL OPENBF(IFBFR,'OUT',INBFR)
            IF(IEDTN.NE.3.AND.SUBDATE_CHECK.NE.'NO')
     $       CALL PKVS01('BEN',IEDTN)
            IF(IMESSAGE_LENGTH.GT.0)  CALL MAXOUT(IMESSAGE_LENGTH)

            IF(CHGRP_RSTPROD.EQ.'YES') THEN

C  CHANGE GROUP & PERMISSION ON RESTRICTED TANKS TO rstprod & 640, RESP
C  --------------------------------------------------------------------
      
              DO IK = 1, IRTANK_TOT
                IF ( ( MTYP .EQ. MTYP_RLIST(IK) ) .AND. 
     +               ( MSBT .EQ. MSBT_RLIST(IK) ) ) THEN
                  CALL SYSTEM(
     $                    'chgrp rstprod '//trim(TANK_DIR)//'/'//FLNEW)
                  CALL SYSTEM('chmod 640 '//trim(TANK_DIR)//'/'//FLNEW)
                  PRINT'("   ---> This tank contains restricted data -",
     $            " only users in rstprod group have read permission")'
                  PRINT'("   ---> Based on call to BUFR_RESTRICT")'
                  CYCLE
                ENDIF ! MTYP & MSBT == select tanks
              ENDDO
            ENDIF ! CHGRP_RSTPROD == YES

         ENDIF ! EXIST
      ELSE  ! CDATE='00000000' means date was rejected/bad
         PRINT'("   NO BUFR TANK CREATED IN DIRECTORY ",A," DUE TO",
     $    " BAD OR REJECTED DATE -- UNIT",I3," SKIPPED OVER")',
     $    trim(TANK_DIR),IFBFR
      ENDIF
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
Cvvvvv remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
  400 continue

      if((BORG_REMAP_xx102(1:1).ne.' '.and.
     $   mtyp.eq.'001' .and. (msbt.eq.'102'.or.msbt.eq.'103')).or.
     $   (BORG_REMAP_002101(1:1).ne.' '.and.
     $   mtyp.eq.'002'.and.msbt.eq.'101')) then

c come here to open file 81 in the event remapping from tank b001/xx102
c  or b001/xx103 to b001/xx002 may occur

         if(mtyp.eq.'001'.and.(msbt.eq.'102'.or.msbt.eq.'103'))
     $   flnew = cdate // '/b001/xx002'
         if(mtyp.eq.'002'.and.msbt.eq.'101')
     $   flnew = cdate // '/b002/xx001'
C CH     drnew = cdate // '/b001'
         drnew = cdate // '/b'//mtyp
         if(cdate.ne.'00000000')  then
            call system('mkdir -p '//trim(tank_dir)//'/'//drnew)
            inquire(file=trim(tank_dir)//'/'//flnew,exist=exist)
            if(exist) then

C  open existing "tank" file in unit 81
C  ------------------------------------

               open(81,file=trim(tank_dir)//'/'//flnew,
     $          form='UNFORMATTED')
              print'("   OPENING   BUFR TANK  ",A,"/",A," IN UNIT 81")',
     $          trim(tank_dir),trim(flnew)
               call openbf(81,'APX',inbfr)

C  Check to see if there was a problem in BUFRLIB routine POSAPX (e.g.,
C   a corrupt message was found at the bottom of the exsiting tank) -
C   if so, attempt to repair the tank
C  --------------------------------------------------------------------
 
               if(igetsc(81).eq.-1) then
              print'("    ###WARNING: BUFR TANK ",A,"/",A," CORRUPTED,",
     $             " ATTEMPT REPAIR - RUN TIME:",I3.2,"/",I2.2,"/",I4,
     $             " AT ",I4.4,"Z")', trim(tank_dir),trim(flnew),
     $            idat(2),idat(3),idat(1),ihhmm
                  call closbf(81)
                  print'(//"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")'
                 if(mtyp.eq.'001'.and.(msbt.eq.'102'.or.msbt.eq.'103'))
     $            call system('set -x; mv -f '//trim(tank_dir)//'/'//
     $             flnew//' '//trim(tank_dir)//'/corrupt_'//cdate//'_'//
     $             '001_002')
                 if(mtyp.eq.'002'.and.msbt.eq.'101')
     $            call system('set -x; mv -f '//trim(tank_dir)//'/'//
     $             flnew//' '//trim(tank_dir)//'/corrupt_'//cdate//'_'//
     $             '002_001')
                 if(mtyp.eq.'001'.and.(msbt.eq.'102'.or.msbt.eq.'103'))
     $          call system('set -x; $CWORDush unblk '//trim(tank_dir)//
     $             '/corrupt_'//cdate//'_'//'001_002'//' '//
     $             trim(tank_dir)//'/'//flnew)
                 if(mtyp.eq.'002'.and.msbt.eq.'101')
     $          call system('set -x; $CWORDush unblk '//trim(tank_dir)//
     $             '/corrupt_'//cdate//'_'//'002_001'//' '//
     $             trim(tank_dir)//'/'//flnew)
                  print'("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"//)'
                  open(81,file=trim(tank_dir)//'/'//flnew,
     $             form='UNFORMATTED')
                  call openbf(81,'APX',inbfr)
                  if(igetsc(81).eq.0) then
                     print'("    ----------> BUFR TANK ",A,"/",A,
     $             " REPAIRED - RUN TIME: ",I3.2,"/",I2.2,"/",I4," AT ",
     $                I4.4,"Z")',trim(tank_dir),trim(flnew),idat(2),
     $                idat(3),idat(1),ihhmm
                  else
                  print'("    ###WARNING: CORRUPTED BUFR TANK ",A,"/",A,
     $               " COULD NOT BE REPAIRED - RUN TIME:",I3.2,"/",I2.2,
     $                "/",I4," AT ",I4.4,"Z")', trim(tank_dir),
     $                trim(flnew),idat(2),idat(3),idat(1),ihhmm
                  endif
               endif
               if(imessage_length.gt.0)  call maxout(imessage_length)
            else

C  open new "tank" file in unit 81
C  -------------------------------

               open(81,file=trim(tank_dir)//'/'//flnew,
     $          form='UNFORMATTED')
              print'("   CREATING  BUFR TANK  ",A,"/",A," IN UNIT 81")',
     $          trim(tank_dir),trim(flnew)

C  Dictionary messages in tank file are always created with edition 3
C  ------------------------------------------------------------------

               if(iedtn.ne.3.and.subdate_check.ne.'NO')
     $          call pkvs01('BEN',3)
               call openbf(81,'OUT',inbfr)
               if(iedtn.ne.3.and.subdate_check.ne.'NO')
     $          call pkvs01('BEN',iedtn)
               if(imessage_length.gt.0)  call maxout(imessage_length)

            endif ! exist
         else  ! cdate='00000000' means date was rejected/bad
            print'("   NO BUFR TANK CREATED IN DIRECTORY ",A," DUE TO",
     $       " BAD OR REJECTED DATE -- UNIT 81 SKIPPED OVER")',
     $       trim(tank_dir)
         endif
         cdate_last_81 = cdate
      endif
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)

      RETURN
 
C.......................................................................
 
C  ENTRY CLCASH CLOSES ALL POSSIBLE OUTPUT BUFR "TANK" FILES AND RESETS
C  THE CACHE (I.E., REMOVES ALL ASSOCIATION BETWEEN FORTRAN UNIT
C  NUMBERS AND OUTPUT FILENAMES)
C  --------------------------------------------------------------------
 
      ENTRY CLCASH

      DO I=1,NFBFR
         CALL CLOSBF(LFUNT+I)
      ENDDO
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
c always close unit 81 (even if empty)
      call closbf(81)
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C^^^^^ remapping b002/xx101 ---> b002/xx001 [dependency]    [CH 11/2019]

      CTABLEA(50:49+NFBFR) = ' '
      FLBFR = ' '
      LFBFR = 0
 
      RETURN
 
      END

