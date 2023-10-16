C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C 
C MAIN PROGRAM: BUFR_TRANJB
C   PRGMMR: HILL             ORG: NP22        DATE: 2019-12-17
C
C ABSTRACT: READS BUFR FILES AND APPENDS EACH REPORT IT ENCOUNTERS
C   INTO THE APPROPRIATE DATABASE FILE.  THE DATABASE FILES ARE USUALLY
C   OF THE FORM "TANK_DIR/yyyymmdd/bXXX/xxsss", WHERE TANK_DIR IS THE
C   TANK (DATABASE PARENT) DIRECTORY (READ FROM SCRIPT ENVIRONMENT
C   VARIABLE "tank_dir"), yyyymmdd IS THE YEAR-MONTH-DAY, XXX IS THE
C   BUFR MESSAGE TYPE, AND sss IS THE MESSAGE SUBTYPE.  CREATES THE
C   APPLICABLE DIRECTORIES/FILES AS NECESSARY, RELATIVE TO "TANK_DIR".
C   SINCE THE INPUT BUFR FILES NEED NOT CONTAIN INTERNAL BUFR MNEMONIC
C   TABLES NEEDED TO READ THEM, THE TABLES ARE ASSUMED TO EXIST IN
C   EXTERNAL FILES, IN ASCII FORMAT, WHICH ARE ASSUMED TO BE GROUPED BY
C   MESSAGE TYPE AND HAVE THE NAME "bufrtab.XXX", WHERE XXX IS THE
C   THREE DIGIT BUFR MESSAGE TYPE.  THE BUFR MNEMONIC TABLE FILES ARE
C   FURTHER ASSUMED TO EXIST IN ONE OF THREE DIRECTORIES: IN "TANK_DIR"
C   (FIRST CHOICE FOR ALL RUN TYPES, SEE ABOVE); IN
C   "SATINGEST_FIX_DIR", WHERE SATINGEST_FIX_DIR IS THE
C   OBSPROC_SATINGEST FIX DIRECTORY (READ FROM SCRIPT ENVIRONMENT
C   VARIABLE "FIXobsproc_satingest") (SECOND CHOICE FOR SATINGEST RUN
C   TYPE AND THIRD CHOICE FOR DECODER RUN TYPE); OR IN "BUFR_FIX_DIR",
C   WHERE BUFR_FIX_DIR IS THE BUFR FIX DIRECTORY (READ FROM SCRIPT
C   ENVIRONMENT VARIABLE "FIXbufr") (SECOND CHOICE FOR DECODER RUN TYPE
C   AND THIRD CHOICE FOR SATINGEST RUN TYPE).  IF THE MNEMONIC TABLES
C   ARE NOT PRESENT IN ONE OF THESE THREE DIRECTORIES, THIS PROGRAM
C   ABORTS.  THERE IS AN OPTION TO CHECK THE INTERNAL DATE OF EACH
C   SUBSET IN ORDER TO ASSURE THAT DATABASE FILES HAVE MESSAGE DATES,
C   ACCURATE TO THE HOUR, WHICH ARE CONSISTENT WITH THE INTERNAL DATES
C   OF EACH REPORT IN THE MESSAGE.  THE DEFAULTS ARE TO PERFORM SUBSET
C   DATE CHECKING FOR UNCOMPRESSED MESSAGES AND TO SKIP IT FOR
C   COMPRESSED MESSAGES.  FOR SUBSET DATE CHECKING, IF THERE ARE
C   MULTIPLE DATES IN THE SUBSET, THE DATE USED IN THE CHECKING IS THE
C   FIRST DATE ENCOUNTERED UNLESS A TIME SIGNIFICANCE QUALIFIER (TSIG)
C   OF 25 ("Nominal Reporting time") IS ASSOCIATED WITH A DIFFERENT 
C   DATE IN THE SUBSET.  OUTPUT BUFR DATA MESSAGES ARE
C   TANKED WITH SAME BUFR EDITION NUMBER AS INPUT MESSAGES (EITHER 3 OR
C   4), BUT DICTIONARY MESSAGES ARE ALWAYS CREATED WITH EDITION 3.
C   OUTPUT BUFR MESSAGES ENCODE THE WALL-CLOCK PROCESSING TIME
C   (YYYYMMDDHHMM) IN RESERVED BYTES (19-24 FOR BUFR EDITION 3, 23-28
C   FOR BUFR EDITION 4) IN SECTION 1.  SINCE THIS PROGRAM REFERENCES
C   FULL PATH NAMES FOR THE DATABASE AND BUFR MNEMONIC TABLE FILES IN
C   OPEN AND INQUIRE STATEMENTS, AS WELL AS IN THE ARGUMENT IN CALLS TO
C   SUBROUTINE SYSTEM, THE EXECUTING SCRIPT CAN BE RUN IN ANY USER-
C   SUPPLIED CURRENT WORKING DIRECTORY.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN -- ORIGINAL VERSION FOR IMPLEMENTATION
C 1996-12-09  J. WOOLLEN -- ADDED PROCESSING OF MULTIPLE MESSAGE TYPE
C     FILES
C 1996-12-18  J. WOOLLEN -- FIXED TABLE SYNCHRONIZING BUG WITH CACHED
C     OUTPUT FILES
C 1998-01-11  J. WOOLLEN -- ORIGINAL VERSION FOR Y2K COMPLIANCE
C 1999-02-17  J. WOOLLEN -- USE RECEIPT TIME TO STORE MESSAGE TYPE 031
C     (OCEANOGRAPHIC) DATA
C 2000-02-08  W. FACEY   -- MODIFIED TO OPEN ONE BUFR DATA TANK AT A
C     TIME INSTEAD OF CACHING UP TO NINE AT A TIME - THIS CHANGE
C     PREVENTS THE PROGRAM FROM FAILING AND CAUSING A LOSS OF DATA WHEN
C     IT OPENS A BUFR TABLE A SECOND TIME DURING A GIVEN EXECUTION
C 2000-04-07  W. FACEY   -- MODIFIED TO STORE OCEANOGRAPHIC DATA (BUFR
C     TYPE 031) BY RECEIPT TIME INSTEAD OF BY OBSERVATION TIME
C 2001-01-30  D. KEYSER  -- ADDED RUN TIME STAMP TO BAD AND REJECTED
C     DATE PRINTOUT; FIXED SUBR. OPENBT TO ASSIGN VALUE FOR BUFR TABLE
C     UNIT NUMBER (NECESSARY DUE TO BUFRLIB CHANGE SINCE LAST
C     COMPILATION OF THIS CODE); EXITS CLEANLY WHEN INPUT BUFR FILE
C     DOES NOT HAVE A PROPER FIRST MESSAGE TYPE
C 2001-02-22  J. WOOLLEN -- SPECIAL ENTRY ROUTE ADDED FOR COMPRESSED
C     DATA
C 2001-03-03  J. WOOLLEN -- REROUTED OCEANOGRAPHIC DATA INTO OBSERVED
C     TIME STORAGE
C 2002-03-12  L. SAGER   -- STORES OCEANOGRAPHIC DATA (BUFR TYPE 031)
C     IN DAILY FILES (LIKE ALL OTHER BUFR TYPES) IF THE BUFR SUBTYPE IS
C     .GE. 100 SINCE FUTURE NAVOCEANO TOPEX IGDR PROCESSED DATA
C     (SUBTYPE 100 AND UP) WILL NEED TO BE LOADED INTO DAILY FILES (ALL
C     OTHER OCEANOGRAPHIC DATA SUBTYPES CONTINUE TO BE LOADED INTO
C     MONTHLY FILES)
C 2002-04-08  D. KEYSER  -- ADDED DOCBLOCKS, ADDED COMMENTS,
C     STREAMLINED; EXPANDED STANDARD OUTPUT PRINT TO SUMMARIZE NUMBER
C     OF REPORTS READ, WRITTEN AND SKIPPED FOR EACH NEW TABLE A ENTRY
C     READ IN, AS WELL AS FOR THE TOTAL AT THE END OF ALL PROCESSING;
C     IMPROVED ALL STANDARD OUTPUT PRINT; LIMITS PRINT TO 100 FOR
C     REPORTS SKIPPED DUE TO BAD OR REJECTED DATE (WITHIN EACH TABLE A
C     ENTRY READ IN), INSTEAD A LINE SUMMARIZING THE NUMBER OF REPORTS
C     SKIPPED IS PRINTED AT THE END OF THE TABLE A ENTRY PROCESSING
C 2002-10-10  L. SAGER   -- IGNORES THE DATE SCREEN IF CERTAIN TYPES OF
C     OCEANOGRAPHIC DATA IN BUFR TYPE 031 (NAMELY ALL NON-NAVOCEANO
C     DATA WITH BUFR SUBTYPES LESS THAN 100) ARE ENCOUNTERED, THISo
C     ALLOWS STORING OF >10 DAY OLD DATA IN MONTHLY FILES
C 2003-08-13  D. KEYSER/J. ATOR -- WHEN A NEW "TANK" IS CREATED, CHECKS
C     TO SEE IF IT QUALIFIES AS A "RESTRICTED" DATA TYPE - IF SO,
C     CHANGES GROUP TO "rstprod" AND PERMISSION TO "640" (IF SCRIPT
C     ENVIRONMENT VARIABLE CHGRP_RSTPROD is YES)
C 2003-09-02  D. KEYSER  -- REPLACED CALL TO BUFRLIB ROUTINE READTJ
C     WITH CALL TO READMG SINCE THEY ARE IDENTICAL; REPLACED CALL TO
C     IN-LINE SUBROUTINE MESGBF WITH CALL TO NEW BUFRLIB ROUTINE MESGBC
C 2003-12-16  D. KEYSER  -- TESTS FOR THE EXISTENCE OF THE BUFR
C     MNEMONIC TABLE IN THE DATABASE PARENT DIRECTORY, IF NOT FOUND
C     PROGRAM NOW STOPS WITH COND. CODE 99
C 2004-06-29  D. KEYSER  -- NOW TESTS A BUFR MESSAGE FOR COMPRESSION
C     EACH TIME A NEW TABLE A ENTRY MESSAGE IS READ FROM THE INPUT BUFR
C     FILE AND ASSUMES ALL SUBSEQUENT MESSAGES READ WITH THIS SAME
C     TABLE A ENTRY ARE THE SAME W.R.T. COMPRESSION, PRIOR TO THIS ONLY
C     TESTED THE FIRST REPORT DATA MESSAGE IN THE BUFR FILE FOR
C     COMPRESSION AND ASSUMED ALL SUBSEQUENT MESSAGES READ WERE THE
C     SAME W.R.T. COMPRESSION REGARDLESS OF THEIR TABLE A ENTRY - THIS
C     DID NOT ALLOW FOR THE CASE OF A BUFR FILE CONTAINING A MIXUTRE OF
C     TABLE A ENTRIES, SOME WITH COMPRESSED MESSAGES AND OTHERS WITH
C     UNCOMPRESSED MESSAGES, AS COULD HAPPEN WHEN THIS PROGRAM IS
C     PROCESSING A BUFR FILE SENT FROM THE DECODER SYSTEM - THIS COULD
C     LEAD TO THE IMPROPER STORAGE OF MESSAGES IN THE DATABASE FILE; AS
C     A RESULT OF NOTED CHANGE THE BUFRLIB ROUTINE MESGBC WAS MODIFIED
C     TO ADD THE OPTION TO CHECK THE COMPRESSION OF BUFR MESSAGES
C     CURRENTLY OPEN IN MEMORY - UNTIL THIS UPDATED ROUTINE IS ADDED TO
C     THE BUFRLIB IT IS TEMPORARY ADDED AS AN IN-LINE SUBROUTINE TO
C     THIS PROGRAM
C 2004-10-19  D. KEYSER  -- USES FULL PATH NAMES FOR DATABASE AND BUFR
C     MNEMONIC TABLE FILES IN OPEN AND INQUIRE STATEMENTS AND IN
C     ARGUMENT IN CALLS TO SUBROUTINE SYSTEM (BEFORE USED ONLY FILE
C     NAMES AND ASSUMED EXECUTING SCRIPT WAS RUNNING IN DATABASE PARENT
C     DIRECTORY); NOW LOOKS FOR BUFR MNEMONIC TABLE (FILE) IN EITHER
C     PARENT DATABASE DIRECTORY (FIRST CHOICE) OR IN FIXED FIELD
C     DIRECTORY SPECIFIED BY EXECUTING SCRIPT (SECOND CHOICE) (BEFORE
C     ONLY CHOICE WAS IN PARENT DATABASE DIRECTORY), THIS IS IN
C     CONJUNCTION WITH THE OPERATIONAL TRANSITION OF THE TABLES FROM
C     THE DATABASE PARENT DIRECTORY /dcom/us007003 TO THE PRODUCTION
C     FIXED FIELD DIRECTORY /nwprod/fix ; REMOVED IN-LINE SUBROUTINE
C     MESGBC WHICH WAS TEMPORARILY ADDED UNTIL ITS UPDATED FORM (WHICH
C     WAS IN-LINED HERE) WAS ADDED TO THE OPERATIONAL BUFRLIB, THIS HAS
C     NOW BEEN DONE
C 2005-11-01  D. KEYSER  -- SWITCH TO TIME SCREEN INPUT MESSAGES/
C     SUBSETS IS NOW OBTAINED DIRECTLY FROM SCRIPT ENVIRONMENT VARIABLE
C     "SCREEN" IN PARENT tranjb SCRIPT VIA CALL TO GETENV RATHER THAN
C     BEING READ FROM STANDARD INPUT; NOW LOOKS FOR SCRIPT ENVIRONMENT
C     VARIABLE "SUBDATE_CHECK" VIA CALL TO GETENV, IF "YES" WILL NOW
C     UNPACK EVERY SUBSET AND CHECK ITS DATE TO ENSURE THAT EACH SUBSET
C     IS WRITTEN TO A BUFR MESSAGE {LATER APPENDED TO THE OUTPUT
C     DATABASE ("TANK") FILE} WHICH HAS THE SAME YEAR, MONTH, DAY AND
C     HOUR AS THE SUBSET (BEFORE THIS WAS ALWAYS DONE FOR UNCOMPRESSED
C     MESSAGES), IF "NO" SUBSETS ARE NOT UNPACKED AND EACH INPUT
C     MESSAGE IS SIMPLY APPENDED DIRECTLY TO THE OUTPUT DATABASE
C     ("TANK") FILE (BEFORE THIS WAS ALWAYS DONE FOR COMPRESSED
C     MESSAGES), THE DEFAULTS (WHEN "SUBDATE_CHECK" IS NOT FOUND) ARE
C     TO STILL UNPACK/DATE-CHECK EACH SUBSET FOR UNCOMPRESSED MESSAGES
C     AND TO STILL DO A DIRECT MESSAGE COPY (WITHOUT UNPACKING SUBSETS)
C     FOR COMPRESSED MESSAGES; FOR NEXRAD LEVEL 2 RADAR TYPES (I.E.,
C     ANY MESSAGE TYPE NC006xxx, WHERE xxx IS GREATER THAN 002) AND ALL
C     MESONET TYPES (NC255xxx), WILL ONLY PRINT COMPRESSION INDICATOR
C     ONCE PER FILE RATHER THAN EVERY TIME A NEW MESSAGE TYPE IS READ
C     IN FROM THE SAME FILE (AS IS DONE FOR ALL OTHER TYPES), THIS
C     REDUCES PRINTOUT SINCE THESE TYPES ARE INTERMINGLED IN THEIR
C     INPUT FILES AND THERE ARE NORMALLY A LOT OF MESSAGES); ADDED
C     CANADIAN AMDAR (NC004009) TO LIST OF TYPES WHICH QUALIFIES AS
C     "RESTRICTED" (MEANING GROUP WILL BE CHANGED TO "rstprod" AND
C     PERMISSION WILL BE SET TO "640")
C 2005-11-01  S. BENDER -- MODIFIED TO ALLOW PROCESSING OF COMPRESSED
C     BUFR MESSAGES WHEN INPUT SUBSETS CONTAIN MORE THAN ONE YYYYMMDD,
C     OUTPUT MESSAGES ARE CLOSED WHEN YYYYMMDD CHANGES (SO THAT FILE
C     IS DISCONNECTED FROM SOFTWARE) BECAUSE THE BUFRLIB CAN ONLY
C     HANDLE ONE OUTPUT COMPRESSED BUFR FILE AT A TIME
C 2007-04-20  D. KEYSER -- MODIFIED TO INCLUDE REPORT LAT AND LON IN
C     DIAGNOSTIC PRINT OF REPORTS WITH REJECTED OR BAD DATE; MODIFIED
C     LOGIC SO THAT NEW TANKS b255/xx161 (MADIS SNOW FEED SNOW DATA)
C     AND b255/xx131 (MADIS HYDRO FEED DENVER URBAN DRAINAGE AND FLOOD
C     CONTROL) DO NOT DEFAULT TO BEING RESTRICTED DATA TYPES SINCE
C     (THESE DATA ARE NOT RESTRICTED BY THEIR PROVIDERS), PREVIOUSLY
C     ALL TANKS IN b255 DIRECTORY WERE UNILATERALLY SET TO RESTRICTED;
C     FOR SUBSET DATE CHECKING, IF THERE ARE MULTIPLE DATES IN THE
C     SUBSET, THE DATE USED IN THE CHECKING IS NO LONGER HARDWIRED TO
C     BE THE FIRST DATE ENCOUNTERED, RATHER A CHECK IS MADE TO SEE IF
C     ANY OF THE DATES HAVE A DATA SIGNIFICANCE QUALIFIER (DATSIG) OF
C     3, IF SO THEN THIS DATE IS SELECTED SINCE IT IS DEFINED AS THE
C     BALLOON LAUNCH POINT DATE, IF NOT THEN, AS BEFORE, THE FIRST DATE
C     ENCOUNTERD IS USED FOR CHECKING (THIS CURRENTLY APPLIES ONLY TO
C     RRS DATA IN MESSAGE TYPE NC002019 SINCE THE FIRST DATE IN THE
C     SUBTYPE IS THE BALLOON MANUFACTURE DATE WHILE THE SECOND DATE IS
C     THE BALLOON LAUNCH DATE)
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
C 2007-10-30 D. KEYSER -- FOR CASES WHERE SUBSET DATE CHECKING IS
C     PERFORMED (SUBDATE_CHECK=YES), MODIFIED TO HANDLE INPUT BUFR
C     SUBSETS THAT ENCODE DAY-OF-YEAR (MNEMONIC "DOYR") IN PLACE OF
C     USUAL MONTH AND DAY-OF-MONTH (MNEMONICS "MNTH" AND "DAYS") BY
C     CONVERTING FORMER TO LATTER (VIA NEW SUBROUTINE REMTDY) IN ORDER
C     TO OBTAIN BUFR MESSAGE SECTION 1 DATE (YYYYMMDDHH) ASSOCIATED
C     WITH THIS SUBSET IN OUTPUT DATABASE FILE (SUBSETS ARE STILL
C     ENCODED INTO DATABASE FILE WITH "DOYR" IN PLACE OF "MNTH" AND
C    "DAYS" IF THAT IS HOW THEY APPEAR IN INPUT FILE)
C 2007-11-21 D. KEYSER -- ADDED SURFACE SHIP (NC001001) TO LIST OF
C     TYPES WHICH QUALIFIES AS "RESTRICTED" WHEN "CHGRP_RSTPROD" IS
C     "YES" (MEANING GROUP WILL BE CHANGED TO "rstprod" AND PERMISSION
C     WILL BE SET TO "640") (NEEDED BECAUSE OF SHIP CALL SIGN MASKING
C     AFTER ~ 12/1/2007)
C 2008-03-06 D. KEYSER -- FOR SUBDATE_CHECK=NO, CORRECTED BUG WHICH LED
C     TO ARRAY OVERFLOW PROBLEM FOR CASES OF MESSAGES WITH REJECTED
C     DATES (OUTSIDE OF TIME SCREEN) WHEN MORE THAN 100 SUBSETS ARE IN
C     SUCH MESSAGES (NOW CORRECTLY LIMITS PRINT TO FIRST 100 MESSAGES
C     SKIPPED DUE TO DATE REJECTION, AND PRINTS ACTUAL REJECTED DATE
C     RATHER THAN "0000000000")
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
C     CONSIDERED TO BE RESTRICTED); ADDED TEMPORARY IN-LINE VERSION OF
C     BUFRLIB ROUTINE MAXOUT WHICH HAS BEEN MODIFIED TO NO LONGER
C     PRINT THE RECORD LENGTH CHANGE DIAGNOSTIC IF THE REQUESTED RECORD
C     LENGTH PASSED IN AS MAX0 IS ACTUALLY THE SAME AS THE PREVIOUS
C     RECORD LENGTH (THIS WILL BE REMOVED WHEN THEN NEXT UPDATE TO THE
C     BUFRLIB IS IMPLEMENTED SINCE IT WILL CONTAIN THIS UPDATED VERSION
C     OF MAXOUT)
C 2010-05-21 D. KEYSER -- CHECKS THE EDITION NUMBER OF EACH INPUT BUFR
C     MESSAGE (VIA CALL TO "IUPVS01") AND IN TURN OUTPUTS BUFR MESSAGES
C     WITH THE SAME EDITION NUMBER VIA CALL TO "PKVS01", PRIOR TO THIS
C     ALL OUTPUT TANK BUFR MESSAGES WERE EDITION 3 (STILL THE DEFAULT),
C     SOME BUFR MESSAGES ARE NOW COMING IN WITH EDITION 4 AND THIS
C     CHANGE WILL PRESERVE THIS IN THE OUTPUT BUFR TANKS (NOTE: THE
C     DICTIONARY MESSAGES WILL ALWAYS BE CREATED WITH EDITION NUMBER 3,
C     REGARDLESS OF THE EDITION NUMBER OF THE DATA MESSAGES); CALLS NEW
C     BUFRLIB ROUTINE "STRCPT" TO ENCODE THE WALL-CLOCK PROCESSING TIME
C     (YYYYMMDDHHMM) INTO RESERVED BYTES (19-24 FOR BUFR EDITION 3,
C     23-28 FOR BUFR EDITION 4) IN SECTION 1 OF EACH OUTPUT TANK
C     MESSAGE; REMOVED TEMPORARY IN-LINE VERSION OF BUFRLIB ROUTINE
C     "MAXOUT" ADDED IN 2008-11-18 UPDATE SINCE THIS VERSION OF MAXOUT
C     (MODIFIED TO NO LONGER PRINT THE RECORD LENGTH CHANGE DIAGNOSTIC
C     IF THE REQUESTED RECORD LENGTH PASSED IN AS MAX0 IS ACTUALLY THE
C     SAME AS THE PREVIOUS RECORD LENGTH) IS NOW IN THE BUFRLIB
C 2010-05-21 J. ATOR   -- ATTEMPTS TO REPAIR AN EXISTING TANK IF IT IS
C     FOUND TO BE CORRUPTED
C 2012-09-26 J. WOOLLEN -- ADDED LOGIC TO READ TRANJB INPUT FILE INTO 
C     MEMORY, AND READ THE NON-DICTIONARY MESSAGES SORTED BY MESSAGE
C     TYPE TO MINIMIZE TABLE CHANGES AND FILE MANIPULATIONS. ALSO
C     INCREASED THE FILE CACHE TO MAXIMUM VALUE OF 31 FILES AT A TIME.
C     IN ADDITION RECOMPILED THE PROGRAM INTO 4-BYTE WORD LENGTH AND TO
C     LINK TO LATEST VERSION OF BUFRLIB. THESE CHANGES CUMULATIVELY
C     RESULT IN A FASTER RUN TIME. IF A TRANJB INPUT FILE IS TOO LARGE
C     TO BE COMPLETELY HELD IN MEMORY, THEN SORTING IS BYPASSED AND THE
C     FILE IS READ SEQEUNTIALLY FROM DISK, AS BEFORE.
C 2012-09-26 D. KEYSER -- ADDED CANADIAN RADAR (NC006080, NC006081,
C     NC006082, ... , NC006101, NC006102, NC006103 AND NC006110,
C     NC006111, NC006112, ... , NC006131, NC006132, NC006133) TO LIST
C     OF TYPES WHICH QUALIFIES AS "RESTRICTED" (MEANING GROUP WILL BE
C     CHANGED TO "rstprod" AND PERMISSION WILL BE SET TO "640") (NEEDED
C     BECAUSE OF THESE NEW DATA ARE CONSIDERED TO BE RESTRICTED). THE
C     MAXIMUM VALUE FOR MESSAGE_LENGTH IS INCREASED FROM 50000 TO
C     200000 (200,000 BYTE MESSAGES) (READ IN FROM TRANJB USH SCRIPT).
C 2012-10-03 J. WOOLLEN/D. KEYSER -- CHANGES TO RUN ON WCOSS.
C 2014-01-21 D. KEYSER -- NOW LOOKS FOR SCRIPT ENVIRONMENT VARIABLE
C     "RUN_TYPE" VIA CALL TO GET_ENVIRONMENT_VARIABLE.  IT IS EITHER
C     'decoder' (DEFAULT) or 'satingest'.  NOW THREE CHOICES FOR
C     LOCATING BUFR MNEMONIC TABLE FILE bufrtab.XXX WHICH DEPEND UPON
C     VALUE OF "RUN_TYPE", RATHER THAN THE TWO FIXED CHOICES BEFORE.
C     FIRST CHOICE REMAINS TANK DIRECTORY (tank_dir) REGARDLESS OF RUN
C     TYPE.  SECOND CHOICE IS OBSPROC_SATINGEST FIX DIRECTORY
C     (FIXobsproc_satingest) FOR SATINGEST RUN TYPE AND BUFR FIX
C     DIRECTORY (FIXbufr).  THIRD CHOICE IS FIXbufr FOR SATINGEST RUN
C     TYPE AND FIXobsproc_satingest FOR DECODER RUN TYPE. THIS CHANGE
C     WILL ALLOW DECODER RUNS TO USE THIS VERSION OF BUFR_TRANJB SINCE
C     THE DECODER RUNS WILL USE THE BUFR MNEMONIC TABLES IN ITS
C     PREFERRED LOCATION of FIXbufr. THE SATINGEST RUNS WILL, FOR NOW,
C     USE THE BUFR MNEMONIC TABLES IN THEIR THIRD CHOICE LOCATION
C     (FIXbufr), BUT WILL AUTOMATICALLY TRANSITION TO THE SECOND CHOICE
C     FIXobsproc_satingest ONCE THE BUFR MNEMONIC TABLES ARE AVAILABLE
C     THERE.
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
C 2015-11-10 D. STOKES -- 
C     - Modified msg counting loop to continue past bad msgs, ensuring
C     sufficient array allocation.  
C     - Allow user to control setting of BUFRLIB var MAXSS (max number
C     of data values in an uncompressed bufr subset).  A default value 
C     of 200000 is now used if MAXSS is not set by user.
C     - Use new bufrlib function IGETPRM to check MAXMSG setting. That
C     value is then used to determine in advance whether or not to
C     attempt to store messages in memory, potentially avoiding the 
C     need to read the input data twice.
C 2016-04-20 D. STOKES/D. KEYSER -- 
C     - Updated logic to override default BUFR message length upper
C       limit for new messages.  Script environment variable
C       "MESSAGE_LENGTH" now tested for "unset" in this code, rather
C       than in parent bufr_tranjb.sh script.  (In this case, the
C       default BUFRLIB output message length limit is still used.)
C       There is no longer a maximum of 200000 set on the value of
C       "MESSAGE_LENGTH" imported into code.  Instead, if the imported 
C       value exceeds the current BUFRLIB maximum message length, it
C       will override that setting (and an informational message is 
C       sent to stdout).  There is no longer a minimum of 10000 set
C       on the value of "MESSAGE_LENGTH" imported into code.  It still
C       makes sense not to import values less than, say, 2500.
C     - Allow user to control setting of BUFRLIB var MXMSGL (maximum
C       length in bytes of a BUFR message that can be read or written).
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
C 2016-09-21 D. STOKES -- 
C     - Updated the logic used to establish a subset's representative
C       time.  Removed use of DATSIG as a qualifier since the RRS data
C       for which the old logic was designed was never implemented and
C       said logic caused failures when processing a new data sequence
C       which includes DATSIG values unrelated to time.  Instead apply
C       a similar check on TSIG (time significance), which is a useful 
C       qualifier for some data sequences currently processed.  For now,
C       only use TSIG=25 ("Nominal reporting time") to select one time
C       over any others available in the subset.  If no such qualifier
C       is found, a follow-up call to bufrlib routine UFBINT is invoked
C       to extract just the first set of date/time values which are then
C       assumed to be the best representative of the subset time.
C 2016-11-07 D. Keyser -- 
C       Added logic to examine subsets being written into tank
C       b001/xx102 and remap some into tank b001/xx002. This allows for
C       a (temporary) workaround in response to the termination of many
C       TAC BUFR buoy reports that had been written to b001/xx002 on
C       11/1/16. Once we are ready to handle the BUFR-feed in tank
C       b001/xx102 this logic can be removed.
C         - Added new script environment variable BORG_REMAP_xx102 to
C           list no more than 16 different bulletin originators (BORG)
C           for which the above remapping may occur. If this is not
C           set, then no remapping from b001/xx102 to b001/xx002 will
C           occur.
C 2016-11-10 D. Stokes -- 
C       Extended the remapping process described above to include 
C       TAO-type buoys (ATLAS/TRITON/PIRATA) extracted from b001/xx103.
C       This included remapping of current profiles from these buoys.
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
C 2017-01-01 D. Stokes -- 
C     - Increased string length for array CBAD.
C     - Added i/o status checks for internal writes to CBAD and CREJ.
C 2018-09-11 Y. Ling --
C     - Added a call to CLOSMG to close the output message before trying
C       to COPYMG.
C 2019-12-04 C. Hill --
C     - Added logic to examine subsets being written into tank
C       b002/xx101 and remap some into tank b002/xx001. This allows for
C       a (temporary) workaround in response to the termination of CMA
C       TAC radiosonde reports that had been written to b002/xx001 up 
C       until 01/15/20. Once we are ready to handle the BUFR-feed in tank
C       b002/xx101 this logic can be removed.
C
C 2023-06-07 S. Stegall --
C       Added logic to cycle to next report when ONE OR MORE SUBSETS READ DO NOT HAVE AN INTERNAL DATE
C       SKIPPING TO NEXT REPORT and print an error message.



C USAGE:
C   INPUT FILES:
C     UNIT 08  - INPUT BUFR FILE (OUTPUT FROM NCO/SIB-DEVELOPED DECODER
C                PROGRAM OR EMC-DEVELOPED SATELLITE INGEST PROGRAM)
C     UNIT 20  - EXTERNAL BUFR MNEMONIC TABLE FOR APPROPRIATE MESSAGE
C                TYPE {EITHER TANK_DIR/bufrtab.XXX (FIRST CHOICE FOR
C                ALL RUN TYPES), SATINGEST_FIX_DIR/bufrtab.XXX (SECOND
C                CHOICE FOR SATINGEST RUN TYPE AND THIRD CHOICE FOR
C                DECODER RUN TYPE), OR BUFR_FIX_DIR/bufrtab.XXX (SECOND
C                CHOICE FOR DECODER RUN TYPE AND THIRD CHOICE FOR
C                SATINGEST RUN TYPE); WHERE TANK_DIR IS THE ROOT OF THE
C                DIRECTORY PATH TO THE BUFR DATABASE TANK FILE (E.G.,
C                /dcom/us007003) (READ FROM SCRIPT ENVIRONMENT VARIABLE
C                "tank_dir"), SATINGEST_FIX_DIR IS THE
C                OBSPROC_SATINGEST FIX DIRECTORY (READ FROM SCRIPT
C                ENVIRONMENT VARIABLE "FIXobsproc_satingest"),
C                BUFR_FIX_DIR IS THE BUFR FIX DIRECTORY (READ FROM
C                SCRIPT ENVIRONMENT VARIABLE "FIXbufr"), XXX IS MESSAGE
C                TYPE - THIS WILL BE CONNECTED TO DIFFERENT FILENAMES
C                IN THE PATH DEPENDING UPON THE MESSAGE TYPE}
C
C   OUTPUT FILES:
C     UNIT 50  - OUTPUT BUFR "TANK" FILES {NOTE: CURRENTLY ONLY ONE
C      thru      BUFR TANK CAN BE OPEN AT A TIME, BUT FOR EACH OPENING
C     UNIT 80    OF A NEW MESSAGE TYPE (AND THUS A NEW BUFR MNEMONIC
C                TABLE), EACH FILE WITH A UNIQUE DATE AND/OR SUBTYPE
C                OPENS A NEW OUPUT FILE - THE FIRST FILE OPENED IS
C                ALWAYS UNIT 50 AND EACH NEW FILE IS INCREMENTED
C                FROM THERE)}
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C     unit 81  - output bufr "tank" file containing data remapped from
C                b001/xx102 or b001/xx103 to b001/xx002
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - TYPTIM   CLCASH   OPENBT   REMTDY
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
c                  remap
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C     SYSTEM:    - GET_ENVIRONMENT_VARIABLE   SYSTEM
C     LIBRARY:
C       W3NCO    - W3UTCDAT W3MOVDAT ERREXIT  W3DOXDAT W3FS26
C       W3EMC    - ORDERS
C       BUFRLIB  - OPENBF   DATELEN  COPYMG   READMG   UFBREP
C                  UFBINT   OPENMB   UFBCPY   WRITSB   CLOSBF
C                  IREADSB  NMSUB    MESGBC   MESGBF   WRITCP
C                  CLOSMG   MAXOUT   STRCPT   IUPVS01  PKVS01
C                  IGETSC   UFBMEX   READMM   COBFL    CRBMG
C                  CCBFL    IBFMS    ISETPRM  IGETPRM  SETBMISS
C                  GETBMISS
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
c                  parstr   status
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL RUN, SPECIFICALLY:
C                   93 - ATTEMPT TO SET MXMSGL FAILED
C                   94 - ATTEMPT TO SET MAXSS FAILED
C                   95 - A MESSAGE READ FROM INPUT BUFR FILE HAS AN
C                        INVALID MESSAGE TYPE
C                   96 - ONE OR MORE SUBSETS READ IN FROM INPUT BUFR
C                        FILE DO NOT HAVE AN INTERNAL DATE
C                   97 - UNABLE TO ALLOCATE ARRAYS
C                   98 - BUFR MNEMONIC TABLE CANNOT BE LOCATED IN
C                        EITHER FIRST ($tank_dir), SECOND
C                        ($FIXobsproc_satingest FOR SATINGEST RUN TYPE,
C                        $FIXbufr FOR DECODER RUN TYPE) OR THIRD
C                        ($FIXbufr FOR SATINGEST RUN TYPE,
C                        $FIXobsproc_satingest FOR DECODER RUN TYPE)
C                        CHOICE LOCATION
C
C REMARKS: THESE SCRIPT ENVIRONMENT VARIABLES ARE READ IN:
C            RUN_TYPE      - If = 'satingest' then this program is
C                              being executed by a satellite ingest
C                              job developed by NCEP/EMC
C                            If = 'decoder' then this program is being
C                              executed by a decoder job developed by
C                              NCEP/NCO/SIB (default)
C                            Note: Right now, this switch is only used
C                                  to determine the order of choices 2
C                                  and 3 for locating the external BUFR
C                                  mnemonic table for the appropriate
C                                  message type)
C            SUBDATE_CHECK - If = 'YES' then each subset is unpacked
C                              from the current input BUFR message and
C                              its date (defined as either the first
C                              date in the subset or the date
C                              associated with the balloon launch point
C                              for RRS data types) is checked in order
C                              to ensure that all database tank files
C                              have Section 1 message dates with the
C                              same year, month, day and hour as each
C                              subset within the message (Section 1
C                              message dates are stored up to the
C                              nearest hour, subset dates are stored up
C                              to the nearest second); in this case
C                              subsets are written back into new BUFR
C                              messages which are then appended to the
C                              BUFR database tank files
C                                  -- this is the default for all
C                                     uncompressed BUFR messages
C                            If = 'NO' then an assumption is made that
C                              all subsets in the current input BUFR
C                              message have the same year, month, day
C                              and hour as the Section 1 message date
C                              itself; in this case messages are simply
C                              appended directly (as is) to the BUFR
C                              database tank files
C                                  -- this is the default for all
C                                     compressed BUFR messages
C                            The second option allows this program to
C                              run much faster but presents a risk if
C                              the assumption is not true
C                            Note: The program does not allow for
C                                  toggling back and forth between
C                                  SUBDATE_CHECK = 'YES' and
C                                  SUBDATE_CHECK = 'NO' for a single
C                                  run of this program, it will remain
C                                  whatever it is initally read in as
C                                  {exception, if it is read in as
C                                  all blanks (i.e., not set in parent
C                                  script) then it will follow the
C                                  default values based on whether
C                                  the messages are compressed or
C                                  uncompressed (see above)}
C            SCREEN        - If = 'OFF' or 'off' no time screening is
C                               performed
C                            Otherwise time screening is performed
C                              (default)
C                              if SUBDATE_CHECK = 'YES' then each input
C                                subset date  (defined as either the
C                                first date in the subset or the date
C                                associated with the balloon launch
C                                point for RRS data types) is compared
C                                to the current wall-clock date
C                              if SUBDATE_CHECK = 'NO' then the Sec. 1
C                                date of each input BUFR message is
C                                compared to the current wall-clock
C                                date
C                              in either case, if the subset/message
C                              date is either more than 10 days prior
C                              to the current wall-clock date or more
C                              than 12 hours after the current wall-
C                              clock date, the subset/message is
C                              skipped over and not written out
C            TANK_DIR      - The root of the directory path to the BUFR
C                            database tank file {up to, but not
C                            including, the date sub-directory (e.g.,
C                            "/dcom/us007003")}
C                          - This directory path is the first choice
C                            for locating the external BUFR mnemonic
C                            tables for all run types (see
C                            SATINGEST_FIX_DIR for the second choice
C                            directory path for satingest run type and
C                            third choice directory path for decoder
C                            run type, and see BUFR_FIX_DIR for the
C                            second choice directory path for decoder
C                            run type and third choice directory path
C                            for satingest run type)
C            SATINGEST_FIX_DIR
C                          - The path to the OBSPROC_SATINGSET fix
C                            directory (e.g.,
C                            /nwprod/obsproc_satingest.v2.0.0/fix)
C                          - This directory path is the second choice
C                            for locating the external BUFR mnemonic
C                            tables for satingest run type and the
C                            third choice for locating the external
C                            BUFR mnemonic tables for decoder run type
C                            (see TANK_DIR for the first choice
C                            directory path for all run types, and see
C                            BUFR_FIX_DIR for the second choice
C                            directory path for decoder run type and
C                            the third choice directory path for
C                            satingest run type)
C            BUFR_FIX_DIR  - The path to the BUFR fix directory (e.g.,
C                            /nwprod/fix)
C                          - This directory path is the second choice
C                            for locating the external BUFR mnemonic
C                            tables for decoder run type and the third
C                            choice for locating the external BUFR
C                            mnemonic tables for satingest run type
C                            (see TANK_DIR for the first choice
C                            directory path for all run types, and see
C                            SATINGEST_FIX_DIR for the second choice
C                            directory path for satingest run type and
C                            the third choice directory path for
C                            decoder run type)
C            CHGRP_RSTPROD - If = 'YES' will check to see if a newly-
C                             created BUFR database tank file qualifies
C                             as a "RESTRICTED" data type
C                            If = "ALL" will automatically set all
C                             newly-created BUFR database tank files to
C                             be "RESTRICTED"
C                            A "RESTRICTED" database tank file's group
C                             is set to "rstprod" and its permission is
C                             set to "640" so that can only be read by
C                             users in the rstprod group
C            MXMSGL        - If set, will override the setting of the
C                            bufrlib variable of the same name defining
C                            maximum length (in bytes) of a BUFR message
C                            stored for input or output.
C                            {Requires Dynamic Allocation BUFRLIB build.
C                            May later be overriden itself if imported
C                            MESSAGE_LENGTH (below) exceeds MXMSGL}
C            MESSAGE_LENGTH- Upper limit to length (in bytes) of each
C                            new output BUFR message opened for
C                            appending to the BUFR database tank file
C                            {will be used to override the current
C                            BUFRLIB maximum message length (MXMSGL) if
C                            MESSAGE_LENGTH value exceeds MXMSGL. DA
C                            build of BUFRLIB is required in this case};
C                            there is technically no minimum value for
C                            MESSAGE_LENGTH but it makes sense not to
C                            set this less than 2500
C                            {default (when "unset" in parent script
C                            bufr_tranjb.sh) is the current BUFRLIB
C                            message length upper limit}
C            MAXSS         - If set, the value will override the setting
C                            of the bufrlib variable of the same name,
C                            defining the max number of data values in 
C                            an uncompressed bufr subset.  (Requires DA
C                            BUFRLIB build)
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C            BORG_REMAP_xx102
C                          - Character string (up to 80 characters)
C                            containing no more than 16 different 4-
C                            character bulletin originators (BORG)
C                            (separated by white space) for which
C                            remapping from b001/xx102 or b001/xx103 to
C                            b001/xx002 will occur. If this is not set,
C                            then no remapping from b001/xx102
C                            or b001/xx103 to b001/xx002 will occur.
C                            e.g., BORG_REMAP_xx102='LFPW LFVW KARS'
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
Cvvvvv remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
C            BORG_REMAP_002101
C                          - String variable of four characters,
C                            representing singular BORG from which
C                            reports populating b002/xx101 require
C                            temporary mapping into b002/xx001,
C                            in lieu of downstream processing that
C                            remains in development.
C^^^^^ remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      PROGRAM BUFR_TRANJB
 
C  Parameter NFBFR is maximum # of BUFR "tank" files that can be opened
C  --------------------------------------------------------------------

      PARAMETER (NFBFR=31)

      COMMON /LUNITS/ INBFR,IFBFR,LFBFR,LFUNT,CTABLEA(50:49+NFBFR),
     $                FLBFR(NFBFR)
      COMMON /KOUNTS/ IRD(49:49+NFBFR),IWT(49:49+NFBFR),
     $                ISK(2,49:49+NFBFR),IFL(2,50:49+NFBFR),IDAT(8),
     $                IHHMM,CREJ(100,50:49+NFBFR),CBAD(100,50:49+NFBFR),
     $                ISKM(50:49+NFBFR),IFLM(50:49+NFBFR)
      COMMON /HOMEDC/ IEDTN,IMESSAGE_LENGTH,TANK_DIR,BUFR_FIX_DIR,
     $                SATINGEST_FIX_DIR,CHGRP_RSTPROD,SUBDATE_CHECK,
     $                RUN_TYPE

      CHARACTER*1,ALLOCATABLE::BMG(:)
      INTEGER,ALLOCATABLE::ISORT(:),MINDX(:),MESG(:)

      CHARACTER*500 TANK_DIR,BUFR_FIX_DIR,SATINGEST_FIX_DIR,FILENAME
      CHARACTER*132 CREJ
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
      real*8       buyt_8
      character*80 BORG_REMAP_xx102
      character*4  borgs(16),BORG_REMAP_002101,borgc
      common /borg_check/BORG_REMAP_xx102,BORG_REMAP_002101
      data ifirst/0/
      DATA BORG_REMAP_002101/'BABJ'/
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C^^^^^ remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
      CHARACTER*132 CBAD
      CHARACTER*22  FLBFR
      CHARACTER*9   RUN_TYPE
      CHARACTER*8   TABLEA,CDATE,CTABLEA,TABLEA_last,MESSAGE_LENGTH
      CHARACTER*8   MXMSGL,MAXSS
      CHARACTER*3   MTYP,MSBT,SUBDATE_CHECK,SUBDATE_CHECK_orig,SCREEN,
     $              CHGRP_RSTPROD
      REAL*8        DATES_8(6,20),ALALO_8(2),DOYR_8,GETBMISS
      REAL          RINC(5)
      INTEGER       JDAT(8),KDAT(8),LDAT(8)
      LOGICAL       L31_0xx,PRINT_006,PRINT_255,SCRN,PRINT_IT
      LOGICAL       MEMOK
      DATA IEDTN_PREV/99/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      
ccccccCALL W3TAGB('BUFR_TRANJB',2021,0337,1200,'NP22')

      CALL GET_ENVIRONMENT_VARIABLE('SUBDATE_CHECK',SUBDATE_CHECK)
      SUBDATE_CHECK_orig = SUBDATE_CHECK
      CALL GET_ENVIRONMENT_VARIABLE('SCREEN',SCREEN)
      CALL GET_ENVIRONMENT_VARIABLE('tank_dir',TANK_DIR)
      CALL GET_ENVIRONMENT_VARIABLE('FIXbufr',BUFR_FIX_DIR)
      CALL GET_ENVIRONMENT_VARIABLE('FIXobsproc_satingest',
     $ SATINGEST_FIX_DIR)
      CALL GET_ENVIRONMENT_VARIABLE('CHGRP_RSTPROD',CHGRP_RSTPROD)
      CALL GET_ENVIRONMENT_VARIABLE('MXMSGL',MXMSGL)
      CALL GET_ENVIRONMENT_VARIABLE('MESSAGE_LENGTH',MESSAGE_LENGTH)
      CALL GET_ENVIRONMENT_VARIABLE('RUN_TYPE',RUN_TYPE)
      CALL GET_ENVIRONMENT_VARIABLE('MAXSS',MAXSS)

      IF(RUN_TYPE(1:1).EQ.' ')  RUN_TYPE = 'decoder'  ! default

      PRINT'(/" ==> Welcome to BUFR_TRANJB -- Version 12-03-2021 -- "
     $ "RUN TYPE IS ",A,/)', trim(RUN_TYPE)


C  See if we want to override the default bufrlib maximum message length
      IF(MXMSGL.NE.'        ') THEN
         READ(MXMSGL,'(I8)') IMXMSGL
         IRET=ISETPRM('MXMSGL',IMXMSGL)
         IF(IRET.EQ.0)  THEN
            PRINT'(/"MAXIMUM LENGTH (IN BYTES) OF A BUFR MESSAGE ",
     $      "(MXMSGL) SET TO ",I0/)',IMXMSGL
            IMXMSGL = IMXMSGL
         ELSE
            PRINT'(/25("*"),"ABORT",25("*")/"ATTEMPT TO SET MXMSGL ",
     $      "FAILED -- STOP 93"/25("*"),"ABORT",25("*")/)'
cccccccc       CALL W3TAGE('BUFR_TRANJB')
               CALL ERREXIT(93)
         ENDIF
      ELSE
C       Get current bufrlib maximum message length
        IMXMSGL = IGETPRM('MXMSGL')
cppppp
ccc     print *, 'BUFRLIB default maximum message length is ',IMXMSGL
cppppp
      ENDIF
      IF(MESSAGE_LENGTH.NE.'        ') THEN
         READ(MESSAGE_LENGTH,'(I8)') IMESSAGE_LENGTH
cppppp
ccc      print *,'Imported upper limit for length of new bufr message',
ccc  $    ' = ', IMESSAGE_LENGTH
cppppp
         IF(IMESSAGE_LENGTH.GT.IMXMSGL)THEN
C Override current bufrlib maximum message length
            IRET=ISETPRM('MXMSGL',IMESSAGE_LENGTH)
            IF(IRET.EQ.0)  THEN
               PRINT'(/"MAXIMUM LENGTH (IN BYTES) OF A BUFR MESSAGE ",
     $         "(MXMSGL) SET TO ",I0/)',IMESSAGE_LENGTH
               IMXMSGL = IMESSAGE_LENGTH
            ELSE
               PRINT'(/25("*"),"ABORT",25("*")/"ATTEMPT TO SET MXMSGL ",
     $         "FAILED -- STOP 93"/25("*"),"ABORT",25("*")/)'
cccccccc       CALL W3TAGE('BUFR_TRANJB')
               CALL ERREXIT(93)
            ENDIF
         ENDIF
      ELSE
         IMESSAGE_LENGTH = -99999  ! sets flag that default BUFRLIB
                                   ! value for message lg should be used
cppppp
ccc      print *, 'Message length upper limit is NOT imported into ',
ccc  $    'code, so it gets default BUFRLIB value and IMESSAGE_LENGTH ',
ccc  $    'is set to ',IMESSAGE_LENGTH,' as a flag for this situation'
cppppp
      ENDIF

C-----------------------------------------------------------------------
C     IF SCREEN = OFF/off, DATA IS NOT TIME SCREENED
C-----------------------------------------------------------------------
      SCRN = (SCREEN.NE.'OFF' .AND. SCREEN.NE.'off')

C  INITIALIZE SEVERAL VARIABLES
C  ----------------------------

      RINC  = 0.0
      IEDTN = 99
      INBFR = 8   ! Unit number of input BUFR file (always 8)
      IFBFR = 0   ! Unit # assigned to current BUFR "tank" (50 to
                  !  49+NFBFR)
      LFBFR = 0   ! Relative position of current "tank" file (1-NFBFR)
      LFUNT =49   ! Baseline for output BUFR "tank" files' unit numbers
      FLBFR = ' ' ! Filenames associated with BUFR "tank" files (NFBFR)
 
      IRD  = 0
      IWT  = 0
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
      call get_environment_variable('BORG_REMAP_xx102',BORG_REMAP_xx102)
      ird81 = 0
      iwt81 = 0
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
      ISK  = 0
      ISKM = 0
      IFL  = 0
      IFLM = 0

      NSUBSETSRD = 0
      IDATE_PREV = 0000000000

      CREJ    = ' '
      CBAD    = ' '
      CTABLEA = ' '
      PRINT_006 = .TRUE.
      PRINT_255 = .TRUE.

C  GET THE RUNDATE AND TRANSLATE WINDOW IF DOING TIME SCREENING
C  ------------------------------------------------------------
 
      CALL W3UTCDAT(IDAT)
      IHHMM = (IDAT(5) * 100) + IDAT(6)

      PRINT'(" RUN TIME: ",I2.2,"/",I2.2,"/",I4.4," AT ",I4.4,"Z"/)',
     $ IDAT(2),IDAT(3),IDAT(1),IHHMM

      IF(SCRN) THEN
         RINC(1) = -10.0   ! Limiting earliest time is 10 full days ago
         CALL W3MOVDAT(RINC,IDAT,JDAT)
         RINC = 0.0
         RINC(2) = 12.0    ! Limiting latest time is 12 hours from now
         CALL W3MOVDAT(RINC,IDAT,KDAT)
         JDATE = JDAT(1)*1000000 + JDAT(2)*10000 + JDAT(3)*100 + JDAT(5)
         KDATE = KDAT(1)*1000000 + KDAT(2)*10000 + KDAT(3)*100 + KDAT(5)
         PRINT'("ACCEPT BETWEEN ",I10," AND ",I10)', JDATE,KDATE
      ELSE
         PRINT'("NO TIME SCREENING - ACCEPT ALL DATA")'
      ENDIF

      IF(SUBDATE_CHECK_orig.EQ.'YES')  THEN
         IF(SCRN) THEN
            PRINT'(/"EACH SUBSET WILL BE UNPACKED AND DATE-CHECKED FOR",
     $      " BOTH TIME SCREENING AND OUTPUT MESSAGE DATE CONSISTENCY")'
         ELSE
            PRINT'(/"EACH SUBSET WILL BE UNPACKED AND DATE-CHECKED FOR",
     $      " OUTPUT MESSAGE DATE CONSISTENCY")'
         ENDIF
      ELSE  IF(SUBDATE_CHECK_orig.EQ.'NO')  THEN
         IF(SCRN) THEN
            PRINT'(/"EACH MESSAGE WILL BE DATE-CHECKED FOR TIME ",
     $       "SCREENING BUT SUBSETS NOT UNPACKED (ASSUME THEIR DATES ",
     $       "ARE CONSISTENT WITH MESSAGE DATE)")'
         ELSE
            PRINT'(/"SUBSETS WILL NOT BE UNPACKED (ASSUME THEIR DATES ",
     $       "ARE CONSISTENT WITH MESSAGE DATE)")'
         ENDIF
      ENDIF

      IF(MAXSS(1:1).EQ.' ')THEN
        IMAXSS=200000
      ELSE
        READ(MAXSS,'(I8)') IMAXSS
      ENDIF
      IRET=ISETPRM('MAXSS',IMAXSS)
      IF(IRET.EQ.0)  THEN
        PRINT'(/" MAXIMUM NUMBER OF DATA VALUES IN AN UNCOMPRESSED",
     $    " BUFR SUBSET (MAXSS) SET TO ",I0)',IMAXSS
      ELSE
         PRINT'(/25("*"),"ABORT",25("*")/"ATTEMPT TO SET MAXSS FAILED ",
     $    " -- STOP 94"/25("*"),"ABORT",25("*")/)'
ccccccccCALL W3TAGE('BUFR_TRANJB')
        CALL ERREXIT(94)
      ENDIF

      CALL DATELEN(10)

C  GET THE MESSAGE TYPE OF THE FIRST REPORT DATA BUFR MESSAGE IN THE
C   INPUT FILE, FIND THE EXTERNAL BUFR MNEMONIC TABLE ASSOCIATED WITH
C   IT, AND OPEN THE INPUT BUFR FILE USING THIS EXTERNAL BUFR TABLE
C  ------------------------------------------------------------------

      CALL MESGBF(INBFR,MSGTYP)
      CALL OPENBT(LUNDX,MSGTYP)
 
C  COUNT MESSAGES IN THE INPUT FILE AND ALLOCATE SPACE IN LOCAL MEMORY
C  -------------------------------------------------------------------

      KMSG=0
      KMSGbad=0
      REWIND(INBFR)
      MXMB=1000000
      ALLOCATE(BMG(MXMB),STAT=I)
      IF(I.NE.0) GOTO 901
      INQUIRE(INBFR,NAME=FILENAME)
ccccc PRINT *, FILENAME
      CALL COBFL(FILENAME,'r')

      CALL SETBMISS(10E8_8)
      print'(1X)'
      print'(" BUFRLIB value for missing is: ",G0)', GETBMISS()
      print'(1X)'

    1 CONTINUE

      CALL CRBMG(BMG,MXMB,NMB,IRET)
      IF(IRET.NE.-1)  THEN  !  Expecting "-1" for end-of-file
        KMSG = KMSG + 1
        IF(IRET.NE.0)THEN
          KMSGbad = KMSGbad + 1
          PRINT'(/"    *** WARNING: CRBMG had return status ",I0,
     $      " for msg number ",I0/)',iret,kmsg
        ENDIF
        GO TO 1
      ENDIF

      CALL CCBFL
      DEALLOCATE(BMG)

      PRINT'(/"Input file contains ",I0," messages")',KMSG
      IF(KMSGbad.gt.0)THEN
        PRINT'("   **** ", I0," of the messages are bad")',KMSGbad
      ENDIF

C  CHECK NUMBER OF BUFR MSGS VS NUMBER THAT CAN BE STORED IN MEMORY
      IMAXMSG=IGETPRM('MAXMSG')
      MEMOK = (KMSG.LE.IMAXMSG)

      IF(MEMOK)THEN
        ALLOCATE(ISORT(KMSG),MINDX(KMSG),MESG(KMSG+1),STAT=I)
        IF(I.NE.0) GOTO 901

C  READ THE FILE INTO MEMORY AND SORT THE LIST OF MESSAGE TYPES FOUND 
C  ------------------------------------------------------------------
        PRINT'(/"Read file into memory and sort list of message types"/
     $    )'
        CALL UFBMEX(INBFR,LUNDX,0,NMSG,MESG)
        CALL ORDERS(1,ISORT,MESG,MINDX,NMSG,1,4,0) 
      ELSE

C  FILE IS TOO BIG FOR BUFRLIB MEMORY.  OPEN IT FOR SEQUENTIAL READING
C  -------------------------------------------------------------------
         PRINT'(/" +++++++++++++++++++++WARNING+++++++++++++++++++++++",
     $    /" BUFR_TRANJB: FILE TOO BIG.  FOREGO SORTING AND RE-OPEN",
     $    " IT FOR SEQUENTIAL READ-IN TO ALLOW FOR COMPLETE READ"/,
     $    " +++++++++++++++++++++WARNING+++++++++++++++++++++++")'
         CALL CLOSBF(INBFR)
         CALL OPENBF(INBFR,'IN',LUNDX)
         DEALLOCATE(ISORT,MINDX,MESG,STAT=I)
      ENDIF
 
C  READ THROUGH ALL OF THE MESSAGES IN THE INPUT BUFR FILE (IF THE
C   MESSAGE TYPE CHANGES, SUBR. OPENBT WILL BE CALLED INTERNALLY IN
C   THE BUFRLIB)
C  ----------------------------------------------------------------
 
      TABLEA_last='XXXXXXXX'
      DO N=1,KMSG

         IF(MEMOK) THEN   

C  FOR FILES READ INTO MEMORY, READ MESSAGES IN SORTED ORDER FROM
C   INTERNAL MEMORY
C  --------------------------------------------------------------

            IF(N.GT.NMSG) EXIT ! exit from loop when # of msgs in memory reached 
            IF(MESG(MINDX(N)).EQ.11) CYCLE ! ignore dictionary messages
            IMSG = MINDX(N)
            CALL READMM(IMSG,TABLEA,MDATE,IRET) 
         ELSE             

C  OTHERWISE, READ MESSAGES IN SEQUENTIALLY FROM DISK (ORIGINAL ORDER)
C  -------------------------------------------------------------------

            CALL READMG(INBFR,TABLEA,MDATE,IRET)
ccccdakccc  IF(IRET.NE.0) CYCLE
            IF(IRET.NE.0) EXIT
         ENDIF

         IF(TABLEA.NE.TABLEA_last) THEN

C  IF THIS MESSAGE CONTAINS A NEW TABLE A ENTRY, DETERMINE IF MESSAGES
C   OF THIS TYPE/SUBTYPE ARE COMPRESSED
C  -------------------------------------------------------------------

            JNBFR=-INBFR  ! negative unit number passed to MESGBC
                          !  examines current message in memory

            CALL MESGBC(JNBFR,MSGTYP,ICOMP)

            IF(MSGTYP.LT.000.OR.MSGTYP.GT.255) THEN
               PRINT'(/"NEW TABLE A MESSAGE ",A," READ FROM INPUT BUFR",
     $          " FILE"//25("*"),"ABORT",25("*")/3X,"FIRST MESSAGE ",
     $          "WITH THIS TABLE A ENTRY HAS AN INVALID MESSAGE TYPE ",
     $          "(= ",I3.3,") -- STOP 95"/25("*"),"ABORT",25("*")/)',
     $          TABLEA,MSGTYP
cccccccccccccccCALL W3TAGE('BUFR_TRANJB')
               CALL ERREXIT(95)
            ENDIF

            IF(ICOMP.EQ.1) THEN
               PRINT'(/"NEW TABLE A MESSAGE ",A," READ FROM INPUT BUFR",
     $          " FILE - FIRST MESSAGE IS ** COMPRESSED ** (ASSUME ALL",
     $          " WITH THIS TABLE A ENTRY ARE)")', TABLEA
               IF(SUBDATE_CHECK_orig.EQ.'   ')  THEN
                  SUBDATE_CHECK = 'NO'
                  IF(SCRN) THEN
                     PRINT'("EACH MESSAGE WILL BE DATE-CHECKED FOR ",
     $                "TIME SCREENING BUT SUBSETS NOT UNPACKED (ASSUME",
     $               " THEIR DATES ARE CONSISTENT WITH MESSAGE DATE)"/)'
                  ELSE
                     PRINT'("SUBSETS WILL NOT BE UNPACKED (ASSUME ",
     $                "THEIR DATES ARE CONSISTENT WITH MESSAGE DATE)"/)'
                  ENDIF
               ELSE
                  PRINT *
               ENDIF
            ELSE
               PRINT_IT = .TRUE.
               IF(TABLEA(1:5).EQ.'NC006'.AND.TABLEA(6:8).GT.'002') THEN
                  PRINT_IT = .FALSE.
                  IF(PRINT_006) THEN
                     PRINT'(/"INPUT BUFR FILE CONSISTS OF INTERMINGLED",
     $                " NEXRAD LVL II OR CANADIAN RADAR TABLE A ",
     $                "MESSAGES (NC006xxx, WHERE xxx > 002) - 1ST ",
     $                "MESSAGE"/"   IS UNCOMPRESSED (ASSUME ALL WITH ",
     $                "THESE TABLE A ENTRIES ARE)")'
                     PRINT_006 = .FALSE.
                     PRINT_IT = .TRUE.
                  ENDIF
               ELSE IF(TABLEA(1:5).EQ.'NC255') THEN
                  PRINT_IT = .FALSE.
                  IF(PRINT_255) THEN
                     PRINT'(/"INPUT BUFR FILE CONSISTS OF INTERMINGLED",
     $                " TABLE A MESSAGES NC255xxx - FIRST MESSAGE IS ",
     $                "UNCOMPRESSED"/"   (ASSUME ALL WITH THESE TABLE ",
     $                "A ENTRIES ARE)")'
                     PRINT_255 = .FALSE.
                     PRINT_IT = .TRUE.
                  ENDIF
               ELSE
                  PRINT'(/"NEW TABLE A MESSAGE ",A," READ FROM INPUT ",
     $             "BUFR FILE - FIRST MESSAGE IS UNCOMPRESSED (ASSUME ",
     $             "ALL WITH THIS TABLE A ENTRY ARE)")', TABLEA
               ENDIF
               IF(SUBDATE_CHECK_orig.EQ.'   ')  THEN
                  SUBDATE_CHECK = 'YES'
                  IF(PRINT_IT)  THEN
                     IF(SCRN) THEN
                        PRINT'("EACH SUBSET WILL BE UNPACKED AND DATE-",
     $                   "CHECKED FOR BOTH TIME SCREENING AND OUTPUT ",
     $                   "MESSAGE DATE CONSISTENCY"/)'
                     ELSE
                        PRINT'("EACH SUBSET WILL BE UNPACKED AND ",
     $                   "DATE-CHECKED FOR OUTPUT MESSAGE DATE ",
     $                   "CONSISTENCY"/)'
                     ENDIF
                  ENDIF
               ELSE
                  IF(PRINT_IT) PRINT *
               ENDIF
            ENDIF

            TABLEA_last = TABLEA

         ENDIF

         MTYP = TABLEA(3:5)
         MSBT = TABLEA(6:8)

         L31_0xx = (MTYP.EQ.'031'.AND.MSBT(1:1).EQ.'0')

C  Check the edition number of this input BUFR message - we want to
C   encode all subsets in this message into output messages with the
C   same edition number
C  -----------------------------------------------------------------

         IEDTN = IUPVS01(INBFR,'BEN')
         IF(IEDTN.NE.IEDTN_PREV)  PRINT'(/" ALL BUFR MESSAGES FROM ",
     $    "THIS POINT FORWARD WILL BE WRITTEN OUT USING EDITION ",I2/)',
     $     IEDTN
         IEDTN_PREV = IEDTN

         IF(SUBDATE_CHECK.EQ.'NO') THEN

C######################################################################
C   COME HERE WHEN ASSUMPTION IS MADE THAT ALL SUBSETS IN INPUT BUFR
C     MESSAGE HAVE SAME YEAR, MONTH, DAY AND HOUR AS MESSAGE ITSELF
C              THESE MESSAGES ARE COPIED DIRECTLY INTO "TANK"
C######################################################################
 
            ISKIP = 0

C  ONLY SEC. 1 (MESSAGE) DATE CAN BE TIME SCREENED IN THIS CASE
C  ------------------------------------------------------------

            IF(SCRN) THEN
               IF((MDATE.LE.JDATE.OR.MDATE.GE.KDATE) .AND.
     $          .NOT.L31_0xx) THEN
                  ISKIP = 1
                  MDATE_rej = MDATE
                  MDATE = 0
               ENDIF
            ELSE  IF(L31_0xx)  THEN

C  Do not do screen oceanographic data in BUFR type 031, subtype < 100
C  -------------------------------------------------------------------

               PRINT'("BUFR MESSAGE CONTAINS OCEANOGRAPHIC DATA - ",
     $          "NO TIME SCREENING - ACCEPT ALL DATA")'
            ENDIF

C  IDENTIFY THE OUTPUT "TANK" FILE AND OPEN IF NECESSARY
C  -----------------------------------------------------
 
            WRITE(CDATE,'(I8.8)') MDATE/100
            CALL TYPTIM(MTYP,MSBT,CDATE,IERR)

            IRD(49)    = IRD(49)    + NMSUB(INBFR)
            IRD(IFBFR) = IRD(IFBFR) + NMSUB(INBFR)
 
            IF(ISKIP.EQ.1) THEN
               ISK(1,49)    = ISK(1,49)    + NMSUB(INBFR)
               ISK(1,IFBFR) = ISK(1,IFBFR) + NMSUB(INBFR)
               ISKM(IFBFR)  = ISKM(IFBFR) + 1
               IF(ISKM(IFBFR).LE.100)  THEN

                  WRITE(CREJ(ISKM(IFBFR),IFBFR),'("REJECTED DATE: ",
     $             I10.10," ALL REPORTS IN TABLE A ENTRY: ",A8,
     $             " SKIPPED - RUN TIME: ",I3.2,"/",I2.2,"/",I4," AT ",
     $             I4.4,"Z")',iostat=ios)
     $             MDATE_rej,TABLEA,IDAT(2),IDAT(3),IDAT(1),IHHMM
                   if(ios.ne.0)then
                     print*,'minor warning: iostat=',ios,
     $                                        ' writing to crej'
                   endif
               ELSE
                  IFLM(IFBFR) = 1
               ENDIF
            ELSE
 
C  COPY THE MESSAGE FROM THE INPUT BUFR FILE TO THE OUTPUT "TANK" FILE
C  -------------------------------------------------------------------
 
               LDAT = 0

C  Encode current wall-clock processing time into Section 1
C  --------------------------------------------------------

               CALL W3UTCDAT(LDAT)
               CALL STRCPT('Y',LDAT(1),LDAT(2),LDAT(3),LDAT(5),LDAT(6))
               CALL CLOSMG(IFBFR)
               CALL COPYMG(INBFR,IFBFR)
               IWT(49) = IWT(49) + NMSUB(INBFR)
               IWT(IFBFR) = IWT(IFBFR) + NMSUB(INBFR)
            ENDIF

         ELSE
 
C######################################################################
C   COME HERE TO DECODE EACH SUBSET, DATE CHECK IT, AND STORE IT IN
C   THE PROPER "TANK" BASED ON THE REPORT YEAR, MONTH, DAY AND HOUR
C              (THE INPUT BUFR MESSAGE DATE IS IRRELEVANT)
C######################################################################

C  Force the BUFR edition number in output message (if one is written
C   here) to be the same as it was in the last input message (overrides
C   default of 3 for case where input BUFR message is edition 4, also
C   handles cases where there is a mixture of edition 3 and 4 input
C   messages)
C  -------------------------------------------------------------------

         CALL PKVS01('BEN',IEDTN)

C  Do not do screen oceanographic data in BUFR type 031, subtype < 100
C  -------------------------------------------------------------------

            IF(L31_0xx)  PRINT'("BUFR MESSAGE CONTAINS OCEANOGRAPHIC ",
     $       "DATA - NO TIME SCREENING - ACCEPT ALL DATA")'
 
C  READ THE NEXT SUBSET IN THE MESSAGE
C  -----------------------------------
 
            DO WHILE(IREADSB(INBFR).EQ.0)
               NSUBSETSRD = NSUBSETSRD + 1


C  USE FIRST DATE ENCOUNTERED; EXCEPTION: MULTIPLE DATES WHERE ONE DATE
C   HAS A TIME SIGNIFICANCE QUALIFIER (TSIG) OF 25, USE THIS DATE
C   SINCE IT SHOULD REPRESENT THE "Nominal reporting time"
C  --------------------------------------------------------------------

               CALL UFBREP(INBFR,DATES_8,6,20,NLEV,
     $                     'TSIG YEAR MNTH DAYS HOUR MINU')
cppppp
ccc   print *, '^^^ nlev = ',nlev
cppppp
               ISELECT = 0
               IF(NLEV.GT.0) THEN
cppppp
ccc               do i=1,nlev
ccc   print *,'^^^ I,DATES_8: ',i,(dates_8(j,i),j=1,6)
ccc               enddo
cppppp
                  DO I=1,NLEV
                     IF (DATES_8(1,I).EQ.25.) THEN
                        ISELECT = I
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
               IF(ISELECT.EQ.0) then
c  There was no "Nominal report time" time significance flag, so just
c     get the first set of date/time values
cppppp
ccc               print*, '^^^ no useful tsig... try without' 
cppppp
                  CALL UFBINT(INBFR,DATES_8,6,1,NLEV,
     $                  'NUL YEAR MNTH DAYS HOUR MINU')
cppppp
ccc!!  can use ufbrep rather than ufbint for more details on available
ccc!!    date/time values in each subset when debugging
ccc               CALL UFBREP(INBFR,DATES_8,6,20,NLEV,
ccc      $              'NUL YEAR MNTH DAYS HOUR MINU')
ccc               do i=1,nlev
ccc                  print *,'^^^ I,DATES_8: ',i,(dates_8(j,i),j=1,6)
ccc               enddo
cppppp
                  IF (NLEV.GT.0) ISELECT=1
               ENDIF
               IF(ISELECT.GT.0) then
cppppp
ccc    print *, '^^^ Choose: ',iselect,(dates_8(j,iselect),j=1,6)
cppppp
                  IY = NINT(DATES_8(2,ISELECT))
                  IM = NINT(DATES_8(3,ISELECT))
                  ID = NINT(DATES_8(4,ISELECT))
                  IH = NINT(DATES_8(5,ISELECT))
                  MI = NINT(DATES_8(6,ISELECT))
               ELSE
                  PRINT'(/25("*"),"ABORT",25("*")/"ONE OR MORE SUBSETS",
     $             " READ IN DO NOT HAVE AN INTERNAL DATE  -- STOP 96"/
     $             25("*"),"ABORT",25("*")/)'
ccccccccccccccccccCALL W3TAGE('BUFR_TRANJB')
                  CALL ERREXIT(96)
               ENDIF
cppppp
ccc    print *, '^^^ reading a subset with Table A ',tablea,
ccc  .  ' from unit ',inbfr
CC       print *, ' IY,IM,ID,IH,MI,ISELECT,NLEV ',IY,IM,ID,IH,MI,ISELECT,NLEV
cppppp
 
               IF(IBFMS(DATES_8(3,ISELECT)).NE.0 .AND.
     $            IBFMS(DATES_8(4,ISELECT)).NE.0) THEN
                  CALL UFBINT(INBFR,DOYR_8,1,1,NLEV,'DOYR');DOYR=DOYR_8
                  IF(IBFMS(DOYR_8).NE.0) THEN
c                     PRINT'(/25("*"),"ABORT",25("*")/"ONE OR MORE ",
c     $                "SUBSETS READ IN DO NOT HAVE AN INTERNAL DATE  ",
c     $                "-- STOP 96"/25("*"),"ABORT",25("*")/)'
cccccccccccccccccccccCALL W3TAGE('BUFR_TRANJB')

	print*, 'ONE OR MORE SUBSETS READ DO NOT HAVE AN INTERNAL DATE'
        print*, 'SKIPPING TO NEXT REPORT'
		     cycle
ccc                     CALL ERREXIT(96)
                  ELSE
                     IDOYR = NINT(DOYR)
cppppp
ccc                  write(6,'(" READ DOYR: ",I3)')  IDOYR
cppppp

                     IF(IY.GT.1583.AND.IY.LT.3300.AND.IDOYR.LT.367) THEN
                        CALL REMTDY(IY,IDOYR,IM,ID)
cppppp
ccc                     write(6,'(" CONVERTED DOYR (",I3,") TO MNTH (",
ccc  .                   I2.2,") AND DAYS (",I2.2,")")') IDOYR,IM,ID
cppppp
                     ENDIF
                  ENDIF
               ENDIF

               IDATE = IY*1000000 + IM*10000 + ID*100 + IH
 
C  TIME SCREEN THE SUBSET DATE (YYYYMMDDHH)
C  ----------------------------------------
 
               ISKIP = 0
               IF((IM.LT.1.OR.IM.GT.12) .OR. (ID.LT.1.OR.ID.GT.31) .OR.
     $            (IH.LT.0.OR.IH.GT.24)) THEN
                  ISKIP = 1
                  IDATE = 0
               ELSE IF(SCRN) THEN
                  IF((IDATE.LE.JDATE.OR.IDATE.GE.KDATE) .AND.
     $               .NOT.L31_0xx) THEN
                     ISKIP = 2
                     IDATE = 0
                  ENDIF
               ENDIF
 
C  IDENTIFY THE OUTPUT "TANK" FILE AND OPEN IF NECESSARY
C  -----------------------------------------------------
 
               WRITE(CDATE,'(I8.8)') IDATE/100

               IF(NSUBSETSRD.EQ.1) IDATE_PREV = IDATE

               IF(ICOMP.EQ.1) THEN
                  IF(INT(IDATE_PREV/100).NE.INT(IDATE/100)) THEN

C  IF THIS SUBSET HAS A DIFFERENT YYYYMMDD THAN THE PREVIOUS SUBSET AND
C  THE INPUT/OUTPUT BUFR FILES CONTAIN COMPRESSED MESSAGES, MUST FIRST
C  CLOSE THE CURRENT OUTPUT MESSAGE AND DISCONNECT ITS LOGICAL UNIT
C  NUMBER FROM THE BUFRLIB SOFTWARE BECAUSE THE BUFRLIB CAN ONLY HANDLE
C  ONE OUTPUT COMPRESSED BUFR FILE AT A TIME
C  --------------------------------------------------------------------

                    print *
                    print *, 'Found date change at subset# ', nsubsetsrd
                    print *, 'IDATE:',idate,'IDATE_prev: ', IDATE_prev
                    print *
                    CALL CLOSMG(IFBFR)
                    IDATE_PREV = IDATE
                  ENDIF
               ENDIF

               CALL TYPTIM(MTYP,MSBT,CDATE,IERR)

               IRD(49)    = IRD(49)  + 1
               IRD(IFBFR) = IRD(IFBFR) + 1
cppppp
ccc    print *, '^^^ this is subset number ',ird(ifbfr),
ccc  .  ' associated with Table A entry ',ctablea(ifbfr)
cppppp
 
               IF(ISKIP.GT.0) THEN
                  CALL UFBINT(INBFR,ALALO_8,2,1,NLEV,'CLAT CLON')
                  IF(IBFMS(ALALO_8(1)).NE.0) THEN
                     CALL UFBINT(INBFR,ALALO_8,2,1,NLEV,'CLATH CLONH')
                  ENDIF
                  IF(ISKIP.EQ.1) THEN
                     ISK(2,49)    = ISK(2,49)  + 1
                     ISK(2,IFBFR) = ISK(2,IFBFR) + 1
                     IF(ISK(2,IFBFR).LE.100)  THEN
                        WRITE(CBAD(ISK(2,IFBFR),IFBFR),'("BAD DATE: ",
     $                   I4.4,3I2.2," REPORT IN TABLE A ENTRY: ",A8,
     $                   " SKIPPED, LAT=",F6.2,"N/LON=",F7.2,"E - RUN",
     $                   " TIME: ",I3.2,"/",I2.2,"/",I4," AT ",I4.4,
     $                   "Z")',iostat=ios)
     $                   IY,IM,ID,IH,TABLEA,ALALO_8(1),ALALO_8(2),
     $                   IDAT(2),IDAT(3),IDAT(1),IHHMM
                         if(ios.ne.0)then
                           print*,' minor warning: iostat=',ios,
     $                                               ' writing to cbad'
                         endif
                     ELSE
                        IFL(2,IFBFR) = 1
                     ENDIF
                  ELSE
                     ISK(1,49)    = ISK(1,49)  + 1
                     ISK(1,IFBFR) = ISK(1,IFBFR) + 1
                     IF(ISK(1,IFBFR).LE.100)  THEN
                        WRITE(CREJ(ISK(1,IFBFR),IFBFR),'("REJECTED ",
     $                   "DATE: ",I4.4,3I2.2," REPORT IN TABLE A ",
     $                   "ENTRY: ",A8," SKIPPED, LAT=",F6.2,"N/LON=",
     $                   F7.2,"E - RUN TIME: ",I2.2,"/",I2.2,"/",I4,
     $                   " AT ",I4.4,"Z")',iostat=ios)
     $                   IY,IM,ID,IH,TABLEA,ALALO_8(1),ALALO_8(2),
     $                   IDAT(2),IDAT(3),IDAT(1),IHHMM
                         if(ios.ne.0)then
                           print*,' minor warning: iostat=',ios,
     $                                               ' writing to crej'
                         endif
                     ELSE
                        IFL(1,IFBFR) = 1
                     ENDIF
                  ENDIF
               ELSE
 
C  WRITE THE SUBSET TO THE OUTPUT BUFR "TANK" FILE
C  -----------------------------------------------

                  LDAT = 0

C  Encode current wall-clock processing time into Section 1
C  --------------------------------------------------------

                  CALL W3UTCDAT(LDAT)
                  CALL STRCPT('Y',LDAT(1),LDAT(2),LDAT(3),LDAT(5),
     $             LDAT(6))
                  CALL OPENMB(IFBFR,TABLEA,IDATE)
                  CALL UFBCPY(INBFR,IFBFR)
                  IF(ICOMP.EQ.1)  THEN
                     CALL WRITCP(IFBFR)  ! Compressed BUFR messages
                  ELSE
                     CALL WRITSB(IFBFR)  ! Uncompressed BUFR messages
                  ENDIF
                  IWT(49) = IWT(49) + 1
                  IWT(IFBFR) = IWT(IFBFR) + 1
cppppp
ccc    print *, '%% writing number ',iwt(ifbfr),' to unit ',ifbfr
cppppp
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
                  if(tablea.eq.'NC001103') then
                    CALL UFBINT(INBFR,BUYT_8,1,1,NLEV,'BUYT')
                    if(ibfms(buyt_8).ne.0) cycle
                    ibuyt=nint(buyt_8)
c  remap only TAO/ATLAS/TRITON/PIRATA buoys from b001/xx103 to xx002
                    if(ibuyt.ne.21.and.ibuyt.ne.22) cycle
                  endif
                  if(tablea.eq.'NC001102'.or.tablea.eq.'NC001103') then
                     if(ifirst.eq.0) then
                        if(BORG_REMAP_xx102(1:1).ne.' ') then
                           print'(/"--> BORG_REMAP_xx102 is imported ",
     $                      "as """,A,""""/)', trim(BORG_REMAP_xx102)
                           call parstr(BORG_REMAP_xx102,borgs,16,ntag,
     $                                 ' ',.true.)
                        endif
                        ifirst = 1
                     endif
                     if(BORG_REMAP_xx102(1:1).ne.' ') then
c this subset is a type we might want to remap to b002/xx001, call 
c  subr. remap which will do so if additional criteria are met  
                        call remap(inbfr,idate,ird(49),iwt(49),ird81,
     $                             iwt81,borgs,ntag,tablea)
                     endif
                  endif
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
Cvvvvv remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
                 if(tablea.eq.'NC002101') then
                     if(ifirst.eq.0) then
                        if(BORG_REMAP_002101(1:1).ne.' ') then
                           print'(/"--> BORG_REMAP_002101 is imported ",
     $                      "as """,A,""""/)', trim(BORG_REMAP_002101)
                           call parstr(BORG_REMAP_002101,borgc,1,ntag,
     $                                 ' ',.true.)
                        endif
                        ifirst = 1
                     endif
                     if(BORG_REMAP_002101(1:1).ne.' ') then
c this subset is a type we might want to remap to b002/xx001, call
c  subr. remap which will do so if additional criteria are met
                        call remap01(inbfr,idate,ird(49),iwt(49),ird81,
     $                             iwt81,borgc,ntag,tablea)
                     endif
                  endif
C^^^^^ remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
               ENDIF
 
            ENDDO

C######################################################################

         ENDIF

      ENDDO


C  ALL EXITS HERE
C  --------------
 
C  Summarize counts from the previous message type/subtype processing
C  ------------------------------------------------------------------

      DO I=1,NFBFR
         IF(FLBFR(I)(1:1) .NE. ' ')  THEN
            IFBFR = LFUNT + I
            PRINT'(80("-"))'
            DO J=1,100
               IF(CREJ(J,IFBFR)(1:1).NE.' ')  THEN
                  PRINT'(A)', CREJ(J,IFBFR)
                  CYCLE
               ENDIF
               EXIT
            ENDDO
            DO J=1,100
               IF(CBAD(J,IFBFR)(1:1).NE.' ')  THEN
                  PRINT'(A)', CBAD(J,IFBFR)
                  CYCLE
               ENDIF
               EXIT
            ENDDO
            PRINT'("Read    ",I7," reports from BUFR messages with ",
     $       "Table A entry: ",A8)', IRD(IFBFR),CTABLEA(IFBFR)
            PRINT'("Wrote   ",I7," reports to BUFR tank in unit ",I2)',
     $       IWT(IFBFR),IFBFR
            IF(ISK(1,IFBFR)+ISK(2,IFBFR).GT.0)
     $       PRINT'("Skipped ",I7," reports")',ISK(1,IFBFR)+ISK(2,IFBFR)
            IF(IFLM(IFBFR).EQ.1)  THEN
               PRINT'(/I6," SUBSETS WITH REJECTED DATE (ONLY FIRST 100",
     $          " MSGS PRINTED) FOR TBL A ENTRY: ",A8," RUN TIME: ",
     $          I2.2,"/",I2.2,"/",I4," AT ",I4.4,"Z")',
     $          ISK(1,IFBFR),CTABLEA(IFBFR),IDAT(2),IDAT(3),IDAT(1),
     $          IHHMM
            ENDIF
            IF(IFL(1,IFBFR).EQ.1)  THEN
               PRINT'(/I6," SUBSETS WITH REJECTED DATE (ONLY FIRST 100",
     $          " PRINTED) FOR TABLE A ENTRY: ",A8,"    RUN TIME: ",
     $          I2.2,"/",I2.2,"/",I4," AT ",I4.4,"Z")',
     $          ISK(1,IFBFR),CTABLEA(IFBFR),IDAT(2),IDAT(3),IDAT(1),
     $          IHHMM
            ENDIF
            IF(IFL(2,IFBFR).EQ.1)  THEN
               PRINT'(/I6," SUBSETS WITH    BAD   DATE (ONLY FIRST 100",
     $          " PRINTED) FOR TABLE A ENTRY: ",A8,"    RUN TIME: ",
     $          I2.2,"/",I2.2,"/",I4," AT ",I4.4,"Z")',
     $          ISK(2,IFBFR),CTABLEA(IFBFR),IDAT(2),IDAT(3),IDAT(1),
     $          IHHMM
            ENDIF
         ENDIF
      ENDDO
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
      if((ird81.gt.0).and.(tablea(1:6).eq.'NC0011'))  then
         print'(80("-"))'
         print'("Read    ",I7," reports from BUFR messages with ",
     $    "Table A entry: NC001102 and/or NC001103")', ird81
         PRINT'("Remapped and Wrote   ",I7," reports to BUFR tank ",
     $    "NC001002 in unit 81")', iwt81
      endif
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
Cvvvvv remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]
      if((ird81.gt.0).and.(tablea.eq.'NC002101'))  then
         print'(80("-"))'
         print'("Read    ",I7," reports from BUFR messages with ",
     $    "Table A entry: NC002101")', ird81
         PRINT'("Remapped and Wrote   ",I7," reports to BUFR tank ",
     $    "NC002001 in unit 81")', iwt81
      endif
C^^^^^ remapping b002/xx101 ---> b002/xx001                 [CH 11/2019]

C  CLOSE ALL POSSIBLE OUTPUT BUFR "TANK" FILES AND RESET THE CACHE
C  (I.E., REMOVE ALL ASSOCIATION BETWEEN FORTRAN UNIT NUMBERS AND
C  OUTPUT FILENAMES)
C  ---------------------------------------------------------------
 
      CALL CLCASH
 
      PRINT'(101("=")/"*** PROCESSING ENDED NORMALLY ***")'

C  Summarize counts from all message type/subtype processing
C  ---------------------------------------------------------

      IF(IRD(IFBFR).NE.IRD(49)) THEN
         PRINT'(/"*** TOTAL NUMBER OF REPORTS READ IN:     ",I7/
     $    "*** TOTAL NUMBER OF REPORTS WRITTEN OUT: ",I7)',
     $    IRD(49),IWT(49)
         IF(ISK(1,49)+ISK(2,49).GT.0)
     $    PRINT'("*** TOTAL NUMBER OF REPORTS SKIPPED:     ",I7)',
     $    ISK(1,49)+ISK(2,49)
      ENDIF

      PRINT *
 
ccccccCALL W3TAGE('BUFR_TRANJB')
 
      STOP
 
C  ERROR EXITS
C  -----------

  901 CONTINUE

      PRINT'(/25("*"),"ABORT",25("*")/"UNABLE TO ALLOCATE ARRAYS -- ",
     $ "STOP 97"/
     $ 25("*"),"ABORT",25("*")/)'
CCCCCCCALL W3TAGE('BUFR_TRANJB')
      CALL ERREXIT(97)

      END

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

      COMMON /LUNITS/ INBFR,IFBFR,LFBFR,LFUNT,CTABLEA(50:49+NFBFR),
     $                FLBFR(NFBFR)
      COMMON /KOUNTS/ IRD(49:49+NFBFR),IWT(49:49+NFBFR),
     $                ISK(2,49:49+NFBFR),IFL(2,50:49+NFBFR),IDAT(8),
     $                IHHMM,CREJ(100,50:49+NFBFR),CBAD(100,50:49+NFBFR),
     $                ISKM(50:49+NFBFR),IFLM(50:49+NFBFR)
      COMMON /HOMEDC/ IEDTN,IMESSAGE_LENGTH,TANK_DIR,BUFR_FIX_DIR,
     $                SATINGEST_FIX_DIR,CHGRP_RSTPROD,SUBDATE_CHECK,
     $                RUN_TYPE

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
      CHARACTER*3   MTYP,MSBT,CHGRP_RSTPROD,SUBDATE_CHECK
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
      
            IF(CHGRP_RSTPROD.EQ.'YES' .OR.
     $         CHGRP_RSTPROD.EQ.'ALL')  THEN

C  CHANGE GROUP & PERMISSION ON RESTRICTED TANKS TO rstprod & 640, RESP
C  --------------------------------------------------------------------
      
              IF((MTYP.EQ.'000'.and.MSBT.eq.'000').OR.   ! b000/xx000
     $           (MTYP.EQ.'000'.and.MSBT.eq.'020').OR.   ! b000/xx020
     $           (MTYP.EQ.'000'.and.MSBT.eq.'100').OR.   ! b000/xx100
     $           (MTYP.EQ.'001'.and.MSBT.eq.'001').OR.   ! b001/xx001
     $           (MTYP.EQ.'001'.and.MSBT.eq.'101').OR.   ! b001/xx101
     $           (MTYP.EQ.'001'.and.MSTB.eq.'121').OR.   ! b001/xx121
     $           (MTYP.EQ.'002'.and.MSBT.eq.'020').OR.   ! b002/xx020
     $           (MTYP.EQ.'003'.and.MSBT.eq.'010').OR.   ! b003/xx010
     $           (MTYP.EQ.'004'.and.MSBT.eq.'003').OR.   ! b004/xx003
     $           (MTYP.EQ.'004'.and.MSBT.eq.'004').OR.   ! b004/xx004
     $           (MTYP.EQ.'004'.and.MSBT.eq.'006').OR.   ! b004/xx006
     $           (MTYP.EQ.'004'.and.MSBT.eq.'007').OR.   ! b004/xx007
     $           (MTYP.EQ.'004'.and.MSBT.eq.'008').OR.   ! b004/xx008
     $           (MTYP.EQ.'004'.and.MSBT.eq.'009').OR.   ! b004/xx009
     $           (MTYP.EQ.'004'.and.MSBT.eq.'010').OR.   ! b004/xx010
     $           (MTYP.EQ.'004'.and.MSBT.eq.'011').OR.   ! b004/xx011
     $           (MTYP.EQ.'004'.and.MSBT.eq.'012').OR.   ! b004/xx012
     $           (MTYP.EQ.'004'.and.MSBT.eq.'013').OR.   ! b004/xx013
     $           (MTYP.EQ.'004'.and.MSBT.eq.'103').OR.   ! b004/xx103
     $           (MTYP.EQ.'007'.and.MSBT.eq.'001').OR.   ! b007/xx001
     $           (MTYP.EQ.'007'.and.MSBT.eq.'002').OR.   ! b007/xx002
     $           (MTYP.EQ.'012'.and.MSBT.eq.'004').OR.   ! b012/xx004
     $           (MTYP.EQ.'021'.and.MSBT.eq.'242').OR.   ! b021/xx242
     $           (MTYP.EQ.'255'.AND.                     ! b255/
     $            (MSBT.NE.'161'.AND.MSBT.NE.'131')).OR. !      xx161, xx131
     $           (MTYP.EQ.'006'.AND.                     ! b006/
     $            (MSBT.GE.'080'.AND.MSBT.LE.'133')).OR. !      xx080-xx133
     $            CHGRP_RSTPROD.EQ.'ALL')  THEN
                 CALL SYSTEM(
     $                   'chgrp rstprod '//trim(TANK_DIR)//'/'//FLNEW)
                 CALL SYSTEM('chmod 640 '//trim(TANK_DIR)//'/'//FLNEW)
                PRINT'("   ---> This tank contains restricted data -",
     $           " only users in rstprod group have read permission")'
              ENDIF ! MTYP & MSBT == select tanks
            ENDIF ! CHGRP_RSTPROD == YES or ALL

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

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    OPENBT
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2017-01-01
C
C ABSTRACT: GIVEN A BUFR MESSAGE TYPE, OPENS THE APPROPRIATE EXTERNAL
C   BUFR MNEMONIC TABLE AND ASSIGNS IT TO FORTRAN UNIT NUMBER
C   "LUNDX".  ALSO MAKES SURE ALL OUTPUT BUFR FILES ARE CLOSED AND THE
C   CACHE (ASSIGNING FILENAMES TO UNIT NUMBERS) IS RESET.
C
C PROGRAM HISTORY LOG:
C 1996-09-06  J. WOOLLEN -- ORIGINAL AUTHOR (OVERRIDES DEFAULT ROUTINE
C     OF SAME NAME IN BUFRLIB)
C 2001-01-30  D. KEYSER  -- ASSIGNS VALUE FOR BUFR TABLE UNIT NUMBER
C     (NECESSARY DUE TO BUFRLIB CHANGE SINCE LAST COMPILATION OF THIS
C     CODE)
C 2002-04-08  D. KEYSER  -- ADDED DOCBLOCKS, ADDED COMMENTS,
C     STREAMLINED; EXPANDED STANDARD OUTPUT PRINT TO SUMMARIZE NUMBER
C     OF REPORTS READ, WRITTEN AND SKIPPED FOR EACH NEW TABLE A ENTRY
C     READ IN; IMPROVED ALL STANDARD OUTPUT PRINT; LIMITS PRINT TO 100
C     FOR REPORTS SKIPPED DUE TO BAD OR REJECTED DATE (WITHIN EACH
C     TABLE A ENTRY READ IN), INSTEAD A LINE SUMMARIZING THE NUMBER OF
C     REPORTS SKIPPED IS PRINTED AT THE END OF THE TABLE A ENTRY
C     PROCESSING
C 2003-12-16  D. KEYSER  -- TESTS FOR THE EXISTENCE OF THE BUFR
C     MNEMONIC TABLE IN THE DATABASE PARENT DIRECTORY, IF NOT FOUND
C     PROGRAM NOW STOPS WITH COND. CODE 99
C 2004-10-19  D. KEYSER  -- USES FULL PATH NAMES FOR BUFR MNEMONIC
C     TABLE FILE IN OPEN STATEMENTS AND IN ARGUMENT IN CALLS TO
C     SUBROUTINE SYSTEM (BEFORE USED ONLY FILE NAMES AND ASSUMED
C     EXECUTING SCRIPT WAS RUNNING IN DATABASE PARENT DIRECTORY); NOW
C     LOOKS FOR BUFR MNEMONIC TABLE (FILE) IN EITHER PARENT DATABASE
C     DIRECTORY (FIRST CHOICE) OR IN FIXED FIELD DIRECTORY SPECIFIED BY
C     EXECUTING SCRIPT (SECOND CHOICE) (BEFORE ONLY CHOICE WAS IN
C     PARENT DATABASE DIRECTORY), THIS IS IN CONJUNCTION WITH THE
C     OPERATIONAL TRANSITION OF THE TABLES FROM THE DATABASE PARENT
C     DIRECTORY /dcom/us007003 TO THE PRODUCTION FIXED FIELD DIRECTORY
C     /nwprod/fix
C 2012-09-26  J. WOOLLEN -- INCREASED THE FILE CACHE TO MAXIMUM VALUE
C     OF 31 FILES AT A TIME
C 2014-01-21 D. KEYSER   -- NOW THREE CHOICES FOR LOCATING BUFR
C     MNEMONIC TABLE FILE bufrtab.XXX WHICH DEPEND UPON VALUE OF
C     "RUN_TYPE", RATHER THAN THE TWO FIXED CHOICES BEFORE.  FIRST
C     CHOICE REMAINS TANK DIRECTORY (tank_dir) REGARDLESS OF RUN TYPE.
C     SECOND CHOICE IS OBSPROC_SATINGEST FIX DIRECTORY
C     (FIXobsproc_satingest) FOR SATINGEST RUN TYPE, AND BUFR FIX
C     DIRECTORY (FIXbufr) FOR DECODER RUN TYPE.  THIRD CHOICE IS
C     FIXbufr FOR SATINGEST RUN TYPE AND FIXobsproc_satingest FOR
C     DECODER RUN TYPE. THIS CHANGE WILL ALLOW DEOCDER RUNS TO USE THIS
C     VERSION OF BUFR_TRANJB SINCE THE DECODER RUNS WILL USE THE BUFR
C     MNEMONIC TABLES IN ITS PREFERRED LOCATION of FIXbufr. THE
C     SATINGEST RUNS WILL, FOR NOW, USE THE BUFR MNEMONIC TABLES IN
C     THEIR THIRD CHOICE LOCATION (FIXbufr), BUT WILL AUTOMATICALLY
C     TRANSITION TO THE SECOND CHOICE FIXobsproc_satingest ONCE THE
C     BUFR MNEMONIC TABLES ARE AVAILABLE THERE.
C 2017-01-01 D. Stokes -- Increased string length for array CBAD.
C
C USAGE:    CALL OPENBT(LUNDX,MTYP)
C   INPUT ARGUMENT LIST:
C     MTYP     - MESSAGE TYPE OF INPUT BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     LUNDX    - UNIT NUMBER OF BUFR MNEMONIC TABLE
C
C   INPUT FILES:
C     UNIT "LUNDX" - BUFR MNEMONIC TABLE
C
C   OUTPUT FILES:
C     UNIT 06      - STANDARD OUTPUT PRINT
C
C REMARKS: THIS SUBROUTINE IS CALLED ONCE AT THE BEGINNING OF THE
C   THE MAIN PROGRAM FOR THE FIRST DATA MESSAGE FOUND IN THE INPUT
C   BUFR FILE.  IT WILL BE CALLED AGAIN FOR SUBSEQUENT DATA MESSAGES
C   WHENEVER THE BUFR MESSAGE TYPE CHANGES.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE OPENBT(LUNDX,MTYP)

      PARAMETER (NFBFR=31)
 
      COMMON /LUNITS/ INBFR,IFBFR,LFBFR,LFUNT,CTABLEA(50:49+NFBFR),
     $                FLBFR(NFBFR)
      COMMON /KOUNTS/ IRD(49:49+NFBFR),IWT(49:49+NFBFR),
     $                ISK(2,49:49+NFBFR),IFL(2,50:49+NFBFR),IDAT(8),
     $                IHHMM,CREJ(100,50:49+NFBFR),CBAD(100,50:49+NFBFR),
     $                ISKM(50:49+NFBFR),IFLM(50:49+NFBFR)
      COMMON /HOMEDC/ IEDTN,IMESSAGE_LENGTH,TANK_DIR,BUFR_FIX_DIR,
     $                SATINGEST_FIX_DIR,CHGRP_RSTPROD,SUBDATE_CHECK,
     $                RUN_TYPE

      CHARACTER*500 TANK_DIR,BUFR_FIX_DIR,SATINGEST_FIX_DIR,BTFILE,
     $              FILE_CHOICE2,FILE_CHOICE3
      CHARACTER*132 CREJ
      CHARACTER*132 CBAD
      CHARACTER*22  FLBFR
      CHARACTER*18  RUN_TYPE_MNEMONIC_CHOICE2,RUN_TYPE_MNEMONIC_CHOICE3
      CHARACTER*11  BUFRTAB
      CHARACTER*9   RUN_TYPE
      CHARACTER*8   CTABLEA
      CHARACTER*3   CHGRP_RSTPROD,SUBDATE_CHECK

      LOGICAL       EXIST

C  Summarize counts from the previous message type/subtype processing
C  ------------------------------------------------------------------

      DO I=1,NFBFR
         IF(FLBFR(I)(1:1) .NE. ' ')  THEN
            IFBFR = LFUNT + I
            PRINT'(80("-"))'
            DO J=1,100
               IF(CREJ(J,IFBFR)(1:1).NE.' ')  THEN
                  PRINT'(A)', CREJ(J,IFBFR)
                  CYCLE
               ENDIF
               EXIT
            ENDDO
            DO J=1,100
               IF(CBAD(J,IFBFR)(1:1).NE.' ')  THEN
                  PRINT'(A)', CBAD(J,IFBFR)
                  CYCLE
               ENDIF
               EXIT
            ENDDO
            PRINT'("Read    ",I7," reports from BUFR messages with ",
     $       "Table A entry: ",A8)', IRD(IFBFR),CTABLEA(IFBFR)
            IF(IWT(IFBFR).GT.0)  THEN
               PRINT'("Wrote   ",I7," reports to BUFR tank in unit ",
     $          I2)', IWT(IFBFR),IFBFR
            ELSE
               PRINT'("Wrote   ",I7," reports")', IWT(IFBFR)
            ENDIF
            IF(ISK(1,IFBFR)+ISK(2,IFBFR).GT.0)
     $       PRINT'("Skipped ",I7," reports")',ISK(1,IFBFR)+ISK(2,IFBFR)
            IF(IFLM(IFBFR).EQ.1)  THEN
               PRINT'(/I6," SUBSETS WITH REJECTED DATE (ONLY FIRST 100",
     $          " MSGS PRINTED) FOR TBL A ENTRY: ",A8," RUN TIME: ",
     $          I2.2,"/",I2.2,"/",I4," AT ",I4.4,"Z")',
     $          ISK(1,IFBFR),CTABLEA(IFBFR),IDAT(2),IDAT(3),IDAT(1),
     $          IHHMM
            ENDIF
            IF(IFL(1,IFBFR).EQ.1)  THEN
               PRINT'(/I6," SUBSETS WITH REJECTED DATE (ONLY FIRST 100",
     $          " PRINTED) FOR TABLE A ENTRY: ",A8,"    RUN TIME: ",
     $          I2.2,"/",I2.2,"/",I4," AT ",I4.4,"Z")',
     $          ISK(1,IFBFR),CTABLEA(IFBFR),IDAT(2),IDAT(3),IDAT(1),
     $          IHHMM
            ENDIF
            IF(IFL(2,IFBFR).EQ.1)  THEN
               PRINT'(/I6," SUBSETS WITH    BAD   DATE (ONLY FIRST 100",
     $          " PRINTED) FOR TABLE A ENTRY: ",A8,"    RUN TIME: ",
     $          I2.2,"/",I2.2,"/",I4," AT ",I4.4,"Z")',
     $          ISK(2,IFBFR),CTABLEA(IFBFR),IDAT(2),IDAT(3),IDAT(1),
     $          IHHMM
            ENDIF
         ENDIF
      ENDDO

      IRD(50:49+NFBFR)   = 0
      IWT(50:49+NFBFR)   = 0
      ISK(:,50:49+NFBFR) = 0
      IFL                = 0
      CREJ               = ' '
      CBAD               = ' '
 

      LUNDX = 20  ! LUNDX must be defined in this subroutine
 
      WRITE(BUFRTAB,'("bufrtab.",i3.3)') MTYP

C----------------------------------------------------------------------
C LOOK FOR THE APPROPRIATE EXTERNAL BUFR MNEMONIC TABLE DIRECTORY PATH
C----------------------------------------------------------------------

      IFOUND = 0

      IF(trim(RUN_TYPE).EQ.'satingest') THEN
         FILE_CHOICE2 = trim(SATINGEST_FIX_DIR)
         FILE_CHOICE3 = trim(BUFR_FIX_DIR)
         RUN_TYPE_MNEMONIC_CHOICE2 = 'SATINGEST_FIX_DIR'
         RUN_TYPE_MNEMONIC_CHOICE3 = 'BUFR_FIX_DIR'
      ELSE
         FILE_CHOICE2 = trim(BUFR_FIX_DIR)
         FILE_CHOICE3 = trim(SATINGEST_FIX_DIR)
         RUN_TYPE_MNEMONIC_CHOICE2 = 'BUFR_FIX_DIR'
         RUN_TYPE_MNEMONIC_CHOICE3 = 'SATINGEST_FIX_DIR'
      ENDIF

C  First choice is the TANK directory "TANK_DIR" regardless of RUN_TYPE
C  --------------------------------------------------------------------

      BTFILE = trim(TANK_DIR)//'/'//BUFRTAB
      INQUIRE(FILE=BTFILE,EXIST=EXIST)
      IF(EXIST) THEN
         CLOSE(LUNDX)
         OPEN (LUNDX,FILE=BTFILE)
         PRINT'(101("=")/"OPENING BUFR MNEMONIC TABLE: ",A,"/",A," IN ",
     $    "UNIT",I3," ---> Clear the cache")',
     $    trim(TANK_DIR),BUFRTAB,LUNDX
         IFOUND = 1
cppppp
ccc   print *, 'Go with 1st choice - TANK_DIR'
cppppp
      ELSE

C  For RUN_TYPE = satingest, second choice is the OBSPROC_SATINGEST
C   FIX directory "SATINGEST_FIX_DIR"
C  For RUN_TYPE = decoder, second choice is the BUFR FIX directory
C   "BUFR_FIX_DIR"
C  ----------------------------------------------------------------

         BTFILE = trim(FILE_CHOICE2)//'/'//BUFRTAB
         INQUIRE(FILE=BTFILE,EXIST=EXIST)
         IF(EXIST) THEN
            CLOSE(LUNDX)
            OPEN (LUNDX,FILE=BTFILE)
            PRINT'(101("=")/"OPENING BUFR MNEMONIC TABLE: ",A,"/",A,
     $       " IN UNIT",I3," ---> Clear the cache")',
     $       trim(FILE_CHOICE2),BUFRTAB,LUNDX
            IFOUND = 1
cppppp
ccc   print *, 'For RUN_TYPE ',trim(RUN_TYPE),', go with 2nd choice - ',
ccc  $         trim(RUN_TYPE_MNEMONIC_CHOICE2)
cppppp
         ELSE

C  For RUN_TYPE = satingest, third choice is the BUFR FIX directory,
C   "BUFR_FIX_DIR"
C  For RUN_TYPE = decoder, third choice is the OBSPROC_SATINGEST FIX
C   directory "SATINGEST_FIX_DIR"
C  -----------------------------------------------------------------

            BTFILE = trim(FILE_CHOICE3)//'/'//BUFRTAB
            INQUIRE(FILE=BTFILE,EXIST=EXIST)
            IF(EXIST) THEN
               CLOSE(LUNDX)
               OPEN (LUNDX,FILE=BTFILE)
               PRINT'(101("=")/"OPENING BUFR MNEMONIC TABLE: ",A,"/",A,
     $          " IN UNIT",I3," ---> Clear the cache")',
     $          trim(FILE_CHOICE3),BUFRTAB,LUNDX
               IFOUND = 1
cppppp
ccc   print *, 'For RUN_TYPE ',trim(RUN_TYPE),', go with 3rd choice - ',
ccc  $         trim(RUN_TYPE_MNEMONIC_CHOICE3)
cppppp
            ENDIF
         ENDIF
      ENDIF
      IF(IFOUND.EQ.0) THEN
         PRINT'(/25("*"),"ABORT",25("*")/"BUFR MNEMONIC TABLE ",A,1X,
     $    "DOES NOT EXIST IN:"/1X,A," (First choice)"/17X,"-- or --"/1X,
     $    A," (Second choice)"/17X,"-- or --"/1X,A," (Third choice)"/
     $    "  ===> STOP 98"/25("*"),"ABORT",25("*")/)',
     $   BUFRTAB,trim(TANK_DIR),trim(FILE_CHOICE2),trim(FILE_CHOICE3)
cccccccccCALL W3TAGE('BUFR_TRANJB')
         CALL ERREXIT(98)
      ENDIF

C  CLOSE ALL POSSIBLE OUTPUT BUFR "TANK" FILES AND RESET THE CACHE
C  (I.E., REMOVE ALL ASSOCIATION BETWEEN FORTRAN UNIT NUMBERS AND
C  OUTPUT FILENAMES)
C  ---------------------------------------------------------------

      CALL CLCASH
 
      RETURN
 
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    REMTDY
C   PRGMMR: SAGER            ORG: NP12       DATE: 2001-03-20
C
C ABSTRACT: DETERMINES MONTH-OF-YEAR AND DAY-OF-MONTH GIVEN FOUR-DIGIT
C   YEAR AND DAY-OF-YEAR.
C
C PROGRAM HISTORY LOG:
C 2001-03-20  L. SAGER -- ORIGINAL AUTHOR
C
C USAGE:    CALL REMTDY(IYEAR,IDOY,MON,IDAY)
C   INPUT ARGUMENT LIST:
C     IYEAR    - YEAR (YYYY)
C     IDOY     - DAY-OF-YEAR
C
C   OUTPUT ARGUMENT LIST:
C     MON      - MONTH-OF-YEAR
C     IDAY     - DAY-OF-MONTH
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C
C REMARKS: THIS SUBROUTINE WILL WORK FROM 1583 A.D. TO 3300 A.D.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C$$$
      SUBROUTINE REMTDY(IYEAR,IDOY,MON,IDAY)

      INTEGER    IDAT(8)

      DATA IDAT  /0,1,1,5*0/

C     First, calculate the Julian day on Jan. 1 of year.

ccccc print *,' remtdy   iyear dayyr = ',iyear,idoy
      IDAT(1) = IYEAR
      CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY)

ccccc print *,' dox-dow doy day ',jdow,jdoy,jday

C     Add the day-of-year to Julian day.

      jday = jday + idoy - 1
ccccc print *,' updated jday idoy are ',jday,idoy

C     Call W3FS26 to get month/day from the Julian day.

      CALL W3FS26(JDAY,IYEAR,MON,IDAY,IDAYWK,IDAYYR)
ccccc print *,' year, month, day = ',iyear,mon,iday

      RETURN
      END
cvvvvv remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)

cccccccccccccccccccc
cccccccccccccccccccc
c SUBROUTINE REMAP
cccccccccccccccccccc
cccccccccccccccccccc

      subroutine remap(inbfr,idate,ird49,iwt49,ird81,iwt81,borgs,ntag,
     $  tablea)

c will only remap reports that meet all of the following criteria:
c - bulletin originator is in list passed in via array borgs
c - 7-digit report id has "00" in characters 3:4
c - WMO Marine Observing Platform extended identifier (WMOP) not missing

      real*8 rpid_8,borg_8,rpid_out_8,wmop_8,bpid_8,obs_8(30),
     $       dattim_8(5,2),obs_repl_8(3,255),obs_repl_out_8(5,255)
      real*8 xtemp_8
      REAL(8) BMISS,GETBMISS,GETVALNB

      character*4 borgs(16)
      character*8 crpid,crpid_out,cwmop,cwmop_out,cborg
      character*8 tablea
      equivalence (rpid_8,crpid),(rpid_out_8,crpid_out),(borg_8,cborg)


      BMISS=GETBMISS()

      call ufbint(inbfr,rpid_8,1,1,nlev,'RPID')
      if(crpid(3:4).eq.'00') then
         call ufbint(inbfr,wmop_8,1,1,nlev,'WMOP')
         if(ibfms(wmop_8).eq.0) then
            call ufbint(inbfr,borg_8,1,1,nlev,'BORG')
            do i = 1,ntag
               if(cborg(1:4).eq.borgs(i)) goto 101
            enddo
            return
  101       continue
            ird49    = ird49  + 1
            ird81 = ird81 + 1

            call openmb(81,'NC001002',idate)
c-----------------------------------------------------------------------
            crpid_out = crpid(1:2)//crpid(5:8)
            call ufbint(81,rpid_out_8,1,1,nlev,'RPID')
c-----------------------------------------------------------------------
            write(cwmop,'(I7.7)') nint(wmop_8)
            cwmop_out = cwmop(1:2)//cwmop(5:8)
            read(cwmop_out,'(F8.0)') bpid_8
            call ufbint(81,bpid_8,1,1,nlev,'BPID')
c-----------------------------------------------------------------------
            obs_8 = bmiss
            call ufbint(inbfr,obs_8,12,1,nlev,
     $ 'BUYT CLATH CLONH QMST QMPR PRES PMSL 3HPC CHPT QMAT TMDB TMDP')
            call ufbint(inbfr,obs_8(13),12,1,nlev,
     $ 'REHU ANTP WDIR WSPD DOMO PLDS QBST QCIL QCLS BVOLH DROT LDDS')
            call ufbint(inbfr,obs_8(25),6,1,nlev,
     $ 'DROD LDRS CORN SST0 AVWP SGWH')

! if input PRES non-missing & PMSL missing, encode PMSL = PRES in output
            if(ibfms(obs_8(6)).eq.0 .and. ibfms(obs_8(7)).ne.0)
     $       obs_8(7) = obs_8(6)

            call ufbint(81,obs_8,12,1,nlev,
     $ 'BUYT CLATH CLONH QMST QMPR PRES PMSL 3HPC CHPT QMAT TMDB TMDP')
            call ufbint(81,obs_8(13),12,1,nlev,
     $ 'REHU ANTP WDIR WSPD DOMO PLDS QBST QCIL QCLS BVOL DROT LDDS')
            call ufbint(81,obs_8(25),6,1,nlev,
     $ 'DROD LDRS CORN SST1 POWV HOWV')
            call ufbint(81,obs_8(2),2,1,nlev,'CLAT CLON')
c-----------------------------------------------------------------------
C  Get temperature/salinity profile data, if available
            nlev_out=0
            obs_repl_8(:,:) = bmiss
            obs_repl_out_8(:,:) = bmiss
            call ufbint(inbfr,obs_repl_8,3,255,nlev_ts,'DBSS SST1 SALN')
            if(nlev_ts.gt.0) then

!  we want the MSDM that belongs to the salinity profile so use
!   first instance of SALN as pivot
              xtemp_8=bmiss
              xtemp_8=getvalnb(inbfr,'SALN',1,'MSDM',-1 )

              call ufbint(81,xtemp_8,1,1,ier,'MSDM')

              levloop_ts: do ilev=1,nlev_ts
                if (ibfms(obs_repl_8(2,ilev)).eq.0.or.
     $              ibfms(obs_repl_8(3,ilev)).eq.0) then
                    if(nlev_out.lt.255)then
                      nlev_out=nlev_out+1
                      obs_repl_out_8(1,nlev_out)=obs_repl_8(1,ilev) !dbss
                      obs_repl_out_8(2,nlev_out)=obs_repl_8(2,ilev) !stmp
                      obs_repl_out_8(3,nlev_out)=obs_repl_8(3,ilev) !saln
                    else
                      print*,'truncating profile at 255 levels'
                    endif
                endif
              enddo  levloop_ts
            endif

C  Now, if we have NC001103 input, do the same for currents, appending
c    available info to the existing profile. (We may get duplicate DBSS 
c    values, but that is how the info is stored in original xx002 
c    profiles from the FM-18 TAC feed.
            if(tablea.eq.'NC001103')then
              obs_repl_8(:,:) = bmiss
              call ufbseq(inbfr,obs_repl_8,3,255,nlev_cp,'BBYCURR')
              if(nlev_cp.gt.0) then
!  Look for DTCC asssociated with current profile.
                xtemp_8=bmiss
                xtemp_8=getvalnb(inbfr,'DROC',1,'DTCC',-1 )
                call ufbint(81,xtemp_8,1,1,ierr,'DTCC')
                levloop_cur: do ilev=1,nlev_cp
                  if(ibfms(obs_repl_8(2,ilev)).eq.0.or.
     $                ibfms(obs_repl_8(3,ilev)).eq.0)then
                   if(nlev_out.lt.255)then
                    nlev_out=nlev_out+1
                    obs_repl_out_8(1,nlev_out)=obs_repl_8(1,ilev) !dbss
                    obs_repl_out_8(4,nlev_out)=obs_repl_8(2,ilev) !dboc
                    obs_repl_out_8(5,nlev_out)=obs_repl_8(3,ilev) !spoc
                   else
                    print*,'truncating profile at 255 levels'
                   endif
                  endif
                enddo  levloop_cur
              endif
            endif

C  Now store accumlated profile info in output subset
            if(nlev_out.gt.0) then
               call ufbint(81,obs_repl_out_8,5,nlev_out,ierr,
     $                 'DBSS STMP SALN DROC SPOC')
            endif

c-----------------------------------------------------------------------
            dattim_8 = bmiss
! assume first repl. of input date is asscoiated with TSIG=25 and second
!  is associated with TSIG=26
            call ufbrep(inbfr,dattim_8,5,2,iret,
     $ 'YEAR MNTH DAYS HOUR MINU')

            call ufbint(81,dattim_8(1,1),5,1,nlev,
     $ 'YEAR MNTH DAYS HOUR MINU')
            call ufbint(81,dattim_8(1,2),5,1,nlev,
     $ 'PSYR PSMN PSDY PSHR PSMI')
c-----------------------------------------------------------------------
            obs_8 = bmiss
            call ufbint(inbfr,obs_8,6,1,nlev,
     $ 'RCTS RCYR RCMO RCDY RCHR RCMI')

            call ufbint(81,obs_8,6,1,nlev,
     $ 'RCTS RCYR RCMO RCDY RCHR RCMI')
c-----------------------------------------------------------------------
            obs_8 = bmiss
            call ufbint(inbfr,obs_8,5,1,nlev,
     $ 'SEQNUM BUHD BORG BULTIM BBB')

            call ufbint(81,obs_8,5,1,nlev,
     $ 'SEQNUM BUHD BORG BULTIM BBB')
c-----------------------------------------------------------------------
            obs_8 = bmiss
            obs_8(1:2) = 0.0
! encode SELV = 0.0 & TOST = 0.0 for all reports in output
            call ufbint(81,obs_8,2,1,nlev,'SELV TOST')
c-----------------------------------------------------------------------
! Map the HSAWS value associated with WSPD to the ANHT in xx002 
            xtemp_8=bmiss
            xtemp_8=getvalnb(inbfr,'WSPD',1,'HSAWS',-1 )
            call ufbint(81,xtemp_8,1,1,nlev,'ANHT')
c-----------------------------------------------------------------------

            call writsb(81)  ! Uncompressed BUFR messages
            iwt49 = iwt49 + 1
            iwt81 = iwt81 + 1
         endif
      endif

      return
      end
c^^^^^ remapping b001/xx102, xx103 -> xx002 workaround (DAK/DCS:11/2016)
C
Cvvvvv remapping b002/xx101 ---> b002/xx001              [CH/JW 11/2019]
      subroutine remap01(inbfr,idate,ird49,iwt49,ird81,iwt81,borgc,ntag,
     $  tablea)

      real*8 rpid_8,borg_8,obs_8(30),dattim_8(5,2),
     $       obs_repl_8(13,255),obs_repl_out_8(13,255)
      REAL(8) BMISS,GETBMISS

      character*4 borgc,uapart(255)
      character*8 cborg,tablea
      equivalence (borg_8,cborg)

      BMISS=GETBMISS()

      call ufbint(inbfr,rpid_8,1,1,nlev,'RPID')
          if(ibfms(rpid_8).eq.0) then
            call ufbint(inbfr,borg_8,1,1,nlev,'BORG')
C           do i = 1,ntag
             if(cborg(1:4).eq.borgc) goto 901
C           enddo
            return
  901       continue
            ird49 = ird49 + 1
            ird81 = ird81 + 1

            call openmb(81,'NC002001',idate)
            call ufbint(81,rpid_8,1,1,nlev,'RPID')
c-----------------------------------------------------------------------
            obs_8 = bmiss
            call ufbint(inbfr,obs_8,3,1,nlev,'CLATH CLONH HSMSL')
C          if((ibfms(obs_8(2)).eq.0.and.ibfms(obs_8(2)).eq.0) then
C           if(mod(int(obs_8(2)*100.),10).ne.0) 
C    $                 obs_8(2)=obs_8(2)
C           if(mod(int(obs_8(3)*100.),10).ne.0)
C    $                 obs_8(3)=obs_8(3)
C          endif
            
            call ufbint(inbfr,obs_8(4),2,1,nlev,'SIRC TTSS')
            call ufbint(inbfr,obs_8(6),2,1,nlev,'RATP A4ME')
            call ufbint(inbfr,obs_8(8),1,1,nlev,'CORN')

             call ufbint(81,obs_8,3,1,nlev,'CLAT CLON SELV')
             call ufbint(81,obs_8(4),2,1,nlev,'SIRC TTSS') 
             call ufbint(81,obs_8(6),3,1,nlev,'RATP A4ME CORN')

c-----------------------------------------------------------------------
C  Profile values of pressure, temperature, humidity, and wind
            nlev_out=0
C           uapart(:)='    '
            obs_repl_8(:,:) = bmiss
            obs_repl_out_8(:,:) = bmiss
           call ufbint(inbfr,obs_repl_8,13,255,nlev_ts,'VSIGX QMPR PRLC 
     $QMGP GP07 GP10 QMAT TMDB QMDD TMDP QMWN WDIR WSPD')

            if(nlev_ts.gt.0) then

              levloop_ts: do ilev=1,nlev_ts
                if ((ibfms(obs_repl_8(3,ilev)).eq.0).and.
     $              (ibfms(obs_repl_8(8,ilev)).eq.0.or.
     $               ibfms(obs_repl_8(13,ilev)).eq.0)) then
                  if(nlev_out.lt.255)then
                    nlev_out=nlev_out+1
                    obs_repl_out_8(1,nlev_out)=obs_repl_8(1,ilev) !VSIG
                    if (ibfms(obs_repl_8(1,ilev)).eq.0) then
                      if (mod(int(obs_repl_8(1,ilev)),2048).eq.0) then
                                  obs_repl_out_8(1,nlev_out)=
     $                            obs_repl_out_8(1,nlev_out)/2048.
                      if (int(obs_repl_out_8(1,nlev_out)).eq.1)
     $                        obs_repl_out_8(1,nlev_out) = 2.
                      if (mod(int(obs_repl_out_8(1,nlev_out)),2).eq.1)
     $                            obs_repl_out_8(1,nlev_out) = 
     $                            obs_repl_out_8(1,nlev_out) + 1.
                      endif
                    endif 
                    obs_repl_out_8(2,nlev_out)=obs_repl_8(2,ilev) !QMPR
                    obs_repl_out_8(3,nlev_out)=obs_repl_8(3,ilev) !PRLC
C                   if (obs_repl_out_8(3,nlev_out).gt.0..and.
C    $                  obs_repl_out_8(3,nlev_out).lt.100.)
C    $                 uapart(nlev_out)='TTDD'
C                   if (obs_repl_out_8(3,nlev_out).ge.100..and.
C    $                  obs_repl_out_8(3,nlev_out).lt.1100.)
C    $                 uapart(nlev_out)='TTBB'
                    obs_repl_out_8(4,nlev_out)=obs_repl_8(4,ilev) !QMGP
                    obs_repl_out_8(5,nlev_out)=obs_repl_8(5,ilev) !GP07
                    obs_repl_out_8(6,nlev_out)=obs_repl_8(6,ilev) !GP10
                    obs_repl_out_8(7,nlev_out)=obs_repl_8(7,ilev) !QMAT
                    obs_repl_out_8(8,nlev_out)=obs_repl_8(8,ilev) !TMDB
                    obs_repl_out_8(9,nlev_out)=obs_repl_8(9,ilev) !QMDD
                    obs_repl_out_8(10,nlev_out)=obs_repl_8(10,ilev) !TMDP
                    obs_repl_out_8(11,nlev_out)=obs_repl_8(11,ilev) !QMWN
                    obs_repl_out_8(12,nlev_out)=obs_repl_8(12,ilev) !WDIR
                    obs_repl_out_8(13,nlev_out)=obs_repl_8(13,ilev) !WSPD
                  else
                    print*,'truncating profile at 255 levels'
                  endif
                endif
              enddo  levloop_ts
            endif

C  Now store accumlated profile info in output subset
            if(nlev_out.gt.0) then
         call ufbint(81,obs_repl_out_8,13,nlev_out,ierr,'VSIG QMPR PRLC 
     $QMGP GP07 GP10 QMAT TMDB QMDD TMDP QMWN WDIR WSPD') 
C  UAPART? 
            endif

c-----------------------------------------------------------------------
            dattim_8 = bmiss

            call ufbrep(inbfr,dattim_8,5,1,iret,
     $ 'YEAR MNTH DAYS HOUR MINU')

            call ufbint(81,dattim_8(1,1),5,1,nlev,
     $ 'YEAR MNTH DAYS HOUR MINU')

c-----------------------------------------------------------------------
            obs_8 = bmiss
            call ufbint(inbfr,obs_8,6,1,nlev,
     $ 'RCTS RCYR RCMO RCDY RCHR RCMI')

            call ufbint(81,obs_8,6,1,nlev,
     $ 'RCTS RCYR RCMO RCDY RCHR RCMI')
c-----------------------------------------------------------------------
            obs_8 = bmiss
            call ufbint(inbfr,obs_8,5,1,nlev,
     $ 'SEQNUM BUHD BORG BULTIM BBB')

            call ufbint(81,obs_8,5,1,nlev,
     $ 'SEQNUM BUHD BORG BULTIM BBB')
c-----------------------------------------------------------------------
            obs_8 = bmiss
            obs_8(1) = 0.0
            obs_8(2) = 3.0
            obs_8(3) = 0.0
C encode SELV = 0.0, TOST = 3.0, and TIWM = 0 for all reports in output
            call ufbint(81,obs_8,1,1,nlev,'TOST')
            call ufbint(81,obs_8(2),1,1,nlev,'TIWM')

c-----------------------------------------------------------------------

            call writsb(81)  ! Uncompressed BUFR messages
            iwt49 = iwt49 + 1
            iwt81 = iwt81 + 1
      endif

      return
      end
C^^^^^ remapping b002/xx101 ---> b002/xx001              [CH/JW 11/2019]
C
