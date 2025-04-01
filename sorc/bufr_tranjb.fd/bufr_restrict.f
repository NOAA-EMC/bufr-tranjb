	SUBROUTINE BUFR_RESTRICT (RTANK_TBL, IRET)
C************************************************************************
C* BUFR_RESTRICT							*
C*									*
C* This subroutine opens and reads a table file containing a list of    * 
C* BUFR tanks whose data access permissions are restricted.             *
C*									*
C* BUFR_RESTRICT (RTANK_TBL, IRET)                                      *
C*									*
C* Input parameters:							*
C*	RTANK_TBL         CHAR*     Restricted BUFR tanks table         *
C*                                  (RSTPROD_BUFR_TANK.TBL)             *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	    Return code:                        *
C*                                      0 = normal return               *
C*                                     -1 = error opening or reading	*
C**                                                                     *
C* Log:									*
C* M. Weiss/NCEP          3/24      New                                 *
C************************************************************************
C*
        PARAMETER (MXTANKS=500)
        PARAMETER (MUNDX=20)

        COMMON /RESTRICT/ IRTANK_TOT, MTYP_RLIST (MXTANKS), 
     +                    MSBT_RLIST (MXTANKS)

        CHARACTER*(*) RTANK_TBL

        CHARACTER REC*100, BUFR_TANK*10 (MXTANKS)
        CHARACTER*3 MTYP_RLIST, MSBT_RLIST 

        INTEGER IRTANK_TOT

        LOGICAL   EXIST
C------------------------------------------------------------------------
        IRET = 0

C*	Open the list of restricted BUFR tanks table.
C*      [open(unit=101, file=fnpos, status='old', action='read')]

        INQUIRE (FILE=RTANK_TBL, EXIST=EXIST)
        IF (EXIST) THEN
          CLOSE (MUNDX)
          OPEN (MUNDX, FILE=RTANK_TBL)
          LTBL = INDEX ( RTANK_TBL, ' ' ) - 1

          PRINT'(101("=")/"OPENING RESTRICTED BUFR TANK TABLE: ",A,
     +    " IN ","UNIT",I3," --> Clear cache")',
     +    RTANK_TBL (LTBL-20:LTBL), MUNDX
  
          ii=0                         ! Count of all records
          kk=0                         ! Count of BUFR tank records
          DO WHILE ( .true. )          ! Read all records
            ii=ii+1
            READ ( UNIT=MUNDX, FMT = '(A)', ERR=900, END=910 ),REC

            IF (ii .LE. 8) CYCLE  ! Skip documentation records
            kk=kk+1
            MTYP_RLIST (kk) = REC(1:3)
            MSBT_RLIST (kk) = REC(10:12)
            BUFR_TANK (kk) = REC(18:27)
            IRTANK_TOT=kk
          ENDDO

        ELSE
          PRINT'(/25("*"),"ABORT",25("*")/"ATTEMPT TO READ RESTRICTED",
     +    " BUFR TANK TABLE FAILED -- STOP 93"/25("*"),
     +    "ABORT",25("*")/)'
          CALL ERREXIT(93)
        ENDIF

  900   IRET = -1
  910   IF ( IRET .eq. -1 ) THEN
          PRINT'(/25("*"),"ABORT",25("*")/"ERROR READING RESTRICTED",
     +    " BUFR TANK TABLE -- STOP 93"/25("*"),"ABORT",25("*")/)'
          CALL ERREXIT(93)
        END IF 
C*
        CLOSE (MUNDX)
C*
        RETURN
	END
