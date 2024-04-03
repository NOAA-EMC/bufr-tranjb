C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    REMTDY
C   PRGMMR: WEISS            ORG: NP12       DATE: 2024-03-28
C
C PROGRAM HISTORY LOG:
C 2001-03-20  L. SAGER -- ORIGINAL AUTHOR
C 2024-03-28  M. WEISS -- SEPARATED SUBPROGRAM REMTDY FROM
C     bufr_tranjb.f. NO OTHER CHANGES
C
C ABSTRACT: DETERMINES MONTH-OF-YEAR AND DAY-OF-MONTH GIVEN FOUR-DIGIT
C   YEAR AND DAY-OF-YEAR.
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
C--------------------------------------------------------------------
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
