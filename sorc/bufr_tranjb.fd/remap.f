C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAMS:    REMAP and REMAP01
C   PRGMMR: WEISS            ORG: NP12       DATE: 2024-03-28
C
C PROGRAM HISTORY LOG:
C 2024-03-28  M. WEISS -- SEPARATED SUBPROGRAMS REMAP AND REMAP01
C     FROM bufr_tranjb.f. NO OTHER CHANGES
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C--------------------------------------------------------------------
C$$$
      subroutine remap(inbfr,idate,ird49,iwt49,ird81,iwt81,borgs,ntag,
     $  tablea)

c will only remap reports that meet all of the following criteria:
c - bulletin originator is in list passed in via array borgs
c - 7-digit report id has "00" in characters 3:4
c - WMO Marine Observing Platform extended identifier (WMOP) not missing
C

      real*8 rpid_8,borg_8,rpid_out_8,wmop_8,bpid_8,obs_8(30),
     $       dattim_8(5,2),obs_repl_8(3,255),obs_repl_out_8(5,255)
      real*8 xtemp_8
      REAL(8) BMISS,GETBMISS,GETVALNB
      integer*4 ibfms

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
