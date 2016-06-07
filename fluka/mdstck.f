*$ CREATE MDSTCK.FOR
*COPY MDSTCK
*
*=== Mdstck ===========================================================*
*
      SUBROUTINE MDSTCK ( IFLAG, NPSECN )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1998-2007      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Created on 04 october  2013  by    P. Garcia Ortega              *
*                                                   CERN               *
*                                                                      *
*     Last change on 18-jan-14     by    A.Mereghetti & P.Garcia Ortega*
*                                                                      *
*                                                                      *
*     New version of Mdstck:  flagging interesting events              *
*                             1. for creating CollTrack-like maps, ie  *
*                                listing position of protons undergoing*
*                                inelastic events;                     *
*                             2. kill secondaries (but not in case of  *
*                                single diffractive events);           *
*                             3. count number of elastic and single    *
*                                diffractive events;                   *
*                                                                      *
*                                                                      *
*        Iflag = 1: standard Kaskad call                               *
*              = 2: Kaskad call after elastic (but hydrogen)           *
*              = 3: Kasneu call                                        *
*              = 4: Emfsco call                                        *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(EMFSTK)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(GENSTK)'
      INCLUDE '(TRACKR)'

*  +-------------------------------------------------------------------*
*  |  Despite the presence of the integer flag in the interface, it's
*  |  better to retrieve the type of event from the EVTFLG common
*  |  NOTA BENE: flagging single diffractive events:
*  |  LEVDIF:
*  |    TRUE  ->     a diffractive event;
*  |    FALSE -> NOT a diffractive event;
*  |  LPRDIF:
*  |    TRUE  -> PROJ   is preserved (and target is excited);
*  |    FALSE -> TARGET is preserved (and proj   is excited);
      INCLUDE '(EVTFLG)'
      INCLUDE '(BALANC)'
      INCLUDE '(BEAMCM)'
      INCLUDE '(COUCNF)'
      INCLUDE '(FLKAIO)'
      INCLUDE '(TRFLIB)'
*  HISIX REQUIRES PARPROP FOR THE WRITEOUT OF A,Z ; PH.
      INCLUDE '(PAPROP)'  


      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /


*  +-------------------------------------------------------------------*
*  |  First call initialisations:
      IF ( LFIRST ) THEN
         WRITE( LUNOUT, * )' '
         WRITE( LUNOUT, * )' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE( LUNOUT, * )' !!  mdstck by P.G.Ortega, 2013-10-04    !!'
         WRITE( LUNOUT, * )' !!      and A.Mereghetti, 2014-01-18    !!'
         WRITE( LUNOUT, * )' !!             first call               !!'
         WRITE( LUNOUT, * )' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE( LUNOUT, * )' '
         WRITE( LUNOUT, * )' to perform flagging of interesting events'
         WRITE( LUNOUT, * )' '
         LFIRST = .FALSE.
      END IF
*  |  end of initialisation
*  +-------------------------------------------------------------------*

*  +-------------------------------------------------------------------*
*  |  Identify particles with the same ID of the beam
* am & ph: identify if particle is of the main beam and if it is a heavy ion
* AMPH ->       IF ( LTRACK .GT. 1 ) RETURN
*
* PH: with off for the moment
*      IF ( JTRACK .NE. 999 ) RETURN
      IF ( JTRACK .GT. 1 .OR. JTRACK .LT. -6 ) RETURN
*
* AMPH ->       IF ( JTRACK .EQ. IJBEAM ) THEN
*  |  +----------------------------------------------------------------*
*  |  |  Reproduce CollTrack-like loss maps, ie dump beam particles
*  |  |  undergoing any inelastic interaction
         IF ( LCRLSM ) THEN
            IF ( LINEVT ) THEN   
               LSDFLG = 1
*  |  |  +-------------------------------------------------------------*
*  |  |  |  Flag a single diffractive event
               IF ( LEVDIF .AND. LPRDIF ) THEN 
                  LSDFLG = 4
               END IF
* HISIX     ELECTROMAGNETIC DISSOCIATION:
            ELSEIF ( LELDIS ) THEN
               LSDFLG = 11
* PH: HISIX: INCLUDE ELASTIC INTERACTION
            ELSEIF ( LELEVT ) THEN
               LSDFLG = 2
            END IF

            NPOINT = 1
*  |  |  |  Extraction roto-translation for coordinates
            XX     = XTRACK(NTRACK)
            YY     = YTRACK(NTRACK)
            ZZ     = ZTRACK(NTRACK)
            CALL DOTRSF( NPOINT, XX, YY, ZZ, IDXEXT(ICPPNT) )
*  |  |  |  Extraction rotation for direction cosines
            TXX    = CXTRCK
            TYY    = CYTRCK
            TZZ    = CZTRCK
            CALL DORTNO( NPOINT, TXX, TYY, TZZ, IDXEXT(ICPPNT) )

            IF ( LCRCLT ) THEN
*  |  |  |  +----------------------------------------------------------*
*  |  |  |  |  Lossmap format in CollTrack-like format
*  |  |  |  |  Origin of losses at begining of jaw
*  |  |  |  |  ph: modified for hisix, write out energy,A,Z
               ZZ1 = ZZ*1.0D-02 - SDSINJ (ICPPNT)
*               WRITE( LUNLSM, 1983) 
*     &              ICPPNT, 0.0D+00, ZZ1, XX*1.D+01, 
*     &              TXX/TZZ*1.0D+03, YY*1.0D+01, TYY/TZZ*1.0D+03, 
*     &              LSDFLG, IDPGEN, ICPTUR
*               WRITE( LUNLSM, 1982) 
*     &              ICPPNT, IDPIDP, IDPGEN, ZZ*1.0D-02, XX*1.D+01, 
*     &              TXX/TZZ*1.0D+03, YY*1.0D+01, TYY/TZZ*1.0D+03, 
*     &              ETRACK, LSDFLG, ICPTUR, IBARCH(JTRACK),
*     &              ICHRGE(JTRACK)
* hack to test hisix
               WRITE( LUNLSM, 1982) 
     &              ICPPNT, IDPIDP, IDPGEN, ZTRACK(NTRACK), 
     &              XTRACK(NTRACK), CXTRCK, YTRACK(NTRACK), 
     &              CYTRCK,
     &              ETRACK, LSDFLG, ICPTUR, IBARCH(JTRACK),
     &              ICHRGE(JTRACK),IFLAG
            ELSE
*  |  |  |  +----------------------------------------------------------*
*  |  |  |  |  Standard lossmap format
               WRITE( LUNLSM, 1982) 
     &              ICPPNT, IDPIDP, IDPGEN, ZZ*1.0D-02, XX*1.D+01, 
     &              TXX/TZZ*1.0D+03, YY*1.0D+01, TYY/TZZ*1.0D+03, 
     &              ETRACK, LSDFLG, ICPTUR, IBARCH(JTRACK),
     &              ICHRGE(JTRACK),IFLAG
*PH: Etrack
            END IF
* AMPH ->          END IF
*  |  |
*  |  +----------------------------------------------------------------*


*  |  +----------------------------------------------------------------*
*  |  Count number of elastic and single diffractive events
      IF ( LCRPRS ) THEN
*  |  +-------------------------------------------------------------*
*  |  |  A primary particle has LTRACK=1, unless it underwent 
*  |  |  single diffractive events, e.g. ISPUSR(2)=1 and LTRACK=2
         IF ( LTRACK-1 .EQ. ISPUSR(2) ) THEN
*  |  |  +----------------------------------------------------------*
*  |  |  |  Elastic and quasi-elastic events
            IF ( LELEVT ) THEN
               ISPUSR(1) = ISPUSR(1) +1
*  |  |  +----------------------------------------------------------*
*  |  |  |  Single diffractive events
            ELSE IF ( LINEVT .AND. LEVDIF .AND. LPRDIF ) THEN 
               ISPUSR(2) = ISPUSR(2) +1
            END IF
*  |  |  |
*  |  |  +----------------------------------------------------------*
         END IF
*  |  |
*  |  +-------------------------------------------------------------*
      END IF


*  |  +----------------------------------------------------------------*
*  |  Kill the secondaries
      IF ( LSKILL ) THEN
*  |  +-------------------------------------------------------------*
*  |  |  proton beam: only in case of any inelastic event being
*  |  |               NOT a single diffractive:
         IF (IJBEAM.EQ.1 .AND. JTRACK.EQ.IJBEAM .AND. LTRACK.GT.1) THEN
            IF ( LINEVT .AND. .NOT. ( LEVDIF .AND. LPRDIF ) ) THEN 
               DO II = 1, NP
                  WEI( II ) = ZERZER
               END DO
            END IF
*  |  +-------------------------------------------------------------*
*  |  |  ion beam: let's kill whatever is not a proton or another ion
         ELSE
            DO II = 1, NP
               IF ( KPART(II).GT.1 .OR. KPART(II).LT.-6 ) THEN
                  WEI( II ) = ZERZER
               END IF
            END DO
         END IF
*  |  |
*  |  +-------------------------------------------------------------*
      END IF

      END IF

      RETURN
* 1982 FORMAT ( 3(1X,I6), 6(1X,1PE15.8), 2(1X,I6) )
 1982 FORMAT ( 3(1X,I6), 6(1X,1PE15.8), 2(1X,I6), 3(1X,I5) )
* 1983 FORMAT ( 1(1X,I6), 6(1X,1PE15.8), 3(1X,I6) ) hisix
 1983 FORMAT ( 1(1X,I6), 7(1X,1PE15.8), 5(1X,I6) )
*=== End of subroutine mdstck =========================================*
      END

