*$ CREATE FLUSCW.FOR
*COPY FLUSCW
*                                                                      *
*=== Fluscw ===========================================================*
*                                                                      *
      DOUBLE PRECISION FUNCTION FLUSCW ( IJ    , PLA   , TXX   , TYY   ,
     &                                   TZZ   , WEE   , XX    , YY    ,
     &                                   ZZ    , NREG  , IOLREG, LLO   ,
     &                                   NSURF )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1989-2005      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*     Author(s):  A. Mereghetti & P.G. Ortega                          *
*                                                                      *
*     Created on 22 october  2010  by    A. Mereghetti, A. Lechner &   *
*                                        D. Sinuela Pastor             *
*                                                 CERN                 *
*                                                                      *
*     Last change on 24-aug-15     by    A.Mereghetti & P.Hermes       *
*                                        F.Cerutti & V.Vlachoudis      *
*                                                                      *
*                                                                      *
*     New version of Fluscw:   send particles back to the Tracker      *
*                              and finalise some online analysis       *
*                                                                      *
*                                                                      *
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *
*     !!! This is a completely dummy routine for Fluka9x/200x. !!!     *
*     !!! The  name has been kept the same as for older  Fluka !!!     *
*     !!! versions for back-compatibility, even though  Fluscw !!!     *
*     !!! is applied only to estimators which didn't exist be- !!!     *
*     !!! fore Fluka89.                                        !!!     *
*     !!! User  developed versions  can be used for  weighting !!!     *
*     !!! flux-like quantities at runtime                      !!!     *
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *
*                                                                      *
*     Input variables:                                                 *
*                                                                      *
*           Ij = (generalized) particle code (Paprop numbering)        *
*          Pla = particle laboratory momentum (GeV/c) (if > 0),        *
*                or kinetic energy (GeV) (if <0 )                      *
*    Txx,yy,zz = particle direction cosines                            *
*          Wee = particle weight                                       *
*     Xx,Yy,Zz = position                                              *
*         Nreg = (new) region number                                   *
*       Iolreg = (old) region number                                   *
*          Llo = particle generation                                   *
*        Nsurf = transport flag (ignore!)                              *
*                                                                      *
*     Output variables:                                                *
*                                                                      *
*       Fluscw = factor the scored amount will be multiplied by        *
*       Lsczer = logical flag, if true no amount will be scored        *
*                regardless of Fluscw                                  *
*                                                                      *
*     Useful variables (common SCOHLP):                                *
*                                                                      *
*     Flux like binnings/estimators (Fluscw):                          *
*          ISCRNG = 1 --> Boundary crossing estimator                  *
*          ISCRNG = 2 --> Track  length     binning                    *
*          ISCRNG = 3 --> Track  length     estimator                  *
*          ISCRNG = 4 --> Collision density estimator                  *
*          ISCRNG = 5 --> Yield             estimator                  *
*          JSCRNG = # of the binning/estimator                         *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(SCOHLP)'
      INCLUDE '(USRBDX)'
      INCLUDE '(PAPROP)'
      INCLUDE '(BEAMCM)'
      INCLUDE '(TRACKR)'
      INCLUDE '(FLKAIO)'
      INCLUDE '(COUCNF)'
      INCLUDE '(TRFLIB)' 

      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /

*  +-------------------------------------------------------------------*
*  |  special sdum for labelling extraction points
      CHARACTER*10 SPCSDM
      SAVE SPCSDM
      DATA SPCSDM / "BACK2ICO" /

*     HISIX: send back protons to the tracker
      CHARACTER*10 HSPRTS
      SAVE HSPRTS
      DATA HSPRTS / "BACK2ICOP" /

*  +-------------------------------------------------------------------*
*  |  First call initialisations:
      IF ( LFIRST ) THEN
         WRITE( LUNOUT, * )' '
         WRITE( LUNOUT, * )' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE( LUNOUT, * )' !!  fluscw by A.Mereghetti, A. Lechner, !!'
         WRITE( LUNOUT, * )' !!      D. Sinuela Pastor and P. Garcia !!'
         WRITE( LUNOUT, * )' !!      Ortega, 2014-01-18              !!'
         WRITE( LUNOUT, * )' !!             first call               !!'
         WRITE( LUNOUT, * )' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE( LUNOUT, * )' '
         WRITE( LUNOUT, * )' to send particles back to the Tracker and '
         WRITE( LUNOUT, * )'    perform some online analysis;'
         WRITE( LUNOUT, * )' '
         LFIRST = .FALSE.
      END IF
*  |  end of initialisation
*  +-------------------------------------------------------------------*


*  +-------------------------------------------------------------------*
*  |  Send back particles through USRBDX cards...
*     ISCRNG = boundary crossing estimator
      IF ( ISCRNG .EQ.1 ) THEN


*  hisix: use last character of SDUM to specifify if protons should be sent back or not
*  |  +----------------------------------------------------------------*
*  |  |  ...the SDUM of which is the special 
         IF ( TITUSX(JSCRNG)(1:8) .EQ. SPCSDM ) THEN
*  |  |  +-------------------------------------------------------------*
*  |  |  |  Select only protons and ions (hisix)...
*           Hisix: count the energy of the particles leaving the collimators
            ESCO = -PLA + AM(IJ)
*           hisix: check if protons should be sent back to the tracker
            IF ( TITUSX(JSCRNG) .EQ. HSPRTS ) THEN
               PROTSQ=1
               WRITE(*,*), 'HISIX: PROTSQ=1'
            ELSE
*           DONT SEND BACK THE PROTONS               
               PROTSQ=0
               WRITE(*,*), 'HISIX: PROTSQ=1'
            END IF
*           HISIX: force the suppression of protons for the moment            
            PROTSQ=0
*           hisix: dump all particles which leave the collimator but which are not 
*                  sent back to the tracker
            IF ( IJ .GT. PROTSQ .OR. IJ .LT. -6 ) THEN
               WRITE(66,*) 
     &                 IJ, IBARCH(IJ), ICHRGE(IJ),ICPPNT,ESCO
            END IF
*           hisix: don't send back to tracker
            IF ( IJ .GT. PROTSQ .OR. IJ .LT. -6 ) RETURN

*              INCLUDE PROTONS IN HISIX
*               IF ( IJ .GT. 1 .OR. IJ .LT. -6 ) THEN
*                  WRITE(66,*) 
*     &                 IJ, IBARCH(IJ), ICHRGE(IJ),ICPPNT,ESCO
*               END IF
*               IF ( IJ .GT. 1 .OR. IJ .LT. -6 ) RETURN
*            ELSE
*               IF ( IJ .GT. 0 .OR. IJ .LT. -6 ) THEN
*                  WRITE(66,*) 
*     &                 IJ, IBARCH(IJ), ICHRGE(IJ),ICPPNT,ESCO
*               END IF
*               IF ( IJ .GT. 0 .OR. IJ .LT. -6 ) RETURN
*            END IF

*           hisix: include protons
*           
*           hisix: exclude protons
*           IF ( IJ .GT. 0 .OR. IJ .LT. -6 ) THEN
*           PH: write out all particles
*          
*            WRITE(66,*) 
*     &           IJ, IBARCH(IJ), ICHRGE(IJ),ICPPNT,ESCO
*           END IF
*          exclude protons
*           IF ( IJ .GT. 0 .OR. IJ .LT. -6 ) RETURN
*          include protons
*           IF ( IJ .GT. 1 .OR. IJ .LT. -6 ) RETURN
*
*            IF ( IJ .GT. 1 .OR. IJ .LT. -6 ) RETURN
*           (PLA is kinetic energy [GeV] (PLA<0), ESCO is total energy)
*
*            ESCO = -PLA + AM(IJ)
*
*            WRITE(128,*)
*     &  IJ, IBARCH(IJ), ICHRGE(IJ), ICPPNT, ESCO 
*       ICPPNT : index of collimator
*            WRITE(66,*) 
*     &           IJ, IBARCH(IJ), ICHRGE(IJ),ICPPNT,ESCO
**           ...in the selected magnetic rigidity range (hisix)
*            IF ( ABS (SQRT (-PLA * (ESCO+AM(IJ))) / ICHRGE(IJ) - BRHONO)
*     &           .GT. AUSBDX(JSCRNG) * BRHONO ) RETURN
*           If generation bigger than 1 generate new particle id
            IF ( LLO .GT. 1 ) THEN
*  |  |  |  +----------------------------------------------------------*
*  |  |  |  |  Save IDP and IDGEN of parent:
               IDP0   = IDPIDP
               IDGEN0 = IDPGEN
*  |  |  |  +----------------------------------------------------------*
*  |  |  |  |  Generate new IDP, assign it. Update parent ID if required
               IF ( .NOT. LCRCLT ) IDPGEN = IDPIDP      
               ICPIDM = ICPIDM+1 
               IDPIDP = ICPIDM   
            END IF
*  |  |  |
*  |  |  +-------------------------------------------------------------*

*  |  |  +-------------------------------------------------------------*
*  |  |  |  Transformation for extraction point   
            NPOINT = 1
*  |  |  |  For coordinates, full roto-translation
            CALL DOTRSF ( NPOINT, XX, YY, ZZ, IDXEXT(ICPPNT) )
*  |  |  |  For direction cosines, only the rotation
            CALL DORTNO ( NPOINT, TXX, TYY, TZZ, IDXEXT(ICPPNT) )
*  |  |  |
*  |  |  +-------------------------------------------------------------*


*  |  |  +-------------------------------------------------------------*
*  |  |  |  send particle back to tracker
            N = NTSENDP(
     &                JFIOCD,
     &                IDPIDP, IDPGEN, WEE,
     &                XX, YY, ZZ,
     &                TXX, TYY, TZZ,
     &                IBARCH(IJ), ICHRGE(IJ),
     &                AM(IJ), ESCO, ATRACK )
            IF ( N .LT. 0 ) THEN
               WRITE( LUNOUT, * ) ' *** Error sending particle *** '
               CALL FLUSH( LUNOUT )
*  |  |  |  |  maybe here stop the simulation?
            END IF
*  |  |  |
*  HISIX: dump all sent particles to fort.66 Nreg = (new) region number                                   *

*  |  |  +-------------------------------------------------------------*


*  |  |  +-------------------------------------------------------------*
*  |  |  |  count number of elastic and single diffractive 
*  |  |  |  events undergone by a primary particle, and
*  |  |  |  measure the total energy lost by a primary particle;
            IF ( LCRPRS ) THEN
*  |  |  |  +----------------------------------------------------------*
*  |  |  |  |  count number of elastic and single diffractive 
               IF ( LLO.EQ.1 ) THEN 
*  |  |  |  |  +-------------------------------------------------------*
*  |  |  |  |  |  a beam particle (no single diffractive event)
                  IF ( ISPUSR(1).GT.0 .OR. ESCO.LT.SPAUSR(1) ) THEN
*  |  |  |  |  |  +----------------------------------------------------*
*  |  |  |  |  |  |  elastic scattering or simple loss of energy
*  |  |  |  |  |  |  through ionisation:
                     WRITE( LUNPRS, 1982 ) ICPTUR,ICPPNT,IDPIDP,IDPGEN,
     &                          ISPUSR(1),ISPUSR(2),ESCO,SPAUSR(1)-ESCO
                  END IF
               ELSE IF ( LLO-1.EQ.ISPUSR(2) ) THEN
*  |  |  |  |  +-------------------------------------------------------*
*  |  |  |  |  |  at least a single diffractive event:
*  |  |  |  |  |  a primary particle has LLO=1, unless it underwent 
*  |  |  |  |  |  single diffractive events, e.g. ISPUSR(2)=1 and 
*  |  |  |  |  |  LLO=2
                  WRITE( LUNPRS, 1982 ) ICPTUR,ICPPNT,IDPIDP,IDPGEN,
     &                          ISPUSR(1),ISPUSR(2),ESCO,SPAUSR(1)-ESCO
               END IF
*  |  |  |  |
*  |  |  |  +----------------------------------------------------------*
            END IF
*  |  |  |
*  |  |  +-------------------------------------------------------------*
   

            IF ( LLO .GT. 1 ) THEN
*  |  |  |  +----------------------------------------------------------*
*  |  |  |  |  restore IDP and IDGEN of parent:
               IDPIDP = IDP0
               IDPGEN = IDGEN0
            END IF
*  |  |  |
*  |  |  +-------------------------------------------------------------*


         END IF
*  |  |  close IF ( TITUSX(JSCRNG) .EQ. SPCSDM ) THEN
*  |  +----------------------------------------------------------------*


      END IF
*  |  close IF ( ISCRNG .EQ.1 ) THEN
*  +-------------------------------------------------------------------*


      FLUSCW = ONEONE
      LSCZER = .FALSE.

      RETURN
 1982 FORMAT ( 6(1X,I6), 2(1X,1PE24.17) )
*=== End of function fluscw ===========================================*
      END
