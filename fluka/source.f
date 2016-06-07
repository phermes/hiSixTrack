*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== Source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*     
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2012      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*     Created on 01 march   2010   by    A. Mereghetti, V. Vlachoudis, *
*                                        F. Cerutti & D. Sinuela Pastor*
*                                                 CERN                 *
*                                                                      *
*     Last change on 24-aug-15     by    A.Mereghetti & P.Hermes       *
*                                        F.Cerutti & V.Vlachoudis      *
*                                                                      *
*                                                                      *
*     New version of Source : load particles received from the Tracker *
*                                                                      *
*                                                                      *
*     Debugging messages concerning the transformation of injection/   *
*        extraction transformations in FLUKA can be triggered by means *
*        of WHASOU( 6): if positive, dump messages are written;        *
*                                                                      *
*                                                                      *
*     The SOURCE cards can be used also for triggering some event by   *
*     event analysis - see also Twiki page:                            *
*       https://twiki.cern.ch/twiki/bin/view/FlukaTeam/                *
*                                              CouplingSVNRepositories *
*                                                                      *
*     - reproduction of loss maps as obtained with a pure CollTrack    *
*       simulation:                                                    *
*       WHASOU( 7): trigger (<0: dump loss map standard format,        *
*                            >0: dump loss map CollTrack-like format); *
*       WHASOU( 8): logical unit where to dump (no internal check);    *
*                                                                      *
*     - kill secondary particles in case of any inelastic interaction, *
*       but single diffractive (a single diffractive proton is actually*
*       a secondary particle):                                         *
*       WHASOU( 9): trigger (>0: kill);                                *
*                                                                      *
*     - count number of elastic and single diffractive events undergone*
*       by a primary particle, and measure the total energy it lost:   *
*       WHASOU(10): trigger (>0: dump);                                *
*       WHASOU(11): logical unit where to dump (no internal check);    *
*       WHASOU(12): kinetic energy threshold ([%] wrt the kinetic      *
*                   energy of the particle undergoing the single       *
*                   diffractive event), above which a secondary        *
*                   particle (of the same type of the beam) is         *
*                   considered as the single diffractive primary       *
*                   surviving the interaction;                         *
*                                                                      *
*     - dump population of received particles:                         *
*       WHASOU(13): min ID to dump (<=0: dump all);                    *
*       WHASOU(14): max ID to dump (<=0: dump all);                    *
*       WHASOU(15): logical unit where to dump (no internal check);    *
*       WHASOU(16): dump frequency (number of turns);                  *
*       the trigger is WHASOU(16)>0;                                   *
*                                                                      *
*     - statistics on received particles:                              *
*       WHASOU(17): logical unit where to dump (no internal check);    *
*       WHASOU(18): dump frequency (number of turns);                  *
*       the trigger is WHASOU(18)>0;                                   *
*                                                                      *
*                                                                      *
*     Free WHASOU: 1-5;                                                *
*                                                                      *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*

      INCLUDE '(BEAMCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(IOIOCM)'
      INCLUDE '(LTCLCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SOURCM)'
      INCLUDE '(SUMCOU)'

      INCLUDE '(FLKAIO)'
      INCLUDE '(COUCNF)'
      INCLUDE '(TRFLIB)'

      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /

*  +-------------------------------------------------------------------*
*  |  Flag for activating statistics about received particles
      LOGICAL LSTPRT
      SAVE LSTPRT
      DATA LSTPRT / .FALSE. /
*  |  Flag for activating the runtime change of transformations
      LOGICAL LCKRTD
      SAVE LCKRTD
      DATA LCKRTD / .FALSE. /

*  +-------------------------------------------------------------------*
*  |  Name of the file stopping the simulation
      CHARACTER*13 STPNAM
      SAVE STPNAM
      DATA STPNAM / 'coupling.stop' /
*  |  Name of file storing network configuration information
      CHARACTER*11 NFONAM
      SAVE NFONAM
      DATA NFONAM / 'network.nfo' /
*  |  Name of file where to dump CollTrack-like loss maps
      CHARACTER*11 LOSNAM
      SAVE LOSNAM
      DATA LOSNAM / 'lossMap.dat' /
*  |  Name of file where to dump number of elastic and sing diff event
*  |  undergone by a primary particle, and measure the total energy
*  |  it lost:
      CHARACTER*15 SCCNAM
      SAVE SCCNAM
      DATA SCCNAM / 'scattCounts.dat' /
*  |  Name of file where to dump received particles
      CHARACTER*12 RECNAM
      SAVE RECNAM
      DATA RECNAM / 'received.dat' /
*  |  Name of file where to dump the statistics on received particles
      CHARACTER*10 STTNAM
      SAVE STTNAM
      DATA STTNAM / 'MEAN_SIGMA' /
*  |  Name of file configuring the runtime change of transformations
      CHARACTER*14 CNFCRT
      SAVE CNFCRT
      DATA CNFCRT / "cngRotDefi.nfo" /
*  |  Name of file configuring the injection/extraction transformations
      CHARACTER*14 TRFNAM
      SAVE TRFNAM
      DATA TRFNAM / "insertion.txt" /

*  +-------------------------------------------------------------------*
*  |  Additional variables for the dump of received particles
*  |     Lrpprt = trigger
*  |     Lcrprt = flag for dumping the whole beam or just a subset
*  |     Irpuni = logical unit where to dump
*  |     Nrptur = dump frequency [# turns]
*  |     Idrmin = ID min of the population \ / in case of a subset of
*  |     Idrmax = ID max of the population / \   the tracked population
      LOGICAL LRPPRT
      LOGICAL LCRPRT
      SAVE IRPUNI, NRPTUR, IDRMIN, IDRMAX, LRPPRT, LCRPRT

*  +-------------------------------------------------------------------*
*  |  Temporary variables
      LOGICAL LEXIST
      CHARACTER*40 TMPNAM

*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
      IERR   = 0
*  +-------------------------------------------------------------------*
*  |  First call initialisations:
      IF ( LFIRST ) THEN
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.


*  |  +----------------------------------------------------------------*
*  |  |  Server Initialisation
         CALL NTINIT()
         JFIOSD = NTSERVER()
         IF ( JFIOSD .LT. 0 ) THEN
            WRITE( LUNOUT, * ) ' *** Error creating server ***'
            GO TO 4300
         END IF
*  |  |  
*  |  +----------------------------------------------------------------*
*  |  |  Get port number where the server is listening
         IPORT = NTSTART(JFIOSD, IPORT)
         IF ( IPORT .LT. 0 ) THEN
            WRITE( LUNOUT, * ) ' *** Error initializing server ***'
            GO TO 4200
         END IF
*  |  |  
*  |  +----------------------------------------------------------------*
*  |  |  Notify lunout
         WRITE(LUNOUT,*) ' '
         WRITE(LUNOUT,*) ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE(LUNOUT,*) ' !!       FlukaIO server initialized     !!'
         WRITE(LUNOUT,*) ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE(LUNOUT,*) ' '
         WRITE(LUNOUT,*) ' *** Listening on port ', IPORT,' ***'
         WRITE(LUNOUT,*) ' '
         WRITE(LUNOUT,*) " *** To stop the simulation, touch file: '"//
     &   STPNAM//"' *** "
         WRITE(LUNOUT,*) ' '
*  |  |  
*  |  +----------------------------------------------------------------*
*  |  |  Write server parameters
         CALL OAUXFI( '../'//NFONAM, LUNRDB, 'APPEND', IERR )
         IF ( IERR .GT. 0 ) THEN
            WRITE( LUNOUT, * ) ''
            WRITE( LUNOUT, * ) ' SOURCE: error while opening file: '//
     &NFONAM
            WRITE( LUNOUT, * ) ''
            GO TO 4100
         END IF
         WRITE( LUNRDB, * ) IPORT
         CLOSE( LUNRDB )
*  |  |  
*  |  +----------------------------------------------------------------*
*  |  |  Accept network connection
         WRITE( LUNOUT, * ) ' *** Waiting for connection ***'
         JFIOCD = NTACCEPT( JFIOSD )
         IF ( JFIOCD .LT. 0 ) THEN
           WRITE( LUNOUT, * ) ' *** Error accepting connection *** '
           CALL FLUSH( LUNOUT )
           GO TO 4100
         END IF
         WRITE( LUNOUT, * ) ' *** Connection accepted ***'
         CALL FLUSH( LUNOUT )
*  |  |  End of server initialisation
*  |  +----------------------------------------------------------------*

*  |  +----------------------------------------------------------------*
*  |  |  Starting value of maximum IDP
*  |  |  value synchronised with starting number of particles being
*  |  |      tracked in SixTrack
*  |  |  message managed by FlukaIO, which passes the maximum number of
*  |  |      particles
         ICPIDF = NTNPART( JFIOCD )
         IF (ICPIDF .LT. 0) THEN
           WRITE( LUNOUT, * ) ' *** Error receiving ICPIDF  *** '
           CALL FLUSH( LUNOUT )
           GO TO 4100
         END IF
         WRITE( LUNOUT, *) ' *** ICPIDF = ',ICPIDF,' ***'
*  |  |
*  |  +----------------------------------------------------------------*


*  |  +----------------------------------------------------------------*
*  |  |  Nominal Brho for hisix
*  |  |  message managed by FlukaIO
*         IERR = NTBRHO( JFIOCD, BRHONO )
*         IF (IERR .LT. 0) THEN
*           WRITE( LUNOUT, * ) ' *** Error receiving BRHONO  *** '
*           CALL FLUSH( LUNOUT )
*           GO TO 4100
*         END IF
*         WRITE( LUNOUT, *) ' *** BRHONO = ',BRHONO,' ***'
*  |  |
*  |  +----------------------------------------------------------------*


*  |  +----------------------------------------------------------------*
*  |  |  Configuration from source cards
*  |  |  1.
*  |  |  Debugging messages about the transformations used
*  |  |  to inject/extract particles into/from FLUKA
         LCPDBG = .FALSE.
         IF ( WHASOU( 6) .GT. ZERZER ) THEN
            WRITE( LUNOUT, * ) ' *** Debugging transformations of '//
     & 'injected/extracted particles ***'
            LCPDBG = .TRUE.
         END IF
         CALL FLUSH( LUNOUT )
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  2.
*  |  |  Reproduction of loss maps as obtained with CollTrack
*  |  |  (see header of source routine and (COUCNF))
         LUNLSM = 0
         LCRLSM = .FALSE.
         LCRCLT = .FALSE.
         IF ( WHASOU( 7) .NE. ZERZER ) THEN
            LUNLSM = INT( WHASOU( 8) + 1.0D-04 )
            LCRLSM = .TRUE.
            IF ( WHASOU( 7) .GT. ZERZER ) LCRCLT = .TRUE.
            CALL OAUXFI( LOSNAM, LUNLSM, 'NEW', IERR )
            IF ( IERR .GT. 0 ) THEN
               WRITE( LUNOUT, * ) ''
               WRITE( LUNOUT, * ) ' SOURCE: error while opening file: '
     &//LOSNAM
               WRITE( LUNOUT, * ) ''
               GO TO 4100
            END IF
            IF ( LCRCLT) THEN
*               WRITE( LUNLSM, '(1(1X,A6),6(1X,A15),3(1X,A6))' )
*     &         '# IPNT', 'ANGLE', 'S[m]', 'X[mm]','XP[mrad]', 
*     &         'Y[mm]', 'YP[mrad]', 'LSDFLG', 'IDP', 'ITURN'
*  |  +----------------------------------------------------------------*
*  |  |  hisix: write out A,Z,Energy of the particle
*  |  |  
*               WRITE( LUNLSM, '(1(1X,A6),6(1X,A15),5(1X,A8))' )
*     &         '# IPNT', 'ANGLE', 'S[m]', 'X[mm]','XP[mrad]', 
*     &         'Y[mm]', 'YP[mrad]', 'LSDFLG', 'IDP', 'ITURN',
*     &         'A', 'Z', 'E[GeV]'
               WRITE( LUNLSM, '(3(1X,A6),6(1X,A15),2(1X,A6))' )
     &         '# IPNT', 'IDP','IDPGEN', 'S[m]', 'X[mm]','XP[mrad]', 
     &         'Y[mm]', 'YP[mrad]', 'ENERGY', 'LSDFLG', 'ITURN'
            ELSE
*               WRITE( LUNLSM, '(3(1X,A6),6(1X,A15),2(1X,A6))' )
*     &         '# IPNT', 'IDP','IDPGEN', 'S[m]', 'X[mm]','XP[mrad]', 
*     &         'Y[mm]', 'YP[mrad]', 'ENERGY', 'LSDFLG', 'ITURN'
               WRITE( LUNLSM, '(3(1X,A6),6(1X,A15),2(1X,A6))' )
     &         '# IPNT', 'IDP','IDPGEN', 'S[m]', 'X[mm]','XP[mrad]', 
     &         'Y[mm]', 'YP[mrad]', 'ENERGY', 'LSDFLG', 'ITURN'
            END IF
            CALL FLUSH( LUNLSM )
         END IF
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  3.
*  |  |  Kill secondary particles from any inelastic interaction
*  |  |  (see header of source routine (COUCNF))
         LSKILL = .FALSE.
         IF ( WHASOU( 9) .GT. ZERZER ) THEN
            WRITE( LUNOUT, * ) ' *** Kill all the secondary particles'//
     & 'but in case of single diffractive events ***'
            LSKILL = .TRUE.
         END IF
         CALL FLUSH( LUNOUT )
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  4.
*  |  |  Count number of elastic and single diffractive events undergone
*  |  |  by a primary particle, and measure the total energy it lost
*  |  |  (see header of source routine and (COUCNF))
         LUNPRS = 0
         LCRPRS = .FALSE.
         ENFCUT = ZERZER
         IF ( WHASOU(10) .GT. ZERZER ) THEN
            LUNPRS = INT( WHASOU(11) + 1.0D-04 )
            LCRPRS = .TRUE.
            IF ( WHASOU(12) .GT. ZERZER ) THEN
               ENFCUT = WHASOU(12) / 100.0 ! from [%] to []
            END IF
            CALL OAUXFI( SCCNAM, LUNPRS, 'NEW', IERR )
            IF ( IERR .GT. 0 ) THEN
               WRITE( LUNOUT, * ) ''
               WRITE( LUNOUT, * ) ' SOURCE: error while opening file: '
     &//SCCNAM
               WRITE( LUNOUT, * ) ''
               GO TO 4100
            END IF
            WRITE(LUNPRS,*)'# count number of elastic and sing. diff.'//
     &                           ' events undergone by a prim. part.,'
            WRITE(LUNPRS,*)'#   and dump the tot. en. lost by a prim.'
            WRITE(LUNPRS,'(6(1X,A6),2(1X,A24))')
     &            '#ITURN', 'IPOINT', 'IDP', 'IDGEN', 'NELAST','NSINDF',
     &                                             'E [GeV]','DE [GeV]'
            CALL FLUSH( LUNPRS )
         END IF
         WRITE(LUNOUT,*)' '
         WRITE(LUNOUT,*)' *** variables for single event analysis ***'
         WRITE(LUNOUT,*)' - LCRLSM, LUNLSM           (loss maps):', 
     &                      LCRLSM, LUNLSM
         WRITE(LUNOUT,*)' - LCRPRS, LUNPRS, ENFCUT  (scattcount):', 
     &                      LCRPRS, LUNPRS, ENFCUT
         WRITE(LUNOUT,*)' - LSKILL (killing secondaries):', LSKILL
         WRITE(LUNOUT,*)' '
         CALL FLUSH( LUNOUT )
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  5.
*  |  |  Dump received particles (see header of source routine)
         LRPPRT = .FALSE.
         LCRPRT = .FALSE.
         IDRMIN = 0
         IDRMAX = 0
         IRPUNI = 0
         NRPTUR = 0
         IF ( WHASOU(16) .GT. ZERZER ) THEN
            IDRMIN = INT( WHASOU(13) + 1.0D-04 )
            IDRMAX = INT( WHASOU(14) + 1.0D-04 )
            LCRPRT = .TRUE.
            IF ( IDRMIN.LE.0 .OR. IDRMAX.LE.0 ) THEN
               IDRMIN = 0
               IDRMAX = 0
               LCRPRT = .FALSE.
            END IF
            IRPUNI = ABS( INT( WHASOU(15) + 1.0D-04 ) )
            NRPTUR = ABS( INT( WHASOU(16) + 1.0D-04 ) )
            LRPPRT = .TRUE.
            CALL OAUXFI( RECNAM, IRPUNI, 'NEW', IERR )
            IF ( IERR .GT. 0 ) THEN
               WRITE( LUNOUT, * ) ''
               WRITE( LUNOUT, * ) ' SOURCE: error while opening file: '
     &//RECNAM
               WRITE( LUNOUT, * ) ''
               GO TO 4100
            END IF
            WRITE(IRPUNI,*)'# dump of beam population received from'//
     &          ' the tracker. FLUKA units are used.'
            WRITE(IRPUNI,*)'# coordinates/direction cosines already'//
     &          ' set according to injection point transformation.'
            WRITE(IRPUNI,'(5(1X,A7),8(1X,A16))') '# ITURN', 'IPOINT',
     &           'IDP', 'IAA', 'IZZ', 'X [cm]', 'Y [cm]', 'Z [cm]',
     &           'TX []',  'TY []', 'TZ []', 'Etot [GeV]', 'DeltaT [s]'
            CALL FLUSH( IRPUNI )
         END IF
         WRITE(LUNOUT,*)' '
         WRITE(LUNOUT,*)' *** variables for dumping received part ***'
         WRITE(LUNOUT,*)' - LRPPRT, LCRPRT:', LRPPRT, LCRPRT
         WRITE(LUNOUT,*)' - IDRMIN, IDRMAX, IRPUNI, NRPTUR:', 
     &                      IDRMIN, IDRMAX, IRPUNI, NRPTUR
         WRITE(LUNOUT,*)' '
         CALL FLUSH( LUNOUT )
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  6.
*  |  |  Statistics on received particles
*  |  |  (see header of source routine and (COUCNF))
         ISTTUN = 0
         NSTTUR = 0
         IF ( WHASOU(18) .NE. ZERZER ) THEN
            ISTTUN = ABS( INT( WHASOU(17) + 1.0D-04 ) )
            NSTTUR = ABS( INT( WHASOU(18) + 1.0D-04 ) )
            LSTPRT = .TRUE.
            CALL OAUXFI( STTNAM, ISTTUN, 'NEW', IERR)
            IF ( IERR .GT. 0 ) THEN
               WRITE( LUNOUT, * ) ''
               WRITE( LUNOUT, * ) ' SOURCE: error while opening file: '
     &//STTNAM
               WRITE( LUNOUT, * ) ''
               GO TO 4100
            END IF
            WRITE( ISTTUN, * ) '# MEAN and SIGMA values'
            WRITE( ISTTUN, '(3(1X,A8),8(1X,A16))' )
     &                                   '#  ITURN', 'IPOINT', 'ICOUNT',
     &        'XMEAN  [cm]', 'YMEAN  [cm]', 'XPMEAN  []', 'YPMEAN  []' ,
     &        'XSIGMA [cm]', 'YSIGMA [cm]', 'XPSIGMA []', 'YPSIGMA []'
            CALL FLUSH( ISTTUN )
         END IF
         WRITE(LUNOUT,*)' '
         WRITE(LUNOUT,*)' *** variables for stat on received parts ***'
         WRITE(LUNOUT,*)' - LSTPRT:', LSTPRT
         WRITE(LUNOUT,*)' - ISTTUN, NSTTUR:', ISTTUN, NSTTUR
         WRITE(LUNOUT,*)' '
         CALL FLUSH( LUNOUT )
*  |  |  End of configuration from source cards
*  |  +----------------------------------------------------------------*


*  |  +----------------------------------------------------------------*
*  |  |  Transformations to be modified runtime (see header of source routine)
         TMPNAM = '../'//CNFCRT
         INQUIRE( FILE = TMPNAM, EXIST = LEXIST )
         IF ( LEXIST ) THEN
*  |  |  |  Initialise common
            CALL CTRINI
*  |  |  |  Parse configuration file
            CALL CTRPRS( TMPNAM, IERR )
            IF ( IERR .GT. 0 ) THEN
               WRITE( LUNOUT, * ) ''
               WRITE( LUNOUT, * ) ' SOURCE: error with file: '//TMPNAM
               WRITE( LUNOUT, * ) ''
               GO TO 4100
            END IF
*  |  |  |  Check that there is at least one changing ROT-DEFI
            CALL CTRCHK( LCKRTD )
*  |  |  |  A printout of all active changing ROT-DEFI
            IF ( LCKRTD ) THEN
               CALL CTRPSA( .TRUE. )
            ELSE
               WRITE( LUNOUT, * ) ''
               WRITE( LUNOUT, * ) " file '"//CNFCRT//"'"
               WRITE( LUNOUT, * ) ' does exist, but no transformation'//
     &       ' will be modified'
               WRITE( LUNOUT, * ) ''
            END IF
         ELSE
            WRITE( LUNOUT, * ) ''
            WRITE( LUNOUT, * ) " file '"//CNFCRT//"'"
            WRITE( LUNOUT, * ) ' does not exist: no transformation'//
     &       ' will be modified'
            WRITE( LUNOUT, * ) ''
         END IF
         CALL FLUSH( LUNOUT )
*  |  |  End of configuration of transformations to be modified runtime
*  |  +----------------------------------------------------------------*


*  |  +----------------------------------------------------------------*
*  |  |  Parse file for configurint the injection/extraction transformations
         WRITE( LUNOUT, * )' '
         WRITE( LUNOUT, * )' *** parsing file for ' //
     &       'injection/extraction transformations ***'
         WRITE( LUNOUT, * )' '
         CALL PRSTRF( TRFNAM , IERR )
         IF ( IERR .GT. 0 ) THEN
            WRITE( LUNOUT, * ) ''
            WRITE( LUNOUT, * ) ' SOURCE: error in file: '//TRFNAM
            WRITE( LUNOUT, * ) ''
            GO TO 4100
         END IF
         CALL FLUSH( LUNOUT )
*  |  |  
*  |  +----------------------------------------------------------------*


*  |  +----------------------------------------------------------------*
*  |  |  Other Initialization
         ITOTAL = 0
         ICPCNT = 0
         ICPTUR = 0
         ICPPNT = 0
*  |  |  Keep track of the previous turn
         ICPOTR = 0
*  |  |  Starting value of maximum IDP (generated by FLUKA):
*  |  |  value set equal to starting number of particles being
*  |  |      tracked in SixTrack, as received from FlukaIO;
         ICPIDM = ICPIDF


      END IF
*  |  End of initialisation
*  +-------------------------------------------------------------------*

*======================================================================*
*                                                                      *
*                 RECEIVE A MESSAGE                                    *
*                                                                      *
*======================================================================*

 1972 CONTINUE
      N = NTWAIT(
     &        JFIOCD,
     &        MTYPE,
     &        IDPIDP, IDPGEN, WGT,
     &        XCBEAM, YCBEAM, ZCBEAM,
     &        UCBEAM, VCBEAM, WCBEAM,
     &        IAA, IZZ, AMBEAM, ECBEAM, PTIME)
      IF ( N .LT. 0 ) THEN
         WRITE( LUNOUT, * ) ' *** Client timeout 1 ***'
         CALL FLUSH( LUNOUT )
*  |  |  Shouldn't I go to 4100?
         GO TO 4200
      END IF
*  |
*  +-------------------------------------------------------------------*
*  |  Message type: insertion point
      IF ( MTYPE .EQ. NFIOIP ) THEN
         GO TO 3000
      END IF
*  |
*  +-------------------------------------------------------------------*
*  |  Message type: particle
      IF ( MTYPE .EQ. NFIOPR ) THEN
         GO TO 3050
      END IF
*  |
*  +-------------------------------------------------------------------*
*  |  Message type: end of batch
      IF ( MTYPE .EQ. NFIOEB ) THEN
         GO TO 3100
      END IF
*  |
*  +-------------------------------------------------------------------*
*  |  Message type: close connection
      IF ( MTYPE .EQ. NFIOEC ) THEN
         GO TO 4000
      END IF
*  |
*  +-------------------------------------------------------------------*

*======================================================================*
*                                                                      *
*                 TURN / INSERTION POINT                               *
*                                                                      *
*======================================================================*

 3000 CONTINUE

*  +-------------------------------------------------------------------*
*  |  The user requested to stop the simulation by touching STPNAM file
      INQUIRE ( FILE = STPNAM, EXIST = LEXIST )
      IF ( LEXIST ) THEN
         WRITE( LUNOUT, * ) " *** '",STPNAM,' detected, ',
     &                            'ending simulation ***'
         GO TO 5000
      END IF

*  +-------------------------------------------------------------------*
*  |  Get the new turn number and insertion point
      ICPTUR = IDPGEN
      ICPPNT = IDPIDP
      WRITE( LUNOUT, * ) ' '
      WRITE( LUNOUT, 1981 )' *** New turn / insertion point: ', 
     &                  ICPTUR, ICPPNT

*  +-------------------------------------------------------------------*
*  |  If a new turn, update ALL changing ROT-DEFIs
      IF ( ICPTUR .NE. ICPOTR ) THEN
         IF ( LCKRTD ) CALL CTRUPA( ICPTUR, IERR )
         IF ( IERR .GT. 0 )  THEN
            WRITE( LUNOUT, * ) ''
            WRITE( LUNOUT, * ) ' SOURCE: error while updating '//
     &           'transformations to be modified runtime'
            WRITE( LUNOUT, * ) ''
            GO TO 5000
         END IF
      END IF

*  +-------------------------------------------------------------------*
*  |  Initialise statistics about received particles
      IF ( LSTPRT ) THEN
         IF ( NSTTUR.EQ.1 .OR. MOD( ICPTUR, NSTTUR ).EQ. 1 ) CALL STTINI
      END IF
*  |
*  +-------------------------------------------------------------------*
*  |  Receive a new message
      GO TO 1972

*======================================================================*
*                                                                      *
*                 NEW PARTICLE                                         *
*                                                                      *
*======================================================================*

 3050 CONTINUE

*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1
      ICPCNT = ICPCNT + 1
*  Wt is the weight of the particle
      WTFLK  (NPFLKA) = WGT
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
*  Particle type
*  +-------------------------------------------------------------------*
*  |  Heavy ion: (hisix)
      IF ( IAA .GT. 1 .AND. IZZ .GT. 0 ) THEN
         IJHION = IZZ  * 1000 + IAA
         IJHION = IJHION * 100 ! + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |
*  +-------------------------------------------------------------------*
*  |  proton:
      ELSE IF ( IAA .EQ. 1 .AND. IZZ .EQ. 1) THEN
         IONID = 1
         ILOFLK (NPFLKA) = 1
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
      ELSE
*       WRITE( LUNOUT, * )' *** which particle is it?, '//
*   &                    ,IAA,IZZ
         WRITE( LUNOUT, * )' *** which particle is it?, ',IAA,IZZ
         GO TO 5000 
      END IF
*
*     hisix
      IF ( ABS(AM(IONID)-AMBEAM) .GT. 1.D-12 * AMBEAM ) THEN
*         WRITE( LUNOUT, * )' *** mass mismatch, '//
*     &                    ,AM(IONID),AMBEAM
         WRITE( LUNOUT, * )' *** mass mismatch, ', AM(IONID), AMBEAM
         WRITE( LUNOUT, * )' *** mass difference, ', AM(IONID)-AMBEAM
         WRITE( LUNOUT, * )' *** reference mass:, ', AM(IONID)
*         hisix don't interrupt the simulation for the moment
*         GO TO 5000
      END IF
*  |
*  +-------------------------------------------------------------------*

* From this point .....
* Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
* AMPH -> *  No channeling:
* AMPH ->       KCHFLK (NPFLKA) = 0
* AMPH ->       ECRFLK (NPFLKA) = ZERZER
* AMPH -> *  Extra infos:
* AMPH ->       INFSTK (NPFLKA) = 0
* AMPH ->       ANFSTK (NPFLKA) = ZERZER
* AMPH -> *  Parent variables:
* AMPH ->       IPRSTK (NPFLKA) = 0
* AMPH ->       EKPSTK (NPFLKA) = ZERZER
*  User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
*  User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
*  Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
*  ... to this point: don't change anything

*  +-------------------------------------------------------------------*
*  |  Initialise counters of number of elastic and single diffractive 
*  |  events undergone by a primary particle, and
*  |  measure the total energy lost by a primary particle
*  |  (only in case of on-line analysis is requested by the user);
      IF ( LCRPRS ) THEN
*  |  |  number of elastic events
         ISPARK( 1, NPFLKA ) = 0
*  |  |  number of single diffractive events
         ISPARK( 2, NPFLKA ) = 0
*  |  |  initial total energy of the primary
         SPAREK( 1, NPFLKA ) = ECBEAM
*  |  |  mass of the primary
         SPAREK( 2, NPFLKA ) = AMBEAM
      END IF
*  |
*  +-------------------------------------------------------------------*

*  Particle age (s)
      AGESTK (NPFLKA) = PTIME
      AKNSHR (NPFLKA) = -TWOTWO
*  Kinetic energy of the particle (GeV)
*     NB: total energy [GeV] received from the tracker
      TKEFLK (NPFLKA) = ECBEAM - AM(IONID)
*  Particle momentum
      PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
     &                       + TWOTWO * AM(IONID) ) )

*  +-------------------------------------------------------------------*
*  |  Get the actual injection position / direction

*  Cosines (tx,ty,tz) (IF no rotation TXX=UCBEAM, TYY=V.. & TZZ=W.. )
*  Apply the injection transformation without translation
      NPOINT = 1
      TXX    = UCBEAM
      TYY    = VCBEAM
      TZZ    = WCBEAM
      CALL UNDRTO ( NPOINT, TXX, TYY, TZZ, IDXINJ(ICPPNT) )      
      TXFLK  (NPFLKA) = TXX
      TYFLK  (NPFLKA) = TYY
      TZFLK  (NPFLKA) = TZZ
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates (BEAMPOS coordinates not used!)
*  Apply the injection transformation 
      XX = XCBEAM
      YY = YCBEAM
      ZZ = ZCBEAM
      CALL UNDOTR ( NPOINT, XX, YY, ZZ, IDXINJ(ICPPNT) )      
      XFLK   (NPFLKA) = XX
      YFLK   (NPFLKA) = YY
      ZFLK   (NPFLKA) = ZZ
*  |
*  +-------------------------------------------------------------------*

*  +-------------------------------------------------------------------*
*  |  Dump received particles if requested
      IF ( LRPPRT ) THEN
*  |  +----------------------------------------------------------------*
*  |  |  But check it's the correct turn!
         IF ( NRPTUR.EQ.1 .OR. MOD( ICPTUR, NRPTUR ).EQ.1 ) THEN
*  |  |  +-------------------------------------------------------------*
*  |  |  |  Check if the whole beam has to be dumped, or if the present
*  |  |  |  primary particle falls in the range requested by the user:
            IF ( .NOT. LCRPRT .OR. ( LCRPRT .AND.
     &      ( IDPIDP.GE.IDRMIN .AND. IDPIDP.LE.IDRMAX ) ) ) THEN
               WRITE( IRPUNI, '(5(1X,I7),8(1X,1PE16.9))' )
     &                                 ICPTUR, ICPPNT, IDPIDP, IAA, IZZ,
     &             XFLK  ( NPFLKA ), YFLK  ( NPFLKA ), ZFLK  ( NPFLKA ),
     &             TXFLK ( NPFLKA ), TYFLK ( NPFLKA ), TZFLK ( NPFLKA ),
     &             ECBEAM          , AGESTK( NPFLKA )
               CALL FLUSH( IRPUNI )
            END IF
*  |  |  |
*  |  |  +-------------------------------------------------------------*
         END IF
*  |  |
*  |  +----------------------------------------------------------------*
      END IF
*  |  
*  +-------------------------------------------------------------------*

*  +-------------------------------------------------------------------*
*  |  Statistics on received particles:
      IF ( LSTPRT ) THEN
*  |  +----------------------------------------------------------------*
*  |  |  But check it's the correct turn!
         IF ( NSTTUR.EQ.1 .OR. MOD( ICPTUR, NSTTUR ).EQ. 1 ) THEN 
            CALL STTUPD( XFLK (NPFLKA),  YFLK (NPFLKA),  ZFLK (NPFLKA) ,
     &                  TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
         END IF
*  |  |
*  |  +----------------------------------------------------------------*
      END IF
*  |  
*  +-------------------------------------------------------------------*


*  Calculate the total kinetic energy of the primaries: don't change
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV

*  +-------------------------------------------------------------------*
*  |  Start tracking
      RETURN

*======================================================================*
*                                                                      *
*                 END OF BATCH                                         *
*                                                                      *
*======================================================================*

 3100 CONTINUE

*  +-------------------------------------------------------------------*
*  |  Notify user
      WRITE( LUNOUT, 1982 ) ' ... End of bunch - # primaries: ', ICPCNT

*  +-------------------------------------------------------------------*
*  |  No particles received in total: something really weird...
      IF ( ICPCNT .EQ. 0 ) THEN
         WRITE( LUNOUT, * )' *** 0 Particles received, '//
     &                     'ending simulation ***'
         GO TO 5000
      END IF

*  +-------------------------------------------------------------------*
*  |  Finalise and dump statistics about received particles
      IF ( LSTPRT ) THEN
*  |  +----------------------------------------------------------------*
*  |  |  But check it's the correct turn!
         IF ( NSTTUR.EQ.1 .OR. MOD( ICPTUR, NSTTUR ).EQ. 1 ) THEN 
            CALL STTFIN
            CALL STTDMP( ISTTUN )
         END IF
*  |  |
*  |  +----------------------------------------------------------------*
      END IF
*  |
*  +-------------------------------------------------------------------*

      ITOTAL = ITOTAL + ICPCNT
      ICPCNT = 0
      
*  +-------------------------------------------------------------------*
*  |  In case, update ICPOTR to present turn
      IF ( ICPTUR .NE. ICPOTR ) ICPOTR = ICPTUR

*  +-------------------------------------------------------------------*
*  |  Echo end of batch to the tracker
      N = NTSENDEOB( JFIOCD )

*  +-------------------------------------------------------------------*
*  |  Receive a new message
      GO TO 1972

*======================================================================*
*                                                                      *
*                 REGULAR CLOSING                                      *
*                                                                      *
*======================================================================*

*  +-------------------------------------------------------------------*
*  |  Send end of computation (eg because the tracking is over)
 4000 CONTINUE
      WRITE( LUNOUT, * ) ' *** Sending End Of Computation ***'
      N = NTSENDEOC( JFIOCD )

*  +-------------------------------------------------------------------*
*  |  Close connection
 4100 CONTINUE
      WRITE( LUNOUT, * ) ' *** Closing connection ***'
      N = NTEND( JFIOCD )
      JFIOCD = -1

*  +-------------------------------------------------------------------*
*  |  Shut server down
 4200 CONTINUE
      WRITE( LUNOUT, * ) ' *** Shutting server down ***'
      N = NTSHDWN( JFIOSD )

*  +-------------------------------------------------------------------*
*  |  Let's finish and go home, man
 4300 CONTINUE
      NOMORE = 1
      CALL FLUSH( LUNOUT )
      RETURN

*======================================================================*
*                                                                      *
*                 EXCEPTIONAL CLOSING                                  *
*                                                                      *
*======================================================================*

*  +-------------------------------------------------------------------*
*  |  Send end of computation message
 5000 CONTINUE
      N = NTSENDEOC( JFIOCD )
      IF ( N .LT. 0 ) THEN
         WRITE( LUNOUT, * ) ' *** Error sending End Of Computation ***'
         CALL FLUSH( LUNOUT )
*  |  |  Shouldn't I go to 4100?
         GO TO 4200
      END IF

*  +-------------------------------------------------------------------*
*  |  Wait for an end of computation message
 5100 CONTINUE
      N = NTWAIT(
     &        JFIOCD,
     &        MTYPE,
     &        IDPIDP, IDPGEN, WGT,
     &        XCBEAM, YCBEAM, ZCBEAM,
     &        UCBEAM, VCBEAM, WCBEAM,
     &        IAA, IZZ, AMBEAM, ECBEAM, PTIME)
      IF ( N .LT. 0 ) THEN
         WRITE( LUNOUT, * ) ' *** Client timeout 2 ***'
         CALL FLUSH( LUNOUT )
*  |  |  Shouldn't I go to 4100?
         GO TO 4200
      END IF
      IF ( MTYPE .NE. NFIOEC ) THEN
         GO TO 5100
      END IF

*  +-------------------------------------------------------------------*
*  |  Go on with regular closing
      GO TO 4100

 1981 FORMAT ( A40, 2(1X,I8) )
 1982 FORMAT ( A40, 1X, I8 )
*=== End of subroutine source =========================================*
      END
