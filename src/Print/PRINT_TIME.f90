PROGRAM PRINT_TIME

! ---------------------------------------------------------------------------- !
!
!**** *PRINT_TIME* - PRINT TIMESERIE FROM GRIDDED WAMODEL OUTPUT.
!
!      H. GUNTHER        ECMWF          DECEMBER 1989
!
!     PURPOSE.
!     --------
!
!          POSTPROCESSING OF WAM MODEL INTEGRATED DATA.
!          PRINT TIME SERIES FROM GRIDDED WAMODEL OUTPUT.
!
!
!**   INTERFACE.
!     ----------
!
!          FILES       UNIT     CONTENT
!          -----       ----     -------
!
!          MAP          IUMAP   CONTAINS WAVE AND WIND FIELDS
!          INPUT        5       USER INPUT
!          OUTPUT       6       PRINTER OUTPUT
!
!          LIBRARIES
!          ---------
!
!          WAMCRLIB2
!          ECLIB
!
!          EXTERNALS
!          ---------
!
!          GFILE      GETS FILES FROM OR IN ECFILE
!          INCDATE    INCREMENTS DATE-TIME-GROUP
!          INGRID     READS WAVEMODEL OUTPUT FILE (MAP)
!          USEIN      READS IN USER INPUT
!          USTRU10    CONVERT FRICTION VELOCITY TO WIND AT 10M
!            LODTAB   LOADS A TABLE OF USTAR AND U10
!
!
!     METHOD.
!     -------
!
!          THIS PROGRAM TAKES THE  WAM MODEL OUTPUTS AS INPUT
!          EXTRACTS TIMESERIES AT SPECIFIED LOCATIONS.
!          THE FILES ARE FETCHED FROM ECFILE.
!
!     REFERENCE
!     ---------
!
!          NONE
!
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE, ONLY:  &
&       DIFDATE,               &  !! TIME DIFFERENCE.
&       INCDATE,               &  !! UPDATES A DATE/TIME GROUP.
&       OPEN_FILE                 !! OPEN A FILE.

! ---------------------------------------------------------------------------- !
!
!*    INTERFACE VARIABLE

USE WAM_PRINT_MODULE, ONLY: IU05, FILE05, IU06, FILE06, ITEST,                 &
&                           CDATEA, CDATEE, IDELDO,                            &
&                           IU01, FILE01, CDTFILE, IDFILE,                     &
&                           U10_GR, UDIR_GR, US_GR, CD_GR,                     &
&                           HS_GR, PPER_GR, MPER_GR, TM1_GR, TM2_GR, MDIR_GR,  &
&                           SPRE_GR, TAUW_GR,                                  &
&                           HS_SEA_GR, PPER_SEA_GR, MPER_SEA_GR, TM1_SEA_GR,   &
&                           TM2_SEA_GR, MDIR_SEA_GR, SPRE_SEA_GR,              &
&                           HS_SWELL_GR, PPER_SWELL_GR, MPER_SWELL_GR,         &
&                           TM1_SWELL_GR, TM2_SWELL_GR, MDIR_SWELL_GR,         &
&                           SPRE_SWELL_GR

USE WAM_OUTPUT_SET_UP_MODULE,ONLY: CDTINTT, NOUTT, COUTT, TITL, SCAL, PFLAG,   &
&                           NOUTP, OUTLONG, OUTLAT, NAME
IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLE

LOGICAL       :: IEOF

INTEGER      ::  NPOUT, IS, IT, NOU

INTEGER      :: IFAIL
INTEGER      :: IDELS
REAL,         ALLOCATABLE :: SERIE(:,:,:)
REAL,         ALLOCATABLE :: WORK1(:,:)
INTEGER,      ALLOCATABLE :: I(:)
INTEGER,      ALLOCATABLE :: K(:)

REAL, PARAMETER :: ZMISS = -999.

! ---------------------------------------------------------------------------- !
!
!*    1. INITALISATION.
!        --------------

FILE05 = 'Time_User'
FILE06 = 'Time_Prot'
OPEN (UNIT=IU05, FILE=FILE05, FORM="FORMATTED", STATUS="OLD")
OPEN (UNIT=IU06, FILE=FILE06, FORM="FORMATTED", STATUS="UNKNOWN")

!
!*    1.1 READ USER INPUT.
!         ----------------

CALL READ_TIME_USER

!*    1.2 ALLOCATE OUTPUT ARRAYS.
!         ------------------------

CALL DIFDATE(CDATEA,CDATEE,IDELS)

NOUTT = INT(IDELS/IDELDO)+1
NPOUT = 29

ALLOCATE (COUTT(NOUTT))
ALLOCATE (SERIE(NOUTT,NPOUT,NOUTP))
SERIE = ZMISS
NOU = 0

ALLOCATE (I(NOUTP))
ALLOCATE (K(NOUTP))

! ---------------------------------------------------------------------------- !
!
!*    2. LOOP OVER OUTPUT FILES.
!        -----------------------
!
FILES: DO 

!*    2.1 FETCH FILE.
!         -----------

   CALL OPEN_FILE (IU06, IU01, FILE01, CDTFILE, 'OLD', IFAIL)
   IF (IFAIL.NE.0) EXIT FILES

!*    2.2  LOOP OVER OUTPUT TIMES.
!          -----------------------

   TIMES: DO 

!*    2.2.1 READ IN WIND AND WAVE FIELDS.
!           -----------------------------

      CALL INGRID (IU01, IEOF)

      IF (IEOF) EXIT TIMES     !! IF END OF FILE ENCOUNTED THEN EXIT TIME LOOP

!*    2.2.3 OUTPUT TIME FOUND?
!           ------------------

      IF (CDTINTT.LT.CDATEA) CYCLE TIMES
      DO WHILE (CDTINTT.GT.CDATEA)
         CALL INCDATE(CDATEA,IDELDO)
         IF (CDATEA.GT.CDATEE) EXIT FILES
         IF (CDTINTT.LT.CDATEA) CYCLE TIMES
      END DO

!     2.2.4. EXTRACT DATA. IF FIRST TIME COMPUTE GRID POINT INDICES.

      NOU = NOU+1
      IF (NOU.EQ.1) THEN
         CALL PREPARE_EXTRACTION (NOUTP, OUTLONG(1:NOUTP),OUTLAT(1:NOUTP), I, K)
         WRITE(IU06,*) ' '
         DO IS = 1,NOUTP
            IF (I(IS).EQ.-1) THEN
               WRITE(IU06,'('' SITE: '',A20,'' LAT. = '',F10.5,'' LONG. = '' , &
&                            F10.5, '' IS NOT IN GRID'')')                     &
&                            NAME(IS), OUTLAT(IS), OUTLONG(IS)
            END IF
         END DO    
      END IF

      IF (NOU.GT.NOUTT) THEN
         NOU = NOUTT
         WRITE(IU06,'(''0'')')
         WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++'
         WRITE(IU06,*) ' +                                         +'
         WRITE(IU06,*) ' +       WARNING ERROR IN PRINT_TIME       +'
         WRITE(IU06,*) ' +       ===========================       +'
         WRITE(IU06,*) ' + NUMBER OF OUTPUT TIMES EXCEEDS DIMENSION+'
         WRITE(IU06,*) ' + DIMENSION IS  NOUTT = ', NOUTT
         WRITE(IU06,*) ' + LAST OUTPUT TIME IS ', COUTT(NOUTT)
         WRITE(IU06,*) ' +                                         +'
         WRITE(IU06,*) ' + LATER TIMES ARE NOT PROCESSED           +'
         WRITE(IU06,*) ' +                                         +'
         WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++'
         EXIT FILES
      END IF

      COUTT(NOU) = CDTINTT

      POINT: DO IS=1,NOUTP
         IF (IS.EQ.-1) CYCLE POINT
         IF (PFLAG( 1)) SERIE(NOU,1,IS) = U10_GR(I(IS),K(IS))
         IF (PFLAG( 2)) SERIE(NOU,2,IS) = UDIR_GR(I(IS),K(IS))
         IF (PFLAG( 3)) SERIE(NOU,3,IS) = US_GR(I(IS),K(IS))
         IF (PFLAG( 4)) SERIE(NOU,4,IS) = CD_GR(I(IS),K(IS))

         IF (PFLAG( 7)) SERIE(NOU,7,IS) = HS_GR(I(IS),K(IS))
         IF (PFLAG( 8)) SERIE(NOU,8,IS) = PPER_GR(I(IS),K(IS))
         IF (PFLAG( 9)) SERIE(NOU,9,IS) = MPER_GR(I(IS),K(IS))
         IF (PFLAG(10)) SERIE(NOU,10,IS) = TM1_GR(I(IS),K(IS))
         IF (PFLAG(11)) SERIE(NOU,11,IS) = TM2_GR(I(IS),K(IS))
         IF (PFLAG(12)) SERIE(NOU,12,IS) = MDIR_GR(I(IS),K(IS))
         IF (PFLAG(13)) SERIE(NOU,13,IS) = SPRE_GR(I(IS),K(IS))
         IF (PFLAG(14)) SERIE(NOU,14,IS) = TAUW_GR(I(IS),K(IS))

         IF (PFLAG(15)) SERIE(NOU,15,IS) = HS_SEA_GR(I(IS),K(IS))
         IF (PFLAG(16)) SERIE(NOU,16,IS) = PPER_SEA_GR(I(IS),K(IS))
         IF (PFLAG(17)) SERIE(NOU,17,IS) = MPER_SEA_GR(I(IS),K(IS))
         IF (PFLAG(18)) SERIE(NOU,18,IS) = TM1_SEA_GR(I(IS),K(IS))
         IF (PFLAG(19)) SERIE(NOU,19,IS) = TM2_SEA_GR(I(IS),K(IS))
         IF (PFLAG(20)) SERIE(NOU,20,IS) = MDIR_SEA_GR(I(IS),K(IS))
         IF (PFLAG(21)) SERIE(NOU,21,IS) = SPRE_SEA_GR(I(IS),K(IS))

         IF (PFLAG(23)) SERIE(NOU,23,IS) = HS_SWELL_GR(I(IS),K(IS))
         IF (PFLAG(24)) SERIE(NOU,24,IS) = PPER_SWELL_GR(I(IS),K(IS))
         IF (PFLAG(25)) SERIE(NOU,25,IS) = MPER_SWELL_GR(I(IS),K(IS))
         IF (PFLAG(26)) SERIE(NOU,26,IS) = TM1_SWELL_GR(I(IS),K(IS))
         IF (PFLAG(27)) SERIE(NOU,27,IS) = TM2_SWELL_GR(I(IS),K(IS))
         IF (PFLAG(28)) SERIE(NOU,28,IS) = MDIR_SWELL_GR(I(IS),K(IS))
         IF (PFLAG(29)) SERIE(NOU,29,IS) = SPRE_SWELL_GR(I(IS),K(IS))
      END DO POINT


!*    4. NEXT OUTPUT TIME.
!        -----------------

      CALL INCDATE(CDATEA,IDELDO)       !! INCREMENT DATE FOR THE NEXT OUTPUT.
      IF (CDATEA.GT.CDATEE) EXIT FILES
   END DO TIMES
   
   CALL INCDATE (CDTFILE, IDFILE)       !! INCREMENT DATE FOR THE NEXT FILE.
   CLOSE (UNIT=IU01, STATUS='KEEP')     !! CLOSE OLD FILE
END DO FILES

!-------------------------------------------------------------------------------!
!
!*    5. OUTPUT OF TIME SERIES.
!        ----------------------


SITE: DO IS = 1,NOUTP
   IF (I(IS).EQ.-1)  CYCLE SITE
   DO IT = 1,NOU
      IF (MOD(IT-1,60).EQ.0) THEN
         WRITE(IU06,'(''1'')')
         WRITE(IU06,'('' TIME SERIES OF INTEGRATED PARAMETERS AT '',A20,       &
&                     '' LAT. = '',F10.5,'' LONG. = '',F10.5)')                &
&                                        NAME(IS), OUTLAT(IS), OUTLONG(IS)

         WRITE(IU06,*) ' '
         WRITE(IU06,*) '|----- DATE ----|-------- WIND ----------',         &
&                        '|--------------- WAVES -----------------',                &
&                        '|--------------- WINDSEA ---------------',                &
&                        '|--------------- SWELL ----------------|'

         WRITE(IU06,*) '                 U10  DIR.  US   CD  TAUW',         &
&                        '   HS    TP    TM   TM1   TM2  DIR. SPR.',                &
&                        '   HS    TP    TM   TM1   TM2  DIR. SPR.',                &
&                        '   HS    TP    TM   TM1   TM2  DIR. SPR.'
         WRITE(IU06,*) ' YYYYMMDDHHMMSS [M/S][DEG][M/S][1000] [%]',         &
&                        '  [M]   [S]   [S]   [S]   [S] [DEG][DEG]',                &
&                        '  [M]   [S]   [S]   [S]   [S] [DEG][DEG]',                &
&                        '  [M]   [S]   [S]   [S]   [S] [DEG][DEG]'
      END IF
      WRITE(IU06,'(2X,A14,F6.1,F5.0,F5.2,F5.2,F5.2,3(5F6.2,2F5.0))')     &
&      COUTT(IT), SERIE(IT,1:3,IS), SERIE(IT,4,IS)*1000, SERIE(IT,14,IS),     &
&                 SERIE(IT,7:13,IS), SERIE(IT,15:21,IS), SERIE(IT,23:29,IS)
   END DO
END DO SITE


STOP
END
