MODULE WAM_OUTPUT_SET_UP_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS: OUTPUT TIMES, FLAGS.                                 !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATE PROCESSING.
&       DIFDATE,                 &  !! COMPUTES TIME DIFFERENCE.
&       OPEN_FILE,               &  !! OPENS A FILE.
&       INCDATE                     !! UPDATE DATE TIME GROUP.

USE WAM_GRID_MODULE,   ONLY:     & 
&       FIND_SEA_POINT              !! FIND SEA POINT NUMBER.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_FILE_MODULE,   ONLY: IU06, ITEST, IU20, FILE20, IU25, FILE25,          &
&                            CDTRES, IDELRES

USE WAM_TIMOPT_MODULE, ONLY: CDATEA, CDATEE, IDELPRO, CDTPRO

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

INTEGER :: I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. NEXT OUTPUT TIMES AND TIME INCREMENTS.                                !
!        --------------------------------------                                !

CHARACTER  :: CDTINTT*14   !! NEXT DATE TO WRITE INTEG. PARA. (TOTAL).
CHARACTER  :: CDTSPT*14    !! NEXT DATE TO WRITE SPECTRA (TOTAL).
INTEGER    :: IDELINT = 0  !! INTEG. PARAMETER OUTPUT TIMESTEP IN SECONDS.
INTEGER    :: IDELSPT = 0  !! SPECTRA OUTPUT TIMESTEP IN SECONDS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. OUTPUT TIMES AS DEFINED IN INPUT FILE.                                !
!        --------------------------------------                                !

INTEGER                   :: NOUTT = 0 !! NUMBER OF OUTPUT TIMES.
CHARACTER*14, ALLOCATABLE :: COUTT(:)  !! OUTPUT TIMES.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. OUTPUT FLAGS.                                                         !
!        -------------                                                         !

INTEGER, PARAMETER :: NPOUT = 34   !! MAXIMUM NUMBER OF OUTPUT PARAMETER.

LOGICAL, DIMENSION(NPOUT) :: FFLAG = (/(.FALSE.,I=1,NPOUT)/)
                             !! FILE OUTPUT FLAG FOR EACH OUTPUT TYPE.
LOGICAL :: FFLAG20 = .FALSE. !! .TRUE. IF FIELDS ARE WRITTEN TO FILE20.
LOGICAL :: FFLAG25 = .FALSE. !! .TRUE. IF SPECTRA ARE WRITTEN TO FILE25.

LOGICAL, DIMENSION(NPOUT) :: PFLAG(NPOUT) = (/(.FALSE.,I=1,NPOUT)/)
                             !! PRINT OUTPUT FLAG FOR EACH OUTPUT TYPE.
LOGICAL :: PFLAG20 = .FALSE. !! .TRUE. IF PRINT OUTPUT OF FIELDS.
LOGICAL :: PFLAG25 = .FALSE. !! .TRUE. IF PRINT OUTPUT OF SPECTRA.

LOGICAL, DIMENSION(NPOUT) :: CFLAG(NPOUT) = (/(.FALSE.,I=1,NPOUT)/) 
                             !! COMPUTATION FLAG FOR EACH OUTPUT TYPE.
LOGICAL :: CFLAG20 = .FALSE. !! .TRUE. IF ANY OUTPUT OF FIELDS.
LOGICAL :: CFLAG25 = .FALSE. !! .TRUE. IF ANY OUTPUT OF SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. TITLE FOR OUTPUT PARAMETER.                                           !
!        ---------------------------                                           !

CHARACTER(LEN=60), DIMENSION(NPOUT) :: TITL = (/                     &
& ' WIND SPEED U10 ( 0.1 METRES/SECOND )                       ',    &   !!  1
& ' WIND DIRECTION ( DEGREE FROM NORTH TO )                    ',    &   !!  2
& ' FRICTION VELOCITY ( 0.01 METRES/SECOND )                   ',    &   !!  3
& ' DRAG COEFFICIENT ( 0.1 *PROMILLE )                         ',    &   !!  4
& ' DUMMY                                                      ',    &   !!  5
& ' DUMMY                                                      ',    &   !!  6
& ' SIGNIFICANT WAVE HEIGHT ( 0.1 METRES )                     ',    &   !!  7
& ' WAVE PEAK PERIOD ( 0.1 SECONDS )                           ',    &   !!  8
& ' WAVE MEAN PERIOD ( 0.1 SECONDS )                           ',    &   !!  9
& ' WAVE TM1 PERIOD ( 0.1 SECONDS )                            ',    &   !! 10
& ' WAVE TM2 PERIOD ( 0.1 SECONDS )                            ',    &   !! 11
& ' WAVE DIRECTION ( DEGREE FROM NORTH TO )                    ',    &   !! 12
& ' DIRECTIONAL SPREAD ( DEGREES )                             ',    &   !! 13
& ' NORMALISED WAVE STRESS ( % )                               ',    &   !! 14
& ' SEA SIGNIFICANT WAVE HEIGHT ( 0.1 METRES )                 ',    &   !! 15
& ' SEA PEAK PERIOD ( 0.1 SECONDS )                            ',    &   !! 16
& ' SEA MEAN PERIOD ( 0.1 SECONDS )                            ',    &   !! 17
& ' SEA TM1 PERIOD ( 0.1 SECONDS )                             ',    &   !! 18
& ' SEA TM2 PERIOD ( 0.1 SECONDS )                             ',    &   !! 19
& ' SEA DIRECTION ( DEGREE FROM NORTH TO )                     ',    &   !! 20
& ' SEA DIRECTIONAL SPREAD ( DEGREES )                         ',    &   !! 21
& ' DUMMY                                                      ',    &   !! 22
& ' SWELL SIGNIFICANT WAVE HEIGHT ( 0.1 METRES )               ',    &   !! 23
& ' SWELL PEAK PERIOD ( 0.1 SECONDS )                          ',    &   !! 24
& ' SWELL MEAN PERIOD ( 0.1 SECONDS )                          ',    &   !! 25
& ' SWELL TM1 PERIOD ( 0.1 SECONDS )                           ',    &   !! 26
& ' SWELL TM2 PERIOD ( 0.1 SECONDS )                           ',    &   !! 27
& ' SWELL DIRECTION ( DEGREE FROM NORTH TO )                   ',    &   !! 28
& ' SWELL DIRECTIONAL SPREAD ( DEGREES )                       ',    &   !! 29
& ' DUMMY                                                      ',    &   !! 30
& ' SPECTRUM                                                   ',    &   !! 31
& ' SEA SPECTRUM                                               ',    &   !! 32
& ' SWELL SPECTRUM                                             ',    &   !! 33
& ' DUMMY                                                      '/)       !! 34

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. SCALING FACTORS FOR OUTPUT PARAMETER.                                 !
!        -------------------------------------                                 !

REAL, PARAMETER, DIMENSION(NPOUT) :: SCAL = (/                                 &
&                     10.            ,    &   !!  1
&                      1.            ,    &   !!  2
&                    100.            ,    &   !!  3
&                  10000.            ,    &   !!  4
&                      1.            ,    &   !!  5
&                      1.            ,    &   !!  6
&                     10.            ,    &   !!  7
&                     10.            ,    &   !!  8
&                     10.            ,    &   !!  9
&                     10.            ,    &   !! 10
&                     10.            ,    &   !! 11
&                      1.            ,    &   !! 12
&                      1.            ,    &   !! 13
&                    100.            ,    &   !! 14
&                     10.            ,    &   !! 15
&                     10.            ,    &   !! 16
&                     10.            ,    &   !! 17
&                     10.            ,    &   !! 19
&                     10.            ,    &   !! 20
&                      1.            ,    &   !! 21
&                      1.            ,    &   !! 22
&                      1.            ,    &   !! 23
&                     10.            ,    &   !! 24
&                     10.            ,    &   !! 25
&                     10.            ,    &   !! 26
&                     10.            ,    &   !! 27
&                     10.            ,    &   !! 28
&                      1.            ,    &   !! 29
&                      1.            ,    &   !! 30
&                      1.            ,    &   !! 31
&                      1.            ,    &   !! 29
&                      1.            ,    &   !! 32
&                      1.            ,    &   !! 33
&                      1.            /)       !! 34

! ---------------------------------------------------------------------------- !
!                                                                              !
!      9. OUTPUT POINTS FOR SPECTRA.                                           !
!         --------------------------                                           !

INTEGER                   :: NOUTP = 0    !! NUMBER OF OUTPUT POINTS.
REAL, ALLOCATABLE         :: OUTLAT (:)   !! LATITUDE OF POINTS [DEG].
REAL, ALLOCATABLE         :: OUTLONG(:)   !! LONGITUDE OF POINTS [DEG].
CHARACTER*20, ALLOCATABLE :: NAME(:)      !! NAMES OF OUTPUT SITES.

INTEGER, ALLOCATABLE      :: IJAR(:)      !! GRIDPOINT NUMBER OF OUTPUT POINT.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SET_OUTPUT_FLAGS            !! SETS FLAGS FOR OUTPUT.
   MODULE PROCEDURE SET_OUTPUT_FLAGS
END INTERFACE
PUBLIC SET_OUTPUT_FLAGS

INTERFACE SET_OUTPUT_SITES            !! SETS SITES FOR OUTPUT.
   MODULE PROCEDURE SET_OUTPUT_SITES
END INTERFACE
PUBLIC SET_OUTPUT_SITES

INTERFACE SET_OUTPUT_TIMES            !! SETS TIMES FOR OUTPUT.
   MODULE PROCEDURE SET_OUTPUT_TIMES_F   !! FIXED OUTPUT TIMES.
   MODULE PROCEDURE SET_OUTPUT_TIMES_I   !! OUTPUT INCREMENTS.
END INTERFACE
PUBLIC SET_OUTPUT_TIMES

INTERFACE PREPARE_OUTPUT              !! PREPARES OUTPUT.
   MODULE PROCEDURE PREPARE_OUTPUT
END INTERFACE
PUBLIC PREPARE_OUTPUT

INTERFACE PRINT_OUTPUT_STATUS         !! PRINT OUTPUT SETTING.
   MODULE PROCEDURE PRINT_OUTPUT_STATUS
END INTERFACE
PUBLIC PRINT_OUTPUT_STATUS

INTERFACE UPDATE_OUTPUT_TIME          !! UPDATES OUTPUT TIMES.
   MODULE PROCEDURE UPDATE_OUTPUT_TIME
END INTERFACE
PUBLIC UPDATE_OUTPUT_TIME

INTERFACE SAVE_OUTPUT_FILES           !! SAVES AND OPENS OUTPUT FILES.
   MODULE PROCEDURE SAVE_OUTPUT_FILES 
END INTERFACE
PUBLIC SAVE_OUTPUT_FILES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE MAKE_OUTPUT_SITES           !! INDEX OF OUTPUT POINTS.
   MODULE PROCEDURE MAKE_OUTPUT_SITES
END INTERFACE
PRIVATE MAKE_OUTPUT_SITES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_OUTPUT_FLAGS (PF, FF)

LOGICAL, INTENT(IN) :: PF(:)   !! PRINTER FLAGS.
LOGICAL, INTENT(IN) :: FF(:)   !! FILE FLAGS.

IF (SIZE(PF).NE.NPOUT .OR. SIZE(FF).NE.NPOUT) THEN
   WRITE(IU06,*) '*  PROGRAM NEEDS ', NPOUT,' FLAGS FOR OUTPUT *'
   WRITE(IU06,*) '*  NUMBER OF PRINTER FLAGS IS : ', SIZE(PF)
   WRITE(IU06,*) '*  NUMBER OF FILE    FLAGS IS : ', SIZE(FF)
   CALL ABORT1
END IF

PFLAG = PF
FFLAG = FF

FFLAG(5)  = .FALSE.    !! CORRECT DUMMY PARAMETER.
FFLAG(6)  = .FALSE.
FFLAG(22) = .FALSE.
FFLAG(30) = .FALSE.
FFLAG(34) = .FALSE.
PFLAG(5)  = .FALSE.
PFLAG(6)  = .FALSE.
PFLAG(22) = .FALSE.
PFLAG(30) = .FALSE.
PFLAG(34) = .FALSE.

CFLAG = FFLAG.OR.PFLAG
FFLAG20 = ANY(FFLAG(1:30))
PFLAG20 = ANY(PFLAG(1:30))
CFLAG20 = FFLAG20.OR.PFLAG20

FFLAG25 = ANY(FFLAG(31:34))
PFLAG25 = ANY(PFLAG(31:34))
CFLAG25 = FFLAG25.OR.PFLAG25

END SUBROUTINE SET_OUTPUT_FLAGS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_OUTPUT_SITES (N, LONG, LAT, NA)

INTEGER,       INTENT(IN) :: N       !! NUMBER OF OUTPUT SITES.
REAL,          INTENT(IN) :: LONG(:) !! LONGITUDES.
REAL,          INTENT(IN) :: LAT(:)  !! LATITUDES.
CHARACTER*(*), INTENT(IN) :: NA(:)   !! SITE NAME.

IF (N.GT.0) THEN
   ALLOCATE (OUTLONG(N))
   ALLOCATE (OUTLAT (N))
   ALLOCATE (NAME   (N))
   NOUTP = N
   OUTLONG = LONG(1:N)
   OUTLAT  = LAT (1:N)
   NAME    = NA  (1:N)
ELSE
   NOUTP = 0
END IF

END SUBROUTINE SET_OUTPUT_SITES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_OUTPUT_TIMES_F (N, TIME)

INTEGER,      INTENT(IN) :: N       !! NUMBER OF OUTPUT TIMES.
CHARACTER*14, INTENT(IN) :: TIME(:) !! OUTPUT TIMES.

IF (N.GT.0) THEN
   ALLOCATE (COUTT(N))
   NOUTT = N
   COUTT = TIME(1:N)
ELSE
   NOUTT = 0
END IF

END SUBROUTINE SET_OUTPUT_TIMES_F

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_OUTPUT_TIMES_I (INT, SPE)

INTEGER,      INTENT(IN) :: INT     !! OUTPUT TIME INCREMENT FOR PARAMETER.
INTEGER,      INTENT(IN) :: SPE     !! OUTPUT TIME INCREMENT FOR SPECTRA.

IDELINT = INT
IDELSPT = SPE

END SUBROUTINE SET_OUTPUT_TIMES_I

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_OUTPUT (IU1, FILE1, IU2, FILE2)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER,           INTENT(IN) :: IU1         !! FILE OUTPUT UNIT FOR PRAMETER.
CHARACTER (LEN=*), INTENT(IN) :: FILE1       !! FILE NAME FOR PRAMETER.
INTEGER,           INTENT(IN) :: IU2         !! FILE OUTPUT UNIT FOR SPECTRA.
CHARACTER (LEN=*), INTENT(IN) :: FILE2       !! FILE NAME FOR SPECTRA.

INTEGER :: J, NW, ISHIFT, IW(1:NOUTT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COMPUTATION AND OUTPUT FLAGS.                                         !
!        -----------------------------                                         !

CFLAG = FFLAG.OR.PFLAG
FFLAG20 = ANY(FFLAG(1:30))
PFLAG20 = ANY(PFLAG(1:30))
CFLAG20 = FFLAG20.OR.PFLAG20

FFLAG25 = ANY(FFLAG(31:34))
PFLAG25 = ANY(PFLAG(31:34))
CFLAG25 = FFLAG25.OR.PFLAG25

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. CHECK CONSISTENCY FOR SPECTRA OUTPUT.                                 !
!        -------------------------------------                                 !

IF (NOUTP.LE.0 .AND. CFLAG25)  THEN
   WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE(IU06,*) ' +                                             +'
   WRITE(IU06,*) ' +    WARNING ERROR IN SUB. PREPARE_OUTPUT     +'
   WRITE(IU06,*) ' +    ====================================     +'
   WRITE(IU06,*) ' + NUMBER OF OUTPUT SITES FOR SPECTRA          +'
   WRITE(IU06,*) ' + FOUND IN THE GRID IS ZERO.                  +'
   WRITE(IU06,*) ' + OUTPUT FLAGS CHANGED TO NO OUTPUT.          +'
   WRITE(IU06,*) ' +                                             +'
   WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++'
   NOUTP = 0
ELSE
   IF (CFLAG25 .AND. NOUTP.GT.0) CALL MAKE_OUTPUT_SITES
END IF

FFLAG(31:34) = FFLAG(31:34) .AND. NOUTP.GT.0
PFLAG(31:34) = PFLAG(31:34) .AND. NOUTP.GT.0
CFLAG = FFLAG.OR.PFLAG
FFLAG25 = ANY(FFLAG(31:34))
PFLAG25 = ANY(PFLAG(31:34))
CFLAG25 = FFLAG25.OR.PFLAG25

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. CHECK CONSISTENCY OUTPUT TIMES AND MODEL TIMESTEPS.                   !
!        ---------------------------------------------------                   !

IF (NOUTT.GT.0) THEN
   IW = 0
   DO J = 1,NOUTT
      CALL DIFDATE (CDATEA, COUTT(J), ISHIFT)
      IF (ISHIFT.LT.0 .OR. MOD(ISHIFT,IDELPRO).NE.0) IW(J) = J
   END DO
   NW = COUNT(IW.NE.0)
   IF (NW.GT.0) THEN
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +     WARNING ERROR IN SUB. PREPARE_OUTPUT      +'
      WRITE(IU06,*) ' +     ====================================      +'
      WRITE(IU06,*) ' + THE FOLLOWING OUTPUT DATES ARE NOT AT THE END +'
      WRITE(IU06,*) ' + OF A PROPAGATION TIMESTEP AND WILL BE IGNORED +'
      WRITE(IU06,*) ' +                                               +'
      DO J = 1,NW,5
         WRITE(IU06,'(5(A14,'', ''))') COUTT(IW(J:MIN(J+4,NW)))
      END DO
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      IF (NW.LT.NOUTT) THEN
         COUTT = PACK(COUTT,IW.EQ.0)
         NOUTT = NOUTT-NW
      ELSE IF (NW.GE.NOUTT) THEN
         WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
         WRITE(IU06,*) ' +                                               +'
         WRITE(IU06,*) ' +     WARNING ERROR IN SUB. PREPARE_OUTPUT      +'
         WRITE(IU06,*) ' +     ====================================      +'
         WRITE(IU06,*) ' + ALL OUTPUT DATES ARE NOT AT THE END OF A      +'
         WRITE(IU06,*) ' + PROPAGATION TIMESTEP.                         +'
         WRITE(IU06,*) ' + OUTPUT IS NOT DONE.                           +'
         WRITE(IU06,*) ' +                                               +'
         WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
         NOUTT = 0
         FFLAG = .FALSE.
         CFLAG = .FALSE.
         CFLAG = .FALSE.
         FFLAG20 = .FALSE.
         PFLAG20 = .FALSE.
         CFLAG20 = .FALSE.

         FFLAG25 = .FALSE.
         PFLAG25 = .FALSE.
         CFLAG25 = .FALSE.
      END IF
   END IF
ELSE
   IF (CFLAG20 .AND. IDELINT.LT.IDELPRO) THEN
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +     WARNING ERROR IN SUB. PREPARE_OUTPUT      +'
      WRITE(IU06,*) ' +     ====================================      +'
      WRITE(IU06,*) ' + OUTPUT TIMESTEP FOR INTEGRATED PARAMETER IS   +'
      WRITE(IU06,*) ' + LESS THAN PROPAGATION TIME STEP.              +'
      WRITE(IU06,*) ' + OUTPUT      TIMESTEP WAS: ',IDELINT, ' SECONDS'
      WRITE(IU06,*) ' + PROPAGATION TIMESTEP  IS: ',IDELPRO, ' SECONDS'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' + TIMESTEP IS CHANGED TO PROPAGATION TIME STEP. +'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      IDELINT = IDELPRO
   ELSE IF (CFLAG20 .AND. MOD(IDELINT,IDELPRO).NE.0) THEN
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +     WARNING ERROR IN SUB. PREPARE_OUTPUT      +'
      WRITE(IU06,*) ' +     ====================================      +'
      WRITE(IU06,*) ' + OUTPUT TIMESTEP FOR INTEGRATED PARAMETER IS   +'
      WRITE(IU06,*) ' + NOT A MULTIPLE OF PROPAGATION TIME STEP.      +'
      WRITE(IU06,*) ' + OUTPUT      TIMESTEP WAS: ',IDELINT, ' SECONDS'
      WRITE(IU06,*) ' + PROPAGATION TIMESTEP  IS: ',IDELPRO, ' SECONDS'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' + TIMESTEP IS CHANGED TO NEAREST MULTIPLE OF    +'
      WRITE(IU06,*) ' + THE PROPAGATION TIME STEP.                    +'
      IDELINT = MAX(NINT(REAL(IDELINT)/REAL(IDELPRO)),1)*IDELPRO
      WRITE(IU06,*) ' + NEW OUTPUT TIMESTEP   IS: ',IDELINT, ' SECONDS'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
   END IF

   IF (CFLAG25 .AND. IDELSPT.LT.IDELPRO) THEN
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +     WARNING ERROR IN SUB. PREPARE_OUTPUT      +'
      WRITE(IU06,*) ' +     ====================================      +'
      WRITE(IU06,*) ' + OUTPUT TIMESTEP FOR SPECTRA IS LESS THAN      +'
      WRITE(IU06,*) ' + PROPAGATION TIME STEP.                        +'
      WRITE(IU06,*) ' + OUTPUT      TIMESTEP WAS: ',IDELSPT, ' SECONDS'
      WRITE(IU06,*) ' + PROPAGATION TIMESTEP  IS: ',IDELPRO, ' SECONDS'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' + TIMESTEP IS CHANGED TO PROPAGATION TIME STEP. +'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      IDELSPT = IDELPRO
   ELSE IF (CFLAG25 .AND. MOD(IDELSPT,IDELPRO).NE.0) THEN
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +     WARNING ERROR IN SUB. PREPARE_OUTPUT      +'
      WRITE(IU06,*) ' +     ====================================      +'
      WRITE(IU06,*) ' + OUTPUT TIMESTEP FOR SPECTRA  IS  NOT A        +'
      WRITE(IU06,*) ' + MULTIPLE OF PROPAGATION TIME STEP.            +'
      WRITE(IU06,*) ' + OUTPUT      TIMESTEP WAS: ',IDELSPT, ' SECONDS'
      WRITE(IU06,*) ' + PROPAGATION TIMESTEP  IS: ',IDELPRO, ' SECONDS'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' + TIMESTEP IS CHANGED TO NEAREST MULTIPLE OF    +'
      WRITE(IU06,*) ' + THE PROPAGATION TIME STEP.                    +'
      IDELSPT = MAX(NINT(REAL(IDELSPT)/REAL(IDELPRO)),1)*IDELPRO
      WRITE(IU06,*) ' + NEW OUTPUT TIMESTEP   IS: ',IDELSPT, ' SECONDS'
      WRITE(IU06,*) ' +                                               +'
      WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++++'
   END IF
END IF

CFLAG = FFLAG.OR.PFLAG
FFLAG20 = ANY(FFLAG(1:30))
PFLAG20 = ANY(PFLAG(1:30))
CFLAG20 = FFLAG20.OR.PFLAG20

FFLAG25 = ANY(FFLAG(31:34))
PFLAG25 = ANY(PFLAG(31:34))
CFLAG25 = FFLAG25.OR.PFLAG25

CFLAG( 1: 3) = CFLAG( 1: 3) .OR. CFLAG25
CFLAG( 7:13) = CFLAG( 7:13) .OR. CFLAG(31)
CFLAG(15:21) = CFLAG(15:21) .OR. CFLAG(32)
CFLAG(23:29) = CFLAG(23:29) .OR. CFLAG(33)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. INITIALIZE OUTPUT TIME VARIABLES.                                     !
!        ---------------------------------                                     !

IF (CFLAG20) THEN
   CDTINTT = CDATEA
ELSE
   CDTINTT = ' '
END IF

IF (CFLAG25) THEN
   CDTSPT = CDATEA
ELSE
   CDTSPT = ' '
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5.  OPEN FIRST OUTPUT FILES.                                             !
!        -------------------------                                             !

CALL SAVE_OUTPUT_FILES (IU1, FILE1, IU2, FILE2)

END SUBROUTINE PREPARE_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_OUTPUT_STATUS

INTEGER :: I

WRITE(IU06,*) '  '
WRITE(IU06,*) ' ------------------------------------------------- '
WRITE(IU06,*) '              MODEL OUTPUT SELECTION:'
WRITE(IU06,*) ' ------------------------------------------------- '
WRITE(IU06,*) '  '

IF (NOUTT.GT.0) THEN
   WRITE(IU06,*) ' NUMBER OF OUTPUT TIMES IS NOUTT = ', NOUTT
   WRITE(IU06,*) ' OUTPUT WILL BE PROCESSED AT:'
   WRITE(IU06,'(6(2X,A14))') (COUTT(I),I=1,NOUTT)
   WRITE(IU06,*) '  '
END IF

IF (CFLAG20) THEN
   I = LEN_TRIM(FILE20)
   WRITE(IU06,*) ' TO PRINTER AND/OR FILE: ', FILE20(1:I),'YYYYMMDDHHMMSS'
   IF (NOUTT.EQ.0) THEN
      WRITE(IU06,*) ' EVERY ', IDELINT, ' SECONDS, STARTING AT: ', CDTINTT
   END IF
   WRITE(IU06,*) '                              F = FALSE   T = TRUE '
   WRITE(IU06,*) '                                                           ',&
&                                                        'PRINTER     UNIT'
   DO I=1,30
      WRITE(IU06,*) TITL(I),'....', PFLAG( I),'......', FFLAG( I)
   END DO
ELSE
   WRITE(IU06,*) '  OUTPUT OF INTEGRATED PARAMETERS IS NOT REQUESTED '
END IF
WRITE(IU06,*) '  '

IF  (CFLAG25) THEN
   I = LEN_TRIM(FILE25)
   WRITE(IU06,*) ' TO PRINTER AND/OR FILE : ', FILE25(1:I),'YYYYMMDDHHMMSS'
   IF (NOUTT.EQ.0) THEN
      WRITE(IU06,*) ' EVERY ', IDELSPT, ' SECONDS, STARTING AT: ', CDTSPT
   END IF
   WRITE(IU06,*) '                                                           ',&
&                                                        'PRINTER     UNIT'
   DO I=31,34
      WRITE(IU06,*) TITL(I),'....', PFLAG( I),'......', FFLAG( I)
   END DO

   WRITE(IU06,*) '  '
   WRITE(IU06,*) ' OUTPUT SITES FOR SPECTRA:'
   WRITE(IU06,*) ' TOTAL NUMBER OF SITES IS..........:', NOUTP
   WRITE(IU06,*) '  '
   WRITE(IU06,'('' | LONGITUDE |  LATITUDE |       SITE NAME      |'',         &
&                  ''    POINT   |'')')
   WRITE(IU06,'('' |-----------|-----------|----------------------|'',         &
&                  ''------------|'')')
   IF (ALLOCATED(IJAR)) THEN
      DO I = 1,NOUTP
         WRITE(IU06,'('' | '',F9.4,'' | '',F9.4,'' | '',A20,'' | '',           &
&                     I10,'' |'')') OUTLONG(I), OUTLAT(I), NAME(I), IJAR(I)
      END DO
   ELSE
      DO I = 1,NOUTP
         WRITE(IU06,'('' | '',F9.4,'' | '',F9.4,'' | '',A20,'' | '',           &
&                   A,'' |'')') OUTLONG(I), OUTLAT(I), NAME(I), 'UNDEFIENED'
      END DO
   END IF
ELSE
   WRITE(IU06,*) '  OUTPUT OF SPECTRA IS NOT REQUESTED '
   WRITE(IU06,*) '  OUTPUT SITES OR PARAMETER WERE NOT SPECIFIED'
   NOUTP = 0
END IF
WRITE(IU06,*) '  '

END SUBROUTINE PRINT_OUTPUT_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MAKE_OUTPUT_SITES

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MAKE_OUTPUT_SITES - ROUTINE TO COMPUTE OUTPUT INDICES.                     !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       TO COMPUTES THE INDICES OF SPECTRA OUTPUT POINTS.                      !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE LATITUDE AND LOGITUDE ARE CONVERTED TO INDICES.                    !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER  :: IO, NG

LOGICAL, ALLOCATABLE, DIMENSION(:) :: MASK

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. NO OUTPUT POINTS SPECIFIED.                                           !
!        ---------------------------                                           !

IF (NOUTP.EQ.0) RETURN

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. SEARCH BLOCK NUMBER AND SEA POINT NUMBER.                             !
!        -----------------------------------------                             !

IF (.NOT.ALLOCATED(IJAR)) ALLOCATE (IJAR(1:NOUTP))

CALL FIND_SEA_POINT (NOUTP, OUTLAT, OUTLONG, IJAR)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. REMOVE OUTPUT POINTS WHICH ARE NOT IN GRID.                           !
!        -------------------------------------------                           !

IF (.NOT.ALLOCATED(MASK)) ALLOCATE(MASK(1:NOUTP))

MASK(1:NOUTP) = (IJAR(1:NOUTP).GT.0)
NG = COUNT(MASK(1:NOUTP))

IF (NG.LT.NOUTP) THEN
   WRITE (IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE (IU06,*) ' +                                             +'
   WRITE (IU06,*) ' +  WARNING ERROR FROM SUB. MAKE_OUTPUT_SITES  +'
   WRITE (IU06,*) ' +  =========================================  +'
   WRITE (IU06,*) ' +                                             +'
   WRITE (IU06,*) ' + A SEAPOINT WAS NOT FOUND FOR ', NOUTP-NG
   WRITE (IU06,*) ' + OUTPUT SITE REQUESTS.                       +'
   WRITE (IU06,*) ' + THE FOLLOWING SITES ARE IGNORED.            +'
   DO IO = 1,NOUTP
      IF (IJAR(IO).LE.0) WRITE(IU06,'(4X,I5,2F10.4,2X,A20)')                   &
&                               IO, OUTLONG(IO), OUTLAT(IO), NAME(IO)
    END DO
   WRITE (IU06,*) ' + NUMBER OF OUTPUT POINTS IS NGOUT = ', NG
   WRITE (IU06,*) ' +                                             +'
   WRITE (IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++++'

   IJAR(1:NG) = PACK (IJAR(1:NOUTP), MASK(1:NOUTP))
   NAME(1:NG) = PACK (NAME(1:NOUTP), MASK(1:NOUTP))
   OUTLONG(1:NG) = PACK (OUTLONG(1:NOUTP), MASK(1:NOUTP))
   OUTLAT(1:NG)  = PACK (OUTLAT(1:NOUTP), MASK(1:NOUTP))
   NOUTP = NG
END IF

DEALLOCATE (MASK)

END SUBROUTINE MAKE_OUTPUT_SITES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE UPDATE_OUTPUT_TIME

INTEGER :: J

IF (CFLAG20 .OR. CFLAG25) THEN
   IF (NOUTT.GT.0) THEN
      DO J = 1,NOUTT
         IF (CDTPRO.EQ.COUTT(J)) THEN
            IF (CFLAG20) CDTINTT = COUTT(J)
            IF (CFLAG25) CDTSPT  = COUTT(J)
            EXIT
         END IF
      END DO
   ELSE
      IF ((CFLAG20) .AND. CDTINTT.LT.CDTPRO) CALL INCDATE (CDTINTT,IDELINT)
      IF ((CFLAG25) .AND. CDTSPT .LT.CDTPRO) CALL INCDATE (CDTSPT,IDELSPT)
   END IF
END IF

END SUBROUTINE UPDATE_OUTPUT_TIME

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SAVE_OUTPUT_FILES (IU1, FILE1, IU2, FILE2)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SAVE_OUTPUT_FILES - GETS/SAVES FILES FROM/TO MASS STORAGE.                 !
!                                                                              !
!     H. GUNTHER    GKSS/ECMWF    OCTOBER 1989                                 !
!     P. JANSSEN    KNMI          OCTOBER 1990   YMP-MODIFICATION              !
!     H. GUNTHER    GKSS/ECMWF    OCTOBER 1990   NEW FILE NAMES.               !
!     H. GUNTHER    GKSS          NOVEMBER 1999  NEW DATES AND FT90.           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       GETS OR SAVES FILES FROM / TO MASS STORAGE.                            !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!        IF OPT = 'S' THEN THE ASSIGNED FILE IS CLOSED AND A NEW ONE IS        !
!           ASSIGNED TO THE UNIT.                                              !
!        IF OPT = 'G' THEN A NEW FILE IS ASSIGNED TO THE UNIT.                 !
!        THE NEW FILES ARE OPENED IN SUB. GFILE.                               !
!                                                                              !
!                                                                              !
!     REFERENCES.                                                              !
!      -----------                                                             !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER,           INTENT(IN) :: IU1         !! FILE OUTPUT UNIT FOR PRAMETER.
CHARACTER (LEN=*), INTENT(IN) :: FILE1       !! FILE NAME FOR PRAMETER.
INTEGER,           INTENT(IN) :: IU2         !! FILE OUTPUT UNIT FOR SPECTRA.
CHARACTER (LEN=*), INTENT(IN) :: FILE2       !! FILE NAME FOR SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE                                                           !

CHARACTER     :: CDATEN*14
INTEGER       :: IFAIL      !! OPEN ERROR CODE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLOSE OLD FILES.                                                      !
!     -------------------                                                      !

IF (FFLAG20) CLOSE (UNIT=IU1, STATUS ="KEEP")     !! INTEGRATED PARAMETER FILE.
IF (FFLAG25) CLOSE (UNIT=IU2, STATUS ="KEEP")     !! SPECTRA FILE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. OPEN A NEW FILE IF MODEL DATE IS BEFORE END DATE.                     !
!        -------------------------------------------------                     !

IF (CDTPRO.LT.CDATEE) THEN
   CDATEN = CDTPRO                      !! DATE OF NEW FILE.
   CALL INCDATE(CDATEN, IDELRES)
   IF (FFLAG20)  THEN                            !! INTEGRATED PARAMETER FILE.
      CALL OPEN_FILE (IU06, IU1, FILE1, CDATEN, 'UNKNOWN', IFAIL)
      IF (IFAIL.NE.0) CALL ABORT1
   END IF

   IF (FFLAG25)  THEN                            !! SPECTRA FILE.
      CALL OPEN_FILE (IU06, IU2, FILE2, CDATEN, 'UNKNOWN', IFAIL)
      IF (IFAIL.NE.0) CALL ABORT1
   END IF
END IF

END SUBROUTINE SAVE_OUTPUT_FILES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_OUTPUT_SET_UP_MODULE
