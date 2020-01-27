MODULE WAM_TIMOPT_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL MODEL TIMES, TIMESTEPS AND OPTIONS                !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1                      !! TERMINATES PROCESSING.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_FILE_MODULE,     ONLY: IU06

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. TIME STEPS.                                                           !
!        -----------                                                           !

INTEGER  :: IDELPRO =-1 !! TIMESTEP WAM PROPAGATION IN SECONDS.
INTEGER  :: IDELT   =-1 !! TIMESTEP SOURCE FUNCTION IN SECONDS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. PROPAGATION OPTIONS.                                                  !
!        --------------------                                                  !

INTEGER  :: ICASE   = 1   !! PROPAGATION FLAG
                          !!  = 1  SPHERICAL COORDINATES
                          !! OTHERWISE CARTESIAN COORDINATES.
INTEGER  :: ISHALLO = 1   !! SHALLOW WATER MODEL FLAG
                          !! = 1  DEEP WATER MODEL
                          !! OTHERWISE  SHALLOW WATER MODEL.
INTEGER  :: IREFRA  = 0   !! REFRACTION OPTION..
                          !! = 0  NO REFRACTION.
                          !! = 1 DEPTH REFRACTION.
                          !! = 2 DEPTH AND CURRENT REFRACTION.
INTEGER  :: ISBR = 0      !! WAVE BREAKING OPTION
                          !! = 1  WAVE BREAKING ACTIVE
                          !! OTHERWISE  INACTIVE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. START OPTION.                                                         !
!        -------------                                                         !

INTEGER :: IOPTI = -1   !! = -1 HOT START.                              
                        !! =  0 WIND INDEPENDENT INITIAL VALUES.                              
                        !! =  1 WIND DEPENDENT INITIAL VALUES AND                             
                        !!      ENERGY EQUAL ZERO IF WINDSPEED IS ZERO                        
                        !! =  2 WIND DEPENDENT INITIAL VALUES AND                             
                        !!      ENERGY COMPUTED FROM GIVEN PARAMETERS IF                      
                        !!      WINDSPEED IS ZERO. 

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. TIME STATUS OF INTEGRATION.                                           !
!        ---------------------------                                           !

CHARACTER (LEN=14) :: CDATEA =' '  !! START DATE OF RUN  (VVYYMMDDHHMMSS).
CHARACTER (LEN=14) :: CDATEE =' '  !! END DATE OF RUN (VVYYMMDDHHMMSS).
CHARACTER (LEN=14) :: CDTPRO =' '  !! END DATE OF PROPAGATION.
CHARACTER (LEN=14) :: CDTSOU =' '  !! END DATE OF SOURCE INTEGRATION.
CHARACTER (LEN=14) :: CDA     =' ' !! DATE OF WINDFIELD USED IN WAM.
CHARACTER (LEN=14) :: CDATEWO =' ' !! DATE OF NEXT WIND FIELD TO BE USED.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SET_MODEL_OPTION             !! SETS MODEL OPTIONS.
   MODULE PROCEDURE SET_MODEL_OPTION
END INTERFACE
PUBLIC SET_MODEL_OPTION

INTERFACE SET_INTEGRATION_TIMESTEPS    !! SETS MODEL TIMESTEPS.
   MODULE PROCEDURE SET_INTEGRATION_TIMESTEPS
END INTERFACE
PUBLIC SET_INTEGRATION_TIMESTEPS

INTERFACE SET_INTEGRATION_PERIOD      !! SETS MODEL TIMESTEPS.
   MODULE PROCEDURE SET_INTEGRATION_PERIOD
END INTERFACE
PUBLIC SET_INTEGRATION_PERIOD

INTERFACE PRINT_TIMOPT_STATUS          !! PRINTS MODULE STATUS.
   MODULE PROCEDURE PRINT_TIMOPT_STATUS
END INTERFACE
PUBLIC PRINT_TIMOPT_STATUS

INTERFACE SET_START_OPTION             !! SETS START OPTION.
   MODULE PROCEDURE SET_START_OPTION
END INTERFACE
PUBLIC SET_START_OPTION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_MODEL_OPTION (CS, DS, R, B)

INTEGER, INTENT(IN)           :: CS  !! PROPAGATION FLAG
                                     !!  = 1  SPHERICAL COORDINATES
                                     !! OTHERWISE CARTESIAN COORDINATES.

INTEGER, INTENT(IN)           :: DS  !! SHALLOW WATER MODEL FLAG
                                     !! = 1  DEEP WATER MODEL
                                     !! OTHERWISE  SHALLOW WATER MODEL.

INTEGER, INTENT(IN), OPTIONAL :: R   !! REFRACTION OPTION..
                                     !! = 0  NO REFRACTION.
                                     !! = 1 DEPTH REFRACTION.
                                     !! = 2 DEPTH AND CURRENT REFRACTION.
INTEGER, INTENT(IN), OPTIONAL :: B   !! WAVE BREAKING OPTION..
                                     !! = 0  NO BREAKING.
                                     !! = 1 BREAKING ACTIVE.
                                    
IF (CS.EQ.1) THEN
   ICASE = 1
ELSE
   ICASE = 0
END IF

IF (DS.EQ.1) THEN
   ISHALLO = 1
ELSE
   ISHALLO = 0
END IF

IF (PRESENT(R)) THEN
   IREFRA   = R
ELSE
   IREFRA   = 0
END IF

IF (PRESENT(B)) THEN
   ISBR   = B
ELSE
   ISBR   = 0
END IF

END SUBROUTINE SET_MODEL_OPTION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_INTEGRATION_TIMESTEPS (P, S)

INTEGER, INTENT(IN)   :: P  !! PROPAGATION TIMSTEP [S].
INTEGER, INTENT(IN)   :: S  !! SOURCE FUNCTION TIMESTEP [S].

IDELPRO = P
IDELT   = S

END SUBROUTINE SET_INTEGRATION_TIMESTEPS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_INTEGRATION_PERIOD (B, E)

CHARACTER (LEN=14), INTENT(IN)   :: B   !! START DATE OF MODEL RUN.
CHARACTER (LEN=14), INTENT(IN)   :: E   !! END DATE OF MODEL RUN.

CDATEA = B
CDATEE = E

END SUBROUTINE SET_INTEGRATION_PERIOD

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_TIMOPT_STATUS

WRITE(IU06,*) '  '
WRITE(IU06,*) ' ------------------------------------------------- '
WRITE(IU06,*) '              MODEL TIMOPT STATUS:'
WRITE(IU06,*) ' ------------------------------------------------- '

WRITE(IU06,*) '  '
WRITE(IU06,*) ' START DATE (YYYYMMDDHHMMSS) IS.....: ', CDATEA
WRITE(IU06,*) ' END   DATE (YYYYMMDDHHMMSS) IS.....: ', CDATEE
WRITE(IU06,*) ' DATE OF PROPAGATION INTEGRATION IS.: ', CDTPRO
WRITE(IU06,*) ' DATE OF SOURCE INTEGRATION IS......: ', CDTSOU
WRITE(IU06,*) ' DATE OF WINDFIELD IS...............: ', CDA
WRITE(IU06,*) ' DATE FOR NEXT WIND FIELD IS........: ', CDATEWO

WRITE(IU06,*) '  '
WRITE(IU06,*) ' MODEL TIME STEPS:'
WRITE(IU06,*) ' SOURCE TERM INTEGRATION TIME STEP..: ',IDELT,' SECS'
WRITE(IU06,*) ' PROPAGATION TIME STEP .............: ',IDELPRO,' SECS'
WRITE(IU06,*) '  '
WRITE(IU06,*) ' MODEL OPTIONS:'
IF (ICASE.EQ.1) THEN
   WRITE(IU06,*) ' PROPAGATION GRID SPHERICAL LAT/LON COORDINATES'
ELSE
   WRITE(IU06,*) ' PROPAGATION GRID CARTESIAN COORDINATES'
END IF
IF (ISHALLO.EQ.1) THEN
   WRITE(IU06,*) ' THIS IS A DEEP WATER RUN '
ELSE
   WRITE(IU06,*) ' THIS IS A SHALLOW WATER RUN '
END IF
IF (IREFRA.EQ.0) THEN
   WRITE(IU06,*) ' MODEL RUNS WITHOUT REFRACTION'
ELSE IF (IREFRA.EQ.1) THEN
   WRITE(IU06,*) ' MODEL RUNS WITH DEPTH REFRACTION'
ELSE IF (IREFRA.EQ.2) THEN
   WRITE(IU06,*) ' MODEL RUNS WITH DEPTH AND CURRENT REFRACTION'
END IF

IF (ISBR.EQ.0) THEN
   WRITE(IU06,*) ' MODEL RUNS WITHOUT WAVE BREAKING'
ELSE 
   WRITE(IU06,*) ' MODEL RUNS WITH  WAVE BREAKING'
END IF

IF (IOPTI.LT.0) THEN
   WRITE (IU06,*) ' INITIAL VALUES FROM A PREVIOUS MODEL RUN (HOT START).'
ELSE 
   IF (IOPTI.EQ.0) THEN
      WRITE (IU06,*) ' INITIAL VALUES ARE COMPUTED FROM INPUT PARAMETERS.'
   ELSE IF (IOPTI.EQ.1) THEN
      WRITE (IU06,*) ' INITIAL VALUES ARE COMPUTED FROM FETCH LAW.'
      WRITE (IU06,*) '    WAVE ENERGY IS ZERO IN CALM WIND AREAS.'
   ELSE IF (IOPTI.EQ.2) THEN
      WRITE (IU06,*) ' INITIAL VALUES ARE COMPUTED FROM FETCH LAW.'
      WRITE (IU06,*) '    PARAMETERS USED IN CALM WIND AREAS.'
   END IF
END IF
WRITE(IU06,*) '  '

END SUBROUTINE PRINT_TIMOPT_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_START_OPTION (OPTION)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER, INTENT(IN)  :: OPTION   !! MODEL START OPTION.

! ---------------------------------------------------------------------------- !

IOPTI  = OPTION

IF (IOPTI.GT.2) THEN
   WRITE(IU06,*) ' ************************************************'
   WRITE(IU06,*) ' *                                              *'
   WRITE(IU06,*) ' *     FATAL ERROR IN SUB. SET_START_OPTION     *'
   WRITE(IU06,*) ' *     ====================================     *'
   WRITE(IU06,*) ' * OPTION FOR INITIAL VALUES IS OUT OF RANGE.   *'
   WRITE(IU06,*) ' * OPTION IS       IOPTI = ', IOPTI
   WRITE(IU06,*) ' * POSSIBLE COLD START OPTIONS ARE: 0, 1, OR 2  *'
   WRITE(IU06,*) ' * HOT START IF IOPTI .LT. 0                    *'
   WRITE(IU06,*) ' *                                              *'
   WRITE(IU06,*) ' *      PROGRAM ABORTS  PROGRAM ABORTS          *'
   WRITE(IU06,*) ' *                                              *'
   WRITE(IU06,*) ' ************************************************'
   CALL ABORT1
END IF

END SUBROUTINE SET_START_OPTION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_TIMOPT_MODULE

