MODULE WAM_FRE_DIR_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS THE SET-UP OF THE FREQUENCY-DIRECTION GRID AND        !
!   THE PRECOMPUTED FREQUENY DIRECTION DEPENDENT CONSTANTS OF THE WAM MODEL.   !
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

USE WAM_FILE_MODULE,    ONLY: IU06, ITEST
USE WAM_GENERAL_MODULE, ONLY: G, PI, ZPI, DEG, R

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!    1. FREQUENCY AND DIRECTION GRID.                                          !

INTEGER            :: ML = -1    !! NUMBER OF FREQUENCIES
INTEGER            :: KL = -1    !! NUMBER OF DIRECTIONS
REAL               :: DELTH = -1 !! ANGULAR INCREMENT OF SPECTRUM [RAD].
REAL               :: DELTR = -1 !! DELTH TIMES RADIUS OF EARTH [RAD*M].
REAL,    PARAMETER :: CO = 1.1   !! FREQUENCY RATIO.
REAL,    PARAMETER :: WETAIL = 0.25   !! WAVE ENERGY TAIL CONSTANT FACTOR.
REAL,    PARAMETER :: FRTAIL = 0.2    !! FREQUENCY TAIL CONSTANT FACTOR.
REAL,    PARAMETER :: WP1TAIL = 1./3. !! PERIOD 1 TAIL CONSTANT FACTOR.
REAL,    PARAMETER :: WP2TAIL = 0.5   !! PERIOD 2 TAIL CONSTANT FACTOR.
REAL,    PARAMETER :: COEF4 = 3.0E-07 !! COEFFICIENT USED TO COMPUTE THE 
                                      !! SPECTRAL LIMITER IN IMPLSCH.

REAL,ALLOCATABLE :: FR(:)        !! FREQUENCIES [HZ].
REAL,ALLOCATABLE :: DFIM(:)      !! FREQUENCY INTERVAL*DIRECTION INTER.
REAL,ALLOCATABLE :: DFIMOFR(:)   !! DFIM/FR
REAL,ALLOCATABLE :: DFFR(:)      !! DFIM*FR
REAL,ALLOCATABLE :: DFFR2(:)     !! DFIM*FR**2 

REAL,ALLOCATABLE, DIMENSION(:) :: GOM   !! DEEP WATER GROUP VELOCITIES [M/S].
REAL,ALLOCATABLE, DIMENSION(:) :: C     !! DEEP WATER PHASE VELOCITIES [M/S].
REAL,ALLOCATABLE, DIMENSION(:) :: TH    !! DIRECTIONS IN RADIANS.
REAL,ALLOCATABLE, DIMENSION(:) :: COSTH !! COS OF DIRECTION.
REAL,ALLOCATABLE, DIMENSION(:) :: SINTH !! SIN OF DIRECTION.

! ---------------------------------------------------------------------------- !
!                                                                              !
!    2. SHALLOW WATER TABLES.                                                  !

INTEGER, PARAMETER :: NDEPTH = 52   !! LENGTH OF SHALLOW WATER TABLES.
REAL,    PARAMETER :: DEPTHA = 5.0  !! MINIMUM DEPTH FOR TABLES [M].
REAL,    PARAMETER :: DEPTHD = 1.1  !! DEPTH RATIO.

REAL,   ALLOCATABLE, DIMENSION(:,:) :: TCGOND !! SHALLOW WATER GROUP VELOCITY.
REAL,   ALLOCATABLE, DIMENSION(:,:) :: TFAK   !! SHALLOW WATER WAVE NUMBER.
REAL,   ALLOCATABLE, DIMENSION(:,:) :: TSIHKD !! TABLE FOR OMEGA/SINH(2KD).

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SET_FRE_DIR                !! TRANSFERS FREQUENY AND DIRECTION
   MODULE PROCEDURE SET_FRE_DIR      !! GRID DEFINITIONS TO MODULE
END INTERFACE
PUBLIC SET_FRE_DIR

INTERFACE PRINT_FRE_DIR_STATUS       !! PRINTS WAM_FRE_DIR_MODULE DATA.
   MODULE PROCEDURE PRINT_FRE_DIR_STATUS
END INTERFACE
PUBLIC PRINT_FRE_DIR_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE MAKE_FRE_DIR               !! COMPUTE FREQUENCY-DIRECTION ARRAYS.
   MODULE PROCEDURE MAKE_FRE_DIR
END INTERFACE
PRIVATE MAKE_FRE_DIR

INTERFACE MAKE_SHALLOW_TABLES        !! COMPUTE TABLES FOR SHALLOW WATER.
   MODULE PROCEDURE MAKE_SHALLOW_TABLES
END INTERFACE
PRIVATE MAKE_SHALLOW_TABLES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_FRE_DIR (N_DIR, N_FRE, FR1)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SET_FRE_DIR - ROUTINE TO PREPARE WAM_FRE_DIR MODULE.                       !
!                                                                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       TO COMPUTE ALL VARAIABLES IN WAM_FRE_DIR MODULE.                       !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       DONE BY CALLS TO MANY SUBS.                                            !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER , INTENT(IN) :: N_DIR   !! NUMBER OF DIRECTIONS.
INTEGER , INTENT(IN) :: N_FRE   !! NUMBER OF FREQUENCIES.
REAL    , INTENT(IN) :: FR1     !! FIRST FREQUENCY [HZ]

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLEAR FRE_DIR GRID.                                                   !
!        -------------------                                                   !

KL = -1
ML = -1

IF (ALLOCATED (FR)     ) DEALLOCATE (FR)
IF (ALLOCATED (GOM)    ) DEALLOCATE (GOM)
IF (ALLOCATED (C)      ) DEALLOCATE (C)
IF (ALLOCATED (TH)     ) DEALLOCATE (TH)
IF (ALLOCATED (COSTH)  ) DEALLOCATE (COSTH)
IF (ALLOCATED (SINTH)  ) DEALLOCATE (SINTH)
IF (ALLOCATED (DFIM)   ) DEALLOCATE (DFIM)
IF (ALLOCATED (DFIMOFR)) DEALLOCATE (DFIMOFR)
IF (ALLOCATED (DFFR)   ) DEALLOCATE (DFFR)
IF (ALLOCATED (DFFR2)  ) DEALLOCATE (DFFR2)
IF (ALLOCATED (TCGOND) ) DEALLOCATE (TCGOND)
IF (ALLOCATED (TFAK)   ) DEALLOCATE (TFAK)
IF (ALLOCATED (TSIHKD) ) DEALLOCATE (TSIHKD)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. CHECK INPUT DATA.                                                     !
!        -----------------                                                     !

IF (N_DIR.LE.0 .OR. N_FRE.LE.0 .OR. FR1.LE.0.) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *          FATAL ERROR IN SUB. SET_FRE_DIR               *'
   WRITE (IU06,*) ' *          ===============================               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * FREQUENCY DIRECTION GRID IS NOT DEFINED.               *'
   WRITE (IU06,*) ' * NUMBER OF DIRECTIONS IS N_DIR = ', N_DIR
   WRITE (IU06,*) ' * NUMBER OF FREUENCIES IS N_FRE = ', N_FRE
   WRITE (IU06,*) ' * FIRST FREQUENCY [HZ]: FR1                              *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COPY FRE_DIR GRID DEFINITIONS.                                        !
!        ------------------------------                                        !

ML = N_FRE
KL = N_DIR
ALLOCATE (FR(ML))
FR(1) = FR1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. FREQUENCY-DIRECTION GRID.                                             !
!        -------------------------                                             !

CALL MAKE_FRE_DIR
IF (ITEST.GT.1) WRITE (IU06,*) ' SUB. MAKE_FRE_DIR DONE'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. SHALLOW WATER TABLES.                                                 !
!        ---------------------                                                 !

CALL MAKE_SHALLOW_TABLES
IF (ITEST.GT.1) WRITE (IU06,*) ' SUB. MAKE_SHALLOW_TABLES DONE'

END SUBROUTINE SET_FRE_DIR

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_FRE_DIR_STATUS

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PRINT_FRE_DIRT_STATUS - PRINT STATUS OF WAM_FRE_DIR_MODULE.                !
!                                                                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000                           !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       MAKE A PRINTER OUTPUT OF THE DATA STORED IN WAN_SOURCE MODULE.         !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       NONE.                                                                  !
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

INTEGER, PARAMETER :: NAN  = 10 !! STEPS FOR SHALLOW WATER TABLE PRINT.
INTEGER :: K, I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. FREQUENCY AND DIRECTION GRID.                                         !
!        -----------------------------                                         !

WRITE (IU06,'(/,'' ----------------------------------------'')')
WRITE (IU06,'(  ''  FREQUENCY AND DIRECTION MODULE STATUS'')')
WRITE (IU06,'(  '' ----------------------------------------'')')
WRITE (IU06,*)' '
IF (KL.GT.0 .AND. ML.GT.0) THEN
   WRITE (IU06,'('' NUMBER OF DIRECTIONS  IS  KL = '',I3)') KL
   WRITE (IU06,'('' NUMBER OF FREQUENCIES IS  ML = '',I3)') ML
   WRITE (IU06,'('' FIRST FREQUENCY IS     FR(1) = '',F10.8,'' [HZ]'')') FR(1)
ELSE
   WRITE (IU06,*) '  INPUT DATA TO MODULE ARE NOT DEFINED'
END IF

IF (ALLOCATED(DFIM)) THEN
   WRITE (IU06,'('' MODEL FREQUENCIES IN HERTZ:'')')
   WRITE (IU06,'(1X,13F10.5)') FR(1:ML)
   WRITE (IU06,'(/,'' MODEL DIRECTIONS IN DEGREE (CLOCKWISE FROM NORTH):'')')
   WRITE (IU06,'(1X,13F10.5)') TH(1:KL)*DEG
   IF (ITEST.GT.0) THEN
      WRITE (IU06,'(/,'' MODEL FREQUENCY INTERVALLS TIMES DIRECTION'',         &
&              '' INTERVALL IN HERTZ*RADIENS'')')
      WRITE (IU06,'(1X,13F10.5)') DFIM(1:ML)
      WRITE (IU06,'(/,'' MODEL DEEP WATER GROUPVELOCITY IN M/S:'')')
      WRITE (IU06,'(1X,13F10.5)') GOM(1:ML)
      WRITE (IU06,'(/,'' MODEL DEEP WATER PHASEVELOCITY IN M/S:'')')
      WRITE (IU06,'(1X,13F10.5)') C(1:ML)
   END IF
ELSE
   WRITE (IU06,*) '  MODULE DATA ARE NOT PREPARED'
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. SHALLOW WATER TABLES.                                                 !
!        ---------------------                                                 !

WRITE (IU06,'(/,'' ----------------------------------------'')')
WRITE (IU06,'(  ''            SHALLOW WATER TABLES'')')
WRITE (IU06,'(  '' ----------------------------------------'',/)')
K = MAX(NDEPTH/NAN,1)
WRITE (IU06,'(''  LOGARITHMIC DEPTH FROM: DEPTHA = '',F5.1,'' TO DEPTHE  = '', &
&             F5.1, ''IN STEPS OF DEPTHD = '',F5.1)')                          &
&             DEPTHA, DEPTHA*DEPTHD**(NDEPTH-1), DEPTHD
IF (ALLOCATED(TSIHKD)) THEN
   IF (ITEST.GT.0) THEN
      WRITE (IU06,'(''  PRINTED IN STEPS OF '',I3,'' ENTRIES'',/)') K
      DO I = 1,NDEPTH,K
         WRITE (IU06,'('' DEPTH = '',F7.1,'' METRES '')') DEPTHA*DEPTHD**(I-1)
         WRITE (IU06,'('' GROUP VELOCITY IN METRES/SECOND'')')
         WRITE (IU06,'(1X,13F10.5)') TCGOND(I,1:ML)
         WRITE (IU06,'('' WAVE NUMBER IN 1./METRES'')')
         WRITE (IU06,'(1X,13F10.5)') TFAK(I,1:ML)
         WRITE (IU06,'('' OMEGA/SINH(2KD) IN 1./SECOND'')')
         WRITE (IU06,'(1X,13F10.5)') TSIHKD(I,1:ML)
      END DO
   END IF
ELSE
   WRITE (IU06,*) '  MODULE DATA ARE NOT PREPARED'
END IF

END SUBROUTINE PRINT_FRE_DIR_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVAT MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MAKE_FRE_DIR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: M, K
REAL    :: CO1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE FREQUENCIES.                                                  !
!        --------------------                                                  !

DO M = 2,ML
   FR(M) = CO*FR(M-1)
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COMPUTE DEEP WATER GROUP VELOCITIES.                                  !
!        ------------------------------------                                  !

IF (.NOT.ALLOCATED (GOM))  ALLOCATE (GOM(1:ML))
GOM = G/(2.*ZPI*FR)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. COMPUTE PHASE VELOCITY IN DEEP WATER.                                 !
!         -------------------------------------                                !

IF (.NOT.ALLOCATED (C))  ALLOCATE (C(1:ML))
C = G/(ZPI*FR)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. COMPUTATION OF DIRECTION BANDWIDTH.                                   !
!        -----------------------------------                                   !

DELTH = ZPI/REAL(KL)
DELTR = DELTH*R

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. COMPUTATION OF DIRECTIONS.                                            !
!        --------------------------                                            !

IF (.NOT.ALLOCATED (TH))  ALLOCATE (TH(1:KL))
DO K = 1,KL
   TH(K) = REAL(K-1)*DELTH + 0.5*DELTH
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. COMPUTATION SIN AND COS OF DIRECTIONS.                                !
!        --------------------------------------                                !

IF (.NOT.ALLOCATED (COSTH))  ALLOCATE (COSTH(1:KL))
IF (.NOT.ALLOCATED (SINTH))  ALLOCATE (SINTH(1:KL))
COSTH = COS(TH)
SINTH = SIN(TH)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. COMPUTATION FREQUENCY DIRECTION AREAS                                 !
!        -------------------------------------                                 !

IF (.NOT.ALLOCATED (DFIM)   )  ALLOCATE (DFIM(1:ML))
IF (.NOT.ALLOCATED (DFIMOFR))  ALLOCATE (DFIMOFR(1:ML))
IF (.NOT.ALLOCATED (DFFR)   )  ALLOCATE (DFFR(1:ML))
IF (.NOT.ALLOCATED (DFFR2)  )  ALLOCATE (DFFR2(1:ML))

CO1 = 0.5*(CO-1.)*DELTH
DFIM(1) = CO1*FR(1)
DFIM(2:ML-1) = CO1 * (FR(2:ML-1)+FR(1:ML-2))
DFIM(ML) = CO1*FR(ML-1)

DFIMOFR = DFIM/FR
DFFR = DFIM*FR
DFFR2 = DFIM*FR**2

END SUBROUTINE MAKE_FRE_DIR

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MAKE_SHALLOW_TABLES

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MAKE_SHALLOW_TABLES - ROUTINE TO COMPUTE TABLES USED FOR SHALLOW WATER.    !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO COMPUTE TABLES USED FOR SHALLOW WATER.                              !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!      TABLES FOR GROUP VELOCITY, WAVE NUMBER AND OMEGA/SINH(2KD) ARE COMPUTED !
!      AT ALL FREQUENCIES AND FOR A DEPTH TABLE OF LENGTH NDEPTH, STARTING AT  !
!      DEPTHA METERS AND INCREMENTED BY DEPTHD METRES.                         !
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

INTEGER        :: M, JD, NAN, NSTP
REAL           :: GH, OM, AD, AK, AKD, DEPTHE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. ALLOCATE TABLE ARRAYS.                                                !
!        ----------------------                                                !

IF (.NOT.ALLOCATED (TFAK))    ALLOCATE (TFAK  (NDEPTH,ML))
IF (.NOT.ALLOCATED (TCGOND))  ALLOCATE (TCGOND(NDEPTH,ML))
IF (.NOT.ALLOCATED (TSIHKD))  ALLOCATE (TSIHKD(NDEPTH,ML))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. GROUP VELOCITY AND WAVE NUMBER.                                       !
!        -------------------------------                                       !

GH = G/(4.*PI)
DO M = 1,ML                             !! LOOP OVER FREQUENCIES.
   OM=ZPI*FR(M)
   DO JD = 1,NDEPTH                     !! LOOP OVER DEPTH.
      AD = DEPTHA*DEPTHD**(JD-1)
      AK = AKI(OM,AD)
      TFAK(JD,M) = AK
      AKD = AK*AD
      IF (AKD.LE.10.0) THEN
         TCGOND(JD,M) = 0.5*SQRT(G*TANH(AKD)/AK) * (1.0+2.0*AKD/SINH(2.*AKD))
         TSIHKD(JD,M) = OM/SINH(2.*AKD)
      ELSE
         TCGOND(JD,M) = GH/FR(M)
         TSIHKD(JD,M) = 0.
      END IF
   END DO
END DO

! ---------------------------------------------------------------------------- !

RETURN

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INTERNAL FUNCTION.                                                    !
!        ------------------                                                    !

CONTAINS

REAL FUNCTION AKI (OM, BETA)

   ! ------------------------------------------------------------------------- !
   !                                                                           !
   !   AKI - FUNCTION TO COMPUTE WAVE NUMBER.                                  !
   !                                                                           !
   !     G. KOMEN, P. JANSSEN   KNMI        01/06/1986                         !
   !                                                                           !
   !     PURPOSE.                                                              !
   !     -------                                                               !
   !                                                                           !
   !       WAVE NUMBER AS FUNCTION OF CIRCULAR FREQUENCY AND WATER DEPTH.      !
   !                                                                           !
   !     METHOD.                                                               !
   !     -------                                                               !
   !                                                                           !
   !       NEWTONS METHOD TO SOLVE THE DISPERSION RELATION IN SHALLOW WATER.   !
   !                                                                           !
   !     REFERENCE.                                                            !
   !     ----------                                                            !
   !                                                                           !
   !       NONE.                                                               !
   !                                                                           !
   ! ------------------------------------------------------------------------- !

   REAL, INTENT(IN) :: OM    !! CIRCULAR FREQUENCY.
   REAL, INTENT(IN) :: BETA  !! WATER DEPTH.

   ! ------------------------------------------------------------------------- !
   !                                                                           !
   !     LOCAL VARIABLES.                                                      !
   !     ----------------                                                      !

   REAL, PARAMETER :: EBS = 0.0001  !! RELATIVE ERROR LIMIT OF NEWTON'S METHOD.

   REAL :: AKP, BO, TH, STH

   ! ------------------------------------------------------------------------- !
   !                                                                           !
   !     1. START WITH MAXIMUM FROM DEEP AND EXTREM SHALLOW WATER WAVE NUMBER. !
   !        ------------------------------------------------------------------ !

   AKI   = MAX ( OM**2/(4.*G), OM/(2.*SQRT(G*BETA)) )

   ! ------------------------------------------------------------------------- !
   !                                                                           !
   !     2. ITERATION LOOP.                                                    !
   !        ---------------                                                    !

   AKP = 10000.
   DO WHILE (ABS(AKP-AKI).GT.EBS*AKI)
      BO = BETA*AKI
      IF (BO.GT.40.) THEN
         AKI = OM**2/G
         EXIT
      ELSE
         AKP = AKI
         TH = G*AKI*TANH(BO)
         STH = SQRT(TH)
         AKI = AKI+(OM-STH)*STH*2./(TH/AKI+G*BO/COSH(BO)**2)
      END IF
   END DO

   END FUNCTION AKI

END SUBROUTINE MAKE_SHALLOW_TABLES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_FRE_DIR_MODULE
