MODULE WAM_CURRENT_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL VARIABLES AND CONSTANTS NECESSARY FOR THE         !
!   PREPROC PROGRAM. ALL PROCEDURES ARE INCLUDED TO COMPUTE THE INFORMATION    !
!   STORED IN WAM_CONNST_MODULE.                                               !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATES PROCESSING.
&       ADJUST,                  &  !! ADJUST LONGITUDES.
&       PRINT_ARRAY                 !! PRINTS AN ARRAY.

USE WAM_GRID_MODULE, ONLY:       &
&       INTERPOLATION_TO_GRID       !! INTERPOLATE TO BLOCKS.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_FILE_MODULE,    ONLY: IU06, ITEST
USE WAM_GENERAL_MODULE, ONLY: DEG, EPS
USE WAM_GRID_MODULE,    ONLY: NSEA, AMOWEP, AMOSOP, AMOEAP, AMONOP,    &
&                             L_S_MASK

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE
PRIVATE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CURRENT INPUT DATA AS PROVIDED BY USER.                               !
!        ---------------------------------------                               !

INTEGER             :: NX_C = -1  !! NUMBER OF LONGITUDES.
INTEGER             :: NY_C = -1  !! NUMBER OF LATITUDES.
LOGICAL             :: PER_C      !! .TRUE. IF GRID IS PERIODICAL.
REAL                :: DY_C       !! LATITUDE INCREMENT [DEG].
REAL                :: DX_C       !! LONGITUDE INCREMENT [DEG].
REAL                :: SOUTH_C    !! SOUTH LATITUDE OF GRID [DEG].
REAL                :: NORTH_C    !! NORTH LATITUDE OF GRID [DEG].
REAL                :: WEST_C     !! WEST LONGITUDE OF GRID [DEG].
REAL                :: EAST_C     !! EAST LONGITUDE OF GRID [DEG].
REAL,   ALLOCATABLE :: UCUR(:,:)  !! U-COMP. OF CURRENTS [M/S].
REAL,   ALLOCATABLE :: VCUR(:,:)  !! V-COMP. OF CURRENTS [M/S].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. MODEL CURRENT DATA.                                                   !
!        -------------------                                                   !

REAL,    ALLOCATABLE :: U(:)      !! U - COMPONENT OF CURRENT (M/S).
REAL,    ALLOCATABLE :: V(:)      !! V - COMPONENT OF CURRENT (M/S).
PUBLIC U, V

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SET_CURRENT                !! TRANSFERS CURRENT DATA TO MODULE.
   MODULE PROCEDURE SET_CURRENT
END INTERFACE
PUBLIC SET_CURRENT

INTERFACE PREPARE_CURRENT             !! PREPARES CURRENT FIELD FOR WAM MODEL.
   MODULE PROCEDURE PREPARE_CURRENT
END INTERFACE
PUBLIC PREPARE_CURRENT

INTERFACE PRINT_CURRENT_STATUS         !! PRINTS CURRENT STATUS.
   MODULE PROCEDURE PRINT_CURRENT_STATUS
END INTERFACE
PUBLIC PRINT_CURRENT_STATUS

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

SUBROUTINE SET_CURRENT (N_LON, N_LAT, D_LON, D_LAT,                            &
&                       SOUTH, NORTH, WEST, EAST, U_MAP, V_MAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER, INTENT(IN)  :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, INTENT(IN)  :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL   , INTENT(IN)  :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL   , INTENT(IN)  :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL   , INTENT(IN)  :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: EAST       !! EAST LONGITUDE OF GRID [DEG].
REAL,    INTENT(IN)  :: U_MAP(:,:) !! U-COMP. OF CURRENTS [M/S].
REAL,    INTENT(IN)  :: V_MAP(:,:) !! V-COMP. OF CURRENTS [M/S].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. CLEAR CURRENT MODULE AND SET STATUS TO UNDEFINED.                     !
!        -------------------------------------------------                     !

NX_C = -1
NY_C = -1
IF (ALLOCATED(UCUR))  DEALLOCATE (UCUR)
IF (ALLOCATED(VCUR))  DEALLOCATE (VCUR)
IF (ALLOCATED(U ))    DEALLOCATE (U )
IF (ALLOCATED(V ))    DEALLOCATE (V )

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COPY GRID DEFINITIONS.                                                !
!        ----------------------                                                !

DY_C = D_LAT      !! LATITUDE INCREMENT [DEG].
DX_C = D_LON      !! LONGITUDE INCREMENT [DEG].
SOUTH_C = SOUTH   !! SOUTH LATITUDE OF GRID [DEG].
NORTH_C = NORTH   !! NORTH LATITUDE OF GRID [DEG].
WEST_C = WEST     !! WEST LONGITUDE OF GRID [DEG].
EAST_C = EAST     !! EAST LONGITUDE OF GRID [DEG].
CALL ADJUST (WEST_C, EAST_C)
NX_C = NINT((EAST_C-WEST_C)/DX_C+1.0)      !! NO. OF LONGITUDES IN FILE
NY_C = NINT((NORTH_C-SOUTH_C)/DY_C+1.0)    !! NO. OF LATITUDES IN FILE
PER_C = ABS(EAST+D_LON-360.-WEST).LT.EPS    !! PERIODIC?

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. CHECK CONSISTENCY.                                                    !
!        ------------------                                                    !

IF (N_LON.NE.NX_C .OR. N_LAT.NE.NY_C .OR.                                        &
&   N_LON.NE.SIZE(U_MAP,1) .OR. N_LAT.NE.SIZE(U_MAP,2) .OR.                    &
&   N_LON.NE.SIZE(V_MAP,1) .OR. N_LAT.NE.SIZE(V_MAP,2) ) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *          FATAL  ERROR IN SUB. SET_CURRENT              *'
   WRITE (IU06,*) ' *          ================================              *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * GRID SPECIFICATINOS ARE NOT CONSISTENT                 *'
   WRITE (IU06,*) ' * USER PROVIDED GRID DEFINITIONS ARE:                    *'
   WRITE (IU06,*) ' * LONGITUDE    WEST = ', WEST
   WRITE (IU06,*) ' * LONGITUDE    EAST = ', EAST
   WRITE (IU06,*) ' * LONGITUDE INCREMENT D_LON = ', D_LON
   WRITE (IU06,*) ' * NO. OF LONGITUDES   N_LON = ', N_LON
   WRITE (IU06,*) ' * LATITUDE    NORTH = ', NORTH
   WRITE (IU06,*) ' * LATITUDE    SOUTH = ', SOUTH
   WRITE (IU06,*) ' * LATITUDE  INCREMENT D_LAT = ', D_LAT
   WRITE (IU06,*) ' * NO. OF LATITUDE     N_LAT = ', N_LAT
   WRITE (IU06,*) ' * COMPUTED GRID SIZES ARE:                               *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES    NX_C = ', NX_C
   WRITE (IU06,*) ' * NO. OF LATITUDE      NY_C = ', NY_C
   WRITE (IU06,*) ' * DIMENSIONS OF U CURRENT MAP ARRAY ARE :                *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES 1. DIMENSION = ', SIZE(U_MAP,1)
   WRITE (IU06,*) ' * NO. OF LATITUDE   2. DIMENSION = ', SIZE(U_MAP,2)
   WRITE (IU06,*) ' * DIMENSIONS OF V CURRENT MAP ARRAY ARE :                *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES 1. DIMENSION = ', SIZE(V_MAP,1)
   WRITE (IU06,*) ' * NO. OF LATITUDE   2. DIMENSION = ', SIZE(V_MAP,2)
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   NX_C = -1
   NY_C = -1
   IF (ALLOCATED(UCUR))  DEALLOCATE (UCUR)
   IF (ALLOCATED(VCUR))  DEALLOCATE (VCUR)
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COPY CURRENT FIELDS.                                                  !
!        --------------------                                                  !

ALLOCATE (UCUR(NX_C,NY_C))
ALLOCATE (VCUR(NX_C,NY_C))

UCUR = U_MAP
VCUR = V_MAP

END SUBROUTINE SET_CURRENT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_CURRENT

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PREPARE_CURRENT - ROUTINE PROCESS A CURRENT FIELD.                         !
!                                                                              !
!     SUSANNE HASSELMANN        JUNE     1990.                                 !
!     H. GUNTHER    ECMWF/GKSS  DECEMBER 1990  MODIFIED FOR CYCLE_4.           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        INTERPOLATION TO CURRENT ARRAYS.                                      !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       LINEAR INTERPOLATION.                                                  !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLEAR MODEL CURRENTS AND CHECK MODULE STATUS.                         !
!        ---------------------------------------------                         !

IF (ALLOCATED(U))     DEALLOCATE(U)
IF (ALLOCATED(V))     DEALLOCATE(V)

IF (NX_C .LE. 0 .OR. NY_C .LE. 0) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *          FATAL ERROR IN SUB. PREPARE_CURRENT           *'
   WRITE (IU06,*) ' *          ===================================           *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * CURRENT INPUT DATA ARE NOT DEFINED IN CURRENT MODULE.  *'
   WRITE (IU06,*) ' * DATA MUST BE DEFINED BY SUB. SET_CURRENT.              *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   CALL ABORT1
END IF
IF (NSEA .LE. 0) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *          FATAL  ERROR IN SUB. PREPARE_CURRENT          *'
   WRITE (IU06,*) ' *          ====================================          *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * MODEL GRID IS NOT DEFINED IN WAM_GRID_MODULE.          *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ALLOCATE CURRENT ARRAYS.                                              !
!        -------------------------                                             !

ALLOCATE(U(1:NSEA))
ALLOCATE(V(1:NSEA))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. INTERPOLATION OF INPUT CURRENTS TO MODEL GRID.                        !
!        ----------------------------------------------                        !

U = 0.
V = 0.
CALL INTERPOLATION_TO_GRID (IU06, PER_C, DX_C, DY_C, WEST_C, SOUTH_C,             &
&                           UCUR, U, VCUR, V)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. CLEAR MODULE AND SET STATUS TO INITILIZED.                            !
!        ------------------------------------------                            !

NX_C = -1
NY_C = -1
IF (ALLOCATED(UCUR)) DEALLOCATE (UCUR)
IF (ALLOCATED(VCUR)) DEALLOCATE (VCUR)

END SUBROUTINE PREPARE_CURRENT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_CURRENT_STATUS

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PRINT_CURRENT_STATUS - PRINTS STATUS OF CURRENT MODULE.                    !
!                                                                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       MAKE A PRINTER OUTPUT OF THE STATUS OF THE CURRENT MODULE.             !
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

CHARACTER (LEN=100) :: TITL
CHARACTER (LEN=14)  :: ZERO = ' '

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. PRINT STATUS.                                                         !
!        -------------                                                         !

WRITE (IU06,'(/,'' ----------------------------------------'')')
WRITE (IU06,'(  ''            CURRENT MODULE STATUS'')')
WRITE (IU06,'(  '' ----------------------------------------'')')
WRITE(IU06,*) ' '

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INPUT GRID DEFINITIONS.                                               !
!        -----------------------                                               !

IF (NX_C.GT.0 .AND. NY_C.GT.0) THEN
   WRITE (IU06,'('' CURRENT INPUT GRID: '')')
   WRITE (IU06,'('' NUMBER OF LONGITUDES IS      NX_C = '',I5)') NX_C
   WRITE (IU06,'('' MOST WESTERN LONGITUDE IS  WEST_C = '',F9.4)') WEST_C
   WRITE (IU06,'('' MOST EASTERN LONGITUDE IS  EAST_C = '',F9.4)') EAST_C
   WRITE (IU06,'('' LATITUDE INCREMENT IS        DX_C = '',F9.4)') DX_C
   WRITE (IU06,'('' NUMBER OF LATITUDES IS       NY_C = '',I5)') NY_C
   WRITE (IU06,'('' MOST SOUTHERN LATITUDE IS SOUTH_C = '',F9.4)') SOUTH_C
   WRITE (IU06,'('' MOST NORTHERN LATITUDE IS NORTH_C = '',F9.4)') NORTH_C
   WRITE (IU06,'('' LATITUDE INCREMENT IS        DY_C = '',F9.4)') DY_C
   IF (PER_C) THEN
      WRITE (IU06,*) 'THE GRID IS EAST-WEST PERIODIC'
   ELSE
      WRITE (IU06,*) 'THE GRID IS NOT EAST-WEST PERIODIC'
   END IF
ELSE
   WRITE (IU06,*) ' CURRENT INPUT GRID IS NOT DEFINED'
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     9. MODEL CURRENT FIELD.                                                  !
!        --------------------                                                  !

IF (ALLOCATED(U) .AND. ALLOCATED(V)) THEN
   WRITE (IU06,*) ' '
   TITL = 'CURRENT SPEED IN 0.01 METRES/SECOND '
   CALL PRINT_ARRAY (IU06, ZERO, TITL,                                         &
&                    UNPACK(SQRT(U**2+V**2), L_S_MASK, 99999.),                &
&                    AMOWEP, AMOSOP, AMOEAP, AMONOP, 100.)

   WRITE (IU06,*) ' '
   TITL = 'CURRENT DIRECTIOM IN DEGREES (CLOCKWISE FROM NORTH)'
   CALL PRINT_ARRAY (IU06, ZERO, TITL,                                         &
&         UNPACK(MOD(ATAN2(U,V+0.1E-10)*DEG+360.,360.), L_S_MASK, 99999.),     &
&         AMOWEP, AMOSOP, AMOEAP, AMONOP)
ELSE
   WRITE (IU06,*) ' MODEL CURRENT IS NOT DEFINED'
END IF

END SUBROUTINE PRINT_CURRENT_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_CURRENT_MODULE 
