MODULE WAM_ICE_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE STORES THE ICE INPUT GRID SPECFICATIONS, AND THE BLOCKED       !
!   MODEL ICE INFORMATION.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE, ONLY:  &
&       ABORT1,                &   !! TERMINATES PROCESSING.
&       PRINT_ARRAY                !! PRINTS AN ARRAY.

USE WAM_FILE_MODULE,  ONLY: IU06, ITEST
USE WAM_GRID_MODULE,  ONLY: NX, NY, NSEA, L_S_MASK,                            &
&                           AMOWEP, AMOSOP, AMOEAP, AMONOP

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. ICE POINTS.                                                           !
!        -----------                                                           !

LOGICAL              :: ICE_RUN = .FALSE. !! TRUE IF ICE IS TAKEN INTO ACCOUNT
INTEGER              :: N_ICE   = 0       !! NUMBER OF ICE POINTS.
INTEGER, ALLOCATABLE :: IJ_ICE(:)         !! INDEX OF ICE POINTS.
LOGICAL, ALLOCATABLE :: ICE_MAP(:,:)      !! ICE MAP.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

INTERFACE SET_ICE
   MODULE PROCEDURE SET_ICE_I
   MODULE PROCEDURE SET_ICE_R
   MODULE PROCEDURE SET_ICE_L
END INTERFACE

INTERFACE PRINT_ICE_STATUS
   MODULE PROCEDURE PRINT_ICE_STATUS
END INTERFACE

INTERFACE PUT_ICE
   MODULE PROCEDURE PUT_ICE_SPEC
   MODULE PROCEDURE PUT_ICE_PAR
END INTERFACE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ICE_I (I_GRID)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SET_ICE - TRANSFERS ICE DATA TO ICE MODULE.  (INTEGER VERSION)             !
!                                                                              !
!     HEINZ GUNTHER    GKSS    JANUARY 2002                                    !
!                                                                              !
!     PURPOSE                                                                  !
!     -------                                                                  !
!                                                                              !
!       READ AN ICE MAP AND BLOCK THE INFORMATION.                             !
!                                                                              !
!     METHOD                                                                   !
!     ------                                                                   !
!                                                                              !
!        NONE.                                                                 !
!                                                                              !
!     REFERENCES                                                               !
!     ----------                                                               !
!                                                                              !
!          NONE                                                                !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER, INTENT(IN) :: I_GRID(:,:)  !! ICE MAP

INTEGER :: IJ, N
INTEGER :: BLOCK(1:NSEA)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CHECKDIMENSIONS OF ICE INFORMATION.                                   !
!        -----------------------------------                                   !

IF (SIZE(I_GRID,1).NE.NX .OR. SIZE(I_GRID,2).NE.NY) THEN
   WRITE(IU06,*) ' *******************************************'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *   FATAL ERROR IN SUB. SET_ICE_INPUT     *'
   WRITE(IU06,*) ' *   =================================     *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * ICE GRID IS NOT EQUAL TO MODEL GRID     *'
   WRITE(IU06,*) ' *   NX_ICE = ', SIZE(I_GRID,1), '   NX  = ', NX
   WRITE(IU06,*) ' *   NY_ICE = ', SIZE(I_GRID,2), '   NY  = ', NY
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * PROGRAM ABORTS     PROGRAM ABORTS       *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *******************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. STORE ICE MAP.                                                        !
!     -----------------                                                        !

IF (ALLOCATED(ICE_MAP)) DEALLOCATE(ICE_MAP)
ALLOCATE (ICE_MAP(1:NX,1:NY))
ICE_MAP = I_GRID.EQ.1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. FIND POINT NUMBERS OF ICE POINTS.                                     !
!     ------------------------------------                                     !

BLOCK = PACK(I_GRID, L_S_MASK)
N_ICE = COUNT(BLOCK.EQ.1)
IF (ALLOCATED(IJ_ICE)) DEALLOCATE(IJ_ICE)

IF (N_ICE.EQ.0) THEN
   ICE_RUN = .FALSE.
ELSE
   ICE_RUN = .TRUE.
   ALLOCATE (IJ_ICE(1:N_ICE))
   N = 0
   DO IJ = 1,NSEA
      IF (BLOCK(IJ) .EQ.1) THEN
         N = N+1
         IJ_ICE(N) = IJ
      END IF
   END DO
END IF

END SUBROUTINE SET_ICE_I

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ICE_R (R_GRID)  !! (REAL VERSION OF SUB. SET_ICE)

REAL, INTENT(IN) :: R_GRID(:,:)  !! ICE MAP
CALL SET_ICE_I (NINT(R_GRID))

END SUBROUTINE SET_ICE_R

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ICE_L (L_GRID)  !! (LOGICAL VERSION OF SUB. SET_ICE)

LOGICAL, INTENT(IN) :: L_GRID(:,:)    !! ICE MAP
INTEGER :: I_GRID(SIZE(L_GRID,1),SIZE(L_GRID,2))
WHERE (L_GRID)
   I_GRID = 1
ELSEWHERE
   I_GRID = 0
END WHERE
CALL SET_ICE_I (I_GRID)

END SUBROUTINE SET_ICE_L

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_ICE_STATUS

CHARACTER (LEN=100) :: TITL
CHARACTER (LEN=1)   :: LST(NX,NY) !! LAND SEA TABLE  L = LAND, S = SEA, I = ICE.
CHARACTER (LEN=14)  :: ZERO = ' '
CHARACTER (LEN=1)   :: PMSK(1:NSEA)

WRITE (IU06,*) '  '
WRITE (IU06,*) ' ------------------------------------------------- '
WRITE (IU06,*) '              ICE MODULE STATUS:'
WRITE (IU06,*) ' ------------------------------------------------- '
WRITE (IU06,*) '  '
IF (ICE_RUN) THEN
   WRITE (IU06,*) ' ICE INITIALISED, NO. OF ICEPOINTS IS N_ICE = ', N_ICE
   IF (ITEST.GT.0 .AND. N_ICE.GT.0) THEN
      PMSK =  'S'                               !! SEA POINTS
      PMSK(IJ_ICE) = 'I'                        !! ICE POINTS
      LST = UNPACK(PMSK, L_S_MASK, 'L')         !! LAND POINTS
      WRITE (IU06,*) ' '
      TITL = ' LAND SEA MAP:  L = LAND, S = SEA,  I = ICE '
      CALL PRINT_ARRAY (IU06, ZERO, TITL, LST, AMOWEP, AMOSOP, AMOEAP, AMONOP)
   END IF
ELSE
   WRITE (IU06,*) ' ICE IS NOT INITIALIZED '
END IF
WRITE (IU06,*) '  '

END SUBROUTINE PRINT_ICE_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PUT_ICE_SPEC (FL3, INDICATOR)

REAL, INTENT(INOUT) :: FL3(:,:,:)   !! BLOCK OF SPECTRA.
REAL, INTENT(IN)    :: INDICATOR    !! VALUE TO BE INSERTED AT ICE POINTS.

FL3(IJ_ICE,:,:) = INDICATOR

END SUBROUTINE PUT_ICE_SPEC

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PUT_ICE_PAR (PAR, INDICATOR)

REAL, INTENT(INOUT) :: PAR(:)      !! BLOCK OF PARAMETER.
REAL, INTENT(IN)    :: INDICATOR   !! VALUE TO BE INSERTED AT ICE POINTS.

PAR(IJ_ICE) = INDICATOR

END SUBROUTINE PUT_ICE_PAR

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_ICE_MODULE
