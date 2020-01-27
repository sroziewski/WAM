MODULE WAM_INITIAL_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL DATA WHICH ARE USED BY THE PRESET PROGRAM.        !
!   ALL PROCEDURES ARE INCLUDED TO COMPUTE THE WAM MODEL COLDSTART FILE,       !
!   TO COMPUTE SAVE AND CONNECT RESTART FILES TO THE WAM MODEL.                !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  & !! TERMINATES PROCESSING.
&       INCDATE                    !! UPDATES DATE/TIME GROUP.

USE WAM_RESTART_MODULE,  ONLY: &
&       CONNECT_RESTART           !! CONNECT RESTART FILE TO THE WAM MODEL.

USE WAM_COLDSTART_MODULE,  ONLY: &
&       PREPARE_COLDSTART         !! PREPARES COLDSTART FIELDS FOR WAMODEL.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE, ONLY: DEG

USE WAM_FRE_DIR_MODULE, ONLY: KL, ML

USE WAM_GRID_MODULE,    ONLY: NSEA 

USE WAM_MODEL_MODULE,   ONLY: FL3, U10, UDIR, TAUW, USTAR, Z0

USE WAM_TIMOPT_MODULE,  ONLY: CDTPRO, IDELT, CDATEWO, IOPTI

USE WAM_FILE_MODULE,    ONLY: IU06, IDELRES, CDTRES

USE WAM_WIND_MODULE,    ONLY: IDELWO

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE
PRIVATE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE PREPARE_START             !! PREPARES START FIELDS FOR WAMODEL.
   MODULE PROCEDURE PREPARE_START
END INTERFACE
PUBLIC PREPARE_START

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

SUBROUTINE PREPARE_START

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. ALLOCATE ARRAYS.                                                      !
!        ----------------                                                      !

IF (ALLOCATED(U10   )) DEALLOCATE(U10  )
IF (ALLOCATED(UDIR  )) DEALLOCATE(UDIR )
IF (ALLOCATED(USTAR )) DEALLOCATE(USTAR)
IF (ALLOCATED(Z0    )) DEALLOCATE(Z0   )
IF (ALLOCATED(TAUW  )) DEALLOCATE(TAUW )
IF (ALLOCATED(FL3   )) DEALLOCATE(FL3  )
ALLOCATE(U10  (1:NSEA))
ALLOCATE(UDIR (1:NSEA))
ALLOCATE(USTAR(1:NSEA))
ALLOCATE(Z0   (1:NSEA))
ALLOCATE(TAUW (1:NSEA))
ALLOCATE(FL3  (1:NSEA, 1:KL,1:ML))
USTAR = 0.
TAUW = 0.
Z0 = 0.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. GENERATE START FIELDS OR READ RESTART FILE.                           !
!        -------------------------------------------                           !

IF (IOPTI.GE.0) THEN
   CALL PREPARE_COLDSTART
ELSE
   CALL CONNECT_RESTART
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. INITIALIZE DATE FOR NEXT WIND FIELD.                                  !
!        ------------------------------------                                  !

CDATEWO = CDTPRO
IF (IDELT.LT.IDELWO) CALL INCDATE(CDATEWO,IDELWO/2)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. INITIALIZE FILE DISPOSE TIME AND RESTART TIME.                        !
!         ---------------------------------------------                        !

CDTRES = CDTPRO
CALL INCDATE(CDTRES, IDELRES)

END SUBROUTINE PREPARE_START

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_INITIAL_MODULE
