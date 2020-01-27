MODULE WAM_RESTART_MODULE

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
&       OPEN_FILE                  !! OPENS A FILE.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_MODEL_MODULE,   ONLY: FL3, U10, UDIR, TAUW

USE WAM_TIMOPT_MODULE,  ONLY: CDATEA, CDTPRO, CDTSOU, CDA

USE WAM_FILE_MODULE,    ONLY: IU06, IU17, FILE17

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

INTERFACE CONNECT_RESTART           !! CONNECT RESTART FILE TO THE WAM MODEL.
   MODULE PROCEDURE CONNECT_RESTART
END INTERFACE
PUBLIC CONNECT_RESTART

INTERFACE SAVE_RESTART              !! SAVE RESTART FILE FOR THE WAM MODEL.
   MODULE PROCEDURE SAVE_RESTART
END INTERFACE
PUBLIC SAVE_RESTART

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE CONNECT_RESTART

! ---------------------------------------------------------------------------- !
!                                                                              !
!   CONNECT_RESTART - CONNECT RESTART FILE TO THE WAM MODEL.                   !
!                                                                              !
!       H. GUNTHER   GKSS    JULY 2001                                         !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO CONNECT THE RESTART FILES TO THE WAM MODEL.                         !
!                                                                              !
!     METHOD.                                                                  !
!     --------                                                                 !
!                                                                              !
!       THE RESTART FILES FOR THE WAM MODEL ARE OPEND AND READ.                !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER       :: IFAIL
CHARACTER*14  :: ZERO = ' '

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. OPEN RESTART FILE.                                                    !
!        ------------------                                                    !

CALL OPEN_FILE (IU06, IU17, FILE17, CDATEA, 'OLD', IFAIL)
IF (IFAIL.NE.0) CALL ABORT1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. READ RESTART.                                                         !
!        -------------                                                         !

READ (IU17) CDTPRO, CDTSOU, CDA
READ (IU17) U10
READ (IU17) UDIR
READ (IU17) TAUW
READ (IU17) FL3

CLOSE (UNIT=IU17, STATUS="KEEP")

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. CHECK RESTART TIME.                                                   !
!        -------------------                                                   !

IF (CDTPRO.NE.CDATEA) THEN
   WRITE(IU06,*) ' ************************************************'
   WRITE(IU06,*) ' *                                              *'
   WRITE(IU06,*) ' *      FATAL ERROR IN SUB. CONNECT_RESTART     *'
   WRITE(IU06,*) ' *      ===================================     *'
   WRITE(IU06,*) ' *                                              *'
   WRITE(IU06,*) ' * START DATE AND RESTART FIELD DO NOT MATCH    *'
   WRITE(IU06,*) ' * START DATE OF RUN       IS CDATEA = ', CDATEA
   WRITE(IU06,*) ' * START DATE FROM RESTART IS CDTPRO = ', CDTPRO
   WRITE(IU06,*) ' *                                              *'
   WRITE(IU06,*) ' *      PROGRAM ABORTS     PROGRAM ABORTS       *'
   WRITE(IU06,*) ' *                                              *'
   WRITE(IU06,*) ' ************************************************'
   CALL ABORT1
END IF

END SUBROUTINE CONNECT_RESTART

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SAVE_RESTART (IU, FILE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SAVE_RESTART  - TO SAVE RESTART FILE FOR THE WAM MODEL.                    !
!                                                                              !
!                                                                              !
!     H. GUENTHER   GKSS  FEBRUARY 2002       FT 90                            !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        COPY ALL DATA FIELDS AND VARIABLES NESSECCARY FOR A RESTART OF THE    !
!        WAM MODEL TO FILE.                                                    !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!        A FILE IS OPENED AND THE FOLLOWING ACTUAL INFORMATION                 !
!        IS COPIED OUT OF THE MODEL RUN AT THE TIME OF THE RESTART:            !
!          - THE WIND INFORMATION TO GATHER WITH THE TIME COUNTERS,            !
!          - THE SPECTRA AT ALL GRID POINTS,                                   !
!        THE FILE NAME CONVENTION IS DEFINED IN SUB. GFILE.                    !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!        NONE                                                                  !
!                                                                              !

! ---------------------------------------------------------------------------- !
!
!*    INTERFACE VARIABLES.
!     --------------------

INTEGER,           INTENT(IN) :: IU    !! UNIT FOR RESTART FILE
CHARACTER (LEN=*), INTENT(IN) :: FILE  !! RESTART FILE IDENTIFIER

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE                                                           !

INTEGER :: IFAIL = 0

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. OPEN FILE AND WRITE OUT.                                              !
!        ------------------------                                              !

CALL OPEN_FILE (IU06, IU, FILE, CDTPRO, 'UNKNOWN', IFAIL)
IF (IFAIL.NE.0) CALL ABORT1

REWIND IU
WRITE(IU) CDTPRO, CDTSOU, CDA
WRITE(IU) U10
WRITE(IU) UDIR
WRITE(IU) TAUW
WRITE(IU) FL3

CLOSE (UNIT=IU, STATUS="KEEP")

WRITE(IU06,*) ' '
WRITE(IU06,*) ' SUB. SAVE_RESTART: RESTART FILE SAVED'
WRITE(IU06,*) ' '
WRITE(IU06,*) ' PROPAGATION DATE IS .............. CDTPRO  = ', CDTPRO
WRITE(IU06,*) ' SOURCE FUNCTION DATE IS .......... CDTSOU  = ', CDTSOU
WRITE(IU06,*) ' WIND FIELD DATE IS .................. CDA  = ', CDA

END SUBROUTINE SAVE_RESTART

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_RESTART_MODULE
