SUBROUTINE INITMDL

! ---------------------------------------------------------------------------- !
!
!**** *INITMDL* - INITIALIZES THE WAM MODEL.
!
!     L. ZAMBRESKY   GKSS/ECMWF    JULY 1988
!
!     MODIFIED BY:   H. GUNTHER    NOVEMBER 1989
!     H. GUNTHER      GKSS         OCTOBER 2000  FT90
!
!*    PURPOSE.
!     --------
!
!       INITIALIZE THE WAM MODEL.
!
!     INTERFACE.
!     ----------
!
!          ---- INPUT/OUTPUT UNITS ---
!
!      THE PROGRAM OPENS AUTOMATICALLY THE FOLLOWING FILES, WHICH ARE 
!      DEFINED IN "WAM_FILE_MODULE.f90":
!
!       UNIT = IU05 = 5  FILE05 = 'WAM_User' TO READ USER INPUT FILE.
!       UNIT = IU06 = 6  FILE06 = 'WAM_Prot' TO WRITE A PROTOCOL.
!
!      ALL OTHER FILE NAMES AND THE UNITS ARE PRE-DEFINED IN WAM_FILE_MODULE
!      TOO.  
!      THE NAMES OF INPUT AND OUTPUT FILES CAN BE CHANGED IN THE WAM USER 
!      INPUT FILE.
!
!      THE PROGRAM USES OPEN TO ASSIGN FILES. 
!      MODEL OUTPUT FILES ARE EXTENTED BY A DATE/TIME.
!      FOR DETAILS OF THE FILE NAME CONVENTION OF THESE FILES SEE SUB. GSFILE.
!
!     METHOD.
!     -------
!
!          THIS ROUTINE INITIALISES THE WAVEMODEL:
!            -  READS THE USER INPUT FILE,
!            -  READS THE DATA PRECOMPUTED BY PROG. PREPROC,
!            -  READS  THE RECOVERY FILES,
!            -  DOES SOME GENERAL BOOKEEPING REGARDING
!               DATES, INTEGRATION TIME STEPS AND OUTPUT TIME STEPS.
!            -  READS COMMON UBUF AND SPECTRA IF ONE BLOCK VERSION.
!            -  PREPARES ICE DATA.
!            -  PREPARES PROPAGATION.
!            -  PERFORMS A CFL CHECK.
!            -  OPENS THE FIRST RESULT FILES.
!
!     REFERENCE
!     ---------
!
!          A MORE DETAILED  DISCUSSION MAY BE FOUND IN SUB WAMODEL.
!
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE,    ONLY:  &
&       ABORT1,                   &  !! TERMINATES PROCESSING.
&       INCDATE,                  &  !! UPDATE DATE TIME GROUP.
&       READ_ICE_INPUT,           &  !! READS ICE DATA.
&       READ_CURRENT,             &  !! READ CURRENT INPUT FILE.
&       READ_WAM_USER                !! READS USER INPUT.

USE WAM_PROPAGATION_MODULE, ONLY: &
&       PREPARE_PROPAGATION         !! PREPARES PROPAGATION, DOES CFL CHECK.

USE WAM_SOURCE_MODULE,      ONLY: &
&       PREPARE_SOURCE              !! PREPARES SOURCE FUNCTIONS.

USE WAM_OUTPUT_SET_UP_MODULE,ONLY: &
&       PREPARE_OUTPUT              !! PREPARES OUTPUT.

USE PREPROC_MODULE,         ONLY: &
&       READ_PREPROC_FILE           !! READS PREPROC OUTPUT FILE.

USE WAM_INITIAL_MODULE,     ONLY: &
&       PREPARE_START               !! PREPARES START FIELDS.

USE WAM_CURRENT_MODULE,     ONLY: &
&       PREPARE_CURRENT             !! PREPARES CURRENT FIELD.      

USE WAM_BOUNDARY_MODULE,    ONLY: &
&       SAVE_BOUNDARY_FILE 

! ---------------------------------------------------------------------------- !
!
!*    MODULE VARIABLES.
!     -----------------

USE WAM_FILE_MODULE,  ONLY: IU06, FILE06, ITEST, FILE09,                       &
&                           IU20, FILE20, IU25, FILE25
USE WAM_NEST_MODULE,  ONLY : COARSE

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLES.
!     ----------------

IMPLICIT NONE

LOGICAL       :: OPND

! ---------------------------------------------------------------------------- !
!
!*    1. INPUT OF USER PARAMETER.
!        ------------------------

INQUIRE (UNIT=IU06, OPENED=OPND)
IF (.NOT.OPND) THEN
   OPEN (UNIT=IU06, FILE=FILE06, FORM="FORMATTED", STATUS="UNKNOWN")
END IF

CALL READ_WAM_USER
IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. INITMDL: READ_INPUT_CHIEF DONE'

! ---------------------------------------------------------------------------- !
!
!*    2. READ PREPROC OUTPUT.
!        ---------------------

CALL READ_PREPROC_FILE
IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. INITMDL: READ_PREPROC_FILE DONE'

! ---------------------------------------------------------------------------- !
!
!*    3. ICE INFORMATION.
!        ----------------

CALL READ_ICE_INPUT
IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. INITMDL: READ_ICE_INPUT DONE'

! ---------------------------------------------------------------------------- !
!
!*    4. CURRENT INFORMATION.
!        --------------------

IF (LEN_TRIM(FILE09).GT.0) THEN
   CALL READ_CURRENT
   IF (ITEST.GT.0) WRITE (IU06,*) '    SUB. INITMDL: READ_CURRENT DONE'
END IF

! ---------------------------------------------------------------------------- !
!
!*    5. PREPARE START SPECTRA AND WINDS.
!        --------------------------------

CALL PREPARE_START
IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. INITMDL: PREPARE_START DONE'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. PREPARE CURRENT FIELD.                                                !
!        ----------------------                                               !

IF (LEN_TRIM(FILE09).GT.0) THEN
   CALL PREPARE_CURRENT
   IF (ITEST.GT.1) WRITE (IU06,*) '    SUB. INITMDL: PREPARE_CURRENT DONE'
END IF

! ---------------------------------------------------------------------------- !
!
!*    7. PREPARE PROPAGATION AND PERFORM CFL CHECK.
!         ------------------------------------------

CALL PREPARE_PROPAGATION
IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. INITMDL: PREPARE_PROPAGATION DONE'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. PREPARE SOURCE FUNCTIONS.                                             !
!        -------------------------                                             !

CALL PREPARE_SOURCE
IF (ITEST.GT.1) WRITE (IU06,*) '    SUB. INITMDL: PREPARE_SOURCE DONE'

! ---------------------------------------------------------------------------- !
!
!*    9. PREPARE OUTPUT FOR INTEGRATED PARAMETER AND/OR SPECTRA.
!        -------------------------------------------------------

CALL PREPARE_OUTPUT (IU20, FILE20, IU25, FILE25)
IF (ITEST.GE.2)  WRITE(IU06,*) '    SUB. INITMDL: PREPARE_OUTPUT DONE'

! ---------------------------------------------------------------------------- !
!
!*   10. PREPARE BOUNDARY VALUE HANDLING.
!        --------------------------------

IF (COARSE) CALL SAVE_BOUNDARY_FILE

! ---------------------------------------------------------------------------- !
!
!*   11. PRINT MODULE STATUS.
!        -------------------- 

WRITE (IU06,*) ' '
WRITE (IU06,*) '        MODULE STATUS AFTER INITIALISATION'
WRITE (IU06,*) '        ----------------------------------'
WRITE (IU06,*) ' '

CALL PRINT_WAM_STATUS
 
WRITE(IU06,*) '  '
WRITE(IU06,*) '         END OF WAM MODEL INITIALISATION'
WRITE(IU06,*) '         -------------------------------'
WRITE(IU06,*) '  '

END SUBROUTINE INITMDL
