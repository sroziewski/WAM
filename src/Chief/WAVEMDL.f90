SUBROUTINE WAVEMDL

! ---------------------------------------------------------------------------- !
!                                                                              !
!****  *WAVEMDL* - SUPERVISES EXECUTION OF MAIN MODULES  OF THE WAVE MODEL     !
!****                                                                          !
!                                                                              !
!      LIANA ZAMBRESKY    GKSS/ECMWF    OCTOBER 1988                           !
!                                                                              !
!      MODIFIED BY H. GUNTHER   ECMWF   MARCH 1990                             !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        THIS SUBROUTINE SUPERVISES THE EXECUTION OF  MAIN MODULES FOR         !
!        WAM MODEL INITIALIZATION, WIND FIELD PREPROCESSING, AND               !
!        WAM MODEL EXECUTION.                                                  !
!                                                                              !
!*    INTERFACE.                                                               !
!     ----------                                                               !
!                                                                              !
!          SEE MAIN MODULES SUB INITMDL, PREPARE_WIND, WAMODEL.                !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE FIRST TIME WAVEMDL IS CALLED, THE WAM MODEL IS INITIALIZED.        !
!       THIS INITIALIZATION INCLUDES GETTING THE INITIAL SEA STATE FILES,      !
!       FILLING MODULES, DEFINING THE GRID AND SETTING GENERAL PARAMETERS.     !
!                                                                              !
!       IN THE FIRST AND ALL SUBSEQUENT CALLS TO WAVEMDL PREPARE_WIND FORMATS  !
!       THE WINDS INTO THE WAM MODEL STRUCTURE AND THE WAM MODEL IS EXECUTED.  !
!       EACH CALL TO WAMODEL INTEGRATES THE WAVE SPECTRA FORWARD IN TIME BY    !
!       ONE INPUT WIND TIME STEP OR PROPAGATION OR SOURCE FUNCTION TIME STEP,  !
!       WHAT EVER IS LONGER.                                                   !
!                                                                              !
!     REFERENCES.                                                              !
!     -----------                                                              !
!                                                                              !
!          NONE                                                                !
!                                                                              !
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE,   ONLY:  &
&       INITMDL,                 &  !! INITIALIZES THE WAM MODEL.
&       WAMODEL                     !! INTEGRATES THE WAVE SPECTRA.

USE WAM_WIND_MODULE,       ONLY: &
&       PREPARE_WIND                !! PREPARES WIND DATA FOR WAVE MODEL.

! ---------------------------------------------------------------------------- !
!
!*    MODULE VARIABLES.
!     -----------------

USE WAM_FILE_MODULE,         ONLY: IU06, ITEST
USE WAM_TIMOPT_MODULE,       ONLY: CDTPRO

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLES.
!     ----------------

LOGICAL, SAVE :: FRSTIME = .TRUE.   !! FIRST CALL OF WAVEMDL FLAG. 

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    1.  THE FIRST CALL TO WAVEMDL PERFORMS INITIALIZATION.                   !
!         --------------------------------------------------                   !

IF (FRSTIME) THEN   
   CALL INITMDL
   FRSTIME = .FALSE.   
   IF (ITEST.GE.1) WRITE(IU06,*) ' SUB. WAVEMDL: INITMDL DONE'
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    2.0  INTEGRATE THE WAVE SPECTRA FORWARD IN TIME.                         !
!          -------------------------------------------                         !

!*    2.1  REFORMAT WINDS FROM GAUSSIAN TO BLOCKED.                            !
!          ----------------------------------------                            !

CALL PREPARE_WIND
IF (ITEST.GE.1) WRITE(IU06,*) ' SUB. WAVEMDL: PREPARE_WIND DONE'

!*    2.2  INTEGRATE THE WAVE SPECTRA FORWARD IN TIME.                         !
!          -------------------------------------------                         !

CALL WAMODEL
IF (ITEST.GE.1)  WRITE(IU06,*) ' SUB. WAVEMDL: WAMODEL DONE'

END SUBROUTINE WAVEMDL
