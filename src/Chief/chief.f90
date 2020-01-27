PROGRAM CHIEF 

!-------------------------------------------------------------------------------!
!
!**** *CHIEF* - SUPERVISES WAVE MODEL EXECUTION.
!
!     LIANA ZAMBRESKY      GKSS/ECMWF  JUNE 1989
!     H. GUNTHER           ECMWF       JUNE 1990  MODIFIED FOR CYCLE_4.
!
!*    PURPOSE.
!     --------
!
!       THIS PROGRAM SUPERVISES THE EXECUTION OF THE WAM MODEL.
!
!**   INTERFACE.
!     ----------
!
!       IN ORDER FOR THE WAM MODEL TO EXECUTE, IT NEEDS
!       FILES FROM ESSENTIALLY FIVE SOURCES.
!
!       1. THE UNFORMATED FILES CREATED BY THE 1 PREPROC
!
!       2. USER INPUT FILE
!
!       3. THE WIND INPUT FILE.
!
!       4  THE BOUNDARY VALUE INPUT FILES CREATED BY 1 BOUINT.
!          THESE FILES ARE DYNAMICALLY ASSIGNED.
!
!       5. THE START FILES:
!          THE RESTART FILES HAVE TO BE CREATED BY 1
!          PRESET, IF A COLD START HAS TO BE DONE.
!          THESE FILES OR FILES FROM A PREVIOUS MODEL RUN
!          ARE AUTOMATICALLY ASSIGNED. (SEE SUB GSFILE).
!
!       EXPLANATIONS FOR ALL FILES ARE GIVEN IN DETAIL IN SUB INITMDL
!
!     LIBRARIES.
!     ----------
!
!         NONE.
!
!     METHOD.
!     -------
!
!       THIS VERSION OF THE WAM MODEL HAS BEEN PRODUCED
!       BY MERGING AND CORRECTLY INTERFACING WHAT USED
!       TO BE THE STAND ALONE PROGRAMS:
!               PREWIND AND THE WAM MODEL.
!       PREWIND REFORMATS WINDS INTO THE WAM MODEL BLOCKED
!       STRUCTURE.  STARTING WITH THE INITIAL SEA STATE
!       FILES, THE WAM MODEL CAN THEN INTEGRATE FORWARD
!       IN TIME, DRIVEN BY THE REFORMATTED WINDS.
!       THE SEA STATE AND RESULT FILES ARE SAVED IN REGULAR
!       INTERVALLS. THE SEA STATE FILE SERVE AS THE INITIAL
!       CONDITION FOR A RESTART.
!
!       EACH CALL OF THE SUB WAVEMDL INTEGRATES FORWARD IN
!       TIME BY ONE WIND INPUT TIMESTEP OR ONE PROPAGATION
!       TIMESTEP, WHAT EVER IS LONGER.
!       IN THE FIRST CALL TO WAVEMDL AN INITIALIZATION IS
!       DONE IN ADDITION.
!
!     REFERENCE.
!     ----------
!
!       EACH MODULE IS OF ITSELF THOROUGHLY DOCUMENTED.
!
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE,   ONLY:  &
&       WAVEMDL                     !! SUPERVISES THE OVERALL FLOW THROUGH
                                    !! THE MAIN MODULES: INITMDL, PREWIND
                                    !! AND WAMODEL.

! ---------------------------------------------------------------------------- !
!
!*    MODULE VARIABLES.
!     -----------------

USE WAM_TIMOPT_MODULE, ONLY: CDATEE, CDTPRO
USE WAM_FILE_MODULE,   ONLY: IU06, FILE06

IMPLICIT NONE 

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLES.
!     ----------------

INTEGER  :: TIME0, TIME, TCOUNT
LOGICAL  :: DA

! ---------------------------------------------------------------------------- !
!
      
CALL SYSTEM_CLOCK(TIME0, TCOUNT)
TIME0 = -TIME0

!  OPEN (UNIT=IU06, FILE=FILE06, FORM="FORMATTED", STATUS="UNKNOWN")

! ---------------------------------------------------------------------------- !
!
!*    2. CALLS TO WAVEMDL UNTIL MODEL DATE REACHES END DATE. EACH CALL 
!*       INTEGRATES ONE WIND INPUT TIMESTEP, OR ONE PROPAGATION TIMESTEP, 
!*        OR ONE SOURCE FUNCTION TIMESTEP WHAT EVER IS LONGER.
!        ----------------------------------------------------------------

CDTPRO = ' '
CDATEE = '99999999999900'

DO WHILE (CDTPRO.LT.CDATEE)
   CALL WAVEMDL
END DO

! ---------------------------------------------------------------------------- !
!
!*    4.  TERMINATE PROTOCOL.
!        --------------------

CALL SYSTEM_CLOCK(TIME, TCOUNT)
TIME = (TIME0 + TIME)/TCOUNT
WRITE (IU06,*) ' ++++++++++++++++++++++++++++++'
WRITE (IU06,*) ' + TOTAL USER TIME IN SECONDS +'
WRITE (IU06,*) ' +                            +'
WRITE (IU06,*) ' + ', TIME
WRITE (IU06,*) ' +                            +'
WRITE (IU06,*) ' ++++++++++++++++++++++++++++++'

STOP
END PROGRAM CHIEF
