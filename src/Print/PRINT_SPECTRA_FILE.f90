PROGRAM PRINT_SPECTRA_FILE

! ------------------------------------------------------------------           !
!                                                                              !
!**** *PRINT_SPECTRA_FILE* - PRINTS SPECTRA FROM WAM-MODEL OUTPUT.             !
!                                                                              !
!     H. GUNTHER    GKSS/ECMWF          DECEMBER 1989                          !
!                                                                              !
!                                                                              !
!*    PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        POSTPROCESSING OF WAM MODEL SPECTRA OUTPUT.                           !
!                                                                              !
!**   INTERFACE.                                                               !
!     ----------                                                               !
!                                                                              !
!       *PROGRAM* *PRINT_SPECTRA_FILE*                                              !
!          *IU01*    INPUT UNIT OF SPECTRA.                                    !
!          *IU05*    USER INPUT.                                               !
!          *IU06*    PRINTER OUTPUT.                                           !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
!                                                                              !
!        ABORT1             - TERMINATES PROCESSING.                           !
!        READ_SPECTRUM      - READS WAVE MODEL SPECTRA OUTPUT FILE.            !
!        GFILE              - OPENS A FILE.                                    !
!        PRINT_SPECTRUM     - PRINTS A SPECTRUM.                               !
!        READ_SPECTRA_USER  - READS IN USER INPUT.                             !
!                                                                              !
!     MODULES.                                                                 !
!     ----------                                                               !
!                                                                              !
!       WAM_PRINT_MODULE                                                       !
!       WAM_OUTPUT_MODULE                                                      !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THIS PROGRAM TAKES THE WAM MODEL OUTPUTS AS INPUT AND EXTRACTS         !
!       SPECTRA AT SPECIFIED LOCATIONS AND TIMES.                              !
!       THE FILES ARE DYNAMICALLY ASSIGNED BY GFILE.                           !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE, ONLY:  &
&       INCDATE,               &  !! UPDATES A DATE/TIME GROUP.
&       PRINT_SPECTRUM,        &  !! PRINT A SPECTRUM.
&       OPEN_FILE                 !! OPEN A FILE.

! ---------------------------------------------------------------------------- !
! 
!*    INTERFACE VARIABLE

USE WAM_PRINT_MODULE, ONLY: IU01, FILE01, IU05, FILE05, IU06, FILE06, ITEST,   &
&                           CDATEA, CDATEE, IDELDO,                            &
&                           FILE01, CDTFILE, IDFILE,                           &
&                           KL, ML, CO, FR, THETA,                             &
&                           SPEC_LAT, SPEC_LON, SPEC_DATE,                     & 
&                           SPEC, U10, UDIR, US, CD,                           &
&                           HS, PPER, MPER, TM1, TM2, MDIR, SPRE, TAUW,        &
&                           SPEC_SEA,                                          &
&                           HS_SEA, PPER_SEA, MPER_SEA, TM1_SEA, TM2_SEA,      &
&                           MDIR_SEA, SPRE_SEA,                                &
&                           SPEC_SWELL,                                        &
&                           HS_SWELL, PPER_SWELL, MPER_SWELL, TM1_SWELL,       &
&                           TM2_SWELL, MDIR_SWELL, SPRE_SWELL

USE WAM_OUTPUT_SET_UP_MODULE,ONLY: TITL,  PFLAG, NOUTT, COUTT,                 &
&                                  NOUTP, OUTLONG, OUTLAT, NAME, CFLAG

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLE

INTEGER     :: IFAIL           !! OPEN ERROR 
LOGICAL     :: IEOF            !! END OF FILE ENCOUNTED IN SUB. READ_SPECTRUM

CHARACTER*14 :: IHH
INTEGER      :: I
CHARACTER*40 :: HEADER

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    1. INITALISATION.                                                        !
!        --------------                                                        !
!                                                                              !
!*    1.1 OPEN FILES.                                                          !
!         -----------                                                          !

FILE05 = 'Spectra_User'
FILE06 = 'Spectra_Prot'
OPEN (UNIT=IU05, FILE=FILE05, FORM='FORMATTED', STATUS="OLD")
OPEN (UNIT=IU06, FILE=FILE06, FORM='FORMATTED', STATUS="UNKNOWN")

!*    1.2 READ USER INPUT.                                                     !
!         ----------------                                                     !

CALL READ_SPECTRA_USER

!*    1.3 FIRST WAVEMODEL OUTPUT FILE DATE AND PRINT DATE.                     !
!         ------------------------------------------------                     !

IF (NOUTT.GT.0) THEN
   CDATEE = '  '                                                             
   CDATEA = COUTT(1)                                                      
   DO I = 1,NOUTT                                                      
      IF (COUTT(I).LT.CDATEA) CDATEA = COUTT(I)                                       
      IF (COUTT(I).GT.CDATEE) CDATEE = COUTT(I)                                       
   END DO                                                              
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    2. LOOP OVER OUTPUT FILES.                                               !
!        -----------------------                                               !

FILES: DO

!*    2.1 FETCH FILE.                                                          !
!         -----------                                                          !

   CALL OPEN_FILE (IU06, IU01, FILE01, CDTFILE, 'OLD', IFAIL)
   IF (IFAIL.NE.0) STOP

!*    2.2  LOOP OVER OUTPUT TIMES.                                             !
!          -----------------------                                             !

   TIMES: DO

!*    2.2.1 READ IN ONE OUTPUT TIME.                                           !
!           ------------------------                                           !

      CALL READ_SPECTRUM (IU06, IU01, IEOF)

!*    2.2.2 END OF FILE ENCOUNTED?                                             !
!           ----------------------                                             !

      IF(IEOF) EXIT TIMES   !! END OF FILE ENCOUNTED

!*    2.2.3 OUTPUT TIME FOUND?                                                 !
!           ------------------                                                 !

      IF (SPEC_DATE.LT.CDATEA) CYCLE TIMES
      DO WHILE (SPEC_DATE.GT.CDATEA)
         CALL NEXT_OUTPUT_TIME
         IF (CDATEA.GT.CDATEE) EXIT FILES
         IF (SPEC_DATE.LT.CDATEA) CYCLE TIMES
      END DO

!*    2.2.4 OUTPUT LOCATION?                                                   !
!           ----------------                                                   !

      LOCATION: DO I = 1,NOUTP
         IF (ABS(MOD(OUTLONG(I)-SPEC_LON+720.,360.)).LT.0.00001 .AND.          &
&           ABS(OUTLAT(I)-SPEC_LAT).LT.0.00001) THEN
            IF (PFLAG(31) .AND. CFLAG(31)) THEN
               HEADER = TITL(31)(1:20)//NAME(I)
               CALL PRINT_SPECTRUM (IU06, SPEC_DATE, SPEC_LON, SPEC_LAT,       &
&                  HEADER, FR, THETA, SPEC, U10, UDIR, US,                     &
&                  HS, PPER, MPER, TM1, TM2, MDIR, SPRE)
            END IF
            IF (PFLAG(32) .AND. CFLAG(32)) THEN
               HEADER = TITL(32)(1:20)//NAME(I)
               CALL PRINT_SPECTRUM (IU06, SPEC_DATE, SPEC_LON, SPEC_LAT,       &
&                  HEADER, FR, THETA, SPEC_SEA, U10, UDIR, US, HS_SEA,         &
&                  PPER_SEA, MPER_SEA, TM1_SEA, TM2_SEA, MDIR_SEA, SPRE_SEA)
            END IF
            IF (PFLAG(33) .AND. CFLAG(33)) THEN
               HEADER = TITL(33)(1:20)//NAME(I)
               CALL PRINT_SPECTRUM (IU06, SPEC_DATE, SPEC_LON, SPEC_LAT,       &
&                  HEADER, FR, THETA, SPEC_SWELL, U10, UDIR, US,               &
&                  HS_SWELL, PPER_SWELL, MPER_SWELL, TM1_SWELL, TM2_SWELL,     &
&                  MDIR_SWELL, SPRE_SWELL)
            END IF
            CYCLE TIMES
         END IF
       END DO LOCATION

   END DO TIMES

   CALL INCDATE(CDTFILE,IDFILE)      !! INCREMENT DATE FOR THE NEXT FILE.
   CLOSE (UNIT=IU01, STATUS='KEEP')  !! CLOSE OLD FILE
END DO FILES

STOP

! ---------------------------------------------------------------------------- !

CONTAINS 

SUBROUTINE NEXT_OUTPUT_TIME
   IF (NOUTT.EQ.0) THEN
      CALL INCDATE(CDATEA,IDELDO)
   ELSE
      IHH = '99999999999999'
      DO I=1,NOUTT
         IF (COUTT(I).GT.CDATEA .AND. COUTT(I).LT.IHH) IHH = COUTT(I)
      END DO
      CDATEA = IHH
   END IF
END  SUBROUTINE NEXT_OUTPUT_TIME

END PROGRAM PRINT_SPECTRA_FILE
