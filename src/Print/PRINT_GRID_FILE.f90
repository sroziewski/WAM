PROGRAM PRINT_GRID_FILE

! ---------------------------------------------------------------------------- !
!
!**** *PRINT_GRID_FILE* - PRINTS MAPS FROM WAMODEL OUTPUT.
!
!     H. GUNTHER    ECMWF/GKSS     DECEMBER 1989
!
!     PURPOSE.
!     --------
!
!       POSTPROCESSING OF WAM MODEL INTEGRATED DATA.
!
!     INTERFACE.
!     ----------
!
!       *PROGRAM* *PRINT_GRID_FILE*
!          IU01     INPUT UNIT WAVE AND WIND FIELDS (WAMODEL IU20).
!          IU05     USER INPUT.
!          IU06     PRINTER OUTPUT.
!
!     EXTERNALS
!     ---------
!
!       *ABORT1*        - TERMINATES PROCESSING.
!       *INCDATE*       - INCREMENTS DATE-TIME-GROUP
!       *INGRID*        - READS WAVEMODEL OUTPUT FILE (MAP) (GRIDDED)
!       *GFILE*         - FETCH A FILE.
!       *PRINT_ARRAY*   - PRINTS AN ARRAY.
!       *READ_GRID_USER* - READS IN USER INPUT.
!
!     METHOD.
!     -------
!
!       THIS PROGRAM TAKES THE  WAM MODEL OUTPUTS AS INPUT AND
!       PRINTS FIELDS OF INTEGRATED DATA.
!
!     REFERENCE.
!     ----------
!
!        NONE.
!
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE, ONLY:  &
&       INCDATE,               &  !! UPDATES A DATE/TIME GROUP.
&       PRINT_ARRAY,           &  !! PRINT AN ARRAY.
&       OPEN_FILE                 !! OPEN A FILE.

! ---------------------------------------------------------------------------- !
!
!*    INTERFACE VARIABLE

USE WAM_PRINT_MODULE, ONLY: IU05, FILE05, IU06, FILE06, ITEST,                 &
&                           CDATEA, CDATEE, IDELDO,                            &
&                           IU01, FILE01, CDTFILE, IDFILE,                     &
&                           NX, NY, AMOWEP, AMOSOP, AMOEAP, AMONOP,            &
&                           XDELLA, XDELLO, U10_GR, UDIR_GR, US_GR, CD_GR,     &
&                           HS_GR, PPER_GR, MPER_GR, TM1_GR, TM2_GR, MDIR_GR,  &
&                           SPRE_GR, TAUW_GR,                                  &
&                           HS_SEA_GR, PPER_SEA_GR, MPER_SEA_GR, TM1_SEA_GR,   &
&                           TM2_SEA_GR, MDIR_SEA_GR, SPRE_SEA_GR,              &
&                           HS_SWELL_GR, PPER_SWELL_GR, MPER_SWELL_GR,         &
&                           TM1_SWELL_GR, TM2_SWELL_GR, MDIR_SWELL_GR,         &
&                           SPRE_SWELL_GR

USE WAM_OUTPUT_SET_UP_MODULE,ONLY: CDTINTT, NOUTT, COUTT, PFLAG, CFLAG,        &
&                                  TITL, SCAL

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLE

LOGICAL       :: IEOF
CHARACTER*14  :: IHH
INTEGER       ::  I, IFAIL
LOGICAL       :: FRSTIME = .TRUE.

! ---------------------------------------------------------------------------- !
!
!*    1. INITALISATION.
!        --------------

OPEN (UNIT=IU05, FILE=FILE05, FORM="FORMATTED", STATUS="OLD")
OPEN (UNIT=IU06, FILE=FILE06, FORM="FORMATTED", STATUS="UNKNOWN")

!*    1.2 READ USER INPUT.
!         ----------------

CALL READ_GRID_USER

!*    1.3 FIRST WAVEMODEL OUTPUT FILE DATE AND PRINT DATE.
!         ------------------------------------------------

IF (NOUTT.GT.0) THEN
   CDATEE = '  '                                                             
   CDATEA = COUTT(1)                                                      
   DO I = 1,NOUTT                                                      
      IF (COUTT(I).LT.CDATEA) CDATEA = COUTT(I)                                       
      IF (COUTT(I).GT.CDATEE) CDATEE = COUTT(I)                                       
   END DO                                                              
END IF
CDTINTT = '  '

! ---------------------------------------------------------------------------- !
!
!     2. LOOP OVER OUTPUT FILES.
!        -----------------------

FILES: DO

!     2.1 FETCH FILE.
!         -----------

   CALL OPEN_FILE (IU06, IU01, FILE01, CDTFILE, 'OLD', IFAIL)
   IF (IFAIL.NE.0) STOP

!     2.2  LOOP OVER OUTPUT TIMES.
!          -----------------------

   TIMES: DO

!     2.2.1 READ IN WIND AND WAVE FIELDS.
!           -----------------------------

      CALL INGRID (IU01, IEOF)

      IF (IEOF) EXIT TIMES     !! IF END OF FILE ENCOUNTED THEN EXIT TIME LOOP

!     2.2.2 OUTPUT TIME FOUND?
!           ------------------

      IF (CDTINTT.LT.CDATEA) CYCLE TIMES
      DO WHILE (CDTINTT.GT.CDATEA)
         CALL NEXT_OUTPUT_TIME
         IF (CDATEA.GT.CDATEE) EXIT FILES
         IF (CDTINTT.LT.CDATEA) CYCLE TIMES
      END DO

!     2.2.3 DO OUTPUT OF REQUESTED FIELDS.
!           ------------------------------

      WRITE (IU06,*) ' '
      IF (FRSTIME) THEN
         DO I = 1,30
           IF (.NOT.PFLAG(I) .AND. CFLAG(I)) THEN
              WRITE(IU06,*) TITL(I), 'IS NOT STORED IN FILE'
            END IF
         END DO     
         FRSTIME = .FALSE.
         WRITE (IU06,*) ' ' 
      END IF
      IF (PFLAG(1) .AND. CFLAG(1)) CALL PRINT_ARRAY (IU06,CDTINTT,     & 
&            TITL(1), U10_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(1))
      IF (PFLAG(2) .AND. CFLAG(2)) CALL PRINT_ARRAY (IU06,CDTINTT,     & 
&            TITL(2), UDIR_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(2))
      IF (PFLAG(3) .AND. CFLAG(3)) CALL PRINT_ARRAY (IU06,CDTINTT,     & 
&            TITL(3), US_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(3))
      IF (PFLAG(4) .AND. CFLAG(4)) CALL PRINT_ARRAY (IU06,CDTINTT,     & 
&            TITL(4), CD_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(4))

      IF (PFLAG(7) .AND. CFLAG(7)) CALL PRINT_ARRAY (IU06,CDTINTT,      & 
&            TITL(7), HS_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(7))
      IF (PFLAG(8) .AND. CFLAG(8)) CALL PRINT_ARRAY (IU06,CDTINTT,      & 
&            TITL(8), PPER_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(8))
      IF (PFLAG(9) .AND. CFLAG(9)) CALL PRINT_ARRAY (IU06,CDTINTT,      & 
&            TITL(9), MPER_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(9))
      IF (PFLAG(10) .AND. CFLAG(10)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(10), TM1_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(10))
      IF (PFLAG(11) .AND. CFLAG(11)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(11), TM2_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(11))
      IF (PFLAG(12) .AND. CFLAG(12)) CALL PRINT_ARRAY (IU06,CDTINTT,    & 
&            TITL(12), MDIR_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(12))
      IF (PFLAG(13) .AND. CFLAG(13)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(13), SPRE_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(13))
      IF (PFLAG(14) .AND. CFLAG(14)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(14), TAUW_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(14))

      IF (PFLAG(15) .AND. CFLAG(15)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(15), HS_SEA_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(15))
      IF (PFLAG(16) .AND. CFLAG(16)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(16), PPER_SEA_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(16))
      IF (PFLAG(17) .AND. CFLAG(17)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(17), MPER_SEA_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(17))
      IF (PFLAG(18) .AND. CFLAG(18)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(18), TM1_SEA_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(18))
      IF (PFLAG(19) .AND. CFLAG(19)) CALL PRINT_ARRAY (IU06,CDTINTT,    & 
&            TITL(19), TM2_SEA_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(19))
      IF (PFLAG(20) .AND. CFLAG(20)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(20), MDIR_SEA_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(20))
      IF (PFLAG(21) .AND. CFLAG(21)) CALL PRINT_ARRAY (IU06,CDTINTT,    & 
&            TITL(21), SPRE_SEA_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(21))

      IF (PFLAG(23) .AND. CFLAG(23)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&             TITL(23), HS_SWELL_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(23))
      IF (PFLAG(24) .AND. CFLAG(24)) CALL PRINT_ARRAY (IU06,CDTINTT,    & 
&            TITL(24), PPER_SWELL_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(24))
      IF (PFLAG(25) .AND. CFLAG(25)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(25), MPER_SWELL_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(25))
      IF (PFLAG(26) .AND. CFLAG(26)) CALL PRINT_ARRAY (IU06,CDTINTT,    & 
&            TITL(26), TM1_SWELL_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(26))
      IF (PFLAG(27) .AND. CFLAG(27)) CALL PRINT_ARRAY (IU06,CDTINTT,   & 
&            TITL(27), TM2_SWELL_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(27))
      IF (PFLAG(28) .AND. CFLAG(28)) CALL PRINT_ARRAY (IU06,CDTINTT,    & 
&            TITL(28), MDIR_SWELL_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(28))
      IF (PFLAG(29) .AND. CFLAG(29)) CALL PRINT_ARRAY (IU06,CDTINTT,  & 
&            TITL(29), SPRE_SWELL_GR, AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(29))

!
!*    2.2.7 NEXT OUTPUT TIME.
!           -----------------

      CALL NEXT_OUTPUT_TIME
      IF (CDATEA.GT.CDATEE) EXIT FILES
   END DO TIMES
   
   CALL INCDATE (CDTFILE, IDFILE)       !! INCREMENT DATE FOR THE NEXT FILE.
   CLOSE (UNIT=IU01, STATUS='KEEP')     !! CLOSE OLD FILE
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
ENDIF
RETURN
END  SUBROUTINE NEXT_OUTPUT_TIME

END PROGRAM PRINT_GRID_FILE
