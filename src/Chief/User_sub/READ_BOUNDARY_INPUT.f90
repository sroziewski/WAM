SUBROUTINE READ_BOUNDARY_INPUT

! ---------------------------------------------------------------------------- !
!
!**** *READ_BOUNDARY_INPUT* - READ BOUNDARY VALUE INPUT FILE.
!
!     H. GUNTHER    GKSS         JANUARY 2002
!
!*    PURPOSE.
!     --------
!
!       READ BOUNDARY BOUNDARY SPECTRA.
!
!     METHOD.
!     -------
!
!       WHEN TIME REACHES A NEW FILE TIME, THE INPUT FILE IS OPENED 
!       THE FILE HEADER IS READ AND THE CONSISTENCY IS CHECKED. 
!       THE SUB. READS A COMPLETE SET OF BOUNDARY VALUES WHEN IT IS CALLED. 

!
!     REFERENCE.
!     ----------
!
!       NONE.
!
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATES PROCESSING.
&       OPEN_FILE                   !! OPENS A FILE.

! ---------------------------------------------------------------------------- !
!
!*    MODULE VARIABLES.
!     -----------------

USE WAM_NEST_MODULE,   ONLY: NBINP

USE WAM_BOUNDARY_MODULE,ONLY: CDTLAST, XLON, XLAT, IDELINP,                    &
&                             CDATE2, EMEAN2, THQ2, FMEAN2, F2

USE WAM_FRE_DIR_MODULE, ONLY: KL, ML, CO, FR, TH

USE WAM_FILE_MODULE,    ONLY: IU06, ITEST, IU02, FILE02, CDTRES

IMPLICIT NONE

! ---------------------------------------------------------------------------- !

INTEGER       :: M, IJ, IOS, IFAIL, KL1, ML1, NBOINP

REAL          :: XANG, XFRE, TH0, FR1, CO1, XBOU, XDELIN

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    1. OPEN SPECTRA INPUT FILE AND READ HEADER.                              !
!        ----------------------------------------                              !

IF (CDTLAST.LT.CDTRES) THEN
   CLOSE (UNIT=IU02)
   CALL OPEN_FILE (IU06, IU02, FILE02, CDTRES, 'OLD', IFAIL)
   IF (IFAIL.NE.0) CALL ABORT1
   CDTLAST = CDTRES
   IOS = 0

   READ (IU02, IOSTAT=IOS) XANG, XFRE, TH0, FR1, CO1, XBOU, XDELIN
   IF (IOS.NE.0) THEN
      WRITE(IU06,*) '****************************************************'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '*    FATAL READ ERROR SUB. READ_BOUNDARY_INPUT.    *'
      WRITE(IU06,*) '*    ==========================================    *'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '* PROGRAM TRIES TO READ HEADER OF BOUNDARY VALUES  *'
      WRITE(IU06,*) '*    FILE ID IS FILE02 = ', FILE02
      WRITE(IU06,*) '*         UNIT IS IU02 = ', IU02
      WRITE(IU06,*) '*               IOSTAT = ', IOS
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '*         PROGRAM ABORTS.   PROGRAM ABORTS.        *'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '****************************************************'
      CALL ABORT1
   END IF

!*    1.1 PROCESS HEADER.
!         ---------------

   KL1 = NINT(XANG)
   ML1 = NINT(XFRE)
   NBOINP  = NINT(XBOU)
   IDELINP = NINT(XDELIN)
   IF (ITEST.GT.3) THEN
      WRITE (IU06,*) ' '
      WRITE (IU06,*) ' BOUNDARY VALUE INPUT FILE HEADER IS:'
      WRITE (IU06,*) ' NO. OF DIRECTIONS IS    KL1    = ', KL1
      WRITE (IU06,*) ' NO. OF FREQUENCIES IS   ML1    = ', ML1
      WRITE (IU06,*) ' FIRST DIRECTION IS      TH0    = ', TH0
      WRITE (IU06,*) ' FIRST FREQUENCY IS      FR1    = ', FR1
      WRITE (IU06,*) ' FREQUENCY RATIO IS      CO1    = ', CO1
      WRITE (IU06,*) ' NO. OF BOUNDRAY POINTS  NBOINP = ', NBOINP
      WRITE (IU06,*) ' TIME STEP OF DATA IS    IDELINP= ', IDELINP
   END IF

!*    1.2 CHECK CONSISTENCY.
!         ------------------

   IF (KL1.NE.KL .OR. ML1.NE.ML .OR. NBOINP.NE.NBINP .OR.                      &
&      FR1.NE.FR(1) .OR. TH0.NE.TH(1)) THEN

      WRITE (IU06,*) '****************************************************'
      WRITE (IU06,*) '*                                                  *'
      WRITE (IU06,*) '*    FATAL READ ERROR SUB. READ_BOUNDARY_INPUT.    *'
      WRITE (IU06,*) '*    ==========================================    *'
      WRITE (IU06,*) '*                                                  *'
      WRITE (IU06,*) '* VALUES IN BOUNDARY FILE HEADER ARE INCONSISTENT  *'
      WRITE (IU06,*) '* WITH MODEL SET-UP.                               *'
      WRITE (IU06,*) '* MODEL VALUES ARE:                                *'
      WRITE (IU06,*) '* NO. OF DIRECTIONS       KL   = ', KL
      WRITE (IU06,*) '* NO. OF FREQUENCIES      ML   = ', ML
      WRITE (IU06,*) '* FIRST DIRECTION       TH0    = ', TH0
      WRITE (IU06,*) '* FIRST FREQUENCY       FR(1)  = ', FR(1)
      WRITE (IU06,*) '* NUMBER OF POINTS      NBINP  = ', NBINP
      WRITE (IU06,*) '*                                                  *'
      WRITE (IU06,*) '* PROGRAM ABORTS.   PROGRAM ABORTS.                *'
      WRITE (IU06,*) '*                                                  *'
      WRITE (IU06,*) '****************************************************'
      CALL ABORT1
   END IF

END IF


! ----------------------------------------------------------------------       !
!                                                                              !
!*    2. READ BOUNDARY VALUES.                                                 !
!        ---------------------                                                 !

DO IJ = 1,NBINP
   READ (IU02, IOSTAT=IOS) XLON(IJ), XLAT(IJ), CDATE2,                         &
&                          EMEAN2 (IJ), THQ2(IJ), FMEAN2(IJ)
   IF (IOS.NE.0) THEN
      WRITE(IU06,*) '****************************************************'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '*    FATAL READ ERROR SUB. READ_BOUNDARY_INPUT.    *'
      WRITE(IU06,*) '*    ==========================================    *'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '* PROGRAM TRIES TO READ HEADER OF SPECTRA          *'
      WRITE(IU06,*) '* SPECTRA COUNTER IS IJ = ', IJ
      WRITE(IU06,*) '*          UNIT IS IU02 = ', IU02
      WRITE(IU06,*) '*                IOSTAT = ', IOS
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '*         PROGRAM ABORTS.   PROGRAM ABORTS.        *'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '****************************************************'
      CALL ABORT1
   END IF

   READ (IU02, IOSTAT=IOS) F2(1:KL,1:ML,IJ)
   IF (IOS.NE.0) THEN
      WRITE(IU06,*) '****************************************************'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '*    FATAL READ ERROR SUB. READ_BOUNDARY_INPUT.    *'
      WRITE(IU06,*) '*    ==========================================    *'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '* PROGRAM TRIES TO READ A SPECTRUM                 *'
      WRITE(IU06,*) '* SPECTRA COUNTER IS IJ = ', IJ
      WRITE(IU06,*) '*          UNIT IS IU02 = ', IU02
      WRITE(IU06,*) '*                IOSTAT = ', IOS
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '*         PROGRAM ABORTS.   PROGRAM ABORTS.        *'
      WRITE(IU06,*) '*                                                  *'
      WRITE(IU06,*) '****************************************************'
      CALL ABORT1
   END IF
END DO

RETURN
END SUBROUTINE READ_BOUNDARY_INPUT
