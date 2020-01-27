SUBROUTINE PREPARE_EXTRACTION (NS, LONGITUDE, LATITUDE, I, K)

! ---------------------------------------------------------------------------- !

USE WAM_PRINT_MODULE, ONLY: IU06, ITEST,                                       &
&                           NX, NY, PER, AMOWEP, AMOSOP, AMOEAP, AMONOP,       &
&                           XDELLA, XDELLO
IMPLICIT NONE

INTEGER, INTENT(IN)    :: NS
REAL,    INTENT(INOUT) :: LONGITUDE(1:NS)
REAL,    INTENT(INOUT) :: LATITUDE(1:NS)
INTEGER, INTENT(OUT)   :: I(1:NS)
INTEGER, INTENT(OUT)   :: K(1:NS)

! ---------------------------------------------------------------------------- !

REAL      :: EPS

! ---------------------------------------------------------------------------- !
!                                                                              !

IF (NX.GT.1) THEN
   XDELLO = (AMOEAP-AMOWEP)/REAL(NX-1)
ELSE
   XDELLO = 1.
END IF
IF (NY.GT.1) THEN
   XDELLA = (AMONOP-AMOSOP)/REAL(NY-1)
ELSE
   XDELLA = 1.
END IF

EPS = ABS(XDELLO)*EPSILON(XDELLO)
PER = ABS(AMOEAP+XDELLO-360.-AMOWEP) .LT. EPS

IF (PER) WRITE (IU06,*) '   THE GRID IS EAST-WEST PERIODIC'

! ---------------------------------------------------------------------------- !
!                                                                              !

I = NINT(MOD(LONGITUDE-AMOWEP+720.,360.)/XDELLO+1.)
K = NINT((LATITUDE-AMOSOP)/XDELLA+1.)
WHERE (I.EQ.NX+1  .AND. PER) I = 1
WHERE (I.EQ.0     .AND. PER) I = NX
LONGITUDE = AMOWEP+REAL(I-1)*XDELLO
LATITUDE  = AMOSOP+REAL(K-1)*XDELLA

! ---------------------------------------------------------------------------- !
!                                                                              !

WHERE (I.LT.1.OR.I.GT.NX .OR. K.LT.1.OR.K.GT.NY) I = -1

END SUBROUTINE PREPARE_EXTRACTION
