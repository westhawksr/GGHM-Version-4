C-------------------------------------------------------------------
C     CONSTRUCT FILENAME FOR EMME MF SPECIFICATION
C-------------------------------------------------------------------
      SUBROUTINE MFNAME2(MNO,FILENAME)
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
	    INTEGER*4    MNO,BEG,END
      CHARACTER*4  SUFFIX
      CHARACTER*1  FNO1,BLANK
      CHARACTER*2  FNO2
      CHARACTER*3  FNO3
      CHARACTER*4  FNO4
      CHARACTER*2  PREFIX
      CHARACTER*200 FILENAME
      LOGICAL      EXISTS
      DATA         PREFIX/'mf'/,SUFFIX/'.emx'/,BLANK/' '/
C
      DO BEG=1,200
      END=BEG
      FILENAME(BEG:END)=BLANK
      END DO
      FILENAME(1:PRELEN)=FPREFIX
      BEG=PRELEN+1
      END=BEG+1
      FILENAME(BEG:END)=PREFIX
      IF(MNO.LE.0) RETURN
      IF(MNO.GE.1000.AND.MNO.LT.10000) THEN
      BEG=END+1
      END=BEG+3
      WRITE(FNO4,'(I4)') MNO
      FILENAME(BEG:END)=FNO4
      END IF
      IF(MNO.GE.100.AND.MNO.LT.1000) THEN
      BEG=END+1
      END=BEG+2
      WRITE(FNO3,'(I3)') MNO
      FILENAME(BEG:END)=FNO3
      END IF
      IF(MNO.GE.10.AND.MNO.LT.100)  THEN
      BEG=END+1
      END=BEG+1
      WRITE(FNO2,'(I2)') MNO
      FILENAME(BEG:END)=FNO2  
      END IF    
      IF(MNO.LT.10) THEN
      BEG=END+1
      END=BEG
      WRITE(FNO1,'(I1)') MNO
      FILENAME(BEG:END)=FNO1
      END IF
C
      BEG=END+1
      END=BEG+3
      FILENAME(BEG:END)=SUFFIX
C......................................................
      IF(DEBUG) THEN
C     WRITE(26,7001) MNO,FILENAME
 7001 FORMAT(' MATRIX NUMBER=',I4,' FILENAME=',A200)
      END IF
C......................................................
      if(chkout) then
      inquire (file=filename,exist=exists)
      if(.not.exists) then
      WRITE(26,7002) filename
      WRITE(*,7002) FILENAME
 7002 FORMAT(' MFNAME 7002 (F) EMME MATRIX BINARY FILE ',A40,
     *       ' NOT FOUND')
      STOP 7002
      END IF
      end if
      RETURN
      END
