C***********************************************************
C     OPEN OUTPUT  FILES                                   *
C***********************************************************
      SUBROUTINE FILEWRITE
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
      CHARACTER*200 FILENAME
C
C FILE CLOSURES
C
      close(21,status='delete')
      do k=44,61
      close(k,status='delete')
      end do
      do k=124,127
      close(k,status='delete')
      end do
      do k=132,136
      close(k,status='delete')
      end do
      do k=161,171
      close(k,status='delete')
      end do
      close(180,status='keep')
      close(181,status='keep')
      do k=184,188
      close(k,status='delete')
      end do
      do k=191,192
      close(k,status='delete')
      end do
      do k=201,205
      close(k,status='delete')
      end do
      close(278,status='delete')
      do k=301,325
      close(k,status='delete')
      end do
      do k=337,339
      close(k,status='delete')
      end do
C
C     OPEN OUTPUT TRIP MATRICES
C
      IF(TRIPSOUT) THEN
C....STATION LEVEL TRIPS
      chkout=.false.
      DO IT=1,14
      CALL MFNAME(MFSTATRP(IT),FILENAME)
      FSTATRP(IT)=FILENAME
      END DO
      open(184,file=fstatrp(1),
     *       status='unknown',form='binary')
      open(44,file=fstatrp(2),
     *       status='unknown',form='binary')
      open(45,file=fstatrp(3),
     *       status='unknown',form='binary')
      open(46,file=fstatrp(4),
     *       status='unknown',form='binary')
      open(47,file=fstatrp(5),
     *       status='unknown',form='binary')
      open(48,file=fstatrp(6),
     *       status='unknown',form='binary')
      open(49,file=fstatrp(7),
     *       status='unknown',form='binary')
      open(50,file=fstatrp(8),
     *       status='unknown',form='binary')
      open(21,file=fstatrp(9),
     *       status='unknown',form='binary')
      open(171,file=fstatrp(10),
     *       status='unknown',form='binary')
      open(185,file=fstatrp(11),
     *       status='unknown',form='binary')
      open(186,file=fstatrp(12),
     *       status='unknown',form='binary')
      open(187,file=fstatrp(13),
     *       status='unknown',form='binary')
      open(188,file=fstatrp(14),
     *       status='unknown',form='binary')
C...ZONE TO ZONE TRANSIT TRIPS
      DO IT=1,17
      CALL MFNAME(MFTRIPS(IT),FILENAME)
      FTRIPS(IT)=FILENAME
      END DO
      chkout=.true.
      open(51,file=ftrips(1),
     *       status='unknown',form='binary')
      open(52,file=ftrips(2),
     *       status='unknown',form='binary')
      open(53,file=ftrips(3),
     *       status='unknown',form='binary')
      open(54,file=ftrips(4),
     *       status='unknown',form='binary')
      open(55,file=ftrips(5),
     *       status='unknown',form='binary')
      open(56,file=ftrips(6),
     *       status='unknown',form='binary')
      open(57,file=ftrips(7),
     *       status='unknown',form='binary')
      open(58,file=ftrips(8),
     *       status='unknown',form='binary')
      open(59,file=ftrips(9),
     *       status='unknown',form='binary')
      open(60,file=ftrips(10),
     *       status='unknown',form='binary')
      open(124,file=ftrips(11),
     *       status='unknown',form='binary')
      open(125,file=ftrips(12),
     *       status='unknown',form='binary')
      open(126,file=ftrips(13),
     *       status='unknown',form='binary')
      open(127,file=ftrips(14),
     *       status='unknown',form='binary')
      open(61,file=ftrips(15),
     *       status='unknown',form='binary')
      if(mftrips(16).gt.0) then
      open(180,file=ftrips(16),
     *       status='unknown',form='binary')
      end if
      if(mftrips(17).gt.0) then
      open(181,file=ftrips(17),
     *       status='unknown',form='binary')
      end if
      END IF
      IF(VEHOUT) THEN
C...ZONE TO ZONE AUTO TRIPS
      chkout=.false.
      DO IT=1,12
      CALL MFNAME(MFATRIPS(IT),FILENAME)
      FATRIPS(IT)=FILENAME
      END DO
      chkout=.true.
      open(161,file=fatrips(1),
     *       status='unknown',form='binary')
      open(162,file=fatrips(2),
     *       status='unknown',form='binary')
      open(163,file=fatrips(3),
     *       status='unknown',form='binary')
      open(164,file=fatrips(4),
     *       status='unknown',form='binary')
      open(165,file=fatrips(5),
     *       status='unknown',form='binary')
      open(166,file=fatrips(6),
     *       status='unknown',form='binary')
      open(167,file=fatrips(7),
     *       status='unknown',form='binary')
      open(168,file=fatrips(8),
     *       status='unknown',form='binary')
      open(169,file=fatrips(9),
     *       status='unknown',form='binary')
      open(170,file=fatrips(10),
     *       status='unknown',form='binary')
      open(191,file=fatrips(11),
     *       status='unknown',form='binary')
      open(192,file=fatrips(12),
     *       status='unknown',form='binary')         
      END IF
C...DESTINATION CHOICE LOGSUM VALUES
      chkout=.false.
      IF(LSBASE) THEN
      DO IT=1,6
      CALL MFNAME(MFLSUM(IT),FILENAME)
      FLSUM(IT)=FILENAME
      END DO
      chkout=.true.
      open(131,file=flsum(1),
     *       status='unknown',form='binary')
      if(ncats.gt.1) then
      open(132,file=flsum(2),
     *       status='unknown',form='binary')
      open(133,file=flsum(3),
     *       status='unknown',form='binary')
      open(134,file=flsum(4),
     *       status='unknown',form='binary')
      open(135,file=flsum(5),
     *       status='unknown',form='binary')
      open(136,file=flsum(6),
     *       status='unknown',form='binary')
      end if
      END IF
C...AIR PASSENGER TRIPS
      chkout=.false.
      IF(AIRPASS.AND.TRIPSOUT.AND.VEHOUT) THEN
      DO IT=1,6
      CALL MFNAME(MFAIR(IT),FILENAME)
      FAIR(IT)=FILENAME
      END DO
      chkout=.true.
      open(201,file=fair(1),
     *       status='unknown',form='binary')
      open(202,file=fair(2),
     *       status='unknown',form='binary')
      open(203,file=fair(3),
     *       status='unknown',form='binary')
      open(204,file=fair(4),
     *       status='unknown',form='binary')
      open(205,file=fair(5),
     *       status='unknown',form='binary')
      open(278,file=fair(6),
     *       status='unknown',form='binary')
      END IF
C...ZONE TO PARKING LOT (AIR PASSENGER TRIPS)
      chkout=.false.
      IF(AIRPASS.AND.TRIPSOUT.AND.VEHOUT) THEN
      DO IT=1,3
      CALL MFNAME(MFLOT(IT),FILENAME)
      FLOT(IT)=FILENAME
      END DO
c     chkout=.true.
      open(337,file=flot(1),
     *       status='unknown',form='binary')
      open(338,file=flot(2),
     *       status='unknown',form='binary')
      open(339,file=flot(3),
     *       status='unknown',form='binary')
      END IF
C...ZONE TO ZONE TRANSIT TRIPS FOR VSKIM ONLY
      chkout=.false.
      IF(VSKIM) THEN
      DO IT=1,25     
      CALL MFNAME(MFVSKIM(IT),FILENAME)
      FVSKIMT(IT)=FILENAME
      END DO                                
      chkout=.true.                         
      do it=1,25                            
      open((it+300),file=fvskimt(it),       
     *       status='unknown',form='binary')
      end do                                
      END IF    
      RETURN
      END
                            