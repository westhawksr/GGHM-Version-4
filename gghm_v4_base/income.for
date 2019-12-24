C *******************************************************
C     INCOME SUBMODEL APPLICATION                       *
C *******************************************************
      SUBROUTINE INCOME(AVGINC)
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
      INTEGER*4    T
      REAL*4       AVGINC
      REAL*4       ICOEFF(4,6),INC_RATIO,INC1P,INC2P,INC3P,INC4P
      REAL*4       INC2N,INC3A,INC3B,INCTP,HHLDINC(5),HHLDPER(4)
      REAL*4       TAZDATA(4)
      DATA   ICOEFF/1.002835698,0.072746423,0.006493451,-0.013249680,
     *             -0.927810080,0.253469804,0.524404588,0.786059694,
     *              0.454250993,0.634249960,0.527564524,1.096602696,
     *              0.351314841,4.014653028,0.000000000001,0.187184906,
     *              1.730770247,3.828376874,0.141845285,0.256097678,
     *              0,0,1.515812221,0/
      HHLDINC=0.0
      HHLDPER=0.0
C
C     COMPUTE HOUSEHOLD INCOME PROPORTIONS
C
      DO T=1,MAX_IZONES
      INC_RATIO=ZHHD(8,T)/AVGINC
C.....INCOME GROUP 1
      IF(INC_RATIO.GT.0.3) THEN
      INC1P=ICOEFF(1,4)*ALOG(1.0-(2**(-1.0/ICOEFF(1,5))))
      INC1P=-1.0*((INC_RATIO-INC1P-ICOEFF(1,3))/ICOEFF(1,4))
      INC1P=ICOEFF(1,1)+ICOEFF(1,2)*
     *      (1.0-EXP(INC1P))**ICOEFF(1,5)
      ELSE
      CALL INTERP(INC_RATIO,INC1P)
      END IF
C.....INCOME GROUP 2
      INC2N=ICOEFF(2,5)*INC_RATIO+INC_RATIO+ICOEFF(2,4)-
     *      ICOEFF(2,3)*ICOEFF(2,5)-ICOEFF(2,3)
      INC2P=(INC_RATIO-ICOEFF(2,3))*((ICOEFF(2,5)+1)**2)/INC2N
      INC2P=ICOEFF(2,1)+((ICOEFF(2,2)*ICOEFF(2,4))/INC2N)*
     *      EXP(INC2P)*(INC2N/ICOEFF(2,4))**(-ICOEFF(2,5))
C.....INCOME GROUP 3
      INC3A=EXP(-1.0*((INC_RATIO-ICOEFF(3,3)+
     *      (ICOEFF(3,4)/2))/ICOEFF(3,5)))
      INC3B=EXP(-1.0*((INC_RATIO-ICOEFF(3,3)-
     *      (ICOEFF(3,4)/2))/ICOEFF(3,6)))
      INC3P=ICOEFF(3,1)+(ICOEFF(3,2)/(1+INC3A))*(1.0-(1.0/(1+INC3B)))
C.....INCOME GROUP 4
      INC4P=ICOEFF(4,4)*ALOG(2**(1.0/ICOEFF(4,5))-1.0)
      INC4P=EXP((INC_RATIO+INC4P-ICOEFF(4,3))/ICOEFF(4,4))
      INC4P=ICOEFF(4,1)+ICOEFF(4,2)*(1.0-(1+INC4P)**(-ICOEFF(4,5)))
C.....COMPUTE TOTAL PERCENT
      INCTP=INC1P+INC2P+INC3P+INC4P
      IF(DEBUG) WRITE(26,15) T,INC_RATIO,INC1P,INC2P,INC3P,INC4P,INCTP
   15 FORMAT(' TAZ=',I4,' INC_RATIO=',F5.2,' INC1P=',F8.4,
     *       ' INC2P=',F8.4,' INC3P=',F8.4,' INC4P=',F8.4,
     *       ' TOTAL=',F8.4)
C
C     NORMALIZE RESULTS
C
      TAZDATA(1)=(INC1P/INCTP)*ZHHD(9,T)
      TAZDATA(2)=(INC2P/INCTP)*ZHHD(9,T)
      TAZDATA(3)=(INC3P/INCTP)*ZHHD(9,T)
      TAZDATA(4)=ZHHD(9,T)-TAZDATA(1)-
     *              TAZDATA(2)-TAZDATA(3)
      ZHHD(14,T)=TAZDATA(4)
      HHLDINC(1)=HHLDINC(1)+TAZDATA(1)
      HHLDINC(2)=HHLDINC(2)+TAZDATA(2)
      HHLDINC(3)=HHLDINC(3)+TAZDATA(3)
      HHLDINC(4)=HHLDINC(4)+TAZDATA(4)
      IF(DEBUG) WRITE(26,16) T,ZHHD(9,T),TAZDATA(1),TAZDATA(2),
     *               TAZDATA(3),TAZDATA(4)
   16 FORMAT(' TAZ=',I4,' TOTHH=',F8.2,' INCOME=',4(1X,F6.1))
      END DO
      HHLDINC(5)=HHLDINC(1)+HHLDINC(2)+HHLDINC(3)+HHLDINC(4)
      HHLDPER(1)=(HHLDINC(1)/HHLDINC(5))*100.0
      HHLDPER(2)=(HHLDINC(2)/HHLDINC(5))*100.0
      HHLDPER(3)=(HHLDINC(3)/HHLDINC(5))*100.0
      HHLDPER(4)=(HHLDINC(4)/HHLDINC(5))*100.0
      WRITE(26,9003) ((HHLDINC(K),HHLDPER(K)),K=1,4),HHLDINC(5)
 9003 FORMAT(//' TOTAL REGIONAL HOUSEHOLDS BY INCOME GROUP'/
     *         ' -----------------------------------------'/
     *       ' INCOME GROUP 1=',F10.1,2X,F5.1,'%'/
     *       ' INCOME GROUP 2=',F10.1,2X,F5.1,'%'/
     *       ' INCOME GROUP 3=',F10.1,2X,F5.1,'%'/
     *       ' INCOME GROUP 4=',F10.1,2X,F5.1,'%'//
     *       ' TOTAL          ',F10.1/)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE INTERP(INC_RATIO,INC1P)
      REAL*4    INC_RATIO,INC1P,INC1(4)
      DATA   INC1/0.99,0.87,0.75,0.60/
      INC1P=99.9
C
      IF(INC_RATIO.LE.0.1) THEN
      INC1P=(INC_RATIO/0.1)*(0.99-0.87)
      INC1P=0.99-INC1P
      RETURN
      END IF
      IF(INC_RATIO.EQ.0.1) THEN
      INC1P=0.87
      RETURN
      END IF
      IF(INC_RATIO.LE.0.2) THEN
      INC1P=((INC_RATIO-0.1)/0.1)*(0.87-0.75)
      INC1P=0.87-INC1P
      RETURN
      END IF
      IF(INC_RATIO.EQ.0.2) THEN
      INC1P=0.75
      RETURN
      END IF
      IF(INC_RATIO.LE.0.3) THEN
      INC1P=((INC_RATIO-0.2)/0.1)*(0.75-0.60)
      INC1P=0.75-INC1P
      RETURN
      END IF
      IF(INC_RATIO.EQ.0.3) THEN
      INC1P=0.60
      RETURN
      END IF
      RETURN
      END