C***********************************************************
C     OBTAIN INPUT FILES                                   *
C***********************************************************
      SUBROUTINE FILEOPEN
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
C
C FILE CLOSURES
C
      do k=11,40
      if(k.eq.20.or.k.eq.21) cycle
      if(k.eq.26) cycle
      if(ncats.le.1.and.(k.ge.35.and.k.le.39)) cycle
      close(k,status='keep')
      end do
      do k=62,97
      close(k,status='keep')
      end do
      do k=108,119
      close(k,status='keep')
      end do
      do k=120,123
      close(k,status='keep')
      end do
      close(128,status='keep')
      do k=142,145
      close(k,status='keep')
      end do
      if(hwyrel) then
      do k=195,198
      close(k,status='keep')
      end do
      end if
      if(hwy3p) then
      do k=220,227
      close(k,status='keep')
      end do
      if(hwyrel) then
      do k=228,229
      close(k,status='keep')
      end do
      end if
      end if
      if(avmdl) then
      do k=231,236
      close(k,status='keep')
      end do
      do k=239,246
      close(k,status='keep')
      end do
      if(hwyrel) then
      close(239,status='keep')
      close(240,status='keep')
      close(247,status='keep')
      close(248,status='keep')
      end if
      if(hwy3p) then
      do k=249,256
      close(k,status='keep')
      end do
      if(hwyrel) then
      do k=257,258
      close(k,status='keep')
      end do
      end if
      end if       
      end if
C
C DIRECT WALK TO STATION
C
      open(19,file=fwlksta,
     *       status='old',form='binary')
C
C     OPEN ZONE-TO-STATION & STATION-TO-ZONE FILE NAMES
C
      open(11,file=fzsta(1),
     *       status='old',form='binary')
      open(12,file=fzsta(2),
     *       status='old',form='binary')
      open(13,file=fzsta(3),
     *       status='old',form='binary')
      open(14,file=fzsta(4),
     *       status='old',form='binary')
      open(15,file=fzsta(5),
     *       status='old',form='binary')
      open(16,file=fzsta(6),
     *       status='old',form='binary')
      open(17,file=fzsta(7),
     *       status='old',form='binary')
      open(18,file=fzsta(8),
     *       status='old',form='binary')
      open(32,file=fzsta(9),
     *       status='old',form='binary')
      open(65,file=fzsta(10),
     *       status='old',form='binary')
      open(66,file=fzsta(11),
     *       status='old',form='binary')
      if(ccr) then
      open(108,file=fzsta(12),
     *       status='old',form='binary')
      open(109,file=fzsta(13),
     *       status='old',form='binary')
      open(110,file=fzsta(14),
     *       status='old',form='binary')
      open(111,file=fzsta(15),
     *       status='old',form='binary')
      end if
C.....FARE MATRICES
      open(62,file=fzstafr(1),
     *       status='old',form='binary')
      open(63,file=fzstafr(2),
     *       status='old',form='binary')
      open(64,file=fzstafr(3),
     *       status='old',form='binary')
C
C  DRIVE ALONE SKIMS
C
      open(84,file=fhwy0(1),
     *       status='old',form='binary')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      open(86,file=fhwy0(3),
     *       status='old',form='binary')
      open(87,file=fhwy0(4),
     *       status='old',form='binary')
      open(88,file=fhwy0(5),
     *       status='old',form='binary')
      open(89,file=fhwy0(6),
     *       status='old',form='binary')
      if(hwyrel) then
      open(195,file=fhwy0(7),
     *       status='old',form='binary')
      open(196,file=fhwy0(8),
     *       status='old',form='binary')
      end if
C
C  HOV 2-PERSON SKIMS
C
      open(90,file=fhwy2p(1),
     *       status='old',form='binary')
      open(91,file=fhwy2p(2),
     *       status='old',form='binary')
      open(92,file=fhwy2p(3),
     *       status='old',form='binary')
      open(93,file=fhwy2p(4),
     *       status='old',form='binary')
      open(94,file=fhwy2p(5),
     *       status='old',form='binary')
      open(95,file=fhwy2p(6),
     *       status='old',form='binary')
      open(96,file=fhwy2p(7),
     *       status='old',form='binary')
      open(97,file=fhwy2p(8),
     *       status='old',form='binary')
      if(hwyrel) then
      open(197,file=fhwy2p(9),
     *       status='old',form='binary')
      open(198,file=fhwy2p(10),
     *       status='old',form='binary')
      end if
C
C  HOV 3+ PERSON SKIMS
C
      IF(HWY3P) THEN
      open(220,file=fhwy3p(1),
     *       status='old',form='binary')
      open(221,file=fhwy3p(2),
     *       status='old',form='binary')
      open(222,file=fhwy3p(3),
     *       status='old',form='binary')
      open(223,file=fhwy3p(4),
     *       status='old',form='binary')
      open(224,file=fhwy3p(5),
     *       status='old',form='binary')
      open(225,file=fhwy3p(6),
     *       status='old',form='binary')
      open(226,file=fhwy3p(7),
     *       status='old',form='binary')
      open(227,file=fhwy3p(8),
     *       status='old',form='binary')
      if(hwyrel) then
      open(228,file=fhwy3p(9),
     *       status='old',form='binary')
      open(229,file=fhwy3p(10),
     *       status='old',form='binary')
      end if
      END IF
C
C  WALK TO GO BUS SKIMS
C
      open(22,file=fwkgobus(1),
     *       status='old',form='binary')
      open(23,file=fwkgobus(2),
     *       status='old',form='binary')
      open(24,file=fwkgobus(3),
     *       status='old',form='binary')
      open(27,file=fwkgobus(4),
     *       status='old',form='binary')
      open(28,file=fwkgobus(5),
     *       status='old',form='binary')
      open(29,file=fwkgobus(6),
     *       status='old',form='binary')
      open(30,file=fwkgobus(7),
     *       status='old',form='binary')
      open(31,file=fwkgobus(8),
     *       status='old',form='binary')
      open(33,file=fwkgobus(9),
     *       status='old',form='binary')
      open(25,file=fwkgobus(10),
     *       status='old',form='binary')
      open(40,file=fwkgobus(11),
     *       status='old',form='binary')
      open(128,file=fwkgobus(16),
     *       status='old',form='binary')
      IF(CCR) THEN
      open(112,file=fwkgobus(12),
     *       status='old',form='binary')
      open(113,file=fwkgobus(13),
     *       status='old',form='binary')
      open(114,file=fwkgobus(14),
     *       status='old',form='binary')
      open(115,file=fwkgobus(15),
     *       status='old',form='binary')
      END IF
C
C  WALK TO BUS/STREETCAR SKIMS
C
      open(67,file=fwkbustr(1),
     *       status='old',form='binary')
      open(68,file=fwkbustr(2),
     *       status='old',form='binary')
      open(69,file=fwkbustr(3),
     *       status='old',form='binary')
      open(70,file=fwkbustr(4),
     *       status='old',form='binary')
      open(71,file=fwkbustr(5),
     *       status='old',form='binary')
      open(72,file=fwkbustr(6),
     *       status='old',form='binary')
      open(73,file=fwkbustr(7),
     *       status='old',form='binary')
      open(74,file=fwkbustr(8),
     *       status='old',form='binary')
      IF(CCR) THEN
      open(120,file=fwkbustr(9),
     *       status='old',form='binary')
      open(121,file=fwkbustr(10),
     *       status='old',form='binary')
      open(122,file=fwkbustr(11),
     *       status='old',form='binary')
      open(123,file=fwkbustr(12),
     *       status='old',form='binary')
      END IF
C
C  WALK TO RAPID BUS SKIMS
C
      open(75,file=fwkrap(1),
     *       status='old',form='binary')
      open(76,file=fwkrap(2),
     *       status='old',form='binary')
      open(77,file=fwkrap(3),
     *       status='old',form='binary')
      open(78,file=fwkrap(4),
     *       status='old',form='binary')
      open(79,file=fwkrap(5),
     *       status='old',form='binary')
      open(80,file=fwkrap(6),
     *       status='old',form='binary')
      open(81,file=fwkrap(7),
     *       status='old',form='binary')
      open(82,file=fwkrap(8),
     *       status='old',form='binary')
      open(83,file=fwkrap(9),
     *       status='old',form='binary')
      IF(CCR) THEN
      open(116,file=fwkrap(10),
     *       status='old',form='binary')
      open(117,file=fwkrap(11),
     *       status='old',form='binary')
      open(118,file=fwkrap(12),
     *       status='old',form='binary')
      open(119,file=fwkrap(13),
     *       status='old',form='binary')
      END IF
C
C     INPUT PERSON TRIP MATRICES
C
      if((.not.spevent).and.(.not.visitor)) then
      open(34,file=ftotper(1),
     *       status='old',form='binary')
      if(ncats.gt.1) then
      open(35,file=ftotper(2),
     *       status='old',form='binary')
      open(36,file=ftotper(3),
     *       status='old',form='binary')
      open(37,file=ftotper(4),  
     *       status='old',form='binary')
      open(38,file=ftotper(5),
     *       status='old',form='binary')
      open(39,file=ftotper(6),
     *       status='old',form='binary')
      end if
      end if
C
C     OPEN WALK AND BICYLCE LEVEL OF SERVICE MATRICES
C
      IF(NMOT) THEN
      open(142,file=fnmotor(1),
     *         status='old',form='binary')
      open(143,file=fnmotor(2),
     *         status='old',form='binary')
      open(144,file=fnmotor(3),
     *         status='old',form='binary')
      open(145,file=fnmotor(4),
     *         status='old',form='binary')
      END IF
C -------------------------------------------------
C              AUTOMATED VEHICLE SKIMS
C -------------------------------------------------
      if(avmdl) then
C
C  DRIVE ALONE SKIMS
C
      open(231,file=vhwy0(1),
     *       status='old',form='binary')
      open(232,file=vhwy0(2),
     *       status='old',form='binary')
      open(233,file=vhwy0(3),
     *       status='old',form='binary')
      open(234,file=vhwy0(4),
     *       status='old',form='binary')
      open(235,file=vhwy0(5),
     *       status='old',form='binary')
      open(236,file=vhwy0(6),
     *       status='old',form='binary')
      if(hwyrel) then
      open(237,file=vhwy0(7),
     *       status='old',form='binary')
      open(238,file=vhwy0(8),
     *       status='old',form='binary')
      end if
C
C  HOV 2-PERSON SKIMS
C
      open(239,file=vhwy2p(1),
     *       status='old',form='binary')
      open(240,file=vhwy2p(2),
     *       status='old',form='binary')
      open(241,file=vhwy2p(3),
     *       status='old',form='binary')
      open(242,file=vhwy2p(4),
     *       status='old',form='binary')
      open(243,file=vhwy2p(5),
     *       status='old',form='binary')
      open(244,file=vhwy2p(6),
     *       status='old',form='binary')
      open(245,file=vhwy2p(7),
     *       status='old',form='binary')
      open(246,file=vhwy2p(8),
     *       status='old',form='binary')
      if(hwyrel) then
      open(247,file=vhwy2p(9),
     *       status='old',form='binary')
      open(248,file=vhwy2p(10),
     *       status='old',form='binary')
      end if
C
C  HOV 3+ PERSON SKIMS
C
      IF(HWY3P) THEN
      open(249,file=vhwy3p(1),
     *       status='old',form='binary')
      open(250,file=vhwy3p(2),
     *       status='old',form='binary')
      open(251,file=vhwy3p(3),
     *       status='old',form='binary')
      open(252,file=vhwy3p(4),
     *       status='old',form='binary')
      open(253,file=vhwy3p(5),
     *       status='old',form='binary')
      open(254,file=vhwy3p(6),
     *       status='old',form='binary')
      open(255,file=vhwy3p(7),
     *       status='old',form='binary')
      open(256,file=vhwy3p(8),
     *       status='old',form='binary')
      if(hwyrel) then
      open(257,file=vhwy3p(9),
     *       status='old',form='binary')
      open(258,file=vhwy3p(10),
     *       status='old',form='binary')
      end if
      END IF
      END IF
      RETURN
      END
