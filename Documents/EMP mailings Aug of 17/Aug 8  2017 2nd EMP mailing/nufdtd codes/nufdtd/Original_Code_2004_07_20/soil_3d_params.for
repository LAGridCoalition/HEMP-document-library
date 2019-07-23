C
C     SPECIFY THE PROBLEM SPACE SIZE HERE.  nxf, nyf AND nzf SPECIFY
C     THE SIZE OF THE PROBLEM SPACE IN THE X, Y AND Z DIRECTIONS.
C
C	 5 different soil type that is why is xx2, xx3, uu2, uu3, 
C      uu1x, uu1y, uu1z, k0, ..., k3, dtedxs, dtedys, dtedzs you see dim (20)


      PARAMETER (nxf=129,nyf=129,nzf=49
     $,nxf1=nxf-1,nyf1=nyf-1,nzf1=nzf-1)
	PARAMETER (nzfsoil=nzf)

	parameter(plane=.true.)

	PARAMETER (puind=23192,pgind=1768,mtind=77560,rind=8)
	PARAMETER (a1ind=6340,a2ind=11330,a3ind=17584)
	PARAMETER (a4ind=19594,a5ind=19594,a6ind=19594)
	PARAMETER(IPUL=(nxf+1)/2,JPUL=3)
	PARAMETER(KPUL1=nzf/2-25,KPUL4=nzf+1-KPUL1)
	PARAMETER(KPUL2=KPUL1+3,KPUL3=KPUL4-3)
C


C     DEFINE THE FIELD OUTPUT QUANTITIES HERE.  NTEST SPECIFIES
C     THE NUMBER OF FIELD SAMPLE LOCATIONS FOR NEAR ZONE FIELDS.
C
       PARAMETER (NTEST=30)
C
C     DEFINE NSTOP, (MAXIMUM NUMBER OF TIME STEPS) HERE.

c       PARAMETER (NSTOP=1000)
c Length of time sequence at the far field.
c	parameter (mstop=nstop+250)
C
C     DEFINE CELL SIZE (DELX, DELY, DELZ, IN METERS) HERE.


C    ******** new f ***********************************
c      PARAMETER (DELX=3e-2,DELY=3e-2,DELZ=1e-2)
c	common /var14/delx,dely,delz,rf
c	Length of the pulser output file is defined here.
	parameter (len_pulser=613)

cc One half of the diagonal distance of the computational domain.. 
c	parameter (rf=90*dely)

c *********** new fend ******************************** 
cc Reference point coordinates at the center of the computational domain
	parameter (ic=(nxf+1)/2,jc=(nyf+1)/2,kc=(nzf+1)/2)
cc Number of units to be recessed from the ends of the domain to define 
cc a cubic surface to calculate the far fields..
	parameter (nrecess=30)
C
C     SET INCIDENCE ANGLE OF INCIDENT PLANE WAVE HERE
C
C     SINCE THIS IS TOTAL FIELD FORMULATION CODE, 
C     THE INCIDENCE ANGLES ARE TAKEN AS THE ANGLES THE
C     PROPAGATION VECTOR MAKES WITH THE X AND Z AXES.
C     THINC AND PHINC ARE SPECIFIED AS FOLLOWS:
C     THINC IS SPECIFIED FROM THE +Z AXIS
C     PHINC IS SPECIFIED FROM THE +X AXIS
C     (ALL ANGLES ARE SPECIFIED IN DEGREES)
C
       PARAMETER (THINC=90.0,PHINC=270.0)
C
C     SPECIFY POLARIZATION OF INCIDENT PLANE WAVE HERE
C
C     SET INCIDENT WAVE POLARIZATION--ETHINC = 1 FOR THETA POLARIZED
C     ELECTRIC FIELD, EPHINC = 1 FOR PHI POLARIZED
C
       PARAMETER (ETHINC=0.0,EPHINC=1.0)
C
C     SET INCIDENT WAVEFORM PARAMETERS
C
C     PARAMETER AMP IS THE MAXIMUM AMPLITUDE OF THE INCIDENT PLANE
C     WAVE AND BETA IS THE TEMPORAL WIDTH OF A GAUSSIAN PULSE SPECIFIED
C     IN TIME STEPS.
C
c       PARAMETER (AMP=0.0,BETA=500.0)
       PARAMETER (AMP=0.0)
C
C
       PARAMETER (EPS0=8.854E-12,XMU0=1.2566306E-6,ETA0=376.733341,
     $	EPSprime=4.167,EPS2prime=1.95563,EPS3prime=5.03815
     $	,eps4prime=7.0,eps5prime=2.08814, eps6prime=75.3619813
     $    ,eps7prime=7.69,eps8prime=8.462, eps9prime=9.40
     $    ,eps10prime=6.912,eps11prime=4.9508098,eps12prime=20.9
     $    ,eps13prime=3.64315259,eps14prime=13.60445
     $    ,eps15prime=3.69898497,eps16prime=2.1)
	character *8,fansair

	real *8 a1,b0,b1,b2,k0,k1,k2,k3
     $        ,l0,l1,l2,l3 

	real *8 airthf,antdepf,corerdf,shellthf,expdiehf,diethf
	  real *8 iairth,iantdep,icorerd,ishellth,iexpdieh,idierd,feps3
     	 real *8 EX1D,EX1D1,EX1D2,EX1D3,HY1D,HY1D1,feps,sigmaff
	  real *8 delx,dely,delz,exs,eys,ezs,hxs,hys,hzs,lgapf,ilgap
	real *8 CXD,CXU,CYD,CYU,CZD,CZU,xx1X,xx1Y,xx1Z
	real *8 CXX,CYY,CZZ,CXFYD,CXFZD,CYFXD,CYFZD,CZFXD,CZFYD
     	real *8 uu1X,uu1Y,uu1Z,uu2,uu3,xreceiv,yreceiv,zreceiv
	real *8 xsource,ysource,zsource
c	real *8 xobject,yobject,zobject,fll,fww,fhh
C
	 COMMON/TEMPS/TEMPI,TEMPJ,TEMPK
       COMMON/IDS/IDONE(nxf,nyf,nzf),IDTWO(nxf,nyf,nzf),
     $            IDTHRE(nxf,nyf,nzf),idone14(nxf,nyf,nzf)
       COMMON/ESCAT/EXS(nxf,nyf,nzf),EYS(nxf,nyf,nzf),EZS(nxf,nyf,nzf)
       COMMON/HSCAT/HXS(nxf,nyf,nzf),HYS(nxf,nyf,nzf),HZS(nxf,nyf,nzf)
       COMMON/FIELD1D/EX1D(nzf),EX1D1(nzf),EX1D2(nzf),EX1D3(nzf),
     $				HY1D(nzf),HY1D1(nzf)
c-----------------------------------------------------------------
c      Added by JDP
c       COMMON/EINC/EXI(nxf,nyf,nzf),EYI(nxf,nyf,nzf),EZI(nxf,nyf,nzf)
c       COMMON/HINC/HXI(nxf,nyf,nzf),HYI(nxf,nyf,nzf),HZI(nxf,nyf,nzf)
c-----------------------------------------------------------------
C
       COMMON /MUR/  CXD,CXU,CYD,CYU,CZD,CZU,xx1X,xx1Y,xx1Z
       COMMON /MUR2/ CXX,CYY,CZZ,CXFYD,CXFZD,CYFXD,CYFZD,CZFXD,CZFYD,
     $			   uu1X(50),uu1Y(50),uu1Z(50),uu2(50),uu3
C************************************************************
C - need to add a, b, and k constants for conductivity calc's here
	 COMMON/DISP/a1(50),b0(50),b1(50),b2(50),k0(50),k1(50),k2(50)
     $	 ,k3(50),l0,l1,l2,l3,xx2(50),xx3(50)
c     $  ,nx,ny,nz,nx1,ny1,nz1,nzsoil

	 COMMON/VAR1/xsource,ysource,zsource,xreceiv,yreceiv,zreceiv
	 COMMON/VAR2/xobject,yobject,zobject,fll,fww,fhh
c	real *8 nx,ny,nz,nx1,ny1,nz1,nzsoil
C************************************************************
C 19990811 jt - added following for soil mod's
C	Place avg soil level at halfway of xmax in mesh
C	This will split the array at the Ex half-indice.
C      Even with varying soil, this will be the absolute
C      min that we can use for Soil placement in the 
C      x direction.  The SOIL subroutine will be used
C      to add a random component to the soil surface.

C************************************************************
C************************************************************
C 19990811 jt - updated saved value sace for soil mod's
C - still need to check limits for edge/boundary calc's
c--------------------------------------------
c      Added by JDP
       REAL*8 EXS1(nxf1,nyf1,nzfsoil),EYS1(nxf1,nyf1,nzfsoil),
     $        EZS1(nxf1,nyf1,nzfsoil)
       REAL*8 EXS2(nxf1,nyf1,nzfsoil),EYS2(nxf1,nyf1,nzfsoil),
     $        EZS2(nxf1,nyf1,nzfsoil)
       REAL*8 EXS3(nxf1,nyf1,nzfsoil),EYS3(nxf1,nyf1,nzfsoil),
     $        EZS3(nxf1,nyf1,nzfsoil)
       REAL*8 HXS1(nxf1,nyf1,nzfsoil),HYS1(nxf1,nyf1,nzfsoil),
     $        HZS1(nxf1,nyf1,nzfsoil)
       REAL*8 pulse_y(len_pulser,38)
       REAL*8 pulse_x(len_pulser,38)
       REAL*8 pulse(25,25)

c ---- new f -----------------
       COMMON/save1/nx,ny,nz,nx1,ny1,nz1,nzsoil,sltype(50),sltypef(50)
c     $	,xcenreg,ycenreg,zcenreg,xlenreg,ylenreg,zlenreg
c ---------new fend -------------

c--------------------------------------------
       COMMON/Esoil1/EXS1,EYS1,EZS1
       COMMON/Esoil2/EXS2,EYS2,EZS2
       COMMON/Esoil3/EXS3,EYS3,EZS3
       COMMON/Hsoil1/HXS1,HYS1,HZS1
       COMMON/pulsedata/pulse,pulse_x,pulse_y

c************************************************************
       common/radsav/eysx1(4,nyf1,nzf1),ezsx1(4,nyf1,nzf1),
     $               ezsy1(nxf1,4,nzf1),exsy1(nxf1,4,nzf1),
     $               exsz1(nxf1,nyf1,4),eysz1(nxf1,nyf1,4)
       COMMON/RADSV2/EYSX2(4,nyf1,nzf1),EZSX2(4,nyf1,nzf1),
     $               EZSY2(nxf1,4,nzf1),EXSY2(nxf1,4,nzf1),
     $               EXSZ2(nxf1,nyf1,4),EYSZ2(nxf1,nyf1,4)
	common/radsav3/eysx3(4,nyf1,nzf1),ezsx3(4,nyf1,nzf1),
     $               ezsy3(nxf1,4,nzf1),exsy3(nxf1,4,nzf1),
     $               exsz3(nxf1,nyf1,4),eysz3(nxf1,nyf1,4)

C************************************************************
C
       COMMON /INCPW/ AMPX, AMPY, AMPZ, XDISP, YDISP, ZDISP, DELAY
     $               ,TAU,OFF, AMODFREQ, SBEGIN, SFREQ, GAMMA,
     $			   cosquare,sinsquare
       COMMON/EXTRAS/N,DT,T,NPTS,C,PI,ALPHA,PERIOD,BETADT,W1,W2,W3
C
       COMMON/TERMS/ESCTC(13),EINCC(13),DTEDXD(13),DTEDYD(13),DTEDZD(13)
     $       ,DTEDX,DTEDY,DTEDZ,DTMDX,DTMDY,DTMDZ,DTE1D,DTXI
	  common /var14/delx,dely,delz,rf,flanadf

C
       COMMON/SOILTERMS/DTEDXS(50),DTEDYS(50),DTEDZS(50),nsoillayf
     $  ,feps,freqf,nsoilreg

       COMMON/CONSTI/EPS(13),SIGMA(13), PULSER(LEN_PULSER), AVEXSPR(6) !injast
c       COMMON/CONSTI/EPS(30),SIGMA(30), PULSER(LEN_PULSER), AVEXSPR(6)
C
       COMMON/SAVE1/store(ntest),STOREz(NTEST),STOREx(NTEST)
     $  ,STORH(NTEST),hsoilf(10),epsprimef(50),NTYPE(NTEST)
     $  ,STORExx(NTEST),IOBS(NTEST),JOBS(NTEST),KOBS(NTEST)
     $ ,idiek(150,150),idiei(200),idiej(200) !,diek(2000)
     $ ,ishellk(100,100),ishelli(200),ishellj(200) !,diek(2000)
	common/chara/fansair

	  common/fara/airthf,antdepf,corerdf,shellthf,expdiehf,diethf
     $	,iairth,iantdep,icorerd,ishellth,iexpdieh,idierd,feps3,sigmaff
     $	,isource,jsource,ksource,issf,jssf,kssf,kbcore,nstop,mstop
     $  	,lgapf, ilgap, ireceiv,jreceiv, kreceiv,reclen,diptype,ngauss

		
c	character (15) ::fsource,fnamef,fansparf,frantenna
c	character (15) ::fansobj,fansrec,fansrecp,fansairf












