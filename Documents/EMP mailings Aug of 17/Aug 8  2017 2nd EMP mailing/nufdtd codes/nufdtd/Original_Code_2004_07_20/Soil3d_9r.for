c   MODULATED INCIDENT WAVE...
C   ADNAN SAHIN,  8/12/97
C 	CAREY RAPPAPORT 5/98
C 	JOHN TALBOT 8/129
C	PANOS KOSMAS 6/2000
C		M. FARID
  
c if you add soil type, you gotto check wherever says !type+19  
c if you add object shape, you gotto check wherever says !type+20  

      PROGRAM soil_3d
 


	character *15,fansparf,frantenna,frantenna2,frmagn, fsoilreg
 	character *15,fansobj,fansrec,fansrecp,fansobjrec,fansdiprec
z


c new fend
	
      INCLUDE "soil_3d_params.for"
      


C   ZERO PARAMETERS, GENERATE PROBLEM SPACE, INTERACTION OBJECT,
C   AND EXCITATION
  

c----------------------------
c     JDP
      character*15 fnamevx,fnamevz,fnamevxz,fnamehx,fnamehz,fnamehxz
      character*15 fnamevy,ansgauss

      character*15 fmagnvx,fmagnvz,fmagnvxz,fmagnhx,fmagnhz,fmagnhxz
      character*15 fmagnhy,fmagnhxy,fmagnvy

c      character*15 fmagnvx,fmagnvz,fmagnvxz,fmagnhx,fmagnhz,fmagnhxz
      character*15 fnamehy,fnamehxy,fnameh,fmagnf
c     $ ,fnamenhx,fnamevy,fname2,fnamef,fsource
c      character*15 fmagnhxy,fmagnhy
c	fmagnfield='mag'

      character*15 fnovyy,fnovyz,fnovyyz
c      character*15 fname2
      character*15 foutput,fansourc
      real*8 temp,alak,flandaf, regl, regw, regh,ratiozz,cz1
      real*8 xcenreg,ycenreg,zcenreg,xlenreg,ylenreg,zlenreg,iflz10
c	real*8 lgapf, ilgap,xbcore,ybcore,zbcore


		print *,'Enter Grid Cell Size in cm (dx,dy,dz)'
c	print *,'ATTENTION: In case of Realistic Antenna, use: dx = dy '


		do while(delx==0. .or. dely==0. .or. delz==0.)
			read *,delx,dely,delz
		end do
		


		delx=delx/100.
		dely=dely/100.
		delz=delz/100.
		rf=90.*dely

c ****************
c	OPEN(UNIT=4111,FILE='alak.txt',STATUS='unknown')
c ****************
	print *,'Enter Total Steps to run the code (NSTOP)'
	
      do while(nstop==0)
		read *,nstop
	end do


	mstop=nstop+250
	print *,'Enter how often do you need output print (Default=10)'

	do while(ntime==0)
		read *,ntime
	end do
	 
  11	Print*,'Is the case 2D or 3D(2=2D and 3=3D)'
	
      do while(f23d/=2 .and. f23d/=3)
		read *,f23d
	end do

      if (f23d/=2 .and. f23d/=3) goto 11

		print *,'Delx=',delx*100,'cm'
		print *,'Dely=',dely*100,'cm'
		print *,'Delz=',delz*100,'cm'

	if (f23d==2) then

		print *,'Enter Grid size (Ny, Nz)'
		print *,'They should be odd not even'
		print *,'and you should chnage format to the biggest N'

	do while(ny==0 .or. nz==0)
		read *,ny,nz
	end do

		nx=5
	    fny=dfloat(ny)*100.*dely
		fnz=dfloat(nz)*100.*delz

	else
	
     		print *,'Enter Grid Size (Nx,Ny,Nz)'
		print *,'They should be odd not even'
		print *,'and you should chnage format to the biggest N'

		do while(nx==0 .or. ny==0 .or. nz==0)
			read *,nx,ny,nz
		end do


		fnx=dfloat(nx)*100.*delx
		fny=dfloat(ny)*100.*dely
		fnz=dfloat(nz)*100.*delz

	endif

		print *,'(Nx,Ny,Nz)=',nx,ny,nz
		print *,'(Length,Width,Height) of Grid=',fnx,fny,fnz

		print *,'Do you have AIR within the model?'
		
		do while(fansair/='y' .and. fansair/='Y' .and. 
     $		fansair/='n' .and. fansair/='N')
			read *, fansair
		end do





		nx1=nx-1
		ny1=ny-1
		nz1=nz-1


	if (fansair=='y' .or. fansair=='Y') then
	  print *,'Enter Grid line for top of soil(Nzsoil) other than Nz'
	  
	  do while (nzsoil==0 .or. nzsoil>=nz)
		read *, nzsoil
	  end do
	
      else
	  nzsoil=nz
	endif


c --new fend	-------

	print *, 'Would you like to assign a Mono-pole Antenna?'
 	
      do 	while(frantenna/='y' .and. frantenna/='Y' .and. 
     $ frantenna/='n' .and. frantenna/='N')
		read *,frantenna
	end do


	if (frantenna/='y' .and. frantenna/='Y') then
		print *, 'Would you like to assign a Dipole Antenna?'
 		
		do	while(frantenna2/='y' .and. frantenna2/='Y' .and. 
     $		frantenna2/='n' .and. frantenna2/='N')
			read *,frantenna2
		end do

	endif



	print *, 'Would you like to print out Magnetic field values?'
	
      do while(frmagn/='y' .and. frmagn/='Y' .and.
     $ frmagn/='n' .and. frmagn/='N')
   		read *,frmagn
	end do 


c   Input the coordinates of the transmitter
c --------- new f ----------------------------
		feps3=2.1
	if (frantenna=='y' .or. frantenna=='Y') then

		print *,'Give (x,y) of the bottom of the Core of Antenna'
      	print *,'relative to centerline of system in cm'
		read *,xbcore,ybcore ! zbcore
		ibcore=(nx+1)/2+nint(xbcore/100./delx)
		jbcore=(ny+1)/2+nint(ybcore/100./dely)
c    		kbcore=(nz+1)/2+nint(zbcore/100./delz)

		airthf=dfloat(nz-nzsoil)*delz*100.
		print *,'Thickness of AIR layer =',airthf,' in cm'
c		read *,airthf

		alak=100.*sqrt(delx**2+dely**2)/2.

		
		print *,'Give Depth of Antenna in cm'
		do while(antdepf==0.)
     			read *,antdepf
		end do		

	print *,'Radius of Antenna Core should be smaller than'
     $,alak,' cm'
	
     		print *,'Give Radius of Antenna Core in cm'
		
		do while(corerdf==0.)
			read *,corerdf
		end do


	print *,'Minimum Thickness of Antenna Dielectric and Shell >='
     $,2*alak,' cm'

	 print *,'Give Antenna Dielctric Thickness in cm, it can be thin'
		
		do while(diethf==0.)	
			read *,diethf
		end do


		
		print *,'Give Thickness of Antenna Shell in cm'
		
		do while(shellthf==0.)	
			read *,shellthf
		end do
		
		print *,'Give Depth of Exposed Dielectric in cm'

c  *****************ISOLATING SHIELD *******************************			
c		do while(expdiehf<0.)	
			read *,expdiehf
c		end do
c  *****************ISOLATING SHIELD *******************************			
c		do while(expdiehf<=0.)	
c			read *,expdiehf
c		end do
c  *****************ISOLATING SHIELD *******************************			

		feps3=2.1
		eps(3)=feps3*eps0
		sigma(3)=0

	    isource=ibcore
	    jsource=jbcore
   	    kbcore=nz-nint((airthf+antdepf)/100./delz)
		ksource=nz-2

c		call buildant


	elseif (frantenna2=='y' .or. frantenna2=='Y') then

		print *,'Give (x,y) of the bottom of the Core of Antenna'
      	print *,'relative to centerline of system in cm'
		read *,xbcore,ybcore ! zbcore
        print *,'Height of Total Excitation(Ez) on Dielectric in cm'
		
		do while(lgapf==0.)
			read *,lgapf
		end do
		
		ibcore=(nx+1)/2+nint(xbcore/100./delx)
		jbcore=(ny+1)/2+nint(ybcore/100./dely)
c    		kbcore=(nz+1)/2+nint(zbcore/100./delz)

		


		airthf=dfloat(nz-nzsoil)*delz*100.
		print *,'Thickness of AIR layer =',airthf,' in cm'
c		read *,airthf

		alak=100.*sqrt(delx**2+dely**2)/2.

		
		print *,'Give Depth of Antenna in cm'
		
		do while(antdepf==0.)
			read *,antdepf
		end do

	print *,'Radius of Antenna Core should be smaller than'
     $,alak,' cm'
		print *,'Give Radius of Antenna Core in cm'
		
		do while(corerdf==0.)
			read *,corerdf
		end do


	print *,'Give Antenna Dielctric Thickness in cm, thick as:'
     $,3*alak,' cm'		
		do while(diethf==0.)
			read *,diethf
		end do

	print *,'Thickness of Antenna Shell can even be thin as:'
     $,2*alak,' cm'
		
		print *,'Give Thickness of Antenna Shell in cm'
		
		do while(shellthf==0.)
			read *,shellthf
		end do

		print *,'Give Length of Exposed part of the antenna in cm'
		
		do while(expdiehf==0.)
			read *,expdiehf
		end do

c		print *,'Give Dielectric Parameters of Antenna Dielectric'
c		print *,'Permitivity and Conductivity (Eps, SIgma)'
c		read *,feps3,sigma(3)
		feps3=2.1
		eps(3)=feps3*eps0
		sigma(3)=0

	    isource=ibcore
	    jsource=jbcore
   	    kbcore=nz-nint((airthf+antdepf)/100./delz)
		ksource=nz-2


		issf=isource
		jssf=jsource
		kssf=kbcore
		icorerd=corerdf/100./delx
		ishellth=shellthf/100./delx
		idierd=icorerd+diethf/100./delx
		iantdep=antdepf/100./delz
		iairth=airthf/100./delz
		iexpdieh=expdiehf/100./delz
		ilgap=lgapf/100./delz

		print *,'Would you like Type-1 Dipole or Type-2 Dipole (1 or 2)?'

	    do while(diptype/=1 .and. diptype/=2)
			read *,diptype
		end do


	else

	print *, 'Would you like to assign a Source other than center?'
 	
      do while(fansourc/='y' .and. fansourc/='Y' .and.
     $ fansourc/='n' .and. fansourc/='Y')
		read *,fansourc
	end do

	 if (fansourc=='y' .or. fansourc=='Y') then

     	  if (f23d==3) then

		print *,'Give (x,y,z) of the source'
      	print *,'relative to centerline of system in cm'

		read *,xsource,ysource,zsource

		isource=(nx+1)/2+nint(xsource/100./delx)
		jsource=(ny+1)/2+nint(ysource/100./dely)
     		ksource=(nz+1)/2+nint(zsource/100./delz)

		print *,'(x,y,z) of source = ',isource,jsource,ksource

     	   elseif (f23d==2) then

		print *,'Give (y,z) of the source'
     		print *,'Relative to centerline of system in cm'
		
		read *, ysource,zsource
       
		isource=(nx+1)/2
		jsource=(ny+1)/2+nint(ysource/100/dely)
     		ksource=(nz+1)/2+nint(zsource/100/delz)
		print *,'(y,z) of source =',jsource,ksource

	   endif

	  else

	      isource=(nx+1)/2
	      jsource=(ny+1)/2
   	      ksource=(nz+1)/2
		  print *,'(x,y,z) of source = ',isource,jsource,ksource

	  endif
	
	endif
	

c	    Input the coordinates of the receiver

c	    print *, 'Would you like to assign a receiver?'
c    	read *,fansrec
	print *, 'Would you like to assign a receiver other than center?'
	
	do while(fansrec/='y' .and. fansrec/='Y' .and.
     $ fansrec/='n' .and. fansrec/='Y')
 		read *,fansrec
	end do



	if (fansrec=='y' .or. fansrec=='Y') then

	  if (f23d==3) then 


c ************* Output at a dipole areceiver at the surface **************
		print *,'Do you like to have a Dipole antenna on the surface'

			do 	while(fansdiprec/='y' .and. fansdiprec/='Y' .and. 
     $	 fansdiprec/='n' .and. fansdiprec/='N')
			read *,fansdiprec
			end do
	
     	   if (fansdiprec/='n' .and. fansdiprec/='N') then

		print *, 'What is the length of Dipole on each side (in cm)?'
		
	    do while(reclen==0)
			read *,reclen
		end do

     		print *,'Give (x,y) of the observation point'
          print *,'Relative to centerline of system in cm'
		read *,xreceiv,yreceiv

	    ireceiv=(nx+1)/2+nint(xreceiv/100./delx)
	    jreceiv=(ny+1)/2+nint(yreceiv/100./dely)
     		kreceiv=nzsoil+3

		print *,'Node (x,y,z) of receiver =',ireceiv,jreceiv,kreceiv
c ************************************************************************	   

	   else
	
     		print *,'Give (x,y,z) of the observation point'
          print *,'Relative to centerline of system in cm'
		read *,xreceiv,yreceiv,zreceiv

	    ireceiv=(nx+1)/2+nint(xreceiv/100./delx)
	    jreceiv=(ny+1)/2+nint(yreceiv/100./dely)
     		kreceiv=(nz+1)/2+nint(zreceiv/100./delz)

		print *,'Node (x,y,z) of receiver =',ireceiv,jreceiv,kreceiv
	   
	   endif

	  elseif (f23d==2) then

	    print *,'Give (y,z) of the observation point'
     		print *,'Relative to centerline of system in cm'
		read *,yreceiv,zreceiv

	    ireceiv=(nx+1)/2
		jreceiv=(ny+1)/2+nint(yreceiv/100./dely)
     		kreceiv=(nz+1)/2+nint(zreceiv/100./delz)

		print *,'Node (y,z) of receiver =',jreceiv,kreceiv

	  endif


	

	else

	    ireceiv=(nx+1)/2
	    jreceiv=(ny+1)/2
     		kreceiv=(nz+1)/2
		print *,'(x,y,z) of receiver =',ireceiv,jreceiv,kreceiv

	endif
c ------- new fend ------------------------------



	 print *,'Would you like to print receiver result in output file'
	
       do while(fansrecp/='y' .and. fansrecp/='Y' .and.
     $  fansrecp/='n' .and. fansrecp/='Y')
		read *,fansrecp
	end do 

	 if (fansrecp=='y' .or. fansrecp=='Y') then

		print *,'Give name of output file'
		read *,foutput



	 endif








c---------------------------- 
      CALL ZERO

C     BUILD SOIL

c -----new f --------------------
		print *,'Number of soil layers (up to 1)='
		
		do while(nsoillayf==0)
			read *,nsoillayf
		end do

	 do 443 i=1, nsoillayf
c	  if (nsoillayf>1) then
	print *,'Thickness of layer',i,'   from bottom (out of',nzsoil,')'

 4429		read *,hsoilf(i)

	if (hsoilf(i)==0. .or. hsoilf(i)>nzsoil) then
		print *,' Layer Thickness should be less than',nzsoil
		goto 4429
	endif


c	  endif
		print *,'	f=1.5GHz										'
		print *,'   (1=Lossy Puertorican Soil at: 20 ps)'
		print *,'	(2=Lossy Puertorican Soil at: 10 ps)'
		print *,'	(3=Lossy Bosnian Soil at: 20 ps)'
		print *,'	(4=Lossy Bosnian Soil at: 100 ps)'
		print *,'	(5=Lossy Bosnian Soil at: 2 ps)'
		print *,'	f=100MHz										'		
		print *,'	(6= Water at: 50 ps)'
		print *,'													'
		print *,'	(7= Bosnian Soil at: 50 ps Moisture content=2.5%)'
		print *,'	(8= Bosnian Soil at: 50 ps Moisture content=5%)'
		print *,'	(9= Bosnian Soil at: 50 ps Moisture content=10%)'
		print *,'	(10=Bosnian Soil at: 50 ps Moisture content=20%)'
		print *,'													'
		print *,'	f=1.3GHz										'
		print *,'	(11=Sandy Soil at: 2 ps Moisture content=4%)'
		print *,'	(12=Sandy Soil at: 2 ps Moisture content=17%)'
		print *,'													'
		print *,'	(13=Sandy Soil at: 20 ps Moisture content=4%)'
		print *,'	(14=Sandy Soil at: 20 ps Moisture content=17%)'
		print *,'													'
		print *,'	(15=Sandy Soil at: 6 ps Moisture content=17%)'
		print *,'	(16=Dielectric)'		
		do while(sltypef(i)==0 .or. sltypef(i)>16)	!type+19
			read *,sltypef(i)
		end do

c ------ for 1 layer -------
c		sltype=sltypef(i)
c --------------------------
 443	 continue

	print *, ' Do you have any region other than layers?'
	
      do while(fsoilreg/='y' .and. fsoilreg/='Y' .and.
     $  fsoilreg/='n' .and. fsoilreg/='Y')
 		read *, fsoilreg
	end do


	nsoilreg =0
 	if (fsoilreg=='y' .or. fsoilreg=='Y') then

	  nsoilreg =1
	  print*,'Coordinates of center of the region
     $	   relative to center point(x,y,x in cm)'
    	  read*,xcenreg,ycenreg,zcenreg

	  print*,' Size of the region (L,W,H in cm)'
	  
	  do while(xlenreg==0. .or. ylenreg==0. .or. zlenreg==0.)
		read*,xlenreg,ylenreg,zlenreg
	  end do

		print *,'	f=1.5GHz										'
		print *,'   (1=Lossy Puertorican Soil at: 20 ps)'
		print *,'	(2=Lossy Puertorican Soil at: 10 ps)'
		print *,'	(3=Lossy Bosnian Soil at: 20 ps)'
		print *,'	(4=Lossy Bosnian Soil at: 100 ps)'
		print *,'	(5=Lossy Bosnian Soil at: 2 ps)'
		print *,'	f=100MHz										'		
		print *,'	(6= Water at: 50 ps)'
		print *,'													'
		print *,'	(7= Bosnian Soil at: 50 ps Moisture content=2.5%)'
		print *,'	(8= Bosnian Soil at: 50 ps Moisture content=5%)'
		print *,'	(9= Bosnian Soil at: 50 ps Moisture content=10%)'
		print *,'	(10=Bosnian Soil at: 50 ps Moisture content=20%)'
		print *,'													'
		print *,'	f=1.3GHz										'
		print *,'	(11=Sandy Soil at: 2 ps Moisture content=4%)'
		print *,'	(12=Sandy Soil at: 2 ps Moisture content=17%)'
		print *,'													'
		print *,'	(13=Sandy Soil at: 20 ps Moisture content=4%)'
		print *,'	(14=Sandy Soil at: 20 ps Moisture content=17%)'
		print *,'													'
		print *,'	(15=Sandy Soil at: 6 ps Moisture content=17%)'	!type+19
		print *,'	(16=Dielectric)'	!type+19
			
	 do while(sltypef(nsoillayf+1)==0 .or. sltypef(nsoillayf+1)>16) !type+19
     		read *,sltypef(nsoillayf+1)
	 end do


		sc1reg=(nx+1)/2+nint(xcenreg/100/delx)
		sc2reg=(ny+1)/2+nint(ycenreg/100/dely)
		sc3reg=(nz+1)/2+nint(zcenreg/100/delz)



	    regl=xlenreg/100/delx
		regw=ylenreg/100/dely
		regh=zlenreg/100/delz


	endif

	
c -------new f------
	
		if (sltypef(1)==1) then
			freqf=1e9
		elseif (sltypef(1)==2) then
			freqf=2e9
		elseif (sltypef(1)==3) then
			freqf=1e9
		elseif (sltypef(1)==4) then
			freqf=0.2e9
		elseif (sltypef(1)==5) then
			freqf=1.5e9
		elseif (sltypef(1)==6) then 
			freqf=0.2e9
		elseif (sltypef(1)>6.and.sltypef(1)<=10) then !type+19
			freqf=0.1e9
		elseif (sltypef(1)>=11 .and. sltypef(1)<=15) then 
			freqf=1.3e9
  		elseif (sltypef(1)==16) then 
			freqf=1.3e9
		endif
		
		print *,'Enter the Frequency='

		read *,freqf
		do while(freqf==0.)
			read *,freqf
		end do
			
		print *,'Soil type=',sltypef(1),'	===>	 Freq=',freqf

c	  if (frantenna=='y' .or. frantenna=='Y') then
	print *, ' Would you like a Narrow width Gaussian only?'
	
      do while(ansgauss/='y' .and. ansgauss/='Y' .and.
     $  ansgauss/='n' .and. ansgauss/='Y')
 		read *, ansgauss
	end do

	   if (ansgauss =='y' .or. ansgauss=='Y')then		
		print *,'Enter Width of Gaussian pulse (>=1/fmax of RF)'
		read *, gaussw
		ngauss=0.05/delz
	   endif

		print *,'Enter Gaussian Peak time: t0 (>4*GaussW) in Sec.'
		read *, tzf
		if (tzf==0) then
			tzf=7.5/freqf
		endif
		print *,'T0=',tzf 
c	  endif




	 feps=2.3
	 sigma(2)=0.00000056
	 feps3=2.1
	 sigma(3)=0.0

c	 print *,'Relative Permittivity of dielectric(default=2.9)=?'
c	 read *,feps
c	 print *,'Conductivity of dielectric(default=0.00005)=?'
c	 read *,sigma(2)

c ------new fend -----

		
c ------new f -------------------------------



		epsprimef(29)=EPS16prime				!type+19
		epsprimef(28)=EPS15prime				!type+19
		epsprimef(27)=EPS14prime			 
		epsprimef(26)=EPS13prime				 
		epsprimef(25)=EPS12prime			 
		epsprimef(24)=EPS11prime				 
		epsprimef(23)=EPS10prime				 
		epsprimef(22)=EPS9prime				
		epsprimef(21)=EPS8prime				
		epsprimef(20)=EPS7prime				
		epsprimef(19)=EPS6prime				
		epsprimef(18)=EPS5prime
		epsprimef(17)=EPS4prime
		epsprimef(16)=EPS3prime
 		epsprimef(15)=EPS2prime
 		epsprimef(14)=EPSprime


c ******* Defining Soil Type and Water BEGIN ********************************************************

c ******* Defining Soil zones *************************************

       do 222 I = 1, nx
         do 333 J = 1, ny
            do 444 K = 1, nzsoil


	        if (nsoillayf==1) then

				IDONE(I,J,K) = sltypef(1)+13
				IDONE14(I,J,K) = sltypef(1)+13
				IDTWO(I,J,K) = sltypef(1)+13
				IDTHRE(I,J,K)= sltypef(1)+13
c				epsprimef(idone14(i,j,k))=epsprime

	        elseif (nsoillayf==2) then

			  if (k<=nint(hsoilf(1))) then

				IDONE(I,J,K) = sltypef(1)+13
				IDONE14(I,J,K) = sltypef(1)+13
				IDTWO(I,J,K) = sltypef(1)+13
				IDTHRE(I,J,K)= sltypef(1)+13

			  elseif (k>nint(hsoilf(1))) then

				IDONE(I,J,K) = sltypef(2)+13
				IDONE14(I,J,K) = sltypef(2)+13
				IDTWO(I,J,K) = sltypef(2)+13
				IDTHRE(I,J,K)= sltypef(2)+13

			  endif

	        elseif (nsoillayf==3) then

			  if (k<=nint(hsoilf(1))) then
				
				IDONE(I,J,K) = sltypef(1)+13
				IDONE14(I,J,K) = sltypef(1)+13
				IDTWO(I,J,K) = sltypef(1)+13
				IDTHRE(I,J,K)= sltypef(1)+13

			  elseif (k>nint(hsoilf(1)) .and.
     $		   k<=(nint(hsoilf(1))+nint(hsoilf(2)))) then

				IDONE(I,J,K) = sltypef(2)+13
				IDONE14(I,J,K) = sltypef(2)+13
				IDTWO(I,J,K) = sltypef(2)+13
				IDTHRE(I,J,K)= sltypef(2)+13

			  elseif (k>(nint(hsoilf(1))
     $		   +nint(hsoilf(2)))) then
				

				IDONE(I,J,K) = sltypef(3)+13
				IDONE14(I,J,K) = sltypef(3)+13
				IDTWO(I,J,K) = sltypef(3)+13
				IDTHRE(I,J,K)= sltypef(3)+13

			  endif

			endif

c ******* Defining Soil Type *********************************



c ************ Region in soil Begin *****************
 	if (fsoilreg=='y' .or. fsoilreg=='Y') then
		if (i>=(sc1reg-regl/2).and.i<=(sc1reg+regl/2).and.
     $	j>=(sc2reg-regw/2).and.j<=(sc2reg+regw/2).and.
     $	k>=(sc3reg-regh/2).and.k<=(sc3reg+regh/2)) then
c ***** Another way of dcube(i,j,k,10,0,0,(sltypef(nsoillayf+1)+13)) ******
			IDONE(I,J,K) = sltypef(nsoillayf+1)+13
			IDONE14(I,J,K) = sltypef(nsoillayf+1)+13
			IDTWO(I,J,K) = sltypef(nsoillayf+1)+13
			IDTHRE(I,J,K)= sltypef(nsoillayf+1)+13
c			print *, idone(i,j,k),idone14(i,j,k)
c			print *, idtwo(i,j,k),idthre(i,j,k)
		endif
	endif
c ************ Region in soil End *****************






c ************ Region underneath Dipoe and in borehole (Water too)*******Ahai**********	
      if (frantenna2=='y' .or. frantenna2=='Y') then


	  r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)

	  if (k>=kssf .and. k<(kssf+iexpdieh-ilgap)) then
     		  if (r<=(idierd+ishellth)) then
c			IDONE(I,J,K) = 19
c			IDONE14(I,J,K) = 19
c			IDTWO(I,J,K) = 19
c			IDTHRE(I,J,K)= 19
c			if (i>(nx+1)/2 .and. j>(ny+1)/2) then
c				IDONE(I,J+1,K) = 19
c				IDONE14(I,J+1,K) = 19
c				IDTWO(I,J+1,K) = 19
c				IDTHRE(I,J+1,K)= 19
				IDONE(I+1,J,K) = 19		! pay attention, i+1 shift, is because
				IDONE14(I+1,J,K) = 19	! of the shift in excitation of dipole
				IDTWO(I+1,J,K) = 19
				IDTHRE(I+1,J,K)= 19
c				IDONE(I+1,J+1,K) = 19
c				IDONE14(I+1,J+1,K) = 19
c				IDTWO(I+1,J+1,K) = 19
c				IDTHRE(I+1,J+1,K)= 19
c			endif
		  endif	
     	  endif

	  if (k==(kssf+iexpdieh-ilgap/2))then
     		  if (r<=idierd+ishellth) then
c			IDONE(I,J,K) = 19
c			IDONE14(I,J,K) = 19
c			IDTWO(I,J,K) = 19
c			IDTHRE(I,J,K)= 19
cc			if (i>(nx+1)/2 .and. j>(ny+1)/2) then
cc				IDONE(I,J+1,K) = 19
cc				IDONE14(I,J+1,K) = 19
cc				IDTWO(I,J+1,K) = 19
cc				IDTHRE(I,J+1,K)= 19
				IDONE(I+1,J,K) = 19
				IDONE14(I+1,J,K) = 19
				IDTWO(I+1,J,K) = 19
				IDTHRE(I+1,J,K)= 19
c				IDONE(I+1,J+1,K) = 19
cc				IDONE14(I+1,J+1,K) = 19
cc				IDTWO(I+1,J+1,K) = 19
cc				IDTHRE(I+1,J+1,K)= 19
cc			endif
		  endif	
    	  endif

c ***** Water above Dipole type 2 in Borehole Begin *****************
       if (diptype ==2) then

		iantdep=antdepf/100./delz
		iairth=airthf/100./delz
		iexpdieh=expdiehf/100./delz
		ilgap=lgapf/100./delz


	  if (k>(kssf+iexpdieh) .and. k<=(kssf+iantdep)) then
     		  if (r<=(idierd+ishellth)) then
c			IDONE(I,J,K) = 19
c			IDONE14(I,J,K) = 19
c			IDTWO(I,J,K) = 19
c			IDTHRE(I,J,K)= 19
c			if (i>(nx+1)/2 .and. j>(ny+1)/2) then
c				IDONE(I,J+1,K) = 19
c				IDONE14(I,J+1,K) = 19
c				IDTWO(I,J+1,K) = 19
c				IDTHRE(I,J+1,K)= 19
				IDONE(I+1,J,K) = 19		! pay attention, i+1 shift, is because
				IDONE14(I+1,J,K) = 19	! of the shift in excitation of dipole
				IDTWO(I+1,J,K) = 19
				IDTHRE(I+1,J,K)= 19
c				IDONE(I+1,J+1,K) = 19
c				IDONE14(I+1,J+1,K) = 19
c				IDTWO(I+1,J+1,K) = 19
c				IDTHRE(I+1,J+1,K)= 19
c			endif
		  endif	
     	  endif

	 endif

c ***** Water above Dipole type 2 in Borehole End *****************


	endif



c ******* Defining Soil Type and Water END ********************************************************
c        ********************************


			  
 444       Continue
 333	  Continue
 222	 Continue



c ------new f end -------------------------------




C     BUILD BURIED OBJECT
c ------new f-----------------


	print *,'Would you like to have object and and build its shape'
	 
	 do while(fansobj/='y' .and. fansobj/='Y' .and. 
     $  fansobj/='n' .and. fansobj/='N')
		read *,fansobj
	 end do



	print *,'Would you like to have a receiver at object location'
	 
	 do while(fansobjrec/='y' .and. fansobjrec/='Y' .and. 
     $  fansobjrec/='n' .and. fansobjrec/='N')
		read *,fansobjrec
	 end do





c	if ((fansobjrec=='y' .or. fansobjrec=='Y').and.
c     $ fansobj=='y' .or. fansobj=='Y') then
      if (fansobj=='y' .or. fansobj=='Y') then
		print *,'Give (x,y,z) of DNAPL Center in cm'
		print *,'Relative to centerline of grid'
		read *,xobject,yobject,zobject
		sc1=(nx+1)/2+nint(xobject/100/delx)
		sc2=(ny+1)/2+nint(yobject/100/dely)
		sc3=(nz+1)/2+nint(zobject/100/delz)
	endif

	if (fansobj=='y' .or. fansobj=='Y') then

c		feps=2.3
c		sigma(2)=.00005

	print *,'DNAPL Relative permittivity=2.3 & conductivity=.00000056'
	print *,'Would you like to change dielectric parameters?'
	
       do while(fansparf/='y' .and. fansparf/='Y' .and. 
     $    fansparf/='n' .and. fansparf/='N')
		read *,fansparf
	 end do


	 if (fansparf=='y' .or. fansparf=='Y') then
		print *,'Relative Permittivity of dielectric(default=2.3)=?'
	
     		do while(feps==0.)
			read *,feps
		end do

		print *,'Conductivity of dielectric(default=0.00000056)=?'
		
		read *,sigmaff
		
	 endif

c	ss=sigma(2)
	 CALL BUILD

	endif


	if (frantenna=='y' .or. frantenna=='Y') then

		call buildant

	elseif (frantenna2=='y' .or. frantenna2=='Y') then

		call buildant2


	endif


	if (fansdiprec/='n' .and. fansdiprec/='N') then
		call buildantrec
	endif


C     ROUTINE FOR INITIALIZING CERTAIN PARAMETERS OF THE PROBLEM 
       CALL SETUP


C*****************************************************************
C     MAIN LOOP FOR FIELD COMPUTATIONS AND DATA SAVING
C*****************************************************************

c      T=0.0

	 

	 iii=0
c----------------------------------------------------
c    Store input file of the excitation pulse 

c	open(unit=15,file='field3.dat',status='unknown')
c	do 66 itemp=1,len_pulser
c66	read(15,*) pulse(itemp)
c	close(unit=15)

c	open(unit=15,file='field.dat',status='unknown')
c	do 77 itemp=1,len_pulser
c77	read(15,*) (pulse_y(itemp,jj),jj=1,38)
c	close(unit=15)

c	open(unit=16,file='field2.dat',status='unknown')
c	do 88 itemp=1,len_pulser
c88	read(16,*) (pulse_x(itemp,ii),ii=1,38)
c	close(unit=16)

c  ----- new f -------
c   Ask for name of input file
c	if (fansrecp=='y' .or. fansrecp=='Y') then
c		print *,'Give name of output file'
c		read *,foutput
c	endif
c  ---- new fend -----



       DO 100 N=1,NSTOP
		print *, 'Time step', N

c   came from bottom

c    ------ EXCITATION---------------
c for 2D &3D both

		temp=0.0
c		arg =dfloat(N-200)/50.0

c	if (frantenna=='y' .or. frantenna=='Y'			!ABout changing the excitation for
c     $      .or. frantenna2=='y' .or. frantenna2=='Y') then   ! DIPOLE antenna
	if (frantenna=='y' .or. frantenna=='Y') then

	   if (ansgauss =='y' .or. ansgauss=='Y')then
		temp =EXP(-((dfloat(n)*dt-tzf)/gaussw)**2)
     	   else
		temp =EXP(-((dfloat(n)*dt-tzf)*freqf)**2)
     $	*COS(2.*3.141515926536*freqf*dfloat(n)*dt)
	   endif 
	    	
	else

	   if (ansgauss =='y' .or. ansgauss=='Y')then
		temp =EXP(-((dfloat(n)*dt-tzf)/gaussw)**2)
     	   else
		arg =dfloat(N-200)/50.0
c    self-created modulated pulse for 2D and 3D
c		arg =dfloat(N-200)/50.0
		temp =EXP(-arg**2)*COS(2.*3.1415926536*freqf*dfloat(N)*DT)

		if (ABS(N-200).GT.200) then 
		  temp=0.0 
		endif
	   endif

	endif









C    ADVANCE SCATTERED MAGNETIC FIELD
		CALL HXSFLD
		CALL HYSFLD
		CALL HZSFLD
 
C    ADVANCE TIME BY 1/2 TIME STEP
c      T=T+DT/2.

C    ADVANCE SCATTERED ELECTRIC FIELD
		CALL EXSFLD
		CALL EYSFLD
		CALL EZSFLD

C    APPLY RADIATION BC (SECOND ORDER)
c --------new f-------------
		if (f23d==3) then
			CALL RADEYX
			CALL RADEZX
			CALL RADEZY
		endif

		CALL RADEXY
		CALL RADEXZ

		if (f23d==3) then
			CALL RADEYZ
		endif

c ******* move to the top
c    BOUNDARY CONDITION FOR METALLIC PLATES

		if (f23d==2) then
			CALL METAL
		endif

c -----new f---------------

	if (frantenna=='y' .or. frantenna=='Y') then

c	***********	Excitation of MonoPole ****************
	 do 109 jjf=1,ny
	  do 108 iif=1,nx

c		print *,'Excitation Core Point(i,j,k)=('
c     $,nint(corei(iff)),',',nint(corej(iff))
c     $,',',nint(corek(iff)),')'

		if (idiek(iif,jjf)==11) then


c ***** to try with new DCUBE 1113 ******************************
			fltetax=sqrt((dfloat(iif)-dfloat(isource))**2
     $    	+(dfloat(jjf)-(dfloat(jsource)))**2)
			fltetay=sqrt((dfloat(iif)-(dfloat(isource)))**2
     $    	+(dfloat(jjf)-dfloat(jsource))**2)

c			fltetax=sqrt((dfloat(iif)-dfloat(isource))**2
c     $    	+(dfloat(jjf)-(0.5+dfloat(jsource)))**2)
c			fltetay=sqrt((dfloat(iif)-(0.5+dfloat(isource)))**2
c     $    	+(dfloat(jjf)-dfloat(jsource))**2)
c ***** to try with new DCUBE 1113 ******************************



			costeta=(dfloat(iif)-dfloat(isource))/fltetax
			sinteta=(dfloat(jjf)-dfloat(jsource))/fltetay
c			costeta=(dfloat(iif)-(0.5+dfloat(isource)))/fltetax
c			sinteta=(dfloat(jjf)-(0.5+dfloat(jsource)))/fltetay


	   if (ansgauss =='y' .or. ansgauss=='Y')then
	   		exs(iif,jjf,ksource)=costeta*temp
			eys(iif,jjf,ksource)=sinteta*temp
	   else
			exs(iif,jjf,ksource)=costeta*temp+exs(iif,jjf,ksource)
			eys(iif,jjf,ksource)=sinteta*temp+eys(iif,jjf,ksource)
	   endif


		  if (n>=40.and.n<41) then
			print *,'exs(',iif,jjf,ksource,')=',exs(iif,jjf,ksource) 
			print *,'eys(',iif,jjf,ksource,')=',eys(iif,jjf,ksource) 
		  endif

		endif

108	  continue
109	 continue

	elseif (frantenna2=='y' .or. frantenna2=='Y') then

c	***********	Excitation of DIPOLE****************
	do 1091 kkf=1,nz
	 do 1081 iif=1,nx
	  do 1071 jjf=1,ny



	   if (idiek(iif,jjf)==11) then
			
		  iflz10=int(kkf-(kbcore+iexpdieh-ilgap))

		cz1=1.0/SQRT(XMU0*EPS0*eps(3)) ! epsprimef(idone(iif,jjf,kkf))
c		print *,'idone', idone(iif,jjf,kkf)

		if (ansgauss =='y' .or. ansgauss=='Y')then
		 temp =EXP(-(((dfloat(kkf-(kbcore+iexpdieh-ilgap/2))*delz)/cz1
     $      -dfloat(N)*DT-tzf)/gaussw)**2)  
		else
			temp =COS(2.*3.1415926536*freqf*
     $	     (dfloat(kkf-(kbcore+iexpdieh-ilgap/2))*delz)/cz1
     $	      -dfloat(N)*DT)*EXP(-((dfloat(n)*dt-tzf)*freqf)**2)  
		endif
			

c		temp =EXP(-((dfloat(n)*dt-tzf)*freqf)**2)
c     $	*COS(2.*3.141515926536*freqf*dfloat(n)*dt
c     $    -dfloat(kkf-(kbcore+iexpdieh-ilgap/2))*delz)


		if (iflz10>ilgap) then			
		
			ratiozz=0.
		
		elseif (iflz10==ilgap) then
		
			ratiozz=0.
			if (ansgauss =='y' .or. ansgauss=='Y')then
				ezs(iif,jjf,kkf)=ratiozz*temp
			else
				ezs(iif,jjf,kkf)=ezs(iif,jjf,kkf)+ratiozz*temp
			endif
			
		elseif (iflz10>(ilgap/2) .and. iflz10<ilgap) then
		
			ratiozz=dfloat(-iflz10+ilgap)/(ilgap/2)
			if (ansgauss =='y' .or. ansgauss=='Y')then
				ezs(iif,jjf,kkf)=ratiozz*temp
			else
				ezs(iif,jjf,kkf)=ezs(iif,jjf,kkf)+ratiozz*temp
			endif

		elseif (iflz10==(ilgap/2)) then

				ratiozz=0

		elseif(iflz10>0 .and. iflz10<(ilgap/2)) then
				
			ratiozz=dfloat(iflz10)/(ilgap/2)
			if (ansgauss =='y' .or. ansgauss=='Y')then
				ezs(iif,jjf,kkf)=ratiozz*temp
			else
				ezs(iif,jjf,kkf)=ezs(iif,jjf,kkf)+ratiozz*temp
			endif

		elseif(int(iflz10)==0) then
		
			ratiozz=0.
			if (ansgauss =='y' .or. ansgauss=='Y')then
				ezs(iif,jjf,kkf)=ratiozz*temp
			else
				ezs(iif,jjf,kkf)=ezs(iif,jjf,kkf)+ratiozz*temp
			endif

		elseif (iflz10<0.) then											
	
     				ratiozz=0.

		else

				print *, ' There is an error in ratio'

		endif


    	

 		if (n==25 .and. kkf==18) then
			print *,'ezs(',iif,jjf,kkf,')=',ezs(iif,jjf,kkf) 
		endif

			
	   endif

1071	  continue
1081	 continue
1091	continue



	else

	 if (f23d==2) then
         do 111 i  = 1,nx
           EXS(i,jsource,ksource)=temp 
     $     +EXS(i,jsource,ksource)
111      continue
	 else
		do 112 i=isource-1, isource+1
			do 113 j=jsource-1, jsource+1
				do 114 k=ksource-1, ksource+1
					EXS(i,j,k)=temp+EXS(i,j,k)
c		EXS(isource,jsource,ksource)=temp+EXS(isource,jsource,ksource)
 114				continue
 113			continue
 112		continue
	 endif

	endif



c------new f ------------------------------------------------------
C    SOME SAVING ROUTING
c      print *,'Would you like to print receiver result in output file'
c      read *,fansrecp







c *** Print out an output file at a point over time *********************************

      if (fansrecp=='y' .or. fansrecp=='Y') then
		OPEN(UNIT=6,FILE=foutput,
     $	ACCESS='sequential',STATUS='unknown')

	if (ansgauss=='y' .or. ansgauss=='Y') then	

       if (frantenna=='y' .or. frantenna=='Y' .or. 
     $  frantenna2=='y' .or. frantenna2=='Y') then

	 write(6,17) (EXS(isource+1,jsource+1,ksource)),(EYS(isource+1,
     $ jsource+1,ksource)),(EZS(isource+1,jsource+1,ksource)),
     $ (EXS(isource+1,jsource+1,ksource-1)),(EYS(isource+1,
     $ jsource+1,ksource-1)),(EZS(isource+1,jsource+1,ksource-1)),
     $ (EXS(isource+1,jsource+1,ksource-2)),(EYS(isource+1,
     $ jsource+1,ksource-2)),(EZS(isource+1,jsource+1,ksource-2)),
     $ (EXS(isource+1,jsource+1,ksource-3)),(EYS(isource+1,
     $ jsource+1,ksource-3)),(EZS(isource+1,jsource+1,ksource-3)),
     $ (EXS(isource+1,jsource+1,ksource-4)),(EYS(isource+1,
     $ jsource+1,ksource-4)),(EZS(isource+1,jsource+1,ksource-4)),
     $ (EXS(isource+1,jsource+1,ksource-5)),(EYS(isource+1,
     $ jsource+1,ksource-5)),(EZS(isource+1,jsource+1,ksource-5)),
     $ (EXS(isource+1,jsource+1,ksource-6)),(EYS(isource+1,
     $ jsource+1,ksource-6)),(EZS(isource+1,jsource+1,ksource-6)),
     $ (EXS(isource+1,jsource+1,ksource-7)),(EYS(isource+1,
     $ jsource+1,ksource-7)),(EZS(isource+1,jsource+1,ksource-7)),
     $ (EXS(isource+1,jsource+1,ksource-8)),(EYS(isource+1,
     $ jsource+1,ksource-8)),(EZS(isource+1,jsource+1,ksource-8)),
     $ (EXS(isource+1,jsource+1,ksource-9)),(EYS(isource+1,
     $ jsource+1,ksource-9)),(EZS(isource+1,jsource+1,ksource-9)),
     $ (EXS(isource+1,jsource+1,ksource-10)),(EYS(isource+1,
     $ jsource+1,ksource-10)),(EZS(isource+1,jsource+1,ksource-10)),
     $ (EXS(isource+1,jsource+1,ksource-11)),(EYS(isource+1,
     $ jsource+1,ksource-11)),(EZS(isource+1,jsource+1,ksource-11)),
     $ (EXS(isource+1,jsource+1,ksource-12)),(EYS(isource+1,
     $ jsource+1,ksource-12)),(EZS(isource+1,jsource+1,ksource-12)),
     $ (EXS(isource+1,jsource+1,ksource-13)),(EYS(isource+1,
     $ jsource+1,ksource-13)),(EZS(isource+1,jsource+1,ksource-13)),
     $ (EXS(isource+1,jsource+1,ksource-14)),(EYS(isource+1,
     $ jsource+1,ksource-14)),(EZS(isource+1,jsource+1,ksource-14)),
     $ (EXS(isource+1,jsource+1,ksource-15)),(EYS(isource+1,
     $ jsource+1,ksource-15)),(EZS(isource+1,jsource+1,ksource-15)),
     $ (EXS(isource+1,jsource+1,ksource-16)),(EYS(isource+1,
     $ jsource+1,ksource-16)),(EZS(isource+1,jsource+1,ksource-16)),
     $ (EXS(isource+1,jsource+1,ksource-17)),(EYS(isource+1,
     $ jsource+1,ksource-17)),(EZS(isource+1,jsource+1,ksource-17)),
     $ (EXS(isource+1,jsource+1,ksource-18)),(EYS(isource+1,
     $ jsource+1,ksource-18)),(EZS(isource+1,jsource+1,ksource-18)),
     $ (EXS(isource+1,jsource+1,ksource-19)),(EYS(isource+1,
     $ jsource+1,ksource-19)),(EZS(isource+1,jsource+1,ksource-19)),


     $ (EXS(isource+1,jsource+1,nzsoil+1)),(EYS(isource+1,
     $ jsource+1,nzsoil+1)),(EZS(isource+1,jsource+1,nzsoil+1)),
     $ (EXS(isource+1,jsource+1,(kbcore+ksource)/2)),(EYS(isource+1,
     $ jsource+1,(kbcore+ksource)/2)),(EZS(isource+1,jsource+1,(kbcore
     $ +ksource)/2)),
     $ (EXS(isource+1,jsource+1,kbcore+iexpdieh+1)),(EYS(isource+1,
     $ jsource+1,kbcore+iexpdieh+1)),(EZS(isource+1,jsource+1,kbcore
     $ +iexpdieh+1)),
     $ (EXS(isource+1,jsource+1,kbcore)), (EYS(isource+1,jsource
     $ +1,kbcore)),(EZS(isource+1,jsource+1,kbcore)),

     $ (EXS(ireceiv,jreceiv,kreceiv-ngauss)), (EYS(ireceiv,j
     $receiv,kreceiv-ngauss)),(EZS(ireceiv,jreceiv,kreceiv-ngauss)),(EXS
     $(ireceiv,jreceiv,kreceiv)),(EYS(ireceiv,jreceiv,kreceiv)),(EZS(ire
     $ceiv,jreceiv,kreceiv)),(EXS(ireceiv,jreceiv,kreceiv+ngauss)),(EYS(
     $ireceiv,jreceiv,kreceiv+ngauss)),(EZS(ireceiv,jreceiv,kreceiv+ngau
     $ss))
 17	  format(81(1x,f22.16))

	 print *, (EXS(isource+1,jsource+1,ksource)), (EYS(isource+1,jsou
     $  rce+1,ksource)),(EZS(isource+1,jsource+1,ksource))
	 print *, (EXS(isource+1,jsource+1,ksource-1)), (EYS(isource+1,js
     $ ource+1,ksource-1)),(EZS(isource+1,jsource+1,ksource-1))
	 print *, (EXS(isource+1,jsource+1,ksource-2)), (EYS(isource+1,js
     $ ource+1,ksource-2)),(EZS(isource+1,jsource+1,ksource-2))
	 print *, (EXS(isource+1,jsource+1,ksource-3)), (EYS(isource+1,js
     $ ource+1,ksource-3)),(EZS(isource+1,jsource+1,ksource-3))
	 print *, (EXS(isource+1,jsource+1,ksource-4)), (EYS(isource+1,js
     $ ource+1,ksource-4)),(EZS(isource+1,jsource+1,ksource-4))
	 print *, (EXS(isource+1,jsource+1,ksource-5)), (EYS(isource+1,js
     $ ource+1,ksource-5)),(EZS(isource+1,jsource+1,ksource-5))
	 print *, (EXS(isource+1,jsource+1,ksource-6)), (EYS(isource+1,js
     $ ource+1,ksource-6)),(EZS(isource+1,jsource+1,ksource-6))
	 print *, (EXS(isource+1,jsource+1,ksource-7)), (EYS(isource+1,js
     $ ource+1,ksource-7)),(EZS(isource+1,jsource+1,ksource-7))
	 print *, (EXS(isource+1,jsource+1,ksource-8)), (EYS(isource+1,js
     $ ource+1,ksource-8)),(EZS(isource+1,jsource+1,ksource-8))
	 print *, (EXS(isource+1,jsource+1,ksource-9)), (EYS(isource+1,js
     $ ource+1,ksource-9)),(EZS(isource+1,jsource+1,ksource-9))
	 print *, (EXS(isource+1,jsource+1,ksource-10)), (EYS(isource+1,js
     $ ource+1,ksource-10)),(EZS(isource+1,jsource+1,ksource-10))
	 print *, (EXS(isource+1,jsource+1,ksource-11)), (EYS(isource+1,js
     $ ource+1,ksource-11)),(EZS(isource+1,jsource+1,ksource-11))
	 print *, (EXS(isource+1,jsource+1,ksource-12)), (EYS(isource+1,js
     $ ource+1,ksource-12)),(EZS(isource+1,jsource+1,ksource-12))
	 print *, (EXS(isource+1,jsource+1,ksource-13)), (EYS(isource+1,js
     $ ource+1,ksource-13)),(EZS(isource+1,jsource+1,ksource-13))
	 print *, (EXS(isource+1,jsource+1,ksource-14)), (EYS(isource+1,js
     $ ource+1,ksource-14)),(EZS(isource+1,jsource+1,ksource-14))
	 print *, (EXS(isource+1,jsource+1,ksource-15)), (EYS(isource+1,js
     $ ource+1,ksource-15)),(EZS(isource+1,jsource+1,ksource-15))
	 print *, (EXS(isource+1,jsource+1,ksource-16)), (EYS(isource+1,js
     $ ource+1,ksource-16)),(EZS(isource+1,jsource+1,ksource-16))
	 print *, (EXS(isource+1,jsource+1,ksource-17)), (EYS(isource+1,js
     $ ource+1,ksource-17)),(EZS(isource+1,jsource+1,ksource-17))
	 print *, (EXS(isource+1,jsource+1,ksource-18)), (EYS(isource+1,js
     $ ource+1,ksource-18)),(EZS(isource+1,jsource+1,ksource-18))
	 print *, (EXS(isource+1,jsource+1,ksource-19)), (EYS(isource+1,js
     $ ource+1,ksource-19)),(EZS(isource+1,jsource+1,ksource-19))


       print *,(EXS(isource+1,jsource+1,nzsoil+1)),(EYS(isou
     $ rce+1,jsource+1,nzsoil+1)),(EZS(isource+1,jsource+1, nzsoil+1))
       print *,(EXS(isource+1,jsource+1,(kbcore+ksource)/2)),(EYS(isou
     $ rce+1,jsource+1,(kbcore+ksource)/2)),(EZS(isource+1,jsource+1,
     $ (kbcore+ksource)/2))
       print *,(EXS(isource+1,jsource+1,kbcore+iexpdieh+1)),(EYS(isource
     $ +1,jsource+1,kbcore+iexpdieh+1)),(EZS(isource+1,jsource+1,kbcore
     $ +iexpdieh+1))

       print *, (EXS(isource+1,jsource+1,kbcore)), (EYS(isource+1,js
     $  ource+1,kbcore)),(EZS(isource+1,jsource+1,kbcore))
       print *, (EXS(ireceiv,jreceiv,kreceiv-ngauss)), (EYS(ireceiv,j
     $  receiv,kreceiv-ngauss)),(EZS(ireceiv,jreceiv,kreceiv-ngauss))
       print *, (EXS
     $(ireceiv,jreceiv,kreceiv)),(EYS(ireceiv,jreceiv,kreceiv)),(EZS(ire
     $  ceiv,jreceiv,kreceiv))
       print *, (EXS(ireceiv,jreceiv,kreceiv+ngauss)),(EYS(
     $ireceiv,jreceiv,kreceiv+ngauss)),(EZS(ireceiv,jreceiv,kreceiv+ngau
     $  ss))

	 else

	write(6,1711) (EXS(isource,jsource,ksource)), (EYS(isource,jsource
     $ ,ksource)),(EZS(isource,jsource,ksource)),
     $ (EXS(ireceiv,jreceiv,kreceiv-ngauss)), (EYS(ireceiv,j
     $receiv,kreceiv-ngauss)),(EZS(ireceiv,jreceiv,kreceiv-ngauss)),(EXS
     $(ireceiv,jreceiv,kreceiv)),(EYS(ireceiv,jreceiv,kreceiv)),(EZS(ire
     $ceiv,jreceiv,kreceiv)),(EXS(ireceiv,jreceiv,kreceiv+ngauss)),(EYS(
     $ireceiv,jreceiv,kreceiv+ngauss)),(EZS(ireceiv,jreceiv,kreceiv+ngau
     $ss))
 1711	  format(12(1x,f22.16))

	 print *, (EXS(isource,jsource,ksource)), (EYS(isource,jsource
     $  ,ksource)),(EZS(isource,jsource,ksource))
       print *, (EXS(ireceiv,jreceiv,kreceiv-ngauss)), (EYS(ireceiv,j
     $  receiv,kreceiv-ngauss)),(EZS(ireceiv,jreceiv,kreceiv-ngauss))
       print *, (EXS
     $(ireceiv,jreceiv,kreceiv)),(EYS(ireceiv,jreceiv,kreceiv)),(EZS(ire
     $  ceiv,jreceiv,kreceiv))
       print *, (EXS(ireceiv,jreceiv,kreceiv+ngauss)),(EYS(
     $ireceiv,jreceiv,kreceiv+ngauss)),(EZS(ireceiv,jreceiv,kreceiv+ngau
     $  ss))

	 endif

	else

	  write(6,171) (EXS(ireceiv,jreceiv,kreceiv)), (EYS(
     $ireceiv,jreceiv,kreceiv)),(EZS(ireceiv,jreceiv,kreceiv))
  171	  format(f22.16,f22.16,f22.16)


        print *, EXS(ireceiv,jreceiv,kreceiv)
     $ ,EYS(ireceiv,jreceiv,kreceiv),EZS(ireceiv,jreceiv,kreceiv)
 
      endif
	
      endif

c **************************************************************************




c ------new fend------


c      OPEN(UNIT=7,FILE='pulse.dat',ACCESS='sequential',STATUS='unknown')
c	write(7,18)(pulse(i,1),i=1,25)
c18    format(25(1x,f22.16))

       IF(MOD(N,ntime) .NE. 0 )GOTO 100
       i2 =  MOD(iii,10)
       i1 =  MOD( (iii-i2)/10, 10 )
       i0 =  MOD( (iii-i1*10-i2)/100, 10)
c	 print *,'Enter output-file name for slice excitation' 
c	 read *,fanmef
c       fname=fnamef//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'


	if (frantenna=='y' .or. frantenna=='Y'
     $    .or. frantenna2=='y' .or. frantenna2=='Y') then
	  fnamevx='mbdvx'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  fnamevy='mbdvy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  fnamevz='mbdvz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	 fnamevxz='mbdvxz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  fnamehx='mbdhx'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  fnamehy='mbdhy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  fnamehz='mbdhz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	 fnamehxy='mbdhxy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	 fnamehxz='mbdhxz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  
	  if (frmagn=='y' .or. frmagn=='Y') then
	   fmagnf='magnetH'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
         fmagnvx='magnvx'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
         fmagnvy='magnvx'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
         fmagnvz='magnvz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
       fmagnvxz='magnvxz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	   fmagnhx='magnhx'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	   fmagnhy='magnhy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
         fmagnhz='magnhz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
       fmagnhxy='magnhxy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
       fmagnhxz='magnhxz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  endif


	  if (fansobjrec=='y' .or. fansobjrec=='Y') then
         fnovyy='msovyy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
         fnovyz='msovyz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
       fnovyyz='msovyyz'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	  endif






c       fnameh='ah'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
       iii = iii + 1
       print *,fnamevx !,fname2
       print *,fnamevy !,fname2
       print *,fnamevz !,fname2
       print *,fnamevxz !,fname2
       print *,fnamehx !,fname2
       print *,fnamehy !,fname2
       print *,fnamehxy !,fname2
       print *,fnamehxz !,fname2
	 if (frmagn=='y' .or. frmagn=='Y') then
	   print *,fmagnf
         print *,fmagnvx !,fname2
         print *,fmagnvy !,fname2
         print *,fmagnvz !,fname2
         print *,fmagnvxz !,fname2
         print *,fmagnhx !,fname2
         print *,fmagnhy !,fname2
         print *,fmagnhxy !,fname2
         print *,fmagnhxz !,fname2
	 endif


c       print *,fnamenvx !,fname2
	 if (fansobjrec=='y' .or. fansobjrec=='Y')then
         print *,fnovyy !,fname2
         print *,fnovyz !,fname2
         print *,fnovyyz !,fname2
c        print *,fnameovxz !,fname2
	 endif


c       print *,fnameh !,fname2

c       fname2='ed'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	else
c    3D case no Real Antenna  
       fnamevx='fvx'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	 fnamevy='fvy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	 fnamehx='fhx'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
	 fnamehy='fhy'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'
       fnameh='fh'//char(48+i0)//char(48+i1)//char(48+i2)//'.dat'       
	 iii = iii + 1
       print *,fnamevx !,fname2
	 print *,fnamevy !,fname2
       print *,fnamehx !,fname2
	 print *,fnamehy !,fname2
	 print *,fnameh !,fname2
	endif
c ------new f -----------------------
	

	  if (f23d==3) then
c		OPEN(UNIT=91,FILE=fmagnfield,STATUS='unknown')          
	  if ((frantenna=='y' .or. frantenna=='Y')
     $	 .or. (frantenna2=='y' .or. frantenna2=='Y')) then
          
		OPEN(UNIT=4,FILE=fnamevx,STATUS='unknown')
	    write(4,15)((EXS(ii,jreceiv,kk),ii=1,nx),kk=1,nz)
 15       format(129(1x,f22.16))
          CLOSE(UNIT = 4)

          OPEN(UNIT=14,FILE=fnamevz,STATUS='unknown')
	    write(14,151)((EZS(ii,jreceiv,kk),ii=1,nx),kk=1,nz)
 151      format(129(1x,f22.16))
          CLOSE(UNIT = 14)

          OPEN(UNIT=140,FILE=fnamevy,STATUS='unknown')
	    write(140,1511)((EYS(ireceiv,jj,kk),jj=1,ny),kk=1,nz)
 1511      format(129(1x,f22.16))
          CLOSE(UNIT = 140)


          OPEN(UNIT=141,FILE=fnamevxz,STATUS='unknown')
	    write(141,152)((sqrt(EXS(ii,jreceiv,kk)**2
     $ 	+EZS(ii,jreceiv,kk)**2),ii=1,nx),kk=1,nz)
 152      format(129(1x,f22.16))
          CLOSE(UNIT = 141)

          OPEN(UNIT=24,FILE=fnamehx,STATUS='unknown')
	    write(24,153)((EXS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 153       format(129(1x,f22.16))
          CLOSE(UNIT = 24)

          OPEN(UNIT=34,FILE=fnamehy,STATUS='unknown')
	    write(34,154)((EYS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 154      format(129(1x,f22.16))
          CLOSE(UNIT = 34)

    
          OPEN(UNIT=44,FILE=fnamehz,STATUS='unknown')
	    write(44,155)((EZS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 155      format(129(1x,f22.16))
          CLOSE(UNIT = 44)

          OPEN(UNIT=54,FILE=fnamehxy,STATUS='unknown')
	    write(54,156)((sqrt(EXS(ii,jj,kreceiv)**2
     $	+EYS(ii,jj,kreceiv)**2),ii=1,nx),jj=1,ny)
 156      format(129(1x,f22.16))
          CLOSE(UNIT = 54)

          OPEN(UNIT=64,FILE=fnamehxz,STATUS='unknown')
	    write(64,157)((sqrt(EXS(ii,jj,kreceiv)**2
     $	+EZS(ii,jj,kreceiv)**2),ii=1,nx),jj=1,ny)
 157      format(129(1x,f22.16))
          CLOSE(UNIT = 64)


	  if (frmagn=='y' .or. frmagn=='Y') then
		
		OPEN(UNIT=91,FILE=fmagnf,STATUS='unknown')          
		icorerd=corerdf/100./delx
	 	ishellth=shellthf/100./delx
		idierd=icorerd+diethf/100./delx
		iantdep=antdepf/100./delz
		iairth=airthf/100./delz
		iexpdieh=expdiehf/100./delz


        do 228 i = 1, nx
         do 338 J = 1, ny
           do 448 k = 1, nz


		 r=SQRT(dfloat(i-isource)**2+dfloat(j-jsource)**2)

		 if (k>=(kbcore+iexpdieh) .and. k<(kbcore+iantdep+iairth)) then

		  if (r>idierd .and. r<=(idierd+ishellth+1)) then

			write(91,*) 'i, j, k =', i,j,k
			write(91,*) '			Hx			Hy			Hz'
			write(91,*) HXS(i,j,k),HYS(i,j,k),HZS(i,j,k)


		 endif
		endif
		
	if (i==15.and.j==15) then
	print *, EXS(i,j,k),EYS(i,j,k),EZS(i,j,k)
	endif

448	    continue
338	   continue
228	  continue

	    CLOSE(UNIT = 91)

c	  endif

		OPEN(UNIT=912,FILE=fmagnvx,STATUS='unknown')
	    write(912,160)((HXS(ii,jreceiv,kk),ii=1,nx),kk=1,nz)
 160       format(129(1x,e22.16))
          CLOSE(UNIT = 912)

		OPEN(UNIT=913,FILE=fmagnvy,STATUS='unknown')
	    write(913,1602)((HYS(ii,jreceiv,kk),ii=1,nx),kk=1,nz)
 1602       format(129(1x,e22.16))
          CLOSE(UNIT = 913)



          OPEN(UNIT=914,FILE=fmagnvz,STATUS='unknown')
	    write(914,161)((HZS(ii,jreceiv,kk),ii=1,nx),kk=1,nz)
 161      format(129(1x,e22.16))
          CLOSE(UNIT = 914)

          OPEN(UNIT=915,FILE=fmagnvxz,STATUS='unknown')
	    write(915,162)((sqrt(HXS(ii,jreceiv,kk)**2
     $ 	+HZS(ii,jreceiv,kk)**2),ii=1,nx),kk=1,nz)
 162      format(129(1x,e22.16))
          CLOSE(UNIT = 915)


          OPEN(UNIT=916,FILE=fmagnhx,STATUS='unknown')
	    write(916,163)((HXS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 163       format(129(1x,e22.16))
          CLOSE(UNIT = 916)

          OPEN(UNIT=917,FILE=fmagnhy,STATUS='unknown')
	    write(917,164)((HYS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 164      format(129(1x,e22.16))
          CLOSE(UNIT = 917)

    
          OPEN(UNIT=918,FILE=fmagnhz,STATUS='unknown')
	    write(918,165)((HZS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 165      format(129(1x,e22.16))
          CLOSE(UNIT = 918)

          OPEN(UNIT=919,FILE=fmagnhxy,STATUS='unknown')
	    write(919,166)((sqrt(HXS(ii,jj,kreceiv)**2
     $	+HYS(ii,jj,kreceiv)**2),ii=1,nx),jj=1,ny)
 166      format(129(1x,e22.16))
          CLOSE(UNIT = 919)

          OPEN(UNIT=920,FILE=fmagnhxz,STATUS='unknown')
	    write(920,167)((sqrt(HXS(ii,jj,kreceiv)**2
     $	+HZS(ii,jj,kreceiv)**2),ii=1,nx),jj=1,ny)
 167      format(129(1x,e22.16))
          CLOSE(UNIT = 920)

	  endif




	  if (fansobjrec=='y' .or. fansobjrec=='Y')then

		OPEN(UNIT=74,FILE=fnovyy,STATUS='unknown')
	    write(74,158)((EYS(sc1,jj,kk),jj=1,ny),kk=1,nz)
 158       format(129(1x,f22.16))
          CLOSE(UNIT = 74)

          OPEN(UNIT=84,FILE=fnovyz,STATUS='unknown')
	    write(84,159)((EZS(sc1,jj,kk),jj=1,ny),kk=1,nz)
 159      format(129(1x,f22.16))
          CLOSE(UNIT = 84)

          OPEN(UNIT=94,FILE=fnovyyz,STATUS='unknown')
	    write(94,1601)((sqrt(EYS(sc1,jj,kk)**2
     $ 	+EZS(sc1,jj,kk)**2),jj=1,ny),kk=1,nz)
 1601     format(129(1x,f22.16))
          CLOSE(UNIT = 94)

	  endif



	 else
c    3D case no Real Antenna  
		OPEN(UNIT=4,FILE=fnamevx,STATUS='unknown')
	    write(4,355)((EXS(ireceiv,jj,kk),jj=1,ny),kk=1,nz)
 355       format(129(1x,f22.16))
          CLOSE(UNIT = 4)

		OPEN(UNIT=41,FILE=fnamevy,STATUS='unknown')
	    write(41,3551)((EYS(ii,jreceiv,kk),ii=1,nx),kk=1,nz)
 3551       format(129(1x,f22.16))
          CLOSE(UNIT = 41)



          OPEN(UNIT=24,FILE=fnamehx,STATUS='unknown')
	    write(24,356)((EXS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 356       format(129(1x,f22.16))
          CLOSE(UNIT = 24)

          OPEN(UNIT=241,FILE=fnamehy,STATUS='unknown')
	    write(241,3561)((EYS(ii,jj,kreceiv),ii=1,nx),jj=1,ny)
 3561       format(129(1x,f22.16))
          CLOSE(UNIT = 241)



          OPEN(UNIT=44,FILE=fnameh,STATUS='unknown')
	    write(44,357)((sqrt(EXS(ii,jj,kreceiv)**2
     $	+EYS(ii,jj,kreceiv)**2),ii=1,nx),jj=1,ny)
 357      format(129(1x,f22.16))
          CLOSE(UNIT = 44)

	 endif

c		alak=EXS(ireceiv,II,JJ)	
c	print *,alak
	elseif (f23d==2) then

          OPEN(UNIT=4,FILE=fnamevx,STATUS='unknown')
	    write(4,616)((EXS((nx+1)/2,II,JJ),II=1,ny),JJ=1,nz)
 616       format(59(1x,f22.16))
		CLOSE(UNIT = 4)
		alak=EXS((nx+1)/2,II,JJ)
c		print *,alak
	endif
	  



C------------------------------------------
100    CONTINUE


       CLOSE(UNIT = 6)
c	   CLOSE(UNIT = 7)
c      T=NSTOP*DT


	  if (fansrecp=='y' .or. fansrecp=='Y') then

		OPEN(UNIT=11,FILE=foutput//'.rec')

		write(11,*)'	Nx 	Ny	Nz  of Grid'
		write(11,*)nx,ny,nz
		write(11,*)'	dx 	dy	dz  of Grid'
		write(11,*)delx,dely,delz
		write(11,*)'Soil type',sltype
		write(11,*)'	Eps  Sigma'
		write(11,*)feps,sigma(2)
		write(11,*)'Coordinates of Source relative to the center'
		write(11,*)xsource,ysource,zsource
	write(11,*)'Coordinates of Observation point relative to center'
		write(11,*)xreceiv,yreceiv,zreceiv
		write(11,*)'Coordinates of Object relative to the center'
		write(11,*)xobject,yobject,zobject
		write(11,*)'      Length		Width  Height of DNAPL(l,w,h)'
		write(11,*)fll,fww,fhh
		close (unit=11)

	  endif
		
      STOP
      END


C*************************************************************
C*************************************************************
      SUBROUTINE BUILD

      INCLUDE "soil_3d_params.for"

C     THIS SUBROUTINE IS USED TO DEFINE THE SCATTERING OBJECT WITHIN
C     THE FDTD SOLUTION SPACE.  USER MUST SPECIFY IDONE, IDTWO AND
C     IDTHRE AT DIFFERENT CELL LOCATIONS TO DEFINE THE SCATTERING
C     OBJECT.  SEE THE YEE PAPER (IEEE TRANS. ON AP, MAY 1966) FOR
C     A DESCRIPTION OF THE FDTD ALGORITHM AND THE LOCATION OF FIELD
C     COMPONENTS.
C
C     GEOMETRY DEFINITION
C
C     IDONE, IDTWO, AND IDTHRE ARE USED TO SPECIFY MATERIAL IN CELL
C     I,J,K.
C     IDONE DETERMINES MATERIAL FOR X COMPONENTS OF E
C     IDTWO FOR Y COMPONENTS, IDTHRE FOR Z COMPONENTS
C     THUS ANISOTROPIC MATERIALS WITH DIAGONAL TENSORS CAN BE MODELLED
C
C     SET IDONE,IDTWO, AND/OR IDTHRE FOR EACH I,J,K CELL =
C               0       FOR FREE SPACE
C               1       FOR PEC
C               2-13    FOR LOSSY DIELECTRICS
C               14-29   FOR LOSSY SOIL		!type+19
C
C     SUBROUTINE DCUBE BUILDS A CUBE OF DIELECTRIC MATERIAL BY SETTING
C     IDONE, IDTWO, IDTHRE TO THE SAME MATERIAL TYPE.  THE MATERIAL
C     TYPE IS SPECIFIED BY MTYPE.  SPECIFY THE STARTING CELL (LOWER
C     LEFT CORNER (I.E. MINIMUM I,J,K VALUES) AND SPECIFY THE CELL
C     WIDTH IN EACH DIRECTION (USE THE NUMBER OF CELLS IN EACH
C     DIRECTION).  USE nzWIDE=0 FOR A INFINITELY THIN PLATE IN THE
C     XY PLANE.  FOR PEC PLATE USE MTYPE=1.  ISTART, JSTART, KSTART ARE
C     USED TO DEFINE THE STARTING CELL AND nxWIDE, nyWIDE AND nzWIDE EACH
C     SPECIFY THE OBJECT WIDTH IN CELLS IN THE X, Y AND Z DIRECTIONS.
C     INDIVIDUAL IDONE, TWO OR THRE COMPONENTS CAN BE SET MANUALLY
C     FOR WIRES, ETC.  DCUBE DOES NOT WORK FOR WIRES (I.E. nxWIDE=0 AND
C     nyWIDE=0 FOR EXAMPLE)!
C

C     Build sphere with center at (SC1,SC2,SC3) and radius RA
C	or cylinder with radius RA and height Z

c  -------new f--------
  
  
c  18	 do while(ichoice<0 .or. ichoice>3)
		print *,'What is the choice?'
	print *,'(0=Metalic Cylender,1=Metalic Sphere,2=realistic mine
     $			& 3=Rectangular DNAPL)'
		read *,ichoice
c	 end do


c	 if (ichoice/=0 .and. ichoice/=1 .and.
c     $	ichoice/=2 .and. ichoice/=3) then
c	  goto 18
c	 endif


c  --------new fend-----

c    If choice=1, buried object is a sphere
c    If choice=0, buried object is a cylinder
c    If choice=2, buried object is a more realistic model for mine  

c	ichoice=0

c  -------new f--------
c  19	print *,'Matrial type?(0=Air,1=Metal,2=dielectric & 14=Soil)'
c      read *,mtype
c	if (mtype/=0 .and. mtype/=1 .and. mtype/=2 .and. mtype/=14) then
c	 goto 19
c	endif
c  --------new fend-----





c	mtype=2


c   SC1=(nx+1)/2
c	SC2=(ny+1)/2
c	SC3=nzsoil-5

c ------ new f -----------------
c ***************************
c ***************************
c		WRITE BUILD FOR A RECTANGLE OF DNAPL

	if (ichoice==3) then
c
c		print *,'Give (x,y,z) of DNAPL Center in cm'
c		print *,'Relative to centerline of grid'
c		read *,xobject,yobject,zobject
		print *,'Give Length(x),Width(y) & Height(z) of DNAPL(l,w,h)'
c		
		do while(fll==0. .or. fww==0. .or. fhh==0.)
			read *, fll,fww,fhh
		end do

		print *,'Length=',fll,'Width=',fww,'Height=',fhh
		fl=fll/100/delx
		fw=fww/100/dely
		fh=fhh/100/delz
		sc1=(nx+1)/2+nint(xobject/100/delx)
		sc2=(ny+1)/2+nint(yobject/100/dely)
		sc3=(nz+1)/2+nint(zobject/100/delz)

	endif

c ****************************
c ****************************
c  -------new f--------
	if (ichoice==2) then

	  fh=20
	  depth=20
	  rin=1.1
	  rout=2.4
  	  print *,'Height of Metalic cylender(default=20)='
	  read *,fh
	  print *,'Depth of Metalic cylender(default=20)='
	  read *,depth
	  print *,'Inner Radius of Metalic cylender(default=1.1)='
	  read *,rin
	  print *,'Outer Radius of Metalic cylender(default=2.4)='
	  read *,rout

	endif
c  --------new fend-----



c	Rout=2.4
c	Rin=1.1

	DO 100 i=1,nx
	  DO 200 j=1,ny
	    DO 300 k=1,nz

c  ----new f--------
			if (ichoice == 1) then
				mtype=1
				R=SQRT((I-SC1)**2+(J-SC2)**2+(K-SC3)**2)
				IF (R.LT.Rout) CALL DCUBE(I,J,K,1,1,1,MTYPE)

			elseif (ichoice == 0) then

				mtype=1
				height=int(20.0E-2/delz)
				Rin=int(6.25E-2/delx)
				depth=int(6.0E-2/delz)
				ICX=149+Rin
				ICY=149+Rin
				ICZ=nzsoil-depth-height
				R=SQRT(real((I-ICX)**2+(J-ICY)**2))
c		IF (R.LT.Rout.AND.(K-SC3).GT.0.AND.(K-SC3).LT.HEIGHT)
c     $           CALL DCUBE(I,J,K,1,1,1,MTYPE)
				IF (R.LT.Rin.AND.(K-ICZ).GT.0.AND.(K-ICZ).LT.HEIGHT)
     $	        CALL DCUBE(I,J,K,1,1,1,1)

			elseif (ichoice == 2) then

				mtype=0
				height1=int(5.5E-2/delz)
c 40      height1=int(5.5E-2/delz)
				SC3=nzsoil-10-height1
				Rtnt=5.5E-2
				R=dfloat(SQRT((I-SC1)**2+(J-SC2)**2))
				IF (R.LE.Rtnt.AND.(K-SC3).GE.0.AND.(K-SC3).LE.height1)
     $	        CALL DCUBE(I,J,K,1,1,1,MTYPE)
				IF ( (R.GE.1.25E-2).AND.(R.LE.(Rtnt-1.25E-2)).AND.
     $			(K-SC3).GE.(height1-1).AND.(K-SC3).LE.height1)
     $			CALL DCUBE(I,J,K,1,1,1,0)

			elseif (ichoice==3) then
				mtype=2
				if (i>=(sc1-fl/2).and.i<=(sc1+fl/2).and.
     $			j>=(sc2-fw/2).and.j<=(sc2+fw/2).and.
     $			k>=(sc3-fh/2).and.k<=(sc3+fh/2)) then
     				call DCUBE(I,J,K,1,1,1,mtype)
				endif

			endif

c	------new fend---------------

300	    CONTINUE
200 	  CONTINUE
100	CONTINUE

c	print *,'depth=',depth

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE DCUBE (ISTART,JSTART,KSTART,nxWIDE,nyWIDE,nzWIDE,MTYPE)

      INCLUDE "soil_3d_params.for"

C   THIS SUBROUTINE SETS ALL TWELVE IDXXX COMPONENTS FOR ONE CUBE
C   TO THE SAME MATERIAL TYPE SPECIFIED BY MTYPE.  IF nxWIDE, nyWIDE,
C   OR nzWIDE=0, THEN ONLY 4 IDXXX ARRAY COMPONENTS WILL BE SET
C   CORRESPONDING TO AN INFINITELY THIN PLATE.  THE SUBROUTINE IS MOST
C   USEFUL CONSTRUCTING OBJECTS WITH MAny CELLS OF THE SAME MATERIAL
C   (I.E. CUBES, PEC PLATES, ETC.).  THIS SUBROUTINE DOES NOT
C   AUTOMATICALLY DO WIRES!  AHAII





	if (nxwide==2) then
		IMAX=ISTART
		JMAX=JSTART
		KMAX=KSTART

c **** So dielectric would not be confused with soil in EXSFLD ****
	elseif (nxwide==10 .or. nxwide==25) then
		IMAX=ISTART
		JMAX=JSTART
		KMAX=KSTART
c **** So dielectric would not be confused with soil in EXSFLD ****

	else
		IMAX=ISTART+nxWIDE-1
		JMAX=JSTART+nyWIDE-1
		KMAX=KSTART+nzWIDE-1
	endif
	

      IF (nxWIDE==10) THEN
        DO 20 K=KSTART,KMAX
          DO 10 J=JSTART,JMAX
            IDONE(ISTART,J,K)=MTYPE
c            IDONE14(ISTART,J,K)=MTYPE
c            IDONE(ISTART,J+1,K)=MTYPE
            IDTWO(ISTART,J,K)=MTYPE
c            IDTWO(ISTART,J+1,K)=MTYPE
            IDTHRE(ISTART,J,K)=MTYPE
	if (mtype==29) then
            IDONE14(ISTART,J,K)=MTYPE
	endif

c            IDTHRE(ISTART,J+1,K)=MTYPE
c            IDONE(ISTART,J,K)=MTYPE
c            IDONE(ISTART,J,K+1)=MTYPE
c            IDONE(ISTART,J+1,K)=MTYPE
cc
c            IDONE(ISTART,J+1,K+1)=MTYPE	   
c            IDTWO(ISTART,J,K)=MTYPE
c            IDTWO(ISTART,J+1,K)=MTYPE
c            IDTWO(ISTART,J,K+1)=MTYPE
cc
c            IDTWO(ISTART,J+1,K+1)=MTYPE	  
c            IDTHRE(ISTART,J,K)=MTYPE
c            IDTHRE(ISTART,J+1,K)=MTYPE
c            IDTHRE(ISTART,J,K+1)=MTYPE
cc
c            IDTHRE(ISTART,J+1,K+1)=MTYPE	 
 10       CONTINUE
 20     CONTINUE
      ELSEIF (nxWIDE.EQ.0) THEN
        DO 201 K=KSTART,KMAX
          DO 101 J=JSTART,JMAX
              IDTWO(ISTART,J,K)=MTYPE
              IDTWO(ISTART,J,K+1)=MTYPE
              IDTHRE(ISTART,J,K)=MTYPE
              IDTHRE(ISTART,J+1,K)=MTYPE
 101       CONTINUE
 201     CONTINUE
      ELSEIF (nyWIDE.EQ.0) THEN
        DO 40 K=KSTART,KMAX
          DO 30 I=ISTART,IMAX
            IDONE(I,JSTART,K)=MTYPE
            IDONE(I,JSTART,K+1)=MTYPE
            IDTHRE(I,JSTART,K)=MTYPE
            IDTHRE(I+1,JSTART,K)=MTYPE
 30       CONTINUE
 40     CONTINUE
      ELSEIF (nzWIDE.EQ.0) THEN
        DO 60 J=JSTART,JMAX
          DO 50 I=ISTART,IMAX
            IDONE(I,J,KSTART)=MTYPE
            IDONE(I,J+1,KSTART)=MTYPE
            IDTWO(I,J,KSTART)=MTYPE
            IDTWO(I+1,J,KSTART)=MTYPE
 50       CONTINUE
 60     CONTINUE
      ELSEIF (nxWIDE.EQ.1) THEN
        DO 90 K=KSTART,KMAX
          DO 80 J=JSTART,JMAX
            DO 70 I=ISTART,IMAX
              IDONE(I,J,K)=MTYPE
              IDONE(I,J,K+1)=MTYPE
              IDONE(I,J+1,K+1)=MTYPE
              IDONE(I,J+1,K)=MTYPE
              IDTWO(I,J,K)=MTYPE
              IDTWO(I+1,J,K)=MTYPE
              IDTWO(I+1,J,K+1)=MTYPE
              IDTWO(I,J,K+1)=MTYPE
              IDTHRE(I,J,K)=MTYPE
              IDTHRE(I+1,J,K)=MTYPE
              IDTHRE(I+1,J+1,K)=MTYPE
              IDTHRE(I,J+1,K)=MTYPE
 70         CONTINUE
 80       CONTINUE
 90     CONTINUE

      ELSEIF (nxWIDE.EQ.2) THEN
        DO 91 K=KSTART,KMAX
          DO 81 J=JSTART,JMAX
            DO 71 I=ISTART,IMAX
              IDONE(I,J,K)=MTYPE
              IDTWO(I,J,K)=MTYPE
              IDTHRE(I,J,K)=MTYPE
 71         CONTINUE
 81       CONTINUE
 91     CONTINUE

      ENDIF

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE SETUP

      INCLUDE "soil_3d_params.for"

C   THIS SUBROUTINE INITIALIZES THE COMPUTATIONS



c---- new f --------
c *****  new approach, involving the soil type

	C=1.0/SQRT(XMU0*EPS0)
	C1=1.0/SQRT(epsprimef(idone14(1,1,1)))
c ---- new fend ---------


      PI=4.0*ATAN(1.0)

C   CALCULATE DT--THE MAXIMUM TIME STEP ALLOWED BY THE
C   COURANT STABILITY CONDITION

      DTXI=C/DELX
      DTYI=C/DELY
      DTZI=C/DELZ
      

c ***** new f ***************************************
		if (sltypef(1)==1) then
			alfaf=20e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)==2) then
			alfaf=10e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)==3) then
			alfaf=20e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)==4) then
			alfaf=100e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)==5) then
			alfaf=2e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)>=6.and.sltypef(1)<=10) then		!type+19
			alfaf=50e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
c			alfaf=10e-12*SQRT(DTXI**2+DTYI**2+DTZI**2)
		elseif (sltypef(1)==11 .or. sltypef(1)==12) then	!type+19
			alfaf=2e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)==13 .or. sltypef(1)==14) then	!type+19
			alfaf=20e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)==15) then						!type+19
			alfaf=6e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		elseif (sltypef(1)==16) then						!type+19
			alfaf=2e-12*SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
		endif
		
		print *,'alfa=',alfaf
		
		

		fmaxdx=delx
		if (dely>delx) then fmaxdx=dely
		if (delz>fmaxdx) then fmaxdx=delz

		flandaf=c*c1/sqrt(6.3)/freqf/10
		print *,'Max(dx,dy,dz)=',fmaxdx,'m	should be <=',flandaf,'m'

		flandaf=c*c1/freqf/10
		print *,'Max(dx,dy,dz)=',fmaxdx,'m	should be <=',flandaf,'m'

	
c      DT=0.302985148/SQRT(DTXI**2+DTYI**2+DTZI**2)
c	dt=50e-12
	DT=alfaf/SQRT(c1**2*(DTXI**2+DTYI**2+DTZI**2))
c      DT=0.344439849/SQRT(DTXI**2+DTYI**2+DTZI**2)

c ********* new fend ********************************
	print *,'DT=',DT

C   PARAMETER ALPHA IS THE DECAY RATE DETERMINED BY BETA.
	beta=1/(2*freqf*dt)
      ALPHA=(1./(BETA*DT/4.0))**2

      BETADT =  BETA*DT
      PERIOD = 2.0*BETADT
      AMODFREQ=freqf

C   SET OFFSET FOR COMPUTING INCIDENT FIELDS

      OFF=1.0

C   THE FOLLOWING LINES ARE FOR SMOOTH COSINE INCIDENT FUNCTION

      W1 = 2.0*PI/PERIOD
      W2 = 2.0*W1
      W3 = 3.0*W1

C   FIND DIRECTION COSINES FOR INCIDENT FIELD

      COSTH=COS(PI*THINC/180.)
      SINTH=SIN(PI*THINC/180.)
      COSPH=COS(PI*PHINC/180.)
      SINPH=SIN(PI*PHINC/180.)

C   FIND AMPLITUDE OF INCIDENT FIELD COMPONENTS

C   FIND RELATIVE SPATIAL DELAY FOR X, Y, Z CELL DISPLACEMENT

      XDISP=-COSPH*SINTH
      YDISP=-SINPH*SINTH
      ZDISP=-COSTH

C   DEFINE CONSTITUTIVE PARAMETERS--EPS,SIGMA
C   THIS CODE ASSUMES THAT THE DIELECTRIC MATERIALS ALL HAVE
C   A PERMEABILITY OF MU0.  IF YOU NEED A MATERIAL WITH MAGNETIC
C   PROPERTIES, USE ANOTHER FDTD CODE SUCH AS FDTDC, FDTDD, FDTDG
C   OR FDTDH.

C   THESE CORRESPOND TO MATERIAL TYPES IN IDONE, IDTWO, AND
C   IDTHRE ARRAYS.  SEE COMMENTS IN SUBROUTINE BUILD

C   VALID CASES:  IDXXX(I,J,K)  = 0 FOR FREE SPACE IN CELL I,J,K
C                               = 1 FOR PEC
C                               = 2-13 FOR LOSSY DIELECTRICS
C   FOR PARTICULAR COMPONENTS OF FIELDS AS DETERMINED BY XXX

C   IF IDONE (10,10,10) = 3 THEN THE USER MUST SPECIFY AN EPSILON, MU
C   AND SIGMA FOR MATERIAL TYPE 3.  THAT IS, EPS(3)=EPSILON FOR
C   MATERIAL TYPE 3 AND SIGMA(3)= CONDUCTIVITY FOR MATERIAL TYPE 3.
C   ALL PARAMETERS ARE DEFINED IN MKS UNITS.
      DO 10 I=1,13
        EPS(I)=EPS0
        SIGMA(I)=0.0
 10   CONTINUE

c -------new f------
	
c	feps=2.9
c	sigma(2)=.00005
c	print *,'Relative Permittivity of dielectric(default=2.9)=?'
c	read *,feps
c	print *,'Conductivity of dielectric(default=0.00005)=?'
c	read *,sigma(2)
c    LOSSY DIELECTRIC 
      EPS(2)=feps*EPS0
	EPS(3)=feps3*EPS0
	SIGMA(2)=sigmaff  
	SIGMA(3)=0.0  

c ------new fend ---


C   GENERATE MULTIPLICATIVE CONSTANTS FOR FIELD UPDATE EQUATIONS


C   FREE SPACE
 
      DTEDX=DT/(EPS0*DELX)
      DTEDY=DT/(EPS0*DELY)
      DTEDZ=DT/(EPS0*DELZ)
      DTMDX=DT/(XMU0*DELX)
      DTMDY=DT/(XMU0*DELY)
      DTMDZ=DT/(XMU0*DELZ)


c	print *,'Waht type of soil do you have (1, 2 or 3)?'
c	print *,'(Lossy Peurtorican Soil at 20 ps = case 1)'
c	print *,'(Lossy Peurtorican Soil at 10 ps = case 2)'
c	print *,'(Lossy Bosnian Soil = case 3)'
c	read *, fsoilcase

c	do 1011 i=1,nsoillayf+nsoilreg

c	  if (sltypef(i) == 1) then

C   LOSSY PUERTORICAN SOIL PARAMETERS	AT 20 ps
c	    EPSsoil=EPSprime*EPS0
	    a1(14)=-0.88
	    b0(14)=0.916249
	    b1(14)=-1.67662
	    b2(14)=0.761072
		epsprimef(14)=EPSprime
c		epsf=epsprimef(14)

c	  elseif (sltypef(i) == 2) then

C   LOSSY PUERTORICAN SOIL PARAMETERS	AT 10 ps
c		EPSsoil=EPS2prime*EPS0
		a1(15)=-0.95
		b0(15)=3.76795
		b1(15)=-7.30659
		b2(15)=3.53892
		epsprimef(15)=EPS2prime
c		epsf=epsprimef(15)

c	  elseif (sltypef(i) == 3) then

C   LOSSY BOSNIAN SOIL PARAMETERS
c		EPSsoil=EPS3prime*EPS0
		a1(16)=-0.925
		b0(16)=1.76106
		b1(16)=-3.32102
		b2(16)=1.56193
		epsprimef(16)=EPS3prime
c		epsf=epsprimef(16)

c	  elseif (sltypef(i)== 4) then

C   LOSSY BOSNIAN SOIL PARAMETERS AT 100 ps
c		EPSsoil=EPS4prime*EPS0
		a1(17)=-0.925
		b0(17)=1.76106
		b1(17)=-3.32102
		b2(17)=1.56193
		epsprimef(17)=EPS4prime
c		epsf=epsprimef(17)
c	  elseif (sltypef(i) == 5) then

C   LOSSY BOSNIAN SOIL PARAMETERS	AT 2 ps
c		EPSsoil=EPS5prime*EPS0
		a1(18)=-0.9555
		b0(18)=12.9552
		b1(18)=-25.0192
		b2(18)=12.0648
		epsprimef(18)=EPS5prime
c		epsf=epsprimef(18)

c	  elseif (sltypef(i) == 6) then

C            WATER AT  50 ps
c		EPSsoil=EPS6prime*EPS0
		a1(19)=-0.9685
		b0(19)=0.358437
		b1(19)=-0.690476
		b2(19)=0.332181
		epsprimef(19)=EPS6prime			!type+19


C         Soil AT  50 ps
c		Water Content = 2.5%

		a1(20)=-0.9085			
		b0(20)=-0.2375335
		b1(20)=0.46763
		b2(20)=-0.229884
		epsprimef(20)=EPS7prime			!type+19


C         Soil AT  50 ps
c		Water Content = 2.5%

		a1(21)=-0.8685			
		b0(21)=-0.408516
		b1(21)=0.789749
		b2(21)=-0.380641
		epsprimef(21)=EPS8prime			


C         Soil AT  50 ps
c		Water Content = 2.5%

		a1(22)=-0.8585			
		b0(22)=-0.611398
		b1(22)=1.17886
		b2(22)=-0.566334
		epsprimef(22)=EPS9prime			


C         (Type10) Soil AT  50 ps
c		Water Content = 2.5%

		a1(23)=-0.8685			
		b0(23)=-0.242174
		b1(23)=0.554974
		b2(23)=-0.308386
		epsprimef(23)=EPS10prime			!type+19

C         (type11) Sandy Soil AT  2 ps
c		Water Content = 4%

		a1(24)=-0.8285
		b0(24)=-6.4532
		b1(24)=12.5242
		b2(24)=-6.07063
		epsprimef(24)=EPS11prime			!type+19


C         (type12)Sandy Soil AT  2 ps
c		Water Content = 17%

		a1(25)=-0.8985	
		b0(25)=-34.3627
		b1(25)=68.7577
		b2(25)=-34.3945
		epsprimef(25)=EPS12prime			!type+19


C         (type13)Sandy Soil AT  20 ps
c		Water Content = 4%

		a1(26)=-0.4785	
		b0(26)=0.323846
		b1(26)=-0.458101
		b2(26)=0.135271
		epsprimef(26)=EPS13prime			!type+19


C         (type14)Sandy Soil AT  20 ps
c		Water Content = 17%

		a1(27)=-0.8585	
		b0(27)=2.88719
		b1(27)=-5.1354
		b2(27)=2.24691
		epsprimef(27)=EPS14prime			!type+19


C         (type15)Sandy Soil AT  6 ps
c		Water Content = 17%

		a1(28)=-0.8785	
		b0(28)=21.7478
		b1(28)=-40.2878
		b2(28)=18.5404
		epsprimef(28)=EPS15prime			!type+19



C         (type16)Dielectric 2 ps

		a1(29)=0.0	
		b0(29)=0.0
		b1(29)=0.0
		b2(29)=0.0
		epsprimef(29)=EPS16prime			!type+19


c		epsf=epsprimef(19)

									
c	  endif

1011	continue
      
 
C   LOSSY DIELECTRICS
c   ahai inajst
      DO 20 I=2,13
        ESCTC(I)=1.0+(0.5*SIGMA(I)*DT/EPS(I))
        DTEDXD(I)=(DT/(EPS(I)*DELX))/ESCTC(I)
        DTEDYD(I)=(DT/(EPS(I)*DELY))/ESCTC(I)
        DTEDZD(I)=(DT/(EPS(I)*DELZ))/ESCTC(I)  
        EINCC(I)=(1.0-(0.5*SIGMA(I)*DT/EPS(I)))/ESCTC(I)
 20   CONTINUE

C   FIND MAXIMUM SPATIAL DELAY TO MAKE SURE PULSE PROPAGATES
C   INTO SPACE PROPERLY.

      DELAY=0.0
      IF (XDISP.LT.0.) DELAY=DELAY-XDISP*nx1*DELX
      IF (YDISP.LT.0.) DELAY=DELAY-YDISP*ny1*DELY
      IF (ZDISP.LT.0.) DELAY=DELAY-ZDISP*nz1*DELZ


C   COMPUTE 1ST ORDER ORBC CONSTANTS

C   FREE SPACE

      CXD=(C*DT-DELX)/(C*DT+DELX)
      CYD=(C*DT-DELY)/(C*DT+DELY)
      CZD=(C*DT-DELZ)/(C*DT+DELZ)

      CXU=CXD
      CYU=CYD
      CZU=CZD

C   SOIL 
      xx1X=1.0/DELX
      xx1Y=1.0/DELY
      xx1Z=1.0/DELZ



C   COMPUTE 2ND ORDER ORBC CONSTANTS

C   FREE SPACE

      CXX=2.*DELX/(C*DT+DELX)
      CYY=2.*DELY/(C*DT+DELY)
      CZZ=2.*DELZ/(C*DT+DELZ)

      CXFYD=DELX*C*DT*C*DT/(2.*DELY*DELY*(C*DT+DELX))
      CXFZD=DELX*C*DT*C*DT/(2.*DELZ*DELZ*(C*DT+DELX))
      CYFZD=DELY*C*DT*C*DT/(2.*DELZ*DELZ*(C*DT+DELY))
      CYFXD=DELY*C*DT*C*DT/(2.*DELX*DELX*(C*DT+DELY))
      CZFXD=DELZ*C*DT*C*DT/(2.*DELX*DELX*(C*DT+DELZ))
      CZFYD=DELZ*C*DT*C*DT/(2.*DELY*DELY*(C*DT+DELZ))

C   SOIL 
c ********eps3prime to epsff
c ******      xx2=-sqrt(EPS3prime)/(C*DT)
c ******      xx3=-ETA0/(2.0*sqrt(EPS3prime))
c ****  new soil layers ******
	uu3=-XMU0/(2.0*DT)
	
	do 2221 I = 1, nx
       do 3331 J = 1, ny
        do 4441 K = 1, nzsoil
	  
	
c	if (idone(i,j,k)>14) then
c		idone14(i,j,k)=idone(i,j,k)
c	else
c		idone14(i,j,k)=idone(1,1,1)
c	endif

	    xx2(idone14(i,j,k))=-sqrt(epsprimef(idone14(i,j,k)))/(C*DT)
	   xx3(idone14(i,j,k))=-ETA0/(2.0*sqrt(epsprimef(idone14(i,j,k))))
	  uu1X(idone14(i,j,k))=sqrt(epsprimef(idone14(i,j,k)))/(C*DT*DELX)
	  uu1Y(idone14(i,j,k))=sqrt(epsprimef(idone14(i,j,k)))/(C*DT*DELY)
	  uu1Z(idone14(i,j,k))=sqrt(epsprimef(idone14(i,j,k)))/(C*DT*DELZ)
		uu2(idone14(i,j,k))=-epsprimef(idone14(i,j,k))/(C*C*DT*DT)
		
		k0(idone14(i,j,k))=1+(b0(idone14(i,j,k))*DT
     $		/(2*epsprimef(idone14(i,j,k))*EPS0))
		k1(idone14(i,j,k))=(a1(idone14(i,j,k))-1)+(b0(idone14(i,j,k))
     $		+b1(idone14(i,j,k)))*DT/(2*epsprimef(idone14(i,j,k))
     $	*EPS0)
		k2(idone14(i,j,k))=-a1(idone14(i,j,k))+(b1(idone14(i,j,k))
     $		+b2(idone14(i,j,k)))*DT/(2*epsprimef(idone14(i,j,k))
     $	*EPS0)
		k3(idone14(i,j,k))=b2(idone14(i,j,k))*DT
     $		/(2*epsprimef(idone14(i,j,k))*EPS0)
c	print *, k0(idone14(i,j,k)), k1(idone14(i,j,k)), k2(idone14(i,j,k))

	   DTEDXS(idone14(i,j,k))=DT/(epsprimef(idone14(i,j,k))*EPS0*DELX)
	   DTEDYS(idone14(i,j,k))=DT/(epsprimef(idone14(i,j,k))*EPS0*DELY)
	   DTEDZS(idone14(i,j,k))=DT/(epsprimef(idone14(i,j,k))*EPS0*DELZ)

c	if (i>=6) then
c		print *, 'i,j,k=', i,j,k
c	  	print *, 'xxx2,xx3',xx2(idone14(i,j,k)),xx3(idone14(i,j,k))
c	  	print *, 'uu1x,y,z',uu1X(idone14(i,j,k)),uu1Y(idone14(i,j,k))
c     $		,uu1Z(idone14(i,j,k)),uu2(idone14(i,j,k))
c	  	print *, 'k',k0(idone14(i,j,k)),k1(idone14(i,j,k))
c	  	print *, k2(idone14(i,j,k)),k3(idone14(i,j,k))
c	  	print *, 'dtedx,y,zs',DTEDXS(idone14(i,j,k)),DTEDYS(idone14(i,j,k))
c     $	  ,DTEDXS(idone14(i,j,k))
c	endif
4441    CONTINUE
3331   CONTINUE
2221  CONTINUE

c	print *, k0(idone14(i,j,k)), k1(idone14(i,j,k)), k2(idone14(i,j,k))


	RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE EXSFLD

      INCLUDE "soil_3d_params.for"

 
C  SAVE PAST VALUES
      DO 70 K=2,nzsoil
        DO 60 J=2,ny1
          DO 50 I=1,nx1
		EXS3(I,J,K)=EXS2(I,J,K)
		EXS2(I,J,K)=EXS1(I,J,K)
		EXS1(I,J,K)=EXS(I,J,K)
 50     CONTINUE
 60    CONTINUE
 70	CONTINUE


C   THIS SUBROUTINE UPDATES THE EX SCATTERED FIELD
      DO 30 K=2,nz1
        KK = K
        DO 20 J=2,ny1
          JJ = J
          DO 10 I=1,nx1


C   DETERMINE MATERIAL TYPE
          
		IF(IDONE(I,J,K).EQ.0) GO TO 100
          
c		IF(IDONE(I,J,K).EQ.14 ) GO TO 150
c		IF(IDONE(I,J,K).EQ.15 ) GO TO 150
c		IF(IDONE(I,J,K).EQ.16 ) GO TO 150
c		IF(IDONE(I,J,K).EQ.17 ) GO TO 150
c		IF(IDONE(I,J,K).EQ.18 ) GO TO 150
c		IF(IDONE(I,J,K).EQ.19 ) GO TO 150			!type+19
		IF(IDONE(I,J,K)>=14 .and. IDONE(I,J,K)<=29) GO TO 150	!type+19


          IF(IDONE(I,J,K).EQ.1) GO TO 200
	    IF(IDONE(I,J,K).EQ.2) GO TO 300
		IF(IDTWO(I,J,K).EQ.3) GO TO 310		   
            GO TO 10
 
C     FREE SPACE
100   EXS(I,J,K)=EXS(I,J,K)+(HZS(I,J,K)-HZS(I,J-1,K))*DTEDY
     $ -(HYS(I,J,K)-HYS(I,J,K-1))*DTEDZ
            GO TO 10
 
 
C     LOSSY SOIL
150   EXS(I,J,K)=(1/k0(idone14(i,j,k)))*(
     $ -k1(idone14(i,j,k))*EXS(I,J,K)-k2(idone14(i,j,k))*EXS2(I,J,K)
     $  -k3(idone14(i,j,k))*EXS3(I,J,K)
     $ +DTEDYS(idone14(i,j,k))*(HZS(I,J,K)-HZS(I,J-1,K))
     $ -DTEDZS(idone14(i,j,k))*(HYS(I,J,K)-HYS(I,J,K-1))
     $ +DTEDYS(idone14(i,j,k))*a1(idone14(i,j,k))*(HZS1(I,J,K)
     $   -HZS1(I,J-1,K))
     $ -DTEDZS(idone14(i,j,k))*a1(idone14(i,j,k))*(HYS1(I,J,K)
     $   -HYS1(I,J,K-1))
     $ )
c	print *, 'exs(i,j,k)=', exs(i,j,k)
c	write(4111,*) exs(i,j,k)
      GO TO 10


C     PERFECT CONDUCTOR
 200  II = I
      EXS(I,J,K)= 0

      GO TO 10


C     LOSSY DIELECTRIC
 300	II=I
      EXS(I,J,K)=
     $    EXS(I,J,K)*EINCC(IDONE(I,J,K))
     $  +(HZS(I,J,K)-HZS(I,J-1,K))*DTEDYD(IDONE(I,J,K))
     $  -(HYS(I,J,K)-HYS(I,J,K-1))*DTEDZD(IDONE(I,J,K))
            GO TO 10

C     LOSSY DIELECTRIC
 310	II=I
      EXS(I,J,K)=
     $    EXS(I,J,K)*EINCC(IDONE(I,J,K))
     $  +(HZS(I,J,K)-HZS(I,J-1,K))*DTEDYD(IDONE(I,J,K))
     $  -(HYS(I,J,K)-HYS(I,J,K-1))*DTEDZD(IDONE(I,J,K))
c            GO TO 10


 
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE
 

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE EYSFLD

      INCLUDE "soil_3d_params.for"

c      print *, k0,k1,k2,k3
c      stop

C   SAVE PAST VALUES
      DO 70 K=2,nzsoil
        DO 60 J=1,ny1
          DO 50 I=2,nx1
            EYS3(I,J,K)=EYS2(I,J,K)
            EYS2(I,J,K)=EYS1(I,J,K)
            EYS1(I,J,K)=EYS(I,J,K)
 50      CONTINUE
 60	  CONTINUE
 70	CONTINUE


C   THIS SUBROUTINE UPDATES THE EY SCATTERED FIELD COMPONENTS
      DO 30 K=2,nz1
        KK = K
        DO 20 J=1,ny1
          JJ = J
          DO 10 I=2,nx1

C   DETERMINE MATERIAL TYPE

            IF(IDTWO(I,J,K).EQ.0) GO TO 100
            
c		  IF(IDTWO(I,J,K).EQ.14) GO TO 150
c           IF(IDTWO(I,J,K).EQ.15) GO TO 150
c           IF(IDTWO(I,J,K).EQ.16) GO TO 150
c           IF(IDTWO(I,J,K).EQ.17) GO TO 150
c           IF(IDTWO(I,J,K).EQ.18) GO TO 150
c           IF(IDTWO(I,J,K).EQ.19) GO TO 150			!type+19
            IF(IDTWO(I,J,K)>=14 .and. IDTWO(I,J,K)<=29) GO TO 150	!type+19


		  IF(IDTWO(I,J,K).EQ.1) GO TO 200
	      IF(IDTWO(I,J,K).EQ.2) GO TO 300
		  IF(IDTWO(I,J,K).EQ.3) GO TO 310
            GO TO 10
 
C   FREE SPACE
 
 100  EYS(I,J,K)=EYS(I,J,K)+(HXS(I,J,K)-HXS(I,J,K-1))*DTEDZ
     $ -(HZS(I,J,K)-HZS(I-1,J,K))*DTEDX
        GO TO 10
 
 
C   LOSSY SOIL
150   EYS(I,J,K)=(1/k0(idone14(i,j,k)))*(
     $ -k1(idone14(i,j,k))*EYS(I,J,K)-k2(idone14(i,j,k))*EYS2(I,J,K)
     $  -k3(idone14(i,j,k))*EYS3(I,J,K)
     $ +DTEDZS(idone14(i,j,k))*(HXS(I,J,K)-HXS(I,J,K-1))
     $ -DTEDXS(idone14(i,j,k))*(HZS(I,J,K)-HZS(I-1,J,K))
     $ +DTEDZS(idone14(i,j,k))*a1(idone14(i,j,k))*(HXS1(I,J,K)
     $    -HXS1(I,J,K-1))
     $ -DTEDXS(idone14(i,j,k))*a1(idone14(i,j,k))*(HZS1(I,J,K)
     $    -HZS1(I-1,J,K))
     $ )

c	print *, 'eys(i,j,k)=', eys(i,j,k)
c	write(4111,*) eys(i,j,k)

        GO TO 10
 
 
C   PERFECT CONDUCTOR
 200  II = I
      EYS(I,J,K)= 0
      GO TO 10


C     LOSSY DIELECTRIC
 300	II=I
      EYS(I,J,K)=EYS(I,J,K)*EINCC(IDTWO(I,J,K))
     $  +(HXS(I,J,K)-HXS(I,J,K-1))*DTEDZD(IDTWO(I,J,K))
     $  -(HZS(I,J,K)-HZS(I-1,J,K))*DTEDXD(IDTWO(I,J,K))
            GO TO 10

C     LOSSY DIELECTRIC
 310	II=I
      EYS(I,J,K)=EYS(I,J,K)*EINCC(IDTWO(I,J,K))
     $  +(HXS(I,J,K)-HXS(I,J,K-1))*DTEDZD(IDTWO(I,J,K))
     $  -(HZS(I,J,K)-HZS(I-1,J,K))*DTEDXD(IDTWO(I,J,K))
c            GO TO 10

 


  10      CONTINUE
  20    CONTINUE
  30  CONTINUE

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE EZSFLD

      INCLUDE "soil_3d_params.for"

C   SAVE PAST VALUES
      DO 70 K=1,nzsoil
        DO 60 J=2,ny1
          DO 50 I=2,nx1
            EZS3(I,J,K)=EZS2(I,J,K)
            EZS2(I,J,K)=EZS1(I,J,K)
            EZS1(I,J,K)=EZS(I,J,K)
 50     CONTINUE
 60 	 CONTINUE
 70	CONTINUE

C   THIS SUBROUTINE UPDATES THE EZ SCATTERED FIELD COMPONENTS
      DO 30 K=1,nz1
        KK = K
        DO 20 J=2,ny1
          JJ = J
          DO 10 I=2,nx1
             

C   DETERMINE MATERIAL TYPE
            
		  IF(IDTHRE(I,J,K).EQ.0) GO TO 100
            
c		  IF(IDTHRE(I,J,K).EQ.14) GO TO 150
c		  IF(IDTHRE(I,J,K).EQ.15) GO TO 150            
c		  IF(IDTHRE(I,J,K).EQ.16) GO TO 150
c		  IF(IDTHRE(I,J,K).EQ.17) GO TO 150
c		  IF(IDTHRE(I,J,K).EQ.18) GO TO 150
c		  IF(IDTHRE(I,J,K).EQ.19) GO TO 150			!type+19
		  IF(IDTHRE(I,J,K)>=14 .and. IDTHRE(I,J,K)<=29) GO TO 150 !type+19
		  

		  IF(IDTHRE(I,J,K).EQ.1) GO TO 200
            IF(IDTHRE(I,J,K).EQ.2) GO TO 300
		  IF(IDTHRE(I,J,K).EQ.3) GO TO 310
            GO TO 10

C   FREE SPACE
            
 100  EZS(I,J,K)=EZS(I,J,K)+(HYS(I,J,K)-HYS(I-1,J,K))*DTEDX
     $ -(HXS(I,J,K)-HXS(I,J-1,K))*DTEDY
         GO TO 10
 
 
C   LOSSY SOIL
   
 150  EZS(I,J,K)=(1/k0(idone14(i,j,k)))*(
     $ -k1(idone14(i,j,k))*EZS(I,J,K)-k2(idone14(i,j,k))*EZS2(I,J,K)
     $  -k3(idone14(i,j,k))*EZS3(I,J,K)
     $ +DTEDXS(idone14(i,j,k))*(HYS(I,J,K)-HYS(I-1,J,K))
     $ -DTEDYS(idone14(i,j,k))*(HXS(I,J,K)-HXS(I,J-1,K))
     $ +DTEDXS(idone14(i,j,k))*a1(idone14(i,j,k))*(HYS1(I,J,K)
     $    -HYS1(I-1,J,K))
     $ -DTEDYS(idone14(i,j,k))*a1(idone14(i,j,k))*(HXS1(I,J,K)
     $    -HXS1(I,J-1,K))
     $ )

c	print *, 'ezs(i,j,k)=', ezs(i,j,k)
c	write(4111,*) ezs(i,j,k)

         GO TO 10


C   PERFECT CONDUCTOR
 200  II = I
      EZS(I,J,K)= 0
         GO TO 10


C     LOSSY DIELECTRIC
 300	II=I
      EZS(I,J,K)=EZS(I,J,K)*EINCC(IDTHRE(I,J,K))
     $  +(HYS(I,J,K)-HYS(I-1,J,K))*DTEDXD(IDTHRE(I,J,K))
     $  -(HXS(I,J,K)-HXS(I,J-1,K))*DTEDYD(IDTHRE(I,J,K))
          GO TO 10

C     LOSSY DIELECTRIC
 310	II=I
      EZS(I,J,K)=EZS(I,J,K)*EINCC(IDTHRE(I,J,K))
     $  +(HYS(I,J,K)-HYS(I-1,J,K))*DTEDXD(IDTHRE(I,J,K))
     $  -(HXS(I,J,K)-HXS(I,J-1,K))*DTEDYD(IDTHRE(I,J,K))
c          GO TO 10



 10       CONTINUE
 20     CONTINUE
 30   CONTINUE


      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE RADEZX

      INCLUDE "soil_3d_params.for"


C   DO EDGES WITH FIRST ORDER ORBC

	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1
	endif

c      DO 5 K=1,nzsoil
c      DO 5 K=1,nz1
	do 5 k=1,nzquesf

        J=2

        EZS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k))))*(
     $   EZS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EZSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X
     $     -(1.0-a1(idone14(2,j,k)))*xx2(idone14(2,j,k))
     $  +b0(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EZSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X
     $    -(1.0-a1(idone14(1,j,k)))*xx2(idone14(1,j,k))
     $    +b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $    *xx2(idone14(2,j,k))
     $  +b1(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EZSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k)
     $  )*xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EZSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k))))



        EZS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EZS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EZSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $     -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))
     $  +b0(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EZSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $     -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $    *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EZSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     $    *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EZSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k)))
     $  )

        J=ny1

        EZS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k))))*(
     $   EZS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EZSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X-(1.0
     $    -a1(idone14(2,j,k)))*xx2(idone14(2,j,k))+b0(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EZSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X-(1.0
     $     -a1(idone14(1,j,k)))*xx2(idone14(1,j,k))+b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $   *xx2(idone14(2,j,k))
     $     +b1(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EZSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k))
     $    *xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EZSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k)))
     $  )

        EZS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EZS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EZSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $    -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))+b0(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EZSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $    -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $    *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EZSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     $     *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EZSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k)))
     $  )

5     CONTINUE


	if (fansair=='y' .or. fansair=='Y') then
		DO 10 K=nzsoil+1,nz1
			J=2
			EZS(1,J,K)=EZSX1(2,J,K)+CXD*(EZS(2,J,K)-EZSX1(1,J,K))
			EZS(nx,J,K)=EZSX1(3,J,K)+CXU*(EZS(nx1,J,K)-EZSX1(4,J,K))
			J=ny1
			EZS(1,J,K)=EZSX1(2,J,K)+CXD*(EZS(2,J,K)-EZSX1(1,J,K))
			EZS(nx,J,K)=EZSX1(3,J,K)+CXU*(EZS(nx1,J,K)-EZSX1(4,J,K))

 10		CONTINUE
	endif


      DO 20 J=3,ny1-1
        K=1

        EZS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k))))*(
     $   EZS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EZSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X-(1.0
     $     -a1(idone14(2,j,k)))*xx2(idone14(2,j,k))+b0(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EZSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X-(1.0
     $    -a1(idone14(1,j,k)))*xx2(idone14(1,j,k))+b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $     *xx2(idone14(2,j,k))+b1(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EZSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k))
     $     *xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EZSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k)))
     $  )

        EZS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EZS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EZSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $    -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))+b0(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EZSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $    -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $    *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EZSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     $    *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EZSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k)))
     $  )

        K=nz1

	if (fansair=='y' .or. fansair=='Y') then
        
	  EZS(1,J,K)=EZSX1(2,J,K)+CXD*(EZS(2,J,K)-EZSX1(1,J,K))
        EZS(nx,J,K)=EZSX1(3,J,K)+CXU*(EZS(nx1,J,K)-EZSX1(4,J,K))
	
      else

        EZS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k)) ))*(
     $   EZS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EZSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X-(1.0
     $     -a1(idone14(2,j,k)))*xx2(idone14(2,j,k))+b0(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EZSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X-(1.0
     $    -a1(idone14(1,j,k)))*xx2(idone14(1,j,k))+b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $     *xx2(idone14(2,j,k))+b1(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EZSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k))
     $     *xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EZSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EZSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k)))
     $  )

        EZS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EZS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EZSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $    -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))+b0(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EZSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $    -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $    *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EZSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     $    *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EZSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EZSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k))))

	endif

 20   CONTINUE


C   CALC 2ND ORDER ORBC ON REMAINING PORTIONS OF FACES


	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1-1
	endif


c      DO 140 K=2,nzsoil
c      DO 140 K=2,nz1-1

	do 140 k=2,nzquesf

       DO 130 J=3,ny1-1

	EZS(1,J,K)=(1.0/(uu1X(idone14(2,j,k))-uu2(idone14(2,j,k))
     $ 	-b0(idone14(2,j,k))*uu3))*(
     $   EZS(2,J,K)*(uu1X(idone14(2,j,k))+uu2(idone14(2,j,k))
     $  +b0(idone14(2,j,k))*uu3)
     $  +EZSX1(2,J,K)*(a1(idone14(2,j,k))*uu1X(idone14(2,j,k))+(-2.0
     $    +a1(idone14(2,j,k)))*uu2(idone14(2,j,k))
     $   +(-b0(idone14(2,j,k))+b1(idone14(2,j,k)))*uu3)
     $  +EZSX1(1,J,K)*(-a1(idone14(1,j,k))*uu1X(idone14(1,j,k))
     $   +(-2.0+a1(idone14(1,j,k)))*uu2(idone14(1,j,k))
     $   +(-b0(idone14(1,j,k))+b1(idone14(1,j,k)))*uu3)
     $  +EZSX2(2,J,K)*(-uu1X(idone14(2,j,k))+(1-2*a1(idone14(2,j,k)))
     $     *uu2(idone14(2,j,k))
     $  +(-b1(idone14(2,j,k))+b2(idone14(2,j,k)))*uu3)
     $  +EZSX2(1,J,K)*(uu1X(idone14(1,j,k))+(1-2*a1(idone14(1,j,k)))
     $     *uu2(idone14(1,j,k))
     $  +(-b1(idone14(1,j,k))+b2(idone14(1,j,k)))*uu3)
     $  +EZSX3(2,J,K)*(-a1(idone14(2,j,k))*uu1X(idone14(2,j,k))
     $     +a1(idone14(2,j,k))*uu2(idone14(2,j,k))
     $  -b2(idone14(2,j,k))*uu3)
     $  +EZSX3(1,J,K)*(a1(idone14(1,j,k))*uu1X(idone14(1,j,k))
     $     +a1(idone14(1,j,k))*uu2(idone14(1,j,k))
     $  -b2(idone14(1,j,k))*uu3)
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 term:
     $     EZSX1(1,J+1,K)-2.*EZSX1(1,J,K)+EZSX1(1,J-1,K)
     $    +EZSX1(2,J+1,K)-2.*EZSX1(2,J,K)+EZSX1(2,J-1,K)
     $    +a1(idone14(2,j-1,k))*(
     $    +EZSX2(1,J+1,K)-2.*EZSX2(1,J,K)+EZSX2(1,J-1,K)
     $    +EZSX2(2,J+1,K)-2.*EZSX2(2,J,K)+EZSX2(2,J-1,K)
     $     )
     $   ) ! end of d/dy**2 term
     $  +(1.0/(2.0*DELZ**2))*( ! The d/dz**2 term:
     $     EZSX1(1,J,K+1)-2.*EZSX1(1,J,K)+EZSX1(1,J,K-1)
     $    +EZSX1(2,J,K+1)-2.*EZSX1(2,J,K)+EZSX1(2,J,K-1)
     $    +a1(idone14(2,j,k-1))*(
     $    +EZSX2(1,J,K+1)-2.*EZSX2(1,J,K)+EZSX2(1,J,K-1)
     $    +EZSX2(2,J,K+1)-2.*EZSX2(2,J,K)+EZSX2(2,J,K-1)
     $     )
     $   ) ! end of d/dz**2 term
     $  )

	EZS(nx,J,K)=(1.0/(uu1X(idone14(nx1,j,k))-uu2(idone14(nx1,j,k))
     $   -b0(idone14(nx1,j,k))*uu3))*(
     $   EZS(nx1,J,K)*(uu1X(idone14(nx1,j,k))+uu2(idone14(nx1,j,k))
     $   +b0(idone14(nx1,j,k))*uu3)
     $  +EZSX1(3,J,K)*(a1(idone14(3,j,k))*uu1X(idone14(3,j,k))+(-2.0
     $   +a1(idone14(3,j,k)))
     $   *uu2(idone14(3,j,k))+(-b0(idone14(3,j,k))
     $    +b1(idone14(3,j,k)))*uu3)
     $  +EZSX1(4,J,K)*(-a1(idone14(4,j,k))*uu1X(idone14(4,j,k))+(-2.0
     $     +a1(idone14(4,j,k)))
     $   *uu2(idone14(4,j,k))
     $  +(-b0(idone14(4,j,k))+b1(idone14(4,j,k)))*uu3)
     $  +EZSX2(3,J,K)*(-uu1X(idone14(3,j,k))+(1-2*a1(idone14(3,j,k)))
     $     *uu2(idone14(3,j,k))
     $   +(-b1(idone14(3,j,k))+b2(idone14(3,j,k)))*uu3)
     $  +EZSX2(4,J,K)*(uu1X(idone14(4,j,k))+(1-2*a1(idone14(4,j,k)))
     $     *uu2(idone14(4,j,k))
     $   +(-b1(idone14(4,j,k))+b2(idone14(4,j,k)))*uu3)
     $  +EZSX3(3,J,K)*(-a1(idone14(3,j,k))*uu1X(idone14(3,j,k))
     $     +a1(idone14(3,j,k))*uu2(idone14(3,j,k))
     $   -b2(idone14(3,j,k))*uu3)
     $  +EZSX3(4,J,K)*(a1(idone14(4,j,k))*uu1X(idone14(4,j,k))
     $     +a1(idone14(4,j,k))*uu2(idone14(4,j,k))
     $   -b2(idone14(4,j,k))*uu3)
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 term:
     $    +EZSX1(4,J+1,K)-2.*EZSX1(4,J,K)+EZSX1(4,J-1,K)
     $    +EZSX1(3,J+1,K)-2.*EZSX1(3,J,K)+EZSX1(3,J-1,K)
     $    +a1(idone14(3,j-1,k))*(
     $    +EZSX2(4,J+1,K)-2.*EZSX2(4,J,K)+EZSX2(4,J-1,K)
     $    +EZSX2(3,J+1,K)-2.*EZSX2(3,J,K)+EZSX2(3,J-1,K)
     $     )
     $   ) ! end of d/dy**2 term
     $  +(1.0/(2.*DELZ**2))*( ! The d/dz**2 term:
     $    +EZSX1(4,J,K+1)-2.*EZSX1(4,J,K)+EZSX1(4,J,K-1)
     $    +EZSX1(3,J,K+1)-2.*EZSX1(3,J,K)+EZSX1(3,J,K-1)
     $    +a1(idone14(3,j,k-1))*(
     $    +EZSX2(4,J,K+1)-2.*EZSX2(4,J,K)+EZSX2(4,J,K-1)
     $    +EZSX2(3,J,K+1)-2.*EZSX2(3,J,K)+EZSX2(3,J,K-1)
     $     )
     $   ) ! end od d/dz**2 term
     $  )

130    CONTINUE
140   CONTINUE

	if (fansair=='y' .or. fansair=='Y') then

       DO 40 K=nzsoil+1,nz1-1
        DO 30 J=3,ny1-1
          EZS(1,J,K)=-EZSX2(2,J,K)+CXD*(EZS(2,J,K)+EZSX2(1,J,K))
     $    +CXX*(EZSX1(1,J,K)+EZSX1(2,J,K))
     $    +CXFYD*(EZSX1(1,J+1,K)-2.*EZSX1(1,J,K)+EZSX1(1,J-1,K)
     $    +EZSX1(2,J+1,K)-2.*EZSX1(2,J,K)+EZSX1(2,J-1,K))
     $    +CXFZD*(EZSX1(1,J,K+1)-2.*EZSX1(1,J,K)+EZSX1(1,J,K-1)
     $    +EZSX1(2,J,K+1)-2.*EZSX1(2,J,K)+EZSX1(2,J,K-1))
          EZS(nx,J,K)=-EZSX2(3,J,K)+CXD*(EZS(nx1,J,K)+EZSX2(4,J,K))
     $    +CXX*(EZSX1(4,J,K)+EZSX1(3,J,K))
     $    +CXFYD*(EZSX1(4,J+1,K)-2.*EZSX1(4,J,K)+EZSX1(4,J-1,K)
     $    +EZSX1(3,J+1,K)-2.*EZSX1(3,J,K)+EZSX1(3,J-1,K))
     $    +CXFZD*(EZSX1(4,J,K+1)-2.*EZSX1(4,J,K)+EZSX1(4,J,K-1)
     $    +EZSX1(3,J,K+1)-2.*EZSX1(3,J,K)+EZSX1(3,J,K-1))
 30     CONTINUE
 40    CONTINUE
	endif

C   SAVE PAST VALUES
      DO 60 K=1,nz1
        DO 50 J=2,ny1
          EZSX3(1,J,K)=EZSX2(1,J,K)
          EZSX3(2,J,K)=EZSX2(2,J,K)
          EZSX3(3,J,K)=EZSX2(3,J,K)
          EZSX3(4,J,K)=EZSX2(4,J,K)
          EZSX2(1,J,K)=EZSX1(1,J,K)
          EZSX2(2,J,K)=EZSX1(2,J,K)
          EZSX2(3,J,K)=EZSX1(3,J,K)
          EZSX2(4,J,K)=EZSX1(4,J,K)
          EZSX1(1,J,K)=EZS(1,J,K)
          EZSX1(2,J,K)=EZS(2,J,K)
          EZSX1(3,J,K)=EZS(nx1,J,K)
          EZSX1(4,J,K)=EZS(nx,J,K)
 50     CONTINUE
 60   CONTINUE

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE RADEYX

      INCLUDE "soil_3d_params.for"


C   DO EDGES WITH FIRST ORDER ORBC

	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1
	endif


c      DO 5 K=2,nzsoil
c      DO 5 K=2,nz1
	do 5 k=2,nzquesf
        
	  J=1

        EYS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k))))*(
     $  +EYS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EYSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X-(1.0
     $     -a1(idone14(2,j,k)))*xx2(idone14(2,j,k))+b0(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X-(1.0
     $    -a1(idone14(1,j,k)))*xx2(idone14(1,j,k))+b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $     *xx2(idone14(2,j,k))+b1(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k))
     $     *xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EYSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k)))
     $  )

        EYS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EYS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EYSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $    -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))+b0(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $     -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $   *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     %    *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EYSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k)))
     $  )

c        EYS(nx,J,K)=EYSX1(3,J,K)+CXU*(EYS(nx1,J,K)-EYSX1(4,J,K)) !delete

        J=ny1

        EYS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k))))*(
     $   EYS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EYSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X-(1.0
     $    -a1(idone14(2,j,k)))*xx2(idone14(2,j,k))+b0(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X-(1.0
     $     -a1(idone14(1,j,k)))*xx2(idone14(1,j,k))+b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $     *xx2(idone14(2,j,k))+b1(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k))
     $     *xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EYSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k)))
     $  )

c        EYS(nx,J,K)=EYSX1(3,J,K)+CXU*(EYS(nx1,J,K)-EYSX1(4,J,K))

        EYS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EYS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EYSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $     -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))+b0(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $     -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $   *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     $    *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EYSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k)))
     $  )

5     CONTINUE

	if (fansair=='y' .or. fansair=='Y') then

       DO 10 K=nzsoil+1,nz1
        J=1
        EYS(1,J,K)=EYSX1(2,J,K)+CXD*(EYS(2,J,K)-EYSX1(1,J,K))
        EYS(nx,J,K)=EYSX1(3,J,K)+CXU*(EYS(nx1,J,K)-EYSX1(4,J,K))
        J=ny1
        EYS(1,J,K)=EYSX1(2,J,K)+CXD*(EYS(2,J,K)-EYSX1(1,J,K))
        EYS(nx,J,K)=EYSX1(3,J,K)+CXU*(EYS(nx1,J,K)-EYSX1(4,J,K))
 10    CONTINUE
	endif

      DO 20 J=2,ny1-1
        K=2

        EYS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k))))*(
     $   EYS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EYSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X-(1.0
     $     -a1(idone14(2,j,k)))*xx2(idone14(2,j,k))+b0(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X-(1.0
     $     -a1(idone14(1,j,k)))*xx2(idone14(1,j,k))+b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $     *xx2(idone14(2,j,k))+b1(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k))
     $    *xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EYSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k)))
     $  )

        EYS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EYS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EYSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $    -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))+b0(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $    -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $     *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     $    *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EYSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k)))
     $  )

        K=nz1

	if (fansair=='y' .or. fansair=='Y') then
        
	  EYS(1,J,K)=EYSX1(2,J,K)+CXD*(EYS(2,J,K)-EYSX1(1,J,K))
        EYS(nx,J,K)=EYSX1(3,J,K)+CXU*(EYS(nx1,J,K)-EYSX1(4,J,K))
	
      else

        EYS(1,J,K)=(1.0/(xx1X-xx2(idone14(2,j,k))))*(
     $   EYS(2,J,K)*(xx1X+xx2(idone14(2,j,k)))
     $  +EYSX1(2,J,K)*((1.0+a1(idone14(2,j,k)))*xx1X-(1.0
     $     -a1(idone14(2,j,k)))*xx2(idone14(2,j,k))+b0(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX1(1,J,K)*(-(1.0+a1(idone14(1,j,k)))*xx1X-(1.0
     $     -a1(idone14(1,j,k)))*xx2(idone14(1,j,k))+b0(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX2(2,J,K)*(a1(idone14(2,j,k))*xx1X-a1(idone14(2,j,k))
     $    *xx2(idone14(2,j,k))+b1(idone14(2,j,k))
     $  *xx3(idone14(2,j,k)))
     $  +EYSX2(1,J,K)*(-a1(idone14(1,j,k))*xx1X-a1(idone14(1,j,k))
     $   *xx2(idone14(1,j,k))+b1(idone14(1,j,k))
     $  *xx3(idone14(1,j,k)))
     $  +EYSX3(2,J,K)*(b2(idone14(2,j,k))*xx3(idone14(2,j,k)))
     $  +EYSX3(1,J,K)*(b2(idone14(1,j,k))*xx3(idone14(1,j,k))))

        EYS(nx,J,K)=(1.0/(xx1X-xx2(idone14(nx1,j,k))))*(
     $   EYS(nx1,J,K)*(xx1X+xx2(idone14(nx1,j,k)))
     $  +EYSX1(3,J,K)*((1.0+a1(idone14(3,j,k)))*xx1X-(1.0
     $    -a1(idone14(3,j,k)))*xx2(idone14(3,j,k))+b0(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX1(4,J,K)*(-(1.0+a1(idone14(4,j,k)))*xx1X-(1.0
     $    -a1(idone14(4,j,k)))*xx2(idone14(4,j,k))+b0(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX2(3,J,K)*(a1(idone14(3,j,k))*xx1X-a1(idone14(3,j,k))
     $    *xx2(idone14(3,j,k))+b1(idone14(3,j,k))
     $  *xx3(idone14(3,j,k)))
     $  +EYSX2(4,J,K)*(-a1(idone14(4,j,k))*xx1X-a1(idone14(4,j,k))
     $     *xx2(idone14(4,j,k))+b1(idone14(4,j,k))
     $  *xx3(idone14(4,j,k)))
     $  +EYSX3(3,J,K)*(b2(idone14(3,j,k))*xx3(idone14(3,j,k)))
     $  +EYSX3(4,J,K)*(b2(idone14(4,j,k))*xx3(idone14(4,j,k))))

	endif


 20   CONTINUE


C   CALC 2ND ORDER ORBC ON REMAINING PORTIONS OF FACES

	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1-1
	endif

c      DO 140 K=3,nzsoil	   !SOIL
c      DO 140 K=3,nz1-1	   !SOIL
	do 140 k=3,nzquesf	   !SOIL

       DO 130 J=2,ny1-1

	EYS(1,J,K)=(1.0/(uu1X(idone14(2,j,k))-uu2(idone14(2,j,k))
     $	-b0(idone14(2,j,k))*uu3))*(
     $   EYS(2,J,K)*(uu1X(idone14(2,j,k))+uu2(idone14(2,j,k))
     $   +b0(idone14(2,j,k))*uu3)
     $  +EYSX1(2,J,K)*(a1(idone14(2,j,k))*uu1X(idone14(2,j,k))+(-2.0
     $     +a1(idone14(2,j,k)))
     $  *uu2(idone14(2,j,k))+(-b0(idone14(2,j,k))
     $    +b1(idone14(2,j,k)))*uu3)
     $  +EYSX1(1,J,K)*(-a1(idone14(1,j,k))*uu1X(idone14(1,j,k))
     $  +(-2.0+a1(idone14(1,j,k)))*uu2(idone14(1,j,k))
     $  +(-b0(idone14(1,j,k))+b1(idone14(1,j,k)))*uu3)
     $  +EYSX2(2,J,K)*(-uu1X(idone14(2,j,k))+(1-2*a1(idone14(2,j,k)))
     $   *uu2(idone14(2,j,k))
     $   +(-b1(idone14(2,j,k))+b2(idone14(2,j,k)))*uu3)
     $  +EYSX2(1,J,K)*(uu1X(idone14(1,j,k))+(1-2*a1(idone14(1,j,k)))
     $    *uu2(idone14(1,j,k))
     $   +(-b1(idone14(1,j,k))+b2(idone14(1,j,k)))*uu3)
     $  +EYSX3(2,J,K)*(-a1(idone14(2,j,k))*uu1X(idone14(2,j,k))
     $    +a1(idone14(2,j,k))*uu2(idone14(2,j,k))
     $   -b2(idone14(2,j,k))*uu3)
     $  +EYSX3(1,J,K)*(a1(idone14(1,j,k))*uu1X(idone14(1,j,k))
     $    +a1(idone14(1,j,k))*uu2(idone14(1,j,k))
     $   -b2(idone14(1,j,k))*uu3)
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 term:
     $     EYSX1(1,J+1,K)-2.*EYSX1(1,J,K)+EYSX1(1,J-1,K)
     $    +EYSX1(2,J+1,K)-2.*EYSX1(2,J,K)+EYSX1(2,J-1,K)
     $    +a1(idone14(2,j-1,k))*(
     $     EYSX2(1,J+1,K)-2.*EYSX2(1,J,K)+EYSX2(1,J-1,K)
     $    +EYSX2(2,J+1,K)-2.*EYSX2(2,J,K)+EYSX2(2,J-1,K)
     $     )
     $   ) !end of d/dy**2 term
     $  +(1.0/(2.0*DELZ**2))*( ! The d/dz**2 term:
     $     EYSX1(1,J,K+1)-2.*EYSX1(1,J,K)+EYSX1(1,J,K-1)
     $    +EYSX1(2,J,K+1)-2.*EYSX1(2,J,K)+EYSX1(2,J,K-1)
     $    +a1(idone14(2,j,k-1))*(
     $    +EYSX2(1,J,K+1)-2.*EYSX2(1,J,K)+EYSX2(1,J,K-1)
     $    +EYSX2(2,J,K+1)-2.*EYSX2(2,J,K)+EYSX2(2,J,K-1)
     $     )
     $   ) !end of d/dz**2 term
     $  )

	EYS(nx,J,K)=(1.0/(uu1X(idone14(nx1,j,k))-uu2(idone14(nx1,j,k))
     $   -b0(idone14(nx1,j,k))*uu3))*(
     $   EYS(nx1,J,K)*(uu1X(idone14(nx1,j,k))+uu2(idone14(nx1,j,k))
     $    +b0(idone14(nx1,j,k))*uu3)
     $  +EYSX1(3,J,K)*(a1(idone14(3,j,k))*uu1X(idone14(3,j,k))+(-2.0
     $    +a1(idone14(3,j,k)))
     $   *uu2(idone14(3,j,k))+(-b0(idone14(3,j,k))
     $   +b1(idone14(3,j,k)))*uu3)
     $  +EYSX1(4,J,K)*(-a1(idone14(4,j,k))*uu1X(idone14(4,j,k))+(-2.0
     $    +a1(idone14(4,j,k)))
     $  *uu2(idone14(4,j,k))+(-b0(idone14(4,j,k))
     $    +b1(idone14(4,j,k)))*uu3)
     $  +EYSX2(3,J,K)*(-uu1X(idone14(3,j,k))+(1-2*a1(idone14(3,j,k)))
     $   *uu2(idone14(3,j,k))+(-b1(idone14(3,j,k))
     $    +b2(idone14(3,j,k)))*uu3)
     $  +EYSX2(4,J,K)*(uu1X(idone14(4,j,k))+(1-2*a1(idone14(4,j,k)))
     $    *uu2(idone14(4,j,k))
     $   +(-b1(idone14(4,j,k))+b2(idone14(4,j,k)))*uu3)
     $  +EYSX3(3,J,K)*(-a1(idone14(3,j,k))*uu1X(idone14(3,j,k))
     $    +a1(idone14(3,j,k))*uu2(idone14(3,j,k))
     $   -b2(idone14(3,j,k))*uu3)
     $  +EYSX3(4,J,K)*(a1(idone14(4,j,k))*uu1X(idone14(4,j,k))
     $     +a1(idone14(4,j,k))*uu2(idone14(4,j,k))
     $    -b2(idone14(4,j,k))*uu3)
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 term:
     $     EYSX1(4,J+1,K)-2.*EYSX1(4,J,K)+EYSX1(4,J-1,K)
     $    +EYSX1(3,J+1,K)-2.*EYSX1(3,J,K)+EYSX1(3,J-1,K)
     $    +a1(idone14(3,j-1,k))*(
     $    +EYSX2(4,J+1,K)-2.*EYSX2(4,J,K)+EYSX2(4,J-1,K)
     $    +EYSX2(3,J+1,K)-2.*EYSX2(3,J,K)+EYSX2(3,J-1,K)
     $     )
     $   ) !end of d/dy**2 term
     $  +(1.0/(2.0*DELZ**2))*( ! The d/dz**2 term:
     $    +EYSX1(4,J,K+1)-2.*EYSX1(4,J,K)+EYSX1(4,J,K-1)
     $    +EYSX1(3,J,K+1)-2.*EYSX1(3,J,K)+EYSX1(3,J,K-1)
     $    +a1(idone14(3,j,k-1))*(
     $    +EYSX2(4,J,K+1)-2.*EYSX2(4,J,K)+EYSX2(4,J,K-1)
     $    +EYSX2(3,J,K+1)-2.*EYSX2(3,J,K)+EYSX2(3,J,K-1)
     $     )
     $   ) !end of d/dz**2 term
     $  )
130    CONTINUE
140	CONTINUE

	if (fansair=='y' .or. fansair=='Y') then

       DO 40 K=nzsoil+1,nz1-1	 !FREE SPACE
        DO 30 J=2,ny1-1
          EYS(1,J,K)=-EYSX2(2,J,K)+CXD*(EYS(2,J,K)+EYSX2(1,J,K))
     $    +CXX*(EYSX1(1,J,K)+EYSX1(2,J,K))
     $    +CXFYD*(EYSX1(1,J+1,K)-2.*EYSX1(1,J,K)+EYSX1(1,J-1,K)
     $    +EYSX1(2,J+1,K)-2.*EYSX1(2,J,K)+EYSX1(2,J-1,K))
     $    +CXFZD*(EYSX1(1,J,K+1)-2.*EYSX1(1,J,K)+EYSX1(1,J,K-1)
     $    +EYSX1(2,J,K+1)-2.*EYSX1(2,J,K)+EYSX1(2,J,K-1))
          EYS(nx,J,K)=-EYSX2(3,J,K)+CXD*(EYS(nx1 ,J,K)+EYSX2( 4,J,K))
     $    +CXX*(EYSX1(4,J,K)+EYSX1(3,J,K))
     $    +CXFYD*(EYSX1(4,J+1,K)-2.*EYSX1(4,J,K)+EYSX1(4,J-1,K)
     $    +EYSX1(3,J+1,K)-2.*EYSX1(3,J,K)+EYSX1(3,J-1,K))
     $    +CXFZD*(EYSX1(4,J,K+1)-2.*EYSX1(4,J,K)+EYSX1(4,J,K-1)
     $    +EYSX1(3,J,K+1)-2.*EYSX1(3,J,K)+EYSX1(3,J,K-1))
 30     CONTINUE
 40    CONTINUE
	endif

C   SAVE PAST VALUES
      DO 60 K=2,nz1
        DO 50 J=1,ny1
          EYSX3(1,J,K)=EYSX2(1,J,K)
          EYSX3(2,J,K)=EYSX2(2,J,K)
          EYSX3(3,J,K)=EYSX2(3,J,K)
          EYSX3(4,J,K)=EYSX2(4,J,K)
          EYSX2(1,J,K)=EYSX1(1,J,K)
          EYSX2(2,J,K)=EYSX1(2,J,K)
          EYSX2(3,J,K)=EYSX1(3,J,K)
          EYSX2(4,J,K)=EYSX1(4,J,K)
          EYSX1(1,J,K)=EYS(1,J,K)
          EYSX1(2,J,K)=EYS(2,J,K)
          EYSX1(3,J,K)=EYS(nx1,J,K)
          EYSX1(4,J,K)=EYS(nx,J,K)
 50     CONTINUE
 60   CONTINUE

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE RADEZY

      INCLUDE "soil_3d_params.for"


C   DO EDGES WITH FIRST ORDER ORBC

	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1
	endif

c      DO 110 K=1,nzsoil
c      DO 110 K=1,nz1
      DO 110 K=1,nzquesf
        I=2

        EZS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $  +EZS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EZSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $    -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $    -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EZSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $    *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $    *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EZSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EZSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k)))
     $  )

        EZS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EZS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EZSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $    -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))+b0(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EZSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $    -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $   *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EZSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $    *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EZSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k)))
     $  )

        I=nx1

        EZS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $   EZS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EZSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $     -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $    -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EZSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $   *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $   *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EZSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EZSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k)))
     $  )

        EZS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EZS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EZSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $   -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))+b0(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EZSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $    -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $    *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EZSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $    *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EZSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k)))
     $  )

110   CONTINUE

	if (fansair=='y' .or. fansair=='Y') then

       DO 10 K=nzsoil+1,nz1
        I=2
        EZS(I,1,K)=EZSY1(I,2,K)+CYD*(EZS(I,2,K)-EZSY1(I,1,K))
        EZS(I,ny,K)=EZSY1(I,3,K)+CYD*(EZS(I,ny1,K)-EZSY1(I,4,K))
        I=nx1
        EZS(I,1,K)=EZSY1(I,2,K)+CYD*(EZS(I,2,K)-EZSY1(I,1,K))
        EZS(I,ny,K)=EZSY1(I,3,K)+CYD*(EZS(I,ny1,K)-EZSY1(I,4,K))
 10    CONTINUE

	endif


      DO 20 I=3,nx1-1
        K=1

        EZS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $  +EZS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EZSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $    -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $   -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EZSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $    *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $    *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $   *xx3(idone14(i,1,k)))
     $  +EZSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EZSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k)))
     $  )

        EZS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EZS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EZSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $   -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))+b0(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EZSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $    -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $    *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EZSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $    *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EZSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k)))
     $  )

        K=nz1

	if (fansair=='y' .or. fansair=='Y') then
        EZS(I,1,K)=EZSY1(I,2,K)+CYD*(EZS(I,2,K)-EZSY1(I,1,K))
        EZS(I,ny,K)=EZSY1(I,3,K)+CYD*(EZS(I,ny1,K)-EZSY1(I,4,K))
	else
	  EZS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $  +EZS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EZSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $    -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $    -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EZSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $   *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EZSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $    *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EZSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EZSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k))))

        EZS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EZS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EZSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $   -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))+b0(idone14(i,3,k))
     $   *xx3(idone14(i,3,k)))
     $  +EZSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $    -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $     *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EZSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $    *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EZSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EZSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k))))
	endif

 20   CONTINUE


C   CALC 2ND ORDER ORBC ON REMAINING PORTIONS OF FACES


	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1-1
	endif


c      DO 140 K=2,nzsoil
c      DO 140 K=2,nz1-1
	do 140 k=2,nzquesf

        DO 130 I=3,nx1-1

	EZS(I,1,K)=(1.0/(uu1Y(idone14(i,2,k))-uu2(idone14(i,2,k))
     $     -b0(idone14(i,2,k))*uu3))*(
     $   EZS(I,2,K)*(uu1Y(idone14(i,2,k))+uu2(idone14(i,2,k))
     $     +b0(idone14(i,2,k))*uu3)
     $  +EZSY1(I,2,K)*(a1(idone14(i,2,k))*uu1Y(idone14(i,2,k))+(-2.0
     $    +a1(idone14(i,2,k)))
     $   *uu2(idone14(i,2,k))+(-b0(idone14(i,2,k))
     $     +b1(idone14(i,2,k)))*uu3)
     $  +EZSY1(I,1,K)*(-a1(idone14(i,1,k))*uu1Y(idone14(i,1,k))+(-2.0
     $    +a1(idone14(i,1,k)))
     $   *uu2(idone14(i,1,k))
     $   +(-b0(idone14(i,1,k))+b1(idone14(i,1,k)))*uu3)
     $  +EZSY2(I,2,K)*(-uu1Y(idone14(i,2,k))+(1-2*a1(idone14(i,2,k)))
     $    *uu2(idone14(i,2,k))
     $    +(-b1(idone14(i,2,k))+b2(idone14(i,2,k)))*uu3)
     $  +EZSY2(I,1,K)*(uu1Y(idone14(i,1,k))+(1-2*a1(idone14(i,1,k)))
     $    *uu2(idone14(i,1,k))
     $    +(-b1(idone14(i,1,k))+b2(idone14(i,1,k)))*uu3)
     $  +EZSY3(I,2,K)*(-a1(idone14(i,2,k))*uu1Y(idone14(i,2,k))
     $     +a1(idone14(i,2,k))*uu2(idone14(i,2,k))
     $    -b2(idone14(i,2,k))*uu3)
     $  +EZSY3(I,1,K)*(a1(idone14(i,1,k))*uu1Y(idone14(i,1,k))
     $    +a1(idone14(i,1,k))*uu2(idone14(i,1,k))
     $    -b2(idone14(i,1,k))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $     EZSY1(I+1,1,K)-2.*EZSY1(I,1,K)+EZSY1(I-1,1,K)
     $    +EZSY1(I+1,2,K)-2.*EZSY1(I,2,K)+EZSY1(I-1,2,K)
     $    +a1(idone14(i-1,2,k))*(
     $    +EZSY2(I+1,1,K)-2.*EZSY2(I,1,K)+EZSY2(I-1,1,K)
     $    +EZSY2(I+1,2,K)-2.*EZSY2(I,2,K)+EZSY2(I-1,2,K)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELZ**2))*( ! The d/dz**2 term:
     $    +EZSY1(I,1,K+1)-2.*EZSY1(I,1,K)+EZSY1(I,1,K-1)
     $    +EZSY1(I,2,K+1)-2.*EZSY1(I,2,K)+EZSY1(I,2,K-1)
     $    +a1(idone14(i,2,k-1))*(
     $    +EZSY2(I,1,K+1)-2.*EZSY2(I,1,K)+EZSY2(I,1,K-1)
     $    +EZSY2(I,2,K+1)-2.*EZSY2(I,2,K)+EZSY2(I,2,K-1)
     $     )
     $   ) ! end of d/dz**2 term
     $  )

	EZS(I,ny,K)=(1.0/(uu1Y(idone14(i,ny1,k))-uu2(idone14(i,ny1,k))
     $    -b0(idone14(i,ny1,k))*uu3))*(
     $   EZS(I,ny1,K)*(uu1Y(idone14(i,ny1,k))+uu2(idone14(i,ny1,k))
     $    +b0(idone14(i,ny1,k))*uu3)
     $  +EZSY1(I,3,K)*(a1(idone14(i,3,k))*uu1Y(idone14(i,3,k))+(-2.0
     $   +a1(idone14(i,3,k)))
     $    *uu2(idone14(i,3,k))+(-b0(idone14(i,3,k))
     $     +b1(idone14(i,3,k)))*uu3)
     $  +EZSY1(I,4,K)*(-a1(idone14(i,4,k))*uu1Y(idone14(i,4,k))+(-2.0
     $    +a1(idone14(i,4,k)))
     $    *uu2(idone14(i,4,k))
     $  +(-b0(idone14(i,4,k))+b1(idone14(i,4,k)))*uu3)
     $  +EZSY2(I,3,K)*(-uu1Y(idone14(i,3,k))+(1-2*a1(idone14(i,3,k)))
     $    *uu2(idone14(i,3,k))
     $   +(-b1(idone14(i,3,k))+b2(idone14(i,3,k)))*uu3)
     $  +EZSY2(I,4,K)*(uu1Y(idone14(i,4,k))+(1-2*a1(idone14(i,4,k)))
     $    *uu2(idone14(i,4,k))
     $    +(-b1(idone14(i,4,k))+b2(idone14(i,4,k)))*uu3)
     $  +EZSY3(I,3,K)*(-a1(idone14(i,3,k))*uu1Y(idone14(i,3,k))
     $    +a1(idone14(i,3,k))*uu2(idone14(i,3,k))
     $   -b2(idone14(i,3,k))*uu3)
     $  +EZSY3(I,4,K)*(a1(idone14(i,4,k))*uu1Y(idone14(i,4,k))
     $    +a1(idone14(i,4,k))*uu2(idone14(i,4,k))
     $  -b2(idone14(i,4,k))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $    +EZSY1(I+1,4,K)-2.*EZSY1(I,4,K)+EZSY1(I-1,4,K)
     $    +EZSY1(I+1,3,K)-2.*EZSY1(I,3,K)+EZSY1(I-1,3,K)
     $    +a1(idone14(i-1,3,k))*(
     $    +EZSY2(I+1,4,K)-2.*EZSY2(I,4,K)+EZSY2(I-1,4,K)
     $    +EZSY2(I+1,3,K)-2.*EZSY2(I,3,K)+EZSY2(I-1,3,K)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELZ**2))*( ! The d/dz**2 part:
     $    +EZSY1(I,4,K+1)-2.*EZSY1(I,4,K)+EZSY1(I,4,K-1)
     $    +EZSY1(I,3,K+1)-2.*EZSY1(I,3,K)+EZSY1(I,3,K-1)
     $    +a1(idone14(i,3,k-1))*(
     $    +EZSY2(I,4,K+1)-2.*EZSY2(I,4,K)+EZSY2(I,4,K-1)
     $    +EZSY2(I,3,K+1)-2.*EZSY2(I,3,K)+EZSY2(I,3,K-1)
     $     )
     $   ) ! end of d/dz**2 term   
     $  )

130    CONTINUE
140	CONTINUE

	if (fansair=='y' .or. fansair=='Y') then

       DO 40 K=nzsoil+1,nz1-1
        DO 30 I=3,nx1-1
          EZS(I,1,K)=-EZSY2(I,2,K)+CYD*(EZS(I,2,K)+EZSY2(I,1,K))
     $    +CYY*(EZSY1(I,1,K)+EZSY1(I,2,K))
     $    +CYFXD*(EZSY1(I+1,1,K)-2.*EZSY1(I,1,K)+EZSY1(I-1,1,K)
     $    +EZSY1(I+1,2,K)-2.*EZSY1(I,2,K)+EZSY1(I-1,2,K))
     $    +CYFZD*(EZSY1(I,1,K+1)-2.*EZSY1(I,1,K)+EZSY1(I,1,K-1)
     $    +EZSY1(I,2,K+1)-2.*EZSY1(I,2,K)+EZSY1(I,2,K-1))
          EZS(I,ny,K)=-EZSY2(I,3,K)+CYD*(EZS(I,ny1,K)+EZSY2(I,4,K))
     $    +CYY*(EZSY1(I,4,K)+EZSY1(I,3,K))
     $    +CYFXD*(EZSY1(I+1,4,K)-2.*EZSY1(I,4,K)+EZSY1(I-1,4,K)
     $    +EZSY1(I+1,3,K)-2.*EZSY1(I,3,K)+EZSY1(I-1,3,K))
     $    +CYFZD*(EZSY1(I,4,K+1)-2.*EZSY1(I,4,K)+EZSY1(I,4,K-1)
     $    +EZSY1(I,3,K+1)-2.*EZSY1(I,3,K)+EZSY1(I,3,K-1))
 30    CONTINUE
 40   CONTINUE

	endif

C   SAVE PAST VALUES
      DO 60 K=1,nz1
        DO 50 I=2,nx1
          EZSY3(I,1,K)=EZSY2(I,1,K)
          EZSY3(I,2,K)=EZSY2(I,2,K)
          EZSY3(I,3,K)=EZSY2(I,3,K)
          EZSY3(I,4,K)=EZSY2(I,4,K)
          EZSY2(I,1,K)=EZSY1(I,1,K)
          EZSY2(I,2,K)=EZSY1(I,2,K)
          EZSY2(I,3,K)=EZSY1(I,3,K)
          EZSY2(I,4,K)=EZSY1(I,4,K)
          EZSY1(I,1,K)=EZS(I,1,K)
          EZSY1(I,2,K)=EZS(I,2,K)
          EZSY1(I,3,K)=EZS(I,ny1,K)
          EZSY1(I,4,K)=EZS(I,ny,K)
 50     CONTINUE
 60   CONTINUE

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE RADEXY

      INCLUDE "soil_3d_params.for"

	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1
	endif


C   DO EDGES WITH FIRST ORDER ORBC
c      DO 110 K=2,nzsoil
c      DO 110 K=2,nz1
	 do 110 k=2,nzquesf

        I=1

        EXS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $   EXS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EXSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $   -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EXSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $    -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $    *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EXSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $    *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EXSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k)))
     $  )

        EXS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EXS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EXSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $    -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))
     $    +b0(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $   -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $   *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $   *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EXSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k)))
     $  )

        I=nx1

        EXS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $   EXS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EXSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $   -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EXSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $   -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $    *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EXSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $    *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EXSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k)))
     $  )
        EXS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EXS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EXSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $   -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))+b0(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $   -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $    *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $    *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EXSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k)))
     $  )

110   CONTINUE

	if (fansair=='y' .or. fansair=='Y') then

       DO 10 K=nzsoil+1,nz1
        I=1
        EXS(I,1,K)=EXSY1(I,2,K)+CYD*(EXS(I,2,K)-EXSY1(I,1,K))
        EXS(I,ny,K)=EXSY1(I,3,K)+CYD*(EXS(I,ny1,K)-EXSY1(I,4,K))
        I=nx1
        EXS(I,1,K)=EXSY1(I,2,K)+CYD*(EXS(I,2,K)-EXSY1(I,1,K))
        EXS(I,ny,K)=EXSY1(I,3,K)+CYD*(EXS(I,ny1,K)-EXSY1(I,4,K))
  10   CONTINUE

	endif

      DO 20 I=2,nx1-1

        K=2

        EXS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $   EXS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EXSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $    -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EXSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $   -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $    *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EXSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $    *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EXSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k)))
     $  )

        EXS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EXS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EXSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $   -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))+b0(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $   -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $    *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $   *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EXSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k)))
     $  )

	k=nz1

       if (fansair=='y' .or. fansair=='Y') then
        
	  EXS(I,1,K)=EXSY1(I,2,K)+CYD*(EXS(I,2,K)-EXSY1(I,1,K))
        EXS(I,ny,K)=EXSY1(I,3,K)+CYD*(EXS(I,ny1,K)-EXSY1(I,4,K))
	
       else

        EXS(I,1,K)=(1.0/(xx1Y-xx2(idone14(i,2,k))))*(
     $   EXS(I,2,K)*(xx1Y+xx2(idone14(i,2,k)))
     $  +EXSY1(I,2,K)*((1.0+a1(idone14(i,2,k)))*xx1Y-(1.0
     $   -a1(idone14(i,2,k)))*xx2(idone14(i,2,k))+b0(idone14(i,2,k))
     $   *xx3(idone14(i,2,k)))
     $  +EXSY1(I,1,K)*(-(1.0+a1(idone14(i,1,k)))*xx1Y-(1.0
     $  -a1(idone14(i,1,k)))*xx2(idone14(i,1,k))+b0(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY2(I,2,K)*(a1(idone14(i,2,k))*xx1Y-a1(idone14(i,2,k))
     $   *xx2(idone14(i,2,k))+b1(idone14(i,2,k))
     $  *xx3(idone14(i,2,k)))
     $  +EXSY2(I,1,K)*(-a1(idone14(i,1,k))*xx1Y-a1(idone14(i,1,k))
     $   *xx2(idone14(i,1,k))+b1(idone14(i,1,k))
     $  *xx3(idone14(i,1,k)))
     $  +EXSY3(I,2,K)*(b2(idone14(i,2,k))*xx3(idone14(i,2,k)))
     $  +EXSY3(I,1,K)*(b2(idone14(i,1,k))*xx3(idone14(i,1,k))))

        EXS(I,ny,K)=(1.0/(xx1Y-xx2(idone14(i,ny1,k))))*(
     $   EXS(I,ny1,K)*(xx1Y+xx2(idone14(i,ny1,k)))
     $  +EXSY1(I,3,K)*((1.0+a1(idone14(i,3,k)))*xx1Y-(1.0
     $   -a1(idone14(i,3,k)))*xx2(idone14(i,3,k))+b0(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY1(I,4,K)*(-(1.0+a1(idone14(i,4,k)))*xx1Y-(1.0
     $   -a1(idone14(i,4,k)))*xx2(idone14(i,4,k))+b0(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY2(I,3,K)*(a1(idone14(i,3,k))*xx1Y-a1(idone14(i,3,k))
     $   *xx2(idone14(i,3,k))+b1(idone14(i,3,k))
     $  *xx3(idone14(i,3,k)))
     $  +EXSY2(I,4,K)*(-a1(idone14(i,4,k))*xx1Y-a1(idone14(i,4,k))
     $   *xx2(idone14(i,4,k))+b1(idone14(i,4,k))
     $  *xx3(idone14(i,4,k)))
     $  +EXSY3(I,3,K)*(b2(idone14(i,3,k))*xx3(idone14(i,3,k)))
     $  +EXSY3(I,4,K)*(b2(idone14(i,4,k))*xx3(idone14(i,4,k))))

	 endif

 20   CONTINUE


C   CALC 2ND ORDER ORBC ON REMAINING PORTIONS OF FACES

	if (fansair=='y' .or. fansair=='Y') then
		nzquesf=nzsoil
	else
		nzquesf=nz1-1
	endif

c      DO 140 K=3,nzsoil
c      DO 140 K=3,nz1-1
	 do 130 k=3,nzquesf
	 
        DO 130 I=2,nx1-1

	EXS(I,1,K)=(1.0/(uu1Y(idone14(i,2,k))-uu2(idone14(i,2,k))
     $   -b0(idone14(i,2,k))*uu3))*(
     $   EXS(I,2,K)*(uu1Y(idone14(i,2,k))+uu2(idone14(i,2,k))
     $    +b0(idone14(i,2,k))*uu3)
     $  +EXSY1(I,2,K)*(a1(idone14(i,2,k))*uu1Y(idone14(i,2,k))+(-2.0
     $    +a1(idone14(i,2,k)))
     $   *uu2(idone14(i,2,k))+(-b0(idone14(i,2,k))
     $     +b1(idone14(i,2,k)))*uu3)
     $  +EXSY1(I,1,K)*(-a1(idone14(i,1,k))*uu1Y(idone14(i,1,k))+(-2.0
     $   +a1(idone14(i,1,k)))
     $   *uu2(idone14(i,1,k))
     $  +(-b0(idone14(i,1,k))+b1(idone14(i,1,k)))*uu3)
     $  +EXSY2(I,2,K)*(-uu1Y(idone14(i,2,k))+(1-2*a1(idone14(i,2,k)))
     $    *uu2(idone14(i,2,k))+(-b1(idone14(i,2,k))
     $     +b2(idone14(i,2,k)))*uu3)
     $  +EXSY2(I,1,K)*(uu1Y(idone14(i,1,k))+(1-2*a1(idone14(i,1,k)))
     $   *uu2(idone14(i,1,k))
     $    +(-b1(idone14(i,1,k))+b2(idone14(i,1,k)))*uu3)
     $  +EXSY3(I,2,K)*(-a1(idone14(i,2,k))*uu1Y(idone14(i,2,k))
     $    +a1(idone14(i,2,k))*uu2(idone14(i,2,k))
     $    -b2(idone14(i,2,k))*uu3)
     $  +EXSY3(I,1,K)*(a1(idone14(i,1,k))*uu1Y(idone14(i,1,k))
     $    +a1(idone14(i,1,k))*uu2(idone14(i,1,k))
     $    -b2(idone14(i,1,k))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $    +EXSY1(I+1,1,K)-2.*EXSY1(I,1,K)+EXSY1(I-1,1,K)
     $    +EXSY1(I+1,2,K)-2.*EXSY1(I,2,K)+EXSY1(I-1,2,K)
     $    +a1(idone14(i-1,2,k))*(
     $    +EXSY2(I+1,1,K)-2.*EXSY2(I,1,K)+EXSY2(I-1,1,K)
     $    +EXSY2(I+1,2,K)-2.*EXSY2(I,2,K)+EXSY2(I-1,2,K)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELZ**2))*( ! The d/dz**2 term:
     $    +EXSY1(I,1,K+1)-2.*EXSY1(I,1,K)+EXSY1(I,1,K-1)
     $    +EXSY1(I,2,K+1)-2.*EXSY1(I,2,K)+EXSY1(I,2,K-1)
     $    +a1(idone14(i,2,k-1))*(
     $    +EXSY2(I,1,K+1)-2.*EXSY2(I,1,K)+EXSY2(I,1,K-1)
     $    +EXSY2(I,2,K+1)-2.*EXSY2(I,2,K)+EXSY2(I,2,K-1)
     $     )
     $   ) ! end of d/dz**2 term
     $  )

	EXS(I,ny,K)=(1.0/(uu1Y(idone14(i,ny1,k))-uu2(idone14(i,ny1,k))
     $    -b0(idone14(i,ny1,k))*uu3))*(
     $   EXS(I,ny1,K)*(uu1Y(idone14(i,ny1,k))+uu2(idone14(i,ny1,k))
     $    +b0(idone14(i,ny1,k))*uu3)
     $  +EXSY1(I,3,K)*(a1(idone14(i,3,k))*uu1Y(idone14(i,3,k))+(-2.0
     $    +a1(idone14(i,3,k)))
     $   *uu2(idone14(i,3,k))+(-b0(idone14(i,3,k))
     $    +b1(idone14(i,3,k)))*uu3)
     $  +EXSY1(I,4,K)*(-a1(idone14(i,4,k))*uu1Y(idone14(i,4,k))+(-2.0
     $    +a1(idone14(i,4,k)))
     $   *uu2(idone14(i,4,k))
     $   +(-b0(idone14(i,4,k))+b1(idone14(i,4,k)))*uu3)
     $  +EXSY2(I,3,K)*(-uu1Y(idone14(i,3,k))+(1-2*a1(idone14(i,3,k)))
     $   *uu2(idone14(i,3,k))+(-b1(idone14(i,3,k))
     $    +b2(idone14(i,3,k)))*uu3)
     $  +EXSY2(I,4,K)*(uu1Y(idone14(i,4,k))+(1-2*a1(idone14(i,4,k)))
     $   *uu2(idone14(i,4,k))+(-b1(idone14(i,4,k))
     $     +b2(idone14(i,4,k)))*uu3)
     $  +EXSY3(I,3,K)*(-a1(idone14(i,3,k))*uu1Y(idone14(i,3,k))
     $    +a1(idone14(i,3,k))*uu2(idone14(i,3,k))
     $    -b2(idone14(i,3,k))*uu3)
     $  +EXSY3(I,4,K)*(a1(idone14(i,4,k))*uu1Y(idone14(i,4,k))
     $    +a1(idone14(i,4,k))*uu2(idone14(i,4,k))
     $    -b2(idone14(i,4,k))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $    +EXSY1(I+1,4,K)-2.*EXSY1(I,4,K)+EXSY1(I-1,4,K)
     $    +EXSY1(I+1,3,K)-2.*EXSY1(I,3,K)+EXSY1(I-1,3,K)
     $    +a1(idone14(i-1,3,k))*(
     $    +EXSY2(I+1,4,K)-2.*EXSY2(I,4,K)+EXSY2(I-1,4,K)
     $    +EXSY2(I+1,3,K)-2.*EXSY2(I,3,K)+EXSY2(I-1,3,K)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELZ**2))*( ! The d/dz**2 term:
     $    +EXSY1(I,4,K+1)-2.*EXSY1(I,4,K)+EXSY1(I,4,K-1)
     $    +EXSY1(I,3,K+1)-2.*EXSY1(I,3,K)+EXSY1(I,3,K-1)
     $    +a1(idone14(i,3,k-1))*(
     $    +EXSY2(I,4,K+1)-2.*EXSY2(I,4,K)+EXSY2(I,4,K-1)
     $    +EXSY2(I,3,K+1)-2.*EXSY2(I,3,K)+EXSY2(I,3,K-1)
     $     )
     $   ) ! end of d/dz**2 term
     $  )

130    CONTINUE
140	CONTINUE

	if (fansair=='y' .or. fansair=='Y') then

       DO 40 K=nzsoil+1,nz1-1
        DO 30 I=2,nx1-1
          EXS(I,1,K)=-EXSY2(I,2,K)+CYD*(EXS(I,2,K)+EXSY2(I,1,K))
     $    +CYY*(EXSY1(I,1,K)+EXSY1(I,2,K))
     $    +CYFXD*(EXSY1(I+1,1,K)-2.*EXSY1(I,1,K)+EXSY1(I-1,1,K)
     $    +EXSY1(I+1,2,K)-2.*EXSY1(I,2,K)+EXSY1(I-1,2,K))
     $    +CYFZD*(EXSY1(I,1,K+1)-2.*EXSY1(I,1,K)+EXSY1(I,1,K-1)
     $    +EXSY1(I,2,K+1)-2.*EXSY1(I,2,K)+EXSY1(I,2,K-1))
          EXS(I,ny,K)=-EXSY2(I,3,K)+CYD*(EXS(I,ny1,K)+EXSY2(I,4,K))
     $    +CYY*(EXSY1(I,4,K)+EXSY1(I,3,K))
     $    +CYFXD*(EXSY1(I+1,4,K)-2.*EXSY1(I,4,K)+EXSY1(I-1,4,K)
     $    +EXSY1(I+1,3,K)-2.*EXSY1(I,3,K)+EXSY1(I-1,3,K))
     $    +CYFZD*(EXSY1(I,4,K+1)-2.*EXSY1(I,4,K)+EXSY1(I,4,K-1)
     $    +EXSY1(I,3,K+1)-2.*EXSY1(I,3,K)+EXSY1(I,3,K-1))
 30     CONTINUE
 40    CONTINUE

	endif



C   SAVE PAST VALUES
      DO 60 K=2,nz1
        DO 50 I=1,nx1
          EXSY3(I,1,K)=EXSY2(I,1,K)
          EXSY3(I,2,K)=EXSY2(I,2,K)
          EXSY3(I,3,K)=EXSY2(I,3,K)
          EXSY3(I,4,K)=EXSY2(I,4,K)
          EXSY2(I,1,K)=EXSY1(I,1,K)
          EXSY2(I,2,K)=EXSY1(I,2,K)
          EXSY2(I,3,K)=EXSY1(I,3,K)
          EXSY2(I,4,K)=EXSY1(I,4,K)
          EXSY1(I,1,K)=EXS(I,1,K)
          EXSY1(I,2,K)=EXS(I,2,K)
          EXSY1(I,3,K)=EXS(I,ny1,K)
          EXSY1(I,4,K)=EXS(I,ny,K)
 50     CONTINUE
 60   CONTINUE

      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE RADEXZ

      INCLUDE "soil_3d_params.for"


C     DO EDGES WITH FIRST ORDER ORBC

      DO 10 J=2,ny1
        I=1

        EXS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $   EXS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EXSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $    -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))
     $    +b0(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EXSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $   -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))+b0(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EXSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $    *xx2(idone14(i,j,2))+b1(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EXSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $    *xx2(idone14(i,j,1))+b1(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EXSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EXSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )

	if (fansair=='y' .or. fansair=='Y') then
        
	  EXS(I,J,nz)=EXSZ1(I,J,3)+CZD*(EXS(I,J,nz1)-EXSZ1(I,J,4))
	
      else

        EXS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $   EXS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EXSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $    -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))+b0(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $   -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))+b0(idone14(i,j,4))
     $  *xx3(idone14(i,j,4)))
     $  +EXSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $    *xx2(idone14(i,j,3))+b1(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $    *xx2(idone14(i,j,4))+b1(idone14(i,j,4))
     $  *xx3(idone14(i,j,4)))
     $  +EXSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EXSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4))))

	endif


        I=nx1

        EXS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $   EXS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EXSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $   -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))+b0(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EXSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $    -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))+b0(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EXSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $   *xx2(idone14(i,j,2))+b1(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EXSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $   *xx2(idone14(i,j,1))+b1(idone14(i,j,1))
     $   *xx3(idone14(i,j,1)))
     $  +EXSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EXSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )

	if (fansair=='y' .or. fansair=='Y') then

        EXS(I,J,nz)=EXSZ1(I,J,3)+CZD*(EXS(I,J,nz1)-EXSZ1(I,J,4))

	else

        EXS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $   EXS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EXSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $   -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))+b0(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $   -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))+b0(idone14(i,j,4))
     $  *xx3(idone14(i,j,4)))
     $  +EXSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $   *xx2(idone14(i,j,3))+b1(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $   *xx2(idone14(i,j,4))+b1(idone14(i,j,4))
     $  *xx3(idone14(i,j,4)))
     $  +EXSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EXSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4))))
	endif

 10   CONTINUE

      DO 20 I=2,nx1-1

        J=2

        EXS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $   EXS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EXSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $   -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))+b0(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EXSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $   -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))+b0(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EXSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $   *xx2(idone14(i,j,2))+b1(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EXSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $   *xx2(idone14(i,j,1))+b1(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EXSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EXSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )

	if (fansair=='y' .or. fansair=='Y') then

        EXS(I,J,nz)=EXSZ1(I,J,3)+CZD*(EXS(I,J,nz1)-EXSZ1(I,J,4))

	else

        EXS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $   EXS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EXSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $   -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))+b0(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $   -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))+b0(idone14(i,j,4))
     $  *xx3(idone14(i,j,4)))
     $  +EXSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $   *xx2(idone14(i,j,3))+b1(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $   *xx2(idone14(i,j,4))+b1(idone14(i,j,4))
     $  *xx3(idone14(i,j,4)))
     $  +EXSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EXSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  )

	endif



        J=ny1

        EXS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $   EXS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EXSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $   -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))+b0(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EXSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $   -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))+b0(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EXSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $   *xx2(idone14(i,j,2))+b1(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EXSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $   *xx2(idone14(i,j,1))+b1(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EXSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EXSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )

	if (fansair=='y' .or. fansair=='Y') then

        EXS(I,J,nz)=EXSZ1(I,J,3)+CZD*(EXS(I,J,nz1)-EXSZ1(I,J,4))

	else

        EXS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $   EXS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EXSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $   -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))+b0(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $   -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))+b0(idone14(i,j,4))
     $   *xx3(idone14(i,j,4)))
     $  +EXSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $   *xx2(idone14(i,j,3))+b1(idone14(i,j,3))
     $  *xx3(idone14(i,j,3)))
     $  +EXSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $   *xx2(idone14(i,j,4))+b1(idone14(i,j,4))
     $  *xx3(idone14(i,j,4)))
     $  +EXSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EXSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  )

	endif


 20   CONTINUE


C   CALC 2ND ORDER ORBC ON REMAINING PORTIONS OF FACES

      DO 40 J=3,ny1-1
        DO 30 I=2,nx1-1

	EXS(I,J,1)=(1.0/(uu1Z(idone14(i,j,2))-uu2(idone14(i,j,2))
     $    -b0(idone14(i,j,2))*uu3))*(
     $  +EXS(I,J,2)*(uu1Z(idone14(i,j,2))+uu2(idone14(i,j,2))
     $   +b0(idone14(i,j,2))*uu3)
     $  +EXSZ1(I,J,2)*(a1(idone14(i,j,2))*uu1Z(idone14(i,j,2))+(-2.0
     $    +a1(idone14(i,j,2)))
     $   *uu2(idone14(i,j,2))+(-b0(idone14(i,j,2))
     $      +b1(idone14(i,j,2)))*uu3)
     $  +EXSZ1(I,J,1)*(-a1(idone14(i,j,1))*uu1Z(idone14(i,j,1))+(-2.0
     $    +a1(idone14(i,j,1)))
     $   *uu2(idone14(i,j,1))
     $   +(-b0(idone14(i,j,1))+b1(idone14(i,j,1)))*uu3)
     $  +EXSZ2(I,J,2)*(-uu1Z(idone14(i,j,2))+(1-2*a1(idone14(i,j,2)))
     $    *uu2(idone14(i,j,2))+(-b1(idone14(i,j,2))
     $     +b2(idone14(i,j,2)))*uu3)
     $  +EXSZ2(I,J,1)*(uu1Z(idone14(i,j,1))+(1-2*a1(idone14(i,j,1)))
     $   *uu2(idone14(i,j,1))+(-b1(idone14(i,j,1))
     $    +b2(idone14(i,j,1)))*uu3)
     $  +EXSZ3(I,J,2)*(-a1(idone14(i,j,2))*uu1Z(idone14(i,j,2))
     $   +a1(idone14(i,j,2))*uu2(idone14(i,j,2))
     $      -b2(idone14(i,j,2))*uu3)
     $  +EXSZ3(I,J,1)*(a1(idone14(i,j,1))*uu1Z(idone14(i,j,1))
     $   +a1(idone14(i,j,1))*uu2(idone14(i,j,1))
     $     -b2(idone14(i,j,1))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $    +EXSZ1(I+1,J,1)-2.*EXSZ1(I,J,1)+EXSZ1(I-1,J,1)
     $    +EXSZ1(I+1,J,2)-2.*EXSZ1(I,J,2)+EXSZ1(I-1,J,2)
     $    +a1(idone14(i-1,j,2))*(
     $    +EXSZ2(I+1,J,1)-2.*EXSZ2(I,J,1)+EXSZ2(I-1,J,1)
     $    +EXSZ2(I+1,J,2)-2.*EXSZ2(I,J,2)+EXSZ2(I-1,J,2)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 term:
     $    +EXSZ1(I,J+1,1)-2.*EXSZ1(I,J,1)+EXSZ1(I,J-1,1)
     $    +EXSZ1(I,J+1,2)-2.*EXSZ1(I,J,2)+EXSZ1(I,J-1,2)
     $    +a1(idone14(i,j-1,2))*(
     $    +EXSZ2(I,J+1,1)-2.*EXSZ2(I,J,1)+EXSZ2(I,J-1,1)
     $    +EXSZ2(I,J+1,2)-2.*EXSZ2(I,J,2)+EXSZ2(I,J-1,2)
     $     )
     $   ) ! end of d/dy**2 term
     $ )


	if (fansair=='y' .or. fansair=='Y') then

         EXS(I,J,nz)=-EXSZ2(I,J,3)+CZD*(EXS(I,J,nz1)+EXSZ2(I,J,4))
     $   +CZZ*(EXSZ1(I,J,4)+EXSZ1(I,J,3))
     $   +CZFXD*(EXSZ1(I+1,J,4)-2.*EXSZ1(I,J,4)+EXSZ1(I-1,J,4)
     $   +EXSZ1(I+1,J,3)-2.*EXSZ1(I,J,3)+EXSZ1(I-1,J,3))
     $   +CZFYD*(EXSZ1(I,J+1,4)-2.*EXSZ1(I,J,4)+EXSZ1(I,J-1,4)
     $   +EXSZ1(I,J+1,3)-2.*EXSZ1(I,J,3)+EXSZ1(I,J-1,3))

	else

	  EXS(I,J,nz)=(1.0/(uu1Z(idone14(i,j,nz1))-uu2(idone14(i,j,nz1))
     $      -b0(idone14(i,j,nz1))*uu3))*(
     $  +EXS(I,J,nz1)*(uu1Z(idone14(i,j,nz1))+uu2(idone14(i,j,nz1))
     $    +b0(idone14(i,j,nz1))*uu3)
     $  +EXSZ1(I,J,3)*(a1(idone14(i,j,3))*uu1Z(idone14(i,j,3))+(-2.0
     $   +a1(idone14(i,j,3)))
     $    *uu2(idone14(i,j,3))+(-b0(idone14(i,j,3))
     $    +b1(idone14(i,j,3)))*uu3)
     $  +EXSZ1(I,J,4)*(-a1(idone14(i,j,4))*uu1Z(idone14(i,j,4))+(-2.0
     $    +a1(idone14(i,j,4)))
     $     *uu2(idone14(i,j,4))
     $   +(-b0(idone14(i,j,4))+b1(idone14(i,j,4)))*uu3)
     $  +EXSZ2(I,J,3)*(-uu1Z(idone14(i,j,3))+(1-2*a1(idone14(i,j,3)))
     $       *uu2(idone14(i,j,3))+(-b1(idone14(i,j,3))
     $     +b2(idone14(i,j,3)))
     $   *uu3)
     $  +EXSZ2(I,J,4)*(uu1Z(idone14(i,j,4))+(1-2*a1(idone14(i,j,4)))
     $      *uu2(idone14(i,j,4))+(-b1(idone14(i,j,4))
     $     +b2(idone14(i,j,4)))
     $    *uu3)
     $  +EXSZ3(I,J,3)*(-a1(idone14(i,j,3))*uu1Z(idone14(i,j,3))
     $   +a1(idone14(i,j,3))*uu2(idone14(i,j,3))
     $     -b2(idone14(i,j,3))*uu3)
     $  +EXSZ3(I,J,4)*(a1(idone14(i,j,4))*uu1Z(idone14(i,j,4))
     $   +a1(idone14(i,j,4))*uu2(idone14(i,j,4))
     $     -b2(idone14(i,j,4))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $    +EXSZ1(I+1,J,4)-2.*EXSZ1(I,J,4)+EXSZ1(I-1,J,4)
     $    +EXSZ1(I+1,J,3)-2.*EXSZ1(I,J,3)+EXSZ1(I-1,J,3)
     $    +a1(idone14(i-1,j,3))*(
     $    +EXSZ2(I+1,J,4)-2.*EXSZ2(I,J,4)+EXSZ2(I-1,J,4)
     $    +EXSZ2(I+1,J,3)-2.*EXSZ2(I,J,3)+EXSZ2(I-1,J,3)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 term:
     $    +EXSZ1(I,J+1,4)-2.*EXSZ1(I,J,1)+EXSZ1(I,J-1,1)
     $    +EXSZ1(I,J+1,3)-2.*EXSZ1(I,J,2)+EXSZ1(I,J-1,2)
     $    +a1(idone14(i,j-1,2))*(
     $    +EXSZ2(I,J+1,4)-2.*EXSZ2(I,J,1)+EXSZ2(I,J-1,1)
     $    +EXSZ2(I,J+1,3)-2.*EXSZ2(I,J,2)+EXSZ2(I,J-1,2)
     $     )
     $   ) ! end of d/dy**2 term
     $ )

	endif


 30     CONTINUE
 40   CONTINUE


C   SAVE PAST VALUES

      DO 60 J=2,ny1
        DO 50 I=1,nx1
          EXSZ3(I,J,1)=EXSZ2(I,J,1)
          EXSZ3(I,J,2)=EXSZ2(I,J,2)
          EXSZ3(I,J,3)=EXSZ2(I,J,3)
          EXSZ3(I,J,4)=EXSZ2(I,J,4)
          EXSZ2(I,J,1)=EXSZ1(I,J,1)
          EXSZ2(I,J,2)=EXSZ1(I,J,2)
          EXSZ2(I,J,3)=EXSZ1(I,J,3)
          EXSZ2(I,J,4)=EXSZ1(I,J,4)
          EXSZ1(I,J,1)=EXS(I,J,1)
          EXSZ1(I,J,2)=EXS(I,J,2)
          EXSZ1(I,J,3)=EXS(I,J,nz1)
          EXSZ1(I,J,4)=EXS(I,J,nz)

 50     CONTINUE
60    CONTINUE

      RETURN

      END


C*************************************************************
C*************************************************************
      SUBROUTINE RADEYZ

      INCLUDE "soil_3d_params.for"


C     DO EDGES WITH FIRST ORDER ORBC

      DO 10 J=1,ny1

        I=2

        EYS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $  +EYS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EYSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $   -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))+b0(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EYSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $   -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))+b0(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EYSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $   *xx2(idone14(i,j,2))+b1(idone14(i,j,2))
     $  *xx3(idone14(i,j,2)))
     $  +EYSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $    *xx2(idone14(i,j,1))+b1(idone14(i,j,1))
     $  *xx3(idone14(i,j,1)))
     $  +EYSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )


	if (fansair=='y' .or. fansair=='Y') then

        EYS(I,J,nz)=EYSZ1(I,J,3)+CZD*(EYS(I,J,nz1)-EYSZ1(I,J,4))

	else

        EYS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $  +EYS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EYSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $    -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))
     $  +b0(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $    -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))
     $   +b0(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $    *xx2(idone14(i,j,3))
     $  +b1(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $   *xx2(idone14(i,j,4))
     $  +b1(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  )

	endif


        I=nx1

        EYS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $  +EYS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EYSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $    -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))
     $   +b0(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $   -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))
     $  +b0(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  +EYSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $    *xx2(idone14(i,j,2))
     $   +b1(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $    *xx2(idone14(i,j,1))
     $   +b1(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  +EYSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )


	if (fansair=='y' .or. fansair=='Y') then

        EYS(I,J,nz)=EYSZ1(I,J,3)+CZD*(EYS(I,J,nz1)-EYSZ1(I,J,4))

	else

        EYS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $  +EYS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EYSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $   -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))
     $   +b0(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $   -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))
     $  +b0(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $   *xx2(idone14(i,j,3))
     $  +b1(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $   *xx2(idone14(i,j,4))
     $  +b1(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  )

	endif


 10   CONTINUE

      DO 20 I=3,nx1-1

       J=1

        EYS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $  +EYS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EYSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $    -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))
     $  +b0(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $    -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))
     $  +b0(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  +EYSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $    *xx2(idone14(i,j,2))
     $  +b1(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $    *xx2(idone14(i,j,1))
     $  +b1(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  +EYSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )


	if (fansair=='y' .or. fansair=='Y') then

        EYS(I,J,nz)=EYSZ1(I,J,3)+CZD*(EYS(I,J,nz1)-EYSZ1(I,J,4))

	else

        EYS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $  +EYS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EYSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $    -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))
     $  +b0(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $    -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))
     $   +b0(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $   *xx2(idone14(i,j,3))
     $   +b1(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $   *xx2(idone14(i,j,4))
     $   +b1(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  )

	endif


        J=ny1

        EYS(I,J,1)=(1.0/(xx1Z-xx2(idone14(i,j,2))))*(
     $  +EYS(I,J,2)*(xx1Z+xx2(idone14(i,j,2)))
     $  +EYSZ1(I,J,2)*((1.0+a1(idone14(i,j,2)))*xx1Z-(1.0
     $    -a1(idone14(i,j,2)))*xx2(idone14(i,j,2))
     $  +b0(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ1(I,J,1)*(-(1.0+a1(idone14(i,j,1)))*xx1Z-(1.0
     $   -a1(idone14(i,j,1)))*xx2(idone14(i,j,1))
     $  +b0(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  +EYSZ2(I,J,2)*(a1(idone14(i,j,2))*xx1Z-a1(idone14(i,j,2))
     $   *xx2(idone14(i,j,2))
     $  +b1(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ2(I,J,1)*(-a1(idone14(i,j,1))*xx1Z-a1(idone14(i,j,1))
     $    *xx2(idone14(i,j,1))
     $   +b1(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  +EYSZ3(I,J,2)*(b2(idone14(i,j,2))*xx3(idone14(i,j,2)))
     $  +EYSZ3(I,J,1)*(b2(idone14(i,j,1))*xx3(idone14(i,j,1)))
     $  )

	if (fansair=='y' .or. fansair=='Y') then

        EYS(I,J,nz)=EYSZ1(I,J,3)+CZD*(EYS(I,J,nz1)-EYSZ1(I,J,4))

	else

        EYS(I,J,nz)=(1.0/(xx1Z-xx2(idone14(i,j,nz1))))*(
     $  +EYS(I,J,nz1)*(xx1Z+xx2(idone14(i,j,nz1)))
     $  +EYSZ1(I,J,3)*((1.0+a1(idone14(i,j,3)))*xx1Z-(1.0
     $   -a1(idone14(i,j,3)))*xx2(idone14(i,j,3))
     $  +b0(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ1(I,J,4)*(-(1.0+a1(idone14(i,j,4)))*xx1Z-(1.0
     $    -a1(idone14(i,j,4)))*xx2(idone14(i,j,4))
     $  +b0(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ2(I,J,3)*(a1(idone14(i,j,3))*xx1Z-a1(idone14(i,j,3))
     $   *xx2(idone14(i,j,3))
     $  +b1(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ2(I,J,4)*(-a1(idone14(i,j,4))*xx1Z-a1(idone14(i,j,4))
     $   *xx2(idone14(i,j,4))
     $   +b1(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  +EYSZ3(I,J,3)*(b2(idone14(i,j,3))*xx3(idone14(i,j,3)))
     $  +EYSZ3(I,J,4)*(b2(idone14(i,j,4))*xx3(idone14(i,j,4)))
     $  )

	endif


 20   CONTINUE

C   CALC 2ND ORDER ORBC ON REMAINING PORTIONS OF FACES
c ta injast babam jan
      DO 40 J=2,ny1-1
        DO 30 I=3,nx1-1

	EYS(I,J,1)=(1.0/(uu1Z(idone14(i,j,2))-uu2(idone14(i,j,2))
     $	-b0(idone14(i,j,2))*uu3))*(
     $  +EYS(I,J,2)*(uu1Z(idone14(i,j,2))+uu2(idone14(i,j,2))
     $    +b0(idone14(i,j,2))*uu3)
     $  +EYSZ1(I,J,2)*(a1(idone14(i,j,2))*uu1Z(idone14(i,j,2))+(-2.0
     $   +a1(idone14(i,j,2)))
     $     *uu2(idone14(i,j,2))+(-b0(idone14(i,j,2))
     $     +b1(idone14(i,j,2)))*uu3)
     $  +EYSZ1(I,J,1)*(-a1(idone14(i,j,1))*uu1Z(idone14(i,j,1))+(-2.0
     $   +a1(idone14(i,j,1)))
     $      *uu2(idone14(i,j,1))
     $   +(-b0(idone14(i,j,1))+b1(idone14(i,j,1)))*uu3)
     $  +EYSZ2(I,J,2)*(-uu1Z(idone14(i,j,2))+(1-2*a1(idone14(i,j,2)))
     $     *uu2(idone14(i,j,2))+(-b1(idone14(i,j,2))
     $    +b2(idone14(i,j,2)))*uu3)
     $  +EYSZ2(I,J,1)*(uu1Z(idone14(i,j,1))+(1-2*a1(idone14(i,j,1)))
     $      *uu2(idone14(i,j,1))+(-b1(idone14(i,j,1))
     $    +b2(idone14(i,j,1)))*uu3)
     $  +EYSZ3(I,J,2)*(-a1(idone14(i,j,2))*uu1Z(idone14(i,j,2))
     $   +a1(idone14(i,j,2))*uu2(idone14(i,j,2))
     $    -b2(idone14(i,j,2))*uu3)
     $  +EYSZ3(I,J,1)*(a1(idone14(i,j,1))*uu1Z(idone14(i,j,1))
     $   +a1(idone14(i,j,1))*uu2(idone14(i,j,1))
     $      -b2(idone14(i,j,1))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $     EYSZ1(I+1,J,1)-2.*EYSZ1(I,J,1)+EYSZ1(I-1,J,1)
     $    +EYSZ1(I+1,J,2)-2.*EYSZ1(I,J,2)+EYSZ1(I-1,J,2)
     $    +a1(idone14(i-1,j,2))*(
     $    +EYSZ2(I+1,J,1)-2.*EYSZ2(I,J,1)+EYSZ2(I-1,J,1)
     $    +EYSZ2(I+1,J,2)-2.*EYSZ2(I,J,2)+EYSZ2(I-1,J,2)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 part:
     $     EYSZ1(I,J+1,1)-2.*EYSZ1(I,J,1)+EYSZ1(I,J-1,1)
     $    +EYSZ1(I,J+1,2)-2.*EYSZ1(I,J,2)+EYSZ1(I,J-1,2)
     $    +a1(idone14(i,j-1,2))*(
     $    +EYSZ2(I,J+1,1)-2.*EYSZ2(I,J,1)+EYSZ2(I,J-1,1)
     $    +EYSZ2(I,J+1,2)-2.*EYSZ2(I,J,2)+EYSZ2(I,J-1,2)
     $     )
     $   ) ! end of d/dy**2 term
     $  )


	if (fansair=='y' .or. fansair=='Y') then

          EYS(I,J,nz)=-EYSZ2(I,J,3)+CZD*(EYS(I,J,nz1)+EYSZ2(I,J,4))
     $    +CZZ*(EYSZ1(I,J,4)+EYSZ1(I,J,3))
     $    +CZFXD*(EYSZ1(I+1,J,4)-2.*EYSZ1(I,J,4)+EYSZ1(I-1,J,4)
     $    +EYSZ1(I+1,J,3)-2.*EYSZ1(I,J,3)+EYSZ1(I-1,J,3))
     $    +CZFYD*(EYSZ1(I,J+1,4)-2.*EYSZ1(I,J,4)+EYSZ1(I,J-1,4)
     $    +EYSZ1(I,J+1,3)-2.*EYSZ1(I,J,3)+EYSZ1(I,J-1,3))

	else

		EYS(I,J,nz)=(1.0/(uu1Z(idone14(i,j,nz1))-uu2(idone14(i,j,nz1))
     $       -b0(idone14(i,j,nz1))*uu3))*(
     $  +EYS(I,J,nz1)*(uu1Z(idone14(i,j,nz1))+uu2(idone14(i,j,nz1))
     $   +b0(idone14(i,j,nz1))*uu3)
     $  +EYSZ1(I,J,3)*(a1(idone14(i,j,3))*uu1Z(idone14(i,j,3))+(-2.0
     $    +a1(idone14(i,j,3)))
     $      *uu2(idone14(i,j,3))+(-b0(idone14(i,j,3))
     $    +b1(idone14(i,j,3)))*uu3)
     $  +EYSZ1(I,J,4)*(-a1(idone14(i,j,4))*uu1Z(idone14(i,j,4))+(-2.0
     $    +a1(idone14(i,j,4)))
     $     *uu2(idone14(i,j,4))
     $   +(-b0(idone14(i,j,4))+b1(idone14(i,j,4)))*uu3)
     $  +EYSZ2(I,J,3)*(-uu1Z(idone14(i,j,3))+(1-2*a1(idone14(i,j,3)))
     $     *uu2(idone14(i,j,3))+(-b1(idone14(i,j,3))
     $     +b2(idone14(i,j,3)))*uu3)
     $  +EYSZ2(I,J,4)*(uu1Z(idone14(i,j,4))+(1-2*a1(idone14(i,j,4)))
     $     *uu2(idone14(i,j,4))+(-b1(idone14(i,j,4))
     $      +b2(idone14(i,j,4)))*uu3)
     $  +EYSZ3(I,J,3)*(-a1(idone14(i,j,3))*uu1Z(idone14(i,j,3))
     $   +a1(idone14(i,j,3))
     $      *uu2(idone14(i,j,3))-b2(idone14(i,j,3))*uu3)
     $  +EYSZ3(I,J,4)*(a1(idone14(i,j,4))*uu1Z(idone14(i,j,4))
     $   +a1(idone14(i,j,4))*uu2(idone14(i,j,4))
     $      -b2(idone14(i,j,4))*uu3)
     $  +(1.0/(2.0*DELX**2))*( ! The d/dx**2 term:
     $     EYSZ1(I+1,J,4)-2.*EYSZ1(I,J,4)+EYSZ1(I-1,J,4)
     $    +EYSZ1(I+1,J,3)-2.*EYSZ1(I,J,3)+EYSZ1(I-1,J,3)
     $    +a1(idone14(i-1,j,3))*(
     $    +EYSZ2(I+1,J,4)-2.*EYSZ2(I,J,4)+EYSZ2(I-1,J,4)
     $    +EYSZ2(I+1,J,3)-2.*EYSZ2(I,J,3)+EYSZ2(I-1,J,3)
     $     )
     $   ) ! end of d/dx**2 term
     $  +(1.0/(2.0*DELY**2))*( ! The d/dy**2 part:
     $     EYSZ1(I,J+1,4)-2.*EYSZ1(I,J,4)+EYSZ1(I,J-1,4)
     $    +EYSZ1(I,J+1,3)-2.*EYSZ1(I,J,3)+EYSZ1(I,J-1,3)
     $    +a1(idone14(i,j-1,3))*(
     $    +EYSZ2(I,J+1,4)-2.*EYSZ2(I,J,4)+EYSZ2(I,J-1,4)
     $    +EYSZ2(I,J+1,3)-2.*EYSZ2(I,J,3)+EYSZ2(I,J-1,3)
     $     )
     $   ) ! end of d/dy**2 term
     $  )

	endif

 30     CONTINUE
 40   CONTINUE


C   SAVE PAST VALUES
      DO 60 J=1,ny1
        DO 50 I=2,nx1
          EYSZ3(I,J,1)=EYSZ2(I,J,1)
          EYSZ3(I,J,2)=EYSZ2(I,J,2)
          EYSZ3(I,J,3)=EYSZ2(I,J,3)
          EYSZ3(I,J,4)=EYSZ2(I,J,4)
          EYSZ2(I,J,1)=EYSZ1(I,J,1)					 
          EYSZ2(I,J,2)=EYSZ1(I,J,2)
          EYSZ2(I,J,3)=EYSZ1(I,J,3)
          EYSZ2(I,J,4)=EYSZ1(I,J,4)
          EYSZ1(I,J,1)=EYS(I,J,1)
          EYSZ1(I,J,2)=EYS(I,J,2)
          EYSZ1(I,J,3)=EYS(I,J,nz1)
          EYSZ1(I,J,4)=EYS(I,J,nz)
 50     CONTINUE
 60   CONTINUE

      RETURN
      END



C*************************************************************
C*************************************************************
      SUBROUTINE HXSFLD

      INCLUDE "soil_3d_params.for"

C   SAVE PAST VALUES
      DO 70 K=1,nzsoil
        DO 60 J=1,ny1
          DO 50 I=2,nx1
            HXS1(I,J,K)=HXS(I,J,K)
 50      CONTINUE
 60	 CONTINUE
 70	CONTINUE 

C     THIS SUBROUTINE UPDATES THE HX SCATTERED FIELD COMPONENTS
      DO 30 K=1,nz1
        DO 20 J=1,ny1
          DO 10 I=2,nx1
            HXS(I,J,K)=HXS(I,J,K)-(EZS(I,J+1,K)-EZS(I,J,K))*DTMDY
     $                           +(EYS(I,J,K+1)-EYS(I,J,K))*DTMDZ
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE


      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE HYSFLD

      INCLUDE "soil_3d_params.for"

C   SAVE PAST VALUES
      DO 70 K=1,nzsoil
        DO 60 J=2,ny1
          DO 50 I=1,nx1
            HYS1(I,J,K)=HYS(I,J,K)
 50     CONTINUE
 60	 CONTINUE
 70	CONTINUE

C     THIS SUBROUTINE UPDATES THE HY SCATTERED FIELD COMPONENTS
      DO 30 K=1,nz1
        DO 20 J=2,ny1
          DO 10 I=1,nx1
            HYS(I,J,K)=HYS(I,J,K)-(EXS(I,J,K+1)-EXS(I,J,K))*DTMDZ
     $                           +(EZS(I+1,J,K)-EZS(I,J,K))*DTMDX
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE


      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE HZSFLD

      INCLUDE "soil_3d_params.for"
C   SAVE PAST VALUES 
      DO 70 K=2,nzsoil
        DO 60 J=1,ny1
          DO 50 I=1,nx1
            HZS1(I,J,K)=HZS(I,J,K)
 50       CONTINUE
 60	  CONTINUE
 70	CONTINUE


C   THIS SUBROUTINE UPDATES THE HZ SCATTERED FIELD COMPONENTS
      DO 30 K=2,nz1
        DO 20 J=1,ny1
          DO 10 I=1,nx1
            HZS(I,J,K)=HZS(I,J,K)-(EYS(I+1,J,K)-EYS(I,J,K))*DTMDX
     $                           +(EXS(I,J+1,K)-EXS(I,J,K))*DTMDY
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE

 
      RETURN
      END

C*************************************************************
C*************************************************************
	SUBROUTINE METAL
	INCLUDE "soil_3d_params.for"

	DO 20 J=1,ny
	 DO 10 K=1,nz
	   EYS(1,J,K)=0.0
	   EZS(1,J,K)=0.0
	   EYS(nx,J,K)=0.0
	   EZS(nx,J,K)=0.0
 10	 CONTINUE
 20	CONTINUE

	RETURN
	END

C*************************************************************
C*************************************************************
      SUBROUTINE ZERO

      INCLUDE "soil_3d_params.for"
 
C     THIS SUBROUTINE INITIALIZES VARIOUS ARRAYS AND CONSTANTS TO ZERO.
      T=0.0
       DO 10 I=1,nx
        DO 20 J=1,ny
         DO 30 K=1,nz
            EXS(I,J,K)=0.0
            EYS(I,J,K)=0.0
            EZS(I,J,K)=0.0
            HXS(I,J,K)=0.0
            HYS(I,J,K)=0.0
            HZS(I,J,K)=0.0
            IDONE(I,J,K)=0
            IDTWO(I,J,K)=0
            IDTHRE(I,J,K)=0
 30       CONTINUE
 20		CONTINUE
 10	   CONTINUE
       DO 15 I=1,nx1
        DO 25 J=1,ny1
	   DO 35 K=1,nzsoil
            EXS1(I,J,K)=0.0
            EYS1(I,J,K)=0.0
            EZS1(I,J,K)=0.0
            EXS2(I,J,K)=0.0
            EYS2(I,J,K)=0.0
            EZS2(I,J,K)=0.0
            EXS3(I,J,K)=0.0
            EYS3(I,J,K)=0.0
            EZS3(I,J,K)=0.0
            HXS1(I,J,K)=0.0
            HYS1(I,J,K)=0.0
            HZS1(I,J,K)=0.0
 35       CONTINUE
 25     CONTINUE
 15   CONTINUE
      DO 60 K=1,nz1
        DO 50 J=1,ny1
          DO 40 I=1,4
            EYSX1(I,J,K)=0.0
            EYSX2(I,J,K)=0.0
            EYSX3(I,J,K)=0.0
			EZSX1(I,J,K)=0.0
            EZSX2(I,J,K)=0.0
            EZSX3(I,J,K)=0.0
 40       CONTINUE
 50     CONTINUE
 60   CONTINUE
      DO 90 K=1,nz1
        DO 80 J=1,4
          DO 70 I=1,nx1
            EXSY1(I,J,K)=0.0
            EXSY2(I,J,K)=0.0
            EXSY3(I,J,K)=0.0
            EZSY1(I,J,K)=0.0
            EZSY2(I,J,K)=0.0
            EZSY3(I,J,K)=0.0
 70       CONTINUE
 80     CONTINUE
 90   CONTINUE
      DO 120 K=1,4
        DO 110 J=1,ny1
          DO 100 I=1,nx1
            EXSZ1(I,J,K)=0.0
            EXSZ2(I,J,K)=0.0
            EXSZ3(I,J,K)=0.0
            EYSZ1(I,J,K)=0.0
            EYSZ2(I,J,K)=0.0
            EYSZ3(I,J,K)=0.0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE
      DO 130 L=1,13
        ESCTC(L)=0.0
        EINCC(L)=0.0
        DTEDXD(L)=0.0
        DTEDYD(L)=0.0
        DTEDZD(L)=0.0
 130  CONTINUE

	do 140 i=1,25
	 do 150 j=1,25
	pulse(i,j)=0.0
 150   continue
140	continue


      RETURN
      END


C*************************************************************
C*************************************************************

C*************************************************************
C*************************************************************



      SUBROUTINE BUILDANT

      INCLUDE "soil_3d_params.for"

C     THIS SUBROUTINE IS USED TO DEFINE THE REALISTIC ANTENNA WITHIN
C     THE FDTD SOLUTION SPACE.  USER MUST SPECIFY IDONE, IDTWO AND
C     IDTHRE AT DIFFERENT CELL LOCATIONS TO DEFINE THE ANTENNA
C     SEE THE YEE PAPER (IEEE TRANS. ON AP, MAY 1966) FOR
C     A DESCRIPTION OF THE FDTD ALGORITHM AND THE LOCATION OF FIELD
C     COMPONENTS.
C
C     GEOMETRY DEFINITION
C
C     IDONE, IDTWO, AND IDTHRE ARE USED TO SPECIFY MATERIAL IN CELL
C     I,J,K.
C     IDONE DETERMINES MATERIAL FOR X COMPONENTS OF E
C     IDTWO FOR Y COMPONENTS, IDTHRE FOR Z COMPONENTS
C     THUS ANISOTROPIC MATERIALS WITH DIAGONAL TENSORS CAN BE MODELLED
C
C     SET IDONE,IDTWO, AND/OR IDTHRE FOR EACH I,J,K CELL =
C               0       FOR FREE SPACE
C               1       FOR PEC (METAL)
C               2-13    FOR LOSSY DIELECTRICS
C               14-18   FOR LOSSY SOIL
C
C     SUBROUTINE DCUBE BUILDS A CUBE OF DIELECTRIC MATERIAL BY SETTING
C     IDONE, IDTWO, IDTHRE TO THE SAME MATERIAL TYPE.  THE MATERIAL
C     TYPE IS SPECIFIED BY MTYPE.  SPECIFY THE STARTING CELL (LOWER
C     LEFT CORNER (I.E. MINIMUM I,J,K VALUES) AND SPECIFY THE CELL
C     WIDTH IN EACH DIRECTION (USE THE NUMBER OF CELLS IN EACH
C     DIRECTION).  USE nzWIDE=0 FOR A INFINITELY THIN PLATE IN THE
C     XY PLANE.  FOR PEC PLATE USE MTYPE=1.  ISTART, JSTART, KSTART ARE
C     USED TO DEFINE THE STARTING CELL AND nxWIDE, nyWIDE AND nzWIDE EACH
C     SPECIFY THE OBJECT WIDTH IN CELLS IN THE X, Y AND Z DIRECTIONS.
C     INDIVIDUAL IDONE, TWO OR THRE COMPONENTS CAN BE SET MANUALLY
C     FOR WIRES, ETC.  DCUBE DOES NOT WORK FOR WIRES (I.E. nxWIDE=0 AND
C     nyWIDE=0 FOR EXAMPLE)!
C

C     Build sphere with center at (SC1,SC2,SC3) and radius RA
C	or cylinder with radius RA and height Z


          OPEN(UNIT=9161,FILE='alak')

		icorerd=corerdf/100./delx
		ishellth=shellthf/100./delx
		idierd=icorerd+diethf/100./delx
		iantdep=antdepf/100./delz
		iairth=airthf/100./delz
c		iairth=nz-nzsoil
		iexpdieh=expdiehf/100./delz



		issf=isource
		jssf=jsource
		kssf=kbcore

		alak1=0
		alak2=0
		alak3=0
		ifff=0
		iffg=0



	DO 1301 k=1,nz
	  DO 1201 j=1,ny
	    DO 1101 i=1,nx
	
		r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)

		if (k>=kssf .and. k<(kssf+iantdep+iairth)) then

			
			if (r<=idierd+ishellth) then

c   ************************ DIELECTRIC *******************************			
			   if (k>=nz-1) then
c					call DCUBE(i,j,k,10,0,0,1)
			   else
c **** So dielectric would not be confused with soil in EXSFLD ****
c					call DCUBE(i,j,k,1,1,1,3)
	 				call DCUBE(i,j,k,10,0,0,29)
c **** So dielectric would not be confused with soil in EXSFLD ****
		       endif

	
     			endif

		endif





1101	    CONTINUE
1201     CONTINUE
1301	CONTINUE





c ********  First Model Dielectric, then Metal *************************


	DO 301 k=1,nz
	  DO 201 j=1,ny
	    DO 101 i=1,nx
	
		r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)

		if (k>=kssf .and. k<(kssf+iantdep+iairth)) then

			
			if (r>icorerd .and. r<=idierd) then

c   ************************ DIELECTRIC *******************************			
			   if (k>=nz-1) then
c					call DCUBE(i,j,k,10,0,0,1)
					call DCUBE(i,j,k,1,1,1,1)
			   else
c **** So dielectric would not be confused with soil in EXSFLD ****
c					call DCUBE(i,j,k,1,1,1,3)
	 				call DCUBE(i,j,k,10,0,0,29)
c **** So dielectric would not be confused with soil in EXSFLD ****
		       endif

				alak2=alak2+1
	
     				if (k==(kssf+iantdep+iairth-1)) then
				 ifff=ifff+1
				 idiei(ifff)=i
				 idiej(ifff)=j
c				 diek(ifff)=k
				 idiek(i,j)=11
               		if (k==nz-1) then
			    	 print *,'Excitation Dielectric Point(i,j,k)=('
     $,i,',',j,',',k,')'
				    endif
				 endif
	
     			endif

		endif





101	    CONTINUE
201     CONTINUE
301	CONTINUE





	DO 601 k=1,nz
	  DO 501 j=1,ny
	    DO 401 i=1,nx
	
		r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)


		if (k>=kssf .and. k<(kssf+iantdep+iairth)) then

			if (r<=icorerd) then

c  ************************ CORE *******************************			

c				call DCUBE(i,j,k,10,0,0,1)  ! asl
				call DCUBE(i,j,k,1,1,1,1)  


				print *,'Core',i,j,k
C				locate i,j
				alak1=alak1+1


			endif
			
c
		endif

		if (k>=(kssf+iexpdieh) .and. k<(kssf+iantdep+iairth)) then

			if (r>idierd .and. r<=(idierd+ishellth)) then

c  ************************ SHIELD *******************************			
c				call DCUBE(i,j,k,10,0,0,1)
				call DCUBE(i,j,k,1,1,1,1)


				write (9161,*) i,j,k
C				locate i,j
				alak3=alak3+1

     				if (k==nz-1) then
				 iffg=iffg+1
				 ishelli(iffg)=i
				 ishellj(iffg)=j
c				 diek(iffe)=k
				 ishellk(i,j)=12
				 print *,'Top Shell Point(i,j,k)=('
     $,i,',',j,',',k,')'
				endif



			endif

		endif


c  *****************ISOLATING SHIELD *******************************			
c		if (k==(kssf+iexpdieh) .and. r<=(idierd+ishellth)) then
c
c				call DCUBE(i,j,k,10,0,0,1)
c		endif
c  *****************ISOLATING SHIELD *******************************			



401	    CONTINUE
501    CONTINUE
601	CONTINUE









	print *,'No.of Core Nodes=',alak1
	print *,'No.of Dielectric Nodes=',alak2
	print *,'No.of Shell Nodes=',alak3

	print *,'No.of Excitation Points=',ifff



	close(unit=9161)

      RETURN
      END

C*************************************************************
C*************************************************************



      SUBROUTINE BUILDANT2

      INCLUDE "soil_3d_params.for"

C     THIS SUBROUTINE IS USED TO DEFINE THE REALISTIC ANTENNA WITHIN
C     THE FDTD SOLUTION SPACE.  USER MUST SPECIFY IDONE, IDTWO AND
C     IDTHRE AT DIFFERENT CELL LOCATIONS TO DEFINE THE ANTENNA
C     SEE THE YEE PAPER (IEEE TRANS. ON AP, MAY 1966) FOR
C     A DESCRIPTION OF THE FDTD ALGORITHM AND THE LOCATION OF FIELD
C     COMPONENTS.
C
C     GEOMETRY DEFINITION
C
C     IDONE, IDTWO, AND IDTHRE ARE USED TO SPECIFY MATERIAL IN CELL
C     I,J,K.
C     IDONE DETERMINES MATERIAL FOR X COMPONENTS OF E
C     IDTWO FOR Y COMPONENTS, IDTHRE FOR Z COMPONENTS
C     THUS ANISOTROPIC MATERIALS WITH DIAGONAL TENSORS CAN BE MODELLED
C
C     SET IDONE,IDTWO, AND/OR IDTHRE FOR EACH I,J,K CELL =
C               0       FOR FREE SPACE
C               1       FOR PEC (METAL)
C               2-13    FOR LOSSY DIELECTRICS
C               14-29   FOR LOSSY SOIL			!type+19
C
C     SUBROUTINE DCUBE BUILDS A CUBE OF DIELECTRIC MATERIAL BY SETTING
C     IDONE, IDTWO, IDTHRE TO THE SAME MATERIAL TYPE.  THE MATERIAL
C     TYPE IS SPECIFIED BY MTYPE.  SPECIFY THE STARTING CELL (LOWER
C     LEFT CORNER (I.E. MINIMUM I,J,K VALUES) AND SPECIFY THE CELL
C     WIDTH IN EACH DIRECTION (USE THE NUMBER OF CELLS IN EACH
C     DIRECTION).  USE nzWIDE=0 FOR A INFINITELY THIN PLATE IN THE
C     XY PLANE.  FOR PEC PLATE USE MTYPE=1.  ISTART, JSTART, KSTART ARE
C     USED TO DEFINE THE STARTING CELL AND nxWIDE, nyWIDE AND nzWIDE EACH
C     SPECIFY THE OBJECT WIDTH IN CELLS IN THE X, Y AND Z DIRECTIONS.
C     INDIVIDUAL IDONE, TWO OR THRE COMPONENTS CAN BE SET MANUALLY
C     FOR WIRES, ETC.  DCUBE DOES NOT WORK FOR WIRES (I.E. nxWIDE=0 AND
C     nyWIDE=0 FOR EXAMPLE)!
C

C     Build sphere with center at (SC1,SC2,SC3) and radius RA
C	or cylinder with radius RA and height Z


c      SC1=(nx+1)/2
c	SC2=(ny+1)/2
c	SC3=nzsoil-5
          OPEN(UNIT=9171,FILE='alak')

		icorerd=corerdf/100./delx
		ishellth=shellthf/100./delx
		idierd=icorerd+diethf/100./delx
		iantdep=antdepf/100./delz
		iairth=airthf/100./delz
c		iairth=nz-nzsoil
		iexpdieh=expdiehf/100./delz



		issf=isource
		jssf=jsource
		kssf=kbcore

		alak1=0
		alak2=0
		alak3=0
		ifff=0
		iffg=0


c *****Dipole type 1 has SHIELD above the dipole and Dipole type 2 has no SHIELD ****

	if (diptype==1) then

c ******* Dipole Type 1 Begin ********************************************************
c ********  First Model Dielectric, then Metal *************************


	DO 301 k=1,nz
	  DO 201 j=1,ny
	    DO 101 i=1,nx
	
		r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)

	if ((k>=(kssf+iexpdieh-ilgap) .and. k<(kssf+iexpdieh-ilgap/2))
     $.or. (k<(kssf+iantdep+iairth) .and.k>(kssf+iexpdieh-ilgap/2)))then

c  ************************ DIELECTRIC *******************************			
			if (r>icorerd .and. r<=idierd) then

			   if (k>=nz-1) then
c					call DCUBE(i,j,k,1,1,1,1)
					call DCUBE(i,j,k,10,0,0,1)
			   else
c				    call DCUBE(i,j,k,0,0,1,3)	 
				    call DCUBE(i,j,k,10,0,0,3)	 
			   
		       endif


				alak2=alak2+1
	
c  
     			if (r>=idierd-1 .and. r<=idierd) then
				 ifff=ifff+1
				 idiei(ifff)=i
				 idiej(ifff)=j

c      **** Pay attention to this part we shifted excitations, so we had to add some points to the gaps

			if (i<=(nx+1)/2 .and. j<=(ny+1)/2) then
				idiek(i,j)=11
			endif
			if (i>=(nx+1)/2 .and. j<=(ny+1)/2) then
				idiek(i+1,j)=11
			endif
			if (i>=(nx+1)/2 .and. j>=(ny+1)/2) then
				idiek(i+1,j+1)=11
			endif
			if (i<=(nx+1)/2 .and. j>=(ny+1)/2) then 
				idiek(i,j+1)=11
			endif	
c	 ******************************************************************************************



               		if (k==nz-1) then
			    	 print *,'Excitation Dielectric Point(i,j,k)=('
     $						,i,',',j,',',k,')'
				    endif

			 endif
	
     		  endif					
		
		
		endif





101	    CONTINUE
201     CONTINUE
301	CONTINUE





	DO 601 k=1,nz
	  DO 501 j=1,ny
	    DO 401 i=1,nx
	
		r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)

c		if (k>=kssf .and. k<=(kssf+iantdep+iairth)) then
		if (k>=(kssf+iexpdieh-ilgap) .and. 
     $		k<(kssf+iantdep+iairth))then

c  **************************** CORE *******************************								
			if (r<=icorerd) then

c 				call DCUBE(i,j,k,1,1,1,1)    ! 19
 				call DCUBE(i,j,k,10,0,0,1)    ! 19
				print *,'Core',i,j,k
C				locate i,j
				alak1=alak1+1


			endif
			
		endif

		if (k>=(kssf+iexpdieh) .and. k<(kssf+iantdep+iairth)) then

c  ************************* SHIELD *******************************			
			if (r>idierd .and. r<=(idierd+ishellth)) then


c				call DCUBE(i,j,k,1,1,1,1)	  !19
				call DCUBE(i,j,k,10,0,0,1)	  !19

				write (9171,*) i,j,k
C				locate i,j
				alak3=alak3+1

     				if (k==nz-1) then
				 iffg=iffg+1
				 ishelli(iffg)=i
				 ishellj(iffg)=j
c				 diek(iffe)=k
				 ishellk(i,j)=12
				 print *,'Top Shell Point(i,j,k)=('
     $,i,',',j,',',k,')'
				endif



			endif

		endif


401	    CONTINUE
501    CONTINUE
601	CONTINUE

c ******* Dipole Type 1 End ********************************************************


	

	elseif (diptype==2) then


c ******* Dipole Type 2 Begin ********************************************************
c ********  First Model Dielectric, then Metal *************************


	DO 1301 k=1,nz
	  DO 1201 j=1,ny
	    DO 1101 i=1,nx
	
		r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)

	if ((k>=(kssf+iexpdieh-ilgap) .and. k<(kssf+iexpdieh-ilgap/2))
     $.or. (k<(kssf+iexpdieh) .and.k>(kssf+iexpdieh-ilgap/2)))then

c  ************************ DIELECTRIC *******************************			
			if (r>icorerd .and. r<=idierd) then

			   if (k>=nz-1) then
c					call DCUBE(i,j,k,1,1,1,1)
					call DCUBE(i,j,k,10,0,0,1)
			   else

c				    call DCUBE(i,j,k,0,0,1,3)	 
				    call DCUBE(i,j,k,10,0,1,3)	 
								   
		       endif


				alak2=alak2+1
	
c  
     			if (r>=idierd-1 .and. r<=idierd) then
				 ifff=ifff+1
				 idiei(ifff)=i
				 idiej(ifff)=j

c      **** Pay attention to this part we shifted excitations, so we had to add some points to the gaps

			if (i<=(nx+1)/2 .and. j<=(ny+1)/2) then
				idiek(i,j)=11
			endif
			if (i>=(nx+1)/2 .and. j<=(ny+1)/2) then
				idiek(i+1,j)=11
			endif
			if (i>=(nx+1)/2 .and. j>=(ny+1)/2) then
				idiek(i+1,j+1)=11
			endif
			if (i<=(nx+1)/2 .and. j>=(ny+1)/2) then 
				idiek(i,j+1)=11
			endif	
c	 ******************************************************************************************



               		if (k==nz-1) then
			    	 print *,'Excitation Dielectric Point(i,j,k)=('
     $						,i,',',j,',',k,')'
				    endif

			 endif
	
     		  endif					
		
		
	endif





1101	    CONTINUE
1201     CONTINUE
1301	CONTINUE





	DO 1601 k=1,nz
	  DO 1501 j=1,ny
	    DO 1401 i=1,nx
	
		r=SQRT(dfloat(i-issf)**2+dfloat(j-jssf)**2)

c		if (k>=kssf .and. k<=(kssf+iantdep+iairth)) then
	if ((k>=(kssf+iexpdieh-ilgap) .and. k<(kssf+iexpdieh-ilgap/2))
     $.or. (k<(kssf+iexpdieh) .and.k>(kssf+iexpdieh-ilgap/2)))then

c  **************************** CORE *******************************								
			if (r<=icorerd) then

c 				call DCUBE(i,j,k,1,1,1,1)    ! 19
 				call DCUBE(i,j,k,10,0,0,1)    ! 19
				print *,'Core',i,j,k
C				locate i,j
				alak1=alak1+1


			endif
			
	endif


c ************** NO Shield ****************************************
c  ************************* SHIELD *******************************			
c
c		if (k>=(kssf+iexpdieh) .and. k<(kssf+iantdep+iairth)) then
c
c
c			if (r>idierd .and. r<=(idierd+ishellth)) then
c
c
c				call DCUBE(i,j,k,1,1,1,1)	  !19
c
c
c				write (9171,*) i,j,k
c
c				alak3=alak3+1
c
c     			if (k==nz-1) then
c				 iffg=iffg+1
c				 ishelli(iffg)=i
c				 ishellj(iffg)=j
c
c				 ishellk(i,j)=12
c				 print *,'Top Shell Point(i,j,k)=('
c     $,i,',',j,',',k,')'
c				endif
c
c
c
c			endif
c
c		endif
c
c ************** NO Shield ****************************************




1401	    CONTINUE
1501    CONTINUE
1601	CONTINUE

c ******* Dipole Type 2 End ********************************************************

	else
		
		print *, 'Something is wrong with the Dipole Type'
	
      endif





	print *,'No.of Core Nodes=',alak1
	print *,'No.of Dielectric Nodes=',alak2
	print *,'No.of Shell Nodes=',alak3

	print *,'No.of Excitation Points=',ifff




	close(unit=9171)

      RETURN
      END



C*************************************************************
C*************************************************************









C*************************************************************
C*************************************************************
	SUBROUTINE METALANT

	INCLUDE "soil_3d_params.for"

	DO 208 j=1,ny
	 DO 207 i=1,nx
		if (ishellk(i,j)==12) then
			EXS(i,j,nz-1)=0.0
			EYS(i,j,nz-1)=0.0
			EZS(i,j,nz-1)=0.0
		endif
		if (i==isource .and. j==jsource) then
			EXS(i,j,nz-1)=0.0
			EYS(i,j,nz-1)=0.0
			EZS(i,j,nz-1)=0.0
		endif
		if (idiek(i,j)==11) then
			EXS(i,j,nz-1)=0.0
			EYS(i,j,nz-1)=0.0
			EZS(i,j,nz-1)=0.0
		endif
 207	 CONTINUE
 208	CONTINUE

	RETURN
	END

C*************************************************************
C*************************************************************


C*************************************************************
C*************************************************************



      SUBROUTINE BUILDANTREC

      INCLUDE "soil_3d_params.for"

C     THIS SUBROUTINE IS USED TO DEFINE THE REALISTIC DIPOLE ANTENNA ON
C     THE FDTD SOLUTION SPACE IN THE AIR NEXT TO THE INTERFACE. It includes two metalic parts
C     and one node of Dilectric material
C
C     SET IDONE,IDTWO, AND/OR IDTHRE FOR EACH I,J,K CELL =
C               0       FOR FREE SPACE
C               1       FOR PEC (METAL)
C               2-13    FOR LOSSY DIELECTRICS
C               14-29   FOR LOSSY SOIL			!type+19
C
C     SUBROUTINE DCUBE BUILDS A CUBE OF DIELECTRIC MATERIAL BY SETTING
C     IDONE, IDTWO, IDTHRE TO THE SAME MATERIAL TYPE.  THE MATERIAL
C     TYPE IS SPECIFIED BY MTYPE.  SPECIFY THE STARTING CELL (LOWER
C     LEFT CORNER (I.E. MINIMUM I,J,K VALUES) AND SPECIFY THE CELL
C     WIDTH IN EACH DIRECTION (USE THE NUMBER OF CELLS IN EACH
C     DIRECTION).  USE nzWIDE=0 FOR A INFINITELY THIN PLATE IN THE
C     XY PLANE.  FOR PEC PLATE USE MTYPE=1.  ISTART, JSTART, KSTART ARE
C     USED TO DEFINE THE STARTING CELL AND nxWIDE, nyWIDE AND nzWIDE EACH
C     SPECIFY THE OBJECT WIDTH IN CELLS IN THE X, Y AND Z DIRECTIONS.
C     INDIVIDUAL IDONE, TWO OR THRE COMPONENTS CAN BE SET MANUALLY
C     FOR WIRES, ETC.  DCUBE DOES NOT WORK FOR WIRES (I.E. nxWIDE=0 AND
C     nyWIDE=0 FOR EXAMPLE)!
C

C     Build sphere with center at (SC1,SC2,SC3) and radius RA
C	or cylinder with radius RA and height Z


c      SC1=(nx+1)/2
c	SC2=(ny+1)/2
c	SC3=nzsoil-5
c          OPEN(UNIT=9171,FILE='alak')

		

		ireclen=reclen/100./delx
	    
		issf1=ireceiv
		jssf1=jreceiv
		kssf1=nzsoil+1

c ********  First Model Dielectric, then Metal *************************

c		call DCUBE(issf1,jssf1,kssf1,0,0,1,3)	 
		call DCUBE(issf1,jssf1,kssf1,10,0,0,3)	 


c ************************ METAL *************************

		ii1=issf1-ireclen
		ii2=issf1-1
		ii3=issf1+1
		ii4=issf1+ireclen

		DO 1301 i=ii1,ii2

c 			call DCUBE(i,jssf1,kssf1,1,1,1,1)    ! 19
 			call DCUBE(i,jssf1,kssf1,10,0,0,1)    ! 19

1301		CONTINUE


		DO 1304 i=ii3,ii4

c 			call DCUBE(i,jssf1,kssf1,1,1,1,1)    ! 19
 			call DCUBE(i,jssf1,kssf1,10,0,0,1)    ! 19

1304		CONTINUE




c ********  First Model Dielectric, then Metal *************************
c		kssf1=nzsoil+3
c		ii1=issf1-2
c		ii2=issf1+2
c		jj1=jssf1-1
c		jj2=jssf1+1
c		kk1=kssf1-1
c		kk2=kssf1+1

c		DO 2301 i=ii1,ii2
c		 DO 2302 j=jj1,jj2
c		  DO 2303 k=kk1,kk2
c
c			************************ DIELECTRIC ********************
c			call DCUBE(i,j,k,0,0,1,3)	 
c
c
c2303		  CONTINUE
c2302		 CONTINUE
c2301		CONTINUE




c		ii1=issf1-ireclen
c		ii2=issf1-3
c		ii3=issf1+3
c		ii4=issf1+ireclen
		
c		DO 1301 i=ii1,ii2
c		 DO 1302 j=jj1,jj2
c		  DO 1303 k=kk1,kk2 

c			************************ METAL *************************
c 			call DCUBE(i,j,k,1,1,1,1)    ! 19
c
c
c1303		  CONTINUE
c1302		 CONTINUE
c1301		CONTINUE
c
c
c		DO 1304 i=ii3,ii4
c		 DO 1305 j=jj1,jj2
c		  DO 1306 k=kk1,kk2 
c
cc			************************ METAL *************************
c 			call DCUBE(i,j,k,1,1,1,1)    ! 19
c
c
c1306		  CONTINUE
c1305		 CONTINUE
c1304		CONTINUE




      RETURN
      END



C*************************************************************
C*************************************************************

