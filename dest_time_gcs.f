      program dest_time_gcs
      implicit none
      
      real*8  km,sm,ggg,au,yr,pi,pc,dist_sun
      real*8  m_tot,m_test,m_test_sm,m_tot_sm,m_mean_sm,m_mean
      real*8  r_core,orb_r,r_core_pc,orb_r_au,dest
      real*8  time,time_yr,r,v,time_r,time_yr_r,r_cm 
      real*8  res_time_core, res_time_ns, res_time_bh
      
   
c     declaration of function names      
      real*8 time_destf, time_dest_ns, time_dest_r, rto_cm
c234567      
      
      character*8 namecluster
      character*22  namefile
      character*1  string1
      character*14 namefile_r, namefile_v
      
      character*12 buffer12
      character*4  buffer4
      character*35 fmt5 
  
      common /a/ km,sm,ggg,au,yr,pi
      common /b/ pc,dist_sun
      
      integer i_m, i_u, k0, k1, k2, i_clu, i_u_r ,i_u_v, i_clu_max
      integer n_file_res
      
      dimension m_test_sm(7)
           data m_test_sm/0.1,0.2,0.3,0.4,0.5,0.6,0.7/
c                         1   2   3   4   5   6   7
      
       km = 1.d5     ! km in  cm
       sm  = 1.99d33 ! solarmass in gr
       pc  = 3.09d18 ! parsec in cm
       ggg = 6.67d-8 ! gravitational constant in  dyne cm**2/g
       au  = 1.5d13  ! astronomical unit in cm
       yr  = 3.2d7   ! year in s
       pi=4*atan(1.0)! pi greco constant
       
       
  1     format (a8,a12,f6.2,a12,f4.2,a12,f3.2,a12,f4.2) 
  2     format (f6.2,a4,f4.2) 
  3     format (f6.2)     
  4     format (f6.2,a4,f18.2)  
  
  
c     read from file name cluster and other parametrers proper of the 
c     cluster itself
      
      n_file_res = 1
      i_clu_max = 2
      !the total number of clusters in unit 0 plus 1
      
      open(unit=0, file='Dati_GCs.dat', action='read', status='old', 
     & iostat= k0)
     
c     in this file put the resulting time calculated in years for core radius
c     ns and bh for every cluster in format 5 (see the connection with 
c     using a format Fortra 77 with gnuplot library)
      
      open(unit=1,file='resultsGCs.ris',action='write',status='replace')
      write(1,*)"#result of dest_time(yr) in core,m_test(sm), NS and BH"
      write(1,*)"#"
      
      if (k0 .ne. 0) then 
      write (*,*) 'Dati_GCs.dat cannot be opened'
      end if
      
      i_clu = 0 
       
      
      read(0,*) ! skip the first line
      read(0,*) ! skip the second line
      
c234567     
  55  continue ! until loop
       
      i_clu = i_clu +1 
      write(1,*)"#"
      
      read(0, fmt=1) namecluster,buffer12,m_tot_sm,buffer12,
     & r_core_pc,buffer12,m_mean_sm,buffer12,dist_sun
     
          
      write(*,*) "namecluster ",namecluster
      write(*,*) "total mass in solar masses =",  m_tot_sm
      write(*,*) "core radius in pc = ", r_core_pc
      write(*,*) "mean mass in solar masses =",m_mean_sm
      write(*,*) "distance from sun in kpc =",dist_sun
     
     
      
      r_core = r_core_pc * pc
      m_tot    = m_tot_sm * sm
      m_mean   = m_mean_sm * sm
      
      
      
      do 13 i_m = 1,7 
      write(*,*)
      write(*,*) "for i_m =", i_m
      write(*,*)
      i_u = 2 + i_clu_max*2 + (i_clu -1)*7 + i_m 
      string1= char(i_m + 48)
      
c    because in ascii table numbers start from 48 
      
      
      namefile= 'profile_'//namecluster//'_'//string1//'.ris'
      
      open (unit=i_u, file=namefile,status='replace', action='write')
      m_test = m_test_sm(i_m)*sm
      
      write(i_u,*) "#"
      write(i_u,*) "# for mass star test (sm) =", m_test_sm(i_m) 
      write(i_u,*) "#"
   
c in order to the article of F. Selsis et al. 2007 the orbital radius
c changes with the mass of the star test
      
      if (i_m == 1 ) orb_r_au   = 0.04
      if (i_m == 2 ) orb_r_au   = 0.09
      if (i_m == 3 ) orb_r_au   = 0.15
      if (i_m == 4 ) orb_r_au   = 0.20
      if (i_m == 5 ) orb_r_au   = 0.25
      if (i_m == 6 ) orb_r_au   = 0.30
      if (i_m == 7 ) orb_r_au   = 0.45
      
      orb_r   = orb_r_au * au 
      dest = orb_r * (2*m_mean_sm/m_test_sm(i_m))**(1./3.)
      
c calculus of time of destabilization for core radius
      
      time = time_destf(dest,m_tot_sm,r_core,m_mean_sm,m_test)
      time_yr = time/yr 
      res_time_core = time_yr
      
      write(i_u,*) "#"
      write(i_u,*) "# for core radius calculus of time in year"
      write(i_u,*) "#"
      write(i_u,*) "#", time_yr
      write(i_u,*) "#"
      
      time = time_dest_ns(dest,m_tot,r_core,m_mean_sm,m_test)
      time_yr = time/yr 
      res_time_ns = time_yr
      
      write(i_u,*) "#"
      write(i_u,*) "# for core radius calculus of time due to 100 ns"
      write(i_u,*) "#"
      write(i_u,*) "#", time_yr
      write(i_u,*) "#"
      
      fmt5 = "(a8,a12,f4.2,a4,es9.2e2,a4,es9.2e2)"  
      buffer12 = "            "
      buffer4 = "    " 
      write(1,fmt =fmt5) namecluster,buffer12,m_test_sm(i_m),buffer4,
     & res_time_core,buffer4,res_time_ns
     
     
      write(i_u,*) "#  for generic radii"
      write(i_u,*) "#  radius(pc) time (year)"
      

      
      
c calculus of the time of destabilization of an orbital planet traiectory
c from the data presents on the online database 
c https://people.smp.uq.edu.au/HolgerBaumgardt/globular/veldis.html
c put in the files: Dati_GCs.dat and NGC_0000_r.dat and NGC_0000_v.dat
c and so on for the error bands
      
      
      namefile_r = namecluster//'_r.dat'
      namefile_v = namecluster//'_v.dat'
      i_u_r = i_clu + n_file_res
      i_u_v = i_u_r + 1

      open(unit=i_u_r, file=namefile_r, action='read', status='old')
      open(unit=i_u_v, file=namefile_v, action='read', status='old')
      
      read(i_u_r,*) ! skip the first two lines in every file
      read(i_u_r,*) 
      read(i_u_v,*) 
      read(i_u_v,*) 
    
      
      do 15
      read (i_u_r, fmt=3) r ! arcsec
      read (i_u_v, fmt=3) v ! km/s
      
      if(r == 0 .or. v == 0 ) go to 44
      
      r_cm = rto_cm(r)
      time_r = time_dest_r(dest,r_cm,v,m_test,m_mean)
      
      time_yr_r = time_r/yr 
      
      write(unit=i_u,fmt =4) r_cm/pc,'    ', time_yr_r 
 
  15  continue 
      
  44  close(i_u_r) 
      close(i_u_v)
      close(i_u)
      write(*,*)
      write(*,*) "ho chiuso il file =",namefile_r
      write(*,*) "dell'unità", i_u_r
      write(*,*) "ho chiuso il file =",namefile_v
      write(*,*) "dell'unità", i_u_v 
      write(*,*) "ho chiuso il file =",namefile
      write(*,*) "dell'unità", i_u 
      write(*,*)
      
  13  continue 
      
      
      
      if((k0 .eq. 0) .and. (i_clu .lt. i_clu_max)) then 
      
      go to 55
      else 
      close(0)
      close(1)
      endif
      
      
      end program dest_time_gcs


c=====================================================================
c functions and subroutines

c calculus of time of destabilization for core radius
c234567      
      real*8 function time_destf(df,m_totsm_f,r_cor_f,mean_mfsm,mtestf)
      implicit none
      real*8  km,sm,ggg,au,yr,pi
      common /a/ km,sm,ggg,au,yr,pi
      
      real*8 df,m_totsm_f,r_cor_f,mean_mfsm,mtestf
      real*8 v_quad_f, v_mean_f, d_core_f, focusg_f, m_tot_f
      
      m_tot_f = m_totsm_f*sm
      v_quad_f    = m_tot_f * ggg/(6 * r_cor_f)
      v_mean_f  = sqrt(v_quad_f)
      d_core_f = 3*m_totsm_f/(8*mean_mfsm*pi*r_cor_f**3)
      focusg_f  = (df**2 + (ggg * mtestf *df)/v_quad_f)
      time_destf = 1/(4*sqrt(pi)*v_mean_f*d_core_f*focusg_f) 
      return
      end 


c this function yelds the time of destabilization for a star mass
c with mass = m_test due to about 100 neutron stars of mass m = 1.4 sm
c in the core of the cluster: ref Hills&Day(1976)
c234567
      real*8 function time_dest_ns(df,m_tot_f,r_cor_f,mean_mfsm,mtestf)
      implicit none
      real*8  km,sm,ggg,au,yr,pi
      common /a/ km,sm,ggg,au,yr,pi
      
      real*8 df,m_tot_f,r_cor_f,mean_mfsm,mtestf
      real*8 v_quad_f,v_mean_f,v_mean_ns,d_core_ns
      real*8 crossec0, l, v_inf_quad, gamma, vol
     
      v_quad_f  = m_tot_f * ggg/(6 * r_cor_f)
      v_mean_f  = sqrt(v_quad_f)
      vol = (4/3)*pi*(r_cor_f**3)
      d_core_ns = 100/vol
      l = (sqrt(1.4/(2*(mean_mfsm + 1.4))))/v_mean_f
      v_inf_quad = 2*ggg*(mtestf + 1.4*sm)/df**2
      crossec0 = pi*(df**2)
      gamma = 2*l*crossec0*(1/(l**2) + v_inf_quad)/sqrt(pi)
      time_dest_ns = 1/(d_core_ns*gamma)
      return
      end 
      
c234567
c function for conversion from '' to cm
      real*8 function rto_cm (rf)
      implicit none
      real*8 rf,pc,dist_sun
      common /b/ pc,dist_sun
      rto_cm = rf*dist_sun*1000*pc/206265 ! conversion from '' to cm
      return
      end 
      
      
c calculus of time of destabilization for generic radius
      real*8 function time_dest_r(destf,r_cmf,vf,m_testf,m_meanf)
      implicit none
      real*8  km,sm,ggg,au,yr,pi
      common /a/ km,sm,ggg,au,yr,pi
      
      real*8 destf,r_cmf,vf,m_testf,m_meanf
      real*8 v_cm,m_r,d_r,v_quad_r,focusg_r
      
      v_cm = km*vf !conversion in cm/s
      
      m_r = (r_cmf)*6*(v_cm**2)/ggg
   
      v_quad_r = v_cm**2
      d_r = 3*m_r/(8*m_meanf*pi*r_cmf**3) !density at r
      focusg_r  = (destf**2 + (ggg*m_testf*destf)/v_quad_r)
      time_dest_r = 1/(4*sqrt(pi)*v_cm*d_r*focusg_r) 
      return
      end 


