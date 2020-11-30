      subroutine ndenit_V2
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the denitrification rate
!!    and nitrious oxide in HRU level by using reduction
!!    function method developed by Parton, Mosier et al. 1996
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ihru          |none          |HRU number
!!    mhru          |none          |max number of HRUs
!!    mlyr          |none          |max number of soil layers
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_clay(:,:) |%             |percent clay content in soil material
!!    sol_sand(:,:) |%             |percent sand content in soil material
!!    sol_silt(:,:) |%             |percent silt content in soil material
!!    sol_ph(:,:)   |none          |daily average PH of soil layer
!!    sol_nly(:)    |none          |number of soil layers
!!    sol_no3(:,:)  |mg N/kg soil  |nitrate concentration in soil layer
!!    sol_rock(:,:) |%             |percent rock content in soil material
!!    sol_tmp(:,:)  |deg C         |daily average temperature of soil layer
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer
!!                                 |on any given day (less wp water)
!!    sol_ul(:,:)   |mm H2O        |amount of water held in the soil layer at
!!                                 |saturation (sat - wp water)
!!    sol_wpmm(:,:) |mm H20        |water content of soil at -1.5 MPa (wilting
!!                                 |point)
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    n2o_nit(k,j)  |Kg N/ha       |amount of nitrious oxide from nitrification 
!!
!!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nitr_oxide     |Kg N/ha       |Nitrious oxide in the soil profile
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |number for HRU
!!    k           |none          |counter
!!    nly         |none          |number of layers
!!    fd_co2(:,:) |g N/ha/day    |the maximum total N gas flux for a given soil respiration or
!!                               |index function for carbon availability on total
!!                               |denitrification( N2O + N2) N gas flux
!!    fd_ph(:,:)  |none          |effect of PH on denitrification rate
!!    fd_no3(:,:) |g N/ha/day    |the maximum total N gas flux for a given soil No3 level
!!    fd_tmp(:,:) |none          |effect of temperature on denitrification rate
!!    fd_wfps(:,:)|%             |effect of wfps(water filled pore space on denitrification rate
!!    fr_co2      |g N/ha/day    |effect of soil respiration or carbon on ratio
!!    fr_no3      |g N/ha/day    |effect of soil nitrate on ratio
!!    fr_wfps     |%             |effect of water filled pore space on ratio
!!    sol_cmass   |kg c/ha       |amount of carbon stored in the soil layer
!!                               |converted by using percent organic carbon
!!    sum_N20     |kg N/ha       |amount of N lost by denitrification in the soil profile
!!    wfps(:,:)   |%             |water filled pore space for each soil layer
!!    xx's        |none          |variables to hold value for denitrification
!!    xx_r's      |none          |Variables to hold for nitrious oxide calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Atan, Min
!!    SWAT:
!!
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!!
!!
       use parm
       integer :: j,k,nly,kk
       real :: sol_thick,xx,xx1,xx2,xx3,xx4,xx5,xx6
       real :: xx_r1,xx_r2,xx_r3,xx_r4,wc,sat
       real, parameter :: PI=3.141592653589793D0,convert= 0.001
       real ::fd_co2,fd_no3, fd_ph,fd_wfps
       real :: fr_co2, fr_no3, fr_ph,fr_wfps,nitr_N2_pro
       real :: ratio_N2_N2O,nitr_N2O_pro,nitr_N2O,sol_no3_load,nitr_N2
       real :: sol_mass,sol_cmass,fd_tmp,wfps,dent_flux,sol_tex
       real :: dent_flux_pro
!!      real :: fdr_no3_pro, fdr_co2_pro
!!
!! initialize local variables
!!
         k = 0
         j= 0
         j = ihru
         nly = sol_nly(j)
         wc = 0.
         sat = 0.
         sum_N2O = 0.
         sol_mass = 0.
         sol_cmass = 0.
         fd_co2 = 0.
         fd_no3 = 0.
         fd_ph = 0.
         fd_wfps = 0.
         fd_tmp =0.
         wfps = 0.
         fr_co2 = 0.
         fr_no3 = 0.
         fr_ph = 0.
         fr_wfps = 0.
         sol_thick = 0.
         xx = 0.;xx6=0.
         xx1=0.;xx2 = 0.;xx3 =0.;xx4 =0.;xx5 = 0.
         xx_r1=0.;xx_r2 =0.;xx_r3 = 0.;xx_r4 = 0.
         ratio_N2_N2O = 0.
         dent_flux =0.
         sol_tex = 0.
         nitr_N2O = 0.
		 nitr_N2 = 0.
         nitr_N2O_pro =0.
		 nitr_N2_pro = 0.
         nitr_oxide(j)=0.
	      n2_den(j) = 0. 
         !den_tot =0.	  
         sol_no3_load =0.
		 dent_flux_pro =0.
!!        den_tot(j) = 0.
!!        nitr(j)    = 0.
!!         fdr_no3_pro =0.
!!        fdr_co2_pro =0.
!!
!!soil carbon mass from the soil and reduction function
!!calculation for each layer
!!
      !if (cden==1) then
      do k = 1,nly
          if (k == 1) then
            sol_thick = sol_z(k,j)
          else
            sol_thick = sol_z(k,j) - sol_z(k-1,j)
          end if
!! kg/ha( 10,000 for ha, originally it was (kg/m2)
            sol_mass=(sol_thick/1000.)*10000.*sol_bd(k,j)
     &               * 1000.*(1- sol_rock(k,j)/100.)
 !! mass of carbon( kg/ha)
 !!
            sol_cmass=sol_mass *(sol_cbn(k,j)/100.)
            xx = 0.35*sol_cmass
            if (xx <= 35.) then
            xx = Exp(0.35*sol_cmass)
            xx = 200./xx
	        xx= 1.+ xx
            fd_co2=(24000./xx)-100.
           else
            xx = Exp(35.)
            xx = 200./xx
            xx= 1.+ xx
            fd_co2=(24000./xx)-100.
           end if
!!
!!fd_co2 first in g N/ha and converted in to Kg N/ha
 !!          fd_co2=fd_co2*convert
!!
!! soil_no3 level is in concentration( mg/kg ) not in Kg/ha;
!! so kg/ha is changed in to mg/kg
!! from .chm input file for each layer, fd_no3 in gN/ha
!!
            xx6=sol_bd(k,j)*sol_thick*100.
           sol_no3(k,j)=sol_no3(k,j)/xx6   !! sol_no3 is converted in to mg/kg 
            !if (sol_no3(k,j) > 0.) then 
            xx1=40000.*(Atan(PI*0.002*(sol_no3(k,j)-180.)))/PI
            if (xx1 > -11000.) then
            fd_no3=11000.+xx1
            else
            fd_no3 = 1.e-6
            end if
!!           else if(sol_no3(k,j) < = 0.
 		    !fd_no3 = 0.
           !end if 
!! fd_no3 in Kg N/ha
!!           fd_no3=fd_no3*convert
!!
!!calculates PH reduction factor for each soil layer based on
!!soil PH input provided in the .sol input file
!!
          kk=k
         if(k==1) kk=2
      if (sol_tmp(kk,j) > 0. .and. sol_st(kk,j) > 0.) then
	   !! microbial processes if temp > 0 C
		!! microbial processes if water > pwp
         if (sol_ph(kk,j) <= 2.5) then
            fd_ph=1.e-6
          else if( sol_ph(kk,j) > 2.5 .and. sol_ph(kk,j)< 6.5) then
            fd_ph=(sol_ph(kk,j) - 2.5)/4.
          else
            fd_ph= 1.
          end if
!! calculate the temperature reduction function based on
!! soil temperature of each layer
!!
            xx2=sol_tmp(kk,j)/(sol_tmp(kk,j)+
     &         Exp( 9.93 + 0.312*sol_tmp(kk,j)))
            xx2= 0.9*xx2+0.1
            fd_tmp= Max(xx2,0.1)
!!
!! calculate wfps(water filled pore space) based on water content and
!! saturation level at each soil layer and determine the water content
!! effect on total denitrification based on the texture of soil in 
!! each layer;three d/t equations depending on dominant soil texture
!! are used to determine the effect of water content
!! on denitrification rate as described in the
!! paper of Parton,Mosier et al. 1996
!!
            wc=sol_st(kk,j)+sol_wpmm(kk,j)
!!
!! wc units mm
            sat=sol_ul(kk,j)+sol_wpmm(kk,j)
!! sat units mm
            wfps=(wc/sat)
            sol_tex=MAX(sol_clay(k,j),sol_silt(k,j),
     &             sol_sand(k,j))
           if (wfps >=0.30) then
             if(sol_tex == sol_clay(k,j)) then
                xx3=1.06*wfps
                xx3=18.**xx3
                xx3=22./xx3
                xx3=18.**xx3
                fd_wfps=60./xx3
             else if (sol_tex == sol_silt(k,j))then
                xx4=1.39*wfps
                xx4=14.**xx4
                xx4=16./xx4
                xx4=14.**xx4
                fd_wfps=4.82/xx4
             else
                xx5=2.01*wfps
                xx5=12.**xx5
                xx5=16./xx5
                xx5=12.**xx5
                fd_wfps=1.56/xx5
             end if
         else
            fd_wfps=0.
         end if
!!
!!Calculate the reduction functions / factors for ratio of N2:N2O
!!to determine GHG's particularly N20 for each layer based on
!!reduction function; first calculate the water filled pore space
!!for water content effect on ratio of N2: N2
!!
          if (wfps>=0.30) then
            xx_r1=2.2*wfps
            xx_r1=13.**xx_r1
            xx_r1=17./xx_r1
			xx_r1=13.**xx_r1
            fr_wfps=1.4/xx_r1
          else
            fr_wfps=0.
          end if
!!
!!Calculate PH reduction function/ factor effect on ratio of N2:N2O
!!
            xx_r2=Exp(-sol_ph(kk,j)*1.1)
            fr_ph=1./(1470.*xx_r2)
!!
!!calculate the effect of soil No3 on the ratio N2:N2O
!!
            xx7=sol_bd(k,j)*sol_thick*100.
           sol_no3(k,j)=sol_no3(k,j)/xx6
!! 
          !if (sol_no3(k,j) > 0.) then 
            xx_r3=1.*(Atan(PI*0.01*(sol_no3(k,j)-190.)))/PI
!!            if (xx_r3<-0.46) then
            xx_r3 = 0.5+xx_r3
            fr_no3=(1.- xx_r3)*25.
!!
!! Calculate the effect of the soil respiration on
!! the ratio N2:N20,in g N/ha
!!
            xx_r4=30.78*(Atan(PI*0.07*(sol_cmass - 13.)))/PI
!!            if (xx_r4 > -13.) then
            fr_co2=13.+xx_r4
!!           else
!!           fr_co2 = 1.e-6
!!           end if
!! in Kg N/ha
!!           fr_co2=fr_co2*convert
!!
!!
!! calculate the total denitrification flux(N2 + N2O) based on
!!the above reduction functions
!! Kg N/ha
            dent_flux=Min(fd_no3,fd_co2)*fd_tmp*fd_wfps*fd_ph
!!
!! calculate the ratio N2:N2O based on the above reduction functions
!! or factors and partition in to
!! Nitrous oxide(N2O) and de nitrogen(N2)
!!
            ratio_N2_N2O=Min(fr_no3,fr_co2)*fr_wfps*fr_ph
!!
!! amount of N lost as nitrious oxide for each layer
!! 
            if(ratio_N2_N2O > 0.) then 
            nitr_N2O = dent_flux/(1.+ratio_N2_N2O)
			nitr_N2 = dent_flux/ (1. + (1.0/ratio_N2_N2O))
            else
		     nitr_N2O = 0.
		     nitr_N2 = dent_flux
           end if
         if(sol_no3(k,j) > 0.001*dent_flux) then
		 sol_no3(k,j) = sol_no3(k,j) - 0.001*dent_flux  !! convert in to kg/ha
	     else
         sol_no3(k,j) = 0.
         end if 		 
        if(cden ==1) then 
		 wdnt1 = nitr_N2 + wdntl
         end if 
!!		 den_tot = den_tot + dent_flux
			
      end if 
!!
!!for testing purpose only
!!
!! nitrious oxide outputs for .hru file
!!
            nitr_oxide(j)=nitr_oxide(j)+nitr_N2O
		    n2_den(j) = n2_den(j) + nitr_N2
!!          sol_no3(k,j) = sol_no3(k,j)- nitr_oxide(j)
		   
!!          den_tot(j) = nitr_oxide(j) + nitr(j)
!!
!! amount of N lost as nitriuos oxide in the soil profile in Kg N/ha
!!          if(iyr == 2009) then
            write (1003,9000)iyr,i,k,j,sol_ph(k,j),sol_tmp(k,j),fd_ph,  
     &                       sol_cmass,fd_co2,fd_no3,dent_flux,
     &                       ratio_N2_N2O,nitr_N2O,fd_tmp,sol_no3(k,j) 
	        write (1006,9002)iyr,i,k,j,wfps,fr_no3,fr_co2,fr_wfps,fr_ph,
     &                   fd_wfps,sol_cbn(k,j),sol_thick,sol_tex
!!          end if 
!!
!!
!!      den_tot = den_tot + dent_flux
       dent_flux_pro=dent_flux_pro +dent_flux
       nitr_N2O_pro=nitr_N2O_pro+nitr_N2O
	   nitr_N2_pro = nitr_N2_pro + nitr_N2
!!
      end do
      !end if 
!!
!! writing daily output file
9000     format(i4,';',i4,';',i4,';',i4,11(';',f10.3))
9002     format(i4,';',i4,';',i4,';',i4,9(';',f10.3))
!!
        write (1002,3001)k,iyr,i,j,dent_flux_pro,nitr_N2O_pro, 
     & 		              nitr_oxide(j),nitr_N2_pro,n2_den(j)              
3001   format(i4,1x,i4,1x,i3,1x,i4,1x,f10.3,1x, 2f10.3,1x,2f10.3)
      return
      end   