      subroutine nitvol

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily mineralization (NH3 to NO3)
!!    and volatilization of NH3

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr         |none          |current year of simulation
!!    hru_dafr(:)   |km**2/km**2   |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    nyskip        |none          |number of years to skip output
!!                                 |summarization and printing
!!    sol_fc(:,:)   |mm H2O        |amount of water available to plants in soil
!!                                 |layer at field capacity (fc - wp)
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_nly(:)    |none          |number of layers in soil profile
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool in soil layer
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer
!!                                 |on any given day (less wp water)
!!    sol_tmp(:,:)  |deg C         |daily average temperature of soil layer
!!    sol_wpmm(:,:) |mm H20        |water content of soil at -1.5 MPa (wilting
!!                                 |point)
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    wshd_nitn     |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from the NH3 to the NO3 pool by
!!                                 |nitrification in the watershed
!!    wshd_voln     |kg N/ha       |average annual amount if nitrogen lost by
!!                                 |ammonia volatilization in watershed
!!    sol_ph(:,:)   |none          |daily average PH of soil layer
!!    nit_fr        |none          |fraction of nitration rate converted to nitrious oxide
!!    n2o_nit(k,j)  |kg N/ha       |amount N in the form nitrious oxide due to nitrification
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool in soil layer
!!    wshd_nitn     |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from the NH3 to the NO3 pool by
!!                                 |nitrification in the watershed
!!    wshd_voln     |kg N/ha       |average annual amount if nitrogen lost by
!!                                 |ammonia volatilization in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    akn         |
!!    akv         |
!!    cecf        |none          |volatilization CEC factor
!!    dmidl       |
!!    dpf         |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    rnit        |kg N/ha       |amount of nitrogen moving from the NH3 to the
!!                               |NO3 pool (nitrification) in the layer
!!    rnv         |
!!    rvol        |kg N/ha       |amount of nitrogen lost from the NH3 pool due
!!                               |to volatilization
!!    sw25        |
!!    swf         |
!!    swwp        |
!!    tf          |
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm

      integer :: j, k, kk, nly
      real :: sw25, swwp, swf, xx, dmidl, dpf, akn, akv, rnv, rnit, rvol
      real :: pHf,nit_den,nit_fr = 0.02
      real :: tf, ph_r, tmp_r, nitr_pro
      real :: cecf = 0.15
      real, parameter :: PI=3.141592653589793D0

      j = 0
      j = ihru
      nly = sol_nly(j)	  
      nitr(j) = 0.
	  pHf = 0.
	  ph_r = 0.
	  tmp_r = 0.
	  nitr_pro = 0.
      do k = 1, nly
	   kk=k
       if(k==1) kk=2
!!        if(sol_tmp(kk,j)>5.) then      !! added by Moges
!!       tf = 0.
!!       tf = .41 * (sol_tmp(kk,j) - 5.) / 10.
!!       else 
!!	    tf=0.
!!      end if 
!! added by Moge for temperature reduction
          tmp_r = 0.13 * Exp(0.07*sol_tmp(kk,j))
		  tf = -0.06 + tmp_r
!! ph reduction function calculation added by Moges
		  ph_r = -5. + sol_ph(kk,j)
		  ph_r = PI * 0.45 * ph_r
		  ph_r = Atan(ph_r) / PI
		  pHf = 0.56 + ph_r
!!		  pHf = ph_r + pHf
!!         if (sol_nh3(k,j) > 0. .and. tf >= 0.001) then
          sw25 = 0.
          swwp = 0.
          sw25 = sol_wpmm(kk,j) + 0.25 * sol_fc(kk,j)
          swwp = sol_wpmm(kk,j) + sol_st(kk,j)
          if (swwp < sw25) then
            swf = 0.
            swf = (swwp - sol_wpmm(kk,j)) /(sw25 - sol_wpmm(kk,j))
          else
            swf = 1.
          end if

          if (k == 1) then
            xx = 0.
          else
            xx = 0.
            xx = sol_z(k-1,j)
          endif
!! define pH here
!!         if (sol_ph(kk,j) < 7.0) then
!!		    pHf=0.
!!           pHf = 0.307 * sol_ph(kk,j) - 1.269
!!         else if (sol_ph(kk,j)>= 7.0 .and. sol_ph(kk,j)<=7.4) then
!!           pHf= 1.0
!!          else 
!!          pHf = 5.367 - 0.599*sol_ph(kk,j)
!!        end if
!!         if (pHf < 0.) pHf =0.0001 		          
!! inttialize paremeters
          dmidl = 0.
          dpf = 0.
          akn = 0.
          akv = 0.
          rnv = 0.
          rnit = 0.
          rvol = 0.
          dmidl = (sol_z(k,j) + xx) / 2.
          dpf = 1. - dmidl / (dmidl + Exp(4.706 - .0305 * dmidl))
          akn = tf * swf
          akv = tf * dpf * cecf
          rnv = sol_nh3(k,j) * (1. - Exp(-akn - akv))
          rnit = 1. - Exp(-akn)
          rvol = 1. - Exp(-akv)  
          !! calculate nitrification (NH3 => NO3)
	    !! apply septic algorithm only to active septic systems
          if(k/=i_sep(j).or.isep_opt(j)/= 1) then  ! J.Jeong for septic, biozone layer
             if (rvol + rnit > 1.e-6) then
               rvol = rnv * rvol / (rvol + rnit)
               rnit = rnv - rvol
!! define N2O from nitrification by using equation added by Moges
   ! n20_nitr
   ! define from reduction factions
   ! define globally and call in nden_V2 denitrification code
!!rnit= nit- n2o_nit ! my code
!!
                nit_den = 0.
				nit_den = nit_fr * swf * tf * pHf
                nit_den = rnit * nit_den
				rnit = rnit - nit_den
				nitr_pro = nitr_pro + 100.*nit_den
				nitr(j)= 100.* nit_den + nitr(j)
			    if (rnit < 0.) rnit = 0.
                sol_nh3(k,j) = Max(1.e-6, sol_nh3(k,j) - rnit)
             endif
             if (sol_nh3(k,j) < 0.) then
               rnit = rnit + sol_nh3(k,j)
               sol_nh3(k,j) = 0.
             endif
             sol_no3(k,j) = sol_no3(k,j) + rnit

             !! calculate ammonia volatilization
             sol_nh3(k,j) = Max(1.e-6, sol_nh3(k,j) - rvol)
             if (sol_nh3(k,j) < 0.) then
               rvol = rvol + sol_nh3(k,j)
               sol_nh3(k,j) = 0.
             endif

             !! summary calculations
             if (curyr > nyskip) then
               wshd_voln = wshd_voln + rvol * hru_dafr(j)
               wshd_nitn = wshd_nitn + rnit * hru_dafr(j)
             end if 
          end if
!!		    nitr(j)= nit_den + nitr(j)
!!        else
!!		rnit = 0.
!!		nitr(j) = 0.
!!        end if
          if(i==365) then
          write (1004,9008)iyr,i,k,j,sol_ph(k,j),sol_tmp(k,j),tf,pHf,  
     &                 sol_nh3(k,j),nit_den,swf,akn,akv,rnit
	      end if
      end do
	

9008	  format(i4,';',i3,';',i1,';',i4,10(';',f10.3))
!!
          write (1005,9010)k,iyr,i,j,nitr(j), nitr_pro
9010      format(i4,1x,i4,1x,i3,1x,i4,1x,2f10.3)

      return
      end