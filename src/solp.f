      subroutine solp
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of phosphorus lost from the soil
!!    profile in runoff and the movement of soluble phosphorus from the first
!!    to the second layer via percolation   {!!tlv - and down through the remaining layers - March 25 2016}

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conv_wt(:,:)  |none         |factor which converts kg/kg soil to kg/ha
!!    curyr         |none         |current year of simulation
!!    hru_dafr(:)   |none         |fraction of watershed area located in HRU
!!    ihru          |none         |HRU number
!!    nyskip        |none         |number of years to skip output summarization
!!                                |and printing
!!    phoskd        |none         |Phosphorus soil partitioning coefficient
!!                                |Ratio of phosphorus attached to sediment to
!!                                |phosphorus dissolved in soil water
!!    pperco        |none         |phosphorus percolation coefficient (0-1)
!!                                |0:concentration of soluble P in surface
!!                                |  runoff is zero
!!                                |1:percolate has same concentration of soluble
!!                                |  P as surface runoff
!!    sol_bd(:,:)   |Mg/m**3      |bulk density of the soil
!!    sol_nly(:)    |none         |number of layers in soil profile
!!    sol_prk(:,:)  |mm H2O       |percolation from soil layer on current day
!!    sol_solp(:,:) |kg P/ha      |amount of phosohorus stored in solution
!!    sol_z(:,:)    |mm           |depth to bottom of soil layer
!!    surfq(:)      |mm H2O       |surface runoff generated on day in HRU
!!    wshd_plch     |kg P/ha      |average annual amount of phosphorus leached
!!                                |into second soil layer 
!!  phosphorus change 2/4/2014 gsm
!!    man_dry       |kg/ha        |Dry weight on manure on soil surface
!!    man_cov       |fractional   |Fraction of surface covered by manure
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_solp(:,:) |kg P/ha       |amount of phosphorus stored in solution
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    wshd_plch     |kg P/ha       |average annual amount of phosphorus leached
!!                                 |into second soil layer 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    vap         |kg P/ha       |amount of P leached from soil layer
!!    xx          |none          |variable to hold intermediate calculation
!!                               |result
!!  phosphorus change 2/4/2014 gsm
!!    w           |cm3/g         | Intermediate variable see Vadas2006 eq (12)
!!    wep_i_leach |kg P/ha       | Water extractable inorganic P leached
!!    wep_o_leach |kg P/ha       | Water extractable organic P leached
!!    mansolp     |kg p/ha       | Soluble P loss in runoff from manure
!!    wepcon      |mg p/L        | Soluble P leached from manure as concentration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
!!  phosphorus change 2/4/2014 gsm  also added vap_tile from developers code
      real :: xx, vap, w, pdfactor, wep_i_leach, wep_o_leach, mansolp   &
     & ,wepcon, vap_tile
!!      real :: xx, vap, vap_tile
!!    added lines below to zero out variables for Amy Collick 2/10/2016 gsm      
      mansolp = 0.
      xx = 0.
      vap = 0.
      w = 0.
      pdfactor = 0.
      wep_i_leach = 0.
      wep_o_leach = 0.
      wepcon = 0.
      vap_tile = 0.

      j = 0
      j = ihru
      surqsolp(j) = 0 !! set losses to zero
!!  phosphorus change 2/4/2014 gsm
       
!! Manure Layer Soluble P losses MJW 11/16/12
      if(manp_flag == 1) then
        if (precipday > 0.001) then
          !! Vadas 2007 eq 12
!! temporarily put statement below in gsm 4/28/2015
          if (man_cov(j) == 0) man_cov(j) = .0000001
          if (man_dry(j) == 0) man_dry(j) = .0000001     
          w = precipday / man_dry(j)* man_cov(j)* 10000.
          
          !! Vadas 2007 eq 11
          wep_i_leach = (2.2 * (w /(w+300.1)))* manwip(j)
          wep_i_leach = min(wep_i_leach, manwip(j))
          wep_i_leach = max(wep_i_leach, 0.)
          manwip(j) = manwip(j) - wep_i_leach  !! account for loss
          
          !! Vadas 2007 eq 11
          wep_o_leach = (2.2* (w /(w+300.1)))* manwop(j)* 1.6
          wep_o_leach = min(wep_o_leach, manwop(j))
          wep_o_leach = max(wep_o_leach, 0.)
          manwop(j) = manwop(j) - wep_o_leach !! account for loss
          
          !! Vadas 2007 eq 14                    
          pdfactor = (surfq(j)/precipday) ** 0.225
          
      !! Vadas 2007 eq 13 manure losses to runoff
          wepcon = (wep_i_leach + wep_o_leach)* 100. / precipday     ! concentration basis
          
          mansolp = wepcon * surfq(j)* pdfactor / 100.
          xx = wep_i_leach + wep_o_leach
          mansolp = min(mansolp, xx)
          mansolp = max(mansolp, 0.)
          
      !! Leached from manure but did not go to runoff 60% goes to soil layer 1 40% to layer 2
         vap =(wep_i_leach + wep_o_leach) - mansolp
         sol_solp(1,j) = sol_solp(1,j) + vap * 0.6
         sol_solp(2,j) = sol_solp(2,j) + vap * 0.4
        else
        ! no precip, no leaching
         wep_i_leach = 0.
         wep_o_leach = 0.
        end if
       end if
       
      vap_tile = 0.                !{tlv March 25 2016: changed from "0" to "0." since this is a real}

!! compute soluble P lost in surface runoff
      xx = 0.
      xx = sol_bd(1,j) * sol_z(1,j) * phoskd(j)
      surqsolp(j) = sol_solp(1,j) * surfq(j) / xx 
        !!units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
!!  phosphorus change 2/4/2014 gsm
!       if (surfq(j) > 0.001) then
!       write (17,77) i, iyr, sol_bd(1,j), sol_z(1,j), phoskd, surfq(j),  &
!       &              sol_solp(1,j), surqsolp(j)
!       end if
! 77    format(2i6,6f10.3)
        surqsolp(j) = Min(surqsolp(j), sol_solp(1,j))
        surqsolp(j) = Max(surqsolp(j), 0.)
        sol_solp(1,j) = sol_solp(1,j) - surqsolp(j)
        
        !! Manure Layer Soluble P losses MJW 11/16/12
        if(manp_flag == 1) then
            surqsolp(j) = surqsolp(j)+ mansolp
        end if
!     if (surfq(j) > 0.001) then
!     write (17,77) i, iyr, sol_bd(1,j), sol_z(1,j), phoskd(j), surfq(j),  &
!    &              sol_solp(1,j), surqsolp(j)
!     end if
! 77  format(2i6,6f10.3)
!      surqsolp(j) = Min(surqsolp(j), sol_solp(1,j))
!      surqsolp(j) = Max(surqsolp(j), 0.)
!      sol_solp(1,j) = sol_solp(1,j) - surqsolp(j)


!! compute soluble P leaching
      vap = 0.
      vap = sol_solp(1,j) * sol_prk(1,j) / ((conv_wt(1,j) / 1000.)      
     &                                            * pperco_sub(1,j))
      vap = Min(vap, .5 * sol_solp(1,j))
      sol_solp(1,j) = sol_solp(1,j) - vap
      
      !! estimate soluble p in tiles due to crack flow
      if (ldrain(j) > 0) then
        xx = Min(1., sol_crk(j) / 3.0)
        vap_tile = xx * vap
        vap = vap - vap_tile
      end if

      if (sol_nly(j) >= 2) then
        sol_solp(2,j) = sol_solp(2,j) + vap
      end if
   
   !!   do ii = 2, sol_nly(j)
   !!  phosphorus change 2/4/2014 gsm
      do ii= 2, sol_nly(j) - 1
        vap = 0.
	 if (ii /= i_sep(j)) then
         vap = sol_solp(ii,j) * sol_prk(ii,j) / ((conv_wt(ii,j) 
     &	   / 1000.) * pperco_sub(ii,j))
	   vap = Min(vap, .2 * sol_solp(ii,j))
	   sol_solp(ii,j) = sol_solp(ii,j) - vap
!!  phosphorus change 2/4/2014 gsm
!	   if (ii == sol_nly(j)) then
!           sol_solp(ii+1,j) = sol_solp(ii+1,j) + vap
!         end if
!         if (ii == ldrain(j)) then
!           vap = sol_solp(ii,j) * qtile / (conv_wt(ii,j) / 1000.
!     *                                         * pperco_sub(ii,j))
!           sol_solp(ii,j) = sol_solp(ii,j) - vap
!           tilep = vap
!         endif
	   sol_solp(ii+1,j) = sol_solp(ii+1,j) + vap
           if (ii == ldrain(j)) then
             vap = sol_solp(ii,j) * qtile / (conv_wt(ii,j) / 1000.
     *           * pperco_sub(ii,j))
             sol_solp(ii,j) = sol_solp(ii,j) - vap
             tilep = vap
           endif
	 endif
	end do
	percp(j) = vap
	
      !! summary calculation
      if (curyr > nyskip) then
        wshd_plch = wshd_plch + vap * hru_dafr(j)
        wshd_ptile = wshd_ptile + vap_tile * hru_dafr(j)
      end if
!!   added 11/29/2012 for MJW by gsm  
!!   default isolp = 0 not to print solp.out gsm 5/4/2015
!!   set isolp to 1 in basins.bsn file to print solp.out gsm 5/4/2015
      if (isolp == 1) then
       write (177,77) curyr, i, j, sol_solp(1,j), surfq(j), 
     *  man_dry(j), man_cov(j), 
     *   surqsolp(j), wep_i_leach, wep_o_leach, mansolp, manwip(j), 
     *   manwop(j), mansip(j), mansop(j) 
 77    format(i4, 2i12, 12f15.3)
      endif

      return
      end