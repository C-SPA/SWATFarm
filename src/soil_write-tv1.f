      subroutine soil_write

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes output to the output.sol file  !!tlv march 25, 2016 - output.snu file
      !!tlv - march 24, 2016 -- variables added to print the EDI (l=1), 6 inches of the top soil layer (l = 2), and separate other soil layers
      !!tlv - may 2, 2016 -- since soil not managed differently within layer, prob best to split out plow layer in the soil inputs. (prob don't need 2t and 2b here)
      
      
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!      sol_solp(:,:)  |kg P/ha    |amount of phosphorus stored in solution,  Index:(layer,HRU)       
!      sol_z(:,:)     |mm         |depth to bottom of soil layer, Index:(layer,HRU)
!      sol_bd(:,:)    |Mg/m**3    |bulk density of the soil      ! entered through *.sol file as g/cc
!      sol_rsd(:,:)   |kg/ha      |amount of organic matter in the soil, classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!      solp_t     |kg P/ha       |summation of sol_solp across all soil depths for a given HRU
!      solorgp_t  |kg P/ha       |summation of sol_orgp across all soil depths for a given HRU
!      solno3_t   |kg N/ha       |summation of sol_no3 across all soil depths for a given HRU
!      solorgn_t  |kg N/ha       |summation of sol_orgn across all soil depths for a given HRU
!
!      solp_?, solorgp_?, solno3_?, solorgn_?    all in ppm
            !! "1" is ppm of sol_solp (etc) for the 1st soil layer of a given HRU      
!           !! "2t" signifies the minimum of the top 6 inches of the 2nd layer, or the 2nd layer (for layers < 6)
!           !! "2b" signifies the remainder of the 2nd layer
!       j         |NA            loop counter for hrus
!       l         |NA            loop counter for soil layers
!       plow_z    |mm            |plow layer considered as part of the 2nd layer (the input first layer) = 6 inches in code
!       solz_2t   |mm            |top 6 inch depth (or less for shallow soils) for calculations in the 2nd layer
!       solz_2b   |mm            |soil depth remaining in the 2nd layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
       
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      
      integer :: j, l
      real :: solp_t, solno3_t, solorgn_t, solorgp_t
      
      !!next declaration lines and associated calculations added by tlv March 25, 2016
          !!note change in variable order, just for consistency and species grouping
      real :: solp_1, solorgp_1, solno3_1, solorgn_1
!      real :: solp_2t, solorgp_2t, solno3_2t, solorgn_2t
!      real :: solp_2b, solorgp_2b, solno3_2b, solorgn_2b
 !     real :: plow_z, solz_2t, solz_2b
      real :: solz_2, solz_3
      real :: solp_2, solorgp_2, solno3_2, solorgn_2
      real :: solp_3, solorgp_3, solno3_3, solorgn_3
      real :: y2c, q_liter
      real :: surqsolpC, sedorgpC, sedminpC, surqno3C, sedorgnC
!      plow_z = 6.0 * 25.4
!      solz_2t = 0.0
!      solz_2b = 0.0
      

      do j = 1,nhru
          !!initialize internal variables to zero
          y2c = 0.
          q_liter = 0.

          surqsolpC = 0.
          sedorgpC = 0.
          sedminpC = 0.
          surqno3C = 0.
          sedorgnC = 0.
      
          solp_t = 0.
	    solorgp_t = 0.
          solno3_t = 0.
          solorgn_t = 0.

          solp_1 = 0.
	    solorgp_1 = 0.
          solno3_1 = 0.
          solorgn_1 = 0.
          
          solz_2 = 0.
          solz_3 = 0.
          
          solp_2 = 0.
	    solorgp_2 = 0.
          solno3_2 = 0.
          solorgn_2 = 0.
          
          solp_3 = 0.
	    solorgp_3 = 0.
          solno3_3 = 0.
          solorgn_3 = 0.
          
      !   solp_2t = 0.
      !   solorgp_2t = 0.
      !   solno3_2t = 0.
      !   solorgn_2t = 0.
      !   
      !   solp_2b = 0.
      !   solorgp_2b = 0.
      !   solno3_2b = 0.
      !   solorgn_2b = 0.


          !!calculate values for layer 1 (the top 10mm layer)
          !!conversion to ppm (mg/kg) = (100 * kgP/ha) / ((MgSoil/m**3)*(non-cumulative mm_depth of layer))
            !!method from soil_chem subroutine
            !dg = sol_z(1,j)
            !wt1 = sol_bd(l,j) * dg / 100.              !! mg/kg => kg/ha
            
            solp_1 = sol_solp(1,j) * 100.0 / (sol_bd(1,j) * sol_z(1,j))
            solp_1sw = sol_solp(1,j)
            orgp_1 = sol_orgp(1,j) * 100.0 / (sol_bd(1,j) * sol_z(1,j))
            no3_1 = sol_no3(1,j) * 100.0 / (sol_bd(1,j) * sol_z(1,j))
            orgn_1 = sol_orgn(1,j) * 100.0 / (sol_bd(1,j) * sol_z(1,j))
          
          !!!calculate non-cumulative soil depths for layers 2 & 3
          !!if layer 2 (the first input soil layer) is > the 6-inch plow layer, then split out the plow layer in the SNU output file
          !if (sol_z(2,j) .GT. plow_z) then
          !    solz_2t = plow_z - sol_z(1,j)  
          !else
          !    solz_2t = sol_z(2,j) - sol_z(1,j) 
          !end if
          !solz_2b = sol_z(2,j) - solz_2t
            solz_2 = sol_z(2,j) - sol_z(1,j)           
            solz_3 = sol_z(3,j) - sol_z(2,j)
          !    
          !!!calculate concentrations for the plow layer (the top 6 inches of layer 2)          
          !solp_2t = sol_solp(2,j) * 100.0 / (sol_bd(2,j) * solz_2t)
          !orgp_2t = sol_orgp(2,j) * 100.0 / (sol_bd(2,j) * solz_2t)
          !solno3_2t = sol_no3(2,j) * 100.0 / (sol_bd(2,j) * solz_2t)
          !solorgn_2t = sol_orgn(2,j) * 100.0 / (sol_bd(2,j) * solz_2t)
          !
          !!!calculate concentrations for the remainder of layer 2 (don't bother if layer 2 = plow layer)
          !if (solz_2b > 0) then
          !  solp_2b = sol_solp(2,j) * 100.0 / (sol_bd(2,j) * solz_2b) 
          !  orgp_2b = sol_orgp(2,j) * 100.0 / (sol_bd(2,j) * solz_2b) 
          !  solno3_2b = sol_no3(2,j) * 100.0 / (sol_bd(2,j) * solz_2b) 
          !  solorgn_2b = sol_orgn(2,j) * 100.0 /(sol_bd(2,j) * solz_2b) 
          !else
          !  solp_2b = 0.
          !  orgp_2b = 0.
          !  solno3_2b = 0.
          !  solorgn_2b = 0.
          !end if

          !!calculate concentrations for soil layer 2
          solp_2 = sol_solp(2,j) * 100.0 / (sol_bd(2,j) * solz_2) 
          orgp_2 = sol_orgp(2,j) * 100.0 / (sol_bd(2,j) * solz_2) 
          solno3_2 = sol_no3(2,j) * 100.0 / (sol_bd(2,j) * solz_2) 
          solorgn_2 = sol_orgn(2,j) * 100.0 / (sol_bd(2,j) * solz_2) 

          !!calculate concentrations for soil layer 3
          solp_3 = sol_solp(3,j) * 100.0 / (sol_bd(3,j) * solz_3) 
          orgp_3 = sol_orgp(3,j) * 100.0 / (sol_bd(3,j) * solz_3) 
          solno3_3 = sol_no3(3,j) * 100.0 / (sol_bd(3,j) * solz_3) 
          solorgn_3 = sol_orgn(3,j) * 100.0 / (sol_bd(3,j) * solz_3) 
          
          !!calculate kg/ha values for the entire soil layer
          do l = 1,sol_nly(j)
              solp_t = solp_t + sol_solp(l,j)
              solno3_t = solno3_t + sol_no3(l,j)
		   
		    !!By Zhang
		    !!============
              if (cswat == 0) then
			    solorgn_t = solorgn_t + sol_orgn(l,j)
	        end if
	        if (cswat == 1) then
			    solorgn_t = solorgn_t + sol_n(l,j)
              end if		   
		    if (cswat ==2) then
		        solorgn_t = solorgn_t + sol_HSN(l,j) + sol_HPN(l,j)
		    end if
		    !!By Zhang
		    !!============		   
		   
              solorgp_t = solorgp_t + sol_orgp(l,j)
          end do  !!end loop through soil layer

      !!note that cn and solorgP columns are in a different order than in original
           !! the "2t" is the minimum of the top 6 inches of the 2nd layer, or the 2nd layer (for layers < 6)
           !! the "2b" is the remainder of the 2nd layer
          !! area in ha, q in L HRU output for sedyld (kh/ha) = sedyld(j) / hru_ha(j)
      !convert kg/ha to mg/l == {VALUE kg/ha} * hru_km(j)*100 * 1.e6 /( {VALUE mm = qdr(j)} *hru_km(j)*1.e6)
      !only provide a concentration if runoff was positive. NOTE: yields by water technically only occur if there is runoff also.
          if (qdr(j) .GT. 0.01) then
              q_liter = qdr(j)*hru_km(j)*1.e6
              y2c = 100.0 / qdr(j)
          end if        
          if (surqsolp(j) .GT. 0.001) then 
              surqsolpC = surqsolp(j)*y2c
          end if
          if (sedorgp(j) .GT. 0.001)  then 
              sedorgpC = sedorgp(j) * y2c
          end if              
          if ((sedminpa(j) + sedminps(j)) .GT. 0.001)  then 
              sedminpC = (sedminpa(j) + sedminps(j)) * y2c
          end if
          if (surqno3(j) .GT. 0.001) then 
              surqno3C = surqno3(j) * y2c
          end if
          if (sedorgn(j) .GT. 0.001) then 
              sedorgnC = sedorgn(j) * y2c 
          end if
      
          write (121,1000) hruno(j),  subnum(j),hruno(j),
     &        subnum(j), iyr, i_mo,icl(iida),
     &        iida, hru_km(j)*100, cnday(j), qdr(j), q_liter,
     &        sedyld(j) / (hru_km(j)*100), sedyld(j), 
     &        surqsolp(j), sedorgp(j), sedminpa(j) + sedminps(j),
     &        surqno3(j), sedorgn(j),
     &        surqsolpC, sedorgpC, sedminpC,
     &        surqno3C, sedorgnC,          
!     &        q_0_m3, sed_0_kg, solp_0_ppm,
!     &        orgp_0_ppm, solno3_0_ppm, solorgn_0_ppm,     
     &        solp_1, orgp_1, solno3_1, solorgn_1,
     &        solp_2, orgp_2, solno3_2, solorgn_2,
!     &        solp_2t, orgp_2t, solno3_2t, solorgn_2t,
!     &        solp_2b, orgp_2b, solno3_2b, solorgn_2b,
     &        solp_3, orgp_3, solno3_3, solorgn_3,
     &        sol_rsd(1,j), sol_bd(1,j), sol_z(1,j), sol_st(1,j),      
     &        sol_solp(1,j), sol_orgp(1,j), sol_no3(1,j), sol_orgn(1,j),
     &        sol_rsd(2,j), sol_bd(2,j), sol_z(2,j), sol_st(2,j),
     &        sol_solp(2,j), sol_orgp(2,j), sol_no3(2,j), sol_orgn(2,j),
     &        sol_rsd(3,j), sol_bd(3,j), sol_z(3,j), sol_st(3,j),
     &        sol_solp(3,j), sol_orgp(3,j), sol_no3(3,j), sol_orgn(3,j),
     &        solp_t, solorgp_t, solno3_t, solorgn_t
 
!          !tlv comment out 
!          write (121,1000) i, subnum(j), hruno(j), sol_rsd(1,j), solp_t, 
!     &        solno3_t, solorgn_t, solorgp_t, cnday(j)
      end do  !!loop through HRU
      
      return

1000  format (a5,",",x1a5a4,",",a6,",",i5,",",i3,",",i5,",", i5,",",
     &   f10.5, 55(",",f10.3))  !!tlv comment - "SNU" = a6, day = i4, ",", GISnum = a5 ,",", a4, ",", vars 32f10.2

!         !tlv comment out
! 1000 format ('SNU   ',i4,",",a5,a4,",",6f10.2)
      end
      
      
!!    qday          |mm H2O        |surface runoff loading to main channel for
!!                                 |day in HRU
!!    qdr(:)        |mm H2O        |total amount of water entering main channel
!!                                 |for day from HRU
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedorgn(:)    |kg N/ha       |amount of organic nitrogen in surface runoff
!!                                 |in HRU for the day
!!    sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface
!!                                 |runoff in HRU for the day
!!    sedyld(:)     |metric tons   |daily soil loss caused by water erosion
!!    sol_cnsw(:)   |mm H2O        |soil water content used to calculate daily
!!                                 |CN value (initial soil wter content for day)
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |at end of any given day
!!    surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                                 |the day
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    tloss         |mm H2O        |amount of water removed from surface runoff
!!                                 |via transmission losses on day in HRU
!!    tmn(:)        |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)      |deg C         |average temperature for the day in HRU
!!    tmx(:)        |deg C         |maximum temperature for the day in HRU
!!    usle          |metric tons   |daily soil loss predicted with USLE equation
      !from etact
      !!    sol_st(:,:)  |mm H2O        |amount of water stored in the soil layer on
!!                                |current day