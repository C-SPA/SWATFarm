      subroutine dairy_total
      !implicit none


!     ~ ~ ~ PURPOSE ~ ~ ~
!     this subroutine computes the nutrients produced by dairy cows in the farm.
!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!     variable          |definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     nDay              | day in cycle 
!     nCalf             | number of calves
!     nHeifer_first_lact| first lactation cows           
!     nHeifer_first_dry | cows dry after first lactation 
!     nHeifer_second_lact| second lactation cows           
!     nHeifer_second_dry| number of cows dry after sec lactation           
!     nHeifer_third_lact| third lactation cows           
!     nHeifer_third_dry | number of third lactation dry      
!     nLact             | number of fourth lactation or greater, cows
!     nDry              | cow number fourth lactation or greater, dry cows
!     nBarn             | total number of animals in the barn
!     nPasture          | total number of animal in the pasture 
!     calf_ME           | calf metabolizable energy of feed, kcal/kg       
!     calf_CP           | calf feed crude protein, %
!     heifer_ME         | heifer ME of feed, kcal/kg
!     heifer_CP         | heifer crude protein, % 
!     lact_CP           | lactating crude protein, %
!     dry_CP            | dry cow crude protein, %
!     lact_target       | target lactating herd size   
!     kCull             | culling target year-over-year, %
!     kMortality        | mortality rate in herd, %
!     milkCow           | Milk yield, kg/d 
!     Breed             |Breed index, 1 if holstein - 0 otherwise             
!     PKYD              | peak milk yield, kg/d       
!     MW                | mature weight of lactating animal, kg      
!     kPreg             | pregnancy rate
!     MilkFat           | milk fat, %
!     MilkProtein       | milk protein, %
!     FCM               | fat corrected milk
!     Temp              | temperature, C       
!     RHMD              | Relative Humidity, %
!     WS                | wind speed, kph
!     HRS               | Hours perceived sunlight
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!     variable                | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     TotalNP_barn            | total nitrogen and phosphorus in manure in the barn, Kg
!     total_N_barn            | total nitrogen in manure in the barn, Kg
!     total_P_barn            | total phosphuros in manure in the barn, Kg
!     total_N_Past            | total nitrogen in manure in the pasture, Kg
!     total_P_Past            | total phosphorus in manure in the pasture, Kg
!     Norg_barn               | organic nitrogen in manure in the barn, Kg
!     Nmin_barn               | mineralized nitrogen in manure in the barn, Kg
!     Porg_barn               | organic phosphuros in manure in the barn, Kg
!     Pmin_barn               | mineralized phosphuros in manure in the barn, Kg 
!     Norg_past               | organic nitrogen in maure in the pasture, Kg 
!     Nmin_past               | mineralized nitrogen in manure in the pasture, Kg 
!     Porg_past               | organic phosphorus in manure in the pasture, Kg 
!     Pmin_past               | mineralized phosphorus in manure in the pasture, Kg 
!     Pmin_frac               | fraction of mineralized phosphorus
!     Porg_frac               | fraction of organic phosphuros
!     Norg_frac               | fraction of organic nitrogen 
!     Nmin_frac               | fraction of mineralized nitrogen 
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!     name                    | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     firstCull               | cull rate first lactation
!     secondCull              | cull rate second lact 
!     thirdCull               | cull rate third lact 
!     fourthCull              | cull rate fourth lact 
!     age_out                 | calves becoming first lac
!     first_age_out           | first lact becoming second 
!     second_age_out          | second lact becoming third 
!     third_age_out           | third lact becoming fourth 
!     fourth_age_out          | fourth lact becoming meat 
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm

      !Set starting animal numbers
      !real :: iCalf
      !real :: iHeifer_first_lact
      !real :: iHeifer_second_lact
      !real :: iHeifer_third_lact
      !real :: iHeifer_first_dry
      !real :: iHeifer_second_dry
      !real :: iHeifer_third_dry
      !real :: iLact
      !real :: iDry


      ! Animal Characteristics
      real :: lact_target
      real :: kCull
      real :: kMortality
      real :: BI
      real :: Breed
      real :: PKYD
      real :: MW
      real :: kPreg
      real :: MilkFat
      real :: MilkProtein
      real :: FCM
      real :: born, calf_NEma, CETI, culling, heifer_NEma, meat_produced
      real :: new_dry_herd, new_first_dry, new_first_lact, new_lact_from_third
      real :: new_lact_herd, new_second_lact, new_second_dry, new_third_dry
      real :: new_third_lact, ADG, DIM

      !###########################################
      ! Feed Characteristics


      ! Calf Feeding
      !real :: calf_ME
      !real :: calf_CP

      !Yearling Feed
      !real :: heifer_ME
      !real :: heifer_CP

      !Lactating Cow Feed
      !real :: lact_CP

      !Dry Cow Diet
      !real :: dry_CP


      !###########################################
      ! Environmental Conditions
      !real :: HRS

      ! Perceived Temeprature
      !real :: Temp
      !real :: RHMD
      !real :: WS
      real :: DMIAF_temp
      real :: DMINC


      ! Set all rates to 0 before reading files
      real :: kMature
      real :: kDry
      real :: kFreshening
      real :: kMilk

      ! Set outputs to 0
      real :: Meat
      real :: Milk
      real :: Nmin
	  real :: NminP
      real :: Norg
	  real :: NorgP
      real :: Pmin
	  real :: PminP
      real :: Porg
	  real :: PorgP
      real :: P, N
      !real :: total_N_barn
	  !real :: total_P_barn
	  !real :: total_N_Past
   	  !real :: total_P_Past
	  !real :: TotalNP_barn
	  !real :: TotalNP_past

      ! Set animal numbers to start
      !real :: nCalf
      !real :: nHeifer_first_lact
      !real :: nHeifer_first_dry
      !real :: nHeifer_second_lact
      !real :: nHeifer_second_dry
      !real :: nHeifer_third_lact
      !real :: nHeifer_third_dry
      !real :: nLact
      !real :: nDry
      real :: age_out
      real :: first_age_out
      real :: second_age_out
      real :: third_age_out
      real :: fourth_age_out
      real :: calves_death, calves_cull
      real :: first_lact_death, first_lact_cull
      real :: second_lact_death, second_lact_cull
      real :: third_lact_death, third_lact_cull
      real :: fourth_lact_death, fourth_lact_cull
      real :: firstCull, secondCull, thirdCull, fourthCull, kHeiferCull
	  real :: first_dry_age_out
	  real :: second_dry_age_out
	  real :: third_dry_age_out
	  real :: dry_age_out
	  real :: dry_cull
	  real :: dry_death
	  real :: first_dry_cull
	  real :: first_dry_death
	  real :: lact_cull
	  real :: lact_death
	  real :: second_dry_cull
	  real :: second_dry_death
	  real :: third_dry_cull
	  real :: third_dry_death


      !real :: wtCalf
      real :: wtHeifer_first_lact
      real :: wtHeifer_second_lact
      real :: wtHEifer_third_lact
      !real :: WTHFD
      !real :: WTSFD
      !real :: WTHTD
      real :: wtLact
      !real :: wtDry
      real :: lact_DMI
      real :: dry_DMI
      real :: calf_DMI
      real :: heifer_first_lact_DMI
      real :: heifer_second_lact_DMI
      real :: heifer_third_lact_DMI
      real :: heifer_first_dry_DMI
      real :: heifer_second_dry_DMI
      real :: heifer_third_dry_DMI
      real :: lact_N
      real :: dry_N
      real :: calf_N
      real :: heifer_first_lact_N
      real :: heifer_second_lact_N
      real :: heifer_third_lact_N
      real :: heifer_first_dry_N
      real :: heifer_second_dry_N
      real :: heifer_third_dry_N
      real :: lact_P
      real :: dry_P
      real :: calf_P
      real :: heifer_first_lact_P
      real :: heifer_second_lact_P
      real :: heifer_third_lact_P
      real :: heifer_first_dry_P
      real :: heifer_second_dry_P
      real :: heifer_third_dry_P
      !real :: fed_calf_P, fed_lact_P, fed_heifer_P, fed_dry_P
	  !real :: Pasture_area, manure_past_perhac
	  !real :: manureStore_per_hac
	 
	  ! manure update
	  real :: nBarn
	  real :: nPasture
	  !real :: manure_barn
	  !real :: manure_past
	  !real :: manureStore
	  !real :: Norg_barn
	  !real :: Norg_past
	  !real :: Nmin_barn
	  !real :: Nmin_past 
	  !real :: Porg_barn
	  !real :: Porg_past
	  !real :: Pmin_barn
	  !real :: Pmin_past
	  !real :: Norg_frac
	  !real :: NorgP_frac
	  !real :: Nmin_frac
	  !real :: NminP_frac
	  !real :: Porg_frac
	  !real :: PorgP_frac
	  !real :: Pmin_frac
	  !real :: PminP_frac
      !real :: Total_N_barn_stored
      !real :: Total_P_barn_stored
      !real :: Total_N_past_deposited
      !real :: Total_P_past_deposited
      !real :: Pmin_stored
      !real :: Porg_stored
      !real :: Nmin_stored
      !real :: Norg_stored
      !real :: Norg_stored_barn
      !real :: Nmin_stored_barn
      !real :: Porg_stored_barn
      !real :: Pmin_stored_barn
      !real :: Pmin_deposited
      !real :: Porg_deposited
      !real :: Nmin_deposited
      !real :: Norg_deposited
      !real :: Pmin_deposited_past
      !real :: Porg_deposited_past
      !real :: Nmin_deopsited_past
      !real :: Norg_deposited_past
      !!
      !###############################################
      ! Set Starting value for animal characteristics
      !###############################################
      lact_target = 100
      kHeiferCull = 0.22
      born = 0

      firstCull = 0.1
      secondCull = 0.2
      thirdCull = 0.3
      fourthCull = 0.5
      ! Animal Characteristics
      kCull = 80.0 !rate of cows leaving milking herd, %
      kMortality = 7.0 !rate of cows lost to disease, etc., %
      Breed = 1 !if Holsteins 1, else 0
      PKYD = 43.0 !peak milk yield, kg/d
      !MW = 600.0 !mature weight in kg
      kPreg = 40.0
      MilkFat = 3.5 !average MF% of herd
      MilkProtein = 3.5 !average Protein% of herd
      FCM = 3.0 !Fat corrected milk, %

      !###########################################
      ! Feed Characteristics
      !###########################################
      ! Calf Feeding
      !calf_ME = 5.0 !calf ME of diet
      !calf_CP = 16.0 !CP, %
      !fed_calf_P = 0.45 !P, % added P

      !Yearling Feed
      !heifer_ME = 5.0 !yearling ME of diet
      !heifer_CP = 17.0 !CP, %
      !fed_heifer_P = 0.45 !P, %

      !Lactating Cow Feed
      !lact_CP = 17.0 !CP, %
      !fed_lact_P = 0.45 ! P, %

      !Dry Cow Diet
      !dry_CP = 17.0 !CP, %
      !fed_dry_P = 0.45 ! P, %


      !###########################################
      ! Environmental Conditions
      !###########################################
      !HRS = 12.0 !Hours perceived sunlight

      ! Perceived Temeprature
      !Temp = 24.0 !Indoor temp, C
      !RHMD = 75.0 !Relative Humidity, %
      !WS = 1.0 !Average wind speed, kph



      ! Set all rates to 0 before reading files
      kMature = 1/(365.0*2)
      kDry = (1/305.0)
      kFreshening = (1/60.0)
      kMilk = 0.0
      kMortality = 0.07 !as whole number percentage

      ! Set outputs to 0
      Meat = 0
      Milk = 0
      N = 0
      P = 0
	  wtCalf = 0
      wtHeifer_first_lact = 0
      wtHeifer_second_lact = 0
      wtHEifer_third_lact = 0
      WTHFD = 0
      WTSFD = 0
      WTHTD = 0
      wtLact = 0
      wtDry = 0
      born = (lact_target*0.5)/365
	  
	     if (i .eq. 1) then 
		 !Initialize day 1 populations
		 nCalf = iCalf
		 nHeifer_first_lact = iHeifer_first_lact
		 nHeifer_second_lact = iHeifer_second_lact
		 nHeifer_third_lact = iHeifer_third_lact
         nHeifer_first_dry = iHeifer_first_dry
		 nHeifer_second_dry = iHeifer_second_dry
		 nHeifer_third_dry = iHeifer_third_dry
		 nLact =iLact
		 nDry = iDry
		 nBarn = iHeifer_first_lact + iHeifer_second_lact + iHeifer_third_lact + iLact
		 nPasture = iCalf + iHeifer_first_dry + iHeifer_second_dry + iHeifer_third_dry + iDry
		 else
		 !Defining calf populations
		 calves_death = nCalf*kMortality/365
         calves_cull = nCalf*kHeiferCull/365
         age_out = (nCalf )*(1/(365.0*2))
		 born = (nHeifer_first_dry + nHeifer_second_dry + nHeifer_third_dry + nDry)/(60*0.5)
		 nCalf = nCalf + born - age_out - calves_death - calves_cull
         iCalf = nCalf

		!Defining 1st lactation heifer pools
         first_lact_death = (nHeifer_first_lact*kMortality)/365
         first_lact_cull = (nHeifer_first_lact*firstCull)/365
         first_age_out = (nHeifer_first_lact/305)
         nHeifer_first_lact = nHeifer_first_lact + age_out - first_age_out - first_lact_cull - first_lact_death
         iHeifer_first_lact = nHeifer_first_lact
		 
		 !Defining 1st dry heifer pools
         first_dry_death = (nHeifer_first_dry*kMortality)/365
         first_dry_cull = (nHeifer_first_dry*firstCull)/365
         first_dry_age_out = (nHeifer_first_dry/60)
         nHeifer_first_dry = nHeifer_first_dry + first_age_out - first_dry_age_out - first_dry_cull - first_dry_death
		 iHeifer_first_dry = nHeifer_first_dry
		 !Defining 2nd lactation heifer pools
         second_lact_death = (nHeifer_second_lact*kMortality)/365
         second_lact_cull = (nHeifer_second_lact*secondCull)/365
         second_age_out = (nHeifer_second_lact/305)
		 nHeifer_second_lact = nHeifer_second_lact + first_dry_age_out - second_age_out -second_lact_death-second_lact_cull
         iHeifer_second_lact = nHeifer_second_lact
		 
		 !Defining 2nd dry heifer pools
	     second_dry_death = (nHeifer_second_dry*kMortality)/365
         second_dry_cull = (nHeifer_second_dry*secondCull)/365
         second_dry_age_out = (nHeifer_second_dry/60)
         nHeifer_second_dry = nHeifer_second_dry + second_age_out - second_dry_age_out - second_dry_cull - second_dry_death
         iHeifer_second_dry = nHeifer_second_dry
		 !Defining 3rd lactation heifer pools
		 third_lact_death = (nHeifer_third_lact*kMortality)/365
         third_lact_cull = (nHeifer_third_lact*thirdCull)/365
         third_age_out = (nHeifer_third_lact/305)
		 nHeifer_third_lact = nHeifer_third_lact + second_dry_age_out - third_age_out -third_lact_death-third_lact_cull
         iHeifer_third_lact = nHeifer_third_lact
		 
		 
		 !Defining 3rd dry heifer pools
		 third_dry_death = (nHeifer_third_dry*kMortality)/365
         third_dry_cull = (nHeifer_third_dry*thirdCull)/365
         third_dry_age_out = (nHeifer_third_dry/60)
         nHeifer_third_dry = nHeifer_third_dry + third_age_out - third_dry_age_out - third_dry_cull - third_dry_death
		 iHeifer_third_dry = nHeifer_third_dry
		 
		 !Defining mature lactation heifer pools
		 lact_death = (nLact*kMortality)/365
         lact_cull = (nLact*fourthCull)/365
         fourth_age_out = (nLact/305)
		 nLact = nLact + third_dry_age_out - fourth_age_out -lact_death-lact_cull
         iLact = nLact
		 !Defining mature dry heifer pools		 
		 dry_death = (nDry*kMortality)/365
         dry_cull = (nDry*thirdCull)/365
         dry_age_out = (nDry/60)
         nDry = nDry + fourth_age_out - dry_age_out - dry_cull - dry_death
         iDry = nDry
         nBarn = nHeifer_first_lact + nHeifer_second_lact + nHeifer_third_lact + nLact
		 nPasture = nCalf + nHeifer_first_dry + nHeifer_second_dry + nHeifer_third_dry + nDry
         end if

!##########################################################################
!    calculation of    CETI | Current month’s effective temperature index, C  (fox et al 2004)
!#####################################################################
      CETI = 27.88 - (0.456 * Temp) + (0.010754 * Temp**2)- &
     &  (0.4905 * RHMD) + (0.00088 * RHMD**2)+ (1.1507 * WS) - &
     & (0.126447 * WS**2)+ (0.019876 * Temp * RHMD)- &
     & (0.046313 * Temp * WS)+ (0.4167 * HRS)
!#######################################################################
! calculation of DMINC |  DMI (DRY MATTER INTAKE) night cooling adjustment, dimensionless
!#########################################################################
      DMINC = (119.62 - 0.9708 * CETI)/100
!################################################# DMIF_temp is DMI adjustment factor with night cooling, dimensionless
      if(Temp .ge. 20) then 
        DMIAF_temp = DMINC
      end if 
      if(Temp .lt. 20) then 
        DMIAF_temp = 1.0433 - (0.0044 * Temp) + (0.0001 * Temp**2)
      end if
!#############################################  NEma is Dietary content of net energy for maintenance (kcal/kg)
      calf_NEma = (1.37 * calf_ME) - (0.138 * calf_ME**2) + (0.0105 * calf_ME**3) - 1.12
      heifer_NEma = (1.37 * heifer_ME) - (0.138 * heifer_ME**2) + (0.0105 * heifer_ME**3) - 1.12

      !correction for breed index (fox et al 2004)
      if(Breed == 1) then 
        BI = 1.08
      else 
        BI = 1
      end if

      ! calculations of weights
      
      !wt updating which is a function of t (time)
      wtLact = dairy_ADG(2500)
      wtDry = dairy_ADG(2500)
      wtCalf = dairy_ADG(365)
      wtHeifer_first_lact = dairy_ADG(365*2)
      wtHeifer_second_lact = dairy_ADG(365*3)
      wtHeifer_third_lact = dairy_ADG(365*4)
      WTHFD = dairy_ADG(365*2+305)
      WTSFD = dairy_ADG(365*3+305)
      WTHTD = dairy_ADG(365*4+305)
      
      ! cow DMIs dry matter intake
      lact_DMI = lactDMI(wtLact)
      calf_DMI = calfDMI(wtCalf)
      heifer_first_lact_DMI = heiferDMI(wtHeifer_first_lact)
      heifer_second_lact_DMI = heiferDMI(wtHeifer_second_lact)
      heifer_third_lact_DMI = heiferDMI(wtHeifer_third_lact)
      heifer_first_dry_DMI = heiferDMI(WTHFD)
      heifer_second_dry_DMI = heiferDMI(WTSFD)
      heifer_third_dry_DMI = heiferDMI(WTHTD)
      dry_DMI = dryDMI(wtDry)
!##################################################
	   !N and P produced by 1 
!##################################################
      N_dry = dryNexc(dry_DMI)
      N_calf = calfNexc(calf_DMI)
      N_heifer_first_dry = heiferNexc(heifer_first_dry_DMI)
      A = N_heifer_first_dry
      N_heifer_second_dry = heiferNexc(heifer_second_dry_DMI)
      B = N_heifer_second_dry
      N_heifer_third_dry = heiferNexc(heifer_third_dry_DMI)
      C = N_heifer_third_dry
      P_dry = dryPexc(dry_DMI,fed_dry_P)
      P_calf = calfPexc(calf_DMI)
      P_heifer_first_dry = dryPexc(heifer_first_dry_DMI, fed_heifer_P)
      D = P_heifer_first_dry
      P_heifer_second_dry = dryPexc(heifer_second_dry_DMI, fed_heifer_P)
      E = P_heifer_second_dry
      P_heifer_third_dry = dryPexc(heifer_third_dry_DMI, fed_heifer_P)
      F = P_heifer_third_dry
	  !total N+P produced by dry cows
      NP_dry = N_dry + P_dry
	  ! total N+P produced by calves
      NP_calf = N_calf + P_calf
	  !total N+P produced by Heifer first dry cows
      NP_HFD = N_heifer_first_dry + P_heifer_first_dry
	  !total N+P produced by heifer second dry
      NP_HSD = N_heifer_second_dry + P_heifer_second_dry
	  ! total N+P produced by heifer third druy cows
      NP_HTD = N_heifer_third_dry + P_heifer_third_dry
!#####################################################
      !N and P updates for the number of animals multiply by the number of animal in each pool
!#####################################################
!###N is just a function of DMI
      lact_N = lactNexc(lact_DMI)*nLact
      dry_N = dryNexc(dry_DMI)*nDry
      calf_N = calfNexc(calf_DMI)*nCalf
      heifer_first_lact_N = heiferNexc(heifer_first_lact_DMI)*nHeifer_first_lact
      heifer_second_lact_N = heiferNexc(heifer_second_lact_DMI)*nHeifer_second_lact
      heifer_third_lact_N = heiferNexc(heifer_third_lact_DMI)*nHeifer_third_lact
      heifer_first_dry_N = heiferNexc(heifer_first_dry_DMI)*nHeifer_first_dry
      heifer_second_dry_N = heiferNexc(heifer_second_dry_DMI)*nHeifer_second_dry
      heifer_third_dry_N = heiferNexc(heifer_third_dry_DMI)*nHeifer_third_dry
!! #########P is a fuction of DMI and fed_P	except calves  
      lact_P = cowPexc(lact_DMI,fed_lact_P)*nLact
      dry_P = dryPexc(dry_DMI,fed_dry_P)*nDry
      calf_P = calfPexc(calf_DMI)*nCalf
      heifer_first_lact_P = cowPexc(heifer_first_lact_DMI, fed_heifer_P)*nHeifer_first_lact
      heifer_second_lact_P = cowPexc(heifer_second_lact_DMI,  fed_heifer_P)*nHeifer_second_lact
      heifer_third_lact_P = cowPexc(heifer_third_lact_DMI,  fed_heifer_P)*nHeifer_third_lact
      heifer_first_dry_P = dryPexc(heifer_first_dry_DMI, fed_heifer_P)*nHeifer_first_dry
      heifer_second_dry_P = dryPexc(heifer_second_dry_DMI, fed_heifer_P)*nHeifer_second_dry
      heifer_third_dry_P = dryPexc(heifer_third_dry_DMI, fed_heifer_P)*nHeifer_third_dry
!#####################################TOTAL N and P in Barn AND PASTURE
      total_N_barn = lact_N+heifer_first_lact_N+heifer_second_lact_N+ &
     & heifer_third_lact_N
      total_P_barn = lact_P+heifer_first_lact_P+heifer_second_lact_P+ &
     & heifer_third_lact_P
	 total_N_Past = dry_N + calf_N + heifer_first_dry_N + heifer_second_dry_N + &
	 & heifer_third_dry_N
	 total_P_Past = dry_P + calf_P + heifer_first_dry_P + heifer_second_dry_P + &
	 & heifer_third_dry_P
! ################################################# TOTAL N AND P STORED IN THE BARN
      Total_N_barn_stored = Total_N_barn_stored + total_N_barn !total_N_BARN that stored
      Total_P_barn_stored = Total_P_barn_stored + total_P_barn  !total_P_BARN that stored
!  #################################################
! ##################TOTAL N AND P DEPOSITED IN THE PASTURE
      Total_N_past_deposited = Total_N_past_deposited + total_N_Past
      Total_P_past_deposited = Total_P_past_deposited + total_P_Past
!######################################################
      !eghball 2002 
      Pmin = total_P_barn * 0.75 ! grams 
      Porg = total_P_barn * 0.25 ! grams
      PminP = total_P_Past * 0.75 ! grams
      PorgP = total_P_Past * 0.25 ! gramS
      Pmin_stored = Total_P_barn_stored * 0.75 !grms
      Porg_stored = Total_P_barn_stored * 0.25 !grms
      Pmin_deposited = Total_P_past_deposited * 0.75 !grms
      Porg_deposited = Total_P_past_deposited * 0.25 !grms
!#########################################################
      ! Van Kessel 2002 
      Nmin = total_N_barn * 0.4 ! grams  
      Norg = total_N_barn * 0.6 ! grams
      Nmin_stored = Total_N_barn_stored * 0.4 !grms
      Norg_stored = Total_N_barn_stored * 0.6 !grms
      NminP = total_N_Past * 0.4 ! grams  
      NorgP = total_N_Past * 0.6 ! grams
      Nmin_deposited = Total_N_past_deposited * 0.4 !grms
      Norg_deposited = Total_N_past_deposited * 0.6 !grms
!###########################################################
!###############################TOTAL N+P FROM STORED MANURE IN BARN 
      TotalNP_barn = (Total_N_barn_stored + Total_P_barn_stored)/1000 !kg
      if (Frt_Man < TotalNP_barn) TotalNP_barn = TotalNP_barn - Frt_Man
      if (Frt_Man > TotalNP_barn) STOP
! ################# TOTAL N+P FROM DEPOSITED MANURE IN PASTURE      
      TotalNP_past = (Total_N_past_deposited + Total_P_past_deposited)/1000 !kg
! ################### MEAT PRODUCTION Kg
      meat_produced = culling*(wtLact)
! #################################
      !Get Fractions
! ##################################
	  Norg_barn = Norg/1000.0  !Kg
	  Nmin_barn = Nmin/1000.0  !Kg
	  Porg_barn = Porg/1000.0  !kg
	  Pmin_barn = Pmin/1000.0  !kg
	  Norg_past = NorgP/1000.0 !kg
	  Nmin_past = NminP/1000.0 !kg
	  Porg_past = PorgP/1000.0  !kg
	  Pmin_past = PminP/1000.0  !kg
      Norg_stored_barn = Norg_stored/1000.0 !kg
      Nmin_stored_barn = Nmin_stored/1000.0 !kg
      Porg_stored_barn = Porg_stored/1000.0 !kg
      Pmin_stored_barn =  Pmin_stored/1000.0 !kg
      Norg_deposited_past = Norg_deposited/1000.0 !kg
      Nmin_deopsited_past = Nmin_deposited/1000.0 !kg
      Porg_deposited_past = Porg_deposited/1000.0 !kg
      Pmin_deposited_past = Pmin_deposited/1000.0 !kg
!###################fractions
	  if (TotalNP_barn .eq. 0) then
	  
	        Pmin_frac = 0.
            Porg_frac = 0.
            Norg_frac = 0.
            Nmin_frac = 0.	           
	  else   
         Pmin_frac = Pmin_stored_barn/TotalNP_barn
         Porg_frac = Porg_stored_barn/TotalNP_barn
         Norg_frac = Norg_stored_barn/TotalNP_barn
         Nmin_frac = Nmin_stored_barn/TotalNP_barn
      end if
	  if (TotalNP_past .eq. 0) then
	  
            PminP_frac = 0.
            PorgP_frac = 0.
            NorgP_frac = 0.
            NminP_frac = 0.	
       else			
         PminP_frac = Pmin_deposited_past/TotalNP_past
         PorgP_frac = Porg_deposited_past/TotalNP_past
         NorgP_frac = Norg_deposited_past/TotalNP_past
         NminP_frac = Nmin_deopsited_past/TotalNP_past
      end if
      total_N_barn = total_N_barn/1000.0 !kg
      total_P_barn = total_P_barn/1000.0  !kg
      total_N_past = total_N_past/1000.0  !kg
      total_P_past = total_P_past/1000.0  !kg
	  
	  
   

      write(1798,9008)i,TotalNP_barn,total_N_barn,total_P_barn,total_N_Past, &
     & total_P_Past,Norg_barn,Nmin_barn,Porg_barn,Pmin_barn, &
     & Norg_past,Nmin_past,Porg_past,Pmin_past,Pmin_frac,Porg_frac,&
     & Norg_frac,Nmin_frac 

	  return 
9008     format(i4,1x,17f15.3)

      ! subroutines 
      contains
! #####  BW function 
            function dairy_ADG(i) result(BW)
                implicit none
                integer, intent(in) :: i
                real :: BW, A, k, b, M
                A = 619 !asymptotic weight, kg
                k = 0.0020 !Rate parameter
                b = 0.905 ! integration constant
                M = 1.2386 ! inflection parameter
                BW = A*(1-(b*exp(-k*i)))**M
                return 
            end function dairy_ADG
! ######### DMI dry matter intak function for calf
            function calfDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW !SHRUNK BODY WEIGHT
                  DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.1128)/calf_NEma)*DMIAF_temp*BI
                  return
            end function calfDMI
! ############DMI function for heifers
            function heiferDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW
                  DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.0869)/heifer_NEma)*DMIAF_temp*BI
                  return
            end function heiferDMI
! #############DMI function for lact
            function lactDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW
                  DMI = ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
                  return
            end function lactDMI
!  ################ DMI for dry cows
            function dryDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW
                  DMI = (0.0185 * SBW)*DMIAF_temp*BI
                  return
            end function dryDMI
!##################################################
            ! Nitrogen Equations per cow
! ###################################################
            function heiferNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = ((((heifer_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4) !per cow
                  return
            end function heiferNexc

            function calfNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = ((((calf_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*112.55)
                  return
            end function calfNexc

            function lactNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = (((((lact_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*84.1)+(wtLact*0.196))
                  return
            end function lactNexc

            function dryNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = ((((dry_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4)
                  return
            end function dryNexc
!#####################################################
            ! Phosphorus Equations
!######################################################
! ####calf
            function calfPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = ((((fed_calf_P/100)*(DMI*1000))/(DMI*1000)*DMI)*622.03)
                  return
            end function calfPexc
 ! ### heifer and lact 
           function cowPexc(DMI, fed_P) result(Pexc)
                  real, intent(in) :: DMI, fed_P
                  real :: Pexc
                  Pexc = 7.5 + ((((fed_P/100)*(DMI*1000))/(DMI*1000)*DMI)*560.7+2.1 )
                  return
            end function cowPexc
   ! ###### dry          
            function dryPexc(DMI, fed_P) result(Pexc)
                  real, intent(in) :: DMI, fed_P
                  real :: Pexc
                  Pexc = ((fed_P/100)*(DMI*1000)*0.76)
                  return
            end function dryPexc
            end subroutine
