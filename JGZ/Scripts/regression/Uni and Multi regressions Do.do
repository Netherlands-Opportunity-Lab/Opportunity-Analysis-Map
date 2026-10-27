
********************************************************************************
* Generate variables for regression
********************************************************************************
clear
use "H:\Coen\TempData\Data_CatchmentArea_3pooled_withvariables.dta"

label define l_org 6 "LimburgNoord" 7 "ZONL" 10 "Verian" 12 "org12"
label values org l_org

sum gbageboortejaar if gem!="Apeldoorn"

*IMPORTANT: DROP GEMEENTE APELDOORN BECAUSE IT IS NOT INCLUDED IN THE DATA
drop if gemeente=="Apeldoorn"

destring gbageslacht, replace //sex variable is currently a string

*0. Categories for known/unknown status of parents

	*Create numeric rin variables for father and mother
	destring rinpersoonma, gen(num_rinma) force
	sum num_rinma

	destring rinpersoonpa, gen(num_rinpa) force
	sum num_rinpa

	*Create categorical variable
	gen c_status_parents=.
	replace c_status_parents=1 if num_rinma!=. & num_rinpa!=. // both parents known
	replace c_status_parents=2 if num_rinma==. & num_rinpa!=. // only mother unknown
	replace c_status_parents=3 if num_rinma!=. & num_rinpa==. // only father unknown
	replace c_status_parents=4 if num_rinma==. & num_rinpa==. // both parents unknown

	tab c_status_parents
	
	*Dummy variable for both parents being known/unknown
	gen d_parents_unknown =  (c_status_parents!=1)

*1. Birth order
	sum birth_order // gives the birth rank of the child among all children born from the same mother 

	*Categories:
	gen c_birth_order=.
	replace c_birth_order=1 if birth_order==1
	replace c_birth_order=2 if birth_order==2
	replace c_birth_order=3 if birth_order==3
	replace c_birth_order=4 if birth_order>=4 & birth_order!=.
	
*2. Ethnicity
	tab ETNGRP	

*3. Income parents in calendar year of contact moment
	*father
	sum cm_inppersbrutpa // gives gross income of father in calendar year of contact moment
	*mother
	sum cm_inppersbrutma // gives gross income of mother in calendar year of contact moment
	*parents combined 
	gen cm_inc_parents = cm_inppersbrutpa + cm_inppersbrutma
	replace cm_inc_parents = cm_inppersbrutpa if cm_inppersbrutma==.
	replace cm_inc_parents = cm_inppersbrutma if cm_inppersbrutpa==.
	sum cm_inc_parents // gives gross income of both parents in calendar calendaryear of contact moment
	
		*For children who have less than 2 legal parents replace income by household income of the registered "hoofdkostwinner"
		replace cm_inc_parents = inhbrutinkh if inrange(c_status_parents, 2, 4)
	
		*Set to zero for remaining missings
		replace cm_inc_parents = 0 if cm_inc_parents==.
		
		*Adjust for CPI based on Statline statistics (2015=100; see excel file in 202009 folder)
			*note: I use income 3 years after year of birth_inpp100pbrutma
						*		CPI
						*Perioden2015 = 100
						*2007	87,20
						*2008	89,37
						*2009	90,44
						*2010	91,59
						*2011	93,73
						*2012	96,04
						*2013	98,44
						*2014	99,40
						*2015	100,00
						*2016	100,32
						*2017	101,70
						*2018	103,44
						*2019	106,16
			gen cpi_inc_parents=.
			forvalues i=2008(1)2014{
				
				if `i'==2008 local cpi=.9373
				if `i'==2009 local cpi=.9604
				if `i'==2010 local cpi=.9844
				if `i'==2011 local cpi=.9940
				if `i'==2012 local cpi=1
				if `i'==2013 local cpi=1.0032
				if `i'==2014 local cpi=1.0170
			
				replace cpi_inc_parents = cm_inc_parents / `cpi'  if gbageboortejaar==`i'
			}
			
		
	*Deciles for parents combined
			*OLD CODE BY BIRTHYEAR: egen cm_inc_decile = xtile(cm_inc_parents), by(gbageboortejaar) p(10(10)90)
	egen cm_inc_decile = xtile(cm_inc_parents), p(10(10)90)
	tab cm_inc_decile // divides parents' income into 10 equal sized groups 
	
			*Create scalars for the 9 cut-points of deciles
				forvalues i=1(1)9 {
				
					sum cm_inc_parents if cm_inc_decile==`i'
					scalar cut_`i'=r(max)
			
				}
		
		
		
*4. Educational attainment of parents
	*father
	tab pa_ISCED2011LevelHB // highest level of completed education for father in ISCED2011 categories (see wikipedia)
	*mother
	tab ma_ISCED2011LevelHB // highest level of completed education for mother in ISCED2011 categories (see wikipedia)

	gen c_educ_pa=.
	replace c_educ_pa=1 if pa_ISCED2011LevelHB==0 | pa_ISCED2011LevelHB==1
	replace c_educ_pa=2 if pa_ISCED2011LevelHB==2
	replace c_educ_pa=3 if pa_ISCED2011LevelHB==3
	replace c_educ_pa=4 if pa_ISCED2011LevelHB==4 | pa_ISCED2011LevelHB==5
	replace c_educ_pa=5 if pa_ISCED2011LevelHB>=6 & pa_ISCED2011LevelHB!=.
	
	gen c_educ_ma=.
	replace c_educ_ma=1 if ma_ISCED2011LevelHB==0 | ma_ISCED2011LevelHB==1
	replace c_educ_ma=2 if ma_ISCED2011LevelHB==2
	replace c_educ_ma=3 if ma_ISCED2011LevelHB==3
	replace c_educ_ma=4 if ma_ISCED2011LevelHB==4 | ma_ISCED2011LevelHB==5
	replace c_educ_ma=5 if ma_ISCED2011LevelHB>=6 & ma_ISCED2011LevelHB!=.
	
	*Highest educ of both parents
	gen c_educ_parents=c_educ_pa
	replace c_educ_parents=c_educ_ma if c_educ_ma>c_educ_pa

	*Create college indicator (set to 0 for unobserved)
	gen d_college_parents = (c_educ_parents==5 & c_educ_parents!=.)
	
	*Categorical variable for HBO and WO
	gen c_educ_hbo_wo=0
	replace c_educ_hbo_wo=1 if ((pa_OPLNIVSOI2016AGG4HBMETNIRWO==3111 | pa_OPLNIVSOI2016AGG4HBMETNIRWO==3112 | pa_OPLNIVSOI2016AGG4HBMETNIRWO==3211) & pa_OPLNIVSOI2016AGG4HBMETNIRWO!=.) | ((ma_OPLNIVSOI2016AGG4HBMETNIRWO==3111 | ma_OPLNIVSOI2016AGG4HBMETNIRWO==3112 | ma_OPLNIVSOI2016AGG4HBMETNIRWO==3211) & ma_OPLNIVSOI2016AGG4HBMETNIRWO!=.) //hbo tot wo master
	replace c_educ_hbo_wo=2 if ((pa_OPLNIVSOI2016AGG4HBMETNIRWO==3113 | pa_OPLNIVSOI2016AGG4HBMETNIRWO==3212 | pa_OPLNIVSOI2016AGG4HBMETNIRWO==3213) & pa_OPLNIVSOI2016AGG4HBMETNIRWO!=.) | ((ma_OPLNIVSOI2016AGG4HBMETNIRWO==3113 | ma_OPLNIVSOI2016AGG4HBMETNIRWO==3212 | ma_OPLNIVSOI2016AGG4HBMETNIRWO==3213) & ma_OPLNIVSOI2016AGG4HBMETNIRWO!=.) //wo master of hoger
	
*5. Parents on same address as child in month of contact moment
		*child and father
		gen same_address_pa=0
		replace same_address_pa=1 if soortobjectnummer_child==soortobjectnummer_pa & rinobjectnummer_child==rinobjectnummer_pa & rinobjectnummer_child!=""
		
		*child and mother
		gen same_address_ma=0
		replace same_address_ma=1 if soortobjectnummer_child==soortobjectnummer_ma & rinobjectnummer_child==rinobjectnummer_ma & rinobjectnummer_child!=""
		
		*child and both parents
		gen same_address_parents=same_address_pa+same_address_ma
		gen d_same_address_parents=0 if same_address_parents==0 | same_address_parents==1
		replace d_same_address_parents=1 if same_address_parents==2

		*Create categorical variable
		gen c_address_parents=1 if d_same_address_parents==1
		replace c_address_parents=2 if same_address_parents==1 & same_address_ma==1
		replace c_address_parents=3 if same_address_parents==1 & same_address_pa==1
		replace c_address_parents=4 if same_address_parents==0 

		
*6. Age mother at birth of child
	
	*Create quintiles --> CHECK WHAT THIS PICKS UP, IS IT FINE ENOUGHT TO CONSIDER TEENAGE MOMS?
	egen c_agebirth_quintile = xtile(age_birth_first_ma), p(20(20)80)
	tab c_agebirth_quintile 
	
	gen d_teenage=0
	replace d_teenage=1 if age_birth_first_ma<240
	
	gen d_voorzorg_age=0
	replace d_voorzorg_age=1 if age_birth_first_ma<312
	
	*3 groups: (i) >26, (ii) 24-26 and (iii) below 24
	gen age_mom_3groups=1 if age_birth_first_ma>323 & age_birth_first_ma!=.
	replace age_mom_3groups=2 if inrange(age_birth_first_ma, 288, 323)
	replace age_mom_3groups=3 if age_birth_first_ma<288
	
*7. Labor market status parents

	destring cm_inpsecjpa cm_inpsecjma, replace
	
	*Father
	gen d_lms_pa = 1 if inrange(cm_inpsecjpa, 11, 15) // working
	replace d_lms_pa = 2 if cm_inpsecjpa==22 // welfare (bijstand)
	replace d_lms_pa = 3 if cm_inpsecjpa==24 // disability insurance
	replace d_lms_pa = 4 if d_lms_pa==. // all other non-work, incl. missings
	
	*Mother
	gen d_lms_ma = 1 if inrange(cm_inpsecjma, 11, 15) // working
	replace d_lms_ma = 2 if cm_inpsecjma==22 // welfare (bijstand)
	replace d_lms_ma = 3 if cm_inpsecjma==24 // disability insurance
	replace d_lms_ma = 4 if d_lms_ma==. // all other non-work, incl. missings
	
	
	*ALTERNATIVE DEFINITION:
	
		*Father 
		gen c_lms5_pa=1 if cm_inpsecjpa==11 // employed
		replace c_lms5_pa = 2 if inrange(cm_inpsecjpa, 12, 15) // self-employed
		replace c_lms5_pa = 3 if cm_inpsecjpa==22 // welfare (bijstand)
		replace c_lms5_pa = 4 if cm_inpsecjpa==24 // disability insurance
		replace c_lms5_pa = 5 if cm_inpsecjpa==32 // no income
		replace c_lms5_pa = 6 if c_lms5_pa==. // all other non-work, incl. missings
		
		*Mother 
		gen c_lms5_ma=1 if cm_inpsecjma==11 // employed
		replace c_lms5_ma = 2 if inrange(cm_inpsecjma, 12, 15) // self-employed
		replace c_lms5_ma = 3 if cm_inpsecjma==22 // welfare (bijstand)
		replace c_lms5_ma = 4 if cm_inpsecjma==24 // disability insurance
		replace c_lms5_ma = 5 if cm_inpsecjma==32 | cm_inpsecjma==99 // no income
		replace c_lms5_ma = 6 if c_lms5_ma==. // all other non-work, incl. missings
		
	
*8. Indicator for christian municipalities - based on 2014 city council election
		*Merge in municipality code of address at age 36 months
		rename soortobjectnummer temp_soortobjectnummer
		rename rinobjectnummer temp_rinobjectnummer
		rename soortobjectnummer_child soortobjectnummer
		rename rinobjectnummer_child rinobjectnummer
		
		recast str32 rinobjectnummer
		
		merge m:1 soortobjectnummer rinobjectnummer using "H:\Coen\TempData\Municipality_codes_2014.dta", keep(master match) keepusing(gem2014)
		drop _merge
		
		*Merge in election results municipal elections 2014
		merge m:1 gem2014 using  "H:\Coen\TempData\Verkiezing_Gemeenteraad_2014.dta", keep(master match) 
		drop _merge
		
			*Create indicator variable
			gen d_christian_3y=0 if percentagesgpcu<.1
			replace d_christian_3y=1 if percentagesgpcu>=.1
			
*9. Deciles for health care expenditure of child 
	
	*set healthcare costs to 0 if it is unobserved
	replace child_tot_health_exp = 0 if child_tot_health_exp==.
	
	*Adjust for CPI based on Statline statistics (2015=100; see excel file in 202009 folder)
			*note: I use income 3 years after year of birth_inpp100pbrutma
						*		CPI
						*Perioden2015 = 100
						*2007	87,20
						*2008	89,37
						*2009	90,44
						*2010	91,59
						*2011	93,73
						*2012	96,04
						*2013	98,44
						*2014	99,40
						*2015	100,00
						*2016	100,32
						*2017	101,70
						*2018	103,44
						*2019	106,16
			gen cpi_health_exp=.
			forvalues i=2008(1)2014{
				
				if `i'==2008 local cpi=.9373
				if `i'==2009 local cpi=.9604
				if `i'==2010 local cpi=.9844
				if `i'==2011 local cpi=.9940
				if `i'==2012 local cpi=1
				if `i'==2013 local cpi=1.0032
				if `i'==2014 local cpi=1.0170
			
				replace cpi_health_exp = child_tot_health_exp / `cpi'  if gbageboortejaar==`i'
			}
			
			
	*Create deciles for expenditures
	egen health_exp_decile = xtile(cpi_health_exp), p(10(10)90)
	
			*Create scalars for the 9 cut-points of deciles for each birth cohort
				forvalues i=1(1)9 {
				
					sum child_tot_health_exp if health_exp_decile==`i' 
					scalar cut_health_`i'=r(max)
			
				}
		
	
*10. Indicator for a health care center in the neighborhood
	*NOTE: You still need to check which postalcode you select for a child, at what age (children who move can have multiple, for consistency also take 36 months here) - BElOW you merge in postal codes at age 3
	gen d_health_center = 0

	local pc4_centers ///
	5953 5854 6101 6591 5961 5977 6095 6093 6051 6031 5991 5993 5768 5981 6077 6049 6042 5916 5921 5914 5931 5802 5863 6001 6006 /// Limburg-Noord
	8331 8356 8355 8375 8341 8326 8325 8303 8309 8316 8307 8312 8322 8375 8309 8316 8307 /// ZONL
	7361 6971 6961 8161 8051 8181 8167 7391 3888 8171 8191 7333 7312 7316 7329 7323 /// Verian
	9761 9481 9471 9451 9468 9514 9461 7741 7841 7861 7944 7948 3891 8256 8251 8255 8224 8232 8226 8091 8096 8081 8084 8075 8071 3844 3851 3881 // org12

	foreach pc4 of local pc4_centers {
		replace d_health_center = 1 if postcode_4==`pc4'
	}
	

	*Merge in postal code at age 3
	gen rinobjectnummer_str = substr(rinobjectnummer, 1, 32)
drop rinobjectnummer
gen rinobjectnummer = soortobjectnummer + rinobjectnummer_str
drop rinobjectnummer_str

	merge m:1 rinobjectnummer using "H:\Sonya\JGZ pilot data\postcodes_latest.dta", keep(master match)
	drop _merge
	
	replace postcodenum ="" if missing(real(postcodenum)) // 2 observations contain missing postcodes that prevent the variable from destringing
	destring postcodenum, replace
	rename postcodenum pc4_3y
	
			gen d_health_center_3y = 0

			local pc4_centers ///
			5953 5854 6101 6591 5961 5977 6095 6093 6051 6031 5991 5993 5768 5981 6077 6049 6042 5916 5921 5914 5931 5802 5863 6001 6006 /// Limburg-Noord
			8331 8356 8355 8375 8341 8326 8325 8303 8309 8316 8307 8312 8322 8375 8309 8316 8307 /// ZONL
			7361 6971 6961 8161 8051 8181 8167 7391 3888 8171 8191 7333 7312 7316 7329 7323 /// Verian
			9761 9481 9471 9451 9468 9514 9461 7741 7841 7861 7944 7948 3891 8256 8251 8255 8224 8232 8226 8091 8096 8081 8084 8075 8071 3844 3851 3881 // org12

			foreach pc4 of local pc4_centers {
				replace d_health_center_3y = 1 if pc4_3y==`pc4'
			}

 
*11. Religion and church visits
	merge m:1 gem2014 using  "H:\Coen\TempData\Religie_kerkbezoek_2010_2014.dta", keep(master match) keepusing(maandelijks_bezoek kerk_gezindte katholiek hervormd gereformeerd pkn islam joods hindoe boeddhist anders)
	drop _merge
	
	*Generate categorical variables for religion
	set trace on
	set tracedepth 1
	
	local religions maandelijks_bezoek katholiek hervormd gereformeerd pkn islam
	foreach religion of local religions {
	
		gen c_`religion'=0 if `religion'>=0 & `religion'<20
		
		local j=1
		forvalues i=20(10)40 {
			replace c_`religion'=`j' if `religion'>=`i' & `religion'<`i'+10
		
			local j = `j' + 1
		}
	replace  c_`religion'=4 if `religion'>=50
	}
	
	*Create variable for dominant religion
	gen aux_dominant_religion=max(katholiek, hervormd, gereformeerd, pkn)
	gen c_dominant_religion=1 if aux_dominant_religion==katholiek
	replace c_dominant_religion=2 if aux_dominant_religion==hervormd
	replace c_dominant_religion=3 if aux_dominant_religion==gereformeerd
	replace c_dominant_religion=4 if aux_dominant_religion==pkn
	

	
*CHECK WHICH VARIABLES CAN BE OBSERVED FOR WHO
sum d_obs_cat c_birth_order	ETNGRP cm_inc_decile c_educ_parent d_college_parents c_educ_hbo_wo c_address_parents d_lms_pa d_lms_ma c_agebirth_quintile d_teenage d_voorzorg_age gbageslacht d_christian_3y pa_d_mental_care ma_d_mental_care health_exp_decile d_health_center if gemeente!="Apeldoorn"


*Set parent variables to reference category in case parent is unknown 
	*Birth order: set to 1 for children without known mother
	replace c_birth_order = 1 if num_rinma==.
	*Age groups of mothers first child
	replace age_mom_3groups=1 if num_rinma==.

	
*Set variables to reference group that do not have a values
	*Mental health care
	replace pa_d_mental_care=0 if pa_d_mental_care==.
	replace ma_d_mental_care=0 if ma_d_mental_care==.
	

save "H:\Sonya\JGZ pilot data\Data_CatchmentArea_3pooled_morevariables.dta", replace


********************************************************************************
* Regression
********************************************************************************	
use "H:\Sonya\JGZ pilot data\Data_CatchmentArea_3pooled_morevariables.dta", clear

*Generate variable for always observed
gen d_obs_always = (d_obs_cat==2)


*Variables for regression
	*1. Birth order - 4 categories
	*2. Ethnicity - 7 categories
	*3. Income - 10 deciles
	*4. Educational attainment - 3 categories
	*5. Child and parent addresses - 4 categories
	*6. Mom's age at birth first child - 5 quintiles 
	*7. Labor market status father - 6 categories
	*8. Labor market status mother - 6 categories
	*9. Gender - 2 categories
	*10. Indicator Christian municipality - 2 categories
	*11. Indicator mental healthcare use father - 2 categories
	*12. Indicator mental healthcare use mother - 2 categories
	*13. Deciles for child tot health care expenditure - 10 categories
	*14. Indicator for healthcare center in neighborhood
	
	
********************************************************************************
* Individual + local characteristics
********************************************************************************		
*OUTCOME 1: ever observed

set trace on
set tracedepth 1

	matrix obs_ever = J(67, 17, .)
	
	local i=0

*Univariate
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
	reg d_observed_3cms i.`regressor'
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_ever[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_ever[`i', 2]= _b[`level'.`regressor']
			matrix obs_ever[`i', 3] = _se[`level'.`regressor']
			matrix obs_ever[`i', 4] = _b[_cons]
			matrix obs_ever[`i', 5] = _se[_cons]
			matrix obs_ever[`i', 6] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_ever[`i', 7]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_ever[`i', 8]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
			*Add number of individuals (N) in each group
			sum `regressor' if `regressor' == `level'
			local group=r(N)
			matrix obs_ever[`i', 16]=r(N)
			
			*Add percentage of total regression
			sum `regressor'
			local total=r(N)
			matrix obs_ever[`i', 17]=`group'/`total'
			
		}
		
		matrix list obs_ever
	
	}
	
*Multivariate	
	
	local i=0
	
	reg d_observed_3cms i.c_birth_order i.ETNGRP i.cm_inc_decile i.c_educ_hbo_wo i.c_address_parents i.age_mom_3groups i.c_lms5_pa i.c_lms5_ma i.gbageslacht i.pa_d_mental_care i.ma_d_mental_care i.health_exp_decile i.d_parents_unknown i.d_health_center_3y i.d_christian_3y
	
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_ever[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_ever[`i', 9]= _b[`level'.`regressor']
			matrix obs_ever[`i', 10] = _se[`level'.`regressor']
			matrix obs_ever[`i', 11] = _b[_cons]
			matrix obs_ever[`i', 12] = _se[_cons]
			matrix obs_ever[`i', 13] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_ever[`i', 14]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_ever[`i', 15]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
		}
		
		matrix list obs_ever
		
		matrix colnames obs_ever = Counter Uni_Coef Uni_Coef_SE Uni_Const Uni_Const_SE Uni_Pval Uni_LB95 Uni_UB95 Multi_Coef Multi_Coef_SE Multi_Const Multi_Const_SE Multi_Pval Multi_LB95 Multi_UB95 N Frac_observed
		
		matrix rownames obs_ever =  "First born"  "Second born"  "Third born"  "Fourth born or later" ///
 "Native Dutch"  "Moroccan"  "Turkish"  "Surinamese" "Dutch Antilles"  "Other Western"  "Other Non-Western" ///
 "Income: 1st decile"  "Income: 2nd decile"  "Income: 3rd decile"  "Income: 4th decile"  "Income: 5th decile" ///
 "Income: 6th decile"  "Income: 7th decile" "Income: 8th decile"  "Income: 9th decile"  "Income: 10th decile" ///
 "Education: vocational"  "Education: college (HBO)"  "Education: academic (WO)" ///
 "Household: both parents"  "Household: mother only"  "Household: father only"  "Household: no parents" ///
 "Age mother: >26"  "Age mother: 24-26"  "Age mother: <24" ///
 "Father: employed"   "Father: self-employed"  "Father: welfare"  "Father: disabled"  "Father: no income"  "Father: other" ///
 "Mother: employed"   "Mother: self-employed"  "Mother: welfare"  "Mother: disabled"  "Mother: no income"  "Mother: other" ///
 "Gender: boys"  "Gender: girls" ///
 "Father: no mental healthcare"  "Father: mental healthcare" ///
 "Mother: no mental healthcare"  "Mother: mental healthcare" ///
 "Health expenditure: 1st decile"  "Health expenditure: 2nd decile" "Health expenditure: 3rd decile"  "Health expenditure: 4th decile"  "Health expenditure: 5th decile" ///
"Health expenditure: 6th decile"  "Health expenditure: 7th decile"  "Health expenditure: 8th decile"  "Health expenditure: 9th decile"  "Health expenditure: 10th decile" ///
 "Parents: both known"  "Parents: one or both unknown" ///
 "Neighborhood: no health center"  "Neighborhood: with health center" ///
 "Municipality: Christian<.10"  "Municipality: Christian>=.10" 
				
		*Write matrix to excel
		putexcel set "H:\Sonya\JGZ pilot data/Bereik Regression Results\obs_ever_all.xls", sheet(obs_ever) replace
		putexcel A1 = matrix(obs_ever), names
	
	}
	
*Create plot

svmat obs_ever

rename obs_ever1 obs_ever_count
rename obs_ever2 obs_ever_uni_coef
rename obs_ever7 obs_ever_uni_lb
rename obs_ever8 obs_ever_uni_ub
rename obs_ever9 obs_ever_multi_coef
rename obs_ever14 obs_ever_multi_lb
rename obs_ever15 obs_ever_multi_ub
rename obs_ever16 obs_ever_N
rename obs_ever17 obs_ever_percentage

*Univariate
graph twoway (rcap obs_ever_uni_lb obs_ever_uni_ub obs_ever_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_count obs_ever_uni_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
//name(obs_ever_uni)

*Multivariatie
graph twoway (rcap obs_ever_multi_lb obs_ever_multi_ub obs_ever_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_count obs_ever_multi_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_multi)	

*Compare univariate and multivariate	
graph twoway (rcap obs_ever_uni_lb obs_ever_uni_ub obs_ever_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_count obs_ever_uni_coef, yscale(reverse) mcolor(black)) ///
(rcap obs_ever_multi_lb obs_ever_multi_ub obs_ever_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_count obs_ever_multi_coef, yscale(reverse) mcolor(red)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_compare)
	

	

	
	

	
	
*For each organization separately 
set trace on
set tracedepth 1
local organizations 7 10 12
//local organizations 6

*Univariate
	
	foreach organization in `organizations' {
		
	matrix obs_ever_`organization' = J(67, 17, .)
	
	local i=0
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	
	foreach regressor of local regressors {
	
	reg d_observed_3cms i.`regressor' if org_catchment == `organization'
	
		levels `regressor', local(var_levels)

		foreach level of local var_levels{
			
			local i = `i'+1
			
			matrix obs_ever_`organization'[`i', 1]=`i'
		
			
			
			display _b[`level'.`regressor']
				
			matrix obs_ever_`organization'[`i', 2]= _b[`level'.`regressor']
			matrix obs_ever_`organization'[`i', 3] = _se[`level'.`regressor']
			matrix obs_ever_`organization'[`i', 4] = _b[_cons]
			matrix obs_ever_`organization'[`i', 5] = _se[_cons]
			matrix obs_ever_`organization'[`i', 6] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_ever_`organization'[`i',7]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_ever_`organization'[`i',8]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
			
			*Add number of individuals (N) in each group
			sum `regressor' if `regressor' == `level' & org_catchment == `organization'
			local group=r(N)
			matrix obs_ever_`organization'[`i', 16]=r(N)
		
			*Add percentage of total regression
			sum `regressor' if org_catchment == `organization'
			local total=r(N)
			matrix obs_ever_`organization'[`i', 17]= `group'/`total'
			
		
		}
		
		matrix list obs_ever_`organization'
	
	}
	}
	
*Multivariate	



foreach organization in `organizations' {
	
	local i=0
	
	reg d_observed_3cms i.c_birth_order i.ETNGRP i.cm_inc_decile i.c_educ_hbo_wo i.c_address_parents i.age_mom_3groups i.c_lms5_pa i.c_lms5_ma i.gbageslacht i.pa_d_mental_care i.ma_d_mental_care i.health_exp_decile i.d_parents_unknown i.d_health_center_3y i.d_christian_3y if org_catchment == `organization'
	
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
		levels `regressor', local(var_levels)
		
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_ever_`organization'[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_ever_`organization'[`i', 9]= _b[`level'.`regressor']
			matrix obs_ever_`organization'[`i', 10] = _se[`level'.`regressor']
			matrix obs_ever_`organization'[`i', 11] = _b[_cons]
			matrix obs_ever_`organization'[`i', 12] = _se[_cons]
			matrix obs_ever_`organization'[`i', 13] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_ever_`organization'[`i', 14]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_ever_`organization'[`i', 15]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
		}
		
		matrix list obs_ever_`organization'
		
		matrix colnames obs_ever = Counter Uni_Coef Uni_Coef_SE Uni_Const Uni_Const_SE Uni_Pval Uni_LB95 Uni_UB95 Multi_Coef Multi_Coefi_Const Multi_Pval Multi_LB95 Multi_UB95 N Frac_observed
		
		matrix rownames obs_ever_`organization' =  "First born"  "Second born"  "Third born"  "Fourth born or later" ///
 "Native Dutch"  "Moroccan"  "Turkish"  "Surinamese" "Dutch Antilles"  "Other Western"  "Other Non-Western" ///
 "Income: 1st decile"  "Income: 2nd decile"  "Income: 3rd decile"  "Income: 4th decile"  "Income: 5th decile" ///
 "Income: 6th decile"  "Income: 7th decile" "Income: 8th decile"  "Income: 9th decile"  "Income: 10th decile" ///
 "Education: vocational"  "Education: college (HBO)"  "Education: academic (WO)" ///
 "Household: both parents"  "Household: mother only"  "Household: father only"  "Household: no parents" ///
 "Age mother: >26"  "Age mother: 24-26"  "Age mother: <24" ///
 "Father: employed"   "Father: self-employed"  "Father: welfare"  "Father: disabled"  "Father: no income"  "Father: other" ///
 "Mother: employed"   "Mother: self-employed"  "Mother: welfare"  "Mother: disabled"  "Mother: no income"  "Mother: other" ///
 "Gender: boys"  "Gender: girls" ///
 "Father: no mental healthcare"  "Father: mental healthcare" ///
 "Mother: no mental healthcare"  "Mother: mental healthcare" ///
 "Health expenditure: 1st decile"  "Health expenditure: 2nd decile" "Health expenditure: 3rd decile"  "Health expenditure: 4th decile"  "Health expenditure: 5th decile" ///
"Health expenditure: 6th decile"  "Health expenditure: 7th decile"  "Health expenditure: 8th decile"  "Health expenditure: 9th decile"  "Health expenditure: 10th decile" ///
 "Parents: both known"  "Parents: one or both unknown" ///
 "Neighborhood: no health center"  "Neighborhood: with health center" ///
 "Municipality: Christian<.10"  "Municipality: Christian>=.10" 
				
		*Write matrix to excel
		putexcel set "H:\Sonya\JGZ pilot data\Bereik Regression Results\obs_ever_`organization'.xls", sheet(obs_ever_`organization') replace
		putexcel A1 = matrix(obs_ever_`organization'), names
	
	}
}

*Create plots
local organizations 6 7 10 12

foreach organization in `organizations' {

svmat obs_ever_`organization'

rename obs_ever_`organization'1 obs_ever_`organization'_count
rename obs_ever_`organization'2 obs_ever_`organization'_uni_coef
rename obs_ever_`organization'7 obs_ever_`organization'_uni_lb
rename obs_ever_`organization'8 obs_ever_`organization'_uni_ub
rename obs_ever_`organization'9 obs_ever_`organization'_multi_coef
rename obs_ever_`organization'14 obs_ever_`organization'_multi_lb
rename obs_ever_`organization'15 obs_ever_`organization'_multi_ub
rename obs_ever_`organization'16 obs_ever_`organization'_N
rename obs_ever_`organization'17 obs_ever_`organization'_percentage

*Univariate
graph twoway (rcap obs_ever_`organization'_uni_lb obs_ever_`organization'_uni_ub obs_ever_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_`organization'_count obs_ever_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_`organization'_uni)

*Multivariatie
graph twoway (rcap obs_ever_`organization'_multi_lb obs_ever_`organization'_multi_ub obs_ever_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_`organization'_count obs_ever_`organization'_multi_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_`organization'_multi)	

*Compare univariate and multivariate	
graph twoway (rcap obs_ever_`organization'_uni_lb obs_ever_`organization'_uni_ub obs_ever_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_`organization'_count obs_ever_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
(rcap obs_ever_`organization'_multi_lb obs_ever_`organization'_multi_ub obs_ever_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_`organization'_count obs_ever_`organization'_multi_coef, yscale(reverse) mcolor(red)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_`organization'_compare)
	
}

*For all combinations of 3 organizations pooled 
local organizations 6 7 10 12

*Univariate
	
	foreach organization in `organizations' {
		
	matrix obs_ever_excl_`organization' = J(67, 17, .)
	
	local i=0
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	
	foreach regressor of local regressors {
	
	reg d_observed_3cms i.`regressor' if org_catchment != `organization'
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			
			matrix obs_ever_excl_`organization'[`i', 1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_ever_excl_`organization'[`i', 2]= _b[`level'.`regressor']
			matrix obs_ever_excl_`organization'[`i', 3] = _se[`level'.`regressor']
			matrix obs_ever_excl_`organization'[`i', 4] = _b[_cons]
			matrix obs_ever_excl_`organization'[`i', 5] = _se[_cons]
			matrix obs_ever_excl_`organization'[`i', 6] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_ever_excl_`organization'[`i',7]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_ever_excl_`organization'[`i',8]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
			*Add number of individuals (N) in each group
			sum `regressor' if `regressor' == `level' & org_catchment != `organization'
			local group=r(N)
			matrix obs_ever_excl_`organization'[`i', 16]=r(N)
			
			*Add percentage of total regression
			sum `regressor' if org_catchment != `organization'
			local total=r(N)
			matrix obs_ever_excl_`organization'[`i', 17]= `group'/`total'
			
		}
		
		matrix list obs_ever_excl_`organization'
	
	}
	}
	
*Multivariate	

foreach organization in `organizations' {
	
	local i=0
	
	reg d_observed_3cms i.c_birth_order i.ETNGRP i.cm_inc_decile i.c_educ_hbo_wo i.c_address_parents i.age_mom_3groups i.c_lms5_pa i.c_lms5_ma i.gbageslacht i.pa_d_mental_care i.ma_d_mental_care i.health_exp_decile i.d_parents_unknown i.d_health_center_3y i.d_christian_3y if org_catchment != `organization'
	
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_ever_excl_`organization'[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_ever_excl_`organization'[`i', 9]= _b[`level'.`regressor']
			matrix obs_ever_excl_`organization'[`i', 10] = _se[`level'.`regressor']
			matrix obs_ever_excl_`organization'[`i', 11] = _b[_cons]
			matrix obs_ever_excl_`organization'[`i', 12] = _se[_cons]
			matrix obs_ever_excl_`organization'[`i', 13] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_ever_excl_`organization'[`i', 14]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_ever_excl_`organization'[`i', 15]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
		}
		
		matrix list obs_ever_excl_`organization'
		
		matrix colnames obs_ever_excl_`organization' = Counter Uni_Coef Uni_SE Uni_Const Uni_Pval Uni_LB95 Uni_UB95 Multi_Coef Multi_SE Multi_Const Multi_Pval Multi_LB95 Multi_UB95 N Frac_observed
		
		matrix rownames obs_ever_excl_`organization' =  "First born"  "Second born"  "Third born"  "Fourth born or later" ///
 "Native Dutch"  "Moroccan"  "Turkish"  "Surinamese" "Dutch Antilles"  "Other Western"  "Other Non-Western" ///
 "Income: 1st decile"  "Income: 2nd decile"  "Income: 3rd decile"  "Income: 4th decile"  "Income: 5th decile" ///
 "Income: 6th decile"  "Income: 7th decile" "Income: 8th decile"  "Income: 9th decile"  "Income: 10th decile" ///
 "Education: vocational"  "Education: college (HBO)"  "Education: academic (WO)" ///
 "Household: both parents"  "Household: mother only"  "Household: father only"  "Household: no parents" ///
 "Age mother: >26"  "Age mother: 24-26"  "Age mother: <24" ///
 "Father: employed"   "Father: self-employed"  "Father: welfare"  "Father: disabled"  "Father: no income"  "Father: other" ///
 "Mother: employed"   "Mother: self-employed"  "Mother: welfare"  "Mother: disabled"  "Mother: no income"  "Mother: other" ///
 "Gender: boys"  "Gender: girls" ///
 "Father: no mental healthcare"  "Father: mental healthcare" ///
 "Mother: no mental healthcare"  "Mother: mental healthcare" ///
 "Health expenditure: 1st decile"  "Health expenditure: 2nd decile" "Health expenditure: 3rd decile"  "Health expenditure: 4th decile"  "Health expenditure: 5th decile" ///
"Health expenditure: 6th decile"  "Health expenditure: 7th decile"  "Health expenditure: 8th decile"  "Health expenditure: 9th decile"  "Health expenditure: 10th decile" ///
 "Parents: both known"  "Parents: one or both unknown" ///
 "Neighborhood: no health center"  "Neighborhood: with health center" ///
 "Municipality: Christian<.10"  "Municipality: Christian>=.10" 
				
		*Write matrix to excel
		putexcel set "H:\Sonya\JGZ pilot data\Bereik Regression Results\obs_ever_excl_`organization'.xls", sheet(obs_ever_excl_`organization') replace
		putexcel A1 = matrix(obs_ever_excl_`organization'), names
	
	}
}

*Create plots
local organizations 6 7 10 12

foreach organization in `organizations' {

svmat obs_ever_excl_`organization'

rename obs_ever_excl_`organization'1 obs_ever_excl_`organization'_count
rename obs_ever_excl_`organization'2 obs_ever_excl_`organization'_uni_coef
rename obs_ever_excl_`organization'7 obs_ever_excl_`organization'_uni_lb
rename obs_ever_excl_`organization'8 obs_ever_excl_`organization'_uni_ub
rename obs_ever_excl_`organization'9 obs_ever_excl_`organization'_multi_coef
rename obs_ever_excl_`organization'14 obs_ever_excl_`organization'_multi_lb
rename obs_ever_excl_`organization'15 obs_ever_excl_`organization'_multi_ub
rename obs_ever_excl_`organization'16 obs_ever_excl_`organization'_N
rename obs_ever_excl_`organization'17 obs_ever_excl_`organization'_percentage

*Univariate
graph twoway (rcap obs_ever_excl_`organization'_uni_lb obs_ever_excl_`organization'_uni_ub obs_ever_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_excl_`organization'_count obs_ever_excl_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_excl_`organization'_uni)

*Multivariatie
graph twoway (rcap obs_ever_excl_`organization'_multi_lb obs_ever_excl_`organization'_multi_ub obs_ever_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_excl_`organization'_count obs_ever_excl_`organization'_multi_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_excl_`organization'_multi)	

*Compare univariate and multivariate	
graph twoway (rcap obs_ever_excl_`organization'_uni_lb obs_ever_excl_`organization'_uni_ub obs_ever_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_excl_`organization'_count obs_ever_excl_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
(rcap obs_ever_excl_`organization'_multi_lb obs_ever_excl_`organization'_multi_ub obs_ever_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_ever_excl_`organization'_count obs_ever_excl_`organization'_multi_coef, yscale(reverse) mcolor(red)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_ever_excl_`organization'_compare)
	
}





*OUTCOME 2: always observed
*Generate variable for always observed
gen d_obs_always = (d_obs_cat==2)
set trace on
set tracedepth 1

	matrix obs_always = J(67, 17, .)
	
	local i=0

*Univariate
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
	reg d_obs_always i.`regressor'
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_always[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_always[`i', 2]= _b[`level'.`regressor']
			matrix obs_always[`i', 3] = _se[`level'.`regressor']
			matrix obs_always[`i', 4] = _b[_cons]
			matrix obs_always[`i', 5] = _se[_cons]
			matrix obs_always[`i', 6] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always[`i',7]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always[`i',8]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
			*Add number of individuals (N) in each group
			sum `regressor' if `regressor' == `level'
			local group=r(N)
			matrix obs_always[`i', 16]=r(N)
			
			*Add percentage of total regression
			sum `regressor'
			local total=r(N)
			matrix obs_always[`i', 17]=`group'/`total'
			
		}
		
		matrix list obs_always
	
	}
	
*Multivariate	
	
	local i=0
	
	reg d_obs_always i.c_birth_order i.ETNGRP i.cm_inc_decile i.c_educ_hbo_wo i.c_address_parents i.age_mom_3groups i.c_lms5_pa i.c_lms5_ma i.gbageslacht i.pa_d_mental_care i.ma_d_mental_care i.health_exp_decile i.d_parents_unknown i.d_health_center_3y i.d_christian_3y
	
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_always[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_always[`i', 9]= _b[`level'.`regressor']
			matrix obs_always[`i', 10] = _se[`level'.`regressor']
			matrix obs_always[`i', 11] = _b[_cons]
			matrix obs_always[`i', 12] = _se[_cons]
			matrix obs_always[`i', 13] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always[`i', 14]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always[`i', 15]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
		}
		
		matrix list obs_always
		
		matrix colnames obs_always = Counter Uni_Coef Uni_SE Uni_Const Uni_Pval Uni_LB95 Uni_UB95 Multi_Coef Multi_SE Multi_Const Multi_Pval Multi_LB95 Multi_UB95 N Frac_observed
		
		matrix rownames obs_always =  "First born"  "Second born"  "Third born"  "Fourth born or later" ///
 "Native Dutch"  "Moroccan"  "Turkish"  "Surinamese" "Dutch Antilles"  "Other Western"  "Other Non-Western" ///
 "Income: 1st decile"  "Income: 2nd decile"  "Income: 3rd decile"  "Income: 4th decile"  "Income: 5th decile" ///
 "Income: 6th decile"  "Income: 7th decile" "Income: 8th decile"  "Income: 9th decile"  "Income: 10th decile" ///
 "Education: vocational"  "Education: college (HBO)"  "Education: academic (WO)" ///
 "Household: both parents"  "Household: mother only"  "Household: father only"  "Household: no parents" ///
 "Age mother: >26"  "Age mother: 24-26"  "Age mother: <24" ///
 "Father: employed"   "Father: self-employed"  "Father: welfare"  "Father: disabled"  "Father: no income"  "Father: other" ///
 "Mother: employed"   "Mother: self-employed"  "Mother: welfare"  "Mother: disabled"  "Mother: no income"  "Mother: other" ///
 "Gender: boys"  "Gender: girls" ///
 "Father: no mental healthcare"  "Father: mental healthcare" ///
 "Mother: no mental healthcare"  "Mother: mental healthcare" ///
 "Health expenditure: 1st decile"  "Health expenditure: 2nd decile" "Health expenditure: 3rd decile"  "Health expenditure: 4th decile"  "Health expenditure: 5th decile" ///
"Health expenditure: 6th decile"  "Health expenditure: 7th decile"  "Health expenditure: 8th decile"  "Health expenditure: 9th decile"  "Health expenditure: 10th decile" ///
 "Parents: both known"  "Parents: one or both unknown" ///
 "Neighborhood: no health center"  "Neighborhood: with health center" ///
 "Municipality: Christian<.10"  "Municipality: Christian>=.10" 
				
		*Write matrix to excel
		putexcel set "H:\Sonya\JGZ pilot data/Bereik Regression Results\obs_always_all.xls", sheet(obs_always) replace
		putexcel A1 = matrix(obs_always), names
	
	}
	
*Create plot

svmat obs_always

rename obs_always1 obs_always_count
rename obs_always2 obs_always_uni_coef
rename obs_always7 obs_always_uni_lb
rename obs_always8 obs_always_uni_ub
rename obs_always9 obs_always_multi_coef
rename obs_always14 obs_always_multi_lb
rename obs_always15 obs_always_multi_ub
rename obs_always16 obs_always_N
rename obs_always17 obs_always_percentage

*Univariate
graph twoway (rcap obs_always_uni_lb obs_always_uni_ub obs_always_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_count obs_always_uni_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_uni)

*Multivariatie
graph twoway (rcap obs_always_multi_lb obs_always_multi_ub obs_always_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_count obs_always_multi_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_multi)	

*Compare univariate and multivariate	
graph twoway (rcap obs_always_uni_lb obs_always_uni_ub obs_always_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_count obs_always_uni_coef, yscale(reverse) mcolor(black)) ///
(rcap obs_always_multi_lb obs_always_multi_ub obs_always_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_count obs_always_multi_coef, yscale(reverse) mcolor(red)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_compare)
	

	

	
	

*For each organization separately 
set trace on
set tracedepth 1
local organizations 6

*Univariate
	
	foreach organization in `organizations' {
		
	matrix obs_always_`organization' = J(67, 17, .)
	
	local i=0
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y
	
	foreach regressor of local regressors {
	
	reg d_obs_always i.`regressor' if org_catchment == `organization'
	
		levels `regressor', local(var_levels)

		foreach level of local var_levels{
			
			local i = `i'+1
			
			matrix obs_always_`organization'[`i', 1]=`i'
		
			
			
			display _b[`level'.`regressor']
				
			matrix obs_always_`organization'[`i', 2]= _b[`level'.`regressor']
			matrix obs_always_`organization'[`i', 3] = _se[`level'.`regressor']
			matrix obs_always_`organization'[`i', 4] = _b[_cons]
			matrix obs_always_`organization'[`i', 5] = _se[_cons]
			matrix obs_always_`organization'[`i', 6] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 7]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 8]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
			
			*Add number of individuals (N) in each group
			sum `regressor' if `regressor' == `level' & org_catchment == `organization'
			local group=r(N)
			matrix obs_always_`organization'[`i', 16]=r(N)
		
			*Add percentage of total regression
			sum `regressor' if org_catchment == `organization'
			local total=r(N)
			matrix obs_always_`organization'[`i', 17]= `group'/`total'
			
		
		}
		
		matrix list obs_always_`organization'
	
	}
	}

	*Multivariate	

foreach organization in `organizations' {
	
	local i=0
	
	reg d_obs_always i.c_birth_order i.ETNGRP i.cm_inc_decile i.c_educ_hbo_wo i.c_address_parents i.age_mom_3groups i.c_lms5_pa i.c_lms5_ma i.gbageslacht i.pa_d_mental_care i.ma_d_mental_care i.health_exp_decile i.d_parents_unknown i.d_health_center_3y if org_catchment == `organization'
	
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y
	foreach regressor of local regressors {
	
		levels `regressor', local(var_levels)
		
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_always_`organization'[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_always_`organization'[`i', 9]= _b[`level'.`regressor']
			matrix obs_always_`organization'[`i', 10] = _se[`level'.`regressor']
			matrix obs_always_`organization'[`i', 11] = _b[_cons]
			matrix obs_always_`organization'[`i', 12] = _se[_cons]
			matrix obs_always_`organization'[`i', 13] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 14]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 15]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
		}
		
		matrix list obs_always_`organization'
		
		matrix colnames obs_always_`organization' = Counter Uni_Coef Uni_SE Uni_Const Uni_Pval Uni_LB95 Uni_UB95 Multi_Coef Multi_SE Multi_Const Multi_Pval Multi_LB95 Multi_UB95 N Frac_observed
		
		matrix rownames obs_always_`organization' =  "First born"  "Second born"  "Third born"  "Fourth born or later" ///
 "Native Dutch"  "Moroccan"  "Turkish"  "Surinamese" "Dutch Antilles"  "Other Western"  "Other Non-Western" ///
 "Income: 1st decile"  "Income: 2nd decile"  "Income: 3rd decile"  "Income: 4th decile"  "Income: 5th decile" ///
 "Income: 6th decile"  "Income: 7th decile" "Income: 8th decile"  "Income: 9th decile"  "Income: 10th decile" ///
 "Education: vocational"  "Education: college (HBO)"  "Education: academic (WO)" ///
 "Household: both parents"  "Household: mother only"  "Household: father only"  "Household: no parents" ///
 "Age mother: >26"  "Age mother: 24-26"  "Age mother: <24" ///
 "Father: employed"   "Father: self-employed"  "Father: welfare"  "Father: disabled"  "Father: no income"  "Father: other" ///
 "Mother: employed"   "Mother: self-employed"  "Mother: welfare"  "Mother: disabled"  "Mother: no income"  "Mother: other" ///
 "Gender: boys"  "Gender: girls" ///
 "Father: no mental healthcare"  "Father: mental healthcare" ///
 "Mother: no mental healthcare"  "Mother: mental healthcare" ///
 "Health expenditure: 1st decile"  "Health expenditure: 2nd decile" "Health expenditure: 3rd decile"  "Health expenditure: 4th decile"  "Health expenditure: 5th decile" ///
"Health expenditure: 6th decile"  "Health expenditure: 7th decile"  "Health expenditure: 8th decile"  "Health expenditure: 9th decile"  "Health expenditure: 10th decile" ///
 "Parents: both known"  "Parents: one or both unknown" ///
 "Neighborhood: no health center"  "Neighborhood: with health center" ///
 "Municipality: Christian<.10"  "Municipality: Christian>=.10" 
				
		*Write matrix to excel
		putexcel set "H:\Sonya\JGZ pilot data\Bereik Regression Results\obs_always_`organization'.xls", sheet(obs_always_`organization') replace
		putexcel A1 = matrix(obs_always_`organization'), names
	
	}
}


	
set trace on
set tracedepth 1
local organizations 7 10 12

*Univariate
	
	foreach organization in `organizations' {
		
	matrix obs_always_`organization' = J(67, 17, .)
	
	local i=0
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	
	foreach regressor of local regressors {
	
	reg d_obs_always i.`regressor' if org_catchment == `organization'
	
		levels `regressor', local(var_levels)

		foreach level of local var_levels{
			
			local i = `i'+1
			
			matrix obs_always_`organization'[`i', 1]=`i'
		
			
			
			display _b[`level'.`regressor']
				
			matrix obs_always_`organization'[`i', 2]= _b[`level'.`regressor']
			matrix obs_always_`organization'[`i', 3] = _se[`level'.`regressor']
			matrix obs_always_`organization'[`i', 4] = _b[_cons]
			matrix obs_always_`organization'[`i', 5] = _se[_cons]
			matrix obs_always_`organization'[`i', 6] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 7]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 8]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
			
			*Add number of individuals (N) in each group
			sum `regressor' if `regressor' == `level' & org_catchment == `organization'
			local group=r(N)
			matrix obs_always_`organization'[`i', 16]=r(N)
		
			*Add percentage of total regression
			sum `regressor' if org_catchment == `organization'
			local total=r(N)
			matrix obs_always_`organization'[`i', 17]= `group'/`total'
			
		
		}
		
		matrix list obs_always_`organization'
	
	}
	}
	
	
*Multivariate	



foreach organization in `organizations' {
	
	local i=0
	
	reg d_obs_always i.c_birth_order i.ETNGRP i.cm_inc_decile i.c_educ_hbo_wo i.c_address_parents i.age_mom_3groups i.c_lms5_pa i.c_lms5_ma i.gbageslacht i.pa_d_mental_care i.ma_d_mental_care i.health_exp_decile i.d_parents_unknown i.d_health_center_3y i.d_christian_3y if org_catchment == `organization'
	
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
		levels `regressor', local(var_levels)
		
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_always_`organization'[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_always_`organization'[`i', 9]= _b[`level'.`regressor']
			matrix obs_always_`organization'[`i', 10] = _se[`level'.`regressor']
			matrix obs_always_`organization'[`i', 11] = _b[_cons]
			matrix obs_always_`organization'[`i', 12] = _se[_cons]
			matrix obs_always_`organization'[`i', 13] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 14]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always_`organization'[`i', 15]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
		}
		
		matrix list obs_always_`organization'
		
		matrix colnames obs_always_`organization' = Counter Uni_Coef Uni_SE Uni_Const Uni_Pval Uni_LB95 Uni_UB95 Multi_Coef Multi_SE Multi_Const Multi_Pval Multi_LB95 Multi_UB95 N Frac_observed
		
		matrix rownames obs_always_`organization' =  "First born"  "Second born"  "Third born"  "Fourth born or later" ///
 "Native Dutch"  "Moroccan"  "Turkish"  "Surinamese" "Dutch Antilles"  "Other Western"  "Other Non-Western" ///
 "Income: 1st decile"  "Income: 2nd decile"  "Income: 3rd decile"  "Income: 4th decile"  "Income: 5th decile" ///
 "Income: 6th decile"  "Income: 7th decile" "Income: 8th decile"  "Income: 9th decile"  "Income: 10th decile" ///
 "Education: vocational"  "Education: college (HBO)"  "Education: academic (WO)" ///
 "Household: both parents"  "Household: mother only"  "Household: father only"  "Household: no parents" ///
 "Age mother: >26"  "Age mother: 24-26"  "Age mother: <24" ///
 "Father: employed"   "Father: self-employed"  "Father: welfare"  "Father: disabled"  "Father: no income"  "Father: other" ///
 "Mother: employed"   "Mother: self-employed"  "Mother: welfare"  "Mother: disabled"  "Mother: no income"  "Mother: other" ///
 "Gender: boys"  "Gender: girls" ///
 "Father: no mental healthcare"  "Father: mental healthcare" ///
 "Mother: no mental healthcare"  "Mother: mental healthcare" ///
 "Health expenditure: 1st decile"  "Health expenditure: 2nd decile" "Health expenditure: 3rd decile"  "Health expenditure: 4th decile"  "Health expenditure: 5th decile" ///
"Health expenditure: 6th decile"  "Health expenditure: 7th decile"  "Health expenditure: 8th decile"  "Health expenditure: 9th decile"  "Health expenditure: 10th decile" ///
 "Parents: both known"  "Parents: one or both unknown" ///
 "Neighborhood: no health center"  "Neighborhood: with health center" ///
 "Municipality: Christian<.10"  "Municipality: Christian>=.10" 
				
		*Write matrix to excel
		putexcel set "H:\Sonya\JGZ pilot data\Bereik Regression Results\obs_always_`organization'.xls", sheet(obs_always_`organization') replace
		putexcel A1 = matrix(obs_always_`organization'), names
	
	}
}

*Create plots
local organizations 6 7 10 12

foreach organization in `organizations' {

svmat obs_always_`organization'

rename obs_always_`organization'1 obs_always_`organization'_count
rename obs_always_`organization'2 obs_always_`organization'_uni_coef
rename obs_always_`organization'7 obs_always_`organization'_uni_lb
rename obs_always_`organization'8 obs_always_`organization'_uni_ub
rename obs_always_`organization'9 obs_always_`organization'_multi_coef
rename obs_always_`organization'14 obs_always_`organization'_multi_lb
rename obs_always_`organization'15 obs_always_`organization'_multi_ub
rename obs_always_`organization'16 obs_always_`organization'_N
rename obs_always_`organization'17 obs_always_`organization'_percentage

*Univariate
graph twoway (rcap obs_always_`organization'_uni_lb obs_always_`organization'_uni_ub obs_always_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_`organization'_count obs_always_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_`organization'_uni)

*Multivariatie
graph twoway (rcap obs_always_`organization'_multi_lb obs_always_`organization'_multi_ub obs_always_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_`organization'_count obs_always_`organization'_multi_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_`organization'_multi)	

*Compare univariate and multivariate	
graph twoway (rcap obs_always_`organization'_uni_lb obs_always_`organization'_uni_ub obs_always_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_`organization'_count obs_always_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
(rcap obs_always_`organization'_multi_lb obs_always_`organization'_multi_ub obs_always_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_`organization'_count obs_always_`organization'_multi_coef, yscale(reverse) mcolor(red)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_`organization'_compare)
	
}





*For all combinations of 3 organizations pooled 
local organizations 6 7 10 12

*Univariate
	
	foreach organization in `organizations' {
		
	matrix obs_always_excl_`organization' = J(67, 17, .)
	
	local i=0
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	
	foreach regressor of local regressors {
	
	reg d_obs_always i.`regressor' if org_catchment != `organization'
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			
			matrix obs_always_excl_`organization'[`i', 1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_always_excl_`organization'[`i', 2]= _b[`level'.`regressor']
			matrix obs_always_excl_`organization'[`i', 3] = _se[`level'.`regressor']
			matrix obs_always_excl_`organization'[`i', 4] = _b[_cons]
			matrix obs_always_excl_`organization'[`i', 5] = _se[_cons]
			matrix obs_always_excl_`organization'[`i', 6] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always_excl_`organization'[`i',7]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always_excl_`organization'[`i',8]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
			*Add number of individuals (N) in each group
			sum `regressor' if `regressor' == `level' & org_catchment != `organization'
			local group=r(N)
			matrix obs_always_excl_`organization'[`i', 16]=r(N)
			
			*Add percentage of total regression
			sum `regressor' if org_catchment != `organization'
			local total=r(N)
			matrix obs_always_excl_`organization'[`i', 17]= `group'/`total'
			
		}
		
		matrix list obs_always_excl_`organization'
	
	}
	}
	
*Multivariate	

foreach organization in `organizations' {
	
	local i=0
	
	reg d_obs_always i.c_birth_order i.ETNGRP i.cm_inc_decile i.c_educ_hbo_wo i.c_address_parents i.age_mom_3groups i.c_lms5_pa i.c_lms5_ma i.gbageslacht i.pa_d_mental_care i.ma_d_mental_care i.health_exp_decile i.d_parents_unknown i.d_health_center_3y i.d_christian_3y if org_catchment != `organization'
	
	local regressors c_birth_order ETNGRP cm_inc_decile c_educ_hbo_wo c_address_parents age_mom_3groups c_lms5_pa c_lms5_ma gbageslacht pa_d_mental_care ma_d_mental_care health_exp_decile d_parents_unknown d_health_center_3y d_christian_3y
	foreach regressor of local regressors {
	
		levels `regressor', local(var_levels)
		foreach level of local var_levels{
		
			local i = `i'+1
			matrix obs_always_excl_`organization'[`i',1]=`i'
		
			display _b[`level'.`regressor']
			
			matrix obs_always_excl_`organization'[`i', 9]= _b[`level'.`regressor']
			matrix obs_always_excl_`organization'[`i', 10] = _se[`level'.`regressor']
			matrix obs_always_excl_`organization'[`i', 11] = _b[_cons]
			matrix obs_always_excl_`organization'[`i', 12] = _se[_cons]
			matrix obs_always_excl_`organization'[`i', 13] = 2*(1 - ttail(e(df_r), abs(_b[`level'.`regressor'] / _se[`level'.`regressor'])))
			if _se[`level'.`regressor']!=0 matrix obs_always_excl_`organization'[`i', 14]= _b[`level'.`regressor'] - invttail(e(df_r),0.025)*_se[`level'.`regressor']
			if _se[`level'.`regressor']!=0 matrix obs_always_excl_`organization'[`i', 15]= _b[`level'.`regressor'] + invttail(e(df_r),0.025)*_se[`level'.`regressor']
			
		}
		
		matrix list obs_always_excl_`organization'
		
		matrix colnames obs_always_excl_`organization' = Counter Uni_Coef Uni_SE Uni_Const Uni_Pval Uni_LB95 Uni_UB95 Multi_Coef Multi_SE Multi_Const Multi_Pval Multi_LB95 Multi_UB95 N Frac_observed
		
		matrix rownames obs_always_excl_`organization' =  "First born"  "Second born"  "Third born"  "Fourth born or later" ///
 "Native Dutch"  "Moroccan"  "Turkish"  "Surinamese" "Dutch Antilles"  "Other Western"  "Other Non-Western" ///
 "Income: 1st decile"  "Income: 2nd decile"  "Income: 3rd decile"  "Income: 4th decile"  "Income: 5th decile" ///
 "Income: 6th decile"  "Income: 7th decile" "Income: 8th decile"  "Income: 9th decile"  "Income: 10th decile" ///
 "Education: vocational"  "Education: college (HBO)"  "Education: academic (WO)" ///
 "Household: both parents"  "Household: mother only"  "Household: father only"  "Household: no parents" ///
 "Age mother: >26"  "Age mother: 24-26"  "Age mother: <24" ///
 "Father: employed"   "Father: self-employed"  "Father: welfare"  "Father: disabled"  "Father: no income"  "Father: other" ///
 "Mother: employed"   "Mother: self-employed"  "Mother: welfare"  "Mother: disabled"  "Mother: no income"  "Mother: other" ///
 "Gender: boys"  "Gender: girls" ///
 "Father: no mental healthcare"  "Father: mental healthcare" ///
 "Mother: no mental healthcare"  "Mother: mental healthcare" ///
 "Health expenditure: 1st decile"  "Health expenditure: 2nd decile" "Health expenditure: 3rd decile"  "Health expenditure: 4th decile"  "Health expenditure: 5th decile" ///
"Health expenditure: 6th decile"  "Health expenditure: 7th decile"  "Health expenditure: 8th decile"  "Health expenditure: 9th decile"  "Health expenditure: 10th decile" ///
 "Parents: both known"  "Parents: one or both unknown" ///
 "Neighborhood: no health center"  "Neighborhood: with health center" ///
 "Municipality: Christian<.10"  "Municipality: Christian>=.10" 
				
		*Write matrix to excel
		putexcel set "H:\Sonya\JGZ pilot data\Bereik Regression Results\obs_always_excl_`organization'.xls", sheet(obs_always_excl_`organization') replace
		putexcel A1 = matrix(obs_always_excl_`organization'), names
	
	}
}

*Create plots
local organizations 6 7 10 12

foreach organization in `organizations' {

svmat obs_always_excl_`organization'

rename obs_always_excl_`organization'1 obs_always_excl_`organization'_count
rename obs_always_excl_`organization'2 obs_always_excl_`organization'_uni_coef
rename obs_always_excl_`organization'7 obs_always_excl_`organization'_uni_lb
rename obs_always_excl_`organization'8 obs_always_excl_`organization'_uni_ub
rename obs_always_excl_`organization'9 obs_always_excl_`organization'_multi_coef
rename obs_always_excl_`organization'14 obs_always_excl_`organization'_multi_lb
rename obs_always_excl_`organization'15 obs_always_excl_`organization'_multi_ub
rename obs_always_excl_`organization'16 obs_always_excl_`organization'_N
rename obs_always_excl_`organization'17 obs_always_excl_`organization'_percentage

*Univariate
graph twoway (rcap obs_always_excl_`organization'_uni_lb obs_always_excl_`organization'_uni_ub obs_always_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_excl_`organization'_count obs_always_excl_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_excl_`organization'_uni)

*Multivariatie
graph twoway (rcap obs_always_excl_`organization'_multi_lb obs_always_excl_`organization'_multi_ub obs_always_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_excl_`organization'_count obs_always_excl_`organization'_multi_coef, yscale(reverse) mcolor(black)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_excl_`organization'_multi)	

*Compare univariate and multivariate	
graph twoway (rcap obs_always_excl_`organization'_uni_lb obs_always_excl_`organization'_uni_ub obs_always_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_excl_`organization'_count obs_always_excl_`organization'_uni_coef, yscale(reverse) mcolor(black)) ///
(rcap obs_always_excl_`organization'_multi_lb obs_always_excl_`organization'_multi_ub obs_always_excl_`organization'_count, horizontal yscale(reverse) lcolor(gs10)) ///
(scatter obs_always_excl_`organization'_count obs_always_excl_`organization'_multi_coef, yscale(reverse) mcolor(red)) ///
, xline(0) ///
ylabel(1 "First born" 2 "Second born" 3 "Third born" 4 "Fourth born or later" ///
5 "Native Dutch" 6 "Moroccan" 7 "Turkish" 8 "Surinamese" 9 "Dutch Antilles" 10 "Other Western" 11 "Other Non-Western" ///
12 "Income: 1st decile" 13 "Income: 2nd decile" 14 "Income: 3rd decile" 15 "Income: 4th decile" 16 "Income: 5th decile" ///
17 "Income: 6th decile" 18 "Income: 7th decile" 19 "Income: 8th decile" 20 "Income: 9th decile" 21 "Income: 10th decile" ///
22 "Education: vocational" 23 "Education: college (HBO)" 24 "Education: academic (WO)" ///
25 "Household: both parents" 26 "Household: mother only" 27 "Household: father only" 28 "Household: no parents" ///
29 "Age mother: >26" 30 "Age mother: 24-26" 31 "Age mother: <24" ///
32 "Father: employed"  33 "Father: self-employed" 34 "Father: welfare" 35 "Father: disabled" 36 "Father: no income" 37 "Father: other" ///
38 "Mother: employed"  39 "Mother: self-employed" 40 "Mother: welfare" 41 "Mother: disabled" 42 "Mother: no income" 43 "Mother: other" ///
44 "Gender: boys" 45 "Gender: girls" ///
46 "Father: no mental healthcare" 47 "Father: mental healthcare" ///
48 "Mother: no mental healthcare" 49 "Mother: mental healthcare" ///
50 "Health expenditure: 1st decile" 51 "Health expenditure: 2nd decile" 52 "Health expenditure: 3rd decile" 53 "Health expenditure: 4th decile" 54 "Health expenditure: 5th decile" ///
55"Health expenditure: 6th decile" 56 "Health expenditure: 7th decile" 57 "Health expenditure: 8th decile" 58 "Health expenditure: 9th decile" 59 "Health expenditure: 10th decile" ///
60 "Parents: both known" 61 "Parents: one or both unknown" ///
62 "Neighborhood: no health center" 63 "Neighborhood: with health center" ///
64 "Municipality: Christian<.10" 65 "Municipality: Christian>=.10" ///
, angle(0) labsize(tiny)) ///
ytitle("") ///
xlabel(-.04(.02).04) ///
graphregion(color(white)) legend(off) ///
name(obs_always_excl_`organization'_compare)
	
}
	