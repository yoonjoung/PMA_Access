* This dofile creates dataset and figures to asess EA-SDP link 
* Such link can be used to create EA-level service environment data using SDP and CEI surveys 
* 		=> save "$data/EAlevel_SDP_`survey'.dta", replace 
* 		This is required to run some of the access variables in "Effective_Access_Indicators_IR.do" 
* TWO summary datasets 
* 		=> 1. describing link quality: EA_SDP_Link_Summary.dta
* 		=> 2. describing summary SDP indicator estimates: summary_Access_Indicators_SR.dta (EXPORT THIS for Shiny App)

* Table of contents
* A. SETTING
* B. Link EA-SDP-IR  
* 	B.1 Get EA-level information from HHQFQ ===> EA_`survey'.dta
* 	B.2 Gen SDP type variables in the SDP file. 
* 	B.3 Reshape SDP to EA-SDP level LONG file ===> EASDP_`survey'.dta
* 	B.4 Create EA-level data which has service environment characterisics based on linked SDPs ===> EAlevel_SDP_`survey'.dta
* C. Create dataset summarizing the link results ===> EA_SDP_Link_Summary.dta
* D. Create dataset summarizing the SDP level results ===> summary_Access_Indicators_SR.dta 

clear
clear matrix
clear mata
capture log close
set more off
numlabel, add

set scheme s1color

*************************************************************************************************
* A SETTING 
*************************************************************************************************

cd "C:\Users\YoonJoung Choi\Dropbox\0 iSquared\iSquared_PMA\Effective Access\"
global data "C:\Users\YoonJoung Choi\Dropbox\0 Data\PMA\"

local today=c(current_date)
local c_today= "`today'"
global date=subinstr("`c_today'", " ", "",.)

#delimit;
global datalist " 
	BFR1 BFR2 BFR3 BFR4 BFR5 BFR6 
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7
	ETR2 ETR3 ETR4 ETR5 ETR6
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER2 KER3 KER4 KER5 KER6 KER7 
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 
	NGLagosR2 NGLagosR3 NGLagosR4 NGLagosR5 
	NGKanoR3  NGKanoR4  NGKanoR5  
	UGR2 UGR3 UGR4 UGR5 UGR6
	";
	#delimit cr
	
#delimit;
global datalistminusone " 
	BFR2 BFR3 BFR4 BFR5 BFR6 
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7
	ETR2 ETR3 ETR4 ETR5 ETR6
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER2 KER3 KER4 KER5 KER6 KER7 
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 
	NGLagosR2 NGLagosR3 NGLagosR4 NGLagosR5 
	NGKanoR3  NGKanoR4  NGKanoR5  
	UGR2 UGR3 UGR4 UGR5 UGR6
	";
	#delimit cr

	
*************************************************************************************************
* B.1 Get EA-level information from HHQFQ 
*************************************************************************************************
	
set more off
foreach survey  in $datalist{
*foreach survey  in BFR6{
	use "$data/HR_`survey'.dta", clear

		sort EA_ID
		keep if EA_ID!=EA_ID[_n-1]
		
		
			capture confirm variable country
			if !_rc {
				gen India=1 if country=="India_Rajasthan"
			}		

			*create "region" as an admin level-1 variable
			capture confirm variable region
			if !_rc {
			}
			else {
				capture confirm variable county 
				if !_rc {
					gen region=county 
					}
				}
				else{
					capture confirm variable state
					if !_rc {
						gen region=state
						}
					}
					else{
						capture confirm variable province
						if !_rc {
							gen region=province
							}
						}
			
		keep round region ur EA_ID 
		foreach x of varlist round region ur  {
			rename `x' HHQFQ`x'
			}
		gen xsurvey="`survey'"		
		sort EA_ID
	save "$data/EA_`survey'.dta", replace
}	

			**************************************************
			/************************************ For Figure 2 
			set more on
			
			use "$data/EA_UGR6.dta", clear
			sort EA_ID 
			keep if EA_ID==3023 | EA_ID==3086 | EA_ID==3252 | EA_ID==3018
				list EA_ID HHQFQur HHQFQregion
				

				 +----------------------------------------------------+
				 | HHQFQr~d    HHQFQur   HHQFQreg~n   EA_ID   xsurvey |
				 |----------------------------------------------------|
			  1. |        6   2. rural   2. Eastern    3018      UGR6 |
			  2. |        6   1. urban   1. Central    3023      UGR6 |
			  3. |        6   1. urban   1. Central    3086      UGR6 |
			  4. |        6   1. urban   1. Central    3252      UGR6 |
				 +----------------------------------------------------+

				
			*/

*Add EA-level MCPR
set more off
foreach survey  in $datalist{
*foreach survey  in BFR6{
	use "$data/IR_`survey'.dta", clear
		collapse  (mean) mcp  [pw=FQweight], by(EA_ID)	
		
		merge EA_ID using "$data/EA_`survey'.dta"
			tab _merge, m
			drop _merge

	sort EA_ID
	save "$data/EA_`survey'.dta", replace		
	}

*************************************************************************************************
* B.2 Gen SDP type variables 
*************************************************************************************************
/*
***** CHECK all surveys for their facility type classification 
foreach survey  in $datalist{
	use "$data/SR_`survey'.dta", clear	
	tab country xsurvey, m
	tab facility_type managing_authority, m
	codebook facility_type 
	}
	
foreach survey  in $datalist{
	use "$data/SR_`survey'.dta", clear	
	sum round facility_type
	}	
*/
	
set more off
foreach survey  in $datalist{
*foreach survey  in BFR6{
	use "$data/SR_`survey'.dta", clear	

	replace country=substr(xsurvey, 1,2)
	
			capture confirm variable ur
			if !_rc {
				drop ur
			}	
	
	* SDP characteristics 
	gen SDPall			= 1
	gen byte SDPpub		= managing_authority==1

	gen SDPpub12=0
	gen SDPlow=0

	/*
	replace SDPpub12	=1 if country=="BF" & managing_authority==1 & (facility_type>=4 & facility_type<=7) /*primary & secondary*/
	replace SDPlow		=1 if country=="BF" & (facility_type>=4 & facility_type<=7) /*primary & secondary level, both sectors*/

	replace SDPpub12	=1 if country=="CI" & managing_authority==1 & (facility_type>=5 & facility_type<=15) /*primary & secondary*/
	replace SDPlow		=1 if country=="CI" & (facility_type>=5 & facility_type<=15) /*primary & secondary level, both sectors*/

	replace SDPpub12	=1 if country=="CD" & managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	replace SDPlow		=1 if country=="CD" & (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/
	
	replace SDPpub12	=1 if country=="ET" & managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	replace SDPlow		=1 if country=="ET" & (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/
	
	replace SDPpub12	=1 if country=="IN" & managing_authority==1 & (facility_type>=2 & facility_type<=6) /*primary & secondary*/
	replace SDPlow		=1 if country=="IN" & (facility_type>=2 & facility_type<=6) /*primary & secondary level, both sectors*/
	
	replace SDPpub12	=1 if country=="KE" & managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	replace SDPlow		=1 if country=="KE" & (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/

	replace SDPpub12	=1 if country=="NE" & managing_authority==1 & (facility_type>=4 & facility_type<=11) /*primary & secondary*/
	replace SDPlow		=1 if country=="NE" & (facility_type>=4 & facility_type<=11) /*primary & secondary level, both sectors*/	
	
	replace SDPpub12	=1 if country=="NG" & managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	replace SDPlow		=1 if country=="NG" & (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/

	replace SDPpub12	=1 if country=="UG" & managing_authority==1 & (facility_type>=3 & facility_type<=5) /*primary & secondary*/
	replace SDPlow		=1 if country=="UG" & (facility_type>=3 & facility_type<=5) /*primary & secondary level, both sectors*/
	*/

	replace SDPpub12	=1 if country=="BF" & managing_authority==1 & (facility_type>=4) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="BF" & (facility_type>=4) /*excludnig hospitals*/

	replace SDPpub12	=1 if country=="CI" & managing_authority==1 & (facility_type>=5) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="CI" & (facility_type>=5) /*excludnig hospitals*/

	replace SDPpub12	=1 if country=="CD" & managing_authority==1 & (facility_type>=2) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="CD" & (facility_type>=2) /*excludnig hospitals*/
	
	replace SDPpub12	=1 if country=="ET" & managing_authority==1 & (facility_type>=2) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="ET" & (facility_type>=2) /*excludnig hospitals*/
	
	replace SDPpub12	=1 if country=="IN" & managing_authority==1 & (facility_type>=2) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="IN" & (facility_type>=2) /*excludnig hospitals*/
	
	replace SDPpub12	=1 if country=="KE" & managing_authority==1 & (facility_type>=2) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="KE" & (facility_type>=2) /*excludnig hospitals*/

	replace SDPpub12	=1 if country=="NE" & managing_authority==1 & (facility_type>=4) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="NE" & (facility_type>=4) /*excludnig hospitals*/
	
	replace SDPpub12	=1 if country=="NG" & managing_authority==1 & (facility_type>=2) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="NG" & (facility_type>=2) /*excludnig hospitals*/

	replace SDPpub12	=1 if country=="UG" & managing_authority==1 & (facility_type>=3) /*public, excludnig hospitals*/
	replace SDPlow		=1 if country=="UG" & (facility_type>=3) /*excludnig hospitals*/
	
	save SR_SDPtype_`survey'.dta, replace 	
}

			**************************************************
			/************************************ For Figure 2 
			set more on
			
			use "$data/SR_UGR6.dta", clear	
			codebook facility_ID
			
			#delimit; 
			keep if   facility_ID==3185 | 
					  facility_ID==3033 |
					  facility_ID==3094 |
					  facility_ID==3185 |
					  facility_ID==3188 |
					  facility_ID==3260 |
					  facility_ID==3266 | 
					  facility_ID==3334 |
					  facility_ID==3464 |
					  facility_ID==3469 |
					  facility_ID==3533 |
					  facility_ID==3674
					  ;
					  #delimit cr
						
			sort facility_ID 
			list facility_ID facility_type EAserved1 EAserved2 EAserved3 EAserved4 EAserved5   
			list facility_ID facility_type EAserved6 EAserved7 EAserved8 EAserved9 EAserved10 
			list facility_ID facility_type EAserved11 EAserved12 EAserved13 EAserved14 
							
				 +---------------------------------------------------------------------------------------+
				 | facili~D   EAser~d1   EAser~d2   EAser~d3   EAser~d4   EAserv~5   EAserv~6   EAserv~7 |
				 |---------------------------------------------------------------------------------------|
			  1. |     3033       3023       3702       3610       3086       3393       3069       3252 |
			  2. |     3094       3018          .          .          .          .          .          . |
			  3. |     3185       3023       3086       3252          .          .          .          . |
			  4. |     3188       3023       3069       3531       3056          .          .          . |
			  5. |     3260       3252          .          .          .          .          .          . |
				 |---------------------------------------------------------------------------------------|
			  6. |     3266       3023          .          .          .          .          .          . |
			  7. |     3334       3811       3515       3905       3023       3702       3610       3086 |
			  8. |     3464       3023          .          .          .          .          .          . |
			  9. |     3469       3252          .          .          .          .          .          . |
			 10. |     3533       3023          .          .          .          .          .          . |
				 |---------------------------------------------------------------------------------------|
			 11. |     3674       3540       3853       3813       3381       3640       3061       3376 |
				 +---------------------------------------------------------------------------------------+


				 +---------------------------------------------------------------------------------------+
				 | facili~D   EAserv~8   EAserv~9   EAser~10   EAser~11   EAser~12   EAser~13   EAser~14 |
				 |---------------------------------------------------------------------------------------|
			  1. |     3033       3531       3056       3246          .          .          .          . |
			  2. |     3094          .          .          .          .          .          .          . |
			  3. |     3185          .          .          .          .          .          .          . |
			  4. |     3188          .          .          .          .          .          .          . |
			  5. |     3260          .          .          .          .          .          .          . |
				 |---------------------------------------------------------------------------------------|
			  6. |     3266          .          .          .          .          .          .          . |
			  7. |     3334       3393       3069       3252       3531       3056       3246          . |
			  8. |     3464          .          .          .          .          .          .          . |
			  9. |     3469          .          .          .          .          .          .          . |
			 10. |     3533          .          .          .          .          .          .          . |
				 |---------------------------------------------------------------------------------------|
			 11. |     3674       3714       3323       3045       3233       3137       3931       3018 |
				 +---------------------------------------------------------------------------------------+

							
				
			*/

*************************************************************************************************
* B.3 Reshape SDP to EA-SDP level LONG file 
*************************************************************************************************
/*
**** CHECK if SDP has duplicates => NONE confirmed
foreach survey  in $datalist{
	use SR_SDPtype_`survey'.dta, clear	
	tab xsurvey
	duplicates report
	}

***** CHECK all surveys have "EAserved..."
foreach survey  in $datalist{
	use SR_SDPtype_`survey'.dta, clear	
	lookfor EAserved
	sum EAserved*
	}
	
***** CHECK all surveys for date var
foreach survey  in $datalist{
	use SR_SDPtype_`survey'.dta, clear	
	tab xsurvey
	lookfor todaySIF
	}
	
*/

set more off
foreach survey  in $datalist{
	use SR_SDPtype_`survey'.dta, clear	
	
	* reshape SDP to EA-SDP level long file. After this, some SDPs will be linked to multiple EAs. 	
	* codebook EAserved1  EAserved2 EAserved3 EAserved4 EAserved5 EAserved6 EAserved7 EAserved8 EAserved9 EAserved10
	* sum EAserved1  EAserved2 EAserved3 EAserved4 EAserved5 EAserved6 EAserved7 EAserved8 EAserved9 EAserved10
	reshape long EAserved , i(facility_ID) 
	drop if EAserved==.

	rename EA_ID EA_ID_OLD /*this is randomly assigned EA_ID among multiple EAserved?*/
	rename EAserved EA_ID	
	
		codebook EA_ID
	
	gen space=" "		
	egen EASDP_ID = concat (facility_ID space EA_ID)   
	drop space 	
	
	lab var EA_ID "ID of EA served by SDP"
	lab var EASDP_ID "ID of EA-SDP pair: facility_ID & EA_ID"

	sort EA_ID facility_ID EASDP_ID	
	save "$data/EASDP_`survey'.dta", replace	
}


log using EA_SDP_Link_Check_$date.log, replace

**** CHECK if EA_SDP has duplicates => DROPPED 
foreach survey  in $datalist{
	use "$data/EASDP_`survey'.dta", clear	
	
	tab xsurvey
	duplicates report 	EASDP_ID  EA_ID facility_ID
	duplicates list 	EASDP_ID  EA_ID facility_ID
	duplicates drop 	EASDP_ID  EA_ID facility_ID, force
	tab xsurvey
	save "$data/EASDP_`survey'.dta", replace	
	}
	
**** Bring in urban/rural variable from EA_`survey'.dta: This is to check link quality 
set more off
foreach survey  in $datalist{
	use "$data/EASDP_`survey'.dta", clear	
	
	tab xsurvey, m
	
	sort EA_ID
	merge EA_ID using "$data/EA_`survey'.dta", keep(HHQFQur)
	tab _merge xsurvey, m
		drop if _merge==2
		rename _merge merge_EASDP_EA
		/*
		CHECK AND ASK: 
		merge_EASDP_EA==1: EASDP observation that were NOT linked to any EA. How is it possible?? Likely data entry error?? 
		merge_EASDP_EA==2: EA observation that did NOT have any SDP. Design + implementation issues
		*/
	
	save "$data/EASDP_`survey'.dta", replace		
}

log close


			**************************************************
			/************************************ For Figure 2 
			set more on
			
			use "$data/EASDP_UGR6.dta", clear	
			
			keep if EA_ID==3023 | EA_ID==3086 | EA_ID==3018
			codebook facility_ID EA_ID EASDP_ID 
			
			sort facility_ID EA_ID
			list facility_ID EA_ID facility_type managing
			
				 +-------------------------------------------------------+
				 | facili~D   EA_ID        facility_type   managing_au~y |
				 |-------------------------------------------------------|
			  1. |     3033    3023   4. health_center_2   1. Government |
			  2. |     3033    3086   4. health_center_2   1. Government |
			  3. |     3094    3018   4. health_center_2   1. Government |
			  4. |     3185    3023   3. health_center_3   1. Government |
			  5. |     3185    3086   3. health_center_3   1. Government |
				 |-------------------------------------------------------|
			  6. |     3188    3023   2. health_center_4   1. Government |
			  7. |     3266    3023          6. pharmacy      4. Private |
			  8. |     3334    3023          1. hospital   1. Government |
			  9. |     3334    3086          1. hospital   1. Government |
			 10. |     3464    3023     5. health_clinic      4. Private |
				 |-------------------------------------------------------|
			 11. |     3533    3023           7. chemist      4. Private |
			 12. |     3674    3018          1. hospital   1. Government |
				 +-------------------------------------------------------+

						
			keep if EA_ID==3023 | EA_ID==3086 | EA_ID==3252
			
			
			*/


/*
set more off
foreach survey  in $datalist{
	use "$data/EASDP_`survey'.dta", clear	
		tab xsurvey 
		sum SDPall SDPpub SDPpub12 SDPlow
	}
	
set more off
foreach survey  in $datalist{
	use "$data/EASDP_`survey'.dta", clear
		tab  HHQFQur, m
	}
*/

*************************************************************************************************
* B.4 Create EA-level data which has service environment characterisics based on linked SDPs
*************************************************************************************************


set more off
foreach survey  in $datalist{
	use "$data/EASDP_`survey'.dta", clear

*1. MERGE EA_level information from HHQFQ 
	sort EA_ID
	merge EA_ID using "$data/EA_`survey'.dta"
	tab _merge xsurvey, m 

		/*
		RED FLAG 1
		check EAs with no SDP (i.e., _merge==2 or round==.) 
		check SDPs with no EA (i.e., _merge==1) 
		This should be none, but not necessarily in reality. WHY???
		*/
			tab xsurvey, m
			tab EA_ID if _merge==1 
			tab EA_ID if _merge==2 
		*keep if _merge==3
		*drop _merge
		
		gen ur=HHQFQur
		
	save temp.dta, replace
	
*2. CREATE number of SDPs by type - per EA
	use temp.dta, replace
		collapse (sum) SDPall SDPpub SDPpub12 SDPlow , by(xsurvey round ur EA_ID)
		foreach x of varlist SDPall SDPpub SDPpub12 SDPlow {
			rename `x' num_`x'
			lab var num_`x' "Numbe of `x' per EA"
			}
		
		*sum num_SDP*		
		
		sort EA_ID
		merge EA_ID using "$data/EA_`survey'.dta"
			tab _merge, m 
			drop _merge
			
	
		gen byte noSDPany 	=num_SDPall==0
		gen byte noSDPpub 	=num_SDPpub==0
		gen byte noSDPpub12 =num_SDPpub12==0
		gen byte noSDPlow 	=num_SDPlow==0
		
		lab var noSDPany "EA with no SDP at all"
		lab var noSDPpub "EA with no public SDP"
		lab var noSDPpub12 "EA with no public, primary or secondary SDP"
		lab var noSDPlow "EA with no primar or secondary SDP (regardless of sector)"
		
		sort EA_ID
		save "$data/EAlevel_SDP_`survey'.dta", replace

		
*3. CREATE number of SDPs by type that have offer/have/ready for methods - per EA
*THIS section is not necessarily about linking. It requires variables constructed in the SDP Recode file.

	use temp.dta, replace
	foreach x of varlist SDPall SDPpub SDPpub12 SDPlow {
	use temp.dta, replace
		*preserve
		keep if `x'==1

		collapse (mean)`x'  (sum)  essential* ready*, by(xsurvey round ur EA_ID )
		foreach var of varlist essential* ready*{
			rename `var' `x'_`var'
			lab var `x'_`var' "Number of `x' with `var' per EA"
			}
		
		sort EA_ID
		merge EA_ID using "$data/EAlevel_SDP_`survey'.dta", 
			tab _merge, m 
			drop _merge	
		*restore	
		sort EA_ID		
		save "$data/EAlevel_SDP_`survey'.dta", replace		
		}	
		
		gen country=substr(xsurvey, 1, 2)
		
		sort EA_ID		
		save "$data/EAlevel_SDP_`survey'.dta", replace
				
}			
			
/*	
set more off
foreach survey  in $datalist{
	use "$data/EAlevel_SDP_`survey'.dta", clear
		bysort ur: sum round noSDP* 
		list xsurvey EA_ID noSDP* if noSDPany==1
		*list xsurvey EA_ID noSDP* if noSDPpub==1
		*list xsurvey EA_ID noSDP* if noSDPpub12==1			
	}
	
set more off
foreach survey  in INR1 INR2 INR3 INR4{
	use "$data/EAlevel_SDP_`survey'.dta", clear
		tab xsurvey 
		sum round num_SDP* SDPall_essential*
	}	

set more off
foreach survey  in INR1 INR2 INR3 INR4{	
	use "$data/EASDP_`survey'.dta", clear	
		tab xsurvey 
		sum round SDPall SDPpub SDPpub12 SDPlow 
	}
*/	
	
*************************************************************************************************	
* C. Create dataset summarizing the link results 
*************************************************************************************************

*** 1. Number of EA	
use "$data/EA_BFR1.dta", clear			
		egen nEA=group(EA_ID)
		collapse (count) nEA, by(xsurvey)
		save temp.dta, replace
	use "$data/EA_BFR1.dta", clear			
		egen nEA=group(EA_ID)
		collapse (count) nEA, by(xsurvey HHQFQur)
		append using temp.dta, 
		save temp.dta, replace		
	foreach survey  in $datalistminusone{
	use "$data/EA_`survey'.dta", clear		
		egen nEA=group(EA_ID)
		collapse (count) nEA, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
	use "$data/EA_`survey'.dta", clear		
		egen nEA=group(EA_ID)
		collapse (count) nEA, by(xsurvey HHQFQur)
		append using temp.dta, 
		save temp.dta, replace
		}
		
	tab xsurvey HHQFQur, m
		
	lab var nEA "Total number of EA in the survey" 

	sort xsurvey HHQFQur
	save EA_SDP_Link_Summary.dta, replace	
		
*** 2. Number of SDP	
use SR_SDPtype_BFR1.dta, clear
		gen nSDP	= 1
		gen nSDPpub	=1 if managing_authority==1
		gen nSDPprivate	=1 if managing_authority>1	
		gen nSDPhospital=1 if SDPlow!=1
		gen nSDPlow	=1 if SDPlow==1	
		collapse (count) nSDP*, by(xsurvey)	
		save temp.dta, replace

	foreach survey  in $datalistminusone{
	use SR_SDPtype_`survey'.dta, clear
		gen nSDP	= 1
		gen nSDPpub	=1 if managing_authority==1
		gen nSDPprivate	=1 if managing_authority>1	
		gen nSDPhospital=1 if SDPlow!=1
		gen nSDPlow	=1 if SDPlow==1	
		collapse (count) nSDP*, by(xsurvey)	
		append using temp.dta, 
		save temp.dta, replace
		
		}
		
	lab var nSDP "Total number of SDPs in the survey" 	
	lab var nSDPpub "Total number of PUBLIC SDPs in the survey" 	
	lab var nSDPprivate "Total number of PRIVATE SDPs in the survey" 	
	lab var nSDPhospital "Total number of hospitals in the survey" 	
	lab var nSDPlow "Total number of non-hospital SDPs in the survey" 	
		
	sort xsurvey
	merge xsurvey using EA_SDP_Link_Summary.dta
		tab _merge, m
		keep if _merge==3 /*SHOULD BE ALL*/
		drop _merge
		
	sort xsurvey HHQFQur
	save EA_SDP_Link_Summary.dta, replace

*** 3. Number of EASDP	
use "$data/EASDP_BFR1.dta", clear			
		egen nEASDP=group(EASDP_ID)
		collapse (count) nEASDP, by(xsurvey)
		save temp.dta, replace
	use "$data/EASDP_BFR1.dta", clear			
		egen nEASDP=group(EASDP_ID)
		collapse (count) nEASDP, by(xsurvey HHQFQur)
			drop if HHQFQur==. /*this is to drop odd EASDPs that do not have EAs to link: merge_EASDP_EA==1 */
		append using temp.dta, 
		save temp.dta, replace		
	foreach survey  in $datalistminusone{
	*foreach survey  in BFR6{
	use "$data/EASDP_`survey'.dta", clear		
		egen nEASDP=group(EASDP_ID)
		collapse (count) nEASDP, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
	use "$data/EASDP_`survey'.dta", clear		
		egen nEASDP=group(EASDP_ID)
		collapse (count) nEASDP, by(xsurvey HHQFQur)
			drop if HHQFQur==. /*this is to drop odd EASDPs that do not have EAs to link: merge_EASDP_EA==1 */
		append using temp.dta, 
		save temp.dta, replace		
		}

	tab xsurvey HHQFQur, m
	
	lab var nEASDP "Total number of EA-SDP pairs in the survey" 		
	
	sort xsurvey HHQFQur
	merge xsurvey HHQFQur using EA_SDP_Link_Summary.dta, 
		tab _merge, m
		*keep if _merge==3 /*SHOULD BE ALL*/
		drop _merge
		
	sort xsurvey HHQFQur
	save EA_SDP_Link_Summary.dta, replace	
	
*** 4. Number of average, min, max SDP per EA
use "$data/EAlevel_SDP_BFR1.dta", clear	
		gen 	min_num_SDPall 	=num_SDPall 
		gen		min_num_SDPpub	=num_SDPpub
		gen		min_num_SDPpub12=num_SDPpub12
		gen		min_num_SDPlow	=num_SDPlow
		gen 	max_num_SDPall 	=num_SDPall 
		gen		max_num_SDPpub	=num_SDPpub
		gen		max_num_SDPpub12=num_SDPpub12
		gen		max_num_SDPlow	=num_SDPlow
		collapse (mean) num_SDP* noSDP* (min) min_num_* (max) max_num_* , by(xsurvey)
		save temp.dta, replace
	use "$data/EAlevel_SDP_BFR1.dta", clear	
		gen 	min_num_SDPall 	=num_SDPall 
		gen		min_num_SDPpub	=num_SDPpub
		gen		min_num_SDPpub12=num_SDPpub12
		gen		min_num_SDPlow	=num_SDPlow
		gen 	max_num_SDPall 	=num_SDPall 
		gen		max_num_SDPpub	=num_SDPpub
		gen		max_num_SDPpub12=num_SDPpub12
		gen		max_num_SDPlow	=num_SDPlow
		collapse (mean) num_SDP* noSDP* (min) min_num_* (max) max_num_* , by(xsurvey HHQFQur)
			drop if HHQFQur==. /*this is to drop odd EASDPs that do not have EAs to link: merge_EASDP_EA==1 */
		append using temp.dta, 
		save temp.dta, replace		
	foreach survey  in $datalistminusone{
	use "$data/EAlevel_SDP_`survey'.dta", clear
		gen 	min_num_SDPall 	=num_SDPall 
		gen		min_num_SDPpub	=num_SDPpub
		gen		min_num_SDPpub12=num_SDPpub12
		gen		min_num_SDPlow	=num_SDPlow
		gen 	max_num_SDPall 	=num_SDPall 
		gen		max_num_SDPpub	=num_SDPpub
		gen		max_num_SDPpub12=num_SDPpub12
		gen		max_num_SDPlow	=num_SDPlow
		collapse (mean) num_SDP* noSDP* (min) min_num_* (max) max_num_* , by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
	use "$data/EAlevel_SDP_`survey'.dta", clear
		gen 	min_num_SDPall 	=num_SDPall 
		gen		min_num_SDPpub	=num_SDPpub
		gen		min_num_SDPpub12=num_SDPpub12
		gen		min_num_SDPlow	=num_SDPlow
		gen 	max_num_SDPall 	=num_SDPall 
		gen		max_num_SDPpub	=num_SDPpub
		gen		max_num_SDPpub12=num_SDPpub12
		gen		max_num_SDPlow	=num_SDPlow
		collapse (mean) num_SDP* noSDP* (min) min_num_* (max) max_num_* , by(xsurvey HHQFQur)
			drop if HHQFQur==. /*this is to drop odd EASDPs that do not have EAs to link: merge_EASDP_EA==1 */
		append using temp.dta, 
		save temp.dta, replace		
		}
		
		tab xsurvey HHQFQur, m

		foreach var of varlist num_SDP*{
			format `var' %4.1f
		}
		lab var num_SDPall        	"number of SDP/EA, average across EAs"
		lab var num_SDPpub     		"number of Public SDP/EA, average across EAs"
		lab var num_SDPpub12      	"number of 1st/2nd level Public SDP/EA, average across EAs"
		lab var num_SDPlow      	"number of 1st/2nd level SDP/EA (any sector), average across EAs"
		lab var min_num_SDPall    	"number of SDP/EA, minimum across EAs"
		lab var min_num_SDPpub 		"number of Public SDP/EA, minimum across EAs"
		lab var min_num_SDPpub12  	"number of 1st/2nd level Public SDP/EA, minimum across EAs"
		lab var min_num_SDPlow  	"number of 1st/2nd level SDP/EA, minimum across EAs"
		lab var max_num_SDPall    	"number of SDP/EA, maximum across EAs"
		lab var max_num_SDPpub 		"number of Public SDP/EA, maximum across EAs"
		lab var max_num_SDPpub12  	"number of 1st/2nd level Public SDP/EA, maximum across EAs"
		lab var max_num_SDPlow  	"number of 1st/2nd level SDP/EA, maximum across EAs"
		
		foreach var of varlist noSDP*{
			replace `var' =100*`var'
			format `var' %4.1f
		}

		lab var noSDPany "% EAs with no linked SDP"
		lab var noSDPpub "% EAs with no linked public SDP"
		lab var noSDPpub12 "% EAs with no linked public primary or secondary SDP"
		lab var noSDPlow "% EAs with no linked primary or secondary SDP"
				
	sort xsurvey HHQFQur
	merge xsurvey HHQFQur using EA_SDP_Link_Summary.dta
		tab _merge, m
		*keep if _merge==3 /*SHOULD BE ALL*/
		drop _merge
		
	gen country=substr(xsurvey, 1, length(xsurvey)-2)
	
	gen round=substr(xsurvey, -1, .)
	
	save EA_SDP_Link_Summary.dta, replace	
	
	export delimited using EA_SDP_Link_Summary.csv, replace
	

*************************************************************************************************	
* D. Create dataset summarizing the SDP level results 
*************************************************************************************************

*** 1. Number of SDP	
use SR_SDPtype_BFR1.dta, clear
		gen nSDP	= 1

			gen temp = dofc(todaySIF)
			format %td temp
			gen tempmonth = month(temp)
			gen tempyear = year(temp)
			gen tempcmc 	= 12*(tempyear - 1900) + tempmonth
			
		egen cmc = median(tempcmc)
		gen year = 1900 + trunc((cmc-1)/12)
		gen month= cmc-12*(year - 1900)		
	
		collapse (count) nSDP* (mean) year month cmc, by(xsurvey)	
		gen group="All"
		save temp.dta, replace

	foreach survey  in $datalistminusone{
	use SR_SDPtype_`survey'.dta, clear
		gen nSDP	= 1
	
			gen temp = dofc(todaySIF)
			format %td temp
			gen tempmonth = month(temp)
			gen tempyear = year(temp)
			gen tempcmc 	= 12*(tempyear - 1900) + tempmonth
			
		egen cmc = median(tempcmc)
		gen year = 1900 + trunc((cmc-1)/12)
		gen month= cmc-12*(year - 1900)				
		
		collapse (count) nSDP* (mean) year month cmc, by(xsurvey)	
		gen group="All"
		append using temp.dta, 
	
		save temp.dta, replace
	}

	foreach survey  in $datalist{
	use SR_SDPtype_`survey'.dta, clear
	
	keep if SDPlow==1
	
		gen nSDP	= 1
	
			gen temp = dofc(todaySIF)
			format %td temp
			gen tempmonth = month(temp)
			gen tempyear = year(temp)
			gen tempcmc 	= 12*(tempyear - 1900) + tempmonth
			
		egen cmc = median(tempcmc)
		gen year = 1900 + trunc((cmc-1)/12)
		gen month= cmc-12*(year - 1900)	
		
		collapse (count) nSDP* (mean) year month cmc, by(xsurvey)	
		
		gen group="Excluding hospitals"
		append using temp.dta, 
	
		save temp.dta, replace
	}	
	
		
	lab var nSDP "Total number of SDPs analyzed"
		
	sort xsurvey group
	save "summary_Access_Indicators_SR.dta", replace
	
*** 2. Percent of SDPs with the essential 5 methods 

use SR_SDPtype_BFR1.dta, clear	

		collapse (mean) essential5_* essential5ec_* , by(xsurvey)
		gen group="All"
		save temp.dta, replace
	
	foreach survey  in $datalistminusone{
	use SR_SDPtype_`survey'.dta, clear	
	
		collapse (mean) essential5_* essential5ec_* , by(xsurvey)
		gen group="All"
		append using temp.dta, 
		save temp.dta, replace		
	}
	
	
	foreach survey  in $datalist{
	use SR_SDPtype_`survey'.dta, clear	
	keep if SDPlow==1
	
		collapse (mean) essential5_* essential5ec_* , by(xsurvey)
		gen group="Excluding hospitals"
		append using temp.dta, 
		save temp.dta, replace		
	}	

		foreach var of varlist essential5_* essential5ec_*{
			replace `var' = `var'*100
			format `var' %4.1f
		}
	
	sort xsurvey group
	merge xsurvey group using "summary_Access_Indicators_SR.dta", 
		tab _merge, m
		
		keep if _merge==3 /*all*/
		drop _merge
		
	gen country=substr(xsurvey, 1, length(xsurvey)-2)
	gen countrycode=substr(xsurvey, 1, 2)
	gen round=substr(xsurvey, -1, .)
			
		replace country="Burkina Faso" if country=="BF"
		replace country="Cote d'Ivoire" if country=="CI"
		replace country="DRC, Kinshasa" if country=="CDKinshasa"
		replace country="DRC, Kongo Central" if country=="CDKongoCentral"
		replace country="Ethiopia" if country=="ET"
		replace country="India, Rajasthan" if country=="INRajasthan"
		replace country="Kenya" if country=="KE"
		replace country="Niger" if country=="NE"
		replace country="Niger, Niamey" if country=="NENiamey"
		replace country="Nigeria, Kano" if country=="NGKano"
		replace country="Nigeria, Lagos" if country=="NGLagos"
		replace country="Uganda" if country=="UG"	
	
	save "summary_Access_Indicators_SR.dta", replace
	
	export delimited using summary_Access_Indicators_SR.csv, replace
	
	* save in additional folders for apps 
	export delimited using ShinyAppPopAccessToMethods/summary_Access_Indicators_SR.csv, replace
	
erase temp.dta	
*browse
tab country round, m

	/*
	keep if group=="All"
	drop essential*
	sort xsurvey
	list xsurvey round year month 
	*/
	
foreach survey  in $datalist{
	erase SR_SDPtype_`survey'.dta 
	}

OKAY SURVEY-level summary data ready HERE
*/

foreach survey  in $datalist{
	use SR_SDPtype_`survey'.dta , clear
	tab xsurvey
	sum essential5_* essential5ec_*
	sum essential5_* essential5ec_* if SDPlow==1
	}

*Table 1 in the technical report 	
	use  EA_SDP_Link_Summary.dta, clear
	keep if HHQFQur ==. 
	keep xsurvey nEA nSDP*
	browse

*Figure 2 in the technical report 	

	use EA_SDP_Link_Summary.dta, clear
	drop if HHQFQur ==. 
	keep if (country!="NENiamey" & country!="NGLagos" & country!="CDKinshasa" & country!="CDKongoCentral")
    keep if (xsurvey!="BFR6" & xsurvey!="NENiameyR5" & xsurvey!="NGKanoR3")
	codebook xsurvey
	
	ttest noSDPany, by(HHQFQur) 
	ttest noSDPlow, by(HHQFQur)
	
	*PAIRED t-test
	keep xsurvey noSDPany noSDPlow HHQFQur
	reshape wide noSDPany noSDPlow, i(xsurvey) j(HHQFQur)
	ttest noSDPany1 ==noSDPany2
	ttest noSDPlow1 ==noSDPlow2



	
	
	