* Effective Access 

clear
clear matrix
clear mata
capture log close
set more off
numlabel, add

************************************************************************
* A. SETTING 
************************************************************************

cd "C:\Users\YoonJoung Choi\Dropbox\0 iSquared\iSquared_PMA\Effective Access\"
global data "C:\Users\YoonJoung Choi\Dropbox\0 Data\PMA\"

global country "Kenya"
global today=c(current_date)
local c_today= "`today'"
global date=subinstr("`c_today'", " ", "",.)

#delimit;
global datalist " 
	KER2 KER3 KER4 KER5 KER6 KER7
	";
	#delimit cr
	
#delimit;
global datalistminusone " 
	KER3 KER4 KER5 KER6	KER7
	";
	#delimit cr

*********************************************************
* B. PREP HR 
*********************************************************	

*****0. getting EA-level information from HHQFQ ready (to check linkage - optional, but just in case)
 
set more off
foreach survey  in $datalist{
	use "$data/`survey'_HR.dta", clear
	sort EA_ID
	keep if EA_ID!=EA_ID[_n-1]
	keep round  ur EA_ID 
	foreach x of varlist round ur  {
		rename `x' HHQFQ`x'
		}
	gen xsurvey="`survey'"		
	sort EA_ID
	save "$data/EA_`survey'.dta", replace
	}
	
	/*
	*repleace Nigeria one including state variable
	use "$data/Nigeria_R3_HR.dta", clear
	sort EA_ID
	keep if EA_ID!=EA_ID[_n-1]
	keep round  ur EA_ID state
	foreach x of varlist round ur  {
		rename `x' HHQFQ`x'
		}
	gen xsurvey="Nigeria_R3"		
	sort EA_ID
	save EA_Nigeria_R3.dta, replace
	*/
	
/*
set more off
foreach survey  in $datalist{
	use EA_`survey'.dta, clear
	tab HHQFQur HHQFQround, m
	}	
*/

*********************************************************
* C. PREP SDP 
*********************************************************	

/*
set more off
foreach survey  in $datalist{
	use "$data/SDP_`survey'.dta", clear
	tab xsurvey
	tab facility_type managing_authority  , m
	}

set more off
foreach survey  in $datalist{
	use "$data/SDP_`survey'.dta", clear
	tab xsurvey
	sum stock_*
	}	

set more off
foreach survey  in $datalist{
	use "$data/SDP_`survey'.dta", clear
	tab xsurvey
	sum implant_insert implant_remove iud_insert iud_remove
	}

set more off
foreach survey  in $datalist{
	use "$data/SDP_`survey'.dta", clear
	tab xsurvey
	sum iud_forceps iud_speculums iud_tenaculum iud_clamp
	sum implant_gloves implant_antiseptic implant_sterile_gauze implant_anesthetic implant_sealed_pack implant_blade
	}
	
*/

***** 1. gen facility variables
set more off
foreach survey  in $datalist{
	use "$data/rawSDP/SDP_`survey'.dta", clear
	
	gen xall	= 1
	gen byte xpublic	= managing_authority==1
	gen byte xprivate	= managing_authority>1	
	gen byte xpub12	= xpublic==1 & (facility_type==2 | facility_type==3) /*primary & secondary*/
	gen byte xpub1	= xpublic==1 & facility_type==3 /*primary*/
	gen byte xlowest= xpub1==1 | xprivate==1 	
	
	codebook facility_ID
	drop if facility_ID==. /*round 5 only?*/
	
	gen xsurvey="`survey'"
	
	save "$data/SDP_`survey'.dta", replace	
	}

	/*
	tab xnumall if EA_ID~=EA_ID[_n-1], m
	tab xnumpub if EA_ID~=EA_ID[_n-1], m	
	*/

***** 2. gen FP stockout variables 	
/*
	*** FIRST RECODE PREP ***
	#delimit;
	global inject1 " 
		BurkinaFaso_R3
		DRCKinshasa_R5
		DRCKongoCentral_R5
		";
		#delimit cr
		
	#delimit;
	global inject2 " 
		Ghana_R3
		Ghana_R4
		";
		#delimit cr	
		
	#delimit;
	global progestinpill " 
		Ethiopia_R2
		Ethiopia_R3
		Ethiopia_R4
		";
		#delimit cr				
		
	foreach survey  in $inject1{	
	use "$data/`survey'_EASDP.dta", clear	
		gen stock_injectables=.
			replace stock_injectables =3 if (stock_sayana>=1 & stock_sayana<=3) | (stock_depo>=1 & stock_depo<=3)
			replace stock_injectables =2 if (stock_sayana>=1 & stock_sayana<=2) | (stock_depo>=1 & stock_depo<=2)						
			replace stock_injectables =1 if stock_sayana ==1 | stock_depo==1
		save "$data/`survey'_EASDP.dta", replace	
		}
		
	foreach survey  in $inject2{	
	use "$data/`survey'_EASDP.dta", clear	
		gen stock_injectables=.
			replace stock_injectables =3 if (stock_injectables_1mo>=1 & stock_injectables_1mo<=3) | (stock_injectables_3mo>=1 & stock_injectables_3mo<=3)
			replace stock_injectables =2 if (stock_injectables_1mo>=1 & stock_injectables_1mo<=2) | (stock_injectables_3mo>=1 & stock_injectables_3mo<=2)						 
			replace stock_injectables =1 if stock_injectables_1mo==1 | stock_injectables_3mo==1
		save "$data/`survey'_EASDP.dta", replace	
		}
	
	foreach survey  in $progestinpill{	
	use "$data/`survey'_EASDP.dta", clear	
		rename stock_pills stock_combined_pills
		gen stock_pills =.
			replace stock_pills =3 if (stock_combined_pills>=1 & stock_combined_pills<=3) | (stock_progestin_pills>=1 & stock_progestin_pills<=3)
			replace stock_pills =2 if (stock_combined_pills>=1 & stock_combined_pills<=2) | (stock_progestin_pills>=1 & stock_progestin_pills<=2)						 
			replace stock_pills =1 if  stock_combined_pills==1 | stock_combined_pills==1		
		save "$data/`survey'_EASDP.dta", replace	
		}		
*/		
	*** THEN SECOND GEN VAR ***	
	
set more off
foreach survey  in $datalist{
	use "$data/SDP_`survey'.dta", clear	
	
	foreach x of varlist stock_iud stock_implant {
		/*
		gen byte bin`x'=`x'==1
		replace bin`x'=. if `x'==.
		lab var bin`x' "`x' In-stock and observed, among facilities offering `x'"
		*/
		gen byte av`x'=`x'==1
		lab var av`x' "`x' In-stock and observed, among all facilities"	
		}

	gen byte hr_iud		= iud_insert==1 & iud_remove==1		
	gen byte hr_implant = implant_insert==1 & implant_remove==1

	egen sup_iud 		= rowtotal(iud_forceps iud_speculums iud_tenaculum )
		replace sup_iud	= 0 if sup_iud<=2
		replace sup_iud = 1 if sup_iud==3
	egen sup_implant	= rowtotal(implant_gloves implant_antiseptic implant_sterile_gauze implant_anesthetic implant_sealed_pack implant_blade)
		replace sup_implant	= 0 if sup_implant<=5
		replace sup_implant = 1 if sup_implant==6
		
	gen byte access_iud 	= avstock_iud ==1 & hr_iud==1 & sup_iud==1	
	gen byte access_implant = avstock_implant ==1 & hr_implant==1 & sup_implant==1
	
	lab var hr_iud 		"personnel for insert/removal"
	lab var sup_iud 	"supplies for insert/removal"	
	lab var access_iud 	"personnel AND supplies AND stock" 
	lab var hr_implant 		"personnel for insert/removal"
	lab var sup_implant 	"supplies for insert/removal"	
	lab var access_implant 	"personnel AND supplies AND stock" 
	
	save "$data/SDP_`survey'.dta", replace	
	}	

*********************************************************
* D. EA-SDP level data by RESHAPE
*********************************************************	
		
***** 3. RESHAPE LONG		
set more off
foreach survey  in $datalist{
	use "$data/SDP_`survey'.dta", clear	

	reshape long EAserved , i(facility_ID) 
	drop if EAserved==.

	rename EA_ID EA_ID_OLD /*this is randomly assigned EA_ID among multiple EAserved?*/
	rename EAserved EA_ID	
	gen space=" "		
	egen EASDP_ID = concat (facility_ID space EA_ID)   
	drop space 	
	
	lab var EA_ID "ID of EA served by SDP"
	lab var EASDP_ID "ID of EA-SDP pair: facility_ID & EA_ID"
	
	codebook facility_ID EASDP_ID	
	sort EA_ID facility_ID EASDP_ID	
	/*
	ASK: why some EAs were listed twice
	IF segmented EAs, we don't need to fix the problem, which is not really a problem. 
	*/
	save "$data/`survey'_EASDP.dta", replace	
}


*********************************************************
*E. GEN EA level data by MERGE SDP & EA, and then collapse  
*********************************************************	

***** 4. merge EA_level information from HHQFQ with SDP and COLLAPSE 

set more off
foreach survey  in KER2{
	use "$data/`survey'_EASDP.dta", clear

	sort EA_ID
	merge EA_ID using EA_`survey'.dta
	tab _merge, m /*check EAs with no SDP. This should be none, but not necessarily in reality*/
		*keep if _merge==3
		*drop _merge
	save temp.dta, replace

	use temp.dta, replace
		collapse (sum) xall xpublic xpub12 , by(xsurvey EA_ID)
		foreach x of varlist xall xpublic xpub12 {
			rename `x' num_`x'
			lab var num_`x' "Numbe of `x' per EA"
			}
			
		sort EA_ID
		merge EA_ID using EA_`survey'.dta
			tab _merge, m 
			drop _merge
		bysort HHQFQur: sum num*
		sort EA_ID
		save "$data/`survey'_EAlevel_SDP.dta", replace
	
	use temp.dta, replace
	foreach x of varlist xall xpublic xpub12 {
	use temp.dta, replace
		keep if `x'==1
		collapse (mean)`x'  (sum)  av* sup_* hr_* access_*, by(xsurvey EA_ID )
		foreach var of varlist av* sup_* hr_* access_*{
			rename `var' `x'_`var'
			lab var `x'_`var' "average `var' per EA, if `x'==1"
			} 
		
		sort EA_ID
		merge EA_ID using "$data/`survey'_EAlevel_SDP.dta", 
			tab _merge, m 
			drop _merge	
		sort EA_ID
		save "$data/`survey'_EAlevel_SDP.dta", replace
		}
	}			
okok
/*
*right here investigate ensure any>public>pub12
foreach survey  in $datalist{
	use "$data/`survey'_EAlevel_SDP.dta", clear

		gen test1=num_xall>= num_xpublic 
		gen test2=num_xpublic >= num_xpub12
		tab test1 test2, m
}
*/
*********************************************************
*F. MERGE with IR   
*********************************************************	

***** 6. merge with IR 

set more off
foreach survey  in $datalist{
	use "$data/`survey'_IR.dta", clear

	sort EA_ID
	merge EA_ID using "$data/`survey'_EAlevel_SDP.dta"
	tab _merge, m /*check EAs with no SDP. This should be none, but not necessarily in reality*/
		*keep if _merge==3
		*drop _merge
	
	save "$data/`survey'_Access.dta", replace
	}	
	
*********************************************************
***** Analysis and constrict a summary dataset 
*********************************************************	

*** Number of EA	
use "$data/EA_KER2.dta", clear			
		egen noEA=group(EA_ID)
		collapse (count) noEA, by(xsurvey)
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/EA_`survey'.dta", clear		
		egen noEA=group(EA_ID)
		collapse (count) noEA, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
	
	lab var noEA "Total number of EA in the survey" 
	
	/*
	use "$data/EA_Nigeria_R3.dta", clear				
		egen noEA=group(EA_ID)
		collapse (count) noEA, by(xsurvey state)
		append using temp.dta
		
	sort xsurvey state
	*/
	sort xsurvey 
	save Access_summary.dta, replace	
		
*** Number of SDP	
use "$data/KER2_SDP.dta", clear
		gen noSDP	= 1
		gen noSDPpublic	=1 if managing_authority==1
		gen noSDPprivate	=1 if managing_authority>1	
		collapse (count) noSDP*, by(xsurvey)	
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/SDP_`survey'.dta", clear
		gen noSDP	= 1
		gen noSDPpublic	=1 if managing_authority==1
		gen noSDPprivate	=1 if managing_authority>1	
		collapse (count) noSDP*, by(xsurvey)	
		append using temp.dta, 
		save temp.dta, replace
		}
		
	lab var noSDP "Total number of SDPs in the survey" 	
	lab var noSDPpublic "Total number of PUBLIC SDPs in the survey" 	
	lab var noSDPprivate "Total number of PRIVATE SDPs in the survey" 	
		
	/*
	use "$data/Nigeria_R3_SDP.dta", clear
		gen noall	= 1
		gen nopublic	=1 if managing_authority==1
		gen noprivate	=1 if managing_authority>1	
		collapse (count) noall nopublic noprivate, by(xsurvey state)	
		append using temp.dta
		
	sort xsurvey state
	merge xsurvey state using Access_summary.dta
		tab _merge, m
		drop _merge
		
	sort xsurvey state	
	*/
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	
	
*** Number of EASDP	
use "$data/KER2_EASDP.dta", clear			
		egen noEASDP=group(EASDP_ID)
		collapse (count) noEASDP, by(xsurvey)
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/`survey'_EASDP.dta", clear		
		egen noEASDP=group(EASDP_ID)
		collapse (count) noEASDP, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
		
	lab var noEASDP "Total number of EA-SDP pairs in the survey" 		
		
	/*
	use "$data/Nigeria_R3_EASDP.dta", clear	
		egen noEASDP=group(EASDP_ID)
		collapse (count) noEASDP, by(xsurvey state)
		append using temp.dta, 	
	
	sort xsurvey state
	merge xsurvey state using Access_summary.dta
		tab _merge, m
		drop _merge
		
	sort xsurvey state	
	*/
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	
	
*** Number of average, min, max SDP per EA
use "$data/KER2_EAlevel_SDP.dta", clear	
		gen 	min_num_xall 	=num_xall 
		gen		min_num_xpublic	=num_xpublic
		gen		min_num_xpub12	=num_xpub12
		gen 	max_num_xall 	=num_xall 
		gen		max_num_xpublic	=num_xpublic
		gen		max_num_xpub12	=num_xpub12
		collapse (mean) num_* (min) min_num_* (max) max_num_*, by(xsurvey)
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/`survey'_EAlevel_SDP.dta", clear
		gen 	min_num_xall 	=num_xall 
		gen		min_num_xpublic	=num_xpublic
		gen		min_num_xpub12	=num_xpub12
		gen 	max_num_xall 	=num_xall 
		gen		max_num_xpublic	=num_xpublic
		gen		max_num_xpub12	=num_xpub12
		collapse (mean) num_* (min) min_num_* (max) max_num_*, by(xsurvey)		
		append using temp.dta, 
		save temp.dta, replace
		}

		lab var num_xall        "number of SDP/EA, average across EAs"
		lab var num_xpublic     "number of Public SDP/EA, average across EAs"
		lab var num_xpub12      "number of 1st/2nd level Public SDP/EA, average across EAs"
		lab var min_num_xall    "number of SDP/EA, minimum"
		lab var min_num_xpublic "number of Public SDP/EA, minimum"
		lab var min_num_xpub12  "number of 1st/2nd level Public SDP/EA, minimum"
		lab var max_num_xall    "number of SDP/EA, maximum"
		lab var max_num_xpublic "number of Public SDP/EA, maximum"
		lab var max_num_xpub12  "number of 1st/2nd level Public SDP/EA, maximum"
		
	/*
	use "$data/Nigeria_R3_EAlevel_SDP.dta", clear	
		collapse (mean) num_*, by(xsurvey state)
		append using temp.dta, 	

	sort xsurvey state
	merge xsurvey state using Access_summary.dta
		tab _merge, m
		drop _merge
		
	sort xsurvey state	
	*/
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	
	
*** Pop level data	
use "$data/KER2_Access.dta", clear	
	gen noWomen=1
	collapse (count) noWomen, by(xsurvey)
	save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/`survey'_Access.dta", clear
		gen noWomen=1
		collapse (count) noWomen, by(xsurvey)		
		append using temp.dta, 
		save temp.dta, replace
		}
		
		lab var noWomen "Female sample size per survey"
		
	/*
	use "$data/Nigeria_R3_Access.dta", clear	
		gen noWomen=1
		collapse (count) noWomen, by(xsurvey state)		
		append using temp.dta, 	

	sort xsurvey state
	merge xsurvey state using Access_summary.dta
		tab _merge, m
		drop _merge
		
	sort xsurvey state	
	*/
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	

*** Pop level access	
#delimit;
global allvar " 	
	xall_avstock_* xall_sup_* xall_hr_* xall_access_* 		
	xpublic_avstock_* xpublic_sup_* xpublic_hr_* xpublic_access_* 	
	xpub12_avstock_* xpub12_sup_* xpub12_hr_* xpub12_access_* 	
	"
	;
	#delimit cr
	
use "$data/KER2_Access.dta", clear	
	foreach x of varlist $allvar{
		replace `x'=1 if `x'>=1 & `x'~=. 
		}
	collapse  $allvar [pw=FQweight], by(xsurvey)
	save temp.dta, replace
	
	foreach survey  in $datalistminusone{
	use "$data/`survey'_Access.dta", clear
		foreach x of varlist $allvar{
			replace `x'=1 if `x'>=1 & `x'~=. 
			}
		collapse $allvar [pw=FQweight], by(xsurvey)	
		append using temp.dta, 
		save temp.dta, replace
		}
		
		lab var xall_access_iud     "% women who have access to EA-linked any SDP(s) that is ready to provide IUD"
		lab var xall_access_implant "% women who have access to EA-linked any SDP(s) that is ready to provide implants"
		lab var xpublic_access_iud     "% women who have access to EA-linked public SDP(s) that is ready to provide IUD"
		lab var xpublic_access_implant "% women who have access to EA-linked public SDP(s) that is ready to provide implants"
		lab var xpub12_access_iud     "% women who have access to EA-linked 1st/2nd public SDP(s) that is ready to provide IUD"
		lab var xpub12_access_implant "% women who have access to EA-linked 1st/2nd public SDP(s) that is ready to provide implants"
	/*
	use "$data/Nigeria_R3_Access.dta", clear	
		foreach x of varlist $allvar{
			replace `x'=1 if `x'>=1 & `x'~=. 
			}
		collapse $allvar [pw=FQweight], by(xsurvey state)	
		append using temp.dta, 	

	sort xsurvey state
	merge xsurvey state using Access_summary.dta
		tab _merge, m
		drop _merge
		
	sort xsurvey state	
	save Access_summary.dta, replace		
	*/
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	
	
*** SDP level
use "$data/KER2_SDP.dta", clear	
	sort facility_ID
	keep if facility_ID~=facility_ID[_n-1]
	collapse (mean) avstock_* hr_* sup_* access_* , by(xsurvey)
	save temp.dta, replace
	
	foreach survey  in $datalistminusone{
	use "$data/SDP_`survey'.dta", clear
		sort facility_ID
		keep if facility_ID~=facility_ID[_n-1]
		collapse (mean) avstock_* hr_* sup_* access_* , by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
	
		lab var access_iud     "% SDPs ready to provide IUD"
		lab var access_implant "% SDPs ready to provide implant"
		lab var avstock_iud     "% SDPs with IUD in stock"
		lab var avstock_implant "% SDPs with implants in stock"
		lab var hr_iud     "% SDPs with trained staff for IUD insertion/removal"
		lab var hr_implant "% SDPs with trained staff for implants insertion/removal"
		lab var sup_iud     "% SDPs with supplies/equipment for IUD"
		lab var sup_implant "% SDPs with supplies/equipment for implants"
		
	/*
	use "$data/Nigeria_R3_EASDP.dta", clear	
		sort facility_ID
		keep if facility_ID~=facility_ID[_n-1]	
		collapse (mean) avstock_* hr_* sup_* access_* , by(xsurvey state)
		append using temp.dta, 	

	sort xsurvey state
	merge xsurvey state using Access_summary.dta
		tab _merge, m
		drop _merge
	*/
	
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	
	foreach x of varlist avstock* hr* sup* access* xall_* xpublic_* xpub12_*{
		replace `x' = `x'*100
		format `x' %9.1f
		}
	
	sort xsurvey 
	save Access_summary.dta, replace	

*Great job! 
	 
*********************************************************
***** Analysis and constrict a summary dataset 
*********************************************************	

capture putdocx clear 
putdocx begin

use Access_summary.dta, clear	

putdocx paragraph
putdocx text ("Trends of access to specific methods: population vs. facility"), linebreak bold 
		
	foreach method in iud implant {
		#delimit; 	
		graph bar xall_access_`method' xpub12_access_`method' access_`method', 
			over(xsurvey)
			title("Access to `method'")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(cranberry*0.8))  
			legend(pos(6) row(1) size(small) 
				label(1 "% women with" "access to `method'," "any SDPs*")	
				label(2 "% women with" "access to `method'," "primary/secondary public SDPs*")					
				label(3 "% of SDPs" "ready to provide" "`method'")	)	
			note("*% women with 1+ EA-linked SDPs 'ready' to provide `method'", size(small))
			xsize(7) ysize(4)
		;
		#delimit cr
		
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		
putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png				
		
		}			

putdocx pagebreak		
putdocx paragraph
putdocx text ("Trends of access: SDP readiness, any"), linebreak bold 
		
	foreach method in iud implant {
		#delimit; 	
		graph bar avstock_`method' sup_`method' hr_`method' access_`method', 
			over(xsurvey)
			title("% of SDPs ready to provide `method'")
			legend(pos(6) row(1) size(vsmall) 
				label(1 "Method in stock")	
				label(2 "Trained staff" "for insertion/removal")		
				label(3 "Supplies/equipment" "in stock") 
				label(4 "All elements") )
				
			bar(1, color(navy)) 
			bar(2, color(navy*0.8)) 
			bar(3, color(navy*0.6)) 
			bar(4, color(cranberry))  				
				
			xsize(7) ysize(4)	
		;
		#delimit cr
		
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		
putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png				
		
		}	
	
putdocx save Access_$country_$date.docx, replace

erase graph.png
erase temp.dta
