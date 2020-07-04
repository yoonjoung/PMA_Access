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

local country "Kenya"
local today=c(current_date)
local c_today= "`today'"
local date=subinstr("`c_today'", " ", "",.)
	
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
	
************************************************************************
* A.2 Non-public data for consultancy
************************************************************************

use "$data/KEP1_WealthWeightAll_13Jan2020.dta", clear

	keep if HHQ_result==1
				
	sort metainstanceID 
	keep if metainstanceID!=metainstanceID[_n-1]
	codebook metainstanceID
	
		gen round=8
		egen EA_ID=group(EA)

	save "$data/HR_KER8.dta", replace

use "$data/KEP1_WealthWeightAll_13Jan2020.dta", clear

	keep if HHQ_result==1
	keep if FRS_result==1
	keep if FQmetainstanceID!=""
	keep if last_night==1

		gen round=8
		egen EA_ID=group(EA)
		
	save "$data/IR_KER8.dta", replace
	
*********************************************************
* B. PREP EA-level data  
*********************************************************	

*****0. getting EA-level information from HHQFQ ready (to check linkage - optional, but just in case)
 
set more off
foreach survey  in $datalist{
	use "$data/HR_`survey'.dta", clear
	sort EA_ID
	keep if EA_ID!=EA_ID[_n-1]
	keep round county ur EA_ID 
	foreach x of varlist round county ur  {
		rename `x' HHQFQ`x'
		}
	gen xsurvey="`survey'"		

		tab xsurvey HHQFQround, m
	
	sort EA_ID
	save "$data/EA_`survey'.dta", replace
	}

*********************************************************
* D. EA-SDP level data by RESHAPE
*********************************************************	
		
***** 3. RESHAPE LONG		
foreach survey  in $datalist{
	use "$data/SR_`survey'.dta", clear	
	lookfor EAserved
	sum EAserved*
	}
	
set more off
foreach survey  in $datalist{
	use "$data/SR_`survey'.dta", clear	

	gen SDPall			= 1
	gen byte SDPpub		= managing_authority==1
	gen byte SDPpub12	= managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	gen byte SDPlow		= (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/
			
	reshape long EAserved , i(facility_ID) 
	drop if EAserved==.

	rename EA_ID EA_ID_OLDRANDOM /*this is randomly assigned EA_ID among multiple EAserved?*/
	rename EAserved EA_ID	
	gen space=" "		
	egen EASDP_ID = concat (facility_ID space EA_ID)   
	drop space 	
	
	lab var EA_ID "ID of EA served by SDP"
	lab var EASDP_ID "ID of EA-SDP pair: facility_ID & EA_ID"
	
		tab xsurvey round, m
		
	codebook facility_ID EASDP_ID	
	sort EA_ID facility_ID EASDP_ID	
	/*
	CHECK AND ASK: why some EAs were listed twice
	IF segmented EAs, we don't need to fix the problem, which is not really a problem. 
	*/
	save "$data/EASDP_`survey'.dta", replace	
	tab xsurvey
}

*********************************************************
*E. GEN EA level data by MERGE SDP & EA, and then collapse  
*********************************************************	

***** 4. merge EA_level information from HHQFQ with SDP and COLLAPSE 

set more off
foreach survey  in $datalist{
	use "$data/EASDP_`survey'.dta", clear /*this is reshaped SDP data*/

	sort EA_ID
	merge EA_ID using "$data/EA_`survey'.dta" /*merge with EA list from HHQFQ*/
	tab _merge, m 
		/*
		RED FLAG
		check EAs with no SDP (i.e., _merge==2 or round==.) 
		This should be none, but not necessarily in reality. WHY???
		*/
		
		tab _merge round, m
		tab EA_ID if _merge==2
		*keep if _merge==3
		*drop _merge
					
	save temp.dta, replace

	use temp.dta, replace
		collapse (sum) SDPall SDPpub SDPpub12 SDPlow , by(xsurvey EA_ID)
		foreach x of varlist SDPall SDPpub SDPpub12 SDPlow {
			rename `x' num_`x'
			lab var num_`x' "Numbe of `x' per EA"
			}
			
		sort EA_ID
		merge EA_ID using "$data/EA_`survey'.dta"
			tab _merge, m 
			drop _merge
		bysort HHQFQur: sum num*
		sort EA_ID
		save "$data/EAlevel_SDP_`survey'.dta", replace
	
	use temp.dta, replace
	foreach x of varlist SDPall SDPpub SDPpub12 SDPlow {
	use temp.dta, replace
		keep if `x'==1
		collapse (mean)`x'  (sum)  essential* ready*, by(xsurvey EA_ID )
		foreach var of varlist essential* ready*{
			rename `var' `x'_`var'
			lab var `x'_`var' "average `var' per EA, if `x'==1"
			} 
		
		sort EA_ID
		merge EA_ID using "$data/EAlevel_SDP_`survey'.dta", 
			tab _merge, m 
			drop _merge	
		sort EA_ID
		save "$data/EAlevel_SDP_`survey'.dta", replace /*EA-level data for linked SDP data*/
		}
	}			
	
*********************************************************
*F. MERGE with IR   
*********************************************************	

***** 6. merge with IR 

set more off
foreach survey  in $datalist{
	use "$data/IR_`survey'.dta", clear

	sort EA_ID
	merge EA_ID using "$data/EAlevel_SDP_`survey'.dta"
	tab _merge, m /*check EAs with no SDP. This should be none, but not necessarily in reality*/
		*keep if _merge==3
		*drop _merge
	
	save "$data/Access_`survey'.dta", replace
	}	

/*
foreach survey  in $datalist{
use "$data/Access_`survey'.dta", clear	
	sum round $allvar
}
*/	
	
	
*********************************************************
***** Analysis and constrict a summary dataset 
*********************************************************	

*** Number of EA	
use "$data/EA_KER2.dta", clear			
		egen nEA=group(EA_ID)
		collapse (count) nEA, by(xsurvey)
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/EA_`survey'.dta", clear		
		egen nEA=group(EA_ID)
		collapse (count) nEA, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
	
	lab var nEA "Total number of EA in the survey" 
	
	sort xsurvey 
	save Access_summary.dta, replace	
		
*** Number of SDP	
use "$data/SR_KER2.dta", clear
		gen nSDP	= 1
		gen nSDPpub	=1 if managing_authority==1
		gen nSDPprivate	=1 if managing_authority>1	
		collapse (count) nSDP*, by(xsurvey)	
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/SR_`survey'.dta", clear
		gen nSDP	= 1
		gen nSDPpub	=1 if managing_authority==1
		gen nSDPprivate	=1 if managing_authority>1	
		collapse (count) nSDP*, by(xsurvey)	
		append using temp.dta, 
		save temp.dta, replace
		}
		
	lab var nSDP "Total number of SDPs in the survey" 	
	lab var nSDPpub "Total number of PUBLIC SDPs in the survey" 	
	lab var nSDPprivate "Total number of PRIVATE SDPs in the survey" 	
		
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		keep if _merge==3 /*SHOULD BE ALL*/
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	

*** Number of EASDP	
use "$data/EASDP_KER2.dta", clear			
		egen nEASDP=group(EASDP_ID)
		collapse (count) nEASDP, by(xsurvey)
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/EASDP_`survey'.dta", clear		
		egen nEASDP=group(EASDP_ID)
		collapse (count) nEASDP, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
		
	lab var nEASDP "Total number of EA-SDP pairs in the survey" 		
		
	sort xsurvey
	merge xsurvey using Access_summary.dta, 
		tab _merge, m
		*keep if _merge==3 /*SHOULD BE ALL*/
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	
	
*** Number of average, min, max SDP per EA
use "$data/EAlevel_SDP_KER2.dta", clear	
		gen 	min_num_SDPall 		=num_SDPall 
		gen		min_num_SDPpub	=num_SDPpub
		gen		min_num_SDPpub12	=num_SDPpub12
		gen		min_num_SDPlow	=num_SDPlow
		gen 	max_num_SDPall 		=num_SDPall 
		gen		max_num_SDPpub	=num_SDPpub
		gen		max_num_SDPpub12	=num_SDPpub12
		gen		max_num_SDPlow	=num_SDPlow
		gen 	p10_num_SDPall 		=num_SDPall 
		gen		p10_num_SDPpub	=num_SDPpub
		gen		p10_num_SDPpub12	=num_SDPpub12
		gen		p10_num_SDPlow	=num_SDPlow
		gen 	p25_num_SDPall 		=num_SDPall 
		gen		p25_num_SDPpub	=num_SDPpub
		gen		p25_num_SDPpub12	=num_SDPpub12
		gen		p25_num_SDPlow	=num_SDPlow
		gen 	p50_num_SDPall 		=num_SDPall 
		gen		p50_num_SDPpub	=num_SDPpub
		gen		p50_num_SDPpub12	=num_SDPpub12			
		gen		p50_num_SDPlow	=num_SDPlow
		gen 	p90_num_SDPall 		=num_SDPall 
		gen		p90_num_SDPpub	=num_SDPpub
		gen		p90_num_SDPpub12	=num_SDPpub12		
		gen		p90_num_SDPlow	=num_SDPlow		
		collapse (mean) num_* (min) min_num_* (max) max_num_* (p10) p10_* (p25) p25_* (p50) p50_* (p90) p90_*, by(xsurvey)
		save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/EAlevel_SDP_`survey'.dta", clear
		gen 	min_num_SDPall 		=num_SDPall 
		gen		min_num_SDPpub	=num_SDPpub
		gen		min_num_SDPpub12	=num_SDPpub12
		gen		min_num_SDPlow	=num_SDPlow
		gen 	max_num_SDPall 		=num_SDPall 
		gen		max_num_SDPpub	=num_SDPpub
		gen		max_num_SDPpub12	=num_SDPpub12
		gen		max_num_SDPlow	=num_SDPlow
		gen 	p10_num_SDPall 		=num_SDPall 
		gen		p10_num_SDPpub	=num_SDPpub
		gen		p10_num_SDPpub12	=num_SDPpub12
		gen		p10_num_SDPlow	=num_SDPlow
		gen 	p25_num_SDPall 		=num_SDPall 
		gen		p25_num_SDPpub	=num_SDPpub
		gen		p25_num_SDPpub12	=num_SDPpub12
		gen		p25_num_SDPlow	=num_SDPlow
		gen 	p50_num_SDPall 		=num_SDPall 
		gen		p50_num_SDPpub	=num_SDPpub
		gen		p50_num_SDPpub12	=num_SDPpub12			
		gen		p50_num_SDPlow	=num_SDPlow
		gen 	p90_num_SDPall 		=num_SDPall 
		gen		p90_num_SDPpub	=num_SDPpub
		gen		p90_num_SDPpub12	=num_SDPpub12		
		gen		p90_num_SDPlow	=num_SDPlow
		collapse (mean) num_* (min) min_num_* (max) max_num_* (p10) p10_* (p25) p25_* (p50) p50_* (p90) p90_*, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}

		lab var num_SDPall        	"number of SDP/EA, average across EAs"
		lab var num_SDPpub     		"number of Public SDP/EA, average across EAs"
		lab var num_SDPpub12      	"number of 1st/2nd level Public SDP/EA, average across EAs"
		lab var num_SDPlow      	"number of 1st/2nd level SDP/EA (any sector), average across EAs"
		lab var min_num_SDPall    	"number of SDP/EA, minimum"
		lab var min_num_SDPpub 		"number of Public SDP/EA, minimum"
		lab var min_num_SDPpub12  	"number of 1st/2nd level Public SDP/EA, minimum"
		lab var max_num_SDPall    	"number of SDP/EA, maximum"
		lab var max_num_SDPpub 		"number of Public SDP/EA, maximum"
		lab var max_num_SDPpub12  	"number of 1st/2nd level Public SDP/EA, maximum"
		
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	
	
*** Pop level data: female sample size	
use "$data/Access_KER2.dta", clear	
	gen nWomen=1
	collapse (count) nWomen, by(xsurvey)
	save temp.dta, replace
	foreach survey  in $datalistminusone{
	use "$data/Access_`survey'.dta", clear
		gen nWomen=1
		collapse (count) nWomen, by(xsurvey)		
		append using temp.dta, 
		save temp.dta, replace
		}
		
		lab var nWomen "Female sample size per survey"
		
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	

*** Pop level access: % female whose linked SDPs have methods	
#delimit;
global allvar " 	
	SDPall_essential* 	SDPpub_essential*	SDPpub12_essential*	SDPlow_essential*	
	SDPall_ready* 	SDPpub_ready*	SDPpub12_ready*	SDPlow_ready*
	"
	;
	#delimit cr
/*
foreach survey  in $datalist{
	use "$data/Access_`survey'.dta", clear
	sum $allvar
}	
*/
	
use "$data/Access_KER2.dta", clear	
	foreach x of varlist $allvar{
		replace `x'=0 if `x'==. 
		replace `x'=1 if `x'>=1 
		*replace `x'=1 if `x'>=1 & `x'~=. 
		}
	collapse  $allvar [pw=FQweight], by(xsurvey)
	save temp.dta, replace
	
	foreach survey  in $datalistminusone{
	use "$data/Access_`survey'.dta", clear
		foreach x of varlist $allvar{
			replace `x'=0 if `x'==. 
			replace `x'=1 if `x'>=1 
			*replace `x'=1 if `x'>=1 & `x'~=. 
			}
		collapse $allvar [pw=FQweight], by(xsurvey)	
		append using temp.dta, 
		save temp.dta, replace
		}
	
	foreach x of varlist $allvar{
		rename `x' pop`x'
		}	
		
		lab var popSDPall_essential5_offer 	"% women whose EA-linked any SDP(s) offers `essential' specific five methods"
		lab var popSDPall_essential5_curav 	"% women whose EA-linked any SDP(s) currently has `essential' specific five methods"
		lab var popSDPall_essential5_noso 	"% women whose EA-linked any SDP(s) currently has (with NOSO) `essential' specific five methods"
		lab var popSDPall_essential5_ready 	"% women whose EA-linked any SDP(s) currently has (with readiness**) `essential' specific five methods"

	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	

/*
*right here investigate ensure any>public>pub12

gen test1= (SDPall_essential5 	 >=SDPpub_essential5) 	 & (SDPpub_essential5 	 >=SDPpub12_essential5)
gen test2= (SDPall_essential4 	 >=SDPpub_essential4) 	 & (SDPpub_essential4 	 >=SDPpub12_essential4)
gen test3= (SDPall_essential5_alt>=SDPpub_essential5_alt)& (SDPpub_essential5_alt>=SDPpub12_essential5_alt)
gen test4= (SDPall_ready_iud 	 >=SDPpub_ready_iud)	 & (SDPpub_ready_iud 	 >=SDPpub12_ready_iud)
gen test5= (SDPall_ready_implant >=SDPpub_ready_implant) & (SDPpub_ready_implant >=SDPpub12_ready_implant)
sum test*
*/	
	
	
*** SDP level
use "$data/SR_KER2.dta", clear	

	collapse (mean) essential* ready* , by(xsurvey)
	save temp.dta, replace
	
	foreach survey  in $datalistminusone{
	use "$data/SR_`survey'.dta", clear

		collapse (mean) essential* ready*, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
	
	foreach x of varlist essential* ready*{
		rename `x' SDPall_`x'
		}
	
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	

use "$data/SR_KER2.dta", clear	

	gen byte SDPpub	= managing_authority==1 
	keep if SDPpub==1	
	
	collapse (mean) essential* ready*, by(xsurvey)
	save temp.dta, replace
	
	foreach survey  in $datalistminusone{
	use "$data/SR_`survey'.dta", clear

		gen byte SDPpub	= managing_authority==1 
		keep if SDPpub==1	
	
		collapse (mean) essential* ready*, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
		
	foreach x of varlist essential* ready*{
		rename `x' SDPpub_`x'
		}		
	
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	
	
use "$data/SR_KER2.dta", clear	

	gen byte SDPpub12	= managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	keep if SDPpub12==1	
	
	collapse (mean) essential* ready*, by(xsurvey)
	save temp.dta, replace
	
	foreach survey  in $datalistminusone{
	use "$data/SR_`survey'.dta", clear

		gen byte SDPpub12	= managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
		keep if SDPpub12==1		
		
		collapse (mean) essential* ready*, by(xsurvey)
		append using temp.dta, 
		save temp.dta, replace
		}
		
	foreach x of varlist essential* ready*{
		rename `x' SDPpub12_`x'
		}		
	
	sort xsurvey
	merge xsurvey using Access_summary.dta
		tab _merge, m
		drop _merge
	sort xsurvey
	save Access_summary.dta, replace	

	*convert proportion to percentage
	
	foreach x of varlist SDP* popSDP* {
		replace `x' = `x'*100
		format `x' %9.1f
		}
	
	sort xsurvey 
	save Access_summary_`country'.dta, replace	

erase temp.dta
	
Great job! 

*********************************************************
***** Analysis and constrict a summary dataset 
*********************************************************	

capture putdocx clear 
putdocx begin

use Access_summary_`country'.dta, clear	

putdocx paragraph
putdocx text ("Trends of access to specific methods: population vs. facility"), linebreak bold 
putdocx text (""), linebreak
putdocx text ("- By SDP type denominator"), linebreak 
putdocx text ("- Using examples of `offer'"), linebreak 
putdocx text ("- 4 sets of essential methods: 5, 5+EC, 4*, and 4*+EC"), linebreak 
putdocx text (""), linebreak
putdocx text ("-- Essential 5: IUD, implants, injectables, pills, AND male condoms"), linebreak
putdocx text ("-- Essential 5+EC: IUD, implants, injectables, pills, male condoms, AND EC"), linebreak
putdocx text ("-- Essential 4*: IUD or implants, injectables, pills, AND male condoms"), linebreak
putdocx text ("-- Essential 4*+EC: IUD or implants, injectables, pills, male condoms, AND EC"), linebreak
		
	foreach method in essential5_offer essential5ec_offer essential4_offer essential4ec_offer  {
		#delimit; 	
		graph bar popSDPall_`method' popSDPpub_`method' popSDPpub12_`method' SDPall_`method' SDPpub_`method' SDPpub12_`method'  , 
			over(xsurvey)
			title("Access to `method'")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(cranberry*0.8))  
			bar(5, color(cranberry*0.6))  
			bar(6, color(cranberry*0.4))  
			legend(pos(6) row(1) size(small) stack 
				label(1 "% women" "whose linked SDPs" "`method'," "all SDPs")	
				label(2 "% women" "whose linked SDPs" "`method'," "public SDPs")	
				label(3 "% women" "whose linked SDPs" "`method'," "primary/secondary" "public SDPs")	
				label(4 "% SDPs" "`method'," "all SDPs")	
				label(5 "% SDPs" "`method'," "public SDPs")	
				label(6 "% SDPs" "`method'," "primary/secondary" "public SDPs")	
				)
			ylab(0(20)100, angle(0) labsize(small))
			xsize(7) ysize(4)
		;
		#delimit cr

		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png				
}		


putdocx pagebreak		
putdocx paragraph
putdocx text ("Trends of access to specific methods: population vs. facility"), linebreak bold 
putdocx text ("- 4 sets of access metrics"), linebreak 
putdocx text (""), linebreak
putdocx text ("-- SDP offers all essential methods"), linebreak
putdocx text ("-- SDP currently has all essential methods"), linebreak 
putdocx text ("-- SDP currently has and no 3-mo stockout for all essential methods"), linebreak
putdocx text ("-- SDP ready for all essential methods (readiness for IUD/implant + current availability for other methods)"), linebreak 
		
	foreach method in essential5 essential5ec essential4 essential4ec  {
		#delimit; 	
		graph bar popSDPall_`method'_offer popSDPall_`method'_curav popSDPall_`method'_noso popSDPall_`method'_ready
				  SDPall_`method'_offer SDPall_`method'_curav SDPall_`method'_noso SDPall_`method'_ready , 
			over(xsurvey)
			title("Access to `method': with data from all SDPs")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(navy*0.2)) 
			bar(5, color(cranberry*0.8))  
			bar(6, color(cranberry*0.6))  
			bar(7, color(cranberry*0.4))  
			bar(8, color(cranberry*0.2))  
			legend(pos(6) row(1) size(vsmall) stack 
				label(1 "% women whose" "linked SDPs" "offers" "`method'")	
				label(2 "% women whose" "linked SDPs" "has" "`method'")
				label(3 "% women whose" "linked SDPs" "has NOSO of" "`method'") 
				label(4 "% women whose" "linked SDPs" "ready* for" "`method'") 
				label(5 "% SDPs" "offers" "`method',")
				label(6 "% SDPs" "has" "`method',")
				label(7 "% SDPs" "has NOSO of" "`method',")
				label(8 "% SDPs" "ready* for" "`method',")
				)
			note("*readiness for implants and IUD. For other methods, current availability", size(small) )
			ylab(0(20)100, angle(0) labsize(small))
			xsize(7) ysize(4)
		;
		#delimit cr
		
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy

putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png				
		
		}			

	foreach method in essential5 essential5ec essential4 essential4ec  {
		#delimit; 	
		graph bar popSDPpub_`method'_offer popSDPpub_`method'_curav popSDPpub_`method'_noso popSDPpub_`method'_ready
				  SDPpub_`method'_offer SDPpub_`method'_curav SDPpub_`method'_noso SDPpub_`method'_ready , 
			over(xsurvey)
			title("Access to `method': with data from public SDPs")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(navy*0.2)) 
			bar(5, color(cranberry*0.8))  
			bar(6, color(cranberry*0.6))  
			bar(7, color(cranberry*0.4))  
			bar(8, color(cranberry*0.2))  
			legend(pos(6) row(1) size(vsmall) stack 
				label(1 "% women whose" "linked SDPs" "offers" "`method'")	
				label(2 "% women whose" "linked SDPs" "has" "`method'")
				label(3 "% women whose" "linked SDPs" "has NOSO of" "`method'") 
				label(4 "% women whose" "linked SDPs" "ready* for" "`method'") 
				label(5 "% SDPs" "offers" "`method',")
				label(6 "% SDPs" "has" "`method',")
				label(7 "% SDPs" "has NOSO of" "`method',")
				label(8 "% SDPs" "ready* for" "`method',")
				)
			note("*readiness for implants and IUD. For other methods, current availability", size(small) )
			ylab(0(20)100, angle(0) labsize(small))
			xsize(7) ysize(4)
		;
		#delimit cr
		
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy

putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png				
		
		}			
		
	foreach method in essential5 essential5ec essential4 essential4ec  {
		#delimit; 	
		graph bar popSDPpub12_`method'_offer popSDPpub12_`method'_curav popSDPpub12_`method'_noso popSDPpub12_`method'_ready
				  SDPpub12_`method'_offer SDPpub12_`method'_curav SDPpub12_`method'_noso SDPpub12_`method'_ready , 
			over(xsurvey)
			title("Access to `method': with data from primary or secondary public SDPs")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(navy*0.2)) 
			bar(5, color(cranberry*0.8))  
			bar(6, color(cranberry*0.6))  
			bar(7, color(cranberry*0.4))  
			bar(8, color(cranberry*0.2))  
			legend(pos(6) row(1) size(vsmall) stack 
				label(1 "% women whose" "linked SDPs" "offers" "`method'")	
				label(2 "% women whose" "linked SDPs" "has" "`method'")
				label(3 "% women whose" "linked SDPs" "has NOSO of" "`method'") 
				label(4 "% women whose" "linked SDPs" "ready* for" "`method'") 
				label(5 "% SDPs" "offers" "`method',")
				label(6 "% SDPs" "has" "`method',")
				label(7 "% SDPs" "has NOSO of" "`method',")
				label(8 "% SDPs" "ready* for" "`method',")
				)
			note("*readiness for implants and IUD. For other methods, current availability", size(small) )
			ylab(0(20)100, angle(0) labsize(small))
			xsize(7) ysize(4)
		;
		#delimit cr
		
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy

putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png				
		
		}				

putdocx pagebreak		
putdocx paragraph
putdocx text ("Appendix 1: Distribution of SDPs by sector and type"), linebreak bold 
		
	set more off
	use "$data/SR_KER2.dta", clear	
	foreach survey  in $datalistminusone{
	append using "$data/SR_`survey'.dta", force
		}	
	
		tab xsurvey
		tab facility_type sector, m
		/*
							 |        sector
			Type of facility |    Public    Private |     Total
		---------------------+----------------------+----------
				 1. hospital |       387         15 |       402 
			2. health_center |       501         20 |       521 
			3. health_clinic |         9         82 |        91 
			   4. dispensary |       941         38 |       979 
				 5. pharmacy |         0        241 |       241 
		6. nursing_maternity |         0          6 |         6 
				   96. other |         6          9 |        15 
		---------------------+----------------------+----------
					   Total |     1,844        411 |     2,255 

		*/

		gen pub3	=1 if sector==0 & facility_type==1
		gen pub2	=1 if sector==0 & facility_type==2
		gen pub1	=1 if sector==0 & (facility_type>=3 & facility_type<=4)
		gen puboth	=1 if sector==0 & facility_type>=5
			
		gen prv3	=1 if sector==1 & facility_type==1
		gen prv2	=1 if sector==1 & facility_type==2
		gen prv1	=1 if sector==1 & (facility_type>=3 & facility_type<=4)
		gen prvoth  =1 if sector==1 & facility_type>=5	
		
		collapse (count) pub* prv*, by(xsurvey)

		graph bar pub* prv*, over(xsurvey) stack 	
		
		#delimit; 
		graph bar pub* prv*, over(xsurvey) stack percent
			legend(pos(3) col(1) order (8 7 6 5 4 3 2 1) size(small)
				label(1 "public" "hospitals")
				label(2 "public" "health center")
				label(3 "public" "clinic/dispensary") 
				label(4 "public" "pharmacy/other")
				label(5 "private" "hospitals")
				label(6 "private" "health center")
				label(7 "private" "clinic/dispensary") 
				label(8 "private" "pharmacy/other")
				)
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(navy*0.2)) 
			bar(5, color(cranberry*0.8))  
			bar(6, color(cranberry*0.6))  
			bar(7, color(cranberry*0.4))  
			bar(8, color(cranberry*0.2))  	
			ytitle("% of SDP samples")
			;
			#delimit cr
		
	gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
	gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
	gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy

putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png				

putdocx pagebreak		
putdocx paragraph
putdocx text ("Appendix 2: Number of linked SDPs per EA"), linebreak bold 
putdocx text ("Based on the survey design, each EA would have 3-6 SDPs, though some SDPs may serve multiple EAs. However, not all EAs have three or more SDPs linked to the EA. Some EAs are repeated offerders..."), linebreak

use Access_summary_`country'.dta, clear	
	
putdocx paragraph
putdocx table stable = (1,4), title("Number of EAs, SDPs, and EA-SDP linked pairs: `country'")  
putdocx table table = data(xsurvey nEA nSDP nEASDP) 

use "$data/EAlevel_SDP_KER2.dta", clear	
	foreach survey  in $datalistminusone{
	append using "$data/EAlevel_SDP_`survey'.dta", force
	}
		
		tab xsurvey, m
		
		#delimit; 	
		graph box num_*, over(xsurvey)
			legend(pos(6) row(1) size(small)
				label(1 "All SDPs")
				label(2 "public")
				label(3 "public primary/secondary") 
				label(4 "any primary/secondary") )
			ytitle("Nnumber of linked SDPs per EA")	
			;
			#delimit cr				

	gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
	gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
	gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png		
			
		gen byte noSDPany 	=num_SDPall==0
		gen byte noSDPpub 	=num_SDPpub==0
		gen byte noSDPpub12 =num_SDPpub12==0
		gen byte noSDPlow 	=num_SDPlow==0
			
		#delimit; 
		graph bar noSDP*, over(xsurvey) 
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public primary or secondary") 
				label(4 "Any primary or secondary") )
			ytitle("EAs with no linked SDPs (proportion)") ylab(0(0.05)0.2)
			;
			#delimit cr
	
	gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
	gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
	gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
putdocx paragraph
graph export graph.png, replace	
putdocx image graph.png		
	
putdocx save Access_`country'_`date'.docx, replace

erase graph.png

*** further investigation of EAs with too many SDPs 

use "$data/EAlevel_SDP_KER2.dta", clear	
	foreach survey  in $datalistminusone{
	append using "$data/EAlevel_SDP_`survey'.dta", force
	}
		
		tab xsurvey round, m
		
		tab xsurvey num_SDPall, m
		tab county if num_SDPall>=7
		tab xsurvey county if num_SDPall>=7
		
		sort xsurvey EA_ID
		list xsurvey county EA_ID if num_SDPall>=7
		list xsurvey county EA_ID num_SDPall if num_SDPall>=9
		/*
		.                 list xsurvey county EA_ID num_SDPall if num_SDPall>=9

			 +-----------------------------------------+
			 | xsurvey       county   EA_ID   num_SD~l |
			 |-----------------------------------------|
		  7. |    KER2    3. KIAMBU    4039         10 |
		129. |    KER3    3. KIAMBU    4039          9 |
		251. |    KER4    3. KIAMBU    4039          9 |
		493. |    KER5   1. BUNGOMA    4895          9 |
		498. |    KER5    3. KIAMBU    4919         10 |
			 |-----------------------------------------|
		501. |    KER5     7. NANDI    4938          9 |
		644. |    KER6   1. BUNGOMA    4895          9 |
			 +-----------------------------------------+
		*/
			
		set more off
		foreach survey  in $datalist{
			use "$data/EASDP_`survey'.dta", clear	
			sort county EA_ID facility_ID
			list xsurvey EA_ID facility_ID facility_type if EA_ID==4039
		}		
		
		/*
			 +---------------------------------------------------+
			 | xsurvey   EA_ID   facili~D          facility_type |
			 |---------------------------------------------------|
		120. |    KER2    4039       4006            1. hospital |
		121. |    KER2    4039       4032            1. hospital |
		122. |    KER2    4039       4069            1. hospital |
		123. |    KER2    4039       4119          4. dispensary |
		124. |    KER2    4039       4187       2. health_center |
			 |---------------------------------------------------|
		125. |    KER2    4039       4365       2. health_center |
		126. |    KER2    4039       4611       2. health_center |
		127. |    KER2    4039       4634   6. nursing_maternity |
		128. |    KER2    4039       4827       2. health_center |
		129. |    KER2    4039       4889       2. health_center |
			 +---------------------------------------------------+
		*/
				
		use "$data/SR_KER2.dta", clear	
		#delimit;
		keep if facility_ID==4006 | facility_ID==4032 | facility_ID==4069 | 
				facility_ID==4119 | facility_ID==4187 | facility_ID==4365 |
				facility_ID==4611 | facility_ID==4634 | facility_ID==4827 | facility_ID==4889
				;
				#delimit cr
		keep xsurvey facility_ID EAserved* 
		list facility_ID EAserved1 EAserved2 EAserved3 EAserved4 EAserved5 EAserved6 EAserved8 EAserved9 EAserved10 EAserved11 EAserved12 EAserved13

			
		set more off
		use "$data/SR_KER2.dta", clear	
		foreach survey  in $datalistminusone{
			append using "$data/SR_`survey'.dta", force
			}
			
			global EAserved "EAserved1 EAserved2 EAserved3 EAserved4 EAserved5 EAserved6 EAserved8 EAserved9 EAserved10 EAserved11 EAserved12 EAserved13"	
			
			foreach var of varlist $EAserved {
			gen x_`var' = `var'!=.
			}
		
			egen xTotalEAserved=rowtotal(x_EAserved*)
						
			tab xTotalEAserved xsurvey, m 
			tab xTotalEAserved facility_type, m 
				
			/*
			.                         tab xTotalEAserved xsurvey, m 

			xTotalEAse |                              xsurvey
				  rved |      KER2       KER3       KER4       KER5       KER6       KER7 |     Total
			-----------+------------------------------------------------------------------+----------
					 1 |       288        312        302        378        386        389 |     2,055 
					 2 |        15         15         15         13         10         10 |        78 
					 3 |         7          7          7          5          5          5 |        36 
					 4 |         1          1          1          2          1          1 |         7 
					 5 |         2          2          2          1          2          1 |        10 
					 6 |         2          2          2          3          3          3 |        15 
					 7 |         1          1          1          1          1          2 |         7 
					 8 |         1          1          1          0          0          0 |         3 
					 9 |         1          1          1          2          2          2 |         9 
					10 |         0          0          0          0          1          1 |         2 
					11 |         3          3          3          2          2          2 |        15 
					12 |         3          3          3          3          3          3 |        18 
			-----------+------------------------------------------------------------------+----------
				 Total |       324        348        338        410        416        419 |     2,255 

			.                         tab xTotalEAserved facility_type, m 

			xTotalEAse |                               Type of facility
				  rved | 1. hospit  2. health  3. health  4. dispen  5. pharma  6. nursin  96. other |     Total
			-----------+-----------------------------------------------------------------------------+----------
					 1 |       233        496         91        974        240          6         15 |     2,055 
					 2 |        48         25          0          5          0          0          0 |        78 
					 3 |        36          0          0          0          0          0          0 |        36 
					 4 |         7          0          0          0          0          0          0 |         7 
					 5 |        10          0          0          0          0          0          0 |        10 
					 6 |        15          0          0          0          0          0          0 |        15 
					 7 |         6          0          0          0          1          0          0 |         7 
					 8 |         3          0          0          0          0          0          0 |         3 
					 9 |         9          0          0          0          0          0          0 |         9 
					10 |         2          0          0          0          0          0          0 |         2 
					11 |        15          0          0          0          0          0          0 |        15 
					12 |        18          0          0          0          0          0          0 |        18 
			-----------+-----------------------------------------------------------------------------+----------
				 Total |       402        521         91        979        241          6         15 |     2,255 

			*/
				
*** further investigation of EAs with no linked SDP 

use "$data/EAlevel_SDP_KER2.dta", clear	
	foreach survey  in $datalistminusone{
	append using "$data/EAlevel_SDP_`survey'.dta", force
	}
		
		tab xsurvey round, m		
		gen byte noSDPany 	=num_SDPall==0
		gen byte noSDPpub 	=num_SDPpub==0
		gen byte noSDPpub12 =num_SDPpub12==0
		
		tab noSDPany round, m
		tab noSDPany noSDPpub, m
		tab noSDPpub noSDPpub12, m

		list xsurvey EA_ID noSDP* if noSDPany==1
		list xsurvey EA_ID noSDP* if noSDPpub==1
		list xsurvey EA_ID noSDP* if noSDPpub12==1		
		
	/*
.                 list xsurvey EA_ID noSDP* if noSDPany==1

     +--------------------------------------------------+
     | xsurvey   EA_ID   noSDPany   noSDPpub   noSDP~12 |
     |--------------------------------------------------|
 32. |    KER2    4182          1          1          1 |
152. |    KER3    4182          1          1          1 |
402. |    KER5    4377          1          1          1 |
491. |    KER5    4907          1          1          1 |
510. |    KER5    4990          1          1          1 |
     |--------------------------------------------------|
553. |    KER6    4377          1          1          1 |
662. |    KER6    4994          1          1          1 |
704. |    KER7    4377          1          1          1 |
     +--------------------------------------------------+

.                 list xsurvey EA_ID noSDP* if noSDPpub==1

     +--------------------------------------------------+
     | xsurvey   EA_ID   noSDPany   noSDPpub   noSDP~12 |
     |--------------------------------------------------|
 32. |    KER2    4182          1          1          1 |
152. |    KER3    4182          1          1          1 |
272. |    KER4    4182          0          1          1 |
402. |    KER5    4377          1          1          1 |
452. |    KER5    4684          0          1          1 |
     |--------------------------------------------------|
491. |    KER5    4907          1          1          1 |
510. |    KER5    4990          1          1          1 |
553. |    KER6    4377          1          1          1 |
575. |    KER6    4502          0          1          1 |
577. |    KER6    4529          0          1          1 |
     |--------------------------------------------------|
662. |    KER6    4994          1          1          1 |
704. |    KER7    4377          1          1          1 |
728. |    KER7    4529          0          1          1 |
     +--------------------------------------------------+

*/	
