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

local country "Uganda"
local today=c(current_date)
local c_today= "`today'"
local date=subinstr("`c_today'", " ", "",.)

#delimit;
global datalist " 
	UGR2 UGR3 UGR4 UGR5 UGR6
	";
	#delimit cr
	
#delimit;
global datalistminusone " 
	UGR3 UGR4 UGR5 UGR6
	";
	#delimit cr
	
************************************************************************
* A.2 Non-public data for consultancy
************************************************************************

* N/A for Uganda
	
*********************************************************
* B. PREP EA-level data  
*********************************************************	

*****0. getting EA-level information from HHQFQ ready (to check linkage - optional, but just in case)
 
set more off
foreach survey  in $datalist{
	use "$data/HR_`survey'.dta", clear
	sort EA_ID
	keep if EA_ID!=EA_ID[_n-1]
	keep round region ur EA_ID 
	foreach x of varlist round region ur  {
		rename `x' HHQFQ`x'
		}
	gen xsurvey="`survey'"		
	sort EA_ID
	save "$data/EA_`survey'.dta", replace
	}

*********************************************************
* D. EA-SDP level data by RESHAPE
*********************************************************	

/*
https://en.wikipedia.org/wiki/Healthcare_in_Uganda

The lowest rung of the district-based health system consists of Village Health Teams (VHTs). 
These are volunteer community health workers who deliver predominantly health education, 
preventive services, and simple curative services in communities. They constitute level 1 
health services. The next level is Health Center II, which is an out patient service run by a nurse. 
It is intended to serve 5000 people. Next in level is Health Center III (HCIII) which serves 10,000 people 
and provides in addition to HC II services, in patient, simple diagnostic, and maternal health services. 
It is managed by a clinical officer. Above HC III is the Health Center IV, run by a medical doctor and 
providing surgical services in addition to all the services provided at HC III. HC IV is also intended 
to provide blood transfusion services and comprehensive emergency obstetric care.[4]
*/		
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
	gen byte SDPpub12	= managing_authority==1 & (facility_type>=3 & facility_type<=5) /*primary & secondary*/
	gen byte SDPlow		= (facility_type>=3 & facility_type<=5) /*primary & secondary level, both sectors*/
			
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
	use "$data/EASDP_`survey'.dta", clear

	sort EA_ID
	merge EA_ID using "$data/EA_`survey'.dta"
	tab _merge, m /*check EAs with no SDP. This should be none, but not necessarily in reality*/
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
		save "$data/EAlevel_SDP_`survey'.dta", replace
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
use "$data/EA_UGR2.dta", clear			
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
use "$data/SR_UGR2.dta", clear
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
use "$data/EASDP_UGR2.dta", clear			
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
use "$data/EAlevel_SDP_UGR2.dta", clear	
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
use "$data/Access_UGR2.dta", clear	
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
	
use "$data/Access_UGR2.dta", clear	
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
use "$data/SR_UGR2.dta", clear	

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

use "$data/SR_UGR2.dta", clear	

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
	
use "$data/SR_UGR2.dta", clear	

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


capture putdocx clear 
putdocx begin

putdocx paragraph
putdocx text ("Appendix 1: Distribution of SDPs by sector and type"), linebreak bold 
		
	set more off
	use "$data/SR_UGR2.dta", clear	
	foreach survey  in $datalistminusone{
	append using "$data/SR_`survey'.dta", force
		}	
	
		tab xsurvey
		tab facility_type sector, m
		/*
						   |        sector
		  Type of facility |    Public    Private |     Total
		-------------------+----------------------+----------
			   1. hospital |       169         60 |       229 
		2. health_center_4 |       266          6 |       272 
		3. health_center_3 |       366         33 |       399 
		4. health_center_2 |       318         17 |       335 
		  5. health_clinic |         1        139 |       140 
			   6. pharmacy |         0          7 |         7 
				7. chemist |         0        360 |       360 
				 96. other |         0          7 |         7 
		-------------------+----------------------+----------
					 Total |     1,120        629 |     1,749 
		
		*/
		gen pub3	=1 if sector==0 & facility_type==1
		gen pub2	=1 if sector==0 & facility_type==2
		gen pub1	=1 if sector==0 & (facility_type>=3 & facility_type<=4)
		gen puboth	=1 if sector==0 & facility_type>4
			
		gen prv3	=1 if sector==1 & facility_type==1
		gen prv2	=1 if sector==1 & facility_type==2
		gen prv1	=1 if sector==1 & (facility_type>=3 & facility_type<=4)
		gen prvclinic  =1 if sector==1 & facility_type==5	
		gen prvoth  =1 if sector==1 & facility_type>5
		
		collapse (count) pub* prv*, by(xsurvey)

		graph bar pub* prv*, over(xsurvey) stack 	
		
		#delimit; 
		graph bar pub* prv*, over(xsurvey) stack percent
			legend(pos(3) col(1) order (9 8 7 6 5 4 3 2 1) size(small)
				label(1 "public" "hospitals")
				label(2 "public" "health center IV")
				label(3 "public" "health center II or III") 
				label(4 "public" "pharmacy/other")
				label(5 "private" "hospitals")
				label(6 "private" "health center IV")
				label(7 "private" "health center II or III") 
				label(8 "private" "clinic") 
				label(9 "private" "pharmacy/other")
				)
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(navy*0.2)) 
			bar(5, color(cranberry*0.8))  
			bar(6, color(cranberry*0.6))  
			bar(7, color(cranberry*0.4))  
			bar(8, color(cranberry*0.2))  	
			bar(9, color(orange*0.2))  	
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

use "$data/EAlevel_SDP_UGR2.dta", clear	
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

*** further investigation of EAs with no linked SDP 

use "$data/EAlevel_SDP_UGR2.dta", clear	
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

.                 list xsurvey EA_ID noSDP* if noSDPpub==1

     +--------------------------------------------------+
     | xsurvey   EA_ID   noSDPany   noSDPpub   noSDP~12 |
     |--------------------------------------------------|
224. |    UGR4    3021          0          1          1 |
395. |    UGR5    3639          0          1          1 |
505. |    UGR6    3639          0          1          1 |
     +--------------------------------------------------+

.                 list xsurvey EA_ID noSDP* if noSDPpub12==1              

     +--------------------------------------------------+
     | xsurvey   EA_ID   noSDPany   noSDPpub   noSDP~12 |
     |--------------------------------------------------|
 24. |    UGR2    3223          0          0          1 |
 36. |    UGR2    3298          0          0          1 |
 44. |    UGR2    3366          0          0          1 |
 59. |    UGR2    3524          0          0          1 |
 69. |    UGR2    3600          0          0          1 |
     |--------------------------------------------------|
 98. |    UGR2    3890          0          0          1 |
105. |    UGR2    3964          0          0          1 |
112. |    UGR3    3010          0          0          1 |
134. |    UGR3    3223          0          0          1 |
146. |    UGR3    3298          0          0          1 |
     |--------------------------------------------------|
154. |    UGR3    3366          0          0          1 |
169. |    UGR3    3524          0          0          1 |
179. |    UGR3    3600          0          0          1 |
186. |    UGR3    3663          0          0          1 |
208. |    UGR3    3890          0          0          1 |
     |--------------------------------------------------|
215. |    UGR3    3964          0          0          1 |
224. |    UGR4    3021          0          1          1 |
244. |    UGR4    3223          0          0          1 |
256. |    UGR4    3298          0          0          1 |
264. |    UGR4    3366          0          0          1 |
     |--------------------------------------------------|
269. |    UGR4    3413          0          0          1 |
279. |    UGR4    3524          0          0          1 |
289. |    UGR4    3600          0          0          1 |
318. |    UGR4    3890          0          0          1 |
325. |    UGR4    3964          0          0          1 |
     |--------------------------------------------------|
349. |    UGR5    3167          0          0          1 |
372. |    UGR5    3403          0          0          1 |
395. |    UGR5    3639          0          1          1 |
400. |    UGR5    3655          0          0          1 |
411. |    UGR5    3731          0          0          1 |
     |--------------------------------------------------|
425. |    UGR5    3858          0          0          1 |
428. |    UGR5    3874          0          0          1 |
431. |    UGR5    3909          0          0          1 |
439. |    UGR5    3974          0          0          1 |
440. |    UGR5    3978          0          0          1 |
     |--------------------------------------------------|
451. |    UGR6    3061          0          0          1 |
459. |    UGR6    3167          0          0          1 |
482. |    UGR6    3403          0          0          1 |
505. |    UGR6    3639          0          1          1 |
510. |    UGR6    3655          0          0          1 |
     |--------------------------------------------------|
521. |    UGR6    3731          0          0          1 |
534. |    UGR6    3853          0          0          1 |
535. |    UGR6    3858          0          0          1 |
538. |    UGR6    3874          0          0          1 |
550. |    UGR6    3978          0          0          1 |
     +--------------------------------------------------+

. 
end of do-file

. 
*/
