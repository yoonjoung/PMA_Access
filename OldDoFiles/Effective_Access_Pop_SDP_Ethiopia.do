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

local country "Ethiopia"
local today=c(current_date)
local c_today= "`today'"
local date=subinstr("`c_today'", " ", "",.)

#delimit;
global datalist " 
	ETR2 ETR3 ETR4 ETR5 ETR6
	";
	#delimit cr
	
#delimit;
global datalistminusone " 
	ETR3 ETR4 ETR5 ETR6
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
***** Analysis and construct a summary dataset 
*********************************************************	

*** Number of EA	
use "$data/EA_ETR2.dta", clear			
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
use "$data/SR_ETR2.dta", clear
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
use "$data/EASDP_ETR2.dta", clear			
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
use "$data/EAlevel_SDP_ETR2.dta", clear	
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
use "$data/Access_ETR2.dta", clear	
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
	
use "$data/Access_ETR2.dta", clear	
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
use "$data/SR_ETR2.dta", clear	

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

use "$data/SR_ETR2.dta", clear	

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
	
use "$data/SR_ETR2.dta", clear	

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

putdocx paragraph
putdocx text ("Appendix 1: Distribution of SDPs by sector and type"), linebreak bold 
		
	set more off
	use "$data/SR_ETR2.dta", clear	
	foreach survey  in $datalistminusone{
	append using "$data/SR_`survey'.dta", force
		}	
	
		tab xsurvey
		tab facility_type sector, m
		/*
							  |        sector
			 Type of facility |    Public    Private |     Total
		----------------------+----------------------+----------
		1. Hospital/ Polyclin |       461          4 |       465 
			 2. Health center |       992          0 |       992 
			   3. Health post |       471          0 |       471 
			 4. Health clinic |         0        147 |       147 
				  5. Pharmacy |         0         97 |        97 
			 6. Retail outlet |         0         25 |        25 
					96. Other |         4          4 |         8 
		----------------------+----------------------+----------
						Total |     1,928        277 |     2,205 
		*/
		
		gen pub3	=1 if sector==0 & facility_type==1
		gen pub2	=1 if sector==0 & facility_type==2
		gen pub1	=1 if sector==0 & facility_type==3
		gen puboth	=1 if sector==0 & facility_type>3
			
		gen prv3	=1 if sector==1 & facility_type==1
		*gen prv2	=1 if sector==1 & (facility_type==2|facility_type==3)
		gen prv1	=1 if sector==1 & facility_type==4
		gen prvpharm	=1 if sector==1 & facility_type==5
		gen prvretail	=1 if sector==1 & facility_type==6
		gen prvoth  	=1 if sector==1 & facility_type>6	
		
		collapse (count) pub* prv*, by(xsurvey)

		graph bar pub* prv*, over(xsurvey) stack 	
		
		#delimit; 
		graph bar pub* prv*, over(xsurvey) stack percent
			legend(pos(3) col(1) order (9 8 7 6 5 4 3 2 1) size(small)
				label(1 "public" "hospitals")
				label(2 "public" "health center")
				label(3 "public" "health post") 
				label(4 "public" "other")
				label(5 "private" "hospitals")
				label(6 "private" "clinic")
				label(7 "private" "pharmacy")
				label(8 "private" "retail") 
				label(9 "private" "other")
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

use "$data/EAlevel_SDP_ETR2.dta", clear	
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

use "$data/EAlevel_SDP_ETR2.dta", clear	
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
  45. |    ETR2    2214          1          1          1 |
 187. |    ETR2    2950          1          1          1 |
 391. |    ETR3    2869          1          1          1 |
 425. |    ETR4    2011          1          1          1 |
 639. |    ETR4    2987          1          1          1 |
      |--------------------------------------------------|
 740. |    ETR5    2448          1          1          1 |
 779. |    ETR5    2613          1          1          1 |
 794. |    ETR5    2677          1          1          1 |
 878. |    ETR6    2066          1          1          1 |
 883. |    ETR6    2098          1          1          1 |
      |--------------------------------------------------|
 964. |    ETR6    2448          1          1          1 |
      +--------------------------------------------------+

.                 list xsurvey EA_ID noSDP* if noSDPpub==1

      +--------------------------------------------------+
      | xsurvey   EA_ID   noSDPany   noSDPpub   noSDP~12 |
      |--------------------------------------------------|
  45. |    ETR2    2214          1          1          1 |
 121. |    ETR2    2623          0          1          1 |
 159. |    ETR2    2808          0          1          1 |
 169. |    ETR2    2869          0          1          1 |
 174. |    ETR2    2885          0          1          1 |
      |--------------------------------------------------|
 187. |    ETR2    2950          1          1          1 |
 339. |    ETR3    2623          0          1          1 |
 391. |    ETR3    2869          1          1          1 |
 395. |    ETR3    2885          0          1          1 |
 408. |    ETR3    2950          0          1          1 |
      |--------------------------------------------------|
 425. |    ETR4    2011          1          1          1 |
 562. |    ETR4    2623          0          1          1 |
 603. |    ETR4    2808          0          1          1 |
 639. |    ETR4    2987          1          1          1 |
 676. |    ETR5    2156          0          1          1 |
      |--------------------------------------------------|
 740. |    ETR5    2448          1          1          1 |
 779. |    ETR5    2613          1          1          1 |
 794. |    ETR5    2677          1          1          1 |
 878. |    ETR6    2066          1          1          1 |
 883. |    ETR6    2098          1          1          1 |
      |--------------------------------------------------|
 898. |    ETR6    2156          0          1          1 |
 964. |    ETR6    2448          1          1          1 |
      +--------------------------------------------------+

		