* This dofile creates dataset and figures to asess EA-SDP link 
* Such link can be used to create EA-level service environment data using SDP and CEI surveys 
* 		=> save "$data/EAlevel_SDP_`survey'.dta", replace 
* 		This is required to run some of the access variables in "Effective_Access_Indicators_IR.do" 
* Summary dataset describing link quality 
* 		=> save EA_SDP_Link_Summary.dta, replace	

* Table of contents
* A. SETTING
* B. Link EA-SDP-IR  
* 	B.1 Get EA-level information from HHQFQ ===> EA_`survey'.dta
* 	B.2 Reshape SDP to EA-SDP level LONG file ===> EASDP_`survey'.dta
* 	B.3 Create EA-level data which has service environment characterisics based on linked SDPs ===> EAlevel_SDP_`survey'.dta
* C. Create SURVEY-LEVEL summary dataset about link results ===> EA_SDP_Link_Summary.dta	
* D. Analysis/assessment of the link reults 

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
	ETR2 ETR3 ETR4 ETR5 ETR6
	KER2 KER3 KER4 KER5 KER6 KER7 
	NGLagosR2 NGLagosR3 NGLagosR4 NGLagosR5 
	NGKanoR3  NGKanoR4  NGKanoR5  
	UGR2 UGR3 UGR4 UGR5 UGR6
	";
	#delimit cr
	
#delimit;
global datalistminusone " 
	BFR2 BFR3 BFR4 BFR5 BFR6 
	ETR2 ETR3 ETR4 ETR5 ETR6
	KER2 KER3 KER4 KER5 KER6 KER7 
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
			
		keep round region ur EA_ID 
		foreach x of varlist round region ur  {
			rename `x' HHQFQ`x'
			}
		gen xsurvey="`survey'"		
		sort EA_ID
	save "$data/EA_`survey'.dta", replace
}	

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
* B.2 Reshape SDP to EA-SDP level LONG file 
*************************************************************************************************
/*
**** CHECK if SDP has duplicates => NONE confirmed
foreach survey  in $datalist{
	use "$data/SR_`survey'.dta", clear	
	tab xsurvey
	duplicates report
	}

***** CHECK all surveys have "EAserved..."
foreach survey  in $datalist{
	use "$data/SR_`survey'.dta", clear	
	lookfor EAserved
	sum EAserved*
	}
	
***** CHECK all surveys for their facility type classification 
foreach survey  in $datalist{
	use "$data/SR_`survey'.dta", clear	
	tab country xsurvey, m
	tab facility_type managing_authority, m
	codebook facility_type 
	}
*/
	
set more off
foreach survey  in $datalist{
*foreach survey  in BFR6{
	use "$data/SR_`survey'.dta", clear	

	replace country=substr(xsurvey, 1,2)
	
	* SDP characteristics 
	gen SDPall			= 1
	gen byte SDPpub		= managing_authority==1

	gen SDPpub12=0
	gen SDPlow=0

	replace SDPpub12	=1 if country=="BF" & managing_authority==1 & (facility_type>=4 & facility_type<=7) /*primary & secondary*/
	replace SDPlow		=1 if country=="BF" & (facility_type>=4 & facility_type<=7) /*primary & secondary level, both sectors*/
	
	replace SDPpub12	=1 if country=="ET" & managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	replace SDPlow		=1 if country=="ET" & (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/
	
	replace SDPpub12	=1 if country=="KE" & managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	replace SDPlow		=1 if country=="KE" & (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/
	
	replace SDPpub12	=1 if country=="NG" & managing_authority==1 & (facility_type>=2 & facility_type<=4) /*primary & secondary*/
	replace SDPlow		=1 if country=="NG" & (facility_type>=2 & facility_type<=4) /*primary & secondary level, both sectors*/

	replace SDPpub12	=1 if country=="UG" & managing_authority==1 & (facility_type>=3 & facility_type<=5) /*primary & secondary*/
	replace SDPlow		=1 if country=="UG" & (facility_type>=3 & facility_type<=5) /*primary & secondary level, both sectors*/

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

*************************************************************************************************
* B.3 Create EA-level data which has service environment characterisics based on linked SDPs
*************************************************************************************************

set more off
foreach survey  in $datalist{
	use "$data/EASDP_`survey'.dta", clear

*1. MERGE EA_level information from HHQFQ 
	sort EA_ID
	merge EA_ID using "$data/EA_`survey'.dta"
	tab _merge, m 
		/*
		RED FLAG
		check EAs with no SDP (i.e., _merge==2 or round==.) 
		check SDPs with no EA (i.e., _merge==1) 
		This should be none, but not necessarily in reality. WHY???
		*/
			tab xsurvey, m
			tab EA_ID if _merge==1 
			tab EA_ID if _merge==2 
		*keep if _merge==3
		*drop _merge
	save temp.dta, replace
	
*2. CREATE number of SDPs by type - per EA
	use temp.dta, replace
		collapse (sum) SDPall SDPpub SDPpub12 SDPlow , by(xsurvey round ur EA_ID)
		foreach x of varlist SDPall SDPpub SDPpub12 SDPlow {
			rename `x' num_`x'
			lab var num_`x' "Numbe of `x' per EA"
			}
			
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
	
	
set more off
foreach survey  in $datalist{
*foreach survey  in BFR6{
	use "$data/EAlevel_SDP_`survey'.dta", clear
		bysort ur: sum round noSDP* 
		list xsurvey EA_ID noSDP* if noSDPany==1
		*list xsurvey EA_ID noSDP* if noSDPpub==1
		*list xsurvey EA_ID noSDP* if noSDPpub12==1			
	}

	
*************************************************************************************************	
* C. Create dataset summarizing the link results 
*************************************************************************************************

*** Number of EA	
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
		
*** Number of SDP	
use "$data/SR_BFR1.dta", clear
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
	merge xsurvey using EA_SDP_Link_Summary.dta
		tab _merge, m
		keep if _merge==3 /*SHOULD BE ALL*/
		drop _merge
		
	sort xsurvey HHQFQur
	save EA_SDP_Link_Summary.dta, replace

*** Number of EASDP	
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
	
*** Number of average, min, max SDP per EA
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

	
erase temp.dta	
*browse
tab country round, m
OKAY SURVEY-level summary data ready HERE
*/

*************************************************************************************************	
* D. Analysis/assessment of the link reults 
*************************************************************************************************

capture putdocx clear 
putdocx begin

putdocx paragraph
putdocx text ("EA-SDP link assessment"), linebreak bold 
putdocx text (""), linebreak
putdocx text ("1. Basic background information: number of EAs, SDPs, and EA-SDP linked pairs"), linebreak bold  

	use EA_SDP_Link_Summary.dta, clear	
	
putdocx paragraph
putdocx table stable = (1,4), 
putdocx table table = data(xsurvey nEA nSDP nEASDP) 

putdocx paragraph
putdocx text ("2. Number of linked SDPs per EA"), linebreak bold 
putdocx text ("Based on the survey design, each EA would have 3-6 SDPs, though some SDPs may serve multiple EAs. However, not all EAs have three or more SDPs linked to the EA. Some EAs are repeated offenders..."), linebreak
putdocx table stable = (1,5), 
putdocx table table = data(xsurvey noSDPany noSDPpub noSDPpub12 noSDPlow) 
	
	use EA_SDP_Link_Summary.dta, clear	
		
		#delimit; 	
		graph bar num_*, over(round)
			by(country, row(1) title("Average number of linked SDPs per EA")	)
			legend(pos(6) row(1) size(vsmall)
				label(1 "All SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Average number per EA")	
			;
			#delimit cr					
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy

		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png		
		
	use "$data/EAlevel_SDP_BFR1.dta", clear	
			foreach survey  in $datalistminusone{
			append using "$data/EAlevel_SDP_`survey'.dta", force
			}
		
		#delimit; 	
		graph box num_*, over(round)
			by(country, row(1) title("Distribution of number of linked SDPs per EA")	)
			legend(pos(6) row(1) size(small)
				label(1 "All SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Nnumber of linked SDPs per EA")	
			;
			#delimit cr					
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png		
			
	use EA_SDP_Link_Summary.dta, clear		
	
		#delimit; 
		graph bar noSDP*, over(round) 
			by(country, row(1) title("Percent of EAs with no linked SDP by type")	)
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Percent") 
			;
			#delimit cr	
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png		

putdocx paragraph
putdocx text ("3. Further information on EAs with no SDP"), linebreak bold 

putdocx paragraph
putdocx text ("Burkina Faso"), linebreak bold 

	global datalistminusone "BFR2 BFR3 BFR4 BFR5 BFR6 "
	use "$data/EAlevel_SDP_BFR1.dta", clear	
		foreach survey in $datalistminusone{
		append using "$data/EAlevel_SDP_`survey'.dta", force
		}

		tab xsurvey HHQFQround, m
		
		*** No linked SDP
		list xsurvey HHQFQur EA_ID if num_SDPall==0
		*** No linked public SDP
		list xsurvey HHQFQur EA_ID if num_SDPpub==0
		*** No linked public SDP
		*list xsurvey HHQFQur EA_ID if num_SDPpub12==0		
		tab HHQFQregion HHQFQur if num_SDPpub12==0, m	

		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}

		#delimit; 
		graph bar noSDP*, over(xsurvey) 
			by(HHQFQur,
			title("Percent of EAs with no linked SDP: by residential area"))
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Percent") 			
			;
			#delimit cr						
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png					

	putdocx paragraph
	putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
	putdocx table table = data(xsurvey HHQFQur noSDP*) 

putdocx paragraph
putdocx text ("Ethiopia"), linebreak bold 

	global datalistminusone "ETR3 ETR4 ETR5 ETR6"
	use "$data/EAlevel_SDP_ETR2.dta", clear	
		foreach survey in $datalistminusone{
		append using "$data/EAlevel_SDP_`survey'.dta", force
		}

		tab xsurvey HHQFQround, m
		
		*** No linked SDP
		list xsurvey HHQFQregion HHQFQur EA_ID if num_SDPall==0
		*** No linked public SDP
		list xsurvey HHQFQregion HHQFQur EA_ID if num_SDPpub==0
		*** No linked public SDP
		*list xsurvey HHQFQregion HHQFQur EA_ID if num_SDPpub12==0		
		tab HHQFQregion HHQFQur if num_SDPpub12==0, m
		
		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}
			
		#delimit; 
		graph bar noSDP*, over(xsurvey) 
			by(HHQFQur,
			title("Percent of EAs with no linked SDP: by residential area"))
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Percent") 			
			;
			#delimit cr						
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png					
			
	putdocx paragraph
	putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
	putdocx table table = data(xsurvey HHQFQur noSDP*) 

putdocx paragraph
putdocx text ("Kenya"), linebreak bold 

	global datalistminusone "KER3 KER4 KER5 KER6 KER7"
	use "$data/EAlevel_SDP_KER2.dta", clear	
		foreach survey in $datalistminusone{
		append using "$data/EAlevel_SDP_`survey'.dta", force
		}

		tab xsurvey HHQFQround, m
		
		*** No linked SDP
		list xsurvey HHQFQur EA_ID if num_SDPall==0
		*** No linked public SDP
		list xsurvey HHQFQur EA_ID if num_SDPpub==0
		*** No linked public SDP
		*list xsurvey HHQFQur EA_ID if num_SDPpub12==0		
		tab HHQFQregion HHQFQur if num_SDPpub12==0, m	

		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}

		#delimit; 
		graph bar noSDP*, over(xsurvey) 
			by(HHQFQur,
			title("Percent of EAs with no linked SDP: by residential area"))
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Percent") 			
			;
			#delimit cr						
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png					

	putdocx paragraph
	putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
	putdocx table table = data(xsurvey HHQFQur noSDP*) 

putdocx paragraph
putdocx text ("Nigeria: Lagos"), linebreak bold 

	global datalistminusone "NGLagosR3 NGLagosR4 NGLagosR5 "
	use "$data/EAlevel_SDP_NGLagosR2.dta", clear	
		foreach survey in $datalistminusone{
		append using "$data/EAlevel_SDP_`survey'.dta", force
		}

		tab xsurvey HHQFQround, m
		
		*** No linked SDP
		list xsurvey HHQFQur EA_ID if num_SDPall==0
		*** No linked public SDP
		list xsurvey HHQFQur EA_ID if num_SDPpub==0
		*** No linked public SDP
		*list xsurvey HHQFQur EA_ID if num_SDPpub12==0		
		tab HHQFQregion HHQFQur if num_SDPpub12==0, m	

		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}

		#delimit; 
		graph bar noSDP*, over(xsurvey) 
			by(HHQFQur,
			title("Percent of EAs with no linked SDP: by residential area"))
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Percent") 			
			;
			#delimit cr						
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png					

	putdocx paragraph
	putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
	putdocx table table = data(xsurvey HHQFQur noSDP*) 	
	
putdocx paragraph
putdocx text ("Nigeria: Kano"), linebreak bold 

	global datalistminusone "NGKanoR4  NGKanoR5   "
	use "$data/EAlevel_SDP_NGkanoR3.dta", clear	
		foreach survey in $datalistminusone{
		append using "$data/EAlevel_SDP_`survey'.dta", force
		}

		tab xsurvey HHQFQround, m
		
		*** No linked SDP
		list xsurvey HHQFQur EA_ID if num_SDPall==0
		*** No linked public SDP
		list xsurvey HHQFQur EA_ID if num_SDPpub==0
		*** No linked public SDP
		*list xsurvey HHQFQur EA_ID if num_SDPpub12==0		
		tab HHQFQregion HHQFQur if num_SDPpub12==0, m	

		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}

		#delimit; 
		graph bar noSDP*, over(xsurvey) 
			by(HHQFQur,
			title("Percent of EAs with no linked SDP: by residential area"))
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Percent") 			
			;
			#delimit cr						
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png					

	putdocx paragraph
	putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
	putdocx table table = data(xsurvey HHQFQur noSDP*) 		
	
putdocx paragraph
putdocx text ("Uganda"), linebreak bold 
	
	global datalistminusone "UGR3 UGR4 UGR5 UGR6"
	use "$data/EAlevel_SDP_UGR2.dta", clear	
		foreach survey in $datalistminusone{
		append using "$data/EAlevel_SDP_`survey'.dta", force
		}
	
		tab xsurvey HHQFQround, m
		
		*** No linked SDP
		list xsurvey HHQFQregion HHQFQur EA_ID if num_SDPall==0
		*** No linked public SDP
		list xsurvey HHQFQregion HHQFQur EA_ID if num_SDPpub==0
		*** No linked public SDP
		*list xsurvey HHQFQregion HHQFQur EA_ID if num_SDPpub12==0		
		tab HHQFQregion HHQFQur if num_SDPpub12==0, m
		
		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}
			
		#delimit; 
		graph bar noSDP*, over(xsurvey) 
			by(HHQFQur,
			title("Percent of EAs with no linked SDP: by residential area"))
			legend(pos(6) row(1) size(small)
				label(1 "Any SDPs")
				label(2 "Public")
				label(3 "Public" "primary/secondary") 
				label(4 "Any" "primary/secondary") )
			ytitle("Percent") 			
			;
			#delimit cr						
		gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[1].style.editstyle linestyle(color(white)) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx image graph.png					
			
	putdocx paragraph
	putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
	putdocx table table = data(xsurvey HHQFQur noSDP*) 

putdocx save Access_Link_Assessment_$date.docx, replace	
				
erase graph.png

*************************************************************************************************	
* D. Create EA-level appended dataset to check distributions 
*************************************************************************************************	

use "$data/EAlevel_SDP_BFR1.dta", clear			
		
	foreach survey  in $datalistminusone{
	append using "$data/EAlevel_SDP_`survey'.dta"
	
	}	
	
save EAlevel_SDP_AllSurveys.dta, replace	

use EAlevel_SDP_AllSurveys.dta, clear


	***** DROP surveys with problematic EA-SDP link 
	drop if country=="NG"
	drop if (xsurvey=="ETR6"| xsurvey=="BFR6")
		
	foreach var of varlist SDPall_essential5_offer SDPall_essential5_curav SDPall_essential5_noso SDPall_essential5_ready SDPall_essential5_rnoso{
		replace `var'=1 if `var'>=1
		}

	***** EA_level variation in MCPR
	graph box mcp, over(round) by(country, row(1))
 	
	***** % EAs "with choice"  
	graph bar SDPall_essential5_offer SDPall_essential5_curav SDPall_essential5_noso SDPall_essential5_rnoso, over(round) by(country, row(1))
	
	***** MCPR by EA-level choice 
	
	
		gen mcp_wochoice=mcp
		gen mcp_wchoice=mcp

capture putdocx clear 
putdocx begin
		
		foreach var of varlist SDPall_essential5_offer SDPall_essential5_curav SDPall_essential5_noso SDPall_essential5_rnoso {
			replace mcp_wochoice=mcp
			replace mcp_wchoice=mcp
			replace mcp_wochoice=. if `var'==1
			replace mcp_wchoice=. if `var'==0
			
		#delimit; 
		graph box mcp_*, over(round) 
			by(country, row(1) title("EA-level MCPR by access to `var'", size(small) ) note("") ) 
			legend(
				label(1 "EAs without access")
				label(2 "EAs with access")	
				size(small) )
			box(1, bcolor(cranberry)) 
			box(2, bcolor(navy)) 
			marker(1, mcolor(cranberry) msize(vsmall)) 
			marker(2, mcolor(navy) msize(vsmall)) 
			ytitle("EA-level MCPR", size(small) )
			;
			#delimit cr 						
			gr_edit .plotregion1.subtitle[4].style.editstyle fillcolor(white) editcopy
			gr_edit .plotregion1.subtitle[4].style.editstyle linestyle(color(white)) editcopy
			gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy

			graph export graph.png, replace	
			putdocx paragraph
			putdocx image graph.png		
			
			}
				
putdocx save MCPR_ByAccess_$date.docx, replace	
