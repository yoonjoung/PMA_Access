* Explore potential indicators for "Access" 
* INDIVIDUAL level
* Kenya & Uganda as examples
* Refer to the mapping doc: "Mapping_PMAQuestionsForAccess_2020 02 25.doc"  

* PRIOR TO THIS DO FILE, the following has to be in place
		/*
		***** create RECODE file using public/raw data
		cd "C:\Users\YoonJoung Choi\Dropbox\0 Data\PMA\"
		do createPMA_HRPRIR.do
		do createPMA_SR_BurkinaFaso.do
		do createPMA_SR_Ethiopia.do
		do createPMA_SR_Kenya.do
		do createPMA_SR_Nigeria.do
		do createPMA_SR_Uganda.do 

		***** create EA-level SDP linked data
		cd "C:\Users\YoonJoung Choi\Dropbox\0 iSquared\iSquared_PMA\Effective Access\"
		do Effective_Access_Pop_SDP_Link.do
		*/

* Table of Contents
* 	A. SETTING 
*		(NOTE: earlier surveys do not have "ever heard of" questions) 
* 	B. PREP create women-level access variables  
*		1. Psycosocial 
*		2. Cognitive
*		3. Geographic accessibility  
*		4. Service quality 
*		5. Administrative: N/A for women data
*		6. Affordability 	
*		7. BASIC var
*		8. MERGE with the EA-SDP data, created from "Effective_Access_Pop_SDP_`counry'.do". 
*			ONLY USING PUBLIC DATA, thus set $surveylistEASDPLINK under SETTING 
* 	C. Create summary dataset 
* 	D. ANALYSIS using the summary dataset 

clear
clear matrix
clear mata
capture log close
set more off
numlabel, add

set scheme s1color

************************************************************************
* A. SETTING 
************************************************************************

cd "C:\Users\YoonJoung Choi\Dropbox\0 iSquared\iSquared_PMA\Effective Access\"
global data "C:\Users\YoonJoung Choi\Dropbox\0 Data\PMA\"

local today=c(current_date)
local c_today= "`today'"
global date=subinstr("`c_today'", " ", "",.)
	
#delimit;
global surveylist " 
	BFR2 BFR3 BFR4 BFR5 BFR6 BFR7 
	KER3 KER4 KER5 KER6 KER7 KER8 
	NGLagosR3 NGLagosR4 NGLagosR5 NGLagosR6 
	NGKanoR3  NGKanoR4  NGKanoR5  NGKanoR6 
	UGR3 UGR4 UGR5 UGR6 
	ETR4 ETR5 ETR6 
	";
	#delimit cr
	
#delimit;
global surveylistminusone " 
	BFR3 BFR4 BFR5 BFR6 BFR7 
	KER3 KER4 KER5 KER6 KER7 KER8 
	NGLagosR3 NGLagosR4 NGLagosR5 NGLagosR6 
	NGKanoR3  NGKanoR4  NGKanoR5  NGKanoR6 
	UGR3 UGR4 UGR5 UGR6 
	ETR4 ETR5 ETR6 
	";
	#delimit cr

#delimit;
global surveylistEASDPLINK " 
	BFR2 BFR3 BFR4 BFR5 BFR6  
	KER3 KER4 KER5 KER6 KER7  
	NGLagosR3 NGLagosR4 NGLagosR5 
	NGKanoR3  NGKanoR4  NGKanoR5  
	UGR3 UGR4 UGR5 UGR6 
	ETR4 ETR5 ETR6 
	";
	#delimit cr
	
************************************************************************
* B. PREP women-level access variables  
************************************************************************

*****1. Psycosocial 
	
*****2. Cognitive

* check variables 
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round mcp heard_* FQweight

	}
 
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	
		foreach var of varlist heard_* {
		replace `var'=. if `var'<0
		}
		egen xheard_sum=rowtotal(heard_*)
		egen xheard_sum_t=rowtotal(heard_rhythm heard_rhythm heard_withdrawal heard_other)
		gen xheard_sum_m=xheard_sum - xheard_sum_t
		
		*histogram xheard_sum_m, w(1)
		sum xheard_sum_m, detail
		*return list
		local med: display %5.3f r(p50)	
		gen xheard_sum_m_med=`med'	
		
		gen byte xheard_10=xheard_sum_m>=10
		gen byte xheard_7 =xheard_sum_m>=7
		gen byte xheard_5 =xheard_sum_m>=5
		gen byte xheard_med =xheard_sum_m>=xheard_sum_m_med
		
		egen temp=rowtotal(heard_IUD heard_implant heard_pill heard_male_condom heard_inj)
		gen byte xheard_select5=temp==5
			drop temp
		egen temp=rowtotal(heard_IUD heard_implant heard_pill heard_male_condom heard_inj heard_emergency)
		gen byte xheard_select6=temp==6
			drop temp		
		
				
		lab var xheard_sum "total number of methods she ever heard of, any" 
		lab var xheard_sum_t "total number of methods she ever heard of, traditional" 
		lab var xheard_sum_m "total number of methods she ever heard of, modern" 
		
		lab var xheard_10 "ever heard of 10 or more modern methods"
		lab var xheard_7 "ever heard of 7 or more modern methods"
		lab var xheard_5 "ever heard of 5 or more modern methods"
		lab var xheard_med "number of ever-heard-methods >= median"
				
		lab var xheard_select5 "heard of all of the select 5 methods: IUD, implant, inj, pills, male condom" 
		lab var xheard_select6 "heard of all of the select 6 methods: IUD, implant, inj, pills, male condom, EC" 
		
	sum round mcp heard_* xheard_*

save IR_`survey'_Access_Indicators.dta, replace 	
}	

*****3. Geographic accessibility  

*****4. Service quality (IR variables)

* check variables 
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round fp_side_* fp_told_*
	*sum round implant_protect implant_duration implant_duration_value told_removal 
	}

foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round LAM*
	}
	
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round 
	lookfor lam
	}	
 
set more off
foreach survey in $surveylist{
	use IR_`survey'_Access_Indicators.dta, clear
	
			/*fp_told_future_switch only in latest survey*/
			capture confirm variable fp_told_future_switch
			if !_rc {
			} 
			else{
				gen fp_told_future_switch=.
				}

		foreach var of varlist fp_side_effects fp_side_effects_instructions fp_told_other_methods fp_told_future_switch {
		replace `var'=. if `var'<0
		}
		
		gen byte xmii_side		=fp_side_effects==1
		gen byte xmii_sidewhat	=fp_side_effects_instructions==1
		gen byte xmii_other		=fp_told_other==1
		gen byte xmii_switch	=fp_told_future_switch==1
		
		egen xmii_sum=rowtotal(xmii_*)
		gen byte xmii3=xmii_sum>=3 & xmii_sum!=.
		gen byte xmii4=xmii_sum>=4 & xmii_sum!=.
			replace xmii4=. if xmii_switch==.
				
		foreach var of varlist xmii*{
			replace `var'=. if mcp!=1
			replace `var'=. if fp_side_effects==. 
			*replace `var'=. if mcp==1 & lamonly==1 /*lamonly available only in recent surveys*/
			}
		
		lab var xmii_side 		"MII: told about side effect"
		lab var xmii_sidewhat 	"MII: told about what to do for side effect "
		lab var xmii_other 		"MII: told about other methods"
		lab var xmii_switch 	"MII: told about switching" 
		lab var xmii_sum 	"number of MII items" 
		lab var xmii3 		"told about all three MII elements"
		lab var xmii4 		"told about all three MII elements plus switching"
	
	sum round xmii* 
	
	/*
		egen ximplant_mii_sum=rowtotal(implant_protect told_removal )
		gen byte ximplant_mii2=ximplant_mii_sum>=2 & ximplant_mii_sum!=.
		
		foreach var of varlist ximplant_mii*{
			replace `var'=. if implant!=1
			}
				
		lab var ximplant_mii_sum "number of implant specific information items" 
		lab var ximplant_mii2 "told about both implant specific information items" 
	
	sum round xmii* ximplant_mii*
	*/
		
save IR_`survey'_Access_Indicators.dta, replace 	
}	

*****5. Administrative: N/A for women data

*****6. Affordability 	

* check variables 
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round 
	lookfor insurance 
	}

set more off
foreach survey in $surveylist{
	use IR_`survey'_Access_Indicators.dta, clear
	
		gen byte xinsurance	=. 
			
			/*fp_told_future_switch only in latest survey*/
			capture confirm variable have_insurance 
			if !_rc {
			replace xinsurance=0 if have_insurance==0
			replace xinsurance=1 if have_insurance==1
			}	
		lab var xinsurance "have health insurance" 
		
	sum round xinsurance
	
save IR_`survey'_Access_Indicators.dta, replace 	
}

*****7. BASIC var

* check variables 
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round  wealthquintile ur school FQweight
	tab school, m
	}
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round
	d EA_ID strata
	}
		
set more off
foreach survey in $surveylist{
	use IR_`survey'_Access_Indicators.dta, clear
	
		foreach var of varlist wealthquintile ur school {
		replace `var'=. if `var'<0
		}	
		
		gen byte xwealth5=wealthquintile
		gen byte xtop3=wealthquintile>=3 & wealthquintile!=.
		gen byte xurban=ur==1
		gen byte xedu_never	=school==0
		gen byte xedu_pri	=school==1
		gen byte xedu_sec	=school>=2 & school!=.
		gen byte xedu3=school
			replace xedu3=2 if school>=2 & school!=.

		gen temp = dofc(FQdoi_correctedSIF)
		format %td temp
		gen tempmonth = month(temp)
		gen tempyear = year(temp)
		gen tempcmc 	= 12*(tempyear - 1900) + tempmonth
		
		egen cmc = median(tempcmc)
		egen year= mode(tempyear)
	
		lab var xtop3 "poor (bottom two quintiles) vs. non-poor (1)"
		lab var xurban "rural vs. urban (1) "
		lab var xedu_sec "ever attended: none or primary vs. secondary or higher+ (1)"
		
		lab var cmc "Interviw date in CMC, median per survey round"
		lab var year "Interviw year, mode per survey round"
						
	keep FQmetainstance FQweight EA_ID strata round year cmc mcp x* 
	
save IR_`survey'_Access_Indicators.dta, replace 	
}


*****8. MERGE with the EA-SDP data, created from "Effective_Access_Pop_SDP_`counry'.do"

/*
SINCE Kenya SDP-EA link is problematic, use only Uganda data
Still check the number/% of EAs with no linked SDP information
*/

set more off
foreach survey  in $surveylistEASDPLINK{
	use "$data/EAlevel_SDP_`survey'.dta", clear
	tab num_SDPpub xsurvey 
	tab num_SDPpub12 xsurvey 
	sort EA_ID
	save "$data/EAlevel_SDP_`survey'.dta", replace
}

set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators.dta, clear

	sort EA_ID
	merge EA_ID using "$data/EAlevel_SDP_`survey'.dta",
	tab _merge, m /*check EAs with no SDP. This should be none, but not necessarily in reality*/
		*keep if _merge==3
		*drop _merge
		
		drop SDPlow* /*low level regardless of sector*/
		*drop SDPpub12_essential4* SDPall_essential4* SDPpub_essential4*
		sum SDP*
		
		foreach var of varlist SDP*{
			replace `var'=1 if `var'>=1 & `var'!=.
			*replace `var'=1 if `var'>=1
		}
			
		lab var SDPall_essential5_offer 	"EA-linked any SDP(s) offers `essential' specific five methods"
		lab var SDPall_essential5_curav 	"EA-linked any SDP(s) currently has `essential' specific five methods"
		lab var SDPall_essential5_noso 	"EA-linked any SDP(s) currently has (with NOSO) `essential' specific five methods"
		lab var SDPall_essential5_ready 	"EA-linked any SDP(s) currently has (with readiness**) `essential' specific five methods"	
		
		lab var SDPpub_essential5_offer 	"EA-linked public SDP(s) offers `essential' specific five methods"
		lab var SDPpub_essential5_curav 	"EA-linked public SDP(s) currently has `essential' specific five methods"
		lab var SDPpub_essential5_noso 	"EA-linked public SDP(s) currently has (with NOSO) `essential' specific five methods"
		lab var SDPpub_essential5_ready 	"EA-linked public SDP(s) currently has (with readiness**) `essential' specific five methods"		
		
		lab var SDPpub12_essential5_offer 	"EA-linked public primary/secondary SDP(s) offers `essential' specific five methods"
		lab var SDPpub12_essential5_curav 	"EA-linked public primary/secondary SDP(s) currently has `essential' specific five methods"
		lab var SDPpub12_essential5_noso 	"EA-linked public primary/secondary SDP(s) currently has (with NOSO) `essential' specific five methods"
		lab var SDPpub12_essential5_ready 	"EA-linked public primary/secondary SDP(s) currently has (with readiness**) `essential' specific five methods"			
	
	
	save IR_`survey'_Access_Indicators.dta, replace
	}	

set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators.dta, clear
	sum round SDPall_essential5_noso SDPall_essential5ec_noso SDPall_essential5_curav SDPall_essential5ec_curav
	sum round SDPpub_essential5_noso SDPpub_essential5ec_noso SDPpub_essential5_curav SDPpub_essential5ec_curav
	sum round SDPpub12_essential5_noso SDPpub12_essential5ec_noso SDPpub12_essential5_curav SDPpub12_essential5ec_curav
	}
	
	pwcorr SDPall_essential5_noso SDPall_essential5ec_noso SDPall_essential5_curav SDPall_essential5ec_curav, sig
	pwcorr SDPpub_essential5_noso SDPpub_essential5ec_noso SDPpub_essential5_curav SDPpub_essential5ec_curav, sig
	pwcorr SDPpub12_essential5_noso SDPpub12_essential5ec_noso SDPpub12_essential5_curav SDPpub12_essential5ec_curav, sig

*OKAY DATA READY FOR ANALYSIS 
*/

	
************************************************************************
* C. Create summary dataset 
************************************************************************

#delimit; 
global indicatorlist "
	xheard_10
	xheard_7
	xheard_5
	xheard_select6
	xheard_select5
	xinsurance
	SDPall_essential5_rnoso
	SDPall_essential5_noso 
	SDPall_essential5_ready
	SDPall_essential5ec_rnoso
	SDPall_essential5ec_noso 		
	SDPall_essential5ec_ready	
	";
	#delimit cr
	
#delimit; 
global indicatorlistall "
	xheard_10
	xheard_7
	xheard_5
	xheard_select6	
	xheard_select5
	xinsurance
	SDPall_essential5_rnoso
	SDPall_essential5_noso 
	SDPall_essential5_ready
	SDPall_essential5ec_rnoso
	SDPall_essential5ec_noso 		
	SDPall_essential5ec_ready
	xmii_side
	xmii_sidewhat
	xmii_other
	xmii_switch
	xmii3
	xmii4
	";
	#delimit cr	

global covlist "xtop3 xurban xedu_sec"


use IR_BFR2_Access_Indicators.dta, clear
foreach survey in $surveylistminusone{
	append using IR_`survey'_Access_Indicators.dta, force
	}
	gen obs=1
save temp.dta, replace

/*
tab xsurvey SDPall_essential5_noso, m 
tab xsurvey SDPall_essential5_ready, m 
tab xsurvey SDPall_essential5_rnoso, m 

foreach survey in $surveylist{
	use "$data/SR_`survey'.dta", clear
	sum round essential5_noso essential5_ready essential5_rnoso
}

foreach survey in $surveylistEASDPLINK{
	use "$data/EAlevel_SDP_`survey'.dta", clear
	sum round SDPall_essential5_noso SDPall_essential5_ready SDPall_essential5_rnoso	
}	
*/

***** Weighted eestimates 

	use temp.dta, clear
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round year cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}			
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR.dta, replace 	
	
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round year cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round year cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
	}
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round year cmc)
			foreach var of varlist mcp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round year cmc)
			foreach var of varlist mcp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR.dta, replace 		
	}	

***** Unweighted number of observation

	use temp.dta, clear
		collapse (count) obs , by(xsurvey round year cmc)
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR_obs.dta, replace 	
	
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse (count) obs , by(xsurvey round year cmc)
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse (count) obs , by(xsurvey round year cmc)
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
	}
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse (count) obs , by(xsurvey round year cmc)
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse (count) obs , by(xsurvey round year cmc)
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_obs.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_obs.dta, replace 		
	}	

***** Merge weighted estimates and unweighted number of observations 

use summary_Access_Indicators_IR_obs.dta, clear
	sort xsurvey group grouplabel	
	merge xsurvey group grouplabel	using summary_Access_Indicators_IR.dta
		tab _merge
		drop _merge

***** Further variables and data cleaning 

		lab	var	SDPall_essential5_noso	"SDPall_essential5_noso"
		lab	var	SDPall_essential5_ready	"SDPall_essential5_ready"
		lab	var	SDPall_essential5_rnoso	"SDPall_essential5_rnoso"
		lab	var	SDPall_essential5ec_noso	"SDPall_essential5ec_noso"
		lab	var	SDPall_essential5ec_ready	"SDPall_essential5ec_ready"
		lab	var	SDPall_essential5ec_rnoso	"SDPall_essential5ec_rnoso"
	
	    replace grouplabel="less than secondary" 	if group=="By xedu_sec" & grouplabel=="No"
        replace grouplabel="secondary or more" 		if group=="By xedu_sec" & grouplabel=="Yes" 
        replace grouplabel="Bottom 2 quintiles"	if group=="By xtop3" & grouplabel=="No" 
        replace grouplabel="Top 3 quintiles"	if group=="By xtop3" & grouplabel=="Yes" 
        replace grouplabel="Rural"	if group=="By xurban" & grouplabel=="No"
        replace grouplabel="Urban" 	if group=="By xurban" & grouplabel=="Yes" 
		
		replace group="Education" if group=="By xedu_sec" 
		replace group="HH wealth" if group=="By xtop3"
		replace group="Residential area" if group=="By xurban"
	
	gen country=substr(xsurvey, 1, length(xsurvey)-2)
		replace country="Burkina Faso" if country=="BF"
		replace country="Ethiopia" if country=="ET"
		replace country="Kenya" if country=="KE"
		replace country="Nigeria, Kano" if country=="NGKano"
		replace country="Nigeria, Lagos" if country=="NGLagos"
		replace country="Uganda" if country=="UG"	

	egen temp=max(round), by(country) 
	gen latestIR=round==temp
		drop temp
	gen byte phase=year>=2019 
	gen month= cmc-12*(year - 1900)
	
	foreach var of varlist xinsurance xmii_switch xmii4{	
		replace `var'=. if phase==0
		}
		
	*PROBLEMATIC surveys with EA_SDP link
	foreach var of varlist SDPall* {
		replace `var'=. if (country=="Nigeria, Kano" | country=="Nigeria, Lagos" | xsurvey=="ETR6"| xsurvey=="BFR6")
		}		
				
	* CHECK small n : SHOULD NOT BE an issue in IR , just a process
	sum obs	
	list xsurvey group grouplabel obs if obs<=20
	foreach var of varlist xheard_10 - xmii4 {
		replace `var'=. if obs<=20
		}
	
	sort xsurvey group grouplabel
	save summary_Access_Indicators_IR.dta, replace 	

	export delimited using summary_Access_Indicators_IR.csv, replace
		
erase temp.dta		
erase summary_Access_Indicators_IR_obs.dta

OKAY Summary DATA READY FOR ANALYSIS and Shiny App

*/



************************************************************************
* D. ANALYSIS using the summary dataset 
************************************************************************
		
capture putdocx clear 
putdocx begin

putdocx paragraph
putdocx text ("Potential access variables"), linebreak bold 
putdocx text ("(Updated on $date)"), linebreak 
putdocx text (""), linebreak
putdocx text ("1. Access variables using female survey data"), linebreak bold 
putdocx text ("(Note: three indicators are available only in new/2.0 surveys)"), linebreak
putdocx text ("1.1 Cognitive accessibility (among all women)"), linebreak bold
putdocx text ("--xheard_10       : ever heard of 10 or more modern methods"), linebreak
putdocx text ("--xheard_7        : ever heard of 7 or more modern methods"), linebreak
putdocx text ("--xheard_5        : ever heard of 5 or more modern methods"), linebreak
putdocx text ("--xheard_select5  : heard of all of the select 5 methods: IUD, implant, injectable, pills, male condom"), linebreak
putdocx text ("--xheard_select6  : heard of all of the select 6 methods: IUD, implant, injectable, pills, male condom, EC"), linebreak
putdocx text ("1.2 Affordability (among all women)"), linebreak bold
putdocx text ("--xinsurance(NEW) : have health insurance"), linebreak
putdocx text ("1.3 Service quality"), linebreak bold
putdocx text ("(among all women){EA-LEVEL, only Uganda)"), linebreak bold
putdocx text ("--SDPall_essential5_rnoso: EA with 1+ linked SDP with all five eseential methods (ready AND noso)"), linebreak
putdocx text ("--SDPall_essential5_noso: EA with 1+ linked SDP with all five eseential methods (AND without 3-mo stockout)"), linebreak
putdocx text ("--SDPall_essential5_ready: EA with 1+ linked SDP with all five eseential methods (AND ready)"), linebreak
putdocx text ("--SDPall_essential5ec_rnoso: EA with 1+ linked SDP with all five eseential methods+EC (ready AND noso)"), linebreak
putdocx text ("--SDPall_essential5ec_noso: EA with 1+ linked SDP with all five eseential methods+EC (AND without 3-mo stockout)"), linebreak
putdocx text ("--SDPall_essential5ec_ready: EA with 1+ linked SDP with all five eseential methods+EC (AND ready)"), linebreak
putdocx text ("(among only modern method (minus LAM) users)"), linebreak bold
putdocx text ("--xmii_side       : MII: told about side effect"), linebreak
putdocx text ("--xmii_sidewhat   : MII: told about what to do for side effect"), linebreak
putdocx text ("--xmii_other      : MII: told about other methods"), linebreak
putdocx text ("--xmii_switch(NEW): MII: told about switching"), linebreak
putdocx text ("--xmii3           : told about all three MII elements"), linebreak
putdocx text ("--xmii4(NEW)      : told about all three MII elements plus switching"), linebreak
putdocx text (""), linebreak
putdocx text ("List of figures"), linebreak bold
putdocx text ("Figure 1. Level of potential indicators, latest survey"), linebreak 
putdocx text ("Figure 2. Trends of potential indicators"), linebreak 	
putdocx text ("Figure 3. Level of potential indicators by background, latest survey"), linebreak
putdocx text ("Figure 4. MCPR by potential indicators, latest survey"), linebreak		
putdocx text (""), linebreak
putdocx text ("Summary"), linebreak bold
putdocx text ("1. Cognitive indicators tend to correlated with SES in both Kenya & Uganda."), linebreak  
putdocx text ("2. MII/quality indicator (which is measured only among current modern method users) do not vary by SES as clearly."), linebreak 
putdocx text ("3. MCP does vary by cognitive indicators in both countries (and insurance in the case of Kenya)."), linebreak 
putdocx text ("4. In terms of trends, nothing notable in Kenya. In Uganda, cognitive indicators increased - specifically ever heard of 5+ methods and ever heard of specific 5 methods). EA-level method availability measures fluctuate."), linebreak  

putdocx pagebreak 
putdocx paragraph
putdocx text ("Figure 1. Level of potential indicators, latest survey"), linebreak bold 			
	
	use summary_Access_Indicators_IR.dta, clear

		#delimit; 
		global option "
			blabel(bar)			
			legend(pos(3) col(1) size(small)
				label(1 "MCPR")
				label(2 "Heard of 10+ methods")		
				label(3 "Heard of 7+ methods")		
				label(4 "Heard of 5+ methods")		
				label(5 "Heard of specific 6 methods")		
				label(6 "Heard of specific 5* methods")			
				label(7 "have insurance")
				label(8  "Linked SDP ready & noso: 5*")	
				label(9  "Linked SDP noso: 5*")
				label(10 "Linked SDP ready: 5*")	
				label(11 "Linked SDP ready & noso: 5*&EC")
				label(12 "Linked SDP noso: 5*&EC")
				label(13 "Linked SDP ready: 5*&EC")
				label(14 "MII: side effects")
				label(15 "MII: SE what to do")
				label(16 "MII: other method")
				label(17 "MII: switching")
				label(18 "MII: all 3")
				label(19 "MII: all 4")
				)
			bar(1, bcolor(gray*0.8))
			bar(2, bcolor(maroon*1.0))
			bar(3, bcolor(maroon*0.8))
			bar(4, bcolor(maroon*0.6))
			bar(5, bcolor(cranberry*1.0))
			bar(6, bcolor(cranberry*0.8))
			bar(7, bcolor(dkorange*1.0))
			bar(8,  bcolor(blue*1.0))
			bar(9,  bcolor(blue*0.8))
			bar(10, bcolor(blue*0.6))
			bar(11, bcolor(navy*1.0))
			bar(12, bcolor(navy*0.8))
			bar(13, bcolor(navy*0.6))
			bar(14, bcolor(dkgreen*1.0))
			bar(15, bcolor(dkgreen*0.8))
			bar(16, bcolor(dkgreen*0.6))
			bar(17, bcolor(dkgreen*0.4))			
			bar(18, bcolor(green*1.0))
			bar(19, bcolor(green*0.8))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(4)
			";
			#delimit cr
			
		#delimit; 
		graph bar mcp $indicatorlistall  if xsurvey=="KER8" & group=="All", 
			$option 
			title("Kenya Phase 1 (2019)")
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		
putdocx paragraph
putdocx image graph.png				

	use summary_Access_Indicators_IR.dta, clear

		#delimit; 
		graph bar mcp $indicatorlistall  if xsurvey=="UGR6" & group=="All", 
			$option 
			title("Uganda Round 6 (2018)")
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		
putdocx paragraph
putdocx image graph.png		

putdocx pagebreak
putdocx paragraph
putdocx text ("Figure 2. Trends of potential indicators"), linebreak bold 			

	use summary_Access_Indicators_IR.dta, clear

		#delimit; 
		global option "
			legend(pos(3) col(1) size(small)
				label(1 "MCPR")
				label(2 "Heard of 10+ methods")		
				label(3 "Heard of 7+ methods")		
				label(4 "Heard of 5+ methods")		
				label(5 "Heard of specific 6 methods")		
				label(6 "Heard of specific 5* methods")			
				label(7 "have insurance")
				label(8  "Linked SDP ready & noso: 5*")	
				label(9  "Linked SDP noso: 5*")
				label(10 "Linked SDP ready: 5*")	
				label(11 "Linked SDP ready & noso: 5*&EC")
				label(12 "Linked SDP noso: 5*&EC")
				label(13 "Linked SDP ready: 5*&EC")
				label(14 "MII: side effects")
				label(15 "MII: SE what to do")
				label(16 "MII: other method")
				label(17 "MII: switching")
				label(18 "MII: all 3")
				label(19 "MII: all 4")
				)
			lcolor(
				gray*0.8
				maroon*1.0
				maroon*0.8
				maroon*0.6
				cranberry*1.0
				cranberry*0.8
				dkorange*1.0				
				blue*1.0
				blue*0.8
				blue*0.6
				navy*1.0
				navy*0.8
				navy*0.6
				dkgreen*1.0
				dkgreen*0.8
				dkgreen*0.6
				dkgreen*0.4
				green*1.0
				green*0.8
				)
			ylab(0 (20) 100, angle(0) labsize(small))  
			xtitle("Year")
			xsize(8) ysize(4)
			"; 
			#delimit cr
			
		#delimit; 
		twoway line mcp $indicatorlistall year if xcountry=="KE" & group=="All", 
			$option
			title("Kenya")
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
			
putdocx paragraph
putdocx image graph.png				

		#delimit; 
		twoway line mcp $indicatorlistall year if xcountry=="UG" & group=="All", 
			$option
			title("Uganda")	
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
			
putdocx paragraph
putdocx image graph.png			

putdocx pagebreak			
putdocx paragraph
putdocx text ("Figure 3. Level of potential indicators by background, latest survey"), linebreak bold 			
	
	use summary_Access_Indicators_IR.dta, clear	
	
	foreach country in KE UG{
	foreach cov of varlist $covlist{
		#delimit; 
		graph bar mcp xheard_10 xheard_7 xheard_5 xheard_select6 xheard_select5 if xcountry=="`country'" & xlatest==1 & (`cov'==0 | `cov'==1), 
			by(`cov', row(1) legend(pos(3) size(small)) )
			blabel(bar)			
			legend(pos(3) col(1) size(small)
				label(1 "MCPR")
				label(2 "Heard of 10+ methods")		
				label(3 "Heard of 7+ methods")		
				label(4 "Heard of 5+ methods")		
				label(5 "Heard of specific 6 methods")		
				label(6 "Heard of specific 5 methods")			
				)
			bar(1, bcolor(gray*0.8))
			bar(2, bcolor(maroon*1.0))
			bar(3, bcolor(maroon*0.8))
			bar(4, bcolor(maroon*0.6))
			bar(5, bcolor(cranberry*1.0))
			bar(6, bcolor(cranberry*0.8))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(4)
			note(, size(small))
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		
putdocx paragraph
putdocx text ("`country', by `cov'"), linebreak bold 				
putdocx image graph.png				
}
}

	foreach country in KE{
	foreach cov of varlist $covlist{
		#delimit; 
		graph bar mcp xinsurance if xcountry=="`country'" & xlatest==1 & (`cov'==0 | `cov'==1), 
			by(`cov', row(1) legend(pos(3) size(small)) )
			blabel(bar)			
			legend(pos(3) col(1) size(small)
				label(1 "MCPR")
				label(2 "have insurance")
				)
			bar(1, bcolor(gray*0.8))
			bar(2, bcolor(dkorange*1.0))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(4)
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		
putdocx paragraph
putdocx text ("`country', by `cov'"), linebreak bold 			
putdocx image graph.png				
}
}

	foreach country in UG{
	foreach cov of varlist $covlist{
		#delimit; 
		graph bar mcp 
				SDPall_essential5_rnoso
				SDPall_essential5_noso 
				SDPall_essential5_ready
				SDPall_essential5ec_rnoso
				SDPall_essential5ec_noso 		
				SDPall_essential5ec_ready	 if xcountry=="`country'" & xlatest==1 & (`cov'==0 | `cov'==1), 
			by(`cov', row(1) legend(pos(3) size(small)) )
			blabel(bar)			
			legend(pos(3) col(1) size(small)
				label(1 "MCPR")
				label(2 "Linked SDP ready & noso: 5*")	
				label(3 "Linked SDP noso: 5*")
				label(4 "Linked SDP ready: 5*")	
				label(5 "Linked SDP ready & noso: 5*&EC")
				label(6 "Linked SDP noso: 5*&EC")
				label(7 "Linked SDP ready: 5*&EC")
				)
			bar(1, bcolor(gray*0.8))
			bar(2, bcolor(blue*1.0))
			bar(3, bcolor(blue*0.8))
			bar(4, bcolor(blue*0.6))
			bar(5, bcolor(navy*1.0))
			bar(6, bcolor(navy*0.8))
			bar(7, bcolor(navy*0.6))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(4)
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
	
putdocx paragraph
putdocx text ("`country', by `cov'"), linebreak bold 			
putdocx image graph.png				
}
}

	foreach country in KE UG{
	foreach cov of varlist $covlist{
		#delimit; 
		graph bar mcp xmii* if xcountry=="`country'" & xlatest==1 & (`cov'==0 | `cov'==1), 
			by(`cov', row(1) legend(pos(3) size(small)) )
			blabel(bar)			
			legend(pos(3) col(1) size(small)
				label(1 "MCPR")
				label(2 "MII: side effects")
				label(3 "MII: SE what to do")
				label(4 "MII: other method")
				label(5 "MII: switching")
				label(6 "MII: all 3")
				label(7 "MII: all 4")
				)
			bar(1, bcolor(gray*0.8))
			bar(2, bcolor(dkgreen*1.0))
			bar(3, bcolor(dkgreen*0.8))
			bar(4, bcolor(dkgreen*0.6))
			bar(5, bcolor(dkgreen*0.4))			
			bar(6, bcolor(green*1.0))
			bar(7, bcolor(green*0.8))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(4)
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		
putdocx paragraph
putdocx text ("`country', by `cov'"), linebreak bold 				
putdocx image graph.png				
}
}

putdocx pagebreak			
putdocx paragraph
putdocx text ("Figure 4. MCPR by potential indicators, latest survey"), linebreak bold 			

	use summary_Access_Indicators_IR.dta, clear	

	#delimit; 
	global indicatorlistKE "
		xheard_10
		xheard_7
		xheard_5
		xheard_select6
		xheard_select5
		xinsurance
		";
		#delimit cr		
	
	foreach country in KE {	
	foreach indicator of varlist $indicatorlistKE{
		#delimit; 
		graph bar mcp if xcountry=="`country'" & xlatest==1 & (`indicator'==0 | `indicator'==1) , 
			by(`indicator', row(1) legend(off) )
			blabel(bar)			
			bar(1, bcolor(gray*0.8))
			ylab(0 (20) 100, angle(0) labsize(small))  
			ytitle("MCPR (%)")
			xsize(4) ysize(4)
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy		
		graph save graph_`indicator'.gph, replace	
}
	#delimit; 
	global graphlistKE "
		graph_xheard_10.gph
		graph_xheard_7.gph
		graph_xheard_5.gph
		graph_xheard_select6.gph
		graph_xheard_select5.gph
		graph_xinsurance.gph
		";
		#delimit cr

	gr combine $graphlistKE , col(2) xsize(8) ysize(10)
	gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	graph export graph.png, replace	
	
putdocx paragraph
putdocx text ("`country'"), linebreak bold 			
putdocx image graph.png				
}

	#delimit; 
	global indicatorlistUG1 "
		xheard_10
		xheard_7
		xheard_5
		xheard_select6
		xheard_select5	
		";
		#delimit cr	
		
	#delimit; 
	global indicatorlistUG2 "
		SDPall_essential5_rnoso
		SDPall_essential5_noso 
		SDPall_essential5_ready
		SDPall_essential5ec_rnoso
		SDPall_essential5ec_noso 		
		SDPall_essential5ec_ready	
		";
		#delimit cr			
		
	foreach country in UG{	
		
		foreach indicator of varlist $indicatorlistUG1{
			#delimit; 
			graph bar mcp if xcountry=="`country'" & xlatest==1 & (`indicator'==0 | `indicator'==1) , 
				by(`indicator', row(1) legend(off) )
				blabel(bar)			
				bar(1, bcolor(gray*0.8))
				ylab(0 (20) 100, angle(0) labsize(small))  
				ytitle("MCPR (%)")
				xsize(4) ysize(4)
				; 
				#delimit cr
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy		
			graph save graph_`indicator'.gph, replace	
		}
		
		#delimit; 
		global graphlistUG1 "
			graph_xheard_10.gph
			graph_xheard_7.gph
			graph_xheard_5.gph
			graph_xheard_select6.gph
			graph_xheard_select5.gph
			";
			#delimit cr

		gr combine $graphlistUG1 , col(2) xsize(8) ysize(10)
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx text ("`country'"), linebreak bold 				
		putdocx image graph.png				
		
	foreach indicator of varlist $indicatorlistUG2{
			#delimit; 
			graph bar mcp if xcountry=="`country'" & xlatest==1 & (`indicator'==0 | `indicator'==1) , 
				by(`indicator', row(1) legend(off) )
				blabel(bar)			
				bar(1, bcolor(gray*0.8))
				ylab(0 (20) 100, angle(0) labsize(small))  
				ytitle("MCPR (%)")
				xsize(4) ysize(4)
				; 
				#delimit cr
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy		
			graph save graph_`indicator'.gph, replace	
		}
		
		#delimit; 
		global graphlistUG2 "
			graph_SDPall_essential5_rnoso.gph
			graph_SDPall_essential5_noso.gph 
			graph_SDPall_essential5_ready.gph
			graph_SDPall_essential5ec_rnoso.gph
			graph_SDPall_essential5ec_noso.gph 		
			graph_SDPall_essential5ec_ready.gph
			";
			#delimit cr

		gr combine $graphlistUG2 , col(2) xsize(8) ysize(10)
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx text ("`country'"), linebreak bold 				
		putdocx image graph.png				
		
}
		
putdocx save Access_Potential_Indicators_IR_$date.docx, replace	

erase graph.png

erase graph_xinsurance.gph
foreach graph in $graphlistUG{
	erase `graph'
	}
	
END OF DO FILE	
