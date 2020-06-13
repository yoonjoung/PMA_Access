* PRIOR TO THIS DO FILE, the following has to be in place
		/*
		***** create RECODE file using public/raw data
		cd "C:\Users\YoonJoung Choi\Dropbox\0 Data\PMA\"
		do createPMA_HRPRIR.do
		do createPMA_SR_BurkinaFaso.do
		do createPMA_SR_CotedIvoire.do
		do createPMA_SR_DRC.do
		do createPMA_SR_Ethiopia.do
		do createPMA_SR_India.do
		do createPMA_SR_Kenya.do
		do createPMA_SR_Niger.do
		do createPMA_SR_Nigeria.do
		do createPMA_SR_Uganda.do 

		***** create EA-level SDP linked data
		cd "C:\Users\YoonJoung Choi\Dropbox\0 iSquared\iSquared_PMA\Effective Access\"
		do Effective_Access_Pop_SDP_Link.do
		*/

* VARIATION of "Effective_Access_Indicators_IR.do"
* That do file uses surveys that have most of broad access variables. 
* So, for example, earlier surveys are not used, since they did not have "ever-heard of" questions. 

* This do file only about pop-level access to methods indicators. 
* Use as many surveys as possible. Earlier surveys thus included. 
* Here "surveylist" is same with "surveylistEASDPLINK", which are both expanded 
* FINAL product: "summary_Access_Indicators_IR_PopAccessToMethods.dta"

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
	BFR1 BFR2 BFR3 BFR4 BFR5 BFR6  
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7	
	ETR2 ETR3 ETR4 ETR5 ETR6 
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER2 KER3 KER4 KER5 KER6 KER7
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 	
	NGKanoR3  NGKanoR4  NGKanoR5   
	NGLagosR3 NGLagosR4 NGLagosR5  
	UGR2 UGR3 UGR4 UGR5 UGR6 
	";
	#delimit cr
	
#delimit;
global surveylistminusone " 
	BFR2 BFR3 BFR4 BFR5 BFR6  
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7	
	ETR2 ETR3 ETR4 ETR5 ETR6 
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER2 KER3 KER4 KER5 KER6 KER7
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 	
	NGKanoR3  NGKanoR4  NGKanoR5   
	NGLagosR3 NGLagosR4 NGLagosR5  
	UGR2 UGR3 UGR4 UGR5 UGR6 
	";
	#delimit cr

#delimit;
global surveylistEASDPLINK " 
	BFR1 BFR2 BFR3 BFR4 BFR5 BFR6  
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7	
	ETR2 ETR3 ETR4 ETR5 ETR6 
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER2 KER3 KER4 KER5 KER6 KER7
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 	
	NGKanoR3  NGKanoR4  NGKanoR5   
	NGLagosR3 NGLagosR4 NGLagosR5  
	UGR2 UGR3 UGR4 UGR5 UGR6 
	";
	#delimit cr
	

************************************************************************
* B. PREP women-level access variables  
************************************************************************

*****1. Psycosocial => see at the end of this section, done in two different ways for newer vs. new surveys 
	
*****2. Cognitive

*****3. Geographic accessibility  

*****4. Service quality (IR variables)

*****5. Administrative: N/A for women data

*****6. Affordability 	

*****7. BASIC var
	
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	
		foreach var of varlist wealthquintile ur school marital_status   {
		replace `var'=. if `var'<0
		}	
		gen countrycode=substr(xsurvey, 1, length(xsurvey)-2)
		
		*gen byte xeverbirth=ever_birth==1
		gen byte xwealth5=wealthquintile
		gen byte xtop3=wealthquintile>=3 & wealthquintile!=.
		gen byte xurban=ur==1
		gen byte xinunion=marital_status==1 | marital_status==2
				
		gen byte xedu_never	=school==0
		gen xedu_pri=0
			replace xedu_pri	=1 if school==1 & countrycode!="UG"
			replace xedu_pri	=1 if (school==1|school==2) & countrycode=="UG"
		gen xedu_sec=0
			replace xedu_sec	=1 if school>=2 & countrycode!="UG"
			replace xedu_sec	=1 if school>=3 & countrycode=="UG"		
		gen byte xedu3=0
			replace xedu3=1 if xedu_pri==1
			replace xedu3=2 if xedu_sec==1
		foreach var of varlist xedu*{
			replace `var'=. if school<0 | school==.
			}
	
		gen xedurban=.
			replace xedurban=1 if xurban==0 & xedu_sec==0
			replace xedurban=2 if xurban==1 & xedu_sec==0
			replace xedurban=3 if xurban==0 & xedu_sec==1
			replace xedurban=4 if xurban==1 & xedu_sec==1
		lab define xedurban 1"R, <secondary" 2"U, <secondary" 3"R, >=secondary" 4"U, >=secondary" 
		lab value xedurban xedurban
		
		gen temp = dofc(FQdoi_correctedSIF)
		format %td temp
		gen tempmonth = month(temp)
		gen tempyear = year(temp)
		gen tempcmc 	= 12*(tempyear - 1900) + tempmonth
		
		egen cmc = median(tempcmc)

			drop temp*
		
		lab var xtop3 "poor (bottom two quintiles) vs. non-poor (1)"
		lab var xurban "rural vs. urban (1) "
		lab var xedu_sec "ever attended: none or primary vs. secondary or higher+ (1)"
		
		lab var cmc "Interview date in CMC, median per survey round"
		
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
	tab num_SDPall xsurvey 
	sort EA_ID
	save "$data/EAlevel_SDP_`survey'.dta", replace
}

/*
set more off
foreach survey  in $surveylistEASDPLINK{
	use "$data/EAlevel_SDP_`survey'.dta", clear
	tab xsurvey
	sum round SDPall_essential5_noso SDPall_essential5_rnoso 
}
*/

set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators.dta, clear

	sort EA_ID
	merge EA_ID using "$data/EAlevel_SDP_`survey'.dta",
	tab _merge, m /*check EAs with no SDP. This should be none, but not necessarily in reality*/
		*keep if _merge==3
		*drop _merge
		
		*drop SDPlow* /*low level regardless of sector*/
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
	
/*
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
*/

*****1. Psycosocial  - NEWER 2020 surveys 

*OKAY DATA READY FOR ANALYSIS 
*/

	
************************************************************************
* C. Create summary dataset 
************************************************************************

#delimit; 
global indicatorlist "

	SDPall_essential5_offer
	SDPall_essential5_curav
	SDPall_essential5_rnoso
	SDPall_essential5_noso 
	SDPall_essential5_ready
	SDPlow_essential5_offer
	SDPlow_essential5_curav
	SDPlow_essential5_rnoso
	SDPlow_essential5_noso 
	SDPlow_essential5_ready	
	
	";
	#delimit cr
	
#delimit; 
global indicatorlistall "

	SDPall_essential5_offer
	SDPall_essential5_curav
	SDPall_essential5_rnoso
	SDPall_essential5_noso 
	SDPall_essential5_ready
	SDPlow_essential5_offer
	SDPlow_essential5_curav
	SDPlow_essential5_rnoso
	SDPlow_essential5_noso 
	SDPlow_essential5_ready	

	";
	#delimit cr	

global covlist "xtop3 xurban xedu_sec"

**************************************** AMONG ALL women 

use IR_BFR1_Access_Indicators.dta, clear
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
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}			
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR_PopAccessToMethods.dta, replace 	
		
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods.dta, replace 	
	}
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_PopAccessToMethods.dta, replace 		
	}	

***** Unweighted number of observation

	use temp.dta, clear
		collapse (count) obs , by(xsurvey)
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
	
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
	}
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 		
	}	

***** Merge weighted estimates and unweighted number of observations 
	use summary_Access_Indicators_IR_PopAccessToMethods.dta, clear
		codebook xsurvey
	use summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, clear
		codebook xsurvey
	
	use summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, clear
		sort xsurvey group grouplabel	
		merge xsurvey group grouplabel	using summary_Access_Indicators_IR_PopAccessToMethods.dta
			tab _merge
			drop _merge
			
		tab xsurvey if group=="All"	

		gen groupdemand=0
		lab	var	groupdemand "only women with demand"
		
	save summary_Access_Indicators_IR_PopAccessToMethods.dta, replace
		
**************************************** AMONG women with DEMAND 

use IR_BFR1_Access_Indicators.dta, clear
foreach survey in $surveylistminusone{
	append using IR_`survey'_Access_Indicators.dta, force
	}
	
	keep if cp==1 | unmet==1 /*KEEP Only women with demand for FP*/
	
	gen obs=1
save temp.dta, replace


***** Weighted eestimates 

	use temp.dta, clear
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}			
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, replace 	
		
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, replace 	
	}
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, replace 		
	}	


***** Unweighted number of observation

	use temp.dta, clear
		collapse (count) obs , by(xsurvey)
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
	
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
	}
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, replace 		
	}	

***** Merge weighted estimates and unweighted number of observations 
	use summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, clear
		codebook xsurvey
	use summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, clear
		codebook xsurvey
	
	use summary_Access_Indicators_IR_PopAccessToMethods_obs.dta, clear
		sort xsurvey group grouplabel	
		merge xsurvey group grouplabel	using summary_Access_Indicators_IR_PopAccessToMethods_demand.dta
			tab _merge
			drop _merge
			
		tab xsurvey if group=="All"	
		
		gen groupdemand=1
		lab	var	groupdemand "only women with demand"
		
	save summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, replace
	
**************************************** APPEND BOTH DATA SETS 

	use summary_Access_Indicators_IR_PopAccessToMethods.dta, replace
	append using summary_Access_Indicators_IR_PopAccessToMethods_demand.dta, 
	
***** Further variables and data cleaning 

		lab	var	SDPall_essential5_curav	"SDPall_essential5_curav"
		lab	var	SDPall_essential5_noso	"SDPall_essential5_noso"
		lab	var	SDPall_essential5_ready	"SDPall_essential5_ready"
		lab	var	SDPall_essential5_rnoso	"SDPall_essential5_rnoso"

	    replace grouplabel="currently NOT in union" 		if group=="By xinunion" & grouplabel=="No"
        replace grouplabel="currently in union" 		if group=="By xinunion" & grouplabel=="Yes" 			
	    replace grouplabel="NEVER given birth" 		if group=="By xeverbirth" & grouplabel=="No"
        replace grouplabel="EVER given birth" 		if group=="By xeverbirth" & grouplabel=="Yes" 	
	    replace grouplabel="less than secondary" 	if group=="By xedu_sec" & grouplabel=="No"
        replace grouplabel="secondary or more" 		if group=="By xedu_sec" & grouplabel=="Yes" 
        replace grouplabel="Bottom 2 quintiles"	if group=="By xtop3" & grouplabel=="No" 
        replace grouplabel="Top 3 quintiles"	if group=="By xtop3" & grouplabel=="Yes" 
        replace grouplabel="Rural"	if group=="By xurban" & grouplabel=="No"
        replace grouplabel="Urban" 	if group=="By xurban" & grouplabel=="Yes" 
		
		replace grouplabel="Rural, <secondary" 	if group=="By xedurban" & grouplabel=="1" 
		replace grouplabel="Urban, <secondary" 	if group=="By xedurban" & grouplabel=="2" 
		replace grouplabel="Rural, >=secondary" 	if group=="By xedurban" & grouplabel=="3" 
		replace grouplabel="Urban, >=secondary" 	if group=="By xedurban" & grouplabel=="4" 
				
		replace group="Education" if group=="By xedu_sec" 
		replace group="HH wealth" if group=="By xtop3"
		replace group="Residential area" if group=="By xurban"
		replace group="Parity" if group=="By xeverbirth"
		replace group="Education x Residence" if group=="By xedurban"
		replace group="Union status" if group=="By xinunion"
	
	gen country=substr(xsurvey, 1, length(xsurvey)-2)
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
	gen countrycode=substr(xsurvey, 1, 2)

	gen year = 1900 + int(cmc/12) 
	gen month= cmc-12*(year - 1900)
	
	egen temp=max(round), by(country) 
	gen latestIR=round==temp
		drop temp

	*PROBLEMATIC surveys with EA_SDP link
	foreach var of varlist SDPall* {
		replace `var'=. if (xsurvey=="NENiameyR5" | xsurvey=="NGKanoR3" | xsurvey=="BFR6")
		}		
				
	* CHECK small n : SHOULD NOT BE an issue in IR in most cases
	sum obs	
	sort groupdemand xsurvey
	list xsurvey groupdemand group grouplabel obs if obs<=20
		
		foreach var of varlist mcp - SDPlow_essential5_ready {
			replace `var'=. if obs<=20
			}
	
	sort xsurvey group grouplabel
	save summary_Access_Indicators_IR_PopAccessToMethods.dta, replace 	
	
	export delimited using summary_Access_Indicators_IR_PopAccessToMethods.csv, replace
	* save in additional folders for apps => NOT APPLICABLE
	/*
	export delimited using ShinyAppAccess/summary_Access_Indicators_IR_PopAccessToMethods.csv, replace
	export delimited using ShinyAppPopAccessToMethods/summary_Access_Indicators_IR_PopAccessToMethods.csv, replace
	export delimited using ShinyAppPsychosocial/summary_Access_Indicators_IR_PopAccessToMethods.csv, replace
	*/
	
erase temp.dta		
erase summary_Access_Indicators_IR_PopAccessToMethods_obs.dta

set more off
foreach survey in $surveylist{
	erase IR_`survey'_Access_Indicators.dta 
}

OKAY Summary DATA READY FOR ANALYSIS and Shiny App

*/
