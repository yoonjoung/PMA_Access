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
		
* PLUS this do file is for sensitivity analyses additionally. 
* to compare impact of THREE approaches to assign values for 
* women with no access JUST BECAUSE OF linktage failure

* FINAL product: "summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta"

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
		
save IR_`survey'_Access_Indicators_sensitivity.dta, replace 	
}


*****8. MERGE with the EA-SDP data, created from "Effective_Access_Pop_SDP_`counry'.do"

set more off
foreach survey  in $surveylistEASDPLINK{
	use "$data/EAlevel_SDP_`survey'.dta", clear
	tab num_SDPall xsurvey 
	sort EA_ID
	save "$data/EAlevel_SDP_`survey'.dta", replace
}

set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators_sensitivity.dta, clear

	sort EA_ID
	merge EA_ID using "$data/EAlevel_SDP_`survey'.dta",
	tab _merge, m /*check EAs with no SDP. This should be none, but not necessarily in reality*/
		*keep if _merge==3
		*drop _merge
		
		*keep only SDPall_* and SDPlow_*
		sum SDP*
		drop SDPpub_* SDPpub12_*
		drop SDPall_essential4* SDPlow_essential4*
		drop SDPall_essential5ec* SDPlow_essential5ec*
		drop SDPall_essential5_noso SDPlow_essential5_noso
		
		foreach var of varlist SDP*{
			gen num`var'=`var'
			replace `var'=1 if `var'>=1 & `var'!=.
			*replace `var'=1 if `var'>=1
		}
		
		lab var SDPall_essential5_offer 	"EA-linked any SDP(s) offers `essential' specific five methods"
		lab var SDPall_essential5_curav 	"EA-linked any SDP(s) currently has `essential' specific five methods"
		lab var SDPall_essential5_ready 	"EA-linked any SDP(s) currently has (with readiness**) `essential' specific five methods"	
		lab var SDPall_essential5_rnoso 	"EA-linked any SDP(s) currently has (with readiness**) `essential' specific five methods + no 3-month stock-out"	

save IR_`survey'_Access_Indicators_sensitivity.dta, replace 	
}

*****8. SENSITIVITY 
/*
set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators_sensitivity.dta, clear
	sum round noSDPany noSDPlow xurban 
	}
	
set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators_sensitivity.dta, clear
	sum round numSDPall* SDPall* numSDPlow* SDPlow*
	}
*/

set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators_sensitivity.dta, clear
		
	gen xadmin=.
		capture confirm variable region
			if !_rc {
			replace xadmin=region
			} 
			else{
				capture confirm variable state
				if !_rc {
				replace xadmin=state
				} 
				else{
					capture confirm variable county
					if !_rc {
					replace xadmin=county
					} 
					else{
						capture confirm variable province
						if !_rc {
						replace xadmin=province
						} 
						else{
							tab xsurvey /*surveys without xadmin var*/
						}
					}
				}
			}

save IR_`survey'_Access_Indicators_sensitivity.dta, replace 				
}


set more off
foreach survey  in $surveylistEASDPLINK{
*foreach survey  in KER6{
	use IR_`survey'_Access_Indicators_sensitivity.dta, clear
	
		foreach var of varlist SDPall_essential5* SDPlow_essential5* {
			
			capture drop temp
			
			*** APPROACH 1: GIVE NATIONAL AVERAGE
			egen temp=mean(num`var')
			
			gen `var'_1=`var'
				recode `var'_1 .=1 if temp>=1 
				recode `var'_1 .=0 if temp<1 
				drop temp
						
			*** APPROACH 2: GIVE URBAN/RURAL AVERAGE 
			egen temp=mean(num`var'), by(xurban)
			
			gen `var'_2=`var'
				recode `var'_2 .=1 if temp>=1 
				recode `var'_2 .=0 if temp<1 
				drop temp
						
			*** APPROACH 3: GIVE REGIONAL AVERAGE 
			egen temp=mean(num`var'), by(xadmin)
			
			gen `var'_3=`var'
				recode `var'_3 .=1 if temp>=1 
				recode `var'_3 .=0 if temp<1 
				drop temp
		
			*** APPROACH 4: GIVE 0	
			gen `var'_4=`var'
				recode `var'_4 .=0
				
			}	
save IR_`survey'_Access_Indicators_sensitivity.dta, replace
}	

/*
set more off
foreach survey  in $surveylistEASDPLINK{
	use IR_`survey'_Access_Indicators_sensitivity.dta, clear
	sum round SDPall_essential5_rnoso_s4
}
*/

*****1. Psycosocial  - NEWER 2020 surveys 

*OKAY DATA READY FOR ANALYSIS 

*/

	
************************************************************************
* C. Create summary dataset 
************************************************************************

**************************************** AMONG ALL women 

use IR_BFR1_Access_Indicators_sensitivity.dta, clear
foreach survey in $surveylistminusone{
	append using IR_`survey'_Access_Indicators_sensitivity.dta, force
	}
	gen obs=1
save temp.dta, replace

***** Weighted eestimates 

	use temp.dta, clear
		collapse  (mean) noSDPany noSDPlow SDPall_essential5* SDPlow_essential5* [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist noSDPany noSDPlow SDPall_essential5* SDPlow_essential5* {
				*replace `var'=round(`var'*100, 1)
				replace `var'=round(`var'*100, 0.1)
				}			
			gen group="All"	
			gen grouplabel="All"
		sort xsurvey group grouplabel		
		save summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta, replace 	

***** Unweighted number of observation

	use temp.dta, clear
		collapse (count) obs , by(xsurvey)
			gen group="All"	
			gen grouplabel="All"
		sort xsurvey group grouplabel		
		save summary_Access_Indicators_IR_PopAccessToMethods_obs_sensitivity.dta, replace 	

***** Merge weighted estimates and unweighted number of observations 
	use summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta, clear
		codebook xsurvey
	use summary_Access_Indicators_IR_PopAccessToMethods_obs_sensitivity.dta, clear
		codebook xsurvey
	
	use summary_Access_Indicators_IR_PopAccessToMethods_obs_sensitivity.dta, clear
		sort xsurvey group grouplabel	
		merge xsurvey group grouplabel	using summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta
			tab _merge
			drop _merge
		
	save summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta, replace

	
***** Further variables and data cleaning 

		lab	var	SDPall_essential5_curav	"SDPall_essential5_curav"
		lab	var	SDPall_essential5_ready	"SDPall_essential5_ready"
		lab	var	SDPall_essential5_rnoso	"SDPall_essential5_rnoso"
		
		gen diff_SDPall_essential5_offer_1	 = 	SDPall_essential5_offer_1	 - 	SDPall_essential5_offer
		gen diff_SDPall_essential5_curav_1	 = 	SDPall_essential5_curav_1	 - 	SDPall_essential5_curav
		gen diff_SDPall_essential5_ready_1	 = 	SDPall_essential5_ready_1	 - 	SDPall_essential5_ready
		gen diff_SDPall_essential5_rnoso_1	 = 	SDPall_essential5_rnoso_1	 - 	SDPall_essential5_rnoso
		gen diff_SDPlow_essential5_offer_1	 = 	SDPlow_essential5_offer_1	 - 	SDPlow_essential5_offer
		gen diff_SDPlow_essential5_curav_1	 = 	SDPlow_essential5_curav_1	 - 	SDPlow_essential5_curav
		gen diff_SDPlow_essential5_ready_1	 = 	SDPlow_essential5_ready_1	 - 	SDPlow_essential5_ready
		gen diff_SDPlow_essential5_rnoso_1	 = 	SDPlow_essential5_rnoso_1	 - 	SDPlow_essential5_rnoso
						
		gen ratio_SDPall_essential5_offer_1	 = 	SDPall_essential5_offer_1	 / 	SDPall_essential5_offer
		gen ratio_SDPall_essential5_curav_1	 = 	SDPall_essential5_curav_1	 / 	SDPall_essential5_curav
		gen ratio_SDPall_essential5_ready_1	 = 	SDPall_essential5_ready_1	 / 	SDPall_essential5_ready
		gen ratio_SDPall_essential5_rnoso_1	 = 	SDPall_essential5_rnoso_1	 / 	SDPall_essential5_rnoso
		gen ratio_SDPlow_essential5_offer_1	 = 	SDPlow_essential5_offer_1	 / 	SDPlow_essential5_offer
		gen ratio_SDPlow_essential5_curav_1	 = 	SDPlow_essential5_curav_1	 / 	SDPlow_essential5_curav
		gen ratio_SDPlow_essential5_ready_1	 = 	SDPlow_essential5_ready_1	 / 	SDPlow_essential5_ready
		gen ratio_SDPlow_essential5_rnoso_1	 = 	SDPlow_essential5_rnoso_1	 / 	SDPlow_essential5_rnoso
	
		gen diff_SDPall_essential5_offer_2	 = 	SDPall_essential5_offer_2	 - 	SDPall_essential5_offer
		gen diff_SDPall_essential5_curav_2	 = 	SDPall_essential5_curav_2	 - 	SDPall_essential5_curav
		gen diff_SDPall_essential5_ready_2	 = 	SDPall_essential5_ready_2	 - 	SDPall_essential5_ready
		gen diff_SDPall_essential5_rnoso_2	 = 	SDPall_essential5_rnoso_2	 - 	SDPall_essential5_rnoso
		gen diff_SDPlow_essential5_offer_2	 = 	SDPlow_essential5_offer_2	 - 	SDPlow_essential5_offer
		gen diff_SDPlow_essential5_curav_2	 = 	SDPlow_essential5_curav_2	 - 	SDPlow_essential5_curav
		gen diff_SDPlow_essential5_ready_2	 = 	SDPlow_essential5_ready_2	 - 	SDPlow_essential5_ready
		gen diff_SDPlow_essential5_rnoso_2	 = 	SDPlow_essential5_rnoso_2	 - 	SDPlow_essential5_rnoso
						
		gen ratio_SDPall_essential5_offer_2	 = 	SDPall_essential5_offer_2	 / 	SDPall_essential5_offer
		gen ratio_SDPall_essential5_curav_2	 = 	SDPall_essential5_curav_2	 / 	SDPall_essential5_curav
		gen ratio_SDPall_essential5_ready_2	 = 	SDPall_essential5_ready_2	 / 	SDPall_essential5_ready
		gen ratio_SDPall_essential5_rnoso_2	 = 	SDPall_essential5_rnoso_2	 / 	SDPall_essential5_rnoso
		gen ratio_SDPlow_essential5_offer_2	 = 	SDPlow_essential5_offer_2	 / 	SDPlow_essential5_offer
		gen ratio_SDPlow_essential5_curav_2	 = 	SDPlow_essential5_curav_2	 / 	SDPlow_essential5_curav
		gen ratio_SDPlow_essential5_ready_2	 = 	SDPlow_essential5_ready_2	 / 	SDPlow_essential5_ready
		gen ratio_SDPlow_essential5_rnoso_2	 = 	SDPlow_essential5_rnoso_2	 / 	SDPlow_essential5_rnoso

		gen diff_SDPall_essential5_offer_3	 = 	SDPall_essential5_offer_3	 - 	SDPall_essential5_offer
		gen diff_SDPall_essential5_curav_3	 = 	SDPall_essential5_curav_3	 - 	SDPall_essential5_curav
		gen diff_SDPall_essential5_ready_3	 = 	SDPall_essential5_ready_3	 - 	SDPall_essential5_ready
		gen diff_SDPall_essential5_rnoso_3	 = 	SDPall_essential5_rnoso_3	 - 	SDPall_essential5_rnoso
		gen diff_SDPlow_essential5_offer_3	 = 	SDPlow_essential5_offer_3	 - 	SDPlow_essential5_offer
		gen diff_SDPlow_essential5_curav_3	 = 	SDPlow_essential5_curav_3	 - 	SDPlow_essential5_curav
		gen diff_SDPlow_essential5_ready_3	 = 	SDPlow_essential5_ready_3	 - 	SDPlow_essential5_ready
		gen diff_SDPlow_essential5_rnoso_3	 = 	SDPlow_essential5_rnoso_3	 - 	SDPlow_essential5_rnoso
						
		gen ratio_SDPall_essential5_offer_3	 = 	SDPall_essential5_offer_3	 / 	SDPall_essential5_offer
		gen ratio_SDPall_essential5_curav_3	 = 	SDPall_essential5_curav_3	 / 	SDPall_essential5_curav
		gen ratio_SDPall_essential5_ready_3	 = 	SDPall_essential5_ready_3	 / 	SDPall_essential5_ready
		gen ratio_SDPall_essential5_rnoso_3	 = 	SDPall_essential5_rnoso_3	 / 	SDPall_essential5_rnoso
		gen ratio_SDPlow_essential5_offer_3	 = 	SDPlow_essential5_offer_3	 / 	SDPlow_essential5_offer
		gen ratio_SDPlow_essential5_curav_3	 = 	SDPlow_essential5_curav_3	 / 	SDPlow_essential5_curav
		gen ratio_SDPlow_essential5_ready_3	 = 	SDPlow_essential5_ready_3	 / 	SDPlow_essential5_ready
		gen ratio_SDPlow_essential5_rnoso_3	 = 	SDPlow_essential5_rnoso_3	 / 	SDPlow_essential5_rnoso

		gen diff_SDPall_essential5_offer_4	 = 	SDPall_essential5_offer_4	 - 	SDPall_essential5_offer
		gen diff_SDPall_essential5_curav_4	 = 	SDPall_essential5_curav_4	 - 	SDPall_essential5_curav
		gen diff_SDPall_essential5_ready_4	 = 	SDPall_essential5_ready_4	 - 	SDPall_essential5_ready
		gen diff_SDPall_essential5_rnoso_4	 = 	SDPall_essential5_rnoso_4	 - 	SDPall_essential5_rnoso
		gen diff_SDPlow_essential5_offer_4	 = 	SDPlow_essential5_offer_4	 - 	SDPlow_essential5_offer
		gen diff_SDPlow_essential5_curav_4	 = 	SDPlow_essential5_curav_4	 - 	SDPlow_essential5_curav
		gen diff_SDPlow_essential5_ready_4	 = 	SDPlow_essential5_ready_4	 - 	SDPlow_essential5_ready
		gen diff_SDPlow_essential5_rnoso_4	 = 	SDPlow_essential5_rnoso_4	 - 	SDPlow_essential5_rnoso
						
		gen ratio_SDPall_essential5_offer_4	 = 	SDPall_essential5_offer_4	 / 	SDPall_essential5_offer
		gen ratio_SDPall_essential5_curav_4	 = 	SDPall_essential5_curav_4	 / 	SDPall_essential5_curav
		gen ratio_SDPall_essential5_ready_4	 = 	SDPall_essential5_ready_4	 / 	SDPall_essential5_ready
		gen ratio_SDPall_essential5_rnoso_4	 = 	SDPall_essential5_rnoso_4	 / 	SDPall_essential5_rnoso
		gen ratio_SDPlow_essential5_offer_4	 = 	SDPlow_essential5_offer_4	 / 	SDPlow_essential5_offer
		gen ratio_SDPlow_essential5_curav_4	 = 	SDPlow_essential5_curav_4	 / 	SDPlow_essential5_curav
		gen ratio_SDPlow_essential5_ready_4	 = 	SDPlow_essential5_ready_4	 / 	SDPlow_essential5_ready
		gen ratio_SDPlow_essential5_rnoso_4	 = 	SDPlow_essential5_rnoso_4	 / 	SDPlow_essential5_rnoso
		
		lab	var	diff_SDPall_essential5_offer_1		"% point difference: sensitivity 1 - SDPall_essential5_offer"
		lab	var	ratio_SDPall_essential5_offer_1	"ratio: sensitivity 1 / SDPall_essential5_offer"
	
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
	sort xsurvey
	list xsurvey obs if obs<=20
		
		/*
		foreach var of varlist mcp - SDPlow_essential5_ready {
			replace `var'=. if obs<=20
			}
		*/
		
	sort xsurvey 
	save summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta, replace 	
	
	export delimited using summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.csv, replace
	
erase temp.dta		
erase summary_Access_Indicators_IR_PopAccessToMethods_obs_sensitivity.dta

set more off
foreach survey in $surveylist{
	erase IR_`survey'_Access_Indicators_sensitivity.dta 
}

OKAY Summary DATA READY FOR ANALYSIS

************************************************************************
* D. Sensitivity analysis  
************************************************************************

use summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta, clear 
	drop if (xsurvey=="NENiameyR5" | xsurvey=="NGKanoR3" | xsurvey=="BFR6")
	drop if (country=="Niger, Niamey" | country=="Nigeria, Kano" | xsurvey=="Nigeria, Lagos")
	
	
	sum diff_SDPall* ratio_SDPall*
	sum diff_SDPlow* ratio_SDPlow*
	
	/*
	graph box diff_SDPlow_essential5*
	graph box ratio_SDPlow_essential5*	
	*/
	
	
use summary_Access_Indicators_IR_PopAccessToMethods_sensitivity.dta, clear 
	drop if (xsurvey=="NENiameyR5" | xsurvey=="NGKanoR3" | xsurvey=="BFR6")
	drop if (country=="Niger, Niamey" | country=="Nigeria, Kano" | xsurvey=="Nigeria, Lagos")	

	global estimates 	"SDPall_essential5_offer_ SDPall_essential5_curav_ SDPall_essential5_ready_ SDPall_essential5_rnoso_"	
	global diff 		"diff_SDPall_essential5_offer_ diff_SDPall_essential5_curav_ diff_SDPall_essential5_ready_ diff_SDPall_essential5_rnoso_"	
	global ratio 		"ratio_SDPall_essential5_offer_ ratio_SDPall_essential5_curav_ ratio_SDPall_essential5_ready_ ratio_SDPall_essential5_rnoso_"	
	
	global estimateslow 	"SDPlow_essential5_offer_ SDPlow_essential5_curav_ SDPlow_essential5_ready_ SDPlow_essential5_rnoso_"	
	global difflow  		"diff_SDPlow_essential5_offer_ diff_SDPlow_essential5_curav_ diff_SDPlow_essential5_ready_ diff_SDPlow_essential5_rnoso_"	
	global ratiolow 		"ratio_SDPlow_essential5_offer_ ratio_SDPlow_essential5_curav_ ratio_SDPlow_essential5_ready_ ratio_SDPlow_essential5_rnoso_"	
		
	reshape long $estimates $diff $ratio $estimateslow $difflow $ratiolow , i(xsurvey) j(approach) 
	tostring approach, replace
	gen temp="Approach "
	egen approachgroup = concat(temp approach)
	destring approach, replace
	
	sum diff_SDPall* ratio_SDPall*
	sum diff_SDPlow* ratio_SDPlow*
	sum diff_SDPlow* ratio_SDPlow*, detail
	********** PATTERN
	
	#delimit; 
	twoway scatter 
		diff_SDPall_essential5_offer_ 
		diff_SDPall_essential5_curav_
		diff_SDPall_essential5_ready_
		diff_SDPall_essential5_rnoso_
		noSDPany if approach>=3, 
			by(approachgroup, row(1) note("") legend(pos(6)) )
			ytitle("% point difference: alternative vs. current approach")
			ylab(-5 (1) 2)
			yline(0, lcolor(black))
			xtitle("% of women without linked SDPs")
			legend(row(1) size(small) stack 
					label(1 "5 methods" "offered")
					label(2 "+ currently" "in-stock")
					label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
					label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr		

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle linestyle(color(none)) editcopy

		
		graph save graph_sensitivity_diff_pattern_all.gph, replace
		graph save graph_sensitivity_diff_pattern_all.png, replace		
		
	#delimit; 
	twoway scatter 
		ratio_SDPall_essential5_offer_ 
		ratio_SDPall_essential5_curav_
		ratio_SDPall_essential5_ready_
		ratio_SDPall_essential5_rnoso_
		noSDPany if approach>=3, 
			by(approachgroup, row(1) note("") legend(pos(6)) )
			ytitle("Ratio: alternative vs. current approach")
			ylab(0.9 (0.02) 1.04)
			yline(1, lcolor(black))
			xtitle("% of women without linked SDPs")
			legend(row(1) size(small) stack 
					label(1 "5 methods" "offered")
					label(2 "+ currently" "in-stock")
					label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
					label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr		

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle linestyle(color(none)) editcopy

		
		graph save graph_sensitivity_ratio_pattern_all.gph, replace
		graph save graph_sensitivity_ratio_pattern_all.png, replace				
		
	#delimit; 
	twoway scatter 
		diff_SDPlow_essential5_offer_ 
		diff_SDPlow_essential5_curav_
		diff_SDPlow_essential5_ready_
		diff_SDPlow_essential5_rnoso_
		noSDPany if approach>=3, 
			by(approachgroup, row(1) note("") legend(pos(6)) )
			ytitle("% point difference: alternative vs. current approach")
			ylab(-5 (1) 2)
			yline(0, lcolor(black))
			xtitle("% of women without linked SDPs")
			legend(row(1) size(small) stack 
					label(1 "5 methods" "offered")
					label(2 "+ currently" "in-stock")
					label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
					label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr		

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle linestyle(color(none)) editcopy

		
		graph save graph_sensitivity_diff_pattern_low.gph, replace
		graph save graph_sensitivity_diff_pattern_low.png, replace		
		
	#delimit; 
	twoway scatter 
		ratio_SDPlow_essential5_offer_ 
		ratio_SDPlow_essential5_curav_
		ratio_SDPlow_essential5_ready_
		ratio_SDPlow_essential5_rnoso_
		noSDPany if approach>=3, 
			by(approachgroup, row(1) note("") legend(pos(6)) )
			ytitle("Ratio: alternative vs. current approach")
			ylab(0.9 (0.02) 1.04)
			yline(1, lcolor(black))
			xtitle("% of women without linked SDPs")
			legend(row(1) size(small) stack 
					label(1 "5 methods" "offered")
					label(2 "+ currently" "in-stock")
					label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
					label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr		

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle fillcolor(white) editcopy
		gr_edit .plotregion1.subtitle[2].style.editstyle linestyle(color(none)) editcopy

		
		graph save graph_sensitivity_ratio_pattern_low.gph, replace
		graph save graph_sensitivity_ratio_pattern_low.png, replace			
	
	********** LEVEL 
	
	#delimit; 
	graph box diff_SDPall*, 
		by(approachgroup, row(1) note("") legend(pos(6)) )
		ytitle("% point difference: alternative vs. current approach")
		ylab(-5 (1) 2)
		legend(row(1) size(small) stack 
				label(1 "5 methods" "offered")
				label(2 "+ currently" "in-stock")
				label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
				label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr				

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle fillcolor(none) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle linestyle(color(none)) editcopy
	
		graph save graph_sensitivity_diff_all.gph, replace
		graph save graph_sensitivity_diff_all.png, replace
		
	#delimit; 
	graph box ratio_SDPall*, 
		by(approachgroup, row(1) note("") legend(pos(6)) )
		ytitle("Ratio: alternative vs. current approach")
		ylab(0.9 (0.02) 1.04)
		legend(row(1) size(small) stack 
				label(1 "5 methods" "offered")
				label(2 "+ currently" "in-stock")
				label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
				label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr				

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle fillcolor(none) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle linestyle(color(none)) editcopy
	
		graph save graph_sensitivity_ratio_all.gph, replace		
		graph save graph_sensitivity_ratio_all.png, replace		
		
	#delimit; 
	graph box diff_SDPlow*, 
		by(approachgroup, row(1) note("") legend(pos(6)) )
		ytitle("% point difference: alternative vs. current approach")
		ylab(-10 (5) 5)
		legend(row(1) size(small) stack 
				label(1 "5 methods" "offered")
				label(2 "+ currently" "in-stock")
				label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
				label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr				

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle fillcolor(none) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle linestyle(color(none)) editcopy
	
		graph save graph_sensitivity_diff_low.gph, replace
		graph save graph_sensitivity_diff_low.png, replace
		
	#delimit; 
	graph box ratio_SDPlow*, 
		by(approachgroup, row(1) note("") legend(pos(6)) )
		ytitle("Ratio: alternative vs. current approach")
		ylab(0.8 (0.05) 1.2)
		legend(row(1) size(small) stack 
				label(1 "5 methods" "offered")
				label(2 "+ currently" "in-stock")
				label(3 "+ a trained provider/supplies" "in place for inserting/removing" "implants and IUDs") 
				label(4 "+ no stock-out" "in the past 3 months") )
		;
		#delimit cr				

		gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle fillcolor(none) editcopy
		gr_edit .plotregion1.subtitle[4].style.editstyle linestyle(color(none)) editcopy
	
		graph save graph_sensitivity_ratio_low.gph, replace				
		graph save graph_sensitivity_ratio_low.png, replace

