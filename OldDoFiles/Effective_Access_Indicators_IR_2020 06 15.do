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

* Table of Contents
* 	A. SETTING 
*		(NOTE: earlier surveys do not have "ever heard of" questions) 
* 	B. PREP create women-level access variables  
*		1. Psycosocial =>see at the end of the section
*		2. Cognitive
*		3. Geographic accessibility  
*		4. Service quality 
*		5. Administrative: N/A for women data
*		6. Affordability 	
*		7. BASIC var
*		8. MERGE with the EA-SDP data, created from "Effective_Access_Pop_SDP_`counry'.do". 
*			- ONLY USING PUBLIC DATA, thus set $surveylistEASDPLINK under SETTING 
*		1. Psycosocial: var changed over time  
*			- ONLY using $surveylistNEWER (later PMA2020 surveys)
*			- ONLY using $surveylistNEW (PAM surveys)
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
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7
	ETR4 ETR5 ETR6 
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER3 KER4 KER5 KER6 KER7 KER8 
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 	
	NGKanoR3  NGKanoR4  NGKanoR5  NGKanoR6 
	NGLagosR3 NGLagosR4 NGLagosR5 NGLagosR6 
	UGR3 UGR4 UGR5 UGR6 
	";
	#delimit cr
	
#delimit;
global surveylistminusone " 
	BFR3 BFR4 BFR5 BFR6 BFR7 
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7
	ETR4 ETR5 ETR6 
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER3 KER4 KER5 KER6 KER7 KER8 
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 	
	NGKanoR3  NGKanoR4  NGKanoR5  NGKanoR6 
	NGLagosR3 NGLagosR4 NGLagosR5 NGLagosR6 
	UGR3 UGR4 UGR5 UGR6 
	";
	#delimit cr

#delimit;
global surveylistEASDPLINK " 
	BFR3 BFR4 BFR5 BFR6  
	CIR1 CIR2
	CDKinshasaR3 CDKinshasaR4 CDKinshasaR5 CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR4 CDKongoCentralR5 CDKongoCentralR6 CDKongoCentralR7	
	ETR4 ETR5 ETR6 
	INRajasthanR1 INRajasthanR2 INRajasthanR3 INRajasthanR4 
	KER3 KER4 KER5 KER6 KER7
	NENiameyR1 NENiameyR2 NENiameyR3 NENiameyR4 NENiameyR5
	NER2 NER4 	
	NGKanoR3  NGKanoR4  NGKanoR5   
	NGLagosR3 NGLagosR4 NGLagosR5  
	UGR3 UGR4 UGR5 UGR6 
	";
	#delimit cr
	

************************************************************************
* B. PREP women-level access variables  
************************************************************************

*****1. Denominator 
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	tab xsurvey 
	lookfor intention
	}


*****1. Psycosocial => see at the end of this section, done in two different ways for newer vs. new surveys 
	
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
	sum round  wealthquintile ur school FQweight marital_status
	}
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	tab school xsurvey, m
	}
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	sum round
	d EA_ID strata
	}
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	tab unmet xsurvey, m
	}
		
	
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear	
		capture confirm variable ever_birth
			if !_rc {
			*tab xsurvey /*surveys WITH this var*/
			*sum why_not_decision partner_overall 
			} 
			else{
				tab xsurvey /*surveys without this var*/
				}
	}
	
set more off
foreach survey in $surveylist{
	use IR_`survey'_Access_Indicators.dta, clear
	
		foreach var of varlist wealthquintile ur school marital_status   {
		replace `var'=. if `var'<0
		}	
		gen countrycode=substr(xsurvey, 1, length(xsurvey)-2)
		
		gen byte xd_use  	=unmet>=3 & unmet<=4
 		gen byte xd_unmet	=unmet>=1 & unmet<=2
 		gen byte xd_demand	=unmet>=1 & unmet<=4
		gen byte xd_nodemand			=unmet>=5 | unmet<1
		gen byte xd_nodemand_infecund	=unmet==9
		gen byte xd_nodemand_notSA		=unmet==97 | unmet==-97
		
		foreach var of varlist xd_*   {
		replace `var'=. if unmet==99 | unmet==-99
		}	
		
		lab var xd_use "women category: use"
		lab var xd_unmet "women category: unmet need"
		lab var xd_demand "women category: demand for FP"
		lab var xd_nodemand "women category: no demand for FP"
		lab var xd_nodemand_infecund "women category: no demand for FP, infecund"
		lab var xd_nodemand_notSA 	 "women category: no demand for FP, not SA"
		
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

* check variables - find NEWER surveys
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear	
		capture confirm variable partner_overall
			if !_rc {
			tab xsurvey /*surveys WITH this var*/
			sum why_not_decision partner_overall 
			} 
			else{
				*tab xsurvey /*surveys without this var*/
				}
	}
	
* check variables 	
#delimit;
global surveylistNEWER " 
	BFR4 BFR5 BFR6 
	CIR1 CIR2 
	CDKinshasaR6 CDKinshasaR7
	CDKongoCentralR6 CDKongoCentralR7
	ETR5 ETR6 
	INRajasthanR2 INRajasthanR3 INRajasthanR4
	KER5 KER6 KER7 
	NENiameyR3 NENiameyR4 NENiameyR5
	NER4
	NGLagosR4 NGLagosR5 
	NGKanoR4  NGKanoR5  
	UGR5 UGR6 

	";
	#delimit cr
	
set more off
foreach survey in $surveylistNEWER{
	use "$data/IR_`survey'.dta", clear
		sum partner_overall why_not_decision  
	}
	
set more off
foreach survey in $surveylistNEWER{
	use IR_`survey'_Access_Indicators.dta, clear

		gen byte xdec_users	= (partner_overall==1 | partner_overall==3)
		gen byte xdec_nonusers	= (why_not_decision==1 | why_not_decision==3)
		gen byte xdec = xdec_users==1 | xdec_nonusers ==1
		
		replace xdec_users=. if mcp==0
		replace xdec_nonusers=. if mcp==1
		
		lab var xdec_users "using FP is my/joint decision"
		lab var xdec_nonusers "NOT using FP is my/joint decision"
		lab var xdec "using or not using FP is my/joint decision"
		
	save IR_`survey'_Access_Indicators.dta, replace
	}	
	
*****1. Psycosocial  - NEW 2.0 surveys  

#delimit;
global surveylistNEW " 
	BFR7 KER8 NGLagosR6 NGKanoR6
	";
	#delimit cr	
	
* check variables 
set more off
foreach survey in $surveylistNEW{
	use "$data/IR_`survey'.dta", clear
		codebook partner_overall why_not_decision  
	}
set more off
foreach survey in $surveylistNEW{
	use "$data/IR_`survey'.dta", clear
		sum round wge* 
	}
set more off
foreach survey in $surveylistNEW{
	use "$data/IR_`survey'.dta", clear
		/*
		#delimit; 
		foreach var of varlist 
			fp_promiscuous_self fp_married_self fp_no_child_self fp_lifestyle_self 
			fp_promiscuous_view fp_married_view fp_no_child_view fp_lifestyle_view {;
			#delimit cr   
		replace `var'=. if `var'<0	
		}
		*/
		sum round fp_promiscuous_view - fp_lifestyle_self 
	}
	
set more off
foreach survey  in $surveylistNEW{
	use IR_`survey'_Access_Indicators.dta, clear
	
		gen byte xeverbirth=ever_birth==1
		
		gen byte xdec_users	= (partner_overall==1 | partner_overall==3)
		gen byte xdec_nonusers	= (why_not_decision==1 | why_not_decision==3)
		gen byte xdec = xdec_users==1 | xdec_nonusers ==1
		
		replace xdec_users=. if mcp==0
		replace xdec_nonusers=. if mcp==1
		
		lab var xdec_users "using FP is my/joint decision"
		lab var xdec_nonusers "NOT using FP is my/joint decision"
		lab var xdec "using or not using FP is my/joint decision"

		/*
		#delimit; 
		foreach var of varlist 
			fp_promiscuous_self fp_married_self fp_no_child_self fp_lifestyle_self 
			fp_promiscuous_view fp_married_view fp_no_child_view fp_lifestyle_view {;
			#delimit cr   
			replace `var'=. if `var'<0	
			}
		*/
		gen byte xfp_self_pro	=fp_promiscuous_self==1 
		gen byte xfp_self_mar	=fp_married_self==1 
		gen byte xfp_self_nul	=fp_no_child_self==1
		gen byte xfp_self_life	=fp_lifestyle_self==4

		egen temp=rowtotal(xfp_self_pro xfp_self_mar xfp_self_nul )
		gen byte xfp_self3 = temp==3
			drop temp
			
		egen temp=rowtotal(xfp_self_pro xfp_self_mar xfp_self_nul xfp_self_life)
		gen byte xfp_self4 = temp==4
			drop temp
			
		gen byte xxfp_self_pro	=fp_promiscuous_self==1 | fp_promiscuous_self==2
		gen byte xxfp_self_mar	=fp_married_self==1 | fp_married_self==2
		gen byte xxfp_self_nul	=fp_no_child_self==1 | fp_no_child_self==2 
		gen byte xxfp_self_life	=fp_lifestyle_self==4 | fp_lifestyle_self==3
	
		egen temp=rowtotal(xxfp_self_pro xxfp_self_mar xxfp_self_nul )
		gen byte xxfp_self3 = temp==3
			drop temp
			
		egen temp=rowtotal(xxfp_self_pro xxfp_self_mar xxfp_self_nul xxfp_self_life)
		gen byte xxfp_self4 = temp==4
			drop temp			
		
		lab var xfp_self_pro "FP opinion, promiscuous: St disagree"
		lab var xfp_self_mar "FP opinion, only for married: St disagree"
		lab var xfp_self_nul "FP opinion, only for those with kids: St disagree"
		lab var xfp_self_life "FP opinion, better quality life: St agree"
		
		lab var xxfp_self_pro "FP opinion, promiscuous: St disagree + disagree"
		lab var xxfp_self_mar "FP opinion, only for married: St disagree + disagree"
		lab var xxfp_self_nul "FP opinion, only for those with kids: St disagree + disagree"
		lab var xxfp_self_life "FP opinion, better quality life: St agree + agree"		
		
		#delimit; 
		gen byte xwge_preg_exercise1=	(wge_decide_start_none ==5) |
										(wge_decide_start ==5); 
		gen byte xwge_preg_exercise2=	(wge_partner_talk_start ==5) |
										(wge_decide_another ==5) ; 
		gen byte xwge_preg_exercise3=	(wge_negotiate_stop_none ==5) |
										(wge_negotiate_stop ==5) ; 
		#delimit cr
		
		egen temp=rowtotal(xwge_preg_*)
		gen byte xwge_preg_exercise = temp==3
			drop temp

		gen byte xwge_fp_exercise1=(wge_switch_fp==5)
		gen byte xwge_fp_exercise2=(wge_confident_switch==5)

		egen temp=rowtotal(xwge_fp_*)
		gen byte xwge_fp_exercise = temp==2
			drop temp			
		
		lab var xwge_preg_exercise1 "WGE, pregancy: strongly agree: I can decide when to start/next" 
		lab var xwge_preg_exercise2 "WGE, pregancy: strongly agree: I can discuss when to start/next"
		lab var xwge_preg_exercise3 "WGE, pregancy: strongly agree: I can negotiate when to stop"	
		lab var xwge_preg_exercise "WGE, pregancy: strongly agree: all three"	

		lab var xwge_fp_exercise1 "WGE, FP: strongly agree: i can switch"
		lab var xwge_fp_exercise2 "WGE, FP: strongly agree: i can talk to HW to switch"
		lab var xwge_fp_exercise "WGE, FP: strongly agree: both"		
		
		
		#delimit; 
		gen byte xxwge_preg_exercise1=	(wge_decide_start_none ==5 | wge_decide_start_none ==4 ) |
										(wge_decide_start ==5 | wge_decide_start ==4); 
		gen byte xxwge_preg_exercise2=	(wge_partner_talk_start ==5 | wge_partner_talk_start ==4) |
										(wge_decide_another ==5 | wge_decide_another ==4) ; 
		gen byte xxwge_preg_exercise3=	(wge_negotiate_stop_none ==5 | wge_negotiate_stop_none ==4) |
										(wge_negotiate_stop ==5 | wge_negotiate_stop ==4) ; 
		#delimit cr
		
		egen temp=rowtotal(xxwge_preg_*)
		gen byte xxwge_preg_exercise = temp==3
			drop temp

		gen byte xxwge_fp_exercise1=(wge_switch_fp==5 | wge_switch_fp==4)
		gen byte xxwge_fp_exercise2=(wge_confident_switch==5 | wge_confident_switch==4)

		egen temp=rowtotal(xxwge_fp_*)
		gen byte xxwge_fp_exercise = temp==2
			drop temp			
		
		lab var xxwge_preg_exercise1 "WGE, pregancy: >= agree: I can decide when to start/next" 
		lab var xxwge_preg_exercise2 "WGE, pregancy: >= agree: I can discuss when to start/next"
		lab var xxwge_preg_exercise3 "WGE, pregancy: >= agree: I can negotiate when to stop"	
		lab var xxwge_preg_exercise "WGE, pregancy: >= agree: all three"	

		lab var xxwge_fp_exercise1 "WGE, FP: >= agree: i can switch"
		lab var xxwge_fp_exercise2 "WGE, FP: >= agree: i can talk to HW to switch"
		lab var xxwge_fp_exercise "WGE, FP: >= agree: both"	
	
	save IR_`survey'_Access_Indicators.dta, replace
	}	


*OKAY DATA READY FOR ANALYSIS 
*/

	
************************************************************************
* C. Create summary dataset 
************************************************************************

#delimit; 
global indicatorlist "
	
	xdec
	xwge_preg_exercise1
	xwge_preg_exercise2
	xwge_preg_exercise3
	xwge_preg_exercise
	xwge_fp_exercise1
	xwge_fp_exercise2
	xwge_fp_exercise
	xxwge_preg_exercise1
	xxwge_preg_exercise2
	xxwge_preg_exercise3
	xxwge_preg_exercise
	xxwge_fp_exercise1
	xxwge_fp_exercise2
	xxwge_fp_exercise	
	xfp_self*
	xxfp_self*
	
	xheard_10
	xheard_7
	xheard_5
	xheard_select6
	xheard_select5
	xinsurance
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

	xdec_users
	xdec_nonusers
	xdec
	xwge_preg_exercise1
	xwge_preg_exercise2
	xwge_preg_exercise3
	xwge_preg_exercise
	xwge_fp_exercise1
	xwge_fp_exercise2
	xwge_fp_exercise
	xxwge_preg_exercise1
	xxwge_preg_exercise2
	xxwge_preg_exercise3
	xxwge_preg_exercise
	xxwge_fp_exercise1
	xxwge_fp_exercise2
	xxwge_fp_exercise	
	xfp_self*
	xxfp_self*
	
	xheard_10
	xheard_7
	xheard_5
	xheard_select6	
	xheard_select5
	xinsurance
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
	
	xmii_side
	xmii_sidewhat
	xmii_other
	xmii_switch
	xmii3
	xmii4
	";
	#delimit cr	

global covlist "xeverbirth xtop3 xurban xedu_sec xinunion"

**************************************** AMONG ALL women 

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
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
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
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
	}
	
	use temp.dta, clear
	foreach cov of varlist xedurban{
	use temp.dta, clear
	keep if `cov'==1	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="1"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==2	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="2"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==3	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="3"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==4	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="4"
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 			
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
		append using summary_Access_Indicators_IR.dta, 	
		save summary_Access_Indicators_IR.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round cmc)
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
		collapse (count) obs , by(xsurvey)
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR_obs.dta, replace 	
	
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
	}
	
	use temp.dta, clear
	foreach cov of varlist xedurban{
	use temp.dta, clear
	keep if `cov'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="1"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==2	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="2"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==3	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="3"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==4	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="4"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 			
	}	
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_obs.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_obs.dta, replace 		
	}	

***** Merge weighted estimates and unweighted number of observations 
	use summary_Access_Indicators_IR.dta, clear
		codebook xsurvey
	use summary_Access_Indicators_IR_obs.dta, clear
		codebook xsurvey
	
	use summary_Access_Indicators_IR_obs.dta, clear
		sort xsurvey group grouplabel	
		merge xsurvey group grouplabel	using summary_Access_Indicators_IR.dta
			tab _merge
			drop _merge
			
		tab xsurvey if group=="All"	

		gen groupdemand=0
		lab	var	groupdemand "only women with demand"
		
	save summary_Access_Indicators_IR.dta, replace
		
**************************************** AMONG women with DEMAND 

use IR_BFR2_Access_Indicators.dta, clear
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
		save summary_Access_Indicators_IR_demand.dta, replace 	
		
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
		append using summary_Access_Indicators_IR_demand.dta, 	
		save summary_Access_Indicators_IR_demand.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_demand.dta, 	
		save summary_Access_Indicators_IR_demand.dta, replace 	
	}
	
	use temp.dta, clear
	foreach cov of varlist xedurban{
	use temp.dta, clear
	keep if `cov'==1	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="1"
		append using summary_Access_Indicators_IR_demand.dta, 	
		save summary_Access_Indicators_IR_demand.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==2	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="2"
		append using summary_Access_Indicators_IR_demand.dta, 	
		save summary_Access_Indicators_IR_demand.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==3	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="3"
		append using summary_Access_Indicators_IR_demand.dta, 	
		save summary_Access_Indicators_IR_demand.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==4	
		collapse  (mean) mcp $indicatorlistall [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp $indicatorlistall {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="4"
		append using summary_Access_Indicators_IR_demand.dta, 	
		save summary_Access_Indicators_IR_demand.dta, replace 			
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
		append using summary_Access_Indicators_IR_demand.dta, 	
		save summary_Access_Indicators_IR_demand.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse  (mean) mcp [pw=FQweight], by(xsurvey round cmc)
			foreach var of varlist mcp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_demand.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_demand.dta, replace 		
	}	


***** Unweighted number of observation

	use temp.dta, clear
		collapse (count) obs , by(xsurvey)
			gen group="All"	
			gen grouplabel="All"
		save summary_Access_Indicators_IR_obs.dta, replace 	
	
	use temp.dta, clear
	foreach cov of varlist $covlist{
	use temp.dta, clear
	keep if `cov'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
	}
	
	use temp.dta, clear
	foreach cov of varlist xedurban{
	use temp.dta, clear
	keep if `cov'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="1"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==2	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="2"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==3	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="3"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==4	
		collapse (count) obs , by(xsurvey)
			gen group="By `cov'"	
			gen grouplabel="4"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 			
	}	
				
	use temp.dta, clear				
	foreach indicator of varlist $indicatorlist{
	use temp.dta, clear	
	keep if `indicator'==0	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="No"
		append using summary_Access_Indicators_IR_obs.dta, 	
		save summary_Access_Indicators_IR_obs.dta, replace 	
	
	use temp.dta, clear
	keep if `indicator'==1	
		collapse (count) obs , by(xsurvey)
			gen group="By `indicator'"		
			gen grouplabel="Yes"
		append using summary_Access_Indicators_IR_obs.dta, 	
		
		sort xsurvey group grouplabel
		save summary_Access_Indicators_IR_obs.dta, replace 		
	}	

***** Merge weighted estimates and unweighted number of observations 
	use summary_Access_Indicators_IR_demand.dta, clear
		codebook xsurvey
	use summary_Access_Indicators_IR_obs.dta, clear
		codebook xsurvey
	
	use summary_Access_Indicators_IR_obs.dta, clear
		sort xsurvey group grouplabel	
		merge xsurvey group grouplabel	using summary_Access_Indicators_IR_demand.dta
			tab _merge
			drop _merge
			
		tab xsurvey if group=="All"	
		
		gen groupdemand=1
		lab	var	groupdemand "only women with demand"
		
	save summary_Access_Indicators_IR_demand.dta, replace
	
**************************************** APPEND BOTH DATA SETS 

	use summary_Access_Indicators_IR.dta, replace
	append using summary_Access_Indicators_IR_demand.dta, 
	
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

	foreach var of varlist xinsurance xmii_switch xmii4{	
		replace `var'=. if year<2019 & xsurvey!="BFR6"
		}
		
	*PROBLEMATIC surveys with EA_SDP link
	foreach var of varlist SDPall* {
		replace `var'=. if (xsurvey=="NENiameyR5" | xsurvey=="NGKanoR3" | xsurvey=="BFR6")
		}		
				
	* CHECK small n : SHOULD NOT BE an issue in IR in most cases
	sum obs	
	sort groupdemand xsurvey
	list xsurvey groupdemand group grouplabel obs if obs<=20
	
		foreach var of varlist xdec_users - xmii4 {
			replace `var'=. if obs<=20
			}
	
	sort xsurvey group grouplabel
	save summary_Access_Indicators_IR.dta, replace 	
	
	export delimited using summary_Access_Indicators_IR.csv, replace
	* save in additional folders for apps
	export delimited using ShinyAppAccess/summary_Access_Indicators_IR.csv, replace
	export delimited using ShinyAppPopAccessToMethods/summary_Access_Indicators_IR.csv, replace
	export delimited using ShinyAppPsychosocial/summary_Access_Indicators_IR.csv, replace
		
erase temp.dta		
erase summary_Access_Indicators_IR_obs.dta

set more off
foreach survey in $surveylist{
	erase IR_`survey'_Access_Indicators.dta 
}

OKAY Summary DATA READY FOR ANALYSIS and Shiny App

*/

use  summary_Access_Indicators_IR.dta, clear
keep if group=="All" & groupdemand==0
bysort country: tab xsurvey cmc

use  summary_Access_Indicators_IR.dta, clear
sum xinsurance