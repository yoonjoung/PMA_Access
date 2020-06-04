* Explore potential indicators for "Access" 
*	THIS IS ONLY FOR THE PSYCHOSOCIAL DOMAIN, using PMA WGE questions 
* 	Refer to the mapping doc: "Mapping_PMAQuestionsForAccess_2020 04 27.doc"  

* After identifying indicators, bring them to back to the main do file: "Effective_Access_Indicators_IR.do"

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
* 	C. Exploratory Data Analysis 

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
	
global surveylist "BFR7 KER8 NGKanoR6 NGLagosR6 "
*global surveylist "BFR7 KER8"
	
	
************************************************************************
* B. PREP women-level access variables  
************************************************************************

*****1. Psycosocial 
* check variables 
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
		codebook partner_overall why_not_decision  
	}
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
		tab xsurvey
		lookfor confident
	}
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear		
		sum round wge* 
	}
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
		sum round fp_promiscuous_view - fp_lifestyle_self 
	}

	
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
		sum round wge_decide_start_none - wge_negotiate_stop wge_switch_fp wge_confident_switch
	}	
	
set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	
	tab xsurvey
	tab	wge_decide_start_none wge_decide_start, m
	tab wge_partner_talk_start wge_decide_another, m
	tab wge_negotiate_stop_none wge_negotiate_stop, m
	}	
		

set more off
foreach survey in $surveylist{
	use "$data/IR_`survey'.dta", clear
	
		gen byte xdec_users	= (partner_overall==1 | partner_overall==3)
		gen byte xdec_nonusers	= (why_not_decision==1 | why_not_decision==3)
		gen byte xdec = xdec_users==1 | xdec_nonusers ==1
			
			tab xdec_users xdec_nonusers, m
		
		*replace xdec_users=. if mcp==0
		*replace xdec_nonusers=. if mcp==1
		
		lab var xdec_users "using FP is my/joint decision"
		lab var xdec_nonusers "NOT using FP is my/joint decision"
		lab var xdec "using or not using FP is my/joint decision"
		
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

		gen byte xxwge_fp_exercise1=(wge_switch_fp==5)
		gen byte xxwge_fp_exercise2=(wge_confident_switch==5)

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
		

*****7. BASIC var
	
		foreach var of varlist wealthquintile ur school {
		replace `var'=. if `var'<0
		}	
		
		gen byte xeverbirth=ever_birth==1
		gen byte xwealth5=wealthquintile
		gen byte xtop3=wealthquintile>=3 & wealthquintile!=.
		gen byte xurban=ur==1
		gen byte xedu_never	=school==0
		gen byte xedu_pri	=school==1
		gen byte xedu_sec	=school>=2 & school!=.
		gen byte xedu3=school
			replace xedu3=2 if school>=2 & school!=.
	
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
		egen year= mode(tempyear)
	
		lab var xtop3 "poor (bottom two quintiles) vs. non-poor (1)"
		lab var xurban "rural vs. urban (1) "
		lab var xedu_sec "ever attended: none or primary vs. secondary or higher+ (1)"
		
		lab var cmc "Interviw date in CMC, median per survey round"
		lab var year "Interviw year, mode per survey round"
	
		replace xdec_users=. if mcp==0
		replace xdec_nonusers=. if mcp==1
	
	
	keep FQmetainstance FQweight EA_ID strata round year cmc mcp x* 
	
save IR_`survey'_Access_Indicators.dta, replace 	
}

OKAY HERE 
*/

************************************************************************
* C. EDA
************************************************************************

set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear
	
	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 
	
		tab xsurvey
		svy: tab mcp xurban, col
		svy: tab xedu_sec xurban, col
		svy: tab xtop3 xurban, col
		svy: tab xeverbirth xurban, col
}	

set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear
		
		tab xsurvey
		sum mcp xedu_sec xtop3 xurban xdec* xwge_preg* xwge_fp*
		*pwcorr mcp xdec* xwge*
}		

* What are levels? 
set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear

	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 

		tab xsurvey
		svy: mean xdec_users
		svy: mean xdec_nonusers
		svy: mean mcp xdec xwge_preg* xwge_fp*
	
}

* Does the indicator vary by SES?
set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear

	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 
	tab xsurvey, m
	foreach var of varlist mcp xdec xwge_preg* xwge_fp*{
		svy: tab `var' xedu_sec, col
		}
}

set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear

	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 
	tab xsurvey, m
	foreach var of varlist mcp xdec xwge_preg* xwge_fp*{
		svy: tab `var' xtop3, col
		}
}	

set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear

	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 
	tab xsurvey, m
	foreach var of varlist mcp xdec xwge_preg* xwge_fp*{
		svy: tab `var' xurban, col
		}
}

log using check.log
set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear

	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 
	tab xsurvey, m
	foreach var of varlist mcp xdec xwge_preg* xwge_fp*{
		svy: tab `var' xedurban, col
		svy: tab `var' xedurban if xeverbirth==0, col
		svy: tab `var' xedurban if xeverbirth==1, col	
		}
}
log close





set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear

	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 

		tab xsurvey
		svy: mean mcp xdec xwge_preg* xwge_fp* if xedu3==0
		svy: mean mcp xdec xwge_preg* xwge_fp* if xedu3==1
		svy: mean mcp xdec xwge_preg* xwge_fp* if xedu3==2
		
		svy: mean mcp xdec xwge_preg* xwge_fp* if xtop3==0
		svy: mean mcp xdec xwge_preg* xwge_fp* if xtop3==1
		
		svy: mean mcp xdec xwge_preg* xwge_fp* if xurban==0
		svy: mean mcp xdec xwge_preg* xwge_fp* if xurban==1
}


* Does MCPR vary by indicator? 
set more off
foreach survey in $surveylist{
use IR_`survey'_Access_Indicators.dta, clear

	svyset EA_ID, weight(FQweight) strata(strata) , singleunit(centered) 

		tab xsurvey
		foreach indicator of varlist xdec xwge_preg* xwge_fp*{
		svy: tab mcp `indicator' , col
		}
}

