* Explore potential indicators for "Access" 
* CLIENT level
* Kenya as an example
* Refer to the mapping doc: "Mapping_PMAQuestionsForAccess_2020 02 25.doc"  

* Table of Contents
* 	A. SETTING 
* 	A.2 Non-public data for consultancy
* 	B. PREP create client-level access variables  
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
	KER8 BFR7 NGLagosR6 NGKanoR6
	";
	#delimit cr
	
#delimit;
global surveylistminusone " 
	BFR7 NGLagosR6 NGKanoR6
	";
	#delimit cr

************************************************************************
* A.2 Non-public data for consultancy
************************************************************************
/*
cd "C:\Users\YoonJoung Choi\Dropbox\0 Data\PMA\NonPublicFiles\"
dir
*/
***** BFR7
use "$data/NonPublicFiles/BFP1_CQ_Clean_Data_with_checks_26May2020.dta", clear

		gen round=7
		gen phase=1
		egen EA_ID=group(EA)
		gen xsurvey ="BFR7"

	keep if cei_result	==1
	codebook metainstanceID facility_ID EA
	save "$data/CR_BFR7.dta", replace
	
***** NGLagosR6 NGKanoR6	
use "$data/NonPublicFiles/NGP1_CQ_NONAME_9Apr2020.dta", clear

		gen round=6
		gen phase=1
		egen EA_ID=group(EA)
	keep if cei_result	==1
	save temp.dta, replace
	
	use temp.dta, clear 
	keep if level1=="lagos"
		gen xsurvey ="NGLagosR6"

	codebook metainstanceID facility_ID EA
	save "$data/CR_NGLagosR6.dta", replace

	use temp.dta, clear 
	keep if level1=="kano"
		gen xsurvey ="NGKanoR6"

	codebook metainstanceID facility_ID EA
	save "$data/CR_NGKanoR6.dta", replace	
	
***** KER8	
use "$data/NonPublicFiles/KEP1_CQ_NONAME_17Mar2020.dta", clear
	codebook metainstanceID facility_ID EA
	
		gen round=8
		gen phase=1
		egen EA_ID=group(EA)
		gen xsurvey ="KER8"
	
		/*
		*need urban rural data from IR, but the current "EA_ID" is just a place holder
		*run this section when public datasets are available
		
		sort EA_ID
		merge EA_ID using temp.dta, <=EA-level data
		tab _merge ur, m
			
			keep if _merge==3
			drop_merge
			
		lab define ur 1"urban" 2"rural"	
		lab values ur ur
		*/	
	keep if cei_result	==1

	codebook metainstanceID facility_ID EA /*3930 clients, 251 EAs in 11 counties*/	
	save "$data/CR_KER8.dta", replace

*********************************************************
* B. PREP client-level access variables  
*********************************************************	

*****0. BASIC var

* check variables 
set more off
foreach survey in $surveylist{
	use "$data/CR_`survey'.dta", clear
	sum round age marital_status school hh_location_ladder fp_reason_yn
	codebook round age marital_status school hh_location_ladder fp_reason_yn
	}
		
set more off
foreach survey in $surveylist{
	use "$data/CR_`survey'.dta", clear
	
		foreach var of varlist  age marital_status school hh_location_ladder fp_reason_yn {
		replace `var'=. if `var'<0
		}	
	
		gen byte yfpclient=fp_reason_yn ==1
	
	keep if yfpclient==1
	
		gen yage=age
		egen yagegroup5 = cut(age), at(15,20,25,30,35,40,45,50)
		egen yagegroup10 = cut(age), at(15,20,30,40,50)
		gen byte yhhladder=hh_location_ladder
		*gen byte xurban=ur==1
		gen byte yedu_never	=school==1
		gen byte yedu_pri	=school==2
		gen byte yedu_sec	=school>=3 & school<=4
		gen byte yedu_col	=school>=5 & school!=.
		gen byte yedu4=. 
			replace yedu4=0 if yedu_never==1
			replace yedu4=1 if yedu_pri==1
			replace yedu4=2 if yedu_sec==1			
			replace yedu4=3 if yedu_col==1	
		lab define yedu4 0"none" 1"primary" 2"secondary" 3"college+"
		lab values yedu4 yedu4

		/*	
		gen temp = dofc(FQdoi_correctedSIF)
		format %td temp
		gen tempmonth = month(temp)
		gen tempyear = year(temp)
		gen tempcmc 	= 12*(tempyear - 1900) + tempmonth
		*/
		
		gen tempyear  = substr(today, 1, 4)
		gen tempmonth = substr(today, 6, 2)
		destring tempmonth, replace
		destring tempyear, replace
		gen tempcmc 	= 12*(tempyear - 1900) + tempmonth

		egen cmc = median(tempcmc)
		egen year= mode(tempyear)
			drop temp*	
			
		lab var yage "client's age at interview" 
		lab var yagegroup10 "client's age at interview, 10-year group" 		
		lab var yagegroup5 "client's age at interview, 5-year group" 		
		lab var yhhladder "HH percceived economic ladder"
		*lab var xurban "rural vs. urban (1) "
		lab var yedu_sec "ever attended: none or primary vs. secondary or higher+ (1)"
		
		lab var cmc "Interviw date in CMC, median per survey round"
		lab var year "Interviw year, mode per survey round"
						
	*keep 
	
save CR_`survey'_Access_Indicators.dta, replace 	
}

*****1. Psycosocial - N/A
*****2. Cognitive - N/A
*****3. Geographic accessibility - N/A  
*****5. Administrative - Not ideal 
*****6. Affordability - Not ideal

*****4. Service quality 

/*
* check variables 
set more off
foreach survey in $surveylist{
	use "$data/CR_`survey'.dta", clear
	sum round fp_obtain_desired fp_obtain_desired_whynot fp_first_desired
	codebook fp_obtain_desired fp_obtain_desired_whynot fp_first_desired

	tab fp_obtain_desired_whynot fp_obtain_desired , m
	tab fp_obtain_desired_whynot fp_first_desired , m
	
	tab whatgiven_today  fp_obtain_desired, m
	tab whatgiven_today, m 
	tab method_prescribed, m
	codebook method_prescribed 
	codebook method_prescribed if method_prescribed==5
	codebook method_prescribed if method_prescribed==6
	}
*/

set more off
foreach survey in $surveylist{
	use CR_`survey'_Access_Indicators.dta, clear
		
		#delimit;
		foreach var of varlist 
				fp_obtain_desired fp_obtain_desired_whynot
				pill_counsel inj_counsel
				explain_method-explain_follow_up
				discuss_other_fp-discuss_pro_con_delay 
				howclear_fp_info allow_question understand_answer
				how_staff_treat satisfied_services_today refer_hf return_to_facility
				{;
				#delimit cr
			replace `var'=. if `var'<0
			}
			
		* overall outcome				
		gen byte ywant= fp_obtain_desired==1 | fp_obtain_desired==3
		gen byte yrecommend= fp_obtain_desired_whynot>=4 & fp_obtain_desired_whynot<=6
			replace yrecommend=. if fp_obtain_desired!=2 
		gen byte ywantrec= ywant==1 | yrecommend==1
		
		* technical 
		gen byte ycounsel=pill_counsel==1 |  inj_counsel==1
			replace ycounsel=. if (method_prescribed!=5 & method_prescribed!=6) 
		egen xexplain_sum=rowtotal(explain_method-explain_follow_up) 
		gen byte yexplain_all=xexplain_sum==4
		egen ydiscuss_sum=rowtotal(discuss_other_fp-discuss_switch) 
		gen byte ydiscuss_procon=discuss_pro_con_delay==1 
		gen byte ydiscuss_all5=ydiscuss_sum==4 & ydiscuss_procon==1
		gen byte ydiscuss_all4=ydiscuss_sum==4
		
		* experiential 
		egen ycommunication_sum=rowtotal(howclear_fp_info allow_question understand_answer)
		gen byte ycommunication_all=ycommunication_sum==3
			foreach var of varlist xexplain* ydiscuss* ycommunication*  {
			replace `var' =. if whatgiven_today>=3 
			}
			/*
			*lot of heaping and data quality.... 
			codebook time_wait_h  time_wait_m			
			histogram time_wait_m , w(10)
			histogram time_wait_m if time_wait_m<=240, w(10)
			histogram time_wait_h , w(1)
			histogram time_wait_h if time_wait_h<=10, w(1)
			*/
		
		gen byte ywait15=time_wait_m>15
		gen byte ywait30=time_wait_m>30
		gen byte ywait60=time_wait_m>60
		gen byte ywait_onehour=time_wait_h>1
			
		gen byte ypolitevery	=how_staff_treat==1
		gen byte ypolite		=how_staff_treat<=2
		gen byte ysatisfiedvery	=satisfied_services_today==1
		gen byte ysatisfied		=satisfied_services_today<=2
		gen byte yrefer			=refer_hf==1
		gen byte yreturn		=return_to_facility==1	
		gen byte yreferreturn	=refer_hf==1 & return_to_facility==1
	
		lab var ywant 			"received initially wanted method" 
		lab var yrecommend 		"did not receive initially wanted method because of three potentially acceptable reasons"
		lab var ywantrec 		"received initially wanted method OR did not because of three potentially acceptable reasons" 
		
		lab var ycounsel 		"pill/injectables specific counseling on pregnancy chance if not taken correctly"
		lab var yexplain_all 	"explained on all four items"
		lab var ydiscuss_all5 	"discussed all five items"
		lab var ydiscuss_all4 	"discussed all four items"
		lab var ydiscuss_procon 	"discussed advantage/disadvantage of her method"
		
		lab var ycommunication_all 	"clear, allowed question, and understand"
		lab var ywait15			"waited more than 15 minutes"
		lab var ywait30			"waited more than 30 minutes"
		lab var ywait60			"waited more than 60 minutes"
		lab var ywait_onehour	"waited more than one hour (based on hour reporting)"
		lab var ypolitevery		"very polite"
		lab var ypolite			"polite or very polite"
		lab var ysatisfiedvery		"very satisfied"
		lab var ysatisfied			"satisfied or very satisfied"		
		lab var yrefer			"would refer frient/relative"
		lab var yreturn			"would return"		
		lab var yreferreturn	"would refer AND return"
				
save CR_`survey'_Access_Indicators.dta, replace 	
}	

*okay


*********************************************************
* C. Create summary dataset 
*********************************************************	
#delimit; 
global indicatoroutcome "
	ywant
	yrecommend
	ywantrec 
	";
	#delimit cr
	
#delimit; 
global indicatortech "
	ycounsel 
	yexplain_all 
	ydiscuss_all4 	
	ydiscuss_procon 
	ydiscuss_all5 
	";
	#delimit cr

#delimit; 
global indicatorexp "
	ycommunication_all 
	ywait15
	ywait30
	ywait60
	ywait_onehour
	ypolitevery
	ypolite
	ysatisfiedvery
	ysatisfied
	yrefer
	yreturn
	yreferreturn
	";
	#delimit cr	

global covlist "yedu4 yagegroup10"

***** Get estimates

	use CR_KER8_Access_Indicators.dta, clear
	foreach survey in $surveylistminusone{
		append using CR_`survey'_Access_Indicators.dta, force
		} 
		
	gen obs=1	
	save temp.dta, replace
	
	use temp.dta, clear
		
		sum  $indicatoroutcome $indicatortech $indicatorexp 
		
		collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp yedu_*, by(xsurvey round year cmc)
			foreach var of varlist  $indicatoroutcome $indicatortech $indicatorexp {
				replace `var'=round(`var'*100, 1)
				}
			gen group="All clients"	
			gen grouplabel="All clients"
		save summary_Access_Indicators_CR.dta, replace 	

	use temp.dta, clear
	foreach cov of varlist yedu_sec {
		
		keep if `cov'==0	
			collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp , by(xsurvey round year cmc)
				foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
					replace `var'=round(`var'*100, 1)
					}					
				gen group="By `cov'"	
				gen grouplabel="less than secondary"
			append using summary_Access_Indicators_CR.dta, 
			save summary_Access_Indicators_CR.dta, replace 	

		use temp.dta, clear
		keep if `cov'==1	
			collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp , by(xsurvey round year cmc)
				foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
					replace `var'=round(`var'*100, 1)
					}					
				gen group="By `cov'"	
				gen grouplabel="secondary or more"
			append using summary_Access_Indicators_CR.dta, 
			save summary_Access_Indicators_CR.dta, replace 				
	}		
		
	use temp.dta, clear
	foreach cov of varlist yedu4 {
		
		keep if `cov'==0	
			collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp , by(xsurvey round year cmc)
				foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
					replace `var'=round(`var'*100, 1)
					}					
				gen group="By `cov'"	
				gen grouplabel="none"
			append using summary_Access_Indicators_CR.dta, 
			save summary_Access_Indicators_CR.dta, replace 	
			
		use temp.dta, clear
		keep if `cov'==1	
			collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp , by(xsurvey round year cmc)
				foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
					replace `var'=round(`var'*100, 1)
					}					
				gen group="By `cov'"	
				gen grouplabel="primary"
			append using summary_Access_Indicators_CR.dta, 
			save summary_Access_Indicators_CR.dta, replace 		
			
		use temp.dta, clear
		keep if `cov'==2	
			collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp , by(xsurvey round year cmc)
				foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
					replace `var'=round(`var'*100, 1)
					}					
				gen group="By `cov'"	
				gen grouplabel="secondary"
			append using summary_Access_Indicators_CR.dta, 
			save summary_Access_Indicators_CR.dta, replace 				

		use temp.dta, clear
		keep if `cov'==3	
			collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp , by(xsurvey round year cmc)
				foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
					replace `var'=round(`var'*100, 1)
					}					
				gen group="By `cov'"	
				gen grouplabel="college+"
			append using summary_Access_Indicators_CR.dta, 
			save summary_Access_Indicators_CR.dta, replace 				
	}
	
	use temp.dta, clear
	foreach cov of varlist yagegroup10 {
	keep if `cov'==15	
		collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp, by(xsurvey round year cmc)
			foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="15-19"
		append using summary_Access_Indicators_CR.dta, 
		save summary_Access_Indicators_CR.dta, replace 	
		
	use temp.dta, clear
	keep if `cov'==20	
		collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp, by(xsurvey round year cmc)
			foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="20-29"
		append using summary_Access_Indicators_CR.dta, 
		save summary_Access_Indicators_CR.dta, replace 		
		
	use temp.dta, clear
	keep if `cov'==30	
		collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp, by(xsurvey round year cmc)
			foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="30-39"
		append using summary_Access_Indicators_CR.dta, 
		save summary_Access_Indicators_CR.dta, replace 				

	use temp.dta, clear
	keep if `cov'==40	
		collapse (count) obs (mean) $indicatoroutcome $indicatortech $indicatorexp, by(xsurvey round year cmc)
			foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			gen grouplabel="40 or above"
		append using summary_Access_Indicators_CR.dta, 
		save summary_Access_Indicators_CR.dta, replace 				
	}	

***** Further variables and data cleaning 

		replace group="Education of clients" 	if group=="By yedu4" 
		replace group="Age of clients" 			if group=="By yagegroup10"
		replace group="Education (CR)" 	if group=="By yedu_sec" 
				
	gen country=substr(xsurvey, 1, length(xsurvey)-2)
		replace country="Burkina Faso" 	if country=="BF"
		replace country="Ethiopia" 		if country=="ET"
		replace country="Kenya" 		if country=="KE"
		replace country="Nigeria, Kano" if country=="NGKano"
		replace country="Nigeria, Lagos" if country=="NGLagos"
		replace country="Uganda" 		if country=="UG"	

	egen temp=max(round), by(country) 
	gen latestCR=round==temp
		drop temp
	gen byte phase=year>=2019 
	gen month= cmc-12*(year - 1900)
	
	* CHECK small n  
	sum obs	
	list xsurvey group grouplabel obs if obs<=20
	foreach var of varlist ywant - yreferreturn {
		replace `var'=. if obs<=20
		}

	sort xsurvey group grouplabel
	save summary_Access_Indicators_CR.dta, replace 	
	
	export delimited using summary_Access_Indicators_CR.csv, replace
	* save in additional folders for apps
	export delimited using ShinyAppAccess/summary_Access_Indicators_CR.csv, replace
	export delimited using ShinyAppPopAccessToMethods/summary_Access_Indicators_CR.csv, replace
	export delimited using ShinyAppPsychosocial/summary_Access_Indicators_CR.csv, replace
		
	
OKAY Summary DATA READY FOR ANALYSIS and Shiny App

	/*
	use summary_Access_Indicators_IR.dta, replace 	
	sum round
	use summary_Access_Indicators_CR.dta, replace 	
	sum round
	*/


*/
*********************************************************
* D. ANALYSIS using the summary dataset 
*********************************************************			
#delimit; 
global indicatoroutcome "
	ywant
	ywantrec 
	";
	#delimit cr
	
#delimit; 
global indicatortech "
	ycounsel 
	yexplain_all 
	ydiscuss_all4 	
	ydiscuss_procon 
	ydiscuss_all5 
	";
	#delimit cr

#delimit; 
global indicatorexp "
	ycommunication_all 
	ywait15
	ywait30
	ypolitevery
	ypolite
	ysatisfiedvery
	ysatisfied
	yrefer
	yreturn
	yreferreturn
	";
	#delimit cr	
		
use CR_KER8_Access_Indicators.dta, clear
	codebook metainstanceID facility_ID EA
	tab facility_type managing_authority, m
	/*
	.         tab facility_type managing_authority, m

						  |             Managing authority
		 Type of facility | Governmen        NGO  Faith-bas    Private |     Total
	----------------------+--------------------------------------------+----------
				 Hospital |       885          0          0          5 |       890 
			Health center |     1,123          0          0          2 |     1,125 
			Health clinic |        51          1          0         47 |        99 
			   Dispensary |     1,218          0          0          0 |     1,218 
	Nursing / Maternity H |         0          0          2          1 |         3 
	----------------------+--------------------------------------------+----------
					Total |     3,277          1          2         55 |     3,335 

	*/		
capture putdocx clear 
putdocx begin

putdocx paragraph
putdocx text ("2. Access variables using client exit survey data"), linebreak bold 
putdocx text (""), linebreak
putdocx text ("Note"), linebreak bold
putdocx text ("- Working with 100% prelim data 0 March 17th version. In total, 3930 completed interview."), linebreak
putdocx text ("- 3335 FP clients, from 433 facilities. Mostly from public facilities."), linebreak
putdocx text ("- When EA ID is created for the public dataset, Each EA can have quality scores from CEI."), linebreak
putdocx text (""), linebreak
putdocx text ("2.1 Outcome "), bold
putdocx text ("(among all FP clients)"), linebreak bold	
putdocx text ("--ywant: received initially wanted method"), linebreak 
putdocx text ("--yrecommend: did not receive initially wanted method because of three potentially acceptable reasons"), linebreak
putdocx text ("--ywantrec: received initially wanted method OR did not because of three potentially acceptable reasons"), linebreak 
putdocx text ("2.2 Process quality "), bold
putdocx text ("2.2.1 Technical process quality"), bold
putdocx text ("(among FP clients who received methods/prescription)"), linebreak bold
putdocx text ("--ycounsel: pill/injectables specific counseling on pregnancy chance if not taken correctly"), linebreak	
putdocx text ("--yexplain_all: explained on all four items"), linebreak		
putdocx text ("--ydiscuss_all4: discussed all four items"), linebreak		
putdocx text ("--ydiscuss_procon: discussed advantage/disadvantage of her method"), linebreak		
putdocx text ("--ydiscuss_all5: discussed all five items"), linebreak		
putdocx text ("2.2.2. Experiential process quality "), bold
putdocx text ("(among all FP clients)"), linebreak bold			
putdocx text ("--ycommunication_all : clear, allowed question, and understand"), linebreak		
putdocx text ("--ywait30: waited more than 30 minutes"), linebreak
putdocx text ("--ywait60: waited more than 60 minutes"), linebreak
putdocx text ("--ywait_onehour: waited more than one hour (based on hour reporting)"), linebreak		
putdocx text ("--ypolitevery: very polite"), linebreak	
putdocx text ("--ypolite: polite or very polite"), linebreak
putdocx text ("--ysatisfiedvery: very satisfied"), linebreak	
putdocx text ("--ysatisfied: satisfied or very satisfied"), linebreak
putdocx text ("--yrefer: would refer frient/relative"), linebreak
putdocx text ("--yreturn: would return"), linebreak
putdocx text ("--yreferreturn: would refer AND return"), linebreak	
putdocx text (""), linebreak
putdocx text ("List of figures"), linebreak bold
putdocx text ("Figure 1. Level of potential indicators"), linebreak 
putdocx text ("Figure 2. Level of potential indicators by background"), linebreak
putdocx text (""), linebreak
putdocx text ("Summary"), linebreak bold
putdocx text ("1. Universally high outcome."), linebreak
putdocx text ("2. Technical quality indicators tend to be higher among more educated group"), linebreak
putdocx text ("3. Self-reported experiential quality indicators are problematic. Universally high. "), 
putdocx text ("Still, some have correlation with clients' education level (i.e., communication, "very" satisfied/polite."), linebreak
putdocx text ("4. Waiting time is short and does not vary with education."), linebreak
putdocx text ("Question: what's the plan with ladder data?"), linebreak

putdocx pagebreak 
putdocx paragraph
putdocx text ("Figure 1. Level of potential indicators"), linebreak bold 			
	
	use summary_Access_Indicators_CR.dta, clear
	gen dummy=.
		#delimit; 
		graph bar $indicatoroutcome dummy $indicatortech if xsurvey=="KER8" & group=="All", 
			blabel(bar)			
			legend(pos(3) col(1) size(small)
				label(1 "ywant")
				label(2 "ywantrec")	
				label(3 "")	
				label(4 "ycounsel") 
				label(5 "yexplain_all") 
				label(6 "ydiscuss_all4") 	
				label(7 "ydiscuss_procon") 
				label(8 "ydiscuss_all5") 
			)
			bar(1, bcolor(cranberry*1.0))
			bar(2, bcolor(cranberry*0.8))
			bar(3, bcolor(white))
			bar(4, bcolor(dkorange*0.8))
			bar(5, bcolor(dkgreen*0.8))
			bar(6, bcolor(navy*0.8))
			bar(7, bcolor(navy*0.6))
			bar(8, bcolor(navy*0.4))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(4)
			title("Kenya Phase 1 (2019): outcome and technical process")
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		putdocx paragraph
		putdocx image graph.png		

		#delimit; 
		graph bar $indicatorexp if xsurvey=="KER8" & group=="All", 
			blabel(bar)			
			legend(pos(3) col(1) size(small)
				label(1 "ycommunication_all") 
				label(2 "ywait15")
				label(3 "ywait30")
				label(4 "ypolitevery")
				label(5 "ypolite")
				label(6 "ysatisfiedvery")
				label(7 "ysatisfied")
				label(8 "yrefer")
				label(9 "yreturn")
				label(10 "yreferreturn")
			)
			bar(1, bcolor(lavender))
			bar(2, bcolor(cranberry*0.8))
			bar(3, bcolor(cranberry*0.6))
			bar(4, bcolor(dkorange*0.8))
			bar(5, bcolor(dkorange*0.6))
			bar(6, bcolor(dkgreen*0.8))
			bar(7, bcolor(dkgreen*0.6))
			bar(8, bcolor(navy*0.8))
			bar(9, bcolor(navy*0.6))
			bar(10, bcolor(navy*0.4))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(4)
			title("Kenya Phase 1 (2019): experiential process")
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		putdocx paragraph
		putdocx image graph.png				

putdocx pagebreak			
putdocx paragraph
putdocx text ("Figure 2. Level of potential indicators by background"), bold 			
	
	global covlist "yedu4"
	
	use summary_Access_Indicators_CR.dta, clear	
	gen dummy=.
	
	foreach cov of varlist $covlist{
		#delimit; 
		graph bar $indicatoroutcome dummy $indicatortech if xsurvey=="KER8" & (`cov'!=.), 
			by(`cov', 
				col(1) legend(pos(3) size(vsmall))
				title("Kenya Phase 1 (2019): outcome and technical process", size(small))
				)
			blabel(bar)					
			legend(pos(3) col(1) size(small) stack
				label(1 "ywant")
				label(2 "ywantrec")	
				label(3 "")	
				label(4 "ycounsel") 
				label(5 "yexplain_all") 
				label(6 "ydiscuss_all4") 	
				label(7 "ydiscuss_procon") 
				label(8 "ydiscuss_all5") 
			)
			bar(1, bcolor(cranberry*1.0))
			bar(2, bcolor(cranberry*0.8))
			bar(3, bcolor(white))
			bar(4, bcolor(dkorange*0.8))
			bar(5, bcolor(dkgreen*0.8))
			bar(6, bcolor(navy*0.8))
			bar(7, bcolor(navy*0.6))
			bar(8, bcolor(navy*0.4))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(10)
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx text ("By `cov'"), bold 	
		putdocx image graph.png		
}

	foreach cov of varlist $covlist{
		#delimit; 
		graph bar $indicatorexp if xsurvey=="KER8" & (`cov'!=.), 
			by(`cov', 
				col(1) legend(pos(3) size(vsmall))
				title("Kenya Phase 1 (2019): experiential process", size(small))
				)
			blabel(bar)			
			legend(pos(3) col(1) size(vsmall) stack
				label(1 "ycommunication_all") 
				label(2 "ywait15")
				label(3 "ywait30")
				label(4 "ypolitevery")
				label(5 "ypolite")
				label(6 "ysatisfiedvery")
				label(7 "ysatisfied")
				label(8 "yrefer")
				label(9 "yreturn")
				label(10 "yreferreturn")
			)
			bar(1, bcolor(lavender))
			bar(2, bcolor(cranberry*0.8))
			bar(3, bcolor(cranberry*0.6))
			bar(4, bcolor(dkorange*0.8))
			bar(5, bcolor(dkorange*0.6))
			bar(6, bcolor(dkgreen*0.8))
			bar(7, bcolor(dkgreen*0.6))
			bar(8, bcolor(navy*0.8))
			bar(9, bcolor(navy*0.6))
			bar(10, bcolor(navy*0.4))
			ylab(0 (20) 100, angle(0) labsize(small))  
			xsize(8) ysize(10)
			; 
			#delimit cr
		gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy	
		graph export graph.png, replace	
		
		putdocx paragraph
		putdocx text ("By `cov'"), bold 		
		putdocx image graph.png				
}

/*
putdocx pagebreak			
putdocx paragraph
putdocx text ("Annex 1: HH ladder"), bold 			

	use CR_KER8_Access_Indicators.dta, clear
	
	tab yhhladder 
	tab yedu4, m
	tab yhhladder yedu4, chi 
	histogram yhhladder, w(1) percent by(yedu4, col(1)) xsize(4) ysize(10)
	graph export graph.png, replace	
	putdocx paragraph
	putdocx image graph.png				
	
	histogram yhhladder, w(1) percent by(yagegroup5, col(2)) xsize(4) ysize(10)
	graph export graph.png, replace	
	putdocx paragraph
	putdocx image graph.png				
	
	histogram yhhladder, w(1) percent by(yagegroup10, col(1)) xsize(4) ysize(10)
	graph export graph.png, replace	
	putdocx paragraph
	putdocx image graph.png				
*/
/*
putdocx pagebreak			
putdocx paragraph
putdocx text ("Annex 2: Wait time data"), bold 			

	use CR_KER8_Access_Indicators.dta, clear
	
	sum ywait* 
	
	sum time_wait_m time_wait_h, detail
	histogram time_wait_m , w(30)
	histogram time_wait_h , w(1)
			
*/
	
erase graph.png
		
putdocx save Access_Potential_Indicators_CR_$date.docx, replace	
	
putdocx append Access_Potential_Indicators_IR_$date.docx Access_Potential_Indicators_CR_$date.docx, saving(Access_Potential_Indicators_$date.docx, replace)	

END OF DO FILE  
