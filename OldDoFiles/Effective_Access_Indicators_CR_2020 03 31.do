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
use "$data/NonPublicFiles/BFP1_CQ_Clean_Data_with_checks_23Mar2020.dta", clear

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
	
		gen byte xfpclient=fp_reason_yn ==1
	
	keep if xfpclient==1
	
		gen xage=age
		egen xagegroup5 = cut(age), at(15,20,25,30,35,40,45,50)
		egen xagegroup10 = cut(age), at(15,20,30,40,50)
		gen byte xhhladder=hh_location_ladder
		*gen byte xurban=ur==1
		gen byte xedu_never	=school==1
		gen byte xedu_pri	=school==2
		gen byte xedu_sec	=school>=3 & school<=4
		gen byte xedu_col	=school>=5 & school!=.
		gen byte xedu4=. 
			replace xedu4=0 if xedu_never==1
			replace xedu4=1 if xedu_pri==1
			replace xedu4=2 if xedu_sec==1			
			replace xedu4=3 if xedu_col==1	
		lab define xedu4 0"none" 1"primary" 2"secondary" 3"college+"
		lab values xedu4 xedu4

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
			
		lab var xage "client's age at interview" 
		lab var xagegroup5 "client's age at interview, 5-year group" 		
		lab var xhhladder "HH percceived economic ladder"
		*lab var xurban "rural vs. urban (1) "
		lab var xedu_sec "ever attended: none or primary vs. secondary or higher+ (1)"
		
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
		gen byte xwant= fp_obtain_desired==1 | fp_obtain_desired==3
		gen byte xrecommend= fp_obtain_desired_whynot>=4 & fp_obtain_desired_whynot<=6
			replace xrecommend=. if fp_obtain_desired!=2 
		gen byte xwantrec= xwant==1 | xrecommend==1
		
		* technical 
		gen byte xcounsel=pill_counsel==1 |  inj_counsel==1
			replace xcounsel=. if (method_prescribed!=5 & method_prescribed!=6) 
		egen xexplain_sum=rowtotal(explain_method-explain_follow_up) 
		gen byte xexplain_all=xexplain_sum==4
		egen xdiscuss_sum=rowtotal(discuss_other_fp-discuss_switch) 
		gen byte xdiscuss_procon=discuss_pro_con_delay==1 
		gen byte xdiscuss_all5=xdiscuss_sum==4 & xdiscuss_procon==1
		gen byte xdiscuss_all4=xdiscuss_sum==4
		
		* experiential 
		egen xcommunication_sum=rowtotal(howclear_fp_info allow_question understand_answer)
		gen byte xcommunication_all=xcommunication_sum==3
			foreach var of varlist xexplain* xdiscuss* xcommunication*  {
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
		
		gen byte xwait15=time_wait_m>15
		gen byte xwait30=time_wait_m>30
		gen byte xwait60=time_wait_m>60
		gen byte xwait_onehour=time_wait_h>1
			
		gen byte xpolitevery	=how_staff_treat==1
		gen byte xpolite		=how_staff_treat<=2
		gen byte xsatisfiedvery	=satisfied_services_today==1
		gen byte xsatisfied		=satisfied_services_today<=2
		gen byte xrefer			=refer_hf==1
		gen byte xreturn		=return_to_facility==1	
		gen byte xreferreturn	=refer_hf==1 & return_to_facility==1
	
		lab var xwant 			"received initially wanted method" 
		lab var xrecommend 		"did not receive initially wanted method because of three potentially acceptable reasons"
		lab var xwantrec 		"received initially wanted method OR did not because of three potentially acceptable reasons" 
		
		lab var xcounsel 		"pill/injectables specific counseling on pregnancy chance if not taken correctly"
		lab var xexplain_all 	"explained on all four items"
		lab var xdiscuss_all5 	"discussed all five items"
		lab var xdiscuss_all4 	"discussed all four items"
		lab var xdiscuss_procon 	"discussed advantage/disadvantage of her method"
		
		lab var xcommunication_all 	"clear, allowed question, and understand"
		lab var xwait15			"waited more than 15 minutes"
		lab var xwait30			"waited more than 30 minutes"
		lab var xwait60			"waited more than 60 minutes"
		lab var xwait_onehour	"waited more than one hour (based on hour reporting)"
		lab var xpolitevery		"very polite"
		lab var xpolite			"polite or very polite"
		lab var xsatisfiedvery		"very satisfied"
		lab var xsatisfied			"satisfied or very satisfied"		
		lab var xrefer			"would refer frient/relative"
		lab var xreturn			"would return"		
		lab var xreferreturn	"would refer AND return"
				
save CR_`survey'_Access_Indicators.dta, replace 	
}	

*okay


*********************************************************
* C. Create summary dataset 
*********************************************************	
#delimit; 
global indicatoroutcome "
	xwant
	xrecommend
	xwantrec 
	";
	#delimit cr
	
#delimit; 
global indicatortech "
	xcounsel 
	xexplain_all 
	xdiscuss_all4 	
	xdiscuss_procon 
	xdiscuss_all5 
	";
	#delimit cr

#delimit; 
global indicatorexp "
	xcommunication_all 
	xwait15
	xwait30
	xwait60
	xwait_onehour
	xpolitevery
	xpolite
	xsatisfiedvery
	xsatisfied
	xrefer
	xreturn
	xreferreturn
	";
	#delimit cr	

global covlist "xedu4 xagegroup10"


	use CR_KER8_Access_Indicators.dta, clear
	foreach survey in $surveylistminusone{
		append using CR_`survey'_Access_Indicators.dta, force
		}
	save temp.dta, replace
	
	use temp.dta, clear
	*use CR_KER8_Access_Indicators.dta, clear
		
		sum  $indicatoroutcome $indicatortech $indicatorexp
		
		collapse (mean) $indicatoroutcome $indicatortech $indicatorexp xedu_*, by(xsurvey round year)
			foreach var of varlist  $indicatoroutcome $indicatortech $indicatorexp {
				replace `var'=round(`var'*100, 1)
				}
			foreach cov in $covlist{
				gen `cov'=.
				}				
			gen group="All"	
		save summary_Access_Indicators_CR.dta, replace 	
	
	
	foreach cov of varlist $covlist{
	use temp.dta, clear
	*use CR_KER8_Access_Indicators.dta, clear
		collapse (mean) $indicatoroutcome $indicatortech $indicatorexp xedu_*, by(xsurvey round year `cov')	
			foreach var of varlist $indicatoroutcome $indicatortech $indicatorexp {
				replace `var'=round(`var'*100, 1)
				}					
			gen group="By `cov'"	
			drop if `cov'==.
		append using summary_Access_Indicators_CR.dta, 
		save summary_Access_Indicators_CR.dta, replace 	
		}
						
	gen xcountry=substr(xsurvey, 1, length(xsurvey)-2)
	egen temp=max(round), by(xcountry) 
	gen xlatest=round==temp
		drop temp
		
	sort xcountry xedu4 xagegroup10	
	save summary_Access_Indicators_CR.dta, replace 	
	
	export delimited using summary_Access_Indicators_CR.csv, replace

OKAY Summary DATA READY FOR ANALYSIS and Shiny App
*/
*********************************************************
* D. ANALYSIS using the summary dataset 
*********************************************************			
#delimit; 
global indicatoroutcome "
	xwant
	xwantrec 
	";
	#delimit cr
	
#delimit; 
global indicatortech "
	xcounsel 
	xexplain_all 
	xdiscuss_all4 	
	xdiscuss_procon 
	xdiscuss_all5 
	";
	#delimit cr

#delimit; 
global indicatorexp "
	xcommunication_all 
	xwait15
	xwait30
	xpolitevery
	xpolite
	xsatisfiedvery
	xsatisfied
	xrefer
	xreturn
	xreferreturn
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
putdocx text ("--xwant: received initially wanted method"), linebreak 
putdocx text ("--xrecommend: did not receive initially wanted method because of three potentially acceptable reasons"), linebreak
putdocx text ("--xwantrec: received initially wanted method OR did not because of three potentially acceptable reasons"), linebreak 
putdocx text ("2.2 Process quality "), bold
putdocx text ("2.2.1 Technical process quality"), bold
putdocx text ("(among FP clients who received methods/prescription)"), linebreak bold
putdocx text ("--xcounsel: pill/injectables specific counseling on pregnancy chance if not taken correctly"), linebreak	
putdocx text ("--xexplain_all: explained on all four items"), linebreak		
putdocx text ("--xdiscuss_all4: discussed all four items"), linebreak		
putdocx text ("--xdiscuss_procon: discussed advantage/disadvantage of her method"), linebreak		
putdocx text ("--xdiscuss_all5: discussed all five items"), linebreak		
putdocx text ("2.2.2. Experiential process quality "), bold
putdocx text ("(among all FP clients)"), linebreak bold			
putdocx text ("--xcommunication_all : clear, allowed question, and understand"), linebreak		
putdocx text ("--xwait30: waited more than 30 minutes"), linebreak
putdocx text ("--xwait60: waited more than 60 minutes"), linebreak
putdocx text ("--xwait_onehour: waited more than one hour (based on hour reporting)"), linebreak		
putdocx text ("--xpolitevery: very polite"), linebreak	
putdocx text ("--xpolite: polite or very polite"), linebreak
putdocx text ("--xsatisfiedvery: very satisfied"), linebreak	
putdocx text ("--xsatisfied: satisfied or very satisfied"), linebreak
putdocx text ("--xrefer: would refer frient/relative"), linebreak
putdocx text ("--xreturn: would return"), linebreak
putdocx text ("--xreferreturn: would refer AND return"), linebreak	
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
				label(1 "xwant")
				label(2 "xwantrec")	
				label(3 "")	
				label(4 "xcounsel") 
				label(5 "xexplain_all") 
				label(6 "xdiscuss_all4") 	
				label(7 "xdiscuss_procon") 
				label(8 "xdiscuss_all5") 
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
				label(1 "xcommunication_all") 
				label(2 "xwait15")
				label(3 "xwait30")
				label(4 "xpolitevery")
				label(5 "xpolite")
				label(6 "xsatisfiedvery")
				label(7 "xsatisfied")
				label(8 "xrefer")
				label(9 "xreturn")
				label(10 "xreferreturn")
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
	
	global covlist "xedu4"
	
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
				label(1 "xwant")
				label(2 "xwantrec")	
				label(3 "")	
				label(4 "xcounsel") 
				label(5 "xexplain_all") 
				label(6 "xdiscuss_all4") 	
				label(7 "xdiscuss_procon") 
				label(8 "xdiscuss_all5") 
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
				label(1 "xcommunication_all") 
				label(2 "xwait15")
				label(3 "xwait30")
				label(4 "xpolitevery")
				label(5 "xpolite")
				label(6 "xsatisfiedvery")
				label(7 "xsatisfied")
				label(8 "xrefer")
				label(9 "xreturn")
				label(10 "xreferreturn")
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
	
	tab xhhladder 
	tab xedu4, m
	tab xhhladder xedu4, chi 
	histogram xhhladder, w(1) percent by(xedu4, col(1)) xsize(4) ysize(10)
	graph export graph.png, replace	
	putdocx paragraph
	putdocx image graph.png				
	
	histogram xhhladder, w(1) percent by(xagegroup5, col(2)) xsize(4) ysize(10)
	graph export graph.png, replace	
	putdocx paragraph
	putdocx image graph.png				
	
	histogram xhhladder, w(1) percent by(xagegroup10, col(1)) xsize(4) ysize(10)
	graph export graph.png, replace	
	putdocx paragraph
	putdocx image graph.png				
*/
/*
putdocx pagebreak			
putdocx paragraph
putdocx text ("Annex 2: Wait time data"), bold 			

	use CR_KER8_Access_Indicators.dta, clear
	
	sum xwait* 
	
	sum time_wait_m time_wait_h, detail
	histogram time_wait_m , w(30)
	histogram time_wait_h , w(1)
			
*/
	
erase graph.png
		
putdocx save Access_Potential_Indicators_CR_$date.docx, replace	
	
putdocx append Access_Potential_Indicators_IR_$date.docx Access_Potential_Indicators_CR_$date.docx, saving(Access_Potential_Indicators_$date.docx, replace)	

END OF DO FILE  
