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

local today=c(current_date)
local c_today= "`today'"
local date=subinstr("`c_today'", " ", "",.)

************************************************************************
* B. EA-SDP link assessment 
************************************************************************

capture putdocx clear 
putdocx begin

global datalistminusone "KER3 KER4 KER5 KER6 KER7"
use "$data/EAlevel_SDP_KER2.dta", clear	
	foreach survey in $datalistminusone{
	append using "$data/EAlevel_SDP_`survey'.dta", force
	}
		
		tab xsurvey HHQFQround, m
		keep xsurvey HHQFQcounty HHQFQur EA_ID num_SDP*
		sort xsurvey HHQFQcounty HHQFQur EA_ID 

		*** No linked SDP
		list xsurvey HHQFQcounty HHQFQur EA_ID if num_SDPall==0
		*** No linked public SDP
		list xsurvey HHQFQcounty HHQFQur EA_ID if num_SDPpub==0
		*** No linked public SDP
		*list xsurvey HHQFQcounty HHQFQur EA_ID if num_SDPpub12==0		
		tab HHQFQcounty HHQFQur if num_SDPpub12==0, m	
		
		#delimit; 	
		graph box num_*, over(xsurvey)
			legend(pos(6) row(1) size(small)
				label(1 "All SDPs")
				label(2 "public")
				label(3 "public primary/secondary") 
				label(4 "any primary/secondary") )
			ytitle("Nnumber of linked SDPs per EA")	
			saving(graph_1.gph, replace)
			;
			#delimit cr				

			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
			gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
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
			saving(graph_2.gph, replace)
			;
			#delimit cr
	
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
			gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
		#delimit;	
		gr combine graph_1.gph graph_2.gph , 
			col(1) xsize(8) ysize(10)
			;
			#delimit cr
		
		putdocx paragraph
		graph export graph.png, replace	
		putdocx image graph.png				

		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}
			
		putdocx paragraph
		putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
		putdocx table table = data(xsurvey HHQFQur noSDP*) 

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
		
		#delimit; 	
		graph box num_*, over(xsurvey)
			legend(pos(6) row(1) size(small)
				label(1 "All SDPs")
				label(2 "public")
				label(3 "public primary/secondary") 
				label(4 "any primary/secondary") )
			ytitle("Nnumber of linked SDPs per EA")	
			saving(graph_1.gph, replace)
			;
			#delimit cr				

			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
			gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
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
			saving(graph_2.gph, replace)
			;
			#delimit cr
	
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
			gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
		#delimit;	
		gr combine graph_1.gph graph_2.gph , 
			col(1) xsize(8) ysize(10)
			;
			#delimit cr
		
		putdocx paragraph
		graph export graph.png, replace	
		putdocx image graph.png				
		
		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}
			
		putdocx paragraph
		putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
		putdocx table table = data(xsurvey HHQFQur noSDP*) 
		
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
		
		#delimit; 	
		graph box num_*, over(xsurvey)
			legend(pos(6) row(1) size(small)
				label(1 "All SDPs")
				label(2 "public")
				label(3 "public primary/secondary") 
				label(4 "any primary/secondary") )
			ytitle("Nnumber of linked SDPs per EA")	
			saving(graph_1.gph, replace)
			;
			#delimit cr				

			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
			gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
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
			saving(graph_2.gph, replace)
			;
			#delimit cr
	
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy	
			gr_edit .legend.style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(black))) editcopy
	
		#delimit;	
		gr combine graph_1.gph graph_2.gph , 
			col(1) xsize(8) ysize(10)
			;
			#delimit cr
		
		putdocx paragraph
		putdocx text ("Uganda"), linebreak bold 
		graph export graph.png, replace	
		putdocx image graph.png		
		
		collapse (mean) noSDPany noSDPpub noSDPpub12 noSDPlow, by (xsurvey HHQFQur)
		foreach x of varlist noSDP*{
			replace `x'=`x'*100
			format `x' %9.1f
			}
			
		putdocx paragraph
		putdocx table stable = (1,6), title("% EAs with no linked SDP: by residential area")  
		putdocx table table = data(xsurvey HHQFQur noSDP*) 
		
		
erase graph.png
erase graph_1.gph
erase graph_2.gph

************************************************************************
* C. Figures 
************************************************************************


global countrylist "Kenya Ethiopia Uganda"
foreach country in $countrylist{ 
	use Access_summary_`country'.dta, clear	
	
	*Keep the latest round 
		gen round=substr(xsurvey, 4, 1)
		destring round, replace
		egen latestround=max(round)
		keep if round==latestround
		tab xsurvey round, m

	*GENERATE figure	
	foreach method in essential5_curav  {
		#delimit; 	
		graph bar popSDPall_`method' popSDPpub_`method' popSDPpub12_`method' SDPall_`method' SDPpub_`method' SDPpub12_`method'  , 
			over(xsurvey)
			title("By denominator: `country'")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(cranberry*0.8))  
			bar(5, color(cranberry*0.6))  
			bar(6, color(cranberry*0.4))  
			legend(pos(6) row(1) size(small) stack 
				label(1 "% women" "all" "SDPs")	
				label(2 "% women" "public" "SDPs")	
				label(3 "% women" "primary" "/secondary" "public" "SDPs")	
				label(4 "% SDPs" "all" "SDPs")	
				label(5 "% SDPs" "public" "SDPs")	
				label(6 "% SDPs" "primary" "/secondary" "public" "SDPs")	
				)
			ylab(0(20)100, angle(0) labsize(small))
			xsize(3) ysize(4)
			saving(graph_`country'.gph, replace)
		;
		#delimit cr		
	}
}	
	*Combine 	
		#delimit;	
		gr combine graph_Kenya.gph graph_Ethiopia.gph graph_Uganda.gph , 
			row(1) xsize(9) ysize(4)
			note("essential5_curav")

			;
			#delimit cr
		
		putdocx paragraph
		graph export graph.png, replace	
		putdocx image graph.png	
		
global countrylist "Kenya Ethiopia Uganda"
foreach country in $countrylist{ 
	use Access_summary_`country'.dta, clear	
	
	*Keep the latest round 
		gen round=substr(xsurvey, 4, 1)
		destring round, replace
		egen latestround=max(round)
		keep if round==latestround
		tab xsurvey round, m

	*GENERATE figure	
		#delimit; 	
		graph bar 	popSDPall_essential5ec_curav popSDPall_essential5_curav  
					SDPall_essential5ec_curav    SDPall_essential5_curav      , 
			over(xsurvey)
			title("By essential methods set: `country'")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(cranberry*0.8))  
			bar(4, color(cranberry*0.6))  
			legend(pos(6) row(1) size(vsmall) stack 
				label(1 "% women" "5+EC")	
				label(2 "% women" "5")
				label(3 "% SDPs" "5+EC")
				label(4 "% SDPs" "5")
				)
			ylab(0(20)100, angle(0) labsize(small))
			xsize(7) ysize(4)
			saving(graph_`country'.gph, replace)
		;
		#delimit cr
	}
	
	*Combine 	
		#delimit;	
		gr combine graph_Kenya.gph graph_Ethiopia.gph graph_Uganda.gph , 
			row(1) xsize(9) ysize(4)
			note("currenly available, among all SDPs")
			;
			#delimit cr
	
		putdocx paragraph
		graph export graph.png, replace	
		putdocx image graph.png		

global countrylist "Kenya Ethiopia Uganda"
foreach country in $countrylist{ 
	use Access_summary_`country'.dta, clear	
	
	*Keep the latest round 
		gen round=substr(xsurvey, 4, 1)
		destring round, replace
		egen latestround=max(round)
		keep if round==latestround
		tab xsurvey round, m

	*GENERATE figure	
	foreach method in essential5   {
		#delimit; 	
		graph bar popSDPall_`method'_offer popSDPall_`method'_curav popSDPall_`method'_noso popSDPall_`method'_ready
				  SDPall_`method'_offer SDPall_`method'_curav SDPall_`method'_noso SDPall_`method'_ready , 
			over(xsurvey)
			title("By offer/have definition: `country'")
			bar(1, color(navy*0.8)) 
			bar(2, color(navy*0.6)) 
			bar(3, color(navy*0.4)) 
			bar(4, color(navy*0.2)) 
			bar(5, color(cranberry*0.8))  
			bar(6, color(cranberry*0.6))  
			bar(7, color(cranberry*0.4))  
			bar(8, color(cranberry*0.2))  
			legend(pos(6) row(1) size(vsmall) stack 
				label(1 "% women" "offer" )	
				label(2 "% women" "currently" "have" )
				label(3 "% women" "have NOSO" ) 
				label(4 "% women" "ready*" ) 
				label(5 "% SDPs" "offer" )
				label(6 "% SDPs" "currently" "have" )
				label(7 "% SDPs" "have NOSO" )
				label(8 "% SDPs" "ready*" )
				)
			ylab(0(20)100, angle(0) labsize(small))
			xsize(7) ysize(4)
			saving(graph_`country'.gph, replace)
		;
		#delimit cr
	}
}	
	*Combine 	
		#delimit;	
		gr combine graph_Kenya.gph graph_Ethiopia.gph graph_Uganda.gph , 
			row(1) xsize(9) ysize(4)
			note("essential5, Among all SDPs"
				 "*readiness for implants and IUD. For other methods, current availability", size(small) )
			;
			#delimit cr
	
		putdocx paragraph
		graph export graph.png, replace	
		putdocx image graph.png				

putdocx save Access_Link_Assessment_`date'.docx, replace		
