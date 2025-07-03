preserve

* Regression for position == Public with no fixed effects 

 keep if position == 9  /* Formal Public Sector
	*/ | position == 10 /* Informal Public Sector
	*/ 
 
********************************************************************************
* Step 1: Generate dummy variables by education x quarter
********************************************************************************
cap drop charact_educ*
quietly gen charact_educ0 = educ0
quietly gen charact_educ1 = educ1
quietly xi I.charact_educ0*I.year_quarter, prefix(_aa_) noomit
quietly xi I.charact_educ1*I.year_quarter, prefix(_bb_) noomit

********************************************************************************	
* Step 2: Run the main regression
********************************************************************************
reg job_loss ///
_aa_chaXyea_1_* _bb_chaXyea_1_* ///
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer race age monthly_work_income job_start ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

cap drop error
predict error, stdp

********************************************************************************
* Step 3: Fit values by education level
********************************************************************************
* Category: educ0 (No College)

cap drop fitted_values_educ0
ge fitted_values_educ0 = /* 
 */ _b[_aa_chaXyea_1_20121]*_aa_chaXyea_1_20121 + /*  	
 */ _b[_aa_chaXyea_1_20122]*_aa_chaXyea_1_20122 + /*  
 */ _b[_aa_chaXyea_1_20123]*_aa_chaXyea_1_20123 + /*  
 */ _b[_aa_chaXyea_1_20124]*_aa_chaXyea_1_20124 + /*  
 */ _b[_aa_chaXyea_1_20131]*_aa_chaXyea_1_20131 + /*  
 */ _b[_aa_chaXyea_1_20132]*_aa_chaXyea_1_20132 + /*  
 */ _b[_aa_chaXyea_1_20133]*_aa_chaXyea_1_20133 + /*  
 */ _b[_aa_chaXyea_1_20134]*_aa_chaXyea_1_20134 + /*  
 */ _b[_aa_chaXyea_1_20141]*_aa_chaXyea_1_20141 + /*  
 */ _b[_aa_chaXyea_1_20142]*_aa_chaXyea_1_20142 + /*  
 */ _b[_aa_chaXyea_1_20143]*_aa_chaXyea_1_20143 + /*  
 */ _b[_aa_chaXyea_1_20144]*_aa_chaXyea_1_20144 + /*  
 */ _b[_aa_chaXyea_1_20151]*_aa_chaXyea_1_20151 + /*  
 */ _b[_aa_chaXyea_1_20152]*_aa_chaXyea_1_20152 + /*  
 */ _b[_aa_chaXyea_1_20153]*_aa_chaXyea_1_20153 + /*  
 */ _b[_aa_chaXyea_1_20154]*_aa_chaXyea_1_20154 + /*  
 */ _b[_aa_chaXyea_1_20161]*_aa_chaXyea_1_20161 + /*  
 */ _b[_aa_chaXyea_1_20162]*_aa_chaXyea_1_20162 + /*  
 */ _b[_aa_chaXyea_1_20163]*_aa_chaXyea_1_20163 + /*  
 */ _b[_aa_chaXyea_1_20164]*_aa_chaXyea_1_20164 + /*  
 */ _b[_aa_chaXyea_1_20171]*_aa_chaXyea_1_20171 + /*  
 */ _b[_aa_chaXyea_1_20172]*_aa_chaXyea_1_20172 + /*  
 */ _b[_aa_chaXyea_1_20173]*_aa_chaXyea_1_20173 + /*  
 */ _b[_aa_chaXyea_1_20174]*_aa_chaXyea_1_20174 + /*  
 */ _b[_aa_chaXyea_1_20181]*_aa_chaXyea_1_20181 + /*  
 */ _b[_aa_chaXyea_1_20182]*_aa_chaXyea_1_20182 + /*  
 */ _b[_aa_chaXyea_1_20183]*_aa_chaXyea_1_20183 + /*  
 */ _b[_aa_chaXyea_1_20184]*_aa_chaXyea_1_20184 + /*  
 */ _b[_aa_chaXyea_1_20191]*_aa_chaXyea_1_20191 + /*  
 */ _b[_aa_chaXyea_1_20192]*_aa_chaXyea_1_20192 + /*  
 */ _b[_aa_chaXyea_1_20193]*_aa_chaXyea_1_20193 + /*  
 */ _b[_aa_chaXyea_1_20194]*_aa_chaXyea_1_20194 + /*  
 */ _b[_aa_chaXyea_1_20201]*_aa_chaXyea_1_20201 + /*  
 */ _b[_aa_chaXyea_1_20202]*_aa_chaXyea_1_20202 + /*  
 */ _b[_aa_chaXyea_1_20203]*_aa_chaXyea_1_20203 + /*  
 */ _b[_aa_chaXyea_1_20204]*_aa_chaXyea_1_20204 + /*  
 */ _b[_aa_chaXyea_1_20211]*_aa_chaXyea_1_20211 + /*  
 */ _b[_aa_chaXyea_1_20212]*_aa_chaXyea_1_20212 + /*  
 */ _b[_aa_chaXyea_1_20213]*_aa_chaXyea_1_20213 + /*  
 */ _b[_aa_chaXyea_1_20214]*_aa_chaXyea_1_20214 + /*  
 */ _b[_aa_chaXyea_1_20221]*_aa_chaXyea_1_20221 + /*  
 */ _b[_aa_chaXyea_1_20222]*_aa_chaXyea_1_20222 + /*  
 */ _b[_aa_chaXyea_1_20223]*_aa_chaXyea_1_20223 + /*
 */ _b[_aa_chaXyea_1_20224]*_aa_chaXyea_1_20224 + /*
 */ _b[_aa_chaXyea_1_20231]*_aa_chaXyea_1_20231 + /*
 */ _b[_aa_chaXyea_1_20232]*_aa_chaXyea_1_20232 + /*
 */ _b[_aa_chaXyea_1_20233]*_aa_chaXyea_1_20233 + /*
 */ _b[_aa_chaXyea_1_20234]*_aa_chaXyea_1_20234 + /*
 */ _b[_aa_chaXyea_1_20241]*_aa_chaXyea_1_20241 + /*
 */ _b[_aa_chaXyea_1_20242]*_aa_chaXyea_1_20242 + /*
 */ _b[_aa_chaXyea_1_20243]*_aa_chaXyea_1_20243

replace fitted_values_educ0 = . if fitted_values_educ0==0
generate lb_educ0 = fitted_values_educ0 - invnormal(0.975)*error
generate ub_educ0 = fitted_values_educ0 + invnormal(0.975)*error

* Category: educ1 (Complete College)

cap drop fitted_values_educ1
ge fitted_values_educ1 = /* 
 */ _b[_bb_chaXyea_1_20121]*_bb_chaXyea_1_20121 + /*  
 */ _b[_bb_chaXyea_1_20122]*_bb_chaXyea_1_20122 + /*  
 */ _b[_bb_chaXyea_1_20123]*_bb_chaXyea_1_20123 + /*  
 */ _b[_bb_chaXyea_1_20124]*_bb_chaXyea_1_20124 + /*  
 */ _b[_bb_chaXyea_1_20131]*_bb_chaXyea_1_20131 + /*  
 */ _b[_bb_chaXyea_1_20132]*_bb_chaXyea_1_20132 + /*  
 */ _b[_bb_chaXyea_1_20133]*_bb_chaXyea_1_20133 + /*  
 */ _b[_bb_chaXyea_1_20134]*_bb_chaXyea_1_20134 + /*  
 */ _b[_bb_chaXyea_1_20141]*_bb_chaXyea_1_20141 + /*  
 */ _b[_bb_chaXyea_1_20142]*_bb_chaXyea_1_20142 + /*  
 */ _b[_bb_chaXyea_1_20143]*_bb_chaXyea_1_20143 + /*  
 */ _b[_bb_chaXyea_1_20144]*_bb_chaXyea_1_20144 + /*  
 */ _b[_bb_chaXyea_1_20151]*_bb_chaXyea_1_20151 + /*  
 */ _b[_bb_chaXyea_1_20152]*_bb_chaXyea_1_20152 + /*  
 */ _b[_bb_chaXyea_1_20153]*_bb_chaXyea_1_20153 + /*  
 */ _b[_bb_chaXyea_1_20154]*_bb_chaXyea_1_20154 + /*  
 */ _b[_bb_chaXyea_1_20161]*_bb_chaXyea_1_20161 + /*  
 */ _b[_bb_chaXyea_1_20162]*_bb_chaXyea_1_20162 + /*  
 */ _b[_bb_chaXyea_1_20163]*_bb_chaXyea_1_20163 + /*  
 */ _b[_bb_chaXyea_1_20164]*_bb_chaXyea_1_20164 + /*  
 */ _b[_bb_chaXyea_1_20171]*_bb_chaXyea_1_20171 + /*  
 */ _b[_bb_chaXyea_1_20172]*_bb_chaXyea_1_20172 + /*  
 */ _b[_bb_chaXyea_1_20173]*_bb_chaXyea_1_20173 + /*  
 */ _b[_bb_chaXyea_1_20174]*_bb_chaXyea_1_20174 + /*  
 */ _b[_bb_chaXyea_1_20181]*_bb_chaXyea_1_20181 + /*  
 */ _b[_bb_chaXyea_1_20182]*_bb_chaXyea_1_20182 + /*  
 */ _b[_bb_chaXyea_1_20183]*_bb_chaXyea_1_20183 + /*  
 */ _b[_bb_chaXyea_1_20184]*_bb_chaXyea_1_20184 + /*  
 */ _b[_bb_chaXyea_1_20191]*_bb_chaXyea_1_20191 + /*  
 */ _b[_bb_chaXyea_1_20192]*_bb_chaXyea_1_20192 + /*  
 */ _b[_bb_chaXyea_1_20193]*_bb_chaXyea_1_20193 + /*  
 */ _b[_bb_chaXyea_1_20194]*_bb_chaXyea_1_20194 + /*  
 */ _b[_bb_chaXyea_1_20201]*_bb_chaXyea_1_20201 + /*  
 */ _b[_bb_chaXyea_1_20202]*_bb_chaXyea_1_20202 + /*  
 */ _b[_bb_chaXyea_1_20203]*_bb_chaXyea_1_20203 + /*  
 */ _b[_bb_chaXyea_1_20204]*_bb_chaXyea_1_20204 + /*  
 */ _b[_bb_chaXyea_1_20211]*_bb_chaXyea_1_20211 + /*  
 */ _b[_bb_chaXyea_1_20212]*_bb_chaXyea_1_20212 + /*  
 */ _b[_bb_chaXyea_1_20213]*_bb_chaXyea_1_20213 + /*  
 */ _b[_bb_chaXyea_1_20214]*_bb_chaXyea_1_20214 + /*  
 */ _b[_bb_chaXyea_1_20221]*_bb_chaXyea_1_20221 + /*  
 */ _b[_bb_chaXyea_1_20222]*_bb_chaXyea_1_20222 + /*  
 */ _b[_bb_chaXyea_1_20223]*_bb_chaXyea_1_20223 + /*
 */ _b[_bb_chaXyea_1_20224]*_bb_chaXyea_1_20224 + /*
 */ _b[_bb_chaXyea_1_20231]*_bb_chaXyea_1_20231 + /*
 */ _b[_bb_chaXyea_1_20232]*_bb_chaXyea_1_20232 + /*
 */ _b[_bb_chaXyea_1_20233]*_bb_chaXyea_1_20233 + /*
 */ _b[_bb_chaXyea_1_20234]*_bb_chaXyea_1_20234 + /*
 */ _b[_bb_chaXyea_1_20241]*_bb_chaXyea_1_20241 + /*
 */ _b[_bb_chaXyea_1_20242]*_bb_chaXyea_1_20242 + /*
 */ _b[_bb_chaXyea_1_20243]*_bb_chaXyea_1_20243

replace fitted_values_educ1 = . if fitted_values_educ1==0
generate lb_educ1 = fitted_values_educ1 - invnormal(0.975)*error
generate ub_educ1 = fitted_values_educ1 + invnormal(0.975)*error
cap drop error 
cap drop yhat

********************************************************************************
* Step 4: Collapse results by year_quarter
********************************************************************************

collapse (mean) fitted_values_educ* lb_educ* ub_educ* , by(year_quarter)

********************************************************************************
* Step 5: Convert year_quarter to a quarterly date variable
********************************************************************************

* Create a new variable to store the dates
cap drop date_var 
gen date_var = .

* Loop through each observation and convert the numeric value to a date
forval i = 1/`=_N' {
    local yq = year_quarter[`i']
    local year = substr("`yq'", 1, 4)
    local quarter = substr("`yq'", 5, .)
    
    * Determine the month and day based on the quarter
    * local month = 3 * (`quarter' - 1) + 1
	local month = 3 * (`quarter' - 1) + 1 + 2 // enforce the last month of the quarter
    local day = 1
    
    * Convert the year, month, and day to a date format
    local date = mdy(`month', `day', `year')
    
    * Assign the date to the new variable
    replace date_var = `date' in `i'
}

* Format the variable as a date
format date_var %td
describe date_var
gen quarterly_date = qofd(date_var)
format quarterly_date %tq
tsset quarterly_date

********************************************************************************
* Step 6: Create the line plot
********************************************************************************

tsline fitted_values_educ0 fitted_values_educ1 ///
    lb_educ0 ub_educ0 lb_educ1 ub_educ1 ///
    , ///
    lpattern(dash solid dash dash solid solid) ///
    lwidth(thick thick thin thin thin thin) ///
    lstyle(p1mark p10mark p1mark p1mark p10mark p10mark) ///
    lcolor(black%50 black black%30 black%30 black%30 black%30) ///
	title("Public")	///
	subtitle("") ///
	xtitle("") ///
	xlabel(#5 , angle(0) labsize(2.5) ) ///
	ytitle("Coefficient") ///
	ylabel(#3, angle(0) labsize(2.5) format(%9.2f) ) ///
	yscale( axis(1) range(0.15 0.25) lstyle(none)  ) ///
	tline(2019q4, lcolor(red) lpattern(dash) lwidth(0.3) ) ///
	tline(2021q4, lcolor(red) lpattern(dash) lwidth(0.3) ) ///
    legend(off order(1 "No College" 2 "College") ///
           pos(11) ring(0) col(1) rows(2) size(2.5) symxsize(*0.6) symysize(*0.6)) ///
	note("") ///
	recast(line) ///
	graphregion(fcolor(white)) ///
	scheme( s2gcolor ) /// economist s1mono   s1manual  s2gmanual 
	saving("$ROOT/analysis/tmp/_graph_regression_public_nofe_job_loss_determinants.gph", replace)	

********************************************************************************
* Step 7: Export and save the graph
********************************************************************************

	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_public_nofe_job_loss_determinants.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_public_nofe_job_loss_determinants.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_public_nofe_job_loss_determinants.png", replace	
	
	restore
