preserve

* Regression for race == black with no fixed effects 

 keep if negro == 1


********************************************************************************
* Step 1: Generate dummy variables by education x quarter
********************************************************************************
cap drop charact_educ*
quietly gen charact_educ1 = educ1
quietly replace  charact_educ1 = 1 if educ2 ==1
quietly replace  charact_educ1 = 1 if educ3 ==1
quietly gen charact_educ4 = educ4
quietly xi I.charact_educ1*I.year_quarter, prefix(_yy_) noomit
*quietly xi I.charact_educ2*I.year_quarter, prefix(_zz_) noomit
*quietly xi I.charact_educ3*I.year_quarter, prefix(_xx_) noomit
quietly xi I.charact_educ4*I.year_quarter, prefix(_ww_) noomit
	
********************************************************************************	
* Step 2: Run the main regression
********************************************************************************
reg job_loss ///
_yy_chaXyea_1_* _ww_chaXyea_1_* ///  _xx_chaXyea_1_*  _zz_chaXyea_1_* 
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender age monthly_work_income job_start ///
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
* Category: educ1

cap drop fitted_values_educ1
ge fitted_values_educ1 = /* 
 */ _b[_yy_chaXyea_1_20121]*_yy_chaXyea_1_20121 + /*  	
 */ _b[_yy_chaXyea_1_20122]*_yy_chaXyea_1_20122 + /*  
 */ _b[_yy_chaXyea_1_20123]*_yy_chaXyea_1_20123 + /*  
 */ _b[_yy_chaXyea_1_20124]*_yy_chaXyea_1_20124 + /*  
 */ _b[_yy_chaXyea_1_20131]*_yy_chaXyea_1_20131 + /*  
 */ _b[_yy_chaXyea_1_20132]*_yy_chaXyea_1_20132 + /*  
 */ _b[_yy_chaXyea_1_20133]*_yy_chaXyea_1_20133 + /*  
 */ _b[_yy_chaXyea_1_20134]*_yy_chaXyea_1_20134 + /*  
 */ _b[_yy_chaXyea_1_20141]*_yy_chaXyea_1_20141 + /*  
 */ _b[_yy_chaXyea_1_20142]*_yy_chaXyea_1_20142 + /*  
 */ _b[_yy_chaXyea_1_20143]*_yy_chaXyea_1_20143 + /*  
 */ _b[_yy_chaXyea_1_20144]*_yy_chaXyea_1_20144 + /*  
 */ _b[_yy_chaXyea_1_20151]*_yy_chaXyea_1_20151 + /*  
 */ _b[_yy_chaXyea_1_20152]*_yy_chaXyea_1_20152 + /*  
 */ _b[_yy_chaXyea_1_20153]*_yy_chaXyea_1_20153 + /*  
 */ _b[_yy_chaXyea_1_20154]*_yy_chaXyea_1_20154 + /*  
 */ _b[_yy_chaXyea_1_20161]*_yy_chaXyea_1_20161 + /*  
 */ _b[_yy_chaXyea_1_20162]*_yy_chaXyea_1_20162 + /*  
 */ _b[_yy_chaXyea_1_20163]*_yy_chaXyea_1_20163 + /*  
 */ _b[_yy_chaXyea_1_20164]*_yy_chaXyea_1_20164 + /*  
 */ _b[_yy_chaXyea_1_20171]*_yy_chaXyea_1_20171 + /*  
 */ _b[_yy_chaXyea_1_20172]*_yy_chaXyea_1_20172 + /*  
 */ _b[_yy_chaXyea_1_20173]*_yy_chaXyea_1_20173 + /*  
 */ _b[_yy_chaXyea_1_20174]*_yy_chaXyea_1_20174 + /*  
 */ _b[_yy_chaXyea_1_20181]*_yy_chaXyea_1_20181 + /*  
 */ _b[_yy_chaXyea_1_20182]*_yy_chaXyea_1_20182 + /*  
 */ _b[_yy_chaXyea_1_20183]*_yy_chaXyea_1_20183 + /*  
 */ _b[_yy_chaXyea_1_20184]*_yy_chaXyea_1_20184 + /*  
 */ _b[_yy_chaXyea_1_20191]*_yy_chaXyea_1_20191 + /*  
 */ _b[_yy_chaXyea_1_20192]*_yy_chaXyea_1_20192 + /*  
 */ _b[_yy_chaXyea_1_20193]*_yy_chaXyea_1_20193 + /*  
 */ _b[_yy_chaXyea_1_20194]*_yy_chaXyea_1_20194 + /*  
 */ _b[_yy_chaXyea_1_20201]*_yy_chaXyea_1_20201 + /*  
 */ _b[_yy_chaXyea_1_20202]*_yy_chaXyea_1_20202 + /*  
 */ _b[_yy_chaXyea_1_20203]*_yy_chaXyea_1_20203 + /*  
 */ _b[_yy_chaXyea_1_20204]*_yy_chaXyea_1_20204 + /*  
 */ _b[_yy_chaXyea_1_20211]*_yy_chaXyea_1_20211 + /*  
 */ _b[_yy_chaXyea_1_20212]*_yy_chaXyea_1_20212 + /*  
 */ _b[_yy_chaXyea_1_20213]*_yy_chaXyea_1_20213 + /*  
 */ _b[_yy_chaXyea_1_20214]*_yy_chaXyea_1_20214 + /*  
 */ _b[_yy_chaXyea_1_20221]*_yy_chaXyea_1_20221 + /*  
 */ _b[_yy_chaXyea_1_20222]*_yy_chaXyea_1_20222 + /*  
 */ _b[_yy_chaXyea_1_20223]*_yy_chaXyea_1_20223 

replace fitted_values_educ1 = . if fitted_values_educ1==0
generate lb_educ1 = fitted_values_educ1 - invnormal(0.975)*error
generate ub_educ1 = fitted_values_educ1 + invnormal(0.975)*error

* Category: educ2
/*
cap drop fitted_values_educ2
ge fitted_values_educ2 = /* 
 */ _b[_zz_chaXyea_1_20121]*_zz_chaXyea_1_20121 + /*  
 */ _b[_zz_chaXyea_1_20122]*_zz_chaXyea_1_20122 + /*  
 */ _b[_zz_chaXyea_1_20123]*_zz_chaXyea_1_20123 + /*  
 */ _b[_zz_chaXyea_1_20124]*_zz_chaXyea_1_20124 + /*  
 */ _b[_zz_chaXyea_1_20131]*_zz_chaXyea_1_20131 + /*  
 */ _b[_zz_chaXyea_1_20132]*_zz_chaXyea_1_20132 + /*  
 */ _b[_zz_chaXyea_1_20133]*_zz_chaXyea_1_20133 + /*  
 */ _b[_zz_chaXyea_1_20134]*_zz_chaXyea_1_20134 + /*  
 */ _b[_zz_chaXyea_1_20141]*_zz_chaXyea_1_20141 + /*  
 */ _b[_zz_chaXyea_1_20142]*_zz_chaXyea_1_20142 + /*  
 */ _b[_zz_chaXyea_1_20143]*_zz_chaXyea_1_20143 + /*  
 */ _b[_zz_chaXyea_1_20144]*_zz_chaXyea_1_20144 + /*  
 */ _b[_zz_chaXyea_1_20151]*_zz_chaXyea_1_20151 + /*  
 */ _b[_zz_chaXyea_1_20152]*_zz_chaXyea_1_20152 + /*  
 */ _b[_zz_chaXyea_1_20153]*_zz_chaXyea_1_20153 + /*  
 */ _b[_zz_chaXyea_1_20154]*_zz_chaXyea_1_20154 + /*  
 */ _b[_zz_chaXyea_1_20161]*_zz_chaXyea_1_20161 + /*  
 */ _b[_zz_chaXyea_1_20162]*_zz_chaXyea_1_20162 + /*  
 */ _b[_zz_chaXyea_1_20163]*_zz_chaXyea_1_20163 + /*  
 */ _b[_zz_chaXyea_1_20164]*_zz_chaXyea_1_20164 + /*  
 */ _b[_zz_chaXyea_1_20171]*_zz_chaXyea_1_20171 + /*  
 */ _b[_zz_chaXyea_1_20172]*_zz_chaXyea_1_20172 + /*  
 */ _b[_zz_chaXyea_1_20173]*_zz_chaXyea_1_20173 + /*  
 */ _b[_zz_chaXyea_1_20174]*_zz_chaXyea_1_20174 + /*  
 */ _b[_zz_chaXyea_1_20181]*_zz_chaXyea_1_20181 + /*  
 */ _b[_zz_chaXyea_1_20182]*_zz_chaXyea_1_20182 + /*  
 */ _b[_zz_chaXyea_1_20183]*_zz_chaXyea_1_20183 + /*  
 */ _b[_zz_chaXyea_1_20184]*_zz_chaXyea_1_20184 + /*  
 */ _b[_zz_chaXyea_1_20191]*_zz_chaXyea_1_20191 + /*  
 */ _b[_zz_chaXyea_1_20192]*_zz_chaXyea_1_20192 + /*  
 */ _b[_zz_chaXyea_1_20193]*_zz_chaXyea_1_20193 + /*  
 */ _b[_zz_chaXyea_1_20194]*_zz_chaXyea_1_20194 + /*  
 */ _b[_zz_chaXyea_1_20201]*_zz_chaXyea_1_20201 + /*  
 */ _b[_zz_chaXyea_1_20202]*_zz_chaXyea_1_20202 + /*  
 */ _b[_zz_chaXyea_1_20203]*_zz_chaXyea_1_20203 + /*  
 */ _b[_zz_chaXyea_1_20204]*_zz_chaXyea_1_20204 + /*  
 */ _b[_zz_chaXyea_1_20211]*_zz_chaXyea_1_20211 + /*  
 */ _b[_zz_chaXyea_1_20212]*_zz_chaXyea_1_20212 + /*  
 */ _b[_zz_chaXyea_1_20213]*_zz_chaXyea_1_20213 + /*  
 */ _b[_zz_chaXyea_1_20214]*_zz_chaXyea_1_20214 + /*  
 */ _b[_zz_chaXyea_1_20221]*_zz_chaXyea_1_20221 + /*  
 */ _b[_zz_chaXyea_1_20222]*_zz_chaXyea_1_20222 + /*  
 */ _b[_zz_chaXyea_1_20223]*_zz_chaXyea_1_20223 

replace fitted_values_educ2 = . if fitted_values_educ2==0
generate lb_educ2 = fitted_values_educ2 - invnormal(0.975)*error
generate ub_educ2 = fitted_values_educ2 + invnormal(0.975)*error
*/

* Category: educ3
/*
cap drop fitted_values_educ3
ge fitted_values_educ3 = /* 
 */ _b[_xx_chaXyea_1_20121]*_xx_chaXyea_1_20121 + /*  
 */ _b[_xx_chaXyea_1_20122]*_xx_chaXyea_1_20122 + /*  
 */ _b[_xx_chaXyea_1_20123]*_xx_chaXyea_1_20123 + /*  
 */ _b[_xx_chaXyea_1_20124]*_xx_chaXyea_1_20124 + /*  
 */ _b[_xx_chaXyea_1_20131]*_xx_chaXyea_1_20131 + /*  
 */ _b[_xx_chaXyea_1_20132]*_xx_chaXyea_1_20132 + /*  
 */ _b[_xx_chaXyea_1_20133]*_xx_chaXyea_1_20133 + /*  
 */ _b[_xx_chaXyea_1_20134]*_xx_chaXyea_1_20134 + /*  
 */ _b[_xx_chaXyea_1_20141]*_xx_chaXyea_1_20141 + /*  
 */ _b[_xx_chaXyea_1_20142]*_xx_chaXyea_1_20142 + /*  
 */ _b[_xx_chaXyea_1_20143]*_xx_chaXyea_1_20143 + /*  
 */ _b[_xx_chaXyea_1_20144]*_xx_chaXyea_1_20144 + /*  
 */ _b[_xx_chaXyea_1_20151]*_xx_chaXyea_1_20151 + /*  
 */ _b[_xx_chaXyea_1_20152]*_xx_chaXyea_1_20152 + /*  
 */ _b[_xx_chaXyea_1_20153]*_xx_chaXyea_1_20153 + /*  
 */ _b[_xx_chaXyea_1_20154]*_xx_chaXyea_1_20154 + /*  
 */ _b[_xx_chaXyea_1_20161]*_xx_chaXyea_1_20161 + /*  
 */ _b[_xx_chaXyea_1_20162]*_xx_chaXyea_1_20162 + /*  
 */ _b[_xx_chaXyea_1_20163]*_xx_chaXyea_1_20163 + /*  
 */ _b[_xx_chaXyea_1_20164]*_xx_chaXyea_1_20164 + /*  
 */ _b[_xx_chaXyea_1_20171]*_xx_chaXyea_1_20171 + /*  
 */ _b[_xx_chaXyea_1_20172]*_xx_chaXyea_1_20172 + /*  
 */ _b[_xx_chaXyea_1_20173]*_xx_chaXyea_1_20173 + /*  
 */ _b[_xx_chaXyea_1_20174]*_xx_chaXyea_1_20174 + /*  
 */ _b[_xx_chaXyea_1_20181]*_xx_chaXyea_1_20181 + /*  
 */ _b[_xx_chaXyea_1_20182]*_xx_chaXyea_1_20182 + /*  
 */ _b[_xx_chaXyea_1_20183]*_xx_chaXyea_1_20183 + /*  
 */ _b[_xx_chaXyea_1_20184]*_xx_chaXyea_1_20184 + /*  
 */ _b[_xx_chaXyea_1_20191]*_xx_chaXyea_1_20191 + /*  
 */ _b[_xx_chaXyea_1_20192]*_xx_chaXyea_1_20192 + /*  
 */ _b[_xx_chaXyea_1_20193]*_xx_chaXyea_1_20193 + /*  
 */ _b[_xx_chaXyea_1_20194]*_xx_chaXyea_1_20194 + /*  
 */ _b[_xx_chaXyea_1_20201]*_xx_chaXyea_1_20201 + /*  
 */ _b[_xx_chaXyea_1_20202]*_xx_chaXyea_1_20202 + /*  
 */ _b[_xx_chaXyea_1_20203]*_xx_chaXyea_1_20203 + /*  
 */ _b[_xx_chaXyea_1_20204]*_xx_chaXyea_1_20204 + /*  
 */ _b[_xx_chaXyea_1_20211]*_xx_chaXyea_1_20211 + /*  
 */ _b[_xx_chaXyea_1_20212]*_xx_chaXyea_1_20212 + /*  
 */ _b[_xx_chaXyea_1_20213]*_xx_chaXyea_1_20213 + /*  
 */ _b[_xx_chaXyea_1_20214]*_xx_chaXyea_1_20214 + /*  
 */ _b[_xx_chaXyea_1_20221]*_xx_chaXyea_1_20221 + /*  
 */ _b[_xx_chaXyea_1_20222]*_xx_chaXyea_1_20222 + /*  
 */ _b[_xx_chaXyea_1_20223]*_xx_chaXyea_1_20223 

replace fitted_values_educ3 = . if fitted_values_educ3==0
generate lb_educ3 = fitted_values_educ3 - invnormal(0.975)*error
generate ub_educ3 = fitted_values_educ3 + invnormal(0.975)*error
*/

* Category: educ4

cap drop fitted_values_educ4
ge fitted_values_educ4 = /* 
 */ _b[_ww_chaXyea_1_20121]*_ww_chaXyea_1_20121 + /*  
 */ _b[_ww_chaXyea_1_20122]*_ww_chaXyea_1_20122 + /*  
 */ _b[_ww_chaXyea_1_20123]*_ww_chaXyea_1_20123 + /*  
 */ _b[_ww_chaXyea_1_20124]*_ww_chaXyea_1_20124 + /*  
 */ _b[_ww_chaXyea_1_20131]*_ww_chaXyea_1_20131 + /*  
 */ _b[_ww_chaXyea_1_20132]*_ww_chaXyea_1_20132 + /*  
 */ _b[_ww_chaXyea_1_20133]*_ww_chaXyea_1_20133 + /*  
 */ _b[_ww_chaXyea_1_20134]*_ww_chaXyea_1_20134 + /*  
 */ _b[_ww_chaXyea_1_20141]*_ww_chaXyea_1_20141 + /*  
 */ _b[_ww_chaXyea_1_20142]*_ww_chaXyea_1_20142 + /*  
 */ _b[_ww_chaXyea_1_20143]*_ww_chaXyea_1_20143 + /*  
 */ _b[_ww_chaXyea_1_20144]*_ww_chaXyea_1_20144 + /*  
 */ _b[_ww_chaXyea_1_20151]*_ww_chaXyea_1_20151 + /*  
 */ _b[_ww_chaXyea_1_20152]*_ww_chaXyea_1_20152 + /*  
 */ _b[_ww_chaXyea_1_20153]*_ww_chaXyea_1_20153 + /*  
 */ _b[_ww_chaXyea_1_20154]*_ww_chaXyea_1_20154 + /*  
 */ _b[_ww_chaXyea_1_20161]*_ww_chaXyea_1_20161 + /*  
 */ _b[_ww_chaXyea_1_20162]*_ww_chaXyea_1_20162 + /*  
 */ _b[_ww_chaXyea_1_20163]*_ww_chaXyea_1_20163 + /*  
 */ _b[_ww_chaXyea_1_20164]*_ww_chaXyea_1_20164 + /*  
 */ _b[_ww_chaXyea_1_20171]*_ww_chaXyea_1_20171 + /*  
 */ _b[_ww_chaXyea_1_20172]*_ww_chaXyea_1_20172 + /*  
 */ _b[_ww_chaXyea_1_20173]*_ww_chaXyea_1_20173 + /*  
 */ _b[_ww_chaXyea_1_20174]*_ww_chaXyea_1_20174 + /*  
 */ _b[_ww_chaXyea_1_20181]*_ww_chaXyea_1_20181 + /*  
 */ _b[_ww_chaXyea_1_20182]*_ww_chaXyea_1_20182 + /*  
 */ _b[_ww_chaXyea_1_20183]*_ww_chaXyea_1_20183 + /*  
 */ _b[_ww_chaXyea_1_20184]*_ww_chaXyea_1_20184 + /*  
 */ _b[_ww_chaXyea_1_20191]*_ww_chaXyea_1_20191 + /*  
 */ _b[_ww_chaXyea_1_20192]*_ww_chaXyea_1_20192 + /*  
 */ _b[_ww_chaXyea_1_20193]*_ww_chaXyea_1_20193 + /*  
 */ _b[_ww_chaXyea_1_20194]*_ww_chaXyea_1_20194 + /*  
 */ _b[_ww_chaXyea_1_20201]*_ww_chaXyea_1_20201 + /*  
 */ _b[_ww_chaXyea_1_20202]*_ww_chaXyea_1_20202 + /*  
 */ _b[_ww_chaXyea_1_20203]*_ww_chaXyea_1_20203 + /*  
 */ _b[_ww_chaXyea_1_20204]*_ww_chaXyea_1_20204 + /*  
 */ _b[_ww_chaXyea_1_20211]*_ww_chaXyea_1_20211 + /*  
 */ _b[_ww_chaXyea_1_20212]*_ww_chaXyea_1_20212 + /*  
 */ _b[_ww_chaXyea_1_20213]*_ww_chaXyea_1_20213 + /*  
 */ _b[_ww_chaXyea_1_20214]*_ww_chaXyea_1_20214 + /*  
 */ _b[_ww_chaXyea_1_20221]*_ww_chaXyea_1_20221 + /*  
 */ _b[_ww_chaXyea_1_20222]*_ww_chaXyea_1_20222 + /*  
 */ _b[_ww_chaXyea_1_20223]*_ww_chaXyea_1_20223 

replace fitted_values_educ4 = . if fitted_values_educ4==0
generate lb_educ4 = fitted_values_educ4 - invnormal(0.975)*error
generate ub_educ4 = fitted_values_educ4 + invnormal(0.975)*error
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

tsline fitted_values_educ1 fitted_values_educ4 /// fitted_values_educ2 fitted_values_educ3 
	, 	///
    lpattern(longdash shortdash)  /// dash dot longdash shortdash
    lwidth(thick thick) ///
    lcolor(gs11 gs1) ///
	title("Non-white")	///
	subtitle("") ///
	xtitle("") ///
	xlabel(#5 , angle(0) labsize(2.5) ) ///
	ytitle("Coefficient") ///
	ylabel(#10, angle(0) labsize(2.5) format(%9.2f) ) ///
	yscale( axis(1) range(0.13 0.40) lstyle(none)  ) ///
	tline(2019q4, lcolor(red) lpattern(dash) lwidth(0.3) ) ///
	legend(off order(1 "Incomplete primary school" 2 "Incomplete high school" 3 "Incomplete college" 4 "Complete college") ///
	       pos(11) ring(0) col(1) rows(4) size(2.5) symxsize(*0.6) symysize(*0.6)) ///
	note("") ///
	recast(line) ///
	graphregion(fcolor(white)) ///
	scheme( s2gcolor ) /// economist s1mono   s1manual  s2gmanual 
	saving("$ROOT/analysis/tmp/_graph_regression_black_nofe_job_loss_determinants.gph", replace)	

********************************************************************************
* Step 7: Export and save the graph
********************************************************************************

	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_black_nofe_job_loss_determinants.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_black_nofe_job_loss_determinants.png", replace	
	
restore	
