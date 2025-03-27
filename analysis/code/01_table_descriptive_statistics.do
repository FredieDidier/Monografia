preserve

label variable job_loss "Lost work in the next quarter"
label variable educ0 "No College"
label variable educ1 "Complete College"
label variable signed_work_card "Individual with a formal contract"
label variable hours_worked "Working hours per week"
label variable temporary_worker "Temporary worker"
label variable social_security_taxpayer "Social security taxpayer"
label variable homem "Men"
label variable negro "Non-white race"
label variable age "Years old"
label variable monthly_work_income "Monthly work income (R\\$)"
label variable urbana "Lives in urban area"

gen sec_Agriculture = 1 if sectors == "Agriculture"
replace sec_Agriculture =0 if sec_Agriculture ==.
label variable sec_Agriculture "Agricultural sector"

gen sec_Manufacture = 1 if sectors == "Manufacture"
replace sec_Manufacture =0 if sec_Manufacture ==.
label variable sec_Manufacture "Manufacture sector"

gen sec_Services = 1 if sectors == "Services"
replace sec_Services =0 if sec_Services ==.
label variable sec_Services "Services sector"

gen sec_Trade = 1 if sectors == "Trade" | sectors == "Construction"
replace sec_Trade =0 if sec_Trade ==.
label variable sec_Trade "Trade sector"

gen po_Informal = 1 if position_names == "Informal"
replace po_Informal =0 if po_Informal ==.
label variable po_Informal "Informality"

gen po_Public = 1 if position == 9  /* Formal Public Sector
	*/ | position == 10 /* Informal Public Sector
	*/ 
	
replace po_Public =0 if po_Public ==. 
label variable po_Public "Government"

eststo store_statistics: quietly estpost summarize ///
job_loss ///
educ0 educ1 ///
monthly_work_income  ///
hours_worked ///
temporary_worker ////
social_security_taxpayer ///
signed_work_card ///
po_Informal ///
po_Public ///
sec_Agriculture ///
sec_Manufacture ///
sec_Services ///
sec_Trade ///
homem ///
age ///
negro ///
urbana ///
[aw=weights] 

/*
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] 
*/

* additional stats
count
local num_obs = r(N)
di "`num_obs'"
estadd scalar num_obs = `num_obs'

* local notes
local ttitle "Descriptive statistics"
local tnotes "Source: IBGE's Quarterly Continuous PNAD from 2012 to 2024."
	
#delim ;    
	esttab store_statistics using "$ROOT/analysis/output/descriptive_statistics/_table_descriptive_statistics.tex",		
		cells("mean(fmt(%12.2fc)) sd(fmt(%12.2fc))  min(fmt(%12.0fc)) max(fmt(%20.0fc))  ")
		label
		prehead(
			"\begin{table}[H]"
			"\centering"
			"\label{tabledescriptivestatistics}"
			"\caption{`ttitle'}"					
			"\scalebox{0.75}{"
			"\begin{tabular}{l*{@span}{r}}"
			"\hline \hline"			
    		)
		postfoot(
			"\hline \hline"
			"\end{tabular}"		
			"\begin{tablenotes}"
			"\item \scriptsize{`tnotes'}"
			"\end{tablenotes}"
			"}"
			"\end{table}"
    		)
    	stats(num_obs  , fmt(%12.0fc) labels("Number of observations"))	
    	
    	replace
 ;	 
		
restore
