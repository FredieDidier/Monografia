preserve
label variable job_loss "Lost work in the next quarter"
label variable educ0 "No College degree"
label variable educ1 "Complete College degree"
label variable signed_work_card "Formal Employee"
label variable hours_worked "Working hours per week"
label variable temporary_worker "Temporary worker"
label variable social_security_taxpayer "Social security taxpayer"
label variable age "Age"
label variable monthly_work_income "Monthly work income (R\$)"
label variable urbana "Lives in urban area"

* Generate gender variables
gen men = (gender == 1)
label variable men "Men"
gen women = (gender == 2)
label variable women "Women"

* Generate race variables
gen white_race = (negro == 0)
label variable white_race "White race"
gen nonwhite_race = (negro == 1)
label variable nonwhite_race "Non-white race"

* Generate position variables
gen formal = (position_names == "Formal")
label variable formal "Formal"
gen informal = (position_names == "Informal")
label variable informal "Informal"
gen formal_public = (position == 9)
label variable formal_public "Formal Public Sector"
gen formal_private = (position == 3 | position == 5 | position == 7)
label variable formal_private "Formal Private"
gen formal_selfemployed = (position == 5)
label variable formal_selfemployed "Formal Self-Employed"
gen informal_selfemployed = (position == 6)
label variable informal_selfemployed "Informal Self-Employed"
gen informal_employee = (signed_work_card == 0)
label variable informal_employee "Informal Employee"

eststo clear
eststo store_statistics: quietly estpost summarize ///
    job_loss ///
    educ0 educ1 ///
    men women ///
    white_race nonwhite_race ///
    formal informal ///
    formal_public formal_private ///
    formal_selfemployed informal_selfemployed ///
    signed_work_card informal_employee ///
    hours_worked ///
    temporary_worker ///
    social_security_taxpayer ///
    age ///
    monthly_work_income ///
    urbana ///
    [aw=weights] 

* additional stats
count
local num_obs = r(N)
di "`num_obs'"
estadd scalar num_obs = `num_obs' : store_statistics

* local notes
local ttitle "Descriptive statistics"
local tnotes "Notes: Sample restricted to employed individuals at the interview date. The sample is at the person-quarter level and covers 2012Q1 to 2024Q4. Lost work in the next quarter is defined as a transition from employment in quarter \$t\$ to either unemployment or out of the labor force in quarter \$t+1\$. All statistics are weighted using survey weights."
    
#delim ;    
    esttab store_statistics using "$ROOT/analysis/output/descriptive_statistics/_table_descriptive_statistics.tex",
        cells("Mean(fmt(%12.2fc)) SD(fmt(%12.2fc)) Min(fmt(%12.0fc)) Max(fmt(%12.0fc))")
        label
        noobs
        nonumber
        nomtitle
        prehead(
            "\begin{table}[H]"
            "\centering"
            "\caption{`ttitle'}"
            "\label{tabledescriptivestatistics}"
            "\begin{threeparttable}"
            "\scalebox{0.90}{"
            "\begin{tabular}{l*{4}{r}}"
            "\hline \hline"
            "& mean & sd & min & max\\"
            "\hline"
        )
        postfoot(
            "\hline"
        )
        posthead("")
        prefoot(
            "\hline"
        )
        stats(num_obs, fmt(%12.0fc) labels("Number of observations"))
    ;    
#delim cr

* Append the closing of the table manually
file open myfile using "$ROOT/analysis/output/descriptive_statistics/_table_descriptive_statistics.tex", write append
file write myfile "\hline \hline" _n
file write myfile "\end{tabular}" _n
file write myfile "}" _n
file write myfile "\begin{tablenotes}" _n
file write myfile "\item \begin{minipage}[t]{0.9\textwidth}" _n
file write myfile "\end{minipage}" _n
file write myfile "\end{tablenotes}" _n
file write myfile "\end{threeparttable}" _n
file write myfile "\end{table}" _n
file close myfile
        
restore
