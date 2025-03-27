* ------------------------------------------------------------------------

cap drop macro all
set more off, permanently

*** FOLDERS PATHWAY

* check what your username is in Stata by typing "di c(username)"
if "`c(username)'" == "Francisco"   {
	version 16.1
	global ROOT "C:/Users/Francisco/Dropbox/Research"
}
else if "`c(username)'" == "DELL"   {
	version 16.1
	global ROOT "D:\OneDrive\Documentos\GitHub\Monografia-Fredie"
}
else if "`c(username)'" == "f.cavalcanti"   {
	version 16.1
	global ROOT "C:/Users/f.cavalcanti/Documents/GitHub/"
}
else if "`c(username)'" == "fredie"   {
	version 14.1
	global ROOT "/Users/fredie/Documents/GitHub/Monografia"
}

cd $ROOT

* generate temporary folder
capture mkdir "$ROOT/analysis/tmp"
cap mkdir "$ROOT/analysis/input"
cap mkdir "$ROOT/analysis/output"
cap mkdir "$ROOT/analysis/output/graph"
cap mkdir "$ROOT/analysis/output/regressions"
cap mkdir "$ROOT/analysis/output/descriptive_statistics"

* 1. Replicar o trabalho do Journal of Public Economics "Inequality of the coronavirus shock".
* 2. Primeira analise: perda de emprego #grafico
*do "$ROOT/analysis/code/_graph_job_loss_sectors.do" // OLHAR NO R
* 2.1. Porcentagem de perda de emprego por setor/categoria de emprego e educacao. #graficos
* 3. Regressao para determinantes de perda de emprego e renda.
use "$ROOT/build/output/regression/main_data.dta", clear
gen denominador = 1 if position_names == "Formal"
replace denominador = 1 if position_names == "Informal"

gen numerador = 1 if position_transition == "Formal to Non-Employed"
replace numerador = 1 if position_transition == "Informal to Non-Employed"

gen item1 = numerador * weights
gen item2 = denominador * weights
cap drop job_loss
gen job_loss = .
replace job_loss = 0 if denominador == 1
replace job_loss = 1 if numerador == 1
egen ind = group(id_code)
xtset ind year_quarter
* generate variable of quartely date
*tostring year_quarter, replace
*gen quarter = substr(year_quarter, 5, 1)
*gen year = substr(year_quarter, 1, 4)
*gen iten1 = year + "." + quarter
*gen  trim = quarterly(iten1, "YQ")
*drop iten*
*format trim %tm
*destring id_code, replace
*xtset id_code trim

global CONTROLS signed_work_card cnpj job_function hours_worked temporary_worker position social_security_taxpayer gender race age monthly_work_income weights job_start educ urbana state

* edit indpendent variables
* Criar educ1 (Complete College) - corresponde a educ==1 no R
gen educ1 = 1 if educ == 1
replace educ1 = 0 if educ1 == .
label variable educ1 "Complete College"

* Criar educ0 (No College) - corresponde a educ==0 no R
gen educ0 = 1 if educ == 0
replace educ0 = 0 if educ0 == .
label variable educ0 "No College"

cap drop sector_numeric
cap drop occupation_numeric
encode sector_code, gen(sector_numeric)
encode occupation_code, gen(occupation_numeric)

*****************************
* Main Analysis
*****************************

*  Table: Descriptive Statistics
do "$ROOT/analysis/code/01_table_matching_algorithm.do"

*  Table: Descriptive Statistics
do "$ROOT/analysis/code/01_table_descriptive_statistics.do"

*  Table: Probability of Job Loss Relative to Least Educated Individuals by Educational Level Categories
do "$ROOT/analysis/code/06_table_regression_job_loss_determinants.do"

*  Table: Probability of Job Loss Relative to Least Educated Individuals During Pandemic by Educational Level Categories
do "$ROOT/analysis/code/06_table_regression_job_loss_determinants_with_interaction.do"

*  Graph: Probability of Job Loss Relative to Least Educated Individuals by Educational Level Categories
do "$ROOT/analysis/code/07_graph_regression_job_loss_determinants.do"

*****************************
* Heterogeneous Analysis
*****************************

* Demographics
	
	do "$ROOT/analysis/code/09_graph_regression_men_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/11_graph_regression_women_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/13_graph_regression_black_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/15_graph_regression_white_nofe_job_loss_determinants.do"
	
	* Combine graphs
	graph combine "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants.gph" "$ROOT/analysis/tmp/_graph_regression_women_nofe_job_loss_determinants.gph"  "$ROOT/analysis/tmp/_graph_regression_white_nofe_job_loss_determinants.gph" "$ROOT/analysis/tmp/_graph_regression_black_nofe_job_loss_determinants.gph" /*
		*/ , 	/*
		*/  /* ycommon
		*/  /* xcommon 
		*/ cols(2) /* 
		*/ scheme() /*
		*/ commonscheme /*
		*/ xsize(9) /*
		*/ ysize(6) /*
		*/ scale(1) /*
		*/ graphregion(margin(tiny))	 /* 
		*/ plotregion(margin(zero) ifcolor(none)) /*
		*/ saving("$ROOT/analysis/tmp/_graph_regression_heterogeneous_analysis_demographics_job_loss_determinants.gph", replace)	
	
		* save graph 
		graph use "$ROOT/analysis/tmp/_graph_regression_heterogeneous_analysis_demographics_job_loss_determinants.gph"
		*erase "$ROOT/analysis/graph/_graph_regression_heterogeneous_analysis_demographics_job_loss_determinants.gph"	
		graph export "$ROOT/analysis/output/graph/_graph_regression_heterogeneous_analysis_demographics_job_loss_determinants.png", replace		
		
	cap erase "$ROOT/analysis/output/graph/_graph_regression_women_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/output/graph/_graph_regression_men_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/output/graph/_graph_regression_white_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/output/graph/_graph_regression_black_nofe_job_loss_determinants.png"	
	
* Sectors

	do "$ROOT/analysis/code/17_graph_regression_agriculture_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/19_graph_regression_manufacture_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/21_graph_regression_trade_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/23_graph_regression_services_nofe_job_loss_determinants.do"

	* Combine graphs
	graph combine "$ROOT/analysis/tmp/_graph_regression_agriculture_nofe_job_loss_determinants.gph" "$ROOT/analysis/tmp/_graph_regression_manufacture_nofe_job_loss_determinants.gph"  "$ROOT/analysis/tmp/_graph_regression_trade_nofe_job_loss_determinants.gph" "$ROOT/analysis/tmp/_graph_regression_services_nofe_job_loss_determinants.gph"  /*
		*/ , 	/*
		*/  /* ycommon
		*/  /* xcommon 
		*/ cols(2) /* 
		*/ scheme() /*
		*/ commonscheme /*
		*/ xsize(9) /*
		*/ ysize(6) /*
		*/ scale(1) /*
		*/ graphregion(margin(tiny))	 /* 
		*/ plotregion(margin(zero) ifcolor(none)) /*
		*/ saving("$ROOT/analysis/tmp/_graph_regression_heterogeneous_analysis_sectors_job_loss_determinants.gph", replace)	
	
		* save graph 
		graph use "$ROOT/analysis/tmp/_graph_regression_heterogeneous_analysis_sectors_job_loss_determinants.gph"
		*erase "$ROOT/analysis/graph/_graph_regression_heterogeneous_analysis_sectors_job_loss_determinants.gph"	
		graph export "$ROOT/analysis/output/graph/_graph_regression_heterogeneous_analysis_sectors_job_loss_determinants.png", replace		
		
	cap erase "$ROOT/analysis/output/graph/_graph_regression_agriculture_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/output/graph/_graph_regression_manufacture_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/output/graph/_graph_regression_trade_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/output/graph/_graph_regression_services_nofe_job_loss_determinants.png"
	
* Position: Formal, informal, public, and private

	do "$ROOT/analysis/code/24_graph_regression_formal_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/25_graph_regression_informal_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/26_graph_regression_public_nofe_job_loss_determinants.do"
	do "$ROOT/analysis/code/27_graph_regression_private_nofe_job_loss_determinants.do"

	* Combine graphs
	graph combine "$ROOT/analysis/tmp/_graph_regression_formal_nofe_job_loss_determinants.gph" "$ROOT/analysis/tmp/_graph_regression_informal_nofe_job_loss_determinants.gph"  "$ROOT/analysis/tmp/_graph_regression_public_nofe_job_loss_determinants.gph" "$ROOT/analysis/tmp/_graph_regression_private_nofe_job_loss_determinants.gph"  /*
		*/ , 	/*
		*/  /* ycommon
		*/  /* xcommon 
		*/ cols(2) /* 
		*/ scheme() /*
		*/ commonscheme /*
		*/ xsize(9) /*
		*/ ysize(6) /*
		*/ scale(1) /*
		*/ graphregion(margin(tiny))	 /* 
		*/ plotregion(margin(zero) ifcolor(none)) /*
		*/ saving("$ROOT/analysis/tmp/_graph_regression_heterogeneous_analysis_position_job_loss_determinants.gph", replace)	
	
		* save graph 
		graph use "$ROOT/analysis/tmp/_graph_regression_heterogeneous_analysis_position_job_loss_determinants.gph"		
		graph export "$ROOT/analysis/output/graph/_graph_regression_heterogeneous_analysis_position_job_loss_determinants.png", replace	
		cap erase "$ROOT/analysis/graph/tmp/_graph_regression_heterogeneous_analysis_position_job_loss_determinants.gph"	
		
	cap erase "$ROOT/analysis/tmp/_graph_regression_agriculture_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/tmp/_graph_regression_industries_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/tmp/_graph_regression_trade_nofe_job_loss_determinants.png"
	cap erase "$ROOT/analysis/tmp/_graph_regression_services_nofe_job_loss_determinants.png"	

********************************************************
**	delete temporary files
********************************************************

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.dta"
foreach datafile of local datafiles {
        rm `datafile'
}

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.csv"
foreach datafile of local datafiles {
        rm `datafile'
}

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.txt"
foreach datafile of local datafiles {
        rm "`datafile'"
}

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.nc"
foreach datafile of local datafiles {
        rm "`datafile'"
}


cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.pdf"
foreach datafile of local datafiles {
        rm `datafile'
}

* clear all
clear
