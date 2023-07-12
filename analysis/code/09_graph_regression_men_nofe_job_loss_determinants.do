preserve

* Regression for gender = men with no fixed effects 

 keep if gender == 1

 sample 50

* generate dummy variables by quarter
cap drop charact_educ*
quietly gen charact_educ1 = educ1
quietly gen charact_educ2 = educ2
quietly gen charact_educ3 = educ3
quietly gen charact_educ4 = educ4
quietly xi I.charact_educ1*I.year_quarter, prefix(_yy_) noomit
quietly xi I.charact_educ2*I.year_quarter, prefix(_zz_) noomit
quietly xi I.charact_educ3*I.year_quarter, prefix(_xx_) noomit
quietly xi I.charact_educ4*I.year_quarter, prefix(_ww_) noomit
	
	
* Main regression
reg job_loss ///
_yy_chaXyea_1_* _zz_chaXyea_1_* _xx_chaXyea_1_* _ww_chaXyea_1_* ///  
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer race age monthly_work_income job_start ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

quietly estimates save "$ROOT/analysis/tmp/save_estimates", replace 
estimates store save_estimates

coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("")	/*
	*/ 	keep(_yy_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(4 "2012.4" 8 "2013.4" 12 "2014.4" 16 "2015.4" 20 "2016.4" 24 "2017.4" 28 "2018.4" 32 "2019.4" 36 "2020.4" 40 "2021.4" 44 "2022.4", angle(45)  labsize(2.1)  )	/*
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(off row(1) size(3)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(connected) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  color(*.8) 	/* barwidth(0.10) 
	*/ 	scheme(s2color ) /*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ1.gph", replace) 
	

	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ1.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants_educ2.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_men_nofe_job_loss_determinants_educ1.png", replace	
			 
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("")	/*
	*/ 	keep(_zz_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(4 "2012.4" 8 "2013.4" 12 "2014.4" 16 "2015.4" 20 "2016.4" 24 "2017.4" 28 "2018.4" 32 "2019.4" 36 "2020.4" 40 "2021.4" 44 "2022.4", angle(45)  labsize(2.1)  )	/*
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(off row(1) size(3)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(connected) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  color(*.8) 	/* barwidth(0.10) 
	*/ 	scheme(s2color ) /*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ2.gph", replace) 
	

	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ2.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants_educ2.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_men_nofe_job_loss_determinants_educ2.png", replace	
	
	
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("")	/*
	*/ 	keep(_xx_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(4 "2012.4" 8 "2013.4" 12 "2014.4" 16 "2015.4" 20 "2016.4" 24 "2017.4" 28 "2018.4" 32 "2019.4" 36 "2020.4" 40 "2021.4" 44 "2022.4", angle(45)  labsize(2.1)  )	/*
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(off row(1) size(3)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(connected) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  color(*.8) 	/* barwidth(0.10) 
	*/ 	scheme(s2color ) /*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ3.gph", replace) 
	
	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ3.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants_educ3.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_men_nofe_job_loss_determinants_educ3.png", replace	

	
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("")	/*
	*/ 	keep(_ww_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(4 "2012.4" 8 "2013.4" 12 "2014.4" 16 "2015.4" 20 "2016.4" 24 "2017.4" 28 "2018.4" 32 "2019.4" 36 "2020.4" 40 "2021.4" 44 "2022.4", angle(45)  labsize(2.1)  )	/*
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(off row(1) size(3)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(connected) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  color(*.8) 	/* barwidth(0.10) 
	*/ 	scheme(s2color ) /*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ4.gph", replace) 
	
	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ4.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants_educ4.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_men_nofe_job_loss_determinants_educ4.png", replace	
	
* combine graphs
graph combine "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ1.gph" "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ2.gph" "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ3.gph" "$ROOT/analysis/tmp/_graph_regression_men_nofe_job_loss_determinants_educ4.gph", /*
	*/ ycommon /*
	*/ xcommon /*
	*/ cols(1) /*
	*/ scheme(s2color) /*
	*/ commonscheme /*
	*/ xsize(4) /*
	*/ ysize(5) /*
	*/ scale(1) /*
	*/ graphregion(margin(tiny))	 /* 
	*/	plotregion(margin(zero) ifcolor(none))	
	
	* save graph 
	graph export "$ROOT/analysis/output/graph/_graph_regression_men_nofe_job_loss_determinants.png", replace	
	
	
restore	
