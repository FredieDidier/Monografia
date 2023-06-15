preserve

* generate dummy variables by quarter
cap drop charact_educ*
quietly gen charact_educ2 = educ2
quietly gen charact_educ3 = educ3
quietly gen charact_educ4 = educ4
quietly xi I.charact_educ2*I.year_quarter, prefix(_zz_) noomit
quietly xi I.charact_educ3*I.year_quarter, prefix(_xx_) noomit
quietly xi I.charact_educ4*I.year_quarter, prefix(_ww_) noomit
	
	
* Main regression
reghdfe job_loss ///
_zz_chaXyea_1_* _xx_chaXyea_1_* _ww_chaXyea_1_* ///  
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] ///
, noabsorb

quietly estimates save "$ROOT/analysis/tmp/save_estimates", replace 
estimates store save_estimates
			 
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("")	/*
	*/ 	keep(_zz_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(1 "2012.1" 2"2012.2" 3 "2012.3" 4 "2012.4" 5 "2013.1" 6 "2013.2" 7 "2013.3" 8 "2013.4" 9 "2014.1" 10 "2014.2" 11 "2014.3" 12 "2014.4" 13 "2015.1" 14 "2015.2" 15 "2015.3" 16 "2015.4" 17 "2016.1" 18 "2016.2" 19 "2016.3" 20 "2016.4" 21 "2017.1" 22 "2017.2" 23 "2017.3" 24 "2017.4" 25 "2018.1" 26 "2018.2" 27 "2018.3" 28 "2018.4" 29 "2019.1" 30 "2019.2" 31 "2019.3" 32 "2019.4" 33 "2020.1" 34 "2020.2" 35 "2020.3" 36 "2020.4" 37 "2021.1" 38 "2021.2" 39 "2021.3" 40 "2021.4" 41 "2022.1" 42 "2022.2" 43 "2022.3" 44 "2022.4", angle(45)  labsize(2.1)  )	/*
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
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_educ2.gph", replace) 
	

	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_educ2.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants_educ2.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_job_loss_determinants_educ2.png", replace	
	
	
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("")	/*
	*/ 	keep(_xx_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(1 "2012.1" 2"2012.2" 3 "2012.3" 4 "2012.4" 5 "2013.1" 6 "2013.2" 7 "2013.3" 8 "2013.4" 9 "2014.1" 10 "2014.2" 11 "2014.3" 12 "2014.4" 13 "2015.1" 14 "2015.2" 15 "2015.3" 16 "2015.4" 17 "2016.1" 18 "2016.2" 19 "2016.3" 20 "2016.4" 21 "2017.1" 22 "2017.2" 23 "2017.3" 24 "2017.4" 25 "2018.1" 26 "2018.2" 27 "2018.3" 28 "2018.4" 29 "2019.1" 30 "2019.2" 31 "2019.3" 32 "2019.4" 33 "2020.1" 34 "2020.2" 35 "2020.3" 36 "2020.4" 37 "2021.1" 38 "2021.2" 39 "2021.3" 40 "2021.4" 41 "2022.1" 42 "2022.2" 43 "2022.3" 44 "2022.4", angle(45)  labsize(2.1)  )	/*
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
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_educ3.gph", replace) 
	
	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_educ3.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants_educ3.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_job_loss_determinants_educ3.png", replace	

	
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("")	/*
	*/ 	keep(_ww_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(1 "2012.1" 2"2012.2" 3 "2012.3" 4 "2012.4" 5 "2013.1" 6 "2013.2" 7 "2013.3" 8 "2013.4" 9 "2014.1" 10 "2014.2" 11 "2014.3" 12 "2014.4" 13 "2015.1" 14 "2015.2" 15 "2015.3" 16 "2015.4" 17 "2016.1" 18 "2016.2" 19 "2016.3" 20 "2016.4" 21 "2017.1" 22 "2017.2" 23 "2017.3" 24 "2017.4" 25 "2018.1" 26 "2018.2" 27 "2018.3" 28 "2018.4" 29 "2019.1" 30 "2019.2" 31 "2019.3" 32 "2019.4" 33 "2020.1" 34 "2020.2" 35 "2020.3" 36 "2020.4" 37 "2021.1" 38 "2021.2" 39 "2021.3" 40 "2021.4" 41 "2022.1" 42 "2022.2" 43 "2022.3" 44 "2022.4", angle(45)  labsize(2.1)  )	/*
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
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_educ4.gph", replace) 
	
	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_educ4.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants_educ4.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_job_loss_determinants_educ4.png", replace	
	
restore	
