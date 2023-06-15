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
,  absorb(ind )


quietly estimates save "$ROOT/analysis/tmp/save_estimates", replace 
estimates store save_estimates
			 
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("Incomplete high school")	/*
	*/ 	keep(_zz_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(4 "2012.4" 8 "2013.4" 12 "2014.4" 16 "2015.4" 20 "2016.4" 24 "2017.4" 28 "2018.4" 32 "2019.4" 36 "2020.4" 40 "2021.4" 44 "2022.4", angle(45)  labsize(3.1)  )	/*
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7, angle(0) ) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(off row(1) size(3)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(connected) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  color(*.8) 	/* barwidth(0.10) 
	*/ 	scheme(s2color ) /*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_fixed_effect_model_educ2.gph", replace) 
		
	
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("Incomplete college")	/*
	*/ 	keep(_xx_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(4 "2012.4" 8 "2013.4" 12 "2014.4" 16 "2015.4" 20 "2016.4" 24 "2017.4" 28 "2018.4" 32 "2019.4" 36 "2020.4" 40 "2021.4" 44 "2022.4", angle(45)  labsize(3.1)  )	/*
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7, angle(0) ) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(off row(1) size(3)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(connected) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  color(*.8) 	/* barwidth(0.10) 
	*/ 	scheme(s2color ) /*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_fixed_effect_model_educ3.gph", replace) 
	
coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("Complete college")	/*
	*/ 	keep(_ww_chaXyea_1_* ) 	/* _xx_chaXyea_1_* _ww_chaXyea_1_*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/	xtitle("") 	/*
	*/	graphregion(fcolor(white)) 	/*	
	*/	xlabel(4 "2012.4" 8 "2013.4" 12 "2014.4" 16 "2015.4" 20 "2016.4" 24 "2017.4" 28 "2018.4" 32 "2019.4" 36 "2020.4" 40 "2021.4" 44 "2022.4", angle(45)  labsize(3.1)  )	/*
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7, angle(0) ) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(off row(1) size(3)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(connected) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  color(*.8) 	/* barwidth(0.10) 
	*/ 	scheme(s2color ) /*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_fixed_effect_model_educ4.gph", replace) 
	
	
* combine graphs
graph combine "$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_fixed_effect_model_educ2.gph" "$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_fixed_effect_model_educ3.gph" "$ROOT/analysis/tmp/_graph_regression_job_loss_determinants_fixed_effect_model_educ4.gph", /*
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
	graph export "$ROOT/analysis/output/graph/_graph_regression_job_loss_determinants_fixed_effect_model.png", replace	
	
restore	