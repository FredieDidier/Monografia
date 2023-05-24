preserve
* amazonia legal

reghdfe job_loss ///
position ///
signed_work_card /// 
hours_worked /// i.job_function 
i.temporary_worker /// i.job_function 
i.educ /// 
i.work_category /// 
monthly_work_income ///
urbana ///
job_start ///
i.year_quarter ///
[aw=weights] ///
,  absorb(ind )


quietly estimates save "$ROOT/analysis/tmp/save_estimates", replace 
estimates store save_estimates
	

coefplot (save_estimates, label(nome da especificacao))  	/*
	*/ 	, 	/*
	*/ 	title("bla bla bla")	/*
	*/ 	keep(signed_work_card hours_worked) 	/*
	*/ 	vertical 	/*
	*/	subtitle("")	/*
	*/ 	label(size(small)) /*
	*/	xtitle("") 	/*
	*/	xlabel("", angle(45)  )	/*	
	*/	ytitle("")	/*	
	*/	yline( 0.00, lpattern(longdash_dot) lwidth(medium) lcolor(black))	/* add horizontal lines at specified y values
	*/	ylabel(#7) 	/*
	*/	yscale( axis(1) range() lstyle(none) )	/* how y axis looks	
	*/	legend(row(1) size(small)  symxsize(*0.2) ) /* legend explaining what means what		
	*/	note("")	/*
	*/  recast(bar) /*  recast(bar)
	*/ 	citop  	/*
	*/ 	ciopts(recast(rcap))  barwidth(0.10) color(*.8) 	/*
	*/ 	saving("$ROOT/analysis/tmp/_graph_regression_job_loss_determinants.gph", replace) 
	
	
	* save graph 
	graph use "$ROOT/analysis/tmp/_graph_regression_job_loss_determinants.gph"
	*erase "$ROOT/analysis/graph/_graph_regression_job_loss_determinants.gph"	
	graph export "$ROOT/analysis/output/graph/_graph_regression_job_loss_determinants.png", replace	
	
restore	