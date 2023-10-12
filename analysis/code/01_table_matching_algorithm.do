preserve

clear

use "$ROOT/build/output/painel_2012_1.dta", clear
append using "$ROOT/build/output/painel_2012_2.dta", force
append using "$ROOT/build/output/painel_2012_3.dta", force
append using "$ROOT/build/output/painel_2012_4.dta", force
append using "$ROOT/build/output/painel_2020_2.dta", force
append using "$ROOT/build/output/painel_2020_3.dta", force
append using "$ROOT/build/output/painel_2020_4.dta", force
append using "$ROOT/build/output/painel_2021_1.dta", force

* extract files .csv
clear
cd  "$ROOT/build/output"
pwd

* set files
local uniquefile : dir . files "*.dta"
display `uniquefile'

foreach name_uniquefile in `uniquefile' {	
		*append using "${tmp_dir}/`yr'-`mn'-01_amc_rainfall.dta"
		append using "$ROOT/build/output/`name_uniquefile'", force
	}

	
keep Ano Trimestre UPA Estrato V1008 V1014 V1016 V1028 painel idind	
compress	

sort idind Ano Trimestre

collapse (firstnm) UPA Estrato V1008 V1014 V1016 V1028 painel, by(idind Ano Trimestre)
	
*The share of overlap between pairs of quarters between the quarterly data of PNAD Continuous (in \%)
** Household vs. Individuals
** Time interval 1,2,3,4
** Accross panels 

cap drop n_that_appear_*
cap drop iten*

by idind, sort: gen iten1 = _n
by idind, sort: egen iten2 = max(iten1)
replace iten2 =. if iten1 >1

by painel, sort: egen n_that_appear_1_time = count(idind) if iten2 ==1
by painel, sort: egen n_that_appear_2_time = count(idind) if iten2 ==2
by painel, sort: egen n_that_appear_3_time = count(idind) if iten2 ==3
by painel, sort: egen n_that_appear_4_time = count(idind) if iten2 ==4
by painel, sort: egen n_that_appear_5_time = count(idind) if iten2 ==5
by painel, sort: egen n_that_appear_all = count(idind) if iten2 !=.

cap drop lala*
by painel, sort: egen lala1 = mode(n_that_appear_1_time)
replace lala1 = 0 if lala1==.
by painel, sort: egen lala2 = mode(n_that_appear_2_time)
replace lala2 = 0 if lala2==.
by painel, sort: egen lala3 = mode(n_that_appear_3_time)
replace lala3 = 0 if lala3==.
by painel, sort: egen lala4 = mode(n_that_appear_4_time)
replace lala4 = 0 if lala4==.
by painel, sort: egen lala5 = mode(n_that_appear_5_time)
replace lala5 = 0 if lala5==.

by painel, sort: egen lalaall = mode(n_that_appear_all) 
replace lalaall = 0 if lalaall==.


gen somatorio = lala1 + lala2 + lala3 + lala4 + lala5
gen diferenca  = somatorio - lalaall

cap drop iten*


gen share_of_overlap_between_pairs1 = (lala1 / lalaall)*100

gen share_of_overlap_between_pairs2 = (lala2 / lalaall)*100

gen share_of_overlap_between_pairs3 = (lala3 / lalaall)*100

gen share_of_overlap_between_pairs4 = (lala4 / lalaall)*100

gen share_of_overlap_between_pairs5 = (lala5 / lalaall)*100



by painel, sort: gen lala = group(idind)
by painel, sort: egen lala = group(idind) 

*Losses between pairs of trimesters, following the Basic matching criteria (in \%)

*Losses between pairs of trimesters, following the Advanced matching criteria (in \%)
		
restore