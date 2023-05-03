

use "$ROOT/build/output/regression/main_data.dta", clear

* numerador: position_names = Formal ou Informal
* denominador: position_transition = Formal to Non-Employed ou Informal to Non-Employed
* Iremos utilizar a variável sector_code

gen denominador = 1 if position_names == "Formal"
replace denominador = 1 if position_names == "Informal"

gen numerador = 1 if position_transition == "Formal to Non-Employed"
replace numerador = 1 if position_transition == "Informal to Non-Employed"

gen item1 = numerador * weights
gen item2 = denominador * weights

gen job_loss = (item1/item2) * 100

