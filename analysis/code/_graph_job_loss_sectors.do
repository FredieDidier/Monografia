

use "$ROOT/build/output/regression/main_data.dta", clear

* numerador: position_names = Formal ou Informal
* denominador: position_transition = Formal to Non-Employed ou Informal to Non-Employed
* Iremos utilizar a variável sector_code

gen numerador = 1 if position_names == "Formal"
replace numerador = 1 if position_names == "Informal"

gen denominador = 1 if position_transition == "Formal to Non-Employed"
replace denominador = 1 if position_transition == "Informal to Non-Employed"

gen item1 = numerador * weights
gen item2 = denominador * weights

collapse (sum) item1 item2, by(sector_code)

gen job_loss = (item2/item1) * 100

graph hbar (mean) job_loss if sector_code == "Construcao", saving("$ROOT/analysis/tmp/_graph_job_loss_sectors.gph")

graph use "$ROOT/analysis/tmp/_graph_job_loss_sectors.gph"
graph export "$ROOT/analysis/output/graph/_graph_job_loss_sectors.gph"
