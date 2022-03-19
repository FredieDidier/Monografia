cd C:\GitHub\Monografia

* Gerando painel

datazoom_pnadcontinua, years(2012/2021) ///
					original("C:\GitHub\Monografia\Raw Data") ///
					saving("C:\GitHub\Monografia\Raw Data\Painel") ///
					idrs
					
* Gerando arquivos separados por pares de trimestres

local files : dir "Raw Data/Painel/pnadcontinua" files "*.dta"

local ano1 2012 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021
local tri1 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3
local ano2 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021 2021
local tri2 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 

forvalues i = 1/10{

	local ano1: word `i' of `ano1'
	local tri1: word `i' of `tri1'
	local ano2: word `i' of `ano2'
	local tri2: word `i' of `tri2'
	
		foreach file in `files'{
	
			use `file' if (Ano == `ano1' & Trimestre == `tri1') | (Ano == `ano2' & Trimestre == `tri2'), clear
			
			save "C:\GitHub\Monografia\Raw Data\Trimestres\painel_`ano1'_`tri1'", replace
		
		}
	}
