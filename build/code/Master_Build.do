* ------------------------------------------------------------------------

*** FOLDERS PATHWAY

* check what your username is in Stata by typing "di c(username)"
if "`c(username)'" == "Francisco"   {
	version 16.1
	global ROOT "C:/Users/Francisco/Dropbox/Research"
}
else if "`c(username)'" == "DELL"   {
	version 16.1
	global ROOT "C:\Users\DELL\Documents\GitHub\Monografia-Fredie"
	global ROOT_DATA "D:\Dropbox\DataZoom\BasesIBGE\datazoom_rar\PNAD_CONTINUA\pnadcontinua_trimestral_2022024"
}
else if "`c(username)'" == "f.cavalcanti"   {
	version 16.1
	global ROOT "C:/Users/f.cavalcanti/Documents/GitHub/"
}
else if "`c(username)'" == "Fredie"   {
	version 14.1
	global ROOT "/Users/Fredie/Documents/GitHub/Monografia"
	global ROOT_DATA "/Users/Fredie/Documents/GitHub/Monografia/build/input"
}

cd $ROOT

* generate temporary folder
capture mkdir "$ROOT\build\tmp"
cap mkdir "$ROOT\build\input"
cap mkdir "$ROOT\build\output"

*****
* Rodar painel
*****
do "$ROOT/build/code/_geracao_paineis"
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
