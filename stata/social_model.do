*ssc install estout, replace

clear
capture log close

log using "~/Desktop/regressions.log", text replace
insheet using "~/Desktop/STAT202/Final/data/Y2008.csv", comma

gen gdp_per_capita_current_USD = real(gdppercapitacurrentus)
gen log_GDP_per_capita = log(gdp_per_capita_current_USD + 0.01)

gen child_labor = real(childreninemploymenttotalofchild)
gen life_expect = real(lifeexpectancyatbirthmaleyears)
gen refugee_pop = real(v1017)
gen literacy = real(literacyrateadulttotalofpeopleag)
gen log_child_labor = log(child_labor + 0.01)
gen log_life_expect = log(life_expect + 0.01)
gen log_refugee_pop = log(refugee_pop + 0.01)
gen log_literacy = log(literacy + 0.01)

* SOCIAL DEVELOPMENT MODEL REGRESSIONS
eststo clear
eststo: regress log_GDP_per_capita child_labor
eststo: regress log_GDP_per_capita literacy  
eststo: regress log_GDP_per_capita life_expect refugee_pop
esttab, mtitles("Model A" "Model B" "Model C")

log close
