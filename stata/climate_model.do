*ssc install estout, replace

clear
capture log close

log using "~/Desktop/regressions.log", text replace
insheet using "~/Desktop/STAT202/Final/data/Y2008.csv", comma

gen gdp_per_capita_current_USD = real(gdppercapitacurrentus)
gen log_GDP_per_capita = log(gdp_per_capita_current_USD + 0.01)

gen co2 = real(co2emissionskt)
gen methane = real(methaneemissionsktofco2equivalen)
gen nitrous = real(nitrousoxideemissionsthousandmet)
gen paved_roads = real(roadspavedoftotalroads)
gen electric_cons = real(electricpowerconsumptionkwhperca)
gen log_co2 = log(co2 + 0.01)
gen log_methane = log(methane + 0.01)
gen log_nitrous = log(nitrous + 0.01)
gen log_paved_roads = log(paved_roads + 0.01)
gen log_electric_cons = log(electric_cons + 0.01)


* CLIMATE MODEL REGRESSIONS
eststo clear
eststo: regress log_GDP_per_capita paved_roads log_electric_cons log_co2 log_nitrous log_methane
eststo: regress log_GDP_per_capita paved_roads log_electric_cons log_co2
eststo: regress log_GDP_per_capita paved_roads log_electric_cons
gen co2_elec = log_electric_cons * log_co2
eststo: regress log_GDP_per_capita log_electric_cons log_co2 co2_elec
esttab, mtitles("Model A" "Model B" "Model C" "Model D")

log close
