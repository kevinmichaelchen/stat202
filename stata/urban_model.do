*ssc install estout, replace

clear
capture log close

log using "~/Desktop/regressions.log", text replace
insheet using "~/Desktop/STAT202/Final/data/Y2008.csv", comma

gen gdp_per_capita_current_USD = real(gdppercapitacurrentus)
gen log_GDP_per_capita = log(gdp_per_capita_current_USD + 0.01)

gen urb_sanitation = real(improvedsanitationfacilitiesurba)
gen urb_population = real(urbanpopulationoftotal)
gen urb_water = real(improvedwatersourceurbanofurbanp)

* URBAN MODEL REGRESSIONS
eststo clear
eststo: regress log_GDP_per_capita urb_sanitation urb_population urb_water
eststo: regress log_GDP_per_capita urb_population urb_water
eststo: regress log_GDP_per_capita urb_sanitation urb_population
esttab, mtitles("Model A" "Model B" "Model C")

log close
