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

gen high_tech = real(hightechnologyexportsofmanufactu)
gen research = real(researchanddevelopmentexpenditur)
gen journals = real(scientificandtechnicaljournalart)
gen log_high_tech = log(high_tech + 0.01)
gen log_research = log(research + 0.01)
gen log_journals = log(journals + 0.01)

gen child_labor = real(childreninemploymenttotalofchild)
gen life_expect = real(lifeexpectancyatbirthmaleyears)
gen refugee_pop = real(v1017)
gen literacy = real(literacyrateadulttotalofpeopleag)
gen log_child_labor = log(child_labor + 0.01)
gen log_life_expect = log(life_expect + 0.01)
gen log_refugee_pop = log(refugee_pop + 0.01)
gen log_literacy = log(literacy + 0.01)



* CLIMATE MODEL REGRESSIONS
eststo clear
eststo: regress log_GDP_per_capita paved_roads log_electric_cons log_co2 log_nitrous log_methane
eststo: regress log_GDP_per_capita paved_roads log_electric_cons log_co2
eststo: regress log_GDP_per_capita paved_roads log_electric_cons
gen co2_elec = log_electric_cons * log_co2
eststo: regress log_GDP_per_capita log_electric_cons log_co2 co2_elec
esttab, mtitles("Model A" "Model B" "Model C" "Model D")

* URBAN MODEL REGRESSIONS
eststo clear
eststo: regress log_GDP_per_capita urb_sanitation urb_population urb_water
eststo: regress log_GDP_per_capita urb_population urb_water
eststo: regress log_GDP_per_capita urb_sanitation urb_population
esttab, mtitles("Model A" "Model B" "Model C")

* SCIENCE MODEL REGRESSIONS
eststo clear
gen tech_jour = log_high_tech * log_journals
eststo: regress log_GDP_per_capita log_research log_high_tech log_journals
eststo: regress log_GDP_per_capita log_high_tech log_journals
eststo: regress log_GDP_per_capita log_high_tech log_journals tech_jour
esttab, mtitles("Model A" "Model B" "Model C")

* SOCIAL DEVELOPMENT MODEL REGRESSIONS
eststo clear
eststo: regress log_GDP_per_capita child_labor
eststo: regress log_GDP_per_capita literacy  
eststo: regress log_GDP_per_capita life_expect refugee_pop
esttab, mtitles("Model A" "Model B" "Model C")

log close
