ssc install estout, replace

clear
capture log close

log using "~/Desktop/regressions.log", text replace
insheet using "~/Desktop/STAT202/Final/data/Y2008.csv", comma

gen gdp_per_capita_current_USD = real(gdppercapitacurrentus)
gen log_GDP_per_capita = log(gdp_per_capita_current_USD + 0.01)

gen exports = real(hightechnologyexportsofmanufactu)
gen res = real(researchanddevelopmentexpenditur)
gen jour = real(scientificandtechnicaljournalart)

gen ln_exports = log(exports + 0.01)
gen ln_res = log(res + 0.01)
gen ln_jour = log(jour + 0.01)

gen exports_jour = ln_exports * ln_jour
gen exports_res = ln_exports * ln_res
gen jour_res = ln_jour * ln_res

* SCIENCE MODEL REGRESSIONS
eststo clear
eststo: regress log_GDP_per_capita ln_res ln_exports ln_jour
eststo: regress log_GDP_per_capita ln_exports ln_jour
eststo: regress log_GDP_per_capita ln_exports ln_jour jour_res
eststo: regress log_GDP_per_capita ln_exports ln_jour jour_res exports_jour
eststo: regress log_GDP_per_capita ln_exports ln_jour jour_res exports_jour exports_res
esttab, mtitles("Model A" "Model B" "Model C" "Model D" "Model E")

log close
