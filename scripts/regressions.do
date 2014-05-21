clear
capture log close

log using "~/Desktop/STAT202/Final/scripts/regressions.log", text replace
insheet using "~/Desktop/STAT202/Final/data/Y2008.csv", comma


rename old urb_sanitation
rename old urb_population
rename old urb_water

rename old co2
rename old methane
rename old nitrous
rename old paved_roads
rename old electric_cons
gen log_co2 = log(co2)
gen log_methane = log(methane)
gen log_nitrous = log(nitrous)
gen log_paved_roads = log(paved_roads)
gen log_electric_cons = log(electric_cons)

rename old high_tech
rename old research
rename old journals
gen log_high_tech = log(high_tech)
gen log_research = log(research)
gen log_journals = log(journals)

rename old child_labor
rename old life_expect
rename old refugee_pop
rename old literacy
gen log_child_labor = log(child_labor)
gen log_life_expect = log(life_expect)
gen log_refugee_pop = log(refugee_pop)
gen log_literacy = log(literacy)


*eststo: regress log_GDP_per_capita log_co2

*esttab


log close
