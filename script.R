# Load the data frame
df <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

# Our response
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)

#'
#' AGRICULTURAL MODEL
#'
rural.water <- df$`Improved water source, rural (% of rural population with access)`
rural.poor <- df$`Poverty headcount ratio at urban poverty line (% of urban population)`
food.production <- df$`Food production index (2004-2006 = 100)`
arable.land <- df$`Arable land (% of land area)`
rural.population.percent <- df$`Rural population (% of total population)`

# Regressions
summary(lm(log.GDP.per.capita ~ rural.water + rural.poor + food.production + arable.land + rural.population.percent))
summary(lm(log.GDP.per.capita ~ rural.water + rural.poor + rural.population.percent))
summary(lm(log.GDP.per.capita ~ rural.poor + rural.population.percent))
# the best model
summary(lm(log.GDP.per.capita ~ rural.water + rural.population.percent))

# Plots
plot(x = rural.water, y = log.GDP.per.capita, xlab = "Rural Access to Water (%)")
abline(lm(log.GDP.per.capita ~ rural.water))

plot(x = rural.poor, y = log.GDP.per.capita, xlab = "Rural Poverty (%)")
abline(lm(log.GDP.per.capita ~ rural.poor))

plot(x = food.production, y = log.GDP.per.capita, xlab = "Food Production")
abline(v = 100)

plot(x = arable.land, y = log.GDP.per.capita, xlab = "Arable Land (% of Land Area)")
abline(lm(log.GDP.per.capita ~ arable.land))

plot(x = rural.population.percent, y = log.GDP.per.capita, xlab = "Rural Population (% of Total)")
abline(lm(log.GDP.per.capita ~ rural.population.percent))


#'
#' URBAN MODEL
#'
motor.vehicles.per.1000 <- df$`Motor vehicles (per 1,000 people)`
urban.water <- df$`Improved water source, urban (% of urban population with access)`
urban.sanitation <- df$`Improved sanitation facilities, urban (% of urban population with access)`
urban.primacy <- df$`Population in the largest city (% of urban population)`
urban.population <- df$`Urban population (% of total)`
urban.poverty.percentage <- df$`Poverty headcount ratio at urban poverty line (% of urban population)`
price.of.gas <- df$`Pump price for gasoline (US$ per liter)`
price.of.diesel <- df$`Pump price for diesel fuel (US$ per liter)`
road.sector.energy <- df$`Road sector energy consumption (% of total energy consumption)`

#' mean shortfall from poverty line as a percentage
#' of national urban poverty line. If the national
#' urban poverty line is $20,000, and there are two people
#' in the city -- a rich man making more than 20k, with 0 
#' shortfall, and a poor man making 15k with 5k shortfall
#' -- then the average shortfall, 2.5k, yields a 
#' 12.5% poverty gap. Measures not just incidence, but also
#' depth of poverty.
urban.poverty.gap <- df$`Poverty gap at urban poverty line (%)`

# Regressions
# slightly significant
summary(lm(log.GDP.per.capita ~ price.of.diesel))
summary(lm(log.GDP.per.capita ~ urban.poverty.percentage))

# all super signfiicant
summary(lm(log.GDP.per.capita ~ urban.population))
summary(lm(log.GDP.per.capita ~ urban.sanitation))
summary(lm(log.GDP.per.capita ~ urban.water + urban.sanitation))
# population and water are slightly collinear
summary(lm(log.GDP.per.capita ~ urban.water + urban.sanitation + urban.population))
summary(lm(log.GDP.per.capita ~ urban.sanitation + urban.population))
summary(lm(log.GDP.per.capita ~ urban.water + urban.population))

# Plots
plot(x = price.of.diesel, y = log.GDP.per.capita, xlab = "Price of Diesel", main = "Are you wealthier if you pay more for fuel?")
abline(lm(log.GDP.per.capita ~ price.of.diesel))

plot(x = urban.poverty.percentage, y = log.GDP.per.capita, xlab = "Urban Poverty (%)", main = "More city poverty, less wealth?")
abline(lm(log.GDP.per.capita ~ urban.poverty.percentage))

plot(x = urban.population, y = log.GDP.per.capita, xlab = "Urban Population (% of total)", main = "More cities, more wealth?")
abline(lm(log.GDP.per.capita ~ urban.population))

plot(x = urban.sanitation, y = log.GDP.per.capita, xlab = "Urban Access to Sanitation (%)", main = "More toilets, more wealth?")
abline(lm(log.GDP.per.capita ~ urban.sanitation))


#'
#' CLIMATE CHANGE MODEL
#'
electricity.consumption <- df$`Electric power consumption (kWh per capita)`
emissions.co2.kt <- df$`CO2 emissions (kt)`
emissions.methane <- df$`Methane emissions (kt of CO2 equivalent)`
emissions.nitrous <- df$`Nitrous oxide emissions (thousand metric tons of CO2 equivalent)`
paved.roads <- df$`Roads, paved (% of total roads)`

# all super significant
summary(lm(log.GDP.per.capita ~ electricity.consumption))
summary(lm(log.GDP.per.capita ~ paved.roads))
summary(lm(log.GDP.per.capita ~ paved.roads + electricity.consumption))

# all insignificant
summary(lm(log.GDP.per.capita ~ emissions.co2.kt))
summary(lm(log.GDP.per.capita ~ emissions.methane))
summary(lm(log.GDP.per.capita ~ emissions.nitrous))


#'
#' LABOR MODEL
#'
unemployment <- df$`Unemployment, total (% of total labor force)`
lpr <- df$`Labor force participation rate, total (% of total population ages 15-64)`
lpr.female <- df$`Labor force participation rate, female (% of female population ages 15-64)`
lpr.male <- df$`Labor force participation rate, male (% of male population ages 15-64)`
lpr.youth <- df$`Labor force participation rate, total (% of total population ages 15-24)`
lpr.youth.female <- df$`Labor force participation rate, female (% of female population ages 15-24)`
lpr.youth.male <- df$`Labor force participation rate, male (% of male population ages 15-24)`
services.male <- df$`Employees, services, male (% of male employment)`
services.female <- df$`Employees, services, female (% of female employment)`
industry.male <- df$`Employees, industry, male (% of male employment)`
industry.female <- df$`Employees, industry, female (% of female employment)`
agriculture.male <- df$`Employees, agriculture, male (% of male employment)`
agriculture.female <- df$`Employees, agriculture, female (% of female employment)`

summary(lm(log.GDP.per.capita ~ unemployment))
summary(lm(log.GDP.per.capita ~ unemployment + lpr))
summary(lm(log.GDP.per.capita ~ unemployment + lpr.female))
summary(lm(log.GDP.per.capita ~ unemployment + lpr.youth.female + lpr.youth.male))
summary(lm(log.GDP.per.capita ~ services.male))
summary(lm(log.GDP.per.capita ~ services.female))
summary(lm(log.GDP.per.capita ~ services.male + services.female))
summary(lm(log.GDP.per.capita ~ industry.male + industry.female))
summary(lm(log.GDP.per.capita ~ agriculture.female))
summary(lm(log.GDP.per.capita ~ services.male + services.female + industry.male + industry.female + agriculture.male + agriculture.female))
summary(lm(log.GDP.per.capita ~ services.male + industry.male))

#'
#' ECONOMIC MODEL
#'
exports <- df$`Exports of goods and services (% of GDP)`
imports <- df$`Imports of goods and services (% of GDP)`
trade <- df$`Trade in services (% of GDP)`
debt <- df$`Central government debt, total (% of GDP)`
revenue <- df$`Revenue, excluding grants (% of GDP)`
education <- df$`Public spending on education, total (% of government expenditure)`
military <- df$`Military expenditure (% of central government expenditure)`
taxes <- df$`Tax revenue (% of GDP)`

summary(lm(log.GDP.per.capita ~ imports + exports + debt))
summary(lm(log.GDP.per.capita ~ imports + exports + education))
summary(lm(log.GDP.per.capita ~ imports + exports + military))
summary(lm(log.GDP.per.capita ~ imports + exports + taxes))
