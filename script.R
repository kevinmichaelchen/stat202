# Load the data frame
df <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

# Our response
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)


# AGRICULTURAL INDICATORS
rural.water <- df$`Improved water source, rural (% of rural population with access)`
rural.poor <- df$`Poverty headcount ratio at urban poverty line (% of urban population)`
food.production <- df$`Food production index (2004-2006 = 100)`
arable.land <- df$`Arable land (% of land area)`
rural.population.percent <- df$`Rural population (% of total population)`

predictors <- c(rural.water,rural.poor,food.production,arable.land,rural.population.percent)
names <- c("Rural Access to Water","Rural Poverty (%)","Food Production","Arable Land","Rural Population (% of Total)")


# Regressions
summary(lm(log.GDP.per.capita ~ rural.water + rural.poor + food.production + arable.land + rural.population.percent))
summary(lm(log.GDP.per.capita ~ rural.water + rural.poor + rural.population.percent))
summary(lm(log.GDP.per.capita ~ rural.poor + rural.population.percent))
summary(lm(log.GDP.per.capita ~ rural.water + rural.population.percent))

for (i in 1:length(predictors)) {
  if (names(predictors)[i] == "Food.production") {
    plot(predictors[[i]], log.GDP.per.capita, xlab = names[i])
    abline(v = 100)
  }
  else {
    plot(predictors[[i]], log.GDP.per.capita, xlab = names[i])
    abline(lm(log.GDP.per.capita ~ predictors[[i]]))
  }
}



# URBAN INDICATORS
motor.vehicles.per.1000 <- df$`Motor vehicles (per 1,000 people)`
urban.water <- df$`Improved water source, urban (% of urban population with access)`
urban.sanitation <- df$`Improved sanitation facilities, urban (% of urban population with access)`

urban.primacy <- df$`Population in the largest city (% of urban population)`
urban.population <- df$`Urban population (% of total)`

#' mean shortfall from poverty line as a percentage
#' of national urban poverty line. If the national
#' urban poverty line is $20,000, and there are two people
#' in the city -- a rich man making more than 20k, with 0 
#' shortfall, and a poor man making 15k with 5k shortfall
#' -- then the average shortfall, 2.5k, yields a 
#' 12.5% poverty gap. Measures not just incidence, but also
#' depth of poverty.
urban.poverty.gap <- df$`Poverty gap at urban poverty line (%)`

# Incidence of poverty as percentage of urban population
urban.poverty.percentage <- df$`Poverty headcount ratio at urban poverty line (% of urban population)`

price.of.gas <- df$`Pump price for gasoline (US$ per liter)`
price.of.diesel <- df$`Pump price for diesel fuel (US$ per liter)`
road.sector.energy <- df$`Road sector energy consumption (% of total energy consumption)`


 