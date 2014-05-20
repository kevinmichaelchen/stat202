# Load the data frame
df <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

# Our response
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)
boxplot(log.GDP.per.capita)

#'
#' AGRICULTURAL MODEL
#'
rural.water <- df$`Improved water source, rural (% of rural population with access)`
rural.poor <- df$`Poverty headcount ratio at urban poverty line (% of urban population)`
food.production <- df$`Food production index (2004-2006 = 100)`
arable.land <- df$`Arable land (% of land area)`
rural.population.percent <- df$`Rural population (% of total population)`

haha <- function(xxx, xxxName) {
  plot(x = xxx, xlab = xxxName,
       y = log.GDP.per.capita, ylab = "log(GDP per capita)",
       pch = 17, #dot point
       cex = 1.2, #dot size
       col = "darkblue")
  abline(summary(lm(log.GDP.per.capita ~ 
                      xxx
  )),
  lwd = 3.5,
  col = "firebrick")
}


haha(rural.water, "Rural water")

# Regressions
summary(lm(log.GDP.per.capita ~ rural.water + rural.poor + food.production + arable.land + rural.population.percent))
summary(lm(log.GDP.per.capita ~ rural.water + rural.poor + rural.population.percent))
summary(lm(log.GDP.per.capita ~ rural.poor + rural.population.percent))
# the best model
summary(lm(log.GDP.per.capita ~ rural.water + rural.population.percent))



#'
#' URBAN MODEL
#'
motor.vehicles.per.1000 <- df$`Motor vehicles (per 1,000 people)`
plot(motor.vehicles.per.1000, log.GDP.per.capita) # BAD!!
plot(sqrt(motor.vehicles.per.1000), log.GDP.per.capita)

urban.sanitation <- df$`Improved sanitation facilities, urban (% of urban population with access)`
plot(urban.sanitation, log.GDP.per.capita)

urban.population <- df$`Urban population (% of total)`
plot(urban.population, log.GDP.per.capita)

urban.water <- df$`Improved water source, urban (% of urban population with access)`
# Can't be fixed
plot(urban.water, log.GDP.per.capita)

pred <- c(motor.vehicles.per.1000,
          urban.sanitation,
          urban.primacy,
          urban.population,
          urban.poverty.percentage,
          price.of.gas,
          price.of.diesel,
          road.sector.energy,
          urban.water
)

for (i in 1:length(pred)) {
  p = pred[i]
  s = deparse(substitute(p))
  
  plot(x = p,
       xlab = s,
       y = log.GDP.per.capita,
       ylab = "log(GDP per capita)",
       pch = 17, #dot point
       cex = 1.2, #dot size
       col = "darkblue")
  abline(summary(lm(log.GDP.per.capita ~ p)),
         lwd = 3.5,
         col = "firebrick")
}

log.urban.water <- log(urban.water + 0.01)
par(mfrow=c(1,2))
plot(lm(log.GDP.per.capita ~ urban.water), which=c(1), pch = 16, cex = 1, lwd = 7)
title("urban.water")
plot(lm(log.GDP.per.capita ~ log.urban.water), which=c(1), pch = 16, cex = 1, lwd = 7)
title("log.urban.water")


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

summary(lm(log.GDP.per.capita ~ urban.water + urban.population))


summary(lm(log.GDP.per.capita ~ urban.sanitation + urban.population))

#'
#' CLIMATE CHANGE MODEL
#'
ROOT <- "~/Dropbox/STAT 202 Final/STAT202/Presentation Plots/"
CONDITION.PLOT.DIR <- paste0(ROOT,"condition_plots/")


write.conditions <- function(df.names, titles, path){
  
  # number of predictors
  n <- length(df.names)
  
  # write to jpeg file
  jpeg(paste0(path, substitute(titles.cli), "_logged1.jpeg"), width = 1000*n, height = 200*n)
  
  # 2 rows, one for residuals vs fitted, one for qqline
  par(mfrow = c(1,n), omd = c(4,4.5,2,1))
  
  for(i in 1:n) {
    # j is 1 because the resid vs fitted is the 1st plot given by plot(lm)
    j <- 1  
    my.predictor = eval(parse(text = paste0("log(df$`",df.names[i],"`)")))
    my.lm <- lm(log.GDP.per.capita ~ my.predictor, data = as.data.frame(df), na.action = na.exclude)
    my.sub.caption = eval(parse(text = paste0("'",titles[i],"'")))
    my.main = eval(parse(text = paste0("'",titles[i],"'")))
    # pch=dot type, cex=dot size, lwd=line width
    plot(my.lm, which = c(j), sub.caption = my.sub.caption, main = my.main, pch = 16, cex = 2, lwd = 7)
    title(main = my.main, outer = FALSE)
  }
  
  dev.off()
  graphics.off()
}





predictors.cli <- c("Electric power consumption (kWh per capita)",
                    "CO2 emissions (kt)",
                    "Methane emissions (kt of CO2 equivalent)",
                    "Nitrous oxide emissions (thousand metric tons of CO2 equivalent)",
                    "Roads, paved (% of total roads)")
titles.cli <- c("electricity.consumption",
                "emissions.co2.kt",
                "emissions.methane",
                "emissions.nitrous",
                "paved.roads")


### List of lists of predictors
predictors <- list(predictors.cli)

### List of lists of titles
titles <- list(titles.cli)

for (i in 1:length(predictors)) {
  p = predictors[[i]]
  t = titles[[i]]
  write.conditions(p, t, CONDITION.PLOT.DIR)  
}




emissions.co2.kt <- df$`CO2 emissions (kt)`
emissions.methane <- df$`Methane emissions (kt of CO2 equivalent)`
emissions.nitrous <- df$`Nitrous oxide emissions (thousand metric tons of CO2 equivalent)`
electricity.consumption <- df$`Electric power consumption (kWh per capita)`
paved.roads <- df$`Roads, paved (% of total roads)`

log.emissions.co2.kt <- log(emissions.co2.kt + 0.01)
log.emissions.methane <- log(emissions.methane + 0.01)
log.emissions.nitrous <- log(emissions.nitrous + 0.01)
log.electricity.consumption <- log(electricity.consumption + 0.01)
log.paved.roads <- log(paved.roads + 0.01)



# all super significant
summary(lm(log.GDP.per.capita ~ electricity.consumption))
summary(lm(log.GDP.per.capita ~ paved.roads))
summary(lm(log.GDP.per.capita ~ paved.roads + electricity.consumption))


summary(lm(log.GDP.per.capita ~ paved.roads + log.electricity.consumption + log.emissions.methane))
summary(lm(log.GDP.per.capita ~ paved.roads + log.electricity.consumption + log.emissions.nitrous))
summary(lm(log.GDP.per.capita ~ paved.roads + log.electricity.consumption + log.emissions.co2.kt))

summary(lm(log.GDP.per.capita ~ paved.roads + log.emissions.co2.kt))
summary(lm(log.GDP.per.capita ~ paved.roads + log.emissions.nitrous))
summary(lm(log.GDP.per.capita ~ paved.roads + log.emissions.methane))

summary(lm(log.GDP.per.capita ~ paved.roads + emissions.co2.kt))
summary(lm(log.GDP.per.capita ~ paved.roads + emissions.nitrous))
summary(lm(log.GDP.per.capita ~ paved.roads + emissions.methane))

summary(lm(log.GDP.per.capita ~ log.electricity.consumption + log.emissions.co2.kt))
summary(lm(log.GDP.per.capita ~ log.electricity.consumption + log.emissions.nitrous))
summary(lm(log.GDP.per.capita ~ log.electricity.consumption + log.emissions.methane))

summary(lm(log.GDP.per.capita ~ log.emissions.co2.kt))
summary(lm(log.GDP.per.capita ~ log.emissions.nitrous))
summary(lm(log.GDP.per.capita ~ log.emissions.methane))


summary(lm(log.GDP.per.capita ~ electricity.consumption + emissions.co2.kt))
summary(lm(log.GDP.per.capita ~ electricity.consumption + emissions.nitrous))
summary(lm(log.GDP.per.capita ~ electricity.consumption + emissions.methane))

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

#'
#' SCIENCE MODEL
#'
high.tech <- df$`High-technology exports (% of manufactured exports)`
r.n.d <- df$`Research and development expenditure (% of GDP)`
journals <- df$`Scientific and technical journal articles`
log.high.tech <- log(high.tech + 0.01)
log.research <- log(r.n.d + 0.01)
log.journals <- log(journals + 0.01)

summary(lm(log.GDP.per.capita ~ log.high.tech + log.research + log.journals))

summary(lm(log.GDP.per.capita ~ log.high.tech * log.research + log.journals))

summary(lm(log.GDP.per.capita ~ log.high.tech * log.research + log.high.tech * log.journals + log.research * log.journals))


summary(lm(log.GDP.per.capita ~ log.research + log.journals))
summary(lm(log.GDP.per.capita ~ log.journals))


par(mfrow=c(1,3))
plot(lm(log.GDP.per.capita ~ log.high.tech), which=c(1), pch = 16, cex = 2, lwd = 7)
title("log.high.tech")
plot(lm(log.GDP.per.capita ~ log.research), which=c(1), pch = 16, cex = 2, lwd = 7)
title("log.research")
plot(lm(log.GDP.per.capita ~ log.journals), which=c(1), pch = 16, cex = 2, lwd = 7)
title("log.journals")

summary(lm(log.GDP.per.capita ~ r.n.d + high.tech + journals))
summary(lm(log.GDP.per.capita ~ log.research + log.high.tech + log.journals))

summary(lm(log.GDP.per.capita ~ r.n.d + high.tech))
summary(lm(log.GDP.per.capita ~ log.research + log.high.tech))

summary(lm(log.GDP.per.capita ~ r.n.d + journals))
summary(lm(log.GDP.per.capita ~ high.tech + journals))

# all significant
summary(lm(log.GDP.per.capita ~ journals))
summary(lm(log.GDP.per.capita ~ high.tech))
summary(lm(log.GDP.per.capita ~ r.n.d))

#'
#' SOCIAL DEVELOPMENT MODEL
#'
hiv.prevalence <- df$`Prevalence of HIV, female (% ages 15-24)`
child.labor <- df$`Children in employment, total (% of children ages 7-14)`
life.expectancy <- df$`Life expectancy at birth, male (years)`
refugee.pop <- df$`Refugee population by country or territory of origin`
literacy.rate <- df$`Literacy rate, adult total (% of people ages 15 and above)`

log.child.labor <- log(worker.children + 0.01)
log.life.expect <- log(life.expectancy + 0.01)
log.refugees <- log(refugee.pop + 0.01)
log.literacy <- log(literacy.rate + 0.01)

plot.new()
par(mfrow=c(1,4))
plot(lm(log.GDP.per.capita ~ log.child.labor), which=c(1), pch = 16, cex = 2, lwd = 7)
title("log.child.labor")
plot(lm(log.GDP.per.capita ~ log.life.expect), which=c(1), pch = 16, cex = 2, lwd = 7)
title("log.life.expect")
plot(lm(log.GDP.per.capita ~ log.refugees), which=c(1), pch = 16, cex = 2, lwd = 7)
title("log.refugees")
plot(lm(log.GDP.per.capita ~ log.literacy), which=c(1), pch = 16, cex = 2, lwd = 7)
title("log.literacy")

par(mfrow=c(1,2))
plot(lm(log.GDP.per.capita ~ refugee.pop), which=c(1), pch = 16, cex = 1.3, lwd = 7)
title("refugees")
plot(lm(log.GDP.per.capita ~ log.refugees), which=c(1), pch = 16, cex = 1.3, lwd = 7)
title("log.refugees")

summary(lm(log.GDP.per.capita ~ literacy.rate))

summary(lm(log.GDP.per.capita ~ log.life.expect + log.refugees))



summary(lm(log.GDP.per.capita ~ log.high.tech * log.research + log.high.tech * log.journals + log.research * log.journals + rural.water + rural.population.percent + urban.sanitation + paved.roads + electricity.consumption + log.emissions.co2.kt + log.life.expect + log.refugees))
















#####################################
#####################################
#####################################

IMG_PATH = "~/Desktop/STAT202/Final/writeups/images/"
makePDF <- function(s) paste0(IMG_PATH, s, ".pdf")
makePNG <- function(s) paste0(IMG_PATH, s, ".png")
SCALE = 500

nrow = 1
ncol = 2
png(filename = makePNG("urban_model_scatter_pop_water"), width = SCALE * ncol, height = SCALE * nrow)
par(mfrow=c(nrow,ncol))
plot(x = urban.population,
     xlab = "Urban Population (% of Total)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ urban.population)),
       lwd = 3.5,
       col = "firebrick")

plot(x = urban.water,
     xlab = "% Urban Access to Clean Water",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ urban.water)),
       lwd = 3.5,
       col = "firebrick")
g <- dev.off()





nrow = 1
ncol = 1
png(filename = makePNG("urban_model_scatter_san"), width = SCALE * ncol, height = SCALE * nrow)
par(mfrow=c(nrow,ncol))
plot(x = urban.sanitation,
     xlab = "% of Urban Population with Sanitation Access",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ urban.sanitation)),
       lwd = 3.5,
       col = "firebrick")
g <- dev.off()




nrow = 1
ncol = 2
m <- lm(log.GDP.per.capita ~ urban.sanitation + urban.population)
summary(m)
png(filename = makePNG("urban_model_conditions"), width = SCALE * ncol, height = SCALE * nrow)
par(mfrow=c(nrow,ncol))
plot(m, which=c(1,2))
g <- dev.off()




co2 <- df$`CO2 emissions (kt)`
methane <- df$`Methane emissions (kt of CO2 equivalent)`
nitrous <- df$`Nitrous oxide emissions (thousand metric tons of CO2 equivalent)`
electricity.consumption <- df$`Electric power consumption (kWh per capita)`
paved.roads <- df$`Roads, paved (% of total roads)`

log.co2 <- log(co2 + 0.01)
log.methane <- log(methane + 0.01)
log.nitrous <- log(nitrous + 0.01)
log.electricity.consumption <- log(electricity.consumption + 0.01)
log.paved.roads <- log(paved.roads + 0.01)




nrow = 2
ncol = 3
png(filename = makePNG("climate_model_scatter_pollution"), width = SCALE * ncol, height = SCALE * nrow)
par(mfrow=c(nrow,ncol))
plot(x = co2,
     xlab = "CO2 Emissions (kt)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ co2)),
       lwd = 3.5,
       col = "firebrick")

plot(x = methane,
     xlab = "Methane Emissions (kt)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ methane)),
       lwd = 3.5,
       col = "firebrick")


plot(x = nitrous,
     xlab = "Nitrous Emissions (kt)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ nitrous)),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.co2,
     xlab = "log CO2 Emissions (kt)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.co2)),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.methane,
     xlab = "log Methane Emissions (kt)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.methane)),
       lwd = 3.5,
       col = "firebrick")


plot(x = log.nitrous,
     xlab = "log Nitrous Emissions (kt)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.nitrous)),
       lwd = 3.5,
       col = "firebrick")
g <- dev.off()






nrow = 2
ncol = 2
png(filename = makePNG("climate_model_scatter_elec_roads"), width = SCALE * ncol, height = SCALE * nrow)
par(mfrow=c(nrow,ncol))
plot(x = electricity.consumption,
     xlab = "Electricity Consumption",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ electricity.consumption)),
       lwd = 3.5,
       col = "firebrick")

plot(x = paved.roads,
     xlab = "Paved Roads (% of total)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ paved.roads)),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.electricity.consumption,
     xlab = "log Electricity Consumption",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.electricity.consumption)),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.paved.roads,
     xlab = "log Paved Roads (% of total)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.paved.roads)),
       lwd = 3.5,
       col = "firebrick")
g <- dev.off()



m1 <- lm(log.GDP.per.capita ~ log.electricity.consumption*log.co2)
summary(m1)
m2 <- lm(log.GDP.per.capita ~ log.electricity.consumption:log.co2 + log.co2)
summary(m2)

nrow = 1
ncol = 2
png(filename = makePNG("climate_model_conditions"), width = SCALE * ncol, height = SCALE * nrow)
par(mfrow=c(nrow,ncol))
plot(m2, which=c(1,2))
g <- dev.off()








high.tech <- df$`High-technology exports (% of manufactured exports)`
r.n.d <- df$`Research and development expenditure (% of GDP)`
journals <- df$`Scientific and technical journal articles`
log.high.tech <- log(high.tech + 0.01)
log.research <- log(r.n.d + 0.01)
log.journals <- log(journals + 0.01)



nrow = 2
ncol = 3
png(filename = makePNG("science_model_scatter"), width = SCALE * ncol, height = SCALE * nrow)
par(mfrow=c(nrow,ncol))
plot(x = high.tech,
     xlab = "High-tech exports (% of manufactured exports)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ high.tech)),
       lwd = 3.5,
       col = "firebrick")

plot(x = r.n.d,
     xlab = "R&D Expenditure (% of GDP)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ r.n.d)),
       lwd = 3.5,
       col = "firebrick")


plot(x = journals,
     xlab = "Science Journals",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ journals)),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.high.tech,
     xlab = "log High-tech exports (% of manufactured exports)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.high.tech)),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.research,
     xlab = "log R&D Expenditure (% of GDP)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.research)),
       lwd = 3.5,
       col = "firebrick")


plot(x = log.journals,
     xlab = "log Science Journals",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.journals)),
       lwd = 3.5,
       col = "firebrick")
g <- dev.off()






m <- lm(log.GDP.per.capita ~ log.high.tech * log.journals)
summary(m)














child.labor <- df$`Children in employment, total (% of children ages 7-14)`
life.expect <- df$`Life expectancy at birth, male (years)`
refugees <- df$`Refugee population by country or territory of origin`
literacy <- df$`Literacy rate, adult total (% of people ages 15 and above)`

log.child.labor <- log(child.labor + 0.01)
log.life.expect <- log(life.expect + 0.01)
log.refugees <- log(refugees + 0.01)
log.literacy <- log(literacy + 0.01)





m <- lm(log.GDP.per.capita ~ child.labor + life.expect + refugees + literacy)
summary(m)
m <- lm(log.GDP.per.capita ~ life.expect + refugees)
summary(m)
par(mfrow=c(1,2))
plot(m, which=c(1,2))




par(mfrow=c(2,4))
plot(x = child.labor,
     xlab = "Child Employment (% of children ages 7-14)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ child.labor),
       lwd = 3.5,
       col = "firebrick")

plot(x = life.expect,
     xlab = "Life expectancy at birth, male (years)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ life.expect),
       lwd = 3.5,
       col = "firebrick")

plot(x = refugees,
     xlab = "Refugee Population",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ refugees),
       lwd = 3.5,
       col = "firebrick")

plot(x = literacy,
     xlab = "Literacy Rate (adult total)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ literacy),
       lwd = 3.5,
       col = "firebrick")




plot(x = log.child.labor,
     xlab = "log Child Employment (% of chlidren ages 7-14)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.child.labor),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.life.expect,
     xlab = "log Life expectancy at birth, male (years)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.life.expect),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.refugees,
     xlab = "log Refugee Population",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.refugees),
       lwd = 3.5,
       col = "firebrick")

plot(x = log.literacy,
     xlab = "log Literacy Rate (adult total)",
     y = log.GDP.per.capita,
     ylab = "log(GDP per capita)",
     pch = 17, #dot point
     cex = 1.2, #dot size
     col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.literacy),
       lwd = 3.5,
       col = "firebrick")



m <- lm(log.GDP.per.capita ~ log.life.expect + log.refugees)
summary(m)
par(mfrow=c(1,2))
plot(m, which=c(1,2))





