ROOT <- "~/Desktop/STAT202/Final/"



df <- read.csv(paste0(ROOT, "data/Y2008.csv"), header = TRUE, check.names = FALSE)

IMG_PATH <- paste0(ROOT, "writeups/images/")
makePDF <- function(s) paste0(IMG_PATH, s, ".pdf")
makePNG <- function(s) paste0(IMG_PATH, s, ".png")
PLOT_WIDTH = 500


##################
### INDICATORS ###
##################
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)

urban.sanitation <- df$`Improved sanitation facilities, urban (% of urban population with access)`
urban.population <- df$`Urban population (% of total)`
urban.water <- df$`Improved water source, urban (% of urban population with access)`

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

high.tech <- df$`High-technology exports (% of manufactured exports)`
research <- df$`Research and development expenditure (% of GDP)`
journals <- df$`Scientific and technical journal articles`
log.high.tech <- log(high.tech + 0.01)
log.research <- log(research + 0.01)
log.journals <- log(journals + 0.01)

child.labor <- df$`Children in employment, total (% of children ages 7-14)`
life.expect <- df$`Life expectancy at birth, male (years)`
refugees <- df$`Refugee population by country or territory of origin`
literacy <- df$`Literacy rate, adult total (% of people ages 15 and above)`

log.child.labor <- log(child.labor + 0.01)
log.life.expect <- log(life.expect + 0.01)
log.refugees <- log(refugees + 0.01)
log.literacy <- log(literacy + 0.01)








###################
### URBAN MODEL ###
###################
nrow = 1
ncol = 2
png(filename = makePNG("urban_model_scatter_pop_water"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(x = urban.population, xlab = "Urban Population (% of Total)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ urban.population)), lwd = 3.5, col = "firebrick")

plot(x = urban.water, xlab = "% Urban Access to Clean Water", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ urban.water)), lwd = 3.5, col = "firebrick")
g <- dev.off()




nrow = 1
ncol = 1
png(filename = makePNG("urban_model_scatter_san"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(x = urban.sanitation, xlab = "% of Urban Population with Sanitation Access", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ urban.sanitation)), lwd = 3.5, col = "firebrick")
g <- dev.off()




nrow = 1
ncol = 2
m <- lm(log.GDP.per.capita ~ urban.sanitation + urban.population)
summary(m)
png(filename = makePNG("urban_model_conditions"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(m, which=c(1,2))
g <- dev.off()








#####################
### CLIMATE MODEL ###
#####################
nrow = 2
ncol = 3
png(filename = makePNG("climate_model_scatter_pollution"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(x = co2, xlab = "CO2 Emissions (kt)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ co2)), lwd = 3.5, col = "firebrick")

plot(x = methane, xlab = "Methane Emissions (kt)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ methane)), lwd = 3.5, col = "firebrick")

plot(x = nitrous, xlab = "Nitrous Emissions (kt)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ nitrous)), lwd = 3.5, col = "firebrick")

plot(x = log.co2, xlab = "log CO2 Emissions (kt)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.co2)), lwd = 3.5, col = "firebrick")

plot(x = log.methane, xlab = "log Methane Emissions (kt)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.methane)), lwd = 3.5, col = "firebrick")

plot(x = log.nitrous, xlab = "log Nitrous Emissions (kt)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.nitrous)), lwd = 3.5, col = "firebrick")
g <- dev.off()




nrow = 2
ncol = 2
png(filename = makePNG("climate_model_scatter_elec_roads"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(x = electricity.consumption, xlab = "Electricity Consumption", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ electricity.consumption)), lwd = 3.5, col = "firebrick")

plot(x = paved.roads, xlab = "Paved Roads (% of total)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ paved.roads)), lwd = 3.5, col = "firebrick")

plot(x = log.electricity.consumption, xlab = "log Electricity Consumption", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.electricity.consumption)), lwd = 3.5, col = "firebrick")

plot(x = log.paved.roads, xlab = "log Paved Roads (% of total)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.paved.roads)), lwd = 3.5, col = "firebrick")
g <- dev.off()




m1 <- lm(log.GDP.per.capita ~ log.electricity.consumption*log.co2)
summary(m1)
m2 <- lm(log.GDP.per.capita ~ log.electricity.consumption:log.co2 + log.co2)
summary(m2)

nrow = 1
ncol = 2
png(filename = makePNG("climate_model_conditions"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(m2, which=c(1,2))
g <- dev.off()








#####################
### SCIENCE MODEL ###
#####################
nrow = 2
ncol = 3
png(filename = makePNG("science_model_scatter"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(x = high.tech, xlab = "High-tech exports (% of manufactured exports)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ high.tech)), lwd = 3.5, col = "firebrick")

plot(x = research, xlab = "R&D Expenditure (% of GDP)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ research)), lwd = 3.5, col = "firebrick")

plot(x = journals, xlab = "Science Journals", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ journals)), lwd = 3.5, col = "firebrick")

plot(x = log.high.tech, xlab = "log High-tech exports (% of manufactured exports)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.high.tech)), lwd = 3.5, col = "firebrick")

plot(x = log.research, xlab = "log R&D Expenditure (% of GDP)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.research)), lwd = 3.5, col = "firebrick")

plot(x = log.journals, xlab = "log Science Journals", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(summary(lm(log.GDP.per.capita ~ log.journals)), lwd = 3.5, col = "firebrick")
g <- dev.off()




m <- lm(log.GDP.per.capita ~ log.high.tech * log.journals)
summary(m)

nrow = 1
ncol = 2
png(filename = makePNG("science_model_conditions"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(m, which=c(1,2))
g <- dev.off()








####################
### SOCIAL MODEL ###
####################
m <- lm(log.GDP.per.capita ~ child.labor + life.expect + refugees + literacy)
summary(m)
m <- lm(log.GDP.per.capita ~ life.expect + refugees)
summary(m)

nrow = 1
ncol = 2
png(filename = makePNG("social_model_conditions"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(m, which=c(1,2))
g <- dev.off()




nrow = 2
ncol = 4
png(filename = makePNG("social_model_scatter"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(x = child.labor, xlab = "Child Employment (% of children ages 7-14)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ child.labor), lwd = 3.5, col = "firebrick")

plot(x = life.expect, xlab = "Life expectancy at birth, male (years)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ life.expect), lwd = 3.5, col = "firebrick")

plot(x = refugees, xlab = "Refugee Population", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ refugees), lwd = 3.5, col = "firebrick")

plot(x = literacy, xlab = "Literacy Rate (adult total)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ literacy), lwd = 3.5, col = "firebrick")

plot(x = log.child.labor, xlab = "log Child Employment (% of chlidren ages 7-14)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.child.labor), lwd = 3.5, col = "firebrick")

plot(x = log.life.expect, xlab = "log Life expectancy at birth, male (years)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.life.expect), lwd = 3.5, col = "firebrick")

plot(x = log.refugees, xlab = "log Refugee Population", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.refugees), lwd = 3.5, col = "firebrick")

plot(x = log.literacy, xlab = "log Literacy Rate (adult total)", y = log.GDP.per.capita, ylab = "log(GDP per capita)", pch = 17, cex = 1.2, col = "darkblue")
abline(lm(log.GDP.per.capita ~ log.literacy), lwd = 3.5, col = "firebrick")
g <- dev.off()




m <- lm(log.GDP.per.capita ~ log.life.expect + log.refugees)
summary(m)

nrow = 1
ncol = 2
png(filename = makePNG("social_model_log_conditions"), width = PLOT_WIDTH * ncol, height = PLOT_WIDTH * nrow)
par(mfrow=c(nrow,ncol))
plot(m, which=c(1,2))
g <- dev.off()

