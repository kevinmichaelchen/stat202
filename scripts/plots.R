# Load the data frame
# histogram and #mosaic plot (be careful)
df <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

hist.funct <- function(Names.Predictors,Title.Predictors,Path){
  for(i in 1:length(Names.Predictors)){
    if(Names.Predictors == Names.Response){
      jpeg(paste0(Path,Title.Predictors[i],"_histplot.jpeg"))
      par(mfrow(2,1))
      hist(eval(parse(text = paste0("log(df$`",Names.Predictors[i],"`)"))),main = paste("Histogram of" , Title.Predictors[i]),xlab = Title.Predictors[i])
      dev.off()
      graphics.off()
    }
    
    
    jpeg(paste0(Path,Title.Predictors[i],"_histplot.jpeg"))
    hist(eval(parse(text = paste0("df$`",Names.Predictors[i],"`"))),main = paste("Histogram of" , Title.Predictors[i]),xlab = Title.Predictors[i])
    dev.off()
    graphics.off()
  }
}

# Our response
par(mfrow = c(2,1))
hist(GDP.per.capita)
boxplot(GDP.per.capita)

par(mfrow = c(2,1))
hist(log.GDP.per.capita)
boxplot(log.GDP.per.capita)


GDP.per.capita <- df$`GDP per capita (current US$)`
log.GDP.per.capita <- log(GDP.per.capita)
Names.Response <- c("GDP per capita (current US$)")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Response/"

hist.funct(Names.Response,Names.Response,path)

#'
#' AGRICULTURAL MODEL
#' 
Names.Predictors.Agr <- c("Improved water source, rural (% of rural population with access)","Poverty headcount ratio at urban poverty line (% of urban population)","Food production index (2004-2006 = 100)","Arable land (% of land area)","Rural population (% of total population)")
Title.Predictors.Agr <- c("rural.water","rural.poor","food.production","arable.land","rural.population.percent")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Agricultural/"

hist.funct(Names.Predictors.Agr,Title.Predictors.Agr,path)


rural.water <- df$`Improved water source, rural (% of rural population with access)`
rural.population.percent <- df$`Rural population (% of total population)`

plot(x = rural.water, y = log.GDP.per.capita, xlab = "Rural Access to Water (%)")
abline(lm(log.GDP.per.capita ~ rural.water))

plot(x = rural.population.percent, y = log.GDP.per.capita, xlab = "Rural Population (% of Total)")
abline(lm(log.GDP.per.capita ~ rural.population.percent))



#'
#' URBAN MODEL
#' 
Names.Predictors.Urb <- c("Motor vehicles (per 1,000 people)","Improved water source, urban (% of urban population with access)","Improved sanitation facilities, urban (% of urban population with access)","Population in the largest city (% of urban population)","Urban population (% of total)","Poverty headcount ratio at urban poverty line (% of urban population)","Pump price for gasoline (US$ per liter)","Pump price for diesel fuel (US$ per liter)","Road sector energy consumption (% of total energy consumption)")
Title.Predictors.Urb <- c("motor.vehicles.per.1000","urban.water","urban.sanitation","urban.primacy","urban.population","urban.poverty.percentage","price.of.gas","price.of.diesel","road.sector.energy")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Urban/"

hist.funct(Names.Predictors.Urb,Title.Predictors.Urb,path)

urban.water <- df$`Improved water source, urban (% of urban population with access)`
urban.sanitation <- df$`Improved sanitation facilities, urban (% of urban population with access)`
urban.population <- df$`Urban population (% of total)`

plot(x = urban.population, y = log.GDP.per.capita, xlab = "Urban Population (% of total)", main = "More cities, more wealth?")
abline(lm(log.GDP.per.capita ~ urban.population))

plot(x = urban.sanitation, y = log.GDP.per.capita, xlab = "Urban Access to Sanitation (%)", main = "More toilets, more wealth?")
abline(lm(log.GDP.per.capita ~ urban.sanitation))

plot(x = urban.water, y = log.GDP.per.capita, xlab = "Urban Access to Water (%)", main = "More water access, more wealth?")
abline(lm(log.GDP.per.capita ~ urban.water))



#' 
#' CLIMATE CHANGE MODEL
#' 
Names.Predictors.Cli <- c("Electric power consumption (kWh per capita)","CO2 emissions (kt)","Methane emissions (kt of CO2 equivalent)","Nitrous oxide emissions (thousand metric tons of CO2 equivalent)","Roads, paved (% of total roads)")
Title.Predictors.Cli <- c("emissions.co2.kt","emissions.methane","emissions.nitrous","electricity.consumption","paved.roads")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Climate/"

hist.funct(Names.Predictors.Cli,Title.Predictors.Cli,path)

electricity.consumption <- df$`Electric power consumption (kWh per capita)`
paved.roads <- df$`Roads, paved (% of total roads)`

plot(x = electricity.consumption, y = log.GDP.per.capita, xlab = "Electricity Consumption (kWh per cap)", main = "More electricity, more wealth?")
abline(lm(log.GDP.per.capita ~ electricity.consumption))

plot(x = paved.roads, y = log.GDP.per.capita, xlab = "Percent of Roads that are Paved", main = "More paved roads, more wealth?")
abline(lm(log.GDP.per.capita ~ paved.roads))



#'
#' LABOR MODEL
#'
Names.Predictors.Labor <- c("Unemployment, total (% of total labor force)","Labor force participation rate, total (% of total population ages 15-64)","Labor force participation rate, female (% of female population ages 15-64)","Labor force participation rate, male (% of male population ages 15-64)","Labor force participation rate, total (% of total population ages 15-24)","Labor force participation rate, female (% of female population ages 15-24)","Labor force participation rate, male (% of male population ages 15-24)","Employees, services, male (% of male employment)","Employees, services, female (% of female employment)","Employees, industry, male (% of male employment)","Employees, industry, female (% of female employment)","Employees, agriculture, male (% of male employment)","Employees, agriculture, female (% of female employment)")
Title.Predictors.Labor <- c("unemployment","lpr","lpr.female","lpr.male","lpr.youth","lpr.youth.female","lpr.youth.male","services.male","services.female","industry.male","industry.female","agriculture.male","agriculture.female")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Labor/"

hist.funct(Names.Predictors.Labor,Title.Predictors.Labor,path)

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


#' ECONOMIC MODEL
Names.Predictors.Econ <- c("Exports of goods and services (% of GDP)","Imports of goods and services (% of GDP)","Trade in services (% of GDP)","Central government debt, total (% of GDP)","Revenue, excluding grants (% of GDP)","Public spending on education, total (% of government expenditure)","Military expenditure (% of central government expenditure)","Tax revenue (% of GDP)")
Title.Predictors.Econ <- c("exports","imports","trade","debt","revenue","education","military","taxes")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Economic/"

hist.funct(Names.Predictors.Econ,Title.Predictors.Econ,path)

#'
#' SCIENCE MODEL
#'
Names.Predictors.Sci <- c("High-technology exports (% of manufactured exports)","Research and development expenditure (% of GDP)","Scientific and technical journal articles")
Title.Predictors.Sci <- c("high.tech","r.n.d","journals")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Science/"

hist.funct(Names.Predictors.Sci,Title.Predictors.Sci,path)

high.tech <- df$`High-technology exports (% of manufactured exports)`
r.n.d <- df$`Research and development expenditure (% of GDP)`
journals <- df$`Scientific and technical journal articles`

plot(x = high.tech, y = log.GDP.per.capita, xlab = "High-tech exports (% of manufactured exports)", main = "More electricity, more wealth?")
abline(lm(log.GDP.per.capita ~ high.tech))

plot(x = r.n.d, y = log.GDP.per.capita, xlab = "R & D (% of GDP)", main = "More research, more wealth?")
abline(lm(log.GDP.per.capita ~ r.n.d))

plot(x = journals, y = log.GDP.per.capita, xlab = "Science Journal Articles", main = "More journals, more wealth?")
abline(lm(log.GDP.per.capita ~ journals))



#'
#' SOCIAL DEVELOPMENT MODEL
#'
Names.Predictors.Soc <- c("Children in employment, total (% of children ages 7-14)","Life expectancy at birth, male (years)","Refugee population by country or territory of origin","Literacy rate, adult total (% of people ages 15 and above)")
# "Prevalence of HIV, female (% ages 15-24)" is bombing out because of too many NAs.
Title.Predictors.Soc <- c("worker.children","life.expectancy","refugee.pop","literacy.rate")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/Histogram plots/Social Development/"

hist.funct(Names.Predictors.Soc,Title.Predictors.Soc,path)

worker.children <- df$`Children in employment, total (% of children ages 7-14)`
life.expectancy <- df$`Life expectancy at birth, male (years)`
hiv.prevalence <- df$`Prevalence of HIV, female (% ages 15-24)`
refugee.pop <- df$`Refugee population by country or territory of origin`
literacy.rate <- df$`Literacy rate, adult total (% of people ages 15 and above)`

plot(x = worker.children, y = log.GDP.per.capita, xlab = "Percent of Children Working (7-14)", main = "More child workers, more wealth?")
abline(lm(log.GDP.per.capita ~ worker.children))

plot(x = life.expectancy, y = log.GDP.per.capita, xlab = "Life Expectancy at birth, male (years)", main = "Higher life expectancy, more wealth?")
abline(lm(log.GDP.per.capita ~ life.expectancy))

plot(x = hiv.prevalence, y = log.GDP.per.capita, xlab = "Prevelance of HIV, female (% ages 15-24)", main = "Less HIV, more wealth?")
abline(lm(log.GDP.per.capita ~ hiv.prevalence))

plot(x = refugee.pop, y = log.GDP.per.capita, xlab = "Refugee population by country", main = "Fewer refugees, more wealth?")
abline(lm(log.GDP.per.capita ~ refugee.pop))

plot(x = literacy.rate, y = log.GDP.per.capita, xlab = "Adult Literacy Rate", main = "More literates, more wealth?")
abline(lm(log.GDP.per.capita ~ literacy.rate))






