## Conditions Analysis

# ~/Desktop/STAT202/Final/data/Y2008.csv

ROOT <- "~/Desktop/STAT202/Final/"
DATA <- paste0(ROOT,"data/")
CONDITION.PLOT.DIR <- paste0(ROOT,"writeups/images/condition_plots/")
GVLMA.PLOT.DIR <- paste0(ROOT,"writeups/images/gvlma_plots/")
setwd(DATA)
list.files()

# Load the data frame
df <- read.csv(paste0(DATA,"Y2008.csv"), header = TRUE, check.names = FALSE)





#### Global Statistic Assumptions Test: 
## Ho: B1 = B2 = B3 = Bn = 0
## Ha: B1 != B2! = B3 != Bn != 0
require(gvlma)


gvmodels.funct <- function(Names.Predictors.Substitute,
                           Title.Predictors.Substitute,
                           Path,
                           gvmodel.List.per.bucket,
                           summary.List.per.bucket,
                           plot.List.per.bucket) {
  for(i in 1:length(Names.Predictors.Substitute)) {
    jpeg(paste0(Path,Title.Predictors.Substitute[i],"_gvmodelplot.jpeg"))
    gvmodel.List.per.bucket[[i]] <- gvlma(eval(parse(text = paste0("lm(log.GDP.per.capita ~ df$`",Names.Predictors.Substitute[i],"`,data = df)"))))
    summary.List.per.bucket[[i]] <- summary(gvmodel.List.per.bucket[[i]])
    plot(gvmodel.List.per.bucket[[i]])
    #print(plot.List.per.bucket[[i]])
    dev.off()
    graphics.off()
  }
  #return(gvmodel.List.per.bucket,summary.List.per.bucket)
}

test.list <- list()
test.1 <- paste(paste0("log(df$`",Names.Predictors.Urb,"`)"),collapse = " + ")
test.list[[1]] <- gvlma(eval(parse(text = paste0("lm(log.GDP.per.capita ~ ",test.1,",data = df)"))))

paste0("lm(log.GDP.per.capita ~ ",test.1,",data = df)")


# Our response
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)
boxplot(df$`GDP per capita (current US$)`)
boxplot(log.GDP.per.capita)





# Check for equal variance and Normality

# Agricultural Model

Names.Predictors.Agr <- c("Improved water source, rural (% of rural population with access)",
                          "Poverty headcount ratio at urban poverty line (% of urban population)",
                          "Food production index (2004-2006 = 100)",
                          "Arable land (% of land area)",
                          "Rural population (% of total population)")
Title.Predictors.Agr <- c("rural.water",
                          "rural.poor",
                          "food.production",
                          "arable.land",
                          "rural.population.percent")

for(i in 1:length(Names.Predictors.Agr)) {
  jpeg(paste0(CONDITION.PLOT.DIR,"agricultural/",Title.Predictors.Agr[i],"_residplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Agr[i],"`"))),data = df,na.action = na.exclude),which = c(1),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Agr[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Agr[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Agr[i],"'"))),outer = FALSE)
  dev.off()
  
  jpeg(paste0(CONDITION.PLOT.DIR,"agricultural/",Title.Predictors.Agr[i],"_qqplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Agr[i],"`"))),data = df,na.action = na.exclude),which = c(2),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Agr[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Agr[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Agr[i],"'"))),outer = FALSE)
  dev.off()
}

# gvlm
path <- paste0(GVLMA.PLOT.DIR,"science/")
Agr.gvmodel = list()
summary.Agr.gvmodel = list()
plot.Agr.gvmodel = list()

gvmodels.funct(Names.Predictors.Agr,Title.Predictors.Agr,path,Agr.gvmodel,summary.Agr.gvmodel,plot.Agr.gvmodel)




### Urban

Names.Predictors.Urb <- c("Motor vehicles (per 1,000 people)","Improved water source, urban (% of urban population with access)","Improved sanitation facilities, urban (% of urban population with access)","Population in the largest city (% of urban population)","Urban population (% of total)","Poverty headcount ratio at urban poverty line (% of urban population)","Pump price for gasoline (US$ per liter)","Pump price for diesel fuel (US$ per liter)","Road sector energy consumption (% of total energy consumption)")
Title.Predictors.Urb <- c("motor.vehicles.per.1000","urban.water","urban.sanitation","urban.primacy","urban.population","urban.poverty.percentage","price.of.gas","price.of.diesel","road.sector.energy")

for(i in 1:length(Names.Predictors.Urb)) {
  jpeg(paste0(CONDITION.PLOT.DIR,"urban/",Title.Predictors.Urb[i],"_residplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Urb[i],"`"))),data = df,na.action = na.exclude),which = c(1),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Urb[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Urb[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Urb[i],"'"))),outer = FALSE)
  dev.off()
  
  jpeg(paste0(CONDITION.PLOT.DIR,"urban/",Title.Predictors.Urb[i],"_qqplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Urb[i],"`"))),data = df,na.action = na.exclude),which = c(2),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Urb[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Urb[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Urb[i],"'"))),outer = FALSE)
  dev.off()
}

# gvlm
path <- paste0(GVLMA.PLOT.DIR,"urban/")
Urb.gvmodel = list()
summary.Urb.gvmodel = list()
plot.Urb.gvmodel = list()

gvmodels.funct(Names.Predictors.Urb,Title.Predictors.Urb,path,Urb.gvmodel,summary.Urb.gvmodel,plot.Urb.gvmodel)




### Climate Change

Names.Predictors.Cli <- c("Electric power consumption (kWh per capita)","CO2 emissions (kt)","Methane emissions (kt of CO2 equivalent)","Nitrous oxide emissions (thousand metric tons of CO2 equivalent)","Roads, paved (% of total roads)")
Title.Predictors.Cli <- c("emissions.co2.kt","emissions.methane","emissions.nitrous","electricity.consumption","paved.roads")

for(i in 1:length(Names.Predictors.Cli)) {
  jpeg(paste0(CONDITION.PLOT.DIR,"climate/",Title.Predictors.Cli[i],"_residplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Cli[i],"`"))),data = df,na.action = na.exclude),which = c(1),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Cli[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Cli[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Cli[i],"'"))),outer = FALSE)
  dev.off()
  
  jpeg(paste0(CONDITION.PLOT.DIR,"climate/",Title.Predictors.Cli[i],"_qqplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Cli[i],"`"))),data = df,na.action = na.exclude),which = c(2),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Cli[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Cli[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Cli[i],"'"))),outer = FALSE)
  dev.off()
}

# gvlm
path <- paste0(GVLMA.PLOT.DIR,"climate/")
Cli.gvmodel = list()
summary.Cli.gvmodel = list()
plot.Cli.gvmodel = list()

gvmodels.funct(Names.Predictors.Cli,Title.Predictors.Cli,path,Cli.gvmodel,summary.Cli.gvmodel,plot.Cli.gvmodel)




### Labor Model

Names.Predictors.Labor <- c("Unemployment, total (% of total labor force)","Labor force participation rate, total (% of total population ages 15-64)","Labor force participation rate, female (% of female population ages 15-64)","Labor force participation rate, male (% of male population ages 15-64)","Labor force participation rate, total (% of total population ages 15-24)","Labor force participation rate, female (% of female population ages 15-24)","Labor force participation rate, male (% of male population ages 15-24)","Employees, services, male (% of male employment)","Employees, services, female (% of female employment)","Employees, industry, male (% of male employment)","Employees, industry, female (% of female employment)","Employees, agriculture, male (% of male employment)","Employees, agriculture, female (% of female employment)")
Title.Predictors.Labor <- c("unemployment","lpr","lpr.female","lpr.male","lpr.youth","lpr.youth.female","lpr.youth.male","services.male","services.female","industry.male","industry.female","agriculture.male","agriculture.female")

for(i in 1:length(Names.Predictors.Labor)) {
  jpeg(paste0(CONDITION.PLOT.DIR,"labor/",Title.Predictors.Labor[i],"_residplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Labor[i],"`"))),data = df,na.action = na.exclude),which = c(1),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Labor[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Labor[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Labor[i],"'"))),outer = FALSE)
  dev.off()
  
  jpeg(paste0(CONDITION.PLOT.DIR,"labor/",Title.Predictors.Labor[i],"_qqplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Labor[i],"`"))),data = df,na.action = na.exclude),which = c(2),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Labor[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Labor[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Labor[i],"'"))),outer = FALSE)
  dev.off()
}

# gvlm
path <- paste0(GVLMA.PLOT.DIR,"labor/")
Labor.gvmodel = list()
summary.Labor.gvmodel = list()
plot.Labor.gvmodel = list()

gvmodels.funct(Names.Predictors.Labor,Title.Predictors.Labor,path,Labor.gvmodel,summary.Labor.gvmodel,plot.Labor.gvmodel)




### Economic Model

Names.Predictors.Econ <- c("Exports of goods and services (% of GDP)","Imports of goods and services (% of GDP)","Trade in services (% of GDP)","Central government debt, total (% of GDP)","Revenue, excluding grants (% of GDP)","Public spending on education, total (% of government expenditure)","Military expenditure (% of central government expenditure)","Tax revenue (% of GDP)")
Title.Predictors.Econ <- c("exports","imports","trade","debt","revenue","education","military","taxes")

for(i in 1:length(Names.Predictors.Econ)) {
  jpeg(paste0(CONDITION.PLOT.DIR, "economic/",Title.Predictors.Econ[i],"_residplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Econ[i],"`"))),data = df,na.action = na.exclude),which = c(1),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Econ[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Econ[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Econ[i],"'"))),outer = FALSE)
  dev.off()
  
  jpeg(paste0(CONDITION.PLOT.DIR, "economic/",Title.Predictors.Econ[i],"_qqplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Econ[i],"`"))),data = df,na.action = na.exclude),which = c(2),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Econ[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Econ[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Econ[i],"'"))),outer = FALSE)
  dev.off()
}

# gvlm
path <- paste0(GVLMA.PLOT.DIR,"economic/")
Econ.gvmodel = list()
summary.Econ.gvmodel = list()
plot.Econ.gvmodel = list()

gvmodels.funct(Names.Predictors.Econ,Title.Predictors.Econ,path,Econ.gvmodel,summary.Econ.gvmodel,plot.Econ.gvmodel)




### Science Model

Names.Predictors.Sci <- c("High-technology exports (% of manufactured exports)","Research and development expenditure (% of GDP)","Scientific and technical journal articles")
Title.Predictors.Sci <- c("high.tech","r.n.d","journals")

for(i in 1:length(Names.Predictors.Sci)) {
  jpeg(paste0(CONDITION.PLOT.DIR, "science/",Title.Predictors.Sci[i],"_residplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Sci[i],"`"))),data = df,na.action = na.exclude),which = c(1),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Sci[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Sci[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Sci[i],"'"))),outer = FALSE)
  dev.off()
  
  jpeg(paste0(CONDITION.PLOT.DIR, "science/",Title.Predictors.Sci[i],"_qqplot.jpeg"))
  par(oma= c(0,2,1,2))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Sci[i],"`"))),data = df,na.action = na.exclude),which = c(2),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Sci[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Sci[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Sci[i],"'"))),outer = FALSE)
  dev.off()
}

# gvlm
path <- paste0(GVLMA.PLOT.DIR,"science/")
Sci.gvmodel = list()
summary.Sci.gvmodel = list()
plot.Sci.gvmodel = list()

gvmodels.funct(Names.Predictors.Sci,Title.Predictors.Sci,path,Sci.gvmodel,summary.Sci.gvmodel,plot.Sci.gvmodel)



### Social Development Model

Names.Predictors.Soc <- c("Children in employment, total (% of children ages 7-14)","Life expectancy at birth, male (years)","Refugee population by country or territory of origin","Literacy rate, adult total (% of people ages 15 and above)")
# "Prevalence of HIV, female (% ages 15-24)" is bombing out because of too many NAs.
Title.Predictors.Soc <- c("worker.children","life.expectancy","refugee.pop","literacy.rate")

for(i in 1:length(Names.Predictors.Soc)) {
  jpeg(paste0(CONDITION.PLOT.DIR, "social_development/",Title.Predictors.Soc[i],"_residplot.jpeg"))
  par(oma= c(0,0,1,0))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Soc[i],"`"))),data = df,na.action = na.exclude),which = c(1),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Soc[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Soc[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Soc[i],"'"))),outer = FALSE)
  dev.off()
  
  jpeg(paste0(CONDITION.PLOT.DIR, "social_development/",Title.Predictors.Soc[i],"_qqplot.jpeg"))
  par(oma= c(0,0,1,0))
  plot(lm(log.GDP.per.capita ~ eval(parse(text = paste0("df$`",Names.Predictors.Soc[i],"`"))),data = df,na.action = na.exclude),which = c(2),sub.caption = eval(parse(text = paste0("'",Title.Predictors.Soc[i],"'"))),main = eval(parse(text = paste0("'",Title.Predictors.Soc[i],"'"))))
  title(main = eval(parse(text = paste0("'",Title.Predictors.Soc[i],"'"))),outer = FALSE)
  dev.off()
}


graphics.off()

# gvlm
path <- paste0(GVLMA.PLOT.DIR,"social_development/")

Soc.gvmodel = list()
summary.Soc.gvmodel = list()
plot.Soc.gvmodel = list()

gvmodels.funct(Names.Predictors.Soc,Title.Predictors.Soc,path,Soc.gvmodel,summary.Soc.gvmodel,plot.Soc.gvmodel)





