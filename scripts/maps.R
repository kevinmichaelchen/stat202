## Map of Predictors: Visual Analysis
require(rworldmap)

df <- read.csv(file.choose(), header = TRUE, check.names = FALSE)
a=0
b=0
create.maps <- function(dataFrame,Names.Predictors,Title.Predictors,Path){
  if(length(Names.Predictors) %% 2 == 1){
    n = length(Names.Predictors)
    b=1
  }
  else{n = length(Names.Predictors)}
  if(length(Names.Predictors) < 2) {a=1}
  n = length(Names.Predictors)
  jpeg(paste0(Path,substitute(Title.Predictors),"_countrymap.jpeg"),width = 600,height = 600,quality = 100)
  par(mfrow = c(2-a,(n+b)/2),oma = c(2,1,1,1))  
  
  for(i in 1:length(Names.Predictors)){
    
    map.sync <- joinCountryData2Map(dataFrame, nameJoinColumn = "Country.Code", nameCountryColumn = "Country.Name", suggestForFailedCodes = T, verbose=T)
    mapCountryData(map.sync, nameColumnToPlot = Names.Predictors[i], colourPalette = "heat",mapTitle = Title.Predictors[i], aspect = 'variable', oceanCol = "dark blue",missingCountryCol = "white")    
  }
  dev.off()
  graphics.off()
}

## Our Response
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)
Names.Response <- c("GDP per capita (current US$)")
Title.Response <- c("GDP.per.capita")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/country_maps/Response/"
create.maps(df,Names.Response,Title.Response,path)

## Predictor Buckets:

### Agricultural

Names.Predictors.Agr <- c("Improved water source, rural (% of rural population with access)","Rural population (% of total population)")
Title.Predictors.Agr <- c("rural.water","rural.population.percent")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/country_maps/Agricultural/"
create.maps(df,Names.Predictors.Agr,Title.Predictors.Agr,path)


### Urban

Names.Predictors.Urb <- c("Improved water source, urban (% of urban population with access)","Improved sanitation facilities, urban (% of urban population with access)","Urban population (% of total)")
Title.Predictors.Urb <- c("urban.water","urban.sanitation","urban.population")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/country_maps/Urban/"
create.maps(df,Names.Predictors.Urb,Title.Predictors.Urb,path)



### Climate Change

Names.Predictors.Cli <- c("Electric power consumption (kWh per capita)","CO2 emissions (kt)","Methane emissions (kt of CO2 equivalent)","Nitrous oxide emissions (thousand metric tons of CO2 equivalent)","Roads, paved (% of total roads)")
Title.Predictors.Cli <- c("electricity.consumption","emissions.co2.kt","emissions.methane","emissions.nitrous","paved.roads")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/country_maps/Climate/"
create.maps(df,Names.Predictors.Cli,Title.Predictors.Cli,path)


### Science Model

Names.Predictors.Sci <- c("High-technology exports (% of manufactured exports)","Research and development expenditure (% of GDP)","Scientific and technical journal articles")
Title.Predictors.Sci <- c("high.tech","r.n.d","journals")
path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/country_maps/Science/"
create.maps(df,Names.Predictors.Sci,Title.Predictors.Sci,path)



### Social Development Model

# Names.Predictors.Soc <- c("Children in employment, total (% of children ages 7-14)","Life expectancy at birth, male (years)","Refugee population by country or territory of origin","Literacy rate, adult total (% of people ages 15 and above)")
# # "Prevalence of HIV, female (% ages 15-24)" is bombing out because of too many NAs.
# Title.Predictors.Soc <- c("worker.children","life.expectancy","refugee.pop","literacy.rate")


Names.Predictors.Soc <- c("Children in employment, total (% of children ages 7-14)","Life expectancy at birth, male (years)")
Title.Predictors.Soc <- c("worker.children","life.expectancy")

path <- "/Users/varunsharma/Dropbox/STAT 202 Final/STAT202/country_maps/Social Development/"
create.maps(df,Names.Predictors.Soc,Title.Predictors.Soc,path)

jpeg(paste0(path,substitute(Title.Predictors.Soc),"_countrymap_col.jpeg"),width = 600,height = 380,quality = 100)
par(mfrow = c(1,2),oma = c(2,1,1,1))  
for(i in 1:length(Names.Predictors.Soc)){
  
  map.sync <- joinCountryData2Map(df, nameJoinColumn = "Country.Code", nameCountryColumn = "Country.Name", suggestForFailedCodes = T, verbose=T)
  mapCountryData(map.sync, nameColumnToPlot = Names.Predictors.Soc[i], colourPalette = "heat",mapTitle = Title.Predictors.Soc[i], aspect = 2, oceanCol = "dark blue",missingCountryCol = "white")    
}
dev.off()
graphics.off()



