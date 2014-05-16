### Logistic Regression

df <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

# Our response
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)
Names.Response <- c("GDP per capita (current US$)")

THRESHOLD = 15000

#' AGRICULTURAL MODEL
#' 
Names.Predictors.Agr <- c("Improved water source, rural (% of rural population with access)","Rural population (% of total population)")
Title.Predictors.Agr <- c("rural.water","rural.population.percent")

#' URBAN MODEL
#' 
Names.Predictors.Urb <- c("Improved water source, urban (% of urban population with access)","Improved sanitation facilities, urban (% of urban population with access)","Urban population (% of total)")
Title.Predictors.Urb <- c("urban.water","urban.sanitation","urban.population")

#' CLIMATE CHANGE MODEL
#' 
Names.Predictors.Cli <- c("Electric power consumption (kWh per capita)","CO2 emissions (kt)","Methane emissions (kt of CO2 equivalent)","Nitrous oxide emissions (thousand metric tons of CO2 equivalent)","Roads, paved (% of total roads)")
Title.Predictors.Cli <- c("electricity.consumption","emissions.co2.kt","emissions.methane","emissions.nitrous","paved.roads")



#' SCIENCE MODEL
#'
Names.Predictors.Sci <- c("High-technology exports (% of manufactured exports)","Research and development expenditure (% of GDP)","Scientific and technical journal articles")
Title.Predictors.Sci <- c("high.tech","r.n.d","journals")

#' SOCIAL DEVELOPMENT MODEL
#'
Names.Predictors.Soc <- c("Children in employment, total (% of children ages 7-14)","Life expectancy at birth, male (years)","Refugee population by country or territory of origin","Literacy rate, adult total (% of people ages 15 and above)")
# "Prevalence of HIV, female (% ages 15-24)" is bombing out because of too many NAs.
Title.Predictors.Soc <- c("worker.children","life.expectancy","refugee.pop","literacy.rate")


## New logit dataframe
Names.Response.logit <- paste(Names.Response,"Logit (> 15000$)")
df.logit <- df[c("Country.Name",Names.Response,Names.Predictors.Agr,Names.Predictors.Urb,Names.Predictors.Cli,Names.Predictors.Sci,Names.Predictors.Soc)]
df.logit <- as.data.frame(append(df.logit, list("GDP per capita (current US$) Logit (> 15000$)" = df.logit[,Names.Response]), after = 2),optional = TRUE)



# Set binary data for response (> 15000$) == 1
for(j in 1:nrow(df.logit[Names.Response])){
  if((is.na(df.logit[Names.Response][j,])) == TRUE){next}
  
  if((df.logit[Names.Response][j,] > THRESHOLD) == TRUE){
    df.logit[Names.Response.logit][j,] <- 1
  }
  if((df.logit[Names.Response][j,] > THRESHOLD) == FALSE){
    df.logit[Names.Response.logit][j,] <- 0
  }
}

View(df.logit)

# glm(df.logit$`GDP per capita (current US$) Logit (> 15000$)` ~ df.logit$`Improved water source, rural (% of rural population with access)`, family=binomial)
# 
# # Create list of logit models
# logit.models <- list()
# for(i in 4:ncol(df.logit)){
#   logit.models[[i]] <- eval(parse(text = paste0(
#     "glm(df.logit$'",Names.Response.logit,"' ~ df.logit$'", names(df.logit)[i], "', data = df.logit,family = binomial, na.action = na.exclude)"
#   )))
# }
# 
# # Create list of summaries(logit models),list of confint(logit models),list of odds ratio of logit models:
# summary.logit.models <- list()
# confint.logit.models <- list()
# odds.ratio.logit <- list()
# for(i in 4:length(logit.models)){
#   summary.logit.models[[i]] <- summary(logit.models[[i]])
#   confint.logit.models[[i]] <- confint(logit.models[[i]])
#   odds.ratio.logit[[i]] <- exp(logit.models[[i]]$coef[2])
# }



# # Example of logistic regression analysis for logit.models[[6]]: "Food production index (2004-2006 = 100)"
# logit.models[[6]]
# summary(logit.models[[6]])
# confint(logit.models[[6]])
# exp(confint(logit.models[[6]]))
# odds.ratio <- exp(logit.models[[6]]$coef[2])
# paste("odds decrease of Food Production Index =",1-odds.ratio,sep =" ")
###################################
logit.models.Urb <- NULL
logit.funct <- function(Names.Predictors.Substitute){
  Names.Predictors.comb <- paste(paste0("df.logit$`",Names.Predictors.Substitute,"`"),collapse = " + ")
  logit.models.predictor <- eval(parse(text = paste0(
    "glm(df.logit$'",Names.Response.logit,"' ~ ", Names.Predictors.comb, ", data = df.logit,family = binomial, na.action = na.exclude)"
  )))
  return(logit.models.predictor)
}


logit.models.Agr <- logit.funct(Names.Predictors.Agr)
logit.models.Urb <- logit.funct(Names.Predictors.Urb)
logit.models.Cli <- logit.funct(Names.Predictors.Cli)
logit.models.Sci <- logit.funct(Names.Predictors.Sci)
logit.models.Soc <- logit.funct(Names.Predictors.Soc)


