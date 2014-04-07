# Get Data from World Bank website http://databank.worldbank.org/data/views/variableselection/selectvariables.aspx?source=world-development-indicators
# Download by hand World Development indicators into working directory "API Source"  
setwd(file.choose())

##################

# Dataframe setup will only work with the "reshape" package, for pivoting data
#library("reshape")

# Read in downloaded datset into dataframe 'data'
data <- read.csv(list.files(path=getwd()),header = TRUE)
names(data)[1] <- "Country.Name"
names(data)[5] <- "Y1980"
names(data)[6] <- "Y2008"
View(data)

# Dataframe 'data' is split into dataframes: 'data.test.2008' and 'data.test.1980'
data.test.2008 <- data[-(c(grep("Country.Code",names(data)),grep("Indicator.Code",names(data)),grep("Y1980",names(data))))] # remove Country.Code, Indicator.Code, Y1980 column
data.test.1980 <- data[-(c(grep("Country.Code",names(data)),grep("Indicator.Code",names(data)),grep("Y2008",names(data))))] # remove Country.Code, Indicator.Code, Y2008 column
# Pivot/unmelt dataframe 'data.test.2008' to create new dataframe 'ourFrame.2008' with columns of predictors and response.
ourFrame.2008 <- cast(data.test.2008,Country.Name ~ Indicator.Name, fun.aggregate = NULL, value = "Y2008")
ourFrame.2008 <- ourFrame.2008[,colSums(is.na(ourFrame.2008)) != nrow(ourFrame.2008)]
View(ourFrame.2008)

##############################

View(ourFrame.2008)

# Create vectors of variable names: response.name, predictors.name
# predictors.name <- names(ourFrame.2008)[-c(grep("Country.Name",names(ourFrame.2008)),grep("GDP.current.US.",names(ourFrame.2008)))]
# response.name <- names(ourFrame.2008)[c(grep("GDP.current.US.",names(ourFrame.2008)))]
variables.name <- names(ourFrame.2008)

##############################  

# in dataframe ourFrame.2008 check for presence of NaN values
#check.nan <- NULL
check.nan <- rep(FALSE, ncol(ourFrame.2008))
for(z in 1:ncol(ourFrame.2008)){
  for(l in 1:nrow(ourFrame.2008)){
    if(is.nan(ourFrame.2008[l,z]) == TRUE){
      check.nan[z] <- TRUE
      break
    }
  }
}

grep("TRUE",check.nan)    # searches for TRUE in check.nan. Should result in: "integer(0)". This indicates that there are no NaN in the dataframe.

##############################  

# Setting up choices for log transformation:
# For each column i in dataframe ourFrame.2008, set vector colsign[i] based on the presence/absence of 0 or negative numbers to FALSE/TRUE respectively.
colsign <- NULL
for(i in 2:ncol(ourFrame.2008)){
  colsign[i] <- TRUE                  # setting default colsign = TRUE
  for(j in 1:nrow(ourFrame.2008)){ # iterate through each row in column i
    if((is.na(ourFrame.2008[j,i]) != TRUE) & (ourFrame.2008[j,i]) <= 0) {
      colsign[i] <- FALSE
      break
    }
  }
}            
head(colsign)

# if ourFrame[i] contains all positive numbers, set colsign[i] = TRUE
# if ourFrame[i] contains a 0 or negative number, set colsign[i] = FALSE
# all other cases, set colsign[i] = TRUE

##############################

# Run univariate linear regression of transformed predictors in ourFrame.2008:
# Note: transform is only applicable for positive numbers based on colsign[i] above.
res.1 <- list()
res.2 <- list()
for(i in 2:ncol(ourFrame.2008)){
  if(i == 391){next}                    # skip column 391 which is the response = "GDP.current.US."
  if(colsign[i] == TRUE){
    res.1[[i]] <- eval(parse(text = paste0(
      "lm(log(`GDP (current US$)`) ~log(`", variables.name[i], "`), data = ourFrame.2008, na.action = na.exclude)"
    )))
  }
  if(colsign[i] == FALSE) {
    res.2[[i]] <- eval(parse(text = paste0(
      "lm(log(`GDP (current US$)`) ~ (`", variables.name[i], "`), data = ourFrame.2008, na.action = na.exclude)"
    )))
  }
}
length(res.1)
length(res.2)

##############################  

# Select list() of best univariate predictors based on r^2 > 0.5
res.1.summary <- lapply(res.1, summary)
res.1.r2 <- unlist(lapply(res.1.summary,"[","r.squared"))
head(res.1.r2)
res.1.r2.2 <- unlist(lapply(res.1.summary,"[","adj.r.squared"))
names(res.1.r2.2) <- variables.name
head(res.1.r2.2)
res.1.r2.num <- as.numeric(res.1.r2)
res.1.r2.greater.than.0.5. <- (res.1.r2.num > 0.5)
covariates <- grep("TRUE",res.1.r2.greater.than.0.5.)              # lists the best univariate predictors of GDP (r^2 > 0.5). reports n for res.1.summary[n]
covariates

# Select list() of best univariate predictors based on r^2 > 0.5 for nolog(predictors)
res.2.summary <- lapply(res.2, summary)
res.2.r2 <- unlist(lapply(res.2.summary,"[","r.squared"))
head(res.2.r2)
res.2.r2.2 <- unlist(lapply(res.1.summary,"[","adj.r.squared"))
names(res.2.r2.2) <- variables.name
head(res.2.r2.2)
res.2.r2.num <- as.numeric(res.2.r2)  
res.2.r2.greater.than.0.5. <- (res.2.r2.num > 0.5)
covariates.no.log <- grep("TRUE",res.2.r2.greater.than.0.5.)              # lists the best univariate predictors of GDP (r^2 > 0.5). reports n for res.1.summary[n]
covariates.no.log

##############################

# Use best Univariate predictors in Multi-Variate Model
# View plot with abline for example res.1.summary[25]. I need to figure out how to modularize plots to assess identified predictors for multivariate fit.
#  res.1.summary[25]
#  plot(log(ourFrame.2008$`GDP (current US$)`) ~ log(ourFrame.2008$`Adjusted savings: carbon dioxide damage (current US$)`))
#  abline(lm(log(ourFrame.2008$`GDP (current US$)`) ~ log(ourFrame.2008$`Adjusted savings: carbon dioxide damage (current US$)`),na.action = na.exclude))

# Create multivariate models
# names of the logged predictors
covariates.log.name <- NULL
for(i in 1: length(covariates)){
  covariates.log.name[i] <- names(ourFrame.2008[covariates[i]])
}

# names of unlogged predictor
covariates.no.log.name <- NULL
for(i in 1: length(covariates.no.log)){
  covariates.no.log.name[i] <- names(ourFrame.2008[covariates.no.log[[i]]])
}

# concatenate all logged predictors into string to be evaluated by lm function
test <- paste0("log(`",covariates.log.name,"`)")
test.1 <- paste(test,collapse = " + ")

# multiple regression model of response vs. the unlogged and logged covariates.
test.lm <- NULL
test.lm <- eval(parse(text = paste0("lm(log(`GDP (current US$)`) ~ `Child employment in manufacturing, female (% of female economically active children ages 7-14)` + ", test.1,", data = ourFrame.2008)")))


#  for(i in 1:length(covariates.log.name)){
#    test.lm <- eval(parse(text = paste0(
#      "lm(log(`GDP (current US$)`) ~ `Child employment in manufacturing, female (% of female economically active children ages 7-14))` + ", test[1],"+",test[2],", data = ourFrame.2008, na.action = na.exclude)"
#    )))
#  }

#  test.lm <- NULL
#  for(i in 1:length(covariates.log.name)){
#    test.lm <- eval(parse(text = paste0(
#      "lm(log(`GDP (current US$)`) ~ `Child employment in manufacturing, female (% of female economically active children ages 7-14)` + ", paste("log(`",covariates.log.name[i],sep = "` + `"), "`), data = ourFrame.2008, na.action = na.exclude)"
#    )))
#  }

# Use nested F-test to pick predictors for multivariate model


# Assess/Infer quality of predictors (predictor coefficients must be significantly different from each other) in Multi-Variate Model

# Produce quality of fit of final Multivariate model

