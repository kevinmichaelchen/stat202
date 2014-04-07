ourFrame <- file.choose()
variables.name <- names(ourFrame)

#' Checks for Not A Number (NaN) values.
checkAllNumbers <- function(dataFrame) {
  #isNumber.allColumns <- rep(TRUE, ncol(dataFrame))
  for(z in 1:ncol(dataFrame)){
    for(l in 1:nrow(dataFrame)){
      if(is.nan(dataFrame[l,z])){
        #isNumber.allColumns[z] <- FALSE
        print(paste("Column", z, "contains Not A Number (NaN) value(s)..."))
        break
      }
    }
  }
  #return(isNumber.allColumns)
}

#' Checks for log-ability.
#' A number can be logged if it is a non-zero positive.
checkAllLoggable <- function(dataFrame) {
  #isLoggable.allColumns <- rep(TRUE, ncol(dataFrame))
  for (z in 1:ncol(dataFrame)) {
    for (l in 1:nrow(dataFrame)) {
      if ( !is.na(dataFrame[l,z]) & dataFrame[l,z]) <= 0 ) {
        print(paste("Column", z, "contains a negative and thus is not loggable..."))
        #isLoggable.allColumns[z] <- FALSE
        break
      }
    }
  }
#return(isLoggable.allColumns)
}

#'  linear regression of transformed predictors in ourFrame:
#' Note: transform is only applicable for positive numbers based on colsign[i] above.
res.1 <- list()
res.2 <- list()
for(i in 2:ncol(ourFrame)){
  # skip column 391 which is the response = "GDP.current.US."
  if(i == 391){
    next
  }                    
  if(colsign[i] == TRUE){
    res.1[[i]] <- eval(parse(text = paste0(
      "lm(log(`GDP (current US$)`) ~log(`", variables.name[i], "`), data = ourFrame, na.action = na.exclude)"
    )))
  }
  if(colsign[i] == FALSE) {
    res.2[[i]] <- eval(parse(text = paste0(
      "lm(log(`GDP (current US$)`) ~ (`", variables.name[i], "`), data = ourFrame, na.action = na.exclude)"
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
#  plot(log(ourFrame$`GDP (current US$)`) ~ log(ourFrame$`Adjusted savings: carbon dioxide damage (current US$)`))
#  abline(lm(log(ourFrame$`GDP (current US$)`) ~ log(ourFrame$`Adjusted savings: carbon dioxide damage (current US$)`),na.action = na.exclude))

# Create multivariate models
# names of the logged predictors
covariates.log.name <- NULL
for(i in 1: length(covariates)){
  covariates.log.name[i] <- names(ourFrame[covariates[i]])
}

# names of unlogged predictor
covariates.no.log.name <- NULL
for(i in 1: length(covariates.no.log)){
  covariates.no.log.name[i] <- names(ourFrame[covariates.no.log[[i]]])
}

# concatenate all logged predictors into string to be evaluated by lm function
test <- paste0("log(`",covariates.log.name,"`)")
test.1 <- paste(test,collapse = " + ")

# multiple regression model of response vs. the unlogged and logged covariates.
test.lm <- NULL
test.lm <- eval(parse(text = paste0("lm(log(`GDP (current US$)`) ~ `Child employment in manufacturing, female (% of female economically active children ages 7-14)` + ", test.1,", data = ourFrame)")))


#  for(i in 1:length(covariates.log.name)){
#    test.lm <- eval(parse(text = paste0(
#      "lm(log(`GDP (current US$)`) ~ `Child employment in manufacturing, female (% of female economically active children ages 7-14))` + ", test[1],"+",test[2],", data = ourFrame, na.action = na.exclude)"
#    )))
#  }

#  test.lm <- NULL
#  for(i in 1:length(covariates.log.name)){
#    test.lm <- eval(parse(text = paste0(
#      "lm(log(`GDP (current US$)`) ~ `Child employment in manufacturing, female (% of female economically active children ages 7-14)` + ", paste("log(`",covariates.log.name[i],sep = "` + `"), "`), data = ourFrame, na.action = na.exclude)"
#    )))
#  }

# Use nested F-test to pick predictors for multivariate model


# Assess/Infer quality of predictors (predictor coefficients must be significantly different from each other) in Multi-Variate Model

# Produce quality of fit of final Multivariate model

