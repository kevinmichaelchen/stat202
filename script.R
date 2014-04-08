#' Checks for Not A Number (NaN) values.
checkAllNumbers <- function(dataFrame) {
  #isNumber.allColumns = rep(TRUE, ncol(dataFrame))
  for (z in 1:ncol(dataFrame)) {
    for (l in 1:nrow(dataFrame)) {
      if (is.nan(dataFrame[l,z])) {
        #isNumber.allColumns[z] = FALSE
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
  logs <- rep(TRUE, ncol(dataFrame))
  for (z in 1:ncol(dataFrame)) {
    for (l in 1:nrow(dataFrame)) {
      cell = dataFrame[l,z]
      if (is.na(cell) || class(cell) == 'logical') {next}
      if (as.numeric(cell) <= 0) {        
        print(paste("Column", z, "cannot be logged due to a", cell))
        logs[z] = FALSE
        break
      }
    }
  }
  return(logs)
}

#' Returns two lists of multiple regressions.
#' @param dataFrame The data frame.
#' @return The first list consists of log multiple regressions, the second of non-log regressions.
getRegressionLists <- function(dataFrame) {
  variables = names(dataFrame)
  colsign = checkAllLoggable(dataFrame)
  list.lm.log = list()
  list.lm = list()
  for (i in 2:ncol(dataFrame)) {
    # skip column 391 which is the response = "GDP.current.US."
    if (i == 391) {
      next
    }                    
    if (colsign[i]) {
      list.lm.log[[i]] = eval(parse(text = paste0(
        "lm(log(`GDP (current US$)`) ~log(`", variables[i], "`), data = ourFrame, na.action = na.exclude)"
      )))
    }
    else {
      list.lm[[i]] = eval(parse(text = paste0(
        "lm(log(`GDP (current US$)`) ~ (`", variables[i], "`), data = ourFrame, na.action = na.exclude)"
      )))
    }
  }
  length(list.lm.log)
  length(list.lm)
  return(list(list.lm.log, list.lm))
}

#' Returns a list of the best covariates.
#' A covariate x is deemed good if cor(x,y) > cor.threshold.
#' @param lm.list A list of multiple regression objects
#' @param cor.threshold Defines which covariates are deemed "good"
#' @param names The names of variables in lm.list
listBestCovariates <- function(lm.list, cor.threshold, names) {
  summaries <- lapply(lm.list, summary)
  cors <- unlist(lapply(summaries,"[","r.squared"))
  cors.adj <- unlist(lapply(summaries,"[","adj.r.squared"))
  names(cors.adj) <- names
  cors.adj.num <- as.numeric(cors.adj)
  cors.adj.num.culled <- (cors.adj.num > cor.threshold)
  covariates <- grep("TRUE", cors.adj.num.culled)
  return(covariates)
}

# LOAD THE DATA FRAME
ourFrame <- read.csv(file.choose(), header = TRUE)
variables <- names(ourFrame)
reg.lists <- getRegressionLists(ourFrame)
list.lm.log <- reg.lists[0]
list.lm <- reg.lists[1]
covariates.logged <- listBestCovariates(list.lm.log, 0.5, variables)
covariates <- listBestCovariates(list.lm, 0.5, variables)

##############################

# Use best Univariate predictors in Multi-Variate Model
# View plot with abline for example list.lm.log.summary[25]. I need to figure out how to modularize plots to assess identified predictors for multivariate fit.
#  list.lm.log.summary[25]
#  plot(log(ourFrame$`GDP (current US$)`) ~ log(ourFrame$`Adjusted savings: carbon dioxide damage (current US$)`))
#  abline(lm(log(ourFrame$`GDP (current US$)`) ~ log(ourFrame$`Adjusted savings: carbon dioxide damage (current US$)`),na.action = na.exclude))

# names of the logged predictors
names.covariates.logged <- rep("", length(covariates.logged))
for (i in 1:length(covariates.logged)) {
  # TODO is it [i] or [[i]]?
  covariate = covariates.logged[i]
  names.covariates.logged[i] <- names(ourFrame[covariate])
}

# names of unlogged predictors
names.covariates <- rep("", length(covariates))
for (i in 1:length(covariates)) {
  # TODO is it [i] or [[i]]?
  covariate = covariates[i]
  names.covariates[i] <- names(ourFrame[covariate])
}

# concatenate all logged predictors into string to be evaluated by lm function
#TODO we're missing commas here
test <- paste0("log(`",names.covariates.logged,"`)")
test.1 <- paste(test,collapse = " + ")

# multiple regression model of response vs. the unlogged and logged covariates.
test.lm <- eval(parse(text = paste0("lm(log(`GDP (current US$)`) ~ `Child employment in manufacturing, female (% of female economically active children ages 7-14)` + ", test.1, ", data = ourFrame)")))


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

