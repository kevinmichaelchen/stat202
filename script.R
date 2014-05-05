#' Checks for Not A Number (NaN) values.
isAllNumbers <- function(dataFrame) {
  isNumber.allColumns = rep(TRUE, ncol(dataFrame))
  for (z in 1:ncol(dataFrame)) {
    for (l in 1:nrow(dataFrame)) {
      if (is.nan(dataFrame[l,z]) == TRUE) {
        isNumber.allColumns[z] = FALSE
        print(paste("Column", z, "contains Not A Number (NaN) value(s)..."))
        break
      }
    }
  }
  return(length(which(isNumber.allColumns == FALSE)) <= 0)
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
#' @param dfName The name of the data frame.
#' @param response The response will not appear in the result, since you can't regress an indicator on itself.
#' @return The first list consists of log multiple regressions, the second of non-log regressions.
getRegressionLists <- function(dataFrame, dfName, response) {
  variables = names(dataFrame)
  colsign = checkAllLoggable(dataFrame)
  print("got out of check all logable")
  list.lm.log = list()
  list.lm = list()
  names = names(dataFrame)
  for (i in 2:ncol(dataFrame)) {
    indicator = names[i]
    if (indicator == response) {
      next
    }                   
    print(paste("Checking indicator:", indicator))
    if (colsign[i]) {
      list.lm.log[[i]] = eval(parse(text = paste0(
        "lm(log(`", response,"`) ~log(`", variables[i], "`), data = ", dfName, ", na.action = na.exclude)"
      )))
    }
    else {
      list.lm[[i]] = eval(parse(text = paste0(
        "lm(log(`", response, "`) ~ (`", variables[i], "`), data = ", dfName, ", na.action = na.exclude)"
      )))
    }
  }
  length(list.lm.log)
  length(list.lm)
  return(list("logs"=list.lm.log, "noLogs"=list.lm))
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
df <- read.csv(file.choose(), header = TRUE, check.names = TRUE)
variables <- names(df)
names(df) <- lapply(variables, function(x) {
  
  # if first two letters are 'X.', lop off 'X'
  if (substr(x, 1, 2) == 'X.') {
    x <- substr(x,2,nchar(x))    
  }
  
  # split based on dots
  chars <- unlist(strsplit(x, "[.]"))
  
  # remove empty tokens (i.e., remove double/triple dots)
  new.chars <- Filter(function(x) x != "", chars)
  
  # join the characters back into a string
  return(paste(new.chars, collapse = '.'))
})

# Fix one duplicate
names(df)[1074] <- "Secondary.education.teachers.female.percentage"

response <- "GDP.per.capita.constant.2005.US"
p1 <- "General.government.final.consumption.expenditure.of.GDP"
f1 <- paste(response, "~", p1)
summary(lm(as.formula(f1), data = df))

agriculture <- c(46:63, )

if (isAllNumbers(ourFrame)) {
  print("No Columns Contain NaNs")
}

reg.lists <- getRegressionLists(ourFrame, "ourFrame", "GDP (current US$)")
list.lm.log <- reg.lists$logs
list.lm <- reg.lists$noLogs

covariates.logged <- listBestCovariates(list.lm.log, 0.5, variables)
covariates <- listBestCovariates(list.lm, 0.5, variables)
