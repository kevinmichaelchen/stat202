#' Data from World Bank
#' http://databank.worldbank.org/data/views/variableselection/selectvariables.aspx?source=world-development-indicators

#' Surrounds string in backticks
addq <- function(x) paste0("`", x, "`")

#' Returns a data frame with only one specific year.
#' @param dataFrame Data frame in WIDE format.
#' @param year The year that will be kept (e.g. "Y2008")
#' @param years The list of all years in data frame (e.g. list("Y1980","Y2008"))
getYearFrame <- function(dataFrame, year, years) {
  target = paste(COUNTRY.CODE, INDICATOR.CODE, sep="|")
  # Add every year but the current year to the target to be removed with grep
  for (i in 1:length(years)) {
    if (year == years[i]) {next}
    # the target for Y2008 is "Country Code|Indicator Code|Y0|Y1|...|Y2007"
    target = paste(target, years[i], sep="|")
  }
  return(dataFrame[-(grep(target, names(dataFrame)))])
}



#' Prettify column names.
renameColumns <- function(dataFrame, years) {
  names(dataFrame)[1] <- COUNTRY.NAME
  names(dataFrame)[2] <- COUNTRY.CODE
  names(dataFrame)[3] <- INDICATOR.NAME
  names(dataFrame)[4] <- INDICATOR.CODE
  for (i in 1:length(years)) {
    names(dataFrame)[4 + i] <- years[i]
  }
  # (data <<- dataFrame) same as assign('dataFrame', data, envir=.GlobalEnv)
  return(dataFrame)
}



#' Writes to file a long-format frame for each year.
writeYearFrames <- function(data, years) {
  for (i in 1:length(years)) {
    year = years[i]
    wideYearDF = getYearFrame(data, year, years)
    myFormula = as.formula(paste0("data$", addq(COUNTRY.NAME), "~ ", "data$", addq(INDICATOR.NAME)))
    longYearDF = cast(wideYearDF, formula=myFormula, fun.aggregate=NULL, value=year)
    write.csv(longYearDF, file=paste0(year,".csv"), row.names=FALSE)
  }
}



# CONSTANTS
ORIGINAL.DATA = "originalData.csv"
YEARS <- c("Y1980", "Y2008")
COUNTRY.NAME = "Country Name"
COUNTRY.CODE = "Country Code"
INDICATOR.NAME = "Indicator Name"
INDICATOR.CODE = "Indicator Code"

# Choose original data set to set working directory.
print(paste0("Pick ", ORIGINAL.DATA, " in order to setwd (the file name matters): "))
file <- file.choose()
setwd(substr(file,0,(nchar(file) - nchar(ORIGINAL.DATA))))

# Install the "reshape" package for pivoting the data frame
if (!"reshape" %in% rownames(installed.packages())) {
  install.packages("reshape")
}
library("reshape", lib.loc=.libPaths()[1])



data <- read.csv(file.choose(), header = TRUE, check.names = FALSE)
data <- renameColumns(data, YEARS)
writeYearFrames(data, YEARS)
