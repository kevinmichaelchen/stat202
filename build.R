# Data from World Bank
# http://databank.worldbank.org/data/views/variableselection/selectvariables.aspx?source=world-development-indicators

# List of all the years downloaded
YEARS <- c("Y1980", "Y2008")

# Install the "reshape" package for pivoting the data frame
if (!"reshape" %in% rownames(installed.packages())) {
  install.packages("reshape")
}

#' Returns a year-specific data frame.
#' Filters out all but one year.
#' @param dataFrame The wide-formatted data frame that contains data for more than one year.
#' @param year The only year that will not be discarded / filtered out.
#' @param years The list of all years contained in the data frame.
filter <- function(dataFrame, year, years) {
  target = "Country.Code|Indicator.Code"  
  # Add every year but the current year to the target to be removed with grep
  for (i in 1:length(years)) {
    if (year == years[i]) {next}
    # the target for Y2008 is "Country.Code|Indicator.Code|Y0|Y1|...|Y2007"
    target = paste(target,years[i],sep="|")
  }
  return(dataFrame[-(grep(target, names(dataFrame)))])
}

#' Discard columns that are wholly comprised of NAs.
#' To see what is.na does, try entering:
#' is.na(ourFrame.2008[,0:2])
#' The following reports back the number of NAs in the first 2 columns:
#' colSums(is.na(ourFrame.2008[,0:2])) 
#' @param dataFrame The data frame.
discardNullColumns <- function(dataFrame) {
  return(dataFrame[,colSums(is.na(ourFrame.2008)) != nrow(ourFrame.2008)])
}

# STEP 1: Add year columns to data frame
data <- read.csv(file.choose(),header = TRUE)
names(data)[1] <- "Country.Name"
for (i in 1:length(YEARS)) {
  names(data)[4 + i] <- YEARS[i]
}

# STEP 2: Create data frames for each year
for (year in 1:length(YEARS)) {
  currentYear = YEARS[year]
  wideYearDF = filter(data, currentYear, YEARS)
  longYearDF = discardNullColumns(cast(wideYearDF, Country.Name ~ Indicator.Name, fun.aggregate=NULL, value=currentYear))
  # Write the data frame to file
  write.csv(longYearDF, file=currentYear)
}
