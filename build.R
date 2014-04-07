# Data from World Bank
# http://databank.worldbank.org/data/views/variableselection/selectvariables.aspx?source=world-development-indicators

# List of all the years downloaded
YEARS <- c("Y1980", "Y2008")

# Install the "reshape" package for pivoting the data frame
if (!"reshape" %in% rownames(installed.packages())) {
  install.packages("reshape")
}

#' Finds the country with the longest name.
longestName <- function(dataFrame) {
  maxCountry = ""
  maxCount = 0
  for (r in 1:nrow(dataFrame)) {
    country = dataFrame[r,"Country.Name"]
    # annoying way of getting string length
    count = length(unlist(strsplit(as.character(country),split="")))
    print(paste("Country:", country, "Count:", count))
    if (count > maxCount) {
      maxCountry = country;
      maxCount = count;
    }
  }
  print(paste("Longest country is", maxCountry, "with", maxCount, "letters..."))
}

cellLength <- function(cell) {
  return(length(unlist(strsplit(as.character(cell),split=""))))
}

#' Returns a list of countries with the fewest NA values.
#' Scans rows and returns the first cells of those rows 
#' with the fewest NAs.
#' @param dataFrame The data frame. Must be in LONG format.
#' @param n Will return the top n cells.
fewestNAs <- function(dataFrame, n) {
  minNames = rep("", n)
  minCounts = rep(ncol(dataFrame), n)
  minRows = rep(0, n)
  
  for (country in 1:nrow(dataFrame)) {
    name = dataFrame[country,"Country.Name"]
    na.count = 0
    for (i in 1:ncol(dataFrame)) {
      if (is.na(dataFrame[country,i])) {
        na.count = na.count + 1
      }
    }

    #if (na.count < 300) {print(paste(country, na.count))}
    
    maxCount = max(minCounts)
    # the index of the first max occurrence 
    maxIndex = which.max(minCounts)
    
    # Found a new min! Add it to min list!
    if (na.count < maxCount) {
      minNames[maxIndex] = name
      minCounts[maxIndex] = na.count
      minRows[maxIndex] = country
    }
  }  
  minRows = sort(minRows)
  for (i in length(minRows)) {
    print(dataFrame[i,"Country.Name"])
  }
  return(sort(minRows))
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
