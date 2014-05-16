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