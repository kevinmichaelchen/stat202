### Presentation Predictors Condition plots

# Load the data frame
df <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

ROOT <- "~/Dropbox/STAT 202 Final/STAT202/Presentation Plots/"
CONDITION.PLOT.DIR <- paste0(ROOT,"condition_plots/")

#'
#' @param df.names list of actual names of variables used by eval function
#' @param titles list of titles for plots - can be anything
write.conditions <- function(df.names, titles, path){
  
  # number of predictors
  n <- length(df.names)
  
  # write to jpeg file
  jpeg(paste0(path, titles[1], ".jpeg"), width = 1000 * n, height = 500 * n)
  
  # 2 rows, one for residuals vs fitted, one for qqline
  par(mfrow = c(2,n))
  
  for(i in 1:n){
    # j is 1 because the resid vs fitted is the 1st plot given by plot(lm)
    j <- 1  
    my.predictor = eval(parse(text = paste0("df$`",df.names[i],"`")))
    my.lm <- lm(log.GDP.per.capita ~ my.predictor, data = as.data.frame(df), na.action = na.exclude)
    my.sub.caption = eval(parse(text = paste0("'",titles[i],"'")))
    my.main = eval(parse(text = paste0("'",titles[i],"'")))
    # pch=dot type, cex=dot size, lwd=line width
    plot(my.lm, which = c(j), sub.caption = my.sub.caption, main = my.main, pch = 16, cex = 2, lwd = 7)
    title(main = my.main, outer = FALSE)
  }
  
  for(i in 1:n){
    my.predictor = eval(parse(text = paste0("df$`",df.names[i],"`")))
    my.lm <- lm(log.GDP.per.capita ~ my.predictor, data = as.data.frame(df), na.action = na.exclude)
    
    #qqplot(my.lm)
    #qqline(my.lm, col = "red", lwd = 2, lty = 2)
    
    # j is 2 because the qqline is the 2nd plot given by plot(lm)
    j <- 2
    my.sub.caption = eval(parse(text = paste0("'",titles[i],"'")))
    my.main = eval(parse(text = paste0("'",titles[i],"'")))
    plot(my.lm, which = c(j), sub.caption = my.sub.caption, main = my.main, pch = 16, cex = 2, lwd = 7)
    title(main = my.main, outer = FALSE)
  }
  
  dev.off()
  graphics.off()
}


### Our response
log.GDP.per.capita <- log(df$`GDP per capita (current US$)`)

### Agricultural Model
predictors.agr <- c("Improved water source, rural (% of rural population with access)",
                    "Rural population (% of total population)")
titles.agr <- c("rural.water",
                "rural.population.percent")

### Urban
predictors.urb <- c("Improved water source, urban (% of urban population with access)",
                    "Improved sanitation facilities, urban (% of urban population with access)",
                    "Urban population (% of total)")
titles.urb <- c("urban.water",
                "urban.sanitation",
                "urban.population")

### Climate Change
predictors.cli <- c("Electric power consumption (kWh per capita)",
                    "CO2 emissions (kt)",
                    "Methane emissions (kt of CO2 equivalent)",
                    "Nitrous oxide emissions (thousand metric tons of CO2 equivalent)",
                    "Roads, paved (% of total roads)")
titles.cli <- c("electricity.consumption",
                "emissions.co2.kt",
                "emissions.methane",
                "emissions.nitrous",
                "paved.roads")

### Science Model
predictors.sci <- c("High-technology exports (% of manufactured exports)",
                    "Research and development expenditure (% of GDP)",
                    "Scientific and technical journal articles")
titles.sci <- c("high.tech",
                "r.n.d",
                "journals")

### Social Development Model
predictors.soc <- c("Children in employment, total (% of children ages 7-14)",
                    "Life expectancy at birth, male (years)",
                    "Refugee population by country or territory of origin",
                    "Literacy rate, adult total (% of people ages 15 and above)")
titles.soc <- c("worker.children",
                "life.expectancy",
                "refugee.pop",
                "literacy.rate")

### List of lists of predictors
predictors <- list(predictors.agr, predictors.urb, predictors.cli, predictors.sci, predictors.soc)

### List of lists of titles
titles <- list(titles.agr, titles.urb, titles.cli, titles.sci, titles.soc)

for (i in 1:length(predictors)) {
  p = predictors[[i]]
  t = titles[[i]]
  write.conditions(p, t, CONDITION.PLOT.DIR)  
}

