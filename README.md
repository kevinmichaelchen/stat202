STAT202
=======

## Downloading the data  
1. Go to the following [link](http://databank.worldbank.org/data/views/variableselection/selectvariables.aspx?source=world-development-indicators).  
2. Go to the *COUNTRY* tab and select all countries.  
3. Go to the *SERIES* tab and select all indicators.  
4. Go to the *TIME* tab and select the desired years.  
5. Press *DOWNLOAD* and go to the *CSV* tab.
7. Under *Modify Orientation*:  
  * Export range: Entire dataset
  * Data format: Table
  * Variable format: Both codes & names
  * NA preference: NA
  * Text field delimiter: none

## Overview  
This repo contains the dataset, R script, and writeups for the 202 final project. We analyze data from **The World Bank** and explore relationships between economic growth and a myriad indicators (with an emphasis on urbanization indicators).  

Running `build.R` will ask the user to choose a file of the original wide-format data (e.g., `originalData.csv`). The script will create a data frame for each year that was downloaded.  

The original data is in *wide* format:  

| Country Name | Country.Code | Indicator.Name | Indicator.Code | X1980..YR1980. | X2008..YR2008. |
| ------------ | ------------ | -------------- | -------------- | -------------- | -------------- |
| Afghanistan  | AFG          | CO2 emissions  | EN.ATM.CO2E.KT | 1.760160e+03   | 3.927357e+03   |

Note: originally the downloaded data only had 2 years: "Y1980" and "Y2008".
`YEARS <- c("Y1980", "Y2008")`.
If this changes, the `YEARS` variable at the top of `build.R` must change accordingly.  

Once `build.R` has executed, there should be CSV files for each year, in the *long* format.  

| Country Name | Country.Code | Indicator.Code | CO2 emissions |
| ------------ | ------------ | -------------- | ------------- |
| Afghanistan  | AFG          | EN.ATM.CO2E.KT | 1.760160e+03  |
