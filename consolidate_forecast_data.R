
## Name: Elizabeth Lee
## Date: 11/23/15
## Function: consolidate forecasting data
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(gdata); require(XLConnect)
require(dplyr); require(tidyr)
require(readr)

#### workbook features ################################
model.codes <- c("M1", "M4", "M3B", "M2", "M5")
model.names <- c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Waning Immunity")
model.df <- data.frame(model_code = model.codes, model = model.names, stringsAsFactors = FALSE)
observed.days <- c(10, 30, 50)

#### cleaning functions ################################
cleanFunc1 <- function(dataset, days, modelc){
  dummy <- dataset %>%
    mutate(t = seq_along(TrueNoise)-1) %>%
    mutate(obsTrueNoise = ifelse(t > days, NA, TrueNoise)) %>%
    mutate(generating_model_code = modelc) %>%
    mutate(obsDays = days) %>%
    select(-Model3A) %>%
    rename(M1 = Model1, M2 = Model2, M3B = Model3B, M4 = Model4, M5 = Model5)
  return(dummy)
}
mergeFunc <- function(newDF, tmpDF){
  fullDF <- bind_rows(newDF, tmpDF)
  return(fullDF)
}
cleanFunc2 <- function(fullDF, modDF){
  fullDF2 <- fullDF %>% gather(forecasting_model_code, forecastData, 1:5) %>%
    left_join(modDF, by = c("forecasting_model_code" = "model_code")) %>%
    mutate(forecasting_model = factor(model, levels = model.df$model)) %>%
    select(-model) %>%
    left_join(modDF, by = c("generating_model_code" = "model_code")) %>%
    mutate(generating_model = factor(model, levels = model.df$model)) %>%
    select(-model) %>%
    select(generating_model, generating_model_code, forecasting_model, forecasting_model_code, t, obsDays, forecastData, obsTrueNoise, TrueNoise, TrueClean)
  return(fullDF2)
}

#### read & clean data ################################
setwd("../")

wb <- loadWorkbook("forecastingData.xlsx")
# initialize new dataframe
newdf <- data.frame(M1 = NULL, M2 = NULL, M3B = NULL, M4 = NULL, M5 = NULL, 
                    TrueClean = NULL, TrueNoise = NULL, t = NULL, obsTrueNoise = NULL, 
                    generatingModel = NULL, obsDays = NULL)

for (day in observed.days){
  for (model in model.codes){
    sheetname <- sprintf("%sd Forecasts to %s", day, model)
    print(sheetname)
    tmp <- readWorksheet(wb, sheet = sheetname)
    tmp2 <- cleanFunc1(tmp, day, model)
    newdf <- mergeFunc(newdf, tmp2)
  }
}

fullD <- cleanFunc2(newdf, model.df)

#### write data ################################
setwd("./forecasts")
write_csv(fullD, "all_forecast_data.csv")
# saved 11/23/15
