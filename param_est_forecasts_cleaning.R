## Name: Elizabeth Lee
## Date: 8/8/16
## Function: Clean and export forecast parameter estimates
## Filenames: JTB_submission2_data/forecast_paramEst
## Data Source: 
## Notes: See cholera hangout notes 1/13/16; ME email subject: cholera hangout notes - 1/13/16
## 8/8/16: edits for JTB_submission2_data
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(ggplot2); require(readr)

setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### functions #################################
cleanData_forecast <- function(filename, dataframe){
  fitdays <- as.numeric(substring(filename, 9, 10)) # number of observations to inform forecast
  dummyD <- read_csv(filename, col_types = "iiiiiicddc") %>%
    mutate(obsDays = fitdays)
  newdataframe <- bind_rows(dataframe, dummyD)
  return(newdataframe)
}

#### import & clean forecast data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./JTB_submission2_data/forecast_paramEst")
fnames <- list.files() # grab list of file names
forecast.cl <- data.frame() # create empty data frame

for (i in 1:length(fnames)){
  dummyfile <- fnames[i]
  forecast.cl <- cleanData_forecast(dummyfile, forecast.cl)
}

## export trajectories - data combined ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("./JTB_submission2_data/")
write_csv(forecast.cl, "param_est_forecasts.csv") 
# 8/8/16
