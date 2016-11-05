
## Name: Elizabeth Lee
## Date: 8/8/16
## Function: Clean fit and forecast trajectory data and export
## Filenames: JTB_submission2_data/fits100day_trajectories, JTB_submission2_data/forecast_trajectories
## Data Source: 
## Notes: 8/8/16: edits for JTB_submission2_data
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(ggplot2); require(readr)

setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### functions #################################
cleanData <- function(filename, dataframe){
  fitdays <- as.numeric(substring(filename, 9, 10)) # number of observations to inform forecast
  dummyD <- read_csv(filename, col_types = "ccccid")%>%
    mutate(obsDays = fitdays)
  newdataframe <- bind_rows(dataframe, dummyD)
  return(newdataframe)
}

#### import & clean forecast trajectory data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./JTB_submission2_data/forecast_trajectories")
fnames <- list.files() # grab list of file names
forecasts <- data.frame() # create empty data frame

for (i in 1:length(fnames)){
  dummyfile <- fnames[i]
  forecasts <- cleanData(dummyfile, forecasts)
}

#### prepare forecast trajectory data for plotting ################################
dat2 <- tbl_df(forecasts) %>%
  rename(forecasting_model = fitting_model) %>%
  mutate(generating_model = ifelse(generating_model == "Waning", "Progressive", generating_model)) %>%
  mutate(forecasting_model = ifelse(forecasting_model == "Waning", "Progressive", forecasting_model))

# forecast data only
foreDat <- dat2 %>% 
  filter(forecasting_model != "True Trajectory" & forecasting_model != "Observed Data")
# true data only
truDat <- dat2 %>%
  filter(forecasting_model == "True Trajectory" | forecasting_model == "Observed Data") %>%
  spread(forecasting_model, data) 
names(truDat) <- c("generating_model", "noise", "starting", "t", "obsDays", "obsTrueNoise", "TrueClean")
# re-merge forecast & true data
fullDat <- full_join(foreDat, truDat, by = c("generating_model", "noise", "starting", "t", "obsDays")) %>%
  rename(forecastData = data) %>%
  select(generating_model, forecasting_model, noise, starting, t, obsDays, forecastData, obsTrueNoise, TrueClean)

## export trajectories - data combined ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("./JTB_submission2_data/")
write_csv(fullDat, "trajectory_forecasts_100d.csv") 
# 11/5/16
