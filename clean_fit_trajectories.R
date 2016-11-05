
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
  dummyD <- read_csv(filename, col_types = "ccccid")
  newdataframe <- bind_rows(dataframe, dummyD)
  return(newdataframe)
}

#### import & clean 100d trajectory data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./JTB_submission2_data/fits100day_trajectories")
fnames100 <- list.files() # grab list of file names
fits100 <- data.frame() # create empty data frame

for (i in 1:length(fnames100)){
  dummyfile <- fnames100[i]
  fits100 <- cleanData(dummyfile, fits100)
}

finalDat <- fits100 %>%
  mutate(generating_model = ifelse(generating_model == "Waning", "Progressive", generating_model)) %>%
  mutate(fitting_model = ifelse(fitting_model == "Waning", "Progressive", fitting_model))

#### import & clean 3yr trajectory data ################################
# 8/14/16 added
setwd(dirname(sys.frame(1)$ofile))
setwd("./JTB_submission2_data/fits3yr_trajectories")
fnames3 <- list.files() # grab list of file names
fits3 <- data.frame() # create empty data frame

for (i in 1:length(fnames3)){
  dummyfile <- fnames3[i]
  fits3 <- cleanData(dummyfile, fits3)
}

finalDat3 <- fits3 %>%
  mutate(generating_model = ifelse(generating_model == "Waning", "Progressive", generating_model)) %>%
  mutate(fitting_model = ifelse(fitting_model == "Waning", "Progressive", fitting_model))

## export trajectories - one file per duration ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("./JTB_submission2_data/")
write_csv(finalDat, "trajectory_fits_100d.csv") 
write_csv(finalDat3, "trajectory_fits_3yr.csv")
# 11/5/16 model name
