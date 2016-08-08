
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

## export trajectories - data combined ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("./JTB_submission2_data/")
write_csv(fits100, "trajectory_fits_100d.csv") 
# 8/8/16
