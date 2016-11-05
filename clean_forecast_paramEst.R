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
d <- data.frame() # create empty data frame

for (i in 1:length(fnames)){
  dummyfile <- fnames[i]
  d <- cleanData_forecast(dummyfile, d)
}

#### data cleaning for parameter plots ################################
# 8/8/16 copied from param_est_100d_3y_cleaning.R

# 1) calculate percent deviation from actual value for each parameter, where perc dev = abs(actual - estimate)/actual * 100
d$perc_dev <- abs(d$actual - d$estimate)/d$actual * 100
# 10/1/15 log(0) is -Inf, which will not be plotted in ggplot with log transform

# 2) calculate true deviation (without absolute value) 
d$perc_dev_true <- (d$estimate - d$actual)/d$actual * 100

# 3) ratio of estimate to true value, center plot @ 100%, keep log scale
d$dev_ratio <- d$estimate/d$actual * 100

# create numeric codes for each parameter
# 1 = beta_i, 2 = beta_w, 3 = alpha, 4 = xi, 5 = k, 6 = R0
d$param_code <- 0
d[d$parameter == 'beta_i',]$param_code <- 1
d[d$parameter == 'beta_w',]$param_code <- 2
d[d$parameter == 'alpha',]$param_code <- 3
d[d$parameter == 'xi',]$param_code <- 4
d[d$parameter == 'k',]$param_code <- 5
d[d$parameter == 'R_0',]$param_code <- 6

d2 <- tbl_df(d) 

# create new code for non/pois/norm data
d2$noise <- paste(d2$pois_data, d2$norm_data, sep = '')
d2$noisecode <- 0
d2$noisecode[which(d2$noise == '00')] <- 1 # non-noisy
d2$noisecode[which(d2$noise == '10')] <- 2 # poisson
d2$noisecode[which(d2$noise == '01')] <- 3 # normal

# ###### 8/30/15: check data with tables in ms draft #####################################
# d2.check5a <- tbl_df(d2) %>% filter(model_data==1 & informed==1 & epidemic==1 & pois_data+norm_data==0)
# d2.check5b <- tbl_df(d2) %>% filter(model_data==4 & informed==1 & epidemic==1 & pois_data+norm_data==0)

#### data processing: model numbers should be labeled with words ####################################
# vs. fitting model
d3 <- d2 %>% 
  mutate(model_fit2 = as.factor(ifelse(model_fit==1, 'Exponential', ifelse(model_fit==2, 'Gamma', ifelse(model_fit==3, 'Asymptomatic', ifelse(model_fit==4, 'Dose Response', ifelse(model_fit==5, 'Progressive', NA))))))) %>% 
  mutate(model_fit2 = factor(model_fit2, levels(model_fit2)[c(3, 2, 1, 4, 5)])) %>% 
  mutate(parameter = factor(parameter, c("beta_i", "beta_w", "alpha", "xi", "k", "R_0"))) 
# vs. data simulation model
d4 <- d3 %>% 
  mutate(model_data2 = as.factor(ifelse(model_data==1, 'Exponential', ifelse(model_data==2, 'Gamma', ifelse(model_data==3, 'Asymptomatic', ifelse(model_data==4, 'Dose Response', ifelse(model_data==5, 'Progressive', NA))))))) %>% 
  mutate(model_data2 = factor(model_data2, levels(model_data2)[c(3, 2, 1, 4, 5)]))
# vs. noise
d5 <- d4 %>% 
  mutate(noisecode2 = as.factor(ifelse(noisecode==1, 'None', ifelse(noisecode==2, "Poisson", ifelse(noisecode==3, "Normal", NA))))) %>% 
  mutate(noisecode2 = factor(noisecode2, levels(noisecode2)[c(1, 3, 2)])) %>% 
  mutate(modChoiceID = paste0(model_data, pois_data, norm_data, model_fit, epidemic, informed))

#### 10/17/15: labeller expressions for parameters ##########################################
d6 <- d5  %>% mutate(param_expr = ifelse(parameter == "beta_i", "beta[I]", 
                                         ifelse(parameter == "beta_w", "beta[W]", 
                                                ifelse(parameter == "R_0", "R[0]", as.character(parameter))))) %>% 
  mutate(param_expr = factor(param_expr, levels = c("beta[I]", "beta[W]", "alpha", "xi", "k", "R[0]")))

## export trajectories - data combined ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("./JTB_submission2_data/")
write_csv(d6, "param_est_forecasts.csv") 
# 8/8/16
