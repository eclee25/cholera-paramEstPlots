
## Name: Elizabeth Lee
## Date: 12/28/13 
## Function: Create boxplots for NimBIOS Cholera project parameter estimates -- by parameter, model, type of data -- as a function of percent deviation from the true parameters
## Filenames: param_est_alldata_27_12_13.csv, param_est_m4aic_28_12_13.csv
## Data Source: param_est_m4aic_28_12_13.csv from ForParameterEstimateTables... file, param_est_alldata_27_12_13.csv from EstimatesandAICs... file
## Notes: Michael had to recalculate model 4's AIC values using a different equation and he updated only the ForParamterEstimateTables... file. Elizabeth had to perform a correction on all of model 3's estimates of k because of the 20% symptomatic ratio.
# 12/28/13: Are the m4aic values correct? It seems that some of the values are very large.
# 10/1/15: updates to waning immunity model estimates

## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
# 
# ###########  12/28/13 changes ##################
# setwd('/home/elee/Dropbox/NIMBioS Project Cholera/Param_Est_Plots')
# d <- read.csv('param_est_alldata_27_12_13.csv', colClasses = 'character', na.strings = 'NA', header = TRUE)
# aic <-read.csv('param_est_m4aic_28_12_13.csv', colClasses = 'character', na.strings = 'NA', header = TRUE)
# names(aic) <- names(d)
# 
# # create uqid
# d$uqid <- paste(d$model_data, d$pois_data, d$norm_data, d$model_fit, d$epidemic, d$informed, d$parameter, sep = '')
# aic$uqid <- paste(aic$model_data, aic$pois_data, aic$norm_data, aic$model_fit, aic$epidemic, aic$informed, aic$parameter, sep = '')
# 
# ## merge updated m4 aic values with other data
# d[(which(d$uqid %in% aic$uqid)),]$estimate # aic values that will be removed
# aic$estimate # aic values that will replace those that are removed
# d_minusm4aic <- d[(-which(d$uqid %in% aic$uqid)),]
# d2 <- rbind(d_minusm4aic, aic)
# 
# ## update m3 k values
# # first, identify m3 k values in d2 using regular expressions
# patt_m3kuqid <- '([[:digit:]]{3})3([01]{2})k'
# d2[grep(patt_m3kuqid, d2$uqid),]$uqid # correct items
# # break apart dataset with only m3k values
# m3k <- d2[grep(patt_m3kuqid, d2$uqid),]
# d2_minusm3k <- d2[-(grep(patt_m3kuqid, d2$uqid)),]
# # multiple m3k values by 5 to correct for symptomatic ratio of 20%
# m3k$estimate <- as.numeric(m3k$estimate)
# m3k$estimate <- m3k$estimate * 5
# # re-merge datasets
# d2_minusm3k$estimate <- as.numeric(d2_minusm3k$estimate)
# d3 <- rbind(d2_minusm3k, m3k)
# 
# # write file
# write.csv(d3, 'param_est_alldata.csv', row.names = FALSE) # 12/28/13 20:35 EST
# 
# ###########  1/14/14 changes ##################
# setwd('/home/elee/Dropbox/NIMBioS Project Cholera/Param_Est_Plots')
# d1 <- read.csv('param_est_alldata_14_1_14.csv', colClasses = 'character', na.strings = 'NA', header = TRUE)
# 
# ### where model_fit == 2, the parameter labels of alpha and xi need to be flipped ###
# # these estimates were flipped in EstimatesandAICS.xlsx, which is the spreadsheet on which this data was based
# id_mfit2_alpha <- which(d1$model_fit == 2 & d1$parameter == 'alpha')
# id_mfit2_xi <- which(d1$model_fit == 2 & d1$parameter == 'xi')
# changed_mfit2 <- c(id_mfit2_alpha, id_mfit2_xi)
# # change alpha parameter labels to xi
# newxi <- d1[id_mfit2_alpha,]
# newxi$parameter <- gsub('alpha', 'xi', newxi$parameter)
# newxi$uqid <- gsub('alpha', 'xi', newxi$uqid)
# # change xi parameter labels to alpha
# newalpha <- d1[id_mfit2_xi,]
# newalpha$parameter <- gsub('xi', 'alpha', newalpha$parameter)
# newalpha$uqid <- gsub('xi', 'alpha', newalpha$uqid)
# # re-merged changed data with unchanged data
# d1_notchanged <- d1[-changed_mfit2,]
# d2 <- rbind(d1_notchanged, newxi, newalpha)
# 
# 
# ### where model_fit == 3, add 2 * num of estimated parameters to all AICs ##
# # the AICs were not calculated with the number of parameters penalty term
# id_mfit3_aic <- which(d2$model_fit == 3 & d2$parameter == 'aic')
# newm3aic <- d2[id_mfit3_aic,]
# newm3aic$estimate <- as.numeric(newm3aic$estimate) + 14
# # re-merged changed data with unchanged data
# d2_notchanged <- d2[-id_mfit3_aic,]
# d3 <- rbind(d2_notchanged, newm3aic)
# 
# ### where (model_fit == 3 | model_fit == 1) & epidemic == 0 & pois_data == 1, aic values need to be updated to account for new seasonal poisson data aic calculation ###
# # Previously, these AICs were going to -Inf. A 1e-20 term was added to the AIC calculation for these fits in order to prevent the AICs from reaching -Inf. When these fits were re-run, there was no change to the parameter estimates because AIC was calculated after the fits were optimized.
# # list of uqids that need their AICs replaced: 110301aic, 210301aic, 310301aic, 410301aic, 510301aic, 110300aic, 210300aic, 310300aic, 410300aic, 510300aic, 110101aic, 210101aic, 310101aic, 410101aic, 510101aic, 110100aic, 210100aic, 310100aic, 410100aic, 510100aic
# replaceaic_uqids <- c('110301aic', '210301aic', '310301aic', '410301aic', '510301aic', '110300aic', '210300aic', '310300aic', '410300aic', '510300aic', '110101aic', '210101aic', '310101aic', '410101aic', '510101aic', '110100aic', '210100aic', '310100aic', '410100aic', '510100aic')
# id_mfit13_pois_aic <- which(d3$uqid %in% replaceaic_uqids)
# replaceaic <- d3[id_mfit13_pois_aic,] # model_fit == 1 is before model_fit == 3
# # list new aic values in same order as replaceaic dataset
# newaics_pois <- c(-5591790.37750509, -Inf, -7263584.97584725, -8297336.77681617, -346271.77749087, -5591790.58, -103199.99, -7263584.97, -8297336.69764206, -344295.745877692, -5591789.50814607, -104664.095330821, -7265831.37935805, -8297336.40170543, -342020.042860242, -5591784.84933912, -103202.251242387, -7265831.380145, -8297332.57616819, -343041.453095341)
# # model_data == 2 & model_fit == 1 had imaginary numbers in AIC; still needs to be rerun.
# replaceaic$estimate <- newaics_pois
# # re-merge changed data with unchanged data
# d3_notchanged <- d3[-id_mfit13_pois_aic,]
# d4 <- rbind(d3_notchanged, replaceaic)
# 
# ### write new dataset ###
# setwd('/home/elee/Dropbox/NIMBioS Project Cholera/Param_Est_Plots')
# write.csv(d4, 'param_est_alldata.csv', row.names = FALSE) # 1/14/14 20:52 EST
# 


###########  10/1/15 changes ##################
require(readr)
require(tidyr)
require(dplyr)
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
d.og <- read_csv('param_est_alldata_01_10_15.csv')
names(d.og) <- c('model_data', 'pois_data', 'norm_data', 'model_fit', 'epidemic', 'informed', 'parameter', 'actual', 'estimate', 'uqid')
d2 <- d.og %>% filter(epidemic==1 & parameter !='aic')
d2$parameter <- gsub('.AF8-', '_', d2$parameter) # . matches 1 character
d2$estimate <- gsub('.AC0', '', d2$estimate)
d2$uqid <- gsub('.AF8-', '_', d2$uqid)
# 17 changes to epidemic data outlined in WaningImmunityParameterUpdates.xlsx
d3 <- d2 %>% 
  mutate(estimate = ifelse((model_fit==5 & model_data==5 & norm_data==1 & informed==0 & parameter=='alpha'), 0.002, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==1 & pois_data==1 & informed==1 & parameter=='alpha'), 0.002, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==4 & pois_data==1 & informed==1 & parameter=='beta_i'), 0.2508, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==4 & pois_data==1 & informed==1 & parameter=='beta_w'), 8.9934, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==4 & pois_data==1 & informed==1 & parameter=='alpha'), 0.0253, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==4 & pois_data==1 & informed==1 & parameter=='xi'), 0.0013, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==4 & pois_data==1 & informed==1 & parameter=='k'), 0.2508, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==1 & parameter=='beta_i'), 0.2556, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==1 & parameter=='beta_w'), 0.603, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==1 & parameter=='alpha'), 0.0037, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==1 & parameter=='xi'), 0.0063, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==1 & parameter=='k'), 1.63E-05, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==0 & parameter=='beta_i'), 0.2348, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==0 & parameter=='beta_w'), 0.2995, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==0 & parameter=='alpha'), 0.0002, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==0 & parameter=='xi'), 0.0254, estimate)) %>%
  mutate(estimate = ifelse((model_fit==5 & model_data==3 & pois_data==1 & informed==0 & parameter=='k'), 3.90E-05, estimate))
  
  
  
  
  
  
  
  
  
  
  

