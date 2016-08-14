
## Name: Elizabeth Lee
## Date: 8/8/16
## Function: Create violin plots for NimBIOS Cholera project parameter estimates from forecasting -- by parameter, model, type of data -- as a function of percent deviation from the true parameters 
## Filenames: JTB_submission2_data/param_est_forecasts.csv
## Data Source: 
## Notes: new figure for revision to JTB
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R


#### header #################################
rm(list = ls())

setwd(dirname(sys.frame(1)$ofile)) 
require(tidyr)
require(dplyr)
require(ggplot2)
require(readr)

###### set this! ####################################
# ext for figures: png, pdf
ext <- "pdf"

###### import data ####################################
setwd("./JTB_submission2_data")
dat <- read_csv('param_est_forecasts.csv', col_types = 'iiiiiicddccdddiciccccc') %>% 
  mutate(param_expr = factor(param_expr, levels = c("beta[I]", "beta[W]", "alpha", "xi", "k", "R[0]"))) %>%
  mutate(noisecode2 = factor(noisecode2, levels = c("None", "Poisson", "Normal"), labels = c("No Noise", "Poisson Noise", "Normal Noise"))) %>% 
  mutate(model_data2 = factor(model_data2, levels = c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Waning Immunity"))) %>% 
  mutate(model_fit2 = factor(model_fit2, levels = c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Waning Immunity"))) %>%
  mutate(obsDays = factor(obsDays, levels = c("10", "30", "50")))

#### plotting params #############################################
ylabels <- paste0(c(0.001, 0.01, 0.1, 1, 10, 100, 1000), "%")
ybreaks <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
text.ylab2 <- "percent deviation from true parameter"
text.xlab <- "days of observed data"
# model.colors <- c('#00FF00', '#0000FF', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, waning immunity (green, blue, dark red, orange, purple)
w <- 9; h <- 8
lt <- 2 # line type

####### save figures ####################################################################
setwd(dirname(sys.frame(1)$ofile)) 
setwd("./figures")

### group by parameter, days observed as colors #########################################
param.plot2 <- ggplot(dat, aes(x=obsDays, y=perc_dev_true, group=obsDays)) + 
  geom_violin(position="dodge", trim=TRUE) +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  stat_summary(aes(group=obsDays), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=19) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), plot.title=element_text(hjust=0)) +
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle('A)') +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~param_expr, labeller = label_parsed)
print(param.plot2)
ggsave(sprintf("percTrueDev_forecasts_naiveStarting_byParam.%s", ext), param.plot2, width=w, height=h)

### group by fitting model, days observed as colors #########################################
fit.plot2 <- ggplot(dat, aes(x=obsDays, y=perc_dev_true, group=obsDays)) + 
  geom_violin(position="dodge", trim=TRUE) +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  stat_summary(aes(group=obsDays), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black')) +
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle('fitting model') +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~model_fit2)
ggsave(sprintf("percTrueDev_forecasts_naiveStarting_byModelfit.%s", ext), fit.plot2, width=w, height=h)

### group by simulating model, days observed as colors #########################################
simdata.plot2 <- ggplot(dat, aes(x=obsDays, y=perc_dev_true, group=obsDays)) + 
  geom_violin(position="dodge", trim=TRUE) +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  stat_summary(aes(group=model_data2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black')) +
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle('data simulation model') +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~model_data2)
ggsave(sprintf("percTrueDev_forecasts_naiveStarting_byModeldata.%s", ext), simdata.plot2, width=w, height=h)

### group by noise, days observed as colors #########################################
noise.plot2 <- ggplot(dat, aes(x=obsDays, y=perc_dev_true, group=obsDays)) + 
  geom_violin(position="dodge", trim=TRUE) +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  stat_summary(aes(group=obsDays), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), plot.title=element_text(hjust=0)) +
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle('B)') +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~noisecode2)
ggsave(sprintf("percTrueDev_forecasts_naiveStarting_byNoise.%s", ext), noise.plot2, width=w, height=h)

# exported 8/14/16
