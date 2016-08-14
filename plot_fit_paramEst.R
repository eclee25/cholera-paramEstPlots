
## Name: Elizabeth Lee
## Date: 10/1/15
## Function: Create violin plots for NimBIOS Cholera project parameter estimates -- by parameter, model, type of data -- as a function of percent deviation from the true parameters (cleaned up from v1)
## Filenames: param_est_alldata_100d_3y.csv
## Data Source: 
## Notes: 8/8/16 - add R0 panel to figure, new directory structure
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
# setEpicode = 1 means 100 day simulations
# setEpicode = 2 means 3 year simulations
setEpicode <- 1
ext <- "pdf"
# 4/18/16 11:12 pm

###### plot settings ####################################
if (setEpicode == 1){
  figlab <- "A)"
} else if (setEpicode == 2){
  figlab <- "B)"
}

###### import data ####################################
setwd("./JTB_submission2_data")
d <- read_csv('param_est_100d_3y.csv', col_types = 'iiiiiicddcdddiciccccc') %>% 
  mutate(param_expr = factor(param_expr, levels = c("beta[I]", "beta[W]", "alpha", "xi", "k", "R[0]"))) %>%
  mutate(noisecode2 = factor(noisecode2, levels = c("None", "Poisson", "Normal"))) %>% 
  mutate(model_data2 = factor(model_data2, levels = c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Waning Immunity"))) %>% 
  mutate(model_fit2 = factor(model_fit2, levels = c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Waning Immunity")))

#### plotting params #############################################
ylabels <- paste0(c(0.001, 0.01, 0.1, 1, 10, 100, 1000), "%")
ybreaks <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
text.ylab <- "absolute value of deviation from true parameter"
text.ylab2 <- "percent deviation from true parameter"
text.ylab3 <- "percentage of estimate relative to true parameter"
text.xlab <- "fitting model"
model.colors <- c('#00FF00', '#0000FF', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, waning immunity (green, blue, dark red, orange, purple)
w <- 9; h <- 8
lt <- 2 # line type

####### save figures ####################################################################
setwd(dirname(sys.frame(1)$ofile)) 
setwd("./figures")
epiD <- d %>% filter(epidemic == setEpicode)
code1 <- sprintf("_epi%s", setEpicode)
  
### group by parameter, fitting model as colors #########################################
# log percent deviation
param.plot <- ggplot(epiD, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
  scale_fill_manual(values=model.colors, name='') +
  scale_y_log10(breaks=ybreaks, labels=ylabels) + 
  stat_summary(aes(group=model_fit2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab) +
  xlab(text.xlab) +
  ggtitle('parameter') +
  facet_grid(~param_expr, labeller = label_parsed)
ggsave(sprintf("logPercDev_byParam%s.%s", code1, ext), param.plot, width=w, height=h)

# true deviation
param.plot2 <- ggplot(epiD, aes(x=model_fit2, y=perc_dev_true, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
  scale_fill_manual(values=model.colors, name='') +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  stat_summary(aes(group=model_fit2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=19) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank(), plot.title=element_text(hjust=0)) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle(figlab) +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~param_expr, labeller = label_parsed)
print(param.plot2)
ggsave(sprintf("percTrueDev_byParam%s.%s", code1, ext), param.plot2, width=w, height=h)

### group by simulating model, fitting model as colors #########################################
simdata.plot2 <- ggplot(epiD, aes(x=model_fit2, y=perc_dev_true, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
  scale_fill_manual(values=model.colors, name='') +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  stat_summary(aes(group=model_data2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle('data simulation model') +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~model_data2)
ggsave(sprintf("percTrueDev_byModeldata%s.%s", code1, ext), simdata.plot2, width=w, height=h)

### group by noise, fitting model as colors #########################################
noise.plot2 <- ggplot(epiD, aes(x=model_fit2, y=perc_dev_true, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  scale_fill_manual(values=model.colors, name='') +
  stat_summary(aes(group=noisecode2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle('noise added to simulated data') +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~noisecode2)
ggsave(sprintf("percTrueDev_byNoise%s.%s", code1, ext), noise.plot2, width=w, height=h)
