
## Name: Elizabeth Lee
## Date: 10/1/15
## Function: Create violin plots for NimBIOS Cholera project parameter estimates -- by parameter, model, type of data -- as a function of percent deviation from the true parameters (cleaned up from v1)
## Remove
## Filenames: param_est_alldata.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
setwd(dirname(sys.frame(1)$ofile))


code <- '_rmDR'

require(tidyr)
require(dplyr)
require(ggplot2)
require(readr)

#### import data ######################################

d <- read_csv('param_est_alldata.csv', col_types = 'iiiiiicddc', na = 'NA')
d.Ang <- read.csv('param_est_Angola.csv', header=T)

# folder for saving plots
setwd("../paper draft/figures/paramestplots")

##########################################
## data cleaning

# 1) calculate percent deviation from actual value for each parameter, where perc dev = abs(actual - estimate)/actual * 100
d$perc_dev <- abs(d$actual - d$estimate)/d$actual * 100
# 10/1/15 log(0) is -Inf, which will not be plotted in ggplot with log transform

# 2) calculate true deviation (without absolute value) 
d$perc_dev_true <- (d$estimate - d$actual)/d$actual * 100

# 3) ratio of estimate to true value, center plot @ 100%, keep log scale
d$dev_ratio <- d$estimate/d$actual * 100

# create numeric codes for each parameter
# 1 = beta_i, 2 = beta_w, 3 = alpha, 4 = xi, 5 = k
d$param_code <- 0
d[d$parameter == 'beta_i',]$param_code <- 1
d[d$parameter == 'beta_w',]$param_code <- 2
d[d$parameter == 'alpha',]$param_code <- 3
d[d$parameter == 'xi',]$param_code <- 4
d[d$parameter == 'k',]$param_code <- 5

d2 <- tbl_df(d) 

# create new code for non/pois/norm data
d2$noise <- paste(d2$pois_data, d2$norm_data, sep = '')
d2$noisecode <- 0
d2$noisecode[which(d2$noise == '00')] <- 1 # non-noisy
d2$noisecode[which(d2$noise == '10')] <- 2 # poisson
d2$noisecode[which(d2$noise == '01')] <- 3 # normal


# there are some minor discrepancies, but it is not yet clear which is correct
#### data processing: model_fit should be labeled with words ####################################
# vs. fitting model
d3 <- d2 %>% mutate(model_fit2 = as.factor(ifelse(model_fit==1, 'Exponential', ifelse(model_fit==2, 'Gamma', ifelse(model_fit==3, 'Asymptomatic', ifelse(model_fit==4, 'Dose Response', ifelse(model_fit==5, 'Waning Immunity', NA))))))) %>% mutate(model_fit2 = factor(model_fit2, levels(model_fit2)[c(3, 2, 1, 4, 5)])) %>% mutate(parameter = factor(parameter, c("beta_i", "beta_w", "alpha", "xi", "k"))) 
# vs. data simulation model
d4 <- d3 %>% mutate(model_data2 = as.factor(ifelse(model_data==1, 'Exponential', ifelse(model_data==2, 'Gamma', ifelse(model_data==3, 'Asymptomatic', ifelse(model_data==4, 'Dose Response', ifelse(model_data==5, 'Waning Immunity', NA))))))) %>% mutate(model_data2 = factor(model_data2, levels(model_data2)[c(3, 2, 1, 4, 5)]))
# vs. noise
#### 11/4/15: remove dose response model ##########################################
d5 <- d4 %>% mutate(noisecode2 = as.factor(ifelse(noisecode==1, 'None', ifelse(noisecode==2, "Poisson", ifelse(noisecode==3, "Normal", NA))))) %>% mutate(noisecode2 = factor(noisecode2, levels(noisecode2)[c(1, 3, 2)])) %>% mutate(modChoiceID = paste0(model_data, pois_data, norm_data, model_fit, epidemic, informed)) %>% 
  filter(model_data != 4 & model_fit != 4)

#### 10/17/15: labeller expressions for parameters ##########################################
d6 <- d5  %>% 
  mutate(param_expr = ifelse(parameter == "beta_i", "beta[I]", ifelse(parameter == "beta_w", "beta[W]", as.character(parameter)))) %>% mutate(param_expr = factor(param_expr, levels = c("beta[I]", "beta[W]", "alpha", "xi", "k"))) 

#### 8/30/15: generate summaries for simulated data ##########################################
#1) Compare summaries across parameters
param.summ <- d2 %>% group_by(parameter) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3), mn.percDev = signif(mean(perc_dev), 3), sd.percDev = signif(sd(perc_dev), 3))
fitmodel.summ <- d2 %>% group_by(model_fit) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3), mn.percDev = signif(mean(perc_dev), 3), sd.percDev = signif(sd(perc_dev), 3)) %>% mutate(modelname = c('exponential', 'gamma', 'asymptomatic', 'dose response', 'waning'))
param_fitmodel.summ <- d2 %>% group_by(parameter, model_fit) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3), mn.percDev = signif(mean(perc_dev), 3), sd.percDev = signif(sd(perc_dev), 3)) %>% mutate(modelname = c('Exponential', 'Gamma', 'Asymptomatic', 'Dose Response', 'Waning'))

# 2) quantify parameter estimate deviations
beta_k_deviations <- d5 %>% filter(parameter == 'beta_i' | parameter == 'k') %>% group_by(parameter, model_fit) %>% summarise(mindev = min(perc_dev_true), maxdev = max(perc_dev_true), q10 = quantile(perc_dev_true, probs=.1), q90 = quantile(perc_dev_true, probs=.9))
beta_M3 <- d5 %>% filter(parameter == 'beta_i' & model_fit == 3)
k_M2 <- d5 %>% filter(parameter == 'k' & model_fit == 2)
k_M5 <- d5 %>% filter(parameter =='k' & model_fit == 5)

## 10/1/15 ##
# write.csv(param.summ, 'estimate_summary_by_parameters.csv', row.names=F)
# write.csv(fitmodel.summ, 'estimate_summary_by_fittingmodel.csv', row.names=F)
# write.csv(param_fitmodel.summ, 'estimate_summary_by_parameters_and_fittingmodel.csv', row.names=F)

#### 9/8/15: generate summaries for Angola data ##########################################

param.summ.Ang <- d.Ang %>% filter(parameter != "AIC") %>% group_by(parameter) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3))

## 10/1/15 ##
# write.csv(param.summ.Ang, 'estimate_summary_Angola_by_parameters.csv', row.names=F)


#### plotting params #############################################
ylabels <- paste0(c(0.001, 0.01, 0.1, 1, 10, 100, 1000), "%")
ybreaks <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
text.ylab <- "absolute value of deviation from true parameter"
text.ylab2 <- "percent deviation from true parameter"
text.ylab3 <- "percentage of estimate relative to true parameter"
text.xlab <- "fitting model"
model.colors <- c('#00FF00', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, waning immunity (green, blue, dark red, orange, purple)
w <- 9; h <- 8
lt <- 2 # line type

####### plot beta_w vs xi estimates ######################################################
# 10/16/15: Practical identifiability trade-offs between betaW and xi
identif_bw_xi <- d5 %>% filter(parameter == 'beta_w' | parameter == 'xi') %>% select(modChoiceID, model_fit2, parameter, estimate) %>% spread(parameter, estimate)

identif.plot <- ggplot(identif_bw_xi, aes(x = beta_w, y = xi)) +
  geom_point(aes(color = model_fit2), size = 3) + 
  scale_colour_manual(values=model.colors, name='fitting model') +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.key = element_rect(colour = 'black')) +
  ylab(expression(paste(xi, " estimate"))) +
  xlab(expression(paste(beta[W], " estimate"))) 
print(identif.plot)
ggsave(sprintf("betaW_xi_scatter%s.pdf", code), identif.plot, width=w, height=h)

identif.plot.zm <- ggplot(identif_bw_xi, aes(x = beta_w, y = xi)) +
  geom_point(aes(color = model_fit2), size = 3) + 
  scale_colour_manual(values=model.colors, name='fitting model') +
  theme_bw(base_size = 18, base_family = "") +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), panel.background = element_blank()) +
  ylab(expression(paste(xi, " estimate"))) +
  xlab(expression(paste(beta[W], " estimate"))) +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 0.1))
print(identif.plot.zm)
ggsave(sprintf("betaW_xi_scatterZm.pdf", code), identif.plot.zm, width=w, height=h)

###########################################################################
### group by parameter, fitting model as colors #########################################
# # log percent deviation
# param.plot <- ggplot(d5, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
#   scale_fill_manual(values=model.colors, name='') +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(aes(group=model_fit2), fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab) +
#   xlab(text.xlab) +
#   ggtitle('parameter') +
#   facet_grid(~parameter)
# ggsave("logPercDev_byParam_colModelfit.pdf", param.plot, width=w, height=h)

# true deviation
param.plot2 <- ggplot(d6, aes(x=model_fit2, y=perc_dev_true, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
  scale_fill_manual(values=model.colors, name='') +
  geom_hline(yintercept = c(-20, 20), colour = 'black', linetype = lt) +
  stat_summary(aes(group=model_fit2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab2) +
  xlab(text.xlab) +
  ggtitle('parameter') +
  coord_cartesian(ylim = c(-100, 300)) +
  facet_grid(~param_expr, labeller = label_parsed)
print(param.plot2)
ggsave(sprintf("percTrueDev_byParam_colModelfit%s.pdf", code), param.plot2, width=w, height=h)


# # ratio of estimate to actual
# param.plot3 <- ggplot(d5, aes(x=model_fit2, y=dev_ratio, group=model_fit2)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
#   scale_fill_manual(values=model.colors, name='') +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(aes(group=model_fit2), fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab3) +
#   xlab(text.xlab) +
#   ggtitle('parameter') +
#   coord_cartesian(ylim = c(1, 1000)) +
#   facet_grid(~parameter)
# ggsave("logDevRatio_byParam_colModelfit.pdf", param.plot3, width=w, height=h)


###########################################################################
### group by simulating model, fitting model as colors #########################################
# simdata.plot <- ggplot(d5, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
#   scale_fill_manual(values=model.colors, name='') +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(aes(group=model_data2), fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab) +
#   xlab(text.xlab) +
#   ggtitle('data simulation model') +
#   facet_grid(~model_data2)
# ggsave("logPercDev_byModeldata_colModelfit.pdf", simdata.plot, width=w, height=h)

simdata.plot2 <- ggplot(d6, aes(x=model_fit2, y=perc_dev_true, group=model_fit2)) + 
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
print(simdata.plot2)
ggsave(sprintf("percTrueDev_byModeldata_colModelfit%s.pdf", code), simdata.plot2, width=w, height=h)

# simdata.plot3 <- ggplot(d5, aes(x=model_fit2, y=dev_ratio, group=model_fit2)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
#   scale_fill_manual(values=model.colors, name='') +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(aes(group=model_data2), fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab3) +
#   xlab(text.xlab) +
#   ggtitle('data simulation model') +
#   coord_cartesian(ylim = c(1, 1000)) +
#   facet_grid(~model_data2)
# ggsave("logDevRatio_byModeldata_colModelfit.pdf", simdata.plot3, width=w, height=h)
# 

###########################################################################
### group by noise, fitting model as colors #########################################
# noise.plot <- ggplot(d5, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
#   scale_fill_manual(values=model.colors, name='') +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(aes(group=noisecode2), fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab) +
#   xlab(text.xlab) +
#   ggtitle('noise added to simulated data') +
#   facet_grid(~noisecode2)
# ggsave("logPercDev_byNoise_colModelfit.pdf", noise.plot, width=w, height=h)

noise.plot2 <- ggplot(d5, aes(x=model_fit2, y=perc_dev_true, group=model_fit2)) + 
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
print(noise.plot2)
ggsave(sprintf("percTrueDev_byNoise_colModelfit%s.pdf", code), noise.plot2, width=w, height=h)

# noise.plot3 <- ggplot(d5, aes(x=model_fit2, y=dev_ratio, group=model_fit2)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
#   scale_fill_manual(values=model.colors, name='') +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(aes(group=noisecode2), fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab3) +
#   xlab(text.xlab) +
#   ggtitle('noise added to simulated data') +
#   coord_cartesian(ylim = c(1, 1000)) +
#   facet_grid(~noisecode2)
# ggsave("logDevRatio_byNoise_colModelfit.pdf", noise.plot3, width=w, height=h)

###########################################################################
# ### group by fitting model (no groups), fitting model as colors ### (not in supp)
# fitdata.plot <- ggplot(d5, aes(x=model_fit2, y=perc_dev)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge", trim=TRUE) +
#   scale_fill_manual(values=model.colors, name='') +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab) +
#   xlab(text.xlab) +
#   ggtitle('data fitting model')
# ggsave("logPercDev_colModelfit.pdf", fitdata.plot, width=w, height=h)
# 
