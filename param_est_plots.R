
## Name: Elizabeth Lee
## Date: 12/28/13 
## Function: Create boxplots for NimBIOS Cholera project parameter estimates -- by parameter, model, type of data -- as a function of percent deviation from the true parameters
## Filenames: param_est_alldata.csv
## Data Source: 
## Notes: 
## 8/30/15: check that data matches tables 5-10 in paper draft, recheck figures, calculate mean and coefficient of variation, streamline code structure
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

systype <- Sys.info()[['sysname']]
if (systype=="Windows"){
  wdimport <- 'C:/Users/Elizabeth/Dropbox (Bansal Lab)/NIMBioS Project Cholera/param_est_plots'
  wdexport <- 'C:/Users/Elizabeth/Dropbox (Bansal Lab)/NIMBioS Project Cholera/paper draft/figures/paramestplots'
  wdsource <- 'C:/Users/Elizabeth/Dropbox (Bansal Lab)/code'
}else if(systype=="Linux"){
  wdimport <- '/home/elee/Dropbox/NIMBioS Project Cholera/param_est_plots'
  wdexport <- '/home/elee/Dropbox/NIMBioS Project Cholera/paper draft/figures/paramestplots'
  wdsource <- '/home/elee/Dropbox/code'
}

setwd(wdsource)
source('dfsumm.R')

require(plotrix)
require(dplyr)
require(ggplot2)

##########################################

setwd(wdimport)
d <- read.csv('param_est_alldata.csv', colClasses = 'character', na.strings = 'NA', header = TRUE)
d.Ang <- read.csv('param_est_Angola.csv', header=T)

# folder for saving plots
setwd(wdexport)

##########################################
## data cleaning

### not needed because R recognizes -Inf as a value
# # replace "-Inf" values with -999999999
# d$est_cl <- gsub('-Inf', '-999999999', d$estimate)

# 5/22/14 fix all corrupted characters
names(d) <- c('model_data', 'pois_data', 'norm_data', 'model_fit', 'epidemic', 'informed', 'parameter', 'actual', 'estimate', 'uqid')
d$parameter <- gsub('.AF8-', '_', d$parameter) # . matches 1 character
d$estimate <- gsub('.AC0', '', d$estimate)
d$uqid <- gsub('.AF8-', '_', d$uqid) # added 9/11/14

# convert all but parameter and uqid to numeric
d$model_data <- as.numeric(d$model_data)
d$pois_data <- as.numeric(d$pois_data)
d$norm_data <- as.numeric(d$norm_data)
d$model_fit <- as.numeric(d$model_fit)
d$epidemic <- as.numeric(d$epidemic)
d$informed <- as.numeric(d$informed)
d$actual <- as.numeric(d$actual)

# 2/18/14: noticed that some of the AIC values in the "estimate" column for M1 and M3 fits were "#NAME?" Some value must have gotten corrupted in Excel; not fixing this right now because we are not plotting AICs using this dataset right now
d$estimate <- gsub('#NAME?', NA, d$estimate) # added this line so no warning for NA coercion appears
d$estimate <- as.numeric(d$estimate)

# calculate percent deviation from actual value for each parameter, where perc dev = abs(actual - estimate)/actual * 100
d$perc_dev <- abs(d$actual - d$estimate)/d$actual * 100
# 2/18/14 log(0) is -Inf, which will not be plotted
# replace -Inf values with -99 for plotting purposes
d$log_perc_dev <- ifelse(d$perc_dev == 0, -99, log(d$perc_dev)) # natural log
# okay to calculate plot medians where "-Inf" is replaced by "-99" because -99 is the minimum anyways 9/11/14


# create numeric codes for each parameter
# 1 = beta_i, 2 = beta_w, 3 = alpha, 4 = xi, 5 = k, 6 = aic
d$param_code <- 0
d[d$parameter == 'beta_i',]$param_code <- 1
d[d$parameter == 'beta_w',]$param_code <- 2
d[d$parameter == 'alpha',]$param_code <- 3
d[d$parameter == 'xi',]$param_code <- 4
d[d$parameter == 'k',]$param_code <- 5
d[d$parameter == 'aic',]$param_code <- 6

# remove aic values from dataset used for estimate plots because it is not an estimate
# 8/30/15: remove seasonal estiamtes since we are not longer including that analysis in the paper
d2 <- tbl_df(d) %>% filter(epidemic==1 & parameter != 'aic')

# create new code for non/pois/norm data
d2$noise <- paste(d2$pois_data, d2$norm_data, sep = '')
d2$noisecode <- 0
d2$noisecode[which(d2$noise == '00')] <- 1 # non-noisy
d2$noisecode[which(d2$noise == '10')] <- 2 # poisson
d2$noisecode[which(d2$noise == '01')] <- 3 # normal

##########################################
# 8/30/15: check data with tables in ms draft
d2.check5a <- tbl_df(d2) %>% filter(model_data==1 & informed==1 & epidemic==1 & pois_data+norm_data==0)
d2.check5b <- tbl_df(d2) %>% filter(model_data==4 & informed==1 & epidemic==1 & pois_data+norm_data==0)
# there are some minor discrepancies, but it is not yet clear which is correct
###########################################################################
## data processing: model_fit should be labeled with words ##
# vs. fitting model
d3 <- d2 %>% mutate(model_fit2 = as.factor(ifelse(model_fit==1, 'Exponential', ifelse(model_fit==2, 'Gamma', ifelse(model_fit==3, 'Asymptomatic', ifelse(model_fit==4, 'Dose Response', ifelse(model_fit==5, 'Waning Immunity', NA))))))) %>% mutate(model_fit2 = factor(model_fit2, levels(model_fit2)[c(3, 2, 1, 4, 5)])) %>% mutate(parameter = factor(parameter, c("beta_i", "beta_w", "alpha", "xi", "k"))) 
# vs. data simulation model
d4 <- d3 %>% mutate(model_data2 = as.factor(ifelse(model_data==1, 'Exponential', ifelse(model_data==2, 'Gamma', ifelse(model_data==3, 'Asymptomatic', ifelse(model_data==4, 'Dose Response', ifelse(model_data==5, 'Waning Immunity', NA))))))) %>% mutate(model_data2 = factor(model_data2, levels(model_data2)[c(3, 2, 1, 4, 5)]))
# vs. noise
d5 <- d4 %>% mutate(noisecode2 = as.factor(ifelse(noisecode==1, 'None', ifelse(noisecode==2, "Poisson", ifelse(noisecode==3, "Normal", NA))))) %>% mutate(noisecode2 = factor(noisecode2, levels(noisecode2)[c(1, 3, 2)]))

##########################################
# 8/30/15: generate summaries for simulated data
#1) Compare summaries across parameters
param.summ <- d2 %>% group_by(parameter) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3), mn.percDev = signif(mean(perc_dev), 3), sd.percDev = signif(sd(perc_dev), 3))
fitmodel.summ <- d2 %>% group_by(model_fit) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3), mn.percDev = signif(mean(perc_dev), 3), sd.percDev = signif(sd(perc_dev), 3)) %>% mutate(modelname = c('exponential', 'gamma', 'asymptomatic', 'dose response', 'waning'))
param_fitmodel.summ <- d2 %>% group_by(parameter, model_fit) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3), mn.percDev = signif(mean(perc_dev), 3), sd.percDev = signif(sd(perc_dev), 3)) %>% mutate(modelname = c('Exponential', 'Gamma', 'Asymptomatic', 'Dose Response', 'Waning'))

# 9/7/15
# setwd(wdexport)
# write.csv(param.summ, 'estimate_summary_by_parameters.csv', row.names=F)
# write.csv(fitmodel.summ, 'estimate_summary_by_fittingmodel.csv', row.names=F)
# write.csv(param_fitmodel.summ, 'estimate_summary_by_parameters_and_fittingmodel.csv', row.names=F)

##########################################
# 9/8/15: generate summaries for Angola data
param.summ.Ang <- d.Ang %>% filter(parameter != "AIC") %>% group_by(parameter) %>% summarise(mn.est = signif(mean(estimate), 3), sd.est = signif(sd(estimate), 3), percCV.est = signif(sd.est/mn.est*100, 3))

# 9/8/15
# setwd(wdexport)
# write.csv(param.summ.Ang, 'estimate_summary_Angola_by_parameters.csv', row.names=F)

##########################################
### plotting params ###
ylabels <- paste0(c(0.001, 0.01, 0.1, 1, 10, 100, 1000), "%")
ybreaks <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
text.ylab <- "deviation from true parameter"
text.xlab <- "fitting model"
model.colors <- c('#00FF00', '#0000FF', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, waning immunity (green, blue, dark red, orange, purple)
w <- 9; h <- 8
setwd(wdexport)

###########################################################################
### group by parameter, fitting model as colors ###
# # version 1
# ggplot(d3 %>% group_by(model_fit2), aes(x=parameter, y=perc_dev)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge") +
#   geom_point(aes(y=median(perc_dev)), colour="black")  +
#   ylab(text.ylab) +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   scale_fill_brewer("fitting model", palette = "Set1") +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black')) +
#   guides(fill = guide_legend(override.aes = list(colour = NA)))
# # version 2
# param.plot <- ggplot(d3, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
#   geom_violin(aes(fill=model_fit2), position="dodge") +
#   scale_fill_brewer("", palette = "Set1") +
#   scale_y_log10(breaks=ybreaks, labels=ylabels) + 
#   stat_summary(aes(group=model_fit2), fun.y=median, geom="point", colour = 'black', size=3) +
#   theme_bw(base_size=18) +
#   theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
#   guides(fill = guide_legend(override.aes = list(colour = NA))) + 
#   ylab(text.ylab) +
#   xlab(text.xlab) +
#   ggtitle('parameter') +
#   facet_grid(~parameter)

param.plot <- ggplot(d3, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=FALSE) +
  scale_fill_manual(values=model.colors, name='') +
  scale_y_log10(breaks=ybreaks, labels=ylabels) + 
  stat_summary(aes(group=model_fit2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab) +
  xlab(text.xlab) +
  ggtitle('parameter') +
  facet_grid(~parameter)
ggsave("logest_by_param_color_modelfit_bk.pdf", param.plot, width=w, height=h)

###########################################################################
### group by simulating model, fitting model as colors ###
simdata.plot <- ggplot(d4, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=FALSE) +
  scale_fill_manual(values=model.colors, name='') +
  scale_y_log10(breaks=ybreaks, labels=ylabels) + 
  stat_summary(aes(group=model_data2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab) +
  xlab(text.xlab) +
  ggtitle('data simulation model') +
  facet_grid(~model_data2)
ggsave("logest_by_modeldata_color_modelfit_bk.pdf", simdata.plot, width=w, height=h)

###########################################################################
### group by noise, fitting model as colors ###
noise.plot <- ggplot(d5, aes(x=model_fit2, y=perc_dev, group=model_fit2)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=FALSE) +
  scale_fill_manual(values=model.colors, name='') +
  scale_y_log10(breaks=ybreaks, labels=ylabels) + 
  stat_summary(aes(group=noisecode2), fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab) +
  xlab(text.xlab) +
  ggtitle('noise added to simulated data') +
  facet_grid(~noisecode2)
ggsave("logest_by_noise_color_modelfit_bk.pdf", noise.plot, width=w, height=h)

###########################################################################
### group by fitting model (no groups), fitting model as colors ### (not in supp)
fitdata.plot <- ggplot(d5, aes(x=model_fit2, y=perc_dev)) + 
  geom_violin(aes(fill=model_fit2), position="dodge", trim=FALSE) +
  scale_fill_manual(values=model.colors, name='') +
  scale_y_log10(breaks=ybreaks, labels=ylabels) + 
  stat_summary(fun.y=median, geom="point", colour = 'black', size=3) +
  theme_bw(base_size=18) +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NA))) + 
  ylab(text.ylab) +
  xlab(text.xlab) +
  ggtitle('data fitting model')
ggsave("logest_by_modelfit_color_modelfit_bk.pdf", fitdata.plot, width=w, height=h)

# # ###########################################################################
# ### data indexing for plotting convenience ###
# iM1 <- which(d2$model_fit == '1')
# iM2 <- which(d2$model_fit == '2')
# iM3 <- which(d2$model_fit == '3')
# iM4 <- which(d2$model_fit == '4')
# iM5 <- which(d2$model_fit == '5')
# 
# # legend text
# legtxt.model <- c('model used: Exp.', '... Dose Resp.', '... Asymp.', '... Gamma', '... Waning Imm.')
# legtxt.modelsim <- c('simulated from: M1', '... M2', '... M3', '... M4', '... M5')
# legtxt.param <- c('beta_i', 'beta_w', 'alpha', 'xi', 'k')
# legtxt.epi <- c('seasonal', 'epidemic')
# legtxt.noise = c('none', 'poisson', 'normal')
# # legend colors
# legcol.model <- c('yellow', 'blue', 'green', 'red', 'violet')
# legcol.epi <- c('black', 'blue')
# # percent dev (x-axis)
# xticmarks = c(-99, log(10^-4), log(10^-2), log(10^0), log(10^2), log(10^4))
# xticlabels = c('0', as.expression(bquote(10^.(-4))), as.expression(bquote(10^.(-2))), as.expression(bquote(10^.(0))), as.expression(bquote(10^.(2))), as.expression(bquote(10^.(4))))
# lab.model = c('exp', 'gamma', 'asym', 'dose resp', 'waning')
# ptsize = 1.4
# 
# ##### log estimates w/x-axis break ###### 3/31/14
# ##########################################
# ### group by model_data (log) ###
# 
# ## logest_by_modeldata_color_modelfit_bk median points ##
# median_pts <- ddply(d2, .(model_fit, model_data), summarise, med = median(log_perc_dev, na.rm=TRUE))
# # subtract model_data and adjust to obtain y-axis value
# median_pts$adjust <- 0
# median_pts[which(median_pts$model_fit == '1'),]$adjust <- -0.3
# median_pts[which(median_pts$model_fit == '2'),]$adjust <- -0.15
# median_pts[which(median_pts$model_fit == '4'),]$adjust <- 0.15
# median_pts[which(median_pts$model_fit == '5'),]$adjust <- 0.3
# median_pts$yvalue <- median_pts$model_data + median_pts$adjust
# # plot model_fit one by one to apply colors
# M1 <- median_pts[median_pts$model_fit == '1',]
# M2 <- median_pts[median_pts$model_fit == '2',]
# M3 <- median_pts[median_pts$model_fit == '3',]
# M4 <- median_pts[median_pts$model_fit == '4',]
# M5 <- median_pts[median_pts$model_fit == '5',]
# 
# pdf('logest_by_modeldata_color_modelfit_bk.pdf')
# gap.plot(d2$log_perc_dev[iM1], (d2$model_data[iM1])- 0.3, col = 'yellow', ylim = c(0, 6), xlab = '% deviation from true parameter value (natural log scale)', ylab = 'model that simulated data', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, ytics = 1:5, yticlab = lab.model)
# gap.plot(d2$log_perc_dev[iM4], (d2$model_data[iM4]) - 0.15, col = 'blue', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM3], (d2$model_data[iM3]), col = 'green', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM2], (d2$model_data[iM2]) + 0.15, col = 'red', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM5], (d2$model_data[iM5]) + 0.3, col = 'violet', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(M1$med, M1$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M2$med, M2$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M3$med, M3$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M4$med, M4$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M5$med, M5$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# legend(-97.5, 6.25, legtxt.model, col = legcol.model, pch = 1, cex = 1)
# dev.off()
# 
# ############################################################
# ## logest_by_noise_color_modelfit_bk median points ##
# median_pts <- ddply(d2, .(model_fit, noisecode), summarise, med = median(log_perc_dev, na.rm=TRUE))
# # subtract model_data and adjust to obtain y-axis value
# median_pts$adjust <- 0
# median_pts[which(median_pts$model_fit == '1'),]$adjust <- -0.3
# median_pts[which(median_pts$model_fit == '2'),]$adjust <- -0.15
# median_pts[which(median_pts$model_fit == '4'),]$adjust <- 0.15
# median_pts[which(median_pts$model_fit == '5'),]$adjust <- 0.3
# median_pts$yvalue <- median_pts$noisecode + median_pts$adjust
# # plot model_fit one by one to apply colors
# M1 <- median_pts[median_pts$model_fit == '1',]
# M2 <- median_pts[median_pts$model_fit == '2',]
# M3 <- median_pts[median_pts$model_fit == '3',]
# M4 <- median_pts[median_pts$model_fit == '4',]
# M5 <- median_pts[median_pts$model_fit == '5',]
# 
# pdf('logest_by_noise_color_modelfit_bk.pdf')
# gap.plot(d2$log_perc_dev[iM1], (d2$noisecode[iM1])- 0.3, col = 'yellow', ylim = c(0.5, 3.5), xlab = '% deviation from true parameter value (natural log scale)', ylab = 'noise added to data', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, ytics = 1:3, yticlab = legtxt.noise)
# gap.plot(d2$log_perc_dev[iM4], (d2$noisecode[iM4]) - 0.15, col = 'blue', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM3], (d2$noisecode[iM3]), col = 'green', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM2], (d2$noisecode[iM2]) + 0.15, col = 'red', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM5], (d2$noisecode[iM5]) + 0.3, col = 'violet', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(M1$med, M1$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M2$med, M2$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M3$med, M3$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M4$med, M4$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M5$med, M5$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# legend(-97.5, 3.62, legtxt.model, col = legcol.model, pch = 1, cex = 1)
# dev.off()
# 
# ############################################################
# ## logest_by_param_color_modelfit_bk median points ##
# median_pts <- ddply(d2, .(model_fit, param_code), summarise, med = median(log_perc_dev, na.rm=TRUE))
# # subtract model_data and adjust to obtain y-axis value
# median_pts$adjust <- 0
# median_pts[which(median_pts$model_fit == '1'),]$adjust <- -0.3
# median_pts[which(median_pts$model_fit == '2'),]$adjust <- -0.15
# median_pts[which(median_pts$model_fit == '4'),]$adjust <- 0.15
# median_pts[which(median_pts$model_fit == '5'),]$adjust <- 0.3
# median_pts$yvalue <- median_pts$param_code + median_pts$adjust
# # plot model_fit one by one to apply colors
# M1 <- median_pts[median_pts$model_fit == '1',]
# M2 <- median_pts[median_pts$model_fit == '2',]
# M3 <- median_pts[median_pts$model_fit == '3',]
# M4 <- median_pts[median_pts$model_fit == '4',]
# M5 <- median_pts[median_pts$model_fit == '5',]
# 
# pdf('logest_by_param_color_modelfit_bk.pdf')
# gap.plot(d2$log_perc_dev[iM1], (d2$param_code[iM1])- 0.3, col='yellow', ylim = c(0.5, 5.5), xlab = '% deviation from true parameter value (natural log scale)', ylab = 'parameter', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, ytics = 1:5, yticlab = legtxt.param)
# gap.plot(d2$log_perc_dev[iM4], (d2$param_code[iM4]) - 0.15, col = 'blue', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM3], (d2$param_code[iM3]), col = 'green', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM2], (d2$param_code[iM2]) + 0.15, col = 'red', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM5], (d2$param_code[iM5]) + 0.3, col = 'violet', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# legend(-97.5, 5.7, legtxt.model, col = legcol.model, pch = 1, cex = 1)
# gap.plot(M1$med, M1$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M2$med, M2$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M3$med, M3$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M4$med, M4$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M5$med, M5$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# dev.off()
# 
# ############################################################
# ## logest_by_epi_color_modelfit_bk median points ##
# median_pts <- ddply(d2, .(model_fit, epidemic), summarise, med = median(log_perc_dev, na.rm=TRUE))
# # subtract model_data and adjust to obtain y-axis value
# median_pts$adjust <- 0
# median_pts[which(median_pts$model_fit == '1'),]$adjust <- -0.3
# median_pts[which(median_pts$model_fit == '2'),]$adjust <- -0.15
# median_pts[which(median_pts$model_fit == '4'),]$adjust <- 0.15
# median_pts[which(median_pts$model_fit == '5'),]$adjust <- 0.3
# median_pts$yvalue <- median_pts$epidemic + median_pts$adjust
# # plot model_fit one by one to apply colors
# M1 <- median_pts[median_pts$model_fit == '1',]
# M2 <- median_pts[median_pts$model_fit == '2',]
# M3 <- median_pts[median_pts$model_fit == '3',]
# M4 <- median_pts[median_pts$model_fit == '4',]
# M5 <- median_pts[median_pts$model_fit == '5',]
# 
# pdf('logest_by_epi_color_modelfit_bk.pdf')
# gap.plot(d2$log_perc_dev[iM1], (d2$epidemic[iM1])- 0.3, col='yellow', ylim = c(-0.5, 1.5), xlab = '% deviation from true parameter value (natural log scale)', ylab = 'simulation length', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, ytics = 0:1, yticlab = legtxt.epi)
# gap.plot(d2$log_perc_dev[iM4], (d2$epidemic[iM4]) - 0.15, col = 'blue', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM3], (d2$epidemic[iM3]), col = 'green', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM2], (d2$epidemic[iM2]) + 0.15, col = 'red', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# gap.plot(d2$log_perc_dev[iM5], (d2$epidemic[iM5]) + 0.3, col = 'violet', gap = c(-98, -11), gap.axis='x', xtics = xticmarks, xticlab = xticlabels, add=TRUE)
# legend(-97.5, 1.58, legtxt.model, col = legcol.model, pch = 1, cex = 1)
# gap.plot(M1$med, M1$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M2$med, M2$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M3$med, M3$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M4$med, M4$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# gap.plot(M5$med, M5$yvalue, pch=19, col='black', bg='black', gap = c(-98, -11), gap.axis='x', ylim = c(0, 6), add=TRUE, cex=ptsize)
# dev.off()
# 
# 
# 
