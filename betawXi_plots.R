
## Name: Elizabeth Lee
## Date: 1/21/16
## Function: Create scatterplots of betaW and Xi across all model fits to see if there are identifiability tradeoffs
## Filenames: param_est_alldata_100d_3y.csv
## Data Source: 
## Notes: 8/8/16 - update file paths for restructured directories
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

##########################################
setwd("./JTB_submission2_data")
d5 <- read_csv('param_est_100d_3y.csv', col_types = 'iiiiiicddcdddiciccccc') %>% 
  mutate(model_fit2 = factor(model_fit2, levels = c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Waning Immunity")))

#### plotting params #############################################
model.colors <- c('#00FF00', '#0000FF', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, waning immunity (green, blue, dark red, orange, purple)
model.colors2 <- c('#00FF00', '#800000', '#FF8C00', '#DA70D6') # exponential, asymptomatic, gamma, waning immunity (green, blue, dark red, orange, purple)

w <- 9; h <- 8
lt <- 2 # line type

####### plot beta_w vs xi estimates (100d & 3y) ######################################################
setwd("../figures")

# 10/16/15: Practical identifiability trade-offs between betaW and xi
identif_bw_xi <- d5 %>% filter(parameter == 'beta_w' | parameter == 'xi') %>% 
  select(modChoiceID, model_fit2, parameter, estimate) %>% 
  spread(parameter, estimate)

identif.plot <- ggplot(identif_bw_xi, aes(x = beta_w, y = xi)) +
  geom_point(aes(color = model_fit2), size = 3) + 
  scale_colour_manual(values=model.colors, name='fitting model') +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.key = element_rect(colour = 'black')) +
  ylab(expression(paste(xi, " estimate"))) +
  xlab(expression(paste(beta[W], " estimate"))) #+
print(identif.plot)
ggsave("betaW_xi_scatter.pdf", identif.plot, width=w, height=h)

identif.plot.zm <- ggplot(identif_bw_xi, aes(x = beta_w, y = xi)) +
  geom_point(aes(color = model_fit2), size = 3) + 
  scale_colour_manual(values=model.colors, name='fitting') +
  theme_bw(base_size = 18, base_family = "") +
  theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), panel.background = element_blank()) +
  ylab(expression(paste(xi, " estimate"))) +
  xlab(expression(paste(beta[W], " estimate"))) +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 0.1))
print(identif.plot.zm)
ggsave("betaW_xi_scatterZm.pdf", identif.plot.zm, width=w, height=h)

identif.plot.nonoise <- ggplot(identif_bw_xi %>% filter(substring(modChoiceID, 2, 3) == "00"), aes(x = beta_w, y = xi)) +
  geom_point(aes(color = model_fit2), size = 3) + 
  scale_colour_manual(values=model.colors, name='fitting model') +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.key = element_rect(colour = 'black')) +
  ylab(expression(paste(xi, " estimate"))) +
  xlab(expression(paste(beta[W], " estimate"))) #+
  # coord_cartesian(xlim = c(0, 5), ylim = c(0, 0.1))
print(identif.plot.nonoise)
ggsave("betaW_xi_scatter_nonoise.pdf", identif.plot.nonoise, width=w, height=h)

####### rm DR plots ######################################################
identif_bw_xi_rmDR <- d5 %>% filter(parameter == 'beta_w' | parameter == 'xi') %>% 
  filter(model_fit2 != "Dose Response") %>%
  filter(model_data2 != "Dose Response") %>%
  select(modChoiceID, model_fit2, parameter, estimate) %>% 
  spread(parameter, estimate)

identif.plot.rmDR <- ggplot(identif_bw_xi_rmDR, aes(x = beta_w, y = xi)) +
  geom_point(aes(color = model_fit2), size = 3) + 
  scale_colour_manual(values=model.colors2, name='fitting model') +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.key = element_rect(colour = 'black')) +
  ylab(expression(paste(xi, " estimate"))) +
  xlab(expression(paste(beta[W], " estimate"))) #+
print(identif.plot.rmDR)
ggsave("betaW_xi_scatter_rmDR.pdf", identif.plot.rmDR, width=w, height=h)

# 8/8/16
