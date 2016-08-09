
## Name: Elizabeth Lee
## Date: 11/22/15
## Function: plot 100 day fits in ggplot; rows represent fitting model, columns represent noise type, separate plots for informed and naive starting parameters
## Filenames: trajectory_fits_100d.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(ggplot2)
require(tidyr)
require(dplyr)
require(readr)

#### set these ################################
ext <- "pdf"

#### plot formatting ################################
w <- 8; h <- 9; dp <- 300
sz <- 1.25; szP <- 1.75; textsz <- 14
lty <- 1; sh <- 16; apha <- 0.6
model.colors <- c('#00FF00', '#0000FF', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, waning immunity (green, blue, dark red, orange, purple)
model.order <- c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Waning Immunity")
noise.order <- c("none", "normal", "poisson")
starting.order <- c("informed", "naive")
label.tru <- "true data"
ylabel.big <- "simulation model"
ylabel.sm <- "Infection Incidence"
xlabel.big <- "added noise"
xlabel.sm <- "Time (days)"

#### import data ################################
setwd("./JTB_submission2_data")
imported <- read_csv("trajectory_fits_100d.csv", col_types = "ccccid") 

#### clean data ################################
fitD <- imported %>%
  filter(fitting_model != "True Data") %>% 
  rename(fitted.val = data) 
trueD <- imported %>%
  filter(fitting_model == "True Data") %>%
  rename(true.val = data) %>%
  select(-fitting_model)
fullD <- full_join(fitD, trueD, by = c("generating_model", "noise", "starting", "t")) %>%
  mutate(fitting_model = factor(fitting_model, levels = model.order)) %>%
  mutate(generating_model = factor(generating_model, levels = model.order)) %>%
  mutate(noise = factor(noise, levels = noise.order)) %>%
  mutate(starting = factor(starting, levels = starting.order))

#### plot preparation ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./figures")

#### plotting ################################
pltFunc <- function(dataset){
  dummyP <- ggplot(dataset, aes(x = t, y = fitted.val)) +
    geom_point(aes(y = true.val, size = as.factor(szP), shape = sh), color = 'black') +
    scale_size_discrete(name = label.tru, labels = c(""), guide = "legend") +
    scale_shape_identity() +
    geom_line(aes(colour = fitting_model), linetype = lty, size = sz, alpha = apha) +
    scale_colour_manual(name = "fitting model", breaks = levels(fullD$fitting_model), values = model.colors) +
    geom_hline(aes(yintercept = 2500), alpha = 0) +
    scale_y_continuous(name = ylabel.sm) +
    xlab(xlabel.sm) +
    theme_bw(base_size = textsz, base_family = "") +
    theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), panel.background = element_blank()) +
    guides(colour = guide_legend(order = 1), size = guide_legend(order = 2)) +
    facet_grid(generating_model ~ noise, scales = "free_y") + 
    expand_limits(x = 0, y = 0)
  return(dummyP)
}

for (starts in starting.order) {
  fitP.full <- pltFunc(fullD %>% filter(starting == starts))
  fitP.Ns <- pltFunc(fullD %>% filter(starting == starts & noise != "poisson"))
  ggsave(sprintf("allFits_100d_%sStarting.%s", starts, ext), fitP.full, width = w, height = h, dpi = dp)
#   ggsave(sprintf("noneNormFits_100d_%sStarting.%s", starts, ext), fitP.Ns, width = w, height = h, dpi = dp)
} 
# exported 8/8/16







