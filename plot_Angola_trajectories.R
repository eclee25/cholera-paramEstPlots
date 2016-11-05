
## Name: Elizabeth Lee
## Date: 8/22/16
## Function: plot Angola fits in ggplot; two panels: naive and informed starting parameters
## Filenames: fitsAngola_trajectories/ (AngolaFits_informed_...fitplots.csv, AngolaFits_naive_...fitplots.csv)
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
require(dplyr)
require(tidyr)
require(readr)

#### set these ################################
ext <- "pdf"

#### plot formatting ################################
w <- 9; h <- 5; dp <- 300
sz <- 1.25; szP <- 1.75; textsz <- 14
lty <- 1; sh <- 16; apha <- 0.6
model.colors <- c('#00FF00', '#0000FF', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, Progressive (green, blue, dark red, orange, purple)
model.order <- c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Progressive")
starting.order <- c("naive", "informed")
starting.labels <- c("`Naive' starting parameters", "`Informed' starting parameters")
label.tru <- "Angola data"
ylabel.big <- "simulation model"
ylabel.sm <- "Infection Incidence"
xlabel.sm <- "Time (days)"

#### import data ################################
setwd("./JTB_submission2_data/fitsAngola_trajectories")
fnames <- list.files()

fileDat <- data.frame()
for (i in 1:length(fnames)){
 dummy <- read_csv(fnames[i], col_types = "ccccid") 
 fileDat <- bind_rows(fileDat, dummy)
}

#### clean data ################################
fitD <- fileDat %>%
  filter(fitting_model != "True Data") %>% 
  mutate(fitting_model = ifelse(fitting_model == "Waning", "Progressive", fitting_model)) %>%
  rename(fitted.val = data) 
trueD <- fileDat %>%
  filter(fitting_model == "True Data") %>%
  rename(true.val = data) %>%
  select(-fitting_model)
fullD <- full_join(fitD, trueD, by = c("generating_model", "noise", "starting", "t")) %>%
  mutate(fitting_model = factor(fitting_model, levels = model.order)) %>%
  mutate(starting = factor(starting, levels = starting.order, labels = starting.labels))

#### plot preparation ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./figures")

#### plotting ################################
pltFunc <- function(dataset){
  dummyP <- ggplot(dataset, aes(x = t, y = fitted.val)) +
    scale_size_discrete(name = label.tru, labels = c(""), guide = "legend") +
    scale_shape_identity() +
    geom_line(aes(colour = fitting_model), linetype = lty, size = sz, alpha = apha) +
    geom_point(aes(y = true.val, size = as.factor(szP), shape = sh), color = 'black') +
    scale_colour_manual(name = "fitting model", breaks = levels(fullD$fitting_model), values = model.colors) +
    scale_y_continuous(name = ylabel.sm) +
    xlab(xlabel.sm) +
    theme_bw(base_size = textsz, base_family = "") +
    theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), panel.background = element_blank()) +
    guides(colour = guide_legend(order = 1), size = guide_legend(order = 2)) +
    facet_grid(~starting) + 
    expand_limits(x = 0, y = 0)
  return(dummyP)
}

fit.P <- pltFunc(fullD)
ggsave(sprintf("Angola_fits.%s", ext), fit.P, width = w, height = h, dpi = dp)
# exported 8/25/16







