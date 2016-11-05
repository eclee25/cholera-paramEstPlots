
## Name: Elizabeth Lee
## Date: 11/23/15
## Function: plot forecasts in ggplot with facets
## Filenames: all_forecast_data.csv
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

#### set these! ################################
ext <- "pdf"
starting <- "informed" 
# starting <- "naive"

#### plot formatting ################################
w <- 8; h <- 9; dp <- 300
sz <- 1.25; szP <- 0.5; textsz <- 14
lty <- 1; sh <- 20; apha <- 0.6
model.colors <- c('#00FF00', '#0000FF', '#800000', '#FF8C00', '#DA70D6') # exponential, dose response, asymptomatic, gamma, Progressive (green, blue, dark red, orange, purple)
model.order <- c("Exponential", "Dose Response", "Asymptomatic", "Gamma", "Progressive")
ylabel.sm <- "Infection Incidence"
xlabel.sm <- "Time (days)"
label.obs <- "observed noisy data"
label.tru <- "true epidemic trajectory"

#### functions ################################
pltFunc <- function(dataset){
  dummyP <- ggplot(dataset, aes(x = t, y = forecastData)) +
    scale_shape_identity() +
    geom_point(aes(y = obsTrueNoise, size = szP, shape = sh), color = 'black') +
    scale_size(label.obs, labels = c(""), guide = "legend") +
    geom_line(aes(colour = forecasting_model), linetype = lty, size = sz, alpha = apha) +
    scale_colour_manual(name = "forecasting model", values = model.colors) +
    geom_line(aes(y = TrueClean, alpha = 1), colour = 'black', size = sz*2/3, linetype = lty) +
    scale_alpha(label.tru, labels = c(""), guide = "legend") +
    scale_y_continuous(name = ylabel.sm, limits = c(0, 7100)) +
    xlab(xlabel.sm) +
    theme_bw(base_size = textsz, base_family = "") +
    theme(legend.position = "bottom", legend.key = element_rect(colour = 'black'), panel.background = element_blank()) +
    guides(colour = guide_legend(order = 1), size = guide_legend(order = 2), linetype = guide_legend(order = 2)) +
    facet_grid(generating_model ~ obsDays, scales = "free_y") + 
    expand_limits(x = 0, y = 0)
  return(dummyP)
}

#### import data ################################
if (starting == "naive"){
  setwd("./JTB_submission2_data")
  imported <- read_csv("trajectory_forecasts_100d.csv", col_types = "ccccicddd") %>%
    mutate(generating_model = factor(generating_model, levels = model.order)) %>%
    mutate(forecasting_model = factor(forecasting_model, levels = model.order)) %>%
    mutate(obsDays = paste(obsDays, "days"))
} else if (starting == "informed"){
  setwd("./JTB_submission1_data")
  imported <- read_csv("all_forecast_data.csv", col_types = "cccciddddd") %>% 
    mutate(generating_model = ifelse(generating_model == "Waning Immunity", "Progressive", generating_model)) %>%
    mutate(forecasting_model = ifelse(forecasting_model == "Waning Immunity", "Progressive", forecasting_model)) %>%
    mutate(generating_model = factor(generating_model, levels = model.order)) %>%
    mutate(forecasting_model = factor(forecasting_model, levels = model.order)) %>%
    mutate(obsDays = paste(as.character(obsDays), "days"))
}

#### export plots ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("./figures")

if (starting == "naive"){
nonoisePlot <- pltFunc(imported %>% filter(noise == "none"))
ggsave(sprintf("allForecasts_%sStarting_nonoise.%s", starting, ext), nonoisePlot, width = w, height = h, dpi = dp)
normalPlot <- pltFunc(imported %>% filter(noise == "normal"))
ggsave(sprintf("allForecasts_%sStarting_norm.%s", starting, ext), normalPlot, width = w, height = h, dpi = dp)
} else if (starting == "informed"){
  normalPlot <- pltFunc(imported)
  ggsave(sprintf("allForecasts_%sStarting_norm.%s", starting, ext), normalPlot, width = w, height = h, dpi = dp)
}

# saved 8/8/16





