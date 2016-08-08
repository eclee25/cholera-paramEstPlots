# Load required packages
library(gdata);require(XLConnect)

# Set working directory
setwd("E:/Dropbox/Projects/NIMBioS Project Cholera/matlab files/current/make plots/100daysfits")

# List all relevant files
data_files <- c('NoNoiseInformedFits100days.xlsx',
                'NoNoiseNaiveFits100days.xlsx',
                'NormNoiseInformedFits100days.xlsx',
                'NormNoiseNaiveFits100days.xlsx',
                'PoissonNoiseInformedFits100days.xlsx',
                'PoissonNoiseNaiveFits100days.xlsx')

# initialize the final data set
final <- data.frame(generating_model = NULL,
                    fitting_model    = NULL,
                    noise            = NULL,
                    starting         = NULL,
                    t                = NULL,
                    data             = NULL)

# Loop over all files
for(j in 1:length(data_files)){
  
  # Load workbook
  wb <- loadWorkbook(data_files[j])
  
  # Load sheets
  tmp <- readWorksheet(wb, sheet = getSheets(wb))
  
  # Remove Asymptomatic A Model
  tmp <- tmp[-3]
  
  # Get noise type for this file
  noise <- strsplit(data_files[j],'Noise')[[1]][1]
  
  # Get starting parameter guess for this file
  starting <- tolower(strsplit(strsplit(data_files[j],'Noise')[[1]][2],'Fits')[[1]][1])
  
  # Loop over all sheets
  for(i in 1:length(tmp)){
  
    # Make a subset of temp that removes unnecessary rows & cols from the sheet
    sub <- as.data.frame(tmp[i])[-c(1:2),c(1:2,4:7)]
    
    # Make useful column names
    colnames(sub) <- c('Exponential','Gamma Chain','Asymptomatic','Dose Response','Waning Immunity','True Data')
    
    # Make an output dataframe in long format
    out <- reshape(sub, 
                   varying = colnames(sub), 
                   v.names = "data",
                   idvar   = 't',
                   timevar = "fitting_model", 
                   times = colnames(sub),
                   direction = "long")
    
    # Make t start at 0
    out$t <- out$t - 1
    
    # Make proper row entry for type of noise
    out$noise <- switch(noise,
                        No      = 'none',
                        Norm    = 'normal',
                        Poisson = 'poisson')
    
    # Make proper row entry for starting parameter guess
    out$starting         <- starting
    
    # Get generating model
    out$generating_model <- colnames(sub)[i]
    
    # Make column order match the output dataset
    out <- out[,c(6,1,4,5,3,2)]
    
    # Get rid of annoying row names
    rownames(out) <- NULL
    
    # append 'out' to 'final'
    final <- rbind(final,out)
  }
}

# make sure data classes are correct
final$data <- as.numeric(final$data)
final$t    <- as.integer(final$t)

# write data to file
write.csv(final, 'all_100d_data.csv', row.names=F)
