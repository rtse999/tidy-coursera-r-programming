# ------------------------------------------------------------------------
# Coursera R-Programming: Week 2 Problems using Tidyverse
# Link: http://josiahparry.com/post/tidy-coursera-r-programming/
#
# Location: /Users/raymondtse/Dropbox/Analysis/Misc/tidy-coursera-r-programming.r
# First created: 22:25 - Sunday 15 April 2018
# Last modified: 22:25 - Sunday 15 April 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(tidyverse)



# ------------------------------------------------------------------------
# Problem 1
# ------------------------------------------------------------------------

pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the 
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}

# 10 random IDs in ID range
ids <- sample(1:332, 10)

# Identify all files within directory
files <- list.files("specdata", full.names = TRUE)

# Subset the data
files_filtered <- files[ids]

# View the files to verify
paste(ids, files_filtered)

# Read in the subset of data
specdata <- map_df(files_filtered, read_csv,
                   col_types = list(
                     col_date(),
                     col_double(),
                     col_double(),
                     col_integer()
                   ))
glimpse(specdata)

specdata %>% 
  select(-Date) %>% 
  summarise_if(is.double, mean, na.rm = TRUE) %>% 
  pull(nitrate)

pollutant_mean <- function(directory, pollutant, id = 1:332) {
  files <- list.files(directory, full.names = TRUE)
  files_filtered <- files[id]
  specdata <- map_df(files_filtered, read_csv,
                     col_types = list(
                       col_date(),
                       col_double(),
                       col_double(),
                       col_integer()
                     ))
  specdata %>% 
    select(-Date) %>% 
    summarise_if(is.double, mean, na.rm = TRUE) %>% 
    pull(pollutant) %>% 
    return()
}

pollutant_mean(directory = "specdata", pollutant = "sulfate", id = sample(1:332, 20))
pollutant_mean(directory = "specdata", pollutant = "nitrate", id = sample(1:332, 20))

# ------------------------------------------------------------------------
# Problem 2
# ------------------------------------------------------------------------
