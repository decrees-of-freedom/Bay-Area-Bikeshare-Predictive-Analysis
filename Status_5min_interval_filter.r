### ALY6040 FINAL PROJECT EDA ##
## FILTERING THE STATUS DATASET TO EVERY 5 MIN, RATHER THAN EVERY MIN##

library(dplyr)
library(stringr)

# Import dataset
getwd() #get the current working directory
setwd("C:/Users/molly/Documents/1-ACADEMIC/1-ROUX/ALY6040") 
getwd() #verify wd updated
status <- read.csv("status.csv") 

# Filter the data for every 5 minutes (either ends in 5 or 0)
statusfiltered <- status %>%
                    filter(str_detect(time, "0$") | str_detect(time, "5$"))

#Check
head(statusfiltered)

#Export file
write.csv(statusfiltered,"C:\\Users\\molly\\Documents\\1-ACADEMIC\\1-ROUX\\ALY6040\\statusfiltered.csv", row.names = TRUE )
