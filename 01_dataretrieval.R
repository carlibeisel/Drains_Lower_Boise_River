## ------------------ ##
## RETRIEVE USGS DATA ##
## ------------------ ##

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date adapted: May 22, 2024
# Adapted from Bridget Bittmann (2023; github: bridgetmarie24)


install.packages('dataRetrieval')
library(dataRetrieval)

#Input gauge site numbera and the parameter ID for discharge
siteNumber <- "13213072"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '2016-11-30', '2022-06-28')
rawdailydata$DiversionName <- 'Sand Run Gulch'

# Export csv file
write.csv(file="/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/data_input/dataretrieval/sandrungulch.csv", rawdailydata)

#Input gauge site numbera and the parameter ID for discharge
siteNumber <- "132109867"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '2016-11-30', '2022-06-28')
rawdailydata$DiversionName <- 'East Hartley Gulch'

# Export csv file
write.csv(file="/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/data_input/dataretrieval/easthartley.csv", rawdailydata)

#Input gauge site number and the parameter ID for discharge
siteNumber <- "13212890"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '1971-01-01', '2022-06-28')
rawdailydata$DiversionName <- 'Dixie Drain'

# Export csv file
write.csv(file="/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/data_input/dataretrieval/dixiedrain.csv", rawdailydata)

#Input gauge site numbera and the parameter ID for discharge
siteNumber <- "13212549"
info <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Import raw daily data
rawdailydata <- readNWISdv(siteNumber, parameterCd, '2016-11-30', '2022-06-28')
rawdailydata$DiversionName <- 'Conway Gulch'

# Export csv file
write.csv(file="/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/data_input/dataretrieval/conway.csv", rawdailydata)

