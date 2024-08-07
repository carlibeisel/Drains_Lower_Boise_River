## ------------------ ##
## DRAIN PROCESSING 
## ------------------ ##

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date adapted: May 22, 2024
# Adapted from Bridget Bittmann (2023; github: bridgetmarie24)


# This script standardizes predictor variables, add canal flow values for each 
# drain each year, and does Mann Kendall tests. It also checks for correlations 
# between variables. The output from here can go into the mixed_model_borah script.

## Pre-process data for modeling ##

## IMPORT PACKAGES ##
## --------------- ##

library(plyr)
library(dplyr)
library(Matrix)
library(tidyverse)
library(readr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)
#install.packages('magrittr')
library(magrittr)
library(dplyr)
#install.packages('paletteer')
library(paletteer)
#install.packages('ggpubr')
library(ggpubr)
#install.packages('Kendall')
library(Kendall)
source("http://peterhaschke.com/Code/multiplot.R") 

## INPUT THE DATA ##
## -------------- ##
data <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/model_input_0707.csv')
na_data <- data[is.na(data)] # Check for NA data in the file
data <- data[-c(1)] # Remove python index value column

## DATA DISTRIBUTIONS ## 
## ------------------ ##

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/acreft_hist.pdf',
    width=6,
    height=4)
ggplot(data=data)+
  aes(x=Sum_AF)+
  geom_histogram(color='black')+
  ylab('Count')+
  xlab('Annual Discharge (AF/yr)')+
  theme_bw()
dev.off()

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/SD_AF_hist.pdf',
    width=6,
    height=4)
ggplot(data=data)+
  aes(x=SD_AF)+
  geom_histogram(color='black')+
  ylab('Count')+
  xlab('Annual Discharge (AF/yr)')+
  theme_bw()
dev.off()

## URBAN CHANGE ##
## ------------ ##

#omitted from initial packages 
#make sure dbplyr is installed 
library(magrittr)
library(dplyr)

urb_change <- data %>%
  select(Name, class1_urban, Sum_AF) %>%
  group_by(Name) %>%
  summarize(change = max(class1_urban) - min(class1_urban),
            maximum = max(class1_urban),
            AF = mean(Sum_AF))

## DISCHARGE THROUGH TIME ## 
## ---------------------- ##

names <- unique(data$Name)

for (i in names){
  subdata <- subset(data, Name == i)
  model <- lm(Sum_AF ~ Year, data = subdata)   
  slope <- coef (model)[2]  
  r_squared <- summary(model)$r.squared  
  plot_title <- paste (i, "\nSlope:", round(slope,2), "R-squared:",round(r_squared, 2))   
 
   print(ggplot(data = subdata) + 
          aes(x = Year, y = Sum_AF) +
          geom_point() + 
          geom_smooth(method = 'lm') +
          theme_bw() +
          xlab('Year') + 
          ylab('Discharge (AF)') + 
          ggtitle(plot_title))   
}

for (i in names){
  subdata <- subset(data, Name == i)
  print(ggplot(data = subdata) + 
          aes(x = class1_urban, y = Sum_AF) +
          geom_point() + 
          geom_smooth(method = 'lm', color = '#00798c', fill = '#00798c') +
          theme_bw() +
          xlab('Urban Proportion') + 
          ylab('Discharge (AF)') + 
          ggtitle(i))
}

paletteer_d("colorBlindness::Blue2DarkOrange12Steps")

pal <- paletteer_d("colorBlindness::paletteMartin")

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/regress.pdf',
    width=8,
    height=5)
ggplot(data = data) +
  aes(x = class1_urban, y = Sum_AF, fill = Name, color=Name) +
  geom_point() + 
  scale_shape(solid = FALSE) +
  geom_smooth(method = 'lm', aes(color = Name, fill = Name)) +
  scale_fill_manual(values =c('#000000FF',
                              '#004949FF',
                              '#009292FF',
                              '#FF6DB6FF',
                              '#FFB6DBFF',
                              '#490092FF',
                              '#006DDBFF',
                              '#B66DFFFF',
                              '#6DB6FFFF',
                              '#B6DBFFFF',
                              '#920000FF',
                              '#924900FF',
                              '#DB6D00FF',
                              '#31A354',
                              '#FFAD65FF')) +
  scale_color_manual(values =c('#000000FF',
                               '#004949FF',
                               '#009292FF',
                               '#FF6DB6FF',
                               '#FFB6DBFF',
                               '#490092FF',
                               '#006DDBFF',
                               '#B66DFFFF',
                               '#6DB6FFFF',
                               '#B6DBFFFF',
                               '#920000FF',
                               '#924900FF',
                               '#DB6D00FF',
                               '#31A354',
                               '#FFAD65FF'))+
  theme_bw() +
  xlab('Urban Proportion') + 
  ylab('Discharge (AF)') +
  ylim(0, 120000)
dev.off()

ggplot(data = data) +
  aes(x = Year, y = Sum_AF, fill = Name, color=Name) +
  geom_point() + 
  scale_shape(solid = FALSE) +
  geom_smooth(method = 'lm', aes(color = Name, fill = Name)) +
  scale_fill_manual(values =c('#000000FF',
                              '#004949FF',
                              '#009292FF',
                              '#FF6DB6FF',
                              '#FFB6DBFF',
                              '#490092FF',
                              '#006DDBFF',
                              '#B66DFFFF',
                              '#6DB6FFFF',
                              '#B6DBFFFF',
                              '#920000FF',
                              '#924900FF',
                              '#DB6D00FF',
                              '#31A354',
                              '#FFAD65FF')) +
  scale_color_manual(values =c('#000000FF',
                               '#004949FF',
                               '#009292FF',
                               '#FF6DB6FF',
                               '#FFB6DBFF',
                               '#490092FF',
                               '#006DDBFF',
                               '#B66DFFFF',
                               '#6DB6FFFF',
                               '#B6DBFFFF',
                               '#920000FF',
                               '#924900FF',
                               '#DB6D00FF',
                               '#31A354',
                               '#FFAD65FF'))+
  theme_bw() +
  xlab('Year') + 
  ylab('Discharge (AF)') +
  coord_cartesian(ylim =c(0, 120000))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/AF_v_time.jpg', 
       width = 6,
       height = 4,
       units = 'in')

## Zoom in on the graph
sub_names <- c('Willow Creek',
               'West Hartley Gulch',
               'Sand Run Gulch',
               'East Hartley Gulch',
               'Conway Gulch',
               'North Middleton')
sub_names<- data.frame(sub_names)

displaydat <- filter(data, Name %in% sub_names$sub_names)

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/regress-zoom.pdf',
    width=5,
    height=3)
ggplot(data = displaydat) +
  aes(x = class1_urban, y = Sum_AF, fill = Name, color=Name) +
  geom_point() + 
  scale_shape(solid = FALSE) +
  geom_smooth(method = 'lm', aes(color = Name, fill = Name)) +
  scale_fill_manual(values = c('#000000FF',
                               '#FFB6DBFF',
                               '#B6DBFFFF',
                               '#920000FF',
                               '#31A354',
                               '#FFAD65FF')) +
  scale_color_manual(values = c('#000000FF',
                                '#FFB6DBFF',
                                '#B6DBFFFF',
                                '#920000FF',
                                "#31A354",
                                '#FFAD65FF')) +
  theme_bw() +
  xlab('Urban Proportion') + 
  ylab('Discharge (AF)') +
  ylim(0, 80000)
dev.off()

# Scale all the variables for model input
scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

col_name <- c('ant_prcp',
              'wy_prcp', 
              'irrig_temp', 
              'JuneAug_temp',
              'class1_urban',
              'class2_crops',
              'contagion',
              'largest_patch_index',
              'et')

for (i in col_name) {
  name <- colnames(data[i])
  new_col_name <- paste('scale_', name, sep = "")
  data[new_col_name] <- scale2sd(data[,i])
}

mean(data$scale_class1_urban)
sd(data$scale_class1_urban)
mean(data$scale_DivFlow)
sd(data$scale_DivFlow)
mean(data$scale_wy_prcp) #changed
sd(data$scale_wy_prcp) #changed
mean(data$scale_irrig_temp)
sd(data$scale_irrig_temp)
mean(data$scale_et)
sd(data$scale_et)


## Add canal discharge as a predictor variable and standardize ####

relates <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/drains/SpatialJoin_Drain.csv')
divflows <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/mixed_model_input_0707.csv') 
spatial_dict <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/drains/name_dictionary_spatial.csv')
spatial_dict_drain <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/drains/DrainRelates.csv')
spatial_dict_drain <- subset(spatial_dict_drain, select = -c(Dataset, SiteID))
spatial_dict_drain <- na.omit(spatial_dict_drain)
relates <- dplyr::left_join(relates, spatial_dict, by =('WaterRight' = 'WaterRight') )

years <- data.frame(divflows$Year, divflows$Acre_feet, divflows$Name)
years <- dplyr::left_join(years, relates, by = c('divflows.Name' = 'NewName'))
years <- subset(years, select = -c(WaterRight))
years <- na.omit(years)
years <- dplyr::left_join(years, spatial_dict_drain, by = c('Name' = 'Spatial.Name'))
sums <- years %>%
  select(divflows.Year, divflows.Acre_feet, NewName) %>%
  group_by(divflows.Year, NewName) %>%
  summarize(DivFlow = sum(divflows.Acre_feet))
data <- dplyr :: left_join(data, sums, by = c('Name' = 'NewName',
                                              'Year' = 'divflows.Year'))

data$scale_DivFlow <- scale2sd(data$DivFlow)

write.csv(data,'/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/mixed_model_input_0707.csv', row.names = FALSE)

## Check correlation between variables ##
## Don't want a correlation above 0.4 

col_names <- ('')


avgs <- data %>%
  select(Name, Sum_AF) %>%
  group_by(Name) %>%
  summarize(avg = mean(Sum_AF))


## Perform Mann Kendall Test for each drain
rf <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/model_input_0707.csv')

names <- data.frame(unique(rf$Name))

for (i in names){
  data <- subset(rf, Name == i)
  test<- MannKendall(data$Sum_AF)
  print(c(i, test$sl))
}

for (i in names){
  data <- subset(rf, Name == i)
  print(ggplot(data = data, aes(y= Sum_AF, x = Year)) +
          geom_point() +
          geom_smooth(method = 'lm') + 
          ggtitle(i) +
          theme_bw())
}

change_names <- c('Conway Gulch',
                  'Eagle Drain',
                  'Fifteen Mile Creek',
                  'Mason Drain',
                  'South Middleton',
                  'Thurman Drain')
nochange_names <- c("Dixie Drain",
                    "Drainage District No3",
                    "East Hartley Gulch",
                    "Indian Creek",
                    "Mason Creek",
                    "North Middleton",
                    "Sand Run Gulch",
                    "West Hartley Gulch",
                    "Willow Creek")

change_plots <- function(dataframe, name){
  plt <- ggplot(data = dataframe, aes(y= Sum_AF, x = Year)) +
    geom_point() +
    stat_smooth(method = 'lm', color = 'black') + 
    ggtitle(name) +
    ylab('Discharge (AF)') +
    theme_bw() +
    theme(text = element_text(size = 15)) +
    scale_y_continuous(labels = scales::comma)
  return(plt)
}
myplots <- list()
values <- list()
change <- list()
for (i in change_names){
  p <- change_plots(subset(rf, Name == i), i)
  vals <- ggplot_build(p)
  vals <- vals$data[[2]]
  values[[i]] <- vals
  print(c(i, 'Percent Decrease:', ((max(vals$y) - min(vals$y))/max(vals$y))))
  print(c(i, '1987 Value:', (max(vals$y)), max(vals$y)-vals$ymin[1]))
  print(c(i, '2020 Value:', (min(vals$y)), min(vals$y)-vals$ymin[80]))
  change[[i]] <- max(vals$y)-min(vals$y)
  myplots[[i]] <- p
}

change <- data.frame(change)
sum(change)
ggarrange(plotlist = myplots, ncol=3, nrow=2)
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/mk_fig.jpg', 
       width = 9,
       height = 5,
       units = 'in')

mynoplots <- list()
for (i in nochange_names){
  p <- change_plots(subset(rf, Name == i), i)
  mynoplots[[i]] <- p
}
ggarrange(plotlist = mynoplots, ncol = 3, nrow=3)
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/nosig_mk_fig.jpg', 
       width = 13,
       height = 10,
       units = 'in')

## All drain trend through time #### 

names <- unique(rf$Name)
for (i in names) {
  sub <- subset(rf, Name == i)
  print(c(i, length(sub$Sum_AF)))
}

# Sand run gulch does NOT have 34 years of data so do NOT include in basin analysis for Mann Kendrall

rf_drop <- rf[rf$Name != 'Sand Run Gulch',]

sums <- rf_drop %>%
  group_by(Year) %>%
  summarize(total = sum(Sum_AF))

MannKendall(sums$total)
sum_plot <- ggplot(data = sums, aes(x = Year, y = total)) +
  geom_point() +
  theme_bw() + 
  geom_smooth(method = 'lm') +
  ylab('Total Discharge (AF/yr)') +
  xlab('Year')
