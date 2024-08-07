## ------------------ ##
#Creates figures from GLMM
## ------------------ ##

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date adapted: May 22, 2024

#Note to self: ADD UBRB Precip alone figure

## Figures for drain discharge models ##

## Load packages ##
library(brms)
library(ggplot2)
library(Matrix)
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(modelr)
library(dplyr)
library(ggpubr)

## Unscale function for predictor variables
unscale <- function(x, orig){
  unscaled <- (sd(orig)*2*x)+mean(orig)
  return(unscaled)
}  

## Import data ####
rf <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/mixed_model_input_0707.csv')  
arma_ng <- readRDS('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma_nogroup.Rdata')

# Check out model summary 

summary(arma_ng)

mae_lt <- function(model, data_compare){
  yhat <- exp(posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

mae_lt(arma_ng, rf$Sum_AF)

# Posterior predictive check ###

pp <- pp_check(arma_ng, ndraws = 20) +
  theme_bw() +
  ylab('Density') +
  xlab('log(Discharge)')

ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/ppcheck.png', 
       plot = pp,
       width = 5,
       height = 4)

## URBAN EFFECT ####
## Step 1: Create data to generate the predictions over a continuous range of values:

new = rf %>%
  data_grid(scale_class1_urban = seq_range(scale_class1_urban, n = 200),
            scale_et = mean(scale_et),
            scale_wy_prcp = mean(scale_wy_prcp), 
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_DivFlow = mean(scale_DivFlow),
            scale_ubrb_prcp = mean(scale_ubrb_prcp))
new$Name <- NA

## Step 2: generate predictions from model:

epreddraws <-  add_epred_draws(arma_ng, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

##Step 3: Plot the data

epreddraws$unscale.urban <- unscale(epreddraws$scale_class1_urban, rf$class1_urban)


urban <- ggplot(data=epreddraws,
                aes(x = unscale.urban, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Drain Discharge (Acre-ft/yr)") + xlab("Percent Urban") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma)
urban
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/urb_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')

change_urb <- epreddraws %>%
  select(.epred, unscale.urban) %>%
  group_by(unscale.urban) %>%
  summarize(med = median(exp(.epred)),
            avg = mean(exp(.epred))) %>%
  mutate(differ_use = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA , NA, NA, diff(avg, lag = 21)),
         differ_urb = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA , NA, NA, diff(unscale.urban, lag = 21)))

## ET EFFECT ####

simdata = rf %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_wy_prcp = mean(scale_wy_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_et = seq_range(scale_et, n=200),
            scale_DivFlow = mean(scale_DivFlow),
            scale_ubrb_prcp = mean(scale_ubrb_prcp))
simdata$Name <- NA
epreddraws <-  add_epred_draws(arma_ng, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale_et_in <- unscale(epreddraws$scale_et, rf$et) * 39.3701 #convert ET to inches

et <- ggplot(data=epreddraws, 
             aes(x = unscale_et_in, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Drain Discharge (Acre-ft/yr)") + xlab("Evapotranspiration (in)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(1000, 40000))
et
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/et_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')

change_et <- epreddraws %>%
  select(unscale_et_in, .epred) %>%
  group_by(unscale_et_in) %>%
  summarize(avg = mean(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                         diff(avg, lag = 30)),
         differ_et = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                       diff(unscale_et_in, lag = 30)))
mean(change_et$differ_pred, na.rm = T)

## TEMP EFFECT ####

simdata = rf %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_wy_prcp = mean(scale_wy_prcp),
            scale_irrig_temp = seq_range(scale_irrig_temp, n=200),
            scale_et = mean(scale_et),
            scale_DivFlow = mean(scale_DivFlow),
            scale_ubrb_prcp = mean(scale_ubrb_prcp))
simdata$Name <- NA

epreddraws <-  add_epred_draws(arma_ng, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.temp <- (unscale(epreddraws$scale_irrig_temp,
                                    rf$irrig_temp) * 9/5) +32


temp <- ggplot(data=epreddraws, 
               aes(x = unscale.temp, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Drain Discharge (Acre-ft/yr)") + xlab("Avg. Max. Irrig. Temp. (F)")  +
  theme_bw() +
  theme(text = element_text(size = 13)) + 
  scale_y_continuous(labels = scales::comma)+
  coord_cartesian(ylim = c(1000, 40000))
temp
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/tmp_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')

change_temp <- epreddraws%>%
  select(unscale.temp, .epred) %>%
  group_by(unscale.temp) %>%
  summarize(avg = mean(exp(.epred))) %>%
  mutate(diff_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA , NA, NA,
                       diff(avg, lag = 22)),
         diff_temp = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA , NA, NA,
                       diff(unscale.temp, lag = 22))) 

mean(change_temp$diff_pred, na.rm = T)
## PRECIP POSTERIOR MASS ####

posterior <-as.data.frame(arma_ng)

ggplot(posterior, aes(x = b_scale_wy_prcp,
                      fill = stat(x < 0))) +
  stat_halfeye() +
  scale_fill_manual(values=c( "grey50", "#20a198"))+
  geom_vline(aes(xintercept=0), 
             color="black", size=1, linetype="dashed")+
  ylab("Density") +
  xlab('Effect of Water Year Precipitation')+
  guides(fill="none") + 
  theme_bw() +
  theme(text = element_text(size = 18)) +
  geom_vline(xintercept = median(posterior$b_scale_wy_prcp), linetype = 'dotted')
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/prcp_postmass.jpg', 
       width = 4,
       height = 4,
       units = 'in')

length(which(posterior$b_scale_wy_prcp < 0))/nrow(posterior) 


## PRECIP EFFECT ####

simdata = rf %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_wy_prcp = seq_range(scale_wy_prcp, n=200),
            scale_irrig_temp = mean(scale_irrig_temp),
            et = mean(et),
            scale_DivFlow = mean(scale_DivFlow),
            scale_ubrb_prcp = mean(scale_ubrb_prcp))
simdata$Name <- NA

lt.auto_noyear <- brm(lt ~ (1 | Name) + scale_class1_urban + et + scale_wy_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                           data = rf,
                           iter = 4000,
                           family = 'normal',
                           prior = priors,
                           control = list(max_treedepth = 20,
                                          adapt_delta = 0.999),
                           cores = getOption('mc.cores', parallel::detectCores()),
                           save_pars = save_pars(all = TRUE))
summary(lt.auto_noyear)
save(lt.auto_noyear, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/lt_auto_noyear.Rdata')

epreddraws <-  add_epred_draws(lt.auto_noyear, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.precip <- (unscale(epreddraws$scale_wy_prcp, rf$wy_prcp)) * 0.03937

class(precip)

ggplot(data=epreddraws, 
       aes(x = unscale.precip, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Drain Discharge (Acre-ft/yr)") + xlab("Avg. Total Water Year Precip. (in)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/prcp_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')


## CANAL EFFECT ####

simdata = rf %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_wy_prcp = mean(scale_wy_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_et = mean(scale_et),
            scale_DivFlow = seq_range(scale_DivFlow, n=200),
            scale_ubrb_prcp = mean(scale_ubrb_prcp))
simdata$Name <- NA

epreddraws <-  add_epred_draws(arma_ng, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.canal <- (unscale(epreddraws$scale_DivFlow, rf$DivFlow))


canal <- ggplot(data=epreddraws, 
                aes(x = unscale.canal, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Drain Discharge (Acre-ft/yr)") + xlab("Canal Flow Inputs (AF)")  +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)
canal
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/canal_marg.svg', 
       width = 4,
       height = 4,
       units = 'in')

change_canal <- epreddraws%>%
  select(unscale.canal, .epred) %>%
  group_by(unscale.canal) %>%
  summarize(avg = mean(exp(.epred))) %>%
  mutate(diff_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, diff(avg, lag = 8)),
         diff_flow = c(NA, NA, NA, NA, NA, NA, NA, NA, diff(unscale.canal, lag = 8)))
mean(change_canal$diff_pred, na.rm = T)

## Creating figures with ARMA terms ####

new = rf %>%
  group_by(Name) %>%
  data_grid(scale_class1_urban = seq_range(scale_class1_urban, n = 200),
            et = mean(et),
            scale_wy_prcp = mean(scale_wy_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_DivFlow = mean(scale_DivFlow))
#lt.auto_noyear file from above
epreddraws <-  add_epred_draws(lt.auto_noyear, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale.urban <- epreddraws$scale_class1_urban*100

ggplot(data=epreddraws,
       aes(x = unscale.urban, y = exp(.epred)), fill = Name) +
  stat_lineribbon(aes(fill = Name),
                  .width = 0.5, size=1, alpha = 0.4) + 
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
  ylab("Drain Discharge (Acre-ft/yr)") + xlab("Percent Urban") +
  theme_bw() +
  theme(text = element_text(size = 18)) 
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/all_urb_marg.jpg', 
       width = 6,
       height = 4,
       units = 'in')

## MCMC plots ##
print(colnames(arma_ng))
# Climate
mcmc_plot(arma_ng,
          type = 'areas',
          variable = c('b_scale_et',
                       'b_scale_wy_prcp', 
                       'b_scale_irrig_temp',
                       'b_scale_ubrb_prcp'),
          prob = 0.95) +
  theme_bw() +
  vline_0() +
  scale_y_discrete(labels = c('Evapotranspiration',
                              'Water Year Precipitation',
                              'Temperature', 'UBRB Water Year Precip')) +
  xlab('Relative Effect Size (log)') +
  theme(text = element_text(size=15, family = 'Arial'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/postmass_climate.jpg', 
       width = 6,
       height = 4,
       units = 'in')

# All
color_scheme_set('darkgray')
mcmc_plot(arma_ng,
          type = 'areas',
          variable = c('b_scale_et',
                       'b_scale_wy_prcp',
                       'b_scale_irrig_temp',
                       'b_scale_class1_urban',
                       'b_scale_DivFlow',
                       'b_scale_ubrb_prcp'),
          prob = 0.95) +
  theme_bw() +
  vline_0() +
  scale_y_discrete(labels = c('Evapotranspiration',
                              'Water Year Precipitation',
                              'Temperature',
                              'Urban Percentage',
                              'Canal Flows',
                              'UBRB Water Year Precip')) +
  xlab('Relative Effect Size (log)') +
  theme(text = element_text(size=15, family = 'Arial'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/postmass_all.png', 
       width = 7,
       height = 6,
       units = 'in')

#Urban 

ggplot(posterior, aes(x = b_scale_class1_urban)) +
  stat_halfeye(p_limits = c(0.025, 0.975)) +
  geom_vline(aes(xintercept=0), 
             color="black", size=1, linetype="dashed")+
  ylab("Density") +
  xlab('Effect of Precipitation')+
  guides(fill="none") + 
  theme_bw() +
  theme(text = element_text(size = 18)) +
  geom_vline(xintercept = median(posterior$b_scale_wy_prcp), linetype = 'dotted')

mcmc_plot(arma_ng,
          type = 'areas',
          variable = c('b_scale_class1_urban'),
          prob = 0.95) +
  theme_bw() +
  vline_0() +
  scale_y_discrete(labels = c('Urban Proportion')) +
  xlab('Relative Effect Size (log)') +
  theme(text = element_text(size=15, family = 'Arial'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/postmass_urb.jpg', 
       width = 6,
       height = 4,
       units = 'in')

# Canals 
mcmc_plot(arma_ng,
          type = 'areas',
          variable = c('b_scale_DivFlow'),
          prob = 0.95) +
  theme_bw() +
  vline_0() +
  scale_y_discrete(labels = c('Canal Flows')) +
  xlab('Relative Effect Size (log)') +
  theme(text = element_text(size=15, family = 'Arial'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/postmass_canal.jpg', 
       width = 6,
       height = 4,
       units = 'in')

# ET and temp marg effects combined
et_temp <- ggarrange(et, temp, ncol=2, labels = c('A', 'B'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/et_temp_marg.svg', 
       plot = et_temp,
       width = 8,
       height = 4,
       units = 'in')

## Marginal effects in on plot

ggarrange(urban, et, temp,canal, ncol=3, nrow = 2, labels = c('A', 'B', 'C', 'D', 'E'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/Figures/combined_marg.jpg', 
       width = 8,
       height = 8,
       units = 'in')

