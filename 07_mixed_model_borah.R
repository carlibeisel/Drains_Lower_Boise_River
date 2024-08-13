## ------------------ ##
# Mixed Effects Model for Drains #
## ------------------ ##

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date created: 01/11/23
# Date adapted: August 7, 2024

# Import packages:
#install.packages('brms')
library(brms)
#install.packages('bayesplot')
library(bayesplot)
library(tidyverse)

#install.packages('tidybayes')
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(ggrepel)
#install.packages('flexmix')
library(flexmix)
library(modelr)
library(loo)

# Import the data 

rf <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/mixed_model_input_0813.csv')
rf$lt <- log(rf$Sum_AF)

## MODEL FIT ####
mae <- function(model, data_compare){
  yhat <- posterior_predict(model)
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

mae_lt <- function(model, data_compare){
  yhat <- exp(posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

# Create a model to understand changes in drainage rates through time

# # Create priors for model with all variables ####
# priors <- c(
#    set_prior('normal(2,1)', class = 'Intercept'),
#    set_prior('normal(0,1)', class= 'sd'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
#    set_prior('normal(0,5)', class = 'b', coef = 'et'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_wy_prcp'), 
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_ubrb_prcp'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_pivot_prop'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_Carryover')
#  )
#  
# # # MODEL: ALL WITH GROUP LEVEL EFFECT FOR URBAN AREA ####
# rf.mix.new <- brm(Sum_AF ~ (1  + scale_class1_urban| Name) + scale_Carryover + scale_pivot_prop + scale_ubrb_prcp + scale_class1_urban + et + scale_wy_prcp + scale_irrig_temp + scale_DivFlow,
#                    data = rf,
#                    iter = 2000,
#                    family = 'lognormal',
#                    prior = priors,
#                    control = list(max_treedepth = 20,
#                                   adapt_delta = 0.999),
#                    cores = getOption('mc.cores', parallel::detectCores()))
#  loo1 <- loo(rf.mix.new, reloo = TRUE)
#  print("Model: No arma, all variables")
#  summary(rf.mix.new)
# # 
#  print('LOO')
#  loo1
# # 
#  print('MAE')
#  mae(rf.mix.new, rf$Sum_AF)
# # 
#  saveRDS(rf.mix.new, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/rf_mix.RDS')
#  saveRDS(loo1, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_noarma.RDS')
# 
# Create priors for mix + div_flows model ####
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_wy_prcp'), 
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_ubrb_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_pivot_prop',
  set_prior('normal(0,5)', class = 'b', coef = 'scale_Carryover')
)
# 
# # ## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS ####
# lt.div.auto.011123 <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_wy_prcp + scale_irrig_temp + scale_DivFlow + arma(gr = Name),
#                    data = rf,
#                    iter = 4000,
#                    family = 'normal',
#                    prior = priors,
#                    control = list(max_treedepth = 20,
#                                   adapt_delta = 0.999),
#                    cores = getOption('mc.cores', parallel::detectCores()))
# #Model convergence
# print('Model: ARMA and varying effects')
# summary(lt.div.auto.011123)
# 
# ## Model Fit
# print('MAE')
# mae_lt(lt.div.auto.011123, rf$Sum_AF)
# 
# print('LOO')
# loo2 <- loo(lt.div.auto.011123, reloo = TRUE)
# loo2
# 
# saveRDS(lt.div.auto.011123, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma.RDS')
# saveRDS(loo2, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_arma.RDS')

 ## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS NO GROUP, NO YEAR, order assumed ####

rf_arma_full <- brm(lt ~ (1 | Name) + scale_Carryover + scale_pivot_prop + scale_ubrb_prcp + scale_class1_urban + scale_et + scale_wy_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                    data = rf,
                    iter = 4000,
                    family = 'normal',
                    prior = priors,
                    control = list(max_treedepth = 20,
                                   adapt_delta = 0.999),
                    cores = getOption('mc.cores', parallel::detectCores()),
                    save_pars = save_pars(all = TRUE))
print('Model: Autoregressive, all fixed effects')
summary(rf_arma_full)

# print('LOO')
# loo3 <- loo(rf_arma_full, reloo = TRUE)
# loo3

print('MAE')
mae_lt(rf_arma_full, rf$Sum_AF)

saveRDS(rf_arma_full, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma_nogroup.Rdata')
# saveRDS(loo3, file = /Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_arma_nogroup.RDS')

# # ARMA model for urban and climate, no canals
# priors <- c(
#   set_prior('normal(2,1)', class = 'Intercept'),
#   set_prior('normal(0,1)', class= 'sd'),
#   set_prior('normal(0,5)', class = 'b', coef = 'et'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_wy_prcp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban')
# )
# 
# lt.nocanal <- brm(lt ~ (1 | Name + scale_class1_urban) + scale_class1_urban + et + scale_wy_prcp + scale_irrig_temp + arma( gr = Name),
#                          data = rf,
#                          iter = 4000,
#                          family = 'normal',
#                          prior = priors,
#                          control = list(max_treedepth = 20,
#                                         adapt_delta = 0.999),
#                          cores = getOption('mc.cores', parallel::detectCores()),
#                          save_pars = save_pars(all = TRUE))
# 
# print('Model: Autoregressive, no canals')
# summary(lt.nocanal)
# 
# loo4 <- loo(lt.nocanal, reloo = TRUE)
# print(loo4)
# 
# print('MAE')
# mae_lt(lt.nocanal, rf$Sum_AF)
# 
# saveRDS(lt.nocanal, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma_nocanal.Rdata')
# saveRDS(loo4, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_nocanal.RDS')
# 
# # Model for ARMA + climate
# priors <- c(
#   set_prior('normal(2,1)', class = 'Intercept'),
#   set_prior('normal(0,1)', class= 'sd'),
#   set_prior('normal(0,5)', class = 'b', coef = 'et'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_wy_prcp'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp')
# )
# 
# lt.clim <- brm(lt ~ (1 | Name) + et + scale_wy_prcp + scale_irrig_temp + arma( gr = Name),
#                   data = rf,
#                   iter = 4000,
#                   family = 'normal',
#                   prior = priors,
#                   control = list(max_treedepth = 20,
#                                  adapt_delta = 0.999),
#                   cores = getOption('mc.cores', parallel::detectCores()),
#                   save_pars = save_pars(all = TRUE))
# print('Climate model')
# summary(lt.clim)
# 
# print('LOO')
# loo5 <- loo(lt.clim, reloo = TRUE)
# loo5
# 
# print('MAE')
# mae_lt(lt.clim, rf$Sum_AF)
# 
# saveRDS(lt.clim, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma_clim.Rdata')
# saveRDS(loo5, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_clim.RDS')
# 
# #ARMA with urban 
# priors <- c(
#   set_prior('normal(2,1)', class = 'Intercept'),
#   set_prior('normal(0,1)', class= 'sd'),
#   set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban')
# )
# 
# lt.urb <- brm(lt ~ (1 | Name + scale_class1_urban) + scale_class1_urban + arma( gr = Name),
#                   data = rf,
#                   iter = 4000,
#                   family = 'normal',
#                   prior = priors,
#                   control = list(max_treedepth = 20,
#                                  adapt_delta = 0.999),
#                   cores = getOption('mc.cores', parallel::detectCores()),
#                   save_pars = save_pars(all = TRUE))
# 
# print('Urban model')
# summary(lt.urb)
# 
# print('Loo')
# loo6 <- loo(lt.urb, reloo = TRUE)
# loo6
# 
# print('MAE')
# mae_lt(lt.urb, rf$Sum_AF)
# 
# saveRDS(lt.urb, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma_urb.Rdata')
# saveRDS(loo6, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_urb.RDS')
