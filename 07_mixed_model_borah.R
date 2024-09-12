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
#install.packages("bayestestR")
library(bayestestR)
#install.packages("performance")
library(performance)

# Import the data 

rf <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/mixed_model_input_0912.csv')
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
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'), 
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_ubrb_prcp'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_pivot_prop'),
#    set_prior('normal(0,5)', class = 'b', coef = 'scale_Carryover')
#  )
#  
# # # MODEL: ALL WITH GROUP LEVEL EFFECT FOR URBAN AREA ####
# rf.mix.new <- brm(Sum_AF ~ (1  + scale_class1_urban| Name) + scale_Carryover + scale_pivot_prop + scale_ubrb_prcp + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow,
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
#  print('MAE')
#  mae(rf.mix.new, rf$Sum_AF)
# # 
#  saveRDS(rf.mix.new, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/rf_mix.RDS')

# Create priors for mix + div_flows model ####
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_ubrb_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_pivot_perc'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_Carryover'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_sw_wr'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_gw_wr')
)
# 
# # ## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS ####
# lt.div.auto.011123 <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + et + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma(gr = Name),
#                    data = rf,
#                    iter = 4000,
#                    family = 'normal',
#                    prior = priors,
#                    control = list(max_treedepth = 20,
#                                   adapt_delta = 0.999),
#                    cores = getOption('mc.cores', parallel::detectCores()))
# #Model convergence
# print('Model: ARMA and varying effects')
# summary(lt.div.auto)
# 
# ## Model Fit
# print('MAE')
# mae_lt(lt.div.auto, rf$Sum_AF)
# 
# print('LOO')
# loo2 <- loo(lt.div.auto, reloo = TRUE)
# loo2
# 
# saveRDS(lt.div.auto, file = ''/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma.RDS')
# saveRDS(loo2, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_arma.RDS')

 ## MODEL: AUTOREGRESSIVE MIX + DIV FLOWS NO GROUP, NO YEAR, order assumed ####

rf_arma_full <- brm(lt ~ (1 | Name) + scale_sw_wr + scale_gw_wr + scale_et + scale_Carryover + scale_pivot_perc + scale_ubrb_prcp + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
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

print('MAE')
mae_lt(rf_arma_full, rf$Sum_AF)

saveRDS(rf_arma_full, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/arma_nogroup_mod2.Rdata')

print("LOO")
loo <- loo(rf_arma_full, reloo = TRUE)
loo

saveRDS(loo, file = '/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_output/loo_arma_mod2.RDS')

# Rsquared
r2 <- r2_bayes(rf_arma_full)
print(r2)

