#---------------------#
#   Model Selection   #
#---------------------#


# By: Carli Beisel
# Sept 5, 2024

# Purpose: Using LOO to determine which model best define the system. 

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
#install.packages('flexmix')
library(flexmix)

### Import the data ###

rf <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/Drains_Lower_Boise_River/model_input/mixed_model_input_0822.csv')
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


## MODEL 1: original 
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_DivFlow')
)

rf_arma_mod1 <- brm(lt ~ (1 | Name) + scale_et + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                    data = rf,
                    iter = 4000,
                    family = 'normal',
                    prior = priors,
                    control = list(max_treedepth = 20,
                                   adapt_delta = 0.999),
                    cores = getOption('mc.cores', parallel::detectCores()),
                    save_pars = save_pars(all = TRUE))
print('Model: Autoregressive, all fixed effects')
summary(rf_arma_mod1)
# Compute LOO for the first model
loo_rf_arma_mod1 <- loo(rf_arma_mod1)
print(loo_rf_arma_mod1)

# MODEL 2: All Variables 

### Create priors for mix + div_flows model ####
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
rf_arma_mod2 <- brm(lt ~ (1 | Name) + scale_et + scale_gw_wr + scale_sw_wr + scale_Carryover + scale_pivot_perc + scale_ubrb_prcp + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                    data = rf,
                    iter = 4000,
                    family = 'normal',
                    prior = priors,
                    control = list(max_treedepth = 20,
                                   adapt_delta = 0.999),
                    cores = getOption('mc.cores', parallel::detectCores()),
                    save_pars = save_pars(all = TRUE))
print('Model: Autoregressive, all fixed effects')
summary(rf_arma_mod2)
# Rsquared
r2 <- r2_bayes(rf_arma_mod2)
print(r2)
loo_rf_arma_mod2 <- loo(rf_arma_mod2)
print(loo_rf_arma_mod2)

# MODEL 3: All Variables but WR 

### Create priors for mix + div_flows model ####
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
  set_prior('normal(0,5)', class = 'b', coef = 'scale_Carryover')
)
rf_arma_mod3 <- brm(lt ~ (1 | Name) + scale_et + scale_Carryover + scale_pivot_perc + scale_ubrb_prcp + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                    data = rf,
                    iter = 4000,
                    family = 'normal',
                    prior = priors,
                    control = list(max_treedepth = 20,
                                   adapt_delta = 0.999),
                    cores = getOption('mc.cores', parallel::detectCores()),
                    save_pars = save_pars(all = TRUE))
print('Model: Autoregressive, all fixed effects')
summary(rf_arma_mod3)
loo_rf_arma_mod3 <- loo(rf_arma_mod3)
print(loo_rf_arma_mod3)

# MODEL 4: All Variables - but ET

### Create priors for mix + div_flows model ####
priors <- c(
  set_prior('normal(2,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class= 'sd'),
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
rf_arma_mod4 <- brm(lt ~ (1 | Name) + scale_gw_wr + scale_sw_wr + scale_Carryover + scale_pivot_perc + scale_ubrb_prcp + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + scale_DivFlow + arma( gr = Name),
                    data = rf,
                    iter = 4000,
                    family = 'normal',
                    prior = priors,
                    control = list(max_treedepth = 20,
                                   adapt_delta = 0.999),
                    cores = getOption('mc.cores', parallel::detectCores()),
                    save_pars = save_pars(all = TRUE))
print('Model: Autoregressive, all fixed effects')
summary(rf_arma_mod4)
loo_rf_arma_mod4 <- loo(rf_arma_mod4)
print(loo_rf_arma_mod4)


# Compare models using LOO
loo_comparison <- loo_compare(loo_rf_arma_mod1, loo_rf_arma_mod2, loo_rf_arma_mod3, loo_rf_arma_mod4)
print(loo_comparison)
