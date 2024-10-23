# Author: Fabien Cottier 
# Date: October 2024

# Frontiers in Climate Replication File
# Title: 5_calibration_appendix.R
# Description: calibration of additional models presented in the appendix of the article


## ----initialization-----------------------------------------------------------

# working directory
# setwd("path/to/working/directory")

# libraries
library(readr)
library(rstan)
library(brms)
library(loo)
library(fastDummies)

# stan global parameters
n_cores = 4
n_chains = 4
n_iter = 4000

# memory setup
options(future.globals.maxSize= 1024 * 1024^2 )


## ---- load data-----------------------------------------------------------

# load data
dat <- readr::read_rds("data/calibration_data.Rds")
cepii_dat <- readr::read_csv("data/CEPII_Gravity_V202211_addVars.csv")
# Note: 
#   - CEPI data has been pre-filtered to year  == 2000. 
#   - Boundaries for Ethiopia corresponding to the period after the cold war (CEPII data)
#   - Boundaries for Sudan before the 2011 secession of South Sudan (CEPII data)

# add period dummies
dat <- fastDummies::dummy_cols(dat, select_columns = "year0", remove_first_dummy = T)
dat

# merge dat2 with dat and add common language, common colony & contiguity
dat <- dat |>
    dplyr::left_join(cepii_dat, by = c("orig" = "iso3_o", "dest" = "iso3_d"))


## ----set prior---------------------------------------------------------------

# prior on alpha and betas (M5 and M6)
palpha <- set_prior("student_t(3, 1.24, 6.51)", class = "Intercept")
palpha_nb <- set_prior("student_t(3, 0, 6.51)", class = "Intercept")

pbeta <- set_prior("student_t(3, 0, 57.1)", class = "b", coef = "cy_gepic_princeton_hist_all_idx") +
    set_prior("student_t(3, 0, 41.6)", class = "b", coef = "dis_watergap2_princeton_hist_idx") + 
    set_prior("student_t(3, 0, 6.75)", class = "b", coef = "logO_pwt_gdppc") +  
    set_prior("student_t(3, 0, 4.73)", class = "b", coef = "logpwt_gdppc_ratio") +  
    set_prior("student_t(3, 0, 1.78)", class = "b", coef = "logged_fatal_NP1") +
    set_prior("student_t(3, 0, 1.82)", class = "b", coef = "logmigr_stockP1") +
    set_prior("student_t(3, 0, 9.47)", class = "b", coef = "logdist") +
    set_prior("student_t(3, 0, 5.53)", class = "b", coef = "logO_pop") +
    set_prior("student_t(3, 0, 13.17)", class = "b", coef = "comlang_off") +
    set_prior("student_t(3, 0, 14.67)", class = "b", coef = "comcol") +
    set_prior("student_t(3, 0, 22.06)", class = "b", coef = "contig") +
    set_prior("student_t(3, 0, 14.97)", class = "b") # priors on yearly dummies
pbeta_nb <- set_prior("student_t(3, 0, 57.1)", class = "b", coef = "cy_gepic_princeton_hist_all_idx") +
    set_prior("student_t(3, 0, 41.6)", class = "b", coef = "dis_watergap2_princeton_hist_idx") + 
    set_prior("student_t(3, 0, 6.75)", class = "b", coef = "logO_pwt_gdppc") +  
    set_prior("student_t(3, 0, 4.73)", class = "b", coef = "logpwt_gdppc_ratio") +  
    set_prior("student_t(3, 0, 1.78)", class = "b", coef = "logged_fatal_NP1") +
    set_prior("student_t(3, 0, 1.82)", class = "b", coef = "logmigr_stockP1") +
    set_prior("student_t(3, 0, 9.47)", class = "b", coef = "logdist") +
    set_prior("student_t(3, 0, 5.53)", class = "b", coef = "logO_pop")
    set_prior("student_t(3, 0, 14.97)", class = "b") # priors on yearly dummies

# prior M5
prior_list <- pbeta + palpha 

# prior M6 (negative binomial)
prior_list_nb <- pbeta_nb + palpha_nb


## ----Table A.2, Model 1---------------------------------------------------------

m1SI_f <- formula(
    log(sd_rev_neg_subRef+1) ~ cy_gepic_princeton_hist_all_idx + dis_watergap2_princeton_hist_idx + log(O_pop) + log(pwt_gdppc_ratio) + log(O_pwt_gdppc) + log(ged_fatal_N+1) + log(migr_stock+1) + comlang_off + comcol + contig + log(dist) + year0_1995 + year0_2000 + year0_2005 + (1 | orig) + (1 | dest) + (1 | DirRoute)
)
m1SI <- brms::brm(
    formula = m1SI_f,
    prior = prior_list,
    family = gaussian(),
    data = dat,
    chain = n_chains,
    cores = n_cores,
    iter = n_iter,
    save_pars = save_pars(all = TRUE),
    control = list(adapt_delta = 0.85)
)
rstan::get_elapsed_time(m1SI$fit)

# write output to disk
readr::write_rds(m1SI, file = "./stan_output/m1SI.Rds", compress = "gz")
gc();gc()


## ----Table A.2, Model 2--------------------------------------

# fit model
m2SI_f <- formula(
    sd_rev_neg_subRef ~ cy_gepic_princeton_hist_all_idx + dis_watergap2_princeton_hist_idx + log(O_pop) + log(pwt_gdppc_ratio) + log(O_pwt_gdppc) + log(ged_fatal_N+1) + log(migr_stock+1) + comlang_off + comcol + contig + log(dist) + year0_1995 + year0_2000 + year0_2005 + (1 | orig) + (1 | dest) + (1 | DirRoute)
)
m2SI <- brms::brm(
    formula = m2SI_f,
    prior = prior_list_nb,
    family = negbinomial(),
    data = dat,
    chain = n_chains,
    cores = n_cores,
    iter = n_iter,
    save_pars = save_pars(all = TRUE),
    control = list(adapt_delta = 0.95)
)
rstan::get_elapsed_time(m2SI$fit)

# write output to disk
readr::write_rds(m2SI, file = "./stan_output/m2SI.Rds", compress = "gz")
gc();gc()
