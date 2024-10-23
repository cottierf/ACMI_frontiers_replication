# Author: Fabien Cottier 
# Date: October 2024

# Frontiers in Climate Replication File
# Title: 1_calibration_main.R
# Description: This script calibrates and estimate the models presented in Table 1 in the main text of the article

# Note: code use parallel processing of chains (assuming 4 parallel chain by default). If less cores are avalable, simply
#   change the number of cores used on lines 24 (e.g. n_cores = 1). To check the number of cores available, you can use 
#   the following command: parallel::detectCores() (this requires the parallel R library)

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


## ---- load data-----------------------------------------------------------

# load data
dat <- readr::read_rds("data/calibration_data.Rds")

# add period dummies
dat <- fastDummies::dummy_cols(dat, select_columns = "year0", remove_first_dummy = T)
dat

## ----set prior---------------------------------------------------------------

# prior on alpha and betas (M1, M3, M4)
palpha <- set_prior("student_t(3, 1.24, 6.51)", class = "Intercept")
pbeta <- set_prior("student_t(3, 0, 57.1)", class = "b", coef = "cy_gepic_princeton_hist_all_idx") +
    set_prior("student_t(3, 0, 41.6)", class = "b", coef = "dis_watergap2_princeton_hist_idx") + 
    set_prior("student_t(3, 0, 6.75)", class = "b", coef = "logO_pwt_gdppc") +  
    set_prior("student_t(3, 0, 4.73)", class = "b", coef = "logpwt_gdppc_ratio") +  
    set_prior("student_t(3, 0, 1.78)", class = "b", coef = "logged_fatal_NP1") +
    set_prior("student_t(3, 0, 1.82)", class = "b", coef = "logmigr_stockP1") +
    set_prior("student_t(3, 0, 9.47)", class = "b", coef = "logdist") +
    set_prior("student_t(3, 0, 5.53)", class = "b", coef = "logO_pop") +
    set_prior("student_t(3, 0, 14.97)", class = "b") # priors on yearly dummies
prior_list <- pbeta + palpha 

# prior on alpha and betas (M2)
palpha_m2 <- set_prior("student_t(3, 1.24, 6.51)", class = "Intercept")
pbeta_m2 <- set_prior("student_t(3, 0, 14.97)", class = "b") # priors on yearly dummies
prior_list_m2 <- pbeta_m2 + palpha_m2 


## ----Table 1, Model 1---------------------------------------------------------

# fit model
m1_f <- formula(
    log(sd_rev_neg_subRef+1) ~ cy_gepic_princeton_hist_all_idx + dis_watergap2_princeton_hist_idx + log(O_pop) + log(pwt_gdppc_ratio) + log(O_pwt_gdppc) + log(ged_fatal_N+1) + log(migr_stock+1) + log(dist) + year0_1995 + year0_2000 + year0_2005
)
m1 <- brms::brm(
    formula = m1_f,
    prior = prior_list,
    family = gaussian(),
    data = dat,
    chain = n_chains,
    cores = n_cores,
    iter = n_iter,
    save_pars = save_pars(all = TRUE)
)
rstan::get_elapsed_time(m1$fit)

# show results
summary(m1)

# write output to disk
readr::write_rds(m1, file = "./stan_output/m1.Rds", compress = "gz")
gc();gc()


## ----Table 1, Model 2--------------------------------------

# fit model
m2_f <- formula(
    log(sd_rev_neg_subRef+1) ~ year0_1995 + year0_2000 + year0_2005 + (1 | orig) + (1 | dest) + (1 | DirRoute)
)
m2 <- brms::brm(
    formula = m2_f,
    prior = prior_list_m2,
    family = gaussian(),
    data = dat,
    chain = n_chains,
    cores = n_cores,
    iter = n_iter,
    save_pars = save_pars(all = TRUE),
    control = list(adapt_delta = 0.9, max_treedepth = 12) # increase max treedepth very inneficient model
)
rstan::get_elapsed_time(m2$fit)

# show results
summary(m2)

# write output to disk
readr::write_rds(m2, file = "./stan_output/m2.Rds", compress = "gz")
gc();gc()



## ----Table 1, Model 3---------------------------------------------

# fit model
m3_f <- formula(
    log(sd_rev_neg_subRef+1) ~ cy_gepic_princeton_hist_all_idx + dis_watergap2_princeton_hist_idx + log(O_pop) + log(pwt_gdppc_ratio) + log(O_pwt_gdppc) + log(ged_fatal_N+1) + log(migr_stock+1) + log(dist) + year0_1995 + year0_2000 + year0_2005 + (1 | orig) + (1 | dest) + (1 | DirRoute)
)
m3 <- brms::brm(
    formula = m3_f,
    prior = prior_list,
    family = gaussian(),
    data = dat,
    chain = n_chains,
    cores = n_cores,
    iter = n_iter,
    save_pars = save_pars(all = TRUE),
    control = list(adapt_delta = 0.85)
)
rstan::get_elapsed_time(m3$fit)

# show results
summary(m3)

# write output to disk
readr::write_rds(m3, file = "./stan_output/m3.Rds", compress = "gz")
gc();gc()


## ----Table 1, Model 4---------------------------------------------

# fit model
m4_f <- formula(
    log(sd_rev_neg_subRef+1) ~  cy_gepic_princeton_hist_all_idx + dis_watergap2_princeton_hist_idx + log(O_pop) + log(pwt_gdppc_ratio) + log(O_pwt_gdppc) + log(ged_fatal_N+1) + log(migr_stock+1) + log(dist) + year0_1995 + year0_2000 + year0_2005 + (1 + cy_gepic_princeton_hist_all_idx + dis_watergap2_princeton_hist_idx | q | orig) + (1 | dest) + (1 | DirRoute)
)
m4 <- brms::brm(
    formula = m4_f,
    prior = prior_list,
    family = gaussian(),
    data = dat,
    chain = n_chains,
    cores = n_cores,
    iter = n_iter,
    save_pars = save_pars(all = TRUE),
    control = list(adapt_delta = 0.85)
)
rstan::get_elapsed_time(m4$fit)

# show results
summary(m4)

# write output to disk
readr::write_rds(m4, file = "./stan_output/m4.Rds", compress = "gz")
gc();gc()




## ----leave-one-out cross-validation ELPD--------------------------------------


# WARNING: Computing loo-cv elpd can take a significant amount of time, especially for Model 2

# memory setup & core setup
options(loo.cores = 4)
options(future.globals.maxSize= 1024 * 1024^2 )


# Model 1

m1_loocv <- brms::loo(m1, save_psis = F, moment_match = T, reloo = TRUE)
m1_loocv
readr::write_rds(m1_loocv, file = "./stan_output/loo_cv/m1_loocv.Rds", compress = "gz")
gc();gc()


# Model 2

# Warning reloo is very slow (model will refit 82 times)
#   for faster consider using Kfold (commented line below)
#   Kfold is less accurate and result in a slightly biased elpd estimate, but is significantly faster.

m2_loocv <- brms::loo(m2, save_psis = F, moment_match = T, reloo = TRUE, cores = n_cores)
#m2_loocv <- brms::kfold(m2, k = 10, cores = n_cores)
m2_loocv
readr::write_rds(m2_loocv, file = "./stan_output/loo_cv/m2_loocv.Rds", compress = "gz")
gc();gc()


# Model 3
m3_loocv <- brms::loo(m3, save_psis = F,moment_match = T, reloo = TRUE, cores = n_cores)
m3_loocv
readr::write_rds(m3_loocv, file = "./stan_output/loo_cv/m3_loocv.Rds", compress = "gz")
gc();gc()


# Model 4
m4_loocv <- brms::loo(m4, save_psis = F, moment_match = TRUE, reloo = TRUE, cores = n_cores)
m4_loocv
readr::write_rds(m4_loocv, file = "./stan_output/loo_cv/m4_loocv.Rds", compress = "gz")
gc();gc()