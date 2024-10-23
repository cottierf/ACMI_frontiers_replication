# Author: Fabien Cottier 
# Date: October 2024

# Frontiers in Climate Replication File
# Title: 3_tables_main.R
# Description: This script generates tables 1 and 2 for the main text of the article.


## ----initialization-----------------------------------------------------------

# working directory
# setwd("path/to/working/directory")

# libraries
library(dplyr)
library(readr)
library(rstan)
library(brms)
library(loo)
library(BayesPostEst)
library(glue)


## ---- load models-----------------------------------------------------------


# load stan model objects
m1 <- readr::read_rds("./stan_output/m1.Rds")
m2 <- readr::read_rds("./stan_output/m2.rds")
m3 <- readr::read_rds("./stan_output/m3.Rds")
m4 <- readr::read_rds("./stan_output/m4.Rds")

# load loo objects
m1_loocv <- readr::read_rds("./stan_output/loo_cv/m1_loocv.Rds")
m2_loocv <- readr::read_rds("./stan_output/loo_cv/m2_loocv.Rds")
m3_loocv <- readr::read_rds("./stan_output/loo_cv/m3_loocv.Rds")
m4_loocv <- readr::read_rds("./stan_output/loo_cv/m4_loocv.Rds")
loo_list <- list(m1_loocv, m2_loocv, m3_loocv, m4_loocv)


## ----Table 1---------------------------------------------------------------

# columns names
parsName <-list(
    "b_Intercept" = "Intercept",
    "b_cy_gepic_princeton_hist_all_idx" = "Crop yields", 
    "b_dis_watergap2_princeton_hist_idx" = "River discharge", 
    "b_logO_pop" = "Population, ln",
    "b_logpwt_gdppc_ratio" = "GDP pc ratio, ln",
    "b_logO_pwt_gdppc" = "GDP pc origin, ln",
    "b_logged_fatal_NP1" = "Conflict intensity, ln",
    "b_logmigr_stockP1" = "Migrant stock, ln" ,
    "b_logdist" = "Distance, ln",
    "b_year0_1995" = "$t_{1995}$",
    "b_year0_2000" = "$t_{2000}$",
    "b_year0_2005" = "$t_{2005}$",
    "sd_orig__Intercept" = "$\\tau_{orig}$",
    "sd_dest__Intercept" = "$\\tau_{dest}$",
    "sd_DirRoute__Intercept" = "$\\tau_{orig\\rightarrow dest}$",
    "sd_orig__cy_gepic_princeton_hist_all_idx" = "$\\tau_{crop\\; yields}$",
    "sd_orig__dis_watergap2_princeton_hist_idx" ="$\\tau_{river\\; discharge}$", 
    "sigma" = "$\\sigma$"
)

# regression table
tab1 <- BayesPostEst::mcmcReg(
    pointest = "median",
    list(m1, m2, m3, m4),
    ci = 0.9,
    pars=c("b","sd","sigma"), regex = T,
    custom.coef.map  = parsName,
    table = F,
    custom.gof.rows = list(
      "Observations" = rep(8190,4),
      "elpd$_{loo}$"= sapply(loo_list, function(x) round(x$estimates[1,1],1)),
      "se elpd$_{loo}$"= sapply(loo_list, function(x) paste("($",round(x$estimates[1,2],1),"$)")),
      "$p$ elpd$_{loo}$"= sapply(loo_list, function(x) round(x$estimates[2,1],1)),
      "se $p$ elpd$_{loo}$"= sapply(loo_list, function(x) paste("($",round(x$estimates[2,2],1),"$)"))
    ),
    custom.note = "Median parameter estimates and 90\\% credible interval in square brackets. Group-level correlation parameters for Model 4 omitted.")
tab1 <- gsub(pattern = "^{*}",replacement = "", x= tab1, fixed = T)
tab1

# write table
write(substr(tab1, start = 1+1,stop = stringr::str_locate(string = tab1, "\\$.tau")-2),"./tables/table1.tex",append=F)
write("\\hline","./tables/table1.tex",append=T)
write(substr(tab1, start = stringr::str_locate(string = tab1, "\\$.tau"),stop = nchar(tab1)-1), "./tables/table1.tex",append=T)



## ----Table 2---------------------------------------------------------------

tab2 <- brms::loo_compare(loo_list)
tab2


# write table
write("\\begin{tabular}{l | c c}", "./tables/table2.tex")
write("& $\\Delta~elpd_{loo}$ & $se~~elpd_{loo}$ \\", "./tables/table2.tex",append=T)
write(glue("\\hline Model 4 & ${glue(round(tab2[1,1],1))}$ & ${glue(round(tab2[1,2],1))} \\"), "./tables/table2.tex",append=T)
write(glue("Model 3 & ${glue(round(tab2[2,1],1))}$ & ${glue(round(tab2[2,2],1))} \\"), "./tables/table2.tex",append=T)
write(glue("Model 2 & ${glue(round(tab2[3,1],1))}$ & ${glue(round(tab2[3,2],1))} \\"), "./tables/table2.tex",append=T)
write(glue("Model 1 & ${glue(round(tab2[4,1],1))}$ & ${glue(round(tab2[4,2],1))} \\"), "./tables/table2.tex",append=T)
write("\\end{tabular}", "./tables/table2.tex",append=T)
