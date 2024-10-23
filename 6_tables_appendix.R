# Author: Fabien Cottier 
# Date: October 2024

# Frontiers in Climate Replication File
# Title: 6_tables_appendix.R
# Description: This script generates tables A.1--A.2 of the appendix to the article.


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
library(stargazer)


## ---- load models-----------------------------------------------------------

# load stan model objects
m3 <- readr::read_rds("./stan_output/m3.Rds")
m1SI <- readr::read_rds("./stan_output/m1SI.Rds")
m2SI <- readr::read_rds("./stan_output/m2SI.Rds")


## ----Table 1, Appendix ---------------------------------------------------------------

dat <- model.frame(m3)[,c(2:4,14:19,11:13)]
tab_A1 <- stargazer::stargazer(
  dat,
  covariate.labels = c(
    "Int. migration", "Crop yields", "River discharge", "Population",
    "GDPpc ratio", "GDPpc origin", "Conflict intensity", 
    "Migrant stock", "Distance",
    "t$_{1995}$", "t$_{2000}$", "t$_{2005}$"
  ))
tab_A1

write(tab_A1,"./tables/table_A1.tex")



## ----Table 2, Appendix---------------------------------------------------------------

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
    "b_comlang_off" = "Common language" ,
    "b_comcol" = "Common colonizer" ,
    "b_contig" = "Contiguity" ,
    "b_logdist" = "Distance, ln",
    "b_year0_1995" = "$t_{1995}$",
    "b_year0_2000" = "$t_{2000}$",
    "b_year0_2005" = "$t_{2005}$",
    "sd_orig__Intercept" = "$\\tau_{orig}$",
    "sd_dest__Intercept" = "$\\tau_{dest}$",
    "sd_DirRoute__Intercept" = "$\\tau_{orig\\rightarrow dest}$",
    "sd_orig__cy_gepic_princeton_hist_all_idx" = "$\\tau_{crop\\; yields}$",
    "sd_orig__dis_watergap2_princeton_hist_idx" ="$\\tau_{river\\; discharge}$", 
    "sigma" = "$\\sigma$",
    "shape" = "$\\theta$"
)

# summary table
tab_A2 <- BayesPostEst::mcmcReg(
    pointest = "median",
    list(m1SI, m2SI),
    ci = 0.9,
    pars=c("b","sd","sigma","shape"), regex = T,
    custom.coef.map  = parsName,
    table = F,
    custom.gof.rows = list(
      "Observations" = rep(8190,2)
    ),
    custom.note = "Median parameter estimates and 90\\% credible interval in square brackets.")
tab_A2 <- gsub(pattern = "^{*}",replacement = "", x= tab_A2, fixed = T)
tab_A2

# write table
write(substr(tab_A2, start = 1+1,stop = stringr::str_locate(string = tab_A2, "\\$.tau")-2),"./tables/table_A2.tex")
write("\\hline","./tables/table_A2.tex",append=T)
write(substr(tab_A2, start = stringr::str_locate(string = tab_A2, "\\$.tau"),stop = nchar(tab_A2)-1), "./tables/table_A2.tex",append=T)



