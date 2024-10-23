# Author: Fabien Cottier 
# Date: October 2024

# Frontiers in Climate Replication File
# Title: 2_projection_main.R
# Description: This script projects future migration patterns based on the calibrated model (M2).



# initialization ----------

# working directory
# setwd("path/to/working/directory")

# libraries
library(dplyr)
library(readr)
library(rstan)
library(brms)
library(ggplot2)
library(cowplot)


# load projection script
source("./utilities/prj_loglinear_probabilistic.R")

# load calibration & projection data
prj_dat <- readr::read_rds("data/projection_data.Rds")
cal_dat <- readr::read_rds("data/calibration_data.Rds")

# load stan Model M2
m3 <- readRDS("./stan_output/m3.Rds")


# Prepare projection data ----------

# create No climChange scenario holding the crop yields and river discharge to the 1990-2010 average
prj_dat <- cal_dat |> 
    dplyr::group_by(orig,year0) |> 
    dplyr::summarise(
        cy_gepic_princeton_hist_all_idx = first(cy_gepic_princeton_hist_all_idx),
        dis_watergap2_princeton_hist_idx = first(dis_watergap2_princeton_hist_idx),
        ged_fatal_N = unique(ged_fatal_N)
    ) |> 
    dplyr::group_by(orig) |>
    dplyr::summarise(
        cy_gepic_rcpNC_all_idx = mean(cy_gepic_princeton_hist_all_idx),
        dis_watergap2_rcpNC_idx = mean(dis_watergap2_princeton_hist_idx),
        avgHistConflict = mean(ged_fatal_N)
    ) |> 
    dplyr::right_join(prj_dat, by = "orig", multiple = "all")

# Extract parameters ----------

# extract parameters from model
param_list <- list(
    b = fixef(m3, summary = F) ,
    s = as.vector(brms::as_draws_matrix(m3, variable = "sigma")),
    theta = extractRanef(prj_dat = prj_dat, fittedMod = m3)
)



# Projections -------------

# notes: 
#   1) the projection uses a custom written script titled "prj_loglinear_probabilistic.R", which can be found in the utilities folder.
#   2) The script will run 6 times for each scenario (SSP1, SSP3) and climate scenario (no climate change, RCP 2.6, RCP 6.0)

print("\n\nStart projecting for gepic SSP1 no climate scenario")
GEPIC_WG2_SSP1_NC <- Mprj_prob_fn(
    param_list = param_list,
    prj_dat = prj_dat,
    prj_start = 2010, prj_end = 2045,
    cy_var = "cy_gepic_rcpNC_all_idx",
    dis_var = "dis_watergap2_rcpNC_idx",
    SSP = "SSP1",
    histConflict = TRUE
)

print("Start projecting for gepic SSP1 RCP 2.6")
GEPIC_WG2_SSP1_RCP26 <- Mprj_prob_fn(
    param_list = param_list,
    prj_dat = prj_dat,
    prj_start = 2010, prj_end = 2045,
    cy_var = "cy_gfdl_gepic_rcp26_all_idx",
    dis_var = "dis_gfdl_watergap2_rcp26_idx",
    SSP = "SSP1",
    histConflict = TRUE
)

print("Start projecting for gepic SSP1 RCP 6.0")
GEPIC_WG2_SSP1_RCP60 <- Mprj_prob_fn(
    param_list = param_list,
    prj_dat = prj_dat,
    prj_start = 2010, prj_end = 2045,
    cy_var = "cy_gfdl_gepic_rcp60_all_idx",
    dis_var = "dis_gfdl_watergap2_rcp60_idx",
    SSP = "SSP1",
    histConflict = TRUE
)

print("Start projecting for gepic SSP3 no climate scenario")
GEPIC_WG2_SSP3_NC <- Mprj_prob_fn(
    param_list = param_list,
    prj_dat = prj_dat,
    prj_start = 2010, prj_end = 2045,
    cy_var = "cy_gepic_rcpNC_all_idx",
    dis_var = "dis_watergap2_rcpNC_idx",
    SSP = "SSP3",
    histConflict = TRUE
)

print("Start projecting for gepic SSP3 RCP 2.6")
GEPIC_WG2_SSP3_RCP26 <- Mprj_prob_fn(
    param_list = param_list,
    prj_dat = prj_dat,
    prj_start = 2010, prj_end = 2045,
    cy_var = "cy_gfdl_gepic_rcp26_all_idx",
    dis_var = "dis_gfdl_watergap2_rcp26_idx",
    SSP = "SSP3",
    histConflict = TRUE
)

print("Start projecting for gepic SSP3 RCP 6.0")
GEPIC_WG2_SSP3_RCP60 <- Mprj_prob_fn(
    param_list = param_list,
    prj_dat = prj_dat,
    prj_start = 2010, prj_end = 2045,
    cy_var = "cy_gfdl_gepic_rcp60_all_idx",
    dis_var = "dis_gfdl_watergap2_rcp60_idx",
    SSP = "SSP3",
    histConflict = TRUE
)

# store projection for later use
readr::write_rds(GEPIC_WG2_SSP1_NC, file = "projections/GEPIC_WG2_gfdl_SSP1_NC.Rds", compress = "gz")
readr::write_rds(GEPIC_WG2_SSP1_RCP26, file = "projections/GEPIC_WG2_gfdl_SSP1_RCP26.Rds", compress = "gz")
readr::write_rds(GEPIC_WG2_SSP1_RCP60, file = "projections/GEPIC_WG2_gfdl_SSP1_RCP60.Rds", compress = "gz")
readr::write_rds(GEPIC_WG2_SSP3_NC, file = "projections/GEPIC_WG2_gfdl_SSP3_NC.Rds", compress = "gz")
readr::write_rds(GEPIC_WG2_SSP3_RCP26, file = "projections/GEPIC_WG2_gfdl_SSP3_RCP26.Rds", compress = "gz")
readr::write_rds(GEPIC_WG2_SSP3_RCP60, file = "projections/GEPIC_WG2_gfdl_SSP3_RCP60.Rds", compress = "gz")


