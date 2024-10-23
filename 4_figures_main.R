# Author: Fabien Cottier 
# Date: October 2024

# Frontiers in Climate Replication File
# Title: 4_figures_main.R
# Description: This script generates Figures 1 and 2 as depicted in the main text of the article.


## ----initialization-----------------------------------------------------------

# working directory
# setwd("path/to/working/directory")

# libraries
library(dplyr)
library(readr)
library(rstan)
library(brms)
library(ggplot2)
library(bayesplot)
library(glue)
library(cowplot)
library(gridExtra)

# load customs function
source("./utilities/TotalMigr_plotfn.R")
source("./utilities/sum_arrD_fn.R")
source("./utilities/trendPlot_fn.R")



## ---- load projections-----------------------------------------------------------


# load all six projection file in single list and extract projection arrays
project_sc <- c(
    "GEPIC_WG2_gfdl_SSP1_NC", "GEPIC_WG2_gfdl_SSP1_RCP26", "GEPIC_WG2_gfdl_SSP1_RCP60", 
    "GEPIC_WG2_gfdl_SSP3_NC", "GEPIC_WG2_gfdl_SSP3_RCP26", "GEPIC_WG2_gfdl_SSP3_RCP60"
)
prj_arr_list <- lapply(
    project_sc,
    function(x) readr::read_rds(paste0("projections/", x, ".Rds"))
)
names(prj_arr_list) <- project_sc


## ----Figure 1---------------------------------------------------------

# get total cumulative migration for each scenario
TotalMigration <- as_tibble(
    sum_arr_fn_rev(prj_arr_list, pyear0 = "cumulative"),
    rownames = "scenario"
)

# calculate climate effects by taking difference (RCP 2.6/7.0 - NC (sc without climate change))
TotalMigrationD <- dplyr::bind_rows(
    as_tibble(
        sum_arrD_fn_rev(prj_arr_list[1:3], pyear0 = "cumulative"), 
        rownames = "scenario"
    ),
    as_tibble(
        sum_arrD_fn_rev(prj_arr_list[4:6], pyear0 = "cumulative"), 
        rownames = "scenario"
    )
)
fig_1 <- TotMigr_plotfn_rev(TotalMigration, TotalMigrationD)

# Graph results
cowplot::save_plot("plots/fig_1.pdf", fig_1, nrow = 2, ncol =2, device = cairo_pdf)





## ----Figure 2---------------------------------------------------------


# 1. prepare projections

# get cumulative prjections
prjArrCum <-lapply(prj_arr_list,arrayCum)

# get total cumulative migration for each scenario
cumMigration_Africa <- as_tibble(
    sum_arr_fn_CumTrends(prjArrCum[1:6]),
    rownames = "scenario"
)

# calculate climate effects by taking difference (RCP 2.6/7.0 - NC (sc without climate change))
cumMigrationD_Africa <- dplyr::bind_rows(
    as_tibble(
        sum_arrD_fn_CumTrends(prjArrCum[1:3]), 
        rownames = "scenario"
    ),
    as_tibble(
        sum_arrD_fn_CumTrends(prjArrCum[4:6]), 
        rownames = "scenario"
    )
)

# combine projections in a single dataset
Africa_prj = rbind(cumMigration_Africa, cumMigrationD_Africa)

# Add ancilliary data for legend
Africa_prj$CMIP <- "GFDL"
Africa_prj$ISIMIP <- "GEPIC / WaterGAP2"
Africa_prj$SSP <- ifelse(grepl("SSP1", Africa_prj$scenario), "SSP1", "SSP3")
Africa_prj$RCP <- ifelse(grepl("NC", Africa_prj$scenario), "hist. avg.", 
    ifelse(grepl("RCP26", Africa_prj$scenario),"2.6","6.0"))
Africa_prj$prj_type <- ifelse(grepl("RCP26D", Africa_prj$scenario) | grepl("RCP60D", Africa_prj$scenario), "difference", "level")
Africa_prj <- Africa_prj |>
    dplyr::mutate_at(vars(!one_of(c("scenario","CMIP","ISIMIP","SSP","RCP","prj_type",'yearE'))), round, digits = -2) |>
    dplyr::arrange(CMIP,ISIMIP,SSP,match(RCP,c("hist. avg.","2.6","6.0")),desc(prj_type),yearE)
Africa_prj <- Africa_prj[,c(9:13,2:8)]
Africa_prj


# 2. prepare plot

# Color set up
scenarios <- c('SSP1_6.0','SSP3_6.0','SSP1_2.6','SSP3_2.6') # used to grab correct color for each scenario, must be in same order as scenario display
colors <- list('SSP1_6.0' = "#00998F", 'SSP3_6.0' = "#E1BC24", 'SSP3_2.6' = "#8FC962", 
               'SSP1_2.6' = "#9B4D8A") # saturated colors
lightcolors <- list('SSP1_6.0' = "#D4EEEC", 'SSP3_6.0' = "#F6EBBD", 'SSP3_2.6' = "#E3F1D8", 
                    'SSP1_2.6' = "#ECD2E6") # light background colors
bcolor <- "#EFEFF0" # figure background: light gray
lcolor <- "#BCBEC0" # graph axis lines: medium gray
ocolor <- "#6D6E71" # figure outline: dark gray
blueLine <- "#009BDF" # SSP projections: light blue
darkFill <- '#0C2949' # many uses (table fill, other migrants, SSP projections): dark blue

# Select only 2020, 2030, 2040, and 2050 (years divisible by 10)
data <- Africa_prj
data <- filter(data, yearE %% 10 == 0)

# Add display names
data <- addDisplay(data)

# location of any centered labels
midx <- (max(data$yearE) + min(data$yearE)) / 2

# plot prep: select units (millions or thousands)
maxVal <- max(data$ub90[data$prj_type=="difference"])
minVal <- min(data$lb90[data$prj_type=="difference"])
if (maxVal >= 900000 | abs(minVal) >= 900000){
    units <- c("millions","million",1000000)
} else {
    units <- c("thousands","thousand",1000)
}
maxVal <- maxVal/as.numeric(units[3])
minVal <- minVal/as.numeric(units[3])

# plot prep: decide on y-axis breaks
breakAmounts <- c(0.1, 0.2, 0.25, 0.5, 1, 2, 5, 10, 25, 50, 100, 200, 500)
breakAmount <- breakAmounts[(maxVal++ifelse(minVal<0,abs(minVal),0))/6-breakAmounts < 0][which.max(((maxVal++ifelse(minVal<0,abs(minVal),0))/6-breakAmounts)[maxVal/6-breakAmounts < 0])]
breaks <- seq(floor(minVal/breakAmount)*breakAmount, ceiling(maxVal/breakAmount)*breakAmount, by=breakAmount)
if ((max(breaks)-maxVal)/breakAmount < 0.6){
    breaks <- c(breaks, max(breaks)+breakAmount)
}
if (length(breaks) > 7){
    breakAmount <- breakAmounts[maxVal/6-breakAmounts < 0][which.max((maxVal/6-breakAmounts)[maxVal/6-breakAmounts < 0])+1]
    breaks <- seq(floor(minVal/breakAmount)*breakAmount, ceiling(maxVal/breakAmount)*breakAmount, by=breakAmount)
}
blabels <- c(format(breaks[-c(length(breaks))]),'')

# set up labels
graphTitle <- paste0("Africa climate-induced migrants (",units[1],")")



# 3. Generate plot, including subplots

# create four different plots (one for each scenario)
p1 <- plot_trend_fun(data, scenarios[1], colors, lightcolors, breaks, blabels)
p2 <- plot_trend_fun(data, scenarios[2], colors, lightcolors, breaks, blabels)
p3 <- plot_trend_fun(data, scenarios[3], colors, lightcolors, breaks, blabels)
p4 <- plot_trend_fun(data, scenarios[4], colors, lightcolors, breaks, blabels)

# blank space between graphs and tables
p5 <- ggplot() + 
    geom_blank(
        mapping = NULL,
        data = NULL,
        stat = "identity",position = "identity", 
        show.legend = NA, inherit.aes = TRUE
    ) + 
    theme(plot.background = element_rect(fill = "transparent", color = NA))

#  arrange plots on grid
lay <- rbind(c(1,2,3),c(4,5,6))
fig_2 <- ggdraw(arrangeGrob(
    p5, p1, p2, p5, p3, p4,
    layout_matrix=lay,
    heights=c(10,10), widths=c(0.2,5,4.8)
    )) +
    draw_label(
        graphTitle,
        x=0.01, y=0.5,
        angle=90, size=10, fontface=2
    ) + 
    theme(
        plot.background=element_rect(fill=bcolor, linetype="solid", color=ocolor, size=1.5),
        plot.margin=margin(0.1, 0.1, 0.1, 0.3, "cm")
    )

#migration plot
fig_2
ggsave("./plots/fig_2.png", plot=fig_2, width=6, height=5, units="in", dpi=300)

# NOTE: the plot differs from the one shows in the Article in terms of font used.
#   The font elements were omitted as they required a cumbersome installation process
#   Please contact the Author, if you want to have access to the file using the calibri font
