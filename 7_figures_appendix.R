# Author: Fabien Cottier 
# Date: October 2024

# Frontiers in Climate Replication File
# Title: 6_figures_appendix.R
# Description: This script generates Figures A.1-A.9 as depicted in the appendix of the article.

## ----initialization-----------------------------------------------------------

# working directory
# setwd("path/to/working/directory")

# libraries
library(dplyr)
library(readr)
library(tidyr)
library(rstan)
library(brms)
library(ggplot2)
library(bayesplot)
library(fastDummies)
library(countrycode)

# load customs function
source("./utilities/TotalMigr_plotfn.R")



## ---- load models-----------------------------------------------------------

# load data
dat <- readr::read_rds("data/calibration_data.Rds")

# load stan model objects
m1 <- readr::read_rds("./stan_output/m1.Rds")
m2 <- readr::read_rds("./stan_output/m2.Rds")
m3 <- readr::read_rds("./stan_output/m3.Rds")
m4 <- readr::read_rds("./stan_output/m4.Rds")
m2SI <- readr::read_rds("./stan_output/m2SI.Rds")

# load loo cv elpd object
m3_loocv <- readr::read_rds("./stan_output/loo_cv/m3_loocv.Rds")


## ----Figure A.1---------------------------------------------------------

# posterior predictive checks model M3

# extract data and get posterior samples
y_m3 <- m3$data[,1]
yrep_m3 <- brms::posterior_predict(m3, ndraws  = 1000)


# figure A.1 a) posterior predictive checks for the mean
fig_A1a <- bayesplot::ppc_stat(y = y_m3, yrep = yrep_m3, stat = "mean")
ggplot2::ggsave(fig_A1a, filename = "plots/fig_A1a.pdf", height = 7, width = 7 * 1.5)
fig_A1a

# figure A.1 b) posterior predictive checks for the standard deviation
fig_A1b <- bayesplot::ppc_stat(y = y_m3, yrep = yrep_m3, stat = "sd")
ggplot2::ggsave(fig_A1b, filename = "plots/fig_A1b.pdf", height = 7, width = 7 * 1.5)
fig_A1b

# figure A.1 c) posterior predictive checks for the standard deviation
fig_A1c <- bayesplot::ppc_stat(y = y_m3, yrep = yrep_m3, stat = "max")
ggplot2::ggsave(fig_A1c, filename = "plots/fig_A1c.pdf", height = 7, width = 7 * 1.5)
fig_A1c

# figure A.1 d) posterior predictive density  // limit to 25 random draws
fig_A1d <- bayesplot::ppc_dens_overlay(
    y = log10(exp(y_m3)),
    yrep = log10(exp(yrep_m3[sample(1:1000, 25),]))
)
ggplot2::ggsave(fig_A1d, filename = "plots/fig_A1d.pdf", height = 7, width = 7 * 1.5)
fig_A1d



## ----Figure A.2---------------------------------------------------------

# posterior predictive checks model M6 (negative binomial)

# extract data
y_m2SI <- m2SI$data[,1]
yrep_m2SI <- brms::posterior_predict(m2SI, ndraws = 1000)

# figure A.1 a) posterior predictive checks for the mean
fig_A2a <- bayesplot::ppc_stat(y = log(y_m2SI+1), yrep = log(yrep_m2SI+1), stat = "mean")
ggplot2::ggsave(fig_A2a, filename = "plots/fig_A2a.pdf", height = 7, width = 7 * 1.5)
fig_A2a

# figure A.1 b) posterior predictive checks for the standard deviation
fig_A2b <- bayesplot::ppc_stat(y = log(y_m2SI+1), yrep = log(yrep_m2SI+1), stat = "sd")
ggplot2::ggsave(fig_A2b, filename = "plots/fig_A2b.pdf", height = 7, width = 7 * 1.5)
fig_A2b

# figure A.1 c) posterior predictive checks for the standard deviation
fig_A2c <- bayesplot::ppc_stat(y = log(y_m2SI+1), yrep = log(yrep_m2SI+1), stat = "max")
ggplot2::ggsave(fig_A2c, filename = "plots/fig_A2c.pdf", height = 7, width = 7 * 1.5)
fig_A2c

# figure A.1 d) posterior predictive density  // limit to 25 random draws
fig_A2d <- bayesplot::ppc_dens_overlay(
    y = log10(y_m2SI+1),
    yrep = log10(yrep_m2SI[sample(1:1000, 25),]+1)
)
ggplot2::ggsave(fig_A2d, filename = "plots/fig_A2d.pdf", height = 7, width = 7 * 1.5)
fig_A2d



## ----Figure A.3---------------------------------------------------------

# Predictive performance of the 12 largest directed migration corridors against observed migration

# extract top 12 corridors
CorridorsRanked <- dat |> 
    dplyr::group_by(orig, dest) |> 
    dplyr::summarize(
        sd_rev_neg_subRef = sum(sd_rev_neg_subRef),
        nPeriods = n()
    ) |>
    dplyr::arrange(desc(sd_rev_neg_subRef)) |>
    dplyr::ungroup()
topCorridors <- CorridorsRanked |> 
    dplyr::slice(1:12) |>
    dplyr::mutate(corridor = paste(orig, dest, sep ="_")) |>
    dplyr::pull(corridor)

# subset calibration daya
datProj <- dat |>
    dplyr::filter(paste(orig, dest, sep ="_") %in% topCorridors)
datProj <- fastDummies::dummy_cols(datProj, select_columns = "year0", remove_first_dummy = T)

# Hindcast expected value of the posterior predictive distribution
set.seed(5678)

projM1 <- brms::posterior_epred(m1, newdata = datProj, ndraws = 1000)
colnames(projM1) <- paste(datProj$orig, datProj$dest, datProj$year0, sep ="_") 
projM1 <- t(projM1)

projM2 <- brms::posterior_epred(m2, newdata = datProj, ndraws = 1000)
colnames(projM2) <- paste(datProj$orig, datProj$dest, datProj$year0, sep ="_") 
projM2 <- t(projM2)

projM3 <- brms::posterior_epred(m3, newdata = datProj, ndraws = 1000)
colnames(projM3) <- paste(datProj$orig, datProj$dest, datProj$year0, sep ="_") 
projM3 <- t(projM3)

projM4 <- brms::posterior_epred(m4, newdata = datProj, ndraws = 1000)
colnames(projM4) <- paste(datProj$orig, datProj$dest, datProj$year0, sep ="_") 
projM4 <- t(projM4)

# collate in dataset
projDF <- datProj %>%
    dplyr::select(orig, dest, year0, sd_rev_neg_subRef) |>
    dplyr::left_join(
        tibble(
            orig = substr(rownames(projM1), 1, 3),
            dest = substr(rownames(projM1), 5, 7),
            year0 = as.numeric(substr(rownames(projM1), 9, 12)),
            projMedM1 = apply(projM1, 1, mean),
            projMedM2 = apply(projM2, 1, mean),
            projMedM3 = apply(projM3, 1, mean),
            projMedM4 = apply(projM4, 1, mean)
        )
    ) |>
        dplyr::mutate(
            sd_rev_neg_subRef_ln = log(sd_rev_neg_subRef+1)
    ) |>
    dplyr::select(-sd_rev_neg_subRef)

# pivot to longer format for ggplot
projDF_long <- projDF |>
    dplyr::rename("observed" = "sd_rev_neg_subRef_ln") %>%
    tidyr::pivot_longer(
        !c(orig:year0),
        names_to = "class",
        values_to = "migration"
    ) |>
    dplyr::arrange(orig, dest, class, year0)

# create plot
fig_A3 <- ggplot(
    data = filter(projDF_long, class != "observed", paste(orig, dest, sep ="_") %in% topCorridors), 
    mapping = aes(x = year0 + 5, y = migration, color = class)
) + 
    geom_line(linetype = 1) +
    geom_line(
        data = filter(projDF_long, class == "observed", paste(orig, dest, sep ="_") %in% topCorridors), 
        mapping = aes(x = year0 + 5, y = migration, linetype = "Observed"),
         color = "darkgrey",
    ) + ylab("Migration (ln)") +
    facet_wrap(~ paste(countrycode(orig, "iso3c", "country.name.en"), countrycode(dest, "iso3c", "country.name.en"), sep ="\U2192")) +
    scale_color_discrete(name = NULL, labels = c("Model 1", "Model 2", "Model 3", "Model 4")) +
    scale_linetype_manual('',values='dashed') + xlab(NULL) +
    theme(
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text = element_text(size=10)
    )
fig_A3

# export
ggsave(plot = fig_A3, filename = "./plots/fig_A3.pdf", width = 5.64*2, height = 2*3*(6/4), device = cairo_pdf)



## ----Figure A.4---------------------------------------------------------


# Hindcast expected value of the posterior predictive distribution
dat_fig4 <- dat
predE_m1 <- colMeans(rstanarm::posterior_epred(m1))
predE_m2 <- colMeans(rstanarm::posterior_epred(m2))
predE_m3 <- colMeans(rstanarm::posterior_epred(m3))
predE_m4 <- colMeans(rstanarm::posterior_epred(m4))
dat_fig4$predM_m1 <- predE_m1
dat_fig4$predM_m2 <- predE_m2
dat_fig4$predM_m3 <- predE_m3
dat_fig4$predM_m4 <- predE_m4

# assemble in joint dataset
Rw_df <- dat_fig4  |>
 dplyr::group_by(orig, dest, DirRoute) |>
 dplyr::summarize(
    sd_rev_neg_subRef_agg = sum(sd_rev_neg_subRef),
    R2_ij_m1 = 1 - sum( (log(sd_rev_neg_subRef+1) - predM_m1)^2 ) / sum( (log(sd_rev_neg_subRef+1) - mean(log(sd_rev_neg_subRef+1)) )^2 ),
    R2_ij_m2 = 1 - sum( (log(sd_rev_neg_subRef+1) - predM_m2)^2 ) / sum( (log(sd_rev_neg_subRef+1) - mean(log(sd_rev_neg_subRef+1)) )^2 ),
    R2_ij_m3 = 1 - sum( (log(sd_rev_neg_subRef+1) - predM_m3)^2 ) / sum( (log(sd_rev_neg_subRef+1) - mean(log(sd_rev_neg_subRef+1)) )^2 ),
    R2_ij_m4 = 1 - sum( (log(sd_rev_neg_subRef+1) - predM_m4)^2 ) / sum( (log(sd_rev_neg_subRef+1) - mean(log(sd_rev_neg_subRef+1)) )^2 )
 ) |>
  dplyr::filter(sd_rev_neg_subRef_agg!=0)

# pivot to long format
Rw_df_long <- tidyr::pivot_longer(Rw_df, R2_ij_m1:R2_ij_m4, names_to = "model", values_to = "R2_ij")
Rw_df_long$model[Rw_df_long$model=="R2_ij_m1"] <- "Model 1"
Rw_df_long$model[Rw_df_long$model=="R2_ij_m2"] <- "Model 2"
Rw_df_long$model[Rw_df_long$model=="R2_ij_m3"] <- "Model 3"
Rw_df_long$model[Rw_df_long$model=="R2_ij_m4"] <- "Model 4"

# plot
fig_A4 <- ggplot(data=filter(Rw_df_long, R2_ij!=0), aes(x=model, y=R2_ij)) + 
    geom_boxplot(outliers=FALSE) + 
    ylim(c(-2.5, NA)) + xlab(NULL) + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed")
ggsave(plot= fig_A4, filename = "plots/fig_A4.pdf", height = 5, width = 7)



## ----Figure A.5--A.8 ---------------------------------------------------------

# density and trace plots for each of the population level parameters of Model 3
pdf(file= "plots/fig_A5_A8_trace%02d.pdf", onefile=FALSE)
plot(
    m3,
    pars = c("b", "sigma", "sd"),
    N = 5,
    ask = F,
    newpage = F
)
dev.off()



## ----Figure A.9 ---------------------------------------------------------

# Pareto k shape parameter diagnostic plot of Model 3
plot(m3_loocv)

# export
pdf(file= "plots/fig_A9.pdf")
plot(m3_loocv)
dev.off()
