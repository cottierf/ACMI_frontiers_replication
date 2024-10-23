# R Utility: plot total migration
# Fabien Cottier, October 2024

TotMigr_plotfn_rev <- function(projDF, projDF_diff){

    # initalization
    require(ggplot2)
    require(cowplot)


    # set legend and rescale data -------------
    # level
    maxVal <- max(projDF$ub90)
    if (maxVal >= 900000){
        units <- c("millions","million",1000000)
    } else {
        units <- c("thousands","thousand",1000)
    }
    maxVal <- maxVal/as.numeric(units[3])
    # diff
    maxDiff <- max(max(projDF_diff$ub90), abs(min(projDF_diff$lb90)))
    if (maxDiff >= 900000){
        unitsDiff <- c("millions","million",1000000)
    } else {
        unitsDiff <- c("thousands","thousand",1000)
    }
    maxDiff <- maxDiff/as.numeric(unitsDiff[3])
    # rescale data
    projDF[,c("mean","lb90","ub90")] <- projDF[,c("mean","lb90","ub90")]/as.numeric(units[3])
    projDF_diff[,c("mean","lb90","ub90")] <- projDF_diff[,c("mean","lb90","ub90")]/as.numeric(unitsDiff[3])

    # Plot SSP1
    p1a <- ggplot(projDF[1:3,]) +
        geom_bar( aes(x=scenario, y=mean), stat="identity", fill="skyblue", alpha=0.7, ) +
        geom_errorbar( aes(x=scenario, ymin=lb90, ymax=ub90), width=0.4, colour="orange", alpha=0.9, size=1.3) + 
        scale_x_discrete(labels = c("Hist. average (1990 \U2012 2010)", "RCP 2.6", "RCP 6.0")) +
        ylab(glue("Pred. N migrants ({units[1]})")) +
        ylim(0, maxVal*1.25) +
        theme(axis.title.x = element_blank())
    p1b <- ggplot(projDF_diff[1:2,]) +
        geom_errorbar( aes(x=scenario, ymin=lb90, ymax=ub90), width=0.4, colour="orange", alpha=0.9, size=1.3) + 
        scale_x_discrete(labels = c("RCP 2.6", "RCP 6.0")) +
        ylab(glue("Difference with hist. scenario ({unitsDiff[1]})")) +
        geom_hline(yintercept = 0, linetype="dashed", color = "red") +
        scale_y_continuous(position = "right", limits = c(-maxDiff*1.25, maxDiff*1.25)) +
        theme(axis.title.x = element_blank())
    title1 <- ggdraw() + 
        draw_label(
            "SSP1",
            fontface = 'bold',
            x = 0.5,
            hjust = 0.5
        ) +
        theme(
            # add margin on the left of the drawing canvas,
            # so title is aligned with left edge of first plot
            plot.margin = margin(0, 0, 0, 7)
        )
    p1 <- plot_grid(p1a, p1b, labels = "AUTO", rel_widths = c(3, 2))
    p1 <- plot_grid(
        title1, p1,
        ncol = 1,
        # rel_heights values control vertical title margins
        rel_heights = c(0.1, 1)
    )

    # Plot SSP3
    p2a <- ggplot(projDF[4:6,]) +
        geom_bar( aes(x=scenario, y=mean), stat="identity", fill="skyblue", alpha=0.7, ) +
        geom_errorbar( aes(x=scenario, ymin=lb90, ymax=ub90), width=0.4, colour="orange", alpha=0.9, size=1.3) + 
        scale_x_discrete(labels = c("Hist. average (1990 \U2012 2010)", "RCP 2.6", "RCP 6.0")) +
        ylab(glue("Pred. N migrants ({units[1]})")) +
        ylim(0, maxVal*1.25) +
        theme(axis.title.x = element_blank())
    p2b <- ggplot(projDF_diff[3:4,]) +
        geom_errorbar( aes(x=scenario, ymin=lb90, ymax=ub90), width=0.4, colour="orange", alpha=0.9, size=1.3) + 
        scale_x_discrete(labels = c("RCP 2.6", "RCP 6.0")) +
        ylab(glue("Difference with hist. scenario ({unitsDiff[1]})")) +
        geom_hline(yintercept = 0, linetype="dashed", color = "red") +
        scale_y_continuous(position = "right", limits = c(-maxDiff*1.25, maxDiff*1.25)) +
        theme(axis.title.x = element_blank())
    title2 <- ggdraw() + 
        draw_label(
            "SSP3",
            fontface = 'bold',
            x = 0.5,
            hjust = 0.5
        ) +
        theme(
            # add margin on the left of the drawing canvas,
            # so title is aligned with left edge of first plot
            plot.margin = margin(0, 0, 0, 7)
      )
    p2 <- plot_grid(p2a, p2b, labels = "AUTO", rel_widths = c(3, 2))
    p2 <- plot_grid(
        title2, p2,
        ncol = 1,
        # rel_heights values control vertical title margins
        rel_heights = c(0.1, 1)
    )

    # Combine SSP1 + SSP3 plots
    pcombined <- plot_grid(p1, p2, nrow= 2)
    return(pcombined)
}

