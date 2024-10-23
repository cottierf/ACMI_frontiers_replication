# Auxiliary function for the trend plot


# function to get cumulative projections
arrayCum <- function(x){
  arOut <- array(data=NA, dim = dim(x), dimnames = dimnames(x))
  for (i in 1:dim(x)[3]){
    arOut[,,i] <- rowSums(x[,, 1:i, drop = F], dims = 2)
  }
  return(arOut)
}


# trend plot function
plot_trend_fun <- function(df, scenario, colors, lightcolors, breaks, blabels){
  ggplot(df[df$prj_type=="difference" &  paste(df$SSP, df$RCP, sep="_")==scenario, ], 
         aes(x=yearE, y=mean/as.numeric(units[3]))) + # initialize graph
    geom_ribbon(aes(ymin=lb90/as.numeric(units[3]), ymax=ub90/as.numeric(units[3])), 
                fill="white", show.legend=FALSE) + # filled in confidence interval
    geom_hline(yintercept=breaks[-c(length(breaks))], color=lcolor, size=.5) + # light gray y axis lines
    geom_hline(yintercept=max(breaks), color=colors[[scenario]], size=2) + # top y axis line
    geom_line(color=colors[[scenario]], show.legend=FALSE, size=1.5, lineend="round") + # migrants line
    ylab(NULL) + xlab(NULL) + # no labels
    scale_y_continuous(expand=c(0, 0), limits=c(min(breaks),max(breaks)), breaks=breaks, labels=blabels) + # y axis breaks and labels
    annotate("text", label=df$SCENARIO_DISPLAY[paste(df$SSP, df$RCP, sep="_")==scenario][1], 
             x=midx, y=0.97*breaks[length(breaks)], vjust=1, size=3, lineheight=1) + # top scenario display
    theme(axis.text=element_text(size=7), 
          axis.title=element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank(), # no default grid lines
          axis.ticks=element_blank(), axis.line=element_blank(), # no ticks or axis lines
          panel.background=element_rect(fill=lightcolors[[scenario]]), # set graph background color
          plot.background = element_rect(fill = "transparent", color = NA)) # set larger plot background and outline
}


# Add display titles for the scenario names
# update the levels with the new names if these are not correct
addDisplay <- function(df){
  df$SCENARIO_DISPLAY <- ifelse(df$RCP != "hist. avg.", paste(df$SSP, df$RCP, sep="_"), NA)
  df$SCENARIO_DISPLAY <- factor(df$SCENARIO_DISPLAY, 
                                levels = c("SSP1_6.0", "SSP3_6.0","SSP1_2.6", "SSP3_2.6"),
                                labels=c(
                                  "High Growth & Emissions\n(RCP6.0/SSP1)",
                                  "Business As Usual\n(RCP6.0/SSP3)", 
                                  "Optimistic\n(RCP2.6/SSP1)",
                                  "Low Growth & Emissions\n(RCP2.6/SSP3)"
                                )
  )
  return(df)
}
