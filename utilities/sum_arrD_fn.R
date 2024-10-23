# Projections utilities: summarize change in total number of migrants
sum_arr_fn_CumTrends <- function(prj_arr_list, alt_version = F){

    sumstan_fn <- function(x){
        return(c(
            mean(x),
            median(x),
            quantile(x, probs = c(.1, .9)),
            quantile(x, probs = c(.05, .95))
        ))
    }

    for (year0 in seq(2010,2045,5)){

        slice <- paste0("prj_", year0)
        mat <- matrix(
            data = NA,
            nrow = length(prj_arr_list),
            ncol = 7,
            dimnames= list(
                names(prj_arr_list),
                c("yearE","mean","median","lb80","ub80","lb90","ub90")
            )
        )
        mat[,1] <- year0+5
        for (i in 1:nrow(mat)){
            prj_arr <- prj_arr_list[[i]][,,slice]
            mat[i,2:7] <- round(sumstan_fn(colSums(prj_arr)), -2)
        }

        if (year0==2010){
            outMat <- mat
        } else {
            outMat <- rbind(outMat, mat)
        }
        rm(mat)
    }
    return(outMat)
}

# summarize total // revised version // period
sum_arr_period_fn_rev <- function(prj_arr_list, cumulative = F){

    sumstan_fn <- function(x){
        return(c(
            mean(x),
            median(x),
            quantile(x, probs = c(.1, .9)),
            quantile(x, probs = c(.05, .95))
        ))
    }
    mat <- matrix(
        data = NA,
        nrow = 6*8, # 8 = number of periods
        ncol = 6 + 1, # 1: year column
        dimnames = list(
            rep(names(prj_arr_list),8),
            c("year", "mean","median","lb80","ub80","lb90","ub90")
        )
    )
    for (period in 1:8){
        time = 2015+((period-1)*5)
        if (cumulative != T){
            for (i in 1:length(prj_arr_list)){
                prj_arr <- prj_arr_list[[i]][,, period, drop = T]
                mat[(6*(period-1))+i, 1] <- time
                mat[(6*(period-1))+i,2:7] <- round(sumstan_fn(colSums(prj_arr)), -2)
            }
        } else {
            for (i in 1:length(prj_arr_list)){
                prj_arr <- prj_arr_list[[i]][,, 1:period, drop = F]
                prj_arr <- rowSums(prj_arr, dims = 2)
                mat[(6*(period-1))+i,1] <- time
                mat[(6*(period-1))+i,2:7] <- round(sumstan_fn(colSums(prj_arr)), -2)
            }
        }
    }
    return(mat)
}

# summarize total // revised version
sum_arr_fn_rev <- function(prj_arr_list, pyear0 = "prj_2045"){

    sumstan_fn <- function(x){
        return(c(
            mean(x),
            median(x),
            quantile(x, probs = c(.1, .9)),
            quantile(x, probs = c(.05, .95))
        ))
    }

    mat <- matrix(
        data = NA,
        nrow = 6,
        ncol = 6,
        dimnames= list(
            names(prj_arr_list),
            c("mean","median","lb80","ub80","lb90","ub90")
        )
    )
    if (pyear0 != "cumulative"){
        for (i in 1:nrow(mat)){
            prj_arr <- prj_arr_list[[i]][,,dimnames(prj_arr_list[[i]])[[3]] == pyear0]
            mat[i,] <- round(sumstan_fn(colSums(prj_arr)), -2)
        }
    } else {
        for (i in 1:nrow(mat)){
            prj_arr <- rowSums(prj_arr_list[[i]], dims = 2)
            mat[i,] <- round(sumstan_fn(colSums(prj_arr)), -2)
        }
    }
    return(mat)
}


# summarize change in migration // alt version
sum_arrD_fn_CumTrends <- function(prj_arr_list){

    sumstan_fn <- function(x){
        return(c(
            mean(x),
            median(x),
            quantile(x, probs = c(.1, .9)),
            quantile(x, probs = c(.05, .95))
        ))
    }
    
    for (year0 in seq(2010,2045, 5)){

        slice <- paste0("prj_", year0)
        mat <- matrix(
            data = NA,
            nrow = 2,
            ncol = 7,
            dimnames= list(
                paste0(names(prj_arr_list)[2:3],"D"),
                c("yearE","mean","median","lb80","ub80","lb90","ub90")
            )
        )

        prj_arr_Init <- prj_arr_list[[1]][,,slice]
        mat[,1] <- year0+5
        for (i in 1:nrow(mat)){
            prj_arr <- prj_arr_list[[i+1]][,,slice]
            prj_arrD <- prj_arr - prj_arr_Init
            mat[i,2:7] <- round(sumstan_fn(colSums(prj_arrD)),-2)
        }

        if (year0==2010){
            outMat <- mat
        } else {
            outMat <- rbind(outMat, mat)
        }
        rm(mat)
    }
    return(outMat)
}

# summarize change in migration // revised version
sum_arrD_period_fn_rev <- function(prj_arr_list, level = T, cumulative = F){

    sumstan_fn <- function(x){
        return(c(
            mean(x),
            median(x),
            quantile(x, probs = c(.1, .9)),
            quantile(x, probs = c(.05, .95))
        ))
    }

    mat <- matrix(
        data = NA,
        nrow = 2*8, # 8 = number of periods
        ncol = 6 + 1, # 1: year column
        dimnames= list(
            rep(paste0(names(prj_arr_list)[2:3],"D"), 8),
            c("year","mean","median","lb80","ub80","lb90","ub90")
        )
    )
    if (cumulative != T){
        for (period in 1:8){
            time = 2015+((period-1)*5)
            prj_arr_Init <- prj_arr_list[[1]][,, period, drop = T]
            for (i in 1:(length(prj_arr_list)-1)){
                prj_arr <- prj_arr_list[[i+1]][,, period, drop = T]
                prj_arrD <- prj_arr - prj_arr_Init
                prj_arrD <- `if`(level == F, prj_arrD/prj_arr_Init, prj_arrD)
                mat[(2*(period-1))+i, 1] <- time
                mat[(2*(period-1))+i, 2:7] <- round(sumstan_fn(colSums(prj_arrD)),-2)
            }
        }
    } else {
        for (period in 1:8){
            time = 2015+((period-1)*5)
            prj_arr_Init <- prj_arr_list[[1]][,, 1:period, drop = F]
            prj_arr_Init <- rowSums(prj_arr_Init, dims = 2)
            for (i in 1:(length(prj_arr_list)-1)){
                prj_arr <- prj_arr_list[[i+1]][,, 1:period, drop = F]
                prj_arr <- rowSums(prj_arr, dims = 2)
                prj_arrD <- prj_arr - prj_arr_Init
                prj_arrD <- `if`(level == F, prj_arrD/prj_arr_Init, prj_arrD)
                mat[(2*(period-1))+i, 1] <- time
                mat[(2*(period-1))+i, 2:7] <- round(sumstan_fn(colSums(prj_arrD)),-2)
            }
        }
    }
    return(mat)
}

# summarize change in migration // revised version
sum_arrD_fn_rev <- function(prj_arr_list, level = TRUE, pyear0 = "prj_2045"){

    sumstan_fn <- function(x){
        return(c(
            mean(x),
            median(x),
            quantile(x, probs = c(.1, .9)),
            quantile(x, probs = c(.05, .95))
        ))
    }

    mat <- matrix(
        data = NA,
        nrow = 2,
        ncol = 6,
        dimnames= list(
            paste0(names(prj_arr_list)[2:3],"D"),
            c("mean","median","lb80","ub80","lb90","ub90")
        )
    )
    if (pyear0 != "cumulative"){
        prj_arr_Init <- prj_arr_list[[1]][,,dimnames(prj_arr_list[[1]])[[3]] == pyear0]
        for (i in 1:nrow(mat)){
            prj_arr <- prj_arr_list[[i+1]][,,dimnames(prj_arr_list[[i+1]])[[3]] == pyear0]
            prj_arrD <- prj_arr - prj_arr_Init
            prj_arrD <- `if`(level == F, prj_arrD/prj_arr_Init, prj_arrD)
            mat[i,] <- round(sumstan_fn(colSums(prj_arrD)),-2)
        }
    } else {
         prj_arr_Init <- rowSums(prj_arr_list[[1]], dims = 2)
        for (i in 1:nrow(mat)){
            prj_arr <- rowSums(prj_arr_list[[i+1]], dims = 2)
            prj_arrD <- prj_arr - prj_arr_Init
            prj_arrD <- `if`(level == F, prj_arrD/prj_arr_Init, prj_arrD)
            mat[i,] <- round(sumstan_fn(colSums(prj_arrD)),-2)
        }
    }
    return(mat)
}
