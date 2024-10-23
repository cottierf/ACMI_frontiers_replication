# R Utility: project future migration
# Fabien Cottier, October 2024
# ONLY LOG LINEAR MODELS


# extract random effects function
extractRanef <- function(prj_dat, fittedMod){

    # libraries
    require(dplyr)

    # function
    AdjuRanef <- function(MatIn, Mat_vecD, PosS, PosE){
        vec <- colnames(Mat_vecD)
        for (i in seq_along(vec)){
            Msub <- MatIn[,substr(colnames(MatIn),PosS, PosE) == vec[i] ]
            Msub <- apply(Msub, 2, function(x) x + Mat_vecD[,i])
            MatOut <- if (i == 1) {Msub} else {cbind(MatOut,Msub)}
        }
        return(MatOut)
    }

    # extract ranef
    theta_list <- ranef(fittedMod, summary = F) 

    # get list migration routes
    Route_list <- prj_dat  |>
        dplyr::filter(year0 == 2010) |> 
        dplyr::pull(DirRoute)

    # extract ranef
    thetaOrig <- theta_list$orig[,,1]
    thetaDest <- theta_list$dest[,,1]
    thetaDirRoute <- theta_list$DirRoute[,,1]

    # recombine ranef
    theta <- AdjuRanef(MatIn = thetaDirRoute, Mat_vecD = thetaOrig, PosS = 1, PosE = 3)
    theta <- AdjuRanef(MatIn = theta, Mat_vecD = thetaDest, PosS = 5, PosE = 7)
    theta <- theta[,colnames(theta) %in% Route_list]
    theta <- theta[,order(colnames(theta))]

    # return
    return(theta)

}


# Iterative projections on future data
Mprj_prob_fn <- function(param_list, prj_dat, prj_start, prj_end, cy_var, dis_var, SSP, histConflict = FALSE){

    # load libraries
    require(dplyr)

    # initialization
    prj_seq <- seq(prj_start, prj_end, by = 5)

    # Create out Migration prediction array
    Migr_arr <- array(
        data = NA,
        dim = c(ncol(param_list$theta), 1000, length(prj_seq)),
        dimnames = list(
          paste(
              filter(prj_dat, year0 == prj_start)$orig,
              filter(prj_dat, year0 == prj_start)$dest,
              sep = "_"
          ),
          NULL,
          paste0("prj_", prj_seq)
        )
    )
    Migr_arr <- Migr_arr[order(rownames(Migr_arr)),,]

    # Create out net Migration prediction array
    dest_vec <- sort(unique(substr(rownames(Migr_arr), 5, 7)))
    NetMigr_arr <- array(
        data = NA,
        dim = c(length(dest_vec), 1000, length(prj_seq)),
        dimnames = list(
          dest_vec,
          NULL,
          paste0("prj_", prj_seq)
        )
    )
    NetMigr_arr <- NetMigr_arr[order(rownames(NetMigr_arr)),,]

    # Define set of variable for initial and subsequent round
    basevars = c(
        "orig", "dest", "DirRoute", "year0",
        "Intercept",
        "cyields", "river_dis",
        "O_pop",
        "pwt_gdppc_ratio",
        "O_pwt_gdppc",
        "ged_fatal_N", "migr_stock", "dist",
        "year0_1995", "year0_2000", "year0_2005"
    )

    # subset number of parameters
    set.seed(6734)
    draws <- sample(1:8000, 1000)
    b <- param_list$b[draws,]
    s <- param_list$s[draws]
    theta <- param_list$theta[draws,]


    # projection 2015 to 2050
    for (i in seq_along(prj_seq)){

        # extract data for given projection sequence
        print(paste(
            "Projecting for period",
            paste(prj_seq[i],prj_seq[i]+5,sep="-"),
            "...",
            paste0("(loop ",i,"/",length(prj_seq),")")
        ))

        # prepare data for initial projection
        if (prj_seq[i] == prj_start) {
            X <- prj_dat |>
                dplyr::filter(year0 == prj_start) |>  
                dplyr::mutate(
                    Intercept = 1,
                    cyields = !!rlang::sym(cy_var),
                    river_dis = !!rlang::sym(dis_var),
                    O_pop = log(!!rlang::sym(paste0("O_pop_",SSP))),
                    pwt_gdppc_ratio = log(!!rlang::sym(paste0("gdppc_ratio_",SSP))),
                    O_pwt_gdppc = log(!!rlang::sym(paste0("O_gdppc_",SSP))),
                    ged_fatal_N = `if`(histConflict == TRUE, log(avgHistConflict+1) , 0),
                    migr_stock = log(migr_stock + 1),
                    dist = log(dist),
                    year0_1995 = 0,
                    year0_2000 = 0,
                    year0_2005 = 0
                ) |> 
                dplyr::select(all_of(basevars)) |> 
                dplyr::arrange(orig, dest)

        # prepare data for subsequent projections
        } else {

            # obtain data
            X <- prj_dat |>
                dplyr::filter(year0 == prj_seq[i]) |>  
                dplyr::mutate(
                    Intercept = 1,
                    cyields = !!rlang::sym(cy_var),
                    river_dis = !!rlang::sym(dis_var),
                    pwt_gdppc_ratio = log(!!rlang::sym(paste0("gdppc_ratio_",SSP))),
                    O_pwt_gdppc = log(!!rlang::sym(paste0("O_gdppc_",SSP))),
                    ged_fatal_N = `if`(histConflict == TRUE, log(avgHistConflict+1) , 0),
                    dist = log(dist),
                    year0_1995 = 0,
                    year0_2000 = 0,
                    year0_2005 = 1,
                    # Not logged yet
                    O_pop = !!rlang::sym(paste0("O_",SSP,"_mig0_pop")), 
                    migr_stock = migr_stock,
                ) |> 
                dplyr::select(all_of(basevars)) |> # order variables for computation
                dplyr::arrange(orig, dest)
        }

        # get predictions
        if (i == 1){ # intial rounds
            # computation fixed effects predictions
            Pred <- as.matrix(dplyr::select(X, Intercept:year0_2005)) %*% t(b)
        } else { # subsequent rounds
            # permutate draws
            set.seed(34598 * i)
            perm <- sample(1:1000, 1000)
            b <- b[perm,]
            s <- s[perm]
            theta <- theta[perm,]
            # create out matrix
            Pred <- matrix(NA, nrow = ncol(param_list$theta), ncol = 1000)
            system.time(for (j in 1:1000){
                # add previous year net migration to population total
                netMigr <- as_tibble(NetMigr_arr[,j , i-1]) |>
                    slice(rep(1:n(), each = n()-1)) |> # repeat each row N times
                    pull()
                Xsub <- X
                Xsub$O_pop <- dplyr::if_else(Xsub$O_pop + netMigr < 1, 1, Xsub$O_pop + netMigr) # one very large random parameter estimate for Namibia results in negative population
                Xsub$O_pop <- log(Xsub$O_pop)
                Xsub$migr_stock <- log(Mstock[,j] + 1)
                Pred[,j] <- as.matrix(Xsub[,5:16]) %*% t(b[j,, drop = F])
            })
        }
        # Add random effects
        Pred <- Pred + t(theta)
        # draws from normal distributions
        Pred <- apply(
            Pred,
            2,
            function(x) rnorm(n = length(x), mean = x, sd = s)
        )
        rownames(Pred) <- X$DirRoute
        # obtain predictions in levels
        Pred <- round(exp(Pred))
        Migr_arr[,,i] <- Pred

        # obtain total net migration for each destination country projections
        # note: ordering automatic by design
        for (k in seq_along(dest_vec)){
            MigrIn <- colSums(
                Pred[substr(rownames(Pred), 5, 7) == dest_vec[k], ]
            )
            MigrOut <- colSums(
                Pred[substr(rownames(Pred), 1, 3) == dest_vec[k], ]
            )
            NetMigr_arr[k, , i] <- MigrIn - MigrOut
        }

        # obtain new migr stock values
        if (i == 1){
            Mstock <- apply(Pred, 2, function(x) x + (exp(X$migr_stock) -1))
        } else {
            Mstock <- Mstock + Pred
        }
        # print(mean(Mstock["NAM_ZAF",]))
    }

    # finalize
    return(Migr_arr)
}
