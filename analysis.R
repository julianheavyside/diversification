rm(list=ls())
## for MK models

# simulation --------------------------------------------------------
## estimate rates for trees of different sizes and different levels of subsampling
## run over a range of conditions:

## simulate over range of tree sizes
t_size <- seq(100, 1000, 100)

## ... over range of sampling fractions
samp <- seq(1, .25, -.1)

## ... over range of parameter (q01 and q10) values
## roundabout way to make a long list of parameter combinations (for situations when q01=q10, when they aren't equal, when they are large or small, etc) 
## multidimensional parameter space
pars_list_diff <- list()
pars_list_same <- list()
pars <- seq(0.1, 1, by=0.1)
rev_pars <- rev(pars)

## different values for q01 and q10
for (i in seq_along(pars)){
  pars_list_diff[[i]] <- c(pars[i], rev_pars[i]) 
}

## same values for q01 and q10
for (i in seq_along(pars)){
  pars_list_same[[i]] <- c(pars[i], pars[i])
}

## join both lists to make a full pars_list over a range of values, for situations with same and different values
pars_list <- c(pars_list_diff, pars_list_same) 

## ... over a range of biases towards dropping tips with state=1
bias <- seq(0.3, 0.7, 0.1)

## calculate number of observations produced by the simulation
print(length(t_size) * length(samp) * length(pars_list) * length(bias) * 100) #interations

## now we are ready to start the simulation
## just loop it. Get 100 results at each
source("simulation-functions.R")

res <- data.frame() ## store our simulated data in a data.frame
for (j in seq_along(t_size)){ ## simulate over range of tree sizes
  for (k in seq_along(samp)){ ## ...over range of sampling fractions
    for (l in seq_along(pars_list)){ ## ...over range of parameter values
      for (m in seq_along(bias)){ ## ...over a range of biases towards state=1
        print(c(j,k,l,m))
        out <- simulate_mk2_rsamp(i=100, n=t_size[j], s=samp[k], r=pars_list[[l]], b=bias[m])
        res <- rbind(res,out)}
    }
  }
}


## save simulated dataset as a csv in the working dir
write.csv(res, "res.csv")

# tidying ---------------------------------------------------------
source("tidying-functions.R")

## load simulated data set, using speedy readr magic
res <- read_csv("res.csv", col_types = "iiddddddidd")
res <- dplyr::tbl_df(res[-1]) # excel added a index column, so remove it
glimpse(res)

## select desired levels of tree size, bias, and rate parameters to filter data for plotting with facets
sub <- sub.res(res)
  
ggplot(sub, aes(samp_f, est_q01_samp, color = bias)) +
  geom_point() +
  facet_grid(sim_q01 ~ n, scales = "free_y")

# plotting ----------------------------------------------------------------
source("plotting-functions.R")





