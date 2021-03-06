rm(list=ls())
source("mk2-simulation-functions.R")
## for MK models

## Given a range of parameters, including treesize, character transition rates, and bias, the simulation builds a tree, assigns a binary character state to each tip, builds a likelihood function for the tree given the state data, produces a maximum likelihood estimate, and returns estimates of the rates of character transition rate.

## It also subsamples each tree, with and without bias, and produces estimates of the same rates for a new tree that is missing state data

## The simulation stores the rate estimates as well as the known parameters used to simulate the tree, and accumulates results from a wide array of parameter values in one dataset


## simulate over range of tree sizes
t_size <- c(200, 400, 600, 800)
t_size

## ... over range of sampling fractions
samp <- seq(1, .25, -.1)
samp

## ... over range of parameter (q01 and q10) values
## roundabout way to make a long list of parameter combinations (for situations when q01=q10, when they aren't equal, when they are large or small, etc) 
## multidimensional parameter space
pars_list_diff <- list()
pars_list_same <- list()
pars <- c(0.1, 0.25, 0.5, 0.75, 1) # can't use rate=0; trees fail
rev_pars <- rev(pars)
pars

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
pars_list <- pars_list[-8] # don't need two sets of q01=q10=0.5, so remove one (see list before and after to understand)
pars_list

## ... over a range of biases towards dropping tips with state=1
bias <- c(0.3, 0.5, 0.7)
bias

## calculate number of observations produced by the simulation
print(length(t_size) * length(samp) * length(pars_list) * length(bias) * 100)

## now we are ready to start the simulation
## just loop it. Get 100 results at each
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

## store simulation results as a data frame
saveRDS(res, file = "res.Rda")

