rm(list=ls())
library(diversitree)
source("supporting-functions.R")

# estimate rate parameter for multiple trees
treenum <- 100 # how many trees to simulate
treesize <- 500 # how many taxa in each tree
p_bias <- 0.5
sub <- seq(0, treesize-(treesize/10), by=treesize/10)
sub

# res <- sapply(sub, function(x) get_sim_pars(treesize=treesize, pars=c(1, 1), drop=x))
res <- list()
for(i in seq_along(sub)){
  res[[i]] <- sapply(c(1:treenum), function(x) get_sim_pars(treesize=treesize, pars=c(1, 1), drop=sub[i]))
}

# the mean of all estimates, for each tier of tree subsampling
mean_res <- lapply(res, mean)
mean_res

res_diff <- res # easy way start with a list that is the same size and shape as res

# a list of the difference between each estimate and true parameter
for(i in seq_along(sub)) {
  for(j in 1:treenum){
    res_diff[[i]][j] <- res[[i]][j] - 1
  }
}

mean_diff <- lapply(res_diff, mean)
plot(rev(sub), rev(mean_diff))

sd_diff <- lapply(res_diff, sd)
plot(sub, sd_diff)





# use a range of rates to simulate trees, and produce estimates for them
pars_list <- list()
pars <- seq(0.02, 1, by=0.02)
for (i in seq_along(pars)){
  pars_list[[i]] <- c(pars[i], 1)
}

res2 <- lapply(pars_list, function(x) get_sim_pars(100, x))

# extract 1st element (q01) from each set of pars
pars_q01 <- lapply(pars_list, '[[', 1)

# plot distribution of differences between estimate and true parameter
plot(density(as.numeric(res2)-as.numeric(pars_q01)))






