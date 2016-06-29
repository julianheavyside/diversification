rm(list=ls())
library(diversitree)
source("supporting-functions.R")

# estimate rate parameter for multiple trees
treenum <- 10 # how many trees to simulate
treesize <- 100 # how many taxa in each tree
p_bias <- 0.5
sub <- seq(0, treesize-10, by=10)
sub

# res <- sapply(sub, function(x) get_sim_pars(treesize=treesize, pars=c(1, 1), drop=x))
res <- list()
for(i in seq_along(sub)){
  res[[i]] <- sapply(c(1:treenum), function(x) get_sim_pars(treesize=treesize, pars=c(1, 1), drop=sub[i]))
}

res_diff <- res # 

# a list of the difference between each estimate and true parameter
for(i in seq_along(sub)) {
  for(j in 1:treenum){
    res_diff[[i]][j] <- res[[i]][j] - 1
  }
}


mean_diff <- list()
mean_diff <- lapply(res, mean)

plot(sub, mean_diff)







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






