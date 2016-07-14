rm(list=ls())
library(diversitree)
source("supporting-functions.R")

# estimate rate parameter for multiple trees
treenum <- 1000 # how many trees to simulate
treesize <- 500 # how many taxa in each tree
p_bias <- 0.5
sub <- seq(0, treesize-(treesize/10), by=treesize/10)

# 
res <- list()
for(i in seq_along(sub)){
  res[[i]] <- sapply(c(1:treenum), function(x) get_sim_pars(treesize=treesize, pars=c(2, 1), drop=sub[i]))
}

# the mean of all estimates, for each tier of tree subsampling
mean_res <- lapply(res, mean)

res_diff <- res # easy way start with a list that is the same size and shape as res

# a list of the difference between each estimate and true parameter
for(i in seq_along(sub)) {
  for(j in 1:treenum){
    res_diff[[i]][j] <- res[[i]][j] - 1
  }
}

# get the average difference
mean_diff <- lapply(res_diff, mean)
rev_sub <- sub[rev(order(sub))]

# standard error for diff between estimates and pars, for each level of sub
se_diff <- lapply(res_diff, standard.error)

# adjust ylim to accomodate large error bars
plot(rev_sub, mean_diff, ylim = c(0, range(mean_diff)[2] + max(unlist(se_diff))))

# add standard error bars about each mean
# plot flat arrows above and below each mean, each as long as the SE
for(i in seq_along(rev_sub)){
  arrows(rev_sub[i], mean_diff[[i]] - se_diff[[i]], 
         rev_sub[i], mean_diff[[i]] + se_diff[[i]], 
         length = .1, angle = 90, code = 3)
}
