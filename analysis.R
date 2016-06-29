rm(list=ls())
library(diversitree)
source("supporting-functions.R")

# estimate rate parameter for multiple trees
treesize <- 100
sub <- seq(0, treesize-10, by=10)

# res <- sapply(sub, function(x) get_sim_pars(treesize=treesize, pars=c(1, 1), drop=x))
res <- list()
for(i in seq_along(sub)){
  sapply(c(1:10), function(x) get_sim_pars(treesize=treesize, pars=c(1, 1), drop=sub[i]))
}

mean(res)

sd(res)

plot(density(res), main="", xlab="Estimate", ylab="Density")


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






