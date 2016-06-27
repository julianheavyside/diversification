rm(list=ls())
dev.off()

library(diversitree)

source("supporting-functions.R")

# estimate rate parameter for multiple trees
qs <- c(1, 1)
res <- sapply(c(1:20), function(x) get_sim_pars(treesize=500, pars=qs))
mean(res)
sd(res)

plot(density(res), main="", xlab="Estimate", ylab="Density")


# use a range of rates to simulate trees, and produce estimates for them
pars_list <- list()
pars <- seq(0.02, 1, by=0.02)
for (i in 1:length(pars)){
  pars_list[[i]] <- c(pars[i], 1)
}

res2 <- lapply(pars_list, function(x) get_sim_pars(100, x))

# extract 1st element (q01) from each set of pars
pars_q01 <- lapply(pars_list, '[[', 1)

# plot distribution of differences between estimate and true parameter
plot(density(as.numeric(res2)-as.numeric(pars_q01)))






