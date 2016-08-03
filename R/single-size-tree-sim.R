rm(list=ls())
library(diversitree)
source("supporting-functions.R")

dat <- simulate_mk2(n=100)
sam <- sample_phydat(x=dat, s=0.75)

fit <- fit_mk2(dat)
sfit <- fit_mk2(sam)
res <- c(fit, 0.75, sfit[c(1, 4, 5)])
names(res) <- c("n", "sim_q01","sim_q10", "est_q01", "est_q10", "samp_f", "n_samp", "est_q01_samp", "est_q10_samp")


#
# trees of single size ----------------------------------------------------
##############################################################
# ability to estimate rate parameters when data is missing
# for trees of a single size
##############################################################

## estimate rate parameter for multiple trees
treenum <- 3000 # how many trees to simulate
treesize <- 500 # how many taxa in each tree
p_bias <- 0.5
sub <- seq(0, treesize-(treesize/10), by=treesize/10)
pars <- c(2, 1) # true rates for simulating trees

## 
res <- list()
for(i in seq_along(sub)){
  res[[i]] <- sapply(c(1:treenum), function(x) get_sim_pars(treesize=treesize, pars=pars, drop=sub[i]))
}

## the mean of all estimates, for each tier of tree subsampling
mean_res <- lapply(res, mean)

se <- lapply(res, standard.error)

rev_sub <- sub[rev(order(sub))]

par(mar = c(5, 6, 1, 1))
## adjust ylim to accomodate large error bars
plot(rev_sub, mean_res, ylim = c(0, range(mean_res)[2] + max(unlist(se))), xlab = "Number of taxa with known character state information", ylab = "Mean estimated rate of character evolution")

## a horizontal line for the true rate of character evolution (use to make trees)
abline(h = pars[1], lty = 5)

## add standard error bars about each mean
## plot flat arrows above and below each mean, each as long as the SE
for(i in seq_along(rev_sub)){
  arrows(rev_sub[i], mean_res[[i]] - se[[i]], 
         rev_sub[i], mean_res[[i]] + se[[i]], 
         length = .1, angle = 90, code = 3)
}

