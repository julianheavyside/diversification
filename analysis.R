rm(list=ls())
library(diversitree)
source("supporting-functions.R")

# estimate rate parameter for multiple trees
treenum <- 30 # how many trees to simulate
treesize <- 500 # how many taxa in each tree
p_bias <- 0.5
sub <- seq(0, treesize-(treesize/10), by=treesize/10)
pars <- c(2, 1) # true rates for simulating trees

# 
res <- list()
for(i in seq_along(sub)){
  res[[i]] <- sapply(c(1:treenum), function(x) get_sim_pars(treesize=treesize, pars=pars, drop=sub[i]))
}

# the mean of all estimates, for each tier of tree subsampling
mean_res <- lapply(res, mean)

se <- lapply(res, standard.error)

par(mar = c(5, 6, 1, 1))
# adjust ylim to accomodate large error bars
plot(rev_sub, mean_res, ylim = c(0, range(mean_res)[2] + max(unlist(se))), xlab = "Number of taxa with known character state information", ylab = "Mean estimated rate of character evolution")

# a horizontal line for the true rate of character evolution (use to make trees)
abline(h = pars[1], lty = 5)

# add standard error bars about each mean
# plot flat arrows above and below each mean, each as long as the SE
for(i in seq_along(rev_sub)){
  arrows(rev_sub[i], mean_res[[i]] - se[[i]], 
         rev_sub[i], mean_res[[i]] + se[[i]], 
         length = .1, angle = 90, code = 3)
}


###
# Matt's example code from Issue #2

## MK models

## Simulate i datasets, drawing parameters from a distribution

simulate_mk2 <- function(n, r=c(0.1,0.1)){
  t <- tree.bd(c(1,0), max.taxa=n)
  d <- sim.character(t,r, model="mk2")
  if (length(unique(d)) == 1){
    return(NULL)
  } else {
    return(list(t=t, d=d, r=r))
  }
}

fit_mk2 <- function(x){
  l <- make.mk2(x$t,x$d)
  f <- find.mle(l, x.init=x$r)
  out <- c(Ntip(x$t), x$r[1], x$r[2],
           f$par["q01"], f$par["q10"])
  names(out) <- c("n", "sim_q01","sim_q10", "est_q01", "est_q10")
  out
}

sample_phydat <- function(x, s){
  n     <- Ntip(x$t)
  drop  <- sample(x$t$tip.label, round((1-s)*n))
  new_t <- drop.tip(x$t, tip=drop)
  new_d <- x$d[new_t$tip.label]
  list(t=new_t, d=new_d, r=x$r)
}

## Using rejection algorithm to get rid of failures
## Subsample without bias
simulate_mk2_rsamp <- function(i, n, s, r=c(0.1,0.1)){
  out <- data.frame()
  while(1){
    dat  <- simulate_mk2(n, r)
    if (!is.null(dat)) {
      sam  <- sample_phydat(dat, s)
      if (length(unique(sam$d)) !=  1) {
        fit  <- fit_mk2(dat)
        sfit <- fit_mk2(sam)
        ## Collect and clean output
        res <- c(fit, s, sfit)
        res <- unique(res)
        out <- rbind(out, res)
      }
    }
    if (nrow(out) == i)
      break()
  }
  
  names(out) <- c("n", "sim_q01","sim_q10", "est_q01", "est_q10", "samp_f", "n_samp", "est_q01_samp", "est_q10_samp")
  out
}

## Run this over a bunch of tree sizes and sampling
## Get 100 results at each at each

t_size <- seq(50, 1000, 50)
samp <- seq(1, .25, -.05)


## just loop it
res <- data.frame()
for (i in 1:length(t_size)){
  for (j in 1:length(samp)){
    print(c(i,j))
    out <- simulate_mk2_rsamp(10, t_size[i], samp[j])
    res <- rbind(res,out)
  }
}