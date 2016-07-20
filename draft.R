rm(list=ls())
library(diversitree)
source("supporting-functions.R")

dat <- simulate_mk2(n=100)
sam <- sample_phydat(x=dat, s=0.75)

fit <- fit_mk2(dat)
sfit <- fit_mk2(sam)
res <- c(fit, 0.75, sfit[c(1, 4, 5)])
names(res) <- c("n", "sim_q01","sim_q10", "est_q01", "est_q10", "samp_f", "n_samp", "est_q01_samp", "est_q10_samp")

## MK models

## Run this over a bunch of tree sizes and sampling
## Get 100 results at each at each

t_size <- seq(50, 1000, 50)
samp <- seq(1, .25, -.05)


## just loop it
res <- data.frame()
for (i in 1:length(t_size)){
  for (j in 1:length(samp)){
    print(c(i,j))
    out <- simulate_mk2_rsamp(100, t_size[i], samp[j])
    res <- rbind(res,out)
  }
}