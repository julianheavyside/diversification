rm(list=ls())
library(diversitree)

# not actually using anything yet
# source("supporting-functions.R")

trees <- lapply(1:10, function(i) 
  tree.bd(pars=c(1, 0), max.taxa=200)) # 10 trees

states <- lapply(1:length(trees), function(i) 
  sim.character(trees[[i]], pars=c(1, 1), model="mk2"))

# trees_states <- matrix(c(trees, states), nrow=length(trees), ncol=2)

liks <- lapply(1:length(trees), function(i)
  make.mk2(trees[[i]], states[[i]]))

fits <- lapply(1:length(trees), function(i)
  find.mle(liks[[i]], x.init=c(1, 1)))

res <- lapply(1:length(trees), function(i)
  fits[[i]]$par[1])
res










# simulate "n" trees with "tips" taxa
trees1 <- fn.for.tree.bd(n=15, tips=50)

trees2 <- fn.for.trees(n=15, tips=50)

# evolve binary trait states for each tree, given a trait evo model and transition rates
states <- fn.for.sim.character(trees1, q.pars=c(.1, .2))

# build likelihood function for each tree
liks <- fn.for.make.mk2(trees=trees, states=states)

# run ML for each tree and store q01 parameter estimate
rates <- fn.for.find.mle(liks=liks)
rates


  



























# tree size
# different q values
# unbiased sampling
# biased sampling
# different degrees of bias
# mean squared error

# generate some trees with character states
# subsample each tree to different sampling fraction
# estimate rates for each tree
# cases when q01 = q10, and when q01 > q10
# plot diff between estimate and true parameter as a function of sampling fraction






# xylo_sim <- function(n=100, tips=100, div.pars=c(1, 0), q.pars=c(0, 0), model="mk2", bias=NULL, ...){
#   # generates n trees with t tips with character states
#   set.seed(2)
#   for(i in 1:length(n)){
#     
#   }
#   
#   trees <- make.bd(pars=div.pars, max.taxa=tips)
#   
#   # simulate binary character states for each tree, using Markov model and q transition rates
#   states <- list()
#   for(i in 1:length(trees)){
#     states[i] <- sim.character(trees[i], pars=c(q01, q10), model=model)
#   }
#   
#   tree_states <- matrix(NA, nrow=length(trees), ncol=6)
#   # subsamples from 10% to 90% with specified bias
#   # fn for bias
#   
#   # estimates rates for each tree
#   
# }
# 
# 
# t <- trees(pars=c(1, 0), type="bd", n=4, max.taxa=30)
# t1 <- tree.bd(pars=c(1, 0), max.taxa=30)
# set.seed(2)
# states <- list()
# for(i in 1:length(t)){
#   s <- sim.character(tree=t1, pars=c(.1, .1), model="mk2")
#   states <- c(states, s)
# }
# 
# sapply(1:length(t), function(i) plot(t[[i]], show.tip.label=FALSE))

