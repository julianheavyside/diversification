# FUNCTION PARAMETERS USED
# n: number of trees, Num
# tips: number of taxa in tree, Num
# div.pars: simulate trees with given lambda and mu, c(Num, Num)
# q.pars: simulate character states with given transition rates q01 and q10, c(Num, Num)
# model: simulate states under given model of character evolution, Char
# bias: simulate missing data with subsampling bias relative to state 1, Num 0-1

# so far, this makes n identical trees, but at least make.mk2 can handle them
# not sure why 
# fn.for.tree.bd <- function(n=100, tips=100, div.pars=c(1, 0)){
#   t <- list()
#   for(i in 1:n){
#     set.seed(0)
#     t[[i]] <- tree.bd(pars=div.pars, max.taxa=tips)
#   }
#   t
# }

# this makes unique trees, but make.mk2 doesn't like them
fn.for.trees <- function(n=100, tips=100, div.pars=c(1, 0)){
  trees(pars=div.pars, type="bd", n=n, max.taxa=tips)
}

# evolve binary trait states for each tree, given a trait evo model and transition rates
fn.for.sim.character <- function(trees, q.pars=c(0, 0), model="mk2"){
  s <- list()
  for(i in 1:length(trees)){
    set.seed(1)
    s[[i]] <- sim.character(trees[[i]], pars=q.pars, model=model)
  }
  s
}

# build likelihood function for each tree
fn.for.make.mk2 <- function(trees, states){
  l <- list()
  for(i in 1:length(trees)){
    l[[i]] <- make.mk2(trees[[i]], states[[i]])
  }
  l
}

# run ML for each tree and store q01 parameter estimate
fn.for.find.mle <- function(liks, x.init=c(.1, .1)){
  f <- list()
  r <- list()
  for(i in 1:length(trees)){
    f[[i]] <- find.mle(liks[[i]], x.init=x.init)
    r[[i]] <- f[[i]]$par[1]
  }
  r
}


# a failed attempt to call all above functions at once
# q.estimates <- function(n=100, tips=100, div.pars=c(1, 0), q.pars=c(0, 0), model="mk2", ...){
#   trees <- fn.for.tree.bd(n=n, tips=tips)
#   # trees
#   states <- fn.for.sim.character(trees=trees, q.pars=q.pars)
#   # states
#   liks <- fn.for.make.mk2(trees=trees, states=states)
#   # liks
#   rates <- fn.for.find.mle(liks=liks)
# }

# produce bias to be used for subsampling
bias <- function(d, p=.5){
  as.numeric(ifelse(d==1, p, 1-p))
}

# subsample each tree in increments of 10%
fn.for.MSE <- function(x){
  
}

# plot
fn.for.plot <- function(x){
  
}


# the function bias() takes in a vector of character states, and a sampling probability for tips with state=1, and returns a vector of probabilities to pass to base function sample() 'prob' parameter
# p = probability of dropping a tip
# e.g. for bias(states, 0.6), Pr[dropping taxa with state=1] = 0.6
bias <- function(d, p=.5){
  as.numeric(ifelse(d==1, p, 1-p))
}

