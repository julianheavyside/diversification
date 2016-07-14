# produce bias to be used for subsampling
bias <- function(d, p=.5){
  as.numeric(ifelse(d==1, p, 1-p))
}

# simulate a tree, and then estimate the rate parameter for that tree
get_sim_pars <- function(treesize, pars=c(1,1), drop){
  t <- tree.bd(pars=c(1,0), max.taxa=treesize)
  d <- sim.character(t, pars=pars, model="mk2")
  prob <- bias(d, p=p_bias)
  to_drop <- sample(t$tip.label, size=drop)
  new_t <- drop.tip(t, tip=to_drop)
  new_d <- sim.character(new_t, pars=pars, model="mk2")
  lik <- make.mk2(new_t, new_d)
  as.numeric(find.mle(lik, x.init=pars)$par[1])
}

# standard error function
standard.error <- function(x){
  sqrt(var(x)/length(x))
}


