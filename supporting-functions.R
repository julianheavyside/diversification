# simulate a tree, and then estimate the rate parameter for that tree
get_sim_pars <- function(treesize, pars=c(1,1)){
  t <- tree.bd(pars=c(1,0), max.taxa=treesize)
  d <- sim.character(t, pars=pars, model="mk2")
  lik <- make.mk2(t,d)
  as.numeric(find.mle(lik, x.init=pars)$par[1])
}

# produce bias to be used for subsampling
bias <- function(d, p=.5){
  as.numeric(ifelse(d==1, p, 1-p))
}




