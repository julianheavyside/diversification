library(diversitree)

## produce bias to be used for subsampling
## takes a list of binary numbers, and returns a list of probabilities to pass to sample()
## 1 is assigned p, and 0 is assigned 1-p, allowing differential sampling probability dependent on tip state
samp_bias <- function(d, p=.5){
  as.numeric(ifelse(d==1, p, 1-p))
}

## simulates a tree with n taxa, distributes simulated character data throughout the tree, and returns a list with the tree, the data, and the rate parameter values used in the simulation
simulate_mk2 <- function(n, r=c(0.1,0.1)){
  t <- tree.bd(c(1,0), max.taxa=n)
  d <- sim.character(t,r, model="mk2")
  if (length(unique(d)) == 1){ 
    return(NULL) # condition for rejecting single-state trees
  } else {
    return(list(t=t, d=d, r=r))
  }
}

## takes in a list produced by simulate_mk2, builds a likelihood for the data and the tree, caluculates a maximum likelihood estimate for the rate parameters, and stores them in a named list along with the true parameters and the number of taxa in the tree
fit_mk2 <- function(x){
  l <- make.mk2(x$t,x$d)
  f <- find.mle(l, x.init=x$r)
  out <- c(Ntip(x$t), x$r[1], x$r[2],
           f$par["q01"], f$par["q10"])
  names(out) <- c("n", "sim_q01","sim_q10", "est_q01", "est_q10")
  out
}

fit_bisse <- function(x){
  l <- make.bisse(x$t,x$d, sampling.f = c(0.5, 0.5))
  l_mk2 <- constrain(lik, lamda1~lambda0, mu1~0, mu0~0)
  f <- find.mle(l_mk2, x.init=c(x$r, 1))
  out <- c(Ntip(x$t), x$r[1], x$r[2],
           f$par["q01"], f$par["q10"])
  names(out) <- c("n", "sim_q01","sim_q10", "est_q01", "est_q10")
  out
}

## subsamples the tree taxa, based on a given sampling fraction, drops them, and builds a new tree, with the associated data from the full tree, and lists them 
sample_phydat <- function(x, s, b){
  n <- Ntip(x$t)
  prob <- samp_bias(x$d, p=b)
  drop <- sample(x$t$tip.label, round((1-s)*n), prob=prob)
  new_t <- drop.tip(x$t, tip=drop)
  new_d <- x$d[new_t$tip.label]
  list(t=new_t, d=new_d, r=x$r)
}

## Simulate i datasets, drawing parameters from a distribution
## Subsample with bias, and use a rejection algorithm to get rid of failures (trees with only one character state)
simulate_mk2_rsamp <- function(i, n, s, r=c(0.1,0.1), b=0.5){
  out <- data.frame()
  while(1){
    dat  <- simulate_mk2(n, r)
    if (!is.null(dat)) {
      sam  <- sample_phydat(dat, s, b)
      if (length(unique(sam$d)) !=  1) {
        fit  <- fit_mk2(dat)
        sfit <- fit_mk2(sam)
        ## Collect and clean output
        res <- c(fit, b, s, sfit[c(1, 4, 5)]) #only summarize new info
        out <- rbind(out, res)
      }
    }
    if (nrow(out) == i)
      break()
  }
  names(out) <- c("n", "sim_q01","sim_q10", "est_q01", "est_q10", "bias", "samp_f", "n_samp", "est_q01_samp", "est_q10_samp")
  out
}
