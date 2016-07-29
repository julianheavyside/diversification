## produce bias to be used for subsampling
samp_bias <- function(d, p=.5){
  as.numeric(ifelse(d==1, p, 1-p))
}

## Simulate i datasets, drawing parameters from a distribution

simulate_mk2 <- function(n, r=c(0.1,0.1)){
  t <- tree.bd(c(1,0), max.taxa=n)
  d <- sim.character(t,r, model="mk2")
  if (length(unique(d)) == 1){ 
    return(NULL) # condition for rejecting single-state trees
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

sample_phydat <- function(x, s, b){
  n <- Ntip(x$t)
  prob <- samp_bias(x$d, p=b)
  drop <- sample(x$t$tip.label, round((1-s)*n), prob=prob)
  new_t <- drop.tip(x$t, tip=drop)
  new_d <- x$d[new_t$tip.label]
  list(t=new_t, d=new_d, r=x$r)
}

## Using rejection algorithm to get rid of failures
## Subsample without bias
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
