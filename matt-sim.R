tree <- tree.bd(c(1, 0), max.taxa=200)
dat <- sim.character(tree, pars=c(.1, .1), model="mk2")
length(which(dat==1))

lik <- make.mk2(tree, dat)
f <- find.mle(lik, x.init=c(.1, .1))
f$par

### Simulation

t <- tree.bd(c(1, 0), max.taxa=2000)
d <- sim.character(t, pars=c(.1, .1), model="mk2")
length(which(d==0))

s <- seq(10, 1980, by=10)

lik <- make.mk2(t, d)
f <- find.mle(lik, x.init=c(.1, .1))

r <- f$par[1]

for (i in 1:length(s)){
  to_drop <- sample(t$tip.label, size=s[i])
  new_tree <- drop.tip(t, tip=to_drop)
  new_dat <- d[new_tree$tip.label]
  new_lik <- make.mk2(new_tree, new_dat)
  new_f <- find.mle(new_lik, x.init=c(.1, .1))
  res <- new_f$par[1]
  r <- c(r, res)
}

length(r)
length(s)

plot(c(0, s), r)

lik_diff <- make.mk2(t, d)
lik_same <- constrain(lik_diff, q01~q10)

f_diff <- find.mle(lik_diff, c(.1, .1))
f_same <- find.mle(like_same, .1)
anova(f_diff, f_same)
AIC(f_same)
AIC(f_diff)


for (i in 1:length(s)){
  to_drop <- sample(t$tip.label, size=s[i])
  new_tree <- drop.tip(t, tip=to_drop)
  new_dat <- d[new_tree$tip.label]
  new_lik <- make.mk2(new_tree, new_dat)
  new_lik_const <- constrain(new_lik, q01~q10)
  new_f <- find.mle(new_lik, x.init=c(.1, .1))
  sig <- anova(f_diff, f_same)
  if (sig[2,5]<=0.05){
    r <- FALSE
  } else {
    r <- TRUE
  }
    r <- c(r, res)
}

bias_sample <- function(d, bias){
  prob <- rep(NA, length(d))
  sample()
}



