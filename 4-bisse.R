# 4: Binary traits and diversification: BiSSE

# parameter orders:
# lambda0, lambda1, mu0, mu1, q01, q10

# assymetry in speciation rate: state 1 speciates at twice the rate of state 0
pars <- c(0.1, 0.2, 0.03, 0.03, 0.01, 0.01)
set.seed(4)
phy <- tree.bisse(pars, max.t=30, x0=0)
states <- phy$tip.state
head(states)

lik <- make.bisse(phy, states)
lik(pars)

p <- starting.point.bisse(phy)
p

fit <- find.mle(lik, p)

fit$lnLik

round(coef(fit), 3)

par(mar=rep(0, 4))
plot(history.from.sim.discrete(phy, 0:1), phy, col=col)
