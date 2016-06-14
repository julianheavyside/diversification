## working through examples in Diversitree documentation
library(diversitree)

## Constant-rate birth-death models

set.seed(2)
# speciation rate, lambda = 0.1, extinction rate, mu = 0.03
phy <- tree.bd(c(.1, .03), max.taxa=100)

# construct a likelihood function
lik <- make.bd(phy)

argnames(lik)
# takes a vector of two parameters, and returns the log likelihood of the parameters following Nee et al. 1994

lik(c(.1, .03))

# can accept one addition argument: to disable conditioning on survival (likelihood is conditional on tow lineages surviving to present)
lik(c(.1, .03), condition.surv=FALSE)

plot(phy, no.margin=TRUE, show.tip.label=FALSE)

fit <- find.mle(lik, c(.1, .03), method="subplex")

coef(fit)

logLik(fit)

fit$lnLik

lik.yule <- constrain(lik, mu ~ 0)

fit.yule <- find.mle(lik.yule, coef(fit)[1], method="subplex")

anova(fit, yule=fit.yule)

# MCMC to perform Bayesian analysis

set.seed(1)
samples <- mcmc (lik, fit$par, nsteps=1000, w=.1, print.every=0)

set.seed(1)
phy.sub <- drop.tip(phy, sample(100, 25))

lik.sub <- make.bd(phy.sub, sampling.f=75/100)
fit.sub <- find.mle(lik.sub, c(.1, .03), method="subplex")
coef(fit.sub)

lik.sub.yule <- constrain(lik.sub, mu ~ 0)
fit.sub.yule <- find.mle(lik.sub.yule, coef(fit.sub)[1], method="subplex")
anova(fit.sub, yule=fit.sub.yule)

samples$r <- samples$lambda - samples$mu
col <- c("#eaab00", "#004165", "#618e02")
profiles.plot(samples[c("lambda", "mu", "r")], col.line=col, las=1, alpha=.75, legend.pos="topright")
abline(v=c(.1, .03, .07), col=col, lty=2)



