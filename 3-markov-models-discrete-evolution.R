# 3: Markov models of discrete character evolution

set.seed(2)
phy <- tree.bd(c(.1, .03), max.taxa=50)

set.seed(1)
states <- sim.character(phy, c(.1, .2), x0=0, model="mk2")

lik.mk2 <- make.mk2(phy, states)
fit.mk2 <- find.mle(lik.mk2, c(.1, .1), method="subplex")
coef(fit.mk2)

lik.mk1 <- constrain(lik.mk2, q10 ~ q01)
fit.mk1 <- find.mle(lik.mk1, .1, method="subplex")
anova(fit.mk2, mk1=fit.mk1)

# 3.1: Drawing sampled with MCMC
plot(phy, show.tip.label=FALSE, no.margin=TRUE)
col <- c("#004165", "#eaab00")
tiplabels(col=col[states+1], pch=19, adj=1)
nodelabels(col=col[attr(states, "node.state")+1], pch=19)

prior.exp <- make.prior.exponential(10)

set.seed(1)
samples <- mcmc(lik.mk2, c(.1, .1), nsteps=5000, prior=prior.exp, w=.1, print.every=0)
samples <- subset(samples, i > 500)

mean(samples$q01 > samples$q10)
profiles.plot(samples[c("q01", "q10")], col.line=col, las=1, legend.pos="topright")
abline(v=c(.1, .2), col=col)



