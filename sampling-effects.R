rm(list=ls())
library(diversitree)
library(ape)
library(geiger)

### Effects of sampling (missing data) on parameters of models of discrete character evolution
# this focuses on continuous-time Markov models of discrete binary character evolution, which hold diversification rates constant and allow the character state of a lineage to transition from state to state
# we are specifically interested in producing MLEs for the following two parameters: q01 = transition rate from state 0 to state 1, and q10 = transition rate from state 1 to state 0
# skip to line 50 for simulations of trees with missing data

# start by simulating a tree with 2000 tips, lambda = 0.2, and mu = 0.03
set.seed(2)
tree <- tree.bd(c(.2, .03), max.taxa=2000)

# simulate a binary trait exhibiting a rate of evolution from state 0 to state 1 within a lineage of 0.1, and 0.2 for state 1 to state 0. Use a Mk2 (Markov binary character) model of character evolution
set.seed(1)
states <- sim.character(tree, c(.1, .2), model="mk2")
length(which(states==1))

# build a likelihood function based on Mk2 model (used to find likelihood of parameter values: probability of the observed distribution of character states given that the tree evolved according to a given model of character evolution and/or diversification)
lik.mk2 <- make.mk2(tree, states)

# run ML analysis using an initial parameter guess of (0.1, 0.1)
# I tried with initial guesses ranging from (0.1, 0.9) to (0.9, 0.1) and similar MLEs are produced in each case
fit.mk2 <- find.mle(lik.mk2, c(.1, .1), method="subplex")

# remember, the tree is simulated, so we know the true values of the parameters q01=0.1 and q10=0.2

# parameter estimates for q01 and q10 from the fit
coef(fit.mk2)
# q01        q10 
# 0.08399429 0.23020744 
# difference is slightly larger than the true model parameters (0.1 and 0.2)

# compare our Markov model fit to a null hypothesis model (with parameters q01 = q10) to test if the Mk fit is statistically justified

# build likelihood function based on null model of q01 = q10
lik.mk1 <- constrain(lik.mk2, q10 ~ q01) # q values constrained to be equal

# run ML analysis using intial guess of 0.1 again
fit.mk1 <- find.mle(lik.mk1, .1, method="subplex")
anova(fit.mk2, mk1=fit.mk1)
#      Df   lnLik    AIC    ChiSq   Pr(>|Chi|)   
# full  2  -26.522  57.044                     
# mk1   1  -31.095  64.191  9.1466   0.002492

# we can conclude that the asymmetric Markov model fits better; the rate of evolution from state 1 to state 0 is greater than that of state 0 to state 1






plot(tree, show.tip.label=FALSE, no.margin=TRUE)
col <- c("#004165", "#eaab00")
tiplabels(col=col[states+1], pch=19, adj=1) # use state, but add 1 to give 1s and 2s instead of 0s and 1s
nodelabels(col=col[attr(states, "node.state")+1], pch=19)
