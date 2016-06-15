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

# visualize the tree (watch out, it's big!) with reconstructed character states at all nodes
plot(tree, show.tip.label=FALSE, no.margin=TRUE)
col <- c("#004165", "#eaab00")
tiplabels(col=col[states+1], pch=19, adj=1) # use state, but add 1 to give 1s and 2s instead of 0s and 1s
nodelabels(col=col[attr(states, "node.state")+1], pch=19)

######
### effects of dropping data randomly from a tree
# how sensitive are parameters to varying amounts of missing data
# how does tree size affect this
# 

# simulate a tree as before
set.seed(2)
tree <- tree.bd(c(.2, .03), max.taxa=2000)
states <- sim.character(tree, pars=c(.1, .2), model="mk2") #q01=0.1, q10=0.1
length(which(states==0))

# build likelihood model and run ML analysis with initial guess
lik <- make.mk2(tree, states)
fit <- find.mle(lik, x.init=c(.1, .1))

# store parameter MLE
r01 <- fit$par[1] # MLE of q01
r10 <- fit$par[2] # MLE of q10

# build trees to represent 10, 20,..., 1980 tips missing from the original tree
# first, make a sequence to iterate the loop across various quantities of missing data
s <- seq(10, 1990, by=10)
length(s)

# use a loop to simulate and analyse many trees, each containing 10 fewer tips than the previous run
for (i in 1:length(s)){
  to_drop <- sample(tree$tip.label, size=s[i])
  new_tree <- drop.tip(tree, tip=to_drop)
  new_states <- states[new_tree$tip.label]
  new_lik <- make.mk2(new_tree, new_states)
  new_fit <- find.mle(new_lik, x.init=c(.1, .1))
  r01es <- new_fit$par[1]
  r10es <- new_fit$par[2]
  r01 <- c(r01, r01es)
  r10 <- c(r10, r10es)
}

### effects of biased sampling data from a tree
# bias in the probability of a tip being dropped due to its character state
# how sensitive are parameters to varying amounts of missing biased data
# how does tree size affect this

# taxa in state 1 have P=0.2 of being sampled
bias <- ifelse(states==1, .2, .8)
to_drop <- sample(tree$tip.label, size=s[i], prob=bias)

for (i in 1:length(s)){
  to_drop <- sample(tree$tip.label, size=s[i], prob=bias)
  new_tree <- drop.tip(tree, tip=to_drop)
  new_states <- states[new_tree$tip.label]
  new_lik <- make.mk2(new_tree, new_states)
  new_fit <- find.mle(new_lik, x.init=c(.1, .1))
  r01es <- new_fit$par[1]
  r10es <- new_fit$par[2]
  r01 <- c(r01, r01es)
  r10 <- c(r10, r10es)
}

########
set.seed(2)
tree <- tree.bd(c(.2, .03), max.taxa=40)
states <- sim.character(tree, pars=c(.1, .1), model="mk2")

to_drop <- sample(tree$tip.label, size=25)
new_tree <- drop.tip(tree, tip=to_drop)
new_states <- states[new_tree$tip.label]

bias <- ifelse(states==1, .8, .2) # P[dropping taxa in state 1]=0.8

# a function that takes in list of character states, a state on which to focus the bias, and a probability of being dropped
# e.g. for bias(states, 0, 0.6), Pr[dropping taxa with state=0] = 0.6
bias <- function(d, s=1, p=.5){
  return(ifelse(d==s, p, 1-p))
}

to_drop <- sample(tree$tip.label, size=25, prob=as.numeric(bias))
new_tree <- drop.tip(tree, tip=to_drop)
new_states <- states[new_tree$tip.label]

plot(tree, show.tip.label=FALSE, no.margin=TRUE)
col <- c("#004165", "#eaab00")
tiplabels(col=col[states+1], pch=19, adj=1) # use state, but add 1 to give 1s and 2s instead of 0s and 1s
nodelabels(col=col[attr(states, "node.state")+1], pch=19)

