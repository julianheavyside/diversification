rm(list=ls())
library(diversitree)

### Effects of sampling (missing data) on parameters of models of discrete character evolution
# this focuses on continuous-time Markov models of discrete binary character evolution, which hold diversification rates constant and allow the character state of a lineage to transition from state to state
# can be extended to BiSSE model
# we are specifically interested in producing MLEs for the following two parameters: q01 = transition rate from state 0 to state 1, and q10 = transition rate from state 1 to state 0


# how sensitive are parameters (q01 and q10) to varying amounts of missing data
# ability of model comparison to reject a null hypothesis
# adjust tree size, relative rates of q01 and q10, lambda and mu, etc


# set number of tree tips (tree size)
max.taxa <- 2000

# start by simulating a tree of size "max.taxa"
set.seed(2)
tree <- tree.bd(pars=c(.2, .03), max.taxa=max.taxa) # pars=c(lambda, mu)

# simulate a binary trait exhibiting a rate of evolution from state 0 to state 1 within a lineage of 0.1, and 0.2 for state 1 to state 0. Use a Mk2 (Markov binary character) model of character evolution
set.seed(1)
states <- sim.character(tree, pars=c(.1, .2), model="mk2") # pars=c(q01, q10)

# check how many tips are in state 0 (should be about half if q01=q10)
length(which(states==0))

# build a likelihood function based on Mk2 model (used to find likelihood of parameter values: probability of the observed distribution of character states given that the tree evolved according to a given model of character evolution and/or diversification)
lik <- make.mk2(tree, states)

# run ML analysis using an initial parameter guess of (0.1, 0.1)
fit <- find.mle(lik, x.init=c(.1, .1))

# parameter estimates for q01 and q10 from the fit
coef(fit)

# store MLE of parameter
r01 <- fit$par[1] # MLE of q01

# repeat for many trees varying in amount of missing data
# build trees to represent n, 2n,...max.taxa tips missing from the original tree
# first, make a sequence to iterate the loop across various quantities of missing data, dropping n.drop tips each iteration
n.drop <- 10

s <- seq(10, max.taxa-n.drop, by=n.drop)
length(s)

# to drop data randomly, use sample(x, size, prob=NULL)
prob <- NULL

# to drop data with bias towards sampling a given state more than the other, use the following function:

# function that takes in list of character states, a state on which to focus the bias, and a probability of being dropped, and returns a vector of probabilities to pass to sample() prob parameter
# e.g. for bias(states, 0, 0.6), Pr[dropping taxa with state=0] = 0.6
bias <- function(d, s=1, p=.5){
  return(as.numeric(ifelse(d==s, p, 1-p)))
}

# prob <- bias(states, 1, 0.2)

# use a loop to simulate and analyse many trees, each containing n fewer tips than the previous run
# run ML analysis for each tree, and keep track of the parameter estimate
for (i in 1:length(s)){
  to_drop <- sample(tree$tip.label, size=s[i], prob=prob)
  new_tree <- drop.tip(tree, tip=to_drop)
  new_states <- states[new_tree$tip.label]
  new_lik <- make.mk2(new_tree, new_states)
  new_fit <- find.mle(new_lik, x.init=c(.1, .1))
  r01es <- new_fit$par[1]
  r01 <- c(r01, r01es)
}

# visualize what happens to our parameter estimates when data is missing
plot(c(0,s), r01)

# compare our Markov model fit to a null hypothesis model (with parameters q01 = q10) to test if the Mk fit is statistically justified
# H: the rates are different
lik_diff <- make.mk2(tree, states)

# build likelihood function based on null model of q01 = q10
# Ho: there is no difference in the two rates
lik_same <- constrain(lik_diff, q01~q10)

# run ML analysis using an initial parameter guess of (0.1, 0.1)
fit_diff <- find.mle(lik_diff, c(.1, .1))

# run ML analysis using initial guess of 0.1 again
fit_same <- find.mle(lik_same, .1)
anova(fit_diff, fit_same)
# extract the p-value
anova(fit_diff, fit_same)[2,5]

# if p < 0.05, we can conclude that the asymmetric Markov model fits better; the rates are not equal

# conduct the same hypothesis test for many trees varying in amount of missing data
# in each iteration, we compare our Markov model fit to a null hypothesis model (with parameters q01 = q10) to test if the Mk fit is statistically justified
# to do this, we need to accumulate a vector of the anova results. Start with an empty list
result <- numeric()

for (i in 1:length(s)){
  to_drop <- sample(tree$tip.label, size=s[i], prob=prob)
  new_tree <- drop.tip(tree, tip=to_drop)
  new_states <- states[new_tree$tip.label]
  new_lik_diff <- make.mk2(new_tree, new_states)
  new_lik_same <- constrain(new_lik_diff, q01~q10)
  fit_diff <- find.mle(new_lik_diff, c(.1, .1))
  fit_same <- find.mle(new_lik_same, .1)
  sig <- anova(fit_diff, fit_same)
  if (sig[2,5] <= 0.05){
    res <- 0 # reject null hypothesis
  } else {
    res <- 1 # fail to reject null hypothesis
  }
  result <- c(result, res)
}

plot(s, result)

# we know the true model parameters in advance, so we can measure the effect of missing data on the ability of a hypothesis test to reject a false null hypothesis
# if we know the null hypothesis is true, we can measure Type 1 error

# repeat analysis for a smaller tree (change max.taxa), for different q01, lambda and mu 



# if interested, we can plot with labeled node states
plot(tree, show.tip.label=FALSE, no.margin=TRUE)
col <- c("#004165", "#eaab00")
tiplabels(col=col[states+1], pch=19, adj=1) # use state, but add 1 to give 1s and 2s instead of 0s and 1s
nodelabels(col=col[attr(states, "node.state")+1], pch=19)

