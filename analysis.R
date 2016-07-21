##############################################################
## for MK models
##############################################################
rm(list=ls())
library(diversitree)
source("supporting-functions.R")


#
# trees of single size ----------------------------------------------------
##############################################################
# ability to estimate rate parameters when data is missing
# for trees of a single size
##############################################################

## estimate rate parameter for multiple trees
treenum <- 3000 # how many trees to simulate
treesize <- 500 # how many taxa in each tree
p_bias <- 0.5
sub <- seq(0, treesize-(treesize/10), by=treesize/10)
pars <- c(2, 1) # true rates for simulating trees

## 
res <- list()
for(i in seq_along(sub)){
  res[[i]] <- sapply(c(1:treenum), function(x) get_sim_pars(treesize=treesize, pars=pars, drop=sub[i]))
}

## the mean of all estimates, for each tier of tree subsampling
mean_res <- lapply(res, mean)

se <- lapply(res, standard.error)

rev_sub <- sub[rev(order(sub))]

par(mar = c(5, 6, 1, 1))
## adjust ylim to accomodate large error bars
plot(rev_sub, mean_res, ylim = c(0, range(mean_res)[2] + max(unlist(se))), xlab = "Number of taxa with known character state information", ylab = "Mean estimated rate of character evolution")

## a horizontal line for the true rate of character evolution (use to make trees)
abline(h = pars[1], lty = 5)

## add standard error bars about each mean
## plot flat arrows above and below each mean, each as long as the SE
for(i in seq_along(rev_sub)){
  arrows(rev_sub[i], mean_res[[i]] - se[[i]], 
         rev_sub[i], mean_res[[i]] + se[[i]], 
         length = .1, angle = 90, code = 3)
}

# trees of many sizes --------------------------------------------------------
##############################################################
## estimate rates for trees of different sizes and different
## levels of subsampling
##############################################################

## Run this over a range of conditions

# simulate over range of tree sizes
t_size <- seq(50, 1000, 50) # length = 20

# ... over range of sampling fractions
samp <- seq(1, .25, -.05) # length = 16

# ... over range of parameter (q01 and q10) values
pars_list_diff <- list()
pars_list_same <- list()
pars <- seq(0.1, 1, by=0.1)
rev_pars <- rev(pars)
# different values for q01 and q10
for (i in seq_along(pars)){
  pars_list_diff[[i]] <- c(pars[i], rev_pars[i]) 
}
# same values for q01 and q10
for (i in seq_along(pars)){
  pars_list_same[[i]] <- c(pars[i], pars[i])
}
# join both lists to make a full pars_list over a range of values, for situations with same and different values
pars_list <- c(pars_list_diff, pars_list_same) # length = 20

# ... over a range of biases towards dropping tips with state=1
bias <- seq(0.3, 0.7, 0.1)

## just loop it. Get 100 results at each (produces 20*16*20*5*100iterations=3200000 observations)
res <- data.frame()
for (i in seq_along(t_size)){ # simulate over range of tree sizes
  for (j in seq_along(samp)){ # ...over range of sampling fractions
    for (k in seq_along(pars_list)){ # ...over range of parameter values
      for (l in seq_along(bias)){ # ...over a range of biases towards state=1
        print(c(i,j,k,l))
        out <- simulate_mk2_rsamp(i=100, 
                                  n=t_size[i], 
                                  s=samp[j], 
                                  r=pars_list[[k]], 
                                  b=bias[l])
        res <- rbind(res,out)
      }
    }
  }
}

## check out the giant data frame
View(res)
