rm(list=ls())
## for MK models

# simulation --------------------------------------------------------
## estimate rates for trees of different sizes and different levels of subsampling
## run over a range of conditions:

## simulate over range of tree sizes
t_size <- seq(100, 1000, 100)

## ... over range of sampling fractions
samp <- seq(1, .25, -.1)

## ... over range of parameter (q01 and q10) values
## roundabout way to make a long list of parameter combinations (for situations when q01=q10, when they aren't equal, when they are large or small, etc) 
## multidimensional parameter space
pars_list_diff <- list()
pars_list_same <- list()
pars <- seq(0.1, 1, by=0.1)
rev_pars <- rev(pars)

## different values for q01 and q10
for (i in seq_along(pars)){
  pars_list_diff[[i]] <- c(pars[i], rev_pars[i]) 
}

## same values for q01 and q10
for (i in seq_along(pars)){
  pars_list_same[[i]] <- c(pars[i], pars[i])
}

## join both lists to make a full pars_list over a range of values, for situations with same and different values
pars_list <- c(pars_list_diff, pars_list_same) 

## ... over a range of biases towards dropping tips with state=1
bias <- seq(0.3, 0.7, 0.1)

## calculate number of observations produced by the simulation
options(scipen=999) #get rid of scientific notation
print(length(t_size) * length(samp) * length(pars_list) * length(bias) * 100) #interations

## now we are ready to start the simulation
## just loop it. Get 100 results at each
source("multi-tree-functions.R")

res <- data.frame() #store our simulated data in a data.frame
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

## save simulated dataset as a csv in the working dir
write.csv(res, "res.csv")

# tidying ---------------------------------------------------------
source("tidying-functions.R")

## load simulated data set, using speedy readr magic (
res <- read_csv("res.csv", col_types = "iiddddddidd")


## quick example; small subset to work through potential manipulations
sim_res <- dplyr::tbl_df(res[-1])
glimpse(sim_res)

## with treesize, sim par values, bias held constant
sim_res_500 <- sim_res %>% 
    filter(n==500, sim_q01==0.5, sim_q10==0.5, bias==0.5) %>%
    select(est_q01, samp_f, est_q01_samp) %>%
    group_by(samp_f) %>%
    summarise_each(funs(mean), est_q01, est_q01_samp)

## nesting dataframes using purrr
sim_res_nest <- sim_res %>% 
  group_by(n, sim_q01, sim_q10, bias, samp_f, n_samp) %>% 
  nest()

sim_res_means <- sim_res_nest %>% 
  mutate(avg = map(.$data, colMeans)) %>% 
  mutate(df = map(.$avg, ~ data_frame(est_q01 = .x["est_q01"], 
                                      est_q10  = .x["est_q10"],
                                      est_q01_samp = .x["est_q01_samp"],
                                      est_q10_samp = .x["est_q10_samp"])))

sim_res_means %>% 
  unnest(df)

sim_res_means$avg[1:4] %>% 
  map(~ data_frame(est_q01 = .x["est_q01"], 
                   est_q10  = .x["est_q10"],
                   est_q01_samp = .x["est_q01_samp"],
                   est_q10_samp = .x["est_q10_samp"])) %>% 
  bind_rows

# plotting ----------------------------------------------------------------
source("plotting-functions.R")

plot1 <- ggplot(sim_res_500, aes(samp_f, est_q01_samp)) + geom_point()
plot1


