rm(list=ls())
## for MK models

## simulation --------------------------------------------------------
source("simulation-functions.R")
## estimate rates for trees of different sizes and different levels of subsampling
## run over a range of conditions:

## simulate over range of tree sizes
t_size <- c(200, 400, 600, 800)
t_size

## ... over range of sampling fractions
samp <- seq(1, .25, -.1)
samp

## ... over range of parameter (q01 and q10) values
## roundabout way to make a long list of parameter combinations (for situations when q01=q10, when they aren't equal, when they are large or small, etc) 
## multidimensional parameter space
pars_list_diff <- list()
pars_list_same <- list()
pars <- c(0.1, 0.25, 0.5, 0.75, 1) # can't use rate=0; trees fail
rev_pars <- rev(pars)
pars

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
pars_list <- pars_list[-8] # don't need two sets of q01=q10=0.5, so remove one (see list before and after to understand)
pars_list

## ... over a range of biases towards dropping tips with state=1
bias <- c(0.3, 0.5, 0.7)
bias

## calculate number of observations produced by the simulation
print(length(t_size) * length(samp) * length(pars_list) * length(bias) * 100)

## now we are ready to start the simulation
## just loop it. Get 100 results at each
res <- data.frame() ## store our simulated data in a data.frame
for (j in seq_along(t_size)){ ## simulate over range of tree sizes
  for (k in seq_along(samp)){ ## ...over range of sampling fractions
    for (l in seq_along(pars_list)){ ## ...over range of parameter values
      for (m in seq_along(bias)){ ## ...over a range of biases towards state=1
        print(c(j,k,l,m))
        out <- simulate_mk2_rsamp(i=100, n=t_size[j], s=samp[k], r=pars_list[[l]], b=bias[m])
        res <- rbind(res,out)}
    }
  }
}


## save first simulated dataset as a csv in the working dir
write.csv(res, "res2.csv")

## for my second run, with limited parameter range, saved as data frame
saveRDS(res, file = "res.Rda")

## load saved simulation data -------------------------------------------
## load first simulated data set (which I foolishly saved as a csv), using speedy readr magic
# res <- read_csv("res.csv", col_types = "iiddddddidd")
# res$bias <- ordered(res$bias, levels = c(unique(res$bias)))
# res <- dplyr::tbl_df(res[-1]) # excel added a index column, so remove it
# glimpse(res)

## load in second sim run data frame, if working with that one
res <- readRDS(file = "res.Rda")
res <- dplyr::tbl_df(res)
#order bias variable for ggplot colours
res$bias <- ordered(res$bias, levels = c(unique(res$bias))) 
glimpse(res)


## tidying ---------------------------------------------------------
source("tidying-functions.R")

## select desired levels of tree size, bias, and rate parameters to filter data for plotting with facets

fil_res <- res %>% 
  filter(n %in% c(500, 600, 700),
         bias %in% c(0.3, 0.5, 0.7), 
         sim_q01 %in% c(0.1, 0.5, 0.9),
         sim_q01 == sim_q01) %>% 
  group_by(n, bias, sim_q01, samp_f) %>% 
  # mutate(error = sim_q01 - est_q01_samp)
  summarise_each(funs(mean), est_q01_samp)


ggplot(fil_res, aes(samp_f, est_q01_samp, colour = bias)) +
  geom_point() +
  facet_grid(sim_q01 ~ n, scales = "free_y")


## some of Andrew's wrangling advice incorporated below
res_nest <- res %>%
  mutate(error_q01 = est_q01_samp - sim_q01,
         error_q10 = est_q10_samp - sim_q10) %>%
  group_by(n, sim_q01, sim_q10, bias, samp_f, n_samp) %>%
  nest() %>%

  res_means <- res_nest %>%
  mutate(avg = map(.$data, colMeans)) %>%
  mutate(df = map(.$avg, ~ data_frame(est_q01 = .x["est_q01"],
                                      est_q10  = .x["est_q10"],
                                      est_q01_samp = .x["est_q01_samp"],
                                      est_q10_samp = .x["est_q10_samp"]))) %>%

res_means %>%
  unnest(df)

res_means$avg[1:4] %>%
  map(~ data_frame(est_q01 = .x["est_q01"],
                   est_q10  = .x["est_q10"],
                   est_q01_samp = .x["est_q01_samp"],
                   est_q10_samp = .x["est_q10_samp"])) %>%
  bind_rows

## add new variables for error between estimate and true rate
res_nest <- res %>%
  mutate(error_q01 = est_q01_samp - sim_q01,
         error_q10 = est_q10_samp - sim_q10) %>%
  group_by(n, sim_q01, sim_q10, bias, samp_f, n_samp) %>%
  nest() %>%

  res_means <- res_nest %>%
  mutate(avg = map(.$data, colMeans))

head(res_means)

# plotting ----------------------------------------------------------------
source("plotting-functions.R")

ggplot(sub, aes(samp_f, est_q01_samp, color = bias)) +
  geom_point() +
  facet_grid(sim_q01 ~ n, scales = "free_y")




