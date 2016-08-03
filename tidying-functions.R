library(dplyr)
library(tidyr)
library(readr)
library(purrr)

## standardize dplyr NSE features to allow data frame values and column names to be input as arguments
## still thinking about this...

## select desired levels of tree size, bias, and rate parameters 
filter_conds <- function(data, 
                    tree_cond = c(100, 400, 700), 
                    bias_cond = c(0.3, 0.5, 0.7), 
                    sim_q_cond = c(0.1, 0.5, 0.9)){
  data %>% 
    filter(n %in% tree_cond,
           bias %in% bias_cond,
           sim_q01 %in% sim_q_cond)
    mutate(error_q01 = sim_q01 - est_q01_samp)
  }


# 
# res_nest <- res %>%
#   mutate(error_q01 = est_q01_samp - sim_q01,
#          error_q10 = est_q10_samp - sim_q10) %>%
#   group_by(n, sim_q01, sim_q10, bias, samp_f, n_samp) %>% 
#   nest() %>% 
#   
#   res_means <- res_nest %>% 
#   mutate(avg = map(.$data, colMeans)) %>%
#   mutate(df = map(.$avg, ~ data_frame(est_q01 = .x["est_q01"],
#                                       est_q10  = .x["est_q10"],
#                                       est_q01_samp = .x["est_q01_samp"],
#                                       est_q10_samp = .x["est_q10_samp"]))) %>%
# 
# res_means %>% 
#   unnest(df)
# 
# res_means$avg[1:4] %>% 
#   map(~ data_frame(est_q01 = .x["est_q01"], 
#                    est_q10  = .x["est_q10"],
#                    est_q01_samp = .x["est_q01_samp"],
#                    est_q10_samp = .x["est_q10_samp"])) %>% 
#   bind_rows
# 
# ## add new variables for error between estimate and true rate
# res_nest <- res %>%
#   mutate(error_q01 = est_q01_samp - sim_q01,
#          error_q10 = est_q10_samp - sim_q10) %>%
#   group_by(n, sim_q01, sim_q10, bias, samp_f, n_samp) %>% 
#   nest() %>% 
#   
#   res_means <- res_nest %>%
#   mutate(avg = map(.$data, colMeans))
# 
# head(res_means)