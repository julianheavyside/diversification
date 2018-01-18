## read in dataset containing simulated data
res <- readRDS(file = "../results/output/res.Rda")
res <- dplyr::tbl_df(res)
#order bias variable for ggplot colours
res$bias <- ordered(res$bias, levels = c(unique(res$bias))) 
dplyr::glimpse(res)
