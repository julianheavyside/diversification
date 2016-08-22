library(ggplot2)
library(gridExtra)
library(grid)

plot_by_filter <- function(dat, q01, n.taxa){
  dat <- subset(dat, sim_q01 == q01 & n == n.taxa)
  ggplot(dat, aes(x = samp_f, y = est_q01_samp, colour = bias)) +
  geom_point() +
  geom_smooth() +
  facet_grid(.~n) +
  geom_hline(yintercept=q01, linetype = "dashed") +
  labs(x = "Proportion of tips remaining", y = "Parameter estimate")
  }

