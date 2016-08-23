library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

fil_plot <- function(N, Q01){
  res %>% 
    filter(n == N, sim_q01 == Q01, sim_q01 == sim_q10) %>% 
    group_by(n, bias, sim_q01, samp_f) %>%
    summarise_each(funs(mean), est_q01_samp) %>%
    ggplot(aes(x = samp_f, y = est_q01_samp, group = bias, colour = bias)) +
    geom_point() +
    geom_smooth(show.legend = FALSE, aes(fill = bias)) +
    geom_hline(yintercept = Q01, linetype = "dashed") +
    
    ## format the legend (see theme() below as well)
    scale_color_discrete(guide = guide_legend(title = NULL,
                                             direction = "horizontal", 
                                             label.position = "bottom",
                                             keyheight = 0.4)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill = NA),
          legend.justification = c(1, 1),
          legend.position = c(1, 1)) 
          
}
