source("plotting-functions.R")
# fil_plot(400, 0.1) # a test plot

## make 16 plots: for n = 200, 400, 600, 800, and q01 = 0.1, 0.5, 0.75, 1
## make a list of these combinations for subsetting the original dataset

# Ns <- c(200, 400, 600, 800)
# Q01s <- c(0.1, 0.5, 0.75, 1)

## I want to produce a list of plots, to pass to grid.arrange. having trouble with accumulating a list of plots with loops

# plots <- list()
# for(i in seq_along(Ns)){
#   for(j in seq_along(Q01s)){
#     p <- fil_plot(N = Ns[i], Q01 = Q01s[j])
#     plots <- list(plots, list(p))
#   }
# }


## blank space for place holder in first row, last column (top right in grid arrangement)
blankPlot <- ggplot() + geom_blank(aes(1, 1)) + cowplot::theme_nothing()

## do it manually for now
full <- grid.arrange(textGrob("n = 200"), # text above the plots, indicating tree size
             textGrob("n = 400"),
             textGrob("n = 600"),
             textGrob("n = 800"),
             blankPlot,
             fil_plot(200, 0.1), # individual calls to fil_plot function, with n and q01 specified
             fil_plot(400, 0.1),
             fil_plot(600, 0.1),
             fil_plot(800, 0.1),
             textGrob("Simulated\ntrees using\nq01=0.1\n(Dashed\nline)", gp = gpar(fontsize = 10)), # text in last column to give info
             fil_plot(200, 0.5),
             fil_plot(400, 0.5),
             fil_plot(600, 0.5),
             fil_plot(800, 0.5),
             textGrob("q01=0.5", gp = gpar(fontsize = 10)),
             fil_plot(200, 0.75), 
             fil_plot(400, 0.75),
             fil_plot(600, 0.75),
             fil_plot(800, 0.75),
             textGrob("q01=0.75", gp = gpar(fontsize = 10)),
             fil_plot(200, 1),
             fil_plot(400, 1),
             fil_plot(600, 1),
             fil_plot(800, 1),
             textGrob("q01=1.0", gp = gpar(fontsize = 10)),
             ncol = 5,
             widths = c(rep(12, 4), 5), # adjustments for column widths
             heights = c(1, rep(12, 4)), # adjustments for row heights
             bottom="Proportion of tips remaining after removing data with bias\n(Subsampled with bias (see legend) towards dropping tips in state 1)", 
             left="Estimated parameter (q01)")

print(full)

## save the plot to figures directory
ggsave("full-plot.pdf", plot = full, path = "../results/figs")

