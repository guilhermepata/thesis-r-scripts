library(cowplot)
is.built = FALSE
name = 'Exp3'
source('plot_gait_params.R')

plotlist = list(plot.parameters.spatial, plot.parameters.temporal)

nrow = 1

(p = cowplot::plot_grid(plotlist = plotlist, nrow = nrow, labels = 'AUTO', scale = .9))

ggsave(paste("plots/", name, "_spatiotemporal_plot", ".pdf", sep=""), width = 8.27, height = 5.83/2, device = cairo_pdf)
