library(cowplot)
#is.built = FALSE
name = 'Exp3'
source('plot_gait_params.R')

plotlist = list(plot.parameters.spatial, plot.parameters.temporal)

for (i in 1) {
  delta = 1
  list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i+delta]])
  plotlist[[i]] = a
  plotlist[[i+delta]] = b
}

# for (i in 1) {
#   delta = 1
#   list[a, b] = set_equal_x_lims(plotlist[[i]], plotlist[[i+delta]])
#   plotlist[[i]] = a
#   plotlist[[i+delta]] = b
# }

for (i in 1:length(plotlist)) {
  plotlist[[i]] = plotlist[[i]] +
    theme(plot.margin = unit(c(0.25,0.25,0.25,1), "cm"))
}

nrow = 1

(p = cowplot::plot_grid(plotlist = plotlist, nrow = nrow, labels = 'AUTO', scale = 1))

ggsave(paste("plots/", name, "_spatiotemporal_plot", ".pdf", sep=""), width = 8.27, height = 5.83/2, device = cairo_pdf)




### 3d plot
library(rgl)

plot3d(x = data.zscored$coo,
       y = data.zscored$double_support,
       z = data.zscored$Asym,
       col = get_group_color(data.zscored$Group))
