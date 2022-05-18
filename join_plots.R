library(cowplot)
library(patchwork)
source("my_functions.R")
source("plot_experiment_with_fits.R")
source("plot_inital_error_reduction.R")
source("plot_change_vs_after_effect2.R")

plotlist = list(
  plot.experiment.noswitch,
  plot.errors.noswitch,
  plot.change.noswitch,
  plot.experiment.switch,
  plot.errors.switch,
  plot.change.switch
)


for (i in 1:(length(plotlist) %/% 2)) {
  delta = length(plotlist) %/% 2
  list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i + delta]])
  plotlist[[i]] = a
  plotlist[[i + delta]] = b
}

for (i in 1:length(plotlist)) {
  plotlist[[i]] = plotlist[[i]] +
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10))
}

top.row.list = plotlist[c(1, 4)]
bottom.row.list = plotlist[c(2, 3, 5, 6)]

top.row = cowplot::plot_grid(
  plotlist = top.row.list,
  nrow = 1,
  labels = 'AUTO',
  scale = .9
)

bottom.row = cowplot::plot_grid(
  plotlist = bottom.row.list,
  nrow = 1,
  labels = c('C', 'D', 'E', 'F'),
  scale = .9
)

(p = cowplot::plot_grid(
  top.row,
  bottom.row,
  nrow = 2,
  ncol = 1,
  labels = ''
))

# patchwork = (plot.experiment.noswitch / plot.experiment.switch) +
#             (plot.errors.noswitch / plot.errors.switch) +
#             (plot.change.noswitch / plot.change.noswitch)
