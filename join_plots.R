library(cowplot)
library(patchwork)
source("my_functions.R")
source("plot_experiment_with_fits.R")
source("plot_inital_error_reduction.R")
source("plot_change_vs_after_effect2.R")

plotlist = list(plot.experiment.noswitch, 
                plot.errors.noswitch,
                plot.change.noswitch,
                plot.experiment.switch,
                plot.errors.switch,
                plot.change.switch
                )


for (i in 1:(length(plotlist)%/%2)) {
  delta = length(plotlist)%/%2
  list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i+delta]])
  plotlist[[i]] = a
  plotlist[[i+delta]] = b
}


(p = cowplot::plot_grid(plotlist = plotlist, nrow = 2, labels = 'AUTO', rel_widths = c(1.3, 1, 1.2), scale=0.9))

# patchwork = (plot.experiment.noswitch / plot.experiment.switch) + 
#             (plot.errors.noswitch / plot.errors.switch) +
#             (plot.change.noswitch / plot.change.noswitch)


