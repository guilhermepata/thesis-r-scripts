library(cowplot)
library(patchwork)
is.built = FALSE
name = 'Exp3'
source("my_functions.R")
source("plot_experiment_with_fits.R")

if( name == 'Exp4') {

plotlist = list(plot.experiment.switch,
                plot.experiment.ataxic
)

} else {
  plotlist = list(plot.experiment.noswitch,
                  plot.experiment.switch)
}

for (i in 1) {
  delta = 1
  list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i+delta]])
  plotlist[[i]] = a
  plotlist[[i+delta]] = b
}

for (i in 1:length(plotlist)) {
  plotlist[[i]] = plotlist[[i]] +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10)
    )
}

for (i in 1:length(plotlist)) {
  plotlist[[i]] = plotlist[[i]] +
    theme(plot.margin = unit(c(0.25,0.25,0.25,1), "cm"))
}


(p = cowplot::plot_grid(plotlist = plotlist, nrow = 2, labels = 'AUTO', scale = 1))

ggsave(paste("plots/", name, "_experiment_plot", ".pdf", sep=""), width = 8.27, height = height.short*2, device = cairo_pdf)

