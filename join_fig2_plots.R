library(cowplot)
library(patchwork)
is.built = FALSE
name = 'Exp5'
source("my_functions.R")
source("plot_inital_error_reduction.R")
source("plot_change_vs_after_effect2.R")

if( name == 'Exp4') {
  
  plotlist = list(plot.errors.switch,
                  plot.errors.ataxic
  )
  now = 1
} else if (name == 'Exp5') {
  # plotlist = list(plot.errors.noswitch,
  #                 plot.errors.switch)
  # nrow = 1
  plotlist = list(plot.errors.noswitch,
                  plot.errors.switch,
                  plot.change.noswitch,
                  plot.change.switch)
  nrow = 2
} else if (name == 'Exp3') {
  plotlist = list(plot.errors.noswitch,
                  plot.errors.switch,
                  plot.change.noswitch,
                  plot.change.switch)
  nrow = 2
}

for (i in (1:(length(plotlist)%/%2))*2-1) {
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

(p = cowplot::plot_grid(plotlist = plotlist, nrow = nrow, labels = 'AUTO', scale = .9))

ggsave(paste("plots/", name, "_fig2_plot", ".pdf", sep=""), width = 8.27, height = 5.83, device = cairo_pdf)

