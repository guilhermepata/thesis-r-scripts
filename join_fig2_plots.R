library(cowplot)
library(patchwork)
is.built = FALSE
name = 'Exp3'
source("my_functions.R")
source("plot_inital_error_reduction.R")

if( name == 'Exp4') {
  
  plotlist = list(plot.errors.switch,
                  plot.errors.ataxic
  )
  
} else {
  plotlist = list(plot.errors.noswitch,
                  plot.errors.switch)
}

for (i in 1:(length(plotlist)%/%2)) {
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

(p = cowplot::plot_grid(plotlist = plotlist, nrow = 1, labels = 'AUTO', scale = .9))

ggsave(paste("plots/", name, "_fig2_plot", ".pdf", sep=""), width = 8.27, height = 5.83, device = cairo_pdf)

