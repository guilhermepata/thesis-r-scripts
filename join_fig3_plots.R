library(cowplot)
library(patchwork)
is.built = FALSE
name = 'Exp3'
source("my_functions.R")
source("plot_first_LR.R")
source("plot_final_washout.R")

if (name == 'Exp3') {
  plotlist = list(plot.final.washout.switch,
                  plot.initial.lr.switch) 
  
  
  # for (i in (1:(length(plotlist)%/%2))*2-1) {
  #   delta = 1
  #   list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i+delta]])
  #   plotlist[[i]] = a
  #   plotlist[[i+delta]] = b
  # }
  
  for (i in 1:length(plotlist)) {
    plotlist[[i]] = plotlist[[i]] +
      theme(axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10))
  }
  
  for (i in 1:length(plotlist)) {
    plotlist[[i]] = plotlist[[i]] +
      theme(plot.margin = unit(c(0.25, 0.25, 0.25, 1), "cm"))
  }
  
  nrow = 1
  
  (p = cowplot::plot_grid(
    plotlist = plotlist,
    nrow = nrow,
    labels = 'AUTO'
  ))
  
  ggsave(
    paste("plots/", name, "_fig3_plot", ".pdf", sep = ""),
    device = cairo_pdf,
    width = 8.27,
    height = 5.83 / 2
  )
  
  
}
