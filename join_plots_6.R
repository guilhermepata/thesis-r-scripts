library(cowplot)
# is.built = FALSE
# is.builtLR = FALSE

source("my_functions.R")

for (name in c('Exp3', 'Exp5', 'Exp4')) {
  source("plot_inital_error_reduction.R")
  source("plot_change_vs_after_effect2.R")
  source("plot_learning_rates.R")
  source("plot_retention.R")
  source("plot_final_washout.R")
  source("plot_initial_learning_rates.R")
  
  
  plotlist = list(
    plot.errors,
    plot.change,
    plot.learning.rate,
    plot.retention,
    plot.final.washout,
    plot.first.lr
  )
  
  height = height.short * 2
  
  # reduce text size
  for (i in 1:length(plotlist)) {
    plotlist[[i]] = plotlist[[i]] +
      theme(axis.title = element_text(size = 10))
  }
  
  for (i in 3:length(plotlist)) {
    plotlist[[i]] = plotlist[[i]] +
      theme(axis.title = element_text(size = 7),
            axis.text = element_text(size = 7))
  }
  
  # add margins
  for (i in 1:length(plotlist)) {
    plotlist[[i]] = plotlist[[i]] +
      # theme_gray() + 
      theme(plot.margin = unit(c(0.25, 0, 0.25, .7), "cm"))
  }
  
  legend = cowplot::get_legend(plotlist[[2]] + theme(
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    plot.margin = unit(x = c(0, 0, 0, 0), units = "mm"),
    legend.margin = margin(l = 0, unit = "mm"),
    legend.position = 'left',
    legend.box.margin = margin(0, 0, 0, 0)
  ))
  
  aligned = cowplot::align_plots(plotlist[[1]], plotlist[[3]], align = 'v', axis = 'l')
  plotlist[[1]] = aligned[[1]]
  plotlist[[3]] = aligned[[2]]
  
  toprow = cowplot::plot_grid(
    plotlist = list(aligned[[1]], plotlist[[2]] + theme(legend.position = "none"), legend),
    nrow = 1,
    # align = "vh",
    labels = c('A', 'B', ''),
    label_colour = `if`(dark, "white", "black"),
    rel_widths = c(1.65, 1.65, .65)
  ) 
  # + theme(plot.background = element_rect(fill = `if`(dark,"transparent","white"), color = "transparent"))
  bottomrow = cowplot::plot_grid(
    plotlist = list(aligned[[2]], plotlist[[4]], plotlist[[5]], plotlist[[6]]),
    nrow = 1,
    # align = "vh",
    label_colour = `if`(dark, "white", "black"),
    labels = c('C', 'D', 'E', 'F')
  )
  
  (p = cowplot::plot_grid(
    plotlist = cowplot::align_plots(toprow, bottomrow, align = "v", axis = "l"),
    nrow = 2,
    rel_heights = c(2, 1.3)
    # align = "Vh"
  ))
  
  if (!dark) {
    ggsave(
      plot = p,
      paste("presentation_plots/", name, "_w_summary_plot", ".png", sep = ""),
      bg = "transparent",
      width = 8.27,
      height = height
    )
  } else if (!dark) {
    ggsave(
      plot = p,
      paste("plots/", name, "_summary_plot", ".pdf", sep = ""),
      device = cairo_pdf,
      width = 8.27,
      height = height
    )
  }
  
  
  
  
}
