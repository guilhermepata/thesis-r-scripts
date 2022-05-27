library(cowplot)
library(patchwork)
# is.built = FALSE

source("my_functions.R")

for (name in c('Exp3', 'Exp5', 'Exp4')) {
  source("plot_experiment_with_fits.R")
  source("plot_inital_error_reduction.R")
  source("plot_change_vs_after_effect2.R")
  source("plot_learning_rates.R")
  
  # name = 'Exp5'
  
  if (name == 'Exp4') {
    plotlist = list(
      plot.experiment.switch,
      plot.experiment.ataxic,
      plot.errors.switch,
      plot.errors.ataxic,
      plot.learning.rate.notataxic,
      plot.learning.rate.ataxic
    )
    # nrow = 2
    height = height.xshort * 4
  } else if (name == 'Exp5') {
    # plotlist = list(plot.errors.noswitch,
    #                 plot.errors.switch)
    # nrow = 1
    plotlist = list(
      plot.experiment.noswitch,
      plot.experiment.switch,
      plot.errors.noswitch,
      plot.errors.switch,
      plot.change.noswitch,
      plot.change.switch
    )
    # nrow = 3
    height = height.xshort * 4
  } else if (name == 'Exp3') {
    plotlist = list(
      plot.experiment.noswitch,
      plot.experiment.switch,
      plot.errors.noswitch,
      plot.errors.switch,
      plot.change.noswitch,
      plot.change.switch
    )
    nrow = 2
    height = height.xshort * 4
  }
  
  for (i in (1:(length(plotlist) %/% 2)) * 2 - 1) {
    delta = 1
    list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i + delta]])
    plotlist[[i]] = a
    plotlist[[i + delta]] = b
  }
  
  for (i in 1:length(plotlist)) {
    plotlist[[i]] = plotlist[[i]] +
      theme(axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10))
  }
  
  if (FALSE && name == 'Exp4') {
    toprow = cowplot::plot_grid(
      plotlist = plotlist[c(1, 2)],
      nrow = 2,
      labels = 'AUTO',
      scale = .9
    )
    bottomrow = cowplot::plot_grid(
      plotlist = plotlist[c(3, 4)],
      nrow = 1,
      labels = c('C', 'D'),
      scale = .9
    )
    plotlist = list(toprow, bottomrow)
    nrow = 2
    (
      p = cowplot::plot_grid(
        plotlist = plotlist,
        nrow = nrow,
        labels = '',
        scale = 1,
        rel_heights = c(2, 1)
      )
    )
  } else if (TRUE || FALSE && name == 'Exp3' && name == 'Exp5') {
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        theme(plot.margin = unit(c(0.25, 0.25, 0.25, 1), "cm"))
    }
    
    
    toprow = cowplot::plot_grid(
      plotlist = list(plotlist[[1]],
                      plotlist[[2]]),
      nrow = 2,
      labels = c('A', 'B'),
      align = "vh"
    )
    
    bottomrow = cowplot::plot_grid(
      plotlist = plotlist[c(3, 4, 5, 6)],
      labels = c('C', 'D', 'E', 'F'),
      nrow = 2,
      align = "vh"
    )
    
    (p = cowplot::plot_grid(
      plotlist = list(toprow, bottomrow),
      nrow = 2,
      align = "Vh"
    ))
    
  } else if (FALSE) {
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        theme(plot.margin = unit(c(0.25, 0.25, 0.25, 1), "cm"))
    }
    
    for (i in 3:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        theme(axis.title.x = element_text(size = 8),
              axis.title.y = element_text(size = 8))
    }
    
    ylim = c(-11.36112, 5.919664)
    for (i in 1:2) {
      plotlist[[i]] = plotlist[[i]] +
        ylim(ylim)
    }
    
    
    leftcol = cowplot::plot_grid(
      plotlist = list(plotlist[[1]],
                      plotlist[[2]]),
      nrow = 2,
      labels = c('A', 'D'),
      align = "vh"
    )
    
    rightcol = cowplot::plot_grid(
      plotlist = plotlist[c(3, 5, 4, 6)],
      labels = c('B', 'C', 'E', 'F'),
      nrow = 4,
      align = "vh"
    )
    
    (p = cowplot::plot_grid(
      plotlist = list(leftcol, rightcol),
      nrow = 1,
      rel_widths = c(5, 2),
      align = "Vh"
    ))
    
    height = height.large * 2.5
    
  } else {
    (p = cowplot::plot_grid(
      plotlist = plotlist,
      nrow = nrow,
      labels = 'AUTO',
      scale = .9
    ))
    
  }
  
  ggsave(
    plot = p,
    paste("plots/", name, "_fig2_plot", ".png", sep = ""),
    # device = cairo_pdf,
    width = 8.27,
    height = height
  )
}
