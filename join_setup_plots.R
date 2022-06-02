library(magick)
library(cowplot)
library(dplyr)
library(forcats)
library(ggplot2)

for (name in c('Exp3', 'Exp5', 'Exp4')) {
  
  if (name == 'Exp3') {
    source("plot_protocol.R")
    source("plot_experiment_with_fits.R")
    
    height = height.xshort * 5 * .7 / 4.7 * 3
    
    img_setup <-   image_read("./plots/split_belt_setup.png") %>%
      image_resize("570x380")
    img_swl <-  image_read("./plots/swing_length.png")
    img_sl <-  image_read("./plots/step_length.png")
    img_tracking <-  image_read("./plots/tracking.png")
    
    setup = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0, .7), "cm")
    ) +
      draw_image(img_setup)
    swl = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0, .7), "cm")
    ) +
      draw_image(img_swl)
    sl = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0, .7), "cm")
    ) +
      draw_image(img_sl)
    tracking = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0, .7), "cm")
    ) +
      draw_image(img_tracking)
    
    toplist = list(setup,
                   tracking,
                   swl,
                   sl)
    
    plotlist = list(
      plot.exp3.noswitch.protocol,
      plot.exp3.switch.protocol,
      plot.experiment.noswitch,
      plot.experiment.switch
    )
    
    # equal ylims
    for (i in c(1,3)) {
      delta = 1
      list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i + delta]])
      plotlist[[i]] = a
      plotlist[[i + delta]] = b
    }
    
    # add margins
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        # theme_gray() +
        theme(plot.margin = unit(c(0.25, 0, 0.25, .7), "cm"))
    }
    
    # reduce text size
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        theme(axis.title = element_text(size = 8), axis.text = element_text(size = 8))
    }
    
    toprow = cowplot::plot_grid(
      plotlist = toplist,
      nrow = 1,
      labels = 'AUTO',
      align = "Vh",
      label_colour = `if`(dark, "white", "black"),
      rel_widths = c(2, 2, 1.3, 1.3)
    )
    
    plotlist = align_plots(plotlist = plotlist, align = "vh")
    
    bottomrow = cowplot::plot_grid(
      plotlist = plotlist,
      ncol = 2,
      labels = c('E', 'F', 'G', 'H'),
      label_colour = `if`(dark, "white", "black"),
      rel_heights = c(2.65,2),
      align = "Vh"
    )
    
    (p = cowplot::plot_grid(
      toprow,
      bottomrow,
      nrow = 2,
      align = "vh",
      rel_heights = c(1, 2)
    ))
    
  }
  
  if (name == 'Exp5') {
    source("plot_protocol.R")
    source("plot_experiment_with_fits.R")
    
    height = height.xshort * 5 * .7 / 4.7 * 2
    
    plotlist = list(
      plot.exp5.noswitch.protocol,
      plot.exp5.switch.protocol,
      plot.experiment.noswitch,
      plot.experiment.switch
    )
    
    # equal ylims
    for (i in c(1,3)) {
      delta = 1
      list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i + delta]])
      plotlist[[i]] = a
      plotlist[[i + delta]] = b
    }
    
    # add margins
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        # theme_gray() +
        theme(plot.margin = unit(c(0.25, 0, 0.25, .7), "cm"))
    }
    
    # reduce text size
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        theme(axis.title = element_text(size = 8), axis.text = element_text(size = 8))
    }
    
    plotlist = align_plots(plotlist = plotlist, align = "vh")
    
    bottomrow = cowplot::plot_grid(
      plotlist = plotlist,
      ncol = 2,
      labels = 'AUTO',
      rel_heights = c(2.65,2),
      align = "Vh"
    )
    
    (p = cowplot::plot_grid(
      bottomrow,
      nrow = 1
    ))
    
  }
  
  if (name == 'Exp4') {
    source("plot_protocol.R")
    source("plot_experiment_with_fits.R")
    
    height = height.xshort * 5 * .7 / 4.7 * 3
    
    img_control <-   image_read("./plots/control.png")
    img_ataxic <-  image_read("./plots/ataxic.png")
    img_injection <-  image_read("./plots/injection_protocol.png")
    img_ablation <-  image_read("./plots/ablation.svg")
    
    control = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0.25, .7), "cm")
    ) +
      draw_image(img_control)
    ataxic = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0.25, .7), "cm")
    ) +
      draw_image(img_ataxic)
    injection = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0, .7), "cm")
    ) +
      draw_image(img_injection)
    ablation = ggdraw() + theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(0, 0, 0.25, .7), "cm")
    ) +
      draw_image(img_ablation)
    
    toplist = list(injection,
                   ablation,
                   control,
                   ataxic)
    
    plotlist = list(
      plot.exp4.control.protocol,
      plot.exp4.ataxic.protocol,
      plot.experiment.switch,
      plot.experiment.ataxic
    )
    
    # equal ylims
    for (i in c(1,3)) {
      delta = 1
      list[a, b] = set_equal_y_lims(plotlist[[i]], plotlist[[i + delta]])
      plotlist[[i]] = a
      plotlist[[i + delta]] = b
    }
    
    # add margins
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        # theme_gray() +
        theme(plot.margin = unit(c(0.25, 0, 0.25, .7), "cm"))
    }
    
    # reduce text size
    for (i in 1:length(plotlist)) {
      plotlist[[i]] = plotlist[[i]] +
        theme(axis.title = element_text(size = 8), axis.text = element_text(size = 8))
    }
    
    toprow = cowplot::plot_grid(
      plotlist = toplist,
      nrow = 1,
      labels = 'AUTO',
      align = "Vh",
      rel_widths = c(2, 1, 1, 1)
    )
    
    plotlist = align_plots(plotlist = plotlist, align = "vh")
    
    bottomrow = cowplot::plot_grid(
      plotlist = plotlist,
      ncol = 2,
      labels = c('E', 'F', 'G', 'H'),
      rel_heights = c(2.65,2),
      align = "Vh"
    )
    
    (p = cowplot::plot_grid(
      toprow,
      bottomrow,
      nrow = 2,
      align = "vh",
      rel_heights = c(1, 2)
    ))
    
  }
  
  ggsave(
    plot = p,
    paste("plots/", name, "_setup_plot", ".pdf", sep = ""),
    device = cairo_pdf,
    width = 8.27,
    height = height
  )
}
