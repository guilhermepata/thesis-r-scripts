library(magick)
library(dplyr)
library(forcats)
source("plot_protocol.R")

img <-   image_read("./plots/split_belt_setup.png") %>%
          image_resize("570x380")

setup = ggdraw() + 
  draw_image(img) 

plotlist = list(setup, plot.short.protocol,
                plot.exp3.noswitch.protocol, plot.exp3.switch.protocol, plot.exp5.noswitch.protocol, plot.exp5.switch.protocol, plot.exp4.protocol)

for (i in 1:length(plotlist)) {
  plotlist[[i]] = plotlist[[i]] +
    theme(plot.margin = unit(c(0.25, 0.25, 0.25, 1), "cm"))
}


toprow = cowplot::plot_grid(
  plotlist = plotlist[c(1,2)],
  nrow = 1,
  labels = 'AUTO',
  align = "Vh"
)

bottomrow = cowplot::plot_grid(
  plotlist = plotlist[c(3,4,5,6,7)],
  nrow = 5,
  labels = c('C','D','E','F','G'),
  align = "Vh"
)

p = cowplot::plot_grid(
  toprow, bottomrow,
  nrow = 2,
  align = "vh",
  rel_heights = c(1, 4)
)

