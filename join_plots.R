library(cowplot)



plotlist = list(plot.experiment.noswitch, 
                plot.errors.noswitch,
                plot.experiment.switch,
                plot.errors.switch
                )


plot_grid(plotlist = plotlist, nrow = 2, labels = 'AUTO')
