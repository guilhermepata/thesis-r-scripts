source('build_mega_model.R')



short.df = data.frame(Group = rep('Exp3:NotAtaxic:NoSwitch', 21),
                      Session = rep('S1', 21),
                      Trial = 1:21,
                      Num = c(0:2,
                              0:8,
                              0:8),
                      Phase = c(rep('Baseline', 3),
                                rep('Split', 9),
                                rep('Washout', 9))
                      )

example.curve = function(df, A = 6, r = 0.1, noise = 0.2) {
  res = c()
  adapt = function(x) {
    return(-A*exp(-x*r))
  }
  B = adapt(max(filter(df, Phase == 'Split')$Num)) - (-A)
  washout = function(x) {
    return(B*exp(-x*r*2))
  }
  for (row in 1:nrow(df)) {
    if (df[row,]$Phase == 'Baseline') {
      res = c(res, 0)
    } else if (df[row,]$Phase == 'Split') {
      res = c(res, adapt(df[row,]$Num))
    } else if (df[row,]$Phase == 'Washout') {
      res = c(res, washout(df[row,]$Num))
    }
    
  }
  res = res + rnorm(length(res), sd=noise)
  return(res)
}

short.df$Asym = example.curve(short.df, r = 0.4, noise = 0)

(plot.short.protocol = plot.protocol(short.df,
                                     'Exp3:NotAtaxic:NoSwitch',
                                     sessions=c('S1'), pos ='right', 
                                     red.blue = TRUE))

ggsave(
  plot = plot.short.protocol,
  paste("presentation_plots/", name, "_fig23_plot", ".png", sep = ""),
  bg = 'transparent',
  # device = cairo_pdf,
  width = 4.75,
  height = 2.8
)


(plot.exp3.noswitch.protocol = plot.protocol(mega.data.summary,
                                             'Exp3:NotAtaxic:NoSwitch', sessions = c('S1','S2','S3','S4','S5'), legend=FALSE))
(plot.exp3.switch.protocol = plot.protocol(mega.data.summary,
                                             'Exp3:NotAtaxic:Switch',
                                           sessions = c('S1','S2','S3','S4','S5'),
                                           legend = FALSE))

(plot.exp5.noswitch.protocol = plot.protocol(mega.data.summary,
                                           'Exp5:NotAtaxic:NoSwitch',
                                           legend = FALSE))
(plot.exp5.switch.protocol = plot.protocol(mega.data.summary,
                                           'Exp5:NotAtaxic:Switch',
                                           legend = FALSE))
(plot.exp4.control.protocol = plot.protocol(mega.data.summary,
                                           'Exp4:NotAtaxic:Switch',
                                           legend = FALSE))

(plot.exp4.ataxic.protocol = plot.protocol(mega.data.summary,
                                           'Exp4:Ataxic:Switch',
                                           legend = FALSE))