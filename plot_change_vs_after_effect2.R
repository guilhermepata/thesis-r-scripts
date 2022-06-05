source("build_mega_model.R")
source("my_functions.R")
library("plotrix")



if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  sessions.split = setNames(list(c('S1', 'S4'), c('S1', 'S5')), groups)
  
  (
    plot.change = plot.change.vs.ae(
      # mega.data.split,
      # mega.data.washout,
      # mega.data.summary,
      # model.split,
      # model.washout,
      df.animals,
      df.fit,
      groups = groups,
      # sessions.split = sessions.split,
      # normalized = TRUE,
      # limited = TRUE,
      # plot.ratio = TRUE,
      types = c('Fit'),
      ratio.of.means = FALSE
    )
  )
  
}


if (name == 'Exp5') {
  groups = c(
    'Exp3:NotAtaxic:NoSwitch',
    'Exp3:NotAtaxic:Switch',
    'Exp5:NotAtaxic:NoSwitch',
    'Exp5:NotAtaxic:Switch'
  )
  
  sessions.split = setNames(list(c('S1', 'S4'), c('S1', 'S5'), c('S1', 'S5'), c('S1', 'S5')), groups)
  
  (
    plot.change = plot.change.vs.ae(
      # mega.data.split,
      # mega.data.washout,
      # mega.data.summary,
      # model.split,
      # model.washout,
      df.animals,
      df.fit,
      groups = groups,
      # sessions.split = sessions.split,
      # normalized = TRUE,
      # limited = TRUE,
      # plot.ratio = TRUE,
      types = c('Fit'),
      ratio.of.means = FALSE
    )
  )
  
}

if (name == 'Exp4') {
  groups = c(
    'Exp3:NotAtaxic:NoSwitch',
    'Exp3:NotAtaxic:Switch',
    'Exp4:NotAtaxic:Switch',
    'Exp4:Ataxic:Switch'
  )
  
  sessions.split = setNames(list(c('S1', 'S4'), c('S1', 'S5'), c('S1', 'S2'), c('S1', 'S2')), groups)
  
  (
    plot.change = plot.change.vs.ae(
      # mega.data.split,
      # mega.data.washout,
      # mega.data.summary,
      # model.split,
      # model.washout,
      df.animals,
      df.fit,
      groups = groups,
      # sessions.split = sessions.split,
      # normalized = TRUE,
      # limited = TRUE,
      # plot.ratio = TRUE,
      types = c('Fit'),
      ratio.of.means = FALSE
    )
  )
  
}
