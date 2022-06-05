source("build_mega_model.R")
source("my_functions.R")


if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (plot.retention = plot.retention.func(mega.data.split.summary, model.split,
                                   groups = groups))
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.retention = plot.retention.func(mega.data.split.summary, model.split,
                                   groups = groups))
}

if (name == 'Exp4') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  sessions = list()
  for (group in groups) {
    sessions[group] = list(c('S1', 'S2', 'S3'))
  }
  
  (plot.retention = plot.retention.func(mega.data.split.summary, model.split,
                                   groups = groups, sessions = sessions))
}
