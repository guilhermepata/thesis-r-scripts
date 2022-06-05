source("build_mega_model.R")
# source("plot_limits.R")

### plot experiment with fits

sessions = c('S1', 'S2', 'S3', 'S4', 'S5')


### Plot switch group

{
  group = paste(name, 'NotAtaxic:Switch', sep = ':')
  
  data.switch <-
    filter(mega.data, Group == group, Session %in% sessions)
  data.switch.summary <-
    filter(mega.data.summary, Group == group)
  
  color = get_group_color(group)
  
  (
    plot.experiment.switch <-
      plot.experiment(data.switch, data.switch.summary, color = color)
  )
}


### Plot noswitch group

if (name != 'Exp4') {
  group = paste(name, 'NotAtaxic:NoSwitch', sep = ':')
  
  data.noswitch <-
    filter(mega.data, Group == group, Session %in% sessions)
  data.noswitch.summary <-
    filter(mega.data.summary, Group == group)
  
  color = get_group_color(group)
  
  (
    plot.experiment.noswitch <-
      plot.experiment(data.noswitch, data.noswitch.summary, color = color)
  )
  
}
### Plot ataxic group

if (name == 'Exp4') {
  group = paste(name, 'Ataxic:Switch', sep = ':')
  
  data.ataxic <-
    filter(mega.data, Group == group, Session %in% sessions)
  data.ataxic.summary <-
    filter(mega.data.summary, Group == group)
  
  color = get_group_color(group)
  
  (
    plot.experiment.ataxic <-
      plot.experiment(data.ataxic, data.ataxic.summary, color = color)
  )
}
