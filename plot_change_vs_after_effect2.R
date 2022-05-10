source("build_model.R")
source("my_functions.R")

plot.change <- function(df, df.animals, color='#619CFF') {
  
  color.light = lighten(color)
  
  ggplot() +
    geom_point(data=df.animals, aes(x=Phase, y=Asym, group=Animal), 
               alpha = 0.5, 
               color = color.light) +
    geom_line(data=df.animals, aes(x=Phase, y=Asym, group=Animal),
              alpha = 0.5,
              color = color.light) +
    
    geom_point(data=df, aes(x=Phase, y=Fit), 
               size = 3,
               color = color) +
    geom_errorbar(data=df, aes(x=Phase, y=Fit, ymin=Lower, ymax=Upper), 
                  width=.1,
                  position=position_dodge(0.05),
                  color=color) +
    geom_line(data=df, aes(x=Phase, y=Fit, group=0), 
              size  = 1,
              color=color,
              linetype = "dashed") +
    
    geom_hline(yintercept=c(0), linetype="dashed", alpha = 0.5) +
    
    scale_x_discrete(limits=c('Change over split', 'After effect'), labels=c("Change over split" = "COS", "After effect" = "AE")) +
    theme_classic() + theme(legend.position="none") +
    labs(x=element_blank(), y = "Step length asymmetry (mm)")
}

make_change_df <- function(data.summary, 
                           model.split, 
                           model.washout, 
                           group,
                           sessions.split = c('S1', 'S5')) {
  
  data.summary2 = filter(data.summary, Group == group)
  
  first.trial = min(filter(data.summary2, Phase == 'Split', 
                           Session == sessions.split[[1]])$Trial)
  last.trial = max(filter(data.summary2, Phase == 'Split',
                          Session == sessions.split[[2]])$Trial)
  washout.trial = last.trial + 1
  
  first.trial.row = filter(data.summary2, Trial==first.trial)
  last.trial.row = filter(data.summary2, Trial==last.trial)
  washout.row = filter(data.summary2, Trial==washout.trial)
  
  first.num = first.trial.row$Num
  first.session = first.trial.row$Session
  
  last.num = last.trial.row$Num
  last.session = last.trial.row$Session
  
  washout.num = washout.row$Num
  washout.session = washout.row$Session
  
  s.split = summary(emmeans(
      model.split,
      revpairwise ~ Num * Session * Group,
      at = list(
        Session = c(first.session, last.session),
        Num = c(first.num, last.num),
        Group = group
      )
  )$contrasts, infer=TRUE)
  
  s.split = filter(s.split, contrast == paste(last.num, 
                                              last.session,
                                              group,
                                              '-',
                                              first.num,
                                              first.session,
                                              group,
                                              sep=' '))
  
  s.washout = summary(emmeans(
    model.washout,
    identity ~ Num * Session * Group,
    at = list(
      Session = washout.session,
      Num = washout.num,
      Group = group
    )
  )$contrasts, infer=TRUE)
  
  df <- data.frame(
    Phase = c('Change over split', 'After effect'),
    Fit = c(s.split$estimate, s.washout$estimate),
    Lower = c(s.split$lower.CL, s.washout$lower.CL),
    Upper = c(s.split$upper.CL, s.washout$upper.CL),
    P.value = c(s.split$p.value, s.washout$p.value)
  )
  
  return(df)
  
}

make_change_df_animals <- function(data.split,
                                   data.washout,
                                   data.summary, 
                                   model.split, 
                                   model.washout, 
                                   group,
                                   sessions.split = c('S1', 'S5')) {
  
  data.summary2 = filter(data.summary, Group == group)
  
  first.trial = min(filter(data.summary2, Phase == 'Split', 
                           Session == sessions.split[[1]])$Trial)
  last.trial = max(filter(data.summary2, Phase == 'Split',
                          Session == sessions.split[[2]])$Trial)
  washout.trial = last.trial + 1
  
  first.trial.row = filter(data.summary2, Trial==first.trial)
  last.trial.row = filter(data.summary2, Trial==last.trial)
  washout.row = filter(data.summary2, Trial==washout.trial)
  
  first.num = first.trial.row$Num
  first.session = first.trial.row$Session
  
  last.num = last.trial.row$Num
  last.session = last.trial.row$Session
  
  washout.num = washout.row$Num
  washout.session = washout.row$Session
  
  df.animals = data.frame(Phase = c(), Asym = c(), Animal = c())
  
  for (animal in unique(filter(data.split, Group==group)$Animal)) {
    df.aux = data.frame(Phase = c('Change over split'),
                        Asym = c(filter(data.split, 
                                        Session==last.session & 
                                          Num==last.num & 
                                          Animal==animal)$Asym - 
                                   filter(data.split, 
                                          Session==first.session & 
                                            Num==first.num & 
                                            Animal==animal)$Asym),
                        Animal = c(animal)
    )
    df.animals = rbind(df.animals, df.aux)
  }
  
  for (animal in unique(filter(data.washout, Group==group)$Animal)) {
    df.aux = data.frame(Phase = c('After effect'),
                        Asym = c(filter(data.washout, 
                                        Session==washout.session &
                                          Num==washout.num & 
                                          Animal==animal)$Asym),
                        Animal = c(animal)
    )
    df.animals = rbind(df.animals, df.aux)
  }
  
  return(df.animals)
  
}

if (name != 'Exp4'){

### plot switch group

group = 'NotAtaxic:Switch'
color = get_group_color(group)
  
df.switch <- make_change_df(data.summary,
                            model.split,
                            model.washout,
                            group = group,
                            sessions.split = c('S1','S5'))

  
df.animals.switch = make_change_df_animals(data.split,
                                           data.washout,
                                           data.summary,
                                           model.split,
                                           model.washout,
                                           group = group,
                                           sessions.split = c('S1','S5'))

plot.change.switch <- plot.change(df.switch, df.animals.switch, color=color)


### plot noswitch group
  
  if (name == 'Exp3') {
    sessions.split = c('S1', 'S4')
  } else if (name == 'Exp5') {
    sessions.split = c('S1', 'S5')
  }
  
group = 'NotAtaxic:NoSwitch'
color = get_group_color(group)

df.noswitch <- make_change_df(data.summary,
                            model.split,
                            model.washout,
                            group = group,
                            sessions.split = sessions.split)


df.animals.noswitch = make_change_df_animals(data.split,
                                           data.washout,
                                           data.summary,
                                           model.split,
                                           model.washout,
                                           group = group,
                                           sessions.split = sessions.split)

plot.change.noswitch <- plot.change(df.noswitch, df.animals.noswitch, color=color)
}

if (name == 'Exp4') {
  
  ## plot not ataxic
  
  group = 'NotAtaxic:NoSwitch'
  color = get_group_color(group)
  
  df.switch <- make_change_df(data.summary,
                              model.split,
                              model.washout,
                              group = 'NotAtaxic:Switch',
                              sessions.split = c('S1','S2'))
  
  
  df.animals.switch = make_change_df_animals(data.split,
                                             data.washout,
                                             data.summary,
                                             model.split,
                                             model.washout,
                                             group = 'NotAtaxic:Switch',
                                             sessions.split = c('S1','S2'))
  
  plot.change.switch <- plot.change(df.switch, df.animals.switch, color=color)
  
  ## plot ataxic ???
  
}


