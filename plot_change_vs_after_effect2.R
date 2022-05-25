source("build_mega_model.R")
source("my_functions.R")

plot.change <- function(df, df.animals, df.compare = NULL, color = '#619CFF') {
  color.light = lighten(color)
  color.dark = darken(color)
  
  p = ggplot() +
    geom_point(
      data = df.animals,
      aes(x = Phase, y = Asym, group = Animal),
      alpha = 0.5,
      color = color.light
    ) +
    geom_line(
      data = df.animals,
      aes(x = Phase, y = Asym, group = Animal),
      alpha = 0.5,
      color = color.light
    )  +
    
    geom_point(
      data = df,
      aes(x = Phase, y = Fit),
      size = 3,
      color = color
    ) +
    geom_errorbar(
      data = df,
      aes(
        x = Phase,
        y = Fit,
        ymin = Lower,
        ymax = Upper
      ),
      width = .1,
      position = position_dodge(0.05),
      color = color
    ) +
    geom_line(
      data = df,
      aes(x = Phase, y = Fit, group = 0),
      size  = 1,
      color = color,
      linetype = "dashed"
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               alpha = 0.5) +
    
    scale_x_discrete(
      limits = c('Change over split', 'After effect'),
      labels = c("Change over split" = "COS", "After effect" = "AE")
    ) +
    theme_classic() + theme(legend.position = "none") +
    labs(x = element_blank(), y = "Step length asym. (mm)")
  
  if (!is.null(df.compare)) {
    p = p +
      
      geom_point(
        data = df.compare,
        aes(x = Phase, y = Fit),
        size = 1,
        stroke = 1,
        shape = 2,
        color = color.dark,
        alpha = 0.8
      ) +
      geom_line(
        data = df.compare,
        aes(x = Phase, y = Fit, group = 0),
        size  = .7,
        color = color.dark,
        linetype = "dashed",
        alpha = 0.8
      ) 
    
  }
    
  return(p)
}

make_change_df <- function(data.summary,
                           model.split,
                           model.washout,
                           group,
                           sessions.split = c('S1', 'S5')) {
  sessions.test = sessions.split
  if (length(sessions.split) == 1) {
    sessions.split = c(sessions.split[[1]], sessions.split[[1]])
  }
  
  data.summary2 = filter(data.summary, Group == group)
  
  first.trial = min(filter(data.summary2, Phase == 'Split',
                           Session == sessions.split[[1]])$Trial)
  last.trial = max(filter(data.summary2, Phase == 'Split',
                          Session == sessions.split[[2]])$Trial)
  washout.trial = last.trial + 1
  
  first.trial.row = filter(data.summary2, Trial == first.trial)
  last.trial.row = filter(data.summary2, Trial == last.trial)
  washout.row = filter(data.summary2, Trial == washout.trial)
  
  first.num = first.trial.row$Num
  first.session = first.trial.row$Session
  
  last.num = last.trial.row$Num
  last.session = last.trial.row$Session
  
  washout.num = washout.row$Num
  washout.session = washout.row$Session
  
  # if (length(sessions.split) == 1) {
  #   sessions = 
  # }
  
  s.split = summary(emmeans(
    model.split,
    revpairwise ~ Num * Session * Group,
    at = list(
      Session = sessions.test,
      Num = c(first.num, last.num),
      Group = group
    )
  )$contrasts,
  infer = TRUE)
  
  s.split = filter(
    s.split,
    contrast == paste(
      last.num,
      last.session,
      group,
      '-',
      first.num,
      first.session,
      group,
      sep = ' '
    )
  )
  
  s.washout = summary(emmeans(
    model.washout,
    identity ~ Num * Session * Group,
    at = list(
      Session = washout.session,
      Num = washout.num,
      Group = group
    )
  )$contrasts,
  infer = TRUE)
  
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
  sessions.test = sessions.split
  if (length(sessions.split)  == 1) {
    sessions.split = c(sessions.split[[1]], sessions.split[[1]])
  }
  data.summary2 = filter(data.summary, Group == group)
  
  first.trial = min(filter(data.summary2, Phase == 'Split',
                           Session == sessions.split[[1]])$Trial)
  last.trial = max(filter(data.summary2, Phase == 'Split',
                          Session == sessions.split[[2]])$Trial)
  washout.trial = last.trial + 1
  
  first.trial.row = filter(data.summary2, Trial == first.trial)
  last.trial.row = filter(data.summary2, Trial == last.trial)
  washout.row = filter(data.summary2, Trial == washout.trial)
  
  first.num = first.trial.row$Num
  first.session = first.trial.row$Session
  
  last.num = last.trial.row$Num
  last.session = last.trial.row$Session
  
  washout.num = washout.row$Num
  washout.session = washout.row$Session
  
  df.animals = data.frame(Phase = c(),
                          Asym = c(),
                          Animal = c())
  
  animal_lists = list(
    filter(data.split, Group == group, Session == first.session, Num == first.num)$Animal,
    filter(
      data.split,
      Session == first.session,
        Num == first.num)$Animal,
    filter(
      data.washout,
      Group == group,
      Session == washout.session)$Animal
    )
  
  animals = Reduce(intersect, animal_lists)
  
  for (animal in animals) {
    df.aux = data.frame(
      Phase = c('Change over split'),
      Asym = c(
        filter(
          data.split,
          Session == last.session &
            Num == last.num &
            Animal == animal
        )$Asym -
          filter(
            data.split,
            Session == first.session &
              Num == first.num &
              Animal == animal
          )$Asym
      ),
      Animal = c(animal)
    )
    df.animals = rbind(df.animals, df.aux)
  }
  
  for (animal in animals) {
    df.aux = data.frame(
      Phase = c('After effect'),
      Asym = c(
        filter(
          data.washout,
          Group == group,
          Session == washout.session &
            Num == washout.num &
            Animal == animal
        )$Asym
      ),
      Animal = c(animal)
    )
    df.animals = rbind(df.animals, df.aux)
  }
  
  return(df.animals)
  
}

if (name != 'Exp4') {
  ### plot switch group
  
  group = paste(name, 'NotAtaxic:Switch', sep=':')
  color = get_group_color(group)
  
  df.switch <- make_change_df(
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S1', 'S5')
  )
  
  if (name == 'Exp5') {
    df.compare = make_change_df(
      mega.data.summary,
      model.split,
      model.washout,
      group = 'Exp3:NotAtaxic:Switch',
      sessions.split = c('S1', 'S5')
    )
  } else {
    df.compare = NULL
  }
  
  
  df.animals.switch = make_change_df_animals(
    mega.data.split,
    mega.data.washout,
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S1', 'S5')
  )
  
  plot.change.switch <-
    plot.change(df.switch, df.animals.switch, color = color, df.compare = df.compare)
  
  
  ### plot noswitch group
  
  if (name == 'Exp3') {
    sessions.split = c('S1', 'S4')
  } else if (name == 'Exp5') {
    sessions.split = c('S1', 'S5')
  }
  
  group = paste(name, 'NotAtaxic:NoSwitch', sep=':')
  color = get_group_color(group)
  
  df.noswitch <- make_change_df(
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = sessions.split
  )
  
  
  df.animals.noswitch = make_change_df_animals(
    mega.data.split,
    mega.data.washout,
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = sessions.split
  )
  
  plot.change.noswitch <-
    plot.change(df.noswitch, df.animals.noswitch, color = color)
}

if (name == 'Exp3') {
  # plot s6
  
  group = paste(name, 'NotAtaxic:Switch', sep=':')
  color = get_group_color(group)
  
  df.switch.s6 <- make_change_df(
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S6')
  )
  
  df.compare = df.switch

  
  df.animals.switch.s6 = make_change_df_animals(
    mega.data.split,
    mega.data.washout,
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S6')
  )
  
  plot.change.switch.s6 <-
    plot.change(df.switch.s6, df.animals.switch.s6, color = color, df.compare = df.compare)
  
  
  group = paste(name, 'NotAtaxic:NoSwitch', sep=':')
  color = get_group_color(group)
  
  df.noswitch.s6 <- make_change_df(
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S6')
  )
  
  df.compare = df.noswitch
  
  
  df.animals.noswitch.s6 = make_change_df_animals(
    mega.data.split,
    mega.data.washout,
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S6')
  )
  
  plot.change.noswitch.s6 <-
    plot.change(df.noswitch.s6, df.animals.noswitch.s6, color = color, df.compare = df.compare)
}




if (name == 'Exp4') {
  ## plot not ataxic
  
  group = paste(name, 'NotAtaxic:Switch', sep=':')
  color = get_group_color(group)
  
  df.switch <- make_change_df(
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S1', 'S2')
  )
  
  
  df.animals.switch = make_change_df_animals(
    mega.data.split,
    mega.data.washout,
    mega.data.summary,
    model.split,
    model.washout,
    group = group,
    sessions.split = c('S1', 'S2')
  )
  
  plot.change.switch <-
    plot.change(df.switch, df.animals.switch, color = color)
  
  ## plot ataxic ???
  
}
