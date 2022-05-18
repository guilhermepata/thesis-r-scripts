source("build_model.R")
source("my_functions.R")

plot.initial.error <- function(data.split,
                               data.split.summary,
                               group,
                               color = '#619CFF',
                               xlim = c('S1', 'S2', 'S3', 'S4', 'S5')) {
  color.light <- lighten(color)
  
  p = ggplot() +
    geom_point(
      data = filter(data.split, Group == group, Num == 0),
      aes(x = Session, y = Asym),
      alpha = 0.5,
      color = color.light
    ) +
    geom_line(
      data = filter(data.split, Group == group, Num == 0),
      aes(x = Session, y = Asym, group = Animal),
      alpha = 0.5,
      color = color.light
    ) +
    
    geom_point(
      data = filter(data.split.summary, Group == group, Num == 0),
      aes(x = Session, y = Fit),
      size = 3,
      color = color
    ) +
    geom_line(
      data = filter(data.split.summary, Group == group, Num == 0),
      aes(x = Session, y = Fit, group = 0),
      size = 1,
      color = color
    ) +
    geom_errorbar(
      data = filter(data.split.summary, Group == group, Num == 0),
      aes(
        x = Session,
        y = Fit,
        ymin = Lower,
        ymax = Upper
      ),
      width = .2,
      position = position_dodge(0.05),
      color = color
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               alpha = 0.5) +
    xlim(xlim) +
    theme_classic() + theme(legend.position = "none") +
    labs(x = "Session", y = "Step length asym. (mm)")
  
  return(p)
}


if (name == 'Exp3') {
  ### Plot switch group
  {
    group = 'NotAtaxic:Switch'
    color = get_group_color(group)
    
    (
      plot.errors.switch <-
        plot.initial.error(
          data.split,
          data.split.summary,
          group = group,
          color = color
        )
    )
  }
  
  ### Plot noswitch group
  {
    group = 'NotAtaxic:NoSwitch'
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S4')
    
    (
      plot.errors.noswitch <-
        plot.initial.error(
          data.split,
          data.split.summary,
          group = group,
          color = color,
          xlim = xlim
        )
    )
  }
}

if (name == 'Exp5') {
  ### Plot switch group
  {
    group = 'NotAtaxic:Switch'
    color = get_group_color(group)
    
    (
      plot.errors.switch <-
        plot.initial.error(
          data.split,
          data.split.summary,
          group = group,
          color = color
        )
    )
  }
  
  ### Plot noswitch group
  {
    group = 'NotAtaxic:NoSwitch'
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S4', 'S5')
    
    (
      plot.errors.noswitch <-
        plot.initial.error(
          data.split,
          data.split.summary,
          group = group,
          color = color,
          xlim = xlim
        )
    )
  }
}

if (name == 'Exp4') {
  ### Plot switch group
  {
    group = 'NotAtaxic:Switch'
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S5')
    
    (
      plot.errors.switch <-
        plot.initial.error(
          data.split,
          data.split.summary,
          group = group,
          color = color,
          xlim = xlim
        )
    )
  }
  
  ### Plot ataxic group
  {
    group = 'Ataxic:Switch'
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S5')
    
    (
      plot.errors.ataxic <-
        plot.initial.error(
          data.split,
          data.split.summary,
          group = group,
          color = color,
          xlim = xlim
        )
    )
  }
}
