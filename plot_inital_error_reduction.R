source("build_mega_model.R")
source("my_functions.R")

plot.initial.error <- function(data.split,
                               data.split.summary,
                               group,
                               group.compare = NULL,
                               # color = '#619CFF',
                               show.animals = FALSE,
                               xlim = c('S1', 'S2', 'S3', 'S4', 'S5')) {
  color = get_group_color(group)
  color.light <- lighten(color)
  color.dark <- darken(color)
  
  colors = c()
  shapes = c()
  if (!is.null(group.compare)) {
    color.compare = get_group_color(group.compare)
    groups = c(group, group.compare)
    colors[group] = color
    colors[group.compare] = color.compare
    shapes[group] = 16
    shapes[group.compare] = 17
  } else {
    groups = c(group)
    colors[group] = color
    shapes[group] = 16
  }
  
  p = ggplot()
  
  if (show.animals) {
    p = p +
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
      )
  }
  
  # if (!is.null(group.compare)) {
  #   p = p +
  #     geom_point(
  #       data = filter(data.split.summary, Group == group.compare, Num == 0),
  #       aes(x = Session, y = Fit, group = 1),
  #       size = 1,
  #       stroke = 1,
  #       shape = 2,
  #       color = color.compare,
  #       position = position_dodge(-0.5),
  #       alpha = 0.8
  #     ) +
  #     geom_line(
  #       data = filter(data.split.summary, Group == group.compare, Num == 0),
  #       aes(x = Session, y = Fit, group = 1),
  #       size = .7,
  #       # linetype = "dotted",
  #       color = color.compare,
  #       position = position_dodge(-0.5),
  #       alpha = 0.8
  #     ) +
  #     geom_errorbar(
  #       data = filter(data.split.summary, Group == group.compare, Num == 0),
  #       aes(
  #         x = Session,
  #         y = Fit,
  #         ymin = Lower,
  #         ymax = Upper,
  #         group = 1
  #       ),
  #       width = .2,
  #       position = position_dodge(-0.5),
  #       color = color.compare
  #     )
  # }
  
  p = p +
    geom_point(
      data = filter(data.split.summary, Group %in% groups, Num == 0),
      aes(x = Session, y = Fit, group = Group, color = Group, shape = Group),
      size = 3,
      position = position_dodge(0.5)
    ) +
    geom_line(
      data = filter(data.split.summary, Group %in% groups, Num == 0),
      aes(x = Session, y = Fit, group = Group, color = Group),
      size = 1,
      position = position_dodge(0.5)
    ) +
    geom_errorbar(
      data = filter(data.split.summary, Group %in% groups, Num == 0),
      aes(
        x = Session,
        y = Fit,
        ymin = Lower,
        ymax = Upper,
        group = Group,
        color = Group,
      ),
      width = .2,
      position = position_dodge(0.5)
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               alpha = 0.5) +
    xlim(xlim) +
    theme_classic() + theme(legend.position = "none") +
    scale_fill_manual(values=colors) +
    scale_color_manual(values=colors) + 
    scale_shape_manual(values=shapes) +
    labs(x = "Session", y = "Initial error (mm)")
  
  return(p)
}


if (name == 'Exp3') {
  ### Plot switch group
  {
    group = paste(name, 'NotAtaxic:Switch', sep = ':')
    color = get_group_color(group)
    
    (
      plot.errors.switch <-
        plot.initial.error(mega.data.split,
                           mega.data.split.summary,
                           group = group)
    )
  }
  
  ### Plot noswitch group
  {
    group = paste(name, 'NotAtaxic:NoSwitch', sep = ':')
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S4')
    
    (
      plot.errors.noswitch <-
        plot.initial.error(
          mega.data.split,
          mega.data.split.summary,
          group = group,
          xlim = xlim
        )
    )
  }
}

if (name == 'Exp5') {
  ### Plot switch group
  {
    group = paste(name, 'NotAtaxic:Switch', sep = ':')
    color = get_group_color(group)
    
    (
      plot.errors.switch <-
        plot.initial.error(
          mega.data.split,
          mega.data.split.summary,
          group = group,
          group.compare = 'Exp3:NotAtaxic:Switch'
        )
    )
  }
  
  ### Plot noswitch group
  {
    group = paste(name, 'NotAtaxic:NoSwitch', sep = ':')
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S4', 'S5')
    
    (
      plot.errors.noswitch <-
        plot.initial.error(
          mega.data.split,
          mega.data.split.summary,
          group = group,
          group.compare = 'Exp3:NotAtaxic:Switch',
          xlim = xlim
        )
    )
  }
}

if (name == 'Exp4') {
  ### Plot switch group
  {
    group = paste(name, 'NotAtaxic:Switch', sep = ':')
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S5')
    
    (
      plot.errors.switch <-
        plot.initial.error(
          mega.data.split,
          mega.data.split.summary,
          group = group,
          xlim = xlim
        )
    )
  }
  
  ### Plot ataxic group
  {
    group = paste(name, 'Ataxic:Switch', sep = ':')
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S5')
    
    (
      plot.errors.ataxic <-
        plot.initial.error(
          mega.data.split,
          mega.data.split.summary,
          group = group,
          xlim = xlim
        )
    )
  }
}
