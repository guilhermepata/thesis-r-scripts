source("build_model_first_lr.R")
source("my_functions.R")


(first.lr.values = summary(
  emtrends(
    model.first.lr,
    ~ Session * Group,
    var = 'Perc',
    adjust = 'none'
  ),
  infer = TRUE
))

plot.first.lr.func <- function(values,
                          groups,
                          xlim = c('S1', 'S2', 'S3', 'S4', 'S5'),
                          draw.lines = FALSE) {
  p = ggplot()
  
  if (draw.lines) {
    p = p +
      geom_line(
        data = filter(values, Group %in% groups),
        aes(
          x = Session,
          y = Perc.trend / 6,
          group = Group,
          color = Group
        ),
        size = 0.5,
        linetype = 'dotted',
        position = position_dodge(0.5)
      )
  }
  
  p = p +
    geom_point(
      data = filter(values, Group %in% groups),
      aes(
        x = Session,
        y = Perc.trend / 6,
        group = Group,
        color = Group
      ),
      size = 1,
      position = position_dodge(0.5)
    ) +
    
    geom_errorbar(
      data = filter(values, Group %in% groups),
      aes(
        x = Session,
        y = Perc.trend / 6,
        ymin = lower.CL / 6,
        ymax = upper.CL / 6,
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
    `if`(dark, theme_black(), theme_classic()) + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "1st trial learning rate (mm/s)")
  
  return(p)
}

if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (plot.first.lr = plot.first.lr.func(first.lr.values,
                                 groups = groups))
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.first.lr = plot.first.lr.func(first.lr.values,
                                 groups = groups))
}

if (name == 'Exp4') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  (plot.first.lr = plot.first.lr.func(
    first.lr.values,
    groups = groups,
    xlim = c('S1', 'S2', 'S3', 'S5')
  ))
}
