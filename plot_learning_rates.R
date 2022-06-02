source("build_mega_model.R")
source("my_functions.R")


learning.rate.values = summary(emtrends(
  model.split,
  ~ Session * Group,
  var = 'Num',
  adjust = 'none'
),
infer = TRUE)

plot.learning.rates <- function(values,
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
          y = Num.trend,
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
        y = Num.trend,
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
        y = Num.trend,
        ymin = lower.CL,
        ymax = upper.CL,
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
    labs(x = "Session", y = "Session learning rate (mm/trial)")
  
  return(p)
}

if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (plot.learning.rate = plot.learning.rates(learning.rate.values,
                                            groups = groups))
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.learning.rate = plot.learning.rates(learning.rate.values,
                                            groups = groups))
}

if (name == 'Exp4') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  (plot.learning.rate = plot.learning.rates(learning.rate.values,
                                            groups = groups))
}
