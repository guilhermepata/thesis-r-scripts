source("build_mega_model.R")
source("my_functions.R")

plot.initial.error <- function(data.split,
                               data.split.summary,
                               groups,
                               xlim = c('S1', 'S2', 'S3', 'S4', 'S5')) {
  p = ggplot()
  
  p = p +
    
    # geom_line(
    #   data = filter(data.split.summary, Group %in% groups, Num == 0),
    #   aes(x = Session, y = Fit, group = Group, color = Group),
    #   size = 0.5,
    #   position = position_dodge(0.5)
    # ) +
    geom_point(
      data = filter(data.split.summary, Group %in% groups, Num == 0),
      aes(
        x = Session,
        y = Fit,
        group = Group,
        color = Group
      ),
      size = 2,
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
    `if`(dark, theme_black(), theme_classic()) + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Initial error (mm)")
  
  return(p)
}

if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (plot.errors = plot.initial.error(mega.data.split,
                                    mega.data.split.summary,
                                    groups = groups))
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.errors = plot.initial.error(mega.data.split,
                                    mega.data.split.summary,
                                    groups = groups))
}


if (name == 'Exp4') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  (plot.errors = plot.initial.error(mega.data.split,
                                    mega.data.split.summary,
                                    groups = groups))
}
