source("build_mega_model.R")
source("my_functions.R")


plot.final.washout <- function(data.washout,
                               data.washout.summary,
                               groups,
                               xlim = c('S1', 'S2', 'S3', 'S4', 'S5')) {
  is.max.num <-
    function(num, session, group, data = data.washout.summary) {
      max.num = max(filter(data, Session == session, Group == group)$Num)
      return(num == max.num)
    }
  
  p = ggplot()
  
  p = p +
    
    # geom_line(
    #   data = filter(data.washout.summary, Group %in% groups, is.max.num(Num, Session, Group)),
    #   aes(x = Session, y = Fit, group = Group, color = Group),
    #   size = 0.5,
    #   position = position_dodge(0.5)
    # ) +
    geom_point(
      data = filter(
        data.washout.summary,
        Group %in% groups,
        is.max.num(Num, Session, Group)
      ),
      aes(
        x = Session,
        y = Fit,
        group = Group,
        color = Group
      ),
      size = 1,
      position = position_dodge(0.5)
    ) +
    geom_errorbar(
      data = filter(
        data.washout.summary,
        Group %in% groups,
        is.max.num(Num, Session, Group)
      ),
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
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Post-adaptation final error (mm)")
  
  return(p)
}

if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (
    plot.final.washout = plot.final.washout(mega.data.washout,
                                            mega.data.washout.summary,
                                            groups = groups)
  )
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (
    plot.final.washout = plot.final.washout(mega.data.washout,
                                            mega.data.washout.summary,
                                            groups = groups)
  )
}


if (name == 'Exp4') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  (
    plot.final.washout = plot.final.washout(
      mega.data.washout,
      mega.data.washout.summary,
      groups = groups,
      xlim = c('S1', 'S2')
    )
  )
}