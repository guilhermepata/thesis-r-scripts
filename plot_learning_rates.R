source("build_mega_model.R")
source("my_functions.R")


learning.rate.values = summary(emtrends(model.split, ~ Session * Group, var = 'Num'), infer = TRUE)

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
      size = 3,
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
    theme_classic() + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Learning rate (mm/trial)")
  
  return(p)
}

if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (plot.learning.rate.exp3 = plot.learning.rates(learning.rate.values,
                                                 groups = groups))
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.learning.rate.exp3 = plot.learning.rates(learning.rate.values,
                                                 groups = groups))
}

# # for Exp4
# if (name == 'Exp4') {
#   (learning.rate.test = summary(emtrends(
#     model.split, pairwise ~ Session | Group,
#     var = 'Num',
#     at=list(Session=c('S1','S2','S3','S5'),
#             Group=c('Exp4:NotAtaxic:Switch', 'Exp4:Ataxic:Switch')
#     )
#   )$contrasts, infer=TRUE)
#   )
#
#   (learning.rate.values2 = summary(emtrends(
#     model.split, identity ~ Session * Group,
#     var = 'Num',
#     at=list(Session=c('S1','S2','S3','S5'),
#             Group=c('Exp4:NotAtaxic:Switch', 'Exp4:Ataxic:Switch')
#     )
#   )$emtrends, infer=TRUE)
#   )
#
#   (learning.rate.values = as.data.frame(emtrends(
#     model.split, consec ~ Session | Group,
#     var = 'Num',
#     at=list(Session=c('S1','S2','S3','S5'),
#             Group=c('Exp4:NotAtaxic:Switch', 'Exp4:Ataxic:Switch')
#     )
#   )$emtrends)
#   )
#
#
#   # ataxic
#   {group = paste(name, 'Ataxic:Switch', sep=':')
#
#   color = get_group_color(group)
#
#   (plot.learning.rate.ataxic = ggplot() +
#       geom_point(data=filter(learning.rate.values,Group==group), aes(x=Session, y=Num.trend), size=3, color=color) +
#       geom_line(data=filter(learning.rate.values,Group==group), aes(x=Session, y=Num.trend, group = 0), size= 1, color=color) +
#       geom_errorbar(data=filter(learning.rate.values,Group==group), aes(x=Session, y=Num.trend, ymin=lower.CL, ymax=upper.CL), width=.2,
#                     position=position_dodge(0.05), color=color) +
#       geom_hline(yintercept=c(0), linetype="dashed", alpha=0.5) +
#       xlim('S1', 'S2', 'S3', 'S5') +
#       theme_classic() + theme(legend.position="none") +
#       labs(x="Session", y = "Step length asym. l.r. (mm/trial)")
#   )
#   }
#
#   # not ataxic
#   {group = paste(name, 'NotAtaxic:Switch', sep=':')
#
#     color = get_group_color(group)
#
#     (plot.learning.rate.notataxic = ggplot() +
#       geom_point(data=filter(learning.rate.values,Group==group), aes(x=Session, y=Num.trend), size=3, color=color) +
#       geom_line(data=filter(learning.rate.values,Group==group), aes(x=Session, y=Num.trend, group = 0), size= 1, color=color) +
#       geom_errorbar(data=filter(learning.rate.values,Group==group), aes(x=Session, y=Num.trend, ymin=lower.CL, ymax=upper.CL), width=.2,
#                     position=position_dodge(0.05), color=color) +
#       geom_hline(yintercept=c(0), linetype="dashed", alpha=0.5) +
#       xlim('S1', 'S2', 'S3', 'S5') +
#       theme_classic() + theme(legend.position="none") +
#       labs(x="Session", y = "Step length asym. l.r. (mm/trial)")
#     )
#     }
#
#
# }
