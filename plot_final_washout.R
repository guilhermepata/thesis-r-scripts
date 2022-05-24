source("build_mega_model.R")
source("my_functions.R")

name = 'Exp3'

# plot.final.washout <- function(washout.values,
#                                group='NotAtaxic:Switch',
#                                xlim=c('S1', 'S2', 'S3', 'S4', 'S5'),
#                                color='#619CFF')  {
#
#   p = ggplot() +
#     geom_point(data=filter(washout.values,Group==group), aes(x=Session, y=emmean), color=color, size=3) +
#     geom_line(data=filter(washout.values,Group==group), aes(x=Session, y=emmean, group = 0), color=color, size= 1) +
#     geom_errorbar(data=filter(washout.values,Group==group), aes(x=Session, y=emmean, ymin=lower.CL, ymax=upper.CL), width=.2,
#                   position=position_dodge(0.05),
#                   color=color) +
#     geom_hline(yintercept=c(0), linetype="dashed", alpha = 0.5) +
#     xlim(xlim) +
#     theme_classic() + theme(legend.position="none") +
#     labs(x="Session", y = "Step length asym. (mm)")
#
#   return(p)
# }


plot.final.washout <- function(data.washout,
                               data.washout.summary,
                               group,
                               color = '#619CFF',
                               xlim = c('S1', 'S2', 'S3', 'S4', 'S5')) {
  color.light <- lighten(color)
  
  final.washout.num = max(filter(data.washout.summary,
                                 Group == group,
                                 Session %in% xlim,)$Num)
  
  p = ggplot() +
    geom_point(
      data = filter(data.washout, Group == group, Num == final.washout.num),
      aes(x = Session, y = Asym),
      alpha = 0.5,
      color = color.light
    ) +
    geom_line(
      data = filter(data.washout, Group == group, Num == final.washout.num),
      aes(x = Session, y = Asym, group = Animal),
      alpha = 0.5,
      color = color.light
    ) +
    
    geom_point(
      data = filter(data.washout.summary, Group == group, Num == final.washout.num),
      aes(x = Session, y = Fit),
      size = 3,
      color = color
    ) +
    geom_line(
      data = filter(data.washout.summary, Group == group, Num == final.washout.num),
      aes(x = Session, y = Fit, group = 0),
      size = 1,
      color = color
    ) +
    geom_errorbar(
      data = filter(data.washout.summary, Group == group, Num == final.washout.num),
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



# for Exp3
if (name == 'Exp3') {
  final.washout.num = max(filter(
    mega.data.washout.summary,
    Group == 'Exp3:NotAtaxic:Switch',
    Session %in% c('S1', 'S2', 'S3', 'S4', 'S5'),
  )$Num)
  
  (washout.values = as.data.frame(summary(
    emmeans(
      model.washout,
      ~ Session * Group,
      at = list(
        Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
        Group = 'Exp3:NotAtaxic:Switch',
        Num = final.washout.num
      )
    ), infer = TRUE
  )))
  
  group = 'Exp3:NotAtaxic:Switch'
  color = get_group_color(group)
  xlim = c('S1', 'S2', 'S3', 'S4', 'S5')
  
  # (plot.final.washout.switch = plot.final.washout(washout.values,
  #                              color=color,
  #                              group=group,
  #                              xlim=xlim)
  # )
  
  (
    plot.final.washout.switch = plot.final.washout(
      mega.data.washout,
      mega.data.washout.summary,
      group = group,
      color = color,
      xlim = xlim
    )
  )
  
}



# if (name == 'Exp5') {
#   (washout.test = summary(emtrends(
#     model.first.lr, consec ~ Session | Group,
#     var = 'Perc',
#     at=list(Session=c('S1','S2','S3','S4','S5')
#             # Group='NotAtaxic:Switch'
#     )
#   ))
#   )
#
#   (learning.rate.values = as.data.frame(emtrends(
#     model.first.lr, consec ~ Session | Group,
#     var = 'Perc',
#     at=list(Session=c('S1','S2','S3','S4','S5')
#             # Group='NotAtaxic:Switch'
#     )
#   )$emtrends)
#   )
#
#   secs_perc = 6
#   learning.rate.values[3:7] <- learning.rate.values[3:7] / secs_perc
#
#
#   # switch
#   {
#     group = 'NotAtaxic:Switch'
#     color = get_group_color(group)
#     xlim=c('S1', 'S2', 'S3', 'S4', 'S5')
#
#     (learning.rate.plot.switch = learning.rate.plot(learning.rate.values,
#                                                     color=color,
#                                                     group=group,
#                                                     xlim=xlim)
#     )
#   }
#
#   # no switch
#   {
#     group = 'NotAtaxic:NoSwitch'
#     color = get_group_color(group)
#     xlim=c('S1', 'S2', 'S3', 'S4', 'S5')
#
#     (learning.rate.plot.noswitch = learning.rate.plot(learning.rate.values,
#                                                       color=color,
#                                                       group=group,
#                                                       xlim=xlim)
#     )
#   }
#
# }
#
#
# if (name == 'Exp4') {
#   (learning.rate.test = summary(emtrends(
#     model.first.lr, consec ~ Session | Group,
#     var = 'Perc',
#     at=list(Session=c('S1','S2','S3','S5')
#             # Group='NotAtaxic:Switch'
#     )
#   ))
#   )
#
#   (learning.rate.values = as.data.frame(emtrends(
#     model.first.lr, consec ~ Session | Group,
#     var = 'Perc',
#     at=list(Session=c('S1','S2','S3','S5')
#             # Group='NotAtaxic:Switch'
#     )
#   )$emtrends)
#   )
#
#   secs_perc = 6
#   learning.rate.values[3:7] <- learning.rate.values[3:7] / secs_perc
#
#
#   # ataxic
#   {
#     group = 'Ataxic:Switch'
#     color = get_group_color(group)
#     xlim=c('S1', 'S2', 'S3', 'S5')
#
#     (learning.rate.plot.ataxic = learning.rate.plot(learning.rate.values,
#                                                     color=color,
#                                                     group=group,
#                                                     xlim=xlim)
#     )
#   }
#
#   # not ataxic
#   {
#     group = 'NotAtaxic:Switch'
#     color = get_group_color(group)
#     xlim=c('S1', 'S2', 'S3', 'S5')
#
#     (learning.rate.plot.switch = learning.rate.plot(learning.rate.values,
#                                                     color=color,
#                                                     group=group,
#                                                     xlim=xlim)
#     )
#   }
#
# }
#
#
#
