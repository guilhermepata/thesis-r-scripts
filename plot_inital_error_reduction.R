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
      aes(x = Session, y = Fit, group = Group, color = Group),
      size = 3,
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
    scale_fill_manual(values=get_group_colors(groups)) +
    scale_color_manual(values=get_group_colors(groups)) + 
    labs(x = "Session", y = "Initial error (mm)")
  
  return(p)
}

if (name == 'Exp3') {
  
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
 (plot.errors.exp3 = plot.initial.error(mega.data.split, 
                                        mega.data.split.summary,
                                        groups = groups))
}

if (name == 'Exp5') {
  
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.errors.exp3 = plot.initial.error(mega.data.split, 
                                         mega.data.split.summary,
                                         groups = groups))
}


if (name == 'Exp4') {
  
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  (plot.errors.exp3 = plot.initial.error(mega.data.split, 
                                         mega.data.split.summary,
                                         groups = groups))
}

# if (name == 'Exp3') {
#   ### Plot switch group
#   {
#     group = paste(name, 'NotAtaxic:Switch', sep = ':')
#     color = get_group_color(group)
#     
#     (
#       plot.errors.switch <-
#         plot.initial.error(mega.data.split,
#                            mega.data.split.summary,
#                            groups = c(group))
#     )
#   }
#   
#   ### Plot noswitch group
#   {
#     group = paste(name, 'NotAtaxic:NoSwitch', sep = ':')
#     color = get_group_color(group)
#     xlim = c('S1', 'S2', 'S3', 'S4')
#     
#     (
#       plot.errors.noswitch <-
#         plot.initial.error(
#           mega.data.split,
#           mega.data.split.summary,
#           groups = c(group),
#           xlim = xlim
#         )
#     )
#   }
# }
# 
# if (name == 'Exp5') {
#   ### Plot switch group
#   {
#     group = paste(name, 'NotAtaxic:Switch', sep = ':')
#     color = get_group_color(group)
#     
#     (
#       plot.errors.switch <-
#         plot.initial.error(
#           mega.data.split,
#           mega.data.split.summary,
#           groups = c(group),
#           group.compare = 'Exp3:NotAtaxic:Switch'
#         )
#     )
#   }
#   
#   ### Plot noswitch group
#   {
#     group = paste(name, 'NotAtaxic:NoSwitch', sep = ':')
#     color = get_group_color(group)
#     xlim = c('S1', 'S2', 'S3', 'S4', 'S5')
#     
#     (
#       plot.errors.noswitch <-
#         plot.initial.error(
#           mega.data.split,
#           mega.data.split.summary,
#           groups = c(group, 'Exp3:NotAtaxic:Switch'),
#           xlim = xlim
#         )
#     )
#   }
# }
# 
# if (name == 'Exp4') {
#   ### Plot switch group
#   {
#     group = paste(name, 'NotAtaxic:Switch', sep = ':')
#     color = get_group_color(group)
#     xlim = c('S1', 'S2', 'S3', 'S5')
#     
#     (
#       plot.errors.switch <-
#         plot.initial.error(
#           mega.data.split,
#           mega.data.split.summary,
#           groups = group,
#           xlim = xlim
#         )
#     )
#   }
#   
#   ### Plot ataxic group
#   {
#     group = paste(name, 'Ataxic:Switch', sep = ':')
#     color = get_group_color(group)
#     xlim = c('S1', 'S2', 'S3', 'S5')
#     
#     (
#       plot.errors.ataxic <-
#         plot.initial.error(
#           mega.data.split,
#           mega.data.split.summary,
#           groups = group,
#           xlim = xlim
#         )
#     )
#   }
# }
