source("build_mega_model.R")
source("my_functions.R")




plot.learning.rates <- function(model.split,
                                groups,
                                nums = NULL,
                                xlim = c('S1', 'S2', 'S3', 'S4', 'S5'),
                                draw.lines = FALSE) {
  
  values = data.frame()
  
  if (is.null(nums)) {nums = list()}
  
  for (group in groups) {
    if (is.null(nums[group][[1]])) {nums[group] = list(c(0, 3))}
    values.aux = summary(emmeans(
      model.split, skipconsecavg ~ Num * Session | Group,  
      at=list(Session=c('S1','S2','S3','S4','S5'), 
              Num=nums[group][[1]],
              Group = group)
    ),infer = TRUE)$contrasts
    values = rbind(values, values.aux)
  }
 
  
  
  
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