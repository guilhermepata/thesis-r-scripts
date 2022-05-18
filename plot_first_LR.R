name = 'Exp5'
source("build_model_first_lr.R")
source("my_functions.R")

plot.initial.lr <- function(learning.rate.values,
                            group = 'NotAtaxic:Switch',
                            xlim = c('S1', 'S2', 'S3', 'S4', 'S5'),
                            color = '#619CFF')  {
  p = ggplot() +
    geom_point(
      data = filter(learning.rate.values, Group == group),
      aes(x = Session, y = Perc.trend),
      color = color,
      size = 3
    ) +
    geom_line(
      data = filter(learning.rate.values, Group == group),
      aes(x = Session, y = Perc.trend, group = 0),
      color = color,
      size = 1
    ) +
    geom_errorbar(
      data = filter(learning.rate.values, Group == group),
      aes(
        x = Session,
        y = Perc.trend,
        ymin = lower.CL,
        ymax = upper.CL
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
    labs(x = "Session", y = "Step length asym. l.r. (mm/s)")
  
  return(p)
}


# for Exp3
if (name == 'Exp3') {
  (learning.rate.test = summary(
    emtrends(
      model.first.lr,
      consec ~ Session | Group,
      var = 'Perc',
      at = list(
        Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
        Group = 'NotAtaxic:Switch'
      )
    )
  ))
  
  (learning.rate.values = as.data.frame(
    emtrends(
      model.first.lr,
      consec ~ Session | Group,
      var = 'Perc',
      at = list(Session = c('S1', 'S2', 'S3', 'S4', 'S5')
                # Group='NotAtaxic:Switch' )
      )$emtrends
    ))
    
    secs_perc = 6
    learning.rate.values[3:7] <- learning.rate.values[3:7] / secs_perc
    
    group = 'NotAtaxic:Switch'
    color = get_group_color(group)
    xlim = c('S1', 'S2', 'S3', 'S4', 'S5')
    
    (plot.initial.lr = plot.initial.lr(
      learning.rate.values,
      color = color,
      group = group,
      xlim = xlim
    ))
    
}



if (name == 'Exp5') {
  (learning.rate.test = summary(
    emtrends(
      model.first.lr,
      consec ~ Session | Group,
      var = 'Perc',
      at = list(Session = c('S1', 'S2', 'S3', 'S4', 'S5')
                # Group='NotAtaxic:Switch' )
      )
    ))
   
   (learning.rate.values = as.data.frame(
     emtrends(
       model.first.lr,
       consec ~ Session | Group,
       var = 'Perc',
       at = list(Session = c('S1', 'S2', 'S3', 'S4', 'S5')
                 # Group='NotAtaxic:Switch' )
       )$emtrends
     ))
     
     secs_perc = 6
     learning.rate.values[3:7] <- learning.rate.values[3:7] / secs_perc
     
     
     # switch
     {
       group = 'NotAtaxic:Switch'
       color = get_group_color(group)
       xlim = c('S1', 'S2', 'S3', 'S4', 'S5')
       
       (
         plot.initial.lr.switch = plot.initial.lr(
           learning.rate.values,
           color = color,
           group = group,
           xlim = xlim
         )
       )
     }
     
     # no switch
     {
       group = 'NotAtaxic:NoSwitch'
       color = get_group_color(group)
       xlim = c('S1', 'S2', 'S3', 'S4', 'S5')
       
       (
         plot.initial.lr.noswitch = plot.initial.lr(
           learning.rate.values,
           color = color,
           group = group,
           xlim = xlim
         )
       )
     }
     
}


if (name == 'Exp4') {
  (learning.rate.test = summary(
    emtrends(
      model.first.lr,
      consec ~ Session | Group,
      var = 'Perc',
      at = list(Session = c('S1', 'S2', 'S3', 'S5')
                # Group='NotAtaxic:Switch' )
      )
    ))
   
   (learning.rate.values = as.data.frame(
     emtrends(
       model.first.lr,
       consec ~ Session | Group,
       var = 'Perc',
       at = list(Session = c('S1', 'S2', 'S3', 'S5')
                 # Group='NotAtaxic:Switch' )
       )$emtrends
     ))
     
     secs_perc = 6
     learning.rate.values[3:7] <- learning.rate.values[3:7] / secs_perc
     
     
     # ataxic
     {
       group = 'Ataxic:Switch'
       color = get_group_color(group)
       xlim = c('S1', 'S2', 'S3', 'S5')
       
       (
         plot.initial.lr.ataxic = plot.initial.lr(
           learning.rate.values,
           color = color,
           group = group,
           xlim = xlim
         )
       )
     }
     
     # not ataxic
     {
       group = 'NotAtaxic:Switch'
       color = get_group_color(group)
       xlim = c('S1', 'S2', 'S3', 'S5')
       
       (
         plot.initial.lr.switch = plot.initial.lr(
           learning.rate.values,
           color = color,
           group = group,
           xlim = xlim
         )
       )
     }
     
}
