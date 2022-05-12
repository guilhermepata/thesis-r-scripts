source("build_model.R")
source("my_functions.R")

# for Exp4
if (name == 'Exp4') {
  (learning.rate.test = summary(emtrends(
    model.split, pairwise ~ Session | Group,
    var = 'Num',
    at=list(Session=c('S1','S2','S3','S5')
            # Group='NotAtaxic:Switch' 
    )
  )$contrasts, infer=TRUE)
  )
  
  (learning.rate.values2 = summary(emtrends(
    model.split, identity ~ Session * Group,
    var = 'Num',
    at=list(Session=c('S1','S2','S3','S5')
            # Group='NotAtaxic:Switch' 
    )
  )$emtrends, infer=TRUE)
  )
  
  (learning.rate.values = as.data.frame(emtrends(
    model.split, consec ~ Session | Group,
    var = 'Num',
    at=list(Session=c('S1','S2','S3','S5')
            # Group='NotAtaxic:Switch' 
    )
  )$emtrends)
  )
  
  color = get_group_color('Ataxic:Switch')
  
  # ataxic
  (plot.learning.rate.ataxic = ggplot() +
      # geom_point(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
      # geom_line(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
      geom_point(data=filter(learning.rate.values,Group=='Ataxic:Switch'), aes(x=Session, y=Num.trend), size=3, color=color) +
      geom_line(data=filter(learning.rate.values,Group=='Ataxic:Switch'), aes(x=Session, y=Num.trend, group = 0), size= 1, color=color) +
      geom_errorbar(data=filter(learning.rate.values,Group=='Ataxic:Switch'), aes(x=Session, y=Num.trend, ymin=lower.CL, ymax=upper.CL), width=.2,
                    position=position_dodge(0.05), color=color) +
      geom_hline(yintercept=c(0), linetype="dashed", alpha=0.5) +
      xlim('S1', 'S2', 'S3', 'S5') +
      theme_classic() + theme(legend.position="none") +
      labs(x="Session", y = "Step length asym. l.r. (mm/trial)")
  )
  
  color = get_group_color('NotAtaxic:Switch')
  
  # not ataxic
  (plot.learning.rate.notataxic = ggplot() +
      # geom_point(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
      # geom_line(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
      geom_point(data=filter(learning.rate.values,Group=='NotAtaxic:Switch'), aes(x=Session, y=Num.trend), size=3, color=color) +
      geom_line(data=filter(learning.rate.values,Group=='NotAtaxic:Switch'), aes(x=Session, y=Num.trend, group = 0), size= 1, color=color) +
      geom_errorbar(data=filter(learning.rate.values,Group=='NotAtaxic:Switch'), aes(x=Session, y=Num.trend, ymin=lower.CL, ymax=upper.CL), width=.2,
                    position=position_dodge(0.05), color=color) +
      geom_hline(yintercept=c(0), linetype="dashed", alpha=0.5) +
      xlim('S1', 'S2', 'S3', 'S5') +
      theme_classic() + theme(legend.position="none") +
      labs(x="Session", y = "Step length asym. l.r. (mm/trial)")
  )
  
  # list[plot.learning.rate.notataxic, plot.learning.rate.ataxic] = set_equal_y_lims(plot.learning.rate.notataxic, plot.learning.rate.ataxic)
  # 
  # library(patchwork) 
  # 
  # (plot.learning.rate.ataxic + plot.learning.rate.notataxic)
  
}
