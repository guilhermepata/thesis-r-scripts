source("build_model.R")
source("my_functions.R")

# for Exp4
{
  (learning.rate.test = summary(emtrends(
    model.split, consec ~ Group | Session,
    var = 'Num',
    at=list(Session=c('S1','S2','S3','S5')
            # Group='NotAtaxic:Switch' 
    )
  ))
  )
  
  (learning.rate.values = as.data.frame(emtrends(
    model.split, consec ~ Session | Group,
    var = 'Num',
    at=list(Session=c('S1','S2','S3','S5')
            # Group='NotAtaxic:Switch' 
    )
  )$emtrends)
  )
  
  
  # ataxic
  (plot.learning.rate.ataxic = ggplot() +
      # geom_point(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
      # geom_line(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
      geom_point(data=filter(learning.rate.values,Group=='Ataxic:Switch'), aes(x=Session, y=Num.trend), size=3) +
      geom_line(data=filter(learning.rate.values,Group=='Ataxic:Switch'), aes(x=Session, y=Num.trend, group = 0), size= 1) +
      geom_errorbar(data=filter(learning.rate.values,Group=='Ataxic:Switch'), aes(x=Session, y=Num.trend, ymin=lower.CL, ymax=upper.CL), width=.2,
                    position=position_dodge(0.05)) +
      geom_hline(yintercept=c(0), linetype="dotted") +
      xlim('S1', 'S2', 'S3', 'S4', 'S5') +
      theme_classic() + theme(legend.position="none") +
      labs(x="Session", y = "Step length asym. l.r. (mm/trial)")
  )
  
  # not ataxic
  (plot.learning.rate.notataxic = ggplot() +
      # geom_point(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
      # geom_line(data=filter(data.first.lr, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
      geom_point(data=filter(learning.rate.values,Group=='NotAtaxic:Switch'), aes(x=Session, y=Num.trend), size=3) +
      geom_line(data=filter(learning.rate.values,Group=='NotAtaxic:Switch'), aes(x=Session, y=Num.trend, group = 0), size= 1) +
      geom_errorbar(data=filter(learning.rate.values,Group=='NotAtaxic:Switch'), aes(x=Session, y=Num.trend, ymin=lower.CL, ymax=upper.CL), width=.2,
                    position=position_dodge(0.05)) +
      geom_hline(yintercept=c(0), linetype="dotted") +
      xlim('S1', 'S2', 'S3', 'S4', 'S5') +
      theme_classic() + theme(legend.position="none") +
      labs(x="Session", y = "Step length asym. l.r. (mm/trial)")
  )
  
  list[plot.learning.rate.notataxic, plot.learning.rate.ataxic] = set_equal_y_lims(plot.learning.rate.notataxic, plot.learning.rate.ataxic)
  
  library(patchwork) 
  
  (plot.learning.rate.ataxic + plot.learning.rate.notataxic)
  
}
