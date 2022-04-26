# effects_num <- effects::effect(term= "Session", mod= model.split)

conf.interval.factor = 1.96 # multiply std. error by this value to get the one way amplitude of the 95% conf interval

num.split.trials = 4

data.split.initial.error = as.data.frame(ggpredict(model.split, terms = c("Session", "Num [0]")))
data.split.initial.error <-
  rename(
    data.split.initial.error,
    Session = x,
    Num = group,
  )
data.split.final.error = as.data.frame(ggpredict(model.split, terms = c("Session", "Num [3]")))
data.split.final.error <-
  rename(
    data.split.final.error,
    Session = x,
    Num = group,
  )

change.over.split.animals <- filter(data.split, Session=='S5' & Num==3)$Asym - 
                            filter(data.split, Session=='S1' & Num==0)$Asym
change.over.split.estimate <-   data.split.final.error[data.split.final.error$Session == 'S5',]$predicted - 
                                  data.split.initial.error[data.split.final.error$Session == 'S1',]$predicted
change.over.split.std.error <- sqrt(data.split.final.error[data.split.final.error$Session == 'S5',]$std.error^2 + 
                                       data.split.initial.error[data.split.final.error$Session == 'S1',]$std.error^2
                                    )

data.washout.after.effect = as.data.frame(ggpredict(model.washout, terms = c("Session", "Num [0]")))
data.washout.after.effect <-
  rename(
    data.washout.after.effect,
    Session = x,
    Num = group,
  )

after.effect.animals <- filter(data.washout, Session=='S5' & Num==0)$Asym
after.effect.estimate <- data.washout.after.effect[data.washout.after.effect$Session == 'S5',]$predicted
after.effect.std.error <- data.washout.after.effect[data.washout.after.effect$Session == 'S5',]$std.error

df <- data.frame(Phase = c('Change over split', 'After effect'),
                 Estimate = c(change.over.split.estimate, after.effect.estimate),
                 "Std.Error" = c(change.over.split.std.error, after.effect.std.error)
                 )

df.animals = data.frame(Phase = c(), Estimate = c(), Animal = c())
for (animal in unique(data.split$Animal)) {
  df.aux = data.frame(Phase = c('Change over split'),
                      Estimate = c(filter(data.split, Session=='S5' & Num==3 & Animal==animal)$Asym - 
                        filter(data.split, Session=='S1' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}
for (animal in unique(data.washout$Animal)) {
  df.aux = data.frame(Phase = c('After effect'),
                      Estimate = c(filter(data.split, Session=='S5' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}

(p <- ggplot() +
    geom_point(data=df.animals, aes(x=Phase, y=Estimate, group=Animal, color=Animal)) +
    geom_line(data=df.animals, aes(x=Phase, y=Estimate, group=Animal, color=Animal)) +
    geom_point(data=df, aes(x=Phase, y=Estimate), size  = 3) +
    geom_errorbar(data=df, aes(x=Phase, y=Estimate, ymin=Estimate-Std.Error, ymax=Estimate+Std.Error), width=.1,
                  position=position_dodge(0.05)) +
    geom_line(data=df, aes(x=Phase, y=Estimate, group=0), size  = 1) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('Change over split', 'After effect')
)

p+labs(title="Change over split vs after effect", x="", y = "Step length asymmetry (mm)")+
  theme_classic()+theme(legend.position="none")
