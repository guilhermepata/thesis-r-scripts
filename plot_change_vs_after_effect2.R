source("build_model.R")
source("my_functions.R")

plot.change <- function(df, df.animals) {
  ggplot() +
    geom_point(data=df.animals, aes(x=Phase, y=Asym, group=Animal, color=Animal), alpha = 0.2) +
    geom_line(data=df.animals, aes(x=Phase, y=Asym, group=Animal, color=Animal), alpha = 0.2) +
    geom_point(data=df, aes(x=Phase, y=Fit), size  = 3) +
    geom_errorbar(data=df, aes(x=Phase, y=Fit, ymin=Lower, ymax=Upper), width=.1,
                  position=position_dodge(0.05)) +
    geom_line(data=df, aes(x=Phase, y=Fit, group=0), size  = 1) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    scale_x_discrete(limits=c('Change over split', 'After effect'), labels=c("Change over split" = "COS", "After effect" = "AE")) +
    theme_classic() + theme(legend.position="none") +
    labs(x=element_blank(), y = "Step length asymmetry (mm)")
}

### plot switch group

make_change_df <- function(data.summary, 
                           model.split, 
                           model.washout, 
                           group,
                           sessions.split = c('S1', 'S5')) {
  
  data.summary2 = filter(data.summary, Group == group)
  
  first.trial = min(filter(data.summary2, Phase == 'Split', 
                           Session == sessions.split[[1]])$Trial)
  last.trial = max(filter(data.summary2, Phase == 'Split',
                          Session == sessions.split[[2]])$Trial)
  washout.trial = last.trial + 1
  
  first.trial.row = filter(data.summary2, Trial==first.trial)
  last.trial.row = filter(data.summary2, Trial==last.trial)
  washout.row = filter(data.summary2, Trial==washout.trial)
  
  first.num = first.trial.row$Num
  first.session = first.trial.row$Session
  
  last.num = last.trial.row$Num
  last.session = last.trial.row$Session
  
  washout.num = washout.row$Num
  washout.session = washout.row$Session
  
  s.split = summary(emmeans(
      model.split,
      revpairwise ~ Num * Session * Group,
      at = list(
        Session = c(first.session, last.session),
        Num = c(first.num, last.num),
        Group = group
      )
  )$contrasts, infer=TRUE)
  
  s.split = filter(s.split, contrast == paste(last.num, 
                                              last.session,
                                              group,
                                              '-',
                                              first.num,
                                              first.session,
                                              group,
                                              sep=' '))
  
  s.washout = summary(emmeans(
    model.split,
    identity ~ Num * Session * Group,
    at = list(
      Session = washout.session,
      Num = washout.num,
      Group = group
    )
  )$contrasts, infer=TRUE)
  
  df <- data.frame(
    Phase = c('Change over split', 'After effect'),
    Fit = c(s.split$estimate, s.washout$estimate),
    Lower = c(s.split$lower.CL, s.washout$lower.CL),
    Upper = c(s.split$upper.CL, s.washout$upper.CL),
    P.value = c(s.split$p.value, s.washout$p.value)
  )
  
  return(df)
  
}

df.switch <- make_change_df(data.summary,
                            model.split,
                            model.washout,
                            group = 'NotAtaxic:Switch',
                            sessions.split = c('S1','S5'))
  
  
df.animals.switch = data.frame(Phase = c(), Asym = c(), Animal = c())
for (animal in unique(filter(data.split, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('Change over split'),
                      Asym = c(filter(data.split, Session=='S5' & Num==num.split.trials.switch & Animal==animal)$Asym - 
                                     filter(data.split, Session=='S1' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals.switch = rbind(df.animals.switch, df.aux)
}
for (animal in unique(filter(data.washout, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('After effect'),
                      Asym = c(filter(data.washout, Session=='S5' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals.switch = rbind(df.animals.switch, df.aux)
}

plot.change.switch <- plot.change(df.switch, df.animals.switch)


### plot noswitch group

num.split.trials.noswitch = max(unique(filter(data.split, Protocol == "NoSwitch", Session == 'S4')$Num))

predict.change.over.split.noswitch <- function(mod) {
  first.error.df = data.frame(Num = 0, Group = "NotAtaxic:NoSwitch", Session = "S1")
  final.error.df = data.frame(Num = num.split.trials.noswitch, Group = "NotAtaxic:NoSwitch", Session = "S4")
  return(predict(mod, newdata=final.error.df, re.form = ~0) - predict(mod, newdata=first.error.df, re.form = ~0))
}
change.over.split.noswitch <- statBounded(model.split, predict.change.over.split.noswitch)

predict.after.effect.noswitch <- function(mod) {
  after.effect.df = data.frame(Num = 0, Group = "NotAtaxic:NoSwitch", Session = "S4")
  return(predict(mod, newdata=after.effect.df, re.form = ~0))
}
after.effect.noswitch <- statBounded(model.washout, predict.after.effect.noswitch)

df.noswitch <- cbind(Phase = c('Change over split', 'After effect'), rbind(change.over.split.noswitch, after.effect.noswitch))

df.animals.noswitch = data.frame(Phase = c(), Asym = c(), Animal = c())
for (animal in unique(filter(data.split, Protocol=='NoSwitch')$Animal)) {
  df.aux = data.frame(Phase = c('Change over split'),
                      Asym = c(filter(data.split, Session=='S4' & Num==num.split.trials.noswitch & Animal==animal)$Asym - 
                                filter(data.split, Session=='S1' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals.noswitch = rbind(df.animals.noswitch, df.aux)
}
for (animal in unique(filter(data.washout, Protocol=='NoSwitch')$Animal)) {
  df.aux = data.frame(Phase = c('After effect'),
                      Asym = c(filter(data.washout, Session=='S4' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals.noswitch = rbind(df.animals.noswitch, df.aux)
}

plot.change.noswitch <- plot.change(df.noswitch, df.animals.noswitch)


### Do some stats

