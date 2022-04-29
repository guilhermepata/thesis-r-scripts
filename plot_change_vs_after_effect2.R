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

num.split.trials.switch = max(unique(filter(data.split, Protocol == "Switch", Session == 'S5')$Num))

predict.change.over.split.switch <- function(mod) {
  first.error.df = data.frame(Num = 0, Group = "NotAtaxic:Switch", Session = "S1")
  final.error.df = data.frame(Num = num.split.trials.switch, Group = "NotAtaxic:Switch", Session = "S5")
  return(predict(mod, newdata=final.error.df, re.form = ~0) - predict(mod, newdata=first.error.df, re.form = ~0))
}
change.over.split.switch <- statBounded(model.split, predict.change.over.split.switch)

predict.after.effect.switch <- function(mod) {
  after.effect.df = data.frame(Num = 0, Group = "NotAtaxic:Switch", Session = "S5")
  return(predict(mod, newdata=after.effect.df, re.form = ~0))
}
after.effect.switch <- statBounded(model.washout, predict.after.effect.switch)

df.switch <- cbind(Phase = c('Change over split', 'After effect'), rbind(change.over.split.switch, after.effect.switch))

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

