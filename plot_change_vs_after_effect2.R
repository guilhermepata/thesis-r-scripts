source("build_model.R")
source("my_functions.R")

plot.change <- function(df, df.animals) {
  ggplot() +
    geom_point(data=df.animals, aes(x=Phase, y=Fit, group=Animal, color=Animal), alpha = 0.2) +
    geom_line(data=df.animals, aes(x=Phase, y=Fit, group=Animal, color=Animal), alpha = 0.2) +
    geom_point(data=df, aes(x=Phase, y=Fit), size  = 3) +
    geom_errorbar(data=df, aes(x=Phase, y=Fit, ymin=Lower, ymax=Upper), width=.1,
                  position=position_dodge(0.05)) +
    geom_line(data=df, aes(x=Phase, y=Fit, group=0), size  = 1) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('Change over split', 'After effect') +
    theme_classic() + theme(legend.position="none") +
    labs(x="Session", y = "Step length asymmetry (mm)")
}

### plot switch group

num.split.trials = max(unique(filter(data.split, Protocol == "Switch", Session == 'S5')$Num)) 

predict.change.over.split <- function(mod) {
  first.error.df = data.frame(Num = 0, Group = "NotAtaxic:Switch", Session = "S1")
  final.error.df = data.frame(Num = num.split.trials, Group = "NotAtaxic:Switch", Session = "S5")
  return(predict(mod, newdata=final.error.df, re.form = ~0) - predict(mod, newdata=first.error.df, re.form = ~0))
}
change.over.split <- statBounded(model.split, predict.change.over.split)

predict.after.effect <- function(mod) {
  after.effect.df = data.frame(Num = 0, Group = "NotAtaxic:Switch", Session = "S5")
  return(predict(mod, newdata=after.effect.df, re.form = ~0))
}
after.effect <- statBounded(model.washout, predict.after.effect)

df <- cbind(Phase = c('Change over split', 'After effect'), rbind(change.over.split, after.effect))

# df <- data.frame(Phase = c('Change over split', 'After effect'),
#                  Fit = c(final.delta, after.effect),
#                  "Std.Error" = c(final.delta.std, after.effect.std)
# )

df.animals = data.frame(Phase = c(), Fit = c(), Animal = c())
for (animal in unique(filter(data.split, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('Change over split'),
                      Fit = c(filter(data.split, Session=='S5' & Num==num.split.trials & Animal==animal)$Asym - 
                                     filter(data.split, Session=='S1' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}
for (animal in unique(filter(data.washout, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('After effect'),
                      Fit = c(filter(data.washout, Session=='S5' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}

plot.change.switch <- plot.change(df, df.animals)


### plot noswitch group

num.split.trials = max(unique(filter(data.split, Protocol == "NoSwitch", Session == 'S4')$Num))

predict.change.over.split <- function(mod) {
  first.error.df = data.frame(Num = 0, Group = "NotAtaxic:NoSwitch", Session = "S1")
  final.error.df = data.frame(Num = num.split.trials, Group = "NotAtaxic:NoSwitch", Session = "S4")
  return(predict(mod, newdata=final.error.df, re.form = ~0) - predict(mod, newdata=first.error.df, re.form = ~0))
}
change.over.split <- statBounded(model.split, predict.change.over.split)

predict.after.effect <- function(mod) {
  after.effect.df = data.frame(Num = 0, Group = "NotAtaxic:NoSwitch", Session = "S4")
  return(predict(mod, newdata=after.effect.df, re.form = ~0))
}
after.effect <- statBounded(model.washout, predict.after.effect)

df <- cbind(Phase = c('Change over split', 'After effect'), rbind(change.over.split, after.effect))

df.animals = data.frame(Phase = c(), Fit = c(), Animal = c())
for (animal in unique(filter(data.split, Protocol=='NoSwitch')$Animal)) {
  df.aux = data.frame(Phase = c('Change over split'),
                      Fit = c(filter(data.split, Session=='S4' & Num==num.split.trials & Animal==animal)$Asym - 
                                filter(data.split, Session=='S1' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}
for (animal in unique(filter(data.washout, Protocol=='NoSwitch')$Animal)) {
  df.aux = data.frame(Phase = c('After effect'),
                      Fit = c(filter(data.washout, Session=='S4' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}

plot.change.noswitch <- plot.change(df, df.animals)
