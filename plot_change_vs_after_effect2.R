
conf.interval.factor = 1.96 # multiply std. error by this value to get the one way amplitude of the 95% conf interval

sqrt_sum_sq <- function(...) {
  nums = list(...)
  r = 0
  for (num in nums) {
    r = r + num ^ 2
  }
  sqrt(r)
}

### plot switch group

num.split.trials = max(unique(filter(data.split, Protocol == "Switch", Session == 'S5')$Num)) + 1

coefs.split = as.data.frame(coef(summary(model.split)))

initial.delta = coefs.split["SessionS5",]$Estimate
initial.delta.std = coefs.split["SessionS5",]$"Std. Error"
slope = coefs.split["Num",]$Estimate + coefs.split["Num:SessionS5",]$Estimate 
slope.std = sqrt_sum_sq(coefs.split["Num",]$"Std. Error", coefs.split["Num:SessionS5",]$"Std. Error")   
final.delta = initial.delta + slope * num.split.trials
final.delta.std = sqrt_sum_sq(initial.delta.std, slope.std*num.split.trials)

coefs.washout = as.data.frame(coef(summary(model.washout)))

intercept = coefs.washout["(Intercept)"]$Estimate
intercept.std = coefs.washout["(Intercept)"]$"Std. Error"
washout.delta = coefs.washout["SessionS5"]$Estimate
washout.delta.td = coefs.washout["SessionS5"]$"Std. Error"

after.effect = intercept + washout.delta
after.effect.std = sqrt_sum_sq(intercept.std, washout.delta.std)

df <- data.frame(Phase = c('Change over split', 'After effect'),
                 Estimate = c(final.delta, after.effect),
                 "Std.Error" = c(final.delta.std, after.effect.std)
)

df.animals = data.frame(Phase = c(), Estimate = c(), Animal = c())
for (animal in unique(filter(data.split, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('Change over split'),
                      Estimate = c(filter(data.split, Session=='S5' & Num==3 & Animal==animal)$Asym - 
                                     filter(data.split, Session=='S1' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}
for (animal in unique(filter(data.washout, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('After effect'),
                      Estimate = c(filter(data.split, Session=='S5' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}

plot.change.switch <- ggplot() +
    geom_point(data=df.animals, aes(x=Phase, y=Estimate, group=Animal, color=Animal)) +
    geom_line(data=df.animals, aes(x=Phase, y=Estimate, group=Animal, color=Animal)) +
    geom_point(data=df, aes(x=Phase, y=Estimate), size  = 3) +
    geom_errorbar(data=df, aes(x=Phase, y=Estimate, ymin=Estimate-Std.Error, ymax=Estimate+Std.Error), width=.1,
                  position=position_dodge(0.05)) +
    geom_line(data=df, aes(x=Phase, y=Estimate, group=0), size  = 1) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('Change over split', 'After effect') +
    theme_classic() + theme(legend.position="none") +
    labs(x="Session", y = "Step length asymmetry (mm)")


### plot noswitch group

num.split.trials = max(unique(filter(data.split, Protocol == "NoSwitch", Session == 'S4')$Num)) + 1

coefs.split = as.data.frame(coef(summary(model.split)))

initial.delta = coefs.split["SessionS4",]$Estimate + coefs.split["SessionS5:GroupNotAtaxic:NoSwitch",]$Estimate
initial.delta.std = sqrt_sum_sq(coefs.split["SessionS5",]$"Std. Error", 
                                coefs.split["SessionS5:GroupNotAtaxic:NoSwitch",]$"Std. Error")
slope = coefs.split["Num:SessionS5",]$Estimate
slope.std = coefs.split["Num:SessionS5",]$"Std. Error"

final.delta = initial.delta + slope * num.split.trials
final.delta.std = sqrt_sum_sq(initial.delta.std, slope.std*num.split.trials)

coefs.washout = as.data.frame(coef(summary(model.washout)))

intercept = coefs.washout["(Intercept)"]$Estimate
intercept.std = coefs.washout["(Intercept)"]$"Std. Error"
washout.delta = coefs.washout["SessionS5"]$Estimate
washout.delta.td = coefs.washout["SessionS5"]$"Std. Error"

after.effect = intercept + washout.delta
after.effect.std = sqrt_sum_sq(intercept.std, washout.delta.std)

df <- data.frame(Phase = c('Change over split', 'After effect'),
                 Estimate = c(final.delta, after.effect),
                 "Std.Error" = c(final.delta.std, after.effect.std)
)

df.animals = data.frame(Phase = c(), Estimate = c(), Animal = c())
for (animal in unique(filter(data.split, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('Change over split'),
                      Estimate = c(filter(data.split, Session=='S5' & Num==3 & Animal==animal)$Asym - 
                                     filter(data.split, Session=='S1' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}
for (animal in unique(filter(data.washout, Protocol=='Switch')$Animal)) {
  df.aux = data.frame(Phase = c('After effect'),
                      Estimate = c(filter(data.split, Session=='S5' & Num==0 & Animal==animal)$Asym),
                      Animal = c(animal)
  )
  df.animals <- rbind(df.animals, df.aux)
}

plot.change.noswitch <- ggplot() +
  geom_point(data=df.animals, aes(x=Phase, y=Estimate, group=Animal, color=Animal)) +
  geom_line(data=df.animals, aes(x=Phase, y=Estimate, group=Animal, color=Animal)) +
  geom_point(data=df, aes(x=Phase, y=Estimate), size  = 3) +
  geom_errorbar(data=df, aes(x=Phase, y=Estimate, ymin=Estimate-Std.Error, ymax=Estimate+Std.Error), width=.1,
                position=position_dodge(0.05)) +
  geom_line(data=df, aes(x=Phase, y=Estimate, group=0), size  = 1) +
  geom_hline(yintercept=c(0), linetype="dotted") +
  xlim('Change over split', 'After effect') +
  theme_classic() + theme(legend.position="none") +
  labs(x="Session", y = "Step length asymmetry (mm)")
