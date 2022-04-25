# effects_num <- effects::effect(term= "Session", mod= model.split)

conf.interval.factor = 1.96 # multiply std. error by this value to get the one way amplitude of the 95% conf interval

num.split.trials = 4

model.split.coef <- as.data.frame(coef(summary(model.split)))
change.over.split.estimate <-   model.split.coef["SessionS5",]$Estimate + 
                                model.split.coef["Num:SessionS5",]$Estimate *
                                num.split.trials
change.over.split.std.error <- sqrt( model.split.coef["SessionS5",]$"Std. Error"^2 +
                                    (model.split.coef["Num:SessionS5",]$"Std. Error" *
                                     num.split.trials)^2)

model.washout.coef <- as.data.frame(coef(summary(model.washout)))
after.effect.estimate <- model.washout.coef["(Intercept)",]$Estimate + 
                         model.washout.coef["SessionS5",]$Estimate

after.effect.std.error <-sqrt( model.washout.coef["(Intercept)",]$"Std. Error"^2 +
                              model.washout.coef["SessionS5",]$"Std. Error"^2)

df <- data.frame(Phase = c('Change over split', 'After effect'),
                 Estimate = c(change.over.split.estimate, after.effect.estimate),
                 "Std.Error" = c(change.over.split.std.error, after.effect.std.error)
                 )


(p <- ggplot() +
    # geom_point(data=pred_frame, aes(x=Session, y=predicted), size=3) +
    # geom_line(data=pred_frame, aes(x=Session, y=predicted), size= 1) +
    geom_point(data=df, aes(x=Phase, y=Estimate), size  = 3) +
    geom_errorbar(data=df, aes(x=Phase, y=Estimate, ymin=Estimate-Std.Error, ymax=Estimate+Std.Error), width=.2,
                  position=position_dodge(0.05)) +
    xlim('Change over split', 'After effect')
)

p+labs(title="Change over split vs after effect", x="", y = "Step length asymmetry (mm)")+
  theme_classic()
