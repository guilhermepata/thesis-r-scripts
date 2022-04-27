# effects_num <- effects::effect(term= "Session", mod= model.split)

# pred.mm <- ggpredict(model.split, terms = c("Session"))  # this gives overall predictions for the model




# data.switch = filter(data.total_frame, Protocol == 'Switch')
# 
# data.split.initial.error = as.data.frame(ggpredict(model.split, terms = c("Session", "Num [0]", "Group")))
# 
# to.predict = data.frame(Num = c(), Group = c(), Session = c())
# for (session in unique(data.split$Session)) {
#   df.aux = data.frame(Num = 0,
#                       Group = "NotAtaxic:Switch" ,
#                       Session = session
#                       )
#   to.predict <- rbind(to.predict, df.aux)
# }
# 
# fit <- predict(model.split, newdata = to.predict, re.form = ~0)
# to.predict <- cbind(to.predict, fit)
# 
# to.predict.animals = filter(data.split, Num==0 & Group=="NotAtaxic:Switch")
# fit <- predict(model.split, newdata = to.predict.animals)
# to.predict.animals <- cbind(to.predict.animals, fit)
# 
# # group_means <- to.predict.animals %>%
# #   group_by(Num, Group, Session) %>%
# #   summarise(Mean = std.error(fit))
# 
# 
# data.split.initial.error <-
#   rename(
#     data.split.initial.error,
#     Session = x,
#     Num = group,
#   )

# data.split$Predicted = predict(model.split, re.form = ~0)

data.initial.error = filter(data.split, Num==0)

prediction = as.data.frame(data.initial.error %>%
  group_by(Num, Group, Session) %>%
  summarise(Animal = 'SwitchLeft01'))

prediction$Prediction = predict(model.split, newdata=prediction, re.form = ~0)

prediction = cbind(prediction, predictInterval(model.split, newdata=prediction, which="fixed"))


### Plot switch group

prediction.switch <- filter(prediction, Group == 'NotAtaxic:Switch')

plot.errors.switch <- ggplot() +
    geom_point(data=filter(data.split, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
    geom_line(data=filter(data.split, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
    geom_point(data=prediction.switch, aes(x=Session, y=Prediction), size=3) +
    geom_line(data=prediction.switch, aes(x=Session, y=Prediction, group=0), size= 1) +
    geom_errorbar(data=prediction.switch, aes(x=Session, y=Prediction, ymin=lwr, ymax=upr), width=.2,
        position=position_dodge(0.05)) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('S1', 'S2', 'S3', 'S4', 'S5') +
    theme_classic() + theme(legend.position="none") +
    labs(x="Session", y = "Step length asymmetry (mm)")

### Plot noswitch group

prediction.noswitch <- filter(prediction, Group == 'NotAtaxic:NoSwitch')

plot.errors.noswitch  <- ggplot() +
    geom_point(data=filter(data.split, Protocol=='NoSwitch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
    geom_line(data=filter(data.split, Protocol=='NoSwitch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
    geom_point(data=prediction.noswitch, aes(x=Session, y=Prediction), size=3) +
    geom_line(data=prediction.noswitch, aes(x=Session, y=Prediction, group=0), size= 1) +
    geom_errorbar(data=prediction.noswitch, aes(x=Session, y=Prediction, ymin=lwr, ymax=upr), width=.2,
                  position=position_dodge(0.05)) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('S1', 'S2', 'S3', 'S4') +
    theme_classic() + theme(legend.position="none") +
    labs(x="Session", y = "Step length asymmetry (mm)")


ylim = equal_y_limits(plot.errors.noswitch, plot.errors.switch)

plot.errors.noswitch = plot.errors.noswitch + ylim(ylim)
plot.errors.switch = plot.errors.switch + ylim(ylim)
