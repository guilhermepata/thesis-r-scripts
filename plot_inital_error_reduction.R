# effects_num <- effects::effect(term= "Session", mod= model.split)

# pred.mm <- ggpredict(model.split, terms = c("Session"))  # this gives overall predictions for the model


data.split.initial.error = as.data.frame(ggpredict(model.split, terms = c("Session", "Num [0]", "Group")))

data.split.initial.error <-
  rename(
    data.split.initial.error,
    Session = x,
    Num = group,
  )

data.split$Predicted = predict(model.split)


(p <- ggplot() +
    geom_point(data=data.split[data.split$Num == 0,], aes(x=Session, y=Asym, color=Animal)) +
      geom_line(data=data.split[data.split$Num == 0,], aes(x=Session, y=Asym, color=Animal, group=Animal)) +
    geom_point(data=data.split.initial.error, aes(x=Session, y=predicted), size=3) +
    geom_line(data=data.split.initial.error, aes(x=Session, y=predicted, group=0), size= 1) +
    geom_errorbar(data=data.split.initial.error, aes(x=Session, y=predicted, ymin=conf.low, ymax=conf.high), width=.2,
                  position=position_dodge(0.05)) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('S1', 'S2', 'S3', 'S4', 'S5')
)

p+labs(title="Initial error change across sessions", x="Session", y = "Step length asymmetry (mm)")+
  theme_classic()
