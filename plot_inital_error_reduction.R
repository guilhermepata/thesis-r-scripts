# effects_num <- effects::effect(term= "Session", mod= model.split)

pred.mm <- ggpredict(model.split, terms = c("Session"))  # this gives overall predictions for the model

pred_frame = as.data.frame(pred.mm)
pred_frame <-
  rename(
    pred_frame,
    Session = x,
    # Session = group,
    # Group = facet,
  )

# factor = 1.96
# 
# coef.split <- coef(summary(model.split))

(p <- ggplot(data.split, aes(x = Session, y = Asym)) +
    geom_point(data=(data.split[data.split$Num == 0,]), aes(Session, Asym, color=Animal)) +
    geom_line(data=data.split[data.split$Num == 0,], aes(Session, Asym, color=Animal, group=Animal)) +
    geom_point(data=pred_frame, aes(x=Session, y=predicted), size=3) +
    geom_line(data=pred_frame, aes(x=Session, y=predicted, group=1), size= 1) +
    geom_errorbar(data=pred_frame, aes(x=Session, y=predicted, ymin=conf.low, ymax=conf.high), width=.2,
                  position=position_dodge(0.05)) +
    xlim('S1', 'S2', 'S3', 'S4', 'S5')
)

p+labs(title="Initial error change across sessions", x="Session", y = "Step length asymmetry (mm)")+
  theme_classic()
