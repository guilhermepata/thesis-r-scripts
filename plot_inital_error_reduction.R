source("build_model.R")
source("my_functions.R")

data.initial.error = filter(data.split, Num==0)

to.predict = as.data.frame(data.initial.error %>%
  group_by(Num, Group, Session) %>%
  summarise())

to.predict$Prediction = predict(model.split, newdata=to.predict, re.form = ~0)

predFun <- function(x) predict(x,newdata=to.predict,re.form=NA)
bb <- bootMer(model.split,
              FUN=predFun,
              nsim=200)  
bb_ci <- as.data.frame(t(apply(bb$t,2,quantile,c(0.025,0.975))))
names(bb_ci) <- c("lwr","upr")
to.predict <- cbind(to.predict, bb_ci)

# to.predict = cbind(to.predict, predictInterval(model.split, newdata=to.predict, which="fixed"))


### Plot switch group

to.predict.switch <- filter(to.predict, Group == 'NotAtaxic:Switch')

plot.errors.switch <- ggplot() +
    geom_point(data=filter(data.split, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
    geom_line(data=filter(data.split, Protocol=='Switch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
    geom_point(data=to.predict.switch, aes(x=Session, y=Prediction), size=3) +
    geom_line(data=to.predict.switch, aes(x=Session, y=Prediction, group=0), size= 1) +
    geom_errorbar(data=to.predict.switch, aes(x=Session, y=Prediction, ymin=lwr, ymax=upr), width=.2,
        position=position_dodge(0.05)) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('S1', 'S2', 'S3', 'S4', 'S5') +
     theme_classic() + theme(legend.position="none") +
     labs(x="Session", y = "Step length asymmetry (mm)")

### Plot noswitch group

to.predict.noswitch <- filter(to.predict, Group == 'NotAtaxic:NoSwitch')

plot.errors.noswitch  <- ggplot() +
    geom_point(data=filter(data.split, Protocol=='NoSwitch', Num==0), aes(x=Session, y=Asym, color=Animal), alpha = 0.2) +
    geom_line(data=filter(data.split, Protocol=='NoSwitch', Num==0), aes(x=Session, y=Asym, color=Animal, group=Animal), alpha = 0.2) +
    geom_point(data=to.predict.noswitch, aes(x=Session, y=Prediction), size=3) +
    geom_line(data=to.predict.noswitch, aes(x=Session, y=Prediction, group=0), size= 1) +
    geom_errorbar(data=to.predict.noswitch, aes(x=Session, y=Prediction, ymin=lwr, ymax=upr), width=.2,
                  position=position_dodge(0.05)) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    xlim('S1', 'S2', 'S3', 'S4') +
    theme_classic() + theme(legend.position="none") +
    labs(x="Session", y = "Step length asymmetry (mm)")


# ylim = equal_y_limits(plot.errors.noswitch, plot.errors.switch)
# 
# plot.errors.noswitch = plot.errors.noswitch + ylim(ylim)
# plot.errors.switch = plot.errors.switch + ylim(ylim)
