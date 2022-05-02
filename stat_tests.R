## Initial error

(initial.error.test = summary(emmeans(
                      model.split, pairwise ~ Group, 
                      at=list(Session=c('S1'), Num=0))$contrasts))

## Learning rate

(learning.rate.test = summary(emtrends(
                                    model.split, pairwise ~ Group, 
                                    var = "Num",
                                    at=list(Session=c('S1', 'S2', 'S3', 'S4')))$contrasts))

## Session 6
# initial error
(s6.error.test = summary(emmeans(
  model.split, pairwise ~ Group, 
  at=list(Session=c('S6'), Num=0))$contrasts))
# learning rate
(s6.rate.test = summary(emtrends(
                          model.split, pairwise ~ Group, 
                          var = "Num",
                          at=list(Session=c('S6')))$contrasts))
# after effect
(s6.ae.test = summary(emmeans(
  model.washout, pairwise ~ Group, 
  at=list(Session=c('S6'), Num=0))$contrasts))


## Switch group
(error.reduction.test = summary(emmeans(
  model.split, pairwise ~ Session, 
  at=list(Session=c('S5', 'S4', 'S3', 'S2', 'S1'), Num=0, Group='NotAtaxic:Switch'))$contrasts))
(after.effect.test = summary(emmeans(
  model.washout, ~ Session, 
  at=list(Session=c('S5', 'S4', 'S3', 'S2', 'S1'), Num=0, Group='NotAtaxic:Switch'))))
(after.effect.test = summary(emmeans(
  model.washout, identity ~ Session, 
  at=list(Session=c('S5', 'S4', 'S3', 'S2', 'S1'), Num=0, Group='NotAtaxic:Switch'))))
(washout.test = summary(emmeans(
  model.washout, identity ~ Session, 
  at=list(Session=c('S5', 'S4', 'S3', 'S2', 'S1'), Num=4, Group='NotAtaxic:Switch'))))
(baseline.test = summary(emmeans(
  model.washout, identity ~ Session, 
  at=list(Session=c('S5', 'S4', 'S3', 'S2', 'S1'), Num=0, Group='NotAtaxic:Switch'))))


(conserved.error.test = summary(emmeans(
  model.split, pairwise ~ Session * Num, 
  at=list(Session=c('S2', 'S1'), Num=c(0, 3), Group='NotAtaxic:Switch'))))

(change.over.split.test = as.data.frame(emmeans(
  model.split, pairwise ~ Session * Num, 
  at=list(Session=c('S5', 'S1'), Num=c(3, 0), Group='NotAtaxic:Switch'))$contrasts))
