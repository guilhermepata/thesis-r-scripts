## Initial error

{if (name == 'Exp3' || name == 'Exp5') {
  initial.error.test.at = list(Session=c('S1'), Num=0)
} else if (name == 'Exp4') {
  initial.error.test.at = list(Session=c('S1'), Num=0, 
                               Group=c('NotAtaxic:Switch', 'Ataxic:Switch'))
}

(initial.error.test = summary(emmeans(
                      model.split, pairwise ~ Group, 
                      at=initial.error.test.at)$contrasts, infer=TRUE))
}


## Learning rate

{if (name == 'Exp3') {
  learning.rate.test.at = list(Session=c('S1', 'S2', 'S3', 'S4'))
} else if (name == 'Exp5') {
  learning.rate.test.at = list(Session=c('S1'))
}
  
(learning.rate.test = summary(emtrends(
                                    model.split, pairwise ~ Group, 
                                    var = "Num",
                                    at=learning.rate.test.at)))
}

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


## Washouts
(washouts.test = summary(emmeans(
                            model.washout, pairwise ~ Group,
                            at = list(Session = c('S1', 'S2', 'S3', 'S4', 'S5'), Num=0)
                            )))


## Switch group

{if (name == 'Exp3') {
  switch.group.test.at = list(Session=c('S5', 'S4', 'S3', 'S2', 'S1'), Num=4, Group='NotAtaxic:Switch')
} else if (name == 'Exp5') {
  switch.group.test.at = list(Session=c('S5', 'S4', 'S3', 'S2', 'S1'), Num=0, Group='NotAtaxic:Switch')
}
  
(error.reduction.test = summary(emmeans(
  model.split, pairwise ~ Session, 
  at=switch.group.test.at)$contrasts))
(after.effect.test = summary(emmeans(
  model.washout, ~ Session, 
  at=switch.group.test.at)))
(after.effect.test = summary(emmeans(
  model.washout, identity ~ Session, 
  at=switch.group.test.at)))
(washout.test = summary(emmeans(
  model.washout, identity ~ Session, 
  at=switch.group.test.at)))
(baseline.test = summary(emmeans(
  model.baseline, identity ~ Session, 
  at=switch.group.test.at)))
}

(conserved.error.test = summary(emmeans(
  model.split, pairwise ~ Session * Num, 
  at=list(Session=c('S2', 'S1'), Num=c(0, 3), Group='NotAtaxic:Switch'))))

(change.over.split.test = as.data.frame(emmeans(
  model.split, pairwise ~ Session * Num, 
  at=list(Session=c('S5', 'S1'), Num=c(3, 0), Group='NotAtaxic:Switch'))$contrasts))



## NoSwitch group

(change.over.split.test2 = as.data.frame(emmeans(
  model.split, ~ Group | Session | Num, 
  at=list(Session=c('S4', 'S1'), Num=c(3, 0)))))

(after.effect.test2 = as.data.frame(emmeans(
  model.washout, ~ Group | Session | Num, 
  at=list(Session=c('S4'), Num=0))))


## Exp4 

(error.reduction.test = summary(emmeans(
  model.split, pairwise ~ Session * Num | Group,
  at=list(
    Group = c('NotAtaxic:Switch', 'Ataxic:Switch'),
    Num = c(0,5,11)
    # Session = c('S3','S5')
  ))$contrasts, infer=TRUE))

(learning.rate.test = summary(emtrends(
  model.split, identity ~ Session | Group,
  var = "Num",
  at=list(
    Group = c('NotAtaxic:Switch', 'Ataxic:Switch')
    # Num = c(0,5,11)
    # Session = c('S3','S5')
  ))$contrasts, infer=TRUE))

(after.effect.test = summary(emmeans(
  model.washout, identity ~ Session | Group,
  at=list(
    Group = c('NotAtaxic:Switch', 'Ataxic:Switch'),
    Num = c(5)
    # Session = c('S3','S5')
  ))$contrasts, infer=TRUE))

