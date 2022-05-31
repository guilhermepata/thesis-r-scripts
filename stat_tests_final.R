source("build_mega_model.R")
source("build_model_first_lr.R")
source("build_spatiotemporal_model.R")
source("my_functions.R")

if (name == 'Exp3') {
  print("Alternated exposure to split-belt modifies cerebellar learning")
  
  {
    print("Initial error noswitch")
    print(initial.error.noswitch <- summary(
      emmeans(
        model.split,
        identity ~ Group,
        at = list(
          Session = 'S1',
          Num = 0,
          Group = c('Exp3:NotAtaxic:NoSwitch')
        )
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  {
    print("Final error noswitch")
    print(final.error.noswitch <- summary(
      emmeans(
        model.split,
        identity ~ Group,
        at = list(
          Session = 'S4',
          Num = max(
            filter(
              mega.data.split,
              Group == 'Exp3:NotAtaxic:NoSwitch',
              Session == 'S4'
            )$Num
          ),
          Group = c('Exp3:NotAtaxic:NoSwitch')
        )
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  {
    print("After effect noswitch")
    print(after.effect.noswitch <- summary(
      emmeans(
        model.washout,
        identity ~ Group,
        at = list(
          Session = 'S4',
          Num = 0,
          Group = c('Exp3:NotAtaxic:NoSwitch')
        )
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  {
    print("Learning rate noswitch")
    learning.rates.test <- summary(
      emtrends(
        model.split,
        revpairwise ~ Session,
        var = "Num",
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4'),
          Group = c('Exp3:NotAtaxic:NoSwitch')
        ),
        adjust = "none"
      )$contrasts,
      infer = TRUE
    )[c(1, 2, 4), c(1, 2, 5, 6, 8)]
    learning.rates.test[, 5] = p.adjust(learning.rates.test[, 5], method = "fdr")
    learning.rates.test.s1 <- summary(
      emtrends(
        model.split,
        identity ~ Session,
        var = "Num",
        at = list(
          Session = c('S1'),
          Group = c('Exp3:NotAtaxic:NoSwitch')
        ),
        adjust = "none"
      )$contrasts,
      infer = TRUE
    )[c(1), c(1, 2, 5, 6, 8)]
    learning.rates.test = rbind(learning.rates.test.s1, learning.rates.test)
    print(learning.rates.test)
  }
  
  {
    print("Initial error comparison")
    print(initial.error.test <- summary(
      emmeans(
        model.split,
        pairwise ~ Group,
        at = list(
          Session = 'S1',
          Num = 0,
          Group = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
        )
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  {
    print("Learning rates comparison")
    print(learning.rates.test <- summary(
      emtrends(
        model.split,
        pairwise ~ Group | Session,
        var = "Num",
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4'),
          Group = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
        ),
        adjust = "fdr"
      )$contrasts,
      infer = TRUE
    )[, c(2, 3, 6, 7, 9)])
  }
  
  print("> Asymmetry decreases over time with alternated exposure")
  
  {
    print("Initial error decrease from S1")
    decreased.error.test <- summary(
      emmeans(
        model.split,
        revpairwise ~ Session,
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Num = 0,
          Group = c('Exp3:NotAtaxic:Switch')
        ),
        adjust = "none",
      )$contrasts,
      infer = TRUE
    )[c(1, 2, 4, 7), c(2, 5, 6, 8)]
    decreased.error.test[, 4] = p.adjust(decreased.error.test[, 4], method = "fdr")
    print(decreased.error.test)
  }
  
  {
    print("Average initial error decrease")
    print(decreased.error.test <- summary(
      emmeans(
        model.split,
        revconsecavg ~ Session,
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Num = 0,
          Group = c('Exp3:NotAtaxic:Switch')
        ),
        adjust = "fdr",
      )$contrasts,
      infer = TRUE
    )[1, c(2, 5, 6, 8)])
  }
  
  {
    print("Retention (first row is avg)")
    print(rentention.tests <- (summary(
      emmeans(
        model.split,
        skipconsecavg ~ Num * Session,
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Num = c(0, 3),
          Group = 'Exp3:NotAtaxic:Switch'
        ),
        reverse = TRUE,
        adjust = 'fdr'
      ),
      infer = TRUE
    )$contrasts)[, c(2, 5, 6, 8)])
  }
  
  print("> After-effects are diminished or absent with alternated exposure")
  
  {
    print("After effect switch")
    print(after.effect.switch <- summary(
      emmeans(
        model.washout,
        identity ~ Group,
        at = list(
          Session = 'S5',
          Num = 0,
          Group = c('Exp3:NotAtaxic:Switch')
        )
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  
  print("> Session-wise error reduction is not explained by insufficient washouts nor faster relearning")
  
  {
    print("Last washout switch")
    print(last.washout.switch <- summary(
      emmeans(
        model.washout,
        identity ~ Group,
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Num = max(
            filter(
              mega.data.washout.summary,
              Group == 'Exp3:NotAtaxic:Switch',
              Session == 'S1'
            )$Num
          ),
          Group = c('Exp3:NotAtaxic:Switch')
        )
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  {
    print("Avg. consec diff in session learning rate")
    print(initial.lr.switch <- summary(
      emtrends(
        model.split,
        revconsecavg ~ Session | Group,
        var = "Num",
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Group = c('Exp3:NotAtaxic:Switch')
        ),
        adjust = 'fdr',
      )$contrasts,
      infer = TRUE
    )[1, c(1, 3, 6, 7, 9)])
  }
  
  {
    print("Avg. consec diff in initial learning rate")
    print(initial.lr.switch <- summary(
      emtrends(
        model.first.lr,
        revconsecavg ~ Session | Group,
        var = "Perc",
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Group = c('Exp3:NotAtaxic:Switch')
        ),
        adjust = 'fdr',
      )$contrasts,
      infer = TRUE
    )[1, c(1, 3, 6, 7, 9)])
  }
  
  
  print("> Protocol does not influence the contribution of spatial and temporal components of adaptation")
  
}

if (name == 'Exp5') {
  
  
  print("Statistics of exposure to split-belt can influence adaptation")
  
  {
    print("Initial error comparison")
    print(initial.error.test <- summary(
      emmeans(
        model.split,
        pairwise ~ Group,
        at = list(
          Session = 'S1',
          Num = 0,
          Group = c('Exp3:NotAtaxic:Switch', 'Exp5:NotAtaxic:NoSwitch', 'Exp5:NotAtaxic:Switch')
        )
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  {
    print("Learning rates comparison")
    print(learning.rates.test <- summary(
      emtrends(
        model.split,
        pairwise ~ Group,
        var = "Num",
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4'),
          Group = c('Exp3:NotAtaxic:Switch', 'Exp5:NotAtaxic:NoSwitch', 'Exp5:NotAtaxic:Switch')
        ),
        adjust = "fdr"
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  print("> Redistributing tied trials to the post-adaptation period appears to abolish the effect")
  
  {
    print("Initial error decrease from S1")
    decreased.error.test <- summary(
      emmeans(
        model.split,
        revpairwise ~ Session,
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Num = 0,
          Group = c('Exp5:NotAtaxic:Switch')
        ),
        adjust = "none",
      )$contrasts,
      infer = TRUE
    )[c(1, 2, 4, 7), c(2, 5, 6, 8)]
    decreased.error.test[, 4] = p.adjust(decreased.error.test[, 4], method = "fdr")
    print(decreased.error.test)
  }
  
  {
    print("Average initial error decrease")
    print(decreased.error.test <- summary(
      emmeans(
        model.split,
        revconsecavg ~ Session,
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Num = 0,
          Group = c('Exp5:NotAtaxic:Switch')
        ),
        adjust = "fdr",
      )$contrasts,
      infer = TRUE
    )[1, c(2, 5, 6, 8)])
  }
  
  {
    print("Retention (first row is avg)")
    print(rentention.tests <- (summary(
      emmeans(
        model.split,
        skipconsecavg ~ Num * Session,
        at = list(
          Session = c('S1', 'S2', 'S3', 'S4', 'S5'),
          Num = c(0, 3),
          Group = 'Exp5:NotAtaxic:Switch'
        ),
        reverse = TRUE,
        adjust = 'fdr'
      ),
      infer = TRUE
    )$contrasts)[, c(2, 5, 6, 8)])
  }
  
  {
    print("After effect switch")
    print(after.effect.switch <- summary(
      emmeans(
        model.washout,
        identity ~ Group,
        at = list(
          Session = 'S5',
          Num = 0,
          Group = c('Exp5:NotAtaxic:Switch')
        ),
        adjust = "none"
      )$contrasts,
      infer = TRUE
    )[, c(2, 5, 6, 8)])
  }
  
  
  print("> The observed effect may not necessitate within-session alternation")
  
}
