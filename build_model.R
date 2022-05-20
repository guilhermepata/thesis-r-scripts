library(lme4)
# library(lmerTest)
# library(lmtest)
library(modelsummary)
# library(kableExtra)
library(emmeans)
# library(optimx)
# library(EBImage)
# library(minpack.lm)
library(feather)
library(ggplot2)
# library(ggeffects)
# library(effects)
library(sjPlot)
library(tidyverse)
# library(merTools)
source("my_functions.R")

if (!exists("is.built") || !is.built) {
  ### get file
  if (!exists("name")) {
    name = 'Exp3'
  }
  
  if (name != 'Exp3') {
    data_frame <- read.csv(
      file = paste(
        './data/',
        name,
        '_df.csv',
        sep = "",
        collapse = NULL
      ),
      sep = ",",
      dec = ".",
      stringsAsFactors = TRUE
    )
  } else {
    data_frame = data.frame()
    for (group in c('Switch', 'NoSwitch'))
    {
      data_frame <-  rbind(data_frame,
                           read.csv(
                             file = paste(
                               './data/',
                               name,
                               group,
                               '_df.csv',
                               sep = "",
                               collapse = NULL
                             ),
                             sep = ",",
                             dec = ".",
                             stringsAsFactors = TRUE
                           ))
    }
  }
  
  # data_frame$Group = relevel(data_frame$Phenotype, ref=3)
  data_frame <-
    within(data_frame, Group <- factor(Phenotype:Protocol))
  if (name != 'Exp4') {
    data_frame$Group = relevel(data_frame$Group, 'NotAtaxic:NoSwitch')
  } else {
    data_frame$Group = relevel(data_frame$Group, 'NotAtaxic:Switch')
  }
  data_frame <- data_frame[!is.na(data_frame$Asym), ]
  data_frame = filter(data_frame, Phenotype != 'HalfAtaxic')
  data_frame = cbind(Experiment = rep(name, nrow(data_frame)), data_frame)
  # data_frame <- data_frame[data_frame$Protocol == 'Switch',]
  
  data.total_frame <- data_frame
  data.split <- data_frame[data_frame$Phase == "Split", ]
  data.intersplit <- data_frame[data_frame$Phase == "Intersplit", ]
  
  data.baseline <- data_frame[data_frame$Phase == "Baseline", ]
  data.washout <- data_frame[data_frame$Phase == "Washout", ]
  
  model.equation <-
    'Asym ~ Num * Session * Group + (1 + Num| Animal)'
  if (name != 'Exp4') {
    model.equation.intersplit <-
      'Asym ~ Num * Session + (1 + Num| Animal)'
  } else {
    model.equation.intersplit <-
      'Asym ~ Num * Session * Group + (1 + Num| Animal)'
  }
  
  model.split <-
    lmer(model.equation, data = data.split, REML = "false")
  model.washout <-
    lmer(model.equation, data = data.washout, REML = "false")
  model.intersplit <-
    lmer(model.equation.intersplit,
         data = data.intersplit,
         REML = "false")
  model.baseline <-
    lmer(model.equation, data = data.baseline, REML = "false")
  
  modelsummary(model.split,
               stars = TRUE,
               metrics = c("RMSE", "R2"))
  
  modelsummary(
    model.split,
    stars = TRUE,
    estimate = "{estimate}({p.value})",
    statistic = "[{conf.low}:{conf.high}]"
  )
  
  if (!exists("useOld") || !useOld) {
    data.split.summary = summarise.predict(data.split, model.split, Experiment, Trial, Phase)
    data.washout.summary = summarise.predict(data.washout, model.washout, Experiment, Trial, Phase)
    data.intersplit.summary = summarise.predict(data.intersplit,
                                                model.intersplit,
                                                Experiment,
                                                Trial,
                                                Phase)
    data.baseline.summary = summarise.predict(data.baseline, model.baseline, Experiment, Trial, Phase)
    
  } else {
    data.split.summary = summarise.predict.old(data.split,
                                               model.split,
                                               Num,
                                               Session,
                                               Group,
                                               Experiment,
                                               Trial,
                                               Phase)
    data.washout.summary = summarise.predict.old(data.washout,
                                                 model.washout,
                                                 Num,
                                                 Session,
                                                 Group,
                                                 Experiment,
                                                 Trial,
                                                 Phase)
    data.intersplit.summary = summarise.predict.old(data.intersplit,
                                                    model.intersplit,
                                                    Num,
                                                    Session,
                                                    Group,
                                                    Experiment,
                                                    Trial,
                                                    Phase)
    data.baseline.summary = summarise.predict.old(data.baseline,
                                                  model.baseline,
                                                  Num,
                                                  Session,
                                                  Group,
                                                  Experiment,
                                                  Trial,
                                                  Phase)
  }
  
  data.summary = rbind(
    data.split.summary,
    data.washout.summary,
    data.intersplit.summary,
    data.baseline.summary
  )
  
  
  is.built <- TRUE
  
}

# anova(fm1)
#
# emms1<- emtrends(fm1,pairwise~genotype|cell_deathcat, var="trial_num",
#                  lmer.df = "satterthwaite",type="response")
#
# summary(emms1, infer=TRUE)

# emms1<- emmeans(fm1, pairwise~genotype | cell_deathcat,
#                 lmer.df = "satterthwaite",type="response")
#
# summary(emms1,infer=TRUE)

# emms1<- emtrends(fm1,pairwise~genotype*trial_num, var="cell_death",
#                  lmer.df = "satterthwaite",type="response",
#                  at= list(trial_num= c(4)))
