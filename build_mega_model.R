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
library(MuMIn)
source("my_functions.R")

models = list()
data.summaries = list()
data.totals = list()
for (name in c('Exp3', 'Exp5', 'Exp4')) {
  source('build_model.R')
  models[name] = list(list(
    model.baseline,
    model.split,
    model.intersplit,
    model.washout
  ))
  data.summaries[name] = list(data.summary)
  data.totals[name] = list(data.total_frame)
  rm(list = setdiff(ls(), c(
    "data.totals", "data.summaries", "models"
  )))
}

mega.frame = data.frame()
for (name in c('Exp3', 'Exp5', 'Exp4')) {
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
  data_frame <- data_frame[!is.na(data_frame$Asym),]
  data_frame = filter(data_frame, Phenotype != 'HalfAtaxic')
  data_frame = cbind(Experiment = rep(name, nrow(data_frame)), data_frame)
  
  mega.frame = rbind(mega.frame, data_frame)
  rm(list = setdiff(
    ls(),
    c("data.totals", "data.summaries", "models", "mega.frame")
  ))
}

mega.data.split <- mega.frame[mega.frame$Phase == "Split", ]
mega.data.intersplit <-
  mega.frame[mega.frame$Phase == "Intersplit", ]

mega.data.baseline <- mega.frame[mega.frame$Phase == "Baseline", ]
mega.data.washout <- mega.frame[mega.frame$Phase == "Washout", ]


model.equation <-
  'Asym ~ Num * Session * interaction(Group, Experiment) + (1 + Num| Animal)'

model.equation.intersplit <-
  'Asym ~ Num * Session * interaction(Group, Experiment) + (1 + Num| Animal)'

model.split <-
  lmer(model.equation, data = mega.data.split, REML = "false")
model.washout <-
  lmer(model.equation, data = mega.data.washout, REML = "false")
model.intersplit <-
  lmer(model.equation.intersplit,
       data = mega.data.intersplit,
       REML = "false")
model.baseline <-
  lmer(model.equation, data = mega.data.baseline, REML = "false")

source('my_functions.R')

mega.data.split.summary = summarise.predict.old(mega.data.split,
                                                model.split,
                                                Num,
                                                Session,
                                                Group,
                                                Experiment,
                                                Trial,
                                                Phase)
mega.data.washout.summary = summarise.predict.old(mega.data.washout,
                                                  model.washout,
                                                  Num,
                                                  Session,
                                                  Group,
                                                  Experiment,
                                                  Trial,
                                                  Phase)
mega.data.intersplit.summary = summarise.predict.old(mega.data.intersplit,
                                                     model.intersplit,
                                                     Num,
                                                     Session,
                                                     Group,
                                                     Experiment,
                                                     Trial,
                                                     Phase)
mega.data.baseline.summary = summarise.predict.old(mega.data.baseline,
                                                   model.baseline,
                                                   Num,
                                                   Session,
                                                   Group,
                                                   Experiment,
                                                   Trial,
                                                   Phase)

mega.data.summary = rbind(
  mega.data.split.summary,
  mega.data.washout.summary,
  mega.data.intersplit.summary,
  mega.data.baseline.summary
)


old.data.summary = rbind(data.summaries[[1]], data.summaries[[2]], data.summaries[[3]])
old.data.summary = arrange(old.data.summary, Experiment, Group, Trial)

mega.data.summary = arrange(mega.data.summary, Experiment, Group, Trial)

average.error <- function(c1, c2) {
  return(mean(abs(c1 - c2)) / mean(c(mean(abs(
    c1
  )), mean(abs(
    c2
  )))))
}

max.error <- function(c1, c2) {
  return(max(abs(c1 - c2)) / mean(c(mean(abs(
    c1
  )), mean(abs(
    c2
  )))))
}

old.fit = old.data.summary$Fit
new.fit = mega.data.summary$Fit

compare.fits = data.frame(Old = old.fit,
                          New = new.fit,
                          Difference = old.fit - new.fit)
compare.fits = cbind(mega.data.summary[c(1, 2, 3, 4, 5, 6)], compare.fits)
rel.log = ((compare.fits$New / compare.fits$Old))
compare.fits = cbind(compare.fits, Rel.log.diff = rel.log)

test = average.error(old.fit, new.fit)

hist(
  compare.fits$Rel.log.diff,
  breaks = 1000,
  xlim = c(0.90, 1.10),
  main = 'Histogram of the ratio between \n the "mega model" and the joined models',
  xlab = 'Ratio between mega model prediction and old prediction'
)

compare.r2 = data.frame()
for (name in c('Exp3', 'Exp5', 'Exp4')) {
  for (phase in unique(mega.data.summary$Phase)) {
    if (phase == 'Split') {
      aux = cbind(Experiment = name, Phase = phase, r.squaredGLMM(models[name][[1]][[1]]))
    }
    if (phase == 'Washout') {
      aux = cbind(Experiment = name, Phase = phase, r.squaredGLMM(models[name][[1]][[2]]))
    }
    if (phase == 'Intersplit') {
      aux = cbind(Experiment = name, Phase = phase, r.squaredGLMM(models[name][[1]][[3]]))
    }
    if (phase == 'Baseline') {
      aux = cbind(Experiment = name, Phase = phase, r.squaredGLMM(models[name][[1]][[4]]))
    }
    compare.r2 = rbind(compare.r2, aux)
  }
}


