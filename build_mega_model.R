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

if (!exists("is.built") || !is.built) {
  mega.data = data.frame()
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
    # if (name != 'Exp4') {
    #   data_frame$Group = relevel(data_frame$Group, 'NotAtaxic:NoSwitch')
    # } else {
    #   data_frame$Group = relevel(data_frame$Group, 'NotAtaxic:Switch')
    # }
    data_frame <- data_frame[!is.na(data_frame$Asym), ]
    data_frame = filter(data_frame, Phenotype != 'HalfAtaxic')
    data_frame = filter(data_frame, Animal != 'NoSwitch03')
    data_frame = cbind(Experiment = rep(name, nrow(data_frame)),
                       data_frame,
                       stringsAsFactors = TRUE)
    data_frame <-
      within(data_frame,
             Group <- factor(Experiment:Phenotype:Protocol))
    mega.data = rbind(mega.data, data_frame)
    rm(list = c("data_frame"))
  }
  
  mega.data$Group = relevel(mega.data$Group, 'Exp4:NotAtaxic:Switch')
  mega.data$Group = relevel(mega.data$Group, 'Exp5:NotAtaxic:Switch')
  mega.data$Group = relevel(mega.data$Group, 'Exp5:NotAtaxic:NoSwitch')
  mega.data$Group = relevel(mega.data$Group, 'Exp3:NotAtaxic:Switch')
  mega.data$Group = relevel(mega.data$Group, 'Exp3:NotAtaxic:NoSwitch')
  
  mega.data.split <- mega.data[mega.data$Phase == "Split",]
  mega.data.intersplit <-
    mega.data[mega.data$Phase == "Intersplit",]
  
  mega.data.baseline <- mega.data[mega.data$Phase == "Baseline",]
  mega.data.washout <- mega.data[mega.data$Phase == "Washout",]
  
  
  model.equation <-
    'Asym ~ Num * Session * Group + (1 + Num| Animal)'
  
  model.equation.intersplit <-
    'Asym ~ Num * Session * Group + (1 + Num| Animal)'
  
  model.split <-
    lmer(model.equation, data = mega.data.split, REML = "true")
  model.washout <-
    lmer(model.equation, data = mega.data.washout, REML = "true")
  model.intersplit <-
    lmer(model.equation.intersplit,
         data = mega.data.intersplit,
         REML = "true")
  model.baseline <-
    lmer(model.equation, data = mega.data.baseline, REML = "true")
  
  modelsummary(model.split,
               stars = TRUE,
               metrics = c("RMSE", "R2"))
  
  mega.data.split.summary = summarise.predict(mega.data.split,
                                              model.split,
                                              # Num,
                                              # Session,
                                              # Group,
                                              Experiment,
                                              Trial,
                                              Phase)
  mega.data.washout.summary = summarise.predict(mega.data.washout,
                                                model.washout,
                                                # Num,
                                                # Session,
                                                # Group,
                                                Experiment,
                                                Trial,
                                                Phase)
  mega.data.intersplit.summary = summarise.predict(mega.data.intersplit,
                                                   model.intersplit,
                                                   # Num,
                                                   # Session,
                                                   # Group,
                                                   Experiment,
                                                   Trial,
                                                   Phase)
  mega.data.baseline.summary = summarise.predict(mega.data.baseline,
                                                 model.baseline,
                                                 # Num,
                                                 # Session,
                                                 # Group,
                                                 Experiment,
                                                 Trial,
                                                 Phase)
  
  mega.data.summary = rbind(
    mega.data.split.summary,
    mega.data.washout.summary,
    mega.data.intersplit.summary,
    mega.data.baseline.summary
  )
  
  mega.data.summary = arrange(mega.data.summary, Experiment, Group, Trial)
  
  mega.data.split = predict.fit(mega.data.split, model.split)
  mega.data.washout = predict.fit(mega.data.washout, model.washout)
  mega.data.baseline = predict.fit(mega.data.baseline, model.baseline)
  mega.data.intersplit = predict.fit(mega.data.intersplit, model.intersplit)
  
  mega.data.aux = rbind(mega.data.split, mega.data.washout, mega.data.baseline, mega.data.intersplit)
  mega.data = arrange(mega.data.aux, Experiment, Group, Animal, Trial)
  
  is.built = TRUE
  
}
