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

if (!exists("is.builtLR") ||
    !is.builtLR) {
  ### get file
  mega.data.first.lr = data.frame()
  for (name.aux in c('Exp3', 'Exp5', 'Exp4')) {
    if (name.aux != 'Exp3') {
      data_frame_lr <- read.csv(
        file = paste(
          './data/',
          name.aux,
          '_perc_df.csv',
          sep = "",
          collapse = NULL
        ),
        sep = ",",
        dec = ".",
        stringsAsFactors = TRUE
      )
    } else {
      data_frame_lr = data.frame()
      for (group in c('Switch', 'NoSwitch'))
      {
        data_frame_lr <-  rbind(
          data_frame_lr,
          read.csv(
            file = paste(
              './data/',
              name.aux,
              group,
              '_perc_df.csv',
              sep = "",
              collapse = NULL
            ),
            sep = ",",
            dec = ".",
            stringsAsFactors = TRUE
          )
        )
      }
    }
    
    
    data_frame_lr <-
      within(data_frame_lr, Group <- factor(Phenotype:Protocol))
    
    data_frame_lr <- data_frame_lr[!is.na(data_frame_lr$Asym), ]
    
    data_frame_lr = cbind(
      Experiment = rep(name.aux, nrow(data_frame_lr)),
      data_frame_lr,
      stringsAsFactors = TRUE
    )
    data_frame_lr <-
      within(data_frame_lr,
             Group <- factor(Experiment:Phenotype:Protocol))
    
    data.first.lr <-
      filter(data_frame_lr, Phase == "Split", Num == 0, Perc > 0)
    
    
    mega.data.first.lr = rbind(mega.data.first.lr, data.first.lr)
    
    rm(list = c("data.first.lr", "data_frame_lr"))
    
  }
  
  mega.data.first.lr$Group = relevel(mega.data.first.lr$Group, 'Exp4:NotAtaxic:Switch')
  mega.data.first.lr$Group = relevel(mega.data.first.lr$Group, 'Exp5:NotAtaxic:Switch')
  mega.data.first.lr$Group = relevel(mega.data.first.lr$Group, 'Exp5:NotAtaxic:NoSwitch')
  mega.data.first.lr$Group = relevel(mega.data.first.lr$Group, 'Exp3:NotAtaxic:Switch')
  mega.data.first.lr$Group = relevel(mega.data.first.lr$Group, 'Exp3:NotAtaxic:NoSwitch')
  
  
  model.equation.lr <-
    'Asym ~ Perc * Session * Group + (1 + Perc| Animal)'
  
  model.first.lr <-
    lmer(model.equation.lr, data = mega.data.first.lr, REML = "true",)
  
  modelsummary(model.first.lr,
               stars = TRUE,
               metrics = c("RMSE", "R2"))
  
  
  is.builtLR <- TRUE
  
}
