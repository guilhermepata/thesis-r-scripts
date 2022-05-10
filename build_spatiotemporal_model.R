source("build_model.R")
library(MuMIn)

data.zscored = data.total_frame
data.zscored[10:17] <- scale(data.zscored[10:17])

model.equation.spatiotemporal = 'Asym ~ Group * (coo + double_support) + (1|Animal) + (1|Session) + (1|Phase)'

model.spatiotemporal <-lmer(model.equation.spatiotemporal, data=data.zscored, REML= "false")

# data.split <- data_frame[data_frame$Phase == "Split",]
data.zscored2 <- cbind(data.zscored, AsymFit = predict(model.spatiotemporal))

# data.split.summary <- cbind(data.split, AsymFit = predict(model.spatiotemporal, newdata=data.split.summary, re.form = ~0))

data.summary2 = summarise(group_by(data.zscored2, Trial, Num, Session, Group), 
                                MeanFit = mean(AsymFit), 
                                MedianFit = median(AsymFit), 
                                Mean = mean(Asym),
                                Median = median(Asym))

# data.split.summary2 = summarise.predict(data.split, model.spatiotemporal, Group, Session, Num, Trial)

(p <- ggplot(data=data.summary2, aes(x=Trial)) +
    # facet_wrap(vars(Session), nrow = 1) +
    geom_point(aes(y=Mean, color=Group, shape=Group), alpha=0.3) +
    geom_line(aes(y=Mean, color=Group, shape=Group, group=Group), alpha=0.3) +
    geom_point(aes(y=MeanFit, color=Group, shape=Group), alpha=0.3) +
    geom_line(aes(y=MeanFit, color=Group, group=Group), linestyle='--', alpha=0.3)
  )


ms <- modelsummary(model.spatiotemporal, output='data.frame', stars=TRUE, gof_map = c("rmse","nobs","r2"))

# library(tibble)
# gm <- tribble(
#   ~raw, ~clean, ~fmt,
#   "r.squared", "R Squared", 5)
# # modelsummary(models, gof_map = gm)
# 
# gm <- modelsummary::gof_map
# gof_custom$omit[gof_custom$raw == 'deviance'] <- FALSE
# gof_custom$fmt[gof_custom$raw == 'r.squared'] <- "%.5f"

r.squaredGLMM(model.spatiotemporal)

(means.test = emmeans(model.spatiotemporal, pairwise ~ Group, at = list(Phase='Split', coo=0, double_support=0)))
(spatial.trend.test = emtrends(model.spatiotemporal, pairwise ~ Group, var = "coo", at=list(Phase='Split', double_support=0)))
(temporal.trend.test = emtrends(model.spatiotemporal, pairwise ~ Group, var = "double_support", at=list(Phase='Split')))
# (stl.trend.test = emtrends(model.spatiotemporal, pairwise ~ Group, var = "swing_length", at=list(Phase='Split')))
# (sts.trend.test = emtrends(model.spatiotemporal, pairwise ~ Group, var = "stance_speed", at=list(Phase='Split')))
