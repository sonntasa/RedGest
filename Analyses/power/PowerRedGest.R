library(tidyverse)
library(Superpower)
source("./prep.R")

############################# Reading in the data #############################
dat <- ReadIn()
dat <- MyReformat(dat)
# dat <- MyExclusion(dat, gest = "Head")
dat <- MyExclusion(dat, gest = "Thumb")

cov_dat <- dat %>%
  group_by(vpNum, polarity, matching) %>%
  summarise(MeanRT = round(mean(rt))) %>%
  pivot_wider(
    names_from = c("polarity", "matching"), values_from = "MeanRT"
  ) %>%
  ungroup() %>%
  select(-c(vpNum))

MyMeans <- unlist(lapply(mean, X = cov_dat))
MySDs <- unlist(lapply(sd, X = cov_dat))

design_result <- ANOVA_design(
  design = "2w*2w",
  n = length(levels(dat$vpNum)),
  mu = MyMeans,
  sd = MySDs,
  # r = cor(cov_dat),
  r = 0.9,
  plot = TRUE
)

mean(cor(cov_dat))

plot_power(design_result, min_n = 10, max_n = 250, desired_power = 90)

################################################
#                  Trying lm                   #
################################################
library(lme4)

lm.0 <- lmer(rt ~ 0 * (1 | vpNum), data = dat)
lm.2 <- lmer(rt ~ matching + (1 | vpNum), data = dat)
lm.1 <- lmer(rt ~ polarity + (1 | vpNum), data = dat)
# lm.3 <- lmer(rt ~ which_gest * (1 | vpNum), data = dat)
lm.4 <- lmer(rt ~ polarity + matching + (1 | vpNum), data = dat)
lm.5 <- lmer(rt ~ polarity * matching + (1 | vpNum), data = dat, REML = FALSE)
# lm.6 <- lmer(rt ~ polarity * matching * (1 | vpNum), data = dat)

anova(lm.0, lm.1, lm.2, lm.3, lm.4, lm.5, lm.6)
anova(lm.0, lm.1, lm.2, lm.4, lm.5)

summary(lm.5)
