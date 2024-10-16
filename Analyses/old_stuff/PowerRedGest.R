library(tidyverse)
library(Superpower)

############################# Reading in the data #############################
DatDir <- "~/Desktop/RedGestMaster/data"
datFiles <- list(list.files(path = DatDir, pattern = glob2rx("^Speech*.csv$"), full.names = TRUE, recursive = TRUE))
dat <- do.call(read_csv, datFiles)

################## Formatting ##################

MyReformat <- function(dat) {
  to_factor <- c(
    "vpNum", "matching", "modality", "verbal", "gesture",
    "order", "which_gest", "polarity"
  )
  to_numeric <- c("rt", "age", "trialNum", "error")
  dat <- dat %>% mutate(
    which_gest = case_when(
      startsWith(gesture, "Head") ~ "Head",
      startsWith(gesture, "Thumb") ~ "Thumb",
      TRUE ~ "None"
    ),
    across(all_of(to_factor), ~ as.factor(.)),
    across(all_of(to_numeric), ~ as.numeric(as.character(.))),
    slow = rt > 5000,
    fast = rt < 200,
    exclude = slow | fast
  )
  levels(dat$vpNum) <- seq_along(levels(dat$vpNum))
  return(dat)
}
dat <- MyReformat(dat)

MyExclusion <- function(dat) {
  er <- dat %>%
    group_by(vpNum, polarity, matching) %>%
    summarize(ER = mean(error)) %>%
    filter(ER > 0.3)
  to_exclude <- er$vpNum %>% unique()
  dat <- dat %>% filter(!vpNum %in% to_exclude)
  dat <- dat %>% filter(!exclude)
  dat <- dat %>% filter(error == 0)
  ################### Gesture ####################
  # dat <- dat %>% filter(which_gest == "Thumb")
  dat <- dat %>% filter(which_gest == "Head")
  ################### Gesture ####################
  dat$vpNum <- droplevels(dat$vpNum)
  return(dat)
}
dat <- MyExclusion(dat)

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
  r = 0.8,
  plot = TRUE
)

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
lm.5 <- lmer(rt ~ polarity * matching + (1 | vpNum), data = dat)
# lm.6 <- lmer(rt ~ polarity * matching * (1 | vpNum), data = dat)

anova(lm.0, lm.1, lm.2, lm.3, lm.4, lm.5, lm.6)
anova(lm.0, lm.1, lm.2, lm.4, lm.5)

summary(lm.5)
