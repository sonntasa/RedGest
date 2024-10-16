library(segHT)
library(tidyverse)
library(lsr)

source("./prep.R")

########### Reading In & Formatting ############
dat <- ReadIn()
dat <- MyReformat(dat)
dat <- MyExclusion(dat, gest = "Thumb")

# our ANOVA
dat_aov <- dat %>%
  group_by(vpNum, polarity, matching) %>%
  summarise(
    MeanRT = mean(rt), MeanER = mean(error),
    .groups = "drop"
  ) %>%
  droplevels()
MyAov <- ez::ezANOVA(
  data = dat_aov, dv = MeanRT, wid = vpNum, within = c("polarity", "matching")
)
# F value = 30.45 (thumb), F value = 112.1141 (head)

# but reformulation in terms of two sample t-test
dat_aov <- dat %>%
  group_by(vpNum, polarity, matching) %>%
  summarise(
    MeanRT = mean(rt),
    .groups = "drop"
  ) %>%
  droplevels() %>%
  pivot_wider(names_from = c(polarity, matching), values_from = c(MeanRT)) %>%
  mutate(
    diff_affirmation = affirmation_mismatch - affirmation_match,
    diff_negation = negation_mismatch - negation_match
  )

t.test(dat_aov$diff_affirmation, dat_aov$diff_negation, paired = TRUE)
5.5182**2 # should equal approx F value above thumb
10.588**2 # should equal approx F value above head

# cohens_d of this effect (head ~ 0.87, thumb ~ 0.46)
cd <- cohensD(dat_aov$diff_affirmation, dat_aov$diff_negation, method = "paired")

# From segHT GitHub docs
# Plan
#
# 1) Set your effect size
# 2) Set your power
# 3) Set your kmax, alpha total and alpha strong
# 4) Call alpha_weak() to get alpha_weak
# 3) Call n_for_power(n_per_segment) to find n_per_segment
# 4) for base rate from 0 to 1 in steps, call segmented_hyp_test_outcomes with the single
#    effect size to get n_expected

################ Set Parameters ################
effect_size <- cohensD(dat_aov$diff_affirmation, dat_aov$diff_negation, method = "paired")
kmax <- 2
alpha_total <- .05
alpha_strong <- alpha_total / kmax
stat_procedure_name <- "1t"
target_power <- 0.8

# Setup layout for three plots
alpha_weak <- segHT::alpha_weak(kmax, alpha_total, alpha_strong)
n_per_segment <- segHT::n_for_power(
  target_power,
  stat_procedure_name,
  effect_size,
  kmax,
  alpha_total,
  alpha_strong,
  alpha_weak
)

kmax_table <- segHT::search_kmax(
  alpha_total, alpha_strong, "1t", target_power, effect_size
)

########### n divisible by segments ############
ksize <- n_per_segment
# dat$vpNum <- droplevels(dat$vpNum)

RunSegHT <- function(dat = dat) {
  vps <- as.numeric(unique(dat$vpNum))
  for (ksegment in seq(kmax)) {
    sample_vps <- sample(vps, size = ksize)
    sample_size <- length(sample_vps)
    vps <- vps[!vps %in% sample_vps]
    dat_aov <- dat %>%
      filter(vpNum %in% sample_vps) %>%
      group_by(vpNum, polarity, matching) %>%
      summarise(
        MeanRT = mean(rt), MeanER = mean(error),
        .groups = "drop"
      ) %>%
      droplevels()
    MyAov <- ez::ezANOVA(
      data = dat_aov, dv = MeanRT, wid = vpNum, within = c("polarity", "matching")
    )
    kpval <- MyAov$ANOVA$p[3]
    if (ksegment == 1) {
      TotSample <- sample_vps
    } else {
      TotSample <- c(TotSample, sample_vps)
    }

    if (ksegment < kmax) {
      if (kpval > alpha_weak) {
        message("H0: NOT REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ", n total tested = ", sample_size * ksegment)
        break
      } else if (kpval < alpha_strong) {
        message("H0: REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ",  n total tested = ", sample_size * ksegment)
        break
      } else {
        message("Continuing, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ",  n total tested = ", sample_size * ksegment)
      }
      sample_size <- sample_size + ksize
    } else if (kpval < alpha_weak) {
      message("H0: REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ",  n total tested = ", sample_size * ksegment)
    } else {
      message("H0: NOT REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ", n total tested = ", sample_size * ksegment)
    }
  }
  dat_aov <- dat %>%
    filter(vpNum %in% TotSample) %>%
    group_by(vpNum, polarity, matching) %>%
    summarise(
      MeanRT = mean(rt), MeanER = mean(error),
      .groups = "drop"
    ) %>%
    droplevels()
  MyAov <- ez::ezANOVA(
    data = dat_aov, dv = MeanRT, wid = vpNum, within = c("polarity", "matching")
  )
  message("Overall p value: ", MyAov$ANOVA$p[3])
  return(length(TotSample))
}

number_participants <- replicate(10, RunSegHT(dat))
mean(number_participants)
