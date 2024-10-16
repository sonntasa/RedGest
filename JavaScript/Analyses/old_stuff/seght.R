library(segHT)
library(tidyverse)
library(lme4)

source("./prep.R")

########### Reading In & Formatting ############
dat <- ReadIn()
dat <- MyReformat(dat)
dat <- MyExclusion(dat, gest = "Thumb")
# dat <- MyExclusion(dat, gest = "Head")

# for multiple of 24 for kmax=6
dat <- dat %>%
  filter(vpNum %in% seq(to = 144)) %>%
  droplevels()

################## alpha_weak ##################
# honestly, I still don't quite understand this
# how values are chosen seems completely arbitrary to me.
alpha <- 0.05
kmax <- 6
alpha_strong <- alpha / kmax
alpha_weak <- alpha_weak(kmax, alpha, alpha_strong)

########### n divisible by segments ############
ksize <- length(levels(dat$vpNum)) / kmax

RunSegHT <- function(dat = dat) {
  ksets <- matrix(sample(as.numeric(unique(dat$vpNum))), nrow = kmax)
  sample_size <- ksize
  for (ksegment in seq(kmax)) {
    dat_aov <- dat %>%
      filter(vpNum %in% c(ksets[ksegment, ])) %>%
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
    if (ksegment < kmax) {
      if (kpval > alpha_weak) {
        message("H0: NOT REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ", n total tested = ", sample_size)
        break
      } else if (kpval < alpha_strong) {
        message("H0: REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ",  n total tested = ", sample_size)
        break
      } else {
        message("Continuing, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ",  n total tested = ", sample_size)
      }
      sample_size <- sample_size + ksize
    } else if (kpval < alpha_weak) {
      message("H0: REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ",  n total tested = ", sample_size)
    } else {
      message("H0: NOT REJECTED, iteration: ", ksegment, ", p = ", kpval, ", k sample size = ", ksize, ", n total tested = ", sample_size)
    }
  }
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
  message("Overall p value: ", MyAov$ANOVA$p[3])
  return(sample_size)
}

RunSegHT(dat)

################# Bayes & Lmer #################
ComputeBayes <- function(dat, sample, n) {
  dat_bayes <- dat[dat$vpNum %in% sample[seq(n)], ]
  lm.1 <- lmer(rt ~ polarity * matching + (1 | vpNum), data = dat_bayes, REML = FALSE)
  lm.0 <- update(lm.1, formula = ~ . - polarity:matching)
  BF_BIC <- exp((BIC(lm.0) - BIC(lm.1)) / 2) # BICs to Bayes factor
  return(BF_BIC)
}

BayesSegHT <- function(dat, EvCrit = 100, n = 3) {
  sample <- as.numeric(sample(unique(dat$vpNum), size = length(unique(dat$vpNum))))
  BF_BIC <- ComputeBayes(dat, sample, n)

  while (1 / EvCrit < BF_BIC && BF_BIC < EvCrit) {
    message("Continuing, BF: ", BF_BIC, " n: ", n)
    n <- n + 1
    BF_BIC <- ComputeBayes(dat, sample, n)
  }
  message("Final sample size: ", n, " BF: ", BF_BIC)
}

BayesSegHT(dat, EvCrit = 10, n = 24)
