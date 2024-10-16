library(tidyverse)

dat <- read_csv("./SoundData.csv")

dat %>%
  group_by(Sex) %>%
  summarise(MeandB = mean(PeakdB))

dat %>%
  group_by(GestAffNeg) %>%
  summarise(MeandB = mean(PeakdB))

dat %>%
  group_by(Gest) %>%
  summarise(MeandB = mean(PeakdB))
