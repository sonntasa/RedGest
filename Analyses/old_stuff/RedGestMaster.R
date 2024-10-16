library(tidyverse)
library(Superpower)
# library(psych)
library(irr)

datDir <- "~/Desktop/MasterVideo/data"
datFiles <- list(list.files(path = datDir, pattern = glob2rx("^Speech*.csv$"), full.names = TRUE))
dat <- do.call(read_csv, datFiles)

dat %>% names()
dat$video %>% table()
dat$gesture %>% table()
dat$modality %>% table()

################################ Formatting #################################
dat <- dat %>% mutate(
  WhichGest = as.factor(
    case_when(
      startsWith(gesture, "Thumb") ~ "Thumb",
      startsWith(gesture, "Head") ~ "Head",
      TRUE ~ "None"
    )
  )
)

################## Exclusion ###################
dat$vpNum <- as.factor(dat$vpNum)
levels(dat$vpNum) <- seq_along(levels(dat$vpNum))
dat$polarity <- as.factor(dat$polarity)
dat$matching <- as.factor(dat$matching)
dat$rt <- as.numeric(dat$rt)
dat$error <- as.numeric(dat$error)

to_exclude <- dat %>%
  group_by(vpNum, polarity, matching) %>%
  summarise(ER = mean(error)) %>%
  filter(ER >= 0.3) %>%
  ungroup() %>%
  select(vpNum) %>%
  unique()

dat <- dat %>% mutate(
  slow = rt >= 5000,
  fast = rt <= 200,
  HighER = vpNum %in% to_exclude$vpNum,
  to_exclude = slow | fast | HighER
)

dat_wide <- dat %>%
  select(vpNum, rt) %>%
  spread(key = vpNum, value = rt)

dat_wide %>% names()
dat_wide[32:length(names(dat_wide))]
