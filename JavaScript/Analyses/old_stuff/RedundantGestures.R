library(tidyverse)
library(ez)
library(psychReport)
library(DMCfun)

############################# Reading in the data #############################
DatDir <- "~/Documents/Shared/JavaScript/jsPsychExperiments/Experiments7+/RedundantGestures"
datFiles <- list(list.files(path = DatDir, pattern = glob2rx("^Speech*.csv$"), full.names = TRUE, recursive = TRUE))
dat <- do.call(read_csv, datFiles)

################ readable vpNum ################
dat$vpNum <- as.factor(dat$vpNum)
levels(dat$vpNum) <- seq_along(levels(dat$vpNum))

names(dat)
table(dat$vpNum)
table(dat$vpNum, dat$blockNum)
table(dat$matching)
table(dat$polarity)
table(dat$matching, dat$polarity)
table(dat$gesture)
table(dat$matching)
table(dat$video)
table(dat$verbal)

# # {{{ Errors seem to be coded correctly
# MyCorr <- dat %>%
#   mutate(MyResp = str_to_upper(response)) %>%
#   mutate(MyCorr = MyResp == correct_key)
#
# table(MyCorr$MyCorr)
# table(dat$error)
# # }}}

# # {{{ Slow Fast Issue
# dat %>%
#   dplyr::filter(!(error)) %>%
#   summarise(Min = min(rt), Max = max(rt), Mean = round(mean(rt)))
# # A tibble: 1 × 3
# #     Min   Max  Mean
# #   <dbl> <dbl> <dbl>
# # 1     1  8656  1056
# # NOTE: We have a "correct" reaction time of 1ms. That seems like an issue
# # }}}

dat %>% names()
dat$rt %>% head()

dat <- dat %>%
  mutate(
    Fast = rt <= 150,
    Slow = rt > 2000,
    WhichGest = case_when(
      startsWith(gesture, "Thumb") ~ "Thumb",
      startsWith(gesture, "Head") ~ "Head",
      TRUE ~ "None"
    )
  )

datAgg <- dat %>%
  # filter(vpNum != 2) %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanRT = round(mean(rt)), MeanER = mean(error) * 100)

datAgg %>%
  group_by(matching) %>%
  summarise(MeanRT = round(mean(MeanRT)), MeanER = mean(MeanER))

datAgg %>%
  group_by(polarity) %>%
  summarise(MeanRT = round(mean(MeanRT)), MeanER = mean(MeanER))

################ initial Stats #################
ez::ezANOVA(
  dv = MeanRT, wid = vpNum, data = datAgg, within = c(matching, polarity)
)

#################################### DMC #####################################
delta_thumb <- DMCfun::dmcObservedData(
  dat = dat[dat$WhichGest == "Thumb", ],
  outlier = c(150, 2000),
  nDelta = 9,
  columns = c("polarity", "matching", "rt", "error"),
  compCoding = c("match", "mismatch"),
  errorCoding = c(0, 1)
)

par(mfrow = c(1, 2))
plot(delta_thumb, subject = "affirmation")
plot(delta_thumb, subject = "negation")

delta_head <- DMCfun::dmcObservedData(dat[dat$WhichGest == "Head"])
