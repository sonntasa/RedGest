if (sys.nframe() == 0) {
  library(tidyverse)
  library(psychReport)
  library(ez)
  source("./prep.R")
}

################## reading in ##################
dat <- ReadIn()
dat <- MyReformat(dat)
N <- dat %>% vpInfo()
dat <- MyExclusion(dat, ExcTime = TRUE, ExcAcc = TRUE)
dat %>% vpInfo()

length(unique(dat$vpNum[dat$order == "speech-gesture"]))
length(unique(dat$vpNum[dat$order == "gesture-speech"]))

##############################################################################
#                                    Head                                    #
##############################################################################
######################### 3: matching x 2: polarity ##########################
# {{{
dat_head_er <- MyExclusion(dat, ExcGesture = "Head")
dat_head_rt <- MyExclusion(dat_head_er, ExcError = TRUE)

################################################
#                      RT                      #
################################################
HeadAggRT3 <- dat_head_rt %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanRT = mean(rt))

HeadAovRT3 <- ezANOVA(
  data = HeadAggRT3,
  dv = MeanRT,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

HeadAovRT3 <- aovTable(HeadAovRT3)

################################################
#                      ER                      #
################################################
HeadAggER3 <- dat_head_er %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanER = mean(error))

HeadAovER3 <- ezANOVA(
  data = HeadAggER3,
  dv = MeanER,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

HeadAovER3 <- aovTable(HeadAovER3)
# }}}

######################### 2: matching x 2: polarity ##########################
# {{{
dat_head_er <- dat_head_er %>% MyExclusion(ExcMatching = TRUE)
dat_head_rt <- dat_head_rt %>% MyExclusion(ExcMatching = TRUE)

################################################
#                      RT                      #
################################################
HeadAggRT2 <- dat_head_rt %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanRT = mean(rt))

HeadAovRT2 <- ezANOVA(
  data = HeadAggRT2,
  dv = MeanRT,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

HeadAovRT2 <- aovTable(HeadAovRT2)

################################################
#                      ER                      #
################################################
HeadAggER2 <- dat_head_er %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanER = mean(error))

HeadAovER2 <- ezANOVA(
  data = HeadAggER2,
  dv = MeanER,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

HeadAovER2 <- aovTable(HeadAovER2)
# }}}

##############################################################################
#                                   Thumb                                    #
##############################################################################
######################### 3: matching x 2: polarity ##########################
# {{{
dat_thumb_er <- MyExclusion(dat, ExcGesture = "Thumb")
dat_thumb_rt <- MyExclusion(dat_head_er, ExcError = TRUE)

################################################
#                      RT                      #
################################################
ThumbAggRT3 <- dat_thumb_rt %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanRT = mean(rt))

ThumbAovRT3 <- ezANOVA(
  data = ThumbAggRT3,
  dv = MeanRT,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

ThumbAovRT3 <- aovTable(ThumbAovRT3)

################################################
#                      ER                      #
################################################
ThumbAggER3 <- dat_thumb_er %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanER = mean(error))

ThumbAovER3 <- ezANOVA(
  data = ThumbAggER3,
  dv = MeanER,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

ThumbAovER3 <- aovTable(ThumbAovER3)
# }}}

######################### 2: matching x 2: polarity ##########################
# {{{
dat_thumb_er <- dat_thumb_er %>% MyExclusion(ExcMatching = TRUE)
dat_thumb_rt <- dat_thumb_rt %>% MyExclusion(ExcMatching = TRUE)

################################################
#                      RT                      #
################################################
ThumbAggRT2 <- dat_thumb_rt %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanRT = mean(rt))

ThumbAovRT2 <- ezANOVA(
  data = ThumbAggRT2,
  dv = MeanRT,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

ThumbAovRT2 <- aovTable(ThumbAovRT2)

################################################
#                      ER                      #
################################################
ThumbAggER2 <- dat_thumb_er %>%
  group_by(vpNum, matching, polarity) %>%
  summarise(MeanER = mean(error))

ThumbAovER2 <- ezANOVA(
  data = ThumbAggER2,
  dv = MeanER,
  wid = vpNum,
  within = c(matching, polarity),
  detailed = TRUE,
  return_aov = TRUE
)

ThumbAovER2 <- aovTable(ThumbAovER2)
# }}}

##############################################################################
#                             Comparing Gestures                             #
##############################################################################
######### 2: polarity x 2: stimulus compatibility x 2: gesture type ##########
# {{{
dat_comp_er <- MyExclusion(dat, ExcMatching = TRUE, ExcGesture = c("Head", "Thumb"))
dat_comp_rt <- MyExclusion(dat_comp_er, ExcError = TRUE)

################################################
#                      RT                      #
################################################
AggRT <- dat_comp_rt %>%
  group_by(vpNum, matching, polarity, gesture) %>%
  summarise(MeanRT = mean(rt))

AovRT <- ezANOVA(
  data = AggRT,
  dv = MeanRT,
  wid = vpNum,
  within = c(matching, polarity),
  between = gesture,
  return_aov = TRUE,
  detailed = TRUE
)

AovRT <- aovTable(AovRT)

################################################
#                      ER                      #
################################################
AggER <- dat_comp_er %>%
  group_by(vpNum, matching, polarity, gesture) %>%
  summarise(MeanER = mean(error))

AovER <- ezANOVA(
  data = AggER,
  dv = MeanER,
  wid = vpNum,
  within = c(matching, polarity),
  between = gesture,
  return_aov = TRUE,
  detailed = TRUE
)

AovER <- aovTable(AovER)
# }}}
