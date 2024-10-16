if (sys.nframe() == 0) {
  library(tidyverse)
  library(Superpower)
}
############################# Reading in the data #############################
ReadIn <- function() {
  DatDir <- "~/Desktop/RedGestMaster/data"
  datFiles <- list(list.files(path = DatDir, pattern = glob2rx("^Speech*.csv$"), full.names = TRUE, recursive = TRUE))
  dat <- do.call(read_csv, datFiles)
  return(dat)
}
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
  return(dat)
}

MyExclusion <- function(dat, gest = "Head") {
  er <- dat %>%
    group_by(vpNum, polarity, matching) %>%
    summarize(ER = mean(error)) %>%
    filter(ER > 0.3)
  to_exclude <- er$vpNum %>% unique()
  dat <- dat %>% filter(!vpNum %in% to_exclude)
  dat <- dat %>% filter(!exclude)
  dat <- dat %>% filter(error == 0)
  ################### Gesture ####################
  dat <- dat %>% filter(which_gest %in% gest)
  ################### Gesture ####################
  dat$vpNum <- droplevels(dat$vpNum)
  levels(dat$vpNum) <- seq_along(levels(dat$vpNum))
  return(dat)
}
