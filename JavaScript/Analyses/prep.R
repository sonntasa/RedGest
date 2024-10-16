if (sys.nframe() == 0) {
  library(tidyverse)
}

############################# Reading in the data #############################
ReadIn <- function(DatDir = "../Results") {
  datFiles <-
    list(
      list.files(
        path = DatDir,
        pattern = glob2rx("^Speech*.csv$"),
        full.names = TRUE,
        recursive = TRUE
      )
    )
  dat <- do.call(read_csv, datFiles)
  message("The data directory is: ", DatDir)
  return(dat)
}

################################# Formatting #################################
MyReformat <- function(dat) {
  to_factor <- c(
    "vpNum", "matching", "modality", "verbal", "gesture",
    "order", "specific_gesture", "polarity"
  )
  to_numeric <- c("rt", "age", "trialNum", "error")
  dat <- dat %>%
    rename(specific_gesture = gesture) %>%
    mutate(
      gesture = case_when(
        startsWith(specific_gesture, "Head") ~ "Head",
        startsWith(specific_gesture, "Thumb") ~ "Thumb",
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

################################# Exclusion ##################################
MyExclusion <- function(
    dat,
    ExcAcc = FALSE,
    ExcError = FALSE,
    ExcTime = FALSE,
    ExcGesture = NULL,
    ExcMatching = FALSE) {
  ############## Exclude ER >= 0.3 ###############
  if (ExcAcc) {
    er <- dat %>%
      group_by(vpNum, polarity, matching) %>%
      summarize(ER = mean(error)) %>%
      filter(ER > 0.3)
    to_exclude <- er$vpNum %>% unique()
    dat <- dat %>% filter(!vpNum %in% to_exclude)
  }
  ################ Choice Errors #################
  if (ExcError) {
    dat <- dat %>% filter(error == 0)
  }
  ################# Time Errors ##################
  if (ExcTime) {
    dat <- dat %>% filter(!exclude)
  }
  ################### Gesture ####################
  if (!is.null(ExcGesture)) {
    dat <- dat %>% filter(gesture %in% ExcGesture)
    dat$gesture <- droplevels(dat$gesture)
  }
  ################### Matching ###################
  if (ExcMatching) {
    dat <- dat %>% filter(!matching == "na")
    dat$matching <- droplevels(dat$matching)
  }
  ################# Refactoring ##################
  dat$vpNum <- droplevels(dat$vpNum)
  levels(dat$vpNum) <- seq_along(levels(dat$vpNum))

  return(dat)
}

################################ Demographics ################################
vpInfo <- function(dat) {
  vp <- dat %>%
    distinct(., vpNum, .keep_all = TRUE) %>%
    mutate(age = as.numeric(age)) %>%
    summarize(
      N = n(),
      meanAge = round(mean(age), 2),
      sdAge = round(sd(age), 2),
      minAge = range(age)[1],
      maxAge = range(age)[2],
      nFemale = sum(gender == "female"),
      nMale = sum(gender == "male"),
      nNa = sum(!(gender %in% c("male", "female"))),
      nRight = sum(handedness == "right")
    )
  return(vp)
}
