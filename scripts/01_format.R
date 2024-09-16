
# load libraries ---------------------------------------------------------------
library(tidyverse)


# load data --------------------------------------------------------------------
dat <- read.csv("data/dat.csv")


# format data ------------------------------------------------------------------
dat <- dat %>%
  
  mutate(
    change = post - pre
  ) %>%
  
  mutate(
    change_bin = ifelse(
      change <= 0,
      0,
      1
    )
  ) %>%
  
  mutate(
    spoke_medprovider = ifelse(
      spoke_medprovider == 2,
      "No",
      "Yes"
    )
  ) %>%
  
  mutate(
    spoke_ambassador = ifelse(
      spoke_ambassador == 2,
      "No",
      "Yes"
    )
  ) %>%
  
  mutate(
    vaccinated = ifelse(
      vaccinated == 2,
      "No",
      "Yes"
    )
  ) %>%
  
  mutate(
    gender = ifelse(
      gender == 1,
      "Male",
      ifelse(
        gender == 2,
        "Female",
        "Other"
      )
    )
  )

dat$age <- as.numeric(dat$age)
dat$race <- as.factor(dat$race)
dat$gender <- as.factor(dat$gender)
dat$vaccinated <- as.factor(dat$vaccinated)
dat$doses <- as.factor(dat$doses)
dat$spoke_ambassador <- as.factor(dat$spoke_ambassador)
dat$spoke_ambassador <- factor(dat$spoke_ambassador, levels = c("No", "Yes"))
