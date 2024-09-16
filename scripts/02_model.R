
# load libraries ---------------------------------------------------------------
library(tidyverse)


# fit model --------------------------------------------------------------------
mod <- glm(
  change_bin ~ 
    spoke_medprovider +
    age +
    gender +
    vaccinated +
    doses +
    spoke_ambassador,
  family = "binomial",
  data = dat
)


# variable selection -----------------------------------------------------------
mod <- step(mod)
summary(mod)