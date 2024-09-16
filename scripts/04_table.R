
# load libraries ---------------------------------------------------------------
library(table1)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


# table 1 ----------------------------------------------------------------------
table1(~ age +
         gender +
         vaccinated +
         doses +
         spoke_medprovider +
         spoke_ambassador
       ,
       data = dat)


# model summary ----------------------------------------------------------------
tab_model(mod)

