###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Re-analysis of Gharbi et al. (2019) UTI paper
#
# File:     99_simulate_data.R
# Date:     21/09/2020
# Task:     Simulate fake data with similar marginal distributions as the
#           original data to test the analysis code.
# Note:     The data generated here is a simulation of the dataset obtained
#           after running 01_extract_data.R and 02_derive_tables.R
#
###########################################################################

library(checkpoint)
checkpoint("2019-08-23", R.version = "3.6.3", use.knitr = TRUE)

library(simstudy)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)


set.seed(42)
n <- 280462

# Create the data directory
if(!dir.exists("01_derived")) {
  dir.create("01_derived")
}

# Define all data fields for simulation -----------------------------------

def <- 
  defData(varname = "start", dist = "uniformInt",
          formula = "13604;16506", id = "eventid") %>% 
  defData(varname = "patid", dist = "uniformInt",
          formula = str_c("1", n, sep = ";")) %>% 
  defData(varname = "recur", dist = "categorical",
          formula = "0.75;0.25") %>% 
  defData(varname = "uti_365", dist = "categorical",
          formula = "0.63;0.37") %>% 
  defData(varname = "presc", dist = "categorical",
          formula = "0.87;0.13") %>%
  defData(varname = "sep", dist = "categorical",
          formula = "0.99553;0.00447") %>% 
  defData(varname = "tts", dist = "uniformInt",
          formula = "1;60") %>% 
  defData(varname = "age", dist = "normal",
          formula = 80, variance = 66) %>% 
  defData(varname = "female", dist = "categorical",
          formula = "0.22;0.78") %>% 
  defData(varname = "imd", dist = "categorical",
          formula = "0.25;0.24;0.22;0.17;0.12") %>% 
  defData(varname = "pracid", dist = "uniformInt",
          formula = "1;697") %>% 
  defData(varname = "region", dist = "categorical",
          formula = "0.41;0.1;0.28;0.21") %>% 
  defData(varname = "year", dist = "categorical",
          formula = "0.13;0.12;0.13;0.13;0.13;0.14;0.13;0.09") %>% 
  defData(varname = "cci", dist = "negBinomial", link = "log",
          formula = "0.67", variance = "0.6") %>% 
  defData(varname = "smoke", dist = "categorical",
          formula = "0.6;0.33;0.07") %>% 
  defData(varname = "hosp_30", dist = "categorical",
          formula = "0.93;0.07") %>% 
  defData(varname = "hosp_nights", dist = "negBinomial", link = "log",
          formula = "1.32", variance = "14") %>% 
  defData(varname = "hosp_n", dist = "negBinomial", link = "log",
          formula = "-1.13", variance = "2.8") %>% 
  defData(varname = "ae_30", dist = "categorical",
          formula = "0.96;0.04") %>% 
  defData(varname = "ae_n", dist = "negBinomial", link = "log",
          formula = "-0.8", variance = "2.4") %>% 
  defData(varname = "abx_30", dist = "categorical",
          formula = "0.81;0.19") %>% 
  defData(varname = "constype", dist = "uniformInt",
          formula = "1;55") %>% 
  defData(varname = "home", dist = "categorical",
          formula = "0.96;0.04") %>% 
  defData(varname = "prophy", dist = "categorical",
          formula = "0.98;0.02") %>% 
  defData(varname = "died", dist = "categorical",
          formula = "0.98;0.02") %>% 
  defData(varname = "ttd", dist = "uniformInt",
          formula = "0;60") %>% 
  defData(varname = "other_hosp", dist = "categorical",
          formula = "0.94;0.06") %>% 
  defData(varname = "tth", dist = "uniformInt",
          formula = "0;60") 
  


# Simulate from the defined (independent) distributions -------------------

dt <- genData(n, def)


# Format variables --------------------------------------------------------

fct_yesno <- function(x){
  factor(x, levels = 1:2, c("no", "yes"))
}

region_nms <- c("South of England",
                "London",
                "Midlands and east of England",
                "North of England and Yorkshire")

smoke_nms <- c("non", "ex", "smoke")

dt <- dt %>% 
  mutate(
    start = as_date(start),
    recur = fct_yesno(recur),
    uti_365 = fct_yesno(uti_365),
    presc = factor(presc, levels = 1:2, c("yes", "no")),
    sep = fct_yesno(sep),
    female = fct_yesno(female),
    imd = factor(imd, 1:5, str_c("Q", 1:5)),
    region = factor(region, 1:4, region_nms),
    year = factor(year, 1:8, 2007:2014),
    smoke = factor(smoke, 1:3, smoke_nms),
    hosp_30 = fct_yesno(hosp_30),
    ae_30 = fct_yesno(ae_30),
    home = fct_yesno(home),
    prophy = fct_yesno(prophy),
    died = fct_yesno(died),
    other_hosp = fct_yesno(other_hosp)
  )



# Derive random dependent variables ---------------------------------------

dt <- dt %>% 
  mutate(
    hosp_7 = fct_yesno(
      ifelse(hosp_30 == "yes", sample(1:2, n, TRUE, c(0.68, 0.32)), 1)
    ),
    abx_7 = fct_yesno(
      ifelse(abx_30 == "yes", sample(1:2, n, TRUE, c(0.72, 0.28)), 1)
    )
  )



# Derive non-random dependent variables -----------------------------------


dt <- dt %>% 
  mutate(
    # Time variables (mostly used for definition of other variables)
    before_7 = start %m-% days(7),
    before_30 = start %m-% days(30),
    before_365 = start %m-% days(365),
    after_7 = start %m+% days(7),
    after = start %m+% days(60),
    
    # Time-to-outcome
    tts = ifelse(sep == "yes", tts, NA),
    ttd = ifelse(died == "yes", ttd, NA),
    tth = ifelse(other_hosp == "yes", tth, NA),
    
    # Categorised variables
    age_cat = cut(age, 
                  right = FALSE, 
                  breaks = c(-Inf, 75, 85, Inf),
                  labels = c("65-74", "75-84", "85+"))
  )

dt <- dt %>% 
  arrange(patid, eventid) %>% 
  group_by(patid) %>% 
  mutate(nmr = row_number())




# Order columns and save --------------------------------------------------

dt <- dt %>% 
  select(patid, eventid, pracid, start, before_365, before_30, before_7, 
         after_7, after, year, presc, sep, tts, died, ttd, other_hosp, tth, 
         age, age_cat, female, imd, region, smoke, cci, hosp_30, hosp_7, 
         hosp_nights,hosp_n, ae_30, ae_n, uti_365, nmr, abx_30, abx_7, 
         prophy, recur,constype, home) 

write_rds(dt, "01_derived/epi_60.rds")

