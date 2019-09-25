###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Re-analysis of Gharbi et al. (2019) UTI paper
#
# File:     04_propensity_scores.R
# Date:     28/04/2019
# Task:     Calculate propensity scores using parametric and non-parametric
#           methods
#
###########################################################################





# Path from project directory to this file
# NOTE: must be set in each programme separately
subfolder <- "00_replication/gharbi_2019"

# Initialise the workspace
source(file.path(subfolder, "00_init.R"))

library(ggplot2)
library(purrr)
library(MatchIt)
library(lsr)
library(geepack)
library(twang)

# Load the previously extracted data --------------------------------------

char(epi) %>% 
  walk(load_derived)

epi[, treat := as.integer(presc == "no")]     # Treament = "no antibiotic"
epi[, outcome := as.integer(sep_30 == "yes")] # Outcome = "sepsis 30 days"


# Estimate the propensity scores ------------------------------------------

ps_form <- treat ~ age_cat + female + region + imd + year + 
                   cci + abx_30 + hosp_7 + hosp_30 + hosp_n + 
                   hosp_nights + ae_30 + ae_n + uti_365 + recur + 
                   prophy

# Classical logistic regression propensity score without interactions
ps_glm_mod <- glm(ps_form, data = epi, family = binomial)
epi[, ps_glm := predict(ps_glm_mod, type = "response")]

# Non-parametric gradient boosting regression propensity score
ps_gbm_mod <- ps(ps_form, data = epi, n.trees = 5000, 
                 interaction.depth = 4, 
                 shrinkage = 0.01, 
                 stop.method = "es.mean",
                 estimand = "ATT")
epi[, ps_gbm := ps_gbm_mod$ps$es.mean.ATT]




# Investigate the common support and shape
ggplot(epi, aes(ps, fill = presc)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.01, alpha = 0.5)


stand_diff <- function(dt, var){
  quintile <- ntile(epi$ps, 5)
  d <- rep(NA, 5)
  for(i in 1:5){
    d[i] <- cohensD(as.formula(str_c("`", var, "` ~ presc")), 
                    data = cbind(dt, epi[, .(presc)])[quintile == i])
  }
  d
}

stand_diff(epi, "ps")




# Investigate the balance of covariates -----------------------------------

# Encode categories as dummy vars for standardised difference calc
dummy <- as.data.table(model.matrix(ps_form, data = epi)[, -1])

diffs_before <- map(names(dummy), ~ as.data.table(t(stand_diff(.)))) %>% 
  purrr::set_names(names(dummy)) %>% 
  rbindlist(idcol = "var")





# Match cases and controls ------------------------------------------------

epi[, treat := if_else(presc == "no", 1, 0)]
matched <- matchit(treat ~ age_cat + female, data = epi[, .(patid, eventid, start, treat, age_cat, female)], 
                   distance = epi$ps, method = "nearest", ratio = 5,
                   caliper = 0.25)

write_rds(matched, file.path(subfolder, "02_matched", "nearest_5p.rds"))

match_dt <- match.data(matched) %>% as.data.table()
match_dt <- epi[unique(match_dt[, "patid"]), on = "patid"]
setDT(match_dt)

summary(matched, standardize = TRUE)




dummy_match <- as.data.table(model.matrix(ps_form, data = nearest_1p)[, -1])



stand_diff <- function(dt, var){
  quintile <- ntile(match_dt$ps, 5)
  d <- rep(NA, 5)
  for(i in 1:5){
    d[i] <- cohensD(as.formula(str_c("`", var, "` ~ presc")), 
                    data = cbind(dt, match_dt[, .(presc)])[quintile == i])
  }
  d
}


diffs_match <- map(names(dummy_match), ~ as.data.table(t(stand_diff(dummy_match, .)))) %>% 
  purrr::set_names(names(dummy_match)) %>% 
  rbindlist(idcol = "var")



variables <- c("presc", "age_cat", "female", "imd", "region", "year", "cci", "smoke",
               "hosp_7", "hosp_30", "hosp_nights", "hosp_n", "ae_30", "ae_n", "abx_30", 
               "recur", "uti_365")


match_form <- as.formula(str_c("sep_30 == 'yes' ~ ", str_c(variables, collapse = " + ")))

match_mod <- geeglm(match_form, id = patid, data = match_dt, family = binomial, corstr = "exchangeable")



mytable <- table(match_dt$presc, match_dt$sep_30)
binarysens(mytable[1, 2], mytable[2, 1], Gamma = 6, GammaInc = 1)






# Perform inverse probability weighting -----------------------------------

epi[, outcome := if_else(sep_30 == "yes", 1L, 0L)]
epi[, first := nmr == 1]
epi[, iptw := if_else(treat == 1, mean(ps) / ps, mean(1 - ps) / (1 - ps))]

iptw_form <- as.formula(str_c("outcome ~ ", str_c(variables, collapse = " + ")))

iptw_form <- outcome ~ presc * first + age_cat + female + imd + region + year + cci + 
  smoke + hosp_7 + hosp_30 + hosp_nights + hosp_n + ae_30 + 
  ae_n + abx_30 + recur + uti_365
iptw_mod <- geeglm(iptw_form, id = patid, data = epi, family = binomial, 
                   weights = iptw, corstr = "exchangeable")









