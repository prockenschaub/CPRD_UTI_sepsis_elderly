###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Re-analysis of Gharbi et al. (2019) UTI paper
#
# File:     02_derive_tables.R
# Date:     11/03/2019
# Task:     Calculate the necessary variables to perform the analysis using
#           the records extracted in part 1
#
###########################################################################


# Path from project directory to this file
# NOTE: must be set in each programme separately
subfolder <- "."

# Initialise the workspace
source(file.path(subfolder, "00_init.R"))



time_window <- 60         # Main analysis: 60
sens_exact_codes <- FALSE # Main analysis: FALSE



# Load the previously extracted data --------------------------------------

char(patients, region, uti_prim, uti_sec, sep_prim, sep_sec, 
     abx, cci, smoke, cons, admission, ae, attrition) %>% 
  walk(load_derived)

if(sens_exact_codes){
  uti_sec <- load_derived("uti_sec_gha")
}


# Define an episode -------------------------------------------------------

epi_cand <- rbind(
  uti_prim[, .(patid, start = eventdate, end = eventdate, 
               eventid = consid, source, type, 
               set = "prim")],
  uti_sec[, .(patid, start = admidate, end = discharged, 
              eventid = spno, source = str_to_lower(source), type, 
              set = "sec")]
)

num_in_study <- function(evnts){
  evnts[patients, on = "patid", nomatch = 0] %>% 
      .[evnt_date %between% list(enter_date, leave_date)] %>% 
      .[, .(patid, evnt_date, set)] %>% 
    unique() %>% 
    nrow()
}
attrition$`3_event` <- num_in_study(epi_cand[, .(patid, evnt_date = start, set)])
attrition$`3_event_p` <- num_in_study(uti_prim[, .(patid, evnt_date = eventdate, set = "prim")])
attrition$`3_event_s` <- num_in_study(uti_sec[, .(patid, evnt_date = admidate, set = "sec")])


# Deduplicat entries, give priority to upper UTI and hospital episodes
setorder(epi_cand, patid, start, -end, -set, -type, -eventid, source)
epi_cand %<>% .[, .SD[1], by = .(patid, start)]


# Determine distinct episodes (including continuations)
epi_cand[, diff := time_length(shift(end) %--% start, u = "d")]
epi_cand[is.na(diff) | patid != shift(patid), diff := 0]

epi_start <- epi_cand[, patid != shift(patid)]
epi_start[1] <- TRUE

epi_cont <- vector(mode = "logical", length = nrow(epi_cand))

l <- 1L
for(i in 2:nrow(epi_cand)){
  if(epi_start[i]){
    # First record of each patient is always a new episode
    l <- i
  } else{
    # For all later records of each patient, look at difference 
    # to last record
    
    if(epi_cand$diff[i] > time_window){ 
      # Difference > `time_window` (e.g. 30 days) from last record: 
      #    completely new episode
      l <- i
      epi_start[l] <- TRUE
    }
    else if(sum(epi_cand$diff[(l+1):i]) >= time_window){
      # Cumulative difference > `time_window` from last episode:
      #    continuation
      l <- i
      epi_start[l] <- TRUE
      epi_cont[l] <- TRUE
    }
  }
}

epi_cand[, cont := epi_cont]
epi <- epi_cand[(epi_start)]
epi[, c("end", "diff") := NULL]


attrition$`4_epi` <- num_in_study(epi[, .(patid, evnt_date = start)])
attrition$`4_epi_l` <- num_in_study(epi[type == "lower", .(patid, evnt_date = start)])
attrition$`4_epi_u` <- num_in_study(epi[type == "upper", .(patid, evnt_date = start)])

attrition$`5_excl_su` <- num_in_study(epi[type == "upper" & set == "sec", .(patid, evnt_date = start)])
attrition$`5_excl_sl` <- num_in_study(epi[type == "lower" & set == "sec", .(patid, evnt_date = start)])
attrition$`5_excl_pu` <- num_in_study(epi[type == "upper" & set == "prim", .(patid, evnt_date = start)])

attrition$`5_excl_cont` <- num_in_study(epi[type == "lower" & set == "prim" & cont == TRUE, .(patid, evnt_date = start)])


# Identify recurrent episodes ---------------------------------------------

# a) explicitly coded
rec_read <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                       sheet = "Recurrent - CPRD")
setDT(rec_read)
rec_read[(medical() %>% collect_dt()), on = "readcode", medcode := medcode]


recur_diags <- uti_prim[rec_read, on = "medcode", nomatch = 0]
recur_diags %<>% .[, .(patid, eventdate)]

# Also shift it back by one day to be picked up if the episode itself was
# marked as recurrent UTI
recur_diags %<>% rbind(recur_diags[, .(patid, eventdate = eventdate %m-% days(1))])


# b) identified through frequency of episodes
if(time_window == 60){
  # Replicate the study criteria for recurrent UTt if the time window 
  # matched the time window in Gharbi et al.
  
  recur_freq <- epi[, .(start, 
                        recurr = if_else(start - shift(start) < 365.25, 
                                         "yes", "no", "no")), 
                    by = patid]
} else {
  # Use the more commonly used criteria of 2 in 6 months or 3 in 12 months
  # if a different time window was chosen (most notably 30 days)
  
  recur_freq <- epi[, .(start, 
                        recurr = if_else(start - shift(start) < 365.25 / 2 | 
                                           start - shift(start, n = 2) < 365.25, 
                                         "yes", "no", "no")), 
                    by = patid]
}

recur_freq %<>% .[recurr == "yes", .(patid, eventdate = start)]

# c) Nitrofurantoin or trimethoprim at least 28 days
nt <- abx[substance %in% c("nitrofurantoin", "trimethoprim")]

nt[ndd != 0, duration := qty / ndd]  # for every presc with NDD info
nt[, long_term := duration >= 28]

nt %<>% .[long_term == TRUE, .(patid, eventdate = prescdate, duration)]



# Combine a), b) and c) and add the earliest recurrent UTI to epi
recur <- rbind(recur_diags, recur_freq, nt[, !("duration")])
recur[, window := eventdate %m+% years(1)]

epi[, recur := FALSE]
epi[recur, on = .(patid, start > eventdate, start <= window), recur := TRUE]
epi[, recur := factor(recur, c(FALSE, TRUE), c("no", "yes"))]

# Also remember whether there the immediately preceeding UTI episode was
# less than one year ago
epi[, uti_365 := if_else(start - shift(start) < 365.25, TRUE, FALSE, FALSE),
    by = patid]
epi[, uti_365 := factor(uti_365, c(FALSE, TRUE), c("no", "yes"))]


# Apply further exclusion criteria ----------------------------------------

# Limit to non-remission episdoes
epi %<>% .[cont == FALSE]
epi[, c("cont") := NULL]

# Number each episode by patient
epi[, nmr := 1:.N, by = patid]

# Primary care non-remission lower UTI only
epi %<>% .[type == "lower" & set == "prim"]
epi[, c("type", "set") := NULL]

attrition$`5_excl` <- attrition$`4_epi` - num_in_study(epi[, .(patid, evnt_date = start)])
attrition$`6_epi_plnew` <- num_in_study(epi[, .(patid, evnt_date = start)])

# Only within a patients observation time (i.e. after 65, before transfer)
epi <- 
  epi[(patients[, .(patid, enter_date, leave_date, death_date)]), 
      on = "patid", nomatch = 0] %>%
    .[start %between% list(enter_date, leave_date) &
      (start < leave_date %m-% days(time_window) | 
       death_date == leave_date)] %>% 
    .[, c("enter_date", "leave_date", "death_date") := NULL] %>% 
    .[]

# Patient did not die on the same day (all-cause)
epi %<>% .[!patients, on = .(patid, start = death_date)]

attrition$`7_excl_die` <- num_in_study(epi[, .(patid, evnt_date = start)])

# Patient not in hospital at the same time (all-cause)
epi %<>% .[!admission, on = .(patid, start >= admidate, start <= discharged)]

attrition$`7_excl_adm` <- num_in_study(epi[, .(patid, evnt_date = start)])

# Attendance of A&E on the same day (all-cause)
epi %<>% .[!ae, on = .(patid, start = arrivaldate)]

attrition$`7_excl_ae` <- num_in_study(epi[, .(patid, evnt_date = start)])

# Referral to other care facility on the same day
epi %<>% .[!source == "referral"]
epi[, source := NULL]

attrition$`7_excl_ref` <- num_in_study(epi[, .(patid, evnt_date = start)])


# Adjust attrition estimates
attrition$`7_excl_ref` <- attrition$`7_excl_ae` - attrition$`7_excl_ref`
attrition$`7_excl_ae` <- attrition$`7_excl_adm` - attrition$`7_excl_ae`
attrition$`7_excl_adm` <- attrition$`7_excl_die` - attrition$`7_excl_adm`
attrition$`7_excl_die` <- attrition$`6_epi_plnew` - attrition$`7_excl_die`

attrition$`7_excl` <- attrition$`6_epi_plnew` - num_in_study(epi[, .(patid, evnt_date = start)])
attrition$`8_incl` <- num_in_study(epi[, .(patid, evnt_date = start)])


# Combine sepsis records --------------------------------------------------

# Combine them, keeping unique records per day
sep <- rbind(
  sep_prim[, .(patid, sepdate, code = readcode,  
               eventid = consid, set = "prim")],
  sep_sec[, .(patid, sepdate, code = icd,  
              eventid = spno, set = "sec")]
)




# Exposure: Define who got immediate antibiotics --------------------------

epi[, after_7 := start %m+% days(7)]

# Classify patients by treatment received
epi[abx, on = .(patid, start < prescdate, after_7 >= prescdate),
    presc := "deferred"]

epi[abx, on = .(patid, start = prescdate),
    presc := "immediate"]

epi[is.na(presc), presc := "no"]


# Too many no, hardly any deferred. Non-systemic?
# Make the classification binary, i.e. switch deferred to no's
epi[presc == "deferred", presc := "no"]
epi[presc == "immediate", presc := "yes"]
epi[, presc := factor(presc, c("yes", "no"))] # yes is baseline


attrition$`9_presc_yes` <- sum(epi$presc == "yes")
attrition$`9_presc_no` <- sum(epi$presc == "no")



# Outcome: Identify who got sepsis within 30/60 days ----------------------

epi[, after := start %m+% days(time_window)]
epi[, sep := FALSE]
epi[sep, on = .(patid, start < sepdate, after >= sepdate), 
    sep := TRUE]

epi[, sep := factor(sep, c(FALSE, TRUE), c("no", "yes"))]

attrition$`10_sep_yes_abx` <- nrow(epi[presc == "yes" & sep == "yes"])
attrition$`10_sep_no_abx` <- nrow(epi[presc == "yes" & sep == "no"])
attrition$`10_sep_yes_nabx` <- nrow(epi[presc == "no" & sep == "yes"])
attrition$`10_sep_no_nabx` <- nrow(epi[presc == "no" & sep == "no"])


# Covariates: define values for each episode ------------------------------

# Age group
age_lvl <- c("65-74" = 75, "75-84" = 85, "85+" = Inf)
epi[patients, on = "patid", 
    age := time_length(birth_date %--% start, u = "y")]
epi[, age_cat := cut(age, right = FALSE, 
                     breaks = c(65, age_lvl), 
                     labels = names(age_lvl))]

# Gender
epi[patients, on = "patid", female := female]
epi[, female := factor(female, 0:1, c("no", "yes"))]

# IMD
epi[patients, on = "patid", imd := imd]
epi[, imd := factor(imd, 1:5, str_c("Q", 1:5))]

# Practice
epi[patients, on = "patid", pracid := pracid]


# Region
epi[region, on = "pracid", region := factor(region, 
                                            c("South of England", 
                                              "London", 
                                              "Midlands and east of England",
                                              "North of England and Yorkshire"))]


# Year
epi[, year := if_else(month(start) < 4, year(start) - 1L, year(start))]
epi[, year := factor(year, c(2010, 2007:2009, 2011:2014))]

# Charlson Comorbidity Index
epi_cci <- epi[, .(patid, start, epidate = start)]
epi_cci %<>% .[cci, on = .(patid, epidate >= eventdate), nomatch = 0]
epi_cci %<>% .[, .(score = sum(score)), by = .(patid, start)]

epi[, cci := 0]
epi[epi_cci, on = .(patid, start), cci := score]

remove(epi_cci)

# Smoking status
epi_smoke <- epi[, .(patid, start, epidate = start)]
epi_smoke %<>% .[smoke, on = .(patid, epidate >= eventdate), nomatch = 0]
setorder(epi_smoke, patid, start, -epidate)
epi_smoke %<>% .[, .SD[1], by = .(patid, start)]

epi[, smoke := "non"]
epi[epi_smoke, on = .(patid, start), smoke := status]
epi[, smoke := factor(smoke, c("non", "ex", "smoke"))]

remove(epi_smoke)



# Admission within 7 days prior
epi[, before_7 := start %m-% days(7)]
epi[, hosp_7 := FALSE]
epi[admission, on = .(patid, start > discharged, before_7 <= discharged),
    hosp_7 := TRUE]
epi[, hosp_7 := factor(hosp_7, c(FALSE, TRUE), c("no", "yes"))]


# Admission within 30 days prior
epi[, before_30 := start %m-% days(30)]
epi[, hosp_30 := FALSE]
epi[admission, on = .(patid, start > discharged, before_30 <= discharged),
    hosp_30 := TRUE]
epi[, hosp_30 := factor(hosp_30, c(FALSE, TRUE), c("no", "yes"))]


deduplicate_admission <- function(dt){
  # Some of the admissions in HES are nested within bigger spells,
  # or are overlapping. This function removes episodes that are  
  # completely nested in a bigger spell and shortens that overlap
  #
  # Args:
  #   dt - data.table with hospital admissions
  #
  # Result:
  #   a deduplicated hospital admission data.table
  
  dt <- copy(dt)
  dt[, id := 1:nrow(dt)]
  
  nested <- 
    dt[dt, .(patid, admidate, discharged, id = i.id), nomatch = 0,
       on = .(patid, admidate   <= admidate, 
              discharged >  discharged)]
  
  overlap <-
    dt[dt, .(patid, overlap = discharged, id), nomatch = 0,
       on = .(patid, admidate   < admidate, 
              discharged > admidate, 
              discharged <= discharged)]
  
  dt[overlap, on = .(id), discharged := overlap]
  dt[!nested, on = .(id), !("id")]
}

admission %<>% deduplicate_admission()
admission[, nights := time_length(admidate %--% discharged, u = "d")]

# Number of days in hospital in the previous year
epi[, before_365 := start %m-% days(365)]
epi_nights <- epi[, .(patid, start, before_365, 
                      wdw_start = before_365, wdw_end = start %m-% days(1))]

epi_nights %<>% 
  .[admission, on = .(patid, wdw_start <= discharged, 
                             wdw_end >= discharged), nom = 0] %>% 
  .[, nights := 
      nights - pmax(0, time_length(admidate %--% before_365, u = "d"))] %>% 
  .[, .(nights = sum(nights)), by = .(patid, start)]
epi[, hosp_nights := 0]
epi[epi_nights, on = .(patid, start), hosp_nights := nights]

remove(epi_nights)


# Number of emergency hospitalisations in the previous year
epi_hospn <- epi[, .(patid, start, before_365, 
                     wdw_start = before_365, wdw_end = start %m-% days(1))]

epi_hospn %<>% 
  .[admission, on = .(patid, wdw_start <= discharged, 
                             wdw_end >= discharged), nom = 0] %>% 
  .[source == "Emergency"] %>% 
  .[, .(n = .N), by = .(patid, start)]
epi[, hosp_n := 0L]
epi[epi_hospn, on = .(patid, start), hosp_n := n]

remove(epi_hospn)



# A&E attendance within 30 days prior
epi[, ae_30 := FALSE]
epi[ae, on = .(patid, start > arrivaldate, before_30 <= arrivaldate),
    ae_30 := TRUE]
epi[, ae_30 := factor(ae_30, c(FALSE, TRUE), c("no", "yes"))]

# Number of A&E attendances in the previous year
epi_ae <- epi[, .(patid, start, before_365, 
                  wdw_start = before_365, wdw_end = start %m-% days(1))]

epi_ae %<>% 
  .[ae, on = .(patid, wdw_start <= arrivaldate, 
                      wdw_end >= arrivaldate), nom = 0] %>% 
  .[, .(n = .N), by = .(patid, start)]
epi[, ae_n := 0L]
epi[epi_ae, on = .(patid, start), ae_n := n]

remove(epi_ae)


# Antibiotics in primary care within 30 days prior
epi[, abx_7 := FALSE]
epi[abx, on = .(patid, start > prescdate, before_7 <= prescdate), 
    abx_7 := TRUE]
epi[, abx_7 := factor(abx_7, c(FALSE, TRUE), c("no", "yes"))]

epi[, abx_30 := FALSE]
epi[abx, on = .(patid, start > prescdate, before_30 <= prescdate), 
    abx_30 := TRUE]
epi[, abx_30 := factor(abx_30, c(FALSE, TRUE), c("no", "yes"))]


# Type of index consultation
c_types <- read_excel(file.path(subfolder, "cons_types.xlsx"), 
                      sheet = "Consultation types")
setDT(c_types)

epi[cons, on = .(eventid = consid), constype := constype]
epi[c_types, on = "constype", home := homevisit]

epi[, home := factor(home, c("no", "yes"))]

# Prophylactic treatment in the previous month
nt[, until := eventdate %m+% days(as.integer(duration))]

epi[, prophy := FALSE]
epi[nt, on = .(patid, start > eventdate, before_30 <= eventdate),
    prophy := TRUE]
epi[nt, on = .(patid, start > until, before_30 <= until),
    prophy := TRUE]

epi[, prophy := factor(prophy, c(FALSE, TRUE), c("no", "yes"))]


# Save dataset ------------------------------------------------------------

name <- str_c("epi_", time_window)

if(sens_exact_codes)
  name <- str_c("sens_", name)

save_derived(epi, name, compress = "gz")

if(!sens_exact_codes)
  save_derived(attrition, str_c("attrition_", time_window), compress = "gz")

