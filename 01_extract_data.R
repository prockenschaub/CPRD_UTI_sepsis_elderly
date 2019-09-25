###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Re-analysis of Gharbi et al. (2019) UTI paper
#
# File:     01_extract_data.R
# Date:     08/03/2019
# Task:     Extract all necessary records from the database and store them
#           in local R files for reproducability
#
###########################################################################


# Path from project directory to this file
# NOTE: must be set in each programme separately
subfolder <- "00_replication/gharbi_2019"

# Initialise the workspace
source(file.path(subfolder, "00_init.R"))


attrition <- list() # Define a variable to hold the attrition counts


# Define eligible patients ------------------------------------------------

def_eligible <-
  study_population_db(link = TRUE) %>%  # all linked and acceptable pats
  in_date(study_start, study_end) %>% 
  left_join(imd_db(), by = c("patid", "pracid")) %>% 
  filter(
    !is.na(birth_date), 
    !is.na(female), 
    !is.na(imd)
  )

eligible <- collect_dt(def_eligible, convert = TRUE)
eligible[, turn_65 := birth_date %m+% years(65)]

attrition$`1_eligible` <- nrow(eligible)

# Patients enter the cohort when they turn 65
patients <- eligible[turn_65 < leave_date]
patients[, c("start_date", "end_date") := NULL]
patients[, enter_date := pmax(enter_date, turn_65)]

def_patients <- copy_to(get_db_conn(), patients, name = "patients")

attrition$`2_65_years` <- nrow(patients)
attrition$`2_65_years_fu` <- 
  patients[, sum(time_length(enter_date %--% leave_date, u = "y"))]


# Select all urinary tract events in primary care -------------------------

med_dict <- medical() %>% collect_dt()

# Lower UTI read codes (from Gharbi et al. supplement)
luti_read <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                        sheet = "Lower UTI - CPRD")
setDT(luti_read)
luti_read[, type := "lower"]

# Upper UTI read codes (from Gharbi et al. supplement)
uuti_read <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                        sheet = "Upper UTI - CPRD")
setDT(uuti_read)
uuti_read[, type := "upper"]

# Combine the lists
uti_read <- rbind(luti_read, uuti_read)
uti_read[med_dict, on = "readcode", medcode := medcode] # add medcode


# Select all UTI records in the clinical, test and referral table
uti_prim <- 
  list(clinical_db(), test_db(), referral_db()) %>% 
  map(records_db, code_dt = uti_read) %>% 
  map(~ select(., patid, eventdate, medcode, consid)) %>% 
  map_df(collect_dt, convert = TRUE, .id = "source")


# Limit the records to patients over 65
uti_prim %<>% .[(patients[, "patid"]), on = "patid", nomatch = 0]
uti_prim[uti_read, on = "medcode", type := type]

# Define the source 
prim_srcs <- c("1" = "clinical", "2" = "test", "3" = "referral")
uti_prim[, source := prim_srcs[source]]




# As sensitivity analysis, also look for dipstick, culture, etc. ----------
utest_read <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                        sheet = "Urine test - CPRD")
setDT(utest_read)
utest_read[, type := "test"]
utest_read[med_dict, on = "readcode", medcode := medcode] # add medcode

utest_prim <- 
  records_db(test_db(), code_dt = utest_read) %>% 
  select(., patid, eventdate, medcode, consid, enttype, data1) %>% 
  collect_dt(convert = TRUE)


# Limit the records to patients over 65
utest_prim %<>% .[(patients[, "patid"]), on = "patid", nomatch = 0]
utest_prim[utest_read, on = "medcode", type := type]

# Define the source 
utest_prim[, source := "test"]





# Select all urinary tract events in secondary care  ----------------------

# Lower UTI read codes (from Gharbi et al. supplement)
luti_icd <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                        sheet = "Lower UTI - HES")
setDT(luti_icd)
luti_icd[, type := "lower"]

# Upper UTI read codes (from Gharbi et al. supplement)
uuti_icd <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                       sheet = "Upper UTI - HES")
setDT(uuti_icd)
uuti_icd[, type := "upper"]

# Combine the lists
uti_icd <- rbind(luti_icd, uuti_icd)


# Get all hospital diags out of the database due to collation issues
def_hosp_diag <- 
  hosp_diag_db() %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, spno, icd, d_order)

hosp_diag <- collect_dt(def_hosp_diag) 

# Select only urinary tract infections
uti_sec <- hosp_diag[uti_icd, on = "icd", nomatch = 0]

# Add admission and discharge details
def_admission <-
  admissions_db() %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, spno, admidate, discharged, source = admitype)

admission <- collect_dt(def_admission, convert = TRUE)

uti_sec %<>% .[admission, on = .(patid, spno), nomatch = 0]  
uti_sec[source != "Emergency", source := "Other"]



# SENSITIVITY analysis:
# Some of the ICD UTI codes from hospital used above differ from those
# reported by Gharbi et al. Also extract the exact list used by Gharbi 
# to judge differences.
uti_icd_gha <- read_excel(file.path(subfolder, "uti_sec_gharbi.xlsx"), 
                          sheet = "Upper UTI - HES")
setDT(uti_icd_gha)
uti_icd_gha[, type := "upper"]

uti_sec_gha <- hosp_diag[uti_icd_gha, on = "icd", nomatch = 0]
uti_sec_gha %<>% .[admission, on = .(patid, spno), nomatch = 0]  
uti_sec_gha[source != "Emergency", source := "Other"]



# Extract A&E data --------------------------------------------------------

def_ae <-
  ae_db() %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, arrivaldate)

ae <- collect_dt(def_ae, convert = TRUE)
ae <- unique(ae)



# Get all consultations for the included patients -------------------------

def_consultations <-
  tbl(get_db_conn(), "consultation") %>% 
  semi_join(def_patients, by = "patid") %>% 
  in_date(study_start, study_end) %>% 
  select(patid, consid, eventdate, constype)

cons <- collect_dt(def_consultations, convert = TRUE)



# Get all prescriptions for patients in the study period ------------------

# Define the included and excluded BNF chapters
bnf_systemic <- "0501"
bnf_excl <- c("050109", "050110")

# Get all prescription data for the above defined study period  
def_abx <-
  abx_bnf_db(bnf_systemic, bnf_excl) %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, prescdate = eventdate, prodcode, consid, qty, ndd, issueseq) %>% 
  arrange(patid, eventdate, issueseq)

abx <- collect_dt(def_abx, convert = TRUE)

# Some drugs are classified in 5.1., but are crossover-products
# Exclude those
abx_info <- antibiotics() %>% collect_dt()

atc_a02b <- abx_info[atcchapter == "A02B"]
abx %<>% .[!atc_a02b, on = "prodcode"]
remove(atc_a02b)


abx %<>% .[!(abx_info[group %in% c("Antifungal", "Antileprotic", 
                                   "Antituberculosis", "No antibiotic")]),
           on = "prodcode"]

# Add the antibiotic name
abx_info <- collect_dt(antibiotics())
abx_info[, abx_substance := str_to_lower(substance)]
abx[abx_info, on = "prodcode", substance := abx_substance]



# Identify sepsis in both primary and secondary care ----------------------

# Read codes
sep_read <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                       sheet = "Sepsis - CPRD")
setDT(sep_read)

sep_read[med_dict, on = "readcode", medcode := medcode]

# ICD-10 codes
sep_icd <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                      sheet = "Sepsis - HES")
setDT(sep_icd)


# Collect all records in the database
# a) primary care
sep_prim <- 
  records_db(clinical_db(), sep_read) %>% 
  select(patid, sepdate = eventdate, medcode, consid) %>% 
  collect_dt(convert = TRUE)
sep_prim[sep_read, on = "medcode", readcode := readcode]

# b) secondary care
sep_sec <- 
  hosp_diag[(sep_icd[, "icd"]), on = "icd", nomatch = 0] %>% 
  unique() %>% 
  .[admission, on = .(patid, spno), nomatch = 0]
sep_sec[, sepdate := admidate]





# Region ------------------------------------------------------------------

# Get all practices from the database
def_region <- 
  all_patients_db() %>% 
  select(pracid, prac_region) %>% 
  distinct()

region <- collect_dt(def_region)
setorder(region, pracid)

# Get the region lookup file
reg_lu <- fread(file.path(lu_dir, "TXTFILES", "PRG.txt"), 
                col.names = c("prac_region", "region"), skip = 1)

region[reg_lu, on = "prac_region", region := region]
region[, prac_region := NULL]

# Reclass the regions
region[region %in% c("North West", "North East", "Yorkshire & The Humber"),
       region := "North of England and Yorkshire"]
region[region %in% c("West Midlands", "East Midlands", "East of England"),
       region := "Midlands and east of England"]
region[region %in% c("South East Coast", "South Central", "South West"),
       region := "South of England"]





# Charlson Comorbidity Index ----------------------------------------------

# Get al primary care codes
cci_read <- read_excel(file.path(subfolder, "cci_read.xlsx"))
setDT(cci_read)
cci_read[med_dict, on = "readcode", medcode := medcode]

# Get all primary care records from the clinical table
cci <- records_db(clinical_db(), cci_read) %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(patid, eventdate, medcode) %>% 
  collect_dt(convert = TRUE)
cci[cci_read, on = "medcode", class := str_to_lower(class)]

# Take the earliest of each class for each patient
setorder(cci, patid, class, eventdate)
cci %<>% .[, .SD[1], by = .(patid, class)]


# Classify them according to CCI rules
cci_rules <- dtribble(
  ~ class, ~ score, 
  'cancer', 2,
  'cerebrovascular dis', 1,
  'chronic pulmonary dis', 1,
  'cong heart failure', 1,
  'connective tiss disord', 1,
  'dementia', 1,
  'diabetes', 1,
  'diabetes with complic', 2,
  'hemiplegia/paraplegia', 2,
  'hiv/aids', 6,
  'metastatic cancer', 6,
  'mild liver dis', 1,
  'mod/severe liver dis', 3,
  'mod/severe renal dis', 2,
  'myocardial infarction', 1,
  'peptic ulcer dis', 1,
  'peripheral vascular dis', 1
)

cci[cci_rules, on = "class", score := score]






# Obtain smoking status ----------------------------------------------------
## @knitr smoke
#+ smoke, include = FALSE

smoke <- 
  smoke_db() %>% 
  semi_join(def_patients, by = "patid") %>% 
  select(-adid, -data2, -data3, -num_rows) %>% 
  collect_dt(convert = TRUE)

codes_smoke <- codes_smoke_db() %>% collect_dt()

# Use CALIBER smoking algorithm 
# (www.caliberresearch.org/portal/show/smoking_status_gprd)
smoke[codes_smoke, on = "medcode", status := smoke]
smoke[enttype == 4, 
      status := case_when(data1 == "1" & status == "non" ~ "non & smoke",
                          data1 == "1" & status == "ex"  ~ "ex & smoke",
                          data1 == "1" ~ "smoke",
                          data1 == "2" & status == "smoke" ~ "non & smoke",
                          data1 == "2" & status == "ex" ~ "ex",
                          data1 == "2" ~ "non",
                          data1 == "3" & status == "smoke" ~ "ex & smoke",
                          data1 == "3" ~ "ex",
                          TRUE ~ status)]

# Change the ambivalent codes in favour of non-smoking (this is justified,
# as a) it is more conservative and b) there are very general codes that
# are currently counted as smoking, but those could easily just be used 
# with indicating a smoking status (e.g. 137..00 Tobacco consumption)
smoke[status == "non & smoke", status := "non"]
smoke[status == "ex & smoke", status := "ex"]

smoke[, c("enttype", "data1") := NULL]






# Save all data -----------------------------------------------------------

mget(char(patients, region, uti_prim, utest_prim, uti_sec, uti_sec_gha,  
          sep_prim, sep_sec, cci, smoke, abx, cons, hosp_diag, admission, ae, 
          attrition)) %>% 
  walk2(., names(.), save_derived, compress = "gz")
