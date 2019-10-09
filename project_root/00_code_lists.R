#--------------------------------------------------------------------------
#
# Program: 00_code_lists.R
# Author:  Patrick Rockenschaub
# Date:    13/03/2018
#
# Purpose: Compile standardised code lists that can be easily exchanged
#          in the code
#
#--------------------------------------------------------------------------



# QOF comorbidities -------------------------------------------------------

qof <- function(){
  list <- 
    file.path(lu_dir, "PASS", "comorb_codes_qof_used.xlsx") %>% 
    read_excel(sheet = 1) %>% 
    as.data.table()
  
  list
}


# PASS codelists ----------------------------------------------------------

pass <- function(){
  list <- 
    file.path(lu_dir, "PASS", "180403_PASS_codes_dolk_reclass.xlsx") %>% 
    read_excel(sheet = 1) %>% 
    as.data.table()
  
  # Label systems
  system_lbls <-
    dtribble(
      ~system, ~system_desc        ,
      #------|---------------------#
      "Gastro-intestinal system"    , "gastrointestinal"  ,
      "Ear"                         , "rt/ent"            ,
      "Lower respiratory tract"     , "rt/ent"            ,
      "Upper respiratory tract"     , "rt/ent"            ,
      "Urinary tract"               , "urogenital tract"  ,
      "Genital system"              , "urogenital tract"  ,
      "Urinary / genital distinction not possible" 
      , "urogenital tract"  ,
      "Skin"                        , "skin and wounds"   ,
      "Wounds and surgery"          , "skin and wounds"   ,
      "Cardiovascular system"       , "cardiovascular"    ,
      "Central nervous system"      , "cns"               ,
      "Musculoskeletal system"      , "musculoskeletal"   ,
      "Dental / mouth"              , "dental/mouth"      ,
      "Eye"                         , "eye"               ,
      "Miscellaneous"               , "misc"            
    )
  
  list <- 
    list[system_lbls, on = .(system)]
  
  # Label conditions
  list[, "orig_cond" := condition]
  
  list[system == "Ear", "condition" := "ear-related"]
  list[system == "Lower respiratory tract" & type == "Diagnoses", "condition" := "lower rti"]
  list[system == "Upper respiratory tract" & type == "Diagnoses", "condition" := "upper rti"]
  list[orig_cond == "Cough", "condition" := "cough"]
  list[orig_cond == "Pharyngitis / tonsillitis / sore throat", 
       "condition" := "sore throat"]
  list[system %in% c("Ear", "Lower respiratory tract", "Upper respiratory tract") & 
         !(condition %in% c("ear-related", "lower rti", "upper rti", "cough", "sore throat")),
       "condition" := "other rti"]
  
  list[system == "Urinary tract", "condition" := "urinary tract"]
  list[system == "Genital system", "condition" := "genital tract"]
  list[system == "Urinary / genital distinction not possible", 
       "condition" := "unspecific urogenital"]
  
  list[system == "Skin", "condition" := "other skin"]
  list[system == "Wounds and surgery", "condition" := "wounds"]
  list[orig_cond == "Boil / cyst / abscess", "condition" := "boil, cyst, abscess"]
  list[orig_cond == "Cellulitis", "condition" := "cellulitis"]
  list[orig_cond == "Acne", "condition" := "acne"]
  list[orig_cond == "Ingrowing / infected nail", "condition" := "ingrown/infected nail"]
  list[orig_cond == "Bites", "condition" := "bites"]
  list[orig_cond == "Other / unspecific skin infection", "condition" := "unspecific"]
  
  list[is.na(type), "condition" := system_desc]
  list[system_desc == "misc", "condition" := "misc. codes"]
  
  
  unique(list[, .(readcode, description, system_desc, condition, infectious)])
  
}

pass_for_dolk <- function(){
  # Return the PASS code list adopted for replication of the Dolk et al. 
  # 2018 paper.
  
  list <- 
    file.path(lu_dir, "PASS", "180403_PASS_codes_dolk_reclass.xlsx") %>% 
    read_excel(sheet = 1) %>% 
    as.data.table()
  
  # Label systems
  system_lbls <-
    dtribble(
      ~system, ~system_desc        ,
      #------|---------------------#
      "Gastro-intestinal system"    , "gastrointestinal"  ,
      "Ear"                         , "rt/ent"            ,
      "Lower respiratory tract"     , "rt/ent"            ,
      "Upper respiratory tract"     , "rt/ent"            ,
      "Urinary tract"               , "urogenital tract"  ,
      "Genital system"              , "urogenital tract"  ,
      "Urinary / genital distinction not possible" 
                                    , "urogenital tract"  ,
      "Skin"                        , "skin and wounds"   ,
      "Wounds and surgery"          , "skin and wounds"   ,
      "Cardiovascular system"       , "cardiovascular"    ,
      "Central nervous system"      , "cns"               ,
      "Musculoskeletal system"      , "musculoskeletal"   ,
      "Dental / mouth"              , "dental/mouth"      ,
      "Eye"                         , "eye"               ,
      "Miscellaneous"               , "misc"            
    )
  
  list <- 
    list[system_lbls, on = .(system)]
  
  
  # Label conditions
  list[, "orig_cond" := condition]
  
  list[system == "Ear", "condition" := "ear-related"]
  list[system == "Lower respiratory tract" & type == "Diagnoses", "condition" := "lower rti"]
  list[system == "Upper respiratory tract" & type == "Diagnoses", "condition" := "upper rti"]
  list[orig_cond == "Cough", "condition" := "cough"]
  list[orig_cond == "Pharyngitis / tonsillitis / sore throat", 
       "condition" := "sore throat"]
  list[system %in% c("Ear", "Lower respiratory tract", "Upper respiratory tract") & 
       !(condition %in% c("ear-related", "lower rti", "upper rti", "cough", "sore throat")),
       "condition" := "other rti"]
  
  list[system == "Urinary tract", "condition" := "urinary tract"]
  list[system == "Genital system", "condition" := "genital tract"]
  list[system == "Urinary / genital distinction not possible", 
       "condition" := "unspecific urogenital"]
  
  list[system == "Skin", "condition" := "other skin"]
  list[system == "Wounds and surgery", "condition" := "wounds"]
  list[orig_cond == "Boil / cyst / abscess", "condition" := "boil, cyst, abscess"]
  list[orig_cond == "Cellulitis", "condition" := "cellulitis"]
  list[orig_cond == "Acne", "condition" := "acne"]
  list[orig_cond == "Ingrowing / infected nail", "condition" := "ingrown/infected nail"]
  list[orig_cond == "Bites", "condition" := "bites"]
  list[orig_cond == "Other / unspecific skin infection", "condition" := "unspecific"]
  
  list[is.na(type), "condition" := system_desc]
  list[system_desc == "misc", "condition" := "misc. codes"]
  
  
  unique(list[, .(readcode, description, system_desc, condition, type, infectious)])
}





# Hawker et al. 2014 ------------------------------------------------------

hawker_2014 <- function(){
  suppressMessages({
    list <- 
      read_excel(file.path(lu_dir, "PASS", "hawker_2014.xlsx")) %>% 
      as.data.table()
  })
  
  setnames(list, "syndrome", "condition")
  
  list[condition %in% c("Cold", "URTI", "Sore throat", "Otitis media"),
       system_desc := "rt/ent"]
  list[condition == "UTI", system_desc := "urogenital"]
  
  unique(list[, .(readcode, description, system_desc, condition)])
}





# Dolk et al. 2018 --------------------------------------------------------

dolk_2018_system_class <- dtribble(
  ~system, ~system_desc        ,
  #------|---------------------#
  "1"    , "gastrointestinal"  ,
  "2"    , "rt/ent"            ,
  "3"    , "urogenital tract"  ,
  "4"    , "skin and wounds"   ,
  "5"    , "cardiovascular"    ,
  "6"    , "cns"               ,
  "7"    , "musculoskeletal"   ,
  "8"    , "dental/mouth"      ,
  "9"    , "eye"               ,
  "11"   , "misc"              ,
  "12"   , "misc"              ,  
  "X"    , "misc"            
)

# Label conditions
dolk_2018_conditions_class <-dtribble(
    ~code     , ~condition             ,
    #----------------------------------#
    "1"       , "gastrointestinal"     ,
    
    "2.1.0"   , "other rti"            ,
    "2.1.1"   , "cough"                ,
    "2.1.2"   , "other rti"            ,
    "2.1.3"   , "other rti"            ,
    "2.1.4"   , "other rti"            ,
    "2.1.5"   , "other rti"            ,
    "2.1.6"   , "ear-related"          ,
    "2.1.7"   , "other rti"            ,
    "2.1.8"   , "other rti"            ,
    "2.1.9"   , "other rti"            ,
    "2.2.1"   , "other rti"            ,
    "2.2.2"   , "lower rti"            ,
    "2.2.3.1" , "upper rti"            ,
    "2.2.3.2" , "upper rti"            ,
    "2.2.3.3" , "upper rti"            ,
    "2.2.3.4" , "upper rti"            ,
    "2.2.3.5" , "upper rti"            ,
    "2.2.3.6" , "sore throat"          ,
    "2.2.3.7" , "upper rti"            ,
    "2.2.3.8" , "upper rti"            ,
    "2.2.4"   , "ear-related"          ,
    
    "3.1"     , "unspecific urogenital",
    "3.2"     , "urinary tract"        ,
    "3.3"     , "genital tract"        ,
    
    "4.1.1"   , "other skin"           ,
    "4.1.2.0" , "unspecific"           ,
    "4.1.2.1.", "other skin"           ,
    "4.1.2.2" , "other skin"           ,
    "4.1.2.3" , "cellulitis"           ,
    "4.1.2.4" , "bites"                ,
    "4.1.2.5" , "other skin"           ,
    "4.1.2.6" , "boil, cyst, abscess"  ,
    "4.1.2.7" , "other skin"           ,
    "4.1.2.8" , "other skin"           ,
    "4.1.2.9" , "other skin"           ,
    "4.1.2.10", "ingrown/infected nail",
    "4.1.2.11", "acne"                 ,
    "4.2"     , "wounds"               ,
    
    "5"       , "cardiovascular"       ,
    
    "6"       , "cns"                  ,
    
    "7"       , "musculoskeletal"      ,
    
    "8"       , "dental / mouth"       ,
    
    "9"       , "eye"                  ,
    
    "11"      , "cancer"               ,
    
    "12"      , "prophylactic"         ,
    
    "X"       , "misc. codes"                
  )

dolk_2018 <- function(system_lbls = dolk_2018_system_class,
                      condition_lbls = dolk_2018_conditions_class){
  # Create a classification of reasons for antibiotic prescribing based on 
  # the code list used in Dolk et al. (2018)
  #
  # Args:
  #   system_lbls - data.table that defines systems based on the first digit 
  #                 of the chapter number in the codelist (default given)
  #   condition_lbls - data.table that defines the specific condition based
  #                    on the full (default given)
  #
  # Result:
  #   a data.table with readcode, description, body system and condition
  
  # Read in the code list
  suppressMessages({
    list <- 
      read_tsv(file.path(lu_dir, "PASS", "Dolk_infect_code_lists_clean.txt"), 
               col_names = c("readcode", "description")) %>% 
      as.data.table()
  })
  
  # Propagate the headings to each underlying code
  list[readcode == "XXXXXX", classification := .(description)]
  list[, classification := zoo::na.locf(classification)]
  list[, system := str_sub(classification, end = 2L)]
  list[, system := str_replace(system, "[. ]", "")]
  
  # Get rid of the lines containing only headings
  list %<>% .[readcode != "XXXXXX"]
  
  # Label systems
  list %<>% .[system_lbls, on = .(system)]
  
  condition_lbls <- copy(condition_lbls)
  condition_lbls[, "system" := str_sub(code, end = 2L)]
  condition_lbls[, "system" := str_replace(system, "[. ]", "")]
  condition_lbls[, "code" := str_replace_all(code, "\\.", "\\\\.")]
  
  list <-
    merge(list, condition_lbls, by = "system", allow.cartesian = TRUE) %>% 
    .[str_detect(classification, paste0("^", code))]
  
  unique(list[, .(readcode, description, system_desc, condition)])
}



dolk_symptoms_2018 <- function(){
  # Read in the code list
  suppressMessages({
    list <- 
      read_tsv(file.path(lu_dir, "PASS", "Dolk_infect_code_lists_clean.txt"), 
               col_names = c("readcode", "description")) %>% 
      as.data.table()
  })
  
  # Find all symptom codes
  list[str_detect(description, regex("^([0-9X]\\.){0,2}[0-9] Symptoms")),
       type := "symptom"]
  
  list[str_detect(description, 
          regex("Diagnoses( \\(misc\\.\\))?$|4.2 Wounds and surgery|X.2 Infections")),
       type := "diagnosis"]
  
  # Propagate the code type to each underlying code
  list[, "type" := c(NA, zoo::na.locf(type))]
  
  # Get rid of the lines containing only headings
  list <- 
    unique(list[readcode != "XXXXXX"])
  
  dolk_2018()[list, on = c("readcode", "description")]
}




dolk_abx_2018 <- function(){
  
  abx_list <- collect_dt(antibiotics())
  
  # Add classification based on groups
  atc_group <- dtribble(
    ~group                     , ~class   , ~class_desc                                  ,
    #------------------------------|----------|----------------------------------------------#
    "Penicillin"                   , "J01C"   , "Penicillins"                                ,
    "Macrolides"                   , "J01F"   , "Macrolides, lincosamides and streptogramins",
    "Sulfonamides and trimethoprim", "J01E"   , "Sulphonamides and trimethoprim"             ,
    "Tetracycline"                 , "J01A"   , "Tetracyclines"                              ,
    "Nitrofurantoin"               , "J01XE01", "Nitrofurantoin"                             ,
    "Carbapenem"                   , "J01D"   , "Other beta-lactam antibacterials"           ,
    "Cephalosporin"                , "J01D"   , "Other beta-lactam antibacterials"           ,
    "Quinolone"                    , "J01M"   , "Quinolones"
  )
  
  abx_list <-
    merge(abx_list, atc_group, by = "group", all.x = TRUE)
  
  
  # Add classification based on substance
  atc_substance <- dtribble(
    ~substance                 , ~class   , ~class_desc                                  ,
    #------------------------------|----------|----------------------------------------------#
    "Aztreonam"                    , "J01D"   , "Other beta-lactam antibacterials"           ,
    "Clindamycin"                  , "J01F"   , "Macrolides, lincosamides and streptogramins",
    "Dalfopristin/Quinupristin"    , "J01F"   , "Macrolides, lincosamides and streptogramins",
    "Lincomycin"                   , "J01F"   , "Macrolides, lincosamides and streptogramins",
    "Pristinamycin"                , "J01F"   , "Macrolides, lincosamides and streptogramins"
  )
  
  abx_list[atc_substance, on = "substance", 
           c("class", "class_desc") := .(i.class, i.class_desc)]
  
  
  # Add subgroup classification based on substance
  atc_subgroup <- dtribble(
    ~substance                 , ~subclass      , ~subclass_desc                               ,
    #------------------------------|----------------|----------------------------------------------#
    "Amoxicillin"                  , "J01CA"        , "Extended-spectrum penicillins"              ,   
    "Ampicillin"                   , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Bacampicillin"                , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Azlocillin"                   , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Carbenicillin"                , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Mecillinam"                   , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Mezlocillin"                  , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Piperacillin"                 , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Pivampicillin"                , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Pivmecillinam"                , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Talampicillin"                , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Temocillin"                   , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Ticarcillin"                  , "J01CA"        , "Extended-spectrum penicillins"              ,
    "Benzylpenicillin"             , "J01CE"        , "Beta-lactamase-sensitive penicillins"       ,
    "Phenethicillin"               , "J01CE"        , "Beta-lactamase-sensitive penicillins"       ,
    "Phenoxymethylpenicillin"      , "J01CE"        , "Beta-lactamase-sensitive penicillins"       ,
    "Cloxacillin"                  , "J01CF"        , "Beta-lactamase-resistant penicillins"       ,
    "Flucloxacillin"               , "J01CF"        , "Beta-lactamase-resistant penicillins"       ,
    "Ampicillin/Cloxacillin"       , "J01CR"        , "Combinations of penicillins"                ,
    "Ampicillin/Flucloxacillin"    , "J01CR"        , "Combinations of penicillins"                ,
    "Co-amoxiclav"                 , "J01CR"        , "Combinations of penicillins"                ,
    "Co-ticarclav"                 , "J01CR"        , "Combinations of penicillins"                ,
    "Pip-Taz"                      , "J01CR"        , "Combinations of penicillins"                ,
    "Pivampicillin/Pivmecillinam"  , "J01CR"        , "Combinations of penicillins"                ,
    
    "Doxycycline"                  , "J01AA02"      , "Doxycycline"                                ,
    "Oxytetracycline"              , "J01AA06"      , "Oxytetracycline"                            ,
    "Chlortetracycline"            , "J01AA (other)", "Other tetracyclines"                        ,
    "Chlortetracycline/Demeclocycline/Tetracycline"
                                   , "J01AAx (other)", "Other tetracyclines"                        ,
    "Clomocycline"                 , "J01AAx (other)", "Other tetracyclines"                        ,
    "Demeclocycline"               , "J01AAx (other)", "Other tetracyclines"                        , 
    "Lymecycline"                  , "J01AAx (other)", "Other tetracyclines"                        ,
    "Metacycline"                  , "J01AAx (other)", "Other tetracyclines"                        ,
    "Minocycline"                  , "J01AAx (other)", "Other tetracyclines"                        , 
    "Oxytetracycline/Polymyxin B"  , "J01AAx (other)", "Other tetracyclines"                        ,
    "Tetracycline"                 , "J01AAx (other)", "Other tetracyclines"                        , 
    "Tigecycline"                  , "J01AAx (other)", "Other tetracyclines"                        ,
    
    "Cefadroxil"                   , "J01DB"        , "Cephalosporins (1st generation)"            ,
    "Cefalexin"                    , "J01DB"        , "Cephalosporins (1st generation)"            ,
    "Cefalotin"                    , "J01DB"        , "Cephalosporins (1st generation)"            ,
    "Cefradine"                    , "J01DB"        , "Cephalosporins (1st generation)"            ,
    "Cefaloridine"                 , "J01DB"        , "Cephalosporins (1st generation)"            ,
    
    
    "Trimethoprim"                 , "J01EA"        , "Trimethoprim and derivatives"               ,
    
    "Erythromycin"                 , "J01FA01"      , "Erythromycin"                               ,
    "Clarithromycin"               , "J01FA09"      , "Clarithromycin"                             ,
    "Azithromycin"                 , "J01FAx (other)", "Other macrolides"                           ,
    "Roxithromycin"                , "J01FAx (other)", "Other macrolides"                           ,
    "Spiramycin"                   , "J01FAx (other)", "Other macrolides"                           ,
    "Telithromycin"                , "J01FAx (other)", "Other macrolides"                           ,
    
    "Ciprofloxacin"                , "J01MA"        , "Fluoroquinolones"                           ,
    "Grepafloxacin"                , "J01MA"        , "Fluoroquinolones"                           ,
    "Levofloxacin"                 , "J01MA"        , "Fluoroquinolones"                           ,
    "Moxifloxacin"                 , "J01MA"        , "Fluoroquinolones"                           ,
    "Norfloxacin"                  , "J01MA"        , "Fluoroquinolones"                           ,
    "Ofloxacin"                    , "J01MA"        , "Fluoroquinolones"                           ,
    "Sparfloxacin"                 , "J01MA"        , "Fluoroquinolones"                           ,
    "Temafloxacin"                 , "J01MA"        , "Fluoroquinolones"                           ,
    
    "Metronidazole"                , "J01XD"        , "Imidazole derivatives"                      ,
    "Tinidazole"                   , "J01XD"        , "Imidazole derivatives"                      ,
    "Nitrofurantoin"               , "J01XE01"      , "Nitrofurantoin"                             
  )
  
  abx_list <-
    merge(abx_list, atc_subgroup, by = "substance", all.x = TRUE)
  
  
  abx_list[is.na(class), c("class", "class_desc") := .("", "Others")]
  abx_list[is.na(subclass), c("subclass", "subclass_desc") := .("others", "Others")]
  abx_list[, .(prodcode, productname, group, substance, class, class_desc, subclass, subclass_desc)]
}




# Rothnie/Quint et al. 2016 -----------------------------------------------


rothnie_2016 <- function(type = "medcode"){
  # Return the list defined by Rothnie/Quint et al. 2016 to define acute
  # exacerbations of COPD.
  #
  # Args: 
  #  type - "medcode" for diagnosis/symptoms,  "prodcode" for medication
  #
  # Results
  
  roth_file <- file.path(lu_dir, "PASS", "quint_validation_aecopd_2016.xlsx")
  
  list_names <- excel_sheets(roth_file)
  roth_list <- map(list_names, read_excel, path = roth_file)
  
  choose_med_prod <- map_lgl(roth_list, ~ type %in% names(.x))
  roth_list %<>% .[choose_med_prod]
  roth_list %>% walk(setDT)
  names(roth_list) <- list_names[choose_med_prod]
  
  roth_list
}

