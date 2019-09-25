


subfolder <- "00_replication/gharbi_2019"

# Initialise the workspace
source(file.path(subfolder, "00_init.R"))



# Load the data -----------------------------------------------------------

time_window <- 60

epi <- load_derived(str_c("epi_", time_window))

char(sep_prim, sep_sec, uti_prim, uti_sec, 
     hosp_diag, admission) %>% 
  walk(load_derived)




# Identify all infections that could result in sepsis ---------------------

# Urinary tract infections
uti_sec %<>% unique()

setorder(uti_sec, patid, spno, d_order)
uti_sec %<>% .[, .SD[1], by = .(patid, spno, icd)]


# Other infections (incl. pneumonia)
infect_codes <- read_excel(file.path(subfolder, "uti_sepsis.xlsx"), 
                         sheet = "Other infect - HES")
setDT(infect_codes)

inf_sec <- hosp_diag[infect_codes, on = .(icd), nomatch = 0] %>% 
                   .[admission, on = .(patid, spno), nomatch = 0]
inf_sec %<>% unique()




# Restrict to patients with sepsis ----------------------------------------

epi %<>% .[sep == "yes", .(patid, start, presc)]
epi[, window := start %m+% days(time_window)]






# Look at patients with sepsis in hospital --------------------------------

sep_sec[, linkdate := sepdate]

sec <- sep_sec[epi, on = .(patid, linkdate > start, linkdate <= window), 
               nomatch = 0, .(patid, spno, start, sepdate, window, 
                              code = icd, sep_ord = d_order)]


# Only keep the first sepsis that occurred
setorder(sec, patid, start, sepdate)
sec %<>% .[, .SD[1], by = .(patid, start)]

# Identify urosepsis
sec[uti_sec, on = .(patid, spno), 
    type := if_else(d_order + i.d_order <= 3, "hosp: urosep  1/2", 
                                              "hosp: urosep other")]

# Identify sepsis of other origin
sec[(inf_sec[lrti == TRUE]), on = .(patid, spno), 
    type := if_else(is.na(type), "hosp: sep lrti", type)]

sec[inf_sec, on = .(patid, spno), 
    type := if_else(is.na(type), "hosp: sep other", type)]



# Mark all remaining cases as unspecified sepsis
sec[is.na(type), type := "hosp: sep unspec"]






# Look at all patients with sepsis in primary only ------------------------

sep_prim[, linkdate := sepdate]

prim <- 
  sep_prim[epi, on = .(patid, linkdate > start, linkdate <= window), 
           nomatch = 0, .(patid, start, sepdate, window, 
                        src = "prim", code = readcode)] %>% 
         .[!sec, on = .(patid, start)]
  

# Only keep the first sepsis that occurred
setorder(prim, patid, start, sepdate)
prim %<>% .[, .SD[1], by = .(patid, start)]


# UTI code in hospital before

  # Identify cases coded in hospital
gp_hosp <- 
  prim[, .(patid, start, sepdate, from = start, to = sepdate, code)] %>% 
     .[uti_sec, on = .(patid, from < admidate, to >= admidate),
       .(patid, start, sepdate, code), nomatch = 0]
setorder(gp_hosp, patid, start, sepdate)
gp_hosp %<>% .[, .SD[1], by = .(patid, start)]

gp_hosp[medical() %>% collect_dt(), on = .(code = readcode), 
        medcode := medcode]

gp_hosp_db <- copy_to(get_db_conn(), gp_hosp, "gp_hosp")

  # Find out which staff role enterred the code
staff <- 
  gp_hosp_db %>% 
  inner_join(clinical_db(), by = c("patid", "sepdate" = "eventdate", "medcode")) %>%
  left_join(staff_db(), by = "staffid") %>% 
  select(patid, sepdate, role = role_desc) %>% 
  collect_dt(convert = TRUE)

  # Group roles
staff[str_detect(role, regex("consultant|partner|registrar|locum", ignore_case = TRUE)), role_grp := "clinician"]
staff[str_detect(role, regex("nurse|health care prof|clinical", ignore_case = TRUE)), role_grp := "other care"]
staff[role == "Missing", role_grp := "missing"]
staff[is.na(role_grp), role_grp := "admin"]

  # Add to prim
prim[staff, on = .(patid, sepdate), type := str_c("gp: uti in hosp (", role_grp,")")]






# Other infection in hospital before
prim[inf_sec, on = .(patid, start < admidate, sepdate >= admidate),
     type := if_else(is.na(type), "gp: other inf in hosp", type)]


# Any admission at all between UTI episode start and sepsis date
prim[admission, on = .(patid, start < admidate, sepdate >= admidate),
     type := if_else(is.na(type), "gp: unspec with admit", type)]


# Mark the rest with sepsis in primary care only
prim[is.na(type), type := "gp: unspec"]





# Combine -----------------------------------------------------------------

evidence <- rbind(
  sec[, .(patid, start, sepdate, type)],
  prim[, .(patid, start, sepdate, type)]
)

evidence[epi, on = .(patid, start), presc := presc]


median(evidence[presc == "yes", sepdate - start])
median(evidence[presc == "no", sepdate - start])


# Tabulate ----------------------------------------------------------------

ord <- c("hosp: urosep  1/2",
         "hosp: urosep other",
         "hosp: sep lrti",
         "hosp: sep other",
         "hosp: sep unspec",
         "gp: uti in hosp (clinician)",
         "gp: uti in hosp (other care)",
         "gp: uti in hosp (admin)",
         "gp: uti in hosp (missing)",
         "gp: other inf in hosp",
         "gp: unspec with admit",
         "gp: unspec")

evidence[, .N, by = .(type, presc)] %>% 
       .[, .(type, N, p = prty(N / sum(N) * 100, 1)), by = presc] %>% 
       .[, N_p := str_c(N, " (", p, ")")] %>% 
  dcast(type ~ presc, value.var = "N_p") %>% 
  setkey(type) %>% 
  .[ord]


# Create a flowchart ------------------------------------------------------

evid_sum <- evidence[, .N, by = type]


library(grid)
library(Gmisc)

grid.newpage()
box <- gpar(fill = "white")

leftx = .35; rightx =.75; midx = .5; xslide =.2; width = .35; wf1 = 1
arrow.style <- arrow(angle=20, length = unit(3, "mm"), ends = "last", type = "closed")

(total <- boxGrob(paste0("Total\n N = ", sum(evid_sum$N)),
                  x = midx, y = .86, box_gp = box, width = width))

(hosp <- boxGrob(paste0("Sepsis diagnosis\nin hospital:\nn = ",
                        sum(evid_sum[type %like% "^hosp"]$N)), 
                     x = midx-(xslide+0.1), y = .74, box_gp = box, width = width*wf1))

(hosp_1 <- boxGrob(paste0("Sepsis and UTI code\nin positions 1&2:\nn = ",
                          evid_sum[type %like% "pos 1/2"]$N),
                 x = midx-xslide, y = .62, box_gp = box, width = width*wf1))

(hosp_2 <- boxGrob(paste0("Sepsis and UTI code\nin other positions:\nn = ",
                          evid_sum[type %like% "other pos"]$N),
                   x = midx-xslide, y = .5, box_gp = box, width = width*wf1))

(hosp_3 <- boxGrob(paste0("Sepsis and other \ninfectious code:\nn = ",
                          evid_sum[type %like% "other infect"]$N, 
                          "\n(of which ", lrti_sep, " were LRTI)"),
                   x = midx-xslide, y = .38, box_gp = box, width = width*wf1))

(hosp_4 <- boxGrob(paste0("Only sepsis code:\nn = ",
                          evid_sum[type %like% "hosp \\(unspec\\)"]$N),
                   x = midx-xslide, y = .24, box_gp = box, width = width*wf1))

(gp <- boxGrob(paste0("Sepsis diagnosis\nin primary care only:\nn = ",
                        sum(evid_sum[type %like% "gp"]$N)), 
                 x = midx+(xslide-0.1), y = .74, box_gp = box, width = width*wf1))

(gp_1 <- boxGrob(paste0("UTI code in\nprior hospital stay:\nn = ",
                          evid_sum[type %like% "plus uti in hosp"]$N),
                   x = midx+xslide, y = .62, box_gp = box, width = width*wf1))

(gp_2 <- boxGrob(paste0("UTI code in\npimary care on same day:\nn = ",
                        evid_sum[type %like% "gp \\(plus uti\\)"]$N),
                 x = midx+xslide, y = .5, box_gp = box, width = width*wf1))

(gp_3 <- boxGrob(paste0("Other infection\nin prior hospital stay:\nn = ",
                        evid_sum[type %like% "gp \\(plus other inf in hosp\\)"]$N),
                 x = midx+xslide, y = .38, box_gp = box, width = width*wf1))

(gp_4 <- boxGrob(paste0("Prior hospital stay\nwithout infection:\nn = ",
                        evid_sum[type %like% "unspec with admit"]$N),
                 x = midx+xslide, y = .24, box_gp = box, width = width*wf1))

(gp_5 <- boxGrob(paste0("No prior hospital stay:\nn = ",
                        evid_sum[type %like% "gp \\(unspec\\)"]$N),
                 x = midx+xslide, y = .12, box_gp = box, width = width*wf1))



