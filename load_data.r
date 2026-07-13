library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(lubridate)


tnm_data <- read_csv("tnm_check.csv")
head(tnm_data)

tnm_data <- tnm_data %>% rename(Country=country, Site=site)
valid_T <- c("TX", "T0", "Ta", "Tis", "T1", "T2", "pT2a", "pT2b", "T3", "pT3a", "pT3b", "T4", "T4a", "T4b")
valid_N <- c("NX", "N0", "N1", "N2", "N3")
valid_M <- c("M0", "M1", "M1a", "M1b")
valid_location <- c("bladder", "renal pelvis", 'ureter', 'urethra')
tnm_data <- tnm_data %>% mutate(Eligibility= case_when(
    is.na(TNM_T) | is.na(TNM_N) | is.na(TNM_M) ~ "Missing Data",
    !(TNM_T %in% valid_T) | !(TNM_N %in% valid_N) | !(TNM_M %in% valid_M) ~ "Inconsistent",
    !(str_to_lower(str_trim(PTUMLOC)) %in% valid_location) ~ "Ineligible",
    #bladder stage IVb
    TNM_M == "M1b" ~ "Eligible",
    #bladder stage IVa
    TNM_M == "M1a" ~ "Eligible",
    #any distant metastasis
    TNM_M=="M1" ~ "Eligible",
    ##Stage IV A
    TNM_T == "T4b" & TNM_M=="M0" ~ "Eligible",
    #Stage IIIb
    TNM_N %in% c("N2", "N3") & TNM_M=="M0" ~ "Eligible",
    TRUE ~ "Ineligible"
    ),
    Reason = case_when(
        is.na(TNM_T) ~ "Missing T Stage",
        is.na(TNM_N) ~ "Missing N Stage",
        is.na(TNM_M) ~ "Missing M Stage",

        #invalid values
        !(TNM_T %in% valid_T) ~ "Invalid T Stage", 
        !(TNM_N %in% valid_N) ~ "Invalid N Stage",
        !(TNM_M %in% valid_M) ~ "Invalid M Stage",
        !(str_to_lower(str_trim(PTUMLOC)) %in% valid_location) ~ "Tumor Location not eligible for the study",
        #Eligible
        TNM_M=="M1" ~ "Stage IV (M1)",
        TNM_M=="M1a" ~ "Stage IVA (M1a)",
        TNM_M=="M1b" ~ "Stage IVB (M1b)",
        TNM_T=="T4b" & TNM_M =="M0" ~ "Stage IVA (T4b, M0)",
        TNM_N=="N2" & TNM_M=="M0" ~ "Stage IIIB (N2, M0)",
        TNM_N=="N3" & TNM_M=="M0" ~ "Stage IIIB (N3, M0)",
        TRUE ~ "Does not meet Stage IIIB or Stage IV eligibility criteria"
    ),
    EnrollmentDate=as.Date(enrollment_date),
    RandomizationDate=as.Date(randomization_date),
    Randomization_Delay= as.numeric(RandomizationDate - EnrollmentDate),
    Date_Issue=ifelse(Randomization_Delay<0, "Invalid Dates","Valid"),
    Month= floor_date(EnrollmentDate, "month")
    )

#summary(tnm_data)
glimpse(tnm_data)

