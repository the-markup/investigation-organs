# ---------------------------------------------------------------------------- #
# 01_master_organs_data_clean.R                                                #
# ---------------------------------------------------------------------------- #
# Purpose: Cleaning all raw organs data sets, including:                       #
#   1. Hospital Location Data                                                  #
#   2. Candidate Waitlist Data                                                 #
#   3. Donor Data                                                              #
#   4. Transplant Data                                                         #
#   5. CDC Wonder Data on Liver Mortality                                      #
#   6. Donor Disposition Data                                                  #
# ---------------------------------------------------------------------------- #

## ---------------- ##
## INIT ENVIRONMENT ##
## ---------------- ##

# clear all existing data + vars
rm(list = ls())

# package imports
library(tidyverse)
library(janitor)
library(lubridate)

# set paths
root_path <- here::here()
data_path <- paste0(root_path, "/01_data")

setwd(data_path)

## --------------- ##
## IMPORT RAW DATA ##
## --------------- ##

# import institution + hospital location data
institution.raw <- as_tibble(read.csv("raw/01_hospitals/22F138_Institution.csv"))
institution.latlng.raw <- as_tibble(read.csv("raw/01_hospitals/DONORHOSPITAL_22F168Records(2of3)-redacted.csv"))
donor.hospital <- as_tibble(read.csv("raw/01_hospitals/DONORHOSPITAL_22F168Records(1of3)-redacted.csv"))

# import raw candidate data
candidate.raw <- as_tibble(read.csv("raw/02_candidate/Updated_CAN_LIIN_22F218Records(1of2)-redacted.csv"))

# import raw donor data
donor.raw <- as_tibble(read.csv("raw/03_donor/22F123_Records_Redacted.csv"))

# import raw transplant data
transplant.raw <- as_tibble(read.csv("raw/04_transplant/TX_LI_22F218Records(2of2)-redacted.csv"))

# import raw CDC WONDER data
cdc_wonder.StateYearRace.raw <- read.table("raw/05_cdc_wonder/liver_disease_rates/cdc_wonder_liver_disease_mortality_stats_byStateYearRace_2018to2021.txt",
                                           fill = T, sep = "\t", header = T) %>% as_tibble()
cdc_wonder.StateYear.raw <- read.table("raw/05_cdc_wonder/liver_disease_rates/cdc_wonder_liver_disease_mortality_stats_byStateYear_2018to2021.txt",
                                       fill = T, sep = "\t", header = T) %>% as_tibble()
cdc_wonder.StateRace.raw <- read.table("raw/05_cdc_wonder/liver_disease_rates/cdc_wonder_liver_disease_mortality_stats_byStateRace_2018to2021.txt",
                                       fill = T, sep = "\t", header = T) %>% as_tibble()
cdc_wonder.YearRace.raw <- read.table("raw/05_cdc_wonder/liver_disease_rates/cdc_wonder_liver_disease_mortality_stats_byYearRace_2018to2021.txt",
                                      fill = T, sep = "\t", header = T) %>% as_tibble()

# import raw donor disposition data
don_disp.raw <- as_tibble(read.csv("raw/06_don_disp/DONDISPOSITION_22F167Records-redacted.csv"))

# import optn tx counts
optn_tx_counts.raw <- as_tibble(read.csv("raw/07_optn_tx_counts/OPTN-Transplants_in_the_U.S._by_State-20230216.csv"))

## ---------------------------------------- ##
## DEFINE HELPER FOR TURNING COORD TO STATE ##
## ---------------------------------------- ##

# load high resolution state shapefiles
us_states <- sf::st_read("raw/00_misc/cb_2021_us_state_500k/cb_2021_us_state_500k.shp")

# fn to convert lat and lon to state, 
coordinatesToState <- function(pointsDF,
                               states = us_states,
                               name_col = "STUSPS") {
  
  ## Convert points data.frame to an sf POINTS object
  pts <- sf::st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- sf::st_transform(states, crs = 3857)
  pts <- sf::st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(sf::st_intersects(pts, states))
  state_names[ii]
}

## ---------------------- ##
## CLEAN INSTITUTION DATA ##
## ---------------------- ##

# clean up zip code to be consistent format
# and manually edit zip codes that appear incorrect (see 00_data_cleaning_analysis notebook)
institution <- institution.raw %>%
  mutate(PRIMARY_ZIP = substr(stringr::str_pad(PRIMARY_ZIP, 5, "left", 0), 1, 5)) %>%
  tidylog::mutate(PRIMARY_ZIP = case_when(PROVIDER_NUM == "120010" & PRIMARY_ZIP == "96820" ~ "96817",
                                          PROVIDER_NUM == "170040" & PRIMARY_ZIP == "66103" ~ "66160",
                                          PROVIDER_NUM == "450686" & PRIMARY_ZIP == "79408" ~ "79415",
                                          T ~ PRIMARY_ZIP))

# clean up zip code to be consistent format
# and manually edit zip codes that appear incorrect (see 00_data_cleaning_analysis notebook)
institution.latlng <- institution.latlng.raw %>%
  mutate(HOSPITAL_ZIP = stringr::str_pad(HOSPITAL_ZIP, 5, "left", "0")) %>%
  tidylog::mutate(HOSPITAL_ZIP = case_when(PROVIDER_NUM == "190135" & HOSPITAL_ZIP == "70115" ~ "70119",
                                           PROVIDER_NUM == "220171" & HOSPITAL_ZIP == "01803" ~ "01805",
                                           PROVIDER_NUM == "220163" & HOSPITAL_ZIP == "01605" ~ "01655",
                                           PROVIDER_NUM == "39012F" & HOSPITAL_ZIP == "15213" ~ "15240",
                                           T ~ HOSPITAL_ZIP)) %>%
  mutate(CHG_DT = as.Date(CHG_DT, "%m/%d/%y"))

## -------------------------- ##
## CLEAN + AGG CANDIDATE DATA ##
## -------------------------- ##

# clean all candidate data (children + adult candidates)
all_candidate <- candidate.raw %>%
  
  # limit to liver candidates
  tidylog::filter(WL_ORG == "LI: Liver") %>%
  
  # add clean columns for some key fields (transplant date, listing date, age, race)
  mutate(REC_TX_DT_DATE = as.Date(REC_TX_DT, "%m/%d/%Y"),
         REC_TX_DT_YEAR = year(REC_TX_DT_DATE),
         CAN_LISTING_DT_DATE = as.Date(CAN_LISTING_DT, "%m/%d/%Y"),
         CAN_LISTING_DT_YEAR = year(CAN_LISTING_DT_DATE),
         CAN_AGE_AT_LISTING = recode(CAN_AGE_AT_LISTING, 
                                     "17-Dec" = "12-17", 
                                     "11-Jun" = "6-11", 
                                     "5-Jan" = "1-5"),
         CAN_REM_DT_DATE = as.Date(CAN_REM_DT, "%m/%d/%Y"),
         CAN_PERM_STATE_CLEAN = substr(CAN_PERM_STATE, 1, 2),
         CAN_RACE_SRTR_CLEAN = recode(CAN_RACE_SRTR,
                                      "BLACK: Black" = "Black or African American",
                                      "WHITE: White" = "White",
                                      "NATIVE: Native American" = "American Indian or Alaska Native",
                                      "ASIAN: Asian" = "Asian",
                                      "PACIFIC: Pacific Islander" = "Native Hawaiian or Other Pacific Islander",
                                      "MULTI: Multiracial" = "More than one race",
                                      .default = CAN_RACE_SRTR)) %>%
  
  # limit to candidates who either received a transplant or was added to the waitlist during relevant period
  tidylog::filter(between(REC_TX_DT_DATE, as.Date("2018-01-01"), as.Date("2022-02-28")) |
                  between(CAN_LISTING_DT_DATE, as.Date("2018-01-01"), as.Date("2022-02-28"))) %>%
  
  # join on institution data to identify hospital state
  #   can rely on PRIMARY_STATE, because all CTR_TY = "TX1: Transplant Hospital"
  tidylog::left_join(institution %>%
                       filter(CTR_TY == "TX1: Transplant Hospital") %>%
                       select(CTR_ID, CTR_TY, CAN_HOSPITAL_STATE = PRIMARY_STATE), 
                     by = c("CAN_LISTING_CTR_ID" = "CTR_ID")) %>%
  
  # limit to relevant columns
  select(DONOR_ID, WL_ORG, CAN_AGE_AT_LISTING, CAN_GENDER, CAN_ABO, CAN_RACE, CAN_RACE_SRTR, CAN_RACE_SRTR_CLEAN,
         REC_TX_DT, REC_TX_DT_DATE, REC_TX_DT_YEAR, CAN_LISTING_DT, CAN_LISTING_DT_DATE, CAN_LISTING_DT_YEAR,
         CAN_REM_DT, CAN_REM_DT_DATE, CAN_REM_CD, CAN_INIT_SRTR_LAB_MELD, CAN_INIT_STAT, 
         CAN_PERM_STATE, CAN_PERM_STATE_CLEAN, CAN_LISTING_CTR_ID, CAN_HOSPITAL_STATE)

# limit to adult candidates (for analysis)
candidate <- all_candidate %>%
  tidylog::filter(CAN_AGE_AT_LISTING %in% c("18-34", "35-49", "50-64", "65+", "Unknown"))
  
## ---------------------- ##
## CLEAN + AGG DONOR DATA ##
## ---------------------- ##

donor <- donor.raw %>%
  
  # add clean columns for some key fields (recovery date, home state)
  mutate(DON_RECOV_DT_DATE = as.Date(DON_RECOV_DT, "%m/%d/%y"),
         DON_RECOV_DT_YEAR = year(DON_RECOV_DT_DATE),
         DON_PERM_STATE_CLEAN = substr(DON_HOME_STATE, 1, 2),
         DON_RACE_SRTR_CLEAN = recode(DON_RACE_SRTR,
                                      "BLACK: Black" = "Black or African American",
                                      "WHITE: White" = "White",
                                      "NATIVE: Native American" = "American Indian or Alaska Native",
                                      "ASIAN: Asian" = "Asian",
                                      "PACIFIC: Pacific Islander" = "Native Hawaiian or Other Pacific Islander",
                                      "MULTI: Multiracial" = "More than one race",
                                      .default = DON_RACE_SRTR)) %>%
  
  # limit to donors who had their liver recovered in or after jan 2018 to end of analysis period
  tidylog::filter(between(DON_RECOV_DT_DATE, as.Date("2018-01-01"), as.Date("2022-02-28"))) %>%
  
  # limit to relevant columns
  select(DONOR_ID, DON_RECOV_DT, DON_RECOV_DT_DATE, DON_RECOV_DT_YEAR, DON_OPO_CTR_ID,
         DON_AGE, DON_GENDER, DON_ABO, DON_RACE, DON_RACE_SRTR, DON_RACE_SRTR_CLEAN,
         DON_PERM_STATE = DON_HOME_STATE, DON_PERM_STATE_CLEAN, DON_DEATH_CIRCUM)

## --------------------------- ##
## CLEAN + AGG TRANSPLANT DATA ##
## --------------------------- ##

transplant.pre <- transplant.raw %>%
  
  # limit to recipients 18+
  tidylog::filter(REC_AGE_AT_TX %in% c("18-34", "35-49", "50-64", "65+", "Unknown")) %>%
  
  # clean variables for easier use
  mutate(DON_RECOV_DT_DATE = as.Date(DON_RECOV_DT, "%m/%d/%Y"),
         DON_RECOV_DT_YEAR = year(DON_RECOV_DT_DATE),
         DON_RACE_SRTR_CLEAN = recode(DON_RACE_SRTR,
                                      "BLACK: Black" = "Black or African American",
                                      "WHITE: White" = "White",
                                      "NATIVE: Native American" = "American Indian or Alaska Native",
                                      "ASIAN: Asian" = "Asian",
                                      "PACIFIC: Pacific Islander" = "Native Hawaiian or Other Pacific Islander",
                                      "MULTI: Multiracial" = "More than one race",
                                      .default = DON_RACE_SRTR),
         REC_TX_DT_DATE = as.Date(REC_TX_DT, "%m/%d/%Y"),
         REC_TX_DT_YEAR = year(REC_TX_DT_DATE),
         CAN_LISTING_DT_DATE = as.Date(CAN_LISTING_DT, "%m/%d/%Y"),
         CAN_LISTING_DT_YEAR = year(CAN_LISTING_DT_DATE),
         CAN_AGE_AT_LISTING = recode(CAN_AGE_AT_LISTING, 
                                     "17-Dec" = "12-17", 
                                     "11-Jun" = "6-11", 
                                     "5-Jan" = "1-5"),
         CAN_PERM_STATE_CLEAN = substr(REC_PERM_STATE, 1, 2),
         CAN_RACE_SRTR_CLEAN = recode(CAN_RACE_SRTR,
                                      "BLACK: Black" = "Black or African American",
                                      "WHITE: White" = "White",
                                      "NATIVE: Native American" = "American Indian or Alaska Native",
                                      "ASIAN: Asian" = "Asian",
                                      "PACIFIC: Pacific Islander" = "Native Hawaiian or Other Pacific Islander",
                                      "MULTI: Multiracial" = "More than one race",
                                      .default = CAN_RACE_SRTR)) %>%
  
  # limit to transplants from relevant analysis period
  tidylog::filter(between(REC_TX_DT_DATE, as.Date("2018-01-01"), as.Date("2022-02-28"))) %>%
  
  # limit to relevant columns
  select(TX_ID, DON_RECOV_DT, DON_RECOV_DT_DATE, DON_RECOV_DT_YEAR, REC_TX_DT, REC_TX_DT_DATE, REC_TX_DT_YEAR,
         CAN_LISTING_DT, CAN_LISTING_DT_DATE, CAN_LISTING_DT_YEAR, CAN_AGE_AT_LISTING,
         CAN_GENDER, CAN_ABO, CAN_RACE, REC_PERM_STATE, CAN_PERM_STATE_CLEAN, CAN_RACE_SRTR_CLEAN, REC_CTR_ID,
         DON_GENDER, DON_ABO, DON_RACE, DON_OPO_CTR_ID, DON_DEATH_CIRCUM, DON_DEATH_MECH, DON_RACE_SRTR_CLEAN, DON_TY)

# since DONOR_ID + PER_ID are both censored, we can back into these IDs 
# by comparing to donor/candidate data based on demographics
tx_donor_id_match.pre <- transplant.pre %>%
  
  # merge on candidate data using candidate demo info
  tidylog::left_join(all_candidate %>%
                       filter(!is.na(REC_TX_DT_DATE)) %>%
                       select(DONOR_ID, CAN_GENDER, CAN_ABO, CAN_RACE, CAN_LISTING_CTR_ID,
                              REC_TX_DT_DATE, CAN_LISTING_DT_DATE, CAN_AGE_AT_LISTING),
                     by = c("CAN_GENDER", "CAN_ABO", "CAN_RACE", "REC_CTR_ID" = "CAN_LISTING_CTR_ID",
                            "REC_TX_DT_DATE", "CAN_LISTING_DT_DATE", "CAN_AGE_AT_LISTING")) %>%
  
  # for each transplant, identify if there are multiple potential candidate/donor id matches
  group_by(TX_ID) %>%
  mutate(N_DONOR_ID = n_distinct(DONOR_ID, na.rm = T)) %>%
  ungroup()

# transplants with unique DONOR_IDs identified by candidate demographics
tx_donor_id_match.v1 <- tx_donor_id_match.pre %>%
  filter(N_DONOR_ID == 1) %>%
  select(TX_ID, DONOR_ID)

print(glue::glue("# of tx with DONOR_ID match based on unique candidate: {nrow(tx_donor_id_match.v1)}"))

# transplants with duplicate DONOR_IDs when identified by candidate demographics
# but that can identify unique DONOR_IDs when cross referencing against donor demographics
tx_donor_id_match.v2 <- tx_donor_id_match.pre %>%
  filter(N_DONOR_ID > 1) %>%
  left_join(donor %>%
              select(DONOR_ID, DON_GENDER, DON_ABO, DON_RACE, DON_DEATH_CIRCUM, 
                     DON_OPO_CTR_ID, DON_RECOV_DT_DATE),
            by = "DONOR_ID", suffix = c("", ".DON_MATCH")) %>%
  filter(DON_GENDER == DON_GENDER.DON_MATCH &
           DON_ABO == DON_ABO.DON_MATCH & 
           DON_RACE == DON_RACE.DON_MATCH & 
           DON_DEATH_CIRCUM == DON_DEATH_CIRCUM.DON_MATCH & 
           DON_OPO_CTR_ID == DON_OPO_CTR_ID.DON_MATCH & 
           DON_RECOV_DT_DATE == DON_RECOV_DT_DATE.DON_MATCH) %>%
  select(TX_ID, DONOR_ID)

print(glue::glue("# of tx with DONOR_ID match based on candidate + donor match: {nrow(tx_donor_id_match.v2)}"))

# expand transplant.pre with DONOR_ID
# and then determine recipient + donor hospital location (state, lat, lng)
transplant <- transplant.pre %>%
  
  # merge on DONOR_ID using mix of tx_donor_id_match.v1 + v2 (should be 100% match)
  tidylog::left_join(bind_rows(tx_donor_id_match.v1, tx_donor_id_match.v2),
                     by = "TX_ID") %>%
  
  ## merging on recipient hospital location information
  # join on institution data to identify hospital state
  #   can rely on PRIMARY_STATE, because all CTR_TY = "TX1: Transplant Hospital"
  tidylog::left_join(institution %>%
                       select(CTR_ID, PROVIDER_NUM, CAN_HOSPITAL_STATE = PRIMARY_STATE, PRIMARY_ZIP), 
                     by = c("REC_CTR_ID" = "CTR_ID")) %>%
  
  # join on institution lat/lng using PROVIDER_NUM + ZIP (from institution) to get lat/lng
  tidylog::left_join(institution.latlng %>% 
                       # if ever duplicate locations based on PROVIDER_NUM + HOSPITAL_ZIP
                       # use the latest entry
                       group_by(PROVIDER_NUM, HOSPITAL_ZIP) %>%
                       arrange(desc(CHG_DT), desc(ACTIVE_FLG)) %>%
                       filter(n() == 1 | row_number() == 1) %>%
                       ungroup() %>%
                       select(PROVIDER_NUM, HOSPITAL_ZIP,
                              CAN_HOSPITAL_LATITUDE = LATITUDE,
                              CAN_HOSPITAL_LONGITUDE = LONGITUDE), 
                     by = c("PROVIDER_NUM", "PRIMARY_ZIP" = "HOSPITAL_ZIP")) %>%
  
  ## merging on donor hospital location information
  # join donor hospital data to identify hospital lat/lng
  # + then use that to determine the hospital state
  left_join(donor.hospital, by = "DONOR_ID") %>%
  left_join(institution.latlng %>%
              select(OPO_CTR_CD, OPO_CTR_TY, PROVIDER_NUM, PROVIDER_MBR_TY,
                     DON_HOSPITAL_LATITUDE = LATITUDE,
                     DON_HOSPITAL_LONGITUDE = LONGITUDE),
            by = c("DON_OPO_CTR_CD" = "OPO_CTR_CD",
                   "DON_OPO_CTR_TY" = "OPO_CTR_TY",
                   "DON_HOSP_PROV_NUM" = "PROVIDER_NUM",
                   "DON_HOSP_PROV_TY" = "PROVIDER_MBR_TY")) %>%
  
  # use coordinatesToState function to determine donor hospital state
  # and then use donor lat/lng vs. recipient lat/lng to calculate distance between the two
  mutate(DON_HOSPITAL_STATE = coordinatesToState(data.frame(x = replace_na(DON_HOSPITAL_LONGITUDE, 0),
                                                            y = replace_na(DON_HOSPITAL_LATITUDE, 0))),
         CAN_DON_DISTANCE = geosphere::distHaversine(cbind(DON_HOSPITAL_LONGITUDE, DON_HOSPITAL_LATITUDE), 
                                                     cbind(CAN_HOSPITAL_LONGITUDE, CAN_HOSPITAL_LATITUDE))) %>%
  
  # drop additional fields used for merging (but no longer needed)
  select(-PROVIDER_NUM, -PRIMARY_ZIP, -DON_OPO_CTR_TY, -DON_OPO_CTR_CD, -DON_HOSP_PROV_NUM, -DON_HOSP_PROV_TY)

## --------------------------- ##
## CLEAN + AGG CDC WONDER DATA ##
## --------------------------- ##

# clean all cuts of data straight from CDC Wonder
cdc_wonder.StateYearRace <- cdc_wonder.StateYearRace.raw %>%
  clean_names(case = "all_caps") %>%
  filter(!is.na(YEAR_CODE)) %>%
  select(STATE_NAME = RESIDENCE_STATE, YEAR = YEAR_CODE, RACE = SINGLE_RACE_6,
         DEATHS, POPULATION) %>%
  mutate(YEAR = as.character(YEAR),
         DEATHS = as.numeric(case_when(DEATHS != "Suppressed" ~ DEATHS)))

cdc_wonder.StateYear <- cdc_wonder.StateYear.raw %>%
  clean_names(case = "all_caps") %>%
  filter(!is.na(YEAR_CODE)) %>%
  select(STATE_NAME = RESIDENCE_STATE, YEAR = YEAR_CODE, DEATHS, POPULATION) %>%
  mutate(RACE = "All Races", .after = YEAR) %>%
  mutate(YEAR = as.character(YEAR))

cdc_wonder.StateRace <- cdc_wonder.StateRace.raw %>%
  clean_names(case = "all_caps") %>%
  filter(!is.na(RESIDENCE_STATE_CODE)) %>%
  select(STATE_NAME = RESIDENCE_STATE, RACE = SINGLE_RACE_6, DEATHS, POPULATION) %>%
  mutate(YEAR = "2018 - 2021", .after = STATE_NAME) %>%
  mutate(DEATHS = as.numeric(case_when(DEATHS != "Suppressed" ~ DEATHS)))

cdc_wonder.YearRace <- cdc_wonder.YearRace.raw %>%
  clean_names(case = "all_caps") %>%
  filter(!is.na(YEAR_CODE), NOTES != "Total") %>%
  select(YEAR = YEAR_CODE, RACE = SINGLE_RACE_6, DEATHS, POPULATION) %>%
  mutate(STATE_NAME = "National", .before = everything()) %>%
  mutate(YEAR = as.character(YEAR))

# aggregate YearRace to just Year, just Race, and overall Total counts
cdc_wonder.Year <- cdc_wonder.YearRace %>%
  group_by(STATE_NAME, YEAR, RACE = "All Races") %>%
  summarise(across(c("DEATHS", "POPULATION"), sum, na.rm = T),
            .groups = "drop")

cdc_wonder.Race <- cdc_wonder.YearRace %>%
  group_by(STATE_NAME, YEAR = "2018 - 2021", RACE) %>%
  summarise(across(c("DEATHS", "POPULATION"), sum, na.rm = T),
            .groups = "drop")

cdc_wonder.Total <- cdc_wonder.YearRace %>%
  group_by(STATE_NAME, YEAR = "2018 - 2021", RACE = "All Races") %>%
  summarise(across(c("DEATHS", "POPULATION"), sum, na.rm = T),
            .groups = "drop")

# stack all data together and calculate crude death rate
cdc_wonder <- bind_rows(cdc_wonder.StateYearRace,
                        cdc_wonder.StateYear,
                        cdc_wonder.StateRace,
                        cdc_wonder.YearRace,
                        cdc_wonder.Year,
                        cdc_wonder.Race,
                        cdc_wonder.Total) %>%
  mutate(STATE_NAME = factor(STATE_NAME,
                             levels = c("National", sort(unique(cdc_wonder.StateYear$STATE_NAME)))),
         YEAR = factor(YEAR, levels = c(2018:2021, "2018 - 2021")),
         RACE = factor(RACE, levels = c("All Races", sort(unique(cdc_wonder.Race$RACE)))),
         DEATH_RATE = DEATHS / POPULATION * 1e5) %>%
  arrange(STATE_NAME, YEAR, RACE)

## ----------------------------------- ##
## CLEAN  + AGG DONOR DISPOSITION DATA ##
## ----------------------------------- ##

don_disp <- don_disp.raw %>%
  
  # limit to livers using DON_ORG
  filter(grepl("Liver", DON_ORG)) %>%
  
  # clean DON_RECOV_DT
  mutate(DON_RECOV_DT_DATE = as.Date(DON_RECOV_DT, "%m/%d/%y"),
         DON_RECOV_DT_YEAR = year(DON_RECOV_DT_DATE)) %>%
  
  # limit to donors who had their liver recovered in or after jan 2018 to end of analysis period
  tidylog::filter(between(DON_RECOV_DT_DATE, as.Date("2018-01-01"), as.Date("2022-02-28")))

## -------------------------- ##
## CLEAN + AGG OPTN TX TOTALS ##
## -------------------------- ##

optn_tx_counts <- optn_tx_counts.raw %>%
  select(-X.2, -To.Date, -X2023) %>%
  rename(CENTER_STATE = X,
         REC_AGE = X.1) %>%
  mutate(across(CENTER_STATE:REC_AGE, ~ case_when(.x != "" ~ .x))) %>%
  filter(if_any(CENTER_STATE:REC_AGE, ~ !is.na(.x))) %>%
  fill(CENTER_STATE:REC_AGE, .direction = "down") %>%
  pivot_longer(matches("X[0-9]{4}")) %>%
  mutate(YEAR = as.numeric(gsub("^X", "", name)),
         TX = as.numeric(gsub(",", "", value))) %>%
  filter(YEAR %in% 2017:2022) %>%
  select(CENTER_STATE, REC_AGE, YEAR, TX)

## ------------------- ##
## EXPORT CLEANED DATA ##
## ------------------- ##

lapply(c("all_candidate", "candidate", "donor", "transplant", "cdc_wonder", 
         "don_disp", "optn_tx_counts"), 
       function(d) { 
         saveRDS(get(d),
                 paste0("clean/", gsub("[.]", "_", d), "_clean.rds"))
       })
