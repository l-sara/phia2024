---
title: "CODPHIA - Main Dashboard"
author: "Ladina"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}

#Loading necessary packages
library(tidyverse)
library(viridis)
library(gt)
library(openxlsx)
# library(reshape)
# library(reshape2)
library(scales)
library(fuzzyjoin)
library(lubridate)

knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      fig.height = 10,
                       fig.width = 12,
                     out.width = "90%")
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
theme_set(theme_minimal() + theme(legend.position = "bottom"))

```

```{r data,echo=FALSE}

### Loading the data

csprodir <- "P:\\PHIA Project\\SI\\Curated Files\\RD Congo2\\Main Survey\\1-CSPro Data\\Corrected Flat Files\\"
csprodate <- paste0(format(Sys.Date(), "%Y%m%d"))

## Individual dataset
indiv_df223 = readxl::read_xlsx(
  #paste0(csprodir,
  #       "/PII_CD_FFcorr_ind_int_DRAFT_", csprodate, ".xlsx"), 
  paste0(csprodir,
         "/PII_CD_FFcorr_ind_int.xlsx"), 
  sheet = "Individual") %>% 
  janitor::clean_names() %>% 
  mutate(source = "indiv_df")

## Household dataset
hh_df223 = readxl::read_xlsx(
  #paste0(csprodir,
  #       "/PII_CD_FFcorr_hh_int_DRAFT_", csprodate, ".xlsx"), 
  paste0(csprodir,
         "/PII_CD_FFcorr_hh_int.xlsx"), 
  sheet = "Household",
  guess_max = 10000) %>% 
  janitor::clean_names() %>% 
  mutate(source = "hh_df")

## Roster dataset
roster_df223 = readxl::read_xlsx(
  #paste0(csprodir,
  #       "/PII_CD_FFcorr_roster_DRAFT_", csprodate, 
  #       ".xlsx"), 
  paste0(csprodir,
         "/PII_CD_FFcorr_roster.xlsx"), 
  sheet = "Roster",
  guess_max = 10000) %>% 
  janitor::clean_names() %>% 
  mutate(source = "roster_df")

## Second stage sample file
sstage_dir <- "P:/PHIA Project/SI/Curated Files/RD Congo2/Main Survey/0-Second stage sample/"

sss_df223 = readxl::read_xlsx(paste0(sstage_dir, "CODPHIA_second_stage_sampling_09_08_2024_ls.xlsx"),
                              guess_max = 10000) %>% 
  janitor::clean_names() 

## Select columns for Household dataset and format 
hh_df<- hh_df223 %>% select("province"=hhi_province,"city"=hhi_ct,"eacode"=hhi_eacode,hhi_shh,hhi_team_id,hhformstatus,hhqsts,hhqets,hhqenddt,hhageyears,hhelig,resultndt,hhconstat,startint,hhconf,hhi_uuid,hhqinshh,hhqinsend,hhi_deviceid,hhconstfdt,gv_string,hhqendcompl,hhqendsum,short_ea, staffidhh, hhi_endsurvey,dmflag) %>%
  mutate(shh = hhi_shh)

hh_df$eacode<-as.character(hh_df$eacode)
hh_df$province<-as.character(hh_df$province)
hh_df$city<-as.numeric(hh_df$city)
hh_df$hhconstat<-as.numeric(hh_df$hhconstat)
hh_df$hhi_team_id<-as.numeric(hh_df$hhi_team_id)
hh_df$hhi_shh<-as.character(hh_df$hhi_shh)

## Discard all records marked for discard in the change log
hh_df <- hh_df %>%
  filter(dmflag != "DISCARD" | is.na(dmflag)) %>%
  mutate(short_ea = ifelse(hhi_shh	== "CD181021010035215" & eacode == "CD1810210100352", "ZD_HK_04", short_ea))

## Remove the alphabetic characters from the variables
hh_df$eacode <- str_sub(hh_df$eacode, 3, -1)
hh_df$hhi_shh <- str_sub(hh_df$hhi_shh, 3, -1)
hh_df$shh <- str_sub(hh_df$shh, -2, -1)

## Save the dataset without removing the duplicates
hh_df_dup <- hh_df

## Select columns for Individual dataset and format 
indiv_df<-indiv_df223%>% select("province"=ind_province,"city"=ind_ct,"eacode"=ind_eacode,ind_shh,ind_team_id,constat,ind_hhi_uuid,indformedt,ind_surveyformsdt,indformsdt,indformstatus,bio3sdt,bio3edt,ltchow,coltypead,hivstatus,indstatus,confageysource,confagey, confgend,ind0040,strtinstr,ind_surveyformsdt,inconbio,asybiogt,pprmbio,ea_hhid_ln_fixed,ea_hhid_fixed,primary_roster_status,ageyears,adqxsdt,adqxedt,specdate,endmsg1,indstatus,ltcvrbl,biohivtst1rs,biohivtst1rs2,biohivtst2rs,biohivtst2rs2,biohivtst3rs,biohivtst3rs2,biohivtst1ars,biohivtst1ars2,short_ea,endsurvey,informstatusr, indivstaffid, biostaffid, ind_team_id, ptids, acclang, ppchelangacc, chelangacc,dmflag) %>%
   mutate("eacode_num"=str_sub(eacode,3,-1))

indiv_df$asybiogt<-as.numeric(indiv_df$asybiogt)
indiv_df$inconbio<-as.numeric(indiv_df$inconbio)
indiv_df$pprmbio<-as.numeric(indiv_df$pprmbio)
indiv_df$eacode<-as.character(indiv_df$eacode)
indiv_df$province<-as.character(indiv_df$province)
indiv_df$city<-as.numeric(indiv_df$city)
indiv_df$ind_shh<-as.character(indiv_df$ind_shh)
indiv_df$ind_team_id<-as.numeric(indiv_df$ind_team_id)
indiv_df$confagey<-as.numeric(indiv_df$confagey)
indiv_df$ageyears<-as.numeric(indiv_df$ageyears)
indiv_df$strtinstr<-as.numeric(indiv_df$strtinstr)

## Discard all records marked for discard in the change log
indiv_df <- indiv_df %>%
  filter(dmflag != "DISCARD" | is.na(dmflag))

## Select columns for Roster dataset (all) and format 
roster_df<-roster_df223
roster_df$hhi_eacode<-as.character(roster_df$hhr_eacode)
roster_df$eacode<-roster_df$hhi_eacode
roster_df$province<-as.character(roster_df$hhr_province)
roster_df$city<-as.numeric(roster_df$hhr_ct)
roster_df$ageyears<-as.numeric(roster_df$ageyears)
roster_df$eligible1<-as.numeric(roster_df$eligible1)
roster_df$rostered_final_status<-as.numeric(roster_df$rostered_final_status)
roster_df$hhi_shh <- as.character(roster_df$hhr_shh)
roster_df$hhi_uuid <- as.character(roster_df$hhr_hhi_uuid)
roster_df$hhi_team_id <- as.numeric(roster_df$hhr_team_id)
roster_df$hhi_deviceid <- as.character(roster_df$hhr_deviceid)
roster_df$hhi_province <- as.character(roster_df$hhr_province)
roster_df$hhi_ct <- as.character(roster_df$hhr_ct)

## Remove the alphabetic characters from the variables
roster_df <- roster_df %>%
  mutate("eacode_num"=str_sub(hhr_eacode,3,-1))

## Discard all records marked for discard in the change log
roster_df <- roster_df %>%
  filter(dmflag != "DISCARD" | is.na(dmflag))

## Select columns for 2nd Stage Sample dataset and format 
sss_df<-sss_df223%>% select("province"=id_province,hhid,ea_id1,ea_id,hhs,n_e_as,target_no_e_as,easize,segment,segmentsize,target,"eacode"=ea_code,geovar_string,geovar_short,hh_id,uuid,teamid1,enumerator_id,instlisting4,listingsdt,listingformsdt,listingformend,listingformedt,segmentselect,totalhh_lst,lowhh50,lowhh50reason,stobs,hhnameo,hhsize,code_dpa,sampled_hh,csproid) %>% 
  mutate("city"=substr(code_dpa,1,5))

## Define the start position and length of substring to remove
start_pos <- 6
length_substring <- 4

# Function to remove the substring from the EA code 
 remove_substring <- function(s, start, length) {
   before_substring <- substr(s, 1, start - 1)
   after_substring <- substr(s, start + length, nchar(s))
   paste0(before_substring, after_substring)
 }

# Apply the function to the column using lapply
sss_df$eacode <- unlist(lapply(sss_df$eacode, remove_substring, start = start_pos, length = length_substring))
sss_df$ea_id1<-as.character(sss_df$ea_id1)
sss_df$eacode<-as.character(sss_df$eacode)
sss_df$province<-as.character(sss_df$province)
sss_df$city<-as.numeric(sss_df$city)
sss_df$hh_id<-as.character(sss_df$hh_id)

# ## IDENTIFY DUPLICATE LINE NUMBERS WITHIN A HOUSEHOLD
# duplicated_ln <- roster_df %>%
#   group_by(ea_hhid_fixed) %>%
#   mutate(duplicate = if_else(duplicated(ea_hhid_ln_fixed) | duplicated(ea_hhid_ln_fixed, fromLast = TRUE), "Duplicate", "Unique")) %>%
#   ungroup()
# duplicates_only <- duplicated_ln %>%
#   filter(duplicate == "Duplicate")
# rio::export(list("data"= duplicates_only), paste0("./results/", "CODPHIA_Duplicate_EA_HHID_LN_FIXED_", format(Sys.Date()-1, "%d%b%Y"),".xlsx"))

```

```{r}
### merging the datasets
mydata_dup = roster_df %>%
 full_join(hh_df, by = c("province","city", eacode_num = "eacode", "hhi_shh", "hhi_uuid", "hhi_team_id", "hhi_deviceid")) %>%  full_join(indiv_df, by = c("province", "city", "eacode", "hhi_shh" = "ind_shh", "hhi_uuid" = "ind_hhi_uuid", "ea_hhid_fixed", "ea_hhid_ln_fixed", "hhi_team_id" = "ind_team_id", "ageyears"))


## de-duplicate

# HH - hhi_endsurvey
hh_df <- hh_df %>%
  mutate(completeness = case_when(!is.na(hhi_endsurvey) ~ 1, # if hhi_endsurvey is not blank, assign 1 = complete
                                  .default = 0)) %>% # otherwise 0 = incomplete
  group_by(hhi_shh) %>%
  filter(!(completeness == 0 & any(completeness == 1))) %>% # filter for duplicates (based on hhi_shh) where one is 1 = complete and one/more is/are 0 = incomplete
  arrange(hhi_shh, hhqsts) %>% # arrange in order by hhi_shh and hhqsts
  filter(row_number() == 1 |
         !(duplicated(hhi_shh) | duplicated(hhi_shh, fromLast = TRUE))) %>% # if duplicates exist, discard the latter one(s)
  ungroup()

# IND - endsurvey (same approach as above - cp "HH - hhi_ensurvey")
indiv_df <- indiv_df %>%
  mutate(completeness = case_when(endsurvey == "A" ~ 1,
                                  .default = 0)) %>%
  group_by(ea_hhid_ln_fixed) %>%
  filter(!(completeness == 0 & any(completeness == 1))) %>%
  # group_by(ea_hhid_ln_fixed) %>%
  arrange(ea_hhid_ln_fixed, indformsdt) %>%
  filter(row_number() == 1 | 
         !(duplicated(ea_hhid_ln_fixed) | duplicated(ea_hhid_ln_fixed, fromLast = TRUE))) %>%
  ungroup()

# Roster - hhi_endsurvey (same approach as above - cp "HH - hhi_ensurvey")
roster_df <- roster_df %>%
  mutate(completeness = case_when(!is.na(hhi_endsurvey) ~ 1,
                                  .default = 0)) %>%
  group_by(ea_hhid_ln_fixed) %>%
  filter(!(completeness == 0 & any(completeness == 1))) %>%
  arrange(hhrets) %>%
  filter(row_number() == 1 | 
         !(duplicated(ea_hhid_ln_fixed) | duplicated(ea_hhid_ln_fixed, fromLast = TRUE))) %>%
  ungroup()

## Join datasets (roster > household > individual)
mydata = roster_df %>%
 left_join(hh_df, by = c("province","city", eacode_num = "eacode", "hhi_shh", "hhi_uuid", "hhi_team_id", "hhi_deviceid")) %>%  left_join(indiv_df, by = c("province", "city", "eacode", "hhi_shh" = "ind_shh", "hhi_uuid" = "ind_hhi_uuid", "ea_hhid_fixed", "ea_hhid_ln_fixed", "hhi_team_id" = "ind_team_id", "ageyears")) 

```


## 1a EA OVERALL MONITOR REPORT

```{r 1a1, echo=FALSE,warning=FALSE,message=FALSE}

## List of all EAs from sample file
listofeas <- sss_df %>% 
  select(ea_id1) %>% 
  distinct(ea_id1)

## Target number of households - n. of hh selected per ea in second stage sample file
targethh <- sss_df %>% 
  count(ea_id, hhid) %>% 
  count(ea_id) %>%
  dplyr::rename("Target Number of HHs" = n, "eacode" = ea_id) %>% 
  mutate(eacode = as.character(eacode))

## Actual households for which data was collected (use non-dupe dataset) - merge sample file with hh dataset (if hh is in both, it is counted)
actualhh <- hh_df %>% 
  left_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>%
  group_by(eacode)  %>% 
  summarize(n = sum(n_distinct(hhi_shh))) %>% # count distinct hhi_shh
  dplyr::rename("Actual Number of Collected HHs" = n)

## Actual collected forms - by hhi_uuid (use non-dupe dataset) - number of records in the hh dataset by ea
actualcforms <- hh_df %>% 
  group_by(eacode) %>%
  filter(!is.na(hhqsts)) %>% # for a form to be counted: hhqsts is not null
  summarize(n = sum(n_distinct(hhi_uuid))) %>% 
  dplyr::rename("Actual Number of Collected Forms" = 2)

## Number of duplicate forms in the hh dataset for each ea based on dupe hh dataset
duplicatehh <- hh_df_dup %>% 
  filter(!is.na(hhqets)) %>% # for a form to be counted: hhqsts is not null
  group_by(eacode) %>% 
  summarize(n = NROW(hhi_shh),
            nh = n_distinct(hhi_shh)) %>% 
  group_by(eacode) %>%
  summarize(countn = (n - nh)) %>% 
  mutate(countn = case_when(countn < 0 ~ 0,
                            TRUE ~ as.numeric(countn))) %>%
  dplyr::rename("Count of Duplicate Forms" = countn)

## Number of missing hhs in an ea (ongoing eas only) - target number of hhs minus actual number of hhs
missingforms <- targethh %>%
  left_join(actualhh, by = ("eacode")) %>%
  mutate("Number of HHs missing a form" = `Target Number of HHs` - `Actual Number of Collected HHs`)

mydata_mf <- missingforms %>% 
  left_join(actualcforms, by = ("eacode")) %>% # join datasets of missing forms to other data above
  mutate("Outstanding HHs (Target - Actual)" = `Target Number of HHs`- `Actual Number of Collected HHs`) %>%
  left_join(duplicatehh, by = ("eacode"))
  
## Number of eligible individuals age 15 and older
eligibledat <- roster_df %>%
  mutate(eligible = case_when(eligible1 == 1 & ageyears >= 15 ~ "eligible", 
                              .default = "not eligible")) %>% # create column for "eligible" and "not eligible"
  select("province" = hhi_province, "city" = hhi_ct, "eacode" = eacode_num, hhi_shh,hhi_team_id, eligible, ageyears) %>% 
  filter(eligible == "eligible", ageyears >= 15) %>% # for an individual to be counted: eligible1 = 1 and ageyears >= 15
  group_by(eacode) %>% 
  count(eligible) %>% 
  dplyr::rename("Count of Eligibles (15+ years)"=n) %>% 
  select(1,3)

## Number of individuals eligible for blood draws
blooddrawdat <- indiv_df %>%
  mutate(eligible = ifelse(inconbio == 1 | (pprmbio == 1 & asybiogt == 1), "eligible", "not eligible")) %>% # for an individual to be counted: inconbio = 1 or (pprmbio = 1 and asybiogt = 1)
  select(province, city, "eacode" = eacode_num, ind_shh, ind_team_id, eligible, ageyears) %>% 
  group_by(eacode) %>% 
  filter(eligible == "eligible", ageyears >= 15) %>% 
  count(eligible) %>%
  dplyr::rename("Count of Consents to Blood Draw (15+ years)" = n) %>% 
  select(1, 3)

### Pull dates of first form and last form (start date)
dates <- data.frame("firstformstartdate" =  aggregate(hhqsts~eacode, hh_df, FUN = function(x) min(x)), # earliest hhqsts
                    "lastformstartdate" =  aggregate(hhqsts~eacode, hh_df, FUN=function(x) max(x))) # latest hhqsts

## Select columns
datesdf <- dates %>% 
  select(1, 2, 4) %>%  
  dplyr:: rename("eacode" = 1, "Date of First Form" = 2, "Date of Last Form" = 3)

tt <- sss_df %>% 
  select(province, city, "eacode" = ea_id, geovar_string) %>% 
  distinct()

hh_df_gv <- hh_df %>%
  select(eacode, gv_string, short_ea) %>%
  distinct()

## Join above sub-datasets together by eacode
overall1adf <- tt %>% 
  left_join(mydata_mf, by = ("eacode")) %>% 
  left_join(datesdf, by = ("eacode")) %>%
  left_join(blooddrawdat, by = c("eacode")) %>%
  full_join(eligibledat, by = c("eacode")) %>%
  left_join(hh_df_gv, by = "eacode") %>%
  mutate(province = case_when(province == 18 ~ 'Haut-Katanga', # re-assign province names
                              province == 20 ~ 'Lualaba'))

## Format dates as dates
overall1adf$`Date of First Form` <- ymd_hms(overall1adf$`Date of First Form`)
overall1adf$`Date of Last Form` <- ymd_hms(overall1adf$`Date of Last Form`)

## Rename columns
overall1adf <- overall1adf %>%
  select("Province" = province, "ZD" = short_ea, "Geovar" = gv_string, "EA Code" = eacode, `Target Number of HHs`, 'Actual Number of Collected HHs', 'Actual Number of Collected Forms', 'Count of Duplicate Forms', 'Outstanding HHs (Target - Actual)', 'Count of Eligibles (15+ years)', 'Count of Consents to Blood Draw (15+ years)', 'Date of First Form', 'Date of Last Form')

overall1adf
  
## Export report
rio::export(list("data"= overall1adf), paste0("./results/", "1a-CODPHIA_EA Overall Report_", format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 1b HH FORMS NOT RECEIVED

```{r 1a2, echo=FALSE,warning=FALSE,message=FALSE}

## Format hhids as 'HHID000'
hh_df$hhid <- paste0("HHID", sprintf("%03d", as.numeric(hh_df$shh)))
sss_df$hhid_cspro <- paste0("HHID", sprintf("%03d", as.numeric(sss_df$csproid)))

## Joining the sample file to the hh file - filter the ones where we have no collected hh information
b1 <- sss_df %>%
  left_join(hh_df, by = c("ea_id" = "eacode", "hhid_cspro" = "hhid")) %>%
  filter(is.na(hhi_shh)) %>% # no hh info available, meaning hh not done yet
  group_by(ea_id) %>%
  summarize(`Count of Missing HHs_DEL` = n_distinct(as.numeric(csproid)),
            `Missing HH IDs` = paste(unique(hhid_cspro), collapse = ", "))

## Format the hh dataset, summarize the variables by eacode/short_ea
b2 <- hh_df %>%
  group_by(eacode, short_ea) %>%
  summarize(`Team IDs` = paste(unique(hhi_team_id), collapse = ", "), `Date Started` = min(hhqsts), `Form End Date` = max(hhqsts), .groups = 'drop') %>%
  mutate(`Data Received In Past 7 Days` = case_when(`Form End Date` > Sys.Date() - 7 ~ "Yes",
                                                    .default = "No")) %>%
  rename(`EA Code Long` = eacode,
         `EA Code Short` = short_ea) %>%
  distinct()

## Join b1 and b2 datasets to get the list of hh forms not received with required columns
b3 <- b1 %>%
  left_join(b2, by = c("ea_id" = "EA Code Long")) %>%
  filter(!is.na(`Team IDs`)) %>%
  separate_longer_delim(`Missing HH IDs`, delim = ", ") %>%
  group_by(ea_id, `EA Code Short`) %>%
  mutate(`Count of missing HHs` = row_number(`Missing HH IDs`)) %>%
  select(`EA Code Long` = ea_id, `EA Code Short`, `Team IDs`, `Count of missing HHs`, `Missing HH IDs`, `Date Started`, `Form End Date`, `Data Received In Past 7 Days`)

b3

## Export report
rio::export(list("data"= b3), paste0("./results/", "1b-CODPHIA_HH Forms Not Received_", format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 1c EA MONITOR TOOL

```{r 1c, echo=FALSE,warning=FALSE,message=FALSE}

## Dates - select columns from hh datasets and assign rowids
datt <- hh_df %>% 
  select(eacode, hhqsts, hhqets, hhqenddt, province, city, shh) %>% 
  mutate("rowid" = seq(1, nrow(hh_df))) %>% 
  ungroup()

datt2 <- sss_df %>% 
  select(eacode, province, city, ea_id1, teamid1) %>% 
  mutate("rowid" = 1:nrow(sss_df)) %>% 
  ungroup()

## Format dates in ymd_hms format
datt$hhqsts <- ymd_hms(datt$hhqsts)
datt$hhqets <- ymd_hms(datt$hhqets)
datt$hhqenddt <- ymd_hms(datt$hhqenddt)

## If hhqets and hhqenddt were missing for end date then;
fallback_order <- c("hhqsts", "hhqenddt","hhqets" )

is_all_na <- function(column) {
  all(is.na(column))
}

# Initialize the result column
datt$filled_column <- NA

# Loop through the fallback order
for (col_name in fallback_order) {
  if (!is_all_na(datt[[col_name]])) {
    datt$filled_column <- datt[[col_name]]
    break
  }
} 

dates <- data.frame("startdate" = aggregate(hhqsts~eacode, datt, FUN = function(x) min(x)), # earliest date per ea
                    "enddate" = aggregate(filled_column~eacode, datt, FUN = function(x) max(x))) # latest date per ea

datesdf <- dates %>% 
  select(1, 2, 4) %>% 
  dplyr:: rename("eacode" = 1, "Date Started" = 2, "Date Completed" = 3)

## Format date as date/time
datesdf$`Date Started` <- as_datetime(datesdf$`Date Started`)
datesdf$`Date Completed` <- as_datetime(datesdf$`Date Completed`)

## Use hh dataset joint with sample file to get the number of expected hhs by ea
hh_select = hh_df %>% 
  full_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>%
  group_by(province, city, eacode) %>%
  summarize(n = first(sampled_hh)) %>% 
  dplyr:: rename("Expected Number of HHs" = n) %>% 
  mutate(`Expected Number of HHs` = replace_na(`Expected Number of HHs`, 0))

## Use hh dataset with duplicates to get number of duplicates by ea - total count total number of completed hh forms including duplicates
hh_actual_dup = hh_df_dup %>% 
  group_by(province, city, eacode) %>% 
  filter(!is.na(hhqsts) & (!is.na(hhqets) | !is.na(hhqenddt))) %>% # for a form to be counted as completed: hhqsts is not null and (hhqets or hhqenddt) is not null
  summarize(n = n()) %>% 
  dplyr:: rename("Total Number of HH Forms (Including Duplicates)" = n) %>% 
  mutate(`Total Number of HH Forms (Including Duplicates)` = replace_na(`Total Number of HH Forms (Including Duplicates)`, 0))

## Use hh dataset without duplicates to get number of forms received by ea - unique count total number of completed ea forms
hh_actual_non_dup = hh_df %>% 
  group_by(province,city,eacode) %>% 
  filter(!is.na(hhqsts) & (!is.na(hhqets) | !is.na(hhqenddt))) %>% # for a form to be counted as completed: hhqsts is not null and (hhqets or hhqenddt) is not null
  summarize(n = n_distinct(hhi_shh)) %>% 
  dplyr:: rename("Number of Unique HH Forms" = n) %>% 
  mutate(`Number of Unique HH Forms` = replace_na(`Number of Unique HH Forms`, 0))

## Join dupes and non-dupes datasets created above and format / rename columns plus create columns for overall status columns
myhh_df = hh_select %>% 
  left_join(hh_actual_dup, by = c("province", "city", "eacode")) %>%
  left_join(hh_actual_non_dup, by = c("province", "city", "eacode")) %>%
  mutate(
    `Total Number of HH Forms (Including Duplicates)` = replace_na(`Total Number of HH Forms (Including Duplicates)`, 0),
    `Number of Unique HH Forms` = replace_na(`Number of Unique HH Forms`, 0),
    `Expected Number of HHs` = replace_na(`Expected Number of HHs`, 0),
    "HH Interviews Complete?" = case_when(
      `Number of Unique HH Forms` == 0 & `Total Number of HH Forms (Including Duplicates)` == 0 ~ 'Not Started',
      (`Expected Number of HHs` - `Number of Unique HH Forms` == 0) & `Expected Number of HHs` - `Total Number of HH Forms (Including Duplicates)` == 0 ~ 'Complete',
      `Expected Number of HHs` - `Number of Unique HH Forms` == 0 & `Expected Number of HHs` < `Total Number of HH Forms (Including Duplicates)` ~ 'Complete with Duplicates',
      `Expected Number of HHs` > `Number of Unique HH Forms` ~ 'Incomplete',
      `Expected Number of HHs` < `Number of Unique HH Forms` ~ 'Complete with Data Issues',
      .default = 'Not Started')) # determines completeness of interviews

## Use roster dataset to get number of rostered eligible inds per hh
ind_results1 = roster_df %>% 
  group_by(province, city, "eacode" = eacode_num) %>%
  reframe("Number of Eligible Roster Members" = n_distinct(ea_hhid_ln_fixed[which(eligible1 == 1) ])) %>% # rostered ind eligible: eligible1 = 1
  mutate(`Number of Eligible Roster Members` = replace_na(`Number of Eligible Roster Members`, 0))

## Use joined dataset to get numbers on completed ind interviews (with and without duplicates) and testing consent
ind_results = mydata %>% 
  group_by(province, city, eacode) %>%
  reframe("Number of Unique IND Interviews" = n_distinct(ea_hhid_ln_fixed[which(!is.na(adqxedt) | !is.na(ind0040) | indstatus != 0)]), # final result to be counted: adqxedt not missing or ind0040 not missing or indstatus not = 0
          "Total Number of IND Interviews (Including Duplicates)" = length(ea_hhid_ln_fixed[which(!is.na(adqxedt) | !is.na(ind0040) | indstatus != 0)]), # final result to be counted: adqxedt not missing or ind0040 not missing or indstatus not = 0
          "Number of Individuals who consented to testing" = n_distinct(ea_hhid_ln_fixed[which(inconbio == 1 | asybiogt == 1)]), # hiv eligible: inconbio or asybiogt = 1
          "Number of Individuals who completed testing" = n_distinct(ea_hhid_ln_fixed[which(!is.na(hivstatus) & (inconbio == 1 | asybiogt == 1))])) %>% # result counted: hivstatus not missing and (inconbio or asybiogt) = 1
  mutate("eacode_num"=str_sub(eacode,3,-1)) %>%
  select(-c(`eacode`)) %>%
  rename(eacode = eacode_num) %>%
  right_join(ind_results1, by = c("province", "city", "eacode")) %>%
  filter(!is.na(eacode)) %>% 
  arrange(eacode)  %>%
  full_join(myhh_df, by = c("province", "city", "eacode")) %>%
  mutate(`Number of Unique IND Interviews` = replace_na(`Number of Unique IND Interviews`, 0)) %>% # replace nas with 0
  mutate(`Total Number of IND Interviews (Including Duplicates)` = replace_na(`Total Number of IND Interviews (Including Duplicates)`, 0)) %>% # replace nas with 0
  mutate(`Number of Individuals who consented to testing` = replace_na(`Number of Individuals who consented to testing`, 0)) %>% # replace nas with 0
  mutate(`Number of Individuals who completed testing` = replace_na(`Number of Individuals who completed testing`, 0)) %>% # replace nas with 0
  mutate(`Number of Eligible Roster Members` = replace_na(`Number of Eligible Roster Members`, 0)) %>% # replace nas with 0
  group_by(province,city,eacode,`Number of Unique IND Interviews`,`Total Number of IND Interviews (Including Duplicates)`,`Number of Individuals who consented to testing`,`Number of Individuals who completed testing`,`Number of Eligible Roster Members`) %>%
  mutate(`Number of Eligible Roster Members` = replace_na(`Number of Eligible Roster Members`, 0)) %>%
  reframe("Interview Complete?" = case_when( # adding column for overall completion of ind interviews
    `Number of Unique IND Interviews` == 0 & `Total Number of IND Interviews (Including Duplicates)` ==0 ~ 'Not Started',    
    `Number of Unique IND Interviews` - `Number of Eligible Roster Members` == 0 & `Number of Eligible Roster Members` - `Total Number of IND Interviews (Including Duplicates)` == 0 ~ 'Complete',
    `Number of Unique IND Interviews` - `Number of Eligible Roster Members` == 0 & `Number of Eligible Roster Members` < `Total Number of IND Interviews (Including Duplicates)` ~ 'Complete with Duplicates',
    `Number of Eligible Roster Members` > `Number of Unique IND Interviews` ~ 'Incomplete',
    `Number of Unique IND Interviews` > `Number of Eligible Roster Members` ~ 'Complete with Data Issues',
    .default = 'Not Started'),
    "Testing Complete?" = case_when( # adding column for overall completion of testing
      `Number of Individuals who completed testing` == 0 ~ 'Not Started',
      `Number of Individuals who consented to testing` - `Number of Individuals who completed testing` == 0 ~ 'Complete',
      `Number of Individuals who consented to testing` < `Number of Individuals who completed testing` ~ 'Complete with Duplicates',
      `Number of Individuals who consented to testing` > `Number of Individuals who completed testing` ~ 'Incomplete',
      `Number of Individuals who consented to testing` > `Number of Individuals who completed testing` ~ 'Complete with Data Issues',
      .default = 'Not Started'))
    
## Join datasets created above, create column for overall ea status
overall_df1c_start = myhh_df %>%
  left_join(datesdf, by = ("eacode")) %>% 
  left_join(ind_results, by = c("province","eacode")) %>% 
  filter(!is.na(eacode)) %>% 
  arrange(eacode)  %>%
  mutate("Overall EA Status" = case_when(
    `HH Interviews Complete?` == "Not Started" & `Interview Complete?` == "Not Started" & `Testing Complete?`  == "Not Started" ~ "Not Started",
    `HH Interviews Complete?` %in% c("Complete", "Complete with Duplicates", 'Complete with Data Issues') & `Interview Complete?` %in% c("Complete", "Complete with Duplicates", 'Complete with Data Issues') & `Testing Complete?` %in% c("Complete", "Complete with Duplicates", 'Complete with Data Issues') ~ "Complete",
                              `HH Interviews Complete?` == "Incomplete" | `Interview Complete?` == "Incomplete" | `Testing Complete?` == "Incomplete" ~ "Incomplete",
                              .default = "Not Started"),
        "Duration (Days)" = round(difftime(`Date Completed`,`Date Started`, units = "days"))) # duration (days)

## Select distinct columns from hh dataset
overall_df1c_hh <- hh_df %>%
  select(eacode, gv_string, hhi_team_id, short_ea) %>%
  distinct()

## Join datasets from above, format, and rename columns
overall_df1c <- overall_df1c_start %>%
  left_join(overall_df1c_hh, by = "eacode") %>%
  relocate(gv_string, .after = province) %>%
  rename(Province = province,
         Geovar = gv_string,
         'EA Code' = eacode) %>%
  mutate(Province = case_when(Province == 18 ~ 'Haut-Katanga',
                              Province == 20 ~ 'Lualaba')) %>% # re-assign province names
  select(hhi_team_id, 
         # added GR
         "ZD" = short_ea,
         'EA Code',
         Province:Geovar, 'Date Started', 'Date Completed', 'Duration (Days)', 'Expected Number of HHs', 'Number of Unique HH Forms', 'Total Number of HH Forms (Including Duplicates)', 'HH Interviews Complete?', 'Number of Eligible Roster Members', 'Number of Unique IND Interviews', 'Total Number of IND Interviews (Including Duplicates)', 'Interview Complete?', 'Number of Individuals who consented to testing', 'Number of Individuals who completed testing', 'Testing Complete?', `Overall EA Status`)

## Group dataset by eacode, summarize where needed using first occurrence of variable
overall_df1c <- overall_df1c %>%
  group_by(`EA Code`) %>%
  summarize (hhi_team_id = paste(hhi_team_id, collapse = ", "),
             ZD = first(ZD),
             Province = first(Province),
             Geovar = first(Geovar),
             `Date Started` = first(`Date Started`),
             `Date Completed` = first(`Date Completed`),
             `Duration (Days)` = first(`Duration (Days)`),
             `Expected Number of HHs` = first(`Expected Number of HHs`),
             `Number of Unique HH Forms` = first(`Number of Unique HH Forms`),
             `Total Number of HH Forms (Including Duplicates)` = first(`Total Number of HH Forms (Including Duplicates)`),
             `HH Interviews Complete?` = first(`HH Interviews Complete?`), 
             `Number of Eligible Roster Members` = first(`Number of Eligible Roster Members`), 
             `Number of Unique IND Interviews` = first(`Number of Unique IND Interviews`),
             `Total Number of IND Interviews (Including Duplicates)` = first (`Total Number of IND Interviews (Including Duplicates)`),
             `Number of Individuals who consented to testing` = first(`Number of Individuals who consented to testing`), 
             `Number of Individuals who completed testing` = first(`Number of Individuals who completed testing`), 
             `Interview Complete?` = first(`Interview Complete?`), 
             `Testing Complete?` = first(`Testing Complete?`), 
             `Overall EA Status` =  first(`Overall EA Status`)) %>%
  rename(`Team ID` = hhi_team_id) %>%
  relocate(`EA Code`, .after = 4) %>%
  relocate(`Interview Complete?`, .after = 15)

overall_df1c

## Export report
rio::export(list("data"= overall_df1c), paste0("./results/", "1c-CODPHIA_EA Monitor Tool_", format(Sys.Date(), "%d%b%Y"), ".xlsx"))

```

## 1d HH ROSTER MONITOR REPORT

```{r 1d, echo=FALSE,warning=FALSE,message=FALSE}

## Use hh dataset, select columns, format hhid, and convert column resultndt to text
d1 <- hh_df %>%
  select(`EA Code Long` = eacode, `EA Code Short` = short_ea, HHID = hhi_shh, HHQSTS = hhqsts, `Team ID` = hhi_team_id, `Staff ID` = staffidhh, `Tablet ID` = hhi_deviceid, ResultNDT = resultndt,startint) %>%
  mutate(`HH Number` = str_sub(HHID, 16, -1)) %>%
  mutate(RESULTNDT_DISPLAY = case_when(ResultNDT == 1 ~ "HOUSEHOLD NOT AVAILABLE AT ALL VISIT ATTEMPTS",
                                       ResultNDT == 2 ~ "REFUSED",
                                       ResultNDT == 3 ~ "DWELLING VACANT OR ADDRESS NOT A DWELLING",
                                       ResultNDT == 4 ~ "DWELLING DESTROYED",
                                       ResultNDT == 5 ~ "DWELLING NOT FOUND",
                                       ResultNDT == 6 ~ "HOUSEHOLD ABSENT FOR EXTENDED PERIOD OF TIME",
                                       ResultNDT == 96 ~ "OTHER",
                                       .default = "HH COLLECTED")) %>%
  relocate(`HH Number`, .after = 2) %>%
  select(-c("startint", "ResultNDT"))

d1

## Export report
rio::export(list("data"= d1), paste0("./results/", "1d-CODPHIA_HH Roster Monitor Report_", format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 1e INDIVIDUAL INTERVIEW MONITOR REPORT

```{r 1e, echo=FALSE,warning=FALSE,message=FALSE}

## Use dupe dataset, select columns, and create columns for relationship, indstatus, and rosteredfinalstatus in text
e1 <- mydata_dup %>%
  select(`EA Code Long` = eacode_num.x, `EA Code Short` = short_ea, `HHID` = hhi_shh, `EA HHID LN FIXED` = ea_hhid_ln_fixed, PTID = ptids, `IND Start Date` = indformsdt, `Team ID` = hhi_team_id, `Staff ID (Interview)` = indivstaffid, `Staff ID (Blood)` = biostaffid, `Tablet ID` = hhr_deviceid, `LineNum` = hhr_hhmem_id, Sex = sex, `Age in Years` = ageyears, RELATTOHH = relattohh, indstatus, `Rostered Final Status` = rostered_final_status) %>%
  mutate(`HH Number` = str_sub(HHID, 16, -1)) %>%
  mutate(`RELATTOHH_DISPLAY` = 
           case_when(RELATTOHH == 1 ~ "HEAD",
                     RELATTOHH == 2 ~ "WIFE/HUSBAND/PARTNER",
                     RELATTOHH == 3 ~ "SON OR DAUGHTER",
                     RELATTOHH == 4 ~ "SON-IN-LAW/DAUGHTER-IN-LAW",
                     RELATTOHH == 5 ~ "GRANDCHILD",
                     RELATTOHH == 6 ~ "PARENT",
                     RELATTOHH == 7 ~ "PARENT-IN-LAW",
                     RELATTOHH == 8 ~ "BROTHER/SISTER",
                     RELATTOHH == 9 ~ "CO-WIFE",
                     RELATTOHH == 10 ~ "OTHER RELATIVE",
                     RELATTOHH == 11 ~ "ADOPTED/FOSTER/STEPCHILD",
                     RELATTOHH == 12 ~ "NOT RELATED",
                     RELATTOHH == -8 ~ "DON'T KNOW",
                     .default = "DON'T KNOW")) %>%
  mutate(`INDSTATUS_DISPLAY` = 
           case_when(indstatus == 0 ~ "NOT STARTED",
                     indstatus == 11 ~ "STARTED INTERVIEW-COMPLETED BIOMARKER",
                     indstatus == 10 ~ "STARTED INTERVIEW-DID NOT COMPLETE BIOMARKER",
                     indstatus == 30 ~ "REASSIGNED",
                     indstatus == 40 ~ "WITHDRAWN",
                     indstatus == 21 ~ "CANNOT COLLECT DATA-NOT AVAILABLE AFTER ALL VISIT ATTEMPTS",
                     indstatus == 22 ~ "CANNOT COLLECT DATA-REFUSED",
                     indstatus == 23 ~ "CANNOT COLLECT DATA-INCAPACITATE",
                     indstatus == 29 ~ "CANNOT COLLECT DATA-OTHER",
                     .default = ""
                     )) %>%
  mutate("Rostered Final Status Display" = 
           case_when(`Rostered Final Status` == 1 ~ "Expected and received complete IND record",
                     `Rostered Final Status` == 2 ~ "Expected but did not receive any IND record",
                     `Rostered Final Status` == 3 ~ "Not selected for collection",
                     `Rostered Final Status` == 4 ~ "Expected but received incomplete IND record",
                     .default = "NA")) %>%
  relocate(`HH Number`, .after = 3) %>%
  select(-c("RELATTOHH", "indstatus", "Rostered Final Status"))

## Create list of all eas
list_ea <- hh_df %>%
  select(eacode, short_ea) %>%
  distinct()

## Join list of all eas to dataset above and re-order columns, add sex (text), add status for rostered final status display
e1 <- e1 %>%
  left_join(list_ea, by = c("EA Code Long" = "eacode")) %>%
  select(-("EA Code Short")) %>%
  rename("EA Code Short" = short_ea) %>%
  relocate(`EA Code Short`, .after = 1) %>%
  mutate(Sex = case_when(Sex == 1 ~ "Male",
                         Sex == 2 ~ "Female",
                         .default = NA)) %>%
  mutate(INDSTATUS_DISPLAY = case_when(`Rostered Final Status Display` == "Not selected for collection" ~ "NOT ELIGIBLE FOR INDIVIDUAL INTERVIEW",
                                       `Rostered Final Status Display` == "Expected but did not receive any IND record" ~ "NOT STARTED",
                                       .default = `INDSTATUS_DISPLAY`))

e1

## Export report
rio::export(list("data"= e1), paste0("./results/", "1e-CODPHIA_Individual Interview Monitor Report_", format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 1f MISSING IND INTERVIEW NOT DONE

```{r 1f, echo=FALSE,warning=FALSE,message=FALSE}

## Create list with all hhi_shh and hhi_team_id
f1 <- mydata %>%
  select(hhi_shh, hhi_team_id)

## Create list with status, start date, completion dates
statusstartcompletion <- overall_df1c %>% 
  select(eacode = 'EA Code', 'Date Started', 'Date Completed', `Overall EA Status`)

## Select columns (+ rename) from roster dataset, 
f1 <- roster_df %>% 
  select(Province = province, eacode, `EA Code` = short_ea, "Team ID (Household)" = hhi_team_id, "HH form start date" = hhr_surveyformsdt, ea_hhid_ln_fixed, "lineNum" = hhr_hhmem_id, sex, "Age (Years)" = ageyears, relattohh, rostered_final_status, hhi_shh, "Household Start Date" = hhr_surveyformsdt, "Staff ID (Household)" = staffidhh) %>%
  mutate(hhi_shh = str_sub(hhi_shh, 15, -1)) %>% # substring hhid (remove characters)
  mutate("RELATTOHH Display" = # create display column for relationship
           ifelse(relattohh == 1, "Head of HH",
                  ifelse(relattohh == 2, "Wife/Husband/Partner", "Others")),
         "Rostered Final Status Displayed" = # create display column for rostered final status
           ifelse(rostered_final_status == 1,"Expected and received complete IND record",
                  ifelse(rostered_final_status == 2,"Expected but did not receive any IND record", 
                         ifelse(rostered_final_status == 3, "Not selected for collection",
                                ifelse(rostered_final_status == 4, "Expected but received incomplete IND record", "Not expected")))),
         eacode = str_sub(eacode, 3, -1)) %>% # substring eacode (remove characters)
  mutate(Province = case_when(Province == 18 ~ "Haut-Katanga", # re-assign names to provinces
                              Province == 20 ~ "Lualaba",
                              .default = "NA"))

## Join datasets together, rename columns
overall_1fdf <- f1 %>%
  left_join(statusstartcompletion, by = c("eacode")) %>%
  select(`EA Code`, `Overall EA Status`, `Household ID` = hhi_shh, 'Date First Form Received' = `Date Started`, 'Date Last Form Received' = `Date Completed`, `Household Start Date`, `Team ID (Household)`, `Staff ID (Household)`, EA_HHID_LN_FIXED = ea_hhid_ln_fixed, LineNum = lineNum, Sex = sex, `Age (Years)`, RELATTOHH = relattohh, `RELATTOHH Display`, `Rostered Final Status` = rostered_final_status, `Rostered Final Status Displayed`)

overall_1fdf

## exporting the report
rio::export(list("data"= overall_1fdf), paste0("./results/", "1f-CODPHIA_Missing IND Interview Report_", format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 1g COMPLETED HHs

```{r 1g, echo=FALSE,warning=FALSE,message=FALSE}

## Use dupe hh dataset, select columns
g1 <- hh_df_dup %>%
  select(`EA Code` = short_ea, HHID = hhi_shh, HHQSTS = hhqsts, `Team ID` = hhi_team_id, `Staff ID` = staffidhh, ResultNDT = resultndt, hhqets, hhqenddt, startint) %>%
  mutate(`HH Number` = str_sub(HHID, 16, -1)) %>% # substring hhid
  mutate(`HH Completed` = case_when(!is.na(HHQSTS) & (!is.na(hhqets) |
                                                      !is.na(hhqenddt)) ~ "HH Completed",
                                   .default = "HH Not Completed")) %>% # add completion of hh based on hhqsts and [hhqets or hhqenddt] - if HH.HHQSTS is not missing and (HH.HHQETS or HH.HHQENDDT) is not missing then the HH is considered complete. 
  group_by(`EA Code`) %>%
  mutate(`Serial Number` = row_number()) %>%
  relocate(`HH Number`, .after = 2) %>%
  relocate(`Serial Number`, .after = 1) %>%
  mutate(`HH End Date` = ifelse(is.na(hhqets), hhqenddt, hhqets)) %>%
  rename(`HH Start Date` = HHQSTS) %>%
  select(-c(hhqets, hhqenddt, startint, ResultNDT, HHID)) %>%
  filter(`HH Completed` == "HH Completed") %>%
  select(-(`HH Completed`)) %>%
  relocate(`HH End Date`, .after = 4)

## format date as dates
g1$`HH End Date` <- as.POSIXct(as.numeric(g1$`HH End Date`), format = "%Y-%m-%d %H:%M:%S")

g1

## Export dataset
rio::export(list("data"= g1), paste0("./results/", "1g-CODPHIA_Completed Households_", format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 2a OVERALL REPORT

```{r table e, echo=FALSE, message=FALSE, warning=FALSE}

### eas

## Input the targets - Total number of unique eas from second stage sample
e0=tibble(row_name = "[E0] Target", n = 132)

hh_df <- hh_df%>%
  distinct(hhi_shh, .keep_all = TRUE)

## Create dataset with hhs in progress per ea
e_df = hh_df %>% 
  mutate(hh_complete = if_else(!is.na(hhqsts) & (!is.na(hhqets) | !is.na(hhqenddt)), 1, 0)) %>% # households started/in progress: hhqsts not missing and (hhqets or hhqenddt) not missing
  full_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>% 
  group_by(eacode) %>% 
  reframe(yes = n_distinct(hhi_shh[which(hh_complete == 1)]),
          expected = sampled_hh,
          ratios = yes/expected) %>% # percentage of hhs started or in progress
  distinct()

e1 = e_df %>% 
  filter(ratios > 0 & ratios < 0.96) %>% # filter ratios (any ea where hhs started/in progress are below 96%)
  reframe(n = paste0(n_distinct(eacode), " (", format(round(as.numeric(n_distinct(eacode))/as.numeric(e0$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[E1] Started or In Progress With Collection (E4 + E5)")

## Create dataset with hhs all completed
e2 = e_df %>% 
  filter(ratios >= 0.96) %>% # filter ratios (any ea where hhs started/in progress are equal to or above 96%)
  reframe(n = paste0(n_distinct(eacode), " (", format(round(as.numeric(n_distinct(eacode))/as.numeric(e0$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% # calculate percentage of eas completed out of all eas
  mutate(row_name = "[E2] Completed, n(%)")

## Create dataset with average (median) no. of days to complete ea (only complete eas)
e3 = hh_df %>% 
  mutate(hh_complete = if_else(!is.na(hhqsts) & !is.na(coalesce(hhqets, hhqenddt)), 1, 0)) %>% # use first hhqsts and last hhqets or hhqenddt (if hhqets is not available)
  filter(hh_complete == 1) %>% # filter for complete eas only
  group_by(eacode) %>%
  mutate(start_date = min(lubridate::ymd_hms(hhqsts), na.rm=TRUE),
         end_date = max(lubridate::ymd_hms(coalesce(hhqets, hhqenddt)), na.rm=TRUE), 
         tt_comp = difftime(end_date, start_date, units = "days"), # calculate difference of start date and end date
         tt_comp = as.numeric(tt_comp)) %>% 
  ungroup() %>% 
  reframe(n = paste0(round(mean(tt_comp), 0), " days (", round(median(tt_comp), 0.1), " days)")) %>% # calculate median
  mutate(row_name = "[E3] Average (median) no. days to complete EAs")

## eas in progress (50-95% completed)
e4 = e_df %>% 
  filter(ratios>=0.5 & ratios<0.96)  %>% # filter for ratios
  reframe(n = paste0(n_distinct(eacode), " (", format(round(as.numeric(n_distinct(eacode))/as.numeric(e0$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% # calculate % of in progress out of all eas
  mutate(row_name = "[E4] In progress (50-95% HHs completed), n (%)")

## eas started (under 50% completed)
e5 = e_df %>% 
  filter(ratios>0 & ratios<0.5) %>% # filter for ratios
  reframe(n = paste0(n_distinct(eacode), " (", format(round(as.numeric(n_distinct(eacode))/as.numeric(e0$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% # calculate % of started out of all eas
  mutate(row_name = "[E5] Started (<50% HHs completed), n (%)")
  
## eas without data (not started)
e6 = e_df %>% 
  filter(ratios==0 | is.na(yes)) %>% # filter for ratio = 0
  reframe(n = paste0(n_distinct(eacode), " (", format(round(as.numeric(n_distinct(eacode))/as.numeric(e0$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% # calculate percentage of not started out of all eas
  mutate(row_name = "[E6] Not Started, n (%)")

## Create helper tibble with footnotes
e7 = tibble(row_name = c(NA, "Progress and completion of EAs is based off of completion of HH interviews and does not take into account completion of individual interviews and blood testing."), n = c(NA, NA))

## Combine all tables above
etable <- e0 %>% rbind(e1) %>% rbind(e2) %>% rbind(e3) %>% rbind(e4) %>% rbind(e5) %>% rbind(e6) %>% rbind(e7)
colnames(etable)<-c("EA","Statistic")

etable

```

```{r table h, echo=FALSE,warning=FALSE,message=FALSE}

### Households

## Household targets (total)
h0 = tibble(row_name = "[H0] Target", n = 4974) %>% 
  mutate(nh = paste0(n))

## Unique hhs
h1 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh)) %>%
  mutate(row_name = "[H1] Total HH Counts, n (% of Target) (1)",
         nh = paste0(n, " (", format(round((n/h0$n)*100, 1), nsmall = 1, trim = TRUE),"%)")) # calculate percentage of hhs interviewed out of all hhs

## Total number of eligible hhs
h2 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6))]), # hh considered eligible: hhconstat = 1 or = 2, or startint = 4, or resultndt = 1 or = 2 or = 5
            nh = paste0(n, " (", format(round(n/as.numeric(h1$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[H2] Eligible HHs, n (% of HH forms received) (2)")

h3 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & hhconstat == 1)]),
            nh = paste0(n, " (", format(round(n/as.numeric(h2$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [H3] Consented, n (% of eligible HHs)")

h4 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) &
                                           (resultndt == 2 | hhconstat == 2 | startint == 4))]),
            nh = paste0(n, " (", format(round(n/as.numeric(h2$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [H4] Refused, n (% of eligible HHs)")

h5 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 1)]),
            nh= paste0(n, " (", format(round(n/as.numeric(h2$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [H5] HH not available at all visit attempts, n (% of eligible HHs)")

h6 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 5)]),
            nh = paste0(n, " (", format(round(n/as.numeric(h1$n)*100, 1), nsmall = 1, trim = TRUE),"%)")) %>% 
  mutate(row_name = "[H6] Ineligible - Dwelling not found, n (% of HH forms received)")

h7 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 3)]),
            nh = paste0(n, " (", format(round(n/as.numeric(h1$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[H7] Ineligible - vacant/address not a dwelling, n (% of HH forms received)")

h8 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 6)]),
            nh = paste0(n, " (", format(round(n/as.numeric(h2$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [H8] Household absent for extended period of time, n (% of eligible HHs)")

h9 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 4)]),
            nh = paste0(n, " (", format(round(n/as.numeric(h1$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[H9] Ineligible - destroyed, n (% of HH forms received)")

h10 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 96)]),
            nh = paste0(n, " (", format(round(n/as.numeric(h1$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)")

h11 = tibble(row_name = c(999999, "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload."), n = c(999999, 999999))
  
h12 = tibble(row_name = "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", n = NA)

htable = bind_rows(!!!mget(paste0("h", seq(from = 0, to = 12)))) %>% 
  mutate(row_name = fct_relevel(row_name, "[H0] Target", after = 0L),
         row_name = fct_relevel(row_name, "[H1] Total HH Counts, n (% of Target) (1)", after = 1),
         row_name = fct_relevel(row_name, "[H2] Eligible HHs, n (% of HH forms received) (2)", after = 2),
         row_name = fct_relevel(row_name, "     [H3] Consented, n (% of eligible HHs)", after = 3),
         row_name = fct_relevel(row_name, "     [H4] Refused, n (% of eligible HHs)", after = 4),
         row_name = fct_relevel(row_name, "     [H5] HH not available at all visit attempts, n (% of eligible HHs)", after = 5),
         row_name = fct_relevel(row_name, "     [H8] Household absent for extended period of time, n (% of eligible HHs)", after = 6),
         row_name = fct_relevel(row_name, "[H6] Ineligible - Dwelling not found, n (% of HH forms received)", after = 7),
         row_name = fct_relevel(row_name, "[H7] Ineligible - vacant/address not a dwelling, n (% of HH forms received)", after = 8),
         row_name = fct_relevel(row_name, "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)", after = 13),
         row_name = fct_relevel(row_name, "999999", after = 14),
         row_name = fct_relevel(row_name, "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload.", after = 15),
         row_name = fct_relevel(row_name, "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", after = 16)) %>% 
  arrange(row_name) %>%
  select(row_name,nh) %>%
  mutate_all(~ replace(., . == "999999", NA))
  
colnames(htable)<-c("Household","Statistic")

htable

```

```{r table i, echo=FALSE,warning=FALSE,message=FALSE}

#Individual
i0 = tibble(row_name = "[I0] Target no. of interviews", n1 = 12155, n2 = 5281, n3 = 5282, n4 = 6077, n5 = 6078) %>% 
  mutate(across(everything(), as.character))

mydf = roster_df %>%
  left_join(indiv_df, by = c("province","city","eacode","hhi_shh"="ind_shh","hhi_uuid"="ind_hhi_uuid","ea_hhid_fixed","ea_hhid_ln_fixed","hhi_team_id"="ind_team_id","ageyears")) %>% 
  mutate(confgend = na_if(confgend, 0),
         gender = as.numeric(coalesce(confgend, sex)),
         age = as.numeric(coalesce(confagey, ageyears))) 
 
i1 = mydf %>%
  summarize(n1 = n_distinct(hhi_uuid[which(gender == 1 | gender == 2)], 
                            ea_hhid_ln_fixed[which(gender == 1 | gender == 2)]),
            n2 = n_distinct(hhi_uuid[which(gender == 1 & age >= 15 & age <= 49)],
                            ea_hhid_ln_fixed[which(gender == 1 & age >= 15 & age <= 49)]),
            n3 = n_distinct(hhi_uuid[which(gender == 2 & age >= 15 & age <= 49)],
                            ea_hhid_ln_fixed[which(gender == 2 & age >= 15 & age <= 49)]),
            n4 = n_distinct(hhi_uuid[which(gender == 1 & age >= 15)],
                            ea_hhid_ln_fixed[which(gender == 1 & age >= 15)]),
            n5 = n_distinct(hhi_uuid[which(gender == 2 & age >= 15)],
                            ea_hhid_ln_fixed[which(gender == 2 & age >= 15)])) %>% 
  mutate(row_name = "[I1] Total no. rostered individuals",
         across(everything(), as.character))

i2 = mydf %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 2 & (gender == 1 | gender == 2))], 
                            ea_hhid_ln_fixed[which(eligible1 == 2 & (gender == 1 | gender == 2))]),
            n2 = n_distinct(hhi_uuid[which(gender == 1 & age >= 15 & age <= 49 & eligible1 == 2)], 
                            ea_hhid_ln_fixed[which(gender== 1 & age >= 15 & age <= 49 & eligible1 == 2)]),
            n3 = n_distinct(hhi_uuid[which(gender == 2 & age >= 15 & age<=49 & eligible1 == 2)], 
                            ea_hhid_ln_fixed[which(gender== 2 & age >= 15 & age <= 49 & eligible1 == 2)]),
            n4 = n_distinct(hhi_uuid[which(gender == 1 & age >= 15 & eligible1 == 2)],
                            ea_hhid_ln_fixed[which(gender== 1 & age >= 15 & eligible1 == 2)]),
            n5 = n_distinct(hhi_uuid[which(gender == 2 & age >= 15 & eligible1 == 2)],
                            ea_hhid_ln_fixed[which(gender== 2 & age >= 15 & eligible1 == 2)])) %>% 
  mutate(row_name = "[I2] Ineligible (based on HH roster info)",
         across(everything(), as.character)) 

i3 = mydf %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)]),
            n2 = n_distinct(hhi_uuid[which(gender == 1 & age >= 15 & age <= 49 & eligible1 == 1)], 
                            ea_hhid_ln_fixed[which(gender == 1 & age >= 15 & age <= 49 & eligible1 == 1)]),
            n3 = n_distinct(hhi_uuid[which(gender == 2 & age >= 15 & age <= 49 & eligible1 == 1)], 
                            ea_hhid_ln_fixed[which(gender == 2 & age >= 15 & age <= 49 & eligible1 == 1)]),
            n4 = n_distinct(hhi_uuid[which(gender == 1 & age >= 15 & eligible1 == 1)], 
                            ea_hhid_ln_fixed[which(gender == 1 & age >= 15 & eligible1 == 1)]),
            n5 = n_distinct(hhi_uuid[which(gender == 2 & age >= 15 & eligible1 == 1)], 
                            ea_hhid_ln_fixed[which(gender == 2 & age >= 15 & eligible1 == 1)])) %>% 
  mutate(row_name ="[I3] Eligible HH members (age 15+ and slept over the night before) (1)",
         across(everything(), as.character))

i31 = mydf %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0)) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))],
                            ea_hhid_ln_fixed[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))]),
            n2 = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & 
                                             eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))]),
            n3 = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))], ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))]),
            n4 = n_distinct(hhi_uuid[which(gender==1 & age>=15 & eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))]),
            n5 = n_distinct(hhi_uuid[which(gender==2 & age>=15 & eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))], ea_hhid_ln_fixed[which(gender==2 & age>=15 & eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))])) %>% 
  mutate(row_name = "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status",
         across(everything(), as.character))

i4 = mydf %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15 & age <= 49)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15 & age <= 49)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15 & age <= 49)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15 & age <= 49)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [I4] Consented, n (% of eligible HH members with a status)")%>% 
  select(-contains("_d"))
  
i5 = mydf %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(gender==1 & age>=15 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(gender==2 & age>=15 & eligible1 == 1 & strtinstr == 2 & ind0040 == "1")]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [I5] Cannot Collect: After 3 attempts, n (% of eligible HH members with a status)")%>% 
  select(-contains("_d"))

i55 = mydf %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")], 
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")], 
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")], 
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")], 
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & eligible1 == 1 & strtinstr==2 & ind0040 == "96")]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)")%>% 
  select(-contains("_d"))

i6 = mydf %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                 strtinstr %in% c(1, 4) & constat == "2"))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                         strtinstr %in% c(1, 4) & constat == "2"))]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & 
                                               eligible1 == 1 & 
                                               ((strtinstr==2 & ind0040 == "2") | 
                                                  strtinstr %in% c(1, 4) & constat == "2"))], 
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 &
                                                       eligible1 == 1 & 
                                                       ((strtinstr==2 & ind0040 == "2") | 
                                                          strtinstr %in% c(1, 4) & constat == "2"))]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & 
                                               eligible1 == 1 & 
                                               ((strtinstr==2 & ind0040 == "2") | 
                                                  strtinstr %in% c(1, 4) & constat == "2"))], 
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 &
                                                       eligible1 == 1 & 
                                                       ((strtinstr==2 & ind0040 == "2") | 
                                                          strtinstr %in% c(1, 4) & constat == "2"))]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & 
                                               eligible1 == 1 & 
                                               ((strtinstr==2 & ind0040 == "2") | 
                                                  strtinstr %in% c(1, 4) & constat == "2"))],
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & 
                                                       eligible1 == 1 & 
                                                       ((strtinstr==2 & ind0040 == "2") | 
                                                          strtinstr %in% c(1, 4) & constat == "2"))]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & 
                                                       eligible1 == 1 & 
                                                       ((strtinstr==2 & ind0040 == "2") | 
                                                          strtinstr %in% c(1, 4) & constat == "2"))],
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & 
                                                       eligible1 == 1 & 
                                                       ((strtinstr==2 & ind0040 == "2") | 
                                                          strtinstr %in% c(1, 4) & constat == "2"))]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [I6] Refused, n (% of eligible HH members with a status)")%>% 
  select(-contains("_d"))

i7 = mydf %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2_d = n_distinct(hhi_uuid[which(eligible1 == 1 & gender==1 & 
                                               age>=15 &  age<=49 & 
                                               ((strtinstr == 2 & ind0040 == "3") | 
                                               (strtinstr %in% c(1, 4) & constat == "0" & 
                                                  (is.na(confagey)| confagey < 15)) | 
                                               (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & gender==1 & 
                                                       age>=15 & age<=49 & 
                                                       ((strtinstr == 2 & ind0040 == "3") | 
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey)| confagey < 15)) | 
                                                       (strtinstr %in% c(1, 4) & constat == "99")))]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3_d = n_distinct(hhi_uuid[which(eligible1 == 1 & gender==2 & 
                                               age>=15 &  age<=49 & 
                                               ((strtinstr == 2 & ind0040 == "3") | 
                                               (strtinstr %in% c(1, 4) & constat == "0" & 
                                                  (is.na(confagey) | confagey < 15)) | 
                                               (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & gender==2 & 
                                                       age>=15 & age<=49 & 
                                                       ((strtinstr == 2 & ind0040 == "3") | 
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey)| confagey < 15)) | 
                                                       (strtinstr %in% c(1, 4) & constat == "99")))]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4_d = n_distinct(hhi_uuid[which(eligible1 == 1 & gender==1 & 
                                               age>=15 &
                                               ((strtinstr == 2 & ind0040 == "3") | 
                                               (strtinstr %in% c(1, 4) & constat == "0" & 
                                                  (is.na(confagey)| confagey < 15)) | 
                                               (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & gender==1 & 
                                                       age>=15 &
                                                       ((strtinstr == 2 & ind0040 == "3") | 
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey)| confagey < 15)) | 
                                                       (strtinstr %in% c(1, 4) & constat == "99")))]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5_d = n_distinct(hhi_uuid[which(eligible1 == 1 & gender==2 & 
                                               age>=15 &
                                               ((strtinstr == 2 & ind0040 == "3") | 
                                               (strtinstr %in% c(1, 4) & constat == "0" & 
                                                  (is.na(confagey)| confagey < 15)) | 
                                               (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & gender==2 & 
                                                       age>=15 &
                                                       ((strtinstr == 2 & ind0040 == "3") | 
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey)| confagey < 15)) | 
                                                       (strtinstr %in% c(1, 4) & constat == "99")))]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [I7] Ineligible via screening criteria, n (% of eligible HH members with a status) (2)",
         across(everything(), as.character)) %>%
  select(-contains("_d"))

i71 = mydf %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)")%>% 
  select(-contains("_d"))
  
i8 = mydf %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0),
         exc_8 = case_when(eligible1 == 1 & strtinstr %in% c(1, 4) & constat== "1" ~ 1, #i4
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "1" ~ 1, # i5
                           (eligible1 == 1 & strtinstr==2 & ind0040 == "2" |
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "2") ~ 1, #i6
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "3" ~ 1,
                           eligible1 == 1 & (strtinstr %in% c(1, 4) & constat == "0" & 
                                               (is.na(confagey) | confagey < 15)) ~ 1,
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "99" ~ 1,
                           .default = 0),
         tbd = case_when(inc_8 == 1 | exc_8 == 0 ~ 1,
                         .default = 0)) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & 
                                               eligible1 == 1 & tbd == 1)], 
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 &
                                                       eligible1 == 1 & tbd == 1)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & 
                                               eligible1 == 1 & tbd == 1)], 
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 &
                                                       eligible1 == 1 & tbd == 1)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & 
                                               eligible1 == 1 & tbd == 1)], 
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & 
                                                       eligible1 == 1 & tbd == 1)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & 
                                               eligible1 == 1 & tbd == 1)], 
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & 
                                                       eligible1 == 1 & tbd == 1)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [I8] Approximate TBD, n (% of eligible HH members with a status)",
         across(everything(), as.character)) %>%
  select(-contains("_d"))

i9 = mydf %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))]),
            n2 = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & 
                                             eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 &
                                                     eligible1 == 1 & rostered_final_status %in% c(2, 3))]),
            n3 = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & 
                                             eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 &
                                                     eligible1 == 1 & rostered_final_status %in% c(2, 3))]),
            n4 = n_distinct(hhi_uuid[which(gender==1 & age>=15 & 
                                             eligible1 == 1 & rostered_final_status %in% c(2, 3))],
                            ea_hhid_ln_fixed[which(gender==1 & age>=15 & 
                                                     eligible1 == 1 & rostered_final_status %in% c(2, 3))]),
            n5 = n_distinct(hhi_uuid[which(gender==2 & age>=15 & 
                                                     eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(gender==2 & age>=15 & 
                                                     eligible1 == 1 & rostered_final_status %in% c(2, 3))])) %>% 
  mutate(row_name = "[I9] No IND record",
         across(everything(), as.character))

i10 = tibble(row_name = c("TEST", "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined."), n = c(NA, NA))

i11 = tibble(row_name = "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", n = NA)


itable = bind_rows(!!!mget(paste0("i", c(31, 55, 71, seq(from = 0, to = 11))))) %>% 
  mutate(row_name = fct_relevel(row_name, "[I0] Target no. of interviews", after = 0L),
         row_name = fct_relevel(row_name, "[I1] Total no. rostered individuals", after = 1),
         row_name = fct_relevel(row_name, "[I2] Ineligible (based on HH roster info)", after = 2),
         row_name = fct_relevel(row_name, "[I3] Eligible HH members (age 15+ and slept over the night before) (1)", after = 3),
         row_name = fct_relevel(row_name, "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status", after = 4),
         row_name = fct_relevel(row_name, "     [I4] Consented, n (% of eligible HH members with a status)", after = 5),
         row_name = fct_relevel(row_name, "     [I5] Cannot Collect: After 3 attempts, n (% of eligible HH members with a status)", after = 6),
         row_name = fct_relevel(row_name, "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)", after = 7),
         row_name = fct_relevel(row_name, "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)", after = 10),
         row_name = fct_relevel(row_name, "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined.", after = 15),
         row_name = fct_relevel(row_name, "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", after = 16),
         row_name = fct_relevel(row_name, "TEST", after = 13)) %>%
  arrange(row_name) %>% select(row_name,n1,n2,n3,n4,n5) %>%
  mutate_all(~ replace(., . == "TEST", NA))

colnames(itable)<-c("Statistic","All","Males between 15-49 years","Females between 15-49 years","Males above 15 years","Females above 15 years")

itable

```

```{r table b, echo=FALSE, message=FALSE, warning=FALSE}

mydata = roster_df %>%
 left_join(hh_df, by = c("province","city","eacode","hhi_shh"="shh","hhi_uuid","hhi_team_id","hhi_deviceid")) %>%
  left_join(indiv_df, by = c("province","city","eacode","hhi_shh"="ind_shh","hhi_uuid"="ind_hhi_uuid","ea_hhid_fixed","ea_hhid_ln_fixed","hhi_team_id"="ind_team_id","ageyears")) %>% 
  mutate(gender = coalesce(confgend, sex),
         age = coalesce(confagey, ageyears)) 

i4 = mydf %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i3$n1)*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15 & age <= 49)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15 & age <= 49)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i3$n2)*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15 & age <= 49)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15 & age <= 49)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i3$n3)*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 1 & age >= 15)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i3$n4)*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15)],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1 & gender == 2 & age >= 15)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i3$n5)*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "[I4] Consented, n (% of eligible HH members with a status)")


#Biomarker test

mydata = mydata %>%
  mutate(hivstatus_final =
           case_when(
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 1 | biohivtst2rs2 == 1) & (biohivtst3rs == 1 | biohivtst3rs2 == 1) ~ 1), # hiv positive
             (biohivtst1rs == 2 | biohivtst1rs2 == 2 | ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 2 | biohivtst1ars2 == 2))) ~ 2, # hiv negative
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 1 | biohivtst2rs2 == 1) & (biohivtst3rs == 2 | biohivtst3rs2 == 2) |
                ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 1 | biohivtst1ars2 == 1))) ~ 3, # indeterminate
             biohivtst1rs2 == 3 | biohivtst2rs2 == 3 | biohivtst1ars2 == 3 | biohivtst3rs2 == 3 ~ 4) # invalid 
           )

hivstats_gen <- mydata %>%
  count(hivstatus_final) %>%
  group_by(hivstatus_final)

hivstats_gen_orig <- mydata %>%
  count(hivstatus) %>%
  group_by(hivstatus)

         
b0 = tibble(row_name = "[B0] Target no. of blood draws", n1 = 11522, n2 = 5007, n3 = 5007, n4 = 5761, n5 = 5761) %>% 
  mutate(across(everything(), as.character))

b1 = mydata %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))], ea_hhid_ln_fixed[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4$n1_d)*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i4$n2_d)*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))], 
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i4$n3_d)*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i4$n4_d)*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))], ea_hhid_ln_fixed[which(gender==2 & age>=15 & (inconbio == 1 | (pprmbio == 1 & asybiogt == 1)))]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i4$n5_d)*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "[B1] Consented to blood draw, n (% of consented to interview)") %>% 
  select(-contains("_d"))

b2 = mydata %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4$n1_d)*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n2 = paste0(n2_d, " (", format(round(n2_d/as.numeric(i4$n2_d)*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n3 = paste0(n3_d, " (", format(round(n3_d/as.numeric(i4$n3_d)*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n4 = paste0(n4_d, " (", format(round(n4_d/as.numeric(i4$n4_d)*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==2 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n5 = paste0(n5_d, " (", format(round(n5_d/as.numeric(i4$n5_d)*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "[B2] Blood draws achieved, n (% of consented to interview)")

b3 = mydata %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n1 = paste0(format(round(n1_d/as.numeric(b0$n1)*100, 1), nsmall = 1), "%"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n2 = paste0(format(round(n2_d/as.numeric(b0$n2)*100, 1), nsmall = 1), "%"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n3 = paste0(format(round(n3_d/as.numeric(b0$n3)*100, 1), nsmall = 1), "%"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==1 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n4 = paste0(format(round(n4_d/as.numeric(b0$n4)*100, 1), nsmall = 1), "%"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(gender==2 & age>=15 & !(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))]),
            n5 = paste0(format(round(n5_d/as.numeric(b0$n5)*100, 1), nsmall = 1), "%")) %>% 
  mutate(row_name = "[B3] Percent of target blood draws achieved") %>% 
  select(-contains("_d"))

#mydata %>%
#  filter(is.na(coltypead)) %>%
#  reframe(n_distinct(hhi_uuid,ea_hhid_ln_fixed))

b4 = mydata %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1)], ea_hhid_ln_fixed[which(hivstatus_final == 1)]),
            n1 = paste0(n1_d, " (", format(round(n1_d/b2$n1_d*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & hivstatus_final == 1)], ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & hivstatus_final == 1)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/b2$n2_d*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & hivstatus_final == 1)], ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & hivstatus_final == 1)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/b2$n3_d*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & hivstatus_final == 1)], ea_hhid_ln_fixed[which(gender==1 & age>=15 & hivstatus_final == 1)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/b2$n4_d*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & hivstatus_final == 1)],
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & hivstatus_final == 1)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/b2$n5_d*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "[B4] HIV rapid test: Number (%) of positive")

b4 %>%
  select(-contains("_d"))

b41 = mydata %>%
    summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1 & ltcvrbl == 1)], ea_hhid_ln_fixed[which(hivstatus_final == 1 & ltcvrbl == 1)]),
            n1 = paste0(n1_d, " (", format(round(n1_d/b4$n1_d*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & hivstatus_final == 1 & ltcvrbl == 1)], ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & hivstatus_final == 1 & ltcvrbl == 1)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/b4$n2_d*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & hivstatus_final == 1 & ltcvrbl == 1)], ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & hivstatus_final == 1 & ltcvrbl == 1)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/b4$n3_d*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & hivstatus_final == 1 & ltcvrbl == 1)], ea_hhid_ln_fixed[which(gender==1 & age>=15 & hivstatus_final == 1 & ltcvrbl == 1)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/b4$n4_d*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & hivstatus_final == 1 & ltcvrbl == 1)],
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & hivstatus_final == 1 & ltcvrbl == 1)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/b4$n5_d*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "     [B4.1] Consented to ALTC, n (% of HIV-positive)") %>% 
  select(-contains("_d"))

b5 = mydata %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 2)]),
            n1 = paste0(n1_d, " (", format(round(n1_d/b2$n1_d*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & hivstatus_final == 2)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/b2$n2_d*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & hivstatus_final == 2)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/b2$n3_d*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & hivstatus_final == 2)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/b2$n4_d*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & hivstatus_final == 2)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/b2$n5_d*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "[B5] HIV rapid test: Number (%) of negative") %>% 
  select(-contains("_d"))

b6 = mydata %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 3)]),
            n1 = paste0(n1_d, " (", format(round(n1_d/b2$n1_d*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender == 1 & age >= 15 & age <= 49 & hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(gender == 1 & age >= 15 & age<=49 & hivstatus_final == 3)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/b2$n2_d*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender == 2 & age >= 15 & age <= 49 & hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(gender == 2 & age >= 15 & age <= 49 & hivstatus_final == 3)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/b2$n3_d*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender == 1 & age >= 15 & hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(gender == 1 & age>=15 & hivstatus_final == 3)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/b2$n4_d*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender == 2 & age >= 15 & hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(gender == 2 & age>=15 & hivstatus_final == 3)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/b2$n5_d*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "[B6] HIV rapid test: Number (%) of indeterminate") %>% 
  select(-contains("_d"))

b7 = mydata %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 4)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 4)]),
            n1 = paste0(n1_d, " (", format(round(n1_d/b2$n1_d*100, 1), nsmall = 1), "%)"),
            n2_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & age<=49 & hivstatus_final==4)],
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & age<=49 & hivstatus_final==4)]),
            n2 = paste0(n2_d, " (", format(round(n2_d/b2$n2_d*100, 1), nsmall = 1), "%)"),
            n3_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & age<=49 & hivstatus_final==4)],
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & age<=49 & hivstatus_final==4)]),
            n3 = paste0(n3_d, " (", format(round(n3_d/b2$n3_d*100, 1), nsmall = 1), "%)"),
            n4_d = n_distinct(hhi_uuid[which(gender==1 & age>=15 & hivstatus_final==4)],
                              ea_hhid_ln_fixed[which(gender==1 & age>=15 & hivstatus_final==4)]),
            n4 = paste0(n4_d, " (", format(round(n4_d/b2$n4_d*100, 1), nsmall = 1), "%)"),
            n5_d = n_distinct(hhi_uuid[which(gender==2 & age>=15 & hivstatus_final==4)],
                              ea_hhid_ln_fixed[which(gender==2 & age>=15 & hivstatus_final==4)]),
            n5 = paste0(n5_d, " (", format(round(n5_d/b2$n5_d*100, 1), nsmall = 1), "%)")) %>% 
  mutate(row_name = "[B7] HIV rapid test: Number (%) of invalid") %>% 
  select(-contains("_d"))

btable = bind_rows(!!!mget(paste0("b", c(41, seq(from = 0, to = 7))))) %>% 
  mutate(row_name = fct_reorder(row_name, row_name),
         row_name = fct_relevel(row_name, "     [B4.1] Consented to ALTC, n (% of HIV-positive)", after = 5)) %>%
  arrange(row_name) %>% 
  select(row_name,n1,n2,n3,n4,n5)
  
colnames(btable)<-c("Statistic","All above 15 years","Males between 15-49 years","Females between 15-49 years","Males above 15 years","Females above 15 years")

btable

```

```{r table c, echo=FALSE,warning=FALSE,message=FALSE}

## Interviews

cfunc = function(df, filter1, end_var) {
  result = df %>% 
    filter(endmsg1 == "A") %>% 
    filter(!!enexpr(filter1)) %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>%
    filter(survey_time >= 0) %>%
    #group_by(hhi_eacode) %>% 
    summarize(median = round(median(survey_time, na.rm=TRUE), 1),
              median = as.character(median))
  return(pull(result, median))
}

cfunc_mean = function(df, filter1, end_var) {
  result = df %>% 
    filter(endmsg1 == "A") %>% 
    filter(!!enexpr(filter1)) %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>%
    filter(survey_time >= 0) %>%
    #group_by(hhi_eacode) %>% 
    summarize(mean = round(mean(survey_time, na.rm=TRUE), 1),
              mean = as.character(mean))
  return(pull(result, mean))
}

mydata$specdate<-ymd_hms(mydata$specdate)
mydata$adqxedt<-ymd_hms(mydata$adqxedt)
mydata$indformsdt<-ymd_hms(mydata$indformsdt)
mydata$bio3edt<-ymd_hms(mydata$bio3edt)

c1 = tibble(n1 = cfunc(mydata, TRUE, "adqxedt"),
            n2 = cfunc(mydata, gender==1 & age>=15 & age<=49, "adqxedt"),
            n3 = cfunc(mydata, gender==2 & age>=15 & age<=49, "adqxedt"),
            n4 = cfunc(mydata, gender==1 & age>=15, "adqxedt"),
            n5 = cfunc(mydata, gender==2 & age>=15, "adqxedt")) %>% 
  mutate(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)")

c11 = tibble(n1 = cfunc_mean(mydata, TRUE, "adqxedt"),
            n2 = cfunc_mean(mydata, gender==1 & age>=15 & age<=49, "adqxedt"),
            n3 = cfunc_mean(mydata, gender==2 & age>=15 & age<=49, "adqxedt"),
            n4 = cfunc_mean(mydata, gender==1 & age>=15, "adqxedt"),
            n5 = cfunc_mean(mydata, gender==2 & age>=15, "adqxedt")) %>% 
  mutate(row_name = "[C1] Median (average) time to complete entire individual interview (min) (only includes those who completed the interview)")

c2 = tibble(n1 = cfunc(mydata, TRUE, "bio3edt"),
            n2 = cfunc(mydata, gender==1 & age>=15 & age<=49, "bio3edt"),
            n3 = cfunc(mydata, gender==2 & age>=15 & age<=49, "bio3edt"),
            n4 = cfunc(mydata, gender==1 & age>=15, "bio3edt"),
            n5 = cfunc(mydata, gender==2 & age>=15, "bio3edt")) %>% 
  unnest() %>% 
  mutate(row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection/n(includes those who drop out mid-way after consenting to individual interview consent)",
         across(everything(), as.character))

c21 = tibble(n1 = cfunc_mean(mydata, TRUE, "bio3edt"),
            n2 = cfunc_mean(mydata, gender==1 & age>=15 & age<=49, "bio3edt"),
            n3 = cfunc_mean(mydata, gender==2 & age>=15 & age<=49, "bio3edt"),
            n4 = cfunc_mean(mydata, gender==1 & age>=15, "bio3edt"),
            n5 = cfunc_mean(mydata, gender==2 & age>=15, "bio3edt")) %>% 
  unnest() %>% 
  mutate(row_name = "[C2] Median (average) time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)")

c1 <- c1 %>%
  mutate(across(everything(), ~ paste(.x, " (", c11[[cur_column()]], ")", sep = ""))) %>%
  mutate(row_name = "[C1] Median (average) time to complete entire individual interview (min) (only includes those who completed the interview)")
c2 <- c2 %>%
  mutate(across(everything(), ~ paste(.x, " (", c21[[cur_column()]], ")", sep = ""))) %>%
  mutate(row_name = "[C2] Median (average) time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)")

c4 = mydata %>% 
  summarize(n1 = h3$nh,
            n2 = h3$nh,
            n3 = h3$nh,
            n4 = h3$nh,
            n5 = h3$nh) %>% 
  mutate(row_name = "     [C4] Consented, n (% of eligible HHs)")

c5 = tibble(n1 = paste0(b2$n1_d, " (", format(round(b2$n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2 = paste0(b2$n2_d, " (", format(round(b2$n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3 = paste0(b2$n3_d, " (", format(round(b2$n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4 = paste0(b2$n4_d, " (", format(round(b2$n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5 = paste0(b2$n5_d, " (", format(round(b2$n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [C5] Blood draws achieved, n (% of eligible HH members with a status)")

c3_func <- function(var) {
  # Remove everything before the first opening parenthesis and the parenthesis itself
  result <- gsub(".*\\(\\s*", "", var)
  
  # Remove the closing parenthesis, if any
  result <- gsub("\\)", "", result)
  
  # Remove the percent sign, if any
  result <- gsub("%", "", result)
  
  # Convert the remaining string to a numeric value
  result <- as.numeric(result)
  
  return(result)
}
# c3_func(c4$n1)
# Multiply the percentages of c4 and c5 without recalculating (using strings)
c3 = tibble(n1_a = c3_func(c4$n1),
            n2_a = c3_func(c4$n2),
            n3_a = c3_func(c4$n1),
            n4_a = c3_func(c4$n1),
            n5_a = c3_func(c4$n1),
            n1_b = c3_func(c5$n1),
            n2_b = c3_func(c5$n2),
            n3_b = c3_func(c5$n3),
            n4_b = c3_func(c5$n4),
            n5_b = c3_func(c5$n5),
            n1 = paste0(format(round(n1_a*n1_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n2 = paste0(format(round(n2_a*n2_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n3 = paste0(format(round(n3_a*n3_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n4 = paste0(format(round(n4_a*n4_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n5 = paste0(format(round(n5_a*n5_b/100, 1), nsmall = 1, trim = TRUE), "%")) %>% 
  select(-contains("_a"), -contains("_b")) %>%
  mutate(row_name = "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)") 

c7 = tibble(n1 = paste0(i4$n1_d, " (", format(round(i4$n1_d/as.numeric(i31$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n2 = paste0(i4$n2_d, " (", format(round(i4$n2_d/as.numeric(i31$n2)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n3 = paste0(i4$n3_d, " (", format(round(i4$n3_d/as.numeric(i31$n3)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n4 = paste0(i4$n4_d, " (", format(round(i4$n4_d/as.numeric(i31$n4)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            n5 = paste0(i4$n5_d, " (", format(round(i4$n5_d/as.numeric(i31$n5)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "     [C7] Consented to individual, n (% of eligible HH members with a status)")

c8 = b2 %>%
  mutate(row_name = "     [C8] Blood draws achieved, n (% of consented to interview)") %>%
  select(-contains("_d"))

c6 = tibble(n1_a = c3_func(c7$n1),
            n2_a = c3_func(c7$n2),
            n3_a = c3_func(c7$n1),
            n4_a = c3_func(c7$n1),
            n5_a = c3_func(c7$n1),
            n1_b = c3_func(c8$n1),
            n2_b = c3_func(c8$n2),
            n3_b = c3_func(c8$n3),
            n4_b = c3_func(c8$n4),
            n5_b = c3_func(c8$n5),
            n1 = paste0(format(round(n1_a*n1_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n2 = paste0(format(round(n2_a*n2_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n3 = paste0(format(round(n3_a*n3_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n4 = paste0(format(round(n4_a*n4_b/100, 1), nsmall = 1, trim = TRUE), "%"),
            n5 = paste0(format(round(n5_a*n5_b/100, 1), nsmall = 1, trim = TRUE), "%")) %>% 
  select(-contains("_a"), -contains("_b")) %>%
  mutate(row_name = "[C6] Overall individual response rate (% individual consent X % actual blood draws)")


ctable<-c1 %>% rbind(c2) %>% rbind(c3) %>% rbind(c4) %>% rbind(c5) %>% rbind(c6) %>% rbind(c7) %>% rbind(c8)
colnames(ctable)<-c("All","Males between 15-49 years","Females between 15-49 years","Males above 15 years","Females above 15 years", "Statistic")
ctable = ctable %>% 
  select(Statistic, everything())

ctable

```

### Export

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("EAs"= etable,
                 "Households"=htable,
                 "Individuals"=itable,
                 "Biomarkers tests"=btable,
                 "Interviews"=ctable), 
            paste0("./results/", 
                   "2a-CODPHIA_Overall Monitoring Report_", format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 2c/2d TREND OVER TEAM 

```{r 2cd1, table e, message=FALSE, warning=FALSE, echo=FALSE}

# EAs

hh_df$hhi_team_id <- ifelse(!is.na(hh_df$hhi_team_id) & hh_df$hhi_team_id %in% 1:240,
                         paste0("T", sprintf("%02d", hh_df$hhi_team_id)),
                         'TNA')

#eadf <- sss_df %>% 
#  select(province,city,eacode,hhid,ea_id1,geovar_short,uuid,teamid1,listingformsdt,instlisting4,sampled_hh) %>%
#  left_join(hh_df,by=c("province","city","eacode"))

e0a <- hh_df %>%
  group_by(hhi_team_id) %>%
  summarize(n = round(132/23)) %>%
  mutate(row_name = "[H0] Target")

e_dfa = hh_df %>% 
  full_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>%
  mutate(hh_complete = if_else(!is.na(hhqsts) & (!is.na(hhqets) | !is.na(hhqenddt)), 1, 0)) %>% 
  group_by(eacode) %>%
  summarize(yes = n_distinct(hhi_shh[which(hh_complete == 1)]),
            expected = sum(unique(sampled_hh)),
            ratios = yes/expected,
            Team = hhi_team_id) %>%
  distinct()

summary <- rbind(e1,e2) %>%
  rename(All = n,
         Status = row_name)

etable2 = e_dfa %>% 
  group_by(Team) %>%
  mutate(status = case_when(ratios > 0 & ratios < 0.96 ~ "[E1] Started or In Progress With Collection (E4 + E5)",
                            ratios >= 0.96 ~ "[E2] Completed, n(%)",
                            .default = "[E3] Not started, n (%)")) %>%
  group_by(Team, status) %>%
  summarize(n = n_distinct(eacode)) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  mutate(status = fct_relevel(status, "[E1] Started or In Progress With Collection (E4 + E5)", after = 1),
         status = fct_relevel(status, "[E2] Completed, n(%)", after = 2)) %>%
    arrange(status) %>%  
  mutate_all(~ replace(., is.na(.), "0")) %>%
  rename(Status = status) %>%
  #filter(!row_number() %in% 2) %>%
  merge(summary) %>%
  relocate(All, .after = Status) %>%
  # select(-c("NA")) %>%
  add_row(Status = c(NA, "Progress and completion of EAs is based off of completion of HH interviews and does not take into account completion of individual interviews and blood testing.")) %>%
  add_row(Status = "Number of EAs by team may not add up to total number of EAs as multiple teams may work in the same EA.")

etable2

```

```{r 2cd2, table h, echo=FALSE,warning=FALSE,message=FALSE}

# Households

h0a <- hh_df %>%
  group_by(hhi_team_id) %>%
  summarize(n = ceiling(4974/23)) %>%
  mutate(row_name = "[H0] Target")

h1a <- hh_df %>%  
  group_by(hhi_team_id) %>% 
  summarize(n = n_distinct(hhi_shh)) %>%
  mutate(row_name = "[H1] Total HH Counts, n (% of Target) (1)",
         nh = paste0(n, " (", format(round(n/as.numeric(h0a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h2a = hh_df %>% 
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6))])) %>%
  mutate(row_name = "[H2] Eligible HHs, n (% of HH forms received) (2)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1a$n)*100, digits =1), nsmall = 1, trim = TRUE), "%)"))

h3a = hh_df %>% 
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & hhconstat == 1)])) %>%
  mutate(row_name = "     [H3] Consented, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h4a = hh_df %>% 
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) &
                                           (resultndt == 2 | hhconstat == 2 | startint == 4))])) %>%
  mutate(row_name = "     [H4] Refused, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h5a = hh_df %>%
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 1)])) %>%
  mutate(row_name = "     [H5] HH not available at all visit attempts, n (% of eligible HHs)",
         nh= paste0(n, " (", format(round(n/as.numeric(h2a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h6a = hh_df %>% 
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 5)])) %>% 
  mutate(row_name = "[H6] Ineligible - Dwelling not found, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1a$n)*100, digits = 1), nsmall = 1, trim = TRUE),"%)"))

h7a = hh_df %>%
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 3)])) %>%
  mutate(row_name = "[H7] Ineligible - vacant/address not a dwelling, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h8a = hh_df %>% 
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 6)])) %>% 
  mutate(row_name = "     [H8] Household absent for extended period of time, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h9a = hh_df %>% 
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 4)])) %>% 
  mutate(row_name = "[H9] Ineligible - destroyed, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h10a = hh_df %>% 
  group_by(hhi_team_id) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 96)])) %>% 
  mutate(row_name = "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

h11a = tibble(row_name = c("TEST", "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload."), n = c(NA, NA))
  
h12a = tibble(row_name = "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", n = NA)

h0a = h0a %>%
  rename(nh = n) %>%
  mutate(across(everything(), as.character))

htable2 = bind_rows(!!!mget(paste0("h", seq(from = 0, to = 12), "a"))) %>% 
  select(hhi_team_id, row_name, nh) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  mutate(row_name = fct_relevel(row_name, "[H10] Could not collect for other reasons, n (% of HH forms received)", after = 12)) %>% 
    arrange(row_name) %>%
  merge(htable, by.x = "row_name", by.y = "Household", all = TRUE) %>%
  relocate(Statistic, .after = row_name) %>%
  rename(Households = row_name) %>%
  mutate(Households = fct_relevel(Households, "[H0] Target", after = 0L),
         Households = fct_relevel(Households, "[H1] Total HH Counts, n (% of Target) (1)", after = 1),
         Households = fct_relevel(Households, "[H2] Eligible HHs, n (% of HH forms received) (2)", after = 2),
         # Households = fct_relevel(Households, "[H9] Ineligible - destroyed, n (% of HH forms received)", after = 9), 
         Households = fct_relevel(Households, "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)", after = 12),
         Households = fct_relevel(Households, "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload.", after = 13),
         Households = fct_relevel(Households, "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", after = 14)) %>%
  arrange(Households) %>%
  select(-c("NA")) %>%
  rename(All = Statistic) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "TEST", NA))

htable2

```

```{r 2cd3, table i, echo=FALSE,warning=FALSE,message=FALSE}

# Individuals

mydf = roster_df %>%
  left_join(indiv_df, by = c("province","city","eacode","hhi_shh"="ind_shh","hhi_uuid"="ind_hhi_uuid","ea_hhid_fixed","ea_hhid_ln_fixed","hhi_team_id"="ind_team_id","ageyears")) %>% 
  mutate(confgend = na_if(confgend, 0),
         gender = as.numeric(coalesce(confgend, sex)),
         age = as.numeric(coalesce(confagey, ageyears))) 

mydf$hhi_team_id <- ifelse(!is.na(mydata$hhi_team_id) & mydata$hhi_team_id %in% 1:240,
                         paste0("T", sprintf("%02d", mydata$hhi_team_id)),
                         'TNA')

i0a = mydf %>%
  group_by(hhi_team_id) %>%
  summarize(n = ceiling(12155/23)) %>%
  mutate(row_name = "[I0] Target no. of interviews")
  
i1a = mydf %>%
  group_by(hhi_team_id) %>%
  summarize(n1 = n_distinct(hhi_uuid[which(gender == 1 | gender == 2)], 
                            ea_hhid_ln_fixed[which(gender == 1 | gender == 2)])) %>% 
  mutate(row_name = "[I1] Total no. rostered individuals",
         across(everything(), as.character))

i2a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 2 & (gender == 1 | gender == 2))], 
                            ea_hhid_ln_fixed[which(eligible1 == 2 & (gender == 1 | gender == 2))])) %>%
  mutate(row_name = "[I2] Ineligible (based on HH roster info)",
         across(everything(), as.character)) 

i3a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)])) %>% 
  mutate(row_name ="[I3] Eligible HH members (age 15+ and slept over the night before) (1)",
         across(everything(), as.character))

i31a = mydf %>%
  group_by(hhi_team_id) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0)) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))],
                            ea_hhid_ln_fixed[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))])) %>% 
  mutate(row_name = "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status",
         across(everything(), as.character))
    
i4a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)])) %>%
  mutate(row_name = "     [I4] Consented, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i5a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")])) %>%
  mutate(n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         row_name = "     [I5] Cannot Collect: After 3 attempts, n (% of eligible HH members with a status)",
         across(everything(), as.character))

i55a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")])) %>%
  mutate(row_name = "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i6a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                 strtinstr %in% c(1, 4) & constat == "2"))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                         strtinstr %in% c(1, 4) & constat == "2"))]))%>% 
  mutate(row_name = "     [I6] Refused, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i7a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))])) %>% 
  mutate(row_name = "     [I7] Ineligible via screening criteria, n (% of eligible HH members with a status) (2)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i71a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))])) %>%
  mutate(row_name = "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i8a = mydf %>%
  group_by(hhi_team_id) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0),
         exc_8 = case_when(eligible1 == 1 & strtinstr %in% c(1, 4) & constat== "1" ~ 1, #i4
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "1" ~ 1, # i5
                           (eligible1 == 1 & strtinstr==2 & ind0040 == "2" |
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "2") ~ 1, #i6
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "3" ~ 1,
                           eligible1 == 1 & (strtinstr %in% c(1, 4) & constat == "0" & 
                                               (is.na(confagey) | confagey < 15)) ~ 1,
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "99" ~ 1,
                           .default = 0),
         tbd = case_when(inc_8 == 1 | exc_8 == 0 ~ 1,
                         .default = 0)) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))])) %>% 
  mutate(row_name = "     [I8] Approximate TBD, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i9a = mydf %>%
  group_by(hhi_team_id) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))])) %>% 
  mutate(row_name = "[I9] No IND record",
         across(everything(), as.character))

i10a = tibble(row_name = c("TEST", "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined."), n = c(NA, NA))

i11a = tibble(row_name = "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", n = NA)

i0a = i0a %>%
  rename(n1 = n) %>%
  mutate(across(everything(), as.character))

summary_i <- itable %>%
  select(c("Statistic","All"))

itable2 = bind_rows(!!!mget(paste0("i", c(31, 55, 71, seq(from = 0, to = 11)),"a"))) %>% 
  select(hhi_team_id, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  mutate(row_name = fct_relevel(row_name, "     [I3.1] Eligible HH members (age 15+ and slept over the night before) with a status", after = 6),
         row_name = fct_relevel(row_name, "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)", after = 9)) %>%
  arrange(row_name) %>%
  # select(-c("NA")) %>%
  merge(summary_i, by.x = "row_name", by.y = "Statistic", all = TRUE) %>%
  mutate(row_name = fct_relevel(row_name, "[I0] Target no. of interviews", after = 0L),
         row_name = fct_relevel(row_name, "[I1] Total no. rostered individuals", after = 1),
         row_name = fct_relevel(row_name, "[I2] Ineligible (based on HH roster info)", after = 2),
         row_name = fct_relevel(row_name, "[I3] Eligible HH members (age 15+ and slept over the night before) (1)", after = 3),
         row_name = fct_relevel(row_name, "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status", after = 4),
         row_name = fct_relevel(row_name, "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)", after = 7),
         row_name = fct_relevel(row_name, "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)", after = 10)) %>%
  arrange(row_name) %>%
  select(-c("NA")) %>%
  mutate(row_name = fct_relevel(row_name, "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined.", after = 15),
         row_name = fct_relevel(row_name, "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", after = 16)) %>%
  arrange(row_name) %>%
  relocate("All", .after = row_name) %>%
  rename(Individuals = row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  drop_na("Individuals") %>%
  mutate_all(~ replace(., . == "TEST", NA))

itable2

```

```{r 2cd4, table b, echo=FALSE,warning=FALSE,message=FALSE}

# Biomarker test

mydata = mydata %>%
  mutate(hivstatus_final =
           case_when(
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 1 | biohivtst2rs2 == 1) & (biohivtst3rs == 1 | biohivtst3rs2 == 1) ~ 1), # hiv positive
             (biohivtst1rs == 2 | biohivtst1rs2 == 2 | ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 2 | biohivtst1ars2 == 2))) ~ 2, # hiv negative
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs ==1 | biohivtst2rs2 == 1) & (biohivtst3rs == 2 | biohivtst3rs2 == 2) |
                ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 1 | biohivtst1ars2 == 1))) ~ 3, # indeterminate
             biohivtst1rs2 == 3 | biohivtst2rs2 == 3 | biohivtst1ars2 == 3 | biohivtst3rs2 == 3 ~ 4) # invalid 
           )

mydata$hhi_team_id <- ifelse(!is.na(mydata$hhi_team_id) & mydata$hhi_team_id %in% 1:240,
                         paste0("T", sprintf("%02d", mydata$hhi_team_id)),
                         'TNA')

b0a <- mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1 = ceiling(11522/23)) %>%
  mutate(row_name = "[B0] Target no. of blood draws")

b1a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))], ea_hhid_ln_fixed[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))])) %>% # change i4 to i4a
  mutate(row_name = "[B1] Consented to blood draw, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4a$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b2a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>%
  mutate(row_name = "[B2] Blood draws achieved, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4a$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)"))

b3a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>% 
  mutate(row_name = "[B3] Percent of target blood draws achieved",
         n1 = paste0(format(round(n1_d/as.numeric(b0a$n1)*100, 1), nsmall = 1, trim = TRUE), "%")) %>% 
  select(-contains("_d"))

b4a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1)], ea_hhid_ln_fixed[which(hivstatus_final == 1)])) %>%
  mutate(row_name = "[B4] HIV rapid test: Number (%) of positive",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2a$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)"))

b41a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1 & ltcvrbl == 1)], 
                              ea_hhid_ln_fixed[which(hivstatus_final == 1 & ltcvrbl == 1)])) %>%
  mutate(row_name = "     [B4.1] Consented to ALTC, n (% of HIV-positive)",
         n1 = paste0(n1_d, " (", format(round(n1_d/b4a$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b5a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 2)])) %>%
  mutate(row_name = "[B5] HIV rapid test: Number (%) of negative",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2a$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  mutate(across(everything(), as.character))

b6a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 3)]))%>%
  mutate(row_name = "[B6] HIV rapid test: Number (%) of indeterminate",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2a$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  mutate(across(everything(), as.character))

b7a = mydata %>%
  group_by(hhi_team_id) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 4)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 4)])) %>%
  mutate(row_name = "[B7] HIV rapid test: Number (%) of invalid",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2a$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  mutate(across(everything(), as.character))

b0a = b0a %>%
  mutate(across(everything(), as.character))

summary_b <- btable %>%
  select(c("Statistic", "All above 15 years"))

b4a = b4a %>%
  select(-contains("_d"))

btable2 = bind_rows(!!!mget(paste0("b", c(41, seq(from = 0, to = 7)), "a"))) %>% 
  select(hhi_team_id, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  mutate(row_name = fct_relevel(row_name, "[B0] Target", after = 0L)) %>%
  arrange(row_name) %>%
  merge(summary_b, by.x = "row_name", by.y = "Statistic") %>%
  relocate("All above 15 years", .after = "row_name") %>%
  mutate(row_name = fct_relevel(row_name, "     [B4.1] Consented to ALTC, n (% of HIV-positive)", after = 5)) %>%
  arrange(row_name) %>%
  rename(Biomarkers = row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))

btable2

```

```{r 2cd5, table c, echo=FALSE,warning=FALSE,message=FALSE}

# Interviews
 
teamsc <- tibble(row_name = unique(mydata$hhi_team_id)) %>%
  add_row(row_name = "T99")

crow_func = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(hhi_team_id = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(hhi_team_id) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(hhi_team_id, median) %>% 
    pivot_wider(names_from = hhi_team_id, values_from = median) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

crow_func_mean = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(hhi_team_id = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(hhi_team_id) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(hhi_team_id, mean) %>% 
    pivot_wider(names_from = hhi_team_id, values_from = mean) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

c5_func = function(df1, df2, row_name){
  my_ca = df1 %>% 
    pivot_longer(cols = -1, values_to = "values", names_to = "hhi_eacode") %>% 
    mutate(ind_count1 = str_extract(values, "^[:digit:]+(?=[:space:])"))
  my_cb = df2 %>% 
    pivot_longer(cols = -1, values_to = "ind_count2", names_to = "hhi_eacode")
  my_cc = my_ca %>% 
    left_join(my_cb, by = "hhi_eacode") %>% 
    mutate(across(starts_with("ind_count"), as.numeric),
           ind_percent = format(trim = TRUE, ind_count1/ind_count2*100, digits = 3, nsmall = 1, drop0trailing = TRUE),
           total_ind_counts = paste0(ind_count1, " (", ind_percent, "%)")) %>% 
    select(hhi_eacode, total_ind_counts) %>% 
    pivot_wider(names_from = hhi_eacode, values_from = total_ind_counts) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(my_cc)
}

c1a = crow_func(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))

c11a = crow_func_mean(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))

c2a = crow_func(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))

c21a = crow_func_mean(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))

c1a <- c1a %>%
  mutate(across(everything(), ~ paste(.x, " (", c11a[[cur_column()]], ")", sep = ""))) %>%
  mutate(row_name = "[C1] Median (average) time to complete entire individual interview (min) (only includes those who completed the interview)")
c2a <- c2a %>%
  mutate(across(everything(), ~ paste(.x, " (", c21a[[cur_column()]], ")", sep = ""))) %>%
  mutate(row_name = "[C2] Median (average) time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)")


c4a = h3a %>% 
  mutate(row_name = recode(row_name, "     [H3] Consented, n (% of eligible HHs)" = "     [C4] Consented, n (% of eligible HHs)")) %>%
  select(-("n")) %>%
  pivot_wider(names_from = 1, values_from = 3)

c5a = mydata %>%
  summarize(n1 = paste0(b2a$n1_d, " (", format(round(b2a$n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            hhi_team_id = b2a$hhi_team_id) %>%
  mutate(row_name = "     [C5] Blood draws achieved, n (% of eligible HH members with a status)") %>%
  right_join(teamsc, by = c("hhi_team_id" = "row_name")) %>%
  fill(row_name, .direction =(c("down"))) %>%
  pivot_wider(names_from = 2, values_from = 1)

c5a <- c5a %>% map_df(~unlist(.x)) %>%
  distinct()

# c3a = c3_func(df1 = c4a, df2 = c5a, row_name = "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)")

# Multiply the percentages of c4 and c5 without recalculating (using strings)

c4_helper = h3a %>% 
  mutate(row_name = recode(row_name, "[H3] Consented, n (% of eligible HHs)" = "[C4] Consented, n (% of eligible HHs)")) %>%
  select(-("n"))

c5_helper = mydata %>%
  summarize(n1 = paste0(b2a$n1_d, " (", format(round(b2a$n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
            hhi_team_id = b2a$hhi_team_id) %>%
  mutate(row_name = "[C5] Blood draws achieved, n (% of eligible HH members with a status)") %>%
  right_join(teamsc, by = c("hhi_team_id" = "row_name")) %>%
  fill(row_name, .direction =(c("down")))

extract <- function(x) {
  matches <- regmatches(x, regexpr("\\((\\d+\\.?\\d*)%\\)", x))
  
  if(length(matches) > 0 && matches != "") {
    percentage <- as.numeric(gsub("[()%]", "", matches))
  } else {
    percentage <- NA
  }
  return(percentage)
}

c4_helper = c4_helper %>%
  filter(hhi_team_id != "T99") %>%
  mutate(percentage = sapply(nh, extract)) %>%
  mutate(percentage = percentage / 100)

c5_helper = c5_helper %>%
  filter(hhi_team_id != "T99") %>%
  mutate(percentage = sapply(n1, extract)) %>%
  mutate(percentage = percentage / 100)

c3a <- c4_helper %>%
  inner_join(c5_helper, by = "hhi_team_id", suffix = c("_1", "_2")) %>%
  mutate(multiplied = (percentage_1 * percentage_2) *100) %>%
  select(hhi_team_id, multiplied) %>%
  add_column("[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)") %>%
  mutate(multiplied = format(round(as.numeric(multiplied), 1), nsmall = 1, trim = FALSE)) %>%
  mutate(multiplied = paste0(as.character(multiplied), '%')) %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  rename(row_name = 1)

c7a <- i4a %>%
  mutate(row_name = "     [C7] Consented to individual, n (% of eligible HH members with a status)") %>%
  select(-contains("_d")) 

c8a <- b2a %>%
  mutate(row_name = "     [C8] Blood draws achieved, n (% of consented to interview)") %>%
  select(-contains("_d")) 

c6a <- c7a %>%
  left_join(c8a, by = "hhi_team_id") %>%
  mutate(percentage_x = sapply(n1.x, extract)) %>%
  mutate(percentage_x = percentage_x / 100) %>%
  mutate(percentage_y = sapply(n1.y, extract)) %>%
  mutate(percentage_y = percentage_y / 100) %>%
  mutate(n1 = paste0(format(round(percentage_x * percentage_y * 100, 1), nsmall = 1, trim = FALSE), "%"),
         row_name = "[C6] Overall individual response rate (% individual consent X % actual blood draws)") %>%
  select(hhi_team_id, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3)  

c7a <- c7a %>%
  pivot_wider(names_from = 1, values_from = 3)

c8a <- c8a %>%
  pivot_wider(names_from = 1, values_from = 3) 


summary_c <- ctable %>%
  select(c("Statistic","All"))

ctable2 = bind_rows(!!!mget(paste0("c", seq(from = 1, to = 8), "a"))) %>% 
  merge(summary_c, by.x = "row_name", by.y = "Statistic") %>%
  relocate(All, .after = row_name) %>%
  rename(Interviews = row_name) %>%
  mutate(Interviews = fct_relevel(Interviews, "[C1] Median (average) time to complete entire individual interview (min) (only includes those who completed the interview)", after = 0L),
         Interviews = fct_relevel(Interviews, "[C2] Median (average) time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)", after = 2),
         Interviews = fct_relevel(Interviews, "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)", after = 3),
         Interviews = fct_relevel(Interviews, "     [C4] Consented, n (% of eligible HHs)", after = 4),
         Interviews = fct_relevel(Interviews, "     [C5] Blood draws achieved, n (% of eligible HH members with a status)", after = 5),
         Interviews = fct_relevel(Interviews, "[C6] Overall individual response rate (% individual consent X % actual blood draws)", after = 6),
         Interviews = fct_relevel(Interviews, "     [C7] Consented to individual, n (% of eligible HH members with a status)", after = 7),
         Interviews = fct_relevel(Interviews, "     [C8] Blood draws achieved, n (% of consented to interview)", after = 8)) %>%
  arrange(Interviews) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0%)")) %>%
  mutate_all(~ replace(., . == "Inf", "0"))
  
row_index <- 1

# Keep columns that do not have NA in the specified row
ctable2 <- ctable2 %>%
  select(where(~ !is.na(.x[row_index])))

ctable2 <- ctable2 %>%
  mutate(across(-Interviews, 
                 ~ ifelse(row_number() == 3 & is.na(.), "0.0%", .)))
  
ctable2

```

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("EAs"= etable2,
                 "Households"=htable2,
                 "Individuals"=itable2,
                 "Biomarkers tests"=btable2, 
                 "Interviews"=ctable2), 
            paste0("./results/", "2cd-CODPHIA_Overall Monitoring Report, by Team_", 
                   format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 2g TREND OVER AREA

## Notes:

```{r 2g1, table e, echo=FALSE,warning=FALSE,message=FALSE}

#EAs

e0b = sss_df %>%
  group_by(province) %>%
  summarize(n = n_distinct(eacode))

e_dfb = hh_df %>% 
  mutate(hh_complete = if_else(!is.na(hhqsts) & (!is.na(hhqets) | !is.na(hhqenddt)), 1, 0)) %>% 
  full_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>% 
  group_by(province, eacode) %>% 
  reframe(yes = n_distinct(hhi_shh[which(hh_complete == 1)]),
          expected = sampled_hh,
          ratios = yes/expected) %>%
  distinct()

e1b = e_dfb %>% 
  filter(ratios > 0 & ratios < 0.96) %>%
  group_by(province) %>%
  summarize(n = n_distinct(eacode),
            ratios = median(ratios)) %>%
  reframe(province = province, n = paste0(n, " (", format(round(as.numeric(n)/as.numeric(e0b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[E1] Started or In Progress With Collection (E4 + E5)")

e2b = e_dfb %>% 
  filter(ratios >= 0.96) %>% 
  group_by(province) %>%
  summarize(n = n_distinct(eacode),
            ratios = median(ratios)) %>%
  reframe(province = province, n = paste0(n, " (",format(round(as.numeric(n)/as.numeric(e0b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[E2] Completed, n(%)")

e3b = hh_df %>%
  mutate(hh_complete = if_else(!is.na(hhqsts) & !is.na(coalesce(hhqets, hhqenddt)), 1, 0)) %>% 
  filter(hh_complete == 1) %>%
  group_by(eacode) %>%
  mutate(start_date = min(lubridate::ymd_hms(hhqsts), na.rm=TRUE),
         end_date = max(lubridate::ymd_hms(coalesce(hhqets, hhqenddt)), na.rm=TRUE), 
         tt_comp = difftime(end_date, start_date, units = "days"),
         tt_comp = as.numeric(tt_comp)) %>% 
  group_by(province) %>% 
  reframe(n = paste0(round(mean(tt_comp), 0), " days (", round(median(tt_comp), 0.1), " days)")) %>% 
  mutate(row_name = "[E3] Average (median) no. days to complete EAs")

e4b = e_dfb %>% 
  filter(ratios>=0.5 & ratios<0.96)  %>% 
  group_by(province) %>%
  summarize(n = n_distinct(eacode),
            ratios = median(ratios)) %>%
  reframe(province = province, n = paste0(n, " (", format(round(as.numeric(n)/as.numeric(e0b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(row_name = "[E4] In progress (50-95% HHs completed), n (%)")


e5b1 <- e_dfb %>% 
  filter(ratios > 0 & ratios<0.5) %>%
  group_by(province) %>%
  summarize(n = n_distinct(eacode),
            ratios = median(ratios))

e5b2 <- e0b %>% 
  mutate(e0n = as.integer(n)) %>% 
  select(province, e0n) 
  

e5b <- e5b2 %>% 
  left_join(e5b1) %>% 
  mutate(
    across(c(ratios, n), \(x) replace_na(x, 0)),
    n = paste0(n, " (", format(round(as.numeric(n)/as.numeric(e0n)*100, 1), 
                                      nsmall = 1, trim = TRUE), "%)"),
    row_name = "[E5] Started (<50% HHs completed), n (%)") %>% 
  select(-c(e0n, ratios))

  
e6b = e_dfb %>% 
  filter(ratios==0 | is.na(yes)) %>%
  group_by(province) %>%  
  summarize(n = n_distinct(eacode),
            ratios = median(ratios)) %>%
  reframe(province = province, n = paste0(n, " (", format(round(as.numeric(n)/as.numeric(e0b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  mutate(row_name = "[E6] Not Started, n (%)")

e7b = tibble(row_name = c("TEST", "Progress and completion of EAs is based off of completion of HH interviews and does not take into account completion of individual interviews and blood testing."), n = c(NA, NA), province = c(NA, NA))

e0b = e0b %>%
    mutate(row_name = "[E0] Target",
           across(everything(), as.character))

etable3 = bind_rows(!!!mget(paste0("e", seq(from = 0, to = 7), "b"))) %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  merge(etable, by.x = "row_name", by.y = "EA", all = TRUE) %>%
  select(-("NA")) %>%
  relocate(Statistic, .after = "row_name") %>%
  rename(EAs = row_name,
         All = Statistic,
         "Haut-Katanga" = "18",
         Lualaba = "20") %>%
  mutate(EAs = fct_relevel(EAs, "TEST", after = 7)) %>%
  arrange(EAs) %>%
  drop_na(EAs) %>%
  mutate_all(~ replace(., . == "TEST", NA)) 
# Not needed?
# %>%
#   mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))


etable3

```

```{r 2g2, table h, echo=FALSE,warning=FALSE,message=FALSE}

# Households

h0b <- hh_df %>%
  full_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>%
  group_by(eacode, province) %>%
  summarize(n = (sampled_hh)) %>%
  distinct() %>%
  group_by(province) %>%
  summarize(n = sum(n)) %>%
  mutate(row_name = "[H0] Target")

h1b <- hh_df %>%  
  group_by(province) %>% 
  summarize(n = n_distinct(hhi_shh)) %>%
  mutate(row_name = "[H1] Total HH Counts, n (% of Target) (1)",
         nh = paste0(n, " (", format(round(n/as.numeric(h0b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h2b = hh_df %>% 
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6))])) %>%
  mutate(row_name = "[H2] Eligible HHs, n (% of HH forms received) (2)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h3b = hh_df %>% 
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & hhconstat == 1)])) %>%
  mutate(row_name = "     [H3] Consented, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h4b = hh_df %>% 
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) &
                                           (resultndt == 2 | hhconstat == 2 | startint == 4))])) %>%
  mutate(row_name = "     [H4] Refused, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h5b = hh_df %>%
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 1)])) %>%
  mutate(row_name = "     [H5] HH not available at all visit attempts, n (% of eligible HHs)",
         nh= paste0(n, " (", format(round(n/as.numeric(h2b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h6b = hh_df %>% 
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 5)])) %>% 
  mutate(row_name = "[H6] Ineligible - Dwelling not found, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h7b = hh_df %>%
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 3)])) %>%
  mutate(row_name = "[H7] Ineligible - vacant/address not a dwelling, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h8b = hh_df %>% 
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 6)])) %>% 
  mutate(row_name = "     [H8] Household absent for extended period of time, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h9b = hh_df %>% 
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 4)])) %>% 
  mutate(row_name = "[H9] Ineligible - destroyed, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h10b = hh_df %>% 
  group_by(province) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 96)])) %>% 
  mutate(row_name = "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character))

h11b = tibble(row_name = c("TEST", "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload."), n = c(NA, NA))
  
h12b = tibble(row_name = "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", n = NA)

h0b = h0b %>%
  rename(nh = n) %>%
  mutate(across(everything(), as.character))

htable3 = bind_rows(!!!mget(paste0("h", seq(from = 0, to = 12), "b"))) %>% 
  select(province, row_name, nh) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  merge(htable, by.x = "row_name", by.y = "Household", all = TRUE) %>%
  relocate(Statistic, .after = row_name) %>%
  rename(Households = row_name,
         "Haut-Katanga" = "18",
         Lualaba = "20",
         All = Statistic) %>%
  mutate(Households = fct_relevel(Households, "[H0] Target", after = 0L),
         Households = fct_relevel(Households, "[H1] Total HH Counts, n (% of Target) (1)", after = 1),
         Households = fct_relevel(Households, "[H2] Eligible HHs, n (% of HH forms received) (2)", after = 2),
         # Households = fct_relevel(Households, "[H9] Ineligible - destroyed, n (% of HH forms received)", after = 9),
         Households = fct_relevel(Households, "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)", after = 12),
         Households = fct_relevel(Households, "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload.", after = 13),
         Households = fct_relevel(Households, "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", after = 14),
         Households = fct_relevel(Households, "TEST", after = 11)) %>%
  arrange(Households) %>%
  drop_na(Households) %>%
  select(-c('NA')) %>%
  mutate_all(~ replace(., . == "TEST", NA)) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))


htable3

```

```{r 2g3, table i, echo=FALSE,warning=FALSE,message=FALSE}

# Individuals

mydf = roster_df %>%
  left_join(indiv_df, by = c("province","city","eacode","hhi_shh"="ind_shh","hhi_uuid"="ind_hhi_uuid","ea_hhid_fixed","ea_hhid_ln_fixed","hhi_team_id"="ind_team_id","ageyears")) %>% 
  mutate(confgend = na_if(confgend, 0),
         gender = as.numeric(coalesce(confgend, sex)),
         age = as.numeric(coalesce(confagey, ageyears))) 

i0b = tibble(province = c(18,20),
                n = c(4483, 7672)) %>%
  mutate(row_name = "[I0] Target no. of interviews")
  
i1b = mydf %>%
  group_by(province) %>%
  summarize(n1 = n_distinct(hhi_uuid[which(gender == 1 | gender == 2)], 
                            ea_hhid_ln_fixed[which(gender == 1 | gender == 2)])) %>% 
  mutate(row_name = "[I1] Total no. rostered individuals",
         across(everything(), as.character))

i2b = mydf %>%
  group_by(province) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 2 & (gender == 1 | gender == 2))], 
                            ea_hhid_ln_fixed[which(eligible1 == 2 & (gender == 1 | gender == 2))])) %>%
  mutate(row_name = "[I2] Ineligible (based on HH roster info)",
         across(everything(), as.character)) 

i3b = mydf %>%
  group_by(province) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)])) %>% 
  mutate(row_name ="[I3] Eligible HH members (age 15+ and slept over the night before) (1)",
         across(everything(), as.character))

i31b = mydf %>%
  group_by(province) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0)) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))],
                            ea_hhid_ln_fixed[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))])) %>% 
  mutate(row_name = "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status",
         across(everything(), as.character))
    
i4b = mydf %>%
  group_by(province) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)])) %>%
  mutate(row_name = "     [I4] Consented, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i5b = mydf %>%
  group_by(province) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")])) %>%
  mutate(n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         row_name = "     [I5] Cannot Collect: After 3 attempts, n (% of eligible HH members with a status)",
         across(everything(), as.character))

i55b = mydf %>%
  group_by(province) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")])) %>%
  mutate(row_name = "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i6b = mydf %>%
  group_by(province) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                 strtinstr %in% c(1, 4) & constat == "2"))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                         strtinstr %in% c(1, 4) & constat == "2"))]))%>% 
  mutate(row_name = "     [I6] Refused, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i7b = mydf %>%
  group_by(province) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))])) %>% 
  mutate(row_name = "     [I7] Ineligible via screening criteria, n (% of eligible HH members with a status) (2)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i71b = mydf %>%
  group_by(province) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))])) %>%
  mutate(row_name = "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i8b = mydf %>%
  group_by(province) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0),
         exc_8 = case_when(eligible1 == 1 & strtinstr %in% c(1, 4) & constat== "1" ~ 1, #i4
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "1" ~ 1, # i5
                           (eligible1 == 1 & strtinstr==2 & ind0040 == "2" |
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "2") ~ 1, #i6
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "3" ~ 1,
                           eligible1 == 1 & (strtinstr %in% c(1, 4) & constat == "0" & 
                                               (is.na(confagey) | confagey < 15)) ~ 1,
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "99" ~ 1,
                           .default = 0),
         tbd = case_when(inc_8 == 1 | exc_8 == 0 ~ 1,
                         .default = 0)) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))])) %>% 
  mutate(row_name = "     [I8] Approximate TBD, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i9b = mydf %>%
  group_by(province) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))])) %>% 
  mutate(row_name = "[I9] No IND record",
         across(everything(), as.character))

i10b = tibble(row_name = c("TEST", "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined."), n = c(NA, NA))

i11b = tibble(row_name = "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", n = NA)

i0b = i0b %>%
  rename(n1 = n) %>%
  mutate(across(everything(), as.character))

itable3 = bind_rows(!!!mget(paste0("i", c(31, 55, 71, seq(from = 0, to = 11)),"b"))) %>% 
  select(province, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  merge(summary_i, by.x = "row_name", by.y = "Statistic", all = TRUE) %>%
  mutate(row_name = fct_relevel(row_name, "[I0] Target no. of interviews", after = 0L),
         row_name = fct_relevel(row_name, "[I1] Total no. rostered individuals", after = 1),
         row_name = fct_relevel(row_name, "[I2] Ineligible (based on HH roster info)", after = 2),
         row_name = fct_relevel(row_name, "[I3] Eligible HH members (age 15+ and slept over the night before) (1)", after = 3),
         row_name = fct_relevel(row_name, "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status", after = 4),
         row_name = fct_relevel(row_name, "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)", after = 7),
         row_name = fct_relevel(row_name, "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)", after = 10),
         row_name = fct_relevel(row_name, "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined.", after = 14),
         row_name = fct_relevel(row_name, "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", after = 15),
         row_name = fct_relevel(row_name, "TEST", after = 13)) %>%
  arrange(row_name) %>%
  relocate("All", .after = row_name) %>%
  rename(Individuals = row_name,
         "Haut-Katanga" = "18",
         Lualaba = "20") %>%
  drop_na(Individuals) %>%
  select(-c('NA')) %>%
  mutate_all(~ replace(., . == "TEST", NA)) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))
  
itable3

```

```{r 2g4, table b, echo=FALSE,warning=FALSE,message=FALSE}

# Biomarker test

mydata = mydata %>%
  mutate(hivstatus_final =
           case_when(
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 1 | biohivtst2rs2 == 1) & (biohivtst3rs == 1 | biohivtst3rs2 == 1) ~ 1), # hiv positive
             (biohivtst1rs == 2 | biohivtst1rs2 == 2 | ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 2 | biohivtst1ars2 == 2))) ~ 2, # hiv negative
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs ==1 | biohivtst2rs2 == 1) & (biohivtst3rs == 2 | biohivtst3rs2 == 2) |
                ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 1 | biohivtst1ars2 == 1))) ~ 3, # indeterminate
             biohivtst1rs2 == 3 | biohivtst2rs2 == 3 | biohivtst1ars2 == 3 | biohivtst3rs2 == 3 ~ 4) # invalid 
           )

b0b <- tibble(province = c(18, 20),
              n1 = c("4249", "7273")) %>%
  mutate(row_name = "[B0] Target no. of blood draws")

b1b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))], ea_hhid_ln_fixed[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))])) %>% # change i4 to i4a
  mutate(row_name = "[B1] Consented to blood draw, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4b$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b2b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>%
  mutate(row_name = "[B2] Blood draws achieved, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4b$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)"))

b3b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>% 
  mutate(row_name = "[B3] Percent of target blood draws achieved",
         n1 = paste0(format(round(n1_d/as.numeric(b0b$n1)*100, 1), nsmall = 1, trim = TRUE), "%")) %>% 
  select(-contains("_d"))

b4b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1)], ea_hhid_ln_fixed[which(hivstatus_final == 1)])) %>%
  mutate(row_name = "[B4] HIV rapid test: Number (%) of positive",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2b$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)"))

b41b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1 & ltcvrbl == 1)], 
                              ea_hhid_ln_fixed[which(hivstatus_final == 1 & ltcvrbl == 1)])) %>%
  mutate(row_name = "     [B4.1] Consented to ALTC, n (% of HIV-positive)",
         n1 = paste0(n1_d, " (", format(round(n1_d/b4b$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b5b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 2)])) %>%
  mutate(row_name = "[B5] HIV rapid test: Number (%) of negative",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2b$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b6b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 3)]))%>%
  mutate(row_name = "[B6] HIV rapid test: Number (%) of indeterminate",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2b$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b7b = mydata %>%
  group_by(province) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 4)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 4)])) %>%
  mutate(row_name = "[B7] HIV rapid test: Number (%) of invalid",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2b$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b0b = b0b %>%
  mutate(across(everything(), as.character))

btable3 = bind_rows(!!!mget(paste0("b", c(41, seq(from = 0, to = 7)), "b"))) %>% 
  select(province, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  merge(summary_b, by.x = "row_name", by.y = "Statistic", all = TRUE) %>%
  mutate(row_name = fct_relevel(row_name, "     [B4.1] Consented to ALTC, n (% of HIV-positive)", after = 5),
         row_name = fct_relevel(row_name, "[B0] Target", after = 0L)) %>%
  arrange(row_name) %>%
  relocate("All above 15 years", .after = "row_name") %>%
  rename(Biomarkers = row_name,
         "Haut-Katanga" = "18",
         Lualaba = "20") %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))

btable3

```

```{r 2g5, table c, echo=FALSE,warning=FALSE,message=FALSE}

# Interviews

crow_func = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(province = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(province) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(province, median) %>% 
    pivot_wider(names_from = province, values_from = median) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

crow_func_mean = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(province = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(province) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(province, mean) %>% 
    pivot_wider(names_from = province, values_from = mean) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

c5_func = function(df1, df2, row_name){
  my_ca = df1 %>% 
    pivot_longer(cols = -1, values_to = "values", names_to = "hhi_eacode") %>% 
    mutate(ind_count1 = str_extract(values, "^[:digit:]+(?=[:space:])"))
  my_cb = df2 %>% 
    pivot_longer(cols = -1, values_to = "ind_count2", names_to = "hhi_eacode")
  my_cc = my_ca %>% 
    left_join(my_cb, by = "hhi_eacode") %>% 
    mutate(across(starts_with("ind_count"), as.numeric),
           ind_percent = format(trim = TRUE, ind_count1/ind_count2*100, digits = 3, nsmall = 1, drop0trailing = TRUE),
           total_ind_counts = paste0(ind_count1, " (", ind_percent, "%)")) %>% 
    select(hhi_eacode, total_ind_counts) %>% 
    pivot_wider(names_from = hhi_eacode, values_from = total_ind_counts) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(my_cc)
}

c3_func = function(df1, df2, row_name){
  my_ca = df1 %>% 
    pivot_longer(cols = -1, values_to = "values", names_to = "hhi_eacode") %>% 
    mutate(ind_count1 = str_extract(values, "^[:digit:]+(?=[:space:])"))
  my_cb = df2 %>% 
    pivot_longer(cols = -1, values_to = "values", names_to = "hhi_eacode") %>% 
    mutate(ind_count2 = str_extract(values, "^[:digit:]+(?=[:space:])"))
  my_cc = my_ca %>% 
    left_join(my_cb, by = "hhi_eacode") %>% 
    mutate(across(starts_with("ind_count"), as.numeric),
           ind_percent = format(trim = TRUE, ind_count1/ind_count2*100, digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    select(hhi_eacode, ind_percent) %>% 
    pivot_wider(names_from = hhi_eacode, values_from = ind_percent) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(my_cc)
}

c1b = crow_func(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))
c11b = crow_func_mean(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))

c2b = crow_func(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))
c21b = crow_func(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))

c1b <- c1b %>%
  mutate(across(everything(), ~ paste(.x, " (", c11b[[cur_column()]], ")", sep = ""))) %>%
  mutate(row_name = "[C1] Median (average) time to complete entire individual interview (min) (only includes those who completed the interview)")

c2b <- c2b %>%
  mutate(across(everything(), ~ paste(.x, " (", c21b[[cur_column()]], ")", sep = ""))) %>%
  mutate(row_name = "[C2] Median (average) time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)")

c4b = h3b %>% 
  mutate(row_name = recode(row_name, "     [H3] Consented, n (% of eligible HHs)" = "     [C4] Consented, n (% of eligible HHs)")) %>%
  select(-("n")) %>%
  pivot_wider(names_from = 1, values_from = 3)

b2b
i31b

c5b = tibble('18' = paste0(b2b[1,2], " (", format(round(b2b[1,2]/as.numeric(i31b[1,2])*100, 1), nsmall = 1, trim = TRUE), "%)"),
             '20' = paste0(b2b[2,2], " (", format(round(b2b[2,2]/as.numeric(i31b[2,2])*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  pivot_longer(1:2) %>%
  mutate(row_name = "     [C5] Blood draws achieved, n (% of eligible HH members with a status)") %>%
  rename(province = name,
         n1 = value) %>%
  pivot_wider(names_from = 1, values_from = 2)

c3b_func <- function(var) {
  # Remove everything before the first opening parenthesis and the parenthesis itself
  result <- gsub(".*\\(\\s*", "", var)

  # Remove the closing parenthesis, if any
  result <- gsub("\\)", "", result)

  # Remove the percent sign, if any
  result <- gsub("%", "", result)

  # Convert the remaining string to a numeric value
  result <- as.numeric(result)

  return(result)
}

# Multiply the percentages of c4 and c5 without recalculating (using strings)
c3b = tibble(n1_a = c3b_func(c4b$'18'),
            n1_b = c3b_func(c5b$'18'),
            n2_a = c3b_func(c4b$'20'),
            n2_b = c3b_func(c5b$'20'),
            '18' = paste0(round(n1_a*n1_b/100, 1), "%"),
            '20' = paste0(round(n2_a*n2_b/100, 1), "%")) %>%
  select(-contains("_a"), -contains("_b")) %>%
  mutate(row_name = "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)")

c7b <- i4b %>%
  mutate(row_name = "     [C7] Consented to individual, n (% of eligible HH members with a status)") %>%
  select(-contains("_d"))

c8b <- b2b %>%
  mutate(row_name = "     [C8] Blood draws achieved, n (% of consented to interview)") %>%
  select(-contains("_d"))

c6b <- c7b %>%
  left_join(c8b, by = "province") %>%
  mutate(percentage_x = sapply(n1.x, extract)) %>%
  mutate(percentage_x = percentage_x / 100) %>%
  mutate(percentage_y = sapply(n1.y, extract)) %>%
  mutate(percentage_y = percentage_y / 100) %>%
  mutate(n1 = paste0(format(round(percentage_x * percentage_y * 100, 1), nsmall = 1, trim = FALSE), "%"),
         row_name = "[C6] Overall individual response rate (% individual consent X % actual blood draws)") %>%
  select(province, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3)  

c7b <- c7b %>%
  pivot_wider(names_from = 1, values_from = 3)

c8b <- c8b %>%
  pivot_wider(names_from = 1, values_from = 3)

summary_c <- ctable %>%
  select(c("Statistic","All"))

ctable3 = bind_rows(!!!mget(paste0("c", seq(from = 1, to = 8),"b"))) %>% 
  merge(summary_c, by.x = "row_name", by.y = "Statistic", all = TRUE) %>%
  relocate(All, .after = row_name) %>%
  rename(Interviews = row_name,
         "Haut-Katanga" = "18",
         Lualaba = "20") %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  mutate(Interviews = fct_relevel(Interviews, "[C1] Median (average) time to complete entire individual interview (min) (only includes those who completed the interview)", after = 0L),
         Interviews = fct_relevel(Interviews, "[C2] Median (average) time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)", after = 2),
         Interviews = fct_relevel(Interviews, "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)", after = 3),
         Interviews = fct_relevel(Interviews, "     [C4] Consented, n (% of eligible HHs)", after = 4),
         Interviews = fct_relevel(Interviews, "     [C5] Blood draws achieved, n (% of eligible HH members with a status)", after = 5),
         Interviews = fct_relevel(Interviews, "[C6] Overall individual response rate (% individual consent X % actual blood draws)", after = 6),
         Interviews = fct_relevel(Interviews, "     [C7] Consented to individual, n (% of eligible HH members with a status)", after = 7),
         Interviews = fct_relevel(Interviews, "     [C8] Blood draws achieved, n (% of consented to interview)", after = 8)) %>%
  arrange(Interviews)

ctable3

```

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("EAs"= etable3,
                 "Households"=htable3,
                 "Individuals"=itable3,
                 "Biomarkers tests"=btable3, 
                 "Interviews"=ctable3), 
            paste0("./results/", "2g-CODPHIA_Overall Monitoring Report, by Province_", 
                   format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 2b TREND OVER EA

```{r 2b, table h, echo=FALSE,warning=FALSE,message=FALSE}

# Households
hh_df <- hh_df %>%
   distinct(hhi_shh, .keep_all = TRUE)

h0c <- hh_df %>%
  left_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>%
  group_by(short_ea) %>%
  summarize(nh = (sampled_hh)) %>%
  distinct() %>%
  mutate(row_name = "[H0] Target") %>%
  rename(eacode = short_ea)

h1c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh)) %>%
  mutate(row_name = "[H1] Total HH Counts, n (% of Target) (1)",
         nh = (paste0(n, " (", format(round((n/h0c$nh)*100, 1), nsmall = 1, trim = TRUE), "%)"))) %>%
  rename(eacode = short_ea)

h2c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6))])) %>%
  mutate(row_name = "[H2] Eligible HHs, n (% of HH forms received) (2)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h3c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & hhconstat == 1)])) %>%
  mutate(row_name = "     [H3] Consented, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h4c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) &
                                           (resultndt == 2 | hhconstat == 2 | startint == 4))])) %>%
  mutate(row_name = "     [H4] Refused, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h5c = hh_df %>%
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 1)])) %>%
  mutate(row_name = "     [H5] HH not available at all visit attempts, n (% of eligible HHs)",
         nh= paste0(n, " (", format(round(n/as.numeric(h2c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h6c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 5)])) %>% 
  mutate(row_name = "[H6] Ineligible - Dwelling not found, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h7c = hh_df %>%
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 3)])) %>%
  mutate(row_name = "[H7] Ineligible - vacant/address not a dwelling, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h8c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 6)])) %>% 
  mutate(row_name = "     [H8] Household absent for extended period of time, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h9c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 4)])) %>% 
  mutate(row_name = "[H9] Ineligible - destroyed, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h10c = hh_df %>% 
  group_by(short_ea) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 96)])) %>% 
  mutate(row_name = "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1c$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  rename(eacode = short_ea)

h11c = tibble(row_name = c("TEST", "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload."), nh = c(NA, NA))
  
h12c = tibble(row_name = "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", nh = NA)

h1c = h1c %>%
  mutate(across(everything(), as.character))

h0c = h0c %>%
  mutate(across(everything(), as.character))

htable4 = bind_rows(!!!mget(paste0("h", seq(from = 0, to = 12), "c"))) %>% 
  select(eacode, row_name, nh) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  mutate_all(~ replace(., . == "TEST", NA)) %>%
  merge(htable, by.x = "row_name", by.y = "Household") %>%
  mutate(row_name = case_when(row_number() == 14 ~ "TEST", TRUE ~ row_name)) %>%
  relocate(Statistic, .after = row_name) %>%
  rename(Households = row_name,
         All = Statistic) %>%
  mutate(Households = fct_relevel(Households, "[H0] Target", after = 0L),
         Households = fct_relevel(Households, "[H1] Total HH Counts, n (% of Target) (1)", after = 1),
         Households = fct_relevel(Households, "[H2] Eligible HHs, n (% of HH forms received) (2)", after = 2),
         # # Households = fct_relevel(Households, "[H9] Ineligible - destroyed, n (% of HH forms received)", after = 9),
         Households = fct_relevel(Households, "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)", after = 12),
         Households = fct_relevel(Households, "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload.", after = 14),
         Households = fct_relevel(Households, "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", after = 15),
         Households = fct_relevel(Households, "TEST", after = 11)) %>%
  arrange(Households) %>%
  select(-c("NA")) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "TEST", NA))

htable4

```

```{r 2g6, table i, echo=FALSE,warning=FALSE,message=FALSE}

# Individuals

mydf = roster_df %>%
  left_join(indiv_df, by = c("province","city","eacode","short_ea","eacode_num","hhi_shh"="ind_shh","hhi_uuid"="ind_hhi_uuid","ea_hhid_fixed","ea_hhid_ln_fixed","hhi_team_id"="ind_team_id","ageyears")) %>% 
  mutate(confgend = na_if(confgend, 0),
         gender = as.numeric(coalesce(confgend, sex)),
         age = as.numeric(coalesce(confagey, ageyears))) 

i0c <- h0c %>%
  mutate(n = round(as.numeric(nh)/4974 * 12155)) %>%
  mutate(row_name = "[I0] Target no. of interviews") %>%
  select(eacode, n, row_name)

# i0c = mydf %>%
#   group_by(short_ea) %>%
#   summarize(n = ceiling(12155/132)) %>%
#   mutate(row_name = "[I0] Target no. of interviews") %>%
#   rename(eacode = short_ea)
  
i1c = mydf %>%
  group_by(short_ea) %>%
  summarize(n1 = n_distinct(hhi_uuid[which(gender == 1 | gender == 2)], 
                            ea_hhid_ln_fixed[which(gender == 1 | gender == 2)])) %>% 
  mutate(row_name = "[I1] Total no. rostered individuals",
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i2c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 2 & (gender == 1 | gender == 2))], 
                            ea_hhid_ln_fixed[which(eligible1 == 2 & (gender == 1 | gender == 2))])) %>%
  mutate(row_name = "[I2] Ineligible (based on HH roster info)",
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i3c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)])) %>% 
  mutate(row_name ="[I3] Eligible HH members (age 15+ and slept over the night before) (1)",
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i31c = mydf %>%
  group_by(short_ea) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0)) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))],
                            ea_hhid_ln_fixed[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))])) %>% 
  mutate(row_name = "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status",
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)
    
i4c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)])) %>%
  mutate(row_name = "     [I4] Consented, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31c$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character)) %>%
  rename(eacode = short_ea) %>%
  filter(n1_d != 0)

i5c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")])) %>%
  mutate(n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31c$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         row_name = "     [I5] Cannot Collect: After 3 attempts, n (% of eligible HH members with a status)",
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i55c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")])) %>%
  mutate(row_name = "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31c$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i6c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                 strtinstr %in% c(1, 4) & constat == "2"))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                         strtinstr %in% c(1, 4) & constat == "2"))]))%>% 
  mutate(row_name = "     [I6] Refused, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31c$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i7c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))])) %>% 
  mutate(row_name = "     [I7] Ineligible via screening criteria, n (% of eligible HH members with a status) (2)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31c$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i71c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))])) %>%
  mutate(row_name = "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31c$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i8c = mydf %>%
  group_by(short_ea) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0),
         exc_8 = case_when(eligible1 == 1 & strtinstr %in% c(1, 4) & constat== "1" ~ 1, #i4
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "1" ~ 1, # i5
                           (eligible1 == 1 & strtinstr==2 & ind0040 == "2" |
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "2") ~ 1, #i6
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "3" ~ 1,
                           eligible1 == 1 & (strtinstr %in% c(1, 4) & constat == "0" & 
                                               (is.na(confagey) | confagey < 15)) ~ 1,
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "99" ~ 1,
                           .default = 0),
         tbd = case_when(inc_8 == 1 | exc_8 == 0 ~ 1,
                         .default = 0)) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))])) %>% 
  mutate(row_name = "     [I8] Approximate TBD, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31c$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i9c = mydf %>%
  group_by(short_ea) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))])) %>% 
  mutate(row_name = "[I9] No IND record",
         across(everything(), as.character)) %>%
  rename(eacode = short_ea)

i10c = tibble(row_name = c("TEST", "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined."), n = c(NA, NA))

i11c = tibble(row_name = "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", n = NA)

i0c = i0c %>%
  rename(n1 = n) %>%
  mutate(across(everything(), as.character))

itable4 = bind_rows(!!!mget(paste0("i", c(31, 55, 71, seq(from = 0, to = 11)),"c"))) %>% 
  select(eacode, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  # mutate(row_name = case_when(row_number() == 14 ~ "TEST", TRUE ~ row_name)) %>%
  # mutate_all(~ replace(., . == "TEST", NA)) %>%
  merge(summary_i, by.x = "row_name", by.y = "Statistic", all = TRUE) %>%
  mutate(row_name = fct_relevel(row_name, "[I0] Target no. of interviews", after = 0L),
         row_name = fct_relevel(row_name, "[I1] Total no. rostered individuals", after = 1),
         row_name = fct_relevel(row_name, "[I2] Ineligible (based on HH roster info)", after = 2),
         row_name = fct_relevel(row_name, "[I3] Eligible HH members (age 15+ and slept over the night before) (1)", after = 3),
         row_name = fct_relevel(row_name, "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined.", after = 14),
         row_name = fct_relevel(row_name, "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", after = 15),
         row_name = fct_relevel(row_name, "TEST", after = 13),
         row_name = fct_relevel(row_name, "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status", after = 4),
         row_name = fct_relevel(row_name, "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)", after = 8),
         row_name = fct_relevel(row_name, "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)", after = 10)) %>%
  arrange(row_name) %>%
  relocate("All", .after = row_name) %>%
  rename(Individuals = row_name) %>%
  select(-c("NA")) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "TEST", NA))

itable4 <- itable4 %>%
  mutate(across(everything(), 
                 ~ if_else(row_number() == 6 & is.na(.), "0 (0.0%)", .x)))  
  
itable4

```

```{r 2g7, table b, echo=FALSE,warning=FALSE,message=FALSE}

# Biomarker test

mydata = mydata %>%
  mutate(hivstatus_final =
           case_when(
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 1 | biohivtst2rs2 == 1) & (biohivtst3rs == 1 | biohivtst3rs2 == 1) ~ 1), # hiv positive
             (biohivtst1rs == 2 | biohivtst1rs2 == 2 | ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 2 | biohivtst1ars2 == 2))) ~ 2, # hiv negative
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs ==1 | biohivtst2rs2 == 1) & (biohivtst3rs == 2 | biohivtst3rs2 == 2) |
                ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 1 | biohivtst1ars2 == 1))) ~ 3, # indeterminate
             biohivtst1rs2 == 3 | biohivtst2rs2 == 3 | biohivtst1ars2 == 3 | biohivtst3rs2 == 3 ~ 4) # invalid 
           )

b0c <- i0c %>%
  mutate(n1 = round(as.numeric(n1)/12155 * 11522)) %>%
  mutate(row_name = "[B0] Target no. of blood draws") %>%
  select(eacode, n1, row_name)

# b0c <- mydata %>%
#   group_by(short_ea) %>%
#   summarize(n1 = ceiling(11522/132)) %>%
#   mutate(row_name = "[B0] Target no. of blood draws") %>%
#   rename(eacode = short_ea)

b1c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))], ea_hhid_ln_fixed[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))])) %>% # change i4 to i4a
  drop_na(short_ea) %>%
  mutate(row_name = "[B1] Consented to blood draw, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4c$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  rename(eacode = short_ea)

b2c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>%
  mutate(row_name = "[B2] Blood draws achieved, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4c$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  rename(eacode = short_ea)

b3c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>% 
  filter(!is.na(short_ea)) %>%
  left_join(b0c, by = c("short_ea" = "eacode")) %>%
  mutate(row_name = "[B3] Percent of target blood draws achieved",
         n1 = paste0(format(round(n1_d/as.numeric(n1)*100, 1), nsmall = 1, trim = TRUE), "%")) %>% 
  select(-contains("_d")) %>%
  rename(eacode = short_ea)

b4c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1)], 
                              ea_hhid_ln_fixed[which(hivstatus_final == 1)])) %>%
  mutate(row_name = "[B4] HIV rapid test: Number (%) of positive",
         #n1 = paste0(format(n1_d, " (", round(n1_d/b2c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)"))
         n1 = paste0(n1_d, " (", format(round(n1_d/b2c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  rename(eacode = short_ea)

b41c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1 & ltcvrbl == 1)], 
                              ea_hhid_ln_fixed[which(hivstatus_final == 1 & ltcvrbl == 1)])) %>%
  mutate(row_name = "     [B4.1] Consented to ALTC, n (% of HIV-positive)",
  #       n1 = paste0(format(n1_d, " (", round(n1_d/b4c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  n1 = paste0(n1_d, " (", format(round(n1_d/b4c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  rename(eacode = short_ea)

b5c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 2)])) %>%
  mutate(row_name = "[B5] HIV rapid test: Number (%) of negative",
         #n1 = paste0(format(n1_d, " (", round(n1_d/b2c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
         n1 = paste0(n1_d, " (", format(round(n1_d/b2c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  rename(eacode = short_ea)

b6c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 3)]))%>%
  mutate(row_name = "[B6] HIV rapid test: Number (%) of indeterminate",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  rename(eacode = short_ea)

b7c = mydata %>%
  group_by(short_ea) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 4)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 4)])) %>%
  mutate(row_name = "[B7] HIV rapid test: Number (%) of invalid",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2c$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d")) %>%
  rename(eacode = short_ea)

b0c = b0c %>%
  mutate(across(everything(), as.character))

btable4 = bind_rows(!!!mget(paste0("b", c(41, seq(from = 0, to = 7)), "c"))) %>% 
  select(eacode, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  merge(summary_b, by.x = "row_name", by.y = "Statistic") %>%
  mutate(row_name = fct_reorder(row_name, row_name),
         row_name = fct_relevel(row_name, "     [B4.1] Consented to ALTC, n (% of HIV-positive)", after = 5),
         row_name = fct_relevel(row_name, "[B0] Target", after = 0L)) %>%
  arrange(row_name) %>%
  relocate("All above 15 years", .after = "row_name") %>%
  rename(Biomarkers = row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))  %>%
  select(-c("NA"))

btable4

```

```{r 2g8, table c, echo=FALSE,warning=FALSE,message=FALSE}

# Interviews

crow_func = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(short_ea = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(short_ea) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(short_ea, median) %>% 
    pivot_wider(names_from = short_ea, values_from = median) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

crow_func_mean = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(short_ea = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(short_ea) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(short_ea, mean) %>% 
    pivot_wider(names_from = short_ea, values_from = mean) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

# c3_func = function(df1, df2, row_name){
#   my_ca = df1 %>% 
#     pivot_longer(cols = -1, values_to = "values", names_to = "hhi_eacode") %>% 
#     mutate(ind_count1 = str_extract(values, "^[:digit:]+(?=[:space:])"))
#   my_cb = df2 %>% 
#     pivot_longer(cols = -1, values_to = "values", names_to = "hhi_eacode") %>% 
#     mutate(ind_count2 = str_extract(values, "^[:digit:]+(?=[:space:])"))
#   my_cc = my_ca %>% 
#     left_join(my_cb, by = "hhi_eacode") %>% 
#     mutate(across(starts_with("ind_count"), as.numeric),
#            ind_percent = format(trim = TRUE, ind_count1/ind_count2*100, digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
#     select(hhi_eacode, ind_percent) %>% 
#     pivot_wider(names_from = hhi_eacode, values_from = ind_percent) %>% 
#     mutate(row_name = row_name) %>% 
#     select(row_name, everything())
#   
#   return(my_cc)
# }

c1c = crow_func(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))
c11c = crow_func_mean(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))

c2c = crow_func(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))
c21c = crow_func_mean(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))

c1c <- c1c %>%
  mutate(across(everything(), ~ paste(.x, " (", c11c[[cur_column()]], ")", sep = "")))
c2c <- c2c %>%
  mutate(across(everything(), ~ paste(.x, " (", c21c[[cur_column()]], ")", sep = "")))


c4c = h3c %>% 
  mutate(row_name = recode(row_name, "     [H3] Consented, n (% of eligible HHs)" = "     [C4] Consented, n (% of eligible HHs)")) %>%
  select(-("n"))

b2c
i31c

c5c = tibble(eacode = unique(hh_df$short_ea)) %>%
  left_join(b2c, by = "eacode") %>%
  left_join(i31c, by = "eacode")

c5c = c5c %>%
  mutate(statistic = paste0(n1_d, " (", format(round(n1_d/as.numeric(n1.y)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(c("eacode","statistic")) %>%
  mutate(row_name = "     [C5] Blood draws achieved")

# c3c = c3_func(df1 = c4c, df2 = c5c, row_name = "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)") %>%
#     mutate(across(everything(), ~paste0(., "%")))

c3c <- c5c %>%
  left_join(c4c, by = "eacode") %>%
  mutate(percentage_x = sapply(statistic, extract)) %>%
  mutate(percentage_x = percentage_x / 100) %>%
  mutate(percentage_y = sapply(nh, extract)) %>%
  mutate(percentage_y = percentage_y / 100) %>%
  mutate(n1 = paste0(format(round(percentage_x * percentage_y * 100, 1), nsmall = 1, trim = FALSE), "%"),
         row_name = "[C6] Overall individual response rate (% individual consent X % actual blood draws)") %>%
  select(eacode, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3)

c4c <- c4c %>%
  pivot_wider(names_from = 1, values_from = 3)
c5c <- c5c %>%
  pivot_wider(names_from = 1, values_from = 2)

c7c <- i4c %>%
  mutate(row_name = "     [C7] Consented to individual, n (% of eligible HH members with a status)") %>%
  select(-contains("_d"))

c8c <- b2c %>%
  mutate(row_name = "     [C8] Blood draws achieved, n (% of consented to interview)") %>%
  select(-contains("_d"))

c6c <- c7c %>%
  left_join(c8c, by = "eacode") %>%
  mutate(percentage_x = sapply(n1.x, extract)) %>%
  mutate(percentage_x = percentage_x / 100) %>%
  mutate(percentage_y = sapply(n1.y, extract)) %>%
  mutate(percentage_y = percentage_y / 100) %>%
  mutate(n1 = paste0(format(round(percentage_x * percentage_y * 100, 1), nsmall = 1, trim = FALSE), "%"),
         row_name = "[C6] Overall individual response rate (% individual consent X % actual blood draws)") %>%
  select(eacode, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3)  

c7c <- c7c %>%
  pivot_wider(names_from = 1, values_from = 3)

c8c <- c8c %>%
  pivot_wider(names_from = 1, values_from = 3)

summary_c <- ctable %>%
  select(c("Statistic","All"))

ctable4 = bind_rows(!!!mget(paste0("c", seq(from = 1, to = 8),"c"))) %>% 
  #merge(summary_c, by.x = "row_name", by.y = "Statistic") %>%
  mutate(All = summary_c$All) %>% 
  relocate(All, .after = row_name) %>%
  rename(Interviews = row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "NA (NA%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "NA%", "0.0%")) %>%
  mutate_all(~ replace(., is.na(.), "0"))

ctable4

```

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("Households"=htable4,
                 "Individuals"=itable4,
                 "Biomarkers tests"=btable4,
                 "Interviews"=ctable4), 
            paste0("./results/", "2b-CODPHIA_Overall Monitoring Report, by EA_", 
                   format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 2f TREND OVER TIME

## Notes:

```{r 2f, table e, echo=FALSE,warning=FALSE,message=FALSE}

#EAs

hh_df$week_num <- strftime(hh_df$hhqets, format = "%V")

weeks <- tibble(week_start = seq(ymd("20240819"), ymd("20241231"), by = "weeks"),
                week_end = seq(ymd("20240825"), ymd("20250110"), by = "weeks"),
                week_num = as.character(c(34:53)))
  
e0d = hh_df %>%  
  right_join(weeks, by = "week_num") %>%
  group_by(week_num) %>%
  summarize(n = round(132/15)) # number of weeks 

e_dfd = hh_df %>% 
  mutate(hh_complete = if_else(!is.na(hhqsts) & (!is.na(hhqets) | !is.na(hhqenddt)), 1, 0)) %>% 
  full_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>% 
  group_by(eacode) %>% 
  reframe(yes = n_distinct(hhi_shh[which(hh_complete == 1)]),
          expected = sampled_hh,
          ratios = yes/expected,
          completed = max(hhqsts)) %>%
  distinct()

ea_started <- hh_df %>%
  group_by(eacode) %>%
  summarize(start = min(hhqsts))

eas <- tibble(eacode = unique(sss_df$ea_id)) %>%
  left_join(ea_started) %>%
  left_join(e_dfd)

eas_fuzzy = fuzzy_left_join(eas, weeks, by = c(start = "week_start", start = "week_end"), match_fun = list(`>=`, `<=`)) %>%
  rename(week_num_start = week_num) %>%
  group_by(week_num_start) %>%
  summarize(eas = sum(n_distinct(eacode)))

eas_st_end = fuzzy_left_join(eas, weeks, by = c(completed = "week_start", completed = "week_end"), match_fun = list(`>=`, `<=`)) %>%
  mutate(week_num_end = case_when(ratios >= 0.96 ~ week_num,
                                  ratios < 0.96 ~ NA)) %>%
  group_by(week_num_end) %>%
  summarize(eas = sum(n_distinct(eacode))) %>%
  rename(week_num = week_num_end) %>%
  mutate(row_name = "EAs completed")

week_label <- tibble(week_num = as.character(c(34:53)), week_label = c("Week 1 (19 Aug - 25 Aug)", "Week 2 (26 Aug - 1 Sep)", "Week 3 (2 Sep - 8 Sep)", "Week 4 (9 Sep - 15 Sep)", "Week 5 (16 Sep - 22 Sep)", "Week 6 (23 Sep - 29 Sep)", "Week 7 (30 Sep - 6 Oct)", "Week 8 (7 Oct - 13 Oct)", "Week 9 (14 Oct - 20 Oct)", "Week 10 (21 Oct - 27 Oct)", "Week 11 (28 Oct - 3 Nov)", "Week 12 (4 Nov - 10 Nov)", "Week 13 (11 Nov - 17 Nov)", "Week 14 (18 Nov - 24 Nov)", "Week 15 (25 Nov - 1 Dec)", "Week 16 (2 Dec - 8 Dec)", "Week 17 (9 Dec - 15 Dec)", "Week 18 (16 Dec - 22 Dec)", "Week 19 (23 Dec - 29 Dec)", "Week 20 (30 Dec - 5 Jan 2025)"))

eas_st_end <- week_label %>%
  left_join(eas_st_end, by = "week_num") %>%
  fill(row_name, .direction = "downup") 

curr_week <- strftime(Sys.Date(), format = "%V")

etable5 <- full_join(eas_fuzzy, weeks, by = c("week_num_start" = "week_num")) %>%
  mutate(row_name = "EAs started") %>%
  select(-c("week_start", "week_end")) %>%
  rename(week_num = week_num_start)%>%
  left_join(week_label, by = "week_num") %>%
  rbind(eas_st_end) %>%
  relocate("week_label", .before = 1) %>%
  filter(as.numeric(week_num) <= curr_week) %>%
  drop_na(week_num) %>%
  select(-(week_num)) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  rename("EAs" = row_name)

etable5

```

```{r 2f, table h, echo=FALSE,warning=FALSE,message=FALSE}

# Households

hh_df$week_num <- strftime(hh_df$hhqets, format = "%V")

h0d <- hh_df %>%
  group_by(week_num) %>%
  summarize(n = round(4974/15)) %>% #number of weeks
  mutate(row_name = "[H0] Target") %>%
  drop_na(week_num) 

h1d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh)) %>%
  mutate(row_name = "[H1] Total HH Counts, n (% of Target) (1)",
         nh = (paste0(n, " (", format(round((n/h0d$n)*100, 1), nsmall = 1, trim = TRUE), "%)"))) %>%
  drop_na(week_num) 

h2d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6))])) %>%
  mutate(row_name = "[H2] Eligible HHs, n (% of HH forms received) (2)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h3d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & hhconstat == 1)])) %>%
  mutate(row_name = "     [H3] Consented, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h4d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) &
                                           (resultndt == 2 | hhconstat == 2 | startint == 4))])) %>%
  mutate(row_name = "     [H4] Refused, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h5d = hh_df %>%
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 1)])) %>%
  mutate(row_name = "     [H5] HH not available at all visit attempts, n (% of eligible HHs)",
         nh= paste0(n, " (", format(round(n/as.numeric(h2d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h6d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 5)])) %>% 
  mutate(row_name = "[H6] Ineligible - Dwelling not found, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h7d = hh_df %>%
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 3)])) %>%
  mutate(row_name = "[H7] Ineligible - vacant/address not a dwelling, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h8d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 6)])) %>% 
  mutate(row_name = "     [H8] Household absent for extended period of time, n (% of eligible HHs)",
         nh = paste0(n, " (", format(round(n/as.numeric(h2d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h9d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 4)])) %>% 
  mutate(row_name = "[H9] Ineligible - destroyed, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h10d = hh_df %>% 
  group_by(week_num) %>%
  summarize(n = n_distinct(hhi_shh[which(!(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & resultndt == 96)])) %>% 
  mutate(row_name = "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)",
         nh = paste0(n, " (", format(round(n/as.numeric(h1d$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  mutate(across(everything(), as.character)) %>%
  drop_na(week_num)

h11d = tibble(row_name = c("TEST", "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload."), n = c(NA,NA))
  
h12d = tibble(row_name = "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", n = NA)

h1d = h1d %>%
  mutate(across(everything(), as.character))

h0d = h0d %>%
  rename(nh = n) %>%
  mutate(across(everything(), as.character))

htable5 = bind_rows(!!!mget(paste0("h", seq(from = 0, to = 12), "d"))) %>% 
  select(week_num, row_name, nh) %>%
  left_join(week_label, by = "week_num") %>%
  relocate(week_label, .before = 1) %>%
  select(-c("week_num")) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  select(-("NA")) %>%
  mutate(row_name = fct_relevel(row_name, "[H0] Target", after = 0L),
         row_name = fct_relevel(row_name, "[H1] Total HH Counts, n (% of Target) (1)", after = 1),
         row_name = fct_relevel(row_name, "[H2] Eligible HHs, n (% of HH forms received) (2)", after = 2),
         row_name = fct_relevel(row_name, "     [H8] Household absent for extended period of time, n (% of eligible HHs)", after = 6),
         row_name = fct_relevel(row_name, "[H6] Ineligible - Dwelling not found, n (% of HH forms received)", after = 9),
         row_name = fct_relevel(row_name, "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)", after = 12),
         row_name = fct_relevel(row_name, "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload.", after = 13),
         row_name = fct_relevel(row_name, "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", after = 14)) %>%
  arrange(row_name) %>%
  mutate_all(~ replace(., . == "TEST", NA)) %>%
  mutate_all(~ replace(., . == "NULL", "")) %>%
  rename(Households = row_name)

htable5 <- htable5 %>% map_df(~unlist(.x)) 

htable5

```

```{r 2f, table i, echo=FALSE,warning=FALSE,message=FALSE}

# Individuals

mydf = roster_df %>%
  left_join(indiv_df, by = c("province","city","eacode","hhi_shh"="ind_shh","hhi_uuid"="ind_hhi_uuid","ea_hhid_fixed","ea_hhid_ln_fixed","hhi_team_id"="ind_team_id","ageyears")) %>% 
  mutate(confgend = na_if(confgend, 0),
         gender = as.numeric(coalesce(confgend, sex)),
         age = as.numeric(coalesce(confagey, ageyears))) 

mydf$week_num <- strftime(mydf$hhrets, format = "%V")

i0d = mydf %>%
  group_by(week_num) %>%
  summarize(n1 = round(29136/15)) %>%
  mutate(row_name = "[I0] Target no. of interviews")
  
i1d = mydf %>%
  group_by(week_num) %>%
  summarize(n1 = n_distinct(hhi_uuid[which(gender == 1 | gender == 2)], 
                            ea_hhid_ln_fixed[which(gender == 1 | gender == 2)])) %>% 
  mutate(row_name = "[I1] Total no. rostered individuals",
         across(everything(), as.character))

i2d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 2 & (gender == 1 | gender == 2))], 
                            ea_hhid_ln_fixed[which(eligible1 == 2 & (gender == 1 | gender == 2))])) %>%
  mutate(row_name = "[I2] Ineligible (based on HH roster info)",
         across(everything(), as.character)) 

i3d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & (gender == 1 | gender == 2) & age >= 15)])) %>% 
  mutate(row_name ="[I3] Eligible HH members (age 15+ and slept over the night before) (1)",
         across(everything(), as.character))

i31d = mydf %>%
  group_by(week_num) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0)) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))],
                            ea_hhid_ln_fixed[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))])) %>% 
  mutate(row_name = "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status",
         across(everything(), as.character))
    
i4d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)])) %>%
  mutate(row_name = "     [I4] Consented, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31d$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         (across(everything(), as.character)))

i5d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr == 2 & ind0040 == "1")])) %>%
  mutate(n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31d$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         row_name = "     [I5] Cannot Collect: After 3 attempts, n (% of eligible HH members with a status)",
         across(everything(), as.character))

i55d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")], ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr==2 & ind0040 == "96")])) %>%
  mutate(row_name = "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31d$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i6d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                 strtinstr %in% c(1, 4) & constat == "2"))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr==2 & ind0040 == "2") | 
                                                                         strtinstr %in% c(1, 4) & constat == "2"))]))%>% 
  mutate(row_name = "     [I6] Refused, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31d$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i7d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & ((strtinstr == 2 & ind0040 == "3") |
                                                       (strtinstr %in% c(1, 4) & constat == "0" &
                                                          (is.na(confagey) | confagey < 15)) |
                                                       (strtinstr %in% c(1, 4) & constat == "99")))])) %>% 
  mutate(row_name = "     [I7] Ineligible via screening criteria, n (% of eligible HH members with a status) (2)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31d$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i71d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))],
                              ea_hhid_ln_fixed[which(eligible1 == 1 & (acclang == 2 | ppchelangacc == 2 | chelangacc == 2))])) %>%
  mutate(row_name = "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31d$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i8d = mydf %>%
  group_by(week_num) %>% 
  mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
                           (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
                              (!is.na(confagey) | confagey >= 15)) ~ 1,
                           .default = 0),
         exc_8 = case_when(eligible1 == 1 & strtinstr %in% c(1, 4) & constat== "1" ~ 1, #i4
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "1" ~ 1, # i5
                           (eligible1 == 1 & strtinstr==2 & ind0040 == "2" |
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "2") ~ 1, #i6
                           eligible1 == 1 & strtinstr == 2 & ind0040 == "3" ~ 1,
                           eligible1 == 1 & (strtinstr %in% c(1, 4) & constat == "0" & 
                                               (is.na(confagey) | confagey < 15)) ~ 1,
                           eligible1 == 1 & strtinstr %in% c(1, 4) & constat == "99" ~ 1,
                           .default = 0),
         tbd = case_when(inc_8 == 1 | exc_8 == 0 ~ 1,
                         .default = 0)) %>% 
  summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))], 
                              ea_hhid_ln_fixed[which(eligible1 == 1 & tbd == 1 & (gender == 1 | gender == 2) & (age >= 15))])) %>% 
  mutate(row_name = "     [I8] Approximate TBD, n (% of eligible HH members with a status)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31d$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
         across(everything(), as.character))

i9d = mydf %>%
  group_by(week_num) %>% 
  summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))], 
                            ea_hhid_ln_fixed[which(eligible1 == 1 & rostered_final_status %in% c(2, 3))])) %>% 
  mutate(row_name = "[I9] No IND record",
         across(everything(), as.character))

i10d = tibble(row_name = c("TEST", "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined."), n = c(NA, NA))

i11d = tibble(row_name = "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", n = NA)

i0d = i0d %>%
  mutate(across(everything(), as.character))

itable5 = bind_rows(!!!mget(paste0("i", c(31, 55, 71, seq(from = 0, to = 11)),"d"))) %>% 
  select(week_num, row_name, n1) %>%
  left_join(week_label, by = "week_num") %>%
  relocate(week_label, .before = 1) %>%
  select(-c("week_num")) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  mutate(row_name = fct_relevel(row_name, "[I0] Target no. of interviews", after = 0L),
         row_name = fct_relevel(row_name, "[I1] Total no. rostered individuals", after = 1),
         row_name = fct_relevel(row_name, "[I2] Ineligible (based on HH roster info)", after = 2),
         row_name = fct_relevel(row_name, "[I3] Eligible HH members (age 15+ and slept over the night before) (1)", after = 3),
         row_name = fct_relevel(row_name, "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status", after = 4),
         row_name = fct_relevel(row_name, "     [I4] Consented, n (% of eligible HH members with a status)", after = 5),
         row_name = fct_relevel(row_name, "     [I5] Cannot Collect: After 3 attempts, n (% of eligible HH members with a status)", after = 6),
         row_name = fct_relevel(row_name, "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)", after = 7),
         row_name = fct_relevel(row_name, "     [I6] Refused, n (% of eligible HH members with a status)", after = 8),
         row_name = fct_relevel(row_name, "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)", after = 10),
         row_name = fct_relevel(row_name, "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined.", after = 15),
         row_name = fct_relevel(row_name, "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", after = 16)) %>%
  arrange(row_name) %>%
  select(-("NA")) %>%
  #mutate_all(~ replace(., . == "TEST", NA)) %>%
  rename(Individuals = row_name) %>%
  mutate_all(~ replace(., . == "NULL", 9999999))

itable5 <- itable5 %>% map_df(~unlist(.x)) 

itable5 <- itable5 %>%
  mutate_all(~ replace(., . == 9999999, NA)) %>%
  mutate_all(~ replace(., . == "TEST", NA))
  
itable5

```

```{r 2f, table b, echo=FALSE,warning=FALSE,message=FALSE}

# Biomarker test

mydata = mydata %>%
  mutate(hivstatus_final =
           case_when(
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 1 | biohivtst2rs2 == 1) & (biohivtst3rs == 1 | biohivtst3rs2 == 1) ~ 1), # hiv positive
             (biohivtst1rs == 2 | biohivtst1rs2 == 2 | ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 2 | biohivtst1ars2 == 2))) ~ 2, # hiv negative
             ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs ==1 | biohivtst2rs2 == 1) & (biohivtst3rs == 2 | biohivtst3rs2 == 2) |
                ((biohivtst1rs == 1 | biohivtst1rs2 == 1) & (biohivtst2rs == 2 | biohivtst2rs2 == 2) & (biohivtst1ars == 1 | biohivtst1ars2 == 1))) ~ 3, # indeterminate
             biohivtst1rs2 == 3 | biohivtst2rs2 == 3 | biohivtst1ars2 == 3 | biohivtst3rs2 == 3 ~ 4) # invalid 
           )

mydata$week_num <- strftime(mydata$ind_surveyformsdt, format = "%V")


b0d <- mydata %>%
  group_by(week_num) %>%
  summarize(n1 = round(27621/15)) %>%
  mutate(row_name = "[B0] Target")

b1d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))], ea_hhid_ln_fixed[which(inconbio == 1 | (pprmbio == 1 & asybiogt == 1))])) %>% # change i4 to i4a
  mutate(row_name = "[B1] Consented to blood draw, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4d$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b2d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>%
  mutate(row_name = "[B2] Blood draws achieved, n (% of consented to interview)",
         n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4d$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)"))

b3d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>% 
  mutate(row_name = "[B3] Percent of target blood draws achieved",
         n1 = paste0(format(round(n1_d/as.numeric(b0d$n1)*100, 1), nsmall = 1, trim = TRUE), "%")) %>% 
  select(-contains("_d"))

b4d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1)], ea_hhid_ln_fixed[which(hivstatus_final == 1)])) %>%
  mutate(row_name = "[B4] HIV rapid test: Number (%) of positive",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2d$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)"))

b41d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 1 & ltcvrbl == 1)], 
                              ea_hhid_ln_fixed[which(hivstatus_final == 1 & ltcvrbl == 1)])) %>%
  mutate(row_name = "     [B4.1] Consented to ALTC, n (% of HIV-positive)",
         n1 = paste0(n1_d, " (", format(round(n1_d/b4d$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b5d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 2)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 2)])) %>%
  mutate(row_name = "[B5] HIV rapid test: Number (%) of negative",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2d$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b6d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 3)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 3)]))%>%
  mutate(row_name = "[B6] HIV rapid test: Number (%) of indeterminate",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2d$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b7d = mydata %>%
  group_by(week_num) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(hivstatus_final == 4)],
                              ea_hhid_ln_fixed[which(hivstatus_final == 4)])) %>%
  mutate(row_name = "[B7] HIV rapid test: Number (%) of invalid",
         n1 = paste0(n1_d, " (", format(round(n1_d/b2d$n1_d*100, 1), nsmall = 1, trim = TRUE), "%)")) %>% 
  select(-contains("_d"))

b0d = b0d %>%
  mutate(across(everything(), as.character))

btable5 = bind_rows(!!!mget(paste0("b", c(41, seq(from = 0, to = 7)), "d"))) %>% 
  select(week_num, row_name, n1) %>%
  left_join(week_label, by = "week_num") %>%
  relocate(week_label, .before = 1) %>%
  select(-c("week_num")) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  mutate(row_name = fct_reorder(row_name, row_name),
         row_name = fct_relevel(row_name, "     [B4.1] Consented to ALTC, n (% of HIV-positive)", after = 5),
         row_name = fct_relevel(row_name, "[B0] Target", after = 0L)) %>%
  arrange(row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  select(-("NA")) %>%
  rename(Biomarkers = row_name)

btable5 <- btable5 %>% map_df(~unlist(.x))

btable5

```

```{r 2f, table c, echo=FALSE,warning=FALSE,message=FALSE}

# Interviews

mydata <- mydata %>%
  left_join(week_label, by = "week_num")

crow_func = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(week_label = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(week_label) %>% 
    summarize(median = median(survey_time, na.rm=TRUE),
              median = format(trim = TRUE, as.numeric(median), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(week_label, median) %>% 
    pivot_wider(names_from = week_label, values_from = median) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

crow_func_mean = function(df = mydata, end_var = "adqxedt", row_name){
  c_a = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    # group_by(hhi_eacode) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE)) %>% 
    mutate(week_label = "Total")

  c_b = df %>% 
    filter(endmsg1 == "A") %>% 
    mutate(start_time = indformsdt,
           end_time = eval(as.name(end_var)),
           survey_time = difftime(end_time, start_time, units = "min")) %>% 
    select(start_time, end_time, survey_time, everything()) %>% 
    group_by(week_label) %>% 
    summarize(mean = mean(survey_time, na.rm=TRUE),
              mean = format(trim = TRUE, as.numeric(mean), digits = 3, nsmall = 1, drop0trailing = TRUE))
    
  c_c = bind_rows(c_a, c_b) %>%
    select(week_label, mean) %>% 
    pivot_wider(names_from = week_label, values_from = mean) %>% 
    mutate(row_name = row_name) %>% 
    select(row_name, everything())
  
  return(c_c)
}

c1d = crow_func(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))
c11d = crow_func_mean(row_name = "[C1] Median time to complete entire individual interview (min) (only includes those who completed the interview)") %>%
  select(-("Total"))

c2d = crow_func(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))
c21d = crow_func_mean(end_var = "bio3edt", row_name = "[C2] Median time to complete entire survey from individual questionnaire through biomarker collection (min) (includes those who drop out mid-way after consenting to individual interview consent)") %>%
  select(-("Total"))

c1d <- c1d %>%
  mutate(across(everything(), ~ paste(.x, " (", c11d[[cur_column()]], ")", sep = "")))

c2d <- c2d %>%
  mutate(across(everything(), ~ paste(.x, " (", c21d[[cur_column()]], ")", sep = "")))

c4d = h3d %>% 
  mutate(row_name = recode(row_name, "     [H3] Consented, n (% of eligible HHs)" = "     [C4] Consented, n (% of eligible HHs)")) %>%
  select(-("n")) %>%
  left_join(week_label, by = "week_num") %>%
  relocate("week_label", .before = 1) %>%
  select(-("week_num")) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  select(-("NA"))

c4d <- c4d %>% map_df(~unlist(.x)) 

b2d_a <- b2d %>%
  left_join(week_label, by = "week_num")
i31d_a <- i31d %>%
    left_join(week_label, by = "week_num")

c5d = tibble(week_num = unique(hh_df$week_num)) %>%
  left_join(b2d_a, by = "week_num") %>%
  left_join(i31d_a, by = c("week_num", "week_label"))

c5d = c5d %>%
  mutate(statistic = paste0(n1_d, " (", format(round(n1_d/as.numeric(n1.y)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
  arrange(week_num) %>%
  select(c("week_label","statistic")) %>%
  mutate(row_name = "     [C5] Blood draws achieved") %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  select(-("NA"))

c5d <- c5d %>% map_df(~unlist(.x))

# c3d = c3_func(df1 = c4d, df2 = c5d, row_name = "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)") %>%
#     mutate(across(everything(), ~paste0(., "%")))

extract_percentage <- function(x) {
  as.numeric(str_extract(x, "\\d+\\.\\d+")) / 100
}

percentages_c5d <- map_dbl(c5d, extract_percentage)
percentages_c4d <- map_dbl(c4d, extract_percentage)

percentages_c4d <- data.frame(percentages_c4d) %>%
  filter(!is.na(percentages_c4d))
percentages_c4d <- cbind(rownames(percentages_c4d), data.frame(percentages_c4d, row.names=NULL))
colnames(percentages_c4d) <- c("week_label", "percentages_c4d")
percentages_c4d <- arrange(percentages_c4d, desc(week_label))

percentages_c5d <- data.frame(percentages_c5d) %>%
  filter(!is.na(percentages_c5d))
percentages_c5d <- cbind(rownames(percentages_c5d), data.frame(percentages_c5d, row.names=NULL))
colnames(percentages_c5d) <- c("week_label", "percentages_c5d")
percentages_c5d <- arrange(percentages_c5d, desc(week_label))

c3d <- percentages_c5d %>%
  left_join(percentages_c4d, by = "week_label") %>%
  mutate(percentages_c3d = paste0(format(round(percentages_c5d * percentages_c4d * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  select(c("week_label", "percentages_c3d")) %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  mutate(row_name = "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)")

c7d <- i4d %>%
  mutate(row_name = "     [C7] Consented to individual, n (% of eligible HH members with a status)") %>%
  select(-contains("_d")) %>%
  left_join(week_label, by = "week_num") %>%
  select(-("week_num")) 

c8d <- b2d %>%
  mutate(row_name = "     [C8] Blood draws achieved, n (% of consented to interview)") %>%
  select(-contains("_d")) %>%
  left_join(week_label, by = "week_num") %>%
  select(-("week_num")) %>%
  select(-contains("_d")) 

c6d <- c7d %>%
  left_join(c8d, by = "week_label") %>%
  mutate(percentage_x = sapply(n1.x, extract)) %>%
  mutate(percentage_x = percentage_x / 100) %>%
  mutate(percentage_y = sapply(n1.y, extract)) %>%
  mutate(percentage_y = percentage_y / 100) %>%
  mutate(n1 = paste0(format(round(percentage_x * percentage_y * 100, 1), nsmall = 1, trim = FALSE), "%"),
         row_name = "[C6] Overall individual response rate (% individual consent X % actual blood draws)") %>%
  select(week_label, row_name, n1) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  select(-("NA"))

c6d <- c6d %>% map_df(~unlist(.x))

c7d <- c7d %>%
  pivot_wider(names_from = 3, values_from = 2) %>%
  select(-("NA"))

c7d <- c7d %>% map_df(~unlist(.x))

c8d <- c8d %>%
  pivot_wider(names_from = 3, values_from = 2) %>%
  select(-("NA"))

c8d <- c8d %>% map_df(~unlist(.x))

summary_c <- ctable %>%
  select(c("Statistic","All"))

ctable5 = bind_rows(!!!mget(paste0("c", seq(from = 1, to = 8),"d"))) %>% 
  select(-("NA")) %>%
  #merge(summary_c, by.x = "row_name", by.y = "Statistic") %>%
  mutate(All = summary_c$All) %>% 
  relocate(All, .after = row_name) %>%
  rename(Interviews = row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "NA (NA%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "NA%", "0.0%")) %>%
  mutate_all(~ replace(., is.na(.), "0"))

# Select the first two columns
first_two_columns <- ctable5 %>%
  select(1:2)

# Select columns that match the 'Week' pattern
week_columns <- ctable5 %>%
  select(starts_with("Week")) %>%
  names()

# Extract numbers and create a data frame for sorting
week_order <- data.frame(
  column = week_columns,
  order = as.numeric(str_extract(week_columns, "\\d+"))
)

# Order columns based on the extracted number
ordered_columns <- week_order %>%
  arrange(order) %>%
  pull(column)

# Reorder the original tibble to include the first two columns
ctable5 <- ctable5 %>%
  select(all_of(ordered_columns)) %>%
  bind_cols(first_two_columns) %>%
  relocate("All", .before = 1) %>%
  relocate("Interviews", .before = 1)

ctable5

```

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("EAs"=etable5,
                 "Households"=htable5,
                 "Individuals"=itable5,
                 "Biomarkers tests"=btable5,
                 "Interviews"=ctable5), 
            paste0("./results/", "2f-CODPHIA_Overall Monitoring Report, by Week_", 
                   format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

### 2h Overall Response Rates by Teams / Regions / EAs

```{r 2h, table rr_team, echo=FALSE,warning=FALSE,message=FALSE}

# h3a = hh_df %>% 
#   group_by(hhi_team_id) %>%
#   summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & hhconstat == 1)])) %>%
#   mutate(row_name = "     [H3] Consented, n (% of eligible HHs)",
#          nh = paste0(n, " (", format(round(n/as.numeric(h2a$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))

rr1_t <- h3a %>%
  select("hhi_team_id", "nh") %>%
  rename(HHRR = nh)

# Function to extract percentages
extract_percentages <- function(row) {
  percentages <- str_extract_all(row, "\\d+\\.\\d+")[[1]]  # Extract percentage values
  return(percentages)
}

# Apply function to extract percentages and create a new data frame
rr1_t <- rr1_t %>%
  rowwise() %>%
  mutate(HHRR = list(c(extract_percentages(c_across(HHRR))))) %>%
  select(hhi_team_id, HHRR) %>%
  mutate(HHRR = paste(HHRR, "%"))

rr1_t <- rr1_t %>%
  mutate(HHRR = gsub("\\s+", "", HHRR))

# 
# 
# i4a = mydf %>%
#   group_by(hhi_team_id) %>% 
#   summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
#                               ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)])) %>%
#   mutate(row_name = "[I4] Consented, n (% of eligible HH members with a status)",
#          n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31a$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
#          across(everything(), as.character))

rr2_t <- i4a %>%
  select(hhi_team_id, n1) %>%
  rename(IIRR = n1)

# rr2_t$hhi_team_id <- ifelse(!is.na(rr2_t$hhi_team_id) & rr2_t$hhi_team_id %in% 1:240,
#                          paste0("T", sprintf("%02d", as.numeric(rr2_t$hhi_team_id))),
#                          'TNA')

# Apply function to extract percentages and create a new data frame
rr2_t <- rr2_t %>%
  rowwise() %>%
  mutate(IIRR = list(c(extract_percentages(c_across(IIRR))))) %>%
  select(hhi_team_id, IIRR) %>%
  mutate(IIRR = paste(IIRR, "%"))

rr2_t <- rr2_t %>%
  mutate(IIRR = gsub("\\s+", "", IIRR))


# b2a = mydata %>%
#   group_by(hhi_team_id) %>%
#   summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>%
#   mutate(row_name = "[B2] Blood draws achieved, n (% of consented to interview)",
#          n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4a$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)"))

rr3_t <- b2a %>%
  select(hhi_team_id, n1) %>%
  rename(BDRR = n1)

# Apply function to extract percentages and create a new data frame
rr3_t <- rr3_t %>%
  rowwise() %>%
  mutate(BDRR = list(c(extract_percentages(c_across(BDRR))))) %>%
  select(hhi_team_id, BDRR) %>%
  mutate(BDRR = paste(BDRR, "%"))

rr3_t <- rr3_t %>%
  mutate(BDRR = gsub("\\s+", "", BDRR))


rr_t <- rr1_t %>%
  left_join(rr2_t, by = "hhi_team_id") %>%
  left_join(rr3_t, by = "hhi_team_id")

# Function to extract and multiply percentages
calculate_percentage_product <- function(...) {
  # Extract all values from the input arguments
  values <- unlist(list(...))
  
  # Extract percentages from all values
  percentages <- str_extract_all(values, "\\d+\\.\\d+") 
  
  # Convert extracted percentages to numeric and calculate product
  product <- prod(as.numeric(unlist(percentages)) / 100, na.rm = TRUE) 
  
  return(product)
}

# Apply function to each row and create a new column
rr_t <- rr_t %>%
  rowwise() %>%
  mutate(`Overall RR` = calculate_percentage_product(c_across(HHRR:BDRR))) %>%
  mutate(`Overall RR` = paste0(format(round(`Overall RR` * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  rename(`Team ID` = hhi_team_id) %>%
  mutate_all(~ replace(., is.na(.), "0.0%"))

rr_t <- rr_t %>%
  rowwise() %>%
  mutate(`Overall Individual RR` = calculate_percentage_product(c_across(IIRR:BDRR))) %>%
  mutate(`Overall Individual RR` = paste0(format(round(`Overall Individual RR` * 100, 1), nsmall = 1, trim = TRUE), "%"))


```

```{r 2h, table s, echo=FALSE,warning=FALSE,message=FALSE}

# h3b = hh_df %>% 
#   group_by(province) %>%
#   summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 6)) & hhconstat == 1)])) %>%
#   mutate(row_name = "     [H3] Consented, n (% of eligible HHs)",
#          nh = paste0(n, " (", format(round(n/as.numeric(h2b$n)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
#   mutate(across(everything(), as.character))

rr1_a <- h3b %>%
  select("province", "nh") %>%
  rename(HHRR = nh)

# Function to extract percentages
extract_percentages <- function(row) {
  percentages <- str_extract_all(row, "\\d+\\.\\d+")[[1]]  # Extract percentage values
  return(percentages)
}

# Apply function to extract percentages and create a new data frame
rr1_a <- rr1_a %>%
  rowwise() %>%
  mutate(HHRR = list(c(extract_percentages(c_across(HHRR))))) %>%
  select(province, HHRR) %>%
  mutate(HHRR = paste(HHRR, "%"))

rr1_a <- rr1_a %>%
  mutate(HHRR = gsub("\\s+", "", HHRR))



# i4b = mydf %>%
#   group_by(province) %>% 
#   summarize(n1_d = n_distinct(hhi_uuid[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)], 
#                               ea_hhid_ln_fixed[which(eligible1 == 1 & strtinstr %in% c(1, 4) & constat == 1)])) %>%
#   mutate(row_name = "     [I4] Consented, n (% of eligible HH members with a status)",
#          n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i31b$n1)*100, 1), nsmall = 1, trim = TRUE), "%)"),
#          across(everything(), as.character))

rr2_a <- i4b %>%
  select(province, n1) %>%
  rename(IIRR = n1)

# Apply function to extract percentages and create a new data frame
rr2_a <- rr2_a %>%
  rowwise() %>%
  mutate(IIRR = list(c(extract_percentages(c_across(IIRR))))) %>%
  select(province, IIRR) %>%
  mutate(IIRR = paste(IIRR, "%"))

rr2_a <- rr2_a %>%
  mutate(IIRR = gsub("\\s+", "", IIRR))



# b2b = mydata %>%
#   group_by(province) %>%
#   summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt))])) %>%
#   mutate(row_name = "[B2] Blood draws achieved, n (% of consented to interview)",
#          n1 = paste0(n1_d, " (", format(round(n1_d/as.numeric(i4b$n1_d)*100, 1), nsmall = 1, trim = TRUE), "%)"))

rr3_a <- b2b %>%
  select(province, n1) %>%
  rename(BDRR = n1)

# Apply function to extract percentages and create a new data frame
rr3_a <- rr3_a %>%
  rowwise() %>%
  mutate(BDRR = list(c(extract_percentages(c_across(BDRR))))) %>%
  select(province, BDRR) %>%
  mutate(BDRR = paste(BDRR, "%"))

rr3_a <- rr3_a %>%
  mutate(BDRR = gsub("\\s+", "", BDRR))



rr_a <- rr1_a %>%
  left_join(rr2_a, by = "province") %>%
  left_join(rr3_a, by = "province")

# Function to extract and multiply percentages
calculate_percentage_product <- function(...) {
  # Extract all values from the input arguments
  values <- unlist(list(...))
  
  # Extract percentages from all values
  percentages <- str_extract_all(values, "\\d+\\.\\d+") 
  
  # Convert extracted percentages to numeric and calculate product
  product <- prod(as.numeric(unlist(percentages)) / 100, na.rm = TRUE) 
  
  return(product)
}

# Apply function to each row and create a new column
rr_a <- rr_a %>%
  rowwise() %>%
  mutate(`Overall RR` = calculate_percentage_product(c_across(HHRR:BDRR))) %>%
  mutate(`Overall RR` = paste0(format(round(`Overall RR` * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  rename(`Province` = province) %>%
  mutate_all(~ replace(., . == "18", "Haut-Katanga")) %>%
  mutate_all(~ replace(., . == "20", "Lualaba"))

rr_a <- rr_a %>%
  rowwise() %>%
  mutate(`Overall Individual RR` = calculate_percentage_product(c_across(IIRR:BDRR))) %>%
  mutate(`Overall Individual RR` = paste0(format(round(`Overall Individual RR` * 100, 1), nsmall = 1, trim = TRUE), "%"))

```

```{r 2h, table rr_ea, echo=FALSE,warning=FALSE,message=FALSE}

rr1_ea <- h3c %>%
  select("eacode", "nh") %>%
  rename(HHRR = nh)

# Function to extract percentages
extract_percentages <- function(row) {
  percentages <- str_extract_all(row, "\\d+\\.\\d+")[[1]]  # Extract percentage values
  return(percentages)
}

# Apply function to extract percentages and create a new data frame
rr1_ea <- rr1_ea %>%
  rowwise() %>%
  mutate(HHRR = list(c(extract_percentages(c_across(HHRR))))) %>%
  select(eacode, HHRR) %>%
  mutate(HHRR = paste(HHRR, "%"))

rr1_ea <- rr1_ea %>%
  mutate(HHRR = gsub("\\s+", "", HHRR))

#----

rr2_ea <- i4c %>%
  select(eacode, n1) %>%
  rename(IIRR = n1)

# Apply function to extract percentages and create a new data frame
rr2_ea <- rr2_ea %>%
  rowwise() %>%
  mutate(IIRR = list(c(extract_percentages(c_across(IIRR))))) %>%
  select(eacode, IIRR) %>%
  mutate(IIRR = paste(IIRR, "%"))

rr2_ea <- rr2_ea %>%
  mutate(IIRR = gsub("\\s+", "", IIRR)) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))


#-----

rr3_ea <- b2c %>%
  select(eacode, n1) %>%
  rename(BDRR = n1) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)"))

# Apply function to extract percentages and create a new data frame
rr3_ea <- rr3_ea %>%
  rowwise() %>%
  mutate(BDRR = list(c(extract_percentages(c_across(BDRR))))) %>%
  select(eacode, BDRR) %>%
  mutate(BDRR = paste(BDRR, "%"))

rr3_ea <- rr3_ea %>%
  mutate(BDRR = gsub("\\s+", "", BDRR)) %>%
  filter(!is.na(eacode))


#-----

rr_ea <- rr1_ea %>%
  left_join(rr2_ea, by = "eacode") %>%
  left_join(rr3_ea, by = "eacode") %>%
  mutate_all(~ replace(., is.na(.), "0.0%"))


# Function to extract and multiply percentages
calculate_percentage_product <- function(...) {
  # Extract all values from the input arguments
  values <- unlist(list(...))
  
  # Extract percentages from all values
  percentages <- str_extract_all(values, "\\d+\\.\\d+") 
  
  # Convert extracted percentages to numeric and calculate product
  product <- prod(as.numeric(unlist(percentages)) / 100, na.rm = TRUE) 
  
  return(product)
}

# Apply function to each row and create a new column
rr_ea <- rr_ea %>%
  rowwise() %>%
  mutate(`Overall RR` = calculate_percentage_product(c_across(HHRR:BDRR))) %>%
  mutate(`Overall RR` = paste0(format(round(`Overall RR` * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  rename(`EA Code` = eacode)

rr_ea <- rr_ea %>%
  # left_join(e_df, by = "eacode") %>%
  # filter(ratios >= 0.96) %>%
  select(`EA Code`, HHRR, IIRR, BDRR, `Overall RR`)

rr_ea <- rr_ea %>%
  rowwise() %>%
  mutate(`Overall Individual RR` = calculate_percentage_product(c_across(IIRR:BDRR))) %>%
  mutate(`Overall Individual RR` = paste0(format(round(`Overall Individual RR` * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  mutate_all(~ replace(., . == "%", "0.0%")) %>%
  arrange(`EA Code`)
  

```

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("by Team"=rr_t,
                 "by Province"=rr_a,
                 "by EA" =rr_ea),
            paste0("./results/", "2h-CODPHIA_Response Rates, by Team, Province and EA_", 
                   format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

```{r DRAFT 2h, table s, echo=FALSE,warning=FALSE,message=FALSE}

# ## Overall RR
# 
# # calculate c4e
# 
# h0e <- hh_df %>%
#   left_join(week_label, by = "week_num") %>%
#   group_by(hhi_team_id, week_label) %>%
#   summarize(n = ceiling(4974/230)) %>% # 10 weeks of data collection, 23 teams total
#   mutate(row_name = "[H0] Target") %>%
#   drop_na(week_label)
# 
# target_team <- ceiling(4974/230)
# 
# h1e <- hh_df %>%  
#   left_join(week_label, by = "week_num") %>%
#   group_by(hhi_team_id, week_label) %>% 
#   summarize(n = n_distinct(hhi_shh)) %>%
#   mutate(row_name = "[H1] Total HH Counts, n (% of Target) (1)") %>%
#   drop_na(week_label) %>%
#   mutate(nh = paste0(n, " (", format(round(n/`target_team`*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))
# 
# h2e = hh_df %>% 
#   left_join(week_label, by = "week_num") %>%
#   group_by(hhi_team_id, week_label) %>%
#   summarize(n = n_distinct(hhi_shh[which(hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 5))])) %>%
#   drop_na(week_label) %>%
#   ungroup() %>%
#   mutate(row_name = "[H2] Eligible HHs, n (% of HH forms received) (2)",
#          nh = paste0(n, " (", format(round(n/as.numeric(h1e$n)*100, digits =1), nsmall = 1, trim = TRUE), "%)"))
# 
# c4e = hh_df %>% 
#   left_join(week_label, by = "week_num") %>%
#   group_by(hhi_team_id, week_label) %>%
#   summarize(n = n_distinct(hhi_shh[which((hhconstat %in% c(1,2) | startint==4 | resultndt %in% c(1, 2, 5)) & hhconstat == 1)])) %>%
#   drop_na(week_label) %>%
#   ungroup() %>%
#   mutate(row_name = "[H3] Consented, n (% of eligible HHs)",
#          nh = paste0(n, " (", format(round(n/as.numeric(h2e$n)*100, digits = 1), nsmall = 1, trim = TRUE), "%)"))
# 
# 
# # calculate c5e
# 
# mydf = roster_df %>%
#   left_join(indiv_df, by = c("province","city","eacode","hhi_shh"="ind_shh","hhi_uuid"="ind_hhi_uuid","ea_hhid_fixed","ea_hhid_ln_fixed","hhi_team_id"="ind_team_id","ageyears")) %>% 
#   mutate(confgend = na_if(confgend, 0),
#          gender = as.numeric(coalesce(confgend, sex)),
#          age = as.numeric(coalesce(confagey, ageyears))) 
# 
# mydf$week_num <- strftime(mydf$hhrets, format = "%V")
# 
# mydf$hhi_team_id <- ifelse(!is.na(mydf$hhi_team_id) & mydf$hhi_team_id %in% 1:240,
#                          paste0("T", sprintf("%02d", mydf$hhi_team_id)),
#                          'TNA')
# 
# b2e = mydata %>%
#   group_by(hhi_team_id, week_num) %>%
#   summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")))])) %>%
#   drop_na(week_num)
# 
# i31e = mydf %>%
#   left_join(week_label, by = "week_num") %>%
#   group_by(hhi_team_id, week_num) %>% 
#   mutate(inc_8 = case_when(eligible1 == 1 & is.na(strtinstr) ~ 1,
#                            (eligible1 == 1 & strtinstr %in% c(1, 4) & is.na(constat)) ~ 1,
#                            (eligible1 == 1 & strtinstr %in% c(1,4) & constat == "0" & 
#                               (!is.na(confagey) | confagey >= 15)) ~ 1,
#                            .default = 0)) %>% 
#   summarize(n1 = n_distinct(hhi_uuid[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))],
#                             ea_hhid_ln_fixed[which(eligible1 == 1 & inc_8 == 0 & !(rostered_final_status %in% c(2, 3)))])) %>%
#   drop_na(week_num)
# 
# b2e_a <- b2e %>%
#   left_join(week_label, by = "week_num")
# i31e_a <- i31e %>%
#     left_join(week_label, by = "week_num")
# 
# c5e = b2e_a %>%
#   left_join(i31e_a, by = c("week_num", "week_label", "hhi_team_id"))
# 
# c5e = c5e %>%
#   mutate(statistic = paste0(n1_d, " (", format(round(n1_d/as.numeric(n1)*100, 1), nsmall = 1, trim = TRUE), "%)")) %>%
#   arrange(week_num) %>%
#   select(c("week_label","statistic", "hhi_team_id")) %>%
#   mutate(row_name = "[C5] Blood draws achieved") %>%
#   drop_na(week_label)
# 
# # calculate c3e (% c4e X % c5e)
# 
# c5e_percent <- c5e %>%
#   mutate(percentage_decimal = as.numeric(gsub(".*\\(([^%]+)%\\).*", "\\1", statistic)) / 100)
# 
# c4e_percent <- c4e %>%
#   mutate(percentage_decimal = as.numeric(gsub(".*\\(([^%]+)%\\).*", "\\1", nh)) / 100)
#   
# c3e <- c5e_percent %>%
#   left_join(c4e_percent, by = c("hhi_team_id", "week_label")) %>%
#   mutate(percentages = paste0(format(round(percentage_decimal.x * percentage_decimal.y * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
#   select(c("week_label", "hhi_team_id", "percentages")) %>%
#   pivot_wider(names_from = 1, values_from = 3) %>%
#   mutate(row_name = "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)") %>%
#   select(-("row_name")) %>%
#   rename(`Team ID` = hhi_team_id)
# 
# c3e

```

## 2e WEEKLY RECRUITMENT & RESPONSE RATES (LAST 8 WEEKS)

## Notes: 

```{r 2e, table e, echo=FALSE,warning=FALSE,message=FALSE}

#EAs

curr_week <- strftime(Sys.Date(), format = "%V")

summary_2e <- summary %>%
  mutate(row_name = case_when(Status == "[E1] Started or In Progress With Collection (E4 + E5)" ~ "EAs started",
            Status == "[E2] Completed, n(%)" ~ "EAs completed",
            .default = "NA")) %>%
  select(-("Status")) %>%
  rename(eas = All)

etable7 <- full_join(eas_fuzzy, weeks, by = c("week_num_start" = "week_num")) %>%
  mutate(row_name = "EAs started") %>%
  select(-c("week_start", "week_end")) %>%
  rename(week_num = week_num_start)%>%
  left_join(week_label, by = "week_num") %>%
  rbind(eas_st_end) %>%
  filter(as.numeric(week_num) >= curr_week) %>%
  relocate("week_label", .before = 1) %>%
  drop_na(week_num) %>%
  select(-(week_num)) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  mutate(across(everything(), as.character)) %>%
  merge(summary_2e) %>%
  rename("EAs" = row_name,
         "All" = eas) %>%
  relocate(All, .after = 1) %>%
  select(1:11, `Week 10 (21 Oct - 27 Oct)`, everything())

etable7 <- etable7 %>%
  select(1:2, (ncol(etable7)-7):ncol(etable7))

```

```{r 2e, table h, echo=FALSE,warning=FALSE,message=FALSE}

# Households

htable7 = bind_rows(!!!mget(paste0("h", seq(from = 0, to = 12), "d"))) %>% 
  select(week_num, row_name, nh) %>%
  left_join(week_label, by = "week_num") %>%
  relocate(week_label, .before = 1) %>%
  filter(as.numeric(week_num) > curr_week | is.na(week_num)) %>%
  select(-c("week_num")) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  select(-("NA")) %>%
  mutate_all(~ replace(., . == "TEST", NA)) %>%
  rename(Households = row_name) %>%
  merge(htable, by.x = "Households", by.y = "Household", all = TRUE) %>%
  relocate(Statistic, .after = 1) %>%
  mutate_all(~ replace(., is.na(.), "999999")) %>%
  mutate(Households = fct_relevel(Households, "[H0] Target", after = 0L),
         Households = fct_relevel(Households, "[H1] Total HH Counts, n (% of Target) (1)", after = 1),
         Households = fct_relevel(Households, "[H2] Eligible HHs, n (% of HH forms received) (2)", after = 2),
         Households = fct_relevel(Households, "     [H8] Household absent for extended period of time, n (% of eligible HHs)", after = 6),
         Households = fct_relevel(Households, "[H6] Ineligible - Dwelling not found, n (% of HH forms received)", after = 9),
         Households = fct_relevel(Households, "[H10] Ineligible - Could not collect for other reasons, n (% of HH forms received)", after = 12),
         Households = fct_relevel(Households, "(1) Total HH forms received includes duplicate forms and may exceed 100% of target. Does not include HHs whose status is not yet determined (e.g., unavailable and needing revisit) or forms are awaiting upload.", after = 13),
         Households = fct_relevel(Households, "(2) Total eligible HHs may not equal sum of HH status codes below due to duplicate forms.", after = 14)) %>%
  arrange(Households) %>%
  mutate_all(~ replace(., . == "999999", NA)) %>%
  mutate_all(~ replace(., . == "NULL", ""))

htable7 <- htable7 %>% map_df(~unlist(.x)) 

htable7 <- htable7 %>%
  select(1:2, (ncol(htable7)-7):ncol(htable7))

htable7

```

```{r 2e, table i, echo=FALSE,warning=FALSE,message=FALSE}

# Individuals

itable7 = bind_rows(!!!mget(paste0("i", c(31, 55, 71, seq(from = 0, to = 11)),"d"))) %>% 
  select(week_num, row_name, n1) %>%
  mutate_all(~replace(., . == "[I0] Target", "[I0] Target no. of interviews")) %>%
  left_join(week_label, by = "week_num") %>%
  relocate(week_label, .before = 1) %>%
  filter(as.numeric(week_num) > curr_week | is.na(n1)) %>%
  select(-c("week_num")) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  select(-("NA")) %>%
  mutate_all(~ replace(., . == "TEST", NA)) %>%
  merge(summary_i, by.x = "row_name", by.y = "Statistic") %>%
  rename(Individuals = row_name) %>%
  mutate_all(~ replace(., is.na(.), 999999)) %>%
  mutate(Individuals = fct_relevel(Individuals, "[I0] Target no. of interviews", after = 0L),
         Individuals = fct_relevel(Individuals, "[I1] Total no. rostered individuals", after = 1),
         Individuals = fct_relevel(Individuals, "[I2] Ineligible (based on HH roster info)", after = 2),
         Individuals = fct_relevel(Individuals, "[I3] Eligible HH members (age 15+ and slept over the night before) (1)", after = 3),
         Individuals = fct_relevel(Individuals, "[I3.1] Eligible HH members (age 15+ and slept over the night before) with a status", after = 4),
         Individuals = fct_relevel(Individuals, "     [I4] Consented, n (% of eligible HH members with a status)", after = 5),
         Individuals = fct_relevel(Individuals, "     [I5.5] Cannot Collect: Other, n (% of eligible HH members with a status)", after = 7),
         Individuals = fct_relevel(Individuals, "          [I7.1] Ineligible due to survey language during screening, n (% of eligible HH members with a status)", after = 10),
         Individuals = fct_relevel(Individuals, "(1) Total eligible individuals may not equal sum of individual status codes due to unresolved duplicate records or individuals with status not yet determined.", after = 14),
         Individuals = fct_relevel(Individuals, "(2) Incapacitated; does not speak or understand survey language; does not meet age, hearing, or cognitive capacity requirements.", after = 15)) %>%
  arrange(Individuals) %>%
  relocate(All, .after = 1) %>%
  mutate_all(~ replace(., . == "NULL", NA)) %>%
  mutate_all(~ replace(., . == 999999, NA))

itable7 <- itable7 %>% map_df(~unlist(.x)) 

itable7 <- itable7 %>%
  select(1:2, (ncol(itable7)-7):ncol(itable7))

itable7

```

```{r 2e, table b, echo=FALSE,warning=FALSE,message=FALSE}

# Biomarker test

btable7 = bind_rows(!!!mget(paste0("b", c(41, seq(from = 0, to = 7)), "d"))) %>% 
  mutate_all(~ replace(., . == "[B0] Target", "[B0] Target no. of blood draws")) %>%
  select(week_num, row_name, n1) %>%
  left_join(week_label, by = "week_num") %>%
  relocate(week_label, .before = 1) %>%
  filter(as.numeric(week_num) > curr_week) %>%
  select(-c("week_num")) %>%
  rename(week_num = week_label) %>%
  pivot_wider(names_from = 1, values_from = 3) %>%
  merge(summary_b, by.x = "row_name", by.y = "Statistic") %>%
  mutate(row_name = fct_reorder(row_name, row_name),
         row_name = fct_relevel(row_name, "[B4.1] Consented to ALTC, n (% of HIV-positive)", after = 5),
         row_name = fct_relevel(row_name, "[B0] Target", after = 0L)) %>%
  arrange(row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  select(-("NA")) %>%
  rename(Biomarkers = row_name) %>%
  relocate(`All above 15 years`, .after = 1)

btable7 <- btable7 %>% map_df(~unlist(.x))

btable7 <- btable7 %>%
  select(1:2, (ncol(btable7)-7):ncol(btable7))

btable7

```

```{r 2e, table c, echo=FALSE,warning=FALSE,message=FALSE}

# Interviews

ctable7 = bind_rows(!!!mget(paste0("c", seq(from = 1, to = 8),"d"))) %>% 
  select(-("NA")) %>%
  #merge(summary_c, by.x = "row_name", by.y = "Statistic") %>%
  mutate(All = summary_c$All) %>% 
  relocate(All, .after = row_name) %>%
  rename(Interviews = row_name) %>%
  mutate_all(~ replace(., . == "0 (NaN%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "NA (NA%)", "0 (0.0%)")) %>%
  mutate_all(~ replace(., . == "NA%", "0.0%")) %>%
  mutate_all(~ replace(., is.na(.), "0"))

ctable7 <- ctable7 %>%
  select(1:2, (ncol(ctable7)-7):ncol(ctable7))

ctable7

```

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("EAs"=etable7,
                 "Households"=htable7,
                 "Individuals"=itable7,
                 "Biomarkers tests"=btable7,
                 "Interviews"=ctable7),
            paste0("./results/", "2e-CODPHIA_Overall Monitoring Report, by Week, previous 8 Weeks_",
                   format(Sys.Date(), "%d%b%Y"),".xlsx"))

```

## 4b Enrollment Targets

```{r 4b, table ea, echo=FALSE,warning=FALSE,message=FALSE}

### EAs

# create weeks and targets per week
ea_tibble = tibble(ea_target = seq(9, 135, by = 9), 
                   start = seq(from=lubridate::floor_date(as.POSIXct("2024-08-19", tz="UTC")), 
                               by=as.difftime(7, units = "days"), 
                               to=ceiling_date(as.POSIXct("2024-11-25", tz="UTC"))), 
                   end = seq(from=floor_date(as.POSIXct("2024-08-25", tz="UTC")), 
                             by=as.difftime(7, units = "days"), 
                             to=ceiling_date(as.POSIXct("2024-12-01", tz="UTC"))), 
                   duration = paste0(format(start, "%d-%b"), "-", format(end, "%d-%b"))) %>% 
  mutate(duration = fct_inorder(duration)) %>%
  mutate_all(~ replace(., . == 135, 132))

e_df = hh_df %>% 
  mutate(hh_complete = if_else(!is.na(hhqsts) & (!is.na(hhqets) | !is.na(hhqenddt)), 1, 0)) %>% 
  full_join(sss_df, by = c("province", "city", "eacode" = "ea_id")) %>% 
  group_by(eacode) %>% 
  reframe(yes = n_distinct(hhi_shh[which(hh_complete == 1)]),
          expected = sampled_hh,
          ratios = yes/expected) %>%
  distinct()

# recruitment vs ea targets
new_ea_df = roster_df %>%
  group_by(short_ea) %>% 
  reframe(new_ea = max(hhrets, na.rm=TRUE),
          eacode = eacode_num) %>%
  left_join(e_df, by = "eacode") %>%
  distinct() %>%
  filter(ratios >= 0.96) %>%
  select(new_ea)

# sort eas into weekly periods
ea_df = ea_tibble %>% 
  mutate(interval = interval(start, end),
         ea_df = list(new_ea_df)) %>% 
  rowwise() %>% 
  mutate(count = map(new_ea_df, ~ floor_date(., unit = "day") %within% interval)) %>% 
         # count = map_int(count, sum)) %>% 
  unnest(count) %>% 
  group_by(ea_target, duration) %>% 
  reframe(count = sum(count)) %>% 
  mutate(cum_sum = cumsum(count)) %>%
  mutate(cum_sum = case_when(count == 0 ~ 0,
                             .default = cum_sum)) %>%
  mutate(week_number = 1:n())

# Fit a linear model using only non-zero cumulative sums
model <- lm(cum_sum ~ `week_number`, data = ea_df %>% filter(cum_sum > 0))

# Identify weeks where cum_sum is zero, excluding week 1
future_weeks <- ea_df %>%
  filter(cum_sum == 0 & week_number != 1)

# Create a new data frame for predictions for those weeks
predictions <- future_weeks %>%
  mutate(cum_sum = predict(model, newdata = data.frame(week_number = week_number))) %>%
  select(week_number, duration, cum_sum)

predictions <- ea_df %>%
  left_join(predictions, by = "week_number") %>%
  select(-"duration.y") %>%
  rename(duration = duration.x)

# Extract last data points for labels
last_target <- tail(ea_df, 1)
last_prediction <- tail(predictions, 1)

# Calculate the percentage of the prediction out of the target
percentage <- round((last_prediction$cum_sum.y / last_target$ea_target) * 100, 1)

# Combine the last points into a single data frame for easier labeling
last_points <- data.frame(
  duration = c(last_target$duration, last_prediction$duration),
  y = c(last_target$ea_target, round(last_prediction$cum_sum.y)),  # Round the prediction
  label = c(
    as.character(last_target$ea_target), 
    paste0(round(last_prediction$cum_sum.y), " (", percentage, "%)")
  ),  # Update the label with percentage
  type = c("Cumulative Target", "Cumulative Estimated")
)

ggplot(ea_df, aes(x = duration)) +
  geom_bar(aes(y = count, fill = "Actual Weekly"), stat = "identity", position = position_dodge(width = 0.9)) +
  geom_bar(aes(y = cum_sum, fill = "Actual Cumulative"), stat = "identity", alpha = 0.5, position = position_dodge(width = 0.9)) +
  geom_line(aes(y = ea_target, color = "Cumulative Target", group = 1), linetype = "dashed", size = 1) +
  geom_line(data = predictions, aes(x = duration, y = cum_sum.y, color = "Cumulative Projected", group = 1), linetype = "dashed", size = 1) +
  geom_point(aes(y = ea_target, color = "Cumulative Target"), size = 2) +
  geom_text(aes(y = count, label = count), vjust = -0.5, position = position_dodge(width = 0.9), 
            size = 3, color = "darkgrey") +
  geom_text(aes(y = cum_sum, label = cum_sum), vjust = -0.5, position = position_dodge(width = 0.9), 
            size = 3, color = "darkgrey") +
  # Add labels for last data points
  geom_text(data = last_points, aes(x = duration, y = y, label = label), 
            vjust = -0.5, size = 3, color = "darkgrey", fontface = "bold") +
  labs(title = paste0("CODPHIA Enrollment vs. Targets (", Sys.Date(), ")"),
       subtitle = "Number of completed EAs, cumulative by week",
       x = "Duration",
       y = "Number of Completed EAs") +
  scale_fill_manual(name = "", values = c("Actual Weekly" = "darkgreen", "Actual Cumulative" = "lightgreen")) +
  scale_color_manual(name = "", values = c("Cumulative Target" = "orange", "Cumulative Projected" = "lightgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  # Center the subtitle

ggsave(paste0("./results/", format(Sys.Date(), "%Y%m%d"), "-CODPHIA-recruitment_target-ea_targets.png"), bg = "white")


```

```{r 4b, table hh, echo=FALSE,warning=FALSE,message=FALSE}
### HHs

# create weeks and targets per week
hh_tibble = tibble(hh_target = seq(332, 4980, by = 332), 
                   start = seq(from=lubridate::floor_date(as.POSIXct("2024-08-19", tz="UTC")), 
                               by=as.difftime(7, units = "days"), 
                               to=ceiling_date(as.POSIXct("2024-11-25", tz="UTC"))), 
                   end = seq(from=floor_date(as.POSIXct("2024-08-25", tz="UTC")), 
                             by=as.difftime(7, units = "days"), 
                             to=ceiling_date(as.POSIXct("2024-12-01", tz="UTC"))),  
                   duration = paste0(format(start, "%d-%b"), "-", format(end, "%d-%b"))) %>% 
  mutate(duration = fct_inorder(duration)) %>%
  mutate_all(~ replace(., . == 4980, 4974))

# recruitment vs hh targets
new_hh_df = hh_df %>% 
  filter(!is.na(hhqets)) %>%
  group_by(hhi_shh) %>% 
  reframe(new_hh = min(hhqsts, na.rm=TRUE)) %>% 
  select(new_hh)

# sort hhs into weekly periods
hh_df2 = hh_tibble %>% 
  mutate(interval = interval(start, end),
         hh_df = list(new_hh_df)) %>% 
  rowwise() %>% 
  mutate(count = map(new_hh_df, ~ floor_date(., unit = "day") %within% interval)) %>% 
  unnest(count) %>% 
  group_by(hh_target, duration) %>% 
  reframe(count = sum(count)) %>% 
  mutate(cum_sum = cumsum(count)) %>%
  mutate(cum_sum = case_when(count == 0 ~ 0,
                             .default = cum_sum)) %>%
  mutate(week_number = 1:n())

# Fit a linear model using only non-zero cumulative sums
model <- lm(cum_sum ~ week_number, data = hh_df2 %>% filter(cum_sum > 0))

# Identify weeks where cum_sum is zero, excluding week 1
future_weeks <- hh_df2 %>%
  filter(cum_sum == 0 & week_number != 1)

# Create a new data frame for predictions for those weeks
predictions <- future_weeks %>%
  mutate(cum_sum = predict(model, newdata = data.frame(week_number = week_number))) %>%
  select(week_number, duration, cum_sum)

predictions <- hh_df2 %>%
  left_join(predictions, by = "week_number") %>%
  select(-"duration.y") %>%
  rename(duration = duration.x)

# Extract last data points for labels
last_target <- tail(hh_df2, 1)
last_prediction <- tail(predictions, 1)

# Calculate the percentage of the prediction out of the target
percentage <- round((last_prediction$cum_sum.y / last_target$hh_target) * 100, 1)

# Combine the last points into a single data frame for easier labeling
last_points <- data.frame(
  duration = c(last_target$duration, last_prediction$duration),
  y = c(last_target$hh_target, round(last_prediction$cum_sum.y)),  # Round the prediction
  label = c(
    as.character(last_target$hh_target), 
    paste0(round(last_prediction$cum_sum.y), " (", percentage, "%)")
  ),  # Update the label with percentage
  type = c("Cumulative Target", "Cumulative Estimated")
)

# Graph plot
ggplot(hh_df2, aes(x = duration)) +
  geom_bar(aes(y = count, fill = "Actual Weekly"), stat = "identity", position = position_dodge(width = 0.9)) +
  geom_bar(aes(y = cum_sum, fill = "Actual Cumulative"), stat = "identity", alpha = 0.3, position = position_dodge(width = 0.9)) +
  geom_line(aes(y = hh_target, color = "Cumulative Target", group = 1), linetype = "dashed", size = 1) +
  geom_line(data = predictions, aes(x = duration, y = cum_sum.y, color = "Cumulative Projected", group = 1), linetype = "dashed", size = 1) +
  geom_point(aes(y = hh_target, color = "Cumulative Target"), size = 2) +
  geom_text(aes(y = count, label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 3, color = "darkgrey") +
  geom_text(aes(y = cum_sum, label = cum_sum), vjust = -0.5, position = position_dodge(width = 0.9), size = 3, color = "darkgrey") +
  # Add labels for last data points
  geom_text(data = last_points, aes(x = duration, y = y, label = label), 
            vjust = -0.5, size = 3, color = "darkgrey", fontface = "bold") +
  labs(title = paste0("CODPHIA Enrollment vs. Targets (", Sys.Date(), ")"),
       subtitle = "Number of completed households, cumulative by week",
       x = "Duration",
       y = "Number of Completed Households") +
  scale_fill_manual(name = "", values = c("Actual Weekly" = "darkgreen", "Actual Cumulative" = "lightgreen")) +
  scale_color_manual(name = "", values = c("Cumulative Target" = "orange", "Cumulative Projected" = "lightgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 

ggsave(paste0("./results/", format(Sys.Date(), "%Y%m%d"), "-CODPHIA-recruitment_target-hh_targets.png"), bg = "white")

```

```{r 4b, table blooddraws, echo=FALSE,warning=FALSE,message=FALSE}
### Blooddraws

# create weeks and targets per week
bd_tibble = tibble(bd_target = seq(769, 11535, by = 769), 
                   start = seq(from=lubridate::floor_date(as.POSIXct("2024-08-19", tz="UTC")), 
                               by=as.difftime(7, units = "days"), 
                               to=ceiling_date(as.POSIXct("2024-11-25", tz="UTC"))), 
                   end = seq(from=floor_date(as.POSIXct("2024-08-25", tz="UTC")), 
                             by=as.difftime(7, units = "days"), 
                             to=ceiling_date(as.POSIXct("2024-12-01", tz="UTC"))),  
                   duration = paste0(format(start, "%d-%b"), "-", format(end, "%d-%b"))) %>% 
  mutate(duration = fct_inorder(duration)) %>%
  mutate_all(~ replace(., . == 11535, 11522))

# recruitment vs bd targets
new_bd_df = indiv_df %>% 
  filter(!(coltypead %in% c(NA, "Z")) & !is.na(bio3edt)) %>% 
  group_by(ea_hhid_ln_fixed) %>%
  reframe(new_bd = min(indformsdt, na.rm=TRUE)) %>% 
  select(new_bd)

# sort eas into weekly periods
bd_df = bd_tibble %>% 
  mutate(interval = interval(start, end),
         bd_df = list(new_bd_df)) %>% 
  rowwise() %>% 
  mutate(count = map(new_bd_df, ~ floor_date(., unit = "day") %within% interval)) %>% 
  unnest(count) %>% 
  group_by(bd_target, duration) %>% 
  reframe(count = sum(count)) %>% 
  mutate(cum_sum = cumsum(count)) %>%
  mutate(cum_sum = case_when(count == 0 ~ 0,
                             .default = cum_sum)) %>%
  mutate(week_number = 1:n())

# Fit a linear model using only non-zero cumulative sums
model <- lm(cum_sum ~ week_number, data = bd_df %>% filter(cum_sum > 0))

# Identify weeks where cum_sum is zero, excluding week 1
future_weeks <- bd_df %>%
  filter(cum_sum == 0 & week_number != 1)

# Create a new data frame for predictions for those weeks
predictions <- future_weeks %>%
  mutate(cum_sum = predict(model, newdata = data.frame(week_number = week_number))) %>%
  select(week_number, duration, cum_sum)

predictions <- bd_df %>%
  left_join(predictions, by = "week_number") %>%
  select(-"duration.y") %>%
  rename(duration = duration.x)

# Extract last data points for labels
last_target <- tail(bd_df, 1)
last_prediction <- tail(predictions, 1)

# Calculate the percentage of the prediction out of the target
percentage <- round((last_prediction$cum_sum.y / last_target$bd_target) * 100, 1)

# Combine the last points into a single data frame for easier labeling
last_points <- data.frame(
  duration = c(last_target$duration, last_prediction$duration),
  y = c(last_target$bd_target, round(last_prediction$cum_sum.y)),  # Round the prediction
  label = c(
    as.character(last_target$bd_target), 
    paste0(round(last_prediction$cum_sum.y), " (", percentage, "%)")
  ),  # Update the label with percentage
  type = c("Cumulative Target", "Cumulative Estimated")
)

# Graph plot
ggplot(bd_df, aes(x = duration)) +
  geom_bar(aes(y = count, fill = "Actual Weekly"), stat = "identity", position = position_dodge(width = 0.9)) +
  geom_bar(aes(y = cum_sum, fill = "Actual Cumulative"), stat = "identity", alpha = 0.5, position = position_dodge(width = 0.9)) +
  geom_line(aes(y = bd_target, color = "Cumulative Target", group = 1), linetype = "dashed", size = 1) +
  geom_line(data = predictions, aes(x = duration, y = cum_sum.y, color = "Cumulative Projected", group = 1), linetype = "dashed", size = 1) +
  geom_point(aes(y = bd_target, color = "Cumulative Target"), size = 2) +
  geom_text(aes(y = count, label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 3, color = "darkgrey") +
  geom_text(aes(y = cum_sum, label = cum_sum), vjust = -0.5, position = position_dodge(width = 0.9), size = 3, color = "darkgrey") +
  # Add labels for last data points
  geom_text(data = last_points, aes(x = duration, y = y, label = label), 
            vjust = -0.5, size = 3, color = "darkgrey", fontface = "bold") +
  labs(title = paste0("CODPHIA Enrollment vs. Targets (", Sys.Date(), ")"),
       subtitle = "Number of completed blood draws, cumulative by week",
       x = "Duration",
       y = "Number of Completed Blood Draws") +
  scale_fill_manual(name = "", values = c("Actual Weekly" = "darkgreen", "Actual Cumulative" = "lightgreen")) +
  scale_color_manual(name = "", values = c("Cumulative Target" = "orange", "Cumulative Projected" = "lightgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 

ggsave(paste0("./results/", format(Sys.Date(), "%Y%m%d"), "-CODPHIA-recruitment_target-blooddraws_targets.png"), bg = "white")

```

## 4c Progress Report

```{r 4c, table rr, echo=FALSE,warning=FALSE,message=FALSE}

rr_all1 <- h3d %>%
  left_join(week_label, by = "week_num") %>%
  select(-(c("week_num", "n"))) %>%
  mutate(row_name = case_when(row_name == "     [H3] Consented, n (% of eligible HHs)" ~ "Household interview [H3]",
                              .default = row_name)) %>%
  pivot_wider(names_from = 3, values_from = 2) %>%
  select(-("NA"))
  

rr_all2 <- i4d  %>%
  left_join(week_label, by = "week_num") %>%
  select(-(c("week_num", "n1_d"))) %>%
  mutate(row_name = case_when(row_name == "     [I4] Consented, n (% of eligible HH members with a status)" ~ "Individual interview [I4]",
                              .default = row_name)) %>%
  pivot_wider(names_from = 3, values_from = 2) %>%
  select(-("NA"))


rr_all3 <- b2d  %>%
  left_join(week_label, by = "week_num") %>%
  select(-(c("week_num", "n1_d"))) %>%
  mutate(row_name = case_when(row_name == "[B2] Blood draws achieved, n (% of consented to interview)" ~ "Blood collection achieved (over interviewed) [B2]",
                              .default = row_name)) %>%
  pivot_wider(names_from = 3, values_from = 2) %>%
  select(-("NA"))

rr_all5 <- c3d %>%
  mutate(row_name = case_when(row_name == "[C3] Overall response rate, including HH response rate (% HH consent X % actual blood draws)" ~ "Overall [C3]",
                              .default = row_name))

# rr_all4 <- rr_all1 %>%
#   rbind(rr_all2) %>%
#   rbind(rr_all3)

# List of all tibbles
tibbles <- list(rr_all1, rr_all2, rr_all3)

# Get all unique column names across the tibbles
all_columns <- unique(unlist(map(tibbles, colnames)))

# Function to add missing columns filled with NA
add_missing_columns <- function(tbl, all_columns) {
  missing_columns <- setdiff(all_columns, colnames(tbl))
  tbl <- tbl %>% 
    bind_cols(set_names(as.data.frame(matrix(NA, nrow = nrow(tbl), ncol = length(missing_columns))), missing_columns))
  tbl <- tbl[, all_columns]  # Reorder the columns to match the desired order
  return(tbl)
}

# Add missing columns to each tibble
tibbles <- map(tibbles, add_missing_columns, all_columns = all_columns)

# Now combine the tibbles
rr_all4 <- bind_rows(tibbles)

# Extract percentages while keeping the first column
rr_all4 <- rr_all4 %>%
  mutate(across(-row_name, ~ str_extract(., "\\d+\\.\\d+"))) %>%  # Extract percentages from all columns except Team
  mutate(across(-row_name, ~ paste0(., "%")))

all_columns <- union(colnames(rr_all4), colnames(rr_all5))

# Step 2: Add missing columns to rr_all4 (if any), filled with NA
rr_all4 <- rr_all4 %>%
  bind_cols(set_names(as.data.frame(matrix(NA, nrow = nrow(rr_all4), ncol = length(setdiff(all_columns, colnames(rr_all4))))), 
                      setdiff(all_columns, colnames(rr_all4)))) %>%
  select(all_columns)  # Reorder the columns

# Step 3: Add missing columns to rr_all5 (if any), filled with NA
rr_all5 <- rr_all5 %>%
  # Add missing columns to rr_all5
  bind_cols(set_names(as.data.frame(matrix(NA, nrow = nrow(rr_all5), ncol = length(setdiff(all_columns, colnames(rr_all5))))), 
                      setdiff(all_columns, colnames(rr_all5)))) %>%
  select(all_columns)  # Reorder the columns

# Step 4: Now you can safely rbind(rr_all4) and rr_all5
rr_all4 <- rr_all4 %>%
  rbind(rr_all5) %>%
  rename(Components = row_name) %>%
  mutate_all(~ replace(., . == "NA%", NA))

rr_all4

```

```{r 4c, table bd, echo=FALSE,warning=FALSE,message=FALSE}

## Enrollment totals (number of blood draws achieved)
current_date <- format(Sys.Date(), "%Y-%m-%d")  # Get current date in desired format
column_name1 <- paste("Actual N as of", current_date)  # Construct the column name
column_name2 <- paste("Actual % of target as of", current_date)  

bd1 = mydata %>%
  mutate(`Age group` = case_when(confagey <= 49 ~ "15-49 years",
                                 confagey >= 50 ~ ">= 50 years",
                                 .default = NA)) %>%
  group_by(`Age group`) %>%
  summarize(n1_d = n_distinct(hhi_uuid[which(!(coltypead %in% c(NA, "Z")))], ea_hhid_ln_fixed[which(!(coltypead %in% c(NA, "Z")))])) %>%
  drop_na() %>%
  bind_rows(summarize_all(., ~if(is.numeric(.)) sum(.) else "Total"))

bd2 <- tibble(`Age group` = c("15-49 years", ">= 50 years", "Total"), `Target` = c(10014, 1508, 11522)) %>%
  merge(bd1, by = "Age group") %>%
  mutate(!!column_name2 := paste0(format(round(n1_d / Target * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  rename(!!column_name1 := n1_d) %>%
  mutate(`Age group` = factor(`Age group`, levels = c("15-49 years", ">= 50 years", "Total"))) %>%  # Set the order
  arrange(`Age group`)

bd2
                                 
```

```{r 4c, table ea, echo=FALSE,warning=FALSE,message=FALSE}

## Survey progress

e0=tibble(row_name = "[E0] Target", n = 132)

ea1 = e_df %>% 
  filter(ratios >= 0.96) %>% 
  reframe(n = n_distinct(eacode)) %>% 
  mutate(row_name = "[E2] EAs completed")

ea2 = e_df %>% 
  filter(ratios > 0 & ratios < 0.96) %>% 
  reframe(n = n_distinct(eacode)) %>% 
  mutate(row_name = "[E1] EAs started or in progress")

ea3 = e_df %>% 
  filter(ratios==0 | is.na(yes)) %>% 
  reframe(n = n_distinct(eacode)) %>% 
  mutate(row_name = "[E6] EAs not Started")

ea_table <- ea1 %>%
  rbind(ea2) %>% 
  rbind(ea3) %>%
  mutate("Target" = e0$n) %>%
  mutate(`Percent over total` = paste0(format(round(n / Target * 100, 1), nsmall = 1, trim = TRUE), "%"))

h0 = tibble(row_name = "[H1] Households", n = 4974) %>% 
  mutate(nh = paste0(n)) %>%
  select(`Target` = n, row_name)

h1 = hh_df %>% 
  summarize(n = n_distinct(hhi_shh)) %>%
  select(n)

ea_hh1 <- h0 %>%
  cbind(h1) %>%
  mutate(`Percent over total` = paste0(format(round(n / Target * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  rbind(ea_table) %>% 
  select(row_name, Number = n, `Percent over total`) %>%
  mutate(row_name = factor(row_name, levels = c("[E2] EAs completed", "[E1] EAs started or in progress", "[E6] EAs not Started", "[H1] Households"))) %>%
  arrange(row_name)

# Create dynamic column name
week_label
today <- Sys.Date()
current_week <- week(today)

week_label_curr <- week_label %>%
  filter(week_num == current_week) %>%
  select(week_label) %>%
  pull(week_label)

cleaned_label <- trimws(sub("\\(.*", "", week_label_curr))
cleaned_label_number <- gsub(".*\\D+(\\d+).*", "\\1", cleaned_label)

column_name3 <- paste0(cleaned_label, " of 19 (", format(round(as.numeric(cleaned_label_number)/19 * 100, 1), nsmall = 1, trim = TRUE), "%)")  

ea_hh1 <- ea_hh1 %>%
  rename(!!column_name3 := row_name)

ea_hh1

```

## Export

```{r, echo=FALSE,warning=FALSE,message=FALSE}

## export

rio::export(list("Response Rates"=rr_all4,
                 "Enrollment Totals"=bd2,
                 "Survey Progress"=ea_hh1),
            paste0("./results/", "4c-CODPHIA_Overall Progress Report_",
                   format(Sys.Date(), "%d%b%Y"),".xlsx"))

```
