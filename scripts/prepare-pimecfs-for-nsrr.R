ver="0.1.0"
library(readr)
library(dplyr)

directory_path <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/nsrr-prep/_releases/cleaned_up_datasets"
edf_directory <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/original-edf"

# Get list of all CSV files in the directory
csv_files <- list.files(path = directory_path,
                        pattern = "\\.csv$",
                        full.names = TRUE)

# Function to read CSV files
read_csv_file <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  return(df)
}

data_list <- lapply(csv_files, read_csv_file)
merged_data <- data_list[[1]]

# Merge with each subsequent dataset
if(length(data_list) > 1) {
  for(i in 2:length(data_list)) {
    merged_data <- full_join(merged_data, data_list[[i]], by = "participantid")
  }
}

id_link_file <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/original-covariates/NSRR_ID_Link .csv"
id_link_data <- read_csv(id_link_file, show_col_types = FALSE)

# Get list of EDF files
edf_files <- list.files(path = edf_directory,
                        pattern = "\\.edf$",
                        full.names = FALSE)

# Extract filenames
edf_basenames <- tools::file_path_sans_ext(edf_files)

match_edf_files <- function(de_id_values, edf_basenames) {
  matches <- character(length(de_id_values))
  
  for(i in seq_along(de_id_values)) {
    de_id <- as.character(de_id_values[i])
    
    if(is.na(de_id) || de_id == "" || de_id == "NA") {
      matches[i] <- NA_character_
      next
    }
    
    direct_match <- which(edf_basenames == de_id)
    if(length(direct_match) > 0) {
      matches[i] <- edf_files[direct_match[1]]
      next
    }
    
    contains_match <- which(grepl(de_id, edf_basenames, fixed = TRUE))
    if(length(contains_match) > 0) {
      matches[i] <- edf_files[contains_match[1]]
      next
    }
    
    starts_with_match <- which(startsWith(edf_basenames, de_id))
    if(length(starts_with_match) > 0) {
      matches[i] <- edf_files[starts_with_match[1]]
      next
    }
    
    # If no match found
    matches[i] <- NA_character_
  }
  
  return(matches)
}

edf_matches <- match_edf_files(id_link_data$`De-ID #`, edf_basenames)
id_link_data$edf_filename <- edf_matches

# check mismatches
matched_edf_files <- id_link_data$edf_filename[!is.na(id_link_data$edf_filename)]
unmatched_edf_files <- setdiff(edf_files, matched_edf_files)

if(length(unmatched_edf_files) > 0) {
  unmatched_edf_df <- data.frame(
    EDF_Filename = unmatched_edf_files,
    EDF_Basename = tools::file_path_sans_ext(unmatched_edf_files),
    Issue = "EDF file has no matching De-ID",
    stringsAsFactors = FALSE
  )
} else {
  unmatched_edf_df <- data.frame(
    EDF_Filename = character(0),
    EDF_Basename = character(0),
    Issue = character(0),
    stringsAsFactors = FALSE
  )
}

unmatched_deid_df <- id_link_data[is.na(id_link_data$edf_filename),
                                  c("De-ID #", "MapID", "MECFS ID")] %>%
  mutate(Issue = "De-ID has no matching EDF file")

mapids_in_merged <- unique(merged_data$participantid)
deid_without_mapid <- id_link_data[!id_link_data$MapID %in% mapids_in_merged,
                                   c("De-ID #", "MapID", "MECFS ID")] %>%
  mutate(Issue = "De-ID has no matching participantid in data")

issues_df <- bind_rows(
  # EDF issues
  unmatched_edf_df %>% 
    select(Issue, EDF_Filename, EDF_Basename) %>%
    rename(Filename_or_DeID = EDF_Filename,
           Basename_or_MapID = EDF_Basename) %>%
    mutate(MECFS_ID = NA_character_),
  
  # De-ID issues
  unmatched_deid_df %>%
    select(Issue, `De-ID #`, MapID, `MECFS ID`) %>%
    rename(Filename_or_DeID = `De-ID #`,
           Basename_or_MapID = MapID,
           MECFS_ID = `MECFS ID`),
  
  # MapID issues
  deid_without_mapid %>%
    select(Issue, `De-ID #`, MapID, `MECFS ID`) %>%
    rename(Filename_or_DeID = `De-ID #`,
           Basename_or_MapID = MapID,
           MECFS_ID = `MECFS ID`)
)

merged_data <- merged_data %>%
  left_join(id_link_data %>% select(MapID, `MECFS ID`, `De-ID #`),
            by = c("participantid" = "MapID")) %>%
  rename(mecfsid = `MECFS ID`) %>%
  mutate(edf_filename = ifelse(!is.na(`De-ID #`),
                               paste0(`De-ID #`, "_deidentified.edf"),
                               NA_character_)) %>%
  select(-`De-ID #`)

merged_data <- merged_data %>%
  mutate(
    snore = ifelse(snore == ".", NA, snore),
    trouble_sleep_other_spfy = ifelse(trouble_sleep_other_spfy == ".", NA, trouble_sleep_other_spfy)
  )

# Set edf_filename to NA for rows where De-ID # exists in all 3 columns but shows NA in edf_filename
mapids_to_clear <- id_link_data %>%
  filter(!is.na(`De-ID #`) & 
           !is.na(MapID) & 
           !is.na(`MECFS ID`) & 
           is.na(edf_filename)) %>%
  pull(MapID)

# Set edf_filename to NA for these participants
merged_data <- merged_data %>%
  mutate(edf_filename = ifelse(participantid %in% mapids_to_clear, 
                               NA_character_, 
                               edf_filename))

# Add visit column as 1 and rearrange columns
merged_data$visit <- 1
merged_data <- merged_data %>%
  select(participantid, mecfsid, edf_filename, visit, everything()) %>%
  arrange(participantid)

output_file <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/nsrr-prep/_releases/0.1.0.pre/dataset/pimecfs-dataset-0.1.0.pre.csv"
write_csv(merged_data, output_file, na = '')




#harmonized dataset
harmonized_data<-merged_data[,c("participantid", "edf_filename","visit","age_at_evaluation",
                                "birth_sex","race","ethnic_group","bmi","smoking")]%>%
  dplyr::mutate(nsrrid=participantid,
                nsrr_age=age_at_evaluation,
                nsrr_race=dplyr::case_when(
                  race==1 ~ "asian",
                  race==0 ~ "white",
                  race==2 ~ "multiple",
                  TRUE ~ "not reported"
                ),
                nsrr_sex=dplyr::case_when(
                  birth_sex==0 ~ "male",
                  birth_sex==1 ~ "female",
                  TRUE ~ "not reported"
                ),
                nsrr_ethnicity=dplyr::case_when(
                  ethnic_group==1 ~ "hispanic or latino",
                  ethnic_group==0 ~ "not hispanic or latino",
                  TRUE ~ "not reported"
                ),
                nsrr_bmi = bmi,
                nsrr_ever_smoker = dplyr::case_when(
                  smoking==1 ~ "yes",
                  smoking==2 ~ "yes",
                  smoking==0 ~ "no",
                  TRUE ~ "not reported"
                ),
                nsrr_current_smoker = dplyr::case_when(
                  smoking==1 ~ "no",
                  smoking==2 ~ "yes",
                  smoking==0 ~ "no",
                  TRUE ~ "not reported"
                )) %>% select(nsrrid, edf_filename, visit, nsrr_age, nsrr_race, nsrr_sex, nsrr_ethnicity, nsrr_bmi, nsrr_ever_smoker,nsrr_current_smoker)%>%
  arrange(nsrrid)

write.csv(harmonized_data,file = "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/nsrr-prep/_releases/0.1.0.pre/dataset/pimecfs-harmonized-dataset-0.1.0.pre.csv", row.names = FALSE, na='')




