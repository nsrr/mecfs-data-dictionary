ver="0.1.0"
# Load required libraries
library(readr)
library(dplyr)

# Set the directory path
directory_path <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/nsrr-prep/_releases/cleaned_up_datasets"

# Get list of all CSV files in the directory
csv_files <- list.files(path = directory_path, 
                        pattern = "\\.csv$", 
                        full.names = TRUE)

# Function to read CSV files
read_csv_file <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Check if participantid column exists (case insensitive)
  id_col <- names(df)[tolower(names(df)) == "participantid"]
  
  if(length(id_col) == 0) {
    return(NULL)
  }
  
  # Rename to standard case if needed
  if(id_col != "participantid") {
    names(df)[names(df) == id_col] <- "participantid"
  }
  
  return(df)
}

# Read all CSV files
data_list <- lapply(csv_files, read_csv_file)

# Remove NULL entries (files without participantid)
data_list <- data_list[!sapply(data_list, is.null)]

# Start with the first dataset
merged_data <- data_list[[1]]

# Merge with each subsequent dataset
if(length(data_list) > 1) {
  for(i in 2:length(data_list)) {
    merged_data <- full_join(merged_data, data_list[[i]], by = "participantid")
  }
}

# Read the ID link file to get MECFS ID mapping
id_link_file <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/original-covariates/NSRR_ID_Link .csv"
id_link_data <- read_csv(id_link_file, show_col_types = FALSE)

# Match and add mecfsid column
# MapID in id_link_data corresponds to participantid in merged_data
merged_data <- merged_data %>%
  left_join(id_link_data %>% select(MapID, `MECFS ID`), 
            by = c("participantid" = "MapID")) %>%
  rename(mecfsid = `MECFS ID`)

merged_data <- merged_data %>%
  mutate(
    snore = ifelse(snore == ".", NA, snore),
    trouble_sleep_other_spfy = ifelse(trouble_sleep_other_spfy == ".", NA, trouble_sleep_other_spfy)
  )

# Add visit column as 1 and rearrange columns
merged_data$visit <- 1
merged_data <- merged_data %>%
  select(participantid, mecfsid, visit, everything())

# Save merged dataset
output_file <- "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/nsrr-prep/_releases/0.1.0.pre/dataset/mecfs-dataset-0.1.0.pre.csv"
write_csv(merged_data, output_file, na = '')




#harmonized dataset
harmonized_data<-merged_data[,c("participantid", "visit","age_at_evaluation",
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
                  smoking==0 ~ "no",
                  TRUE ~ "not reported"
                )) %>% select(nsrrid, visit, nsrr_age, nsrr_race, nsrr_sex, nsrr_ethnicity, nsrr_bmi, nsrr_ever_smoker)

write.csv(harmonized_data,file = "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20250114-walitt-mecfs/nsrr-prep/_releases/0.1.0.pre/dataset/mecfs-harmonized-dataset-0.1.0.csv", row.names = FALSE, na='')