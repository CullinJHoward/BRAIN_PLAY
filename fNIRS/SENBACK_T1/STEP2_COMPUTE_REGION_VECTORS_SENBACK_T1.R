## LIBRARY 

library(dplyr)
library(MplusAutomation)
library(ggplot2)
library(signal)

## SET WORKING DIRECTORY 

POUNDTOWN <- 0

# set working directory
if (POUNDTOWN == 1) {
  work_dir <- 'D:\\FNIRS\\'
} else {
  work_dir <- 'F:\\FNIRS\\'
}

setwd(work_dir)


################################################################################
######################### LOAD CLEAN fNIRS CHANNEL DATA 

# Define the folder path
input_folder <- "FINAL_CLEAN_TASK_DATA/SENBACK_T1/ALL_CHANNELS/"  # Use forward slashes for R

# Get the list of CSV files in the folder
csv_files <- list.files(path = input_folder, pattern = "_SENBACK_T1.csv$", full.names = TRUE)

# Loop through each CSV file and load it into the environment
for (csv_file in csv_files) {
  # Extract the base filename (without path)
  base_name <- basename(csv_file)
  
  # Extract the 4-digit ID from the filename (assuming it's present)
  id <- regmatches(base_name, regexpr("\\d{4}", base_name))[[1]]
  
  # Create the data frame name as SB_[ID]
  df_name <- paste0("SB_", id)
  
  # Read the CSV into a data frame
  df <- read.csv(csv_file)
  
  # Assign the data frame to a variable in the environment with the new name
  assign(df_name, df, envir = .GlobalEnv)
  
  # Print a message confirming the loading of the file
  message(paste("Loaded:", df_name))
}

################################################################################
###################### CREATE REGIONS BY REDUCING CHANNELS 

# DEFINE THE REGIONS BY THEIR CHANNELS
regions <- list(
  L_MFG_OX = c("S3_D3.hbo", "S4_D3.hbo", "S4_D5.hbo", "S6_D5.hbo", "S2_D3.hbo"),
  R_MFG_OX = c("S12_D11.hbo", "S10_D11.hbo", "S9_D11.hbo", "S12_D12.hbo", "S14_D12.hbo"),
  L_MFG_DE = c("S3_D3.hbr", "S4_D3.hbr", "S4_D5.hbr", "S6_D5.hbr", "S2_D3.hbr"),
  R_MFG_DE = c("S12_D11.hbr", "S10_D11.hbr", "S9_D11.hbr", "S12_D12.hbr", "S14_D12.hbr"),
  L_aSFG_OX = c("S1_D1.hbo", "S1_D9.hbo", "S2_D9.hbo", "S2_D1.hbo", "S11_D9.hbo"),
  R_aSFG_OX = c("S1_D8.hbo", "S9_D8.hbo", "S9_D9.hbo", "S1_D9.hbo", "S11_D9.hbo"),
  L_aSFG_DE = c("S1_D1.hbr", "S1_D9.hbr", "S2_D9.hbr", "S2_D1.hbr", "S11_D9.hbr"),
  R_aSFG_DE = c("S1_D8.hbr", "S9_D8.hbr", "S9_D9.hbr", "S1_D9.hbr", "S11_D9.hbr"),
  L_pSFG_OX = c("S11_D2.hbo", "S11_D4.hbo", "S6_D4.hbo", "S6_D2.hbo", "S11_D9.hbo"),
  R_pSFG_OX = c("S11_D10.hbo", "S14_D10.hbo", "S14_D4.hbo", "S11_D4.hbo", "S11_D9.hbo"),
  L_pSFG_DE = c("S11_D2.hbr", "S11_D4.hbr", "S6_D4.hbr", "S6_D2.hbr", "S11_D9.hbr"),
  R_pSFG_DE = c("S11_D10.hbr", "S14_D10.hbr", "S14_D4.hbr", "S11_D4.hbr", "S11_D9.hbr"),
  L_TPJ_OX = c("S7_D6.hbo", "S8_D6.hbo", "S7_D7.hbo", "S8_D7.hbo"),
  R_TPJ_OX = c("S15_D14.hbo", "S15_D15.hbo", "S16_D15.hbo", "S16_D14.hbo"),
  L_TPJ_DE = c("S7_D6.hbr", "S8_D6.hbr", "S7_D7.hbr", "S8_D7.hbr"),
  R_TPJ_DE = c("S15_D14.hbr", "S15_D15.hbr", "S16_D15.hbr", "S16_D14.hbr")
)


################################################################################
###################### ADD IN ALL POSSIBLE CHANNELS (NA FOR THOSE THAT MISSING)

# Define the full list of possible columns
all_columns <- c(
  "TIME", "GAME", "VALENCE", "S1_D1.hbo", "S1_D8.hbo", "S1_D9.hbo", "S2_D1.hbo", "S2_D3.hbo", "S2_D9.hbo",
  "S3_D1.hbo", "S3_D3.hbo", "S4_D2.hbo", "S4_D3.hbo", "S4_D5.hbo", "S4_D16.hbo", "S5_D3.hbo", "S6_D2.hbo", "S6_D4.hbo",
  "S6_D5.hbo", "S6_D17.hbo", "S7_D6.hbo", "S7_D7.hbo", "S7_D18.hbo", "S8_D6.hbo", "S8_D7.hbo", "S8_D19.hbo", "S9_D8.hbo",
  "S9_D9.hbo", "S9_D11.hbo", "S10_D8.hbo", "S10_D10.hbo", "S11_D2.hbo", "S11_D4.hbo", "S11_D9.hbo", "S11_D10.hbo", "S12_D10.hbo",
  "S12_D11.hbo", "S12_D12.hbo", "S12_D20.hbo", "S13_D11.hbo", "S14_D4.hbo", "S14_D10.hbo", "S14_D12.hbo", "S14_D21.hbo", "S15_D13.hbo",
  "S15_D14.hbo", "S15_D15.hbo", "S15_D22.hbo", "S16_D14.hbo", "S16_D15.hbo", "S16_D23.hbo", "S1_D1.hbr", "S1_D8.hbr", "S1_D9.hbr",
  "S2_D1.hbr", "S2_D3.hbr", "S2_D9.hbr", "S3_D1.hbr", "S3_D3.hbr", "S4_D2.hbr", "S4_D3.hbr", "S4_D5.hbr", "S4_D16.hbr",
  "S5_D3.hbr", "S6_D2.hbr", "S6_D4.hbr", "S6_D5.hbr", "S6_D17.hbr", "S7_D6.hbr", "S7_D7.hbr", "S7_D18.hbr", "S8_D6.hbr",
  "S8_D7.hbr", "S8_D19.hbr", "S9_D8.hbr", "S9_D9.hbr", "S9_D11.hbr", "S10_D8.hbr", "S10_D11.hbr", "S11_D2.hbr", "S11_D4.hbr",
  "S11_D9.hbr", "S11_D10.hbr", "S12_D10.hbr", "S12_D11.hbr", "S12_D12.hbr", "S12_D20.hbr", "S13_D11.hbr", "S14_D4.hbr", "S14_D10.hbr",
  "S14_D12.hbr", "S14_D21.hbr", "S15_D13.hbr", "S15_D14.hbr", "S15_D15.hbr", "S15_D22.hbr", "S16_D14.hbr", "S16_D15.hbr", "S16_D23.hbr",
  "S10_D11.hbo", "S10_D11.hbr")


# Loop through all data frames with the prefix "SB_" followed by 4 digits
for (df_name in ls(pattern = "^SB_\\d{4}$")) {
  # Get the data frame by its name
  df <- get(df_name)
  
  # Identify missing columns by comparing current columns to all_columns
  missing_cols <- setdiff(all_columns, colnames(df))
  
  # Add missing columns filled with NA
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  # Reorder columns to match all_columns
  df <- df[, all_columns, drop = FALSE]
  
  # Update the data frame in the global environment
  assign(df_name, df, envir = .GlobalEnv)
  
  # Print a message confirming the update
  message(paste("Updated:", df_name, "with", length(missing_cols), "new columns"))
}

################################################################################
###################### COMPUTE AVERAGED REGION TIME SERIES 

regular_mean <- function(df, channel_names) {
  # Subset the relevant channels
  channel_data <- df[, channel_names, drop = FALSE]
  
  # Remove columns that are all NA
  channel_data <- channel_data[, colSums(!is.na(channel_data)) > 0, drop = FALSE]
  
  # If all selected channels are NA, return a vector of NAs
  if (ncol(channel_data) == 0) {
    return(rep(NA, nrow(df)))
  }
  
  # Compute regular mean time series (ignoring NA values)
  mean_series <- rowMeans(channel_data, na.rm = TRUE)
  
  return(mean_series)
}

#### APPLY THE FUNCTION 

# MAKE AN OBJECT THAT LISTS ALL THE fNIRS DATA FRAME 
df_names <- ls(pattern = "^SB_\\d{4}$")

# LOOP THROUGH AND APPLY THE REGULAR MEANS FUNCTION
for (df_name in df_names) {
  df <- get(df_name)  
  
  # COMPUTE EACH REGIONS REGULAR MEAN 
  for (region in names(regions)) {
    df[[region]] <- regular_mean(df, regions[[region]])
  }
  
  assign(df_name, df)  
}

#### APPLY THE FUNCTION 

# MAKE AN OBJECT THAT LISTS ALL THE fNIRS DATA FRAME 
df_names <- ls(pattern = "^SB_\\d{4}$")

# LOOP THROUGH AND APPLY THE WEIGHTED MEANS FUNCTION
for (df_name in df_names) {
  df <- get(df_name)  
  
  # COMPUTE EACH REGIONS WEIGHTED AVERAGE 
  for (region in names(regions)) {
    df[[region]] <- regular_mean(df, regions[[region]])
  }
  
  assign(df_name, df)  
}


################################################################################
######################## SAVE DATA

### DATA SAVED AT THIS POINT IS:
# CLEANED (IN SATORI)
# IMPROBABLE VALES EXCEEDING -100/+100 MOVED TO NA 
# WITHIN COLUMN MISSINGNESS HANDLED USING CUBIC SPLINE INTERPOLATION 
# SMOOTHED USING THE Savitzky-Golay filter
# REGIONS IDENTIFIED AND REACHED COMPUTED REGULAR MEANS


# Define the output folder path
output_folder <- "FINAL_CLEAN_TASK_DATA\\SENBACK_T1\\CHANNELS_W_REGIONS"

# CREATE THE FOLDER IF NOT THERE
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# ID FILES TO SAVE 
df_list <- ls(pattern = "^SB_\\d{4}$")

# Check if any data frames were found
if (length(df_list) == 0) {
  warning("No data frames found with pattern 'SM_SB_XXXX'")
} else {
  # Loop through all data frames with the prefix "SB_" followed by 4 digits
  for (df_name in df_list) {
    # Get the data frame by its name
    df <- get(df_name)
    
    # Extract the 4-digit ID from the data frame name
    id <- sub("^SB_(\\d{4})$", "\\1", df_name)
    
    # Define the file path for saving the CSV
    file_path <- file.path(output_folder, paste0("DORRY3_FNIRS_CHANNELS_REGIONS_", id, "_SENBACK_T1.csv"))
    
    # Save the data frame as a CSV
    write.csv(df, file_path, row.names = FALSE)
    
    # Print the name of the saved file
    message(paste("Saved:", file_path))
  }
}

### ALSO SAVE THE REGIONS OBJECT TO SAVE A HEADACHE ###

### MAKE IT A DF SO WE CAN SAVE IT 
regions_df <- stack(regions)
colnames(regions_df) <- c("CHANNELS", "REGION")

#REORDER VARIABLES TO MAKE SENSSE 
REGION_DF <- regions_df %>%
  select(c("REGION", "CHANNELS" ))

write.csv(REGION_DF, "F:\\FNIRS\\ADMIN_FILES\\DORRY3_fNIRS_REGIONS_BY_CHANNEL.csv", row.names=FALSE, na="")

