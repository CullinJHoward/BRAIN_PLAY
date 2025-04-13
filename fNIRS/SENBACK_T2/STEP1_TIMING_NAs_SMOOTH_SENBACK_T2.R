## LIBRARY 

library(dplyr)
library(MplusAutomation)
library(ggplot2)
library(signal)
library(stringr)

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
######################### LOAD EACH PARTICIPANTS DATA 

# SET PATH TO CSV FOLDER 

SENBACK_T1_CSVS <- file.path("PRE_PROCESSED_UNMASKED_EXTRACTED\\SENBACK_T2\\")

# MAKE A LIST OF CSV'S IN THE FOLDER 
csv_files <- list.files(path = SENBACK_T1_CSVS, pattern = "[.]csv$", full.names = TRUE, ignore.case = TRUE)

# EXTRACT ID NUMBER
for (file in csv_files) {
  id <- str_extract(basename(file), "\\d{4}") 
  # Create the data frame name (e.g., SB_1234)
  df_name <- paste0("SB_", id)
  # Load the CSV into the environment with that name
  assign(df_name, read.csv(file), envir = .GlobalEnv)
}

################################################################################
######################### ADD TIME AND SEN_BACK TIME STAMPS 

###### Fix x to be TIME 

sb_dataframes <- ls(pattern = "^SB_", envir = .GlobalEnv)

for (df_name in sb_dataframes) {
  if (is.data.frame(get(df_name, envir = .GlobalEnv))) {
    df <- get(df_name, envir = .GlobalEnv)
    if ("X" %in% colnames(df)) {
      df <- rename(df, TIME = X)
      assign(df_name, df, envir = .GlobalEnv)
      cat("Renamed 'X' to 'TIME' in", df_name, "\n")
    } else {
      cat("No 'X' column found in", df_name, "\n")
    }
  }
}


###### Add SENBACK TIMING STAMPS 

# LOAD IN THE TIMING FILE 

SENBACK_TIME <- read.csv("R_TASK_CLEANING_SCRIPTS\\SENBACK_T2_STIM_TIME.csv")


# LOOP THROUGH AND ADD SENBACK TIMING VARIABLES 

# LOOP THROUGH SB_ DF
for (df_name in sb_dataframes) {
  if (is.data.frame(get(df_name, envir = .GlobalEnv))) {
    # Get the current SB_ data frame
    df <- get(df_name, envir = .GlobalEnv)
    
    # INITIALIZE NEW GAME and VALENCE COLUMNS WITH "OFF_TASK" AS DEFAULT
    df$GAME <- "OFF_TASK"
    df$VALENCE <- "OFF_TASK"
    
    # LOOP THROUGH DFS
    for (i in 1:nrow(df)) {
      current_time <- df$TIME[i]
      
      # MATCH TO TIMING FILE
      for (j in 1:nrow(SENBACK_TIME)) {
        if (current_time >= SENBACK_TIME$START[j] && current_time <= SENBACK_TIME$END[j]) {
          df$GAME[i] <- SENBACK_TIME$GAME[j]
          df$VALENCE[i] <- SENBACK_TIME$VALENCE[j]
          break  # Exit inner loop once a match is found
        }
      }
    }
    
    # REORDER COLUMNS
    df <- df %>% 
      select(TIME, GAME, VALENCE, everything())
    
    # REASSIGN THE DF BACK TO THE ENVIRONMENT
    assign(df_name, df, envir = .GlobalEnv)
    cat("Updated", df_name, "with GAME and VALENCE in positions 2 and 3\n")
  }
}


################################################################################
######################## VISUALIZE SOME DATA 


# Create the plot
ggplot(SB_1011, aes(x = TIME, y = `S10_D11.hbr`)) + 
  geom_line() +  # Line plot to visualize the change over time
  labs(x = "Time", y = "`S3_D3.hbr`", title = "Time vs `S3_D3.hbr`") +
  theme_minimal()


################################################################################
######################## HANDLE EXTREME VALUES  

## I AM GOING TO CAP ALL VALUES AT 100 OR -100 (DEPENDING ON THEIR VALANCE)
## SINCE WE ARE IN % OF CHANGE, NO VALUE SHOULD EXCEED +/- 100
## THIS TURNS THEM TO NAs SO THEY CAN BE HANDLED THROUGH INTERPOLATION LATER. 

# LOOP THROUGH ALL "SB_" DFS
for (df_name in ls(pattern = "^SB_", envir = .GlobalEnv)) {

  df <- get(df_name, envir = .GlobalEnv)
  
  # ENSURE A MINIMUM OF 4 COLUMNS (TIME, GAME, VALENCE, plus at least 1 more)
  if (ncol(df) >= 4) {
    # FIX TO NA ALL COLUMNS AFTER THE 3RD (index 4 to the end) EXCEEDING +/- 100
    df[, 4:ncol(df)] <- apply(df[, 4:ncol(df), drop = FALSE], 2, function(x) {
      x[x < -100] <- NA #-100
      x[x > 100] <- NA #100
      return(x)
    })
    
    # UPDATE THE DF 
    assign(df_name, df, envir = .GlobalEnv)
    cat("Capped values in", df_name, "for columns 4 to", ncol(df), "\n")
  } else {
    cat("Skipped", df_name, "- fewer than 4 columns\n")
  }
}

## CHECK HOW MANY NA's OCCURED - THIS REPORTS ANY COLUMN WITH MORE THAN 5% NAs

# Get list of data frames with prefix SB_
df_list <- ls(pattern = "^SB_")

# Initialize an empty list to store results
summary_list <- list()

# Loop through each data frame
for (df_name in df_list) {
  # Get the data frame
  df <- get(df_name)
  
  # Calculate percentage NAs per column
  na_percent <- colSums(is.na(df)) / nrow(df) * 100
  
  # Identify columns with >5% NAs
  high_na_cols <- names(df)[na_percent > 5]
  
  # Store in summary list if there are any high NA columns
  if (length(high_na_cols) > 0) {
    summary_list[[df_name]] <- high_na_cols
  }
}

# Print the summary
if (length(summary_list) == 0) {
  cat("No data frames have columns with more than 5% NAs.\n")
} else {
  for (df_name in names(summary_list)) {
    cat("Data Frame:", df_name, "\n")
    cat("Columns with >5% NAs:", paste(summary_list[[df_name]], collapse = ", "), "\n\n")
  }
}

################################################################################
######################## HANDLE WITHIN-COLUMN NAs WITH DATA INTERPOLATION

# Loop through all data frames with the prefix "SB_"
for (df_name in ls(pattern = "^SB_")) {
  # Get the data frame by its name
  df <- get(df_name)
  
  # Ensure the "TIME" column exists in the data frame
  if (!"TIME" %in% colnames(df)) {
    warning(paste("TIME column not found in", df_name, "- Skipping this data frame"))
    next  # Skip to the next data frame if "TIME" column is missing
  }
  
  # Get the number of columns in the data frame
  num_cols <- ncol(df)
  
  # Loop through columns from 4 to the end of the data frame
  for (col_idx in 4:num_cols) {
    # Check if the column contains any non-NA values
    if (any(!is.na(df[[col_idx]]))) {
      # Get the non-NA values and the corresponding TIME values
      non_na_index <- which(!is.na(df[[col_idx]]))
      non_na_TIME <- df$TIME[non_na_index]
      non_na_values <- df[[col_idx]][non_na_index]
      
      # Perform cubic spline interpolation using the TIME column as the x-values
      interpolated_values <- spline(x = non_na_TIME, y = non_na_values, 
                                    xout = df$TIME)$y
      
      # Clip interpolated values to stay within [-100, 100]
      interpolated_values <- pmin(pmax(interpolated_values, -100), 100)
      
      # Replace the values in the column with the clipped interpolated values
      df[[col_idx]] <- interpolated_values
    }
  }
  
  # Update the data frame back to the environment
  assign(df_name, df, envir = .GlobalEnv)
}

# CHECK CHANGES
ggplot(SB_1011, aes(x = TIME, y = `S10_D11.hbr`)) + 
  geom_line() +  # Line plot to visualize the change over time
  labs(x = "Time", y = "`S3_D3.hbr`", title = "Time vs `S3_D3.hbr`") +
  theme_minimal()

################################################################################
######################## SMOOTH THE SIGNAL OUT



# PRE-SMOOTHING PLOT 
ggplot(SB_1152, aes(x = TIME, y = `S6_D17.hbr`)) + 
  geom_line() +  # Line plot to visualize the change over time
  labs(x = "Time", y = "`S3_D3.hbr`", title = "Time vs `S3_D3.hbr`") +
  theme_minimal()


# Loop through all data frames with the prefix "SB_"
for (df_name in ls(pattern = "^SB_")) {
  # Get the data frame by its name
  df <- get(df_name)
  
  # Get the number of columns in the data frame
  num_cols <- ncol(df)
  
  # Create a copy of the data frame for smoothed data
  smoothed_df <- df
  
  # Check if there are at least 4 columns to process
  if (num_cols >= 4) {
    # Loop through columns from 4 to the end of the data frame
    for (col_idx in 4:num_cols) {
      # Check if the column contains any non-NA values
      if (any(!is.na(df[[col_idx]]))) {
        # Apply the Savitzky-Golay filter (p = 3, n = 15) to the column
        smoothed_df[[col_idx]] <- sgolayfilt(df[[col_idx]], p = 3, n = 15)
      }
    }
  } else {
    warning(paste("Data frame", df_name, "has fewer than 4 columns - No smoothing applied"))
  }
  
  # Create new data frame name with "SM_" prefix
  new_df_name <- sub("^SB_", "SM_SB_", df_name)
  
  # Save the smoothed data frame to the global environment
  assign(new_df_name, smoothed_df, envir = .GlobalEnv)
}


# POST-SMOOTHING PLOT 
ggplot(SM_SB_1152, aes(x = TIME, y = `S6_D17.hbr`)) + 
  geom_line() +  # Line plot to visualize the change over time
  labs(x = "Time", y = "`S3_D3.hbr`", title = "Time vs `S3_D3.hbr`") +
  theme_minimal()


################################################################################
######################## SAVE DATA

### DATA SAVED AT THIS POINT IS:
# CLEANED (IN SATORI)
# IMPROBABLE VALES EXCEEDING -100/+100 MOVED TO NA 
# WITHIN COLUMN MISSINGNESS HANDLED USING CUBIC SPLINE INTERPOLATION 
# SMOOTHED USING THE Savitzky-Golay filter


# DEFINE OUTPUT FOLDER 
output_folder <- "FINAL_CLEAN_TASK_DATA\\SENBACK_T2\\ALL_CHANNELS"

# CREATE THE FOLDER IF NOT THERE
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# ID FILES TO SAVE 
df_list <- ls(pattern = "^SM_SB_\\d{4}$")

# Check if any data frames were found
if (length(df_list) == 0) {
  warning("No data frames found with pattern 'SM_SB_XXXX'")
} else {
  # Loop through all data frames with the prefix "SM_SB_" followed by 4 digits
  for (df_name in df_list) {
    # Get the data frame by its name
    df <- get(df_name)
    
    # Extract the 4-digit ID from the data frame name
    id <- sub("^SM_SB_(\\d{4})$", "\\1", df_name)
    
    # Define the file path for saving the CSV
    file_path <- file.path(output_folder, paste0("DORRY3_FNIRS_CHANNELS_", id, "_SENBACK_T2.csv"))
    
    # Save the data frame as a CSV
    write.csv(df, file_path, row.names = FALSE)
    
    # Print the name of the saved file
    message(paste("Saved:", file_path))
  }
}


