## LIBRARY 

library(dplyr)
library(ggplot2)
library(signal)
library(forecast)
library(stringr)
library(purrr)

## SET WORKING DIRECTORY 

POUNDTOWN <- 0

# set working directory
if (POUNDTOWN == 1) {
  work_dir <- 'D:\\FNIRS\\FINAL_CLEAN_TASK_DATA\\SENBACK_T1\\'
} else {
  work_dir <- 'F:\\FNIRS\\FINAL_CLEAN_TASK_DATA\\SENBACK_T1\\'
}

setwd(work_dir)


################################################################################
######################### LOAD CLEAN fNIRS CHANNEL DATA 

# Define the folder path
input_folder <- "CHANNELS_W_REGIONS\\"

# Get the list of CSV files in the folder
csv_files <- list.files(path = input_folder, pattern = "_SENBACK_T1_PW.csv$", full.names = TRUE)

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
##################### REDUCE DF TO COLUMNS OF INTEREST - ADD ID 

# Get list of dataframes with SB_ suffix
df_list <- ls(pattern = "^SB_")

for (df_name in df_list) {
  # Extract numeric portion for ID
  id_num <- as.numeric(gsub("SB_", "", df_name))
  
  # Get the dataframe
  df <- get(df_name)
  
  # Add ID column and select specified variables in desired order
  df <- df %>%
    mutate(ID = id_num) %>%
    select(ID, TIME, GAME, VALENCE, 
           L_MFG_OX, R_MFG_OX, L_MFG_DE, R_MFG_DE,
           L_aSFG_OX, R_aSFG_OX, L_aSFG_DE, R_aSFG_DE,
           L_pSFG_OX, R_pSFG_OX, L_pSFG_DE, R_pSFG_DE,
           L_TPJ_OX, R_TPJ_OX, L_TPJ_DE, R_TPJ_DE)
  
  # Assign modified dataframe back to original name
  assign(df_name, df)
}


################################################################################
###################### CREATE STIMULUS DUMMYS

VALENCE_AND_GAME <- function(df) {
  # Check if VALENCE column exists
  if ("VALENCE" %in% names(df)) {
    # Create new variables based on conditions for VALENCE, treating NA as 0
    df$NEG_FACE <- ifelse(df$VALENCE == "NegFace", 1, 0)
    df$NEUT_FACE <- ifelse(df$VALENCE == "NeutFace", 1, 0)
    df$PLACE <- ifelse(df$VALENCE == "Place", 1, 0)
    df$POS_FACE <- ifelse(df$VALENCE == "PosFace", 1, 0)
    
    # Replace NAs with 0 after applying conditions
    df$NEG_FACE[is.na(df$VALENCE)] <- 0
    df$NEUT_FACE[is.na(df$VALENCE)] <- 0
    df$PLACE[is.na(df$VALENCE)] <- 0
    df$POS_FACE[is.na(df$VALENCE)] <- 0
  } else {
    stop("The 'VALENCE' column does not exist in the data frame.")
  }
  
  # Check if GAME column exists
  if ("GAME" %in% names(df)) {
    # Create new variables based on conditions for GAME, treating NA as 0
    df$Z_BACK <- ifelse(df$GAME == "0-Back", 1, 0)
    df$TWO_BACK <- ifelse(df$GAME == "2-Back", 1, 0)
    
    # Replace NAs with 0 after applying conditions
    df$Z_BACK[is.na(df$GAME)] <- 0
    df$TWO_BACK[is.na(df$GAME)] <- 0
  } else {
    stop("The 'GAME' column does not exist in the data frame.")
  }
  
  return(df)
}

# LIST OUT THE EN-BACK DATA FRAMES 
df_list <- ls(pattern = "^SB_")  

## LOOP THROUGH AND ADD THE DUMMY VARIABLES. 
for (df_name in df_list) {
  df <- get(df_name)
  
  print(paste("Processing:", df_name))  # Debugging
  
  # Apply the function to add the new variables
  result_df <- VALENCE_AND_GAME(df)
  
  # Save the updated data frame with the same name
  assign(df_name, result_df, envir = .GlobalEnv)
}

################################################################################
###################### CONVOLVE THE STIMULI INDICES 


# Define fNIRS-specific HRF for 1Hz sampling
hrf <- function(t, peak1 = 6, undershoot = 16, ratio = 0.05) {
  # Canonical gamma HRF, tuned for fNIRS
  hrf_peak <- (t / peak1)^5 * exp(-t / peak1)  # Peak at ~5s
  hrf_undershoot <- ratio * (t / undershoot)^5 * exp(-t / undershoot)  # Weak undershoot
  hrf <- hrf_peak - hrf_undershoot
  return(hrf / sum(abs(hrf)))  # Normalize to sum of absolute values = 1
}

# Create HRF at 1Hz for 25s
t <- seq(0, 25, by = 1)  # 1Hz, 25s duration
hrf_values <- hrf(t)
# Skip centering to preserve amplitude
hrf_values <- hrf_values

# Function to convolve with validation and scaling
convolve_with_hrf <- function(stimulus, hrf_values) {
  stimulus <- as.numeric(stimulus)
  if (length(stimulus) < length(hrf_values)) {
    warning("Stimulus length shorter than HRF. Padding with zeros.")
    stimulus <- c(stimulus, rep(0, length(hrf_values) - length(stimulus)))
  }
  # Check stimulus validity
  if (sum(!is.na(stimulus)) == 0) {
    warning("Stimulus is all NA.")
    return(rep(0, length(stimulus)))
  }
  if (sd(stimulus, na.rm = TRUE) == 0) {
    warning("Stimulus has zero variance.")
    return(rep(0, length(stimulus)))
  }
  # Convolve
  convolved_signal <- convolve(stimulus, rev(hrf_values), type = "open")
  convolved_signal <- convolved_signal[1:length(stimulus)]
  # Scale to match boxcar variance (approximate)
  if (sd(convolved_signal, na.rm = TRUE) > 0) {
    convolved_signal <- convolved_signal * (sd(stimulus, na.rm = TRUE) / sd(convolved_signal, na.rm = TRUE))
  } else {
    warning("Convolved signal has zero variance.")
    convolved_signal <- rep(0, length(stimulus))
  }
  return(convolved_signal)
}

# Stimulus variables
stimulus_vars <- c("NEG_FACE", "NEUT_FACE", "PLACE", "POS_FACE", "Z_BACK", "TWO_BACK")

# Loop through dataframes
df_list <- ls(pattern = "^SB_")
if (length(df_list) == 0) {
  stop("No SB_* dataframes found.")
}
for (df_name in df_list) {
  df <- get(df_name)
  if (nrow(df) == 0) {
    cat("Skipping empty data frame:", df_name, "\n")
    next
  }
  cat("Processing:", df_name, "\n")
  for (stim_var in stimulus_vars) {
    if (stim_var %in% names(df)) {
      df[[paste0(stim_var, "_HRF")]] <- convolve_with_hrf(df[[stim_var]], hrf_values)
      # Debug: print variance and non-NA count
      cat(stim_var, "_HRF sd:", sd(df[[paste0(stim_var, "_HRF")]], na.rm = TRUE), "\n")
      cat(stim_var, "_HRF non-NA count:", sum(!is.na(df[[paste0(stim_var, "_HRF")]])), "\n")
    } else {
      cat(stim_var, "not found in", df_name, "\n")
    }
  }
  assign(df_name, df, envir = .GlobalEnv)
}

# Verify one dataframe
df <- get(df_list[1])
cat("\nVerification for", df_list[1], ":\n")
if ("TWO_BACK_HRF" %in% names(df)) {
  cat("TWO_BACK_HRF summary:\n")
  print(summary(df$TWO_BACK_HRF))
  cat("sd:", sd(df$TWO_BACK_HRF, na.rm = TRUE), "\n")
  cat("Non-NA count:", sum(!is.na(df$TWO_BACK_HRF)), "\n")
  cat("Unique values (first 10):", 
      paste(head(unique(df$TWO_BACK_HRF[!is.na(df$TWO_BACK_HRF)]), 10), collapse = ", "), "\n")
  # Plot to visualize
  plot(df$TWO_BACK_HRF, type = "l", main = paste("TWO_BACK_HRF in", df_list[1]), 
       xlab = "Time (s)", ylab = "TWO_BACK_HRF")
  # Compare with boxcar
  if ("TWO_BACK" %in% names(df)) {
    cat("\nTWO_BACK boxcar summary:\n")
    print(summary(df$TWO_BACK))
    cat("sd:", sd(df$TWO_BACK, na.rm = TRUE), "\n")
    plot(df$TWO_BACK, type = "l", main = paste("TWO_BACK boxcar in", df_list[1]), 
         xlab = "Time (s)", ylab = "TWO_BACK")
  }
} else {
  cat("TWO_BACK_HRF not found.\n")
}

# POST-SMOOTHING PLOT 
ggplot(SB_1152, aes(x = TIME, y = `TWO_BACK_HRF`)) + 
  geom_line() +  # Line plot to visualize the change over time
  labs(x = "Time", y = "`S3_D3.hbr`", title = "Time vs `S3_D3.hbr`") +
  theme_minimal()


################################################################################
###################### RUN THE WITHIN PERSON GLM


# Function to run GLMs and extract betas for a single df
run_glms_and_extract_betas <- function(df, ID) {
  # List of outcomes
  outcomes <- c("L_MFG_OX", "R_MFG_OX", "L_MFG_DE", "R_MFG_DE",
                "L_aSFG_OX", "R_aSFG_OX", "L_aSFG_DE", "R_aSFG_DE",
                "L_pSFG_OX", "R_pSFG_OX", "L_pSFG_DE", "R_pSFG_DE",
                "L_TPJ_OX", "R_TPJ_OX", "L_TPJ_DE", "R_TPJ_DE")
  
  # Define the predictors
  predictors <- c("NEG_FACE_HRF", "NEUT_FACE_HRF", "PLACE_HRF", "POS_FACE_HRF")

  # Check predictor validity (existence and non-zero variance)
  valid_predictors <- c()
  for (predictor in predictors) {
    if (!predictor %in% names(df)) {
      warning(paste("Predictor", predictor, "missing for ID", ID))
      df[[predictor]] <- 0  # Add zero column to maintain structure
    } else if (sum(!is.na(df[[predictor]])) == 0 || sd(df[[predictor]], na.rm = TRUE) == 0) {
      warning(paste("Predictor", predictor, "has no valid data or zero variance for ID", ID))
      df[[predictor]] <- 0
    } else {
      valid_predictors <- c(valid_predictors, predictor)
    }
  }
  
  # If no valid predictors, return NA betas
  if (length(valid_predictors) == 0) {
    warning(paste("No valid predictors for ID", ID))
    betas_vector <- rep(NA, length(outcomes) * length(predictors))
    colnames_vector <- unlist(lapply(outcomes, function(o) 
      paste(o, predictors, "BETA", sep = "_")))
    betas_df <- as.data.frame(matrix(betas_vector, nrow = 1))
    colnames(betas_df) <- colnames_vector
    betas_df$ID <- ID
    betas_df <- betas_df[, c("ID", setdiff(names(betas_df), "ID"))]
    return(betas_df)
  }
  
  # Initialize betas vector
  betas_vector <- c()
  
  # Loop through each outcome
  for (outcome in outcomes) {
    # Check for NA outcomes
    if (all(is.na(df[[outcome]]))) {
      betas_vector <- c(betas_vector, rep(NA, length(predictors)))
      next
    }
    
    # Construct formula with valid predictors
    formula <- as.formula(paste(outcome, "~", paste(valid_predictors, collapse = " + ")))
    
    # Run GLM
    glm_model <- glm(formula, data = df, family = gaussian())
    
    # Extract coefficients
    coeffs <- summary(glm_model)$coefficients
    
    # Initialize betas for all predictors (NA by default)
    outcome_betas <- rep(NA, length(predictors))
    names(outcome_betas) <- predictors
    
    # Fill in betas for valid predictors
    for (predictor in valid_predictors) {
      if (predictor %in% rownames(coeffs)) {
        outcome_betas[predictor] <- coeffs[predictor, "Estimate"]
      }
    }
    
    # Append betas
    betas_vector <- c(betas_vector, outcome_betas)
  }
  
  # Create column names
  colnames_vector <- c()
  for (outcome in outcomes) {
    for (predictor in predictors) {
      colnames_vector <- c(colnames_vector, paste(outcome, predictor, "BETA", sep = "_"))
    }
  }
  
  # Create betas dataframe
  betas_df <- as.data.frame(matrix(betas_vector, nrow = 1))
  colnames(betas_df) <- colnames_vector
  betas_df$ID <- ID
  betas_df <- betas_df[, c("ID", setdiff(names(betas_df), "ID"))]
  
  return(betas_df)
}

# Loop through dataframes
df_list <- ls(pattern = "^SB_")

# Initialize results list
results_list <- list()

# Loop over each dataframe
for (df_name in df_list) {
  # Get dataframe
  df <- get(df_name)
  
  # Skip empty dataframes
  if (nrow(df) == 0) {
    print(paste("Skipping empty data frame:", df_name))
    next
  }
  
  # Extract ID
  df_id <- sub("^SB_", "", df_name)
  
  # Run GLMs and extract betas
  glm_results_df <- run_glms_and_extract_betas(df, df_id)
  
  # Store result
  results_list[[df_id]] <- glm_results_df
}

# Combine results
final_results_df <- do.call(rbind, results_list)


################################################################################
###################### CREATE CONDITION CONTRASTS 

## WE ARE ONLY MAKING WITHIN-RUN CONDITION CONSTRASTS BECAUSE THIS IS AN EXPERIMENTAL
## DESIGN, IN WHICH THERE IS A FAMILY INTERVENTION BETWEEN THE RUNS. IT ISN'T ACCURATE 
## TO AVERAGE RUN 1 AND RUN 2 PRIOR TO CONTRASTS BECAUSE THEY ARE QUALITATIVELY 
## DIFFERENT STATES


# Get variable names from final_results_df
var_names <- names(final_results_df)

# Filter variables matching the pattern (start with L_ or R_, have 3+ underscores)
var_names <- var_names[grep("^[LR]_.*_.*_.*", var_names)]

# Stop if no valid variables
if (length(var_names) == 0) {
  stop("No variables match the pattern. Check final_results_df columns.")
}

# Parse variable names
var_info <- data.frame(
  var_name = var_names,
  stringsAsFactors = FALSE
)
var_info$parts <- str_split(var_info$var_name, "_")
var_info$hemisphere <- sapply(var_info$parts, "[", 1)
var_info$region <- sapply(var_info$parts, "[", 2)
var_info$oxy_deoxy <- sapply(var_info$parts, "[", 3)
var_info$condition <- sapply(var_info$parts, function(x) paste(x[4:length(x)], collapse = "_"))

# Exclude TWO_BACK and Z_BACK conditions
var_info <- var_info[!var_info$condition %in% c("TWO_BACK_HRF_BETA", "Z_BACK_HRF_BETA"), ]

var_info <- var_info[, c("var_name", "hemisphere", "region", "oxy_deoxy", "condition")]

# Print var_info for debugging
cat("var_info preview:\n")
print(head(var_info))

# Create unique locations
locations <- unique(var_info[, c("hemisphere", "region", "oxy_deoxy")])
locations$location <- paste(locations$hemisphere, locations$region, locations$oxy_deoxy, sep = "_")

# Stop if locations is empty
if (nrow(locations) == 0) {
  stop("No locations created. Check var_info parsing.")
}

# Print locations for debugging
cat("locations preview:\n")
print(head(locations))

# Function to compute contrasts
compute_contrasts <- function(loc_row, data) {
  h <- loc_row[["hemisphere"]]
  r <- loc_row[["region"]]
  od <- loc_row[["oxy_deoxy"]]
  loc <- loc_row[["location"]]
  
  # Filter var_info for this location
  loc_vars <- var_info[var_info$hemisphere == h & var_info$region == r & var_info$oxy_deoxy == od, ]
  
  # Get variable names for conditions
  pos_face <- loc_vars$var_name[loc_vars$condition == "POS_FACE_HRF_BETA"]
  neg_face <- loc_vars$var_name[loc_vars$condition == "NEG_FACE_HRF_BETA"]
  neut_face <- loc_vars$var_name[loc_vars$condition == "NEUT_FACE_HRF_BETA"]
  place <- loc_vars$var_name[loc_vars$condition == "PLACE_HRF_BETA"]
  
  # Initialize contrasts dataframe
  contrasts <- data.frame(row_id = 1:nrow(data))
  
  # PosVNeu
  contrasts[[paste0(loc, "_PosVNeu")]] <- if (length(pos_face) > 0 && length(neut_face) > 0) {
    data[[pos_face]] - data[[neut_face]]
  } else {
    NA
  }
  
  # NEGvNEUT
  contrasts[[paste0(loc, "_NEGvNEUT")]] <- if (length(neg_face) > 0 && length(neut_face) > 0) {
    data[[neg_face]] - data[[neut_face]]
  } else {
    NA
  }
  
  # FACEvPLACE
  contrasts[[paste0(loc, "_FACEvPLACE")]] <- if (length(pos_face) > 0 && length(neg_face) > 0 && 
                                                 length(neut_face) > 0 && length(place) > 0) {
    (data[[pos_face]] + data[[neg_face]] + data[[neut_face]]) - data[[place]]
  } else {
    NA
  }
  
  # EMOvNEUT
  contrasts[[paste0(loc, "_EMOvNEUT")]] <- if (length(pos_face) > 0 && length(neg_face) > 0 && 
                                               length(neut_face) > 0) {
    (data[[pos_face]] + data[[neg_face]]) - data[[neut_face]]
  } else {
    NA
  }
  
  return(contrasts)
}

# Apply contrasts for each location
contrast_list <- lapply(1:nrow(locations), function(i) {
  compute_contrasts(locations[i, ], final_results_df)
})

# Combine contrasts
all_contrasts <- Reduce(function(x, y) {
  merge(x, y, by = "row_id", all = TRUE)
}, contrast_list)
all_contrasts$row_id <- NULL

# Add contrasts to original dataframe
final_results_df <- cbind(final_results_df, all_contrasts)


#######################################################################
######################## SAVE DF 

######### ADD T1/T2 DESIGNATORS 

if (exists("final_results_df")) {
  colnames(final_results_df)[-which(colnames(final_results_df) == "ID")] <- 
    paste0(colnames(final_results_df)[-which(colnames(final_results_df) == "ID")], "_SBT1")
  print("Updated column names in final_results_df:")
  print(colnames(final_results_df))
  write.csv(final_results_df, "SENBACK_T1_GLM_RESULTS.csv", row.names = FALSE)
}


