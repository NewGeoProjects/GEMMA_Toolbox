# LIBRARY CHECK AND INSTALL ====================================================
# List of required libraries
libraries_to_install <- c(
  "rstudioapi"
)

# Function to check and install missing libraries
check_and_install_library <- function(library_name) {
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
    library(library_name, character.only = TRUE)
  } else {
    cat(paste("Library", library_name, "is already installed.\n"))
  }
}

# Install and load the required libraries
lapply(libraries_to_install, check_and_install_library)



# SET FOLDER DIRECTORY==========================================================
folder_directory <- selectDirectory("Select Folder Directory")
setwd(folder_directory)


# LOAD DATASET =================================================================
"
Dataset requirements:
- Accepted formats:
      - Comma-separated values (.csv) *Note: The delimiter must be a comma (,); semicolons (;) are not allowed*
      - Tab-delimited text (.txt)
      - Excel files (.xlsx)
- Must follow a tidy data structure
- Decimal separator must be a dot (.), commas (,) are not allowed
"

input_filename <- selectFile("Select Input Dataset", path = folder_directory)

# Determine file extension
file_ext <- tools::file_ext(input_filename)

# Read the dataset based on the file type
if (file_ext == "csv") {
  df_raw <- read.csv(input_filename, row.names = 1)
} else if (file_ext == "txt") {
  df_raw <- read.delim(input_filename, row.names = 1)
} else if (file_ext == "xlsx") {
  df_raw <- read_excel(input_filename)
  df_raw <- as.data.frame(df_raw) # Convert to a data frame if it's read as a tibble
  rownames(df_raw) <- df_raw[, 1] # Assume the first column contains row names
  df_raw <- df_raw[, -1]          # Remove the first column after setting row names
} else {
  stop("Unsupported file type. Please upload a .csv, .txt, or .xlsx file.")
}


# NA REMOVE=====================================================================
# PRINT RECORD NAMES WITH NAs
na_records <- rownames(df_raw)[apply(df_raw, 1, function(row) any(is.na(row)))]
if (length(na_records) > 0) {
  cat("Records with missing values:\n")
  print(na_records)
} else {
  cat("No records with missing values.\n")
}

# REMOVE NODATA
df_cleaned <- na.omit(df_raw)  # Remove records (rows) with NA values


# SAVE THE CLEANED DATASET =====================================================
output_filename <- file.path(folder_directory, "dataset_na_removed.csv")

if (!is.null(output_filename)) {
  # Ensure row names (IDs) are included as the first column
  df_cleaned <- cbind(ID = rownames(df_cleaned), df_cleaned)
  
  # Save the dataset as a CSV file
  write.table(df_cleaned, file = output_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  cat("Dataset without NA saved successfully at:", output_filename, "\n")
} else {
  cat("File saving was cancelled.\n")
}
