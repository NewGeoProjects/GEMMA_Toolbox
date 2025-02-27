
# LIBRARY CHECK AND INSTALL ====================================================
# List of required libraries
libraries_to_install <- c(
  "rstudioapi",
  "vegan",
  "tidyr",
  "ggplot2",
  "corrplot",
  "readxl"
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


# SET WORKING DIRECTORY ========================================================
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

# SELECT CONTINUOUS VARIABLES ==================================================
"
This example uses the OpenLandslideProject dataset (Licata et al., 2023). 
Variable selection should be adjusted according to the specific dataset used.
"

cat("Dataset columns: ", colnames(df_raw))
print("Please select only the continuous columns...")

env <- subset(df_raw, select = 
                c(slope, spi, tri, twi, plc, prc, CS_perc) # <-- Select only continuous variable columns
) 

# Identify categorical variables (non-numeric columns)
categorical_vars <- df_raw[, !names(df_raw) %in% colnames(env), drop = FALSE]  

# Print selected variable names
cat("Continuous variables (for outlier removal process):\n", colnames(env))
cat("Categorical variables:\n", colnames(categorical_vars))

# Function to detect outliers using the IQR method
identify_outliers <- function(data) {
  apply(data, 1, function(row) {
    outlier_flags <- sapply(row, function(x) {
      if (is.na(x)) return(FALSE)  # Ignore missing values
      q1 <- quantile(row, 0.25, na.rm = TRUE)
      q3 <- quantile(row, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      (x < (q1 - 1.5 * iqr)) || (x > (q3 + 1.5 * iqr))
    })
    any(outlier_flags)  # Flag record as an outlier if any value is an outlier
  })
}

# Standardize the data
standardized_data <- decostand(env, method = "standardize")

# Reshape data into long format for ggplot2 visualization
long_data <- pivot_longer(standardized_data, cols = everything(), names_to = "Variable", values_to = "Value")

# Create a boxplot before outlier removal
qplot_pre <- ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Before Outlier Removal", x = "", y = "Standardized Values")
qplot_pre


# IDENTIFY AND REMOVE OUTLIERS =================================================
outlier_flags <- identify_outliers(env)
outliers <- env[outlier_flags, ]  # Extract records identified as outliers
df_no_outliers <- env[!outlier_flags, ]  # Filter dataset to remove outliers

# PRINT REMOVED OUTLIERS
if (nrow(outliers) > 0) {
  cat("Removed Outlier Records:\n")
  print(rownames(outliers))
} else {
  cat("No outlier records found.\n")
}

# Standardize the cleaned dataset
standardized_data <- decostand(df_no_outliers, method = "standardize")

# Reshape data into long format for visualization
long_data <- pivot_longer(standardized_data, cols = everything(), names_to = "Variable", values_to = "Value")

# Create a boxplot after outlier removal
qplot_post <- ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "After Outlier Removal", x = "", y = "Standardized Values")
qplot_post


# MERGE CATEGORICAL VARIABLES ==================================================
# Check if categorical variables exist
if (ncol(categorical_vars) > 0) {
  # Subset categorical variables for rows retained after outlier removal
  categorical_vars_filtered <- categorical_vars[rownames(df_no_outliers), , drop = FALSE]
  
  # Merge categorical variables back into the dataset
  df_no_outliers <- cbind(df_no_outliers, categorical_vars_filtered)
  
  cat("Categorical variables successfully merged back into the dataset.\n")
} else {
  cat("No categorical variables found in the dataset.\n")
}

# SAVE THE CLEANED DATASET =====================================================
output_filename <- file.path(folder_directory, "dataset_filtered.csv")

if (!is.null(output_filename)) {
  # Ensure row names (IDs) are included as the first column
  df_no_outliers <- cbind(ID = rownames(df_no_outliers), df_no_outliers)
  
  # Save the dataset as a CSV file
  write.table(df_no_outliers, file = output_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  cat("Dataset without outliers saved successfully at:", output_filename, "\n")
} else {
  cat("File saving was cancelled.\n")
}


