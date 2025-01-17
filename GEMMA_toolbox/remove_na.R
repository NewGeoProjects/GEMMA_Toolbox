


# Load the necessary library
library(rstudioapi)

# SET FOLDER DIRECTORY
folder_directory <- selectDirectory("Select Folder Directory")
setwd(folder_directory)

# READ DATASET
input_filename <- selectFile("Select Input Dataset", path = folder_directory)
df_raw <- read.delim(input_filename, row.names = 1)

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

# SAVE THE CLEANED DATAFRAME IN A TEXT-DELIMITED FORMAT
output_filename <- file.path(folder_directory, "dataset_na_removed.txt")
write.table(df_cleaned, file = output_filename, sep = "\t", row.names = TRUE)

# Print column names of the cleaned dataset
cat("Column names of the cleaned dataset:\n")
print(names(df_cleaned))
