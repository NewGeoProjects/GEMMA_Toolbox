


# Load library
library(rstudioapi)
library(vegan)
library(tidyr)
library(ggplot2)
library(corrplot)

# SET FOLDER DIRECTORY
folder_directory <- selectDirectory("Select Folder Directory")
setwd(folder_directory)

# READ DATASET
input_filename <- selectFile("Select Input Dataset", path = folder_directory)
df_raw <- read.delim(input_filename, row.names = 1)
env <- df_raw

# SUBSET OF COLUMNS (if needed)
env <- subset(env, select = -c(plc))

# Function to identify outliers using the IQR method
identify_outliers <- function(data) {
  apply(data, 1, function(row) {
    outlier_flags <- sapply(row, function(x) {
      if (is.na(x)) return(FALSE)  # Ignore NAs
      q1 <- quantile(row, 0.25, na.rm = TRUE)
      q3 <- quantile(row, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      (x < (q1 - 1.5 * iqr)) || (x > (q3 + 1.5 * iqr))
    })
    any(outlier_flags)  # Mark record as outlier if any value is an outlier
  })
}


#Standardize the data
standardized_data <- decostand(env, method = "standardize")

#Convert the data to long format (reshape the dataframe into a format suitable for ggplot2)
long_data <- pivot_longer(standardized_data, cols = everything(), names_to = "Variable", values_to = "Value")

#Create the boxplot
qplot_pre <- ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Before Outlier Removal", x = "", y = "Standardized Values")
qplot_pre


# IDENTIFY AND REMOVE OUTLIERS
outlier_flags <- identify_outliers(env)
outliers <- env[outlier_flags, ]  # Records identified as outliers
df_no_outliers <- env[!outlier_flags, ]  # Dataset without outliers

# PRINT REMOVED OUTLIERS
if (nrow(outliers) > 0) {
  cat("Removed Outlier Records:\n")
  print(rownames(outliers))
} else {
  cat("No outlier records found.\n")
}


#Standardize the data
standardized_data <- decostand(df_no_outliers, method = "standardize")

#Convert the data to long format (reshape the dataframe into a format suitable for ggplot2)
long_data <- pivot_longer(standardized_data, cols = everything(), names_to = "Variable", values_to = "Value")

#Create the boxplot
qplot_post <- ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "After Outlier Removal", x = "", y = "Standardized Values")
qplot_post

# SAVE THE DATASET WITHOUT OUTLIERS
output_filename <- file.path(folder_directory, "dataset_filtered.txt")

if (!is.null(output_filename)) {
  write.table(df_no_outliers, file = output_filename, sep = "\t", row.names = TRUE)
  cat("Dataset without outliers saved successfully at:", output_filename, "\n")
} else {
  cat("File saving was cancelled.\n")
}

# Print column names of the dataset without outliers
cat("Column names of the dataset without outliers:\n")
print(names(df_no_outliers))

