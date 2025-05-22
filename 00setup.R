# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, archive, tools, dplyr)

# Function to check if files exist and download/extract if not
download_and_process_data <- function() {
  # Paths
  base_dir <- "odfs"  # Rename to "odfs" as requested
  rar_url <- "https://datosabiertos.mineduc.cl/wp-content/uploads/2024/10/ENDDEIE_Bases-publicas_SAV.rar"
  rar_file <- "temp_archive.rar"
  
  # Create directory if it doesn't exist
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }
  
  # Define files we want to load
  required_files <- c(
    "ESTUDIANTES_PUBLICA_2023.sav",
    "DOCENTES_PUBLICA_2023.sav",
    "DIRECTORES_PUBLICA_2023.sav",
    "COORDINADORES_PUBLICA_2023.sav",
    "PAUTA_PUBLICA_2023.sav"
  )
  
  # Check if at least one file is missing
  files_exist <- all(sapply(file.path(base_dir, required_files), file.exists))
  
  if (!files_exist) {
    # Download the RAR file
    message("Downloading data files...")
    download.file(rar_url, rar_file, mode = "wb")
    
    # Extract the RAR file
    message("Extracting files...")
    archive_extract(rar_file, dir = ".")
    
    # Rename the extracted directory
    if (dir.exists("Bases publicas sav")) {
      # Move files from "Bases publicas sav" to "odfs"
      file_list <- list.files("Bases publicas sav", full.names = TRUE)
      for (file in file_list) {
        file.copy(file, file.path(base_dir, basename(file)))
      }
      unlink("Bases publicas sav", recursive = TRUE)
    }
    
    # Delete the temporary RAR file
    unlink(rar_file)
  } else {
    message("All required data files already exist in the 'odfs' directory.")
  }
  
  # Load all dataframes
  message("Loading dataframes...")
  
  # Students dataframe
  estodf <- haven::read_sav(file.path(base_dir, "ESTUDIANTES_PUBLICA_2023.sav"))
  
  # Teachers dataframe
  docodf <- haven::read_sav(file.path(base_dir, "DOCENTES_PUBLICA_2023.sav"))
  
  # Principals dataframe
  dirodf <- haven::read_sav(file.path(base_dir, "DIRECTORES_PUBLICA_2023.sav"))
  
  # Coordinators dataframe
  cooodf <- haven::read_sav(file.path(base_dir, "COORDINADORES_PUBLICA_2023.sav"))
  
  # Evaluation form dataframe
  pauodf <- haven::read_sav(file.path(base_dir, "PAUTA_PUBLICA_2023.sav"))
  
  # Return all dataframes as a list
  return(list(
    estodf = estodf,
    docodf = docodf,
    dirodf = dirodf,
    cooodf = cooodf,
    pauodf = pauodf
  ))
}

# Execute the function and store the results
data_list <- download_and_process_data()

# Assign each dataframe to its respective name in the global environment
estodf <- data_list$estodf  # Students
docodf <- data_list$docodf  # Teachers
dirodf <- data_list$dirodf  # Principals
cooodf <- data_list$cooodf  # Coordinators
pauodf <- data_list$pauodf  # Evaluation forms

# Display information about each dataframe
message("Loaded dataframes:")
message("- Students (estodf): ", nrow(estodf), " rows, ", ncol(estodf), " columns")
message("- Teachers (docodf): ", nrow(docodf), " rows, ", ncol(docodf), " columns")
message("- Principals (dirodf): ", nrow(dirodf), " rows, ", ncol(dirodf), " columns")
message("- Coordinators (cooodf): ", nrow(cooodf), " rows, ", ncol(cooodf), " columns")
message("- Evaluation forms (pauodf): ", nrow(pauodf), " rows, ", ncol(pauodf), " columns")

# Remove function & extra large list
rm(download_and_process_data)
rm(data_list)
