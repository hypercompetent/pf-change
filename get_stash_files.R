quiet_library <- function(...) { suppressPackageStartupMessages(library(...)) }
quiet_library(hise)
quiet_library(dplyr)
quiet_library(purrr)
source("_custom_functions/project_folder_helpers.R")

project_folder <- project_folder <- read.csv("project_folder_params")$name

stash_dirs <- read.csv("XX_stash_directories.csv")

positive <- c("y","Y","yes","Yes","YES")

modalities <- unique(stash_dirs$modality)

message(paste0("Available modalities: ", paste(modalities, collapse = ", ")))
message("Retrieve data for all modalities? Y/n: ")
approval <- readLines("stdin", n=1)

if(approval %in% positive) {
    retrieve_paths(stash_dirs, project_folder)
} else {
    for(modality in modalities) {
        message(paste0("Retrieve data for ",modality,"? Y/n: "))
        approval <- readLines("stdin", n=1)
        if(approval %in% positive) {
            modality_dirs <- stash_dirs[stash_dirs$modality == modality,]
            retrieve_paths(modality_dirs, project_folder)
        }
    } 
}

