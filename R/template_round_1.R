# This code 
# 1) downloads all files from gdrive
# 2) process them 
# 3) saves them into party folders

library(tidyverse)
library(googledrive)
#library(purrr)
library(readxl)
#library(stringr)
#library(tidyr)
library(here)

# Get the list of folder ids and names
folder_info <- drive_ls(as_id("1aofkAx5VdE1EAsdZO6lFGw_z5tGceZLH")) %>% 
  select(name, id)

# Function to process files in each folder
process_folder_files <- function(folder_id, folder_name) {
  # Create the directory path
  dir_path <- paste0("~/digipols/romania-ep/data/", folder_name, "/")
  
  # Check if directory exists, if not, create it
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # List all Excel files in the folder
  files <- drive_ls(as_id(folder_id), type = "xlsx")
  
  # Download files
  files$id %>%
    map(~ drive_download(as_id(.x), 
                         path = paste0(dir_path, drive_get(as_id(.x))$name)))
  
  # List downloaded Excel files
  file_paths <- list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  # Read and merge all files
  df <- map_df(file_paths, ~ read_excel(.x) %>%
                 mutate(file_name = basename(.x))) %>%
    select(1:9) %>%
    mutate(
      file_name = str_remove(file_name,".xlsx")) %>%
    separate(file_name, into = c("coder", "party"), sep = "_") %>%
    select(-c(party, Dimensiune)) %>%
    unite("id", c(ID, Item), sep = ") ") %>%
    relocate(coder, .before = `Sursa`)
  
  # Save the dataframe
  save(df, file = here("data", folder_name, "df.rda"))
}

# Apply the function to each folder
folder_info %>%
  rowwise() %>%
  mutate(result = list(process_folder_files(id, name)))
