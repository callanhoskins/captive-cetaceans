# Read in all the cetaceans data

# Author: Callan Hoskins
# Version: 2020-02-03

# Libraries
library(tidyverse)
library(lubridate)

# Parameters
import_file_path <- here::here("data-raw/all_cetaceans_data.csv")
export_file_path <- here::here("data/all_cetaceans_data.rds")

col_types <-
  cols(
    species = col_character(),
    id = col_character(),
    name = col_character(),
    sex = col_character(),
    accuracy = col_character(),
    birthYear = col_integer(),
    acquisition = col_character(),
    originDate = col_date(),
    originLocation = col_character(),
    mother = col_character(),
    father = col_character(),
    transfers = col_character(),
    currently = col_character(),
    region = col_character(),
    status = col_character(),
    statusDate = col_date(),
    COD = col_character(),
    transferDate = col_date(),
    transfer = col_character(),
    entryDate = col_date()
  )

rename_vars <-
  c(
    "birth_year" = "birthYear",
    "origin_date" = "originDate",
    "origin_loc" = "originLocation",
    "status_date" = "statusDate",
    "cod" = "COD",
    "transfer_date" = "transferDate",
    "entry_date" = "entryDate",
    "official_id" = "id"
  )
#===============================================================================
read_csv(
  import_file_path,
  col_types = col_types
) %>%
  rename(!!! rename_vars) %>%
  mutate(birth_year = make_date(birth_year)) %>%
  write_rds(export_file_path)
