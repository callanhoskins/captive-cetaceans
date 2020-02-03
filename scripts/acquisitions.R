# Read in acquisitions of cetaceans data, output to .rds file

# Author: Callan Hoskins
# Version: 2020-02-02

# Libraries
library(tidyverse)

# Parameters
import_file_path <- here::here("data-raw/acquisitions.csv")
export_file_path <- here::here("data/acquisitions.rds")

# rename vars according to tidy style
rename_vars <-
  c(
    "year" = "AcqYear",
    "born" = "Born",
    "captured" = "Capture",
    "rescue" = "Rescue"
  )

col_types <-
  cols(
    "AcqYear" = col_integer(),
    "Born" = col_integer(),
    "Capture" = col_integer(),
    "Rescue" = col_integer(),
    "Total" = col_integer()
  )

#===============================================================================

read_csv(
  import_file_path,
  col_names = TRUE,
  col_types = col_types,
  skip_empty_rows = FALSE
) %>%
  select(!!! rename_vars) %>%
  pivot_longer(
    names_to = "source",
    cols = -year,
    values_to = "num_cetaceans"
  ) %>%
  write_rds(export_file_path)
