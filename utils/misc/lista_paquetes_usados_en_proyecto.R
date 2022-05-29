# List all functions and packages used by R scripts in a project.

library(NCmisc)
# IMPORTANT: Also load any libraries used by the project

# Make list of all functions by package
funcs <-
  list.files(here::here(), pattern ="\\.R$", recursive = TRUE, full.names = TRUE) %>%
  map(list.functions.in.file) %>%
  flatten

# Check on the functions that weren't assigned to any package.
# These may have been missed either because they are custom functions
# defined in this project, or the package for that function hasn't been loaded.
funcs[names(funcs) == "character(0)"] %>% unlist %>% as.character %>% sort

# Extract just the unique package names
packages <-
  funcs %>%
  names %>%
  str_extract("package:[[:alnum:]]*") %>%
  str_split(",") %>%
  unlist %>%
  str_remove("package:") %>%
  unique %>%
  sort

packages

# See which packages aren't in the tidyverse
setdiff(packages, tidyverse_packages())
