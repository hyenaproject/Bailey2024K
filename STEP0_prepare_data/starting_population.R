## Extract starting population

## Prepare workspace
library(hyenaR)
library(tidyr)
library(dplyr)

#This data is up until 2023
## (so that means that individual deaths should be detectable in 2021)
db.path     <- "../hyena_data/Fisidata_2023_03_07.sqlite"

## Starting population file (snapshot of individuals at time 0 for initialising simulations)
load_package_database.full(db.path)
start_pop <- get_data.start(db.path = db.path, interactive = FALSE)
readr::write_excel_csv(start_pop, here("./analysis/data/starting_data.csv"))
