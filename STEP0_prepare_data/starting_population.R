## Extract starting population

## Prepare workspace
library(hyenaR)
library(tidyr)
library(dplyr)
library(SHIM)
library(here)

#This data is up until 2024
## (so that means that individual deaths should be detectable in 2022)
db.path     <- "../hyena_data/Fisidata_20_04_2024.sqlite"

## Starting population file (snapshot of individuals at time 0 for initialising simulations)
load_package_database.full(db.path)
start_pop <- get_data.start(db.path = db.path, interactive = FALSE)
## Needs to be an RDS because we have a nested selections columns!
saveRDS(start_pop, here("./data/starting_data.RDS"))
