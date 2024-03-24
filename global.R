library(dplyr)
library(readxl)
library(purrr)

in_filename <- "./data/lieux_de_camp_2008_2017_avec_adresses.xlsx"
sgdf_data <- read_xlsx(path = in_filename, progress = readxl_progress())
sgdf_data$long_camp <- jitter(sgdf_data$long_camp)
sgdf_data$lat_camp <- jitter(sgdf_data$lat_camp)
# sgdf_data$college <- sgdf_data$college * 100
# sgdf_data$zipcode <- formatC(sgdf_data$zipcode, width=5, format="d", flag="0")
# row.names(sgdf_data) <- sgdf_data$zipcode

# allzips <- readRDS("data_example/superzip.rds")
# allzips$latitude <- jitter(allzips$latitude)
# allzips$longitude <- jitter(allzips$longitude)
# allzips$college <- allzips$college * 100
# allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
# row.names(allzips) <- allzips$zipcode

# cleantable <- allzips %>%
#   select(
#     City = city.x,
#     State = state.x,
#     Zipcode = zipcode,
#     Rank = rank,
#     Score = centile,
#     Superzip = superzip,
#     Population = adultpop,
#     College = college,
#     Income = income,
#     Lat = latitude,
#     Long = longitude
#   )