library(tidyverse)
library(Census2016)
library(eechidna)
library(janitor)

# Check for missing data

polling_place_2pp %>%
  summarise_all(funs(sum(is.na(.)))) %>% 
  t()

# Which booths have no total votes?
  
polling_place_2pp %>%
  filter(is.na(TotalVotes)) %>%
  tabyl(PollingPlace)

# So these consist of Absent, Postals, Pre-Poll and Provisional votes.

polling_place_2pp_novote <- polling_place_2pp %>%
  filter(is.na(TotalVotes))


# On observation of the data, there appear to be other booths that do cover 
# these vote types. So perhaps this is a multi-election coding issue. Seems
# reasonable to remove

polling_place_2pp_clean <- polling_place_2pp %>%
  filter(!is.na(TotalVotes))

polling_place_2pp_clean %>%
  summarise_all(funs(sum(is.na(.)))) %>% 
  t()

# Which polling stations have missing data? Not too concerned about post code, 
# as there are some special booths

polling_place_2pp_clean %>%
  filter(is.na(median_age) | is.na(Latitude))

# Looks like mobile teams and prepoll centres, and are only missing latitude and 
# longitude (makes sense, since they are mobile). 
# Will remove the Brand mobile team, as the demographic data does not look valid.

polling_place_2pp_clean <- polling_place_2pp_clean %>%
  dplyr::filter(PollingPlaceID != 65161)

saveRDS(polling_place_2pp_clean, './Data/polling_place_2pp_clean.RDS')
