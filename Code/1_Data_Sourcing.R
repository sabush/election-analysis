library(tidyverse)
library(Census2016)
library(eechidna)
library(janitor)

## Loading election Data ------------

full_2016_census <- Census2016_wide_by_SA2_year %>%
  filter(year == "2016")
head(full_2016_census)

# Yes - this is what I need.

# Loading other tables

ancestories_2016 <- Census2016_ancestories %>%
  filter(year == "2016")
countries_of_birth_2016 <- Census2016_countries_of_birth %>%
  filter(year == "2016")
languages_2016 <- Census2016_languages %>%
  filter(year == "2016")
religions_2016 <- Census2016_religions %>%
  filter(year == "2016")

## Feature Engineering -------------------------------------------------------

# Each of the four variables ancestory, country of birth, languages and religions are quite granular, and it may make sense to look at these variables at a lower level of granularity. 

# <!-- Ancestory: -->
#   <!-- ```{r, error=F, message=F, warning=F} -->
#   <!-- download.file("https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&12490do0001_201912.xls&1249.0&Data%20Cubes&674EFC4CA0A3D8FDCA2584D30012B905&0&2019&18.12.2019&Latest", "./Data/ancestry_classification.xls", method = "libcurl") -->
#   
#   <!-- ancestry_classification_4dig <-  -->
#   <!--   readxl::read_xls("./Data/ancestry_classification.xls", sheet = "Table 1.3",  -->
#                             <!--                    skip = 7, col_names = c("X1", "X2", "Ancestory_Code_4", "Ancestory")) %>% -->
#   <!--   filter(!is.na(Ancestory)) %>% -->
#   <!--   select(Ancestory_Code_4, Ancestory) -->
#   
#   <!-- ancestry_classification_1dig <-  -->
#   <!--   readxl::read_xls("./Data/ancestry_classification.xls", sheet = "Table 1.1",  -->
#                             <!--                    skip = 5, col_names = c("Ancestory_Code_1", "Ancestory_Group"))%>% -->
#   <!--   filter(!is.na(Ancestory_Group)) -->
#   <!-- ``` -->
  
### Country of birth -------
if (!file.exists("./Data/country_classification.xls")) {
  download.file("https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&sacc_12690do0001_201903.xls&1269.0&Data%20Cubes&480BD730AF42D515CA2583BD007707C5&0&2016&15.03.2019&Latest", "./Data/country_classification.xls", method = "libcurl")
}
country_classification_4dig <-
  readxl::read_xls("./Data/country_classification.xls",
                   sheet = "Table 1.3", skip = 7,
                   col_names = c("X1", "X2", "Country_Code_4", "Country")) %>%
  filter(!is.na(Country)) %>%
  dplyr::select(-X1, -X2)

country_classification_2dig <-
  readxl::read_xls("./Data/country_classification.xls",
                   sheet = "Table 1.2", skip = 6,
                   col_names = c("X1", "Country_Code_2", "Country_Name_2")) %>%
  filter(!is.na(Country_Name_2)) %>%
  dplyr::select(-X1)

country_classification_1dig <-
  readxl::read_xls("./Data/country_classification.xls",
                   sheet = "Table 1.1", skip = 5,
                   col_names = c("Country_Code_1", "Country_Group")) %>%
  filter(!is.na(Country_Group))

### Language -----------
if (!file.exists("./Data/language_classification.xls")) {
  download.file("https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&ASCL_12670DO0001_201703.xls&1267.0&Data%20Cubes&F84620CF6E13F7E8CA257FF1001E68A7&0&2016&28.03.2017&Latest", "./Data/language_classification.xls", method = "libcurl")
}

language_classification_4dig <-
  readxl::read_xls("./Data/language_classification.xls",
                   sheet = "Table 1.3", skip = 8,
                   col_names =
                     c("X1", "X2", "X3", "X4",
                       "Language_Code_3", "Language")) %>%
  filter(!is.na(Language)) %>%
  dplyr::select(-X1, -X2, -X3, -X4)

language_classification_1dig <-
  readxl::read_xls("./Data/language_classification.xls",
                   sheet = "Table 1.1", skip = 5,
                   col_names = c("Language_Code_1", "Language_Group")) %>%
  filter(!is.na(Language_Group))

#### Religion ----------

if (!file.exists("./Data/religion_classification.xls")) {
  download.file("https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&ASCRG_12660DO0001_201707.xls&1266.0&Data%20Cubes&B3EAFE3FE6180D37CA257FF1001E673C&0&2016&14.07.2017&Latest", "./Data/religion_classification.xls", method = "libcurl")
}


religion_classification_3dig <-
  readxl::read_xls("./Data/religion_classification.xls",
                   sheet = "Table 1.2", skip = 6,
                   col_names = c("X1", "Religion_Code_3", "Religion")) %>%
  filter(!is.na(Religion)) %>%
  dplyr::select(-X1)

religion_classification_1dig <-
  readxl::read_xls("./Data/religion_classification.xls",
                   sheet = "Table 1.1", skip = 5,
                   col_names = c("Religion_Code_1", "Religion_Group")) %>%
  filter(!is.na(Religion_Group))

## Combine with election data

# Aggregate at the SA2 level - add unless variable contains median, average, persons_per_bedroom

census_2016_all_vars <- Census2016_wide_by_SA2_year %>%
  filter(year == "2016") %>%
  rowwise() %>%
  mutate(sa2_id = paste0(substr(sa2_code, 1, 1), substr(sa2_code, 6, 9))) %>%
  filter(isMissing == FALSE) %>%
  mutate(percent_female = female / persons,
         percent_defacto = defacto_persons / persons,
         percent_married = married_persons / persons,
         percent_indig = indig_persons / persons,
         percent_born_in_australia = born_in_australia / persons,
         percent_unit = flat_or_unit / n_dwellings,
         percent_mortgage = dwelling_owned_mortgage / n_dwellings,
         percent_rent = dwelling_rented / n_dwellings)


census_2016_means <- census_2016_all_vars %>%
  dplyr::select(median_age, median_household_income, average_household_size,
                persons_per_bedroom, median_weekly_rent,
                median_annual_mortgage, sa2_id) %>%
  group_by(sa2_id) %>%
  summarise_all(mean, na.rm = TRUE)

census_2016_counts <- census_2016_all_vars %>%
  dplyr::select(n_dwellings, persons, female, male,
                married_persons, married_females, married_males,
                defacto_persons, defacto_females, defacto_males,
                notmarried_persons, notmarried_females, notmarried_males,
                indig_persons, indig_males, indig_females, non_indig_persons,
                non_indig_females, non_indig_males, not_stated_indig_persons,
                not_stated_indig_males, not_stated_indig_females,
                born_in_australia, born_overseas, country_not_stated,
                separate_house, flat_or_unit,
                housing_other_or_not_stated, semi_or_townhouse,
                dwelling_owned_outright, dwelling_owned_mortgage,
                dwelling_other_or_not_stated, dwelling_rented, sa2_id) %>%
  group_by(sa2_id) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(percent_female = female / persons,
         percent_defacto = defacto_persons / persons,
         percent_married = married_persons / persons,
         percent_indig = indig_persons / persons,
         percent_born_in_australia = born_in_australia / persons,
         percent_unit = flat_or_unit / n_dwellings,
         percent_mortgage = dwelling_owned_mortgage / n_dwellings,
         percent_rent = dwelling_rented / n_dwellings)


# So what I need is weighted demographic data for each of the polling places based on the number of people from each SLA2 who voted at the polling place. Since we don't know who voted where, and who can vote at all, we are making the naive assumptions that 
# * Voters at each SLA are similar
# * Voters are representative of census respondents at the SLA2 level.
# 
# Download and load polling places by SA1

if (!file.exists("./Data/polling-place-by-sa1s-2016.xlsx")) {
  download.file("https://www.aec.gov.au/Elections/Federal_Elections/2016/files/polling-place-by-sa1s-2016.xlsx",
                "./Data/polling-place-by-sa1s-2016.xlsx", method = "libcurl")
}

polling_place_data <-
  readxl::read_xlsx("./Data/polling-place-by-sa1s-2016.xlsx")


# Aggregate polling place data to SA2

polling_place_sa2 <- polling_place_data %>%
  mutate(sa2_id = floor(SA1_id / 100)) %>%
  group_by(year, state_ab, div_nm, pp_id, pp_nm, sa2_id) %>%
  summarise(votes = sum(votes))

# Combine with demographic data and aggregate

polling_place_demog <- polling_place_sa2 %>%
  mutate(sa2_id = as.character(sa2_id)) %>%
  inner_join(census_2016_all_vars)

polling_place_demog_means <- polling_place_demog %>%
  dplyr::select(year, state_ab, div_nm, pp_id, pp_nm, sa2_id, votes,
                median_age, median_household_income, average_household_size,
                persons_per_bedroom, median_weekly_rent,
                median_annual_mortgage, percent_female, percent_defacto,
                percent_married, percent_indig, percent_born_in_australia,
                percent_unit, percent_mortgage, percent_rent) %>%
  group_by(year, state_ab, div_nm, pp_id, pp_nm) %>%
  summarise_at(vars(median_age, median_household_income,
                    average_household_size, persons_per_bedroom,
                    median_weekly_rent, median_annual_mortgage, percent_female,
                    percent_defacto, percent_married, percent_indig,
                    percent_born_in_australia, percent_unit, percent_mortgage,
                    percent_rent), funs(weighted.mean(., w = votes)))

# Add in 2pp at the polling booth level

election_2pp <- twoparty_pollingbooth_download()

polling_place_2pp <- polling_place_demog_means %>%
  group_by() %>%
  rename(StateAb = state_ab,
         DivisionNm = div_nm,
         PollingPlace = pp_nm,
         PollingPlaceID = pp_id) %>%
  mutate(DivisionNm = toupper(DivisionNm),
         PollingPlace = toupper(PollingPlace)) %>%
  left_join(election_2pp %>%
              filter(year == 2016))

saveRDS(polling_place_2pp, './Data/polling_place_2pp.RDS')