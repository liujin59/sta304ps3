#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
raw_data <- read_dta("usa_00001.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(region,
         statefip,
         sex, 
         age, 
         race,
         hispan,
         marst,
         bpl,
         citizen,
         educd,
         labforce)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data <- reduced_data %>% 
  mutate(state = case_when(statefip == "alabama" ~ "AL",
                           statefip == "alaska" ~ "AK",
                           statefip == "arizona" ~ "AZ",
                           statefip == "arkansas" ~ "AR",
                           statefip == "california" ~ "CA",
                           statefip == "colorado" ~ "CO",
                           statefip == "connecticut" ~ "CT",
                           statefip == "delaware" ~ "DE",
                           statefip == "florida" ~ "FL",
                           statefip == "district of columbia" ~ "DC",
                           statefip == "georgia" ~ "GA",
                           statefip == "hawaii" ~ "HI",
                           statefip == "idaho" ~ "ID",
                           statefip == "illinois" ~ "IL",
                           statefip == "indiana" ~ "IN",
                           statefip == "iowa" ~ "IA",
                           statefip == "kansas" ~ "KS",
                           statefip == "kentucky" ~ "KY",
                           statefip == "louisiana" ~ "LA",
                           statefip == "maine" ~ "ME",
                           statefip == "maryland" ~ "MD",
                           statefip == "massachusetts" ~ "MA",
                           statefip == "michigan" ~ "MI",
                           statefip == "minnesota" ~ "MN",
                           statefip == "mississippi" ~ "MS",
                           statefip == "missouri" ~ "MO",
                           statefip == "montana" ~ "MT",
                           statefip == "nebraska" ~ "NE",
                           statefip == "nevada" ~ "NV",
                           statefip == "new hampshire" ~ "NH",
                           statefip == "new jersey" ~ "NJ",
                           statefip == "new mexico" ~ "NM",
                           statefip == "new york" ~ "NY",
                           statefip == "north carolina" ~ "NC",
                           statefip == "north dakota" ~ "ND",
                           statefip == "ohio" ~ "OH",
                           statefip == "oklahoma" ~ "OK",
                           statefip == "oregon" ~ "OR",
                           statefip == "pennsylvania" ~ "PA",
                           statefip == "rhode island" ~ "RI",
                           statefip == "south carolina" ~ "SC",
                           statefip == "south dakota" ~ "SD",
                           statefip == "tennessee" ~ "TN",
                           statefip == "texas" ~ "TX",
                           statefip == "utah" ~ "UT",
                           statefip == "vermont" ~ "VT",
                           statefip == "virginia" ~ "VA",
                           statefip == "washington" ~ "WA",
                           statefip == "west virginia" ~ "WV",
                           statefip == "wisconsin" ~ "WI",
                           statefip == "wyoming" ~ "WY"),
         
         gender = case_when(sex == "female" ~ "Female",
                            sex == "male" ~ "Male"),
         race = case_when(race == "white" ~ "white",
                          race == "black/african american/negro" ~ "black",
                          race == "other race, nec" ~ "other",
                          race == "other asian or pacific islander" ~ "Pacific",
                          race == "japanese" ~ "Asian",
                          race == "chinese" ~ "Asian",
                          race == "american indian or alaska native" ~ "native",
                          race == "two major races" ~ "other",
                          race == "three or more major races" ~ "other"),
         education = case_when(educd %in% c("grade 1", "grade 2", "grade 3", "grade 4",
                                            "grade 5", "grade 6", "grade 7", "grade 8",
                                            "grade 9", "grade 10", "grade 11", "grade 12",
                                            "12th grade, no diploma", "no schooling completed",
                                            "kindergarten", "nursery school, preschool") ~
                                 "less than high school",
                               educd %in% 
                                 c("bachelor's degree", "master's degree", "doctoral degree",
                                   "associate's degree, type not specified",
                                   "some college, but less than 1 year",
                                   "professional degree beyond a bachelor's degree") ~ "college",
                               educd %in% c("ged or alternative credential") ~ "vocational",
                               educd %in% c("regular high school diploma") ~ "high school"),
         work = case_when(labforce == "no, not in the labor force" ~ "no",
                          labforce == "yes, in the labor force" ~ "yes"))

reduced_data <- reduced_data %>% filter(labforce != "n/a") %>% filter(education != "n/a" & 
                                                                        !is.na(education))

reduced_data <- 
  reduced_data %>%
  count(age, gender, state, race, education , work) %>%
  group_by(age, gender, state, race, education, work) 

reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

reduced_data$age <- as.integer(reduced_data$age)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



         