library(lubridate)
library(hms)
library(tidyverse)

# functions ---------------------------------------------------------------

# Function that scales variables:

scaleVar <-
  function(var) {
    (var - mean(var, na.rm = TRUE)) / sd(var, na.rm = TRUE)
  }

# For 95% confidence interval:

confInt95 <- function(x){
  qnorm(0.975)*sd(x)/sqrt(length(x))
}

# load data ---------------------------------------------------------------

observations <- 
  read_csv('data/observations.csv') %>%
  mutate(observation_id = 1:nrow(.)) %>%
  select(observation_id, date:notes) %>%
  filter(!is.na(species)) %>%
  mutate(
    species = toupper(species) %>%
      ifelse(. == 'HOP', 'HOSP', .))

sampling <- 
  read_csv('data/sampling_events.csv') %>%
  rename(treatment_id = treatement_id) %>%
  select(
    treatment_id:treatment,
    monitoring_start:treatment_end)

species <-
  read_csv('data/species_info.csv')

# wrangle data ------------------------------------------------------------

join_frame <-
  observations %>%
  left_join(
    sampling,
    by = 'treatment_id') %>%
  left_join(
    species,
    by = c('species' = 'alpha_code')) %>%
  select(
    treatment_id,
    treatment,
    time,
    species,
    species_class,
    monitoring_start,
    treatment_start,
    treatment_end)

test_frame <-
  join_frame %>%
  mutate(
    monitoring_start = as.hms(treatment_start - 15*60),
    monitoring_end = as.hms(treatment_end + 15*60),
    tTime = ifelse(
      time < treatment_start,
      as.numeric(time - monitoring_start)/60,
      as.numeric(time - treatment_end)/60)) %>%
  filter(
    time > monitoring_start & time < monitoring_end,
    time < treatment_start | time >= treatment_end) %>%
  # Subset to species observed before and after the treatment:
  group_by(treatment_id) %>%
  filter(
    species %in% c(species[time < treatment_start])) %>% #,
  ungroup %>%
  mutate(prePost = ifelse(
    time < treatment_start,
    'pre',
    'post')) %>%
  select(-c(monitoring_start, monitoring_end))

# How many observations were there for each species?

# How many observations were there for each treatment?
