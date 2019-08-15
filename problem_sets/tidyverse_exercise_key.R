# setup -------------------------------------------------------------------

# Load libraries:

library(tidyverse)
library(lubridate)

gitUrl <-
  'https://raw.githubusercontent.com/bsevansunc/'

courseData <-
  'smsc_data_science/master/data/'

states1975 <- 
  read_csv(
    paste0(
      gitUrl,
      courseData,
      'states1975.csv'))

birdCounts <-
  read_csv(
    paste0(
      gitUrl,
      courseData,
      'bird_rawCounts.csv'))

birdHabits <-
  read_csv(
    paste0(
      gitUrl,
      courseData,
      'birdHabits.csv'))

measurements <- 
  read_csv(
    paste0(
      gitUrl,
      courseData,
      'measurements.csv'))

origins <- 
  read_csv(
    paste0(
      gitUrl,
      courseData,
      'origins.csv'))

# 1. The population of the Bahamas -------------------------------------------

population

# Calculate the population of the Bahamas from 1995-2013. Return your results as 
# a two-column data frame with the columns year and population.
  
population %>%
  filter(country == "Bahamas") %>%
  select(year, population)

# 2. States ---------------------------------------------------------------

# Use group_by and summarize to calculate the population density of each region.
# Return your results as a two-column data frame with the columns region and 
# popDensity.

states1975 %>%
  group_by(region) %>%
  summarize(population_density = sum(population)/sum(area))

# 3. Counting birds at site "apple" ---------------------------------------

birdCounts

birdHabits

# Calculate the total number of birds observed in each diet class observed at site "apple" in  2009.
# Return your results as a two-column data frame with the columns diet and count.

birdCounts %>%
  filter(site == 'apple') %>%
  left_join(birdHabits, by = 'species') %>%
  group_by(diet) %>%
  summarize(n = n())

# 4. Star Wars ------------------------------------------------------------

measurements

origins

# Calculate the average mass and number of characters associated with each  
# species of the Star Wars universe. Return your results as a three-column data
# frame with the columns: species, nCharacters, and meanMass.

origins %>%
  left_join(
    measurements, by = 'name'
  ) %>%
  group_by(species) %>%
  summarize(
    nCharacters = n(),
    meanMass = mean(mass, na.rm = TRUE)
  )

# What's up with that NA? Turns our that Sly Moore, from Umbara, has 
# no species data recorded. This record can be easily fixed using an
# ifelse statment:

origins %>%
  mutate(species = ifelse(
    name == 'Sly Moore',
    'Umbaran',
    species)) %>%
  left_join(
    measurements, by = 'name'
  ) %>%
  group_by(species) %>%
  summarize(
    nCharacters = n(),
    meanMass = mean(mass, na.rm = TRUE)
  )

# 5. Iris petal shape -----------------------------------------------------

irisTbl <-
  as_tibble(iris)

# You have developed a derived variable, "petalShape" to describe Iris flowers. 
# This variable is defined as the ratio of petal.Length to petal.Width 
# (petal.Length/petal.Width). Calculate the average (mean) shape of petals in the
# iris dataset. Return your results as a two-column data frame with the columns 
# species and petalShape_mean.

irisTbl %>% 
  group_by(Species) %>%
  summarize(petalShape = mean(Petal.Length/Petal.Width))

