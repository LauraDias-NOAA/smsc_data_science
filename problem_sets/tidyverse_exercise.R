# setup -------------------------------------------------------------------

# Load libraries:

library(tidyverse)
library(lubridate)

gitUrl <-
  'https://raw.githubusercontent.com/bsevansunc/'

courseData <-
  'smsc_data_science/master/data/'

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


# 2. States ---------------------------------------------------------------

states1975

# Use group_by and summarize to calculate the population density of each region.
# Return your results as a two-column data frame with the columns region and 
# popDensity.    


# 3. Counting birds at site "apple" ---------------------------------------

birdCounts

birdHabits

# Calculate the total number of each diet class observed at site "apple" in  2009.
# Return your results as a two-column data frame with the columns diet and count.


# 4. Star Wars ------------------------------------------------------------

measurements

origins

# Calculate the average mass and number of characters associated with each  
# species of the Star Wars universe. Return your results as a three-column data
# frame with the columns: species, nCharacters, and meanMass.


# 5. Iris petal shape -----------------------------------------------------

irisTbl <-
  as_tibble(iris)

# You have developed a derived variable, "petalShape" to describe Iris flowers. 
# This variable is defined as the ratio of petal.Length to petal.Width 
# (petal.Length/petal.Width). Calculate the average (mean) shape of petals in the
# iris dataset. Return your results as a two-column data frame with the columns 
# species and petalShape_mean.

