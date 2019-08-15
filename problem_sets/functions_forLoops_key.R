
# setup -------------------------------------------------------------------

library(tidyverse)

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

# Question 1 --------------------------------------------------------------

# 1.a Using the population dataset, complete the for loop below to
# calculate the population of the Bahamas from 1995 to 2013. 

population

# For loop to calculate the population of the Bahamas across years:

# Answer: The goal at this stage is to generate a unique vector of years 
# upon which we will run our for loop.

yrs <- 
  unique(
    population[population$country == 'Bahamas', ]$year)

# The above first subsets the population tibble to records (rows) in which
# the country value is equal to "Bahamas". This is equivalent to:

population[population$country == 'Bahamas', ]

# We next extract a vector of years from the subsetted tibble:

population[population$country == 'Bahamas', ]$year

# In the tidyverse, it would look like this:

population %>%
  filter(country == 'Bahamas') %>%
  pull(year)

# How is the output from the below different?

population[population$country == 'Bahamas', 'year']

# Given our year vector, we use the unique function to get the vector of
# years that we will use for each iteration of our for loop:

unique(
  population[population$country == 'Bahamas', ]$year)

# This could be written as a series of independent steps:

bahamas <-
  population[population$country == 'Bahamas', ]

bahamas_yrs <-
  bahamas$year

bahamas_yrs_unique <-
  unique(bahamas_yrs)

# Answer: The goal below is to create an empty object to store our results

populationVector <- 
  vector('numeric', length = length(yrs))
           

# Answer: The goal below was to complete the for loop to create a vector
# of population size by year for the Bahamas. There were two solutions
# you could have used:

# Solution 1:

for (i in 1:length(yrs)) {
  # Subset population tibble to the Bahamas:
  whoCountrySubset <- population[population$country == 'Bahamas', ]
  # Subset the resultant tibble to the chosen year:
  whoYrSubset <- whoCountrySubset[whoCountrySubset$year == yrs[i], ]
  # Extract the population value for that choicen year:
  populationVector[i] <- whoYrSubset$population
}

# Why does this work? See below:

1:length(yrs)

yrs[i = 1]

# We could have reduced the above as follows:

for (i in 1:length(yrs)) {
  populationVector[i] <-
    population[
      population$country == 'Bahamas' &
        population$year == yrs[i], ]$population
}

# Solution 2:

for (i in seq_along(yrs)) {
  # Subset population tibble to the Bahamas:
  whoCountrySubset <- population[population$country == 'Bahamas', ]
  # Subset the resultant tibble to the chosen year:
  whoYrSubset <- whoCountrySubset[whoCountrySubset$year == yrs[i], ]
  # Extract the population value for that choicen year:
  populationVector[i] <- whoYrSubset$population
}

# Why does this work? See below:

seq_along(yrs)

yrs[i = 1]

# 1.b Use the population vector you created above to create a two column data
# frame where each record (row) contains the year (column 1) and population 
# (column 2).

# Answer(s): 

data.frame(
  year = yrs,
  population = populationVector
)

tibble(
  year = yrs,
  population = populationVector
)

# Question 1 in the tidyverse ---------------------------------------------

# The subsetting portion of 1a could have been written in the tidyverse
# as:

population %>%
  filter(country == 'Bahamas') %>%
  pull(year) %>%
  unique()

# The tidyverse makes the for loop we used unnecessary:

population %>%
  filter(country == 'Bahamas') %>%
  select(year, population) %>%
  distinct

# Question 2 --------------------------------------------------------------

# 2.a The states1975 dataset contains the fields (columns) region, division, state
# name, area (in square miles), and population of each US state in 1975. Complete the for
# loop below to calculate the population density of each region (population per 
# square mile). Save your results in a two column data frame with the column names 
# region and populationDensity.

states1975

str(states1975)

# Answer: The goal at this stage is to create a unique vector of regions

regions <- unique(states1975$region)

# Answer: The goal at this stage is to create an empty vector to store the
# values populated by the for loop

densityVector <- vector('numeric', length = length(regions))

# Answer: There are two potential solutions for this, as written. These
# include:

# Solution 1:

for(i in 1:length(regions)) {
  regionSubset <- states1975[states1975$region == regions[i],]
  totalArea <- sum(regionSubset$area)
  totalPopulation <- sum(regionSubset$population)
  densityVector[i] <- totalPopulation/totalArea
}

# 2.b Use the densityVector you created above to create a two column data
# frame where each record (row) contains the region (column 1) and population 
# density for that region (column 2).

# Answer:

data.frame(
  region = regions,
  population_density = densityVector
)

tibble(
  region = regions,
  population_density = densityVector
)

# Question 2 in the tidyverse ---------------------------------------------

states1975 %>%
  group_by(region) %>%
  summarize(population_density = sum(population)/sum(area))

# Question 3 --------------------------------------------------------------

# You have been provided with  two datasets that describe characters of the
# Star Wars movies. One dataset, measurements, provides character names, 
# heights, and body mass measurements. The other dataset, origins, 
# describes characters by home world and species. Please take a moment to
# explore these datasets.

measurements

origins

# 3.a Complete the function below to calculate the average mass of a given 
# species in the Star Wars universe and the sample size for this calculation (i.e.,
# the number of characters of this species). This function should return a data frame
# with the fields (columns) species, nCharacters, and meanMass. Use the function to 
# calculate the average mass and number of Droid characters.

# Answer:

sppMass <- function(spp) {
  # Subset data (split):
  namesSubset <- origins[origins$species == spp, ]$name
  measuresSubset <- measurements[measurements$name %in% namesSubset,]
  # Define output (apply):
  species <- spp
  nCharacters <- length(namesSubset)
  meanMass <- mean(measuresSubset$mass, na.rm = TRUE)
  # Combine output:
  outFrame <- data_frame(species, nCharacters, meanMass)
  return(outFrame)
}

sppMass('Droid')

# 3.b Use the function you created above to write a for loop that will calculate
# the average mass of each species in the Star Wars universe. Return your
# results as a data frame with the columns species, nCharacters, and meanMass.

# Answer:

speciesVector <- unique(origins$species)

outList <- vector('list', length = length(speciesVector))

for(i in 1:length(speciesVector)){
  outList[[i]] <- sppMass(speciesVector[i])
}

# or:

outList <- vector('list', length = length(speciesVector))

for(i in seq_along(speciesVector)){
  outList[[i]] <- sppMass(speciesVector[i])
}

bind_rows(outList)

# or:

outList <- vector('list', length = length(speciesVector))

for(i in speciesVector){
  outList[[i]] <- sppMass(i)
}

bind_rows(outList)


# Question 3 in the tidyverse ---------------------------------------------

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

# How might you have fixed this record in base R?




