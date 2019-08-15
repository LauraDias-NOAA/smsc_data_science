
# setup -------------------------------------------------------------------

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

# Question 1 --------------------------------------------------------------

# 1.a Using the population dataset, complete the for loop below to calculate the 
# population of the Bahamas from 1995 to 2013. 

population

# For loop to calculate the population of the Bahamas across years:

yrs <- unique(population[## COMPLETE

populationVector <- 
  vector('numeric', length = ## COMPLETE

for(i in ## COMPLETE
  whoCountrySubset <- population[population$country## COMPLETE
  whoYrSubset <- whoCountrySubset[whoCountrySubset## COMPLETE
  populationVector[i] <- whoYrSubset$## COMPLETE
}

populationVector

# 1.b Use the population vector you created above to create a two column data
# frame where each record (row) contains the year (column 1) and population 
# (column 2).

# Question 2 --------------------------------------------------------------

# 2.a The states1975 dataset contains the fields (columns) region, division, state
# name, area (in square miles), and population of each US state in 1975. Complete the for
# loop below to calculate the population density of each region (population per 
# square mile). Save your results in a two column data frame with the column names 
# region and populationDensity.

states1975

str(states1975)

regions <- unique(## COMPLETE

densityVector <- vector('numeric', ## COMPLETE

for(i in ## COMPLETE
  regionSubset <- states1975[states1975$## COMPLETE
  totalArea <- sum(regionSubset$## COMPLETE
  totalPopulation <- sum(## COMPLETE
  densityVector[i] <- ## COMPLETE
}

# 2.b Use the densityVector you created above to create a two column data
# frame where each record (row) contains the region (column 1) and population 
# density for that region (column 2).


# Question 3 --------------------------------------------------------------

# You have been provided with  two datasets that describe characters of the Star Wars 
# movies. One dataset, measurements, provides character names, heights, and body mass 
# measurements. The other dataset, origins, describes characters by home world and
# species. Please take a moment to explore these datasets.

measurements

origins

# 3.a Complete the function below to calculate the average mass of a given 
# species in the Star Wars universe and the sample size for this calculation (i.e.,
# the number of characters of this species). This function should return a data frame
# with the fields (columns) species, nCharacters, and meanMass. Use the function to 
# calculate the average mass and number of Droid characters.

sppMass <- function(## COMPLETE
  # Subset data (split):
  namesSubset <- origins[origins$species## COMPLETE
  measuresSubset <- measurements[## COMPLETE
  # Define output (apply):
  species <- spp
  nCharacters <- length(## COMPLETE
  meanMass <- mean(## COMPLETE
  # Combine output:
  outFrame <- data_frame(species, ## COMPLETE
  return(## COMPLETE
}

sppMass('Droid')

# 3.b Use the function you created above to write a for loop that will calculate
# the average mass of each species in the Star Wars universe. Return your
# results as a data frame with the columns species, nCharacters, and meanMass.

speciesVector <- unique(## COMPLETE

outList <- vector('list', ## COMPLETE

for(i in ## COMPLETE
  outList[[i]] <- sppMass(## COMPLETE
}

bind_rows(## COMPLETE

