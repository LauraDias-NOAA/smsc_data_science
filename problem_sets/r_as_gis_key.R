# setup -------------------------------------------------------------------

libraries <-
  c(
    'lubridate',
    'stringr',
    'raster',
    'dismo',
    'prism',
    'tidyverse'
  )

# Use purrr::map to load the libraries (note: for library function, include the argument character.only = TRUE):

purrr::map(
  libraries,
  function(x){
    library(x, character.only = TRUE)
  }
)

# get raster data ---------------------------------------------------------

# Set download directory to temp:

options(prism.path = tempdir())

# Set minimum and maximum dates:

dates <-
  seq.Date(
  as.Date('2019-07-22'),
  as.Date('2019-08-05'),
  by = 'day')

# Get the prism data for maximum temperature from their api:

get_prism_dailys(
  type = 'tmax',
  dates = dates,
  keepZip = FALSE)

# Read the help file ?get_prism_dailys

# Get the path to the prism files:

prism_paths <-
  ls_prism_data(absPath = TRUE)

# What sort of object is prism paths?

# Let's view the first path to the prism file:

prism_paths %>%
  slice(1)

# We can see that dates do not have a dash!

# Let's create a vector of length one that represents the file path to the temperature on the first day of class:

path_dataSci_day1 <- 
  prism_paths %>%
  filter(str_detect(files, '20190805')) %>%
  pull(abs_path)

path_dataSci_day1

# make a raster -----------------------------------------------------------

# We can read in the file as a raster (it is one), using the raster::raster function. Let's do so for the temperature raster from the first day of this class:

r <- 
  raster(path_dataSci_day1)

# Let's take a moment to explore this raster file (it is metadata!):

r

# Projection is important! I typically set my team's GPS units to WGS84.

prj <- '+proj=longlat +datum=WGS84 +ellps=WGS84'

r_prj <-
  r %>%
  projectRaster(crs = prj)

# Let's look at the file metadata again:

r_prj

# Wanna see the raster? We can plot it ... but it's going to be ugly:

plot(r_prj)

# Perhaps we are interested in just a smaller extent, not the whole US. Let's try drawing an extent around the Virginia, DC, and Maryland region:

dataSci_extent <- 
  drawExtent()

# We can also set the extent as such (reproducibility!):

dataSci_extent <- 
  extent(c(-79, -76, 38, 40))

# We can crop the raster to the extent using the crop function:

r_crop <-
  crop(r_prj, dataSci_extent)

# And look at the resultant object:

r_crop

plot(r_crop)

# Let's make it look a little better:

plot(
  r_crop, 
  col = rev(
    rainbow(
      100,
      start = .09,
      end = .8)))


# use pipes to do in one step ---------------------------------------------

r_crop <- 
  prism_paths %>%
  filter(str_detect(files, '20190805')) %>%
  pull(abs_path) %>%
  raster() %>%
  projectRaster(crs = prj) %>%
  crop(dataSci_extent)
  
# And look at the resultant object:
  
r_crop

plot(
  r_crop,
  col = rev(
    rainbow(
      100,
      start = .09,
      end = .8)))
  
# raster math -------------------------------------------------------------

# Not a celsius person? We can use raster math to convert the raster from degrees Celsius to Fahrenheit (degF = degC*1.8 + 32):

r_fahr <-
  r_crop*1.8+32

r_fahr

plot(
  r_fahr,
  col = rev(
    rainbow(
      100,
      start = .09,
      end = .8)))

# file cleaning -----------------------------------------------------------

# Because of their size, it's best to remove the rasters that you are done working with. Let's remove all but r_crop. We'll use the rm (remove) function and purrr::map to do so:

purrr::map(
  c('r', 'r_prj', 'r_fahr'),
  function(x){
    rm(x)
  }
)

# make a spatial point ----------------------------------------------------

# Let's add a point. Open Google Earth and find out where the computer lab is!

# Generate a data frame where column one is the longitude (long) and column two is latitude (lat) of the computer lab: 

data.frame(
  long = -78.16478,
  lat = 38.88687
)

# Use mutate to add a site column. Call the site dataSci_classroom.

data.frame(
  long = -78.16478,
  lat = 38.88687
) %>%
  mutate(site = 'dataSci_classroom')

# We can convert this to a spatial points file using the sp::SpatialPointsDataFrame function. Call the resultant object dataSci_site:

dataSci_site <-
  data.frame(
    long = -78.16478,
    lat = 38.88687) %>%
  mutate(site = 'dataSci_classroom') %>%
  SpatialPointsDataFrame(
    .[,1:2],
    data = .,
    proj4string = CRS(prj))

# Take a moment to look at the attributes of the site:

dataSci_site

# We can add this to our plot as follows:

plot(
  r_crop,
  col = rev(
    rainbow(
      100,
      start = .09,
      end = .8)))

points(dataSci_site)

# We can learn the maximum temperature on the first day of class using raster::extract to extract the value of the raster pixel at dataSci_site on day 1 (maximum temperature on day 1):

raster::extract(
  r_crop,
  dataSci_site)

# We can add buffer argument, mean, and na.rm arguments to calculate the average maximum temperature within 10 km of the site:

raster::extract(
  r_crop, 
  dataSci_site, 
  buffer = 10000, 
  fun = mean, 
  na.rm = TRUE)

# Is the temperature at the classroom higher or lower, on average than the surrounding area?

# Advanced R as GIS ... Avoiding repetition -------------------------------

# We can read in all of the files at once using a for loop or purrr::map. 

# We'll first try it with a for loop.

# Make a date vector does not include the dashes:

dates_simple <- 
  dates %>%
  str_replace_all('-', '')

# Make an empty vector to store prism paths:

read_paths <- 
  vector(
    'list',
    length = length(dates_simple))

# Get the prism paths for those dates using a for loop:

for(i in 1:length(read_paths)){
  read_paths[[i]] <-
    prism_paths %>%
    filter(str_detect(files, dates_simple[i])) %>%
    pull(abs_path)
}

# Let's look at the files we might read:

read_paths

# Re-write the for loop above using mapping with purrr::map:

read_paths <-
  purrr::map(
    dates %>%
      str_replace_all('-', ''),
    function(x) {
      prism_paths %>%
        filter(str_detect(files, x)) %>%
        pull(abs_path)
    })

# We can do all of the processing (set path, read, project, and crop) and reading in one step using map:

maxTemp_list <-
  purrr::map(
    dates %>%
      str_replace_all('-', ''),
    function(x) {
      prism_paths %>%
        filter(str_detect(files, x)) %>%
        pull(abs_path) %>%
        raster() %>%
        projectRaster(crs = prj) %>%
        crop(dataSci_extent)
    })

# Look at the resultant object:

maxTemp_list

# We can use the function stack to convert the list to a raster stack:

maxTemp_stack <- 
  stack(maxTemp_list)

# Look at the resultant object:

maxTemp_stack

# Stacks are very cool. You can plot them all at once (but it won't look good!):

plot(maxTemp_stack)

# Or call an individual item as you would from a list:

plot(maxTemp_stack[[1]])

# You can also run a function over a whole stack, like our conversion between Celsius and Fahrenheit:

fahr_stack <- 
  maxTemp_stack*1.8+32

plot(fahr_stack[[1]])

# When you use extract with a stack, you will get a value for each record:

raster::extract(maxTemp_stack, dataSci_site)

# You can use the df = TRUE argument to extract a stack to a data frame:

raster::extract(
  maxTemp_stack, 
  dataSci_site, 
  df = TRUE) %>%
  as_tibble()

# Ugh! it's wide frame! This is an old package. You can make that long using gather:

raster::extract(
  maxTemp_stack,
  dataSci_site, 
  df = TRUE) %>%
  as_tibble() %>%
  gather(
    key = map_date,
    value = maxTemp_C,
    2:16)

# There seems to be a lot of useless information in that column. Let's use mutate to change the column to just a vector of dates:

raster::extract(
  maxTemp_stack,
  dataSci_site, 
  df = TRUE) %>%
  as_tibble() %>%
  gather(
    key = map_date,
    value = maxTemp_C,
    2:16) %>%
  mutate(map_date = dates)


# plot temperature over the last few weeks --------------------------------

# Let's make a ggplot of the maximum daily temperature over the last two weeks here at Front Royal:

raster::extract(
  maxTemp_stack,
  dataSci_site, 
  df = TRUE) %>%
  as_tibble() %>%
  gather(
    key = map_date,
    value = maxTemp_C,
    2:16) %>%
  mutate(map_date = dates) %>%
  ggplot(
    aes(x = map_date, y = maxTemp_C)
  ) +
  geom_point() +
  geom_line()

# Now you! My office is located at the Smithsonian Migratory Bird Center in the National Zoo (long = -77.04779, lat = 38.92594). 

# - Generate a SpatialPointDataFrame that includes the classroom and my office

spPoints <-
  data.frame(
  long = c(-78.16478, -77.04779),
  lat = c(38.88687, 38.92594),
  site = c('dataSci_classroom', 'myOffice')) %>%
  SpatialPointsDataFrame(
    .[,1:2],
    data = .,
    proj4string = CRS()
  )

# - Extract raster values to each of these points

temp_sites <-
  map_dfr(
    c('dataSci_classroom', 'myOffice'),
    function(x) {
      raster::extract(
        maxTemp_stack,
        spPoints[spPoints$site == x,],
        df = TRUE
      ) %>%
        as_tibble() %>%
        gather(
          key = map_date,
          value = maxTemp_C,
          2:16) %>%
        mutate(map_date = dates) %>%
        rename(site = ID) %>%
        mutate(site = x)
    }
  )

# - Make a tidy frame with the data

# Already done!

# - Use ggPlot to create a scatterplot (it can include points or lines or both) of temperatures over the last two weeks at both sites (x-axis should be day and y-axis should be the maximum daily temperature).

temp_sites %>%
  ggplot(
    aes(x = map_date, y = maxTemp_C)
  ) +
  geom_line(aes(color = site), size = 1) +
  geom_point(aes(fill = site), pch = 21, size = 3) +
  theme_bw()

# Who's office is hotter? Joe's or Brian's? Dangit Joe, it's mine! Why?
  
