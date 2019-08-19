library(httr)

library(jpeg)

library(rinat)

library(robis)

library(tidyverse)

# Introduction to getting data from an api --------------------------------

# submit a request to an api:

zooUrl <-
  'http://www.omdbapi.com/?apikey=6fa7876b&t=Zoolander'

api_data <- 
  httr::GET(zooUrl)

# what did that return?

api_data

names(api_data)

api_data$status_code

# extract the content:

httr::content(api_data, as = 'raw')

httr::content(api_data, as = 'text')

httr::content(api_data, as = 'parsed') 

api_content <-
  httr::content(api_data, as = 'parsed') 

# let's look at the content:

str(api_content)

names(api_content)

api_content[['Plot']]

# We can create a tibble of api content that we want to look at:

tibble(
  title = api_content[['Title']],
  year = api_content[['Year']],
  plot = api_content[['Plot']],
  poster = api_content[['Poster']]
)

# Because these items are objects in a list, this is a great use of map!

purrr::map_dfc(
  c('Title', 'Year', 'Plot', 'Poster'),
  function(x){
    tibble(
      api_content[[x]]) %>%
      set_names(x)
  }
)

# string together multiple api calls --------------------------------------

movies <-
  c('Zoolander',
    'Harold+and+Maude',
    'The+Life+Aquatic',
    'Idiocracy',
    'The+Royal+Tenenbaums')

api <- 'http://www.omdbapi.com/?apikey=6fa7876b&t='

# Write a function that will paste a url with a movie title:

query_movie <-
  function(api, movie_title){
    paste0(api, movie_title)
  }

# Use purrr::map_dfr to query the database by the movies vector:

movie_frame <-
  purrr::map_dfr(
  movies,
  function(x){
    api_content <-
      # construct request:
      query_movie(api, x) %>%
      # make request to api: 
      httr::GET() %>%
      # get content:
      httr::content()
    tibble(
      title = api_content$Title,
      year = api_content$Year,
      plot = api_content$Plot,
      poster = api_content$Poster
    )
  }
)

# what did we get?

movie_frame

View(movie_frame)

# let's look at a poster! -------------------------------------------------

# make temp file:

img_temp <- 
  tempfile()

# save image to temp file:

download.file(
  movie_frame %>%
    filter(title == 'Zoolander') %>%
    pull(poster),
  img_temp)

img_jpg <- jpeg::readJPEG(img_temp)

# read as jpeg:

rasterImage(img_jpg, 0, 0, 1,1)

# we can simplify the above, and avoid assignment, with a function:

get_webImage <- 
  function(image_url){
    # make temp file:
    img_temp <- 
      tempfile()
    
    # save image to temp file:
    download.file(image_url,img_temp)
    
    # read as jpeg:
    jpeg::readJPEG(img_temp)
  }

# run function:

movie_frame %>%
  filter(title == 'The Royal Tenenbaums') %>%
  pull(poster) %>%
  get_webImage() %>%
  rasterImage(0, 0, 1,1)

# iNaturalist --------------------------------------------------------------

# Observations recorded at Susquehanna in 2018:

susquehanna <-
  get_inat_obs(
    query = 'susquehanna',
    year = 2018,
    maxresults = 200) %>%
  as_tibble()

# What are the names of the resultant tibble?

# Subset susquehanna to the columns common_name and scientific_name then the observations that are distinct:

# Subset susquehanna to the columns common_name and scientific_name then the observations that contain the word "Moth" (common_name):

# Observations of cowbirds in 2018:

cowbirds <- 
  get_inat_obs(
    taxon_name = "Molothrus ater",
    year = 2018,
    quality = 'research',
    geo = TRUE,
    maxresults = 1000) %>%
  as_tibble()

# Subset cowbirds to observations in which place_guess includes the word "Illinois" then select the columns common_name, scientific_name and user_login. Mark, do you know any of those observers?

# Migratory dragonflies:

gDarner <-
  get_inat_obs(
    taxon_name = "Anax junius",
    year = 2018,
    month = 9,
    geo = TRUE,
    bounds = c(24, -125, 50, -66)
  )

# Let's build a list of Green darner observations by month!

gDarner_monthList <-
  vector('list', length = 12)

for(i in 1:12){
  gDarner_monthList[[i]] <-
    get_inat_obs(
      taxon_name = "Anax junius",
      year = 2018,
      month = i,
      geo = TRUE,
      bounds = c(24, -125, 50, -66)
    )
}

# We can plot a single map of observations as follows:

inat_map(gDarner_monthList[[1]], plot = FALSE) + 
  borders("state") +
  theme_bw() +
  xlim(-125, -65) +
  ylim(25, 50)

# Let's use the purrr::map function to watch Green darner migration!

purrr::map(
  1:12,
  function(i){
    inat_map(gDarner_monthList[[i]], plot = FALSE) + 
      borders("state") +
      theme_bw() +
      xlim(-125, -65) +
      ylim(25, 50) +
      labs(title = month.name[i])
  }
)

# OBIS --------------------------------------------------------------------

# Ocean Biogeographic Information System!

# Let's observe the sperm whales from 2018 and 2019:

sWhale <- 
  occurrence(
    scientificname = 'Physeter macrocephalus',
    startdate = '2018-01-01',
    enddate = '2019-08-07')

map_leaflet(
  sWhale,
  provider_tiles = 'OpenStreetMap')

