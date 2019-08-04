library(httr)
library(jpeg)
library(tidyverse)

# get data from an api ----------------------------------------------------

# submit a request to an api:

api_data <- 
  httr::GET('http://www.omdbapi.com/?apikey=6fa7876b&t=Zoolander')

# what did that return?

api_data

names(api_data)

api_data$status_code

# extract the content:

httr::content(api_data, as = 'raw')

httr::content(api_data, as = 'text')

httr::content(api_data) 

httr::content(api_data) %>%
  str()

httr::content(api_data) %>%
  names()

httr::content(api_data)$Plot

# string together multiple api calls --------------------------------------

movies <-
  c('Zoolander',
    'Harold+and+Maude',
    'The+Life+Aquatic',
    'Idiocracy')

api <- 'http://www.omdbapi.com/?apikey=6fa7876b&t='

movie_frame <-
  purrr::map_dfr(
  movies,
  function(x){
    api_content <-
      # construct request:
      paste0(api, x) %>%
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
  filter(title == 'Idiocracy') %>%
  pull(poster) %>%
  get_webImage() %>%
  rasterImage(0, 0, 1,1)
