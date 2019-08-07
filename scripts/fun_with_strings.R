library(tidyverse)
library(stringr)

# Title I middle schools in Wayne County, MI

# read in data:

mi <- read_csv('miois.csv')

school_districts <-
  c(
    'Dearborn',
    'Allen Park',
    'Crestwood',
    'Dearborn',
    'Detroit',
    'Ecorse',
    'Flat Rock',
    'Garden City',
    'Gibraltar',
    'Grosse Ile',
    'Grosse Pointe',
    'Hamtramck',
    'Harper Woods',
    'Huron',
    'Inkster City',
    'Lincoln Park',
    'Livonia',
    'Melvindale',
    'Northville',
    'Plymouth Canton',
    'Redford Union',
    'River Rouge',
    'Riverview',
    'Romulus Community',
    'Royal Oak',
    'South Redford',
    'Southgate Community',
    'Taylor',
    'Trenton',
    'Van Buren',
    'Wayne RESA',
    'Wayne-Westland',
    'Westwood',
    'Woodhaven-Brownstown',
    'Wyandotte'
  )

# tabulate title 1 schools ------------------------------------------------

title1 <- 
  map_dfr(
  school_districts,
  function(x){
    filtered_frame <-
      mi %>% 
      filter(
        str_detect(`LEA Name`, x),
        str_detect(tolower(`School Name`), 'middle'),
        !str_detect(`Title I School`, 'Not a Title')
      ) %>%
      mutate(district = x)
    if(nrow(filtered_frame) > 0) {
      filtered_frame %>%
        group_by(district) %>%
        summarize(nSchools = n())
    } else {
      tibble(
        district = x,
        nSchools = 0)
    }
    })

title1

# tabulate title 1 schools without inner assignment -----------------------

title1 <- 
  map_dfr(
    school_districts,
    function(x){
        mi %>% 
        filter(
          str_detect(`LEA Name`, x),
          str_detect(tolower(`School Name`), 'middle'),
          !str_detect(`Title I School`, 'Not a Title')
        ) %>%
        mutate(district = x) %>%
        group_by(district) %>%
        {if(nrow(.) > 0) {
              group_by(., district) %>%
              summarize(nSchools = n())
          } else {
            tibble(
              district = x,
              nSchools = 0)
          }}
    })

# school count ------------------------------------------------------------

sum(title1$nSchools)


