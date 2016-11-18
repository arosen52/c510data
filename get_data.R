library(tidyverse)
library(jsonlite)
library(RCurl)
library(lubridate)

sun.url <- 'http://api.usno.navy.mil/rstt/oneday?date='

tues.thur <- tibble(
  r.date = seq.Date(from = as.Date('2011-01-01'), 
                    to = as.Date('2016-12-31'), 
                    by = 1)
) %>% 
  mutate(dofw = lubridate::wday(r.date, label = T), 
         r.date = format(r.date, '%m/%d/%Y')) %>% 
  filter(dofw %in% c('Tues', 'Thurs')) 

sun.data <- 
  tues.thur %>% 
  # filter(r.date %in% c('01/11/2011', '01/20/2011') ) %>% 
  mutate(data = lapply(.$r.date, function (x) {
    fromJSON(getURL(paste0(sun.url, x, '&loc=Oakland,%20CA')))
  }))

c510 <- bind_cols(
    sun.data %>% 
      select(-data), 
    sun.data$data %>% 
    map_df('sundata') %>% 
    filter(phen == 'R') %>% 
    select(sun.t = time)
) %>% 
  mutate(sunrise = 
    paste(r.date, gsub('\\.', '', sun.t)))