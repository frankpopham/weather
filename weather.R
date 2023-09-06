library(tidyverse)
library(rvest)

#read in html and make a df from table 
#then read in first 10 lines of data to check format of header as varies by station 

html <-read_html("https://www.metoffice.gov.uk/research/climate/maps-and-data/historic-station-data")

df <- html %>%
  html_element("table") %>%
  html_table() %>%
  mutate(Link=html %>%
           html_element("table") %>%
           html_elements("a") %>%
           html_attr("href")) %>%
  mutate(tenline=map(Link, ~read_lines(.x, n_max=10))) %>%
  mutate(skippy=map(tenline, ~str_which(.x, "yyyy"))) %>%
  mutate(skippy=as.numeric(skippy)-1) %>%
  mutate(Data=map2(Link, skippy, ~read_table(.x, skip=.y, col_types = cols(.default = "c"),
                  col_names=c("year", "month", "tmaxdegC", "tmindegC", "afrostdays","rainmm", "sunhours"),
                  na="---"
))) %>%
  unnest(Data)

#tidy data

df <- df %>%
  filter(year!="yyyy") %>%
  filter(year!="degC") %>%
  filter(year!="Site") %>%
  select(-tenline, -skippy) %>%
  mutate(across(-Link, ~str_remove_all(.x, "#"))) %>%
  mutate(across(-Link, ~str_remove_all(.x, "\\*"))) %>%
  separate_wider_delim(Location, delim=",", names=c("longitude", "latitude")) %>%
  mutate(across(longitude:sunhours, ~as.numeric(.x)))

# lowercase names    
df <- rename_with(df, tolower)
  
# save

write_rds(df, "month_weather_station.RDS")
  