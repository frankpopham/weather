library(tidyverse)
library(rvest)
library(gtsummary)

#read in html and make a list of the stations from the options drop down. 

html <-read_html("https://www.metoffice.gov.uk/research/climate/maps-and-data/historic-station-data")
html_el <- html %>%
  html_elements("option") %>%
  html_attrs() 

# removes the first option in drop down that isn't a station

html_el <- html_el[-1]

#adds station names to list 

names(html_el) <- html_el %>%
  map(~read_lines(.x, n_max=1))

#to a dataframe

overall <- html_el %>%
  map_dfr(~.x, .id="station") 

#then read in data to check format of header as varies by station (might be 5th , 6, 7 line)

df <- html_el %>%
  map_dfr(~read_table(.x, skip=5, n_max=1), .id="station") %>%
  left_join(overall) %>%
  mutate(funny=if_else(!is.na(Sunshine), 7, 6))

# then read in data and vary header line (there re warnings but OK to ignore as we sort below)

df <- map2_dfr(html_el, df$funny, ~read_table(.x, skip=.y, 
               col_types = cols(.default = col_character())), .id="station")

#data sort and tidy

df <- df %>%
  rename(year=X1, month=X2, tmaxdegC=degC, tmindegC=degC_1, afrostdays=days,
         rainmm = mm, sunhours=hours) %>%
  mutate(across(everything(), ~na_if(.x, "---" ))) %>%
  separate(year, c("year", "month2")) %>%
  mutate(across(-station, ~str_remove_all(.x, "#"))) %>%
  mutate(across(-station, ~str_remove_all(.x, "\\*"))) %>%
  mutate(across(-station, ~as.numeric(.x))) %>%
  mutate(month=coalesce(.$month, .$month2)) %>%
  select(-month2) %>%
  filter(!is.na(year))

#logic check
df %>%
  tbl_summary(statistic=list(all_continuous() ~ "{median} ({min}, {max})"),
            missing = "no")
  
  
# check for my local station

df %>%
  filter(station=="Leuchars") %>%
  select(-station) %>%
  tbl_summary(by=month,statistic=list(all_continuous() ~ "{median} ({min}, {max})"),
              missing = "no")
  
# save data

write_csv(df, "mo_weather_station.csv")
  