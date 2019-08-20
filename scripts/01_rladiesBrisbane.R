## Load Libraries ----
library(meetupr)
library(lubridate)
library(tidyverse)

# you must change YOURMEETUPKEY ----
# follow the instructions https://github.com/rladies/meetupr
Sys.setenv(MEETUP_KEY = "YOURMEETUPKEY")
urlname <- "rladies-brisbane"
events <- get_events(urlname, c("past", "upcoming"))

events

# munging ----


# display some cols
events %>% 
  select(name, created, status, local_date, yes_rsvp_count)


# mutate
myevents <-
events %>% 
  mutate(created_date = lubridate::date(created),    
            day_event = lubridate::wday(local_date, label = TRUE),
         days_creation = lubridate::time_length(
           lubridate::interval(created_date, local_date), unit = "day"),
         ym_event = forcats::as_factor(paste0(lubridate::year(local_date), "-", 
                     lubridate::month(local_date, label = TRUE)))) %>% 
  select(id, created_date, status, local_date, local_time, yes_rsvp_count,
           day_event, days_creation, ym_event)


myevents

myevents %>% 
  ggplot((aes(x = ym_event, yes_rsvp_count))) +
  scale_y_continuous(limits = c(1, 25)) +
  geom_point(colour = "#88398A", size = 5) +
  geom_step((aes(x = 1:5, yes_rsvp_count)),
               color = "#88398A", linetype = 2) +
  geom_text(nudge_y = 1.5, aes(label = paste(day_event,local_time))) +
  geom_text(nudge_x = -0.2, aes(label = yes_rsvp_count), 
            size = 6) +
  labs(x = "Events by month", y = "Meetup: RSVP",
          title = "Our Meetups are growing") +
  r_ladies_theme() +
  theme(text = element_text(size = 12))


## members ----        
#memb <- get_members(urlname)
# not really the joined date we are looking for
# https://github.com/rladies/meetupr/issues/20
# http://rpubs.com/rladiespdx/meetup-member-dates
# Get the data manually
# http://api.meetup.com/rladies-brisbane/members
# save as json
# convert to csv

library(plyr)
library(RJSONIO)
con <- file("data/members-rladies-brisbane.json", "r")
memb  <- ldply(fromJSON(con), data.frame)
close(con)


members_cleaned <-
memb %>% 
  as_tibble() %>% 
  select(-c(state, country, ends_with("link"), lat, lon, 
            localized_country_name, bio, 
            starts_with("group_profile.group."),
            starts_with("photo"))) %>% 
  mutate(date_joined = lubridate::floor_date(as.POSIXct(
    group_profile.created/1000, tz="UTC", origin="1970-01-01"))) %>% 
  mutate(day_joined = lubridate::wday(date_joined, label = TRUE),
         d_joined = lubridate::day(date_joined),
         month_joined = lubridate::month(date_joined, label = TRUE),
         m_joined = lubridate::month(date_joined),
         y_joined = lubridate::year(date_joined))  %>% 
  mutate(date_joined_ymd = paste0(y_joined,"-",
                                  as.character(m_joined), "-",
                                  as.character(d_joined)))

members_cleaned

str(members_cleaned)



members_cleaned %>% 
  ggplot(aes(x = day_joined)) +
    geom_bar() +
  facet_wrap(~ month_joined) +
  labs(x = "Days of the week", y = "Number of members joined",
       title = "When did our members joined?",
       caption = "First meetup December 2018") +
  r_ladies_theme()


members_cleaned %>%
  ggplot(aes(x = day_joined)) +
  geom_bar(fill = "darkorchid4") +
  labs(x = "Days of the week", y = "Number of members",
       title = "When did our members joined RLadies Brisbane Meetup?",
       caption = "Started May 2018") +
  facet_wrap(~month_joined) +
  r_ladies_theme()



