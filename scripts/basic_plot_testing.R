# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(extrafont)
loadfonts(device = "win")
#source("https://raw.githubusercontent.com/rladies/starter-kit/master/rladiesggplot2theme.R")
r_ladies_theme <- function(){
  theme_bw() %+replace% 
    theme(text = element_text(family = "TT Arial", face = "plain",
                              colour = 'black', size = 10,
                              hjust = .5, vjust = .5, angle = 0, 
                              lineheight = 1.1, 
                              margin = margin(t = 0, r = 0, b = 0, l = 0, 
                                              unit = "pt"), 
                              debug= FALSE), 
          axis.text = element_text(colour = "#181818"), 
          axis.title = element_text(face = "bold", colour = "#88398A", size = rel(1.1)), 
          plot.title = element_text(face = "bold", size = rel(1.4), 
                                    colour = "#88398A", 
                                    margin = margin(t = 0, r = 0, b = 6.6,
                                                    l = 0, unit = "pt")),
          legend.title = element_text(face = "bold", colour = "#181818"),
          panel.grid.major = element_line(color = "#D3D3D3"))
}

# munging a bit ----
events <- read.csv("data/events-rladies-brisbane.csv")

int <- lubridate::interval(lubridate::date(events$created), lubridate::ymd(events$local_date))
days <- lubridate::time_length(int, "days")

event_dates <- paste0(lubridate::month(events$local_date, label = TRUE), "-",
       lubridate::year(events$local_date))

events$days <- days
events$event_dates <- event_dates
level_order <- c("Dec-2018", "Mar-2019", "May-2019", "Jul-2019", "Aug-2019")

## plot ----

p <- 
events %>%
  ggplot(aes(x = factor(event_dates, level = level_order) , y = days )) +
  geom_point(aes(size = 0.3, show.legend = NA)) +
  geom_text(label = days, nudge_y = 1) +
  labs(x = "Events' dates", 
       y = "Days from event creation to event",
       title = "How many days notice to events on meetup?") +
  r_ladies_theme()+
  theme(legend.position = "none") 
p

