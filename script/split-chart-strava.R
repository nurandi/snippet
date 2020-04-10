
library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)
library(plotly)

# read laps data. This is a csv file downloaded from Garmin connect
laps <- read.csv("https://raw.githubusercontent.com/nurandi/snippet/master/data/laps-garmin.csv", stringsAsFactors = F)

# helper functions to reformat time variable
char_to_seconds <- function(i){
  paste0("00:00:",i) %>%
    str_remove("\\..+") %>%
    str_extract("\\d\\d?:\\d\\d?:\\d\\d?$") %>%
    hms %>%
    period_to_seconds}

seconds_to_char <- function(i){
  i %>%
    as_datetime %>%
    format("%H:%M:%S") %>%
    str_remove("^00?:0?")
}


# reverse axis trick
# https://gist.github.com/mikmart/bfbf62839fbdd162b4b88e6d43e0c858

c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inv, breaks, format = format)
}

rev_date <- c_trans("reverse", "time")

# prepare the data
laps <- laps %>%
  mutate_at(vars(matches("Time|Pace")), list(sec = ~char_to_seconds(.))) %>%
  mutate(Cumulative.Distance = cumsum(Distance),
         From.Distance = Cumulative.Distance - Distance,
         Pace_sec = Time_sec/Distance,
         Pace.Baseline_sec = max(Pace_sec) + 30,
         Pace = seconds_to_char(Pace_sec) )

# create ggplot/plotly chart
p <- ggplot(laps, aes(label=Distance, label1=Time, label2=Pace)) +
  geom_rect(
    aes(xmin=From.Distance, xmax=Cumulative.Distance,
        ymin=as_datetime(Pace_sec), ymax=as_datetime(Pace.Baseline_sec), 
        fill=Pace_sec), 
    colour="white") +
  scale_y_continuous(trans = rev_date, labels = date_format("%M:%S")) +
  xlab("Distance (km)") +
  ylab("Pace (minutes/km)") +
  labs(title = "Running Analysis: 18 x 1 minute 5k pace + 10 minutes tempo") +
  theme(legend.position='none')

ggplotly(p, tooltip = c("label", "label1", "label2"))
