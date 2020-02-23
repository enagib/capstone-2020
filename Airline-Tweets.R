library(tidyr)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rtweet)

## install rtweet from CRAN
install.packages("rtweet")
library(rtweet)

# get_timeline() - Get one or more user timelines (tweets posted by target user(s))
# get_favorites() - Get tweets data for statuses favorited by one or more target users.

## quick overview of rtweet functions
    #vignette("auth", package = "rtweet")
## preview users data
    #users_data(alaska_tweets)
## plot time series of tweets frequency
    #ts_plot(alaska_tweets, by = "hours")


##GETTING TWEETS
alaska_tweets <- search_tweets(q = "@AlaskaAir", n = 1000, lang = "en")
frontier_tweets <- search_tweets(q = "@FlyFrontier", n = 1000, lang = "en")
virgin_tweets <- search_tweets(q = "@VirginAmerica", n = 1000, lang = "en")
southwest_tweets <- search_tweets(q = "@Southwestair", n = 1000, lang = "en")
delta_tweets <- search_tweets(q = "@Delta", n = 1000, lang = "en")
american_tweets <- search_tweets(q =  "@Americanair", n = 1000, lang = "en")
jetblue_tweets <- search_tweets(q = "@Jetblue", n = 1000, lang = "en")



########################################################################
## Alaska Airlines
########################################################################

## plot time series of tweets
alaska_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of @AlaskaAir Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")


## PLOTTING TWEETS ON MAP
#create lat/lng variables using all available tweet and profile geo-location data
alaska_rt <- lat_lng(alaska_tweets)
#plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot lat and lng points onto state map
with(alaska_rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))




########################################################################
## Virgin Airlines
########################################################################

## plot time series of tweets
virgin_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of @VirginAmerica Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## PLOTTING TWEETS ON MAP
#create lat/lng variables using all available tweet and profile geo-location data
frontier_rt <- lat_lng(virgin_tweets)
#plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot lat and lng points onto state map
with(frontier_rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))




########################################################################
## Frontier Airlines
########################################################################

## plot time series of tweets
frontier_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of @FlyFrontier Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## PLOTTING TWEETS ON MAP
#create lat/lng variables using all available tweet and profile geo-location data
frontier_rt <- lat_lng(frontier_tweets)
#plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot lat and lng points onto state map
with(frontier_rt , points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))




########################################################################
## Southwest Airlines
########################################################################

## plot time series of tweets
southwest_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of @Southwestair Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## PLOTTING TWEETS ON MAP
#create lat/lng variables using all available tweet and profile geo-location data
southwest_rt <- lat_lng(southwest_tweets)
#plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot lat and lng points onto state map
with(southwest_rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))




########################################################################
## Delta Airlines
########################################################################

## plot time series of tweets
delta_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of @DeltaAssist Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## PLOTTING TWEETS ON MAP
#create lat/lng variables using all available tweet and profile geo-location data
delta_rt <- lat_lng(delta_tweets)
#plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot lat and lng points onto state map
with(delta_rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))




########################################################################
## American Airlines
########################################################################

## plot time series of tweets
american_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of @Americanair Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## PLOTTING TWEETS ON MAP
#create lat/lng variables using all available tweet and profile geo-location data
american_rt <- lat_lng(american_tweets)
#plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot lat and lng points onto state map
with(american_rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))




########################################################################
## Jetblue Airlines
########################################################################

## plot time series of tweets
jetblue_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of @Jetblue Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")

## PLOTTING TWEETS ON MAP
#create lat/lng variables using all available tweet and profile geo-location data
jetblue_rt <- lat_lng(jetblue_tweets)
#plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot lat and lng points onto state map
with(jetblue_rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))



