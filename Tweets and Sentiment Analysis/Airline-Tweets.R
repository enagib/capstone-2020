library(tidyr)
library(tidytext)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rtweet)
library(stringr)
library(quanteda)
library(wordcloud)
library(rtweet)
library(topicmodels)

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

##########SENTIMENT ANALYSIS###############
alaska_text <- alaska_tweets %>% select(status_id, text)
tidy_alaska <- alaska_text %>% unnest_tokens(token, text, token = "tweets", strip_url = T, strip_punct = T, to_lower = T)
tidy_alaska_sw <- tidy_alaska %>% anti_join(get_stopwords(), by=c("token" = "word"))

sw = get_stopwords()
sw = c(sw$word, "@alaskaair", "alaskaair")

tidy_alaska2 = tidy_alaska_sw %>% filter(!token %in% sw)
tidy_alaska2 %>% count(token, sort=T) %>% top_n(15) %>% 
  ggplot(aes(reorder(token, n), n)) +
  geom_col() +
  coord_flip()

alaska_bing <- inner_join(tidy_alaska2, get_sentiments("bing"), by=c("token" = "word"))
head(alaska_bing)

# negative versus positive sentiment words and their frequency. 
alaska_positive = alaska_bing %>% filter(sentiment == "positive") %>% 
  count(token, sort=T) %>% top_n(10) %>% 
  ggplot(aes(reorder(token, n), n)) +
  geom_col() +
  xlab("Top 10 Words") +
  ggtitle("Top Positive Words") +
  coord_flip() 

alaska_negative = alaska_bing %>% filter(sentiment == "negative") %>% 
  count(token, sort=T) %>% top_n(10) %>% 
  ggplot(aes(reorder(token, n), n)) +
  geom_col() +
  xlab("Top 10 Words") +
  ggtitle("Top Negative Words") +
  coord_flip()

gridExtra::grid.arrange(alaska_positive, alaska_negative , nrow = 1)

## Topic modelling
alaska_corpus <- corpus(alaska_text$text)
alaska_dfm = dfm(alaska_corpus,
              remove_punct = T,
              remove = stopwords(),
              remove_numbers = T,
              remove_url = T,
              remove_symbols = T) %>% 
  dfm_trim(min_termfreq = 10, 
           termfreq_type = "count",
           max_docfreq = .65,
           docfreq_type = "prop")

dtm = convert(alaska_dfm, to = "topicmodels")

crm_topics = LDA(dtm, 5, control = list(seed=820))
crm_topics
terms(crm_topics, 10)

crm_beta = tidy(crm_topics, matrix="beta")

crm_top10 = crm_beta %>% 
group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(desc(beta)) 

crm_top10 %>% 
  mutate(term = reorder_within(term, beta, topic))  %>% 
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales="free") +
  coord_flip() +
  scale_x_reordered()

### gamma - mixtures of topics across documents
crm_gamma = tidy(crm_topics, matrix="gamma")
crm_gamma %>% arrange(-gamma) %>% print(n=10)


## top tokens
textstat_frequency(alaska_dfm, n = 10)
textplot_wordcloud(alaska_dfm, min_count = 30)


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



