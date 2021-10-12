library(rtweet)

# # Replace the stuff in quotes with your key, etc.!
# create_token(
#   app = "Epimath Lab Twitter Data",
#   consumer_key = "your consumer key",
#   consumer_secret = "your consumer secret",
#   access_token = "your token",
#   access_secret = "your secret")

source('createtoken.R')

tweetdata = search_tweets("#VaccinesWork", n = 1000, include_rts = FALSE) 

tweetdata %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #VaccinesWork Twitter statuses from past 6-9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Get follower userids
followers = get_followers("epimath", n = 1000) 
# Get info for these users
follower_info =  lookup_users(followers$user_id)


friends = get_friends("BarackObama", n = 1000) 
# There is a max---can set retryonratelimit = TRUE to overcome this 
# (but you may have to wait a while!)
friend_info =  lookup_users(friends$user_id)

timelines = get_timeline(c("cnn", "BBCWorld", "foxnews"), n = 10)
timelines = rbind(timelines,get_timeline(c("cnn"), n = 20, max_id = 1447582032742260739))

stream = stream_tweets(q = "", timeout = 10)
stream = stream_tweets(q = "NASA", timeout = 10)


USAtweets = stream_tweets(lookup_coords("usa"),timeout = 10)

AAtweets = search_tweets(geocode = "42.276894,-83.728278,10mi", n = 500)


library(maps)

# add lat/long data to tweets (not all tweets will have this)
AAtweetlocations = lat_lng(AAtweets) 

# plot
map("state", fill = TRUE, col = "#ffffff", 
    lwd = .25, mar = c(0, 0, 0, 0), 
    xlim = c(-87.2, -81.5), y = c(41.7, 45.8))
with(AAtweetlocations, points(lng, lat, pch = 20, col = "red"))


limits = rate_limit()

rate_limit(query = 'get_timeline')
