#### Import Libraries ####
library(tuber)
library(plyr)   # dataframe manipulation
library(dplyr)  # dataframe manipulation
library(stringr) # string operations
library(ggplot2) # plots
library(wordcloud)
library(RCurl)  # sending http request in R
library(syuzhet) # sentiment analysis in R
library(tm)   # text manipulation


app_id <- "xxxx-xxxx.apps.googleusercontent.com"
app_secret <- "xxxx"

yt_oauth(app_id = app_id, app_secret = app_secret, token="")

##### Sentiment analysis & wordcloud of entire channel ##########


channel_videos = list_channel_videos(channel_id = "UC8MbRQQdYhNwOFeXmpK5UBw", max_results = 500)

channel_videos

write.csv(channel_videos, "list_channel_videos.csv")
View(channel_videos)
video_ids = channel_videos$contentDetails.videoId
video_ids


comments <- c()

#get comments of all videos
for(video in video_ids){
  print(video)
  res <- get_comment_threads(c(video_id=video))
  x <- as.vector(res$textOriginal)
  comments <- c(comments, x)
}

length(comments)
comments.df <- data.frame(comments)
write.csv(comments.df, "comments_text.csv")


# get the text
some_txt_A = sapply(comments, function(x) x)
View(some_txt_A)

# Cleaning 1- remove html links
some_txt_A2 = gsub("http[^[:blank:]]+", "", some_txt_A)

# Cleaning 2- remove people names

some_txt_A3 = gsub("@\\w+", "", some_txt_A2)

# Cleaning 3- remove Punctuations 

some_txt_A4 = gsub("[[:punct:]]", " ", some_txt_A3)

# Cleaning 4- remove alphanumeric words

some_txt_A5 = gsub("[^[:alnum:]]", " ", some_txt_A4)

#View(some_txt_A5)

some_txt_A6 <- Corpus(VectorSource(some_txt_A5))
some_txt_A6 <- tm_map(some_txt_A6, content_transformer(tolower))
some_txt_A6 <- tm_map(some_txt_A6, removeWords, stopwords("english"))
some_txt_A6 <- tm_map(some_txt_A6, stripWhitespace)

# Building wordcloud

pal <- brewer.pal(12,"Paired")
wordcloud(some_txt_A6, min.freq = 5,  max.words = Inf, width=1000, height =1000,  random.order = FALSE, color=pal)

# Sentiment analysis

mysentiment_A <- get_nrc_sentiment(some_txt_A5)
SentimentScores_A <- data.frame(colSums(mysentiment_A[,]))
names(SentimentScores_A) <- "Score"
SentimentScores_A <- cbind("sentiment" = rownames(SentimentScores_A), SentimentScores_A)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores_A, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score for most disliked videos")


########### Word cloud for most liked video ##########

channel_stat = get_all_channel_video_stats(channel_id = "UC8MbRQQdYhNwOFeXmpK5UBw")
View(channel_stat)
write.csv(channel_stat, "allChannelVideoStats.csv")

channel_stat.df <- data.frame(channel_stat)

# By viewing the channel_stat, observe the data and get the IDs of the videos with 
# maximum like counts and dislike counts

liked_videos <- c("5CtbYRLZVRc", "9brj_lYjRw0", "OHGrNiaQjbo","y1KCooT1tSU","z5Db23Jl7qc")
disliked_videos <- c("5CtbYRLZVRc", "9brj_lYjRw0", "9YzZjxCTBlQ","y1KCooT1tSU","De4QxTsiJy8")

comments <- c()

#get comments of all disliked videos
for(video in disliked_videos){
  print(video)
  res <- get_comment_threads(c(video_id=video))
  x <- as.vector(res$textOriginal)
  comments <- c(comments, x)
}

length(comments)





####### Get top 10 active users ############
channel_videos = list_channel_videos(channel_id = "UC8MbRQQdYhNwOFeXmpK5UBw")
View(channel_videos)
video_ids1 = channel_videos$contentDetails.videoId


res <- get_comment_threads(c(video_id="N708P-A45D0"))
userComments <- data.frame(matrix(ncol = 12, nrow = 0))
columnnames = names(res[,1:12])
colnames(userComments) <- columnnames

length(video_ids1)
valid = c()
invalid = c()
for(video in video_ids1){
  tryCatch(
    {
      res <- get_comment_threads(c(video_id=video))
      valid = c(valid,video)
      if(nrow(res)!=0){
        df <- res[c("videoId","textDisplay","textOriginal","authorDisplayName",
                    "authorProfileImageUrl","authorChannelUrl","authorChannelId.value",
                    "canRate","viewerRating","likeCount","publishedAt","updatedAt")]
        #df <- res[,1:12]
        userComments <- rbind(userComments, df)
      }
      
    },
    error = function(err){
      print(video)
      invalid = c(invalid,video)
      
    }
  )
  
}


View(userComments)
write.csv(userComments, "active_users.csv")
