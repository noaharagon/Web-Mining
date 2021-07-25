#Introduction to Web Mining For Social Scientists
#Noah Julian Angara
#June 2021

# load all required packages
library(httr)       
library(rvest)      
library(dplyr)      
library(stringr)   
library(tidytext) 
library(vader)      
library(readr)
library(fBasics)
library(ggthemes)
library(ggplot2)
library(data.table)
library(stargazer)
library(RCurl)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(lmtest)
library(extrafont)

#set working directory
setwd("/Users/noahangara/Documents/Master's/8th Semester/Web Mining/Assignment")

#read in cleaned data
nyt_articles <- read_csv("nyt_articles.csv")
guardian_articles <- read_csv("guardian_articles.csv")
approval_rating <- read_csv("approval_rating.csv")

#plot articles per day for both NYT and Guardian
nyt_plot_df <- nyt_articles %>% count(Date)
guardian_plot_df <- guardian_articles %>% count(Date)

nyt_plot <- ggplot(nyt_plot_df) + geom_bar(aes(x = Date, y = n), stat = "identity", fill = "#30a2da") +
  theme_fivethirtyeight()
ggsave(nyt_plot, file = "nyt_plot.png", width = 14, height = 10, units = "cm")

guardian_plot <- ggplot(guardian_plot_df) + geom_bar(aes(x = Date, y = n), stat = "identity", fill = "#fc4f30") +
  theme_fivethirtyeight()
ggsave(guardian_plot, file = "guardian_plot.png", width = 14, height = 10, units = "cm")

rm(guardian_plot_df, nyt_plot_df, nyt_plot, guardian_plot)

# Sentiment Analysis ------------------------------------------------------

#merge articles into one df
article_master_df <- rbind(guardian_articles, nyt_articles)

#Create Word Cloud of Comments
word_cloud <- Corpus(VectorSource(article_master_df$Headline))
# Convert the text to lower case
word_cloud <- tm_map(word_cloud, content_transformer(tolower))
# Remove numbers
word_cloud <- tm_map(word_cloud, removeNumbers)
# Remove english common stopwords as well as custom ones
word_cloud <- tm_map(word_cloud, removeWords, stopwords("english"))
word_cloud <- tm_map(word_cloud, removeWords, t(stop_words))
word_cloud <- tm_map(word_cloud, removeWords, c("â€“","â€“", "â€", "â€“", "â€˜ "))
# Remove punctuation
word_cloud <- tm_map(word_cloud, removePunctuation)
# Eliminate extra white spaces
word_cloud <- tm_map(word_cloud, stripWhitespace)

#Add Elements to matrix and count number of words
dtm <- TermDocumentMatrix(word_cloud)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d[-c(2,6),]
#Generate Word Cloud
set.seed(71295) #for reproducibility
png("wordcloud.png", width = 14, height = 10, units = "cm", res = 1200)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(12, "Dark2"), scale = c(2,0.4), family = "Palatino", font = 1)
dev.off()


# Modelling Sentiment and Approval Ratings --------------------------------
article_master_df <- article_master_df %>%
  mutate(sentiment = vader_df(Headline)$compound)

article_master_df <- article_master_df %>%
  group_by(Date) %>% 
  summarise(sentiment = mean(sentiment, na.rm = T))

sentiment_plot <- ggplot(tail(article_master_df,131)) + geom_line(aes(x = Date, y = sentiment), color = "#009f29") + 
  geom_vline(aes(xintercept = as.Date("2021-02-27"), color = "#30a2da"), alpha = 0.5, size = 1.2)+
  geom_vline(aes(xintercept = as.Date("2021-03-11"), color = "#fc4f30"), alpha = 0.5, size = 1.2)+
  geom_vline(aes(xintercept = as.Date("2021-05-16"), color = "#ff7400"), alpha = 0.5, size = 1.2)+
  scale_color_manual(labels = c("House Passes American Rescue Plan Act 2021", 
                                "Biden Signs Stimulus into Law", 
                                "Israeli Air Raids on Gaza"), 
                     values = c("#30a2da", "#fc4f30", "#ff7400")) +
  labs(y = "Sentiment Score", x = "", color = "") + 
  theme_fivethirtyeight()
ggsave(sentiment_plot, file = "sentiment_plot.png", width = 20, height = 10, units = "cm")


# investigate information flow in approval ratings (indicative of non-stationarities)
acf(approval_rating$approve_estimate)
pacf(approval_rating$approve_estimate)

# investigate information flow in sentiment (appears to be stationary)
acf(article_master_df$sentiment)
pacf(article_master_df$sentiment)

# perform Ljung-Box to test for autocorrelations and Dickey-Fuller for Stationarity
Box.test(approval_rating$approve_estimate, type = "Ljung-Box")
Box.test(article_master_df$sentiment, type = "Ljung-Box")


# function to select order for Granger Causality (compute information criterion)
select_lag_order <- function(x, y, max.lag=8) {
  y <-as.numeric(y)
  y.lag <-embed(y,max.lag+1)[,-1,drop=FALSE]
  x.lag <-embed(x,max.lag+1)[,-1,drop=FALSE]
  
  t<-tail(seq_along(y),nrow(y.lag))
  
  #regression with lags ranging from 1 until the max lag (set to 8 here)
  ms=lapply(1:max.lag,function(i) lm(y[t]~y.lag[,1:i]+x.lag[,1:i]))
  #get pvalues
  pvals <- mapply(function(i) anova(ms[[i]],ms[[i-1]])[2,"Pr(>F)"],max.lag:2)
  ind <- which(pvals<0.05)[1]
  ftest<-ifelse(is.na(ind),1,max.lag-ind+1)
  
  AIC <- as.numeric(lapply(ms,AIC)) #Akaike Information Criterion
  BIC <- as.numeric(lapply(ms,BIC)) #Bayesian Information Criterion
  structure(list(ic=cbind(AIC=AIC, BIC=BIC),pvals=pvals,
                 selection=list(AIC=which.min(AIC),BIC=which.min(AIC),ftest=ftest)))#smallest information criterion
}

#run function to select order of lags
select_lag_order(diff(approval_rating$approve_estimate), tail(article_master_df$sentiment, 130))
select_lag_order(tail(article_master_df$sentiment, 130), diff(approval_rating$approve_estimate))

# granger causality test (both ways)
granger <- grangertest.default(tail(article_master_df$sentiment, 130), diff(approval_rating$approve_estimate),
                               order = 2)

granger_rev <- grangertest.default(diff(approval_rating$approve_estimate), tail(article_master_df$sentiment, 130),
                               order = 1)

#output results into latex format
stargazer(granger, summary = F, align = T)
stargazer(granger_rev, summary = F, align = T)






