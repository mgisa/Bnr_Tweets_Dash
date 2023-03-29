#ANALYSIS OF BNR TWEETS SINCE 2016-Today
#Source:https://medium.com/datadriveninvestor/an-analysis-of-muhammadu-buharis-tweet-36e92c22ef6d
#______________________________________
library(purrr)
library(rtweet)
library(lubridate)
library(tidyverse)
library(tidytext)
library(zoo)
library(xts)

#setting wd
setwd("C:/Users/jmurera/Desktop/BNR_Projects/BNR_Tweets")
#Get as many tweets as possible

df <- get_timeline("@CentralBankRw",n=14200) #Write to csv
save_as_csv(df,"BNR_Tweets2017_May2022.csv" )

dim(df)
View(df)
#Write the data to csv
df %>% write_as_csv("C:/Users/jmurera/Desktop/BNR_Projects/BNR_Tweets/BNR_Tweets2018_Feb2021.csv") 
#The result of the query got about 3211 rows, 90 columns.
#______________________
#
#Lets take a look at his tweet trend over the years
df<-read_csv("C:/Users/jmurera/Desktop/BNR_Projects/BNR_Tweets/BNR_Tweets2016_Oct2021.csv")

p<-df %>%
  separate(created_at, into = c("date", "time"), sep = " ") %>%
  mutate(date =  as.POSIXct(date, format = "%Y-%m-%d")) %>% 
  mutate(year = year(date)) %>% 
  mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
  mutate(yearmon = as.yearmon(yearmon)) %>% 
  group_by(yearmon) %>% 
  count() %>% 
  ggplot(., aes(yearmon, n, group = 1)) +
  geom_line(size = 1, color = "#00ba38") +
  geom_point(color = "#f8766d") +
  geom_smooth(method = "lm", se = F) +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Humanitarian \ Crisis",x = as.yearmon("Dec 2016"), 
  #y= 154, color = "black", size =3) +
  annotate(geom = "label", vjust = 0.1, 
           label="Covid'19",x = as.yearmon("Apr 2020"), 
           y= 67, color = "red", size =5) +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Corruption",x = as.yearmon("Sep 2018"), 
  #y= 163, color = "black", size =3)  +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Football/Corruption/Tragedy",x = as.yearmon("Oct 2019"), 
  #y= 157, color = "black", size =3) +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Carfew 8PM",x = as.yearmon("Dec 2020"), 
  #y= 60, color = "black", size =5) +
  theme_bw()  +
  scale_x_yearmon(format = "%b %Y", n = 10) +
  labs(x = "Date", y = "Number of Tweets", caption = "BNR_mgisa") +
  theme(axis.title.x = element_text(face ="bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20))

ggplotly(p)
#____________________________
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>%
  mutate(date =  as.POSIXct(date, format = "%Y-%m-%d")) %>% 
  mutate(year = year(date)) %>% 
  mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
  mutate(yearmon = as.yearmon(yearmon)) %>% 
  group_by(yearmon) %>% 
  count() %>% 
  ggplot(., aes(yearmon, n, group = 1)) +
  geom_line(size = 1, color = "#00ba38") +
  geom_point(color = "#f8766d") +
  geom_smooth(method = "lm", se = F) +
  #annotate(geom = "label", vjust = -0.1, 
           #label="Humanitarian \ Crisis",x = as.yearmon("Dec 2016"), 
           #y= 154, color = "black", size =3) +
 annotate(geom = "label", vjust = 0.1, 
           label="Covid'19",x = as.yearmon("Apr 2020"), 
           y= 67, color = "red", size =5) +
  #annotate(geom = "label", vjust = -0.1, 
           #label="Corruption",x = as.yearmon("Sep 2018"), 
           #y= 163, color = "black", size =3)  +
  #annotate(geom = "label", vjust = -0.1, 
           #label="Football/Corruption/Tragedy",x = as.yearmon("Oct 2019"), 
           #y= 157, color = "black", size =3) +
  #annotate(geom = "label", vjust = -0.1, 
           #label="Carfew 8PM",x = as.yearmon("Dec 2020"), 
           #y= 60, color = "black", size =5) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 20,face = "bold",size = 12),
        axis.text.y = element_text(angle = 360,face = "bold",size=12)) +
  scale_x_yearmon(format = "%b %Y", n = 10) +
  labs(title = "National Bank of Rwanda's Tweets Trend Visualization",
       subtitle = "Oct2016 - May2022",caption = "Source:@CentralBankRw",
       x="Time ",y="Number of Tweets")
#________________________________________________
#See the top 10 words used in this tweets.
pal1<-RColorBrewer::brewer.pal(n=10,name="Set3")
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  mutate(text = gsub("amp", "", text)) %>% 
  mutate(text = gsub("â", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  arrange(desc(n)) %>% 
  top_n(10)%>% 
  mutate(Rate=round((n/sum(n))*100,digits = 2))%>%
  ggplot(., aes(reorder(word, n), n)) +
  geom_bar(stat = "identity",aes(fill=word)) +
  scale_fill_manual(values = pal1,name="Top10 used words")+theme_bw()+
  coord_flip() + geom_text(aes(label=str_c(Rate,"%")),vjust=0.1,angle=360,size=5,color="black")+
  labs(x = "Most Used Words",y="Appearence Frequency",caption = "@CentralBankRw")+
  theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
        axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
        axis.title.x = element_text(size = 15, angle = 360,vjust = 5, face = "bold",colour = "black"),
        axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black"),
        legend.position = "none")
  
#________________________________________________
#See which words were dominant across each year contained in the tweets.
#Most used words for each year
df %>% separate(created_at, into = c("date", "time"), sep = " ") %>%
  mutate(date =  as.POSIXct(date, format = "%Y-%m-%d")) %>% 
  mutate(year = year(date)) %>% 
  mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
  mutate(yearmon = as.yearmon(yearmon)) %>% 
  group_by(yearmon) %>% 
  count() %>% 
  ggplot(., aes(yearmon, n, group = 1)) +
  geom_line(size = 1, color = "#00ba38") +
  geom_point(color = "#f8766d") +
  geom_smooth(method = "lm", se = F) +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Humanitarian \ Crisis",x = as.yearmon("Dec 2016"), 
  #y= 154, color = "black", size =3) +
  annotate(geom = "label", vjust = 0.1, 
           label="Covid'19",x = as.yearmon("Apr 2020"), 
           y= 67, color = "red", size =5) +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Corruption",x = as.yearmon("Sep 2018"), 
  #y= 163, color = "black", size =3)  +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Football/Corruption/Tragedy",x = as.yearmon("Oct 2019"), 
  #y= 157, color = "black", size =3) +
  #annotate(geom = "label", vjust = -0.1, 
  #label="Carfew 8PM",x = as.yearmon("Dec 2020"), 
  #y= 60, color = "black", size =5) +
  theme_bw()  +
  scale_x_yearmon(format = "%b %Y", n = 10) +
  labs(x = "Date", y = "Number of Tweets", caption = "Source:@CentralBankRw") +
  theme(axis.title.x = element_text(face ="bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20))
  #_________________________

  df%>%separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(date =  as.POSIXct(date, format = "%Y-%m-%d"))%>% 
  mutate(year = year(date)) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  mutate(text = gsub("amp", "", text)) %>% 
  mutate(text = gsub("?", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  group_by(year) %>% 
  count(word, sort = T) %>% 
  arrange(desc(n)) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice(1:10) %>% 
  ungroup()%>%
  mutate(Rate=round((n/sum(n))*100,digits = 2))%>%
  ggplot(., aes(reorder_within (word, n, year), n, fill = as.character(year))) +
  geom_col(show.legend = F) +
  coord_flip() +geom_text(aes(label=str_c(Rate,"%")),vjust=0.1,angle=360,size=3,color="black")+
  facet_wrap(~year, ncol =  2, scales = "free") + theme_bw() +
  labs(x = "Most Dominants Used Words", y="Appearence Frequency",caption = "Source:@CentralBankRw")+
  theme(axis.text.x = element_text(angle = 360,face ="bold", size = 10,colour = "black"),
        axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
        axis.title.x = element_text(size = 15, angle = 360,vjust = 5, face = "bold",colour = "black"),
        axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black"),
        legend.position = "none")

#____________________________________________________
#An hourly count of the tweets faceted for each year reveals an interesting fact. 
#You should expect the BNR will be tweeting at later hours of the day.
#Typically during morning times
#Hourly tweet Trend
df %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(time = hms(time)) %>% 
  mutate(hour = lubridate::hour(time)) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = year(date)) %>% 
  group_by(hour, year) %>%
  count()%>% 
  ggplot(., aes(factor(hour), n, fill = as.character(year))) +
  geom_col(show.legend = F) +
  facet_wrap(~year, ncol = 2, scales = "free") +theme_bw()+
  labs(x = "Hour of Day", y="Total Number of tweets",caption = "Source:@CentralBankRw")
#____________________________________________
#               Sentiment Analysis
#combining the future data with the current one data
df_2016<-read_csv("BNR_Tweets2016_Oct2021.csv") 
df_2022<- read_csv("BNR_Tweets2017_May2022.csv")

df_2016<-df_2016%>% separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(date =  as.POSIXct(date, format = "%m/%d/%Y")) %>% 
  select(date,time,Username=screen_name,text,source,
         reply_to_Username=reply_to_screen_name,is_retweet,favorite_count,
         retweet_count,reply_count,hashtags)

df_2022<-df_2022 %>% separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(date =  as.POSIXct(date, format = "%Y-%m-%d"))%>% 
  select(date,time,Username=screen_name,text,source,
         reply_to_Username=reply_to_screen_name,is_retweet,favorite_count,
         retweet_count,reply_count,hashtags)
#Joining and supress message for joining by=*
suppressWarnings(suppressMessages(df_all<- full_join(df_2016,df_2022)))
# examine the top words either used positively or negatively.
#Sentiments 
df_all %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("https", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ggplot(., aes(reorder(word, n), n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  coord_flip() +
  labs(x =  "Words")
#________________________________________
#A more interesting approach would be to see the pattern of positivity (or negativity) over time. 
#I will make it overall (2016-2022) and as well for each year.
#Yearly trend of sentiments
#dataInput <- df
data_format <- df_all %>% 
  mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(row_id = row_number()) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("brt", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment, row_id, Username) %>% 
  spread(sentiment, n, fill = 0) %>%  # made data wide rather than narrow
  mutate(sentiment = positive - negative)

All_banks <- df_all%>%
  mutate(row_id = row_number())

data_format <- data_format %>% 
  left_join(All_banks, by = "row_id")

#convert negative column to negative values
data_format2<- data_format %>% 
  mutate(negative = negative * (-1))

dfp<-data_format2%>% 
  #separate(created_at, into = c("date", "time"), sep = " ") %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>% 
  mutate(year = year(date)) %>% 
  mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
  mutate(yearmon = as.yearmon(yearmon)) %>% 
  group_by(yearmon) %>% 
  summarise(negative = sum(negative), positive = sum(positive)) %>% 
  gather(key = "sentiment", value = "n", 2:3) #%>% 

 p<- ggplot(dfp, aes(yearmon, n, group = sentiment, color = sentiment)) +
  geom_line(size = 0.8) +theme_bw() +
  scale_x_yearmon(format = "%b %Y", n = 10) +
  labs(title="Sentiment Evolution behind BNR Tweets",x = "Date",y="Tweet Counts",caption = "Source:@CentralBankRw")+
  theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
        axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
        axis.title.x = element_text(size = 15, angle = 360,vjust = 5, face = "bold",colour = "black"),
        axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black"),
        legend.position = c(.1, .8),
        legend.title = element_text(colour="blue", size=10, 
                                    face="bold"),
        legend.text = element_text(colour="blue", size=10, 
                                   face="bold"))
 
p
 library(plotly)
ggplotly(p)
#________________________NRC SENTIMENTS(FEELINGS&EXPRESSIONS)______________
nrc <- df_all %>% 
  mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
  mutate(text = tolower(text))

nrc1 <- nrc$text
#class(nrc)
nrcsent<-get_nrc_sentiment(as.character(nrc1))
nrcsent<-as.data.frame(colSums(nrcsent))

sentiments<-c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise",
              "Trust","Negative","Positive")

sentiments<-as.data.frame(sentiments)

dfp<-as.data.frame(nrcsent[,1])

PDF<-cbind( sentiments,dfp)
colnames(PDF)[2]<-paste("Frequency")

NRC<-PDF%>%arrange(desc(Frequency))
#PLotting
pal<-brewer.pal(n=10,name = "Set3")
NRC%>%mutate(sentiments,
             Rate=round((Frequency/sum(Frequency))*100,digits = 2))%>%
  ggplot(aes(reorder(sentiments,-Frequency),y=Frequency))+ 
  geom_bar(stat = "identity", show.legend = F, aes(fill=sentiments)) + #coord_flip() +
  theme_bw() + geom_text(aes(label=str_c(Rate,"%")),vjust=0.1,angle=360,size=4,color="black")+
  labs(x = "Sentiments", y = "Total word Count",caption = "Source:@CentralBankRw(2016-2022)") +
  scale_fill_manual(values = pal,name="Legend")+
  theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
        axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
        axis.title.x = element_text(size = 15, angle = 90,vjust = 5, face = "bold",colour = "black"),
        axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black")
  )

#______________________Sentiments____________

x <- df_all
x$text <- enc2native(x$text)

#PROCESSING TWEETS TEXT/TEXT WRANGLING
#___________________________________________

# Extract URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA
                                -F][0-9a-fA-F]))+"
x$contentURL <- str_extract(x$text, url_pattern)

# Clean content of text
x$text <- gsub("^[[:space:]]*","",x$text)          #Remove leading whitespaces.
x$text <- gsub("[[:space:]]*$","",x$text)          #Remove trailing whitespaces.
x$text <- gsub(" +"," ",x$text)                    #Remove extra whitespaces.
x$text <- gsub("'", "%%", x$text)                  #Replacing apostrophes with %%.
x$text <- iconv(x$text, "latin1", "ASCII", sub="") #Remove emojis/dodgy unicode.
x$text <- gsub("<(.*)>", "", x$text)               #Remove pesky Unicodes like <U+A>.
x$text <- gsub("\\ \\. ", " ", x$text)             #Replace orphaned fullstops with space.
x$text <- gsub(" ", " ", x$text)                   #Replace double space with single space.
x$text <- gsub("%%", "\'", x$text)                 #Changing %% back to apostrophes.
x$text <- gsub("https(.*)*$", "", x$text)          #Remove tweet URL.
x$text <- gsub("\\n", "-", x$text)                 #Replace line breaks with -
x$text <- gsub("--", "-", x$text)                  #Remove double - from double line breaks.
x$text <- gsub("&amp;", "&", x$text)               #Fix ampersand &
x$text[x$text == " "] <- "<no text>"

for (i in 1:nrow(x)) {
  if (x$truncated[i] == TRUE) {
    x$text[i] <- gsub("[[:space:]]*$","...",x$text[i])
  }
}

# Remove unused columns
BNR_cleanTweets <- x #%>% dplyr::select("Username","created_at","text", "favorite_count","retweet_count")
head(BNR_cleanTweets)
tail(BNR_cleanTweets) #Data frame 
dim(BNR_cleanTweets)
#___________________________________________________
library(sentimentr)
library(textclean)
#library(bloom)
library(cleanNLP)
library(stm)
library(quanteda)
library(ggrepel)
library(scales)
tweets <- BNR_cleanTweets %>%
  sentimentr::get_sentences()%>% 
  sentimentr::sentiment(polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                        valence_shifters_dt = lexicon::hash_valence_shifters)%>% 
  group_by(element_id) %>% 
  mutate(text = add_comma_space(text)) %>% 
  mutate(tweet = paste(text, collapse=" ")) %>% 
  ungroup() %>% 
  mutate(mentionBNR = ifelse(str_detect(tweet, "CentralBankRw")==TRUE,yes=1,no=0)) %>% 
  mutate(text = replace_white(text)) %>% 
  mutate(tweet = replace_white(tweet)) %>% 
  group_by(tweet) %>% 
  mutate(tweetlevelsentiment = mean(sentiment)) %>% 
  ungroup %>% 
  filter(!is.na(word_count)) %>% 
  filter(text!=" ")  %>% 
  mutate(sentimentbinary = ifelse(sentiment>0,"Positive", 
                                  ifelse(sentiment<0,"Negative","Neutral"))) %>% 
  mutate(tweetsentimentbinary = ifelse(tweetlevelsentiment>0,"Positive",
                                       ifelse(tweetlevelsentiment<0,"Negative","Neutral"))) %>% 
  mutate(distancefromzero = abs(sentiment))%>% 
  select(date, time, Username, tweet, text,source,reply_to_Username,
         is_retweet,reply_count,hashtags,sentiment, sentimentbinary, 
         tweetlevelsentiment, tweetsentimentbinary, distancefromzero,
         favorite_count, retweet_count, mentionBNR,word_count)


#summary(tweets)
#________________SENTIMENTS VISUALIZATION__________________
#Frequency of sentiment by Individual sentences sentences
pg<-tweets%>% 
  count(sentimentbinary)%>%
  mutate(Rate=round(prop.table(table(tweets$sentimentbinary))*100,digits = 2))%>%
  ggplot( aes(x=sentimentbinary,n, fill=sentimentbinary)) +
  geom_bar(stat="identity", show.legend =FALSE) + theme_bw()+
  labs(x="Prevailed Sentiments",y="Number of Sentences Tweeted",
       caption = "Source: @CentralBankRw(2016-2022)") +
  geom_text(aes(label=str_c(Rate, "%")),vjust=-0.1,face="bold",
            angle= 360, size = 6,color="black") +
  ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
  theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
        axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
        axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
        axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
        plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none")
pg
ggsave("pg.png",width = 10, height = 7)
#Individual sentences tend to be overwhelmingly positive. Does a different trend arise when we
#look at entire Tweets (by averaging the scores of each individual sentence)?

#Frequency of sentiment by Tweets
p<-tweets %>% 
  count(tweetsentimentbinary)%>%
  mutate(Rate=round(prop.table(table(tweets$tweetsentimentbinary))*100,digits = 2))%>%
  #distinct(id, .keep_all = TRUE) %>% 
  ggplot(aes(x=tweetsentimentbinary,n,fill=tweetsentimentbinary)) +
  geom_bar(stat="identity", show.legend = FALSE) +theme_bw()+
  labs(x="Sentiment",
       y="Number of Tweets",
       title = "Frequency of Sentiment by Tweet",
       caption = "Source: @CentralBankRw(2016-2022)")+
  geom_text(aes(label=str_c(Rate, "%")),vjust=-0.1,face="bold",
            angle= 360, size = 7.5,color="black")+
  theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
        axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
        axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
        axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
        plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none")
p

#ggsave("p.png",width = 10, height = 7)

#_________WORLD CLOUD_______________________
library(wordcloud2)
library(tm)
df<-read.csv("Bnr.csv")
##Word clouds for all tweets
table_1 <- df%>% 
  mutate(rowmumber = row_number()) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("rt", "", text)) %>% 
  mutate(text = gsub("https","", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = removeNumbers(text)) %>% 
  mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) 
#WorldCloud materials:https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, rotateRatio = 0.4,
                         shape = "circle", ellipticity = 0.75, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) #color="random-light"
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

wordcloud2a(table_1, size = 0.75, shape = "circle", ellipticity = 0.60)

#______________________PREDICTIVE ANALYSIS_____________________________
library(caret)
library(patchwork)
library(corrplot)
library(corrr)
library(grid)
library(gridExtra)
library(DMwR)
library(InformationValue)
library(ROCR)
library(e1071)
library(knitr)
library(caTools)
library(ranger)
#__________________________
dataframe<-tweets%>%
  filter(sentiment!= 0)%>%
  select("tweetlevelsentiment",
         "distancefromzero", "favorite_count", 
         "retweet_count","mentionBNR",
         "element_id","sentence_id","word_count","sentiment")%>%
  mutate(sentiment=as.factor(cut(sentiment, c(-1, 0, Inf), c("Pos", "Neg"), include.lowest=TRUE)))%>%
  mutate(element_id=as.numeric(element_id),
         sentence_id=as.numeric(sentence_id),
         word_count=as.numeric(word_count))
#Setting the output class as factor
colnames(dataframe)
factor_variable_position<-c(9)
dataframe<-dataframe%>%mutate_at(.,vars(factor_variable_position),~as.factor(.))
#Feature Selection  by PCs
#SELECTING THE PCS 
Retained_PCs = preProcess(dataframe[,1:8],
                          method=c( "center", 
                                    "scale", "pca"))
Retained_PCs #Only 8 PCs retained since they are contributing 95% of total variability (variance)in data set.
PCs <-predict(Retained_PCs, dataframe[,1:8])
# Retained PCs 
head(PCs, 3)
#Creating Dataframe from 8 PCs
data<-cbind(PCs,dataframe$sentiment)
Tweets_data<- data %>% as.data.frame()
colnames(Tweets_data)[9] <- 'class'

#________DATA PREPROCESSING BEFORE TRAINING MODELS__________________

#Splitting data into training and test sets
set.seed(12345) #seed for reproducibility
sample_set<-Tweets_data%>%
  pull(.)%>%
  sample.split(SplitRatio = 0.80)

#Training and test sets
dataTrain<-subset(Tweets_data,sample_set==TRUE)
dim(dataTrain)
dataTest<-subset(Tweets_data,sample_set==FALSE)
dim(dataTest)
#____________________
#BALANCINNG THE IMBALANCE OF OUTPUT/CLASS
as.data.frame(table(Tweets_data$class))
remotes::install_github("cran/DMwR",lib="C:/app/R-4.0.4/library")
library(DMwR)
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalance In Binary Classification
dataTrain<-SMOTE(class~., data=dataTrain,
                 perc.over = 80,k=5, perc.under = 200) #46.8/53.2%

as.data.frame(table(dplyr::select(dataTrain,class),exclude = NULL))

#Training Predicting models
#_________________________
#Define training control
fitControl<-trainControl(
  method = "cv", #k-fold cross validation
  number = 10, #Number of k-folds
  savePredictions = "final", #saves the prediction for optimal tuning parametor
  selectionFunction = "best",
  allowParallel = TRUE #Allow parallel computating
)

#Converting "Pos" and "Neg"
library(forcats)
dataTest<-dataTest%>%
  mutate(class=as.factor(fct_recode(dataTest$class, "1"="Pos","0"="Neg")))

dataTrain<-dataTrain%>%
  mutate(class=as.factor(fct_recode(class,"1"="Pos" ,"0"="Neg")))
#Storing the following features for later use.
xytrain<-dataTrain
xtrain<- dataTrain[,-ncol(dataTrain)]
ytrain<-dataTrain$class
xytest<-dataTest
xtest<-dataTest[,-ncol(dataTest)]
ytest<-dataTest$class
ntr<-nrow(dataTrain)
nte<-nrow(dataTest)
######
#______________
#Model1. Logistic Model
logit.model<-glm(class~.,family = binomial,data=na.omit(dataTrain))
#predict the outcomes
logit.pred.prob<-predict(logit.model,na.omit(dataTest),type = "response")
logit.pred<-as.factor(ifelse(logit.pred.prob > 0.5,1,0))
#_____
library(gplots)
ROCRpred <- prediction(logit.pred.prob, as.numeric(dataTest$class))
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

yhat.logit<- predict(logit.model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.logit)
ConfMatrixLogit<-caret::confusionMatrix(logit.pred,dataTest$class,positive="1")
#Model2:CART:Classification and Regression Tree
ctrl<-trainControl(method ="repeatedcv",
                   number = 10,
                   repeats= 5,
                   selectionFunction = "best",
                   allowParallel = TRUE)
grid<-expand.grid(.cp=seq(from=0.0001,to=0.0005,by=0.0001))
set.seed(12345)
tree.model<-train(class~.,
                  data=dataTrain,
                  method="rpart",
                  metric="Kappa", 
                  preProc=c("center","scale"),
                  trControl=ctrl,
                  tuneGrid=grid)
#tree.model
## Make predictions based on our candidate model
tree.pred.prob <- predict(tree.model, dataTest, type = "prob")
tree.pred <- predict(tree.model, dataTest, type = "raw")
###View Confusion Matrix
ConfMatrixTree<-caret::confusionMatrix(tree.pred, dataTest$class, positive = "1")
#___________________
#Model 3. Random Forest

# Create a grid search based on the available parameters.
grid <- expand.grid(.mtry = c(1:8))

## Build the random forest model
rf.mod <- train(class ~.,
        data = dataTrain,
        method = 'rf',
        metric = 'Kappa',
        trControl = ctrl,
        tuneGrid = grid)
## Make the predictions
rf.pred <- predict(rf.mod, dataTest, type = "raw")
rf.pred.prob <- predict(rf.mod, dataTest, type = "prob")

#View Confusion Matrix
ConfMatix_rf<- caret::confusionMatrix(rf.pred, dataTest$class, positive = "1")
#_________________
#Model4. Adaboost
library(xgboost)
model.adaboost <- train(class~., 
                        data = dataTrain, method = "xgbTree" ,
                        trControl = ctrl, 
                        metric = "Kappa")

# predict the outcome on a test set (Testing model or model validation)
adaboost.pred.prob <- predict(model.adaboost,dataTest, type = "prob")
adaboost.pred <- predict(model.adaboost, dataTest, type = "raw")

#View Confusion Matric
ConfMatrix_adaboost<-caret::confusionMatrix(adaboost.pred, dataTest$class, positive = "1")
#________
#Model5. Neural Network
nnet.mod <- train(class~., 
                  data = dataTrain, method = 'nnet' ,
                  trControl = ctrl, 
                  metric = "Kappa")
#nnet.mod
# predict the outcome on a test set (Testing model or model validation)
nnet.pred.prob <- predict(nnet.mod,dataTest, type = "prob")
nnet.pred <- predict(nnet.mod, dataTest, type = "raw")
#View Confusion Matric
ConfMatrix_nnet<-caret::confusionMatrix(nnet.pred, dataTest$class, positive = "1")
#_____________

#COMPARING  THE MODELS
#____________________________________

#1. LOGISTIC REGLESSION.
## Logistic Regression
test <- dataTest$class
pred <- logit.pred
prob <- logit.pred.prob

# Logistic Regression ROC curve
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, main = "ROC Curve for BNR's Tweets Sentiment Prediction", col = 2, lwd = 2)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

## Logistic Regression Performance Metrics
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- tibble(Learning_Machine="Logistic Model", accuracy = accuracy, fmeasure = fmeasure,kappa = kappa, auc = auc)

# CLASSIFICATION AND REGRESSION TREE (CART)
test <- dataTest$class
pred <- tree.pred
prob <- tree.pred.prob[,2]

## Classification Tree ROC Curve
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=3, lwd = 2, add=TRUE)

## Classification Tree Performance Metrics
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Classification Tree (CART)", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 

# RANDOM FOREST

test <- dataTest$class
pred <- rf.pred
prob <- rf.pred.prob[,2]

## Random Forest ROC Curve
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=4, lwd = 2, add=TRUE)

## Random Forest Performance Metrics
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Random Forest", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 

# ADAPTIVE BOOSTING MACHINE
## adaboost
test <- dataTest$class
pred <- adaboost.pred
prob <- adaboost.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=5, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Adaptive Boosting Machine", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#____________________________________________________________

# Neural Network Model
## nnet
test <- dataTest$class
pred <- nnet.pred
prob <- nnet.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=6, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Neural Network", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 

# Draw ROC legend.
legend(0.4, 0.3, c('Logistic Regression', 
                   'Classification Tree', 
                   'Random Forest', 
                   'Adaptive Boosting Machine', 
                   "Neural Network"), 2:6)

#OUTPUT COMPARISON TABLE
#For html table
knitr::kable(comparisons, format = "html")
#For Latex table
knitr::kable(comparisons, format = "latex", caption = "The Model Perfomance Table")

#___________________Prediction with optimal model ____________________________

nnet.pred <- predict(nnet.mod, dataTest, type = "raw")

class(nnet.pred)
table(nnet.pred)
df<-as.data.frame(nnet.pred)
View(df)
table(df)
data<-read_csv("prediction.csv")
class(data)

pred<-ggplot(data=data, aes(x = Sentiment, y = Frequency))+
  geom_bar(stat = 'identity', aes(fill = Sentiment))+
  labs(title = "Artificial Neural Network Predicted Sentiment Score behind BNR Tweets(Oct.2016-Aug.2021)",
       x = 'BNR Tweets Sentiments', y = 'Sentiment Score',
       caption = "Source:@CentralBankRw") +
  #scale_fill_manual(values = pal, name = "BNR Tweets Sentiment Legend") +
  theme_bw() +
  #facet_grid(screenname~.) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=str_c(Rate, "%")),vjust=-0.1,face="bold",
            angle= 360, size = 6,color="black") +
  #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
  theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
        axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
        axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
        axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
        plot.title = element_text(size = 15,face = "bold",colour="red"),legend.position = "none")
pred
ggsave("pred.png",width = 10, height = 7)
#________________END____________________________




#_______________________________________________________________
library(syuzhet)
df<-read.csv("C:/Users/jmurera/Desktop/BNR_Projects/BNR_Tweets/BNR_Tweets2016_2021.csv")
df%>%as_tibble()
as_tibble(df)
usrdf<-lookup_users("CentralBankRw")
Bankdata<-users_data(usrdf)
View(Bankdata)
View(df)
sum(Bankdata$listed_count)
sum(Bankdata$statuses_count)

as.factor(Bankdata$account_created_at)

as.factor(Bankdata$screen_name)
Bankdata$location
readLines('foo.txt', encoding = 'UTF-8')
#____________________________________________________________________________

df1<-read.csv("C:/Users/jmurera/Desktop/BNR_Projects/Beno_App/RDB.csv")

#This codes will open folder, list the csv files and return only most recent file.
#Make a list of data files existing in Folder (WD)
path<- "C:/Users/jmurera/Downloads/BNR_Tweets"
tbl<-list.files(path, pattern= "*.csv", full.names = T,recursive = TRUE)%>%
  enframe(name = NULL) %>% 
  bind_cols(pmap_df(., file.info)) %>% #binding all files with file info.
  filter(mtime==max(mtime)) %>% #select the file with max modified date(mtime) in folder
  pull(value)%>% read.csv(header=TRUE,stringsAsFactors = FALSE,strip.white = 
                            TRUE,sep = ',')

# Reading and binding multiples files  ------------------------------------

#Reading multiples csv files in folder
tbl <-list.files(path, pattern= "*.csv", full.names = T,recursive = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) #Force columns to be character in rowwise merging.


#____
df1<-read.csv("C:/Users/jmurera/Desktop/BNR_Projects/Beno_App/RDB.csv")
df2<-read.csv("C:/Users/jmurera/Desktop/BNR_Projects/Beno_App/RIB.csv")
df3<-read.csv("C:/Users/jmurera/Desktop/BNR_Projects/Beno_App/RURA.csv")
df4<-read.csv("C:/Users/jmurera/Desktop/BNR_Projects/Beno_App/Rwandapolice.csv")
df5<-read.csv("C:/Users/jmurera/Desktop/BNR_Projects/Beno_App/Urugwiro.csv")

#Make a list of data files
data_list <- list(df1, df2, df3,df4,df5)  # Combine data frames to list
my_merge <- function(df1, df2, df3,df4,df5){         # Create own merging function
  merge(df1, df2, df3,df4,df5, by = "verified")
}

PDF<-data_list %>% reduce(inner_join, by = "verified")   
gc()
memory.limit(size = 56000)
dfp<-merge(df1,df2,by = "verified")
dim(dfp)

#_____CREATING TIME SERIES DATA WITH SENTIMENTS

datats<-data_format2%>%
  select(Username=Username.x,
         date,
         time,
         tweet=text,
         source,
         reply_to_Username,
         is_retweet,
         favorite_count,
         retweet_count,
         reply_count,
         hashtags,
         sentiment_Score=sentiment)%>%
  mutate(sentiment_type=ifelse(sentiment_Score>0,"Positive", 
                        ifelse(sentiment_Score<0,"Negative","Neutral"))) %>% 
  arrange(desc(date))

#Make monthly sentiment aggregate
Monthly_aggregated_data<-datats%>% 
  mutate(year = year(date)) %>% 
  mutate(yearMonth =  format(as.Date(date), "%Y-%m")) %>% 
  group_by(yearMonth, sentiment_type) %>% 
  summarise(avg_retweet_count= round(mean(retweet_count), digits = 0),
            #avg_reply_count= mean(reply_count),
            avg_favorite_count= round(mean(favorite_count),digits = 0),
            avg_polarity= round(mean(sentiment_Score),digits = 2),.groups="drop") %>% 
  relocate(sentiment_type, .before=avg_polarity) %>% 
  arrange(desc(yearMonth))

save_as_csv(Monthly_aggregated_data,"BNR_Tweets2016_Oct2022_with_sentiments.csv" )

