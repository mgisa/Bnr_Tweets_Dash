#Project: Social Media data Analytics
# Data: Twitter data
#Author: Murera Gisa
#Date: September 2020
#__________________________
       #SETTING R ENVIRONMENT
#Setting the working directory
setwd("C:/Users/jmurera/Desktop/BNR_Projects/BNR_Tweets")

#Installing the Libraries
pkgs<-c("rtweet","httpuv")
install.packages(pkgs, dependencies = TRUE)
install.packages("reactable",dependencies = TRUE)
install.packages("tidytext",dependencies = TRUE)


#Loading the libraries
library(rtweet)
library(httpuv)
library(tidytext)
library(reactable)
#_____________
#Store API keys
api_key <- "vRnpyCVjCWv6SS2ApbISvukfw"
api_secret_key <- "Co6Msc1I16u0KSUvfqNCMIbvKyhQ6hFVJrpP7B5s1Vv0ADSRDN"
access_token <- "1059794292863045632-KDW9qSp2r4K9fe8vvoB51cb0JyAm76"
access_token_secret <- "rah3op45YUXI9rrkDE55o05TYO6jqUdGzCcaxmwHCryrQ"
# Twitter authentication
token<-create_token(
  app             = "mgisa's Twitter App",
  consumer_key    = api_key,
  consumer_secret = api_secret_key ,
  access_token    = access_token,
  access_secret   = access_token_secret )

token

# Retrieve tweets by including all re-tweets by using special queries
Bnr_tweets <- search_tweets("#MPFSS2020", n = 18000, langs="en", 
                            tweet_mode="extended",include_rts = TRUE,
                            file_name="Bnr_tweets.json",
                            parse=FALSE)

#Scraping the tweets using the user account name
bnr<-get_timeline("@CentralBankRw", lang= "en",n = 3200)

# View output for the first 5 columns and 10 rows
head(bnr, n=3)
###Create the table of users and tweets counts for users
BNR<-search_tweets2("@CentralBankRw",n=18000)
       #Dealing with the TWITTER USER NAMES (WHO ARE TWEETING THE MOST)
#________________________________________
#Counting the number of each tweets on each screen name
table_sc<-table(BNR$screen_name)
head(table_sc) #The screen name of first row and tweets counts on the second row
#Sort the table on descending order of tweets count
table_sc_sort<-sort(table_sc, decreasing = TRUE)
#View top six users and frequencies
head(table_sc_sort,n=6)

      #Dealing with FOLLOWERS ACCOUNT
#_______________________________________

#Extract the user account
bnr_followers<-lookup_users("CentralBankRw")
#Create dataframe with columns user names and followers
bnr_followers_df<-bnr_followers[,c("screen_name","followers_count")]
View(bnr_followers_df) #How many peaple are following BNR
 
      #DEALING WITH RETWEET COUNT AND POPULARITY (FAVORITE)

#Create data frame with text and retweet count
bnr_rtwt<-bnr[,c("text","retweet_count","favorite_count")]
#Sort dataframe based on descending order of retweet and favorite count
library(dplyr)
bnr_rtwt_sort<-bnr_rtwt%>% arrange(desc(retweet_count),desc(favorite_count))

#Exclude rows with duplicate tweets text
bnr_rtwt_unique<-unique(bnr_rtwt_sort,by="text")
 head(bnr_rtwt_unique) #The most re-tweeted text is about IMF Executive Board grants
 DT::datatable(bnr_rtwt_unique)
 
     #Chap2: FILTERING TWEETS (Filtering based on tweets components)
 
 # Count the number of replies on the extracted tweets without filters
 #1. check for the count of replies
 reply_bnr<-bnr%>% count(reply_to_screen_name)
 #Sorting and check the most replier on the BNR tweets
 bnr_reply_sort<-reply_bnr%>% arrange(desc(n))
 #Check the frequent replier (The names and tweets he/she replies)
 head(bnr_reply_sort, n=10)
 #_________
 #2. Check for the count of quotes
 table(bnr$is_quote)
 #3. Check for the count of retweets
 table(bnr$is_retweet)
 #4. Filtering tweets on the languages used.
 lang_bnr<-bnr%>% count(lang)
 #Sorting and check the most replier on the BNR tweets
 bnr_lang_sort<-lang_bnr%>% arrange(desc(n))
 #Check the frequent replier (The names and tweets he/she replies)
 head(bnr_lang_sort, n=10)
 
 
 ###APPLY FILTERIES TO EXCLUDE REPLIES, QUOTES, RETWEETS
 #Apply the"-filter
 bnr_filter<-get_timeline("@CentralBankRw
                          -filter:retweet
                          -filter:quote
                          -filter:replies",
                          n = 3200)
 
 #Filter by retweets and favorites counts
 #min_faves:filter tweets with minimum number of favorites
 #min_retweets: filter tweets with minimum number of retweets
 # you can includ both by using AND
 #Extract tweets with minimum of 100 favorites and retweets
 bnr_filter_pop<-get_timeline("@CentralBankRw min_faves:100 AND min_retweets:100",
                          n = 3200)
 #To see the popularity check the above that by 
 #Creating data frame 
 counts<-bnr_filter_pop[c("retweet_count","favorite_count")]
 head(counts)
 # Use text to see the text retweeted and favorated
 head(bnr_filter_pop$text)
       
       #EXTRACT USER INFORMATION TO MEASURE GOLDEN RATIO (Influence capacity)
 #Extract user information
 user_fit<-users_data(bnr)
 #view column names of user_fit
 colnames(user_fit)
 
 #Extract the user following_count and friends_count
 #Aggregate screen_name against following and friend count
 count_df<-user_fit %>%
   group_by(screen_name)%>%
   summarise(Followers = mean(followers_count), 
             Friends = mean(friends_count))
 #Calculating the Golden Ratio
 #Creating columns to computer the golden ratio
 count_df$ratio<-count_df$Followers/count_df$Friends
 #Check the head of count_df$ratio
 head(count_df$ratio)
 #_____________________DEALING WITH MULTIPLE TWITTER USERS________ 
         #EXploring Users based on the ratio
 #Examine the golden ratios to understand user type
 #First sort data frame in decreasing order of follower account
 count_df_sort<-count_df%>% arrange(desc(Followers))
 #Select the rows where twitter follower is greater than 3000
 count_df_sort[count_df_sort$Followers>3000,] #Medium users to promote the products
 ##Select the rows where twitter follower count is less than 3000
 count_df_sort[count_df_sort$Followers<=3000,] #Position adverts on individual accounts for targeted promotion
  #______________ANALYSIS WITH TWITTER LISTS_______________
 #A twitter user has subscribed to a lists that are interested on
 #Let us extract the lists subscribed to BNR
 bnr_lists<- lists_users("@CentralBankRw")
 bnr_lists # The are no twitter users subscribed to BNR as lists of interests.
 
   #__________TWITTER TRENDS(POPULAR TOPICS)_______
 #Extracting the worldwide popular topics (trending topics) on twitter
 trend_topics<-get_trends()
 head(trend_topics$trend,n=10)
 
 #Extract the locations of available twitter trends
 avai_trends<-trends_available()
 head(avai_trends,n=5)
 #Get trending topics in US
 us_trends<-get_trends("United States")
 #Try for Rwanda
 Africa_trends<-get_trends("Africa") # Dominated by RSA
 #Get trending topics in New york
 ny_trends<-get_trends("New York")
 #Select the most tweeted trends for example in Africa using tweet_volume
 #Aggregate trends and tweets volumes
 trend_df<-Africa_trends %>%
   group_by(trend)%>%
   summarise(Tweet_Volume=mean(tweet_volume))
 head(trend_df)
 #Sorting in ascending order
 trend_df_sort<-Africa_trends %>%
   group_by(trend)%>%
   summarise(Tweet_Volume=mean(tweet_volume))%>%
   arrange(desc(Tweet_Volume))
 head(trend_df_sort)
 
 #PLOTTING TWITTER DATA OVER TIME
 #Tracing the engagement and interest level of twitter user 
 #Create time series flame
 ts_plot(bnr, by="weeks",color="blue")+
    theme_minimal() +
    theme(plot.title = ggplot2::element_text(face = "bold")) +
    labs( x = NULL, y = NULL,
       title = "Frequency of BNR Twitter statuses from past 9 days",
       subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )
 
 #Compare the frequency of tweets
 #The volume of tweets is stronger indicator of brand salience
 # Extract tweets on #walmart and exclude retweets
 walmart_twts <- search_tweets("#walmart", n = 18000, include_rts = FALSE)
 puma_twts <- search_tweets("#puma", n = 18000, include_rts = FALSE)
 
 #Converting into ts data format
 walmat_df<-ts_data( walmart_twts, by="hours")
 # Rename the two columns in the time series object
 names(walmat_df) <- c("time", "walmat_n")
 #Converting into ts data format
 puma_df<-ts_data(puma_twts, by ="hours")
 # Rename the two columns in the time series object
 names(puma_df) <- c("time", "puma_n")
 #Merge dataframe by time
 WaPuma_df<- merge(walmat_df,puma_df, by="time",all=TRUE)
 # Stack all columns except time the tweet frequency columns
 melt_df <- melt(WaPuma_df, na.rm = TRUE, id.vars = "time")
 # Plot frequency of tweets on Puma and Nike
 ggplot(data = melt_df, aes(x = time, y = value, col = variable))+
   geom_line(lwd = 0.8)
#__________________________________
 
     #PROCESSING TWITTER DATA
 #1.Removing the reduntend data
 #Extract tweet text data from the searchable tweets and save in dataframe
 
 bnr_twttext<-bnr$text
 head(bnr_twttext,n=3)
 #CLEANING TEXT DATA
 install.packages("httr", dependencies = TRUE)
 install.packages("twitteR",dependencies = TRUE)
 install.packages("ROAuth", dependencies = TRUE)
 install.packages("NLP", dependencies = TRUE)
 #To activate the authentication key
 install.packages("base64enc", dependencies = TRUE) 
 library(base64enc)
 library(httr)
 library(httpcode)
 library(httpuv)
 library(stringr)
 library(twitteR)
 library(ROAuth)
 library(NLP)
 library(tm)
 library(tidyverse)
 #__________________
 #Installation of recent updated TwitteR from Github.  
 devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
 ##_________
 library(base64enc)
 library(twitteR)

 #Store API keys
 api_key <- "vRnpyCVjCWv6SS2ApbISvukfw"
 api_secret_key <- "Co6Msc1I16u0KSUvfqNCMIbvKyhQ6hFVJrpP7B5s1Vv0ADSRDN"
 access_token<-"1059794292863045632-KDW9qSp2r4K9fe8vvoB51cb0JyAm76"
 access_token_secret <- "rah3op45YUXI9rrkDE55o05TYO6jqUdGzCcaxmwHCryrQ"
 #Registering the OAuth
 
 setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)
#_________________________________
 #SCraping the tweets data restrict other language rather than english, specify the time
 
 BNR_Tweets<-searchTwitter("@CentralBankRw",n=10000,
                           lang = "en" 
                           #, since = '2018-01-01'
                           )
 Tweets.text = lapply(BNR_Tweets,function(t)t$getText())
 
 #twListToDF(BNR_Tweets)
 #Convert the .JSON tweets data into R dataframe
 tweetsDF <- twListToDF(BNR_Tweets)
 dim(tweetsDF)
 #Save the data frame.
 write.csv(tweetsDF, "~/Desktop/NLP/BNR_Tweets_W9.csv")

 # Convert to dataframe, and encode to native
 x <- tweetsDF
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
 x$text <- gsub("  ", " ", x$text)                  #Replace double space with single space.
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
 BNR_cleanTweets <- x %>% dplyr::select("text", "contentURL", "favorited", "favoriteCount",
                             "created", "truncated", "id", "screenName",
                             "retweetCount", "isRetweet", "retweeted")
 head(BNR_cleanTweets)
 tail(BNR_cleanTweets) #Data frame 
 #______________________
 #Buildin a corpus which a large and structured set of texts.
 BNR_corpus=Corpus(VectorSource(BNR_cleanTweets$text))
 BNR_corpus = tm_map(BNR_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
 
 #Convert the BNR_corpus into a lower cases
 BNR_corpus_lower <- tm_map(BNR_corpus, content_transformer(tolower))
 
 #Remove Punctuation on corpus
 BNR_corpus_lower <- tm_map(BNR_corpus_lower, removePunctuation)
 
 #BUILDING A TERM DOCUMENT MATRIX
 #A document terms matrix represent frequency of every word present in the corpus.
 BNR_cleanTweets=TermDocumentMatrix(BNR_corpus_lower)
 inspect(BNR_cleanTweets)
  
 #FIND THE FREQUENCY OF EACH WORD IN CORPUS
 m=as.matrix(BNR_cleanTweets)
 
 v=sort(rowSums(m),decreasing = T)
 BNR_DF=data.frame(word=names(v),freq=v)
 #Check the most 10 frequent unique words in BNR tweets
 head(BNR_DF,n=10)
 #Check the least 10 frequents words
 tail(BNR_DF,n=10)
 
 #Plot Unique Word frequencies (Most 10 frequents words)
 barplot(BNR_DF[1:10,]$freq, las = 2, names.arg = BNR_DF[1:10,]$word,
         col ="lightblue", main ="The Most Ten Frequent Words from National Bank of Rwanda Tweets.",
         ylab = "Word frequencies") 
 #The above chart shows that the most common words used in the tweets are Central bank of Randa
       
 #Interactive Visually way of the frequents words in the tweets
 #_____________________________________________________________
 #Building of the Word Cloud, (WORDCLOUD is the visual representation of words in the tweets)
 
 install.packages("RColorBrewer",dependencies = TRUE)
 
 library(RColorBrewer)
 library(wordcloud)
 set.seed(1234) 
 
 wordcloud(words = BNR_DF$word, freq = BNR_DF$freq, 
           min.freq =sort(BNR_DF$freq,decreasing = TRUE)[[100]],
           max.words=1000, random.order=FALSE,
           scale = c(3, .8), rot.per = .0,colors=brewer.pal(10, "Paired"),
           random.color=TRUE)#,
           #colors =rainbow(start = .6 , end = 0, n = 3))
 
 #The above word cloud shows that most frequently used words in the tweets are the,
 #CentralBankRw and so on
 #he different colors and size of the words indicate their frequency.
 
          #SENTIMENT ANALYTICS
 #___________________________
 #Our main aim is to analyze the sentiments of people around BNR Tweets. The analysis will consist of 
 #eight different emotions and two sentiments positive and negative.
 
 install.packages("syuzhet", dependencies = TRUE)
 library(syuzhet)
 library(lubridate)
 library(scales)
 library(reshape2)
 #________________
 
 #Getting 8 different Emotions (Anger,anticipation disgust,fear,joy,sadness,surprise,trust) 
 #And their corresponding Valence From NRC Dictionary
 #The get_nrc sentiment function from the package syuzhet is used which will compare all the tokenized words with
 #the wordsentinet EmoLex which contain a large number of words with different emotions.
 #__________________
 
     #BNR TWEETS SENTIMENT ANALYTICS
 #Getting the sentiment fromNRC Dictionary
 emotions<-get_nrc_sentiment(tweetsDF$text) #implements Saif Mohammad’s NRC Emotion lexicon
 head(emotions)
 Bnr_sentiments<-get_sentiment(tweetsDF$text, method = "syuzhet",
                               cl=TRUE, lexicon = "nrc")
 
 #____________________________
 # Emotions for each tweet using NRC dictionary 
 #emotions <- get_nrc_sentiment(tweets.df$text)
 emo_bar = colSums(emotions)
 emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
 emo_sum$emotion = factor(emo_sum$emotion, 
                          levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
 
 # Visualize the emotions from NRC sentiments (PLOTTING WITH PLOTLY)
 library(plotly)
 p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
   layout(xaxis=list(title=""), showlegend=FALSE,
          title="Emotions and Sentiments behind BNR Tweets")
 print(p)
 #Here we see majority of the people are discussing positive and express a trust about 
 #BNR tweets which is good indicator for a bank.
 #___________________WHICH WORDS CONTRIBUTE WHICH EMOTIONS
 # Create comparison word cloud data
 
 wordcloud_tweet = c(
   paste(tweetsDF$text[emotions$anger > 0], collapse=" "),
   paste(tweetsDF$text[emotions$anticipation > 0], collapse=" "),
   paste(tweetsDF$text[emotions$disgust > 0], collapse=" "),
   paste(tweetsDF$text[emotions$fear > 0], collapse=" "),
   paste(tweetsDF$text[emotions$joy > 0], collapse=" "),
   paste(tweetsDF$text[emotions$sadness > 0], collapse=" "),
   paste(tweetsDF$text[emotions$surprise > 0], collapse=" "),
   paste(tweetsDF$text[emotions$trust > 0], collapse=" ")
 )
 
 # create corpus
 corpus = Corpus(VectorSource(wordcloud_tweet))
 
 # remove punctuation, convert every word in lower case and remove stop words
 
 corpus = tm_map(corpus, tolower)
 corpus = tm_map(corpus, removePunctuation)
 corpus = tm_map(corpus, removeWords, c(stopwords("english")))
 corpus = tm_map(corpus, stemDocument)
 
 # create document term matrix
 
 tdm = TermDocumentMatrix(corpus)
 
 # convert as matrix
 tdm = as.matrix(tdm)
 tdmnew <- tdm[nchar(rownames(tdm)) <11,]
 
 # column name binding
 colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear',
                   'joy', 'sadness', 'surprise', 'trust')
 colnames(tdmnew) <- colnames(tdm)
 comparison.cloud(tdmnew, random.order=FALSE,
                  colors = c("#00B2FF", "red", "#FF0099", "#6600CC", 
                             "green", "orange", "blue", "brown"),
                  title.size=1, max.words=150, scale=c(1., 1.5),
                  title.colors="black", match.colors=TRUE,
                  title.bg.colors="grey",rot.per=.3)
 
 #Common words appeared
 library(RColorBrewer)
 commonality.cloud(tdmnew, random.order=FALSE, scale=c(1.5, .4),
                   colors = brewer.pal(4, "Dark2"), max.words=1000)
 #__________________________
 #BAR PLOT
 barplot(colSums(BNR_twt_sentiment),las=2,col=rainbow(10),ylab="Total Word Counts/Frequencies",
         main = "Emotion and Sentiment Scores behind BNR Tweets")
 #____________ BAR PLOTS USING GGPLOTS___________
 #reshape the data to make it easier to plot
 tweet_sentiment <- melt(BNR_twt_sentiment)
 dim(tweet_sentiment)
 colnames(tweet_sentiment)[1] <- "Sentiment"
 colnames(tweet_sentiment)[2]<- "Frequency"
 
 df<-as.data.frame(colSums(BNR_twt_sentiment))
 colnames(df)[1]<-paste("Sentiment")
 Sentiments<-c("Anger","Anticipation","Disgust",
          "Fear","Joy","Sadness ","Surprise ",
          "Trust","Negative","Positive ")
 Sentiments<-as.data.frame(Sentiments)
 dfp<-as.data.frame(df[,1])
 
PDF<-cbind(Sentiments,dfp)
colnames(PDF)[2]<-paste("Frequency")
 PDF
 BNR_Tweets_Sentiments<-PDF%>%
    arrange(desc(Frequency))
 #___________PLOTING
 pal <- brewer.pal(n = 10, name = 'Set3')
 Emotions<-BNR_Tweets_Sentiments%>%
   mutate(Sentiments,
          Rate= round((Frequency/sum(Frequency))*100, digits=2))%>%
 ggplot( aes(x = reorder(Sentiments,-Frequency), y =Frequency))+
 geom_bar(stat = 'identity', aes(fill = Sentiments)) +
    labs(title = "Emotion and Sentiment Scores behind BNR Tweets",
         x = 'BNR Tweets Sentiments', y = 'Total Word Count',
         caption = "BNR's Tweets Analytics by Mgisa") +
    scale_fill_manual(values = pal, name = "BNR Tweets Sentiment Legend") +
    theme_bw() +
    #facet_grid(screenname~.) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
   geom_text(aes(label=str_c(Rate, "%")),vjust=-0.1,face="bold",
             angle= 360, size = 6,color="black") +
   #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
   theme(axis.text.x = element_text(angle = 45,face = "bold",colour = "black",size = 15),
         axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
         axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
         axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
         plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none")
 Emotions
 ggsave("Emotions.png",width = 10, height = 7)
 #__________________________
 
 #INTERPRETATION
 #Above Bar graph representation is used to visualize the various sentiments behind BNRtweets.
 #As expected positive is highest followed by Trust & Negative. 
 #This means there are large number of people who thinks that the BNR tweets will bring positive changes. 
 #__________
 #CONCLUSION
 
#My conclusion is purely based on the 500 number of tweets 
#I pulled out from Twitter API on 9th Sept. 2020.
#There was mostly positive reactions from people since it has highest sentiment scores followed by Negative & Trust.
#_____________________________________ 
 
     #BNR TWEETS BASED SENTIMENT MODELLING
 #Topic and Document Modelling using USING LDA (Latent Direchlet Allocation Model)
 
 #Inspect the Document Term Matrix Created from tweets text corpus
 BNR_cleanTweets=TermDocumentMatrix(BNR_corpus_lower)
 inspect(BNR_cleanTweets)
  #PREPARATION OF DTM
 #Filter the DTM of rows that have rows sums greater than zero before apply of LDA
 #Find the sum of word count in each documents
 RowsTotals<-apply(BNR_cleanTweets,1,sum)
 #Select of rows of DTM which rows sum is greater tahn zero.
 BNR_Tweets_New<-BNR_cleanTweets[RowsTotals>0,]
       #Building Topic Model using LDA() function
 install.packages("topicmodels",dependencies = TRUE)
 library(topicmodels)
 LDA_model<-LDA(BNR_Tweets_New,k=100) #k is number of topic
 #Extract top ten terms in topic model
 Top10_terms<-terms(LDA_model,10)
 Top10_terms
 #
#________________________________________________
 #               TWITTER NETWORK ANALYTICS
 
 #________________________________________________
 
 #Create the tweets data frame
 library(twitteR)
 library(data.table)
 Bnr_MPFSS<-search_tweets("#MPC2020+CentralBankRw",n=18000, include_rts = TRUE)
 dim(Bnr_MPFSS)
 fwrite(Bnr_MPFSS, file ="FSCDFhashtag.csv")
 #MPCDFhashtag <- twListToDF(Bnr_MPFSS)
 write.csv(Bnr_MPFSS,"MPCDFhashtag.csv")
 #Create data frame from the networks
 
 rt_df<-Bnr_MPFSS[,c("screen_name","retweet_screen_name")]
 #Source Vertex is the screen name and the target vertex is the retweet screen name
 head(rt_df, n=3)
 #Removing rows with missing values
 rt_df_new<-rt_df[complete.cases(rt_df),]
 #Convert to a matrix in order to construct network
 matrx<-as.matrix(rt_df_new)
 #Create a retweet network
 install.packages("igraph", dependencies = TRUE)
 library(igraph)
 nw_retweet<-graph_from_edgelist(el=matrx, directed = TRUE)
 #There is direct network with 63 edges and 188 networks
 
 #__________CAlculating of Degree of Centrality of users____
 
 #Calculate out-degree
 out_degree<-igraph::degree(nw_retweet,
                            "MMatatajado", #User in Retweet Network
                            mode = c("out"))
                    
 out_degree # Samuel Camarade and MMatatajado have retweeted #MPFSS2020 5 and 6 times respectively.
 #_________________
 #Calculate in-degree
 in_degree<-igraph::degree(nw_retweet,
                            "samuelcamarade", #User in Retweet Network
                            mode = c("in"))
 in_degree #This indicated that this user's post has retweeted zero times.
 
 #__________Identification of user who retweeted the most 
 #by calculating the out-degree of network
 out_degreeNTW<-degree(nw_retweet,
                       mode = c("out"))
 
 
 #Find the top three retweeted #MPFSS2020 the most.
 out_degreeNTW_sort<-out_degreeNTW %>%
   sort(decreasing = TRUE)
   
 out_degreeNTW_sort[1:3] #These users are key players and they can 
 #be used as a medium to retweets a promotional posts of product brand
 View(out_degreeNTW_sort)
 #________________________
 
 #Identification of user whose post were retweeted most
 #Calculate in-degree scores to identify the user whose post were retweeted most.
 in_degree<-degree(nw_retweet,
                   mode = c("in"))
 
 
 in_degree_sort<- in_degree%>% sort(decreasing = TRUE)
 #Top five users whose posts were retweeted most.
 in_degree_sort[1:5] # Imvaho_Shya is the influencer, it can be used to initiate the branding message of a firm
 
 #______CALCULATING THE BETWEENNESS WITHIN NETWORK_____
 
 #Identify the users with high betweenness centrality.
 #calculate the betweenness scores of the networks
 btwn_scores<-betweenness(nw_retweet,directed = TRUE)
 #Sorting users in descending order of betweenness scores
 btwn_scores_sort<-btwn_scores%>%
                   sort(decreasing = TRUE)%>%
                   round(digits = 3)
 #View the top three users who are key bridges btn people who retweet frequently and
 #users whose retweets are tweeted frequently.
 btwn_scores_sort[1:3]   #No connection between people who are retweets frequently 
 #and users whose retweets are tweeted frequently.
 
 #__________VISUALIZATION OF TWITTER NETWORK_____
 
 #Create the basic graph from retweet network
 
 set.seed(12345) #Fixes the randomness to reproduce the plot at everytime
 #Generate the basic plot
 plot.igraph(nw_retweet) #Vertices are indicated by yellow circles and edges by grey lines
 
 #Formating the plot by specifying a color and size of edges
 set.seed(12345)
 plot.igraph(nw_retweet,
             asp=9/16,
             # === vertex
             #_____________
             vertex.size=14, #Vertex size
             vertex.color="lightblue", #color of the vertex
             vertex.label.cex=.5, #Size of the vertex label
             #vertex.label.color="black",
             vertex.shape=c("circle","csquare"),# One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
             #vertex.label=LETTERS[1:10],  # Character vector used to label the nodes
             vertex.label.color=c("red","blue"),
             vertex.label.family="Times",     # Font family of the label (e.g.“Times”, “Helvetica”)
             #vertex.label.font=c(1,2,3,4), # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
             #vertex.label.cex=c(0.5,1,1.5), # Font size (multiplication factor, device-dependent)
             vertex.label.dist=0,             # Distance between the label and the vertex
             vertex.label.degree=0 ,# The position of the label in relation to the vertex (use pi)
             #===EDGES
             #_________
             #edge.color=rep(c("red","pink"),5), 
             edge.arrow.size=0.5, #Size of an arrow
             edge.color="black", #Color of the network edge
             edge.width=1.2,
             edge.lty=c("dashed"),
             layout=layout_randomly,   #layout_with_fr, 
             main="Retweet Network Behind BNR Tweets on #MPFSS2020"
             )

 #Create a variable for out-degree
 ged_out<-degree(nw_retweet,
                 mode=c("out"))
 #Amplify the variable by multiplying by number
 
 vert_size<-(ged_out*1)+2 #To make minimum number ten
 #Assign vertex size to make the size of vertex proportional to the number of retweets
 set.seed(12345)
 plot.igraph(nw_retweet,
             asp=9/16,
             # === vertex
             #_____________
             vertex.size=vert_size, #Vertex size
             vertex.color="lightblue", #color of the vertex
             vertex.label.cex=.5, #Size of the vertex label
             #vertex.label.color="black",
             vertex.shape=c("circle","csquare"),# One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
             #vertex.label=LETTERS[1:10],  # Character vector used to label the nodes
             vertex.label.color=c("red","blue"),
             vertex.label.family="Times",     # Font family of the label (e.g.“Times”, “Helvetica”)
             #vertex.label.font=c(1,2,3,4), # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
             #vertex.label.cex=c(0.5,1,1.5), # Font size (multiplication factor, device-dependent)
             vertex.label.dist=0,             # Distance between the label and the vertex
             vertex.label.degree=0 ,# The position of the label in relation to the vertex (use pi)
             #===EDGES
             #_________
             #edge.color=rep(c("red","pink"),5), 
             edge.arrow.size=0.5, #Size of an arrow
             edge.color="black", #Color of the network edge
             edge.width=1.2,
             edge.lty=c("dashed"),
             layout=layout_randomly,   #layout_with_fr, 
             main="Retweet Network Behind BNR Tweets on #MPFSS2020"
 )
 
 #REshape network plot to include the users with high followers account
 #Create dataframe for followers
 followers<-Bnr_MPFSS[,c("screen_name","followers_count") ]
 #categorize the higher and lower follow account
 #Assign 1 to the account with followers account whose followers is greater than 1000 else assign 0 
 followers$follow<-ifelse(followers$followers_count >1000,"1","0")
 head(followers)
 #Assign External network attribute to retweek network
 
 V(nw_retweet)$followers<-followers$follow
 #View the vertex attributes
 vertex_attr(nw_retweet)
 #Changing vertex color in the graph basing on the vertex attributes
 #Sset the vertex color for the plot
 sub_color<-c("lightgreen","tomato")
 set.seed(12345)
 plot.igraph(nw_retweet,
             asp=9/16,
             # === vertex
             #_____________
             vertex.size=14, #Vertex size
             vertex.color=sub_color[as.factor(vertex_attr(nw_retweet,"followers"))], #color of the vertex
             vertex.label.cex=.5, #Size of the vertex label
             #vertex.label.color="black",
             vertex.shape=c("circle","csquare"),# One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
             #vertex.label=LETTERS[1:10],  # Character vector used to label the nodes
             vertex.label.color=c("black","blue"),
             vertex.label.family="Times",     # Font family of the label (e.g.“Times”, “Helvetica”)
             #vertex.label.font=c(1,2,3,4), # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
             #vertex.label.cex=c(0.5,1,1.5), # Font size (multiplication factor, device-dependent)
             vertex.label.dist=0,             # Distance between the label and the vertex
             vertex.label.degree=0 ,# The position of the label in relation to the vertex (use pi)
             #===EDGES
             #_________
             #edge.color=rep(c("red","pink"),5), 
             edge.arrow.size=0.5, #Size of an arrow
             edge.color="black", #Color of the network edge
             edge.width=1.2,
             edge.lty=c("solid"),
             layout=layout_randomly,   #layout_with_fr, 
             main="Retweet Network Behind BNR Tweets on #MPFSS2020"
 )
 
 #______________PRESENTING TWEETS ON MAP
 
 #Extract the tweets on #MPFSS2020
 Bnr_MPFSS<-search_tweets("#MPFSS2020",n=1000, include_rts = TRUE)
#Search geolocation data and append new columns.
 Bnr_MPFSS_coord<-lat_lng(Bnr_MPFSS)
 View(Bnr_MPFSS_coord)
 # Omit the rows with NA
 Bnr_geo<-na.omit(Bnr_MPFSS_coord[,c("lat","lng")])
 map(database="world",rebind, fill=TRUE, col="light yellow")
 with(Bnr_geo, points(lng,lat,pch=28, cex=1,col="blue"))
#_________________________________________________________________
 #_________________________________________________________________
 #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 #+++++++++SENTIMENT ANALYTICS WITH SENTIMENTR LIBRARY+++++++++
 #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 install.packages("sentimentr",dependencies = TRUE)
 
 library(sentimentr)
 library(knitr)
 library(lubridate)
 uncleaned<-Bnr_MPFSS
 
 uncleanedp<-kable(head(uncleaned$text))
 #uncleanedp
 cleaned <- uncleaned %>%
   filter(!str_detect(text, '^"')) %>%
   mutate(text = str_replace_all(text, "http://t.co/[A-Za-z\\d]+|&amp;", "")) %>% 
   mutate(text = str_replace_all(text, "@[A-Za-z0-9_]+[A-Za-z0-9-_]+", "")) %>% 
   mutate(text = str_replace_all(text, "@", "")) %>% 
   mutate(text = str_replace_all(text, "—", " ")) %>% 
   mutate(text = str_replace_all(text, "http\\S+", "")) %>%
   mutate(createdDate = parse_date_time(created_at, "%Y-%m-%d")) %>%  
   filter(!is.na(text)) %>% 
   filter(text!="")
 cleanedtweets <- cleaned  %>% 
   filter(is_retweet == 1)
 cleanedtweets <- cleaned  %>% 
   filter(is_retweet == 0)
 
 #cleaned$candidate <- recode(cleaned$screen_name, "samuelcamarade"="Samuel Camarade")

 cleanedtweets <- cleaned
 cleanedtweets %>% 
   group_by(screen_name) %>% 
   distinct(status_id, .keep_all = TRUE) %>%  #Removing duplicate in R (dplyr)
   ggplot(aes(x=forcats::fct_infreq(screen_name))) +
   geom_bar(stat="count") +
   coord_flip() +
   labs(y="Number of retweets on #MPFSS2020",
        x="Twitter Users")
 
 
 cleanedtweets %>% 
   ggplot(aes(x=created_at)) +
   geom_histogram(position = "identity", bins = 12, show.legend = FALSE) +
   labs(x="Day of Creation",
        y="Number of Tweets")
 
 cleanedtweets %>% 
   ggplot(aes(x = created_at, fill = screen_name)) +
   geom_histogram(position = "identity", bins = 12, show.legend = FALSE) +
   facet_wrap(~screen_name, scales = "free") +
   labs(x="Month of Creation",
        y="Number of Tweets")
 #________________
#SENTIMENTS
 install.packages(c("textclean","bloom","cleanNLP","stm","quanteda","ggrepel","scales"),
                  dependencies = TRUE)
 library(textclean)
 library(bloom)
 library(cleanNLP)
 library(stm)
 library(quanteda)
 library(ggrepel)
 library(scales)
 tweets <- BNR_cleanTweets %>%
   sentimentr::get_sentences()%>% 
   sentimentr::sentiment(polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                         valence_shifters_dt = lexicon::hash_valence_shifters) %>% 
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
   select(screenName, id, tweet, text, sentiment, sentimentbinary, 
          tweetlevelsentiment, tweetsentimentbinary, distancefromzero,
          favoriteCount, retweetCount, created, mentionBNR,
          element_id, sentence_id, word_count)
 
 
 summary(tweets)
 #df_sentiment<-tweets%>% count(sentimentbinary)
 
#Frequency of sentiment by Individual sentences sentences
 pg<-tweets%>% 
   count(sentimentbinary)%>%
   mutate(Rate=round(prop.table(table(tweets$sentimentbinary))*100,digits = 2))%>%
   ggplot( aes(x=sentimentbinary,n, fill=sentimentbinary)) +
   geom_bar(stat="identity", show.legend =FALSE) + theme_bw()+
   labs(x="Prevailed Sentiments",y="Number of Sentences Tweeted",
        caption = "Source: BNR's Twitter Account analyzed by mgisa") +
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
        caption = "Source: BNR's Twitter Account analyzed by mgisa")+
   geom_text(aes(label=str_c(Rate, "%")),vjust=-0.1,face="bold",
             angle= 360, size = 7.5,color="black")
 p
ggsave("p.png",width = 10, height = 7)

 #___________________________________________________
 #This reveals that there is less neutrality in Tweets than there is in sentences. This makes sense when we consider the likelihood of a
 #Tweet being entirely composed of neutral sentences (sentiment = 0) is relatively low
 #++++++++++++++++++++++++++++++++++++++++++++++++++++++
 #An additional question is if sentiment differs by candidate.
 tweets %>% 
   distinct(id, .keep_all = TRUE) %>% 
   group_by(screenName, tweetsentimentbinary) %>% 
   summarise(n = n()) %>% 
   mutate(percentsentiment = 100*(n/sum(n))) %>% 
   ggplot(aes(x=tweetsentimentbinary, y= percentsentiment, fill=tweetsentimentbinary)) +
   geom_col(show.legend = FALSE) + 
   facet_wrap(~ screenName) +
   labs(x="Sentiment",
        y="Number of Tweets",
        title = "Frequency of Sentiment by Tweet")
 #________________________
 #Top Ten candidates tend to be the most positive and the most negative:
 Top3<-tweets %>%
   group_by(screenName) %>% 
   summarise(sentiment = mean(sentiment)) %>% 
   arrange(desc(sentiment))
 
 #%>% 
   p<-ggplot(Top3[1:3,], aes(x = reorder(screenName, sentiment),
              y = sentiment, fill=screenName)) + geom_bar(stat = "identity",
                          show.legend = FALSE)+theme_bw()+
   #geom_col(show.legend = FALSE) +
   labs(x="Twitter Users", y="Sentiment Scores",
        caption = "BNR's Tweets Analytics by Mgisa") +
   ggtitle("Average Sentiment by Top three Twitter actors in BNR Twitter Network:", 
           subtitle = "The higher scores indicating more positive content, then Neutral and Negative.")  #+
   #coord_flip()
   p
   ggsave("p.png", height = 8, width = 7)
   plotly::plotly_build(p)
   #There is considerable variance in positivity across candidates. Mutabazi’s 
   #Tweets tend to be, on average, 3.74x more positive than those from Asiimwe and Kayibanda.
 #__________________
   # THe average sentiment score behind the tweets that mention BNR VS Don't
   # how does sentiment change in Tweets that mention BNR or Don't
 tweets %>% 
   group_by(mentionBNR) %>% 
   summarise(meansentiment = mean(sentiment))%>% 
   ggplot(aes(x=mentionBNR, y=meansentiment, 
              fill=mentionBNR)) +
   geom_col(show.legend = FALSE) +
   ggtitle("Average Sentiment", subtitle = "Tweets that mention BNR vs those that don't") +
   ylab("Sentiment Score") +
   xlab(element_blank()) +
   scale_x_continuous(breaks=c(0,1), 
                      labels = c("Does Not Mention BNR", "Does Mention BNR"))
 #The Tweets that mention BNR are significantly more positive than others
#__________________________
 #The regression Analytics btn the twitter user interaction and sentimnets.
 KK<-tweets %>%
   distinct(id, .keep_all = TRUE) %>% 
   mutate(interactions = favoriteCount + retweetCount) %>% #Interaction 
   filter(interactions < 900) %>% 
   filter(sentiment<1.5)
 model<-lm(interactions~tweetlevelsentiment, data = KK)
 #%>% 
   ggplot(aes(x=tweetlevelsentiment, y=interactions)) +
   geom_point(alpha = .05) +
   geom_smooth(method=lm, se=TRUE) +
   ylab("Number of Interactions") +
   xlab("Sentiment Score") +
   ggtitle("Relationship Between Tweet Sentiment and Interactions")
 #A linear regression of Tweet interactions by sentiment reveals that a one unit increase 
 #in sentiment results in a loss of 17.96~18 Tweet interactions (p < 0.0001).
   #________________________
   
   
   
   
#PREDICTIVE LEARNING
 #_______________________________
   library(caret)
   library(patchwork)
   library(knitr)
   library(corrplot)
   library(corrr)
   library(grid)
   library(gridExtra)
   library(DMwR)
   library(InformationValue)
   library(ROCR)
   library(e1071)
   library(caTools)
   library(ranger)
   #___________________
   
 dataframe<-tweets%>%
   mutate(sentiment=recode(tweetsentimentbinary,
                      "Positive" = 1 ,"Negative" = -1,"Neutral"= 0))%>%
   filter(sentiment!=0)%>%
   select("tweetlevelsentiment",
          "distancefromzero", "favoriteCount", 
          "retweetCount","mentionBNR",
          "element_id","sentence_id","word_count","sentiment")%>%
   mutate(sentiment=as.factor(recode(sentiment,"1" = "Pos", "-1" = "Neg")))
 
 View(dataframe)
 str(dataframe)
 df_data<-dataframe%>%
   mutate(element_id=as.numeric(element_id),
          sentence_id=as.numeric(sentence_id),
          word_count=as.numeric(word_count))
 #Setting the output class as factor
 factor_variable_position<-c(9)
 dataframe<-dataframe%>%mutate_at(.,vars(factor_variable_position),~as.factor(.))
 colnames(dataframe)
 View(dataframe)
 class(dataframe) 
 class(dataframe$tweetlevelsentiment)
 #SELECTING THE PCS 
 Retained_PCs = preProcess(dataframe[,1:8],
                           method=c( "center", 
                                    "scale", "pca"))
 Retained_PCs #Only 9 PCs retained since they are contributing 95% of total variability (variance)in data set.
 PCs <-predict(Retained_PCs, dataframe[,1:8])
 # Retained PCs 
 head(PCs, 3)
 #Creating Data from 9 PCs
 data<-cbind(PCs,dataframe$sentiment)
 View(data)
 Tweets_data<- data %>% as.data.frame()%>%
   mutate(class=dataframe$sentiment)
 
 dim(Tweets_data)
 View(Tweets_data)
 Tweets_data<- Tweets_data[,-8]
 dim(Tweets_data)
 View(Tweets_data)
 colnames(Tweets_data)
 #_____________

 #Tabulating the output to see the balance
Tweets_data%>% count(class)

#DATA PREPROCESSING BEFORE TRAINING MODELS
#Splitting data into training and test sets
set.seed(12345)#seed for reproducibility
sample_set<-Tweets_data%>%
  pull(.)%>%
  sample.split(SplitRatio = 0.70)

#Training and test sets
dataTrain<-subset(Tweets_data,sample_set==TRUE)
dim(dataTrain)
dataTest<-subset(Tweets_data,sample_set==FALSE)
dim(dataTest)
#____________________
#BALANCINNG THE IMBALANCE OF OUTPUT/CLASS
as.data.frame(table(Tweets_data$class))
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
dataTrain<-SMOTE(class~., data=dataTrain,
                 perc.over = 80,k=5, perc.under = 200)
                 
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
#__________________
#Converting "Pos" and "Neg"
dataTest<-dataTest%>%
  mutate(class=as.factor(recode(class,"Pos" = 1 ,"Neg" = 0))) 
dataTrain<-dataTrain%>%
  mutate(class=as.factor(recode(class,"Pos" = 1 ,"Neg" = 0)))
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
logit.model<-glm(class~.,family = binomial,data=dataTrain)
#predict the outcomes
logit.pred.prob<-predict(logit.model,dataTest,type = "response")
logit.pred<-as.factor(ifelse(logit.pred.prob > 0.5,1,0))
#_____
ROCRpred <- prediction(logit.pred.prob, dataTest$class)
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
rf.mod <- 
  train(class ~.,
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
data<-read_csv("~/Desktop/NLP/prediction.csv")
class(data)

pred<-ggplot(data=data, aes(x = Sentiment, y = Frequency))+
  geom_bar(stat = 'identity', aes(fill = Sentiment))+
  labs(title = "Artificial Neural Network Predicted Sentiment Score behind BNR Tweets",
       x = 'BNR Tweets Sentiments', y = 'Sentiment Score',
       caption = "BNR's Tweets Analytics by Mgisa") +
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
 

