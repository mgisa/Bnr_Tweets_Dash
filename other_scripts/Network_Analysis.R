#TWITTER NETWORK ANALYTICS

#________________________________________________
setwd("~/Desktop/Network_Analysis/")
library(tidyverse)
library(twitteR)
library(purrr)
library(rtweet)
library(data.table)
#Store API keys
api_key <- "mC8Usreri3bMzVMDXFcmaxrT6"
api_secret_key <- "WnRBOjPdY43fIYPhWHdn2ldPtR0wtDAQ9b9AKk9bdbaQi4gbeD"
access_token<-"1059794292863045632-0VVYXCedOVG7jGyMbujGFDlRyENK0W"
access_token_secret <- "LAZ87VJpAmqeIuDAZ1fzXJMLtJk4To05Nu3d0FJzd1Vbt"
#set up authentication credintilas.
setup_twitter_oauth(api_key,api_secret_key,access_token_key,access_token_secret)
# create token named "twitter_token"
#WE HAVE USED THIS
twitter_token <- rtweet::create_token(
  app = "Mgisa's Twitter App",
  consumer_key = "WKOfVK0xqfXUVjKAzsomeFfRh",
  consumer_secret = "1jH4UzbRdEDmyqztR12341lRkyWaecxmQxMlsfLZZ1PGGmf8ex",
  access_token = "1059794292863045632-bncxMd5HBefcMWZP49eZ4Si1a5MUlO",
  access_secret = "W3oXHPE1c2k4CMYuhP9rkp6xV25LjROCr3tP5ZiWOdBtn")
twitter_token
#file.remove(".httr-oauth")
#Create the tweets data frame

Bnr_MPC2021<-search_tweets("#MPC2021+@CentralBankRw",n=18000, include_rts = TRUE)
dim(Bnr_MPC2021)
fwrite(Bnr_MPC2021, file ="C:/Users/jmurera/Desktop/BNR_Projects/BNR_Tweets/FSC2021hashtag.csv")
#MPCDFhashtag <- twListToDF(Bnr_MPC)
write.csv(Bnr_MPC,"MPC2021hashtag.csv")
#Create data frame from the networks

rt_df<-Bnr_MPC2021[,c("screen_name","retweet_screen_name")]
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
nw_retweet
#There is direct network with 44 edges and 188 networks

#__________CAlculating of Degree of Centrality of users____

#Calculate out-degree
out_degree<-igraph::degree(nw_retweet,
                           "SorayaMHlive", #User in Retweet Network
                           mode = c("out"))

out_degree # Sheilla, Allan, Doreen, Thierry have retweeted #MPC2021 4, 5, 6, 3 times respectively.
#Governor:1, DGovernor:1
#_________________
#Calculate in-degree
in_degree<-igraph::degree(nw_retweet,
                          "AllanKar4", #User in Retweet Network
                          mode = c("in"))
in_degree #This indicated that this user's post has retweeted zero times.

#__________Identification of user who retweeted the most 
#by calculating the out-degree of network
out_degreeNTW<-degree(nw_retweet,
                      mode = c("out"))

#Find the top three retweeted #MPC2021 the most.
out_degreeNTW_sort<-out_degreeNTW %>%
  sort(decreasing = TRUE)

out_degreeNTW_sort[1:5] #These users are key players and they can 
#be used as a medium to retweets and deseminating the communicated policy
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
            main="Retweet Network Behind BNR Tweets on #MPC2021"
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
            main="Retweet Network Behind BNR Tweets on #MPC2021"
)

#https://juanitorduz.github.io/text-mining-networks-and-visualization-plebiscito-tweets/

#REshape network plot to include the users with high followers account
#Create dataframe for followers
followers<-Bnr_MPC[,c("screen_name","followers_count") ]
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