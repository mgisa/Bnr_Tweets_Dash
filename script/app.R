#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Project Title: Developing a Live Tweets and Sentiment Monitoring Tool for Central Banks (LTSMT_CB)
#Author: Murera Gisa
#Host: Central Bank of Rwanda(BNR)
#Period: Sept.2020-March 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 #++++++++++++++++++++++++       Setting working directory        ++++++++++++++++
 #++++++++++++++++++++++++                                        ++++++++++++++++
 #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
dirname <-  getwd()
if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)

#----------------Installing and importing the required packages---------------

#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0", lib="C:/app/R-3.6.3/library")
packages <- c("twitteR", "openssl","base64enc","tm",
              "openssl","syuzhet","rsconnect",   
              "base64enc","shiny","rtweet","reactable",
              "scales","reshape2","tidyverse","tidytext",
              "RColorBrewer","shinydashboard","RCurl",
              "httr","twitteR","ROAuth",
              "plotly","glue","rvest",
              "wordcloud2","textdata",
              "purrr","httpuv","zoo",
              "xts","lubridate","shinyjs",
              "shinymanager")
### checking if packages are already installed and installing if not
for(i in packages){
  if(!(i %in% installed.packages()[, "Package"])){
    install.packages(i)
  }
  library(i, character.only = TRUE) ## load packages
}

#------------------Importing the libraries-------------------
#________________________________Setting the app credentials_________



# define some credentials
credentials <- data.frame(
  user = c("BNR","mgisa", "shinymanager"), # mandatory
  password = c("Bnr0123","Mgisa08", "12345"), # mandatory
  start = c("2021-04-10"), # optinal (all others)
  expire = c(NA, NA, "2025-12-31"),
  admin = c(FALSE, FALSE,TRUE),
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE
)



#-------------------------Setting a shiny app backbone-------------

#1. Setting user interface (ui) of a shiny app

header <- dashboardHeader(title = span("CENTRAL BANKS TWEETS  AND SENTIMENTS MONITORING", 
                                       style = "color: #753918; font-size: 8px,font-weight: bold;"),
                          titleWidth = '100%', disable = FALSE)
anchor <- tags$a(href='http://www.bnr.rw',
                 tags$img(src='BNRLOGO.png', height='80', width='80'),
                 "CENTRAL BANKS TWEETS AND SENTIMENTS MONITORING",
                 style = "color: #753918; font-size: 12px,font-weight: bold;",
                 
          tags$li(class = "dropdown",
        tags$style(".main-header {max-height: 100px}"),
        tags$style(".main-header .logo {height: 80px}")
       ))

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: #ab892c }"))), 
  anchor,
  class = 'name')

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++ SETTING USER INTERFACE ++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ui <- dashboardPage(header,
  dashboardSidebar(
    sidebarPanel(
      shinyWidgets::setSliderColor(color = "#753918", sliderId = 1),
      h4(style="color:#753918", sliderInput("Tweets_to_Download",
                                            "Tweets Mining:",
                                            min = 500,
                                            max = 18000,
                                            value = 500,
                                            step = 500)),
      h4(style = "color:#753918", selectInput("Input_Hashtag", "Twitter Handle:", c("CentralBankRw",
                                                                                        "BankOfTanzania",
                                                                                        "CBKKenya",
                                                                                        "BOU_Official",
                                                                                        "BanqueCongo",
                                                                                        "BankiNkuru",
                                                                                        "CentralBankJA", #Jamaica
                                                                                        "MAS_sg",#Singapore
                                                                                        "centralbankKY" #USA                                                                                  "BankofZambia"
                                                                                        )))
    
      
      ,width = 0.15
      
     )#,  #Setting Logo in siderbar
    # tags$div(
    #   style="text-align:center; background-color:#fcf7ea",
    #   tags$img(src = "BNRLOGO.png", height = '100', width = '100')
    # )
  #   tags$div( 
  #     HTML('
  #     <p style="color: #753918; font-size: 12pt; font-weight: bold;">
  #     This tool was extensively built using Shiny R framework and HTML/CSS. 
  #     It was designed to be using twitter’ 
  #     Application Platform Interface(API) to collect tweets and perform live and 
  #     real time public emotion and sentiment Analysis on the collection. 
  #     It is reactive and flexible for the other regional central banks upon to the choice of end-user hence get 
  #     informed on the feelings, emotions and sentiment porality
  #     (audience positivity and negativity rates) from the banks Twitter audience.</p>
  #     '
  # )
  # )
  ),
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++ SETTING DASHBOARD BODY +++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++                         +++++++++++++++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dashboardBody(
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.(BNR COLOR https://www.color-hex.com/color/ffffff)
    tags$head(tags$style(HTML("
                              
         .tabbable > .nav > li > a{background-color: #ab892c; color:#753918;font-size: 15px; font-weight: bold; 
                                   padding-top:10px; 
                                   padding-bottom:10px;}
         
         .box.box-solid.box-warning>.box-header {
          color:#753918;font-size: 15px; font-weight: bold; 
  
          background:#89724e
         }
         
         .skin-blue .main-sidebar {
         background-color: #fcf7ea;
         }

         .sidebar {
           background-color: #fcf7ea;
           min-height: 600px;
         }
         
         .box.box-solid.box-header {
                background-color: #ab892c;
         }
         .box.box-solid.box-primary{
          border-bottom-color:#ab892c;
          border-left-color:#ab892c;
          border-right-color:#ab892c;
          border-top-color:#ab892c;
         } 
         
        /* Change the color of the slider input labels */
        .ui-slider .ui-slider-handle {
        background-color: #fcf7ea;
        }

     /* Change the color of the slider input numbers */
    .ui-slider .ui-slider-handle:before {
    color: #000;
    }

        .skin-blue .main-header .logo {
          background-color:#ab892c ;               
        }
        .skin-blue .main-header .logo:hover {
          background-color: #ab892c;
        }
        .main-sidebar {
            background-color: skinblue !important;
        }
        .small-box.bg-yellow { 
        background-color: #AB892C !important; color: #DBA628!important;color: #ab892c!important;
        }
        
        .small-box.bg-aqua { 
        background-color: #DBA628 !important; color: #DBA628!important;color: #fcf7ea!important;
        }
          
        .small-box.bg-green { 
        background-color: #89724E !important; color: #DBA628!important;color: #753918!important;
        }
        
        .small-box.bg-blue { 
        background-color: #89724E !important; color: #DBA628!important;color: #ab892c!important;
        }
        
        .small-box.bg-orange { 
        background-color: #fcf7ea !important; color: #DBA628!important;color: #753918!important;
        }
        
        .chart {
      background-color: #AB892C !important;
      border: none;
        }
      
      .main-header .logo {
        font-family: 'Georgia', Times, 'Times New Roman', serif !important;
        font-weight: bold;
        font-size: 20px;
      }
      
        ")),tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 20px;
      }
    ')))
    ), #3c8dbc,
    tabsetPanel(
      tabPanel(title = "Sentiments",
               fluidRow(
                 valueBoxOutput("value1"),
                 valueBoxOutput("value2"),
                 valueBoxOutput("value3")),            
               fluidRow(  
                 box(
                   title = "WordCloud(Tweet words Uniqueness)"
                   ,status = "warning"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                   ,wordcloud2Output("wordcloud", height = "300px")
                 ),
                 box(
                   title = "Top 10 Tweet words"
                   ,status = "warning"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("top10", height = "300px")
                 )),
               fluidRow(
                 box(
                   title = "Top Positive and Negative Words",
                   status = "warning",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("bing", height = "350px")
                 ),
                 box(
                   title = "Public Emotions, Expressions and Feelings",
                   status = "warning",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("NRC", height = "350px")
                 )),
               fluidRow(
                 box(
                   title = "Sentiment Polarity",
                   status = "warning",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotlyOutput("Polarity", height = "300px")
                   
                 ))
      ),
      #__________________MPFSS2021 Analysis__________________
      tabPanel(title = "MPFSS2022",
               fluidRow(
                 valueBoxOutput("value13"),
                 valueBoxOutput("value14"),
                 valueBoxOutput("value15")),
               fluidRow(  
                 box(
                   title = "Public Reactions"
                   ,status = "warning"
                   #,width = 12
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                   ,plotOutput("MPFSS2021plot", height = "300px")
                 ),
                 box(
                   title = "MPFSS2021 Re-tweets Network"
                   ,status = "warning"
                   #,width = 12
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("MPFSS2021netplot", height = "300px")
                 ),
                 box(
                   title = "Network Interpretation",
                   status = "warning",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   htmlOutput("MPFSS2021Note", height = "300px")
                 )
                 )
      ),
      
      #__________________MPC2022 Analysis__________________
      tabPanel(title = "MPC2022",
               fluidRow(
                 valueBoxOutput("value16"),
                 valueBoxOutput("value17"),
                 valueBoxOutput("value18")),
               fluidRow(  
                 box(
                   title = "Public Reactions"
                   ,status = "warning"
                   #,width = 12
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                   ,plotOutput("MPC2022plot", height = "300px")
                 ),
                 box(
                   title = "MPC2022 Re-tweets Network"
                   ,status = "warning"
                   #,width = 12
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE #plotOutput("netplot",width = "100%")
                   ,plotOutput("MPC2022netplot", height = "300px")
                 ),
                 box(
                   title = "Graph Interpretation",
                   status = "warning",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   htmlOutput("MPC2022Note", height = "300px")
                 )
               )
      ),
      
      #___________
      tabPanel(title = "Descriptives",
               fluidRow(
                 valueBoxOutput("value4"),
                 valueBoxOutput("value5"),
                 valueBoxOutput("value6")),
               fluidRow(
                 valueBoxOutput("value7"),
                 valueBoxOutput("value8"),
                 valueBoxOutput("value9")),
               fluidRow(reactableOutput("tweet_table"))
               ),
      #______________________Trend Analysis
      
      tabPanel(title = "Time Trend",
               fluidRow(
                 valueBoxOutput("value10"),
                 valueBoxOutput("value11"),
                 valueBoxOutput("value12")),            
               fluidRow(  
                 box(
                   title = "Periodic Trend"
                   ,status = "warning"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                   ,plotOutput("Trends", height = "300px")
                 ),
                 box(
                   title = "Top 10 Most Used Words"
                   ,status = "warning"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("top10M", height = "300px")
                 )),
               fluidRow(
                 box(
                   title = "Yearly Dominants Words",
                   status = "warning",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("Dominants", height = "300px")
                 ),
                 box(
                   title = "Tweets Hourly Count",
                   status = "warning",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("Hourly", height = "300px")
                 )),
               fluidRow(
                 box(
                   title = "Time Sentiments ",
                   status = "warning",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("SentiTrends", height = "300px")
                   
                 ))
      ),
      #_______________________________
       tabPanel(title = "Historical Data",
               fluidRow(
                   DT::dataTableOutput("carstable")
                 )),
      
      tabPanel(title = "Documentation",
               fluidRow(
                 box(title = "Overview",
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 htmlOutput("App_Intention", height = "300px")
              
               ),
               box(title = "Components",
                   status = "warning",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   htmlOutput("App_Components", height = "300px")
                   
               )),
      fluidRow(
        box(
          title = "",
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          htmlOutput("About_Author", height = "300px")
        )
    )
    )
    ),
    
    # Footer with a footnote
    
    tags$footer(
      #setting footnote text
      style = "font-style: italic; text-align: center; background-color:#ab892c; 
            color:#753918;font-size: 14px; font-weight: bold;",
      "BNR Copyright © 2023. All rights reserved",
      #setting logo
      tags$div(
        class = "container-fluid",
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-12",
            tags$div(
              class = "text-center",
              tags$a(href = "https://www.facebook.com/CentralBankRw/", target = "_blank",
                     tags$i(class = "fa fa-facebook-official fa-lg", style = "color: #753918")),
              tags$a(href = "https://twitter.com/CentralBankRw/", target = "_blank",
                     tags$i(class = "fa fa-twitter fa-lg", style = "color: #753918")),
              tags$a(href = "https://www.linkedin.com/company/national-bank-of-rwanda-bnr-/mycompany/", target = "_blank",
                     tags$i(class = "fa fa-linkedin fa-lg", style = "color: #753918")),
              tags$a(href = "https://www.youtube.com/channel/UCNL-AuMdkEhr_FnfeT0BKRQ", target = "_blank",
                     tags$i(class = "fa fa-youtube fa-lg", style = "color: #753918")),
              tags$a(href = "https://www.flickr.com/photos/135529030@N06/", target = "_blank",
                     tags$i(class = "fa-brands fa-flickr", style = "color: #753918"))
            )
          )
        )
      )
    )#Closing footer
    
    )#closing body
  )
# Wrap your UI with secure_app
ui <- secure_app(ui)

#2.Setting a server

# Define server logic 
server <- function(input, output) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  #Store API keys
  api_key <- "mC8Usreri3bMzVMDXFcmaxrT6"
  api_secret_key <- "WnRBOjPdY43fIYPhWHdn2ldPtR0wtDAQ9b9AKk9bdbaQi4gbeD"
  access_token<-"1059794292863045632-0VVYXCedOVG7jGyMbujGFDlRyENK0W"
  access_token_secret <- "LAZ87VJpAmqeIuDAZ1fzXJMLtJk4To05Nu3d0FJzd1Vbt"
 
  #set up 
 setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)
  

 
  dataInput <- reactive({
    data <- searchTwitter(input$Input_Hashtag, n = input$Tweets_to_Download, 
                          resultType = "recent", lang = "en")
    
    twListToDF(data)
  })
 
  #__________________________________
  #Create reactive word cloud
  output$wordcloud <- renderWordcloud2({
    ##Word clouds for all tweets
    table_1 <- dataInput() %>% 
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
  })
  
  #Build value box1
  output$value1 <- renderValueBox({
    n <- dataInput() %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      group_by(word, sentiment) %>% 
      count(word, sentiment, sort = T) %>% 
      ungroup() %>% 
      group_by(sentiment) %>% 
      summarise(n = sum(n)) %>% 
      mutate(n = round((n/sum(n))*100, 2)) %>% 
      filter(sentiment == "positive")
    
    n <- n[,2] # 86.3
    
    
    valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
             icon = icon("smile", lib ="font-awesome" ), color = "orange")
  })
  
  output$value2 <- renderValueBox({
    n <- dataInput()[,1:16] %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      group_by(word, sentiment) %>% 
      count(word, sentiment, sort = T) %>% 
      ungroup() %>% 
      group_by(sentiment) %>% 
      summarise(n = sum(n)) %>% 
      mutate(n = round((n/sum(n))*100, 2)) %>% 
      filter(sentiment == "negative")
    
    n <- n[,2] #13.6
    
    
    valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
             icon = icon("angry", lib ="font-awesome" ), color = "orange")
  })
  #_________________________________________
  
  output$top10 <- renderPlot({
    topwords <-  dataInput()[,1:16] %>% 
      mutate(text = tolower(text)) %>% 
      mutate(text = gsub("rt", "", text)) %>% 
      mutate(text = gsub("https","", text)) %>% 
      mutate(text = gsub("t.co", "", text)) %>% 
      mutate(text = gsub("covid", "", text)) %>% 
      mutate(text = removeNumbers(text)) %>% 
      mutate(text = gsub("19", "", text)) %>% 
      mutate(text = gsub("ppl", "people", text)) %>% 
      mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
      mutate(text = gsub("en", "", text)) %>% 
      mutate(rowmumber = row_number()) %>%#mutate row numbers
      mutate(text = str_remove(text, "rt")) %>% 
      unnest_tokens(word, text) %>%  #unnest words
      anti_join(stop_words) %>% #removes stop words
      count(word, sort = T) %>%#count most occurring words
      top_n(10) #select top 10
    
    ggplot(topwords, aes(reorder(word, n), n, fill = word)) + #piped into ggplot
      geom_bar(stat = "identity", show.legend = F) + coord_flip() +
      labs(x = "Most Frequent Words", y = "Number of Appearence", caption = "BNR_mgisa") + theme_bw() +
      theme(axis.title.x = element_text(face ="bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15),
            axis.text = element_text(face = "bold"))
  })
  
  
  output$value3 <- renderValueBox({
    
    tweets_count <- dataInput() %>% 
      nrow()
    
    
    valueBox(tweets_count, subtitle = "Total Tweets", 
             icon = icon("chart-bar", lib ="font-awesome" ), color = "orange")
  })
  
  output$bing <- renderPlot({
    pos_vs_neg <- dataInput()[,1:16] %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      mutate(text = gsub("fidelity", " ", text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      group_by(word, sentiment) %>% 
      count(word, sentiment, sort = T) %>% 
      ungroup() %>% 
      group_by(sentiment) %>% 
      top_n(10)
    
    ggplot(pos_vs_neg, aes(reorder(word, n), n, fill = word)) +
      geom_col(show.legend = F) +
      facet_wrap(~sentiment, scales = "free_y") +
      coord_flip() + 
      labs(y = "Count", x = "Most Frequent Words",caption = "BNR_mgisa") +
      theme_bw()+
      #____________
      theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
            axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
            axis.title.x = element_text(size = 15, angle = 90,vjust = 5, face = "bold",colour = "black"),
            axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black")
      )
    
    
  })
  
  output$Polarity <- renderPlotly({
    data_format <- dataInput() %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      mutate(row_id = row_number()) %>% 
      mutate(text = tolower(text)) %>% 
      mutate(text = gsub("brt", "", text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      count(sentiment, row_id, screenName) %>% 
      spread(sentiment, n, fill = 0) %>%  # made data wide rather than narrow
      mutate(sentiment = positive - negative)
    
    All_banks <- dataInput() %>%
      mutate(row_id = row_number())
    
    data_format <- data_format %>% 
      left_join(All_banks, by = "row_id")
    
    label_wrap <- label_wrap_gen(width = 60)
    
    data_format2 <- data_format %>% 
      rename(screenName = screenName.x) %>% 
      select(-screenName.y) %>% 
      mutate(text_formatted = str_glue("Row ID: {row_id}
                                   Screen Name: {screenName}
                                   Text: {label_wrap(text)} "))
    
    data_format3<- data_format2 %>% 
      select(1:5, "text_formatted")
    
    
    ggplotly(data_format3 %>% 
               ggplot(aes(row_id, sentiment)) +
               geom_line(color= "black", alpha = 0.5) +
               geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
               geom_hline(aes(yintercept = mean(sentiment), color = "blue")) +
               geom_point(aes(text = text_formatted), color = "orange", shape = 5) +
               geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "orange") +
               geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "orange") +
               theme_bw() +
               labs(y = "sentiment score", x = "Twitter User"),
             tooltip = "text") %>% 
      layout(
        xaxis = list(
          rangeslider = list(type = "date")
        )
      )
  })
  #_________________________Setting data to turn statistics
  #Bankdata <- reactive({
    
    #userdf <- lookup_users(input$Input_Hashtag)
    
    #users_data(userdf)
  #})
  
  #Bankdata<-read.csv("Bankdata.csv")
  
  #Bankdata <- reactive({return(tbl_df(Bankdata) %>% 
                                   #filter(Twitter_screen_name %in% input$Input_Hashtag))})
  #filter(continent %in% input$continent
  
  Bankdata <-function(input) {
             reactive({
               rio::import(here::here('data', "Bankdata.csv"))%>%
            filter(Twitter_screen_name %in% input$Input_Hashtag)
    })
  }
      #Build value box for Account profile
  output$value4 <- renderValueBox({
    
    valueBox(format(sum(Bankdata(input)()$Listed),big.mark = "," ),
             subtitle = "Total Subscribers (Popularity Metric)", 
             icon = icon("chart-line", lib ="font-awesome" ), color = "orange")
  })

  output$value5 <- renderValueBox({
  valueBox(format(sum(Bankdata(input)()$Statuses),big.mark = ","), 
             subtitle = "Total Number of Genuine Tweets",
             icon = icon("chart-bar", lib = "font-awesome"),
             color = "orange")
  })
  
  output$value6 <- renderValueBox({
    retweet_count<-dataInput() %>% 
      mutate(recovery = retweetCount)
    
    valueBox(format(sum(retweet_count$recovery), big.mark=","), 
             subtitle = "Total Number of Re-twitted ",
             icon = icon("stats", lib = "glyphicon"),
             color = "orange")
  })
  
  make_url_html <- function(url) {
    if(length(url) < 2) {
      if(!is.na(url)) {
        as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
      } else {
        ""
      }
    } else {
      paste0(map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
    }
  }
  
  tweet_table_data <- reactive({
    req(dataInput())
    dataInput() %>% 
      rename(user_id = id,
             created_at = created,
             screen_name= screenName,
             favorite_count = favoriteCount,
             retweet_count = retweetCount,
             urls_expanded_url = statusSource) %>% 
      mutate(status_id = user_id) %>% 
      select(user_id, created_at, status_id, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })
  
  output$tweet_table <- renderReactable({
    reactable(tweet_table_data(), 
              filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
              showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 50, 75, 100, 200), 
              columns = list(
                DateTime = colDef(defaultSortOrder = "asc"),
                User = colDef(defaultSortOrder = "asc"),
                Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                URLs = colDef(html = TRUE)
              )
    )
  })
  #Build the NRC sentiment for tweets
  output$NRC <- renderPlot({ height = "300px"
     
    #nrc tweet analysis
    nrc <- dataInput() %>% 
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
       labs(x = "Sentiments", y = "Total word Count",caption = "BNR_mgisa") +
      scale_fill_manual(values = pal,name="Legend")+
      theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
            axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
            axis.title.x = element_text(size = 15, angle = 90,vjust = 5, face = "bold",colour = "black"),
            axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black")
              )
  })
  
  output$value7 <- renderValueBox({
    
    valueBox(format(sum(Bankdata(input)()$Followers), big.mark = ","),
             subtitle = "Total Followers",
             icon = icon("stats", lib = "glyphicon"),
             color = "orange")
  })
  output$value8 <- renderValueBox({
    
    valueBox(format(sum(Bankdata(input)()$Friends),big.mark = "," ),
                    subtitle = "Total Friends",
                    icon = icon("stats", lib = "glyphicon"),
                    color = "orange")
    
  })
  output$value9 <- renderValueBox({
    
    valueBox(format(sum(Bankdata(input)()$Favourites), big.mark = ","), 
                    subtitle = "Total Favorited Tweets",
             icon = icon("stats", lib = "glyphicon"),
             color = "orange")
  })

  output$value10 <- renderValueBox({
  
  valueBox(Bankdata(input)()$Central_Bank_name, subtitle = "Official Bank's Name", 
           icon = icon("car", lib ="font-awesome" ), color = "orange") #icon("cog", lib = "glyphicon")
})

output$value11 <- renderValueBox({
  valueBox(Bankdata(input)()$Account_created_at, subtitle = "Twitter Account Created Date", 
           icon = icon("calendar", lib ="font-awesome" ), color = "orange")
})

output$value12 <- renderValueBox({
  valueBox(Bankdata(input)()$Location, subtitle = "Central Bank Location", 
           icon = icon("home", lib ="font-awesome" ), color = "orange")
})

dataInput1<- rio::import(here::here('data',"BNR_Tweets2016_Oct2021.csv"))


output$Trends <- renderPlot({
  dataInput1 %>%
    separate(created_at, into = c("date", "time"), sep = " ") %>%
    dplyr::mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>% 
    dplyr::mutate(year = year(date)) %>% 
    dplyr::mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
    dplyr::mutate(yearmon = as.yearmon(yearmon)) %>% 
    group_by(yearmon) %>% 
    count() %>% 
    ggplot(., aes(yearmon, n, group = 1)) +
    geom_line(size = 1, color = "#ab892c") +
    geom_point(color = "sienna4") +
    geom_smooth(method = "lm", se = F) +scale_fill_manual(values="sienna4")+
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
})


output$top10M <- renderPlot({#See the top 10 words used in his tweets.
  pal1<-RColorBrewer::brewer.pal(n=10,name="Set3")
   dataInput1 %>% 
    separate(created_at, into = c("date", "time"), sep = " ") %>% 
    mutate(text = tolower(text)) %>% 
    mutate(text = gsub("brt", "", text)) %>% 
    mutate(text = gsub("t.co", "", text)) %>% 
    mutate(text = gsub("https", "", text)) %>% 
    mutate(text = gsub("amp", "", text)) %>% 
    mutate(text = gsub("ÃÂ¢", "", text)) %>% 
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
    labs(x = "Most Used Words",y="Appearence Frequency",caption = "BNR_mgisa")+
    theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
          axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
          axis.title.x = element_text(size = 15, angle = 360,vjust = 5, face = "bold",colour = "black"),
          axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black"),
          legend.position = "none")
  
})
#__________Dominants Words over Years

output$Dominants<-renderPlot({
    dataInput1%>% 
    separate(created_at, into = c("date", "time"), sep = " ") %>% 
    mutate(date =  as.POSIXct(date, format = "%m/%d/%Y")) %>% 
    mutate(year = year(date)) %>% 
    mutate(text = tolower(text)) %>% 
    mutate(text = gsub("brt", "", text)) %>% 
    mutate(text = gsub("t.co", "", text)) %>% 
    mutate(text = gsub("https", "", text)) %>% 
    mutate(text = gsub("amp", "", text)) %>% 
    mutate(text = gsub("Ã¢", "", text)) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    group_by(year) %>% 
    count(word, sort = T) %>% 
    arrange(desc(n)) %>% 
    mutate(word = reorder(word, n)) %>% 
    slice(1:5) %>% 
    ungroup()%>%
    mutate(Rate=round((n/sum(n))*100,digits = 2))%>%
    ggplot(., aes(reorder_within (word, n, year), n, fill = as.character(year))) +
    geom_col(show.legend = F) +
    coord_flip() +geom_text(aes(label=str_c(Rate,"%")),vjust=0.1,angle=360,size=3,color="black")+
    facet_wrap(~year, ncol =  2, scales = "free") + theme_bw() +
    labs(x = "Most Dominants Used Words", y="Appearence Frequency",caption = "BNR_mgisa")+
    theme(axis.text.x = element_text(angle = 360,face ="bold", size = 10,colour = "black"),
          axis.text.y = element_text(angle = 360,face = "bold", size = 8,colour = "black"),
          axis.title.x = element_text(size = 15, angle = 360,vjust = 5, face = "bold",colour = "black"),
          axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black"),
          legend.position = "none")
  
})
#_____________________Hourly Trends


output$Hourly<-renderPlot({
    dataInput1 %>% 
    separate(created_at, into = c("date", "time"), sep = " ") %>% 
    mutate(time = hm(time)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(date =  as.POSIXct(date, format = "%m/%d/%Y")) %>% 
    mutate(year = year(date)) %>% 
    group_by(hour, year) %>%
    count()%>% 
    ggplot(., aes(factor(hour), n, fill = as.character(year))) +
    geom_col(show.legend = F) +
    facet_wrap(~year, ncol = 2, scales = "free") +theme_bw()+
    labs(x = "Hour of Day", y="Total Number of tweets",caption = "BNR_mgisa")+
    theme(axis.text.x = element_text(angle = 360,face ="bold", size = 10,colour = "black"),
          axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
          axis.title.x = element_text(size = 10, angle = 360,vjust = 5, face = "bold",colour = "black"),
          axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black"),
          legend.position = "none")
  
})
#______Sentiments Trends________

output$SentiTrends <- renderPlot({
  #Yearly trend of sentiments
    data_format <- dataInput1 %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      mutate(row_id = row_number()) %>% 
      mutate(text = tolower(text)) %>% 
      mutate(text = gsub("brt", "", text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      count(sentiment, row_id, screen_name) %>% 
      spread(sentiment, n, fill = 0) %>%  # made data wide rather than narrow
      mutate(sentiment = positive - negative)
    
    All_banks <- dataInput1 %>%
      mutate(row_id = row_number())
    
    data_format <- data_format %>% 
      left_join(All_banks, by = "row_id")
    
    #convert negative column to negative values
    data_format2<- data_format %>% 
      mutate(negative = negative * (-1))
    
    data_format2%>% 
      separate(created_at, into = c("date", "time"), sep = " ") %>%
      mutate(date =  as.POSIXct(date, format = "%m/%d/%Y")) %>% 
      mutate(year = year(date)) %>% 
      mutate(yearmon =  format(as.Date(date), "%Y-%m")) %>% 
      mutate(yearmon = as.yearmon(yearmon)) %>% 
      group_by(yearmon) %>% 
      summarise(negative = sum(negative), positive = sum(positive)) %>% 
      gather(key = "sentiment", value = "n", 2:3) %>% 
      ggplot(., aes(yearmon, n, group = sentiment)) +
      geom_line( aes(colour=sentiment), size = 0.8) + theme_bw() + 
      scale_colour_manual(values = c(positive="#ab892c",negative="#753918"))+
      scale_x_yearmon(format = "%b %Y", n = 10) +
      labs(x = "Date",y="Tweet Counts",caption = "BNR_mgisa")+
      theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
            axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
            axis.title.x = element_text(size = 15, angle = 360,vjust = 5, face = "bold",colour = "black"),
            axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black"),
            legend.position = "bottom")
})#_______________
#__________Step10: Rendering Data table________
Dataframe<-dataInput1%>%
  select(created_at, screen_name,text,source,is_retweet,favorite_count,retweet_count,
         followers_count,friends_count,listed_count,statuses_count)
output$carstable <- DT::renderDataTable(
  DT::datatable(data=Dataframe,
                extensions = "Buttons",
                options = list(
                  dom="lfrtBip",#Bottom of table      #"Blfrtip"(Top of table),
                  buttoms=
                    list("copy",list(
                      extend = "collection",
                      buttons=c("copy","csv","excel","pdf","print"),
                      text="Download"
                    )),                     # End of buttons customization
                  lengthMenu=list(c(5,20,50,100,-1),         #Declare values
                                  c(5,20,50,100, "All")      #Declare titles
                  ),                          #End of lengthMenu Customization
                  pageLength=5
                ) #End of options
  )#End of dataTables
) #End of REnderDataTable
#})


#_______________________________HTML OUTPUTS_____________
output$App_Intention<-renderUI({
  tags$div( 
    HTML('
      <p style="color:black; font-size: 12pt">
      This tool was extensively built using Shiny R framework and HTML/CSS. 
      It was designed to be using twitter’ 
      Application Platform Interface(API) to collect tweets and perform live and 
      real time public emotion and sentiment Analysis on the collection. 
      It is reactive and flexible for the other regional central banks upon to the choice of end-user hence get 
      informed on the feelings, emotions and sentiment porality
      (audience positivity and negativity rates) from the banks Twitter audience.</p>
      
      <p style="color:black; font-size: 12pt">
      
      This work would not be possible without using a state-of-art Artificial Intelligence (AI) specifically
      Natural Language Processing (NLP). We constructed a set of sentiment and emotion indicators through the integration 
      of SentiWordNet and NRC Emotion Lexicon dictionaries to classify and categorize sentiments and emotions 
      behind the Bank’s tweet feeds. Hence, the public perception and expression towards Twitter policy communication of
      central banks are extensively identified and quantified. From this, it is easier than before to monitor and control the
      policy communication on Twitter.
      
      <h3 style="color:#753918; font-family:courier;text-align:center; font-weight: bold;"> 
      Application Development </h3>
      
      <p style="color:black; font-size:12pt">
     
    
      This app was developed solely by Data Science Division in the Statistics Department at National Bank of Rwanda,BNR.
      The team members enjoy to work on several data-driven researches and projects to reveal the insightful information hidden from
      diverse datasets. it is passionate in Data Engineering, Artificial Intelligence, Machine Learning, and big data technologies and practices.
      Its vision is to make a relevant change to community through data-driven and evidence-based researches.
      
       <h5 style="color:#753918; font-family:courier;text-align:center; font-weight: bold;"> 
       Contact: bnrdatascience@gmail.com </h5>
      
      
      
       </p>'
      
    ))
})
#______________________________________
output$App_Components<-renderUI({
  tags$div(
    HTML('
      <p style=" color:black; font-size: 11pt">
      This application is composed by different components. Each and every one has its specific task.</p>
      
         <ul style="list-style-type:disc;">
     
  <li> <h4 style="color:#753918;"> App Slider and Input Twitter Username</h4> </li> 
  
  This serves to control how many tweets should be returned. The default amount is 500 and the highest is 18,000. 
  It is noteworthy that the higher the tweets, the longer it may take for the app to load. It primarily requires 
  the premium or enterprise API to extract more and more tweets. But here we have used the standard free API
  which might result to extract small number of tweets (7-10 days). Moving forward, the default Twitter username is for 
  BNR (CentralBankRw) and you can even select other username of your choice.
  
  
  <li><h4 style="color:#753918;font-weight: bold;"> App Tabs </h4> </li>
  
  On the default tab, you have Result of the Sentiment Analysis.This includes;
  interactive worldcloud, emotions, Top 10 words occuring in the retrieved tweets,
  Top 10 most positive and most negative words used and 
  Sentiment Polarity shows the extreme negativity, positivity or neutrality of each tweet. 
  Analyzed using the Bing Lexicon.
  
  <p style="color:black; font-size: 11pt">
  
  On the next tab, you have the real-time statistics. It provides a general and simple overview of
  the username status and live tweets interactive table.It presents each tweets in a searchable and interactive table. 
  You can search the entire tweets or column wise to find what you are interested in. 
 The symboll >> is a clickable link to where any tweet originates from on Twitter itself.
 
 <p style="color:black; font-size: 11pt">
  The next tab indicates the historical trend analysis and repective public expressions and sentiments evolution.
  This follows by the tab which presents the historical data that might be downloaded, printed and copied for further analysis by 
  any end-users. Then finally, the about app page presents a simple description, intention, methodology and how to use efficiently this
  Tweets and Public sentiments Monitoring Tool.
  
  </p>'
         
    ))
})
#________________________________
output$About_Author<-renderUI({
  tags$div( 
    HTML('
    <h2 style="color:#753918; font-family:courier;text-align:center;font-weight: bold;"> 
      ACKNOWLEDGEMENT </h2>
      <p style="color:black; font-size: 12pt">
      
        We are deeply grateful to National Bank of Rwanda, BNR for its utmost various supports.
        I am indebted to everyone contributed to make this project happen especially late Prof. Kigabo Thomas for his endless 
        constructive feedback, weighted motivation and encouragements. May almighty God give him eternal rest.

           </p>'
         
    ))
})
#_______________________MPFSS2021 TAB_________________________

Bnr_MPFSS <- rio::import(here::here('data',"MPFSS2021hashtag.csv"))
#Build value box1
output$value13 <- renderValueBox({
  n <- Bnr_MPFSS %>% 
    mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(word, sentiment) %>% 
    count(word, sentiment, sort = T) %>% 
    ungroup() %>% 
    group_by(sentiment) %>% 
    summarise(n = sum(n)) %>% 
    mutate(n = round((n/sum(n))*100, 2)) %>% 
    filter(sentiment == "positive")
  
  n <- n[,2]
  
  
  valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
           icon = icon("smile", lib ="font-awesome" ), color = "orange")
})

output$value14 <- renderValueBox({
  n <- Bnr_MPFSS[,1:16] %>% 
    mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(word, sentiment) %>% 
    count(word, sentiment, sort = T) %>% 
    ungroup() %>% 
    group_by(sentiment) %>% 
    summarise(n = sum(n)) %>% 
    mutate(n = round((n/sum(n))*100, 2)) %>% 
    filter(sentiment == "negative")
  
  n <- n[,2]
  
  
  valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
           icon = icon("angry", lib ="font-awesome" ), color = "orange")
})

output$value15 <- renderValueBox({
  
  MPFSS_tweets_count <- Bnr_MPFSS %>% 
    nrow()
  
  
  valueBox(MPFSS_tweets_count, subtitle = "Total Tweets", 
           icon = icon("chart-bar", lib ="font-awesome" ), color = "orange")
})

#Plotting the MPFSS2021 based public expression

output$MPFSS2021plot<- renderPlot({ height = "300px"

#nrc tweet analysis
MPFSS2021_nrc <- Bnr_MPFSS %>% 
  mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
  mutate(text = tolower(text))

MPFSS2021_nrc1 <- MPFSS2021_nrc$text
#class(nrc)
nrcsent<-get_nrc_sentiment(as.character(MPFSS2021_nrc1))
nrcsent<-as.data.frame(colSums(nrcsent))

sentiments<-c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise",
              "Trust","Negative","Positive")

sentiments<-as.data.frame(sentiments)

dfp<-as.data.frame(nrcsent[,1])

PDF<-cbind( sentiments,dfp)
colnames(PDF)[2]<-paste("Frequency")

MPFSS2021_NRC<-PDF%>%arrange(desc(Frequency))
#PLotting
pal<-brewer.pal(n=10,name = "Set3")
MPFSS2021_NRC%>%mutate(sentiments,
             Rate=round((Frequency/sum(Frequency))*100,digits = 2))%>%
  ggplot(aes(reorder(sentiments,-Frequency),y=Frequency))+ 
  geom_bar(stat = "identity", show.legend = F, aes(fill=sentiments)) + #coord_flip() +
  theme_bw() + geom_text(aes(label=str_c(Rate,"%")),vjust=0.1,angle=360,size=4,color="black")+
  labs(x = "Sentiments", y = "Total word Count") +
  scale_fill_manual(values = pal,name="Legend")+
  theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
        axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
        axis.title.x = element_text(size = 15, angle = 90,vjust = 5, face = "bold",colour = "black"),
        axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black")
  )
})

#MPFSS2021 Retweet Network Analysis

library(igraph)

output$MPFSS2021netplot<- renderPlot({
#selecting Retweet and coresponsing user name
rt_df<-Bnr_MPFSS[,c("screen_name","retweet_screen_name")]
#Source Vertex is the screen name and the target vertex is the retweet screen name

#Removing the none connected nodes
rt_df<-rt_df[-c(42:67),]
#Removing rows with missing values
rt_df_new<-rt_df[complete.cases(rt_df),]
#Convert to a matrix in order to construct network
matrx<-as.matrix(rt_df_new)
#Create a re-tweet network

nw_retweet<-graph_from_edgelist(el=matrx, directed = TRUE)
#There is direct network with 44 edges and 188 networks

#__________VISUALIZATION OF TWITTER NETWORK_____

#Create the basic graph from retweet network

#Reshape network plot to include the users with high followers account
#Create dataframe for followers
followers<-Bnr_MPFSS[,c("screen_name","followers_count") ]
#categorize the higher and lower follow account
#Assign 1 to the account with followers account whose followers is greater than 1000 else assign 0 
followers$follow<-ifelse(followers$followers_count >1000,"1","0")

#Assign External network attribute to re-tweek network

V(nw_retweet)$followers<-followers$follow
#View the vertex attributes
vertex_attr(nw_retweet)
#Changing vertex color in the graph basing on the vertex attributes
#Set the vertex color for the plot
sub_color<-c("lightgreen","tomato")
set.seed(12345) #Fixes the randomness to reproduce the plot at everytime

plot.igraph(nw_retweet,
            asp=9/16,
            # === vertex
            #_____________
            vertex.size=14, #Vertex size
            vertex.color=sub_color[as.factor(vertex_attr(nw_retweet,"followers"))], #color of the vertex
            vertex.label.cex=.6, #Size of the vertex label
            #vertex.label.color="black",
            vertex.shape=c("circle","csquare"),# One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
            #vertex.label=LETTERS[1:10],  # Character vector used to label the nodes
            vertex.label.color=c("black","blue"),
            vertex.label.family="Helvetica",     # Font family of the label (e.g.“Times”, “Helvetica”)
            vertex.label.font=c(1,2,3,4), # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
            #vertex.label.cex=c(0.5,1,1.5), # Font size (multiplication factor, device-dependent)
            vertex.label.dist=0,             # Distance between the label and the vertex
            vertex.label.degree=0 ,# The position of the label in relation to the vertex (use pi)
            #===EDGES
            #_________
            #edge.color=rep(c("red","pink"),5), 
            edge.arrow.size=0.3, #Size of an arrow
            edge.color="#753918", #Color of the network edge
            edge.width=1.0,
            edge.lty=c("solid"),
            layout= layout_with_fr  #layout_randomly,   #layout_with_fr, 
            #main="Retweet Network Behind BNR Tweets on #MPFSS2021"
)

})

#MPFSS2021 Notes
output$MPFSS2021Note<-renderUI({
  tags$div(
    HTML('
      <p style=" color:black; font-size: 11pt">
      This section describes the above MPFSS2021 weighted network (graph) created from tweets together with re-tweets on #MPFSS2021.</p>
      
         <ul style="list-style-type:disc;">
     
  <li> <h4 style="color:#99632f;"> Overview</h4> </li> 
  
  The graph represents the entire twitter user network around #MPFSS2021. A network node is represented by a user hence
  he/she is connected to central node (BNR) as the initial source of post. The distance from any node to the center (centrality and betweenneess) is depending on the
  twitter relationship with BNR; for example followers who are then following lots of other accounts in network.
  
  
  <li><h4 style="color:#99632f;"> Network Analysis </h4> </li>
  
  This analysis has 3 main types of network metrics to consider.
  
  <li><h5 style="color:#99632f;"> Network Models </5> </li>
  
  It describes the relationship with network actors.
  Here, the actors are related to BNR trending info called #MPFSS2021 which is currently retweeted 154 times.
  
  <li><h5 style="color:#99632f;"> Network Key Players </5> </li>
  
  The key actors and top three re-tweeters are Jean Claude Gaga (36 times), Peace Uwase M (12 times), and Tumukunde Geo (8).
  These could be used as medium to re-tweet and disseminate the communicated posts.
  
  IMVAHO NSHYA has been identified as user whose posts(retweets) were re-tweeted most in network. It could be used as key influencer in network.
  to initiate the branding message of BNR.
  
  The top five retweeters who have many followers are RDBRwanda (252,309), Thereal Kwizera (8,839),RCyusa(2,813),
  GNsengiyumva (2,407) and Peace Uwase M(2,233). They could be used as key transporters and disseminators  
  of BNR posts to huge audience at once (their retweeted post reaches so many people at once). 
  
  Finally, lightgreen colors in network represents the users whose followers is less than 1,000 otherwise (tomato) greather than 1,000.
  
  <li><h4 style="color:#99632f;"> Network Tie Strength </4> </li>
  
  This is used to measure the strenght of users relationship. BNR has a strong tie (relationship) with friends and weak tie
  with followers because it either does not know them at all. Contrary to Facebook, Twitter emphasizes weak ties in its function to 
 spread the information on huge audience at once as it shows up on their twitter feeds. With this, it is advised
 to have many followers as possible to reach a huge audience when communicating.
  
  
  </p>'
         
    ))
})
#_______________________MPC 2022 TAB


Bnr_MPC <- rio::import(here::here('data',"Bnr_MPC2022.csv"))
#Build value box1
output$value16 <- renderValueBox({
  n <- Bnr_MPC %>% 
    mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(word, sentiment) %>% 
    count(word, sentiment, sort = T) %>% 
    ungroup() %>% 
    group_by(sentiment) %>% 
    summarise(n = sum(n)) %>% 
    mutate(n = round((n/sum(n))*100, 2)) %>% 
    filter(sentiment == "positive")
  
  n <- n[,2]
  
  
  valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
           icon = icon("smile", lib ="font-awesome" ), color = "orange")
})

output$value17 <- renderValueBox({
  n <- Bnr_MPC[,1:16] %>% 
    mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(word, sentiment) %>% 
    count(word, sentiment, sort = T) %>% 
    ungroup() %>% 
    group_by(sentiment) %>% 
    summarise(n = sum(n)) %>% 
    mutate(n = round((n/sum(n))*100, 2)) %>% 
    filter(sentiment == "negative")
  
  n <- n[,2]
  
  
  valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
           icon = icon("angry", lib ="font-awesome" ), color = "orange")
})

output$value18 <- renderValueBox({
  
  MPC_tweets_count <- Bnr_MPC %>% 
    nrow()
  
  
  valueBox(MPC_tweets_count, subtitle = "Total Tweets", 
           icon = icon("chart-bar", lib ="font-awesome" ), color = "orange")
})

#Plotting the MPC2022 based public expression

output$MPC2022plot<- renderPlot({ height = "300px"

#nrc tweet analysis
MPC_nrc <- Bnr_MPC %>% 
  mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
  mutate(text = tolower(text))

MPC2022_nrc1 <- MPC_nrc$text
#class(nrc)
nrcsent<-get_nrc_sentiment(as.character(MPC2022_nrc1))
nrcsent<-as.data.frame(colSums(nrcsent))

sentiments<-c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise",
              "Trust","Negative","Positive")

sentiments<-as.data.frame(sentiments)

dfp<-as.data.frame(nrcsent[,1])

PDF<-cbind( sentiments,dfp)
colnames(PDF)[2]<-paste("Frequency")

MPC2022_NRC<-PDF%>%arrange(desc(Frequency))
#PLotting
pal<-brewer.pal(n=10,name = "Set3")
MPC2022_NRC%>%mutate(sentiments,
                       Rate=round((Frequency/sum(Frequency))*100,digits = 2))%>%
  ggplot(aes(reorder(sentiments,-Frequency),y=Frequency))+ 
  geom_bar(stat = "identity", show.legend = F, aes(fill=sentiments)) + #coord_flip() +
  theme_bw() + geom_text(aes(label=str_c(Rate,"%")),vjust=0.1,angle=360,size=4,color="black")+
  labs(x = "Sentiments", y = "Total word Count") +
  scale_fill_manual(values = pal,name="Legend")+
  theme(axis.text.x = element_text(angle = 45,face ="bold", size = 10,colour = "black"),
        axis.text.y = element_text(angle = 360,face = "bold", size = 10,colour = "black"),
        axis.title.x = element_text(size = 15, angle = 90,vjust = 5, face = "bold",colour = "black"),
        axis.title.y = element_text(size = 15, angle = 90,vjust = 0.5, face = "bold",colour = "black")
  )
})

#MPC Retweet Activity Network Analysis

library(igraph)

output$MPC2022netplot<- renderPlot({
  #selecting Retweet and coresponsing user name
  rt_df1<-Bnr_MPC[,c("screen_name","retweet_screen_name")]
  #Source Vertex is the screen name and the target vertex is the retweet screen name
  
  #Removing the none connected nodes
  rt_df1<-rt_df1[-c(42:67),]
  #Removing rows with missing values
  rt_df_new<-rt_df1[complete.cases(rt_df1),]
  #Convert to a matrix in order to construct network
  matrx<-as.matrix(rt_df_new)
  #Create a re-tweet network
  
  nw_retweet<-graph_from_edgelist(el=matrx, directed = TRUE)
  #There is direct network with 44 edges and 188 networks
  
  #__________VISUALIZATION OF TWITTER NETWORK_____
  
  #Create the basic graph from retweet network
  
  #Reshape network plot to include the users with high followers account
  #Create dataframe for followers
  followers<-Bnr_MPC[,c("screen_name","followers_count") ]
  #categorize the higher and lower follow account
  #Assign 1 to the account with followers account whose followers is greater than 1000 else assign 0 
  followers$follow<-ifelse(followers$followers_count >1000,"1","0")
  
  #Assign External network attribute to re-tweek network
  
  V(nw_retweet)$followers<-followers$follow
  #View the vertex attributes
  vertex_attr(nw_retweet)
  #Changing vertex color in the graph basing on the vertex attributes
  #Set the vertex color for the plot
  sub_color<-c("lightgreen","tomato")
  set.seed(12345) #Fixes the randomness to reproduce the plot at everytime
  
  plot.igraph(nw_retweet,
              asp=9/16,
              # === vertex
              #_____________
              vertex.size=14, #Vertex size
              vertex.color=sub_color[as.factor(vertex_attr(nw_retweet,"followers"))], #color of the vertex
              vertex.label.cex=.6, #Size of the vertex label
              #vertex.label.color="black",
              vertex.shape=c("circle","csquare"),# One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
              #vertex.label=LETTERS[1:10],  # Character vector used to label the nodes
              vertex.label.color=c("black","blue"),
              vertex.label.family="Helvetica",     # Font family of the label (e.g.“Times”, “Helvetica”)
              vertex.label.font=c(1,2,3,4), # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
              #vertex.label.cex=c(0.5,1,1.5), # Font size (multiplication factor, device-dependent)
              vertex.label.dist=0,             # Distance between the label and the vertex
              vertex.label.degree=0 ,# The position of the label in relation to the vertex (use pi)
              #===EDGES
              #_________
              #edge.color=rep(c("red","pink"),5), 
              edge.arrow.size=0.3, #Size of an arrow
              edge.color="#753918", #Color of the network edge
              edge.width=1.0,
              edge.lty=c("solid"),
              layout= layout_with_fr  #layout_randomly,   #layout_with_fr, 
              #main="Retweet Network Behind BNR Tweets on #MPFSS2021"
  )
  
})

#MPFSS2021 Notes
output$MPC2022Note<-renderUI({
  tags$div(
    HTML('
      <p style=" color:black; font-size: 11pt">
      This section describes the above MPC2022 weighted network (graph) created from tweets together with re-tweets on #MPC2022,
      #BNREngage and #KnowYourCentralBank.</p>
      
         <ul style="list-style-type:disc;">
     
  <li> <h4 style="color:#99632f;"> Overview</h4> </li> 
  
  The graph represents the entire twitter user network around the mentioned HASHTAGS. A network node is represented by a user hence
  he/she is connected to central node (BNR) as the initial source of post. The distance from any node to the center (centrality and betweenneess) is depending on the
  twitter relationship with BNR; for example followers who are then following lots of other accounts in network.
  
  
  <li><h4 style="color:#99632f;"> Network Analysis </h4> </li>
  
  This analysis has 3 main types of network metrics to consider.
  
  <li><h5 style="color:#99632f;"> Network Models </5> </li>
  
  It describes the relationship with network actors.
  Here, the actors are related to BNR trending info called #MPC2022 which has been retweeted 254 times.
  
  <li><h5 style="color:#99632f;"> Network Key Players </5> </li>
  
  The key actors and top five re-tweeters are Samuelcamarade (16 times), AllanKar4 (12 times), EconomicStat_Rw (8),
  munana_gloria (6) and thierryk7 (3).
  These could be used as medium to re-tweet and disseminate the communicated posts.
  
  IMVAHO NSHYA has been identified as user whose posts(retweets) were re-tweeted most in network. It could be used as key influencer in network
  to initiate the branding message of BNR.
  
  The top five retweeters among others who have many followers are rwangombwajRw (100,456), AllanKar4 (5,161), EconomicStat_Rw (1,671),
  samuelcamarade (1,025) and munana_gloria (135). They could be used as key transporters and disseminators  
  of BNR posts to huge audience at once (their retweeted post reaches so many people at once) because they have most potential to spread information fast and widely. 
  
  However, the analysis shows that the rwangombwajRw appeared and tagged many times in re-tweets whose users are followed 
  by many followers and his opinions are widely agreed upon by others and he is connected with the 
  important twitter users in #MPC2022 network.
  
  Finally, lightgreen colors in network represents the users whose followers is less than 1,000 otherwise (tomato) greather than 1,000.
  
  <li><h4 style="color:#99632f;"> Network Tie Strength </4> </li>
  
  This is used to measure the strenght of users relationship. BNR has a strong tie (relationship) with friends and weak tie
  with followers because it either does not know them at all. Contrary to Facebook, Twitter emphasizes weak ties in its function to 
 spread the information on huge audience at once as it shows up on their twitter feeds. With this, it is advised
 to have many followers as possible to reach a huge audience when communicating.
  
  
  </p>'
         
    ))
})


}
# Run the application 
shinyApp(ui = ui, server = server)


