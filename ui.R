closeAllConnections()
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               shinydashboard,
               Rfacebook,
               data.table,
               scales,
               ggplot2,
               dygraphs,
               magrittr,
               stringi,
               stringr,
               RTextTools,
               tm,
               wordcloud,
               RColorBrewer,
               LSAfun,
               tidyverse,
               tidytext,
               plotly,
               xts,
               NLP,
               openNLP,
               tm,
               openNLPmodels.en)

shinyUI(
     fluidPage(
          
          #This below tag changes the overall color of the app
          tags$head(tags$style(
               HTML('
                    body, input, button, select { 
                    font-family: "Helvetica";
                    background-color: #faebd7;
                    }'))),
          
          titlePanel(
               h1("Facebook analytics on NDTV data",
                  style = "color: #000080")
          ),
          
          sidebarLayout(
               
               sidebarPanel(
                    width = 2,
                    
                    #This tag changes the backgound color of sidebar panel
                    tags$style(".well {background-color:#ffff7f;}"),
                    
                    h5("In your dataset, your records exists:",
                       style = "color: blue"),
                    
                    h5("from", 
                       verbatimTextOutput("o_min_extract_dates")),
                    
                    h5("to", 
                       verbatimTextOutput("o_max_extract_dates")),
                    
                    dateRangeInput("date_choose",
                                   h4("Choose your date range",
                                      style = "color: #323232"),
                                   start = '2018-01-01',
                                   end = '2018-02-28',
                                   startview = "year",
                                   width = "100%")
               ),
               
               mainPanel(
                    
                    width = 9,
                    
                    tabsetPanel(
                         
                         
                         tabPanel(h4("Basic info",
                                     style = "color: blue"),
                                  valueBoxOutput("o_ndtv_posts_no.posts"),
                                  valueBoxOutput("o_ndtv_posts_max.likes"),
                                  valueBoxOutput("o_ndtv_posts_max.comments"),
                                  valueBoxOutput("o_ndtv_posts_max.shares"),
                                  valueBoxOutput("o_ndtv_posts_max.loves"),
                                  valueBoxOutput("o_ndtv_posts_max.sad"),
                                  
                                  br(),
                                  h4("Types of media share"),
                                  plotOutput("o_prop_type"),
                                  
                                  br(),
                                  br(),
                                  
                                  fluidRow(
                                       splitLayout(cellWidths = c("33.3%", 
                                                                  "33.3%",
                                                                  "33.3%"), 
                                                   plotOutput("o_type_likes"), 
                                                   plotOutput("o_type_comments"),
                                                   plotOutput("o_type_shares"))
                                  )
                                  
                                  
                                  ),
                         
                         tabPanel(h4("Special posts",
                                     style = "color: #800080"),
                                  h4("Most Liked Post",
                                     style = "color: magenta"),
                                  verbatimTextOutput("o_ndtv_posts_max_l.msg"),
                                  
                                  br(),
                                  br(),
                                  
                                  h4("Most Commented upon Post",
                                     style = "color:#008000"),
                                  verbatimTextOutput("o_ndtv_posts_max_c.msg"),
                                  
                                  br(),
                                  br(),
                                  
                                  h4("Most Shared Post",
                                     style = "color:#0000cc"),
                                  verbatimTextOutput("o_ndtv_posts_max_s.msg"),
                                  
                                  
                                  br(),
                                  br(),
                                  
                                  
                                  h4("Most Loved Post",
                                     style = "color:#FF00FF"),
                                  verbatimTextOutput("o_ndtv_posts_max_lo.msg"),
                                  
                                  
                                  br(),
                                  br(),
                                  
                                  
                                  h4("Most Sad-enning Post",
                                     style = "color:#000000"),
                                  verbatimTextOutput("o_ndtv_posts_max_sa.msg")),
                         
                         tabPanel(h4("Time series graphs",
                                     style = "color: #ff4081"),
                                  dygraphOutput("o_ndtv_posts_ts_likes"),
                                  dygraphOutput("o_ndtv_posts_ts_comments"),
                                  dygraphOutput("o_ndtv_posts_ts_shares"),
                                  dygraphOutput("o_ndtv_posts_ts_loves"),
                                  dygraphOutput("o_ndtv_posts_ts_sad")),
                         
                         tabPanel(h4("Key topics and personalities",
                                     style = "color: black"),
                                  actionButton("btn2", "Plot will take some time to display"),
                                      h4("What news is being shared",
                                     style = "color: #a500ff"),
                                  plotOutput("o_ndtv_posts_wc")),
                         
                         tabPanel(h4("Sentiment analysis",
                                     style = "color: red"),
                                  actionButton("btn2", "Plot will take some time to display"),
                                  plotlyOutput("o_sentiment_ts_plots"),
                                  
                                  br(),
                                  br(),
                                  
                                  plotOutput("o_sentiment_boxplots_likes"),
                                  plotOutput("o_sentiment_boxplots_comments"),
                                  plotOutput("o_sentiment_boxplots_shares")
                                  
                                  )
                    )
               )
          )
     )
)