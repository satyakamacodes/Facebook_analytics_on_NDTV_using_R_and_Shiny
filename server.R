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

shinyServer(function(input, output){
     
     extract_dates_vec <- reactive({
          
          master_data <- readRDS("SET PATH OF THE ndtvpage.rds")
          
          extract_dates_vec <- master_data$created_time
          
          extract_dates_vec <- gsub("T",
                                    " ",
                                    extract_dates_vec)
          
          extract_dates_vec <- as.POSIXct(extract_dates_vec,
                                          tz="America/Los_Angeles")
          
          extract_dates_vec <- format(extract_dates_vec,
                                      tz="Asia/Kolkata",
                                      usetz = T)
          
          extract_dates_vec <- substr(extract_dates_vec,
                                      start = 1,
                                      stop = 10)
          
          extract_dates_vec <- as.Date(extract_dates_vec,
                                       format = "%Y-%m-%d")
          
          
     })
     
     output$o_min_extract_dates <- renderText({
          
          min_date <- min(extract_dates_vec(), na.rm = T)
          min_date <- format(min_date,
                             format = "%Y-%m-%d")
          
     })
     
     output$o_max_extract_dates <- renderText({
          
          max_date <- max(extract_dates_vec(), na.rm = T)
          max_date <- format(max_date,
                             format = "%Y-%m-%d")
          
     })
     
     #=====================================================================
     
     master_data <- reactive({
          
          master_data <- readRDS("SET PATH OF THE ndtvpage.rds")
          
          master_data <- data.frame(master_data)
          
          master_data$created_time <- substr(master_data$created_time,
                                             start = 1,
                                             stop = 16)
          
          master_data$created_time <- gsub("T",
                                           " ",
                                           master_data$created_time)
          
          master_data$created_time <- as.POSIXct(master_data$created_time,
                                                 tz="America/Los_Angeles")
          
          master_data$created_time <- format(master_data$created_time,
                                             tz="Asia/Kolkata",
                                             usetz = T)
          
          
          master_data$created_time <- as.POSIXct(master_data$created_time,
                                                 format="%Y-%m-%d %H:%M")
          
          
          created_time2 <- master_data$created_time
          
          master_data <- cbind(master_data,
                               created_time2)
          
          
          start_date <- paste(input$date_choose[1], 
                              "00:00:00",
                              "IST",
                              sep = " ")
          
          start_date <- as.POSIXct(start_date,
                                   format = "%Y-%m-%d %H:%M:%S")
          
          
          end_date <- paste(input$date_choose[2], 
                            "23:59:00",
                            "IST",
                            sep = " ")
          
          end_date <- as.POSIXct(end_date,
                                 format = "%Y-%m-%d %H:%M:%S")
          
          
          master_data <- subset(master_data,
                                created_time2 >= start_date & created_time2 <= end_date)

 
     })
     
     # Subsetting only NDTV posts
     
     ndtv_posts <- reactive({
          
          ndtv_posts <- subset(master_data(),
                               from_name == "NDTV")
          
     })
     
     
     output$junkstr1 <- renderPrint({
      
          str(ndtv_posts())
          
     })
     
     #Working with ndtv_posts
     #Value boxes
     
     output$o_ndtv_posts_no.posts <- renderValueBox({
          
          valueBox("Total no. of Posts",
                        length(ndtv_posts()$from_name),
                        icon = icon("clipboard", lib = "font-awesome"))
     })
     
     output$o_ndtv_posts_max.likes <- renderValueBox({
          
          
          
          valueBox("Maximum no. of Likes on a post",
                   max(ndtv_posts()$likes_count, na.rm = T),
                   icon = icon("thumbs-up", lib = "font-awesome"))
     })
     
     
     output$o_ndtv_posts_max.comments <- renderValueBox({
          
          valueBox("Maximum no. of Comments on a post",
                   max(ndtv_posts()$comments_count, na.rm = T),
                   icon = icon("comments", lib = "font-awesome"))
     })
     
     output$o_ndtv_posts_max.shares <- renderValueBox({
          
          valueBox("Maximum no. of Shares on a post",
                   max(ndtv_posts()$shares_count, na.rm = T),
                   icon = icon("share-alt", lib = "font-awesome"))
     })
     
     
     output$o_ndtv_posts_max.loves <- renderValueBox({
          
          valueBox("Maximum no. of Loves on a post",
                   max(ndtv_posts()$love_count, na.rm = T),
                   icon = icon("heart", lib = "font-awesome"))
     })
     
     output$o_ndtv_posts_max.sad <- renderValueBox({
          
          valueBox("Maximum no. of Sad on a post",
                   max(ndtv_posts()$sad_count, na.rm = T),
                   icon = icon("frown", lib = "font-awesome"))
     })
     
     #type = media type
     output$o_prop_type <- renderPlot({
          
          ggplot(ndtv_posts(), aes(type, fill = type))+
                    geom_bar(aes(y=..count../sum(..count..)))+
                    scale_y_continuous(labels=percent_format())+
                    xlab("Types of media shared") +
                    ylab("Proportional frequency") +
                    theme(axis.text=element_text(size=16),
                          axis.title=element_text(size=16,
                                                  face="bold",
                                                  colour = "#000000"),
                          plot.title = element_text(size = 16,
                                                    face = "bold"),
                          panel.background = element_rect(fill= "#FFFFFF"),
                          panel.border = element_rect(colour = "black", 
                                                      fill=NA, 
                                                      size=0.5))
          
          
     })
     
     output$o_type_likes <- renderPlot({
          ggplot(ndtv_posts(), 
                 aes(x = type, y = log(likes_count), fill = type)) +
                         xlab("Types of media shared") +
                         ylab("log(Likes)") +
                         geom_boxplot()+
                         theme(axis.text=element_text(size=16),
                               axis.title=element_text(size=16,
                                                       face="bold"),
                               panel.border = element_rect(colour = "black", 
                                                           fill=NA, 
                                                           size=0.5))
     })
     
     output$o_type_comments <- renderPlot({
          ggplot(ndtv_posts(), 
                 aes(x = type, y = log(comments_count), fill = type)) +
                    xlab("Types of media shared") +
                    ylab("log(Comments)") +
                    geom_boxplot()+
                    theme(axis.text=element_text(size=16),
                          axis.title=element_text(size=16,
                                                  face="bold"),
                          panel.border = element_rect(colour = "black", 
                                                      fill=NA, 
                                                      size=0.5))
     })
     
     output$o_type_shares <- renderPlot({
          ggplot(ndtv_posts(), 
                 aes(x = type, y = log(shares_count), fill = type)) +
                         xlab("Types of media shared") +
                         ylab("log(Shares)") +
                         geom_boxplot()+
                         theme(axis.text=element_text(size=16),
                               axis.title=element_text(size=16,
                                                       face="bold"),
                               panel.border = element_rect(colour = "black", 
                                                           fill=NA, 
                                                           size=0.5))
     })
     
     
     # Working with messages that generate the likes, shares and sad
     
     output$o_ndtv_posts_max_l.msg <- renderText({
          
          ndtv_posts2 <- subset(ndtv_posts(),
                              select = c(likes_count,
                                         message))
          
          ndtv_posts2 <- ndtv_posts2[complete.cases(ndtv_posts2),]
          
          max_l <- which(ndtv_posts2$likes_count == max(ndtv_posts2$likes_count))
          
          ndtv_posts2$message[max_l]
     })
     
     output$o_ndtv_posts_max_c.msg <- renderText({
          
          ndtv_posts3 <- subset(ndtv_posts(),
                              select = c(comments_count,
                                         message))
          
          ndtv_posts3 <- ndtv_posts3[complete.cases(ndtv_posts3),]
          
          max_c <- which(ndtv_posts3$comments_count == max(ndtv_posts3$comments_count))
          
          ndtv_posts3$message[max_c]
     })
     
     
     output$o_ndtv_posts_max_s.msg <- renderText({
          
          ndtv_posts4 <- subset(ndtv_posts(),
                              select = c(shares_count,
                                         message))
          
          ndtv_posts4 <- ndtv_posts4[complete.cases(ndtv_posts4),]
          
          max_s <- which(ndtv_posts4$shares_count == max(ndtv_posts4$shares_count))
          
          ndtv_posts4$message[max_s]
     })
     
     output$o_ndtv_posts_max_lo.msg <- renderText({
          
          ndtv_posts5 <- subset(ndtv_posts(),
                              select = c(love_count,
                                         message))
          
          ndtv_posts5 <- ndtv_posts5[complete.cases(ndtv_posts5),]
          
          max_lo <- which(ndtv_posts5$love_count == max(ndtv_posts5$love_count))
          
          ndtv_posts5$message[max_lo]
     })
     
     
     output$o_ndtv_posts_max_sa.msg <- renderText({
          
          ndtv_posts6 <- subset(ndtv_posts(),
                              select = c(sad_count,
                                         message))
          
          ndtv_posts6 <- ndtv_posts6[complete.cases(ndtv_posts6),]
          
          max_sa <- which(ndtv_posts6$sad_count == max(ndtv_posts6$sad_count))
          
          ndtv_posts6$message[max_sa]
     })
     
     #Time series graphs
     
     output$o_ndtv_posts_ts_likes <- renderDygraph({
          
          df1 <- data.frame(ndtv_posts()$created_time,
                            ndtv_posts()$likes_count)
          
          df2 <- as.xts(df1,
                        order.by = df1[,1])
                        
          dygraph(df2,
                  main = "Number of Likes accross Dates",
                  xlab = "Dates",
                  ylab = "No. of Likes") %>%
               dyRangeSelector() %>%
               dyAxis("y", drawGrid = FALSE)%>%
               dyOptions(colors = "red",
                         drawPoints = T,
                         pointSize = 3)
     })
     
     
     output$o_ndtv_posts_ts_comments <- renderDygraph({
          
          df3 <- data.frame(ndtv_posts()$created_time,
                            ndtv_posts()$comments_count)
          
          df4 <- as.xts(df3,
                        order.by = df3[,1])
          
          dygraph(df4,
                  main = "Number of Comments accross Dates",
                  xlab = "Dates",
                  ylab = "No. of Comments") %>%
               dyRangeSelector() %>%
               dyAxis("y", drawGrid = FALSE)%>%
               dyOptions(colors = "blue",
                         drawPoints = T,
                         pointSize = 3)  
     })
     
     output$o_ndtv_posts_ts_shares <- renderDygraph({
          
          df5 <- data.frame(ndtv_posts()$created_time,
                            ndtv_posts()$shares_count)
          
          df6 <- as.xts(df5,
                        order.by = df5[,1])
          
          dygraph(df6,
                  main = "Number of Shares accross Dates",
                  xlab = "Dates",
                  ylab = "No. of Shares") %>%
               dyRangeSelector() %>%
               dyAxis("y", drawGrid = FALSE)%>%
               dyOptions(colors = "green",
                         drawPoints = T,
                         pointSize = 3) 
     })
     
     output$o_ndtv_posts_ts_loves <- renderDygraph({
          
          df7 <- data.frame(ndtv_posts()$created_time,
                            ndtv_posts()$love_count)
          
          df8 <- as.xts(df7,
                        order.by = df7[,1])
          
          dygraph(df8,
                  main = "Number of Love accross Dates",
                  xlab = "Dates",
                  ylab = "No. of Love") %>%
               dyRangeSelector()%>%
               dyAxis("y", drawGrid = FALSE)%>%
               dyOptions(colors = "magenta",
                         drawPoints = T,
                         pointSize = 3) 
     })
     
     
     output$o_ndtv_posts_ts_sad <- renderDygraph({
          
          df9 <- data.frame(ndtv_posts()$created_time,
                            ndtv_posts()$sad_count)
          
          df10 <- as.xts(df9,
                         order.by = df9[,1])
          
          dygraph(df10,
                  main = "Number of Sad accross Dates",
                  xlab = "Dates",
                  ylab = "No. of Sad") %>%
               dyRangeSelector()%>%
               dyAxis("y", drawGrid = FALSE)%>%
               dyOptions(colors = "black",
                         drawPoints = T,
                         pointSize = 3)
     })
     
     
     #Word cloud
     output$o_ndtv_posts_wc <- renderPlot({
          
          
          ndtv_posts_wc <- str_replace_all(ndtv_posts()$message,
                                           "[^[:alnum:]]",
                                           " ")
          
          ndtv_posts_wc <- removeWords(ndtv_posts_wc, 
                                       c("Read more here",
                                         "Image",
                                         "Instagram",
                                         "Minister",
                                         "Chief"))
          
          ndtv_posts_wc <- data.frame(ndtv_posts_wc,
                                      ndtv_posts()$likes_count)
          
          ndtv_posts_wc <- ndtv_posts_wc[complete.cases(ndtv_posts_wc),]
          
          colnames(ndtv_posts_wc) <- c("comp.messages",
                                       "likes")
          
          s <- ndtv_posts_wc$comp.messages
          
          
          
          s <- as.String(s)
          
          ## Need sentence and word token annotations.
          sent_token_annotator <- Maxent_Sent_Token_Annotator()
          word_token_annotator <- Maxent_Word_Token_Annotator()
          a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
          
          pos_tag_annotator <- Maxent_POS_Tag_Annotator()
          pos_tag_annotator
          a3 <- annotate(s, pos_tag_annotator, a2)
          a3
          ## Variant with POS tag probabilities as (additional) features.
          head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))
          
          ## Determine the distribution of POS tags for word tokens.
          a3w <- subset(a3, type == "word")
          tags <- sapply(a3w$features, `[[`, "POS")
          tags
          table(tags)
          ## Extract token/POS pairs (all of them): easy.
          a <- sprintf("%s/%s", s[a3w], tags)
          
          a
          
          words <- s[a3w]
          
          b <- data.frame(words, tags)
          
          NNPwords <- subset(b,
                             tags == "NNP")
          
          top_talking <- sort(table(NNPwords$words),
                              decreasing = T)
          
          top_talking <- top_talking[1:5]
          
          barplot(prop.table(top_talking)*100,
                  col = rainbow(length(top_talking)),
                  ylim = c(0,
                           max(prop.table(top_talking)*100)*1.5),
                  ylab = "Proportional frequency",
                  xlab = "Topics/personalities",
                  cex.axis = 1.2,
                  cex.names = 1.5,
                  cex.lab = 1.8)
          
          
          
     })
     
     #Text summary
     
     output$o_ndtv_posts_summary <- renderText({
          
          ndtv_posts_sm <- iconv(ndtv_posts()$message,
                               "ASCII", "UTF-8", sub="")
          
          ndtv_posts_sm <- as.character(str_replace_all(ndtv_posts_sm, 
                                                      "[^[:alnum:]]", 
                                                      " "))
     
          ndtv_posts_sm <- paste(ndtv_posts_sm, collapse = " ")
          
          summary <- genericSummary(ndtv_posts_sm,
                                    k = 5)
          
     })
     
     
     #Working with Sentiment analysis
     
     sentiment <- reactive({
          
          sent <- data.frame(words = as.character(ndtv_posts()$message),
                             date = ndtv_posts()$created_time,
                             likes = ndtv_posts()$likes_count,
                             comments = ndtv_posts()$comments_count,
                             shares = ndtv_posts()$shares_count,
                             types = ndtv_posts()$type,
                             stringsAsFactors = F)
          
          sent <- sent[complete.cases(sent),]
          
          pos.words <- scan("SET PATH OF THE FILE positive.csv",
                            what = "character")
          
          pos.words <- as.character(str_replace_all(pos.words, 
                                                    "[^[:alnum:]]", 
                                                    " "))
          
          
          neg.words <- scan("SET PATH OF THE FILE negative.csv",
                            what = "character")
          
          neg.words <- as.character(str_replace_all(neg.words, 
                                                    "[^[:alnum:]]", 
                                                    " "))
          
          
          posWords <- pos.words
          negWords <- neg.words
          
          
          wordsDF<- data.frame(words = posWords, 
                               value = 1,
                               stringsAsFactors=F)
          
          wordsDF<- rbind(wordsDF,
                          data.frame(words = negWords, 
                                     value = -1))
          
          wordsDF$lengths<-unlist(lapply(wordsDF$words, 
                                         nchar))
          
          wordsDF<-wordsDF[ order(-wordsDF[,3]),]
          
          scoreSentence <- function(sentence){
               score<-0
               for(x in 1:nrow(wordsDF)){
                    count<-length(grep(wordsDF[x,1],sentence))
                    if(count){
                         score<-score + (count * wordsDF[x,2])
                         sentence<-sub(wordsDF[x,1],'',sentence)
                    }
               }
               score
          }
          
          
          
          SentimentScore <- unlist(lapply(sent$words, 
                                          scoreSentence))
          
          mat <- cbind(sent, SentimentScore)

          mat
          
          
     })
     
     
     output$o_sentiment_ts_plots <- renderDygraph({
          
          f <- list(family = "Courier New, monospace",
                    size = 18,
                    color = "black")
          
          x <- list(title = "Dates",
                    titlefont = f)
          y <- list(title = "Sentiment scores",
                    titlefont = f)

          p <- plot_ly(x = ~sentiment()$date,
                       y = ~sentiment()$SentimentScore,
                       mode = "lines",
                       marker = list(size = 10,
                                     color = c('red'),
                                     line = list(color = "black",
                                                 width = 1)))%>%
               layout(xaxis = x, yaxis = y)
          
          p
     })
     
     
     output$o_sentiment_boxplots_likes <- renderPlot({
          
          sts <- boxplot.stats(sentiment()$likes)$stats
          
          ggplot(sentiment(),
                 aes(x = as.factor(SentimentScore),
                     y = likes,
                     fill = as.factor(SentimentScore))) +
               xlab("Sentiment scores") +
               ylab("Likes") +
               geom_boxplot()+
               theme(axis.text=element_text(size=16),
                     axis.title=element_text(size=16,
                                             face="bold"))+
               scale_colour_discrete(name = "Sentiment")+
               coord_cartesian(ylim = c(sts*1.05,sts/1.05))

     })
     output$o_sentiment_boxplots_comments <- renderPlot({
          
          sts <- boxplot.stats(sentiment()$comments)$stats
          
          ggplot(sentiment(), 
                 aes(x = as.factor(SentimentScore), 
                     y = comments,
                     fill = as.factor(SentimentScore))) +
               xlab("Sentiment scores") +
               ylab("Comments") +
               geom_boxplot()+
               theme(axis.text=element_text(size=16),
                     axis.title=element_text(size=16,
                                             face="bold"))+
               coord_cartesian(ylim = c(sts*1.05,sts/1.05))
          
     })
     
     output$o_sentiment_boxplots_shares <- renderPlot({
          
          sts <- boxplot.stats(sentiment()$shares)$stats
          
          ggplot(sentiment(), 
                 aes(x = as.factor(SentimentScore), 
                     y = shares,
                     fill = as.factor(SentimentScore))) +
               xlab("Sentiment scores") +
               ylab("Shares") +
               geom_boxplot()+
               theme(axis.text=element_text(size=16),
                     axis.title=element_text(size=16,
                                             face="bold"))+
               coord_cartesian(ylim = c(sts*1.05,sts/1.05))
          
     })
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     

     
          
          
          
     })