#Load packages: Tidyverse, TidyText

#Import Dataset as "td"

View(td)

#BASE CLEANING#

td <- td %>%
  + rename("ID" = "X1") #First column to ID

td <- td %>%
  + rename("Username" = "X2") #Second column to Username

td <- td %>%
  + rename("Text" = "X3") #Third column to Text

td <- td %>%
  + rename("created_at" = "X4") #Fourth column to created_at

td <- td %>%
  + rename("Time" = "X5") #Fifth column to Time

td <- td[, -c(6:7)] #Remove Geo Coordinates and User Language columns (No Values)

td <- td %>%
  + rename("ReplytoUserID" = "X8") #Eighth Column to ReplytoUserID

td <- td %>%
  + rename("ReplytoScreenName" = "X9") #Ninth Column to ReplytoScreenName

td <- td %>%
  + rename("Source" = "X12") #Twelfth column to Source

td <- td %>%
  + rename("ProfileImageURL" = "X13") #Thirteenth column to ProfileImageURL

td <- td %>%
  + rename("UserFollowers" = "X14", "UserFollowing" = "X15") #Fourteenth and Fifteenth Column to User Followers/Following

td <- td %>%
  + rename("StatusURL" = "X17", "EntitiesSTR" = "X18")

#BASE CLEANING DONE#

#TIDYTEXT FORMAT# 

tdvector <- c(td$Text) #Create Character Vector from Tweets

tdtibble <- tibble(line = 1:43987, text = tdvector) #Create Tibble from Character Vector

tdtibble <- tdtibble %>%
  + unnest_tokens(word, text) #Unnest Tokens (For Sentiment Analysis)

tdtibble <- tdtibble %>%
  + anti_join(stop_words) #Remove all stop words from tibble

#TIDYTEXT FORMAT DONE#

#CLEAN FOR UNNECESSARY WORDS#

tdwordcount <- tdtibble %>%
  + count(word, sort = TRUE) #Create word count table using tibble for comparison

tdwordcount <- tdwordcount [!(tdwordcount$word %in% c("t.co", "https", "rt")), ] #remove URLS and RT text

tdtibble <- tdtibble [! (tdtibble$word %in% c("t.co", "https", "rt")), ] #Do the same for Tibble also

#CLEANING DONE#

#COMMENCE SENTIMENT ANALYSIS#

nrc_sentiments <- get_sentiments("nrc")

#bing_sentiments <- get_sentiments("bing") #Load Bing & nrc sentiment lexicons

nrc_sentiments_pos_neg <- nrc_sentiments %>% 
  filter(nrc_sentiments$sentiment == "positive" | nrc_sentiments$sentiment == "negative") #nrc positive and negative dataset

# td_bing_sentiment <- tdtibble %>%
#   inner_join(bing_sentiments) %>%
#   count(word, sentiment, sort = TRUE) %>%    #Sentiment Analysis using bing lexicon (positive/negative)
#   ungroup()

td_nrc_sentiment <- tdtibble %>%
  + inner_join(nrc_sentiments_pos_neg) %>%
  + count(word, sentiment, sort = TRUE) %>%   #Sentiment Analysis using nrc lexicon (positive/negative)
  + ungroup()


#DONE#

#PRODUCE WORDCLOUDS#
install.packages("wordcloud", "reshape2")

tdtibble %>%
  inner_join(bing_sentiments) %>%
  count(word, sentiment, sort = TRUE) %>%                         #Produce bing wordcloud
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("black", "black"), max.words = 100)

tdtibble %>%
  inner_join(nrc_sentiments_pos_neg) %>%
  count(word, sentiment, sort = TRUE) %>%                         #Produce nrc wordcloud
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("black", "black"), max.words = 100)


  + comparison.cloud(colors = brewer.pal(8, "Dark2"), max.words = 100) #for nicer colours use this syntax after acast, ensure colorspace is installed
    geom_text_repel(segment.size = 0, force = 50) #repel words on cloud to avoid overlapping
    
    
    
    
   tdtibble%>%
      + inner_join(nrc_sentiments_pos_neg) %>%                                #OFFICIAL WORDCLOUD FOR BIDEN
      + count(word, sentiment, sort = TRUE) %>%
      + acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      + comparison.cloud(colors = brewer.pal(5, "Set1"), max.words = 300) %>%
      + geom_text_repel(segment.size = 0, force = 35)
   

   
   #DONE#
   
   #PRODUCE TOP N BAR CHART FOR SENTIMENTS#
   
   TOP20DF <- DFSENTIMENTS %>%
     filter(DF$SENTIMENTCOLUMN == 'Positive') %>% #CREATES TABLE CONSISTING OF TOP 20 POSITIVE SENTIMENTS
     top_n(20)
     
   TOP20DF <- SENTIMENTSDF%>%
     filter(DF$SENTIMENTS == 'negative') %>% #CREATES TABLE CONSISTING OF TOP 20 NEGATIVE SENTIMENTS
     top_n(20)  
   
   #DONE
   
   #CREATE BAR CHARTS
   
   theme_get
   theme_set(theme_classic(base_size = 14))   #set font size for graph
   
   ggplot(top20pos) +
     + geom_bar(aes(x = Word, y = Frequency), stat = 'identity', fill ="deepskyblue1") + #BIDEN POSITIVE BAR CHART
     + ggtitle("Top 20 Positive Sentiments for Biden - 15 Oct 2020 - 7th Nov 2020") +
     + rotate_x_text(angle = 90)
    
   ggplot(top20neg) +                                                            #BIDEN NEGATIVE BAR CHART
      + geom_bar(aes(x = Word, y = Frequency), stat = 'identity', fill ="red2") +
      + ggtitle("Top 20 Negative Sentiments for Biden - 15 Oct 2020 - 7th Nov 2020") +
      + rotate_x_text(angle = 90)
   
   #DONE