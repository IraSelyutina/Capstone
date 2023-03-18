library(shiny)
library(dplyr)
library(stringr)
library(stringi)

# Data loading
dfTrain1 =  read.csv("./Unigram.csv")
dfTrain2 =  read.csv("./Bigram.csv")
dfTrain3 =  read.csv("./Trigram.csv")

dfTrain1<-dfTrain1 %>% mutate(word1 =  word(word_right, 1)) 
dfTrain2<-dfTrain2 %>%  mutate(word1 =  word(word_right, 1))%>% mutate(word2 =  word(word_right, 2))
dfTrain3 <- dfTrain3 %>% mutate(word1 =  word(word_right, 1))%>% mutate(word2 =  word(word_right, 2)) %>% mutate(word3 =  word(word_right, 3))


